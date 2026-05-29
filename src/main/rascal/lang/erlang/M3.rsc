module lang::erlang::M3

extend analysis::m3::Core;
extend analysis::m3::TypeSymbol;

import List;
import Message;
import util::Math;
import lang::erlang::AST;

// Used to keep track of visible variables in nested scopes
private alias Env = map[str, loc];

data M3(
    rel[loc caller, loc callee] functionCalls = {}
);

data Language = erlang(str version = "");

data Modifier
    = \public()
    | \private()
    ;

data TypeSymbol
    = erlangType(Type astType)
    ;

loc annoToLoc(loc fileLoc, Annotation \anno) {
    switch (\anno) {
        case \anno(int l, int c): return fileLoc(0, 0, <l, c>, <l, c>);
        default: throw "Unrecognised Annotation <\anno>";
    }
}

M3 extractErlangM3(loc fileLoc, EAF ast) {
    M3 model = m3(fileLoc);

    model.languages = { erlang() };

    str currentModName = "unknown";
    loc currentModule = |erlang+module:///unknown|;

    set[tuple[str name, int arity]] localFunctions = {};
    map[tuple[str name, int arity] _, str \module] importedFunctions = ();

    bool exportAll = hasExportAll(ast);

    // Module first so all other forms have valid location information
    for (moduleAttr(Annotation a, str name) <- ast) {
        currentModName = name;
        currentModule = |erlang+module:///<name>|;
        loc physLoc = annoToLoc(fileLoc, a);

        model.declarations += {<currentModule, physLoc>};
        model.names += {<name, physLoc>};
    }

    // Pre-process exports etc to ensure they're marked as public before big traversal
    for (Form f <- ast) {
        switch (f) {
            // Exports
            case exportAttr(_, list[tuple[str name, int arity]] exports): {
                for (<str funcName, int arity> <- exports) {
                    loc funcLoc = |erlang+function:///<currentModName>/<funcName>/<toString(arity)>|;
                    model.modifiers += {<funcLoc, \public()>};
                }
            }

            // Imports
            case importAttr(_, str \module, list[tuple[str name, int arity]] imports): {
                importedFunctions += (i : \module | i <- imports);
            }

            // Function declarations
            case functionDecl(_, str name, int arity, _): {
                localFunctions += {<name, arity>};
            }

            // Records
            case recordDecl(Annotation a, str name, list[RecordField] fields): {
                loc recLoc = |erlang+record:///<currentModName>/<name>|;
                loc physLoc = annoToLoc(fileLoc, a);

                model.declarations += {<recLoc, physLoc>};
                model.containment += {<currentModule, recLoc>};
                model.names += {<name, physLoc>};

                for (RecordField rf <- fields) {
                    // These will always be overwritten but I like my modules warning-free
                    // and Rascal insists on initialising variables so here are mock values
                    Annotation fieldAnno = \anno(0, 0);
                    Expression fieldExpr = Expression::var(\anno(0, 0), "");

                    switch (rf) {
                        case recordField(Annotation fa, Expression fe): { fieldAnno = fa; fieldExpr = fe; }
                        case recordField(Annotation fa, Expression fe, _): { fieldAnno = fa; fieldExpr = fe; }
                        case typedRecordField(Annotation fa, Expression fe, _): { fieldAnno = fa; fieldExpr = fe; }
                        case typedRecordField(Annotation fa, Expression fe, _, _): { fieldAnno = fa; fieldExpr = fe; }
                        default: throw "M3: Unexpected recordField <rf>";
                    }

                    if (Expression::literal(atom(_, str fn)) := fieldExpr) {
                        loc fieldLoc = |erlang+field:///<currentModName>/<name>/<fn>|;
                        loc fieldPhysLoc = annoToLoc(fileLoc, fieldAnno);
                        
                        model.declarations += {<fieldLoc, fieldPhysLoc>};
                        model.containment += {<recLoc, fieldLoc>};
                        model.names += {<fn, fieldPhysLoc>};
                    } else {
                        throw "M3: Unexpected shape of fieldExpr <fieldExpr>";
                    }
                }
            }

            // (-type) type definitions
            case typeDecl(Annotation a, str name, Type \type, list[Type] vars): {
                loc typeLoc = |erlang+type:///<currentModName>/<name>/<toString(size(vars))>|;
                loc physLoc = annoToLoc(fileLoc, a);

                model.declarations += {<typeLoc, physLoc>};
                model.containment += {<currentModule, typeLoc>};
                model.names += {<name, physLoc>};
                model.types += {<typeLoc, erlangType(\type)>};
            }

            // (-opaque) type definitions
            case opaqueDecl(Annotation a, str name, Type \type, list[Type] vars): {
                loc typeLoc = |erlang+type:///<currentModName>/<name>/<toString(size(vars))>|;
                loc physLoc = annoToLoc(fileLoc, a);

                model.declarations += {<typeLoc, physLoc>};
                model.containment += {<currentModule, typeLoc>};
                model.names += {<name, physLoc>};
                model.types += {<typeLoc, erlangType(\type)>};
            }

            // (-spec) function
            case functionSpec(_, str name, int arity, list[Type] signatures): {
                loc funcLoc = |erlang+function:///<currentModName>/<name>/<toString(arity)>|;
                model.types += {<funcLoc, erlangType(s)> | s <- signatures};
            }

            // (-spec Mod:Name) remote function
            case functionSpec(_, str modName, str name, int arity, list[Type] signatures): {
                loc funcLoc = |erlang+function:///<modName>/<name>/<toString(arity)>|;
                model.types += {<funcLoc, erlangType(s)> | s <- signatures};
            }

            // (-callback) callback spec
            case callbackSpec(_, str name, int arity, list[Type] signatures): {
                loc funcLoc = |erlang+function:///<currentModName>/<name>/<toString(arity)>|;
                model.types += {<funcLoc, erlangType(s)> | s <- signatures};
            }

            // Warnings and errors
            case error(Annotation a, _, value description): {
                model.messages += [error("<description>", annoToLoc(fileLoc, a))];
            }
            case warning(Annotation a, _, value description): {
                model.messages += [warning("<description>", annoToLoc(fileLoc, a))];
            }
        }
    }

    // To ensure unique identifiers for all anonymous scopes
    int scopeIdCounter = 0;

    str getNextScopeId(str prefix) {
        scopeIdCounter += 1;
        return "<prefix>_<scopeIdCounter>";
    }

    // `value` for n should be `node` or `list[node]`
    // TODO: Find out of we can define type unions in Rascal
    Env analyseScope(value n, loc scopeLoc, Env startingEnv) {
        Env currentEnv = startingEnv;

        // We visit root first such that we have the correct function information for subnodes
        top-down visit(n) {
            // Function clauses
            case clause(_, list[Pattern] patterns, GuardSeq guards, Body body): {
                loc innerScope = scopeLoc[path="<scopeLoc.path>/<getNextScopeId("clause")>"];
                innerEnv = currentEnv;
                innerEnv = analyseScope(patterns, innerScope, innerEnv);
                innerEnv = analyseScope(guards, innerScope, innerEnv);
                analyseScope(body, innerScope, innerEnv);

                fail;
            }

            // Anonymous functions
            case fun(_, list[Clause] clauses): {
                loc innerScope = scopeLoc[path="<scopeLoc.path>/<getNextScopeId("fun")>"];
                for (Clause c <- clauses) {
                    analyseScope(c, innerScope, currentEnv);
                }
                fail;
            }
            case namedFun(Annotation a, str name, list[Clause] clauses): {
                loc innerScope = scopeLoc[path="<scopeLoc.path>/<getNextScopeId("named_fun")>"];

                loc nameLoc = innerScope[scheme="erlang+variable"][path="<innerScope.path>/<name>"];
                loc physLoc = annoToLoc(fileLoc, a);
                model.declarations += {<nameLoc, physLoc>};

                // Add function name to its own scope
                funEnv = currentEnv + (name : nameLoc);
                for (Clause c <- clauses) {
                    analyseScope(c, innerScope, funEnv);
                }
                fail;
            }

            // Comprehensions
            case lc(_, Expression expr, list[Qualifier] qualifiers): {
                loc innerScope = scopeLoc[path="<scopeLoc.path>/<getNextScopeId("lc")>"];
                innerEnv = currentEnv;
                innerEnv = analyseScope(qualifiers, innerScope, innerEnv);
                analyseScope(expr, innerScope, innerEnv);
                fail;
            }
            case bc(_, Expression template, list[Qualifier] qualifiers): {
                loc innerScope = scopeLoc[path="<scopeLoc.path>/<getNextScopeId("bc")>"];
                innerEnv = currentEnv;
                innerEnv = analyseScope(qualifiers, innerScope, innerEnv);
                analyseScope(template, innerScope, innerEnv);
                fail;
            }
            case mc(_, Association association, list[Qualifier] qualifiers): {
                loc innerScope = scopeLoc[path="<scopeLoc.path>/<getNextScopeId("mc")>"];
                innerEnv = currentEnv;
                innerEnv = analyseScope(qualifiers, innerScope, innerEnv);
                analyseScope(association, innerScope, innerEnv);
                fail;
            }

            // Match evaluates RHS first, then binds LHS
            case match(_, Pattern pat, Expression expr): {
                currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                currentEnv = analyseScope(pat, scopeLoc, currentEnv);
                fail;
            }
            case maybeMatch(_, Pattern pat, Expression expr): {
                currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                currentEnv = analyseScope(pat, scopeLoc, currentEnv);
                fail;
            }

            // Maybe has its own scope
            case maybe(_, Body body): {
                loc innerScope = scopeLoc[path="<scopeLoc.path>/<getNextScopeId("maybe")>"];
                analyseScope(body, innerScope, currentEnv);
                fail;
            }
            case maybe(_, Body body, _, list[Clause] elseClauses): {
                loc innerScope = scopeLoc[path="<scopeLoc.path>/<getNextScopeId("maybe")>"];
                analyseScope(body, innerScope, currentEnv);
                for (c <- elseClauses)
                    analyseScope(c, innerScope, currentEnv);
                fail;
            }

            // Record instantiation
            case Expression::record(Annotation a, str name, list[RecordFieldExpr] fields): {
                loc recLoc = |erlang+record:///<currentModName>/<name>|;
                loc physLoc = annoToLoc(fileLoc, a);
                model.uses += {<physLoc, recLoc>};

                for (RecordFieldExpr rfe <- fields) {
                    if (recordFieldExpr(Annotation fa, Expression::literal(atom(_, str fn)), _) := rfe) {
                        loc fieldLoc = |erlang+field:///<currentModName>/<name>/<fn>|;
                        loc fieldPhysLoc = annoToLoc(fileLoc, fa);
                        model.uses += {<fieldPhysLoc, fieldLoc>};
                    }
                }
            }
            // Record field access
            case Expression::recordField(Annotation a, Expression expr, str name, Expression field): {
                loc recLoc = |erlang+record:///<currentModName>/<name>|;
                loc physLoc = annoToLoc(fileLoc, a);
                model.uses += {<physLoc, recLoc>};

                if (Expression::literal(atom(Annotation fa, str fn)) := field) {
                    loc fieldLoc = |erlang+field:///<currentModName>/<name>/<fn>|;
                    loc fieldPhysLoc = annoToLoc(fileLoc, fa);
                    model.uses += {<fieldPhysLoc, fieldLoc>};
                }
            }
            // Record index
            case Expression::recordIndex(Annotation a, str name, Expression field): {
                loc recLoc = |erlang+record:///<currentModName>/<name>|;
                loc physLoc = annoToLoc(fileLoc, a);
                model.uses += {<physLoc, recLoc>};

                if (Expression::literal(atom(Annotation fa, str fn)) := field) {
                    loc fieldLoc = |erlang+field:///<currentModName>/<name>/<fn>|;
                    loc fieldPhysLoc = annoToLoc(fileLoc, fa);
                    model.uses += {<fieldPhysLoc, fieldLoc>};
                }
            }
            // Record update
            case Expression::recordUpdate(Annotation a, Expression expr, str name, list[RecordFieldExpr] fields): {
                loc recLoc = |erlang+record:///<currentModName>/<name>|;
                loc physLoc = annoToLoc(fileLoc, a);
                model.uses += {<physLoc, recLoc>};

                for (RecordFieldExpr rfe <- fields) {
                    if (recordFieldExpr(Annotation fa, Expression::literal(atom(_, str fn)), _) := rfe) {
                        loc fieldLoc = |erlang+field:///<currentModName>/<name>/<fn>|;
                        loc fieldPhysLoc = annoToLoc(fileLoc, fa);
                        model.uses += {<fieldPhysLoc, fieldLoc>};
                    }
                }
            }
            // Record pattern match
            case Pattern::record(Annotation a, str name, list[RecordFieldPattern] fields): {
                loc recLoc = |erlang+record:///<currentModName>/<name>|;
                loc physLoc = annoToLoc(fileLoc, a);
                model.uses += {<physLoc, recLoc>};

                for (RecordFieldPattern rfp <- fields) {
                    if (recordFieldPattern(Annotation fa, Pattern::literal(atom(_, str fn)), _) := rfp) {
                        loc fieldLoc = |erlang+field:///<currentModName>/<name>/<fn>|;
                        loc fieldPhysLoc = annoToLoc(fileLoc, fa);
                        model.uses += {<fieldPhysLoc, fieldLoc>};
                    }
                }
            }
            // Record index pattern
            case Pattern::recordIndex(Annotation a, str name, Pattern field): {
                loc recLoc = |erlang+record:///<currentModName>/<name>|;
                loc physLoc = annoToLoc(fileLoc, a);
                model.uses += {<physLoc, recLoc>};

                if (Pattern::literal(atom(Annotation fa, str fn)) := field) {
                    loc fieldLoc = |erlang+field:///<currentModName>/<name>/<fn>|;
                    loc fieldPhysLoc = annoToLoc(fileLoc, fa);
                    model.uses += {<fieldPhysLoc, fieldLoc>};
                }
            }
            
            // Generators P <- E evaluate E first, then bind P
            case generate(_, Pattern pat, Expression expr): {
                currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                currentEnv = analyseScope(pat, scopeLoc, currentEnv);
                fail;
            }
            case generateStrict(_, Pattern pat, Expression expr): {
                currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                currentEnv = analyseScope(pat, scopeLoc, currentEnv);
                fail;
            }
            case bGenerate(_, Pattern pat, Expression expr): {
                currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                currentEnv = analyseScope(pat, scopeLoc, currentEnv);
                fail;
            }
            case bGenerateStrict(_, Pattern pat, Expression expr): {
                currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                currentEnv = analyseScope(pat, scopeLoc, currentEnv);
                fail;
            }
            case mGenerate(_, Association association, Expression expr): {
                currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                currentEnv = analyseScope(association, scopeLoc, currentEnv);
                fail;
            }
            case mGenerateStrict(_, Association association, Expression expr): {
                currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                currentEnv = analyseScope(association, scopeLoc, currentEnv);
                fail;
            }

            // Vars
            case Pattern::var(Annotation a, str name): {
                // TODO: Also ignore anything starting with '_'?
                if (name != "_") {
                    if (name notin currentEnv) {
                        // Declaration
                        loc varLoc = scopeLoc[scheme="erlang+variable"][path="<scopeLoc.path>/<name>"];
                        loc physLoc = annoToLoc(fileLoc, a);

                        model.declarations += {<varLoc, physLoc>};
                        model.containment += {<scopeLoc, varLoc>};
                        
                        currentEnv[name] = varLoc;
                    } else {
                        // Use
                        loc physLoc = annoToLoc(fileLoc, a);
                        model.uses += {<physLoc, currentEnv[name]>};
                    }
                }
            }
            case Expression::var(Annotation a, str name): {
                if (name != "_") {
                    if (name in currentEnv) {
                        loc physLoc = annoToLoc(fileLoc, a);
                        model.uses += {<physLoc, currentEnv[name]>};
                    } else {
                        // If this occurs, it is probably very bad
                        loc varLoc = scopeLoc[scheme="erlang+variable"][path="<scopeLoc.path>/<name>"];
                        loc physLoc = annoToLoc(fileLoc, a);
                        model.uses += {<physLoc, varLoc>};
                    }
                }
            }

            // Local funcall
            // TODO: Check if (nested) named functions are handled correctly
            case Expression::call(Annotation a, Expression funExpr, list[Expression] args): {
                if (Expression::literal(atom(_, str funName)) := funExpr ||
                    Expression::var(_, str funName) := funExpr) {

                    int arity = size(args);

                    loc callee = |unknown:///|;  // Rascal insists on immediate variable initialisation
                    identifier = <funName, arity>;
                    if (identifier in localFunctions) {
                        callee = |erlang+function:///<currentModName>/<funName>/<toString(arity)>|;
                    } else if (identifier in importedFunctions) {
                        str \module = importedFunctions[<funName, arity>];
                        callee = |erlang+function:///<\module>/<funName>/<toString(arity)>|;
                    } else {
                        // Built-in or dynamic call resolved at runtime?
                        callee = |erlang+unresolved:///|;
                    }

                    loc physLoc = annoToLoc(fileLoc, a);

                    model.uses += {<physLoc, callee>};
                    if (scopeLoc.scheme != "unknown") {
                        model.functionCalls += {<scopeLoc, callee>};
                    }
                }
            }

            // Remote funcall
            case Expression::call(Annotation a, Expression modExpr, Expression funExpr, list[Expression] args): {
                if (Expression::literal(atom(_, str targetMod)) := modExpr,
                    Expression::literal(atom(_, str funName)) := funExpr) {

                    int arity = size(args);
                    loc callee = |erlang+function:///<targetMod>/<funName>/<toString(arity)>|;
                    loc physLoc = annoToLoc(fileLoc, a);
                    
                    model.uses += {<physLoc, callee>};
                    if (scopeLoc.scheme != "unknown") {
                        model.functionCalls += {<scopeLoc, callee>};
                    }
                } else {
                    loc physLoc = annoToLoc(fileLoc, a);
                    model.uses += {<physLoc, |unresolved:///dynamic_call|>};
                }
            }
        }

        return currentEnv;
    }

    for (Form f <- ast) {
        if (functionDecl(Annotation a, str name, int arity, list[Clause] clauses) := f) {
            loc funcLoc = |erlang+function:///<currentModName>/<name>/<toString(arity)>|;
            loc physLoc = annoToLoc(fileLoc, a);
            
            model.declarations += {<funcLoc, physLoc>};
            model.containment += {<|erlang+module:///<currentModName>|, funcLoc>};
            model.names += {<name, physLoc>};
            
            if (exportAll)
                model.modifiers += {<funcLoc, \public()>};
            else if (<funcLoc, \public()> notin model.modifiers)
                model.modifiers += {<funcLoc, \private()>};

            for (Clause c <- clauses) 
                analyseScope(c, funcLoc, ());
        }
    }

    return model;
}

/**
 * Checks for export_all or compile_all
 */
bool hasExportAll(EAF ast) {
    for (wildAttr(_, str name, value \value) <- ast) {
        if (name != "compile") continue;

        if (str sv := \value && (sv == "export_all" || sv == "compile_all")) return true;
        if (list[str] lv := \value && ("export_all" in lv || "compile_all" in lv)) return true;
    }

    return false;
}
