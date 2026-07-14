module lang::erlang::M3

extend analysis::m3::Core;
extend analysis::m3::TypeSymbol;

import List;
import Message;
import String;
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

    /**
     * Traverses and resolves identifiers within patterns that are parsed as expressions
     * Some pattern-constructs such as map patters are parsed using Expression nodes
     * This function ensres that any Expression::var nodes occurring within
     * Pattern-like context are treated as declarations and added to environment
     * instead of Expression use, but to make sure that sub-expressions are treated
     * as Expression uses
     */
    Env analysePatternScope(value n, loc scopeLoc, Env currentEnv) {
        top-down-break visit(n) {
            // Treat expression variables in a pattern context as declarations
            case Expression::var(Annotation a, str name): {
                if (name != "_") {
                    loc physLoc = annoToLoc(fileLoc, a);
                    if (name notin currentEnv) {
                        loc varLoc = scopeLoc[scheme="erlang+variable"][path="<scopeLoc.path>/<name>"];
                        model.declarations += {<varLoc, physLoc>};
                        model.containment += {<scopeLoc, varLoc>};
                        currentEnv[name] = varLoc;
                    } else {
                        model.uses += {<physLoc, currentEnv[name]>};
                    }
                }
            }
            case Pattern::var(Annotation a, str name): {
                if (name != "_") {
                    loc physLoc = annoToLoc(fileLoc, a);
                    if (name notin currentEnv) {
                        loc varLoc = scopeLoc[scheme="erlang+variable"][path="<scopeLoc.path>/<name>"];
                        model.declarations += {<varLoc, physLoc>};
                        model.containment += {<scopeLoc, varLoc>};
                        currentEnv[name] = varLoc;
                    } else {
                        model.uses += {<physLoc, currentEnv[name]>};
                    }
                }
            }
            // Recursively handle nested map patterns
            case Pattern::\map(_, list[Association] associations): {
                for (\assoc <- associations) {
                    currentEnv = analyseScope(\assoc.key, scopeLoc, currentEnv);  // Keys are always uses
                    currentEnv = analysePatternScope(\assoc.\value, scopeLoc, currentEnv);  // Values are patterns
                }
            }
            case Expression::\map(_, list[Association] associations): {
                for (\assoc <- associations) {
                    currentEnv = analyseScope(\assoc.key, scopeLoc, currentEnv);
                    currentEnv = analysePatternScope(\assoc.\value, scopeLoc, currentEnv);
                }
            }
            // Handle binary element patterns
            case binElementExpr(_, Expression val, OptSize size, _): {
                currentEnv = analysePatternScope(val, scopeLoc, currentEnv);
                if (size(Expression expr) := size) {
                    currentEnv = analyseScope(expr, scopeLoc, currentEnv);  // Size is a use
                }
            }
            case binElementPatt(_, Pattern val, OptSize size, _): {
                currentEnv = analysePatternScope(val, scopeLoc, currentEnv);
                if (size(Expression expr) := size) {
                    currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                }
            }
            // Handle record fields matching inside map values
            case Expression::record(Annotation a, str name, list[RecordFieldExpr] fields): {
                registerRecordUse(a, name);
                for (recordFieldExpr(Annotation fa, Expression::literal(atom(_, str fn)), _) <- fields) {
                    registerFieldUse(fa, name, fn);
                }
                for (f <- fields) {
                    currentEnv = analysePatternScope(f.\value, scopeLoc, currentEnv);
                }
            }
            case Expression::recordUpdate(Annotation a, Expression expr, str name, list[RecordFieldExpr] fields): {
                registerRecordUse(a, name);
                currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                for (recordFieldExpr(Annotation fa, Expression::literal(atom(_, str fn)), _) <- fields) {
                    registerFieldUse(fa, name, fn);
                }
                for (f <- fields) {
                    currentEnv = analysePatternScope(f.\value, scopeLoc, currentEnv);
                }
            }
        }
        return currentEnv;
    }

    // Traverses nested Type nodes
    void analyseType(Type t) {
        top-down visit(t) {
            case userType(Annotation a, str typeName, list[Type] args): {
                loc typeLoc = |erlang+type:///<currentModName>/<typeName>/<toString(size(args))>|;
                model.uses += {<annoToLoc(fileLoc, a), typeLoc>};
            }
            case remoteType(Annotation a, Type \module, Type name, list[Type] args): {
                if (Type::literal(atom(_, str moduleName)) := \module
                    , Type::literal(atom(_, str typeName)) := name) {
                    loc typeLoc = |erlang+type:///<moduleName>/<typeName>/<toString(size(args))>|;
                    model.uses += {<annoToLoc(fileLoc, a), typeLoc>};
                }
            }
            case record(_, list[Type] fields): {
                if ([Type::literal(atom(Annotation fa, str recName)), *_] := fields) {
                    loc recLoc = |erlang+record:///<currentModName>/<recName>|;
                    model.uses += {<annoToLoc(fileLoc, fa), recLoc>};
                }
            }
        }
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
                        case typedRecordField(Annotation fa, Expression fe, Type t): { 
                            fieldAnno = fa; fieldExpr = fe; 
                            analyseType(t);
                        }
                        case typedRecordField(Annotation fa, Expression fe, _, Type t): { 
                            fieldAnno = fa; fieldExpr = fe; 
                            analyseType(t);
                        }
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

                analyseType(\type);
                for (Type v <- vars) {
                    analyseType(v);
                }
            }

            // (-opaque) type definitions
            case opaqueDecl(Annotation a, str name, Type \type, list[Type] vars): {
                loc typeLoc = |erlang+type:///<currentModName>/<name>/<toString(size(vars))>|;
                loc physLoc = annoToLoc(fileLoc, a);

                model.declarations += {<typeLoc, physLoc>};
                model.containment += {<currentModule, typeLoc>};
                model.names += {<name, physLoc>};
                model.types += {<typeLoc, erlangType(\type)>};

                analyseType(\type);
                for (Type v <- vars) {
                    analyseType(v);
                }
            }

            // (-spec) function
            case functionSpec(_, str name, int arity, list[Type] signatures): {
                loc funcLoc = |erlang+function:///<currentModName>/<name>/<toString(arity)>|;
                model.types += {<funcLoc, erlangType(s)> | s <- signatures};

                for (Type s <- signatures) {
                    analyseType(s);
                }
            }

            // (-spec Mod:Name) remote function
            case functionSpec(_, str modName, str name, int arity, list[Type] signatures): {
                loc funcLoc = |erlang+function:///<modName>/<name>/<toString(arity)>|;
                model.types += {<funcLoc, erlangType(s)> | s <- signatures};

                for (Type s <- signatures) {
                    analyseType(s);
                }
            }

            // (-callback) callback spec
            case callbackSpec(_, str name, int arity, list[Type] signatures): {
                loc funcLoc = |erlang+function:///<currentModName>/<name>/<toString(arity)>|;
                model.types += {<funcLoc, erlangType(s)> | s <- signatures};

                for (Type s <- signatures) {
                    analyseType(s);
                }
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

    void registerRecordUse(Annotation a, str name) {
        loc recLoc = |erlang+record:///<currentModName>/<name>|;
        loc physLoc = annoToLoc(fileLoc, a);
        model.uses += {<physLoc, recLoc>};
    }

    void registerFieldUse(Annotation fa, str rn, str fn) {
        loc fieldLoc = |erlang+field:///<currentModName>/<rn>/<fn>|;
        loc fieldPhysLoc = annoToLoc(fileLoc, fa);
        model.uses += {<fieldPhysLoc, fieldLoc>};
    }

    Env analyseGen(Pattern pat, Expression expr, loc scopeLoc, Env env) {
        env = analyseScope(expr, scopeLoc, env);
        return analyseScope(pat, scopeLoc, env);
    }

    Env analyseMGen(Association association, Expression expr, loc scopeLoc, Env env) {
        env = analyseScope(expr, scopeLoc, env);
        return analyseScope(association, scopeLoc, env);
    }

    void analyseComprehension(str prefix, list[Qualifier] qualifiers, node head, loc scopeLoc, Env env) {
        loc innerScope = scopeLoc[path="<scopeLoc.path>/<getNextScopeId(prefix)>"];
        env = analyseScope(qualifiers, innerScope, env);
        analyseScope(head, innerScope, env);
    }

    loc resolveLocalCallee(str funName, int arity) {
        tuple[str, int] identifier = <funName, arity>;
        if (identifier in localFunctions) {
            return |erlang+function:///<currentModName>/<funName>/<toString(arity)>|;
        } 
        if (identifier in importedFunctions) {
            str \module = importedFunctions[identifier];
            return |erlang+function:///<\module>/<funName>/<toString(arity)>|;
        }
        return |erlang+unresolved:///|;
    }

    void registerCall(Annotation a, loc callee, loc scopeLoc) {
        loc physLoc = annoToLoc(fileLoc, a);
        model.uses += {<physLoc, callee>};
        if (scopeLoc.scheme != "unknown") {
            model.functionCalls += {<scopeLoc, callee>};
        }
    }

    // `value` for n should be `node` or `list[node]`
    // TODO: Find out of we can define type unions in Rascal
    Env analyseScope(value n, loc scopeLoc, Env currentEnv) {
        // We visit root first such that we have the correct function information for subnodes
        top-down-break visit(n) {
            // Function clauses
            case clause(_, list[Pattern] patterns, GuardSeq guards, Body body): {
                loc innerScope = scopeLoc[path="<scopeLoc.path>/<getNextScopeId("clause")>"];
                innerEnv = currentEnv;
                innerEnv = analyseScope(patterns, innerScope, innerEnv);
                innerEnv = analyseScope(guards, innerScope, innerEnv);
                analyseScope(body, innerScope, innerEnv);
            }

            // Anonymous functions
            case fun(_, list[Clause] clauses): {
                loc innerScope = scopeLoc[path="<scopeLoc.path>/<getNextScopeId("fun")>"];
                for (Clause c <- clauses) {
                    analyseScope(c, innerScope, currentEnv);
                }
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
            }

            // Comprehensions
            case lc(_, Expression expr, list[Qualifier] qualifiers): {
                analyseComprehension("lc", qualifiers, expr, scopeLoc, currentEnv);
            }
            case bc(_, Expression template, list[Qualifier] qualifiers): {
                analyseComprehension("bc", qualifiers, template, scopeLoc, currentEnv);
            }
            case mc(_, Association association, list[Qualifier] qualifiers): {
                analyseComprehension("mc", qualifiers, association, scopeLoc, currentEnv);
            }

            // Match evaluates RHS first, then binds LHS
            case match(_, Pattern pat, Expression expr): {
                currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                currentEnv = analyseScope(pat, scopeLoc, currentEnv);
            }
            case maybeMatch(_, Pattern pat, Expression expr): {
                currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                currentEnv = analyseScope(pat, scopeLoc, currentEnv);
            }

            // Maybe has its own scope
            case maybe(_, Body body): {
                loc innerScope = scopeLoc[path="<scopeLoc.path>/<getNextScopeId("maybe")>"];
                analyseScope(body, innerScope, currentEnv);
            }
            case maybe(_, Body body, _, list[Clause] elseClauses): {
                loc innerScope = scopeLoc[path="<scopeLoc.path>/<getNextScopeId("maybe")>"];
                analyseScope(body, innerScope, currentEnv);
                for (c <- elseClauses)
                    analyseScope(c, innerScope, currentEnv);
            }

            // Record instantiation
            case Expression::record(Annotation a, str name, list[RecordFieldExpr] fields): {
                registerRecordUse(a, name);
                for (recordFieldExpr(Annotation fa, Expression::literal(atom(_, str fn)), _) <- fields) {
                    registerFieldUse(fa, name, fn);
                }
                currentEnv = analyseScope(fields, scopeLoc, currentEnv);
            }
            // Record field access
            case Expression::recordField(Annotation a, Expression expr, str name, Expression field): {
                registerRecordUse(a, name);
                currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                if (Expression::literal(atom(Annotation fa, str fn)) := field) {
                    registerFieldUse(fa, name, fn);
                }
                currentEnv = analyseScope(field, scopeLoc, currentEnv);
            }
            // Record index
            case Expression::recordIndex(Annotation a, str name, Expression field): {
                registerRecordUse(a, name);
                if (Expression::literal(atom(Annotation fa, str fn)) := field) {
                    registerFieldUse(fa, name, fn);
                }
                currentEnv = analyseScope(field, scopeLoc, currentEnv);
            }
            // Record update
            case Expression::recordUpdate(Annotation a, Expression expr, str name, list[RecordFieldExpr] fields): {
                registerRecordUse(a, name);
                currentEnv = analyseScope(expr, scopeLoc, currentEnv);
                for (recordFieldExpr(Annotation fa, Expression::literal(atom(_, str fn)), _) <- fields) {
                    registerFieldUse(fa, name, fn);
                }
                currentEnv = analyseScope(fields, scopeLoc, currentEnv);
            }
            // Record pattern match
            case Pattern::record(Annotation a, str name, list[RecordFieldPattern] fields): {
                registerRecordUse(a, name);
                for (recordFieldPattern(Annotation fa, Pattern::literal(atom(_, str fn)), _) <- fields) {
                    registerFieldUse(fa, name, fn);
                }
                currentEnv = analyseScope(fields, scopeLoc, currentEnv);
            }
            // Record index pattern
            case Pattern::recordIndex(Annotation a, str name, Pattern field): {
                registerRecordUse(a, name);
                if (Pattern::literal(atom(Annotation fa, str fn)) := field) {
                    registerFieldUse(fa, name, fn);
                }
                currentEnv = analyseScope(field, scopeLoc, currentEnv);
            }
            
            // Generators P <- E evaluate E first, then bind P
            case generate(_, Pattern pat, Expression expr): {
                currentEnv = analyseGen(pat, expr, scopeLoc, currentEnv);
            }
            case generateStrict(_, Pattern pat, Expression expr): {
                currentEnv = analyseGen(pat, expr, scopeLoc, currentEnv);
            }
            case bGenerate(_, Pattern pat, Expression expr): {
                currentEnv = analyseGen(pat, expr, scopeLoc, currentEnv);
            }
            case bGenerateStrict(_, Pattern pat, Expression expr): {
                currentEnv = analyseGen(pat, expr, scopeLoc, currentEnv);
            }
            case mGenerate(_, Association association, Expression expr): {
                currentEnv = analyseMGen(association, expr, scopeLoc, currentEnv);
            }
            case mGenerateStrict(_, Association association, Expression expr): {
                currentEnv = analyseMGen(association, expr, scopeLoc, currentEnv);
            }

            // Vars
            case Pattern::var(Annotation a, str name): {
                // TODO: Also ignore anything starting with '_'?
                if (name != "_") {
                    loc physLoc = annoToLoc(fileLoc, a);
                    if (name notin currentEnv) {
                        // Declaration
                        loc varLoc = scopeLoc[scheme="erlang+variable"][path="<scopeLoc.path>/<name>"];

                        model.declarations += {<varLoc, physLoc>};
                        model.containment += {<scopeLoc, varLoc>};
                        
                        currentEnv[name] = varLoc;
                    } else {
                        // Use
                        model.uses += {<physLoc, currentEnv[name]>};
                    }
                }
            }
            case Expression::var(Annotation a, str name): {
                if (name != "_") {
                    loc physLoc = annoToLoc(fileLoc, a);
                    if (name in currentEnv) {
                        model.uses += {<physLoc, currentEnv[name]>};
                    } else {
                        // If this occurs, it is probably very bad
                        loc varLoc = scopeLoc[scheme="erlang+variable"][path="<scopeLoc.path>/<name>"];
                        model.uses += {<physLoc, varLoc>};
                    }
                }
            }

            // Local funcall
            // TODO: Check if (nested) named functions are handled correctly
            case Expression::call(Annotation a, Expression funExpr, list[Expression] args): {
                if (Expression::literal(atom(_, str funName)) := funExpr || Expression::var(_, str funName) := funExpr) {
                    loc callee = resolveLocalCallee(funName, size(args));
                    registerCall(a, callee, scopeLoc);
                }
                currentEnv = analyseScope([funExpr, args], scopeLoc, currentEnv);
            }

            // Remote funcall
            case Expression::call(Annotation a, Expression modExpr, Expression funExpr, list[Expression] args): {
                if (Expression::literal(atom(_, str targetMod)) := modExpr, Expression::literal(atom(_, str funName)) := funExpr) {
                    loc callee = |erlang+function:///<targetMod>/<funName>/<toString(size(args))>|;
                    registerCall(a, callee, scopeLoc);
                } else {
                    registerCall(a, |unresolved:///dynamic_call|, scopeLoc);
                }
                currentEnv = analyseScope([modExpr, funExpr, args], scopeLoc, currentEnv);
            }

            // Map
            case Pattern::\map(_, list[Association] associations): {
                for (\assoc <- associations) {
                    currentEnv = analyseScope(\assoc.key, scopeLoc, currentEnv);  // Expression uses
                    currentEnv = analysePatternScope(\assoc.\value, scopeLoc, currentEnv);  // Pattern declarations
                }
            }

            // Local function reference (`fun Name/Arity`)
            case funDecl(Annotation a, str name, int arity): {
                loc callee = resolveLocalCallee(name, arity);
                registerCall(a, callee, scopeLoc);
            }
            // Remote function reference (`fun Mod:Name/Arity`)
            case funDecl(Annotation a, Expression modExpr, Expression funExpr, Expression arityExpr): {
                if (Expression::literal(atom(_, str targetMod)) := modExpr, 
                    Expression::literal(atom(_, str funName)) := funExpr, 
                    Expression::literal(integer(_, str arityStr)) := arityExpr) {
                    
                    int arity = toInt(arityStr);
                    loc callee = |erlang+function:///<targetMod>/<funName>/<toString(arity)>|;
                    registerCall(a, callee, scopeLoc);
                } else {
                    registerCall(a, |unresolved:///dynamic_call|, scopeLoc);
                }
                currentEnv = analyseScope([modExpr, funExpr, arityExpr], scopeLoc, currentEnv);
            }
        }

        return currentEnv;
    }

    for (functionDecl(Annotation a, str name, int arity, list[Clause] clauses) <- ast) {
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

    return model;
}

/**
 * Checks for export_all or compile_all
 */
bool hasExportAll(EAF ast) {
    for (wildAttr(_, "compile", value \value) <- ast) {
        if (str sv := \value && (sv == "export_all" || sv == "compile_all")) return true;
        if (list[str] lv := \value && ("export_all" in lv || "compile_all" in lv)) return true;
    }

    return false;
}
