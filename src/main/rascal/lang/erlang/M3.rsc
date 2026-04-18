module lang::erlang::M3

extend analysis::m3::Core;  // "extend" is is what clair does, I don't know why

import List;
import util::Math;
import lang::erlang::AST;

data M3(
    rel[loc caller, loc callee] functionCalls = {}
);

data Language = erlang(str version = "");

data Modifier
    = \public()
    | \private()
    ;

loc annoToLoc(loc fileLoc, Annotation \anno) {
    switch (\anno) {
        case \anno(int l): return fileLoc(0, 0, <l, 0>, <l, 0>);
        case \anno(int l, _): return fileLoc(0, 0, <l, 0>, <l, 0>);
        default: throw "Unrecognised Annotation <\anno>";
    }
}

M3 extractErlangM3(loc fileLoc, EAF ast) {
    // TODO: Check if state is properly modified (not leaking) when traversing from and to functions

    M3 model = m3(fileLoc);

    model.languages = { erlang() };

    str currentModName = "unknown";
    loc currentModule = |erlang+module:///unknown|;
    loc currentFunction = |unknown:///|;

    // Pre-process exports etc to ensure they're marked as public before big traversal
    for (Form f <- ast) {
        switch (f) {
            // Module definitions
            case moduleAttr(Annotation a, str name): {
                currentModName = name;
                currentModule = |erlang+module:///<name>|;
                loc physLoc = annoToLoc(fileLoc, a);

                model.declarations += {<currentModule, physLoc>};
                model.names += {<name, physLoc>};
            }

            // Exports
            case exportAttr(Annotation a, list[tuple[str name, int arity]] exports): {
                for (<str funcName, int arity> <- exports) {
                    loc funcLoc = |erlang+function:///<currentModName>/<funcName>/<toString(arity)>|;
                    model.modifiers += {<funcLoc, \public()>};
                }
            }

            // Records
            case recordDecl(Annotation a, str name, _): {
                loc recLoc = |erlang+record:///<currentModName>/<name>|;
                loc physLoc = annoToLoc(fileLoc, a);

                model.declarations += {<currentFunction, recLoc>};
                model.containment += {<currentModule, recLoc>};
                model.names += {<name, physLoc>};
            }
        }
    }

    // Separate fn to avoid scope leaks
    void visitNode(node n, loc currentFunction)
        = visitNode([n], currentFunction);
    void visitNode(list[node] n, loc currentFunction) {
        // We visit root first such that we have the correct function information for subnodes
        top-down visit(n) {
            // Functions
            case functionDecl(Annotation a, str name, int arity, list[Clause] clauses): {
                loc funcLoc = |erlang+function:///<currentModName>/<name>/<toString(arity)>|;
                loc physLoc = annoToLoc(fileLoc, a);
                
                model.declarations += {<funcLoc, physLoc>};
                model.containment += {<currentModule, funcLoc>};
                model.names += {<name, physLoc>};
                
                // If not already flagged public: is private
                if (<funcLoc, \public()> notin model.modifiers) {
                    model.modifiers += {<funcLoc, \private()>};
                }

                // Visit children
                for (Clause c <- clauses) {
                    visitNode(c, funcLoc);
                }

                // Don't re-visit clauses
                fail;
            }

            // Local funcall
            case e:Expression::call(Annotation a, Expression funExpr, list[Expression] args): {
                if (Expression::literal(atom(_, str funName)) := funExpr ||
                    Expression::var(_, str funName) := funExpr) {
                        
                    int arity = size(args);
                    loc callee = |erlang+function:///<currentModName>/<funName>/<toString(arity)>|;
                    loc physLoc = annoToLoc(fileLoc, a);
                    
                    model.uses += {<physLoc, callee>};
                    
                    if (currentFunction.scheme != "unknown") {
                        model.functionCalls += {<currentFunction, callee>};
                    }
                } else {
                    iprintln(e);
                    throw "Unexpected funExpr <funExpr> for call <e>";
                }
            }

            // Remote funcall
            case Expression::call(Annotation a, Expression modExpr, Expression funExpr, list[Expression] args): {
                if (Expression::literal(atom(_, str targetMod)) := modExpr, 
                    Expression::literal(atom(_, str funName)) := funExpr) {
                    
                    // Static call
                    int arity = size(args);
                    loc callee = |erlang+function:///<targetMod>/<funName>/<toString(arity)>|;
                    loc physLoc = annoToLoc(fileLoc, a);
                    
                    model.uses += {<physLoc, callee>};
                    if (currentFunction.scheme != "unknown") {
                        model.functionCalls += {<currentFunction, callee>};
                    }
                } else {
                    // Dynamic call (fun/mod as expression)
                    loc physLoc = annoToLoc(fileLoc, a);
                    model.uses += {<physLoc, |unresolved:///dynamic_call|>};
                }
            }

            // Variables
            case Pattern p: {
                if (Pattern::var(Annotation a, str name) := p && name != "_") {
                    loc varLoc = currentFunction[scheme="erlang+variable"][path="<currentFunction.path>/<name>"];
                    loc physLoc = annoToLoc(fileLoc, a);
                    model.declarations += {<varLoc, physLoc>};
                    model.containment += {<currentFunction, varLoc>};
                }
            }
            case Expression e: {
                if (Expression::var(Annotation a, str name) := e && name != "_") {
                    loc varLoc = currentFunction[scheme="erlang+variable"][path="<currentFunction.path>/<name>"];
                    loc physLoc = annoToLoc(fileLoc, a);
                    model.uses += {<physLoc, varLoc>};
                }
            }
        }
    }

    visitNode(ast, |unknown:///|);

    return model;
}
