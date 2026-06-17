module lang::erlang::analysis::DeadClauseAnalyser

import List;
import String;
import lang::erlang::AST;

map[str, set[str]] SUBSUMES = (
    "is_number":    {"is_integer","integer","is_float","float","is_number","number"},
    "number":       {"is_integer","integer","is_float","float","is_number","number"},
    "is_bitstring": {"is_binary","binary","is_bitstring","bitstring"},
    "bitstring":    {"is_binary","binary","is_bitstring","bitstring"}
);

// Normalise source locations
private Annotation mockAnno = \anno(0, 0);
private &T stripAnnos(&T nodeValue)
    = visit(nodeValue) { case Annotation _ => mockAnno };

// Checks if two subtrees are structurally equivalent whilst ignoring source code locations
private bool equals(&T p1, &T p2)
    = stripAnnos(p1) == stripAnnos(p2);

// Traverses AST and reports dead clauses
set[Annotation] findDeadClauses(EAF ast) {
    set[Annotation] res = {};

    visit(ast) {
        case functionDecl(_, _, _, list[Clause] clauses): res += checkClauses(clauses);
        case \case(_, _, list[Clause] clauses): res += checkClauses(clauses);
        case receive(_, list[Clause] clauses): res += checkClauses(clauses);
        case receive(_, list[Clause] clauses, _, _): res += checkClauses(clauses);
        case \if(_, list[Clause] clauses): res += checkClauses(clauses);
        case fun(_, list[Clause] clauses): res += checkClauses(clauses);
        case namedFun(_, _, list[Clause] clauses): res += checkClauses(clauses);
        case maybe(_, _, _, list[Clause] clauses): res += checkClauses(clauses);
        case \try(_, _, list[Clause] caseClauses, list[Clause] catchClauses, _): {
            res += checkClauses(caseClauses);
            res += checkClauses(catchClauses);
        }
    }

    return res;
}

// Loops through clauses and identifies and returns any clause that is subsumed by any earlier clause
set[Annotation] checkClauses(list[Clause] clauses) {
    if (size(clauses) < 1) return {};
    return {
        clauses[i].\anno |
        i <- [1 .. size(clauses)],
        any(j <- [0 .. i], clauseSubsumes(clauses[j], clauses[i]))
    };
}

// Checks if clause c1 subsumes clause c2
bool clauseSubsumes(Clause c1, Clause c2) {
    if (!patternsSubsume(c1.patterns, c2.patterns)) return false;

    varMap = buildVarMapList(c1.patterns, c2.patterns);
    if (guardsSubsume(c1.guards, c2.guards, varMap)) return true;

    varToExprMap = buildVarToExprMapList(c1.patterns, c2.patterns);
    if (evaluateGuardWithExpr(c1.guards, varToExprMap)) return true;

    return false;
}

// Checks if list of patterns subsumes other list of patterns
// All need to subsume sequentially
bool patternsSubsume(list[Pattern] ps1, list[Pattern] ps2) {
    if (size(ps1) != size(ps2)) return false;
    if (size(ps1) == 0) return true;
    return all(i <- [0..size(ps1)], patternSubsumes(ps1[i], ps2[i]));
}

// Checks if pattern 1 subsumes pattern 2
bool patternSubsumes(Pattern p1, Pattern p2) {
    // Variable catches everything
    if (p1 is var) return true;
    
    if (p2 is match)
        return patternSubsumes(p1, p2.lhs) || patternSubsumes(p1, p2.rhs);
    
    if (p1 is match)
        return patternSubsumes(p1.lhs, p2) && patternSubsumes(p1.rhs, p2);
    
    if (p1 is literal, p2 is literal)
        return equals(p1.lit, p2.lit);
    
    if (p1 is \tuple, p2 is \tuple)
        return size(p1.elements) == size(p2.elements) && all(i <- [0..size(p1.elements)], patternSubsumes(p1.elements[i], p2.elements[i]));
    
    if (p1 is cons, p2 is cons)
        return patternSubsumes(p1.head, p2.head) && patternSubsumes(p1.tail, p2.tail);
    
    if (p1 is nil, p2 is nil) return true;

    if (p1 is bitstring, p2 is bitstring) {
        // `<<_/binary>>` or `<<Var/binary>>` matches any binary
        if (size(p1.binElements) == 1, 
            binElementPatt(_, Pattern::var(_, _), _, typeSpecs(list[TypeSpec] specs)) := p1.binElements[0],
            any(typeSpec(str n) <- specs, n == "binary" || n == "bytes")) {
            return true;
        }
        // Fallback to piecewise comparison
        if (size(p1.binElements) == size(p2.binElements)) {
            return all(i <- [0..size(p1.binElements)], 
                patternSubsumes(p1.binElements[i].\value, p2.binElements[i].\value));
        }
    }


    if (p1 is \map, p2 is \map) {
        if (p1.associations == []) return true;
        return all(
            a1 <- p1.associations,
            any(
                a2 <- p2.associations,
                equals(a1.key, a2.key) && exprSubsumes(a1.\value, a2.\value)
            )
        );
    }
    
    if (p1 is record, p2 is record) {
        if (p1.name != p2.name) return false;
        if (p1.fields == []) return true;
        return all(
            f1 <- p1.fields,
            any(
                f2 <- p2.fields,
                equals(f1.field, f2.field) && patternSubsumes(f1.\value, f2.\value)
            )
        );
    }

    // Nil vs Empty String Pattern
    if (p1 is nil, p2 is literal)
        return p2.lit is string && p2.lit.\value == "";
    if (p1 is literal, p2 is nil)
        return p1.lit is string && p1.lit.\value == "";
    
    return false;
}

// Checks whether expression 1 subsumes expression 2
bool exprSubsumes(Expression e1, Expression e2) {
    if (e1 is var) return true;
    
    if (e1 is literal, e2 is literal)
        return equals(e1.lit, e2.lit);
    
    if (e1 is \tuple, e2 is \tuple)
        return size(e1.elements) == size(e2.elements) && all(i <- [0..size(e1.elements)], exprSubsumes(e1.elements[i], e2.elements[i]));
    
    if (e1 is cons, e2 is cons)
        return exprSubsumes(e1.head, e2.head) && exprSubsumes(e1.tail, e2.tail);

    if (e1 is nil, e2 is nil) return true;

    // Nil vs Empty String Expression
    if (e1 is nil, e2 is literal)
        return e2.lit is string && e2.lit.\value == "";
    if (e1 is literal, e2 is nil)
        return e1.lit is string && e1.lit.\value == "";
    
    return false;
}

// Builds a variable renaming map between two lists of patterns
map[str, str] buildVarMapList(list[Pattern] ps1, list[Pattern] ps2) {
    map[str, str] m = ();
    if (size(ps1) != size(ps2)) return m;
    for (i <- [0..size(ps1)]) {
        m = buildVarMapPat(ps1[i], ps2[i], m);
    }
    return m;
}

// Traverses two patterns and records variable name mapping
map[str, str] buildVarMapPat(Pattern p1, Pattern p2, map[str, str] currentMap) {
    if (p1 is var, p2 is var) {
        currentMap[p2.name] = p1.name;
    } else if (p1 is \tuple, p2 is \tuple, size(p1.elements) == size(p2.elements)) {
        for (i <- [0..size(p1.elements)]) {
            currentMap = buildVarMapPat(p1.elements[i], p2.elements[i], currentMap);
        }
    } else if (p1 is cons, p2 is cons) {
        currentMap = buildVarMapPat(p1.head, p2.head, currentMap);
        currentMap = buildVarMapPat(p1.tail, p2.tail, currentMap);
    } else if (p1 is \map, p2 is \map) {
        for (a1 <- p1.associations, a2 <- p2.associations) {
            if (equals(a1.key, a2.key)) {
                currentMap = buildVarMapExpr(a1.\value, a2.\value, currentMap);
            }
        }
    } else if (p1 is match) {
        currentMap = buildVarMapPat(p1.lhs, p2, currentMap);
        currentMap = buildVarMapPat(p1.rhs, p2, currentMap);
    } else if (p2 is match) {
        currentMap = buildVarMapPat(p1, p2.lhs, currentMap);
        currentMap = buildVarMapPat(p1, p2.rhs, currentMap);
    }
    return currentMap;
}

// Traverses two expressions and records variable name mapping
map[str, str] buildVarMapExpr(Expression e1, Expression e2, map[str, str] currentMap) {
    if (e1 is var, e2 is var) {
        currentMap[e2.name] = e1.name;
    } else if (e1 is \tuple, e2 is \tuple, size(e1.elements) == size(e2.elements)) {
        for (i <- [0..size(e1.elements)]) {
            currentMap = buildVarMapExpr(e1.elements[i], e2.elements[i], currentMap);
        }
    } else if (e1 is cons, e2 is cons) {
        currentMap = buildVarMapExpr(e1.head, e2.head, currentMap);
        currentMap = buildVarMapExpr(e1.tail, e2.tail, currentMap);
    } else if (e1 is \map, e2 is \map) {
        for (a1 <- e1.associations, a2 <- e2.associations) {
            if (equals(a1.key, a2.key)) {
                currentMap = buildVarMapExpr(a1.\value, a2.\value, currentMap);
            }
        }
    }
    return currentMap;
}

// Checks if guard 1 subsumes guard 2
bool guardsSubsume(GuardSeq g1, GuardSeq g2, map[str, str] varMap) {
    if (g1 == []) return true;
    
    g2Renamed = renameGuardSeq(g2, varMap);
    if (stripAnnos(g1) == stripAnnos(g2Renamed)) return true;
    
    if (size(g1) == 1, size(g1[0]) == 1, size(g2Renamed) == 1, size(g2Renamed[0]) == 1) {
        Expression t1 = g1[0][0];
        Expression t2 = g2Renamed[0][0];
        if (guardTestSubsumes(t1, t2)) return true;
    }
    
    return false;
}

// Determines if guard expression t1 subsumes guard expression t2
bool guardTestSubsumes(Expression t1, Expression t2) {
    if (compareSubsumes(t1, t2)) return true;
    
    // Type-hierarchy tests on identical target variables
    if (call(_, literal(atom(_, str fn1)), [Expression v1]) := t1,
        call(_, literal(atom(_, str fn2)), [Expression v2]) := t2,
        stripAnnos(v1) == stripAnnos(v2)) {
        
        if (fn1 in SUBSUMES) return fn2 in SUBSUMES[fn1];
    }
    
    return false;
}

// Renames all variable occurrences within a guard sequence according to a rename map
GuardSeq renameGuardSeq(GuardSeq gs, map[str, str] varMap)
    = visit(gs) { case Expression::var(Annotation a, str name) => Expression::var(a, varMap[name] ? name) };

// Compares two relational operations to defermine subsumption
bool compareSubsumes(Expression t1, Expression t2) {
    if (op(Annotation _, str op1, Expression lhs1, Expression rhs1) := t1,
        op(Annotation _, str op2, Expression lhs2, Expression rhs2) := t2) {
        
        if (stripAnnos(lhs1) == stripAnnos(lhs2)) {
            if (literal(integer(_, str val1Str)) := rhs1,
                literal(integer(_, str val2Str)) := rhs2) {
                
                int v1 = toInt(val1Str);
                int v2 = toInt(val2Str);
                
                if (op1 == "\>=", op2 == "\>") return v1 <= v2;
                if (op1 == "\>=", op2 == "\>=") return v1 <= v2;
                if (op1 == "\>", op2 == "\>") return v1 <= v2;
                if (op1 == "\>", op2 == "\>=") return v1 < v2;
                
                if (op1 == "=\<", op2 == "\<") return v1 >= v2;
                if (op1 == "=\<", op2 == "=\<") return v1 >= v2;
                if (op1 == "\<", op2 == "\<") return v1 >= v2;
                if (op1 == "\<", op2 == "=\<") return v1 > v2;
            }
        }
    }
    return false;
}

// Creates a map linking variable names in preceding pattern list to Expression values in the next pattern list
map[str, Expression] buildVarToExprMapList(list[Pattern] ps1, list[Pattern] ps2) {
    map[str, Expression] m = ();
    if (size(ps1) != size(ps2)) return m;
    for (i <- [0..size(ps1)]) {
        m = buildVarToExprMapPat(ps1[i], ps2[i], m);
    }
    return m;
}

// Maps variables in a reference pattern to literals in target pattern
map[str, Expression] buildVarToExprMapPat(Pattern p1, Pattern p2, map[str, Expression] m) {
    if (p1 is var) {
        m[p1.name] = patternToExpr(p2);
    } else if (p1 is \tuple, p2 is \tuple, size(p1.elements) == size(p2.elements)) {
        for (i <- [0..size(p1.elements)]) {
            m = buildVarToExprMapPat(p1.elements[i], p2.elements[i], m);
        }
    } else if (p1 is cons, p2 is cons) {
        m = buildVarToExprMapPat(p1.head, p2.head, m);
        m = buildVarToExprMapPat(p1.tail, p2.tail, m);
    } else if (p1 is \map, p2 is \map) {
        for (a1 <- p1.associations, a2 <- p2.associations) {
            if (equals(a1.key, a2.key)) {
                m = buildVarToExprMapExpr(a1.\value, a2.\value, m);
            }
        }
    } else if (p1 is record, p2 is record) {
        if (p1.name == p2.name) {
            for (f1 <- p1.fields) {
                if (recordFieldPattern(_, Pattern field1, Pattern val1) := f1) {
                    for (f2 <- p2.fields) {
                        if (recordFieldPattern(_, Pattern field2, Pattern val2) := f2) {
                            if (equals(field1, field2)) {
                                m = buildVarToExprMapPat(val1, val2, m);
                            }
                        }
                    }
                }
            }
        }
    } else if (p1 is match) {
        m = buildVarToExprMapPat(p1.lhs, p2, m);
        m = buildVarToExprMapPat(p1.rhs, p2, m);
    } else if (p2 is match) {
        m = buildVarToExprMapPat(p1, p2.lhs, m);
        m = buildVarToExprMapPat(p1, p2.rhs, m);
    } else if (p1 is bitstring, p2 is bitstring) {
        if (size(p1.binElements) == size(p2.binElements)) {
            for (i <- [0..size(p1.binElements)]) {
                m = buildVarToExprMapPat(p1.binElements[i].\value, p2.binElements[i].\value, m);
            }
        }
    }
    return m;
}

map[str, Expression] buildVarToExprMapExpr(Expression e1, Expression e2, map[str, Expression] m) {
    if (e1 is var) {
        m[e1.name] = e2;
    } else if (e1 is \tuple, e2 is \tuple, size(e1.elements) == size(e2.elements)) {
        for (i <- [0..size(e1.elements)]) {
            m = buildVarToExprMapExpr(e1.elements[i], e2.elements[i], m);
        }
    } else if (e1 is cons, e2 is cons) {
        m = buildVarToExprMapExpr(e1.head, e2.head, m);
        m = buildVarToExprMapExpr(e1.tail, e2.tail, m);
    } else if (e1 is \map, e2 is \map) {
        for (a1 <- e1.associations, a2 <- e2.associations) {
            if (equals(a1.key, a2.key)) {
                m = buildVarToExprMapExpr(a1.\value, a2.\value, m);
            }
        }
    }
    return m;
}

// Helper to convert a Pattern node into its matching Expression representation
Expression patternToExpr(Pattern p) {
    switch (p) {
        case Pattern::literal(Literal lit): return Expression::literal(lit);
        case Pattern::nil(Annotation a): return Expression::nil(a);
        case Pattern::cons(Annotation a, Pattern h, Pattern t): 
            return Expression::cons(a, patternToExpr(h), patternToExpr(t));
        case Pattern::\tuple(Annotation a, list[Pattern] elements):
            return Expression::\tuple(a, [patternToExpr(e) | e <- elements]);
        case Pattern::\map(Annotation a, list[Association] associations):
            return Expression::\map(a, associations);
        case Pattern::bitstring(Annotation a, list[BinaryElementPattern] binElements):
            return Expression::bin(a, [binElementExpr(ae, patternToExpr(ve), s, t) | binElementPatt(ae, ve, s, t) <- binElements]);
        default:
            return Expression::var(\anno(0,0), "_");
    }
}

// Substitutes variables with Expression values for comparison
bool evaluateGuardWithExpr(GuardSeq g, map[str, Expression] varToExpr) {
    if (g == []) return true;
    
    gSubst = visit(g) {
        case Expression::var(_, str name): {
            if (name in varToExpr) {
                insert varToExpr[name];
            }
        }
    };
    
    return any(guard <- gSubst, all(\test <- guard, evaluateGuardTest(\test)));
}

// Evaluate built-in type guards on expresions
bool evaluateGuardTest(Expression \test) {
    if (call(_, literal(atom(_, str fnName)), [Expression arg]) := \test) {
        if (fnName == "is_integer" || fnName == "integer") return literal(integer(_, _)) := arg;
        if (fnName == "is_atom" || fnName == "atom") return literal(atom(_, _)) := arg;
        if (fnName == "is_float" || fnName == "float") return literal(float(_, _)) := arg;
        if (fnName == "is_number" || fnName == "number") return literal(integer(_, _)) := arg || literal(float(_, _)) := arg;
        if (fnName == "is_list" || fnName == "list") {
            if (nil(_) := arg || cons(_, _, _) := arg) return true;
            return Expression::literal(Literal lit) := arg && lit is string;
        }
        if (fnName == "is_binary" || fnName == "binary") return bin(_, _) := arg;
    }
    return false;
}
