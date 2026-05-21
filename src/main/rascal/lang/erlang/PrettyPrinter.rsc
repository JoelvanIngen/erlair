module lang::erlang::PrettyPrinter

import IO;
import List;
import String;
import lang::erlang::AST;

private int precAtomic   = 100;  // literals, variables, tuples, lists, maps, funs, etc
private int precUnary    = 90;
private int precMult     = 80;   // * / div rem band and
private int precAdd      = 70;   // + - bor bxor bsl bsr or xor
private int precListOp   = 60;   // ++ --
private int precComp     = 50;   // == /= =< < >= > =:= =/=
private int precAndAlso  = 40;
private int precOrElse   = 30;
private int precMatch    = 20;   // pattern = expr
private int precSend     = 15;   // !
private int precCatch    = 10;   // catch expression
private int precBlock    = 0;    // begin ... end, if, case, etc

private int patPrecAtomic = 100;
private int patPrecCons   = 90;
private int patPrecOp     = 80;
private int patPrecMatch  = 20; // For match pattern (lhs = rhs) inside pattern

// I keep making typos writing "intercalate" so I'm calling it j as short for join instead (join seems to be reserved)
str j(str sep, list[str] l)
    = intercalate(sep, l);

public void prettyPrintToFile(EAF ast, loc file) {
    writeFile(file, pPrint(ast) + "\n");
}

public str pPrint(EAF ast) {
    return j("\n\n", [pForm(f) | f <- ast]);
}

str pAtom(str a) {
    // Only quote if not simple lowercase name
    if (/^[a-z][a-zA-Z0-9_@]*$/ := a) return a;
    return "\'<a>\'";
}

str pNameArity(str name, int arity)
    = "<name>/<arity>";

str pFunSig(list[Type] signatures)
    = j("; ", [pSpecSig(t) | t <- signatures]);

str pSpecSig(boundedFun(_, Type \type, list[TypeConstraint] constraints))
    = "<pSpecSig(\type)> when <j(", ", [pTypeConstraint(c) | c <- constraints])>";
str pSpecSig(fun(_, args, ret))
    = "(<j(", ", [pType(a) | a <- args])>) -\> <pType(ret)>";
default str pSpecSig(Type t)
    = pType(t);

str pTypeParams(list[Type] vars)
    = "(" + j(", ", [pType(v) | v <- vars]) + ")";

str escapeString(str s, bool replaceNewlines=true) {
    s = replaceAll(s, "\\", "\\\\");
    s = replaceAll(s, "\"", "\\\"");
    if (replaceNewlines) s = replaceAll(s, "\n", "\\n");
    s = replaceAll(s, "\r", "\\r");
    s = replaceAll(s, "\t", "\\t");
    return s;
}

str escapeAtom(str s) {
    s = replaceAll(s, "\\", "\\\\");
    s = replaceAll(s, "\'", "\\\'");
    return s;
}

str pMultiLineStr(str s) {
    s = escapeString(s, replaceNewlines=false);
    s = replaceAll(s, "\n", "\"\n\"");
    return s;  // Corpus puts explicit verbatism \n after each line for some reason?
}

str pForm(exportAttr(_, list[tuple[str name, int arity]] exports))
    = "-export([<j(",", [pNameArity(n, a) | <n, a> <- exports])>]).";
str pForm(importAttr(_, str \module, list[tuple[str name, int arity]] imports))
    = "-import(<\module>, [<j(",", [pNameArity(n, a) | <n, a> <- imports])>]).";
str pForm(moduleAttr(_, str name))
    = "-module(<name>).";
str pForm(fileAttr(_, str path, int line))
    = "-file(\"<escapeString(path)>\", <line>).";
str pForm(functionDecl(_, str name, int arity, list[Clause] clauses))
    = "<j("; ", [showClause(name, c, true) | c <- clauses])>.";
str pForm(functionSpec(_, str name, int arity, list[Type] signatures))
    = "-spec <name><pFunSig(signatures)>.";
str pForm(functionSpec(_, str \module, str name, int arity, list[Type] signatures))
    = "-spec <\module>:<name><pFunSig(signatures)>.";
str pForm(callbackSpec(_, str name, int arity, list[Type] signatures))
    = "-callback <name><pFunSig(signatures)>.";
str pForm(recordDecl(_, str name, list[RecordField] fields))
    = "-record(<name>, {<j(", ", [pRecordField(f) | f <- fields])>}).";
str pForm(typeDecl(_, str name, Type \type, list[Type] vars))
    = "-type <name><pTypeParams(vars)> :: <pType(\type)>.";
str pForm(opaqueDecl(_, str name, Type \type, list[Type] vars))
    = "-opaque <name><pTypeParams(vars)> :: <pType(\type)>.";
str pForm(docAttr(_, str text))
    = "-doc \"<pMultiLineStr(text)>\".";
str pForm(commentAttr(_, str text))
    = "-comment \"<pMultiLineStr(text)>\".";
str pForm(wildAttr(_, str name, value \value))
    = "-<name>(<pWildAttrValue(\value)>).";
// Keeping errors/warnings as comments for demonstration purposes
str pForm(error(d))
    = "%% error: <d>";
str pForm(warning(d))
    = "%% warning: <d>";
str pForm(eof(_))
    = "";

str pRecordField(recordField(_, Expression field))
    = pExpr(field, 0);
str pRecordField(recordField(_, Expression field, Expression expr))
    = "<pExpr(field, 0)> = <pExpr(expr, 0)>";
str pRecordField(typedRecordField(_, Expression field, Type t))
    = "<pExpr(field, 0)> :: <pType(t)>";
str pRecordField(typedRecordField(_, Expression field, Expression expr, Type t))
    = "<pExpr(field, 0)> = <pExpr(expr, 0)> :: <pType(t)>";

str pLiteral(atom(_, str s))
    = pAtom(s);
str pLiteral(char(_, str s))
    = "<s>";
str pLiteral(float(_, real f))
    = "<f>";
str pLiteral(integer(_, int i))
    = "<i>";
str pLiteral(string(_, str s))
    = "\"<escapeString(s)>\"";

str pAtom(str s) {
    if (/^[a-z@][a-zA-Z0-9_@]*$/ !:= s && s != "" && !(s in {"","true","false","and","or","xor","not","catch","after","begin","end","fun","case","if","of","when","receive","try","maybe","else"})? true : false)
        return s;
    else
        return "\'" + escapeAtom(s) + "\'";
}

/* Pprints expression and adds parantheses when context precedence is higher than its own */
str pExpr(Expression e, int outerPrec) {
    thisPrec = exprPrec(e);
    str s = showExprNoParens(e);
    if (thisPrec < outerPrec || (thisPrec == outerPrec && needsParensSame(e, outerPrec)))
        return "(" + s + ")";
    else
        return s;
}

str pPattern(Pattern p, int outerPrec = 0) {
    thisPrec = patPrec(p);
    str s = showPatternNoParens(p);
    if (thisPrec < outerPrec)
        return "(" + s + ")";
    else
        return s;
}

/* Needs parentheses when left/right associativity matters */
/* Very simplified version for now */
bool needsParensSame(Expression e, int outerPrec) {
    return exprPrec(e) < precAtomic;
}

int exprPrec(literal)  = precAtomic;
int exprPrec(bc)       = precAtomic;
int exprPrec(bin)      = precAtomic;
int exprPrec(block)    = precAtomic;
int exprPrec(\case)    = precAtomic;
int exprPrec(\catch) = precCatch;
int exprPrec(cons)     = precAtomic;
int exprPrec(funDecl) = precAtomic;
int exprPrec(fun)      = precAtomic;
int exprPrec(namedFun) = precAtomic;
int exprPrec(call)     = precAtomic;
int exprPrec(\if)      = precAtomic;
int exprPrec(lc)       = precAtomic;
int exprPrec(mc)       = precAtomic;
int exprPrec(\map)     = precAtomic;
int exprPrec(mapUpdate) = precAtomic;
int exprPrec(match)    = precMatch;
int exprPrec(maybeMatch) = precMatch;
int exprPrec(maybe)    = precAtomic;
int exprPrec(nil)      = precAtomic;
int exprPrec(op(_, str operator, _, _)) = opPrec(operator);
int exprPrec(op(_, str operator, _)) = opPrec(operator);
int exprPrec(receive)  = precAtomic;
int exprPrec(record)   = precAtomic;
int exprPrec(recordField) = precAtomic;
int exprPrec(recordIndex) = precAtomic;
int exprPrec(recordUpdate) = precAtomic;
int exprPrec(\tuple)   = precAtomic;
int exprPrec(\try)     = precAtomic;
int exprPrec(var)      = precAtomic;
// Helper to avoid writing this all twice
int opPrec(str operator) {
    if (operator == "!") return precSend;
    else if (operator == "==" || operator == "/=" || operator == "=<" || operator == "<" || operator == ">=" || operator == "\>" || operator == "=:=" || operator == "=/=") return precComp;
    else if (operator == "++" || operator == "--") return precListOp;
    else if (operator == "+" || operator == "-" || operator == "bor" || operator == "bxor" || operator == "bsl" || operator == "bsr" || operator == "or" || operator == "xor") return precAdd;
    else if (operator == "*" || operator == "/" || operator == "div" || operator == "rem" || operator == "band" || operator == "and") return precMult;
    else if (operator == "not" || operator == "bnot") return precUnary;
    else if (operator == "+" || operator == "-") return precUnary;  // Unary +?
    else if (operator == "andelse") return precOrElse;
    else if (operator == "andalso") return precAndAlso;
    throw "wtf";
}
default int exprPrec(_)= precAtomic;  // Safe

str showExprNoParens(Expression::literal(l)) = pLiteral(l);
str showExprNoParens(Expression::var(_, name)) = name;
str showExprNoParens(nil(_)) = "[]";
str showExprNoParens(Expression::cons(_, head, tail)) = pList(head, tail);
str showExprNoParens(Expression::\tuple(_, els)) = "{" + j(", ", [pExpr(ex, 0) | ex <- els]) + "}";
str showExprNoParens(bin(_, elems)) = "\<\<" + j(", ", [pBinaryElementExpr(ex) | ex <- elems]) + "\>\>";
str showExprNoParens(Expression::\map(_, assocs)) = "#{" + j(", ", [pAssociation(a) | a <- assocs]) + "}";
str showExprNoParens(mapUpdate(_, m, assocs)) = "<pExpr(m, precAtomic)>#{" + j(", ", [pAssociation(a) | a <- assocs]) + "}";
str showExprNoParens(Expression::record(_, name, fields)) = "#<name>{" + j(", ", [pRecordFieldExpr(f) | f <- fields]) + "}";
str showExprNoParens(Expression::recordIndex(_, name, field)) = "#<name>.<pExpr(field, 0)>";
str showExprNoParens(recordField(_, expr, name, field)) = "<pExpr(expr, precAtomic)>#<name>.<pExpr(field, 0)>";
str showExprNoParens(recordUpdate(_, expr, name, fields)) = "<pExpr(expr, precAtomic)>#<name>{" + j(", ", [pRecordFieldExpr(f) | f <- fields]) + "}";
str showExprNoParens(funDecl(_, name, arity)) = "fun <name>/<arity>";
str showExprNoParens(funDecl(_, \module, name, arity)) = "fun <pExpr(\module, 0)>:<pExpr(name, 0)>/<pExpr(arity, 0)>";
str showExprNoParens(fun(_, clauses)) = "fun <j("; ", [showClause("", c, false) | c <- clauses])> end";
str showExprNoParens(namedFun(_, name, clauses)) = "fun <name> <j("; ", [showClause("", c, false) | c <- clauses])> end";
str showExprNoParens(call(_, f, args)) = "<pExpr(f,precAtomic)>(<j(", ", [pExpr(a,0) | a <- args])>)";
str showExprNoParens(call(_, \module, f, args)) = "<pExpr(\module, precAtomic)>:<pExpr(f, precAtomic)>(<j(", ", [pExpr(a, 0) | a <- args])>)";
str showExprNoParens(bc(_, template, quals)) = "\<\< <pExpr(template, 0)> || <j(", ", [pQualifier(q) | q <- quals])> \>\>";
str showExprNoParens(lc(_, expr, quals)) = "[ <pExpr(expr, 0)> || <j(", ", [pQualifier(q) | q <- quals])> ]";
str showExprNoParens(mc(_, \assoc, quals)) = "#{ <pAssociation(\assoc)> || <j(", ", [pQualifier(q) | q <- quals])> }";
str showExprNoParens(block(_, body)) = "begin\n" + showBody(body) + "\nend";
str showExprNoParens(\case(_, expr, clauses)) = "case <pExpr(expr,0)> of\n" + j(";\n", [showClause("", c, false) | c <- clauses]) + "\nend";
str showExprNoParens(\catch(_, expr)) = "catch <pExpr(expr, precCatch-1)>";
str showExprNoParens(\if(_, clauses)) = "if\n" + j(";\n", [showClause("", c, false, isIf=true) | c <- clauses]) + "\nend";
str showExprNoParens(receive(_, clauses)) = "receive\n" + j(";\n", [showClause("", c, false) | c <- clauses]) + "\nend";
str showExprNoParens(receive(_, clauses, timeoutExpr, timeoutBody)) =
    "receive\n" + j(";\n", [showClause("", c, false) | c <- clauses]) + "\nafter <pExpr(timeoutExpr,0)> -\>\n" + showBody(timeoutBody) + "\nend";
str showExprNoParens(\try(_, body, caseClauses, catchClauses, afterBody)) =
    "try\n" + showBody(body) +
    (caseClauses != [] ? " of\n" + j(";\n", [showClause("", c, false) | c <- caseClauses]) : "") +
    (catchClauses != [] ? " catch\n" + j(";\n", [showClause("", c, false) | c <- catchClauses]) : "") +
    (afterBody != [] ? " after\n" + showBody(afterBody) : "") +
    "\nend";
str showExprNoParens(Expression::match(_, pat, rhs)) = "<pPattern(pat)> = <pExpr(rhs, precMatch-1)>";
str showExprNoParens(maybeMatch(_, pat, rhs)) = "<pPattern(pat)> ?= <pExpr(rhs, precMatch-1)>";
str showExprNoParens(maybe(_, body)) = "maybe\n" + showBody(body) + "\nend";
str showExprNoParens(maybe(_, body, _, elseCls)) = "maybe\n" + showBody(body) + "\nelse\n" + j(";\n", [showClause("", c, false) | c <- elseCls]) + "\nend";
str showExprNoParens(e:Expression::op(_, operator, lhs, rhs)) =  // Binary
    "<pExpr(lhs, exprPrec(e))> <operator> <pExpr(rhs, exprPrec(e))>"
    when operator != "=";
str showExprNoParens(Expression::op(_, operator, operand)) =  // Unary
    operator == "+" || operator == "-" ? "<operator><pExpr(operand, precUnary-1)>" : "<operator> <pExpr(operand, precUnary-1)>";
default str showExprNoParens(Expression e) {
    throw "PrettyPrinter: unexpected Expression <e>";
}

str pList(Expression head, Expression tail) {
    list[str] elems = [pExpr(head, 0)];
    Expression current = tail;
    while (cons(_, h, t) := current) {
        elems += pExpr(h, 0);
        current = t;
    }
    if (nil(_) := current) {
        return "[" + j(", ", elems) + "]";
    }
    return "[" + j(", ", elems) + " | " + pExpr(current, 0) + "]";
}

str showBody(Body b) {
    if (b == []) return "";
    return j(",\n", [pExpr(ex, 0) | ex <- b]);
}

int patPrec(literal) = patPrecAtomic;
int patPrec(bitstring) = patPrecAtomic;
int patPrec(match) = patPrecMatch;
int patPrec(cons) = patPrecCons;
int patPrec(\map) = patPrecAtomic;
int patPrec(nil) = patPrecAtomic;
int patPrec(op) = patPrecOp;
int patPrec(recordIndex) = patPrecAtomic;
int patPrec(record) = patPrecAtomic;
int patPrec(\tuple) = patPrecAtomic;
int patPrec(var) = patPrecAtomic;
default int patPrec(_) = patPrecAtomic;

str showPatternNoParens(Pattern::literal(l)) = pLiteral(l);
str showPatternNoParens(bitstring(_, elements)) = "\<\<" + j(", ", [pBinaryElementPattern(e) | e <- elements]) + "\>\>";
str showPatternNoParens(Pattern::match(_, lhs, rhs)) = "<pPattern(lhs, outerPrec=patPrecMatch-1)> = <pPattern(rhs, outerPrec=patPrecMatch-1)>";
str showPatternNoParens(Pattern::cons(_, h, t)) = "[<pPattern(h)> | <pPattern(t)>]";
str showPatternNoParens(Pattern::\map(_, assocs)) = "#{" + j(", ", [pPatternAssociation(a) | a <- assocs]) + "}";
str showPatternNoParens(nil(_)) = "[]";
str showPatternNoParens(Pattern::op(_, operator, operand)) = "<operator> <pPattern(operand, outerPrec=patPrecOp-1)>";
str showPatternNoParens(Pattern::op(_, operator, lhs, rhs)) = "<pPattern(lhs, outerPrec=patPrecOp-1)> <operator> <pPattern(rhs, outerPrec=patPrecOp-1)>" when operator != "=";
str showPatternNoParens(Pattern::recordIndex(_, name, field)) = "#<name>.<pPattern(field)>";
str showPatternNoParens(Pattern::record(_, name, fields)) = "#<name>{" + j(", ", [pRecordFieldPattern(f) | f <- fields]) + "}";
str showPatternNoParens(Pattern::\tuple(_, elements)) = "{" + j(", ", [pPattern(e) | e <- elements]) + "}";
str showPatternNoParens(Pattern::var(_, name)) = name;

str pPatternAssociation(Association a) = pAssociation(a);  // Same syntax

str pRecordFieldPattern(recordFieldPattern(_, field, val)) = "<pPattern(field)> = <pPattern(val)>";

str pQualifier(\filter(expr)) = pExpr(expr, 0);
str pQualifier(zip(_, gens)) = j(" && ", [pQualifier(g) | g <- gens]);
str pQualifier(generate(_, pat, expr)) = "<pPattern(pat)> \<- <pExpr(expr, 0)>";
str pQualifier(generateStrict(_, pat, e)) = "<pPattern(pat)> \<:- <pExpr(e, 0)>";
str pQualifier(bGenerate(_, pat, e)) = "<pPattern(pat)> \<= <pExpr(e, 0)>";
str pQualifier(bGenerateStrict(_, pat, e)) = "<pPattern(pat)> \<:= <pExpr(e, 0)>";
str pQualifier(mGenerate(_, \assoc, expr)) = "<pAssociation(\assoc)> \<- <pExpr(expr, 0)>";
str pQualifier(mGenerateStrict(_, \assoc, e)) = "<pAssociation(\assoc)> \<:- <pExpr(e, 0)>";

str pAssociation(mapFieldAssoc(_, k, v)) = "<pExpr(k, 0)> =\> <pExpr(v, 0)>";
str pAssociation(mapFieldExact(_, k, v)) = "<pExpr(k, 0)> := <pExpr(v, 0)>";

str showClause(str funcName, Clause clause, bool isFunDecl, bool isIf = false) {
    patterns = clause.patterns;
    guards = clause.guards;
    body = clause.body;
    patStr = j(", ", [pPattern(p) | p <- patterns]);
    guardStr = showGuards(guards);
    bodyStr = showBody(body);

    if (isIf) {
        // Remove 'when' prefix
        str ifCond = j("; ", [j(", ", [pExpr(g, 0) | g <- guard]) | guard <- guards]);
        return "<ifCond> -\> <bodyStr>";
    }

    if (isFunDecl) return "<funcName>(<patStr>)<guardStr> -\> <bodyStr>";
    else return "<patStr><guardStr> -\> <bodyStr>";
}

str showGuards(GuardSeq guards) {
    if (guards == []) return "";
    guardParts = [j(", ", [pExpr(g, 0) | g <- guard]) | guard <- guards];
    return " when " + j("; ", guardParts);
}

str pType(annType(_, var, tp)) = "<pType(var)> :: <pType(tp)>";
str pType(boundedFun(_, Type \type, list[TypeConstraint] constraints))
    = "<pType(\type)> when <j(", ", [pTypeConstraint(c) | c <- constraints])>";
str pType(Type::literal(l)) = pLiteral(l);
str pType(binary(_, m, n)) {
    bool mZero = (Type::literal(integer(_, 0)) := m);
    bool nZero = (Type::literal(integer(_, 0)) := n);
    if (mZero && nZero) return "\<\<\>\>";
    if (nZero) return "\<\<_:<pType(m)>\>\>";
    if (mZero) return "\<\<_:_*<pType(n)>\>\>";
    return "\<\<_:<pType(m)>, _:_*<pType(n)>\>\>";
}
str pType(nil(_)) = "[]";
str pType(fun(_)) = "fun()";
str pType(fun(_, args, ret)) = "fun((<j(", ", [pType(a) | a <- args])>) -\> <pType(ret)>)";
str pType(\any(_)) = "any()";
str pType(range(_, low, high)) = "<pType(low)>..<pType(high)>";
str pType(\mapAny(_)) = "map()";
str pType(Type::\map(_, assocs)) = "#{" + j(", ", [pType(a) | a <- assocs]) + "}";
str pType(Type::op(_, operator, lhs, rhs)) = "<pType(lhs)> <operator> <pType(rhs)>";
str pType(Type::op(_, operator, operand)) = "<operator> <pType(operand)>";
str pType(predefinedType(_, "map_field_assoc", [k, v])) = "<pType(k)> =\> <pType(v)>";
str pType(predefinedType(_, "map_field_exact", [k, v])) = "<pType(k)> := <pType(v)>";
str pType(predefinedType(_, name, args)) = "<name>(" + j(", ", [pType(a) | a <- args]) + ")";
str pType(record(_, [Type::literal(atom(_, str recName)), *fields])) = "#<recName>{" + j(", ", [pType(f) | f <- fields]) + "}";
str pType(remoteType(_, \mod, name, args)) = "<pType(\mod)>:<pType(name)>(" + j(", ", [pType(a) | a <- args]) + ")";
str pType(\tupleAny(_)) = "tuple()";
str pType(Type::\tuple(_, elems)) = "{" + j(", ", [pType(elem) | elem <- elems]) + "}";
str pType(union(_, types)) = j(" | ", [pType(tp) | tp <- types]);
str pType(Type::var(_, name)) = name;
str pType(userType(_, name, args)) = "<name>(" + j(", ", [pType(a) | a <- args]) + ")";

str pTypeConstraint(constraint(_, str kind, Type var, Type \type)) {
    // is_subtype: internal name for the '::' operator in specs
    str op = (kind == "is_subtype") ? "::" : kind;
    return "<pType(var)> <op> <pType(\type)>";
}

str pBinaryElementPattern(binElementPatt(_, val, size, tspecs)) = "<pPattern(val)><pOptSize(size)><pOptTypeSpecs(tspecs)>";

str pBinaryElementExpr(binElementExpr(_, val, size, tspecs)) = "<pExpr(val, 0)><pOptSize(size)><pOptTypeSpecs(tspecs)>";

str pOptSize(defaultSize()) = "";
str pOptSize(size(expr)) = ":" + pExpr(expr, 0);

str pOptTypeSpecs(defaultTypeSpecs()) = "";
str pOptTypeSpecs(typeSpecs(specs)) = "/" + j("-", [pTypeSpec(s) | s <- specs]);

str pTypeSpec(typeSpec(name)) = name;
str pTypeSpec(typeSpec(name, val)) = "<name>-<val>";

str pRecordFieldExpr(recordFieldExpr(_, field, val)) = "<pExpr(field, 0)> = <pExpr(val, 0)>";

// Print raw json for now
str pWildAttrValue(str s) = pAtom(s);
str pWildAttrValue(int i) = "<i>";
str pWildAttrValue(real r) = "<r>";
str pWildAttrValue(list[value] l) {
    // [Name, Arity] handling
    if ([str n, int a] := l) return "<n>/<a>";
    return "[" + j(", ", [pWildAttrValue(i) | i <- l]) + "]";
}
str pWildAttrValue(value v) = "<v>";
