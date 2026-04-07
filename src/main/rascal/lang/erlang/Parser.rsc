module lang::erlang::Parser

import List;
import lang::erlang::AST;
import lang::erlang::ErlangImporter;

list[str] LITERAL_TAGS = ["atom", "char", "float", "integer", "string"];

EAF parseErlangAST(str rawJSON) {
    rawAST = parseErlangJSON(rawJSON);
    return [ parseForm(f) | f <- rawAST ];
}

// TODO: Check with Damian if this construct is possible without the ugly type hack
&T unrecognised(type[&T] _, str t, value v) { throw "Unrecognised <t>: <v>"; }

// Extracting this in case annotations can be extended with columnn data in the future
int parseAnno(int line) = line;
default int parseAnno(value v) = unrecognised(#int, "annotation", v);

Form parseForm(["attribute", value annot, "export", list[list[value]] exports])
    = exportAttr(parseAnno(annot), [ <"<n>", i> | [str n, int i] <- exports ]);
Form parseForm(["attribute", value annot, "import", [str \module, list[list[value]] imports]])
    = importAttr(parseAnno(annot), \module, [ <"<n>", i> | [str n, int i] <- imports ]);
Form parseForm(["attribute", value annot, "module", str name])
    = moduleAttr(parseAnno(annot), name);
Form parseForm(["attribute", value annot, "file", [str path, int fileLine]])
    = fileAttr(parseAnno(annot), path, fileLine);
Form parseForm(["function", value annot, str name, int arity, list[value] clauses])
    = functionDecl(parseAnno(annot), name, arity, [parseClause(c) | c <- clauses]);
// functionSpec
// functionSpec(int line, str \module, str name, int arity, list[Type] signatures)  // -spec Mod:Name Ft_1; ...; Ft_k
// callbackSpec(int line, str name, int arity, list[Type] signatures)
// callbackSpec(int line, str \module, str name, int arity, list[Type] signatures)
// recordDecl(int line, str name, list[RecordField] fields)
// typeDecl(int line, Type \type, str name, list[Type] vars)
// opaqueDecl(int line, Type \type, str name, list[Type] vars)
// wildAttr(int line, str name, Expression \value)
// error(int line, str \module, str description)
// warning(int line, str \module, str description)
Form parseForm(["eof", value annot])
    = eof(parseAnno(annot));
default Form parseForm(value v) = unrecognised(#Form, "Form", v);

// recordField(int line, Association association)
// recordField(int line, Association association, Expression expr)
// typedRecordField(int line, RecordField field, Type \type)  // `field` can only be `recordField`, `typedRecordField` recursion is not allowed
default RecordField parseRecordField(value v) = unrecognised(#RecordField, "RecordField", v);

Literal parseLiteral(["atom", value annot, str v])
    = atom(parseAnno(annot), v);
Literal parseLiteral(["char", value annot, str v])
    = char(parseAnno(annot), v);
Literal parseLiteral(["float", value annot, str v])
    = float(parseAnno(annot), v);
Literal parseLiteral(["integer", value annot, int v])
    = integer(parseAnno(annot), v);
// Literal parseLiteral(["string", value annot, list[value] vs])
//     = string(parseAnno(annot), intercalate("", [parseLiteral(v) | v <- vs]));  // Parse chars
default Literal parseLiteral(value v) = unrecognised(#Literal, "Literal", v);

// literal(Literal lit)
// bitstring(int line, list[BinaryElement] binElements)
// match(int line, Pattern lhs, Pattern rhs)  // lhs = rhs
// cons(int line, Pattern head, Pattern tail)  // [h | t]
// \map(int line, list[Association] associations)  // #{K: V}
// nil(int line)  // []
// op(int line, str operator, Pattern operand)  // Monop
// op(int line, str operator, Pattern lhs, Pattern rhs)  // Binop
// recordIndex(int line, str name, Pattern field)  // #Name.Field
// record(int line, str name, list[RecordFieldPattern] fields)  // #Name{Field_1=P_1, ..., Field_k=P_k}
// \tuple(int line, list[Pattern] elements)  // {P_1, ..., P_k}
Pattern parsePattern(["var", value annot, str name])
    = Pattern::var(parseAnno(annot), name);
default Pattern parsePattern(value v) = unrecognised(#Pattern, "Pattern", v);

// literal(Literal lit)
Expression parseExpr([str t, value annot, value v])
    = Expression::literal(parseLiteral([t, annot, v]))
    when t in LITERAL_TAGS;
// bc(int line, Expression template, list[Qualifier] qualifiers)
// bin(int line, list[BinaryElement] binElements)
// block(int line, Body body)
// \case(int line, Expression expr, list[Clause] clauses)
// \catch(int line, Expression expr)
// cons(int line, Expression head, Expression tail)  // [E_h | E_t]
// funDecl(int line, str name, int arity)
// funDecl(int line, str \module, str name, int arity)
// fun(int line, list[Clause] clauses)
// namedFun(int line, str name, list[Clause] clauses)
// call(int line, Expression fun, list[Expression] args)
// call(int line, str \module, Expression fun, list[Expression] args)  // Remote
// \if(int line, list[Clause] clauses)
// lc(int line, Expression expr, list[Qualifier] qualifiers)
// mc(int line, Association association, list[Qualifier] qualifiers)
// \map(int line, list[Association] associations)
// mapUpdate(int line, Expression expr, list[Association] associations)
// match(int line, Pattern pattern, Expression expr)
// maybeMatch(int line, Pattern pattern, Expression expr)
// maybe(int line, Body body)
// maybe(int line, Body body, list[Clause] elseClauses)
// nil(int line)  // []
// op(int line, str operator, Expression lhs, Expression rhs)  // Binop, does not include '='
// op(int line, str operator, Expression operand)  // Monop
// receive(int line, list[Clause] clauses)
// receive(int line, list[Clause] clauses, Expression timeoutExpr, Body timeoutBody)
// record(int line, str name, list[RecordFieldExpr] fields)
// recordField(int line, Expression expr, str name, Expression field)
// recordIndex(int line, str name, Expression field)
// recordUpdate(int line, Expression expr, str name, list[RecordFieldExpr] fields)
// \tuple(int line, list[Expression] elements)
// \try(int line, Body body, list[Clause] caseClauses, list[Clause] catchClauses, Body afterBody)  // caseClauses, catchClauses, afterBody can be '[]'
Expression parseExpr(["var", value annot, str name])
    = Expression::var(parseAnno(annot), name);
default Expression parseExpr(value v) = unrecognised(#Expression, "Expression", v);

// \filter(Expression expr)
// zip(int line, list[Qualifier] generators)  // Q_1 && ...&& Q_k] with Q_i as a non-zip generator
// generate(int line, Pattern pattern, Expression expr)  // P <- E
// generateStrict(int line, Pattern pattern, Expression expr)  // P <:- E
// bGenerate(int line, Pattern pattern, Expression expr)  // P <= E
// bGenerateStrict(int line, Pattern pattern, Expression expr)  // P <:= E
// mGenerate(int line, Association association, Expression expr)  // P <- E with P_1 := P_2
// mGenerateStrict(int line, Association association, Expression expr)  // P <:- E with P_1 := P_2
default Qualifier parseQualifier(value v) = unrecognised(#Qualifier, "Qualifier", v);

// BinaryElement???

// mapFieldAssoc(int line, Expression key, Expression \value)
// mapFieldExact(int line, Expression key, Expression \value)
default Association parseAssociation(value v) = unrecognised(#Association, "Association", v);

Clause parseClause(["clause", value annot, list[value] patterns, list[list[value]] guards, list[value] body])
    = clause(
        parseAnno(annot),
        [parsePattern(p) | p <- patterns],
        [[parseExpr(g) | g <- guard] | guard <- guards],
        [parseExpr(e) | e <- body]
    );
default Clause parseClause(value v) = unrecognised(#Clause, "Clause", v);

// GuardTest???

// annType(int line, Type var, Type \type)  // A :: T_0
// literal(Literal lit)
// binary(int line, Type m, Type n)  // <<_:M,_:_*N>>
// nil(int line)  // '[]'
// fun(int line)
// fun(int line, list[Type] args, Type returnType)
// \any(int line)
// range(int line, Type low, Type high)
// \mapAny(int line)  // map/0
// \map(int line, list[Type] associations)
// op(int line, str operator, Type lhs, Type rhs)  // Binop
// op(int line, str operator, Type operand)  // Monop
// predefinedType(int line, str name, list[Type] args)
// record(int line, list[Type] fields)
// remoteType(int line, Type \module, str name, list[Type] args)
// \tupleAny(int line)  // tuple/0
// \tuple(int line, list[Type] elements)  // {T_1, ..., T_k}
// union(int line, list[Type] types)  // T_1 | ... | T_k
// var(int line, str name)
// userType(int line, str name, list[Type] args)
default Type parseType(value v) = unrecognised(#Type, "Type", v);

// constraint(int line, str kind, Type var, Type \type)
default TypeConstraint parseTypeConstraint(value v) = unrecognised(#TypeConstraint, "TypeConstraint", v);

// binElement(int line, Expression \value, Expression size, list[TypeSpec] typeSpecs)
default BinaryElement parseBinaryElement(value v) = unrecognised(#BinaryElement, "BinaryElement", v);

// typeSpec(str name)  // Such as 'integer'
// typeSpec(str name, int \value)  // Such as {unit, 8}
default TypeSpec parseTypeSpec(value v) = unrecognised(#TypeSpec, "TypeSpec", v);

// recordFieldPattern(int line, Pattern field, Pattern \value)
default RecordFieldPattern parseRecordFieldPattern(value v) = unrecognised(#RecordFieldPattern, "RecordFieldPattern", v);

// recordFieldExpr(int line, Expression field, Expression \value)
default RecordFieldExpr parseRecordFieldExpr(value v) = unrecognised(#RecordFieldExpr, "RecordFieldExpr", v);
