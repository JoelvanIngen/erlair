module lang::erlang::Parser

import lang::erlang::AST;
import lang::erlang::ErlangImporter;

list[str] LITERAL_TAGS = ["atom", "char", "float", "integer", "string"];

EAF parseErlangAST(str rawJSON) {
    rawAST = parseErlangJSON(rawJSON);
    return [ parseForm(f) | f <- rawAST ];
}

// TODO: Check with Damian if this construct is possible without the ugly type hack
&T unrecognised(type[&T] t, value v) { throw "Unrecognised <t>: <v>"; }

// Extracting this in case annotations can be extended with columnn data in the future
Annotation parseAnno(int line) = \anno(line);
Annotation parseAnno([["generated", "true"], ["location", int line]]) = \anno(line, true);  // Separate functions for generated:true/false to avoid str->bool conv boilerplate
default Annotation parseAnno(value v) = unrecognised(#Annotation, v);

Form parseForm(["attribute", value \anno, "export", list[list[value]] exports])
    = exportAttr(parseAnno(\anno), [ <"<n>", i> | [str n, int i] <- exports ]);
Form parseForm(["attribute", value \anno, "import", [str \module, list[list[value]] imports]])
    = importAttr(parseAnno(\anno), \module, [ <"<n>", i> | [str n, int i] <- imports ]);
Form parseForm(["attribute", value \anno, "module", str name])
    = moduleAttr(parseAnno(\anno), name);
Form parseForm(["attribute", value \anno, "file", [str path, int fileLine]])
    = fileAttr(parseAnno(\anno), path, fileLine);
Form parseForm(["function", value \anno, str name, int arity, list[value] clauses])
    = functionDecl(parseAnno(\anno), name, arity, [parseClause(c) | c <- clauses]);
Form parseForm(["attribute", value \anno, "spec", [str name, int arity], list[list[value]] signatures])
    = functionSpec(parseAnno(\anno), name, arity, [parseType(s) | s <- signatures]);
Form parseForm(["attribute", value \anno, "spec", [str \module, str name, int arity], list[list[value]] signatures])
    = functionSpec(parseAnno(\anno), \module, name, arity, [parseType(s) | s <- signatures]);
Form parseForm(["attribute", value \anno, "callback", [str name, int arity], list[list[value]] signatures])
    = callbackSpec(parseAnno(\anno), name, arity, [parseType(s) | s <- signatures]);
Form parseForm(["attribute", value \anno, "record", [str name, list[list[value]] fields]])
    = recordDecl(parseAnno(\anno), name, [parseRecordField(f) | f <- fields]);
Form parseForm(["attribute", value \anno, "type", [str name, list[value] \type, list[list[value]] vars]])
    = typeDecl(parseAnno(\anno), name, parseType(\type), [parseType(v) | v <- vars]);
Form parseForm(["attribute", value \anno, "opaque", [str name, list[value] \type, list[list[value]] vars]])
    = typeDecl(parseAnno(\anno), name, parseType(\type), [parseType(v) | v <- vars]);
Form parseForm(["attribute", value \anno, str name, value \value])
    = wildAttr(parseAnno(\anno), name, \value);
Form parseForm(["error", value description])
    = error(description);
Form parseForm(["warning", value description])
    = warning(description);
Form parseForm(["eof", value \anno])
    = eof(parseAnno(\anno));
default Form parseForm(value v) = unrecognised(#Form, v);

RecordField parseRecordField(["record_field", value \anno, list[value] field])
    = recordField(parseAnno(\anno), parseExpr(field));
RecordField parseRecordField(["record_field", value \anno, list[value] field, list[value] expr])
    = recordField(parseAnno(\anno), parseExpr(field), parseExpr(expr));
RecordField parseRecordField(["typed_record_field", ["record_field", value \anno, list[value] field], list[value] \type])
    = typedRecordField(parseAnno(\anno), parseExpr(field), parseType(\type));
RecordField parseRecordField(["typed_record_field", ["record_field", value \anno, list[value] field, list[value] expr], list[value] \type])
    = typedRecordField(parseAnno(\anno), parseExpr(field), parseExpr(expr), parseType(\type));
default RecordField parseRecordField(value v) = unrecognised(#RecordField, v);

Literal parseLiteral(["atom", value \anno, str v])
    = atom(parseAnno(\anno), v);
Literal parseLiteral(["char", value \anno, str v])
    = char(parseAnno(\anno), v);
Literal parseLiteral(["float", value \anno, real v])
    = float(parseAnno(\anno), v);
Literal parseLiteral(["integer", value \anno, int v])
    = integer(parseAnno(\anno), v);
Literal parseLiteral(["string", value \anno, str v])
    = string(parseAnno(\anno), v);
Literal parseLiteral(["string", value \anno, []])
    = string(parseAnno(\anno), "");
default Literal parseLiteral(value v) = unrecognised(#Literal, v);

Pattern parsePattern([str t, value \anno, value v])
    = Pattern::literal(parseLiteral([t, \anno, v]))
    when t in LITERAL_TAGS;
Pattern parsePattern(["bin", value \anno, list[list[value]] binElements])
    = bitstring(parseAnno(\anno), [parseBinaryElementPattern(e) | e <- binElements]);
Pattern parsePattern(["match", value \anno, list[value] lhs, list[value] rhs])
    = Pattern::match(parseAnno(\anno), parsePattern(lhs), parsePattern(rhs));
Pattern parsePattern(["cons", value \anno, list[value] head, list[value] tail])
    = cons(parseAnno(\anno), parsePattern(head), parsePattern(tail));
Pattern parsePattern(["map", value \anno, list[list[value]] associations])
    = Pattern::\map(parseAnno(\anno), [parseAssociation(a) | a <- associations]);
Pattern parsePattern(["nil", value \anno])
    = Pattern::nil(parseAnno(\anno));
Pattern parsePattern(["op", value \anno, str operator, list[value] operand])
    = Pattern::op(parseAnno(\anno), operator, parsePattern(operand));
Pattern parsePattern(["op", value \anno, str operator, list[value] lhs, list[value] rhs])
    = Pattern::op(parseAnno(\anno), operator, parsePattern(lhs), parsePattern(rhs));
Pattern parsePattern(["record_index", value \anno, str name, list[value] field])
    = recordIndex(parseAnno(\anno), name, parsePattern(field));
Pattern parsePattern(["record", value \anno, str name, list[list[value]] fields])
    = record(parseAnno(\anno), name, [parseRecordFieldPattern(f) | f <- fields]);
Pattern parsePattern(["tuple", value \anno, list[list[value]] elements])
    = \tuple(parseAnno(\anno), [parsePattern(e) | e <- elements]);
Pattern parsePattern(["var", value \anno, str name])
    = Pattern::var(parseAnno(\anno), name);
default Pattern parsePattern(value v) = unrecognised(#Pattern, v);

Expression parseExpr([str t, value \anno, value v])
    = Expression::literal(parseLiteral([t, \anno, v]))
    when t in LITERAL_TAGS;
Expression parseExpr(["bc", value \anno, list[value] template, list[list[value]] qualifiers])
    = bc(parseAnno(\anno), parseExpr(template), [parseQualifier(q) | q <- qualifiers]);
Expression parseExpr(["bin", value \anno, list[list[value]] elements])
    = bin(parseAnno(\anno), [parseBinaryElementExpr(e) | e <- elements]);
Expression parseExpr(["block", value \anno, list[list[value]] body])
    = block(parseAnno(\anno), [parseExpr(e) | e <- body]);
Expression parseExpr(["case", value \anno, list[value] expr, list[list[value]] clauses])
    = \case(parseAnno(\anno), parseExpr(expr), [parseClause(c) | c <- clauses]);
Expression parseExpr(["catch", value \anno, list[value] expr])
    = \catch(parseAnno(\anno), parseExpr(expr));
Expression parseExpr(["cons", value \anno, list[value] head, list[value] tail])
    = cons(parseAnno(\anno), parseExpr(head), parseExpr(tail));
Expression parseExpr(["fun", value \anno, ["function", str name, int arity]])
    = funDecl(parseAnno(\anno), name, arity);
Expression parseExpr(["fun", value \anno, ["function", list[value] \module, list[value] name, list[value] arity]])
    = funDecl(parseAnno(\anno), parseExpr(\module), parseExpr(name), parseExpr(arity));
Expression parseExpr(["fun", value \anno, ["clauses", list[list[value]] clauses]])
    = fun(parseAnno(\anno), [parseClause(c) | c <- clauses]);
Expression parseExpr(["named_fun", value \anno, str name, list[list[value]] clauses])
    = namedFun(parseAnno(\anno), name, [parseClause(c) | c <- clauses]);
Expression parseExpr(["call", value \anno, list[value] fun, list[list[value]] args])
    = call(parseAnno(\anno), parseExpr(fun), [parseExpr(a) | a <- args]);
Expression parseExpr(["remote", value \anno, list[value] \module, list[value] fun, list[list[value]] args])
    = call(parseAnno(\anno), parseExpr(\module), parseExpr(fun), [parseExpr(a) | a <- args]);
Expression parseExpr(["remote", value \anno, list[value] \module, list[value] fun])
    = call(parseAnno(\anno), parseExpr(\module), parseExpr(fun), []);
Expression parseExpr(["if", value \anno, list[list[value]] clauses])
    = \if(parseAnno(\anno), [parseClause(c) | c <- clauses]);
Expression parseExpr(["lc", value \anno, list[value] expr, list[list[value]] qualifiers])
    = lc(parseAnno(\anno), parseExpr(expr), [parseQualifier(q) | q <- qualifiers]);
Expression parseExpr(["mc", value \anno, list[value] association, list[list[value]] qualifiers])
    = mc(parseAnno(\anno), parseAssociation(association), [parseQualifier(q) | q <- qualifiers]);
Expression parseExpr(["map", value \anno, list[list[value]] associations])
    = Expression::\map(parseAnno(\anno), [parseAssociation(a) | a <- associations]);
Expression parseExpr(["map", value \anno, list[value] expr, list[list[value]] associations])
    = mapUpdate(parseAnno(\anno), parseExpr(expr), [parseAssociation(a) | a <- associations]);
Expression parseExpr(["match", value \anno, list[value] pattern, list[value] expr])
    = match(parseAnno(\anno), parsePattern(pattern), parseExpr(expr));
Expression parseExpr(["maybe_match", value \anno, list[value] pattern, list[value] expr])
    = maybeMatch(parseAnno(\anno), parsePattern(pattern), parseExpr(expr));
Expression parseExpr(["maybe", value \anno, list[list[value]] body])
    = maybe(parseAnno(\anno), [parseExpr(e) | e <- body]);
Expression parseExpr(["maybe", value \anno, list[list[value]] body, ["else", value elseAnno, list[list[value]] elseClauses]])
    = maybe(parseAnno(\anno), [parseExpr(e) | e <- body], parseAnno(elseAnno), [parseClause(c) | c <- elseClauses]);
Expression parseExpr(["nil", value \anno])
    = Expression::nil(parseAnno(\anno));
Expression parseExpr(["op", value \anno, str operator, list[value] lhs, list[value] rhs])
    = op(parseAnno(\anno), operator, parseExpr(lhs), parseExpr(rhs))
    when operator != "=";  // Might be redundant but including for now just in case
Expression parseExpr(["op", value \anno, str operator, list[value] operand])
    = op(parseAnno(\anno), operator, parseExpr(operand));
Expression parseExpr(["receive", value \anno, list[list[value]] clauses])
    = receive(parseAnno(\anno), [parseClause(c) | c <- clauses]);
Expression parseExpr(["receive", value \anno, list[list[value]] clauses, list[value] timeoutExpr, list[list[value]] timeoutBody])
    = receive(parseAnno(\anno), [parseClause(c) | c <- clauses], parseExpr(timeoutExpr), [parseExpr(e) | e <- timeoutBody]);
Expression parseExpr(["record", value \anno, str name, list[list[value]] fields])
    = record(parseAnno(\anno), name, [parseRecordFieldExpr(f) | f <- fields]);
Expression parseExpr(["record_field", value \anno, list[value] expr, str name, list[value] field])
    = recordField(parseAnno(\anno), parseExpr(expr), name, parseExpr(field));
Expression parseExpr(["record_index", value \anno, str name, list[value] field])
    = recordIndex(parseAnno(\anno), name, parseExpr(field));
Expression parseExpr(["record", value \anno, list[value] expr, str name, list[list[value]] fields])
    = recordUpdate(parseAnno(\anno), parseExpr(expr), name, [parseRecordFieldExpr(f) | f <- fields]);
Expression parseExpr(["tuple", value \anno, list[list[value]] elements])
    = \tuple(parseAnno(\anno), [parseExpr(e) | e <- elements]);
Expression parseExpr(["try", value \anno, list[list[value]] body, list[list[value]] caseClauses, list[list[value]] catchClauses, list[list[value]] afterBody])
    = \try(parseAnno(\anno), [parseExpr(e) | e <- body], [parseClause(c) | c <- caseClauses], [parseClause(c) | c <- catchClauses], [parseExpr(e) | e <- afterBody]);
Expression parseExpr(["var", value \anno, str name])
    = Expression::var(parseAnno(\anno), name);
default Expression parseExpr(value v) = unrecognised(#Expression, v);

Qualifier parseQualifier(["zip", value \anno, list[list[value]] generators])
    = zip(parseAnno(\anno), [parseQualifier(q) | q <- generators]);
Qualifier parseQualifier(["generate", value \anno, list[value] pattern, list[value] expr])
    = generate(parseAnno(\anno), parsePattern(pattern), parseExpr(expr));
Qualifier parseQualifier(["generate_strict", value \anno, list[value] pattern, list[value] expr])
    = generateStrict(parseAnno(\anno), parsePattern(pattern), parseExpr(expr));
Qualifier parseQualifier(["b_generate", value \anno, list[value] pattern, list[value] expr])
    = bGenerate(parseAnno(\anno), parsePattern(pattern), parseExpr(expr));
Qualifier parseQualifier(["b_generate_strict", value \anno, list[value] pattern, list[value] expr])
    = bGenerateStrict(parseAnno(\anno), parsePattern(pattern), parseExpr(expr));
Qualifier parseQualifier(["m_generate", value \anno, list[value] association, list[value] expr])
    = mGenerate(parseAnno(\anno), parseAssociation(association), parseExpr(expr));
Qualifier parseQualifier(["m_generate_strict", value \anno, list[value] association, list[value] expr])
    = mGenerateStrict(parseAnno(\anno), parseAssociation(association), parseExpr(expr));
default Qualifier parseQualifier(value v) {
    try {
        return \filter(parseExpr(v));
    } catch _: {
        return unrecognised(#Qualifier, v);
    }
}

Association parseAssociation(["map_field_assoc", value \anno, list[value] key, list[value] \value])
    = mapFieldAssoc(parseAnno(\anno), parseExpr(key), parseExpr(\value));
Association parseAssociation(["map_field_exact", value \anno, list[value] key, list[value] \value])
    = mapFieldExact(parseAnno(\anno), parseExpr(key), parseExpr(\value));
default Association parseAssociation(value v) = unrecognised(#Association, v);

Clause parseClause(["clause", value \anno, list[value] patterns, list[list[value]] guards, list[value] body])
    = clause(parseAnno(\anno), [parsePattern(p) | p <- patterns], [[parseExpr(g) | g <- guard] | guard <- guards], [parseExpr(e) | e <- body]);
default Clause parseClause(value v) = unrecognised(#Clause, v);

// GuardTest???

Type parseType(["ann_type", value \anno, [list[value] var, list[value] \type]])
    = annType(parseAnno(\anno), parseType(var), parseType(\type));
Type parseType([str t, value \anno, value v])
    = Type::literal(parseLiteral([t, \anno, v]))
    when t in LITERAL_TAGS;
Type parseType(["type", value \anno, "binary", [list[value] m, list[value] n]])
    = binary(parseAnno(\anno), parseType(m), parseType(n));
Type parseType(["type", value \anno, "nil", []])
    = Type::nil(parseAnno(\anno));
Type parseType(["type", value \anno, "fun", []])
    = fun(parseAnno(\anno));
Type parseType(["type", value \anno, "fun", [["type", value anyAnno, "any"], list[value] returnType]])  // fun(...)
    = fun(parseAnno(\anno), [\any(parseAnno(anyAnno))], parseType(returnType));
Type parseType(["type", value \anno, "fun", [["type", value prodAnno, "product", list[list[value]] args], list[value] returnType]])
    = fun(parseAnno(\anno),[parseType(a) | a <- args], parseType(returnType));
Type parseType(["type", value \anno, "any", []])
    = \any(parseAnno(\anno));
Type parseType(["type", value \anno, "range", [list[value] low, list[value] high]])
    = range(parseAnno(\anno), parseType(low), parseType(high));
Type parseType(["type", value \anno, "map", "any"])
    = \mapAny(parseAnno(\anno));
Type parseType(["type", value \anno, "map", list[list[value]] associations])
    = Type::\map(parseAnno(\anno), [parseType(a) | a <- associations]);
Type parseType(["op", value \anno, str operator, list[value] lhs, list[value] rhs])
    = Type::op(parseAnno(\anno), operator, parseType(lhs), parseType(rhs));
Type parseType(["op", value \anno, str operator, list[value] operand])
    = Type::op(parseAnno(\anno), operator, parseType(operand));
// predefinedType moved to bottom
Type parseType(["type", value \anno, "record", list[list[value]] fields])
    = Type::record(parseAnno(\anno),[parseType(f) | f <- fields]);
Type parseType(["remote_type", value \anno, [list[value] \module, list[value] name, list[list[value]] args]])
    = remoteType(parseAnno(\anno), parseType(\module), parseType(name), [parseType(a) | a <- args]);
Type parseType(["type", value \anno, "tuple", "any"])
    = \tupleAny(parseAnno(\anno));
Type parseType(["type", value \anno, "tuple", list[list[value]] elements])
    = Type::\tuple(parseAnno(\anno), [parseType(e) | e <- elements]);
Type parseType(["type", value \anno, "union", list[list[value]] types])
    = union(parseAnno(\anno), [parseType(t) | t <- types]);
Type parseType(["var", value \anno, str name])
    = Type::var(parseAnno(\anno), name);
Type parseType(["user_type", value \anno, str name, list[list[value]] args])
    = userType(parseAnno(\anno), name, [parseType(a) | a <- args]);
Type parseType(["type", value \anno, str name, list[list[value]] args])  // Bottom so it doesn't take priority over anything else
    = predefinedType(parseAnno(\anno), name, [parseType(a) | a <- args]);
default Type parseType(value v) = unrecognised(#Type, v);

TypeConstraint parseTypeConstraint(["type", value \anno, "constraint", [["atom", value subtypeAnno, str kind], [list[value] var, list[value] \type]]])
    = constraint(parseAnno(\anno), kind, parseType(var), parseType(\type));
default TypeConstraint parseTypeConstraint(value v) = unrecognised(#TypeConstraint, v);

BinaryElementPattern parseBinaryElementPattern(["bin_element", value \anno, value expr, value size, value typeSpecs])
    = binElementPatt(parseAnno(\anno), parsePattern(expr), parseOptSize(size), parseOptTypeSpecs(typeSpecs));
default BinaryElementPattern parseBinaryElementPattern(value v) = unrecognised(#BinaryElementPattern, v);

BinaryElementExpr parseBinaryElementExpr(["bin_element", value \anno, value expr, value size, value typeSpecs])
    = binElementExpr(parseAnno(\anno), parseExpr(expr), parseOptSize(size), parseOptTypeSpecs(typeSpecs));
default BinaryElementExpr parseBinaryElementExpr(value v) = unrecognised(#BinaryElementExpr, v);

OptSize parseOptSize("default")
    = defaultSize();
default OptSize parseOptSize(value v)
    = size(parseExpr(v));
// No error throwing here, will happen in parseExpr

OptTypeSpecs parseOptTypeSpecs("default")
    = defaultTypeSpecs();
OptTypeSpecs parseOptTypeSpecs(list[value] specs)
    = typeSpecs([parseTypeSpec(s) | s <- specs]);
default OptTypeSpecs parseOptTypeSpecs(value v) = unrecognised(#OptTypeSpecs, v);

TypeSpec parseTypeSpec(str name)
    = typeSpec(name);
// typeSpec(str name, int \value)  // Such as {unit, 8}
default TypeSpec parseTypeSpec(value v) = unrecognised(#TypeSpec, v);

RecordFieldPattern parseRecordFieldPattern(["record_field", value \anno, list[value] field, list[value] \value])
    = recordFieldPattern(parseAnno(\anno), parsePattern(field), parsePattern(\value));
default RecordFieldPattern parseRecordFieldPattern(value v) = unrecognised(#RecordFieldPattern, v);

RecordFieldExpr parseRecordFieldExpr(["record_field", value \anno, list[value] field, list[value] \value])
    = recordFieldExpr(parseAnno(\anno), parseExpr(field), parseExpr(\value));
default RecordFieldExpr parseRecordFieldExpr(value v) = unrecognised(#RecordFieldExpr, v);
