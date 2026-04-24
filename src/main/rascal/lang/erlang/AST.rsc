/**
 * This file follows roughly the same order as documented at https://www.erlang.org/doc/apps/erts/absform.html
 * and aims to be an exhaustive implementation of this source
 */

module lang::erlang::AST

alias EAF = list[Form];

alias Body = list[Expression];
alias GuardTest = Expression;  // Might be bad design choice, but they seem very similar and creating separate data structure might be very redundant
alias Guard = list[GuardTest];  // AND (comma)
alias GuardSeq = list[Guard];  // OR (semicolon)

// Locations in code
data Annotation
    = \anno(int line, int column, bool generated = false)
    ;

// Top-level; covers: module attributes, exports, imports, function decls, record defs, type defs/specs
data Form
    = exportAttr(Annotation \anno, list[tuple[str name, int arity]] exports)  // -export([Fun_1/A_1, ..., Fun_k/A_k])
    | importAttr(Annotation \anno, str \module, list[tuple[str name, int arity]] imports)  // -import(Mod,[Fun_1/A_1, ..., Fun_k/A_k])
    | moduleAttr(Annotation \anno, str name)  // -module(Mod)
    | fileAttr(Annotation \anno, str path, int fileLine)  // -file(File,Line)
    | functionDecl(Annotation \anno, str name, int arity, list[Clause] clauses)  // Name Fc_1 ; ... ; Name Fc_k
    | functionSpec(Annotation \anno, str name, int arity, list[Type] signatures)  // -Spec Name Ft_1; ...; Ft_k
    | functionSpec(Annotation \anno, str \module, str name, int arity, list[Type] signatures)  // -spec Mod:Name Ft_1; ...; Ft_k
    | callbackSpec(Annotation \anno, str name, int arity, list[Type] signatures)
    | recordDecl(Annotation \anno, str name, list[RecordField] fields)
    | typeDecl(Annotation \anno, str name, Type \type, list[Type] vars)
    | opaqueDecl(Annotation \anno, str name, Type \type, list[Type] vars)
    | wildAttr(Annotation \anno, str name, value \value)
    | error(value description)
    | warning(value description)
    | eof(Annotation \anno)
    ;

// Record definitions
data RecordField
    = recordField(Annotation \anno, Expression field)
    | recordField(Annotation \anno, Expression field, Expression expr)
    | typedRecordField(Annotation \anno, Expression field, Type \type)
    | typedRecordField(Annotation \anno, Expression field, Expression expr, Type \type)
    ;

// Constants
data Literal
    = atom(Annotation \anno, str sValue)
    | char(Annotation \anno, str sValue)
    | float(Annotation \anno, real fValue)
    | integer(Annotation \anno, int iValue)
    | string(Annotation \anno, str sValue)
    ;

// Pattern matching/LHS
data Pattern
    = literal(Literal lit)
    | bitstring(Annotation \anno, list[BinaryElementPattern] binElements)
    | match(Annotation \anno, Pattern lhs, Pattern rhs)  // lhs = rhs
    | cons(Annotation \anno, Pattern head, Pattern tail)  // [h | t]
    | \map(Annotation \anno, list[Association] associations)  // #{K: V}
    | nil(Annotation \anno)  // []
    | op(Annotation \anno, str operator, Pattern operand)  // Monop
    | op(Annotation \anno, str operator, Pattern lhs, Pattern rhs)  // Binop
    | recordIndex(Annotation \anno, str name, Pattern field)  // #Name.Field
    | record(Annotation \anno, str name, list[RecordFieldPattern] fields)  // #Name{Field_1=P_1, ..., Field_k=P_k}
    | \tuple(Annotation \anno, list[Pattern] elements)  // {P_1, ..., P_k}
    | var(Annotation \anno, str name)  // Also includes universal pattern ('_')
    ;

// Anything evaluate-able
data Expression
    = literal(Literal lit)
    | bc(Annotation \anno, Expression template, list[Qualifier] qualifiers)
    | bin(Annotation \anno, list[BinaryElementExpr] binElements)
    | block(Annotation \anno, Body body)
    | \case(Annotation \anno, Expression expr, list[Clause] clauses)
    | \catch(Annotation \anno, Expression expr)
    | cons(Annotation \anno, Expression head, Expression tail)  // [E_h | E_t]
    | funDecl(Annotation \anno, str name, int iArity)
    | funDecl(Annotation \anno, Expression \module, Expression eName, Expression eArity)
    | fun(Annotation \anno, list[Clause] clauses)
    | namedFun(Annotation \anno, str name, list[Clause] clauses)
    | call(Annotation \anno, Expression fun, list[Expression] args)
    | call(Annotation \anno, Expression \module, Expression fun, list[Expression] args)  // Remote
    | \if(Annotation \anno, list[Clause] clauses)
    | lc(Annotation \anno, Expression expr, list[Qualifier] qualifiers)
    | mc(Annotation \anno, Association association, list[Qualifier] qualifiers)
    | \map(Annotation \anno, list[Association] associations)
    | mapUpdate(Annotation \anno, Expression expr, list[Association] associations)
    | match(Annotation \anno, Pattern pattern, Expression expr)
    | maybeMatch(Annotation \anno, Pattern pattern, Expression expr)
    | maybe(Annotation \anno, Body body)
    | maybe(Annotation \anno, Body body, Annotation elseAnno, list[Clause] elseClauses)
    | nil(Annotation \anno)  // []
    | op(Annotation \anno, str operator, Expression lhs, Expression rhs)  // Binop, does not include '='
    | op(Annotation \anno, str operator, Expression operand)  // Monop
    // Parentheses skipped as per source
    | receive(Annotation \anno, list[Clause] clauses)
    | receive(Annotation \anno, list[Clause] clauses, Expression timeoutExpr, Body timeoutBody)
    | record(Annotation \anno, str name, list[RecordFieldExpr] fields)
    | recordField(Annotation \anno, Expression expr, str name, Expression field)
    | recordIndex(Annotation \anno, str name, Expression field)
    | recordUpdate(Annotation \anno, Expression expr, str name, list[RecordFieldExpr] fields)
    | \tuple(Annotation \anno, list[Expression] elements)
    | \try(Annotation \anno, Body body, list[Clause] caseClauses, list[Clause] catchClauses, Body afterBody)  // caseClauses, catchClauses, afterBody can be '[]'
    | var(Annotation \anno, str name)
    ;

// Generators?
data Qualifier
    = \filter(Expression expr)
    | zip(Annotation \anno, list[Qualifier] generators)  // Q_1 && ...&& Q_k] with Q_i as a non-zip generator (uneven brackets copied from docs)
    | generate(Annotation \anno, Pattern pattern, Expression expr)  // P <- E
    | generateStrict(Annotation \anno, Pattern pattern, Expression expr)  // P <:- E
    | bGenerate(Annotation \anno, Pattern pattern, Expression expr)  // P <= E
    | bGenerateStrict(Annotation \anno, Pattern pattern, Expression expr)  // P <:= E
    | mGenerate(Annotation \anno, Association association, Expression expr)  // P <- E with P_1 := P_2
    | mGenerateStrict(Annotation \anno, Association association, Expression expr)  // P <:- E with P_1 := P_2
    ;

// Map pairs
data Association
    = mapFieldAssoc(Annotation \anno, Expression key, Expression \value)
    | mapFieldExact(Annotation \anno, Expression key, Expression \value)
    ;

// Function/case branches
data Clause
    // This one entry should actually cover all clause variants I think?
    // 'guards' is nullable
    = clause(Annotation \anno, list[Pattern] patterns, GuardSeq guards, Body body)
    ;

// // 'when' logic
// data GuardTest
//     ;

// Types from -spec and -type
data Type
    = annType(Annotation \anno, Type var, Type \type)  // A :: T_0
    | literal(Literal lit)
    | binary(Annotation \anno, Type m, Type n)  // <<_:M,_:_*N>>
    | nil(Annotation \anno)  // '[]'
    | fun(Annotation \anno)
    | fun(Annotation \anno, list[Type] args, Type returnType)
    | \any(Annotation \anno)
    | range(Annotation \anno, Type low, Type high)
    | \mapAny(Annotation \anno)  // map/0
    | \map(Annotation \anno, list[Type] associations)
    | op(Annotation \anno, str operator, Type lhs, Type rhs)  // Binop
    | op(Annotation \anno, str operator, Type operand)  // Monop
    | predefinedType(Annotation \anno, str sName, list[Type] args)
    | record(Annotation \anno, list[Type] fields)
    | remoteType(Annotation \anno, Type \module, Type name, list[Type] args)
    | \tupleAny(Annotation \anno)  // tuple/0
    | \tuple(Annotation \anno, list[Type] elements)  // {T_1, ..., T_k}
    | union(Annotation \anno, list[Type] types)  // T_1 | ... | T_k
    | var(Annotation \anno, str sName)
    | userType(Annotation \anno, str sName, list[Type] args)
    ;

// 'when' part of -spec
data TypeConstraint
    = constraint(Annotation \anno, str kind, Type var, Type \type)
    ;

// Helpers for bitstrings to prevent super long tuple
data BinaryElementPattern
    = binElementPatt(Annotation \anno, Pattern \value, OptSize size, OptTypeSpecs typeSpecs)
    ;

data BinaryElementExpr
    = binElementExpr(Annotation \anno, Expression \value, OptSize size, OptTypeSpecs typeSpecs)
    ;

// Helpers for BinaryElement
data OptSize
    = defaultSize()
    | size(Expression expr)
    ;

data OptTypeSpecs
    = defaultTypeSpecs()
    | typeSpecs(list[TypeSpec] specs)
    ;

// Helper for part after '/' in a binary
data TypeSpec
    = typeSpec(str name)  // Such as 'integer'
    | typeSpec(str name, int \value)  // Such as {unit, 8}
    ;

// Helper for fields used inside record patterns
data RecordFieldPattern
    = recordFieldPattern(Annotation \anno, Pattern field, Pattern \value)
    ;

// Helper for fields used inside record expressions
data RecordFieldExpr
    = recordFieldExpr(Annotation \anno, Expression field, Expression \value)
    ;
