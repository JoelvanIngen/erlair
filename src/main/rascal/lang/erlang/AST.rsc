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

// Top-level; covers: module attributes, exports, imports, function decls, record defs, type defs/specs
data Form
    = exportAttr(int line, list[tuple[str name, int arity]] exports)  // -export([Fun_1/A_1, ..., Fun_k/A_k])
    | importAttr(int line, str \module, list[tuple[str name, int arity]] imports)  // -import(Mod,[Fun_1/A_1, ..., Fun_k/A_k])
    | moduleAttr(int line, str name)  // -module(Mod)
    | fileAttr(int line, str path, int fileLine)  // -file(File,Line)
    | functionDecl(int line, str name, int arity, list[Clause] clauses)  // Name Fc_1 ; ... ; Name Fc_k
    | functionSpec(int line, str name, int arity, list[Type] signatures)  // -Spec Name Ft_1; ...; Ft_k
    | functionSpec(int line, str \module, str name, int arity, list[Type] signatures)  // -spec Mod:Name Ft_1; ...; Ft_k
    | callbackSpec(int line, str name, int arity, list[Type] signatures)
    | callbackSpec(int line, str \module, str name, int arity, list[Type] signatures)
    | recordDecl(int line, str name, list[RecordField] fields)
    | typeDecl(int line, Type \type, str name, list[Type] vars)
    | opaqueDecl(int line, Type \type, str name, list[Type] vars)
    | wildAttr(int line, str name, Expression \value)
    | error(int line, str \module, str description)
    | warning(int line, str \module, str description)
    | eof(int line)
    ;

// Record definitions
data RecordField
    = recordField(int line, Association association)
    | recordField(int line, Association association, Expression expr)
    // TODO: Maybe write this as two distinct recordField entries just to get rid of the invalid recursion if it's only 1 more line anyway?
    | typedRecordField(int line, RecordField field, Type \type)  // `field` can only be `recordField`, `typedRecordField` recursion is not allowed
    ;

// Constants
data Literal
    = atom(int line, str str_value)
    | char(int line, str str_value)
    | float(int line, str str_value)
    | integer(int line, int int_value)
    | string(int line, str str_value)
    ;

// Pattern matching/LHS
data Pattern
    = literal(Literal lit)
    | bitstring(int line, list[BinaryElement] binElements)
    | match(int line, Pattern lhs, Pattern rhs)  // lhs = rhs
    | cons(int line, Pattern head, Pattern tail)  // [h | t]
    | \map(int line, list[Association] associations)  // #{K: V}
    | nil(int line)  // []
    | op(int line, str operator, Pattern operand)  // Monop
    | op(int line, str operator, Pattern lhs, Pattern rhs)  // Binop
    | recordIndex(int line, str name, Pattern field)  // #Name.Field
    | record(int line, str name, list[RecordFieldPattern] fields)  // #Name{Field_1=P_1, ..., Field_k=P_k}
    | \tuple(int line, list[Pattern] elements)  // {P_1, ..., P_k}
    | var(int line, str name)  // Also includes universal pattern ('_')
    ;

// Anything evaluate-able
data Expression
    = literal(Literal lit)
    | bc(int line, Expression template, list[Qualifier] qualifiers)
    | bin(int line, list[BinaryElement] binElements)
    | block(int line, Body body)
    | \case(int line, Expression expr, list[Clause] clauses)
    | \catch(int line, Expression expr)
    | cons(int line, Expression head, Expression tail)  // [E_h | E_t]
    | funDecl(int line, str name, int arity)
    | funDecl(int line, str \module, str name, int arity)
    | fun(int line, list[Clause] clauses)
    | namedFun(int line, str name, list[Clause] clauses)
    | call(int line, Expression fun, list[Expression] args)
    | call(int line, str \module, Expression fun, list[Expression] args)  // Remote
    | \if(int line, list[Clause] clauses)
    | lc(int line, Expression expr, list[Qualifier] qualifiers)
    | mc(int line, Association association, list[Qualifier] qualifiers)
    | \map(int line, list[Association] associations)
    | mapUpdate(int line, Expression expr, list[Association] associations)
    | match(int line, Pattern pattern, Expression expr)
    | maybeMatch(int line, Pattern pattern, Expression expr)
    | maybe(int line, Body body)
    | maybe(int line, Body body, list[Clause] elseClauses)
    | nil(int line)  // []
    | op(int line, str operator, Expression lhs, Expression rhs)  // Binop, does not include '='
    | op(int line, str operator, Expression operand)  // Monop
    // Parentheses skipped as per source
    | receive(int line, list[Clause] clauses)
    | receive(int line, list[Clause] clauses, Expression timeoutExpr, Body timeoutBody)
    | record(int line, str name, list[RecordFieldExpr] fields)
    | recordField(int line, Expression expr, str name, Expression field)
    | recordIndex(int line, str name, Expression field)
    | recordUpdate(int line, Expression expr, str name, list[RecordFieldExpr] fields)
    | \tuple(int line, list[Expression] elements)
    | \try(int line, Body body, list[Clause] caseClauses, list[Clause] catchClauses, Body afterBody)  // caseClauses, catchClauses, afterBody can be '[]'
    | var(int line, str name)
    ;

// Generators?
data Qualifier
    = \filter(Expression expr)
    | zip(int line, list[Qualifier] generators)  // Q_1 && ...&& Q_k] with Q_i as a non-zip generator
    | generate(int line, Pattern pattern, Expression expr)  // P <- E
    | generateStrict(int line, Pattern pattern, Expression expr)  // P <:- E
    | bGenerate(int line, Pattern pattern, Expression expr)  // P <= E
    | bGenerateStrict(int line, Pattern pattern, Expression expr)  // P <:= E
    | mGenerate(int line, Association association, Expression expr)  // P <- E with P_1 := P_2
    | mGenerateStrict(int line, Association association, Expression expr)  // P <:- E with P_1 := P_2
    ;

// Bitstring segments
data BinaryElement
    ;

// Map pairs
data Association
    = mapFieldAssoc(int line, Expression key, Expression \value)
    | mapFieldExact(int line, Expression key, Expression \value)
    ;

// Function/case branches
data Clause
    // This one entry should actually cover all clause variants I think?
    // 'guards' is nullable
    = clause(int line, list[Pattern] patterns, GuardSeq guards, Body body)
    ;

// // 'when' logic
// data GuardTest
//     ;

// Types from -spec and -type
data Type
    = annType(int line, Type var, Type \type)  // A :: T_0
    | literal(Literal lit)
    | binary(int line, Type m, Type n)  // <<_:M,_:_*N>>
    | nil(int line)  // '[]'
    | fun(int line)
    | fun(int line, list[Type] args, Type returnType)
    | \any(int line)
    | range(int line, Type low, Type high)
    | \mapAny(int line)  // map/0
    | \map(int line, list[Type] associations)
    | op(int line, str operator, Type lhs, Type rhs)  // Binop
    | op(int line, str operator, Type operand)  // Monop
    | predefinedType(int line, str name, list[Type] args)
    | record(int line, list[Type] fields)
    | remoteType(int line, Type \module, str name, list[Type] args)
    | \tupleAny(int line)  // tuple/0
    | \tuple(int line, list[Type] elements)  // {T_1, ..., T_k}
    | union(int line, list[Type] types)  // T_1 | ... | T_k
    | var(int line, str name)
    | userType(int line, str name, list[Type] args)
    ;

// 'when' part of -spec
data TypeConstraint
    = constraint(int line, str kind, Type var, Type \type)
    ;

// Helper for bitstrings to prevent super long tuple
data BinaryElement
    = binElement(int line, Expression \value, Expression size, list[TypeSpec] typeSpecs)
    ;

// Helper for part after '/' in a binary
data TypeSpec
    = typeSpec(str name)  // Such as 'integer'
    | typeSpec(str name, int \value)  // Such as {unit, 8}
    ;

// Helper for fields used inside record patterns
data RecordFieldPattern
    = recordFieldPattern(int line, Pattern field, Pattern \value)
    ;

// Helper for fields used inside record expressions
data RecordFieldExpr
    = recordFieldExpr(int line, Expression field, Expression \value)
    ;
