%{

%}

%union {
	struct AstNode *astNode;
	char *stringVal;
}

// OPERATORS PRECEDENCE BELOW

%left ','
//next two lines of precedence 14
%right '=' OP_ADD_ASSIGN OP_SUB_ASSIGN OP_MUL_ASSIGN OP_DIV_ASSIGN OP_MOD_ASSIGN
%right OP_R_SHIFT_ASSIGN OP_L_SHIFT_ASSIGN OP_AND_ASSIGN OP_OR_ASSIGN OP_XOR_ASSIGN
//TODO: add 13: ternary conditional
%left OP_OR
%left OP_AND
%left '|'
%left '^'
%left '&'
%left OP_EQ_TO OP_NEQ_TO
%left OP_GE_THAN OP_LE_THAN '>' '<'
%left OP_R_SHIFT OP_L_SHIFT
%left '+' '-'
%left '*' '/' '%'
//TODO: dereference
%right '!' '~'
//TODO: postfix and prefix inc/dec precedence
%left OP_INC OP_DEC '.' OP_PTR_ACCESS



%start TranslationUnit

%token IDENTIFIER TYPE_NAME INT_LITERAL FLOAT_LITERAL CHAR_LITERAL STRING_LITERAL
// built-in types
%token CHAR DOUBLE FLOAT INT LONG SHORT VOID BOOL COMPLEX IMAGINARY
// data storage type modifiers
%token AUTO CONST EXTERN INLINE REGISTER RESTRICT SIGNED STATIC UNSIGNED VOLATILE
// keywords
%token ASM BREAK CASE CONTINUE DEFAULT DO ELSE ENUM FOR GOTO IF RETURN SIZEOF STRUCT SWITCH TYPEDEF UNION WHILE ELLIPSIS

%type <stringVal> String
%type <astNode> Constant UnaryExpression PosfixExpression PrimaryExpression Expression AssignmentExpression OptionalExpression
%type <astNode> Statement CompountStatement LabeledStatement ExpressionStatement SelectionStatement IterationStatement JumpStatement

%%

StorageClassSpecifier : AUTO
	| REGISTER
	| STATIC
	| EXTERN

Constant : INT_LITERAL
	| CHAR_LITERAL
	| FLOAT_LITERAL
	| Enumerator

PrimaryExpression : IDENTIFIER
	| Constant
	| STRING_LITERAL
	| '(' Expression ')'

PostfixExpression : PrimaryExpression
	| PostfixExpression '[' Expression ']'
	| PostfixExpression '(' ')'
	| PostfixExpression '(' Expression ')'
	| PostfixExpression '.' IDENTIFIER
	| PostfixExpression OP_PTR_ACCESS IDENTIFIER
	| PostfixExpression OP_INC
	| PostfixExpression OP_DEC

UnaryOperator : '&'
	| '*'
	| '+'
	| '-'
	| '~'
	| '!'

UnaryExpression : PostfixExpression
    	| INC_OP UnaryExpression
    	| DEC_OP UnaryExpression
    	| UnaryOperator CastExpression
    	| SIZEOF UnaryExpression
    	| SIZEOF '(' TypeName ')'

CastExpression : UnaryExpression
    	| '(' TypeName ')' CastExpression

MultiplicativeExpression : CastExpression
	| MultiplicativeExpression '*' CastExpression
	| MultiplicativeExpression '/' CastExpression
	| MultiplicativeExpression '%' CastExpression

AdditiveExpression : MultiplicativeExpression
    	| AdditiveExpression '+' MultiplicativeExpression
    	| AdditiveExpression '-' MultiplicativeExpression

ShiftExpression : AdditiveExpression
   	| ShiftExpression OP_L_SHIFT AdditiveExpression
   	| ShiftExpression OP_R_SHIFT AdditiveExpression

RelationalExpression: ShiftExpression
    	| RelationalExpression '<' ShiftExpression
    	| RelationalExpression '>' ShiftExpression
    	| RelationalExpression OP_LE_THAN ShiftExpression
    	| RelationalExpression OP_GE_THAN ShiftExpression

EqualityExpression : RelationalExpression
    	| EqualityExpression OP_EQ_TO RelationalExpression
    	| EqualityExpression OP_NEQ_TO RelationalExpression

AndExpression : EqualityExpression
    	| AndExpression '&' EqualityExpression

ExclusiveOrExpression : AndExpression
    	| ExclusiveOrExpression '^' AndExpression

InclusiveOrExpression : ExclusiveOrExpression
    	| InclusiveOrExpression '|' ExclusiveOrExpression

LogicalAndExpression : InclusiveOrExpression
    	| LogicalAndExpression AND_OP InclusiveOrExpression

LogicalOrExpression : LogicalAndExpression
    	| LogicalOrExpression OP_OR LogicalAndExpression

ConditionalExpression : LogicalOrExpression
    	| LogicalOrExpression '?' Expression : ConditionalExpression

AssignmentOperator : '='
    	| MUL_ASSIGN
    	| DIV_ASSIGN
    	| MOD_ASSIGN
    	| ADD_ASSIGN
    	| SUB_ASSIGN
    	| LEFT_ASSIGN
    	| RIGHT_ASSIGN
    	| AND_ASSIGN
    	| XOR_ASSIGN
    	| OR_ASSIGN

AssignmentExpression : ConditionalExpression
	| UnaryExpression AssignmentOperator AssignmentExpression { $$ = $2 }

Expression : AssignmentExpression
	| Expression ',' AssignmentExpression

ArgumentExpressionList : AssignmentExpression
    	| ArgumentExpressionList ',' AssignmentExpression

Enumerator : IDENTIFIER
    	| IDENTIFIER '=' Constant

EnumeratorList : Enumerator
    	| EnumeratorList ',' Enumerator

EnumSpecifier : ENUM IDENTIFIER
    	| ENUM '{' EnumeratorList '}'
    	| ENUM IDENTIFIER '{' EnumeratorList '}'

Initializer : AssignmentExpression
    	| '{' InitializerList '}'
    	| '{' InitializerList ',' '}'

InitializerList : Initializer
    	| InitializerList ',' Initializer

IdentifierList : IDENTIFIER
    	| IdentifierList ',' IDENTIFIER

TypeQualifier : CONST | VOLATILE

TypeQualifierList : TypeQualifier
	| TypeQualifierList TypeQualifier

StructOrUnion : STRUCT | UNION

StructOrUnionSpecifier : StructOrUnion IDENTIFIER
	| StructOrUnion '{' StructDeclarationList '}'
	| StructOrUnion IDENTIFIER '{' StructDeclarationList '}'

TypeSpecifier : VOID
	| CHAR
	| SHORT
	| INT
	| LONG
	| FLOAT
	| DOUBLE
	| SIGNED
	| UNSIGNED
	| StructOrUnionSpecifier
	| EnumSpecifier
	| TYPE_NAME

SpecifierQualifier : TypeSpecifier
	| TypeQualifier String

SpecifierQualifierList : SpecifierQualifier
	| SpecifierQualifier

Pointer : '*'
   	| '*' Pointer
   	| '*' TypeQualifierList
   	| '*' TypeQualifierList Pointer

Declarator : Pointer DirectDeclarator
    	| DirectDeclarator

DirectDeclarator : IDENTIFIER
    	| '(' Declarator ')'
    	| DirectDeclarator '(' ')'
    	| DirectDeclarator '[' ']'
    	| DirectDeclarator '(' ParameterTypeList ')'
    	| DirectDeclarator '(' IdentifierList ')'
    	| DirectDeclarator '[' Constant ']'

DirectAbstractDeclarator : '[' ']'
   	| '[' Constant ']'
   	| '(' ')'
   	| '(' ParameterTypeList ')'
   	| '(' AbstractDeclarator ')'
   	| DirectAbstractDeclarator '[' ']'
   	| DirectAbstractDeclarator '[' Constant ']'
   	| DirectAbstractDeclarator '(' ')'
   	| DirectAbstractDeclarator '(' ParameterTypeList ')'

AbstractDeclarator : Pointer
    	| DirectAbstractDeclarator
    	| Pointer DirectAbstractDeclarator

InitDeclarator : Declarator
	| Declarator '=' Initializer

InitDeclaratorList : InitDeclarator
	| InitDeclaratorList ',' InitDeclarator

DeclarationSpecifiers : StorageClassSpecifier
	| TypeSpecifier
	| TypeQualifier
	| StorageClassSpecifier DeclarationSpecifiers
	| TypeSpecifier DeclarationSpecifiers
	| TypeQualifier DeclarationSpecifiers

Declaration : DeclarationSpecifiers ';'
	| DeclarationSpecifiers InitDeclaratorList ';'

DeclarationList : Declaration
	| DeclarationList Declaration

StructDeclarator : Declarator
	| Declarator ':' Constant
	| ':' Constant

StructDeclaration : SpecifierQualifierList StructDeclaratorList ';'

StructDeclarationList : StructDeclaration
	| StructDeclarationList StructDeclaration

NonEmptyStructDeclarationList : StructDeclarator
	| NonEmptyStructDeclarationList StructDeclarator

ParameterDeclaration : DeclarationSpecifiers Declarator
    	| DeclarationSpecifiers AbstractDeclarator
    	| DeclarationSpecifiers

ParameterList : ParameterDeclaration
   	| ParamaterList ',' ParameterDeclaration

ParameterTypeList : ParameterList
	|  ParameterList ',' ELLIPSIS

TypeName : SpecifierQualifierList
    	| SpecifierQualifierList AbstractDeclarator

Statement : LabeledStatement { $$ = $1; }
	| ExpressionStatement
	| CompoundStatement
	| SelectionStatement
	| IterationStatement
	| JumpStatement

ExpressionStatement : ';'
	| Expression ';'

StatementList : Statement
	| StatementList Statement

JumpStatement : GOTO IDENTIFIER ';'
	| CONTINUE ';'
	| BREAK ';'
	| RETURN ';'
	| RETURN Expression ';'

LabeledStatement : Identifier ':' Statement
	| CASE Constant ':' Statement
	| DEFAULT ':' Statement

SelectionStatement : IF '(' Expression ')' Statement
	| IF '(' Expression ')' Statement ELSE Statement
	| SWITCH '(' Expression ')' Statement

IterationStatement : WHILE '(' Expression ')' Statement
	| DO Statement WHILE '(' Expression ')' ';'
	| FOR '(' ExpressionStatement ExpressionStatement ')' Statement
	| FOR '(' ExpressionStatement ExpressionStatement Expression ')' Statement

CompoundStatement : '{' '}'
	| '{' StatementList '}'
	| '{' DeclarationList '}'
	| '{' DeclarationList StatementList '}'

FunctionDefinition : Declarator CompoundStatement
	| Declarator DeclarationList CompoundStatement
	| DeclarationSpecifiers Declarator CompoundStatement
	| DeclarationSpecifiers Declarator DeclarationList CompoundStatement

ExternalDeclaration : Declaration
	| FunctionDefinition

TranslationUnit : ExternalDeclaration
	| TranslationUnit ExternalDeclaration