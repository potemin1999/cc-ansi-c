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
%token ASM BREAK CASE CONTINUE DEFAULT DO ELSE ENUM FOR GOTO IF RETURN SIZEOF STRUCT SWITCH TYPEDEF UNION WHILE

%type <stringVal> String
%type <astNode> Constant UnaryExpression PosfixExpression PrimaryExpression Expression AssignmentExpression OptionalExpression
%type <astNode> Statement CompountStatement LabeledStatement ExpressionStatement SelectionStatement IterationStatement JumpStatement

%%

StorageClassSpecifier : AUTO
	| REGISTER
	| STATIC
	| EXTERN

SpecifierQualifier : TypeSpecifier
	| TypeQualifier String

SpecifierQualifierList : SpecifierQualifier
	| SpecifierQualifier

StructOrUnion : STRUCT | UNION

StructOrUnionSpecifier : StructOrUnion Identifier
	| StructOrUnion '{' NonEmptyStructDeclarationList '}'
	| StructOrUnion identifier '{' NonEmptyStructDeclarationList '}'

NonEmptyStructDeclarationList : StructDeclaration
	| NonEmptyStructDeclarationList StructDeclaration

TypeSpecifier : VOID
	| CHAR
	| SHORT
	| INT
	| LONG
	| FLOAT
	| DOUBLE
	| SIGNED
	| UNSIGNED
	|

Constant : IntegerConstant
	| CharacterConstant
	| FloatingConstant
	| EnumerationConstant

PostfixExpression : PrimaryExpression
	| PostfixExpression '[' Expression ']'
				//Assignment Expression
	| PostfixExpression '(' Expression ')'
	| PostfixExpression '.' Identifier
	| PostfixExpression OP_PTR_ACCESS Identifier
	| PostfixExpression OP_INC
	| PostfixExpression OP_DEC

PrimaryExpression : Identifier
	| Constant
	| String
	| '(' Expression ')'

Expression : AssignmentExpression
	| Expression ',' AssignmentExpression

AssignmentExpression : ConditionalExpression
	| UnaryExpression AssignmentOperator AssignmentExpression { $$ = $2 }


Statement : LabeledStatement { $$ = $1; }
	| ExpressionStatement
	| CompoundStatement
	| SelectionStatement
	| IterationStatement
	| JumpStatement

LabeledStatement : Identifier ':' Statement
	| CASE ConstantExpression ':' Statement
	| DEFAULT ':' Statement

CompoundStatement : '{' '}'
	| '{' StatementList '}'
	| '{' DeclarationList '}'
	| '{' DeclarationList StatementList '}'

DeclarationList : Declaration
	| DeclarationList Declaration

StatementList : Statement
	| StatementList Statement

ExpressionStatement : ';'
	| Expression ';'

SelectionStatement : IF '(' Expression ')' Statement
	| IF '(' Expression ')' Statement ELSE Statement
	| SWITCH '(' Expression ')' Statement

IterationStatement : WHILE '(' Expression ')' Statement
	| DO Statement WHILE '(' Expression ')' ';'
	| FOR '(' ExpressionStatement ExpressionStatement ')' Statement
	| FOR '(' ExpressionStatement ExpressionStatement Expression ')' Statement

JumpStatement : GOTO Identifier ';'
	| CONTINUE ';'
	| BREAK ';'
	| RETURN OptionalExpression ';'

FunctionDefinition : Declarator CompoundStatement
	| Declarator DeclarationList CompoundStatement
	| DeclarationSpecifiers Declarator CompoundStatement
	| DeclarationSpecifiers Declarator DeclarationList CompoundStatement

ExternalDeclaration : Declaration
	| FunctionDefinition

TranslationUnit : ExternalDeclaration
	: TranslationUnit ExternalDeclaration