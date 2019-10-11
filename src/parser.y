%{
	#include "../parser.h"
%}

%union {
	struct AstNode *astNode;
	char *stringVal;
        int oper;
        AssignmentOperator* oper
        Constant *astNode
        UnaryExpression *unaryExpression
        PostfixExpression *postfixExpression
        PrimaryExpression *primaryExpression
        Expression *expression
        AssignmentExpression *assignmentExpresion
        Statement *statement
        CompoundStatement* compoundStatement
        LabeledStatement *labeledStatement
        ExpressionStatement *expressionStatement
        SelectionStatement *selectionStatement
        IterationStatement *iterationStatement
        JumpStatement *jumpStatement
        StructDeclarationList *structDeclarationList
        StructDeclaratorList *structDeclaratorList
        PostfixExpression *postfixExpression
        StatementList *statementList
        Declarator *declarator
        FunctionDefinition *functionDefinition
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

%type <stringVal> IDENTIFIER
%type <stringVal> TypeName
%type <oper> AssignmentOperator
%type <astNode> Constant
%type <unaryExpression> UnaryExpression
%type <postfixExpression> PostfixExpression
%type <primaryExpression> PrimaryExpression
%type <expression> Expression
%type <assignmentExpresion> AssignmentExpression
%type <statement> Statement
%type <compoundStatement> CompoundStatement
%type <labeledStatement> LabeledStatement
%type <expressionStatement> ExpressionStatement
%type <selectionStatement> SelectionStatement
%type <iterationStatement> IterationStatement
%type <jumpStatement> JumpStatement
%type <structDeclarationList> StructDeclarationList
%type <structDeclaratorList> StructDeclaratorList
%type <statementList> StatementList
%type <declarator> Declarator
%type <functionDefinition> FunctionDefinition
%type <externalDeclaration> ExternalDeclaration
%type <translationUnit> TranslationUnit
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

PostfixExpression : PrimaryExpression {$$ = $1; }
	| PostfixExpression '[' Expression ']'
	| PostfixExpression '(' ')'
	| PostfixExpression '(' Expression ')'
	| PostfixExpression '.' IDENTIFIER
	| PostfixExpression OP_PTR_ACCESS IDENTIFIER { $$ = new PostfixExpresion($1,OP_PTR_ACCESS,$3); }
	| PostfixExpression OP_INC
	| PostfixExpression OP_DEC

UnaryOperator : '&'
	| '*'
	| '+'
	| '-'
	| '~'
	| '!'

UnaryExpression : PostfixExpression
    	| OP_INC UnaryExpression
    	| OP_DEC UnaryExpression
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
    	| LogicalAndExpression OP_AND InclusiveOrExpression

LogicalOrExpression : LogicalAndExpression
    	| LogicalOrExpression OP_OR LogicalAndExpression

ConditionalExpression : LogicalOrExpression
    	| LogicalOrExpression '?' Expression : ConditionalExpression

AssignmentOperator : '='
    	| OP_MUL_ASSIGN
    	| OP_DIV_ASSIGN
    	| OP_MOD_ASSIGN
    	| OP_ADD_ASSIGN
    	| OP_SUB_ASSIGN
    	| OP_L_SHIFT_ASSIGN
    	| OP_R_SHIFT_ASSIGN
    	| OP_AND_ASSIGN
    	| OP_XOR_ASSIGN
    	| OP_OR_ASSIGN

AssignmentExpression : ConditionalExpression
	| UnaryExpression AssignmentOperator AssignmentExpression { $$ = $2 }

Expression : AssignmentExpression
	| Expression ',' AssignmentExpression

// TODO begin
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
	| TypeQualifier IDENTIFIER

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
   	| ParameterList ',' ParameterDeclaration

ParameterTypeList : ParameterList
	|  ParameterList ',' ELLIPSIS

TypeName : SpecifierQualifierList {$$ = $1; }
    	| SpecifierQualifierList AbstractDeclarator {$$ = $1, $2; }

Statement : LabeledStatement 	{ $$ = $1; }
	| ExpressionStatement 	{ $$ = $1; }
	| CompoundStatement   	{ $$ = $1; }
	| SelectionStatement	{ $$ = $1; }
	| IterationStatement	{ $$ = $1; }
	| JumpStatement		{ $$ = $1; }

ExpressionStatement : ';'
	| Expression ';'

StatementList : Statement
	| StatementList Statement

JumpStatement : GOTO IDENTIFIER ';' {$$ = $2, 0 ;}
	| CONTINUE ';' {$$ = 1 ;}
	| BREAK ';' {$$ = 2 ;}
	| RETURN ';' {$$ = 3}
	| RETURN Expression ';' {$$ = $ 2, 4}

LabeledStatement : IDENTIFIER ':' Statement {$$ = $1, $3 0}
	| CASE Constant ':' Statement {$$ = $2, $4, 1}
	| DEFAULT ':' Statement {$$ = $3, 2}

SelectionStatement : IF '(' Expression ')' Statement {$$ = $3, $5, 0 ;}
	| IF '(' Expression ')' Statement ELSE Statement {$$ = $3, $5, $7, 1 ;}
	| SWITCH '(' Expression ')' Statement {$$ = $3, $5, 0 ;}

IterationStatement : WHILE '(' Expression ')' Statement {$$ = $3, $5, 0 ;}
	| DO Statement WHILE '(' Expression ')' ';' {$$ = $2, $5, 1 ;}
	| FOR '(' ExpressionStatement ExpressionStatement ')' Statement {$$ = $3, $4, $6, 2 ;}
	| FOR '(' ExpressionStatement ExpressionStatement Expression ')' Statement  {$$ = $3, $4, $5, $7, 3 ;}

CompoundStatement : '{' '}'
	| '{' StatementList '}' {$$ = new CompoundStatement($2); }
	| '{' DeclarationList '}' {$$ = new $2; }
	| '{' DeclarationList StatementList '}' {$$ = $2, $3; }

FunctionDefinition : Declarator CompoundStatement {$$ = $1, $2; }
	| Declarator DeclarationList CompoundStatement {$$ = $1, $2, $3; }
	| DeclarationSpecifiers Declarator CompoundStatement {$$ = $1, $2, $3; }
	| DeclarationSpecifiers Declarator DeclarationList CompoundStatement {$$ = $1, $2, $3, $4; }

ExternalDeclaration : Declaration {$$ = new Declaration($1); }
	| FunctionDefinition {$$ = new FunctionDefinition($1); }

TranslationUnit : ExternalDeclaration {$$ = $1; }
	| TranslationUnit ExternalDeclaration {$$ = $1, $2; }