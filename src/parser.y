%{
extern "C"
{
        int yyparse(void);
        int yyerror(const char *msg);
        int yylex(void);
        int yywrap()
        {
                return 1;
        }

}
#include "../parser.h"
%}

%union {
	AstNode *astNode;
	char *stringVal;
        int oper;
        Constant *constant;
        UnaryExpression *unaryExpression;
        PostfixExpression *postfixExpression;
        PrimaryExpression *primaryExpression;
        Expression *expression;
        AssignmentExpression *assignmentExpresion;
        Statement *statement;
        CompoundStatement* compoundStatement;
        LabeledStatement *labeledStatement;
        ExpressionStatement *expressionStatement;
        SelectionStatement *selectionStatement;
        IterationStatement *iterationStatement;
        JumpStatement *jumpStatement;
        StructDeclarationList *structDeclarationList;
        StructDeclaratorList *structDeclaratorList;
        StatementList *statementList;
        Declarator *declarator;
        FunctionDefinition *functionDefinition;
        ExternalDeclaration *externalDeclaration;
        TranslationUnit *translationUnit;
        DeclarationSpecifiers *declarationSpecifiers;
        StorageClassSpecifier *storageClassSpecifier;
        ArgumentExpressionList *argumentExpressionList;
        SpecifierQualifierList *specifierQualifierList;
        AbstractDeclarator *abstractDeclarator;
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
%type <enumerator> Enumerator
%type <oper> AssignmentOperator
%type <constant> Constant
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
%type <declaration> Declaration
%type <declarationList> DeclarationList
%type <functionDefinition> FunctionDefinition
%type <externalDeclaration> ExternalDeclaration
%type <translationUnit> TranslationUnit
%type <declarationSpecifiers> DeclarationSpecifiers
%type <storageClassSpecifier> StorageClassSpecifier
%type <argumentExpressionList> ArgumentExpressionList
%type <specifierQualifierList> SpecifierQualifierList
%type <abstractDeclarator> AbstractDeclarator

%%

StorageClassSpecifier : AUTO 	{$$ = new StorageClassSpecifier(1); }
	| REGISTER 		{$$ = new StorageClassSpecifier(2); }
	| STATIC 		{$$ = new StorageClassSpecifier(3); }
	| EXTERN 		{$$ = new StorageClassSpecifier(4); }

Constant : INT_LITERAL {$$ = new ConstantType(1); }
	| CHAR_LITERAL {$$ = new ConstantType(2);}
	| FLOAT_LITERAL {$$ = new ConstantType(3);}
	| Enumerator {$$ = new ConstantType($1,4);}

PrimaryExpression : IDENTIFIER
	| Constant
	| STRING_LITERAL
	| '(' Expression ')'

PostfixExpression : PrimaryExpression {$$ = $1; }
	| PostfixExpression '[' Expression ']' {$$ = new PostfixExpression($1, 0, $3);}
	| PostfixExpression '(' ')'{ $$ = new PostfixExpresion($1, 1, nullptr); }
	| PostfixExpression '(' ArgumentExpressionList ')' { $$ = new PostfixExpresion($1, 1, $3); }
	| PostfixExpression '.' IDENTIFIER { $$ = new PostfixExpresion($1, 2, new PrimaryExpression($3)); }
	| PostfixExpression OP_PTR_ACCESS IDENTIFIER { $$ = new PostfixExpression($1, OP_PTR_ACCESS, new PrimaryExpression($3)); }
	| PostfixExpression OP_INC {$$ = new PostfixExpression($1, $2, nullptr);}
	| PostfixExpression OP_DEC {$$ = new PostfixExpression($1, $2, nullptr);}

UnaryOperator : '&' {$$ = 0}
	| '*' {$$ = 1}
	| '+' {$$ = 2}
	| '-' {$$ = 3}
	| '~' {$$ = 4}
	| '!' {$$ = 5}

UnaryExpression : PostfixExpression {$$ = new UnaryExpression(0, $1);}
    	| OP_INC UnaryExpression {$$ = new UnaryExpression($1, $2);}
    	| OP_DEC UnaryExpression {$$ = new UnaryExpression($1, $2);}
    	| UnaryOperator CastExpression {$$ = new UnaryExpression($1, $2);}
    	| SIZEOF UnaryExpression {$$ = new UnaryExpression($1, $2);}
    	| SIZEOF '(' TypeName ')' {$$ = new UnaryExpression($1, $3);}

CastExpression : UnaryExpression {$$ = $1}
    	| '(' TypeName ')' CastExpression {$$ = new CastExpression($2, $4);}

MultiplicativeExpression : CastExpression {$$ = $1}
	| MultiplicativeExpression '*' CastExpression {$$ = new MultiplicativeExpression($1, 0, $3);}
	| MultiplicativeExpression '/' CastExpression {$$ = new MultiplicativeExpression($1, 1, $3);}
	| MultiplicativeExpression '%' CastExpression {$$ = new MultiplicativeExpression($1, 2, $3);}

AdditiveExpression : MultiplicativeExpression {$$ = $1}
    	| AdditiveExpression '+' MultiplicativeExpression {$$ = new AdditiveExpression($1, 0, $3);}
    	| AdditiveExpression '-' MultiplicativeExpression {$$ = new AdditiveExpression($1, 1, $3);}

ShiftExpression : AdditiveExpression {$$ = $1}
   	| ShiftExpression OP_L_SHIFT AdditiveExpression {$$ = new ShiftExpression($1, $2, $3);}
   	| ShiftExpression OP_R_SHIFT AdditiveExpression {$$ = new ShiftExpression($1, $2, $3);}

RelationalExpression: ShiftExpression {$$ = $1}
    	| RelationalExpression '<' ShiftExpression {$$ = new RelationalExpression($1, 0, $3);}
    	| RelationalExpression '>' ShiftExpression {$$ = new RelationalExpression($1, 1, $3);}
    	| RelationalExpression OP_LE_THAN ShiftExpression {$$ = new RelationalExpression($1, $2, $3);}
    	| RelationalExpression OP_GE_THAN ShiftExpression {$$ = new RelationalExpression($1, $2, $3);}

EqualityExpression : RelationalExpression {$$ = $1}
    	| EqualityExpression OP_EQ_TO RelationalExpression {$$ = new EqualityExpression($1, $2, $3);}
    	| EqualityExpression OP_NEQ_TO RelationalExpression {$$ = new EqualityExpression($1, $2, $3);}

AndExpression : EqualityExpression {$$ = $1}
    	| AndExpression '&' EqualityExpression {$$ = new AndExpression($1, $3);}

ExclusiveOrExpression : AndExpression {$$ = $1}
    	| ExclusiveOrExpression '^' AndExpression {$$ = new ExclusiveOrExpression($1, $3);}

InclusiveOrExpression : ExclusiveOrExpression {$$ = $1}
    	| InclusiveOrExpression '|' ExclusiveOrExpression {$$ = new InclusiveOrExpression($1, $3);}

LogicalAndExpression : InclusiveOrExpression {$$ = $1}
    	| LogicalAndExpression OP_AND InclusiveOrExpression {$$ = new LogicalEndExpression($1, $3);}

LogicalOrExpression : LogicalAndExpression {$$ = new LogicalAndExpression(nullptr, $1);}
    	| LogicalOrExpression OP_OR LogicalAndExpression {$$ = $1}

ConditionalExpression : LogicalOrExpression {$$ = $1}
    	| LogicalOrExpression '?' Expression ':' ConditionalExpression {$$ = new ConditionalExpression($1, $2, $3);}

AssignmentOperator : '='    {$$ = 0}
    	| OP_MUL_ASSIGN     {$$ = $1}
    	| OP_DIV_ASSIGN     {$$ = $1}
    	| OP_MOD_ASSIGN     {$$ = $1}
    	| OP_ADD_ASSIGN     {$$ = $1}
    	| OP_SUB_ASSIGN     {$$ = $1}
    	| OP_L_SHIFT_ASSIGN {$$ = $1}
    	| OP_R_SHIFT_ASSIGN {$$ = $1}
    	| OP_AND_ASSIGN     {$$ = $1}
    	| OP_XOR_ASSIGN     {$$ = $1}
    	| OP_OR_ASSIGN      {$$ = $1}

AssignmentExpression : ConditionalExpression {$$ = $1}
	| UnaryExpression AssignmentOperator AssignmentExpression { $$ = new AssignmentExpression($1, $2, $3);}

Expression : AssignmentExpression { $$ = $1}
	| Expression ',' AssignmentExpression {$$ = $1, $3} //TODO maybe fix this

Enumerator : IDENTIFIER  {$$ = new Enumerator($1, nullptr);}
    	| IDENTIFIER '=' Constant {$$ = new Enumerator($1, $3);}

EnumeratorList : Enumerator {$$ = new EnumeratorList($1);}
    	| EnumeratorList ',' Enumerator {$$ = $1->addEnumerator($3);}

EnumSpecifier : ENUM IDENTIFIER {$$ = new EnumSpecifier($1, nullptr);}
    	| ENUM '{' EnumeratorList '}' {$$ = new EnumSpecifier(nullptr, $3);}
    	| ENUM IDENTIFIER '{' EnumeratorList '}' {$$ = new EnumSpecifier($2, $4);}

Initializer : AssignmentExpression {$$ = new Initializer($1);}
    	| '{' InitializerList '}' {$$ = new Initializer($2);}
    	| '{' InitializerList ',' '}' {$$ = new Initializer($2);}

InitializerList : Initializer {$$ = new InitializerList($1);}
    	| InitializerList ',' Initializer {$$ = $1 -> addInitializer($3);}

IdentifierList : IDENTIFIER {$$ = new IdentifierList($1);}
    	| IdentifierList ',' IDENTIFIER {$$ = $1 -> addIdentifier($3);}

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

StructDeclaratorList : StructDeclarator
	| StructDeclaratorList StructDeclarator

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
	| Expression ';' {$$ = $1}

StatementList : Statement {$$ = $1}
	| StatementList Statement {$$ = $1, $2}

JumpStatement : GOTO IDENTIFIER ';' {$$ = $2, 0 ;}
	| CONTINUE ';' {$$ = 1 ;}
	| BREAK ';' {$$ = 2 ;}
	| RETURN ';' {$$ = 3}
	| RETURN Expression ';' {$$ = $2, 4}

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

ExternalDeclaration : Declaration {$$ = new ExternalDeclaration($1); }
	| FunctionDefinition {$$ = new ExternalDeclaration($1); }

TranslationUnit : ExternalDeclaration { $$ = new TranslationUnti($1); }
	| TranslationUnit ExternalDeclaration {$$ = $1, $2; }

%%

