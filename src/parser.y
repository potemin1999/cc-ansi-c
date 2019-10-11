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
        TypeName *typeName;
	IdentifierList *identifierList;
	Pointer *pointer;
	Enumerator *enumerator;
	Constant *constant;
	EnumeratorList *enumeratorList;
	EnumSpecifier *enumSpecifier;
	int unaryOperator;
	UnaryExpression *unaryExpression;
	PostfixExpression *postfixExpression;
	PrimaryExpression *primaryExpression;
	CastExpression *castExpression;
	MultiplicativeExpression *multExpression;
	AdditiveExpression *additiveExpression;
	ShiftExpression *shiftExpression;
	RelationalExpression *relationalExpression;
	EqualityExpression *equalityExpression;
	AndExpression *andExpression;
	ExclusiveOrExpression *exclusiveOrExpression;
	InclusiveOrExpression *inclusiveOrExpression;
	LogicalAndExpression *logicalAndExpression;
	LogicalOrExpression *logicalOrExpression;
	Expression *expression;
	AssignmentExpression *assignmentExpresion;
	Statement *statement;
	ConditionalExpression *conditionalStatement;
	CompoundStatement *compoundStatement;
	LabeledStatement *labeledStatement;
	ExpressionStatement *expressionStatement;
	SelectionStatement *selectionStatement;
	IterationStatement *iterationStatement;
	JumpStatement *jumpStatement;
	InitDeclarator *initDeclarator;
	StructDeclarator *structDeclarator;
	StructDeclaration *structDeclaration;
	StructDeclarationList *structDeclarationList;
	StructDeclaratorList *structDeclaratorList;
	StatementList *statementList;
	Declarator *declarator;
	DirectAbstractDeclarator *directAbstractDeclarator;
	DirectDeclarator *directDeclarator;
	Declaration *declaration;
	DeclarationList *declarationList;
	FunctionDefinition *functionDefinition;
	ExternalDeclaration *externalDeclaration;
	TranslationUnit *translationUnit;
	DeclarationSpecifiers *declarationSpecifiers;
	StorageClassSpecifier *storageClassSpecifier;
	SpecifierQualifierList *specifierQualifierList;
	AbstractDeclarator *abstractDeclarator;
	int typeQualifier;
	TypeSpecifier *typeSpecifier;
	TypeQualifierList *typeQualifierList;
	SpecifierQualifier *specifierQualifier;
	StructOrUnionSpecifier *structOrUnionSpecifier;
	InitDeclaratorList *initDeclaratorList;
	Initializer *initializer;
	InitializerList *initializerList;
	ParameterList *parameterList;
	ParameterTypeList *parameterTypeList;
	ParameterDeclaration *parameterDeclaration;
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
%type <identifierList> IdentifierList
%type <pointer> Pointer
%type <typeName> TypeName
%type <enumerator> Enumerator
%type <oper> AssignmentOperator
%type <constant> Constant
%type <enumeratorList> EnumeratorList
%type <enumSpecifier> EnumSpecifier
%type <unaryOperator> UnaryOperator
%type <unaryExpression> UnaryExpression
%type <postfixExpression> PostfixExpression
%type <primaryExpression> PrimaryExpression
%type <castExpression> CastExpression
%type <multExpression> MultiplicativeExpression
%type <additiveExpression> AdditiveExpression
%type <shiftExpression> ShiftExpression
%type <relationalExpression> RelationalExpression
%type <equalityExpression> EqualityExpression
%type <andExpression> AndExpression
%type <exclusiveOrExpression> ExclusiveOrExpression
%type <inclusiveOrExpression> InclusiveOrExpression
%type <logicalAndExpression> LogicalAndExpression
%type <logicalOrExpression> LogicalOrExpression
%type <expression> Expression
%type <assignmentExpresion> AssignmentExpression
%type <statement> Statement
%type <conditionalStatement> ConditionalExpression
%type <compoundStatement> CompoundStatement
%type <labeledStatement> LabeledStatement
%type <expressionStatement> ExpressionStatement
%type <selectionStatement> SelectionStatement
%type <iterationStatement> IterationStatement
%type <jumpStatement> JumpStatement
%type <initDeclarator> InitDeclarator
%type <structDeclarator> StructDeclarator
%type <structDeclaration> StructDeclaration
%type <structDeclarationList> StructDeclarationList
%type <structDeclaratorList> StructDeclaratorList
%type <statementList> StatementList
%type <declarator> Declarator
%type <directAbstractDeclarator> DirectAbstractDeclarator
%type <directDeclarator> DirectDeclarator
%type <declaration> Declaration
%type <declarationList> DeclarationList
%type <functionDefinition> FunctionDefinition
%type <externalDeclaration> ExternalDeclaration
%type <translationUnit> TranslationUnit
%type <declarationSpecifiers> DeclarationSpecifiers
%type <storageClassSpecifier> StorageClassSpecifier
%type <specifierQualifierList> SpecifierQualifierList
%type <abstractDeclarator> AbstractDeclarator
%type <typeQualifier> TypeQualifier
%type <typeSpecifier> TypeSpecifier
%type <typeQualifierList> TypeQualifierList
%type <specifierQualifier> SpecifierQualifier
%type <oper> StructOrUnion
%type <structOrUnionSpecifier> StructOrUnionSpecifier
%type <initDeclaratorList> InitDeclaratorList
%type <initializer> Initializer
%type <initializerList> InitializerList
%type <parameterList> ParameterList
%type <parameterTypeList> ParameterTypeList
%type <parameterDeclaration> ParameterDeclaration

%%

StorageClassSpecifier : AUTO 	{$$ = new StorageClassSpecifier(1); }
	| REGISTER 		{$$ = new StorageClassSpecifier(2); }
	| STATIC 		{$$ = new StorageClassSpecifier(3); }
	| EXTERN 		{$$ = new StorageClassSpecifier(4); }

Constant : INT_LITERAL {$$ = new Constant(1); }
	| CHAR_LITERAL {$$ = new Constant(2);}
	| FLOAT_LITERAL {$$ = new Constant(3);}
	| Enumerator {$$ = new Constant($1,4);}

PrimaryExpression : IDENTIFIER
	| Constant
	| STRING_LITERAL
	| '(' Expression ')'

PostfixExpression : PrimaryExpression {$$ = new PostfixExpression(nullptr,0, $1); }
	| PostfixExpression '[' Expression ']' {$$ = new PostfixExpression($1, $3);}
	| PostfixExpression '(' ')'{ $$ = new PostfixExpression($1, 1, nullptr); }
	| PostfixExpression '.' IDENTIFIER { $$ = new PostfixExpression($1, 2, new PrimaryExpression($3)); }
	| PostfixExpression OP_PTR_ACCESS IDENTIFIER { $$ = new PostfixExpression($1, OP_PTR_ACCESS, new PrimaryExpression($3)); }
	| PostfixExpression OP_INC {$$ = new PostfixExpression($1, OP_INC, nullptr);}
	| PostfixExpression OP_DEC {$$ = new PostfixExpression($1, OP_DEC, nullptr);}

UnaryOperator : '&' {$$ = 0;}
	| '*' {$$ = 1;}
	| '+' {$$ = 2;}
	| '-' {$$ = 3;}
	| '~' {$$ = 4;}
	| '!' {$$ = 5;}

UnaryExpression : PostfixExpression {$$ = new UnaryExpression(0, $1);}
    	| OP_INC UnaryExpression {$$ = new UnaryExpression(OP_INC, $2);}
    	| OP_DEC UnaryExpression {$$ = new UnaryExpression(OP_DEC, $2);}
    	| UnaryOperator CastExpression {$$ = new UnaryExpression($1, $2);}
    	| SIZEOF UnaryExpression {$$ = new UnaryExpression(SIZEOF, $2);}
    	| SIZEOF '(' IDENTIFIER ')' {$$ = new UnaryExpression($3);}

CastExpression : UnaryExpression {$$ = new CastExpression($1);}
    	| '(' IDENTIFIER ')' CastExpression {$$ = new CastExpression($2, $4);}

MultiplicativeExpression : CastExpression {$$ = new MultiplicativeExpression($1); }
	| MultiplicativeExpression '*' CastExpression {$$ = new MultiplicativeExpression($1, 0, $3);}
	| MultiplicativeExpression '/' CastExpression {$$ = new MultiplicativeExpression($1, 1, $3);}
	| MultiplicativeExpression '%' CastExpression {$$ = new MultiplicativeExpression($1, 2, $3);}

AdditiveExpression : MultiplicativeExpression {$$ = new AdditiveExpression($1); }
    	| AdditiveExpression '+' MultiplicativeExpression {$$ = new AdditiveExpression($1, 0, $3);}
    	| AdditiveExpression '-' MultiplicativeExpression {$$ = new AdditiveExpression($1, 1, $3);}

ShiftExpression : AdditiveExpression {$$ = new ShiftExpression($1); }
   	| ShiftExpression OP_L_SHIFT AdditiveExpression {$$ = new ShiftExpression($1, OP_L_SHIFT, $3);}
   	| ShiftExpression OP_R_SHIFT AdditiveExpression {$$ = new ShiftExpression($1, OP_R_SHIFT, $3);}

RelationalExpression: ShiftExpression {$$ = new RelationalExpression($1); }
    	| RelationalExpression '<' ShiftExpression {$$ = new RelationalExpression($1, 100, $3);}
    	| RelationalExpression '>' ShiftExpression {$$ = new RelationalExpression($1, 101, $3);}
    	| RelationalExpression OP_LE_THAN ShiftExpression {$$ = new RelationalExpression($1, OP_LE_THAN, $3);}
    	| RelationalExpression OP_GE_THAN ShiftExpression {$$ = new RelationalExpression($1, OP_GE_THAN, $3);}

EqualityExpression : RelationalExpression {$$ = new EqualityExpression($1); }
    	| EqualityExpression OP_EQ_TO RelationalExpression {$$ = new EqualityExpression($1, OP_EQ_TO, $3);}
    	| EqualityExpression OP_NEQ_TO RelationalExpression {$$ = new EqualityExpression($1, OP_NEQ_TO, $3);}

AndExpression : EqualityExpression {$$ = new AndExpression(nullptr,$1); }
    	| AndExpression '&' EqualityExpression {$$ = new AndExpression($1, $3);}

ExclusiveOrExpression : AndExpression {$$ = new ExclusiveOrExpression(nullptr,$1); }
    	| ExclusiveOrExpression '^' AndExpression {$$ = new ExclusiveOrExpression($1, $3);}

InclusiveOrExpression : ExclusiveOrExpression {$$ = new InclusiveOrExpression(nullptr,$1); }
    	| InclusiveOrExpression '|' ExclusiveOrExpression {$$ = new InclusiveOrExpression($1, $3);}

LogicalAndExpression : InclusiveOrExpression {$$ = new LogicalAndExpression(nullptr,$1); }
    	| LogicalAndExpression OP_AND InclusiveOrExpression {$$ = new LogicalAndExpression($1, $3);}

LogicalOrExpression : LogicalAndExpression {$$ = new LogicalOrExpression(nullptr,$1); }
    	| LogicalOrExpression OP_OR LogicalAndExpression {$$ = new LogicalOrExpression($1,$3); }

ConditionalExpression : LogicalOrExpression {$$ = new ConditionalExpression($1,nullptr,nullptr);}
    	| LogicalOrExpression '?' Expression ':' ConditionalExpression {$$ = new ConditionalExpression($1, $3, $5);}

AssignmentOperator : '='    {$$ = 0; }
    	| OP_MUL_ASSIGN     {$$ = OP_MUL_ASSIGN; }
    	| OP_DIV_ASSIGN     {$$ = OP_DIV_ASSIGN;}
    	| OP_MOD_ASSIGN     {$$ = OP_MOD_ASSIGN;}
    	| OP_ADD_ASSIGN     {$$ = OP_ADD_ASSIGN;}
    	| OP_SUB_ASSIGN     {$$ = OP_SUB_ASSIGN;}
    	| OP_L_SHIFT_ASSIGN {$$ = OP_L_SHIFT_ASSIGN;}
    	| OP_R_SHIFT_ASSIGN {$$ = OP_R_SHIFT_ASSIGN;}
    	| OP_AND_ASSIGN     {$$ = OP_AND_ASSIGN;}
    	| OP_XOR_ASSIGN     {$$ = OP_XOR_ASSIGN;}
    	| OP_OR_ASSIGN      {$$ = OP_OR_ASSIGN;}

AssignmentExpression : ConditionalExpression {$$ = new AssignmentExpression($1); }
	| UnaryExpression AssignmentOperator AssignmentExpression { $$ = new AssignmentExpression($1, $2, $3);}

Expression : AssignmentExpression { $$ = $1; }
	| Expression ',' AssignmentExpression {$$ = $3; } //TODO maybe fix this

Enumerator : IDENTIFIER  {$$ = new Enumerator($1, nullptr);}
    	| IDENTIFIER '=' Constant {$$ = new Enumerator($1, $3);}

EnumeratorList : Enumerator {$$ = new EnumeratorList($1);}
    	| EnumeratorList ',' Enumerator {$1->addEnumerator($3); $$ = $1;}

EnumSpecifier : ENUM IDENTIFIER {$$ = new EnumSpecifier($2, nullptr);}
    	| ENUM '{' EnumeratorList '}' {$$ = new EnumSpecifier(nullptr, $3);}
    	| ENUM IDENTIFIER '{' EnumeratorList '}' {$$ = new EnumSpecifier($2, $4);}

Initializer : AssignmentExpression {$$ = new Initializer($1);}
    	| '{' InitializerList '}' {$$ = new Initializer($2);}
    	| '{' InitializerList ',' '}' {$$ = new Initializer($2);}

InitializerList : Initializer {$$ = new InitializerList($1);}
    	| InitializerList ',' Initializer {$1 -> addInitializer($3); $$ = $1;}

IdentifierList : IDENTIFIER {$$ = new IdentifierList($1);}
    	| IdentifierList ',' IDENTIFIER {$1 -> addIdentifier($3); $$ = $1;}

TypeQualifier : CONST { $$ = CONST; } | VOLATILE { $$ = VOLATILE; }

TypeQualifierList : TypeQualifier
	{ $$ = new TypeQualifierList($1); 	}
	| TypeQualifierList TypeQualifier
	{ $1->addTypeQualifier($2); $$ = $1;	}

StructOrUnion : STRUCT { $$ = STRUCT; } | UNION { $$ = UNION; }

StructOrUnionSpecifier : StructOrUnion IDENTIFIER
	{ $$ = new StructOrUnionSpecifier($1 == UNION,$2);	}
	| StructOrUnion '{' StructDeclarationList '}'
	{ $$ = new StructOrUnionSpecifier($1 == UNION,$3);	}
	| StructOrUnion IDENTIFIER '{' StructDeclarationList '}'
	{ $$ = new StructOrUnionSpecifier($1 == UNION,$2,$4);	}

TypeSpecifier : VOID
	{ $$ = new TypeSpecifier(VOID); }
	| CHAR
	{ $$ = new TypeSpecifier(CHAR); }
	| SHORT
	{ $$ = new TypeSpecifier(SHORT); }
	| INT
	{ $$ = new TypeSpecifier(INT); }
	| LONG
	{ $$ = new TypeSpecifier(LONG); }
	| FLOAT
	{ $$ = new TypeSpecifier(FLOAT); }
	| DOUBLE
	{ $$ = new TypeSpecifier(DOUBLE); }
	| SIGNED
	{ $$ = new TypeSpecifier(SIGNED); }
	| UNSIGNED
	{ $$ = new TypeSpecifier(UNSIGNED); }
	| StructOrUnionSpecifier
	{ $$ = new TypeSpecifier($1); }
	| EnumSpecifier
	{ $$ = new TypeSpecifier($1); }
	| IDENTIFIER
	{ $$ = new TypeSpecifier($1); }

SpecifierQualifier : TypeSpecifier
	{ $$ = new SpecifierQualifier($1);	}
	| TypeQualifier IDENTIFIER
	{ $$ = new SpecifierQualifier($1,$2);	}

SpecifierQualifierList : SpecifierQualifier
	{ $$ = new SpecifierQualifierList($1); 		}
	| SpecifierQualifierList SpecifierQualifier
	{ $1->addSpecifierQualifier($2); $$ = $1;	}

Pointer : '*'
	{ $$ = new Pointer();		}
   	| '*' Pointer
   	{ $$ = new Pointer($2);		}
   	| '*' TypeQualifierList
   	{ $$ = new Pointer($2);		}
   	| '*' TypeQualifierList Pointer
   	{ $$ = new Pointer($3,$2);	}

Declarator : Pointer DirectDeclarator
	{ $$ = new Declarator($1,$2);		}
    	| DirectDeclarator
    	{ $$ = new Declarator(nullptr,$1);	}

DirectDeclarator : IDENTIFIER
	{ $$ = new DirectDeclarator($1);	}
    	| '(' Declarator ')'
    	{ $$ = new DirectDeclarator($2);	}
    	| DirectDeclarator '(' ')'
    	{ $$ = new DirectDeclarator($1,false);	}
    	| DirectDeclarator '[' ']'
    	{ $$ = new DirectDeclarator($1,true);	}
    	| DirectDeclarator '(' ParameterTypeList ')'
    	{ $$ = new DirectDeclarator($1,$3);	}
    	| DirectDeclarator '(' IdentifierList ')'
    	{ $$ = new DirectDeclarator($1,$3);	}
    	| DirectDeclarator '[' Constant ']'
    	{ $$ = new DirectDeclarator($1,$3);	}

DirectAbstractDeclarator : '[' ']'
	{ $$ = new DirectAbstractDeclarator(true);	}
   	| '[' Constant ']'
   	{ $$ = new DirectAbstractDeclarator($2);	}
   	| '(' ')'
   	{ $$ = new DirectAbstractDeclarator(false);	}
   	| '(' ParameterTypeList ')'
   	{ $$ = new DirectAbstractDeclarator($2);	}
   	| '(' AbstractDeclarator ')'
   	{ $$ = new DirectAbstractDeclarator($2);	}
   	| DirectAbstractDeclarator '[' ']'
   	{ $$ = new DirectAbstractDeclarator($1,true);	}
   	| DirectAbstractDeclarator '[' Constant ']'
   	{ $$ = new DirectAbstractDeclarator($1,$3);	}
   	| DirectAbstractDeclarator '(' ')'
   	{ $$ = new DirectAbstractDeclarator($1,false);	}
   	| DirectAbstractDeclarator '(' ParameterTypeList ')'
   	{ $$ = new DirectAbstractDeclarator($1,$3);	}

AbstractDeclarator : Pointer
	{ $$ = new AbstractDeclarator($1);	}
    	| DirectAbstractDeclarator
    	{ $$ = new AbstractDeclarator($1);	}
    	| Pointer DirectAbstractDeclarator
    	{ $$ = new AbstractDeclarator($1,$2);	}

InitDeclarator : Declarator
	{ $$ = new InitDeclarator($1);		}
	| Declarator '=' Initializer
	{ $$ = new InitDeclarator($1,$3);	}

InitDeclaratorList : InitDeclarator
	{ $$ = new InitDeclaratorList($1);	}
	| InitDeclaratorList ',' InitDeclarator
	{ $1->addDeclarator($3); $$ = $1;	}

DeclarationSpecifiers : StorageClassSpecifier
	{ $$ = new DeclarationSpecifiers(new DeclarationSpecifier($1));	}
	| TypeSpecifier
	{ $$ = new DeclarationSpecifiers(new DeclarationSpecifier($1));	}
	| TypeQualifier
	{ $$ = new DeclarationSpecifiers(new DeclarationSpecifier($1));	}
	| StorageClassSpecifier DeclarationSpecifiers
	{ $2->addDeclarationSpecifier(new DeclarationSpecifier($1)); $$ = $2;	}
	| TypeSpecifier DeclarationSpecifiers
	{ $2->addDeclarationSpecifier(new DeclarationSpecifier($1)); $$ = $2;	}
	| TypeQualifier DeclarationSpecifiers
	{ $2->addDeclarationSpecifier(new DeclarationSpecifier($1)); $$ = $2;	}

Declaration : DeclarationSpecifiers ';'
	{ $$ = new Declaration($1);	}
	| DeclarationSpecifiers InitDeclaratorList ';'
	{ $$ = new Declaration($1,$2);	}

DeclarationList : Declaration
	{ $$ = new DeclarationList($1);		}
	| DeclarationList Declaration
	{ $1->addDeclaration($2); $$ = $1;	}

StructDeclarator : Declarator
	{ $$ = new StructDeclarator($1);	}
	| Declarator ':' Constant
	{ $$ = new StructDeclarator($1,$3);	}
	| ':' Constant
	{ $$ = new StructDeclarator($2);	}

StructDeclaratorList : StructDeclarator
	{ $$ = new StructDeclaratorList($1);	}
	| StructDeclaratorList StructDeclarator
	{ $1->addStructDeclarator($2); $$ = $1;	}

StructDeclaration : SpecifierQualifierList StructDeclaratorList ';'
	{ $$ = new StructDeclaration($1,$2);		}

StructDeclarationList : StructDeclaration
	{ $$ = new StructDeclarationList($1);		}
	| StructDeclarationList StructDeclaration
	{ $1->addStructDeclaration($2); $$ = $1;	}

ParameterDeclaration : DeclarationSpecifiers Declarator
	{ $$ = new ParameterDeclaration($1,$2); 	}
    	| DeclarationSpecifiers AbstractDeclarator
    	{ $$ = new ParameterDeclaration($1,$2);		}
    	| DeclarationSpecifiers
    	{ $$ = new ParameterDeclaration($1);		}

ParameterList : ParameterDeclaration
	{ $$ = new ParameterList($1);			}
   	| ParameterList ',' ParameterDeclaration
   	{ $1->addParameterDeclaration($3), $$ = $1;	}

ParameterTypeList : ParameterList
	{ $$ = new ParameterTypeList($1, false); 	}
	|  ParameterList ',' ELLIPSIS
	{ $$ = new ParameterTypeList($1, true); 	}

TypeName : SpecifierQualifierList
	{ $$ = new TypeName($1); 	}
    	| SpecifierQualifierList AbstractDeclarator
    	{ $$ = new TypeName($1, $2); 	}

Statement : LabeledStatement 	{ $$ = $1; }
	| ExpressionStatement 	{ $$ = $1; }
	| CompoundStatement   	{ $$ = $1; }
	| SelectionStatement	{ $$ = $1; }
	| IterationStatement	{ $$ = $1; }
	| JumpStatement		{ $$ = $1; }

ExpressionStatement : ';'
	{ $$ = new ExpressionStatement();	}
	| Expression ';'
	{ $$ = new ExpressionStatement($1);	}

StatementList : Statement
	{ $$ = new StatementList($1); 		}
	| StatementList Statement
	{ $1->addStatement($2); $$ = $1;	}

JumpStatement : GOTO IDENTIFIER ';'
	{ $$ = new JumpStatement($2, 0 );	}
	| CONTINUE ';'
	{ $$ = new JumpStatement(1);		}
	| BREAK ';'
	{ $$ = new JumpStatement(2);		}
	| RETURN ';'
	{ $$ = new JumpStatement(3); 		}
	| RETURN Expression ';'
	{ $$ = new JumpStatement($2, 4);	}

LabeledStatement : IDENTIFIER ':' Statement
	{ $$ = new LabeledStatement($1, $3);	}
	| CASE Constant ':' Statement
	{ $$ = new LabeledStatement($2, $4, 1); 	}
	| DEFAULT ':' Statement
	{ $$ = new LabeledStatement($3); 	}

SelectionStatement : IF '(' Expression ')' Statement
	{ $$ = new SelectionStatement($3, $5, 0);	}
	| IF '(' Expression ')' Statement ELSE Statement
	{ $$ = new SelectionStatement($3, $5, $7, 1);	}
	| SWITCH '(' Expression ')' Statement
	{ $$ = new SelectionStatement($3, $5, 0 ); 	}

IterationStatement : WHILE '(' Expression ')' Statement
	{ $$ = new IterationStatement($3, $5, 0 );		}
	| DO Statement WHILE '(' Expression ')' ';'
	{ $$ = new IterationStatement($2, $5, 1);		}
	| FOR '(' ExpressionStatement ExpressionStatement ')' Statement
	{ $$ = new IterationStatement($3, $4, $6, 2);		}
	| FOR '(' ExpressionStatement ExpressionStatement Expression ')' Statement
	{ $$ = new IterationStatement($3, $4, $5, $7, 3);	}

CompoundStatement : '{' '}'
	| '{' StatementList '}'
	{ $$ = new CompoundStatement($2); 	}
	| '{' DeclarationList '}'
	{ $$ = new CompoundStatement($2); 	}
	| '{' DeclarationList StatementList '}'
	{ $$ = new CompoundStatement($2, $3); 	}

FunctionDefinition : Declarator CompoundStatement
	{ $$ = new FunctionDefinition($1, $2); 		}
	| Declarator DeclarationList CompoundStatement
	{ $$ = new FunctionDefinition($1, $2, $3); 	}
	| DeclarationSpecifiers Declarator CompoundStatement
	{ $$ = new FunctionDefinition($1, $2, $3); 	}
	| DeclarationSpecifiers Declarator DeclarationList CompoundStatement
	{ $$ = new FunctionDefinition($1, $2, $3, $4); 	}

ExternalDeclaration : Declaration
	{ $$ = new ExternalDeclaration($1); }
	| FunctionDefinition
	{ $$ = new ExternalDeclaration($1); }

TranslationUnit : ExternalDeclaration
	{ $$ = new TranslationUnit($1); }
	| TranslationUnit ExternalDeclaration
	{ $1->addExternalDeclaration($2); $$ = $1; }

%%

