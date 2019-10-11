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
        Constant *constant;
        UnaryExpression *unaryExpression;
        PostfixExpression *postfixExpression;
        PrimaryExpression *primaryExpression;
        Expression *expression;
        Enumerator *enumerator;
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
        Declaration *declaration;
        DeclarationList *declarationList;
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
%type <typeName> TypeName
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

Constant : INT_LITERAL {$$ = new Constant(1); }
	| CHAR_LITERAL {$$ = new Constant(2);}
	| FLOAT_LITERAL {$$ = new Constant(3);}
	| Enumerator {$$ = new Constant($1,4);}

PrimaryExpression : IDENTIFIER
	| Constant
	| STRING_LITERAL
	| '(' Expression ')'

PostfixExpression : PrimaryExpression
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

ArgumentExpressionList : AssignmentExpression {$$ = $1}
    	| ArgumentExpressionList ',' AssignmentExpression {$$ = $1, $3}

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

TypeQualifier : CONST { $$ = $1; } | VOLATILE { $$ = $1; }

TypeQualifierList : TypeQualifier
	{ $$ = new TypeQualifierList($1); 	}
	| TypeQualifierList TypeQualifier
	{ $1->addTypeQualifier($2); $$ = $1;	}

StructOrUnion : STRUCT { $$ = $1; } | UNION { $$ = $1; }

StructOrUnionSpecifier : StructOrUnion IDENTIFIER
	{ $$ = new StructOrUnionSpecifier($1 == UNION,$2);	}
	| StructOrUnion '{' StructDeclarationList '}'
	{ $$ = new StructOrUnionSpecifier($1 == UNION,$3);	}
	| StructOrUnion IDENTIFIER '{' StructDeclarationList '}'
	{ $$ = new StructOrUnionSpecifier($1 == UNION,$2,$3);	}

TypeSpecifier : VOID
	{ $$ = new TypeSpecifier($1); }
	| CHAR
	{ $$ = new TypeSpecifier($1); }
	| SHORT
	{ $$ = new TypeSpecifier($1); }
	| INT
	{ $$ = new TypeSpecifier($1); }
	| LONG
	{ $$ = new TypeSpecifier($1); }
	| FLOAT
	{ $$ = new TypeSpecifier($1); }
	| DOUBLE
	{ $$ = new TypeSpecifier($1); }
	| SIGNED
	{ $$ = new TypeSpecifier($1); }
	| UNSIGNED
	{ $$ = new TypeSpecifier($1); }
	| StructOrUnionSpecifier
	{ $$ = new TypeSpecifier($1); }
	| EnumSpecifier
	{ $$ = new TypeSpecifier($1); }
	| TYPE_NAME
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
    	{ $$ = new Declarator(nullptr,$2);	}

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
   	{ $$ = new DirectAbstractDeclarator($1);	}
   	| '(' ')'
   	{ $$ = new DirectAbstractDeclarator(false);	}
   	| '(' ParameterTypeList ')'
   	{ $$ = new DirectAbstractDeclarator($1);	}
   	| '(' AbstractDeclarator ')'
   	{ $$ = new DirectAbstractDeclarator($1);	}
   	| DirectAbstractDeclarator '[' ']'
   	{ $$ = new DirectAbstractDeclarator($1,true);	}
   	| DirectAbstractDeclarator '[' Constant ']'
   	{ $$ = new DirectAbstractDeclarator($1,$1);	}
   	| DirectAbstractDeclarator '(' ')'
   	{ $$ = new DirectAbstractDeclarator($1,false);	}
   	| DirectAbstractDeclarator '(' ParameterTypeList ')'
   	{ $$ = new DirectAbstractDeclarator($1,$1);	}

AbstractDeclarator : Pointer
	{ $$ = new AbstractDeclarator($1);	}
    	| DirectAbstractDeclarator
    	{ $$ = new AbstractDeclarator($1);	}
    	| Pointer DirectAbstractDeclarator
    	{ $$ = new AbstractDeclarator($1,$2);	}

InitDeclarator : Declarator
	{ $$ = new InitDeclarator($1);		}
	| Declarator '=' Initializer
	{ $$ = new InitDeclarator($1,$2);	}

InitDeclaratorList : InitDeclarator
	{ $$ = new InitDeclaratorList($1);	}
	| InitDeclaratorList ',' InitDeclarator
	{ $1->adDeclarator($3);	$$ = $1;	}

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
	{ $$ = = new StructDeclarator($1);	}
	| Declarator ':' Constant
	{ $$ = = new StructDeclarator($1,$2);	}
	| ':' Constant
	{ $$ = = new StructDeclarator($1);	}

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

