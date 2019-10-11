/**
 * Created by ilya on 9/27/19.
 */

#include <list>
#include "operator.h"

#ifndef CC_LABS_PARSER_H
#define CC_LABS_PARSER_H

#define TRANSLATION_UNIT        100
#define EXTERNAL_DECLARATION    101
#define FUNCTION_DEFINITION     102
#define COMPOUND_STATEMENT      110
#define ITERATION_STATEMENT     111
#define SELECTION_STATEMENT     112
#define LABELED_STATEMENT       113
#define JUMP_STATEMENT          114
#define EXPRESSION_STATEMENT    115
#define STATEMENT_LIST          120
#define STATEMENT               116
#define TYPE_NAME               121
#define PARAMETER_TYPE_LIST     122
#define PARAMETER_LIST          123
#define PARAMETER_DECLARATION   124
#define STRUCT_DECLARATION_LIST 125
#define STRUCT_DECLARATION      126
#define STRUCT_DECLARATOR_LIST  127
#define STRUCT_DECLARATOR       128
#define DECLARATION_LIST        129
#define DECLARATION             134
#define DECLARATION_SPECIFIER   135
#define DECLARATION_SPECIFIERS  136
#define INIT_DECLARATOR_LIST    137
#define INIT_DECLARATOR         138
#define ABSTRACT_DECLARATOR     139
#define DIRECT_ABSTRACT_DECLARATOR  140
#define DIRECT_DECLARATOR       141
#define DECLARATOR              142
#define POINTER                 143

struct AstNode {
    int type{};

    explicit AstNode(int type) : type(type) {}
};

struct PrimaryExpression;

struct PostfixExpression {
    PostfixExpression *expression1;
    Operator *oper;
    PrimaryExpression *expression2;

    PostfixExpression(PostfixExpression expr1, Operator oper, PrimaryExpression expr2) :
            expression1(expr1), oper(oper), expression2(expr2) {}
};

struct UnaryExpression {
    Operator *oper;
    Expression *expr;
};

struct MultiplicativeExpression {
    MultiplicativeExpression *expression1;
    Operator *oper;
    CastExpression expression2;
};

struct AdditiveExpression {
    AdditiveExpression *expression1;
    Operator *oper;
    MultiplicativeExpression *expression2;
};

struct ShiftExpression {
    ShiftExpression *expression1;
    Operator *oper;
    AdditiveExpression *expression2;
};

struct RelationalExpression {
    RelationalExpression *expression1;
    Operator *oper;
    ShiftExpression *expression2;
};

struct EqualityExpression {
    EqualityExpression *expression1;
    Operator *oper;
    RelationalExpression *expression2;
};

struct AndExpression {
    AndExpression *expression1;
    Operator *oper;
    EqualityExpression *expression2;
};

struct ExclusiveOrExpression {
    ExclusiveOrExpression *expression1;
    Operator *oper;
    AndExpression *expression2;
};

struct InclusiveOrExpression {
    InclusiveOrExpression *expression1;
    Operator *oper;
    ExclusiveOrExpression *expression2;
};

struct LogicalAndExpression {
    LogicalAndExpression *expression1;
    Operator *oper;
    InclusiveOrExpression *expression2;
};

struct LogicalOrExpression {
    LogicalOrExpression *expression1;
    Operator *oper;
    LogicalAndExpression *expression2;
};

struct ConditionalExpression {
    LogicalOrExpression *expression1;
    Operator *oper;
    Expression *expression2;
    ConditionalExpression *expression3;
};

struct Pointer;
struct DirectDeclarator;

struct Declarator : AstNode {
    Pointer *pointer{};
    DirectDeclarator *directDeclarator{};

    Declarator(Pointer *pointer, DirectDeclarator *declarator) :
            AstNode(DECLARATOR),
            pointer(pointer), directDeclarator(declarator) {}
};

struct ParameterTypeList;
struct IdentifierList;
struct Constant;

struct DirectDeclarator : AstNode {
    char *identifier{};
    DirectDeclarator *directDeclarator{};
    ParameterTypeList *parameterTypeList{};
    IdentifierList *identifierList{};
    Constant *constant{};

    explicit DirectDeclarator(char *identifier) :
            AstNode(DIRECT_DECLARATOR),
            identifier(identifier) {}

    explicit DirectDeclarator(DirectDeclarator *directDeclarator) :
            AstNode(DIRECT_DECLARATOR),
            directDeclarator(directDeclarator) {}

    DirectDeclarator(DirectDeclarator *directDeclarator, ParameterTypeList *parameterTypeList) :
            AstNode(DIRECT_DECLARATOR),
            directDeclarator(directDeclarator), parameterTypeList(parameterTypeList) {}

    DirectDeclarator(DirectDeclarator *directDeclarator, IdentifierList *identifierList) :
            AstNode(DIRECT_DECLARATOR),
            directDeclarator(directDeclarator), identifierList(identifierList) {}

    DirectDeclarator(DirectDeclarator *directDeclarator, Constant *constant) :
            AstNode(DIRECT_DECLARATOR),
            directDeclarator(directDeclarator), constant(constant) {}
};

struct AbstractDeclarator;

struct DirectAbstractDeclarator : AstNode {
    Constant *constant{};
    ParameterTypeList *parameterTypeList{};
    AbstractDeclarator *abstractDeclarator{};
    DirectAbstractDeclarator *directAbstractDeclarator{};

    DirectAbstractDeclarator() : AstNode(DIRECT_ABSTRACT_DECLARATOR) {};

    explicit DirectAbstractDeclarator(Constant *constant) :
            AstNode(DIRECT_ABSTRACT_DECLARATOR),
            constant(constant) {}

    explicit DirectAbstractDeclarator(ParameterTypeList *parameterTypeList) :
            AstNode(DIRECT_ABSTRACT_DECLARATOR),
            parameterTypeList(parameterTypeList) {}

    explicit DirectAbstractDeclarator(AbstractDeclarator *abstractDeclarator) :
            AstNode(DIRECT_ABSTRACT_DECLARATOR),
            abstractDeclarator(abstractDeclarator) {}

    DirectAbstractDeclarator(DirectAbstractDeclarator *directAbstractDeclarator, Constant *constant) :
            AstNode(DIRECT_ABSTRACT_DECLARATOR),
            directAbstractDeclarator(directAbstractDeclarator), constant(constant) {}

    DirectAbstractDeclarator(DirectAbstractDeclarator *directAbstractDeclarator, ParameterTypeList *parameterTypeList) :
            AstNode(DIRECT_ABSTRACT_DECLARATOR),
            directAbstractDeclarator(directAbstractDeclarator), parameterTypeList(parameterTypeList) {}

};

struct AbstractDeclarator : AstNode {
    Pointer *pointer{};
    DirectAbstractDeclarator *directAbstractDeclarator{};

    explicit AbstractDeclarator(Pointer *pointer) :
            AstNode(ABSTRACT_DECLARATOR),
            pointer(pointer) {}

    explicit AbstractDeclarator(DirectAbstractDeclarator *directAbstractDeclarator) :
            AstNode(ABSTRACT_DECLARATOR),
            directAbstractDeclarator(directAbstractDeclarator) {}

    AbstractDeclarator(Pointer *pointer, DirectAbstractDeclarator *directAbstractDeclarator) :
            AstNode(ABSTRACT_DECLARATOR),
            pointer(pointer), directAbstractDeclarator(directAbstractDeclarator) {}
};

struct Initializer;

struct InitDeclarator : AstNode {
    Declarator *declarator{};
    Initializer *initializer{};

    explicit InitDeclarator(Declarator *declarator) :
            AstNode(INIT_DECLARATOR),
            declarator(declarator) {}

    InitDeclarator(Declarator *declarator, Initializer *initializer)
            : AstNode(INIT_DECLARATOR),
              declarator(declarator), initializer(initializer) {}
};

struct InitDeclaratorList : AstNode {
    std::list<InitDeclarator *> declaratorList;

    explicit InitDeclaratorList(InitDeclarator *declarator) : AstNode(INIT_DECLARATOR_LIST) {
        this->addDeclarator(declarator);
    }

    void addDeclarator(InitDeclarator *initDeclarator) {
        this->declaratorList.push_back(initDeclarator);
    }
};

struct StorageClassSpecifier;
struct TypeSpecifier;
struct TypeQualifier;

struct DeclarationSpecifier : AstNode {
    StorageClassSpecifier *storageClassSpecifier{};
    TypeSpecifier *typeSpecifier{};
    TypeQualifier *typeQualifier{};

    explicit DeclarationSpecifier(StorageClassSpecifier *storageClassSpecifier) :
            AstNode(DECLARATION_SPECIFIER),
            storageClassSpecifier(storageClassSpecifier) {}

    explicit DeclarationSpecifier(TypeSpecifier *typeSpecifier) :
            AstNode(DECLARATION_SPECIFIER),
            typeSpecifier(typeSpecifier) {}

    explicit DeclarationSpecifier(TypeQualifier *typeQualifier) :
            AstNode(DECLARATION_SPECIFIER),
            typeQualifier(typeQualifier) {}
};

struct DeclarationSpecifiers : AstNode {
    std::list<DeclarationSpecifier *> declarationSpecifierList;

    explicit DeclarationSpecifiers(DeclarationSpecifier *declarationSpecifier) :
            AstNode(DECLARATION_SPECIFIERS) {
        addDeclarationSpecifier(declarationSpecifier);
    }

    void addDeclarationSpecifier(DeclarationSpecifier *declarationSpecifier) {
        declarationSpecifierList.push_back(declarationSpecifier);
    }
};

struct Declaration : AstNode {
    DeclarationSpecifiers *declarationSpecifiers;
    InitDeclaratorList *initDeclaratorList{};

    explicit Declaration(DeclarationSpecifiers *declarationSpecifiers) :
            AstNode(DECLARATION),
            declarationSpecifiers(declarationSpecifiers) {}

    Declaration(DeclarationSpecifiers *declarationSpecifiers, InitDeclaratorList *initDeclaratorList) :
            AstNode(DECLARATION),
            declarationSpecifiers(declarationSpecifiers), initDeclaratorList(initDeclaratorList) {}
};

struct DeclarationList : AstNode {
    std::list<Declaration *> declarationList;

    explicit DeclarationList(Declaration *declaration) :
            AstNode(DECLARATION_LIST) {
        addDeclaration(declaration);
    }

    void addDeclaration(Declaration *declaration) {
        declarationList.push_back(declaration);
    }
};

struct StructDeclarator : AstNode {
    Declarator *declarator{};
    Constant *constant{};

    explicit StructDeclarator(Declarator *declarator) :
            AstNode(STRUCT_DECLARATOR), declarator(declarator) {}

    explicit StructDeclarator(Constant *constant) :
            AstNode(STRUCT_DECLARATOR), constant(constant) {}

    StructDeclarator(Declarator *declarator, Constant *constant) :
            AstNode(STRUCT_DECLARATOR), declarator(declarator), constant(constant) {}
};

struct StructDeclaratorList : AstNode {
    std::list<StructDeclarator *> structDeclaratorList;

    explicit StructDeclaratorList(StructDeclarator *structDeclarator) :
            AstNode(STRUCT_DECLARATOR_LIST) {
        addStructDeclarator(structDeclarator);
    }

    void addStructDeclarator(StructDeclarator *structDeclarator) {
        structDeclaratorList.push_back(structDeclarator);
    }
};


struct SpecifierQualifierList;

struct StructDeclaration : AstNode {
    SpecifierQualifierList *specifierQualifierList;
    StructDeclaratorList *structDeclaratorList;

    StructDeclaration(SpecifierQualifierList *specifierQualifierList, StructDeclaratorList *structDeclaratorList) :
            AstNode(STRUCT_DECLARATION),
            specifierQualifierList(specifierQualifierList), structDeclaratorList(structDeclaratorList) {}
};

struct StructDeclarationList : AstNode {
    std::list<StructDeclaration *> structDeclarationList;

    explicit StructDeclarationList(StructDeclaration *structDeclaration) :
            AstNode(STRUCT_DECLARATION_LIST) {
        addStructDeclaration(structDeclaration);
    }

    void addStructDeclaration(StructDeclaration *structDeclaration) {
        this->structDeclarationList.push_back(structDeclaration);
    }
};



//NonEmptyStructDeclarationList

struct ParameterDeclaration : AstNode {
    DeclarationSpecifiers *declarationSpecifiers;
    AbstractDeclarator *abstractDeclarator;
    Declarator *declarator;

    explicit ParameterDeclaration(DeclarationSpecifiers *declarationSpecifiers) :
            AstNode(PARAMETER_DECLARATION),
            declarationSpecifiers(declarationSpecifiers) {}

    ParameterDeclaration(DeclarationSpecifiers *declarationSpecifiers, Declarator *declarator) :
            AstNode(PARAMETER_DECLARATION),
            declarationSpecifiers(declarationSpecifiers), declarator(declarator) {}

    ParameterDeclaration(DeclarationSpecifiers *declarationSpecifiers, AbstractDeclarator *abstractDeclarator) :
            AstNode(PARAMETER_DECLARATION),
            declarationSpecifiers(declarationSpecifiers), abstractDeclarator(abstractDeclarator) {}

};

struct ParameterList : AstNode {
    std::list<ParameterDeclaration *> parameterDeclarationList;

    explicit ParameterList(ParameterDeclaration *parameterDeclaration) : AstNode(PARAMETER_LIST) {
        addParameterDeclaration(parameterDeclaration);
    }

    void addParameterDeclaration(ParameterDeclaration *parameterDeclaration) {
        this->parameterDeclarationList.push_back(parameterDeclaration);
    }
};

struct ParameterTypeList : AstNode {
    ParameterList *parameterList;
    bool withEllipsis;

    explicit ParameterTypeList(ParameterList *parameterList, bool withEllipsis) :
            AstNode(PARAMETER_TYPE_LIST),
            parameterList(parameterList),
            withEllipsis(withEllipsis) {}

};

struct Declaration;

struct TypeName {
    SpecifierQualifierList specifierQualifierList;
    AbstractDeclarator declarator{};

    // TODO Fix initialization
    TypeName(SpecifierQualifierList list) :
            specifierQualifierList(list) {};

    // TODO Fix initialization
    TypeName(SpecifierQualifierList list, AbstractDeclarator decl) :
            specifierQualifierList(list), declarator(decl) {};
};

struct Statement {
};

struct ExpressionStatement : Statement {
    Expression expression{};
};

struct StatementList {
    std::list<Statement> statements;
};


struct JumpStatement : Statement {
    int keyword;
    char *identifier;
    Expression expression;
};

// TODO: keyword?
struct LabeledStatement : Statement {
    char *identifier{};
    Constant constant{};
    Statement statement;
};

// TODO keyword?
struct SelectionStatement : Statement {
    Expression expression;
    Statement statement1;
    Statement statement2;
};

// TODO keyword?
struct IterationStatement : Statement {

    Expression expression;
    Statement statement;
    ExpressionStatement
};

struct CompoundStatement : Statement {
    StatementList statementList;
    DeclarationList declarationList;
};

struct FunctionDefinition {
    Declarator declarator;

    CompoundStatement compoundStatement{};
    DeclarationSpecifiers declarationSpecifiers{};
    DeclarationList declarationList{};

    FunctionDefinition(Declarator decl, CompoundStatement statement) :
            declarator(decl), compoundStatement(statement) {};

    FunctionDefinition(Declarator decl, DeclarationList declList, CompoundStatement statement) :
            declarator(decl), declarationList(declList), compoundStatement(statement) {};

    FunctionDefinition(DeclarationSpecifiers specifiers, Declarator decl, CompoundStatement statement) :
            declarator(decl), declarationSpecifiers(specifiers), compoundStatement(statement) {};

    FunctionDefinition(DeclarationSpecifiers specifiers, Declarator decl, DeclarationList declList,
                       CompoundStatement statement) :
            declarator(decl), declarationSpecifiers(specifiers), declarationList(declList),
            compoundStatement(statement) {};
};

struct ExternalDeclaration : AstNode {
    Declaration declaration{};
    FunctionDefinition funcDefinition{};

    ExternalDeclaration(Declaration decl) :
            declaration(decl) {}

    ExternalDeclaration(FunctionDefinition definition) :
            funcDefinition(definition) {}
};

struct TranslationUnit : AstNode {
    std::list<ExternalDeclaration> declarations;

};

#endif //CC_LABS_PARSER_H