/**
 * Created by ilya on 9/27/19.
 */

#include <list>
#include "operator.h"

#ifndef CC_LABS_PARSER_H
#define CC_LABS_PARSER_H

typedef void* AstNode;

#endif //CC_LABS_PARSER_H

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

struct Declarator: AstNode {
    Pointer *pointer;
    DirectDeclarator *directDeclarator;

    Declarator(Pointer *pointer, DirectDeclarator *declarator) :
            pointer(pointer), directDeclarator(declarator) {}
};

struct ParameterTypeList;
struct IdentifierList;
struct Constant;

struct DirectDeclarator : AstNode{
    char *identifier{};
    DirectDeclarator *directDeclarator{};
    union {
        ParameterTypeList *parameterTypeList;
        IdentifierList *identifierList{};
        Constant *constant;
    };

    explicit DirectDeclarator(char *identifier) : identifier(identifier) {}

    explicit DirectDeclarator(DirectDeclarator *directDeclarator) : directDeclarator(directDeclarator) {}

    DirectDeclarator(DirectDeclarator *directDeclarator, ParameterTypeList *parameterTypeList) : directDeclarator(
            directDeclarator), parameterTypeList(parameterTypeList) {}

    DirectDeclarator(DirectDeclarator *directDeclarator, IdentifierList *identifierList) : directDeclarator(
            directDeclarator), identifierList(identifierList) {}

    DirectDeclarator(DirectDeclarator *directDeclarator, Constant *constant) : directDeclarator(
            directDeclarator), constant(constant) {}
};

struct AbstractDeclarator;

struct DirectAbstractDeclarator : AstNode{
    Constant *constant{};
    ParameterTypeList *parameterTypeList{};
    AbstractDeclarator *abstractDeclarator{};
    DirectAbstractDeclarator *directAbstractDeclarator{};

    DirectAbstractDeclarator() = default;

    explicit DirectAbstractDeclarator(Constant *constant) : constant(constant) {}

    DirectAbstractDeclarator(ParameterTypeList *parameterTypeList) : parameterTypeList(parameterTypeList) {}

    DirectAbstractDeclarator(AbstractDeclarator *abstractDeclarator) : abstractDeclarator(abstractDeclarator) {}

    DirectAbstractDeclarator(DirectAbstractDeclarator *directAbstractDeclarator, Constant *constant)
            : directAbstractDeclarator(directAbstractDeclarator), constant(constant) {}

    DirectAbstractDeclarator(DirectAbstractDeclarator *directAbstractDeclarator, ParameterTypeList *parameterTypeList)
            : directAbstractDeclarator(directAbstractDeclarator), parameterTypeList(parameterTypeList) {}

};

struct AbstractDeclarator : AstNode{
    Pointer *pointer{};
    DirectAbstractDeclarator *directAbstractDeclarator{};

    explicit AbstractDeclarator(Pointer *pointer) : pointer(pointer) {}

    explicit AbstractDeclarator(DirectAbstractDeclarator *directAbstractDeclarator1) : directAbstractDeclarator(
            directAbstractDeclarator) {}

    AbstractDeclarator(Pointer *pointer, DirectAbstractDeclarator *directAbstractDeclarator) :
            pointer(pointer), directAbstractDeclarator(directAbstractDeclarator) {}
};

struct Initializer;

struct InitDeclarator : AstNode{
    Declarator *declarator;
    Initializer *initializer;

    explicit InitDeclarator(Declarator *declarator) : declarator(declarator) {}

    InitDeclarator(Declarator *declarator, Initializer *initializer)
            : declarator(declarator), initializer(initializer) {}
};

struct InitDeclaratorList {
    std::list<InitDeclarator *> declaratorList;

    explicit InitDeclaratorList(InitDeclarator *declarator) {
        this->addDeclarator(declarator);
    }

    void addDeclarator(InitDeclarator *initDeclarator) {
        this->declaratorList.push_back(initDeclarator);
    }
};

struct StorageClassSpecifier;
struct TypeSpecifier;
struct TypeQualifier;

struct DeclarationSpecifier : AstNode{
    StorageClassSpecifier *storageClassSpecifier;
    TypeSpecifier *typeSpecifier;
    TypeQualifier *typeQualifier;

    explicit DeclarationSpecifier(StorageClassSpecifier *storageClassSpecifier) :
            storageClassSpecifier(storageClassSpecifier) {}

    explicit DeclarationSpecifier(TypeSpecifier *typeSpecifier) : typeSpecifier(typeSpecifier) {}

    explicit DeclarationSpecifier(TypeQualifier *typeQualifier) : typeQualifier(typeQualifier) {}
};

struct DeclarationSpecifiers: AstNode {
    std::list<DeclarationSpecifier *> declarationSpecifierList;

    explicit DeclarationSpecifiers(DeclarationSpecifier *declarationSpecifier) {
        addDeclarationSpecifier(declarationSpecifier);
    }

    void addDeclarationSpecifier(DeclarationSpecifier *declarationSpecifier) {
        declarationSpecifierList.push_back(declarationSpecifier);
    }
};

struct Declaration : AstNode{
    DeclarationSpecifiers *declarationSpecifiers;
    InitDeclaratorList *initDeclaratorList{};

    explicit Declaration(DeclarationSpecifiers *declarationSpecifiers) : declarationSpecifiers(declarationSpecifiers) {}

    Declaration(DeclarationSpecifiers *declarationSpecifiers, InitDeclaratorList *initDeclaratorList) :
            declarationSpecifiers(declarationSpecifiers), initDeclaratorList(initDeclaratorList) {}
};

struct DeclarationList : AstNode{
    std::list<Declaration *> declarationList;

    explicit DeclarationList(Declaration *declaration) {
        addDeclaration(declaration);
    }

    void addDeclaration(Declaration *declaration) {
        declarationList.push_back(declaration);
    }
};

struct StructDeclarator : AstNode{
    Declarator *declarator{};
    Constant *constant{};

    explicit StructDeclarator(Declarator *declarator) : declarator(declarator) {}

    explicit StructDeclarator(Constant *constant) : constant(constant) {}

    StructDeclarator(Declarator *declarator, Constant *constant) :
            declarator(declarator), constant(constant) {}
};

struct StructDeclaratorList: AstNode {
    std::list<StructDeclarator *> structDeclaratorList;

    explicit StructDeclaratorList(StructDeclarator *structDeclarator) {
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
            specifierQualifierList(specifierQualifierList), structDeclaratorList(structDeclaratorList) {}
};

struct StructDeclarationList : AstNode {
    std::list<StructDeclaration> structDeclarationList;
    explicit StructDeclarationList(StructDeclaration structDeclaration) {
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

    explicit ParameterDeclaration(DeclarationSpecifiers *declarationSpecifiers) : declarationSpecifiers(
            declarationSpecifiers) {}

    ParameterDeclaration(DeclarationSpecifiers *declarationSpecifiers, Declarator *declarator) :
            declarationSpecifiers(declarationSpecifiers), declarator(declarator) {}

    ParameterDeclaration(DeclarationSpecifiers *declarationSpecifiers, AbstractDeclarator *abstractDeclarator) :
            declarationSpecifiers(declarationSpecifiers), abstractDeclarator(abstractDeclarator) {}

};

struct ParameterList : AstNode {
    std::list<ParameterDeclaration *> parameterDeclarationList;

    explicit ParameterList(ParameterDeclaration *parameterDeclaration) {
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
            parameterList(parameterList), withEllipsis(withEllipsis) {}
};