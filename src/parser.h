/**
 * Created by ilya on 9/27/19.
 */

#include <list>

#ifndef CC_LABS_PARSER_H
#define CC_LABS_PARSER_H

#endif //CC_LABS_PARSER_H

struct AstNode{};

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

struct Declaration;

struct TypeName{
    SpecifierQualifierList specifierQualifierList;
    AbstractDeclarator declarator{};

    // TODO Fix initialization
    TypeName(SpecifierQualifierList list) :
        specifierQualifierList(list){};

    // TODO Fix initialization
    TypeName(SpecifierQualifierList list, AbstractDeclarator decl) :
        specifierQualifierList(list), declarator(decl){};
};

struct Statement{
};

struct ExpressionStatement : Statement{
    Expression expression{};
};

struct StatementList{
    std::list<Statement> statements;
};


struct JumpStatement : Statement{
    int keyword;
    char* identifier;
    Expression expression;
};

// TODO: keyword?
struct LabeledStatement : Statement{
    char* identifier{};
    Constant constant{};
    Statement statement;
};

// TODO keyword?
struct SelectionStatement : Statement{
    Expression expression;
    Statement statement1;
    Statement statement2;
};

// TODO keyword?
struct IterationStatement : Statement{

    Expression expression;
    Statement statement;
    ExpressionStatement
};

struct CompoundStatement : Statement{
    StatementList statementList;
    DeclarationList declarationList;
};

struct FunctionDefinition {
    Declarator declarator;

    CompoundStatement compoundStatement{};
    DeclarationSpecifiers declarationSpecifiers{};
    DeclarationList declarationList{};

    FunctionDefinition(Declarator decl, CompoundStatement statement) :
        declarator(decl), compoundStatement(statement){};

    FunctionDefinition(Declarator decl, DeclarationList declList, CompoundStatement statement) :
        declarator(decl), declarationList(declList), compoundStatement(statement){};

    FunctionDefinition(DeclarationSpecifiers specifiers, Declarator decl, CompoundStatement statement) :
        declarator(decl), declarationSpecifiers(specifiers), compoundStatement(statement){};

    FunctionDefinition(DeclarationSpecifiers specifiers, Declarator decl, DeclarationList declList, CompoundStatement statement) :
        declarator(decl), declarationSpecifiers(specifiers), declarationList(declList), compoundStatement(statement){};
};

struct ExternalDeclaration : AstNode{
    Declaration declaration{};
    FunctionDefinition funcDefinition{};

    ExternalDeclaration(Declaration decl) :
        declaration(decl){}

    ExternalDeclaration(FunctionDefinition definition) :
        funcDefinition(definition){}
};

struct TranslationUnit : AstNode{
    std::list<ExternalDeclaration> declarations;
};