/**
 * Created by ilya on 9/27/19.
 */

#include <list>
#include <utility>

#ifndef CC_LABS_PARSER_H
#define CC_LABS_PARSER_H

#endif //CC_LABS_PARSER_H

struct AstNode {
};

struct ExternalDeclaration : AstNode {
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

struct Declaration;

struct TypeName : AstNode{
    SpecifierQualifierList specifierQualifierList;
    AbstractDeclarator declarator{};

    explicit TypeName(SpecifierQualifierList list) :
            specifierQualifierList(list) {};

    explicit TypeName(SpecifierQualifierList list, AbstractDeclarator decl) :
            specifierQualifierList(list), declarator(decl) {};
};

struct Statement : AstNode {
};

struct ExpressionStatement : Statement {
    Expression expression{};

    ExpressionStatement() {};

    ExpressionStatement(Expression exp) :
        expression(exp){};
};

struct StatementList : AstNode {
    std::list<Statement> statements;

    StatementList() {};

    StatementList(Statement stat) {
        statements.push_back(stat);
    }

    StatementList(const StatementList &statList, Statement stat) : statements(statList.statements) {
        statements.push_back(stat);
    }
};

enum JumpStatementType {
    GOTO, CONTINUE, BREAK, RETURN
};

struct JumpStatement : Statement {
    char *identifier;
    Expression expression;
    JumpStatementType type;

    explicit JumpStatement(int t) :
            type(JumpStatementType(t)) {};

    explicit JumpStatement(char *id, int t) :
            identifier(id), type(JumpStatementType(t)) {};

    explicit JumpStatement(Expression exp, int t) :
            expression(exp), type(JumpStatementType(t)) {};
};

struct LabeledStatement : Statement {
    char *identifier{};
    Constant constant{};
    Statement statement;

    explicit LabeledStatement(Statement stat) :
            statement(stat) {};

    explicit LabeledStatement(char *id, Statement stat) :
            identifier(id), statement(stat) {};

    explicit LabeledStatement(Constant c, Statement stat, int t) :
            constant(c), statement(stat) {};
};

enum SelectionStatementType {
    IF = 0, IF_ELSE = 1, SWITCH = 2
};

struct SelectionStatement : Statement {
    Expression expression;
    Statement statement1;
    Statement statement2{};
    SelectionStatementType type;

    explicit SelectionStatement(Expression exp, Statement stat, int t) :
            expression(exp), statement1(stat), type(SelectionStatementType(t)) {};

    explicit SelectionStatement(Expression exp, Statement stat1, Statement stat2, int t) :
            expression(exp), statement1(stat1), statement2(stat2), type(SelectionStatementType(t)) {};
};

enum IterationStatementType {
    WHILE = 0, DO_WHILE = 1, FOREACH = 2, FORI = 3
};

struct IterationStatement : Statement {
    Statement statement;

    Expression expression{};
    ExpressionStatement expressionStatement1{};
    ExpressionStatement expressionStatement2{};

    IterationStatementType type;

    IterationStatement(Expression exp, Statement stat, int t) :
            expression(exp), statement(stat), type(IterationStatementType(t)) {};

    IterationStatement(Statement stat, Expression exp, int t) :
            statement(stat), expression(exp), type(IterationStatementType(t)) {};

    IterationStatement(ExpressionStatement exp1, ExpressionStatement exp2, Statement stat, int t) :
            expressionStatement1(exp1), expressionStatement2(exp2), statement(stat), type(IterationStatementType(t)) {};

    IterationStatement(ExpressionStatement expStat1, ExpressionStatement expStat2, Expression exp, Statement stat,
                       int t) :
            expressionStatement1(expStat1), expressionStatement2(expStat2), expression(exp), statement(stat),
            type(IterationStatementType(t)) {};
};

struct CompoundStatement : Statement {
    StatementList statementList{};
    DeclarationList declarationList{};

    CompoundStatement() = default;;

    explicit CompoundStatement(StatementList list) :
            statementList(std::move(list)) {};

    explicit CompoundStatement(DeclarationList list) :
            declarationList(std::move(list)) {};

    explicit CompoundStatement(DeclarationList declList, StatementList statList) :
            declarationList(std::move(declList)), statementList(std::move(statList)) {};
};

struct FunctionDefinition : ExternalDeclaration {
    Declarator declarator;

    CompoundStatement compoundStatement{};
    DeclarationSpecifiers declarationSpecifiers{};
    DeclarationList declarationList{};

    FunctionDefinition(Declarator decl, const CompoundStatement &statement) :
            declarator(decl), compoundStatement(statement) {};

    FunctionDefinition(Declarator decl, DeclarationList declList, const CompoundStatement &statement) :
            declarator(decl), declarationList(declList), compoundStatement(statement) {};

    FunctionDefinition(DeclarationSpecifiers specifiers, Declarator decl, const CompoundStatement &statement) :
            declarator(decl), declarationSpecifiers(specifiers), compoundStatement(statement) {};

    FunctionDefinition(DeclarationSpecifiers specifiers, Declarator decl, DeclarationList declList,
                       const CompoundStatement &statement) :
            declarator(decl), declarationSpecifiers(specifiers), declarationList(declList),
            compoundStatement(statement) {};
};

struct TranslationUnit : AstNode {
    std::list<ExternalDeclaration> declarations;

    explicit TranslationUnit(const ExternalDeclaration &decl) {
        declarations.push_back(decl);
    }

    explicit TranslationUnit(const TranslationUnit &unit, const ExternalDeclaration &decl) {
        declarations = unit.declarations;
        declarations.push_back(decl);
    }
};