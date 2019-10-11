/**
 * Created by ilya on 9/27/19.
 */

#include <list>
#include "operator.h"

#ifndef CC_LABS_PARSER_H
#define CC_LABS_PARSER_H

#endif //CC_LABS_PARSER_H

struct AstNode {
};

struct Declaration;
struct FunctionDefinition;

struct ExternalDeclaration : AstNode {
    explicit ExternalDeclaration(Declaration *declaration) : declaration(declaration) {}

    explicit ExternalDeclaration(FunctionDefinition *functionDefinition) : functionDefinition(functionDefinition) {}

    Declaration *declaration{};
    FunctionDefinition *functionDefinition{};
};


struct StorageClassSpecifier {
    int storageClassSpecifierType;

    StorageClassSpecifier(int t) : storageClassSpecifierType(t) {};
};


struct Enumerator;

struct Constant {
    Enumerator *enumerator;
    int constantType;

    explicit Constant(int t) :
            constantType(t) {};

    explicit Constant(Enumerator *e, int t) :
            enumerator(e), constantType(t) {};
};

struct Expression : AstNode {

};

struct PrimaryExpression : Expression {
    PrimaryExpression(char *identifier) : identifier(identifier) {}

    PrimaryExpression(Constant *constant) : constant(constant) {}

    PrimaryExpression(char *stringLiteral, bool literal) : stringLiteral(stringLiteral) {}

    PrimaryExpression(Expression *expression) : expression(expression) {}

    char *identifier;
    Constant *constant;
    char *stringLiteral;
    Expression *expression;


};

struct PostfixExpression : Expression {
    PostfixExpression *expression1;
    Operator oper;
    PrimaryExpression *expression2;

    PostfixExpression(PostfixExpression *expr1, Operator oper, PrimaryExpression *expr2) :
            expression1(expr1), oper(oper), expression2(expr2) {}
};

struct UnaryExpression;

struct CastExpression : Expression {
    Expression *expression{};
    char *typeName{};

    explicit CastExpression(Expression *unaryExpression) :
            Expression(),
            expression(unaryExpression) {}

    CastExpression(char *typeName, Expression *castExpression) :
            Expression(),
            typeName(typeName), expression(castExpression) {}

};

struct UnaryExpression : Expression {
    Operator oper;
    Expression *expr;

    UnaryExpression(Operator oper, Expression *expression) :
            Expression(),
            oper(oper), expr(expression) {}
};

struct MultiplicativeExpression : Expression {
    MultiplicativeExpression *expression1{};
    Operator oper{};
    CastExpression expression2;

    explicit MultiplicativeExpression(CastExpression *castExpression) :
            Expression(),
            expression2(castExpression) {}

    MultiplicativeExpression(MultiplicativeExpression *multExpression, Operator oper, CastExpression *castExpression) :
            Expression(),
            expression1(multExpression), oper(oper), expression2(castExpression) {}
};

struct AdditiveExpression : Expression {
    AdditiveExpression *expression1{};
    Operator oper{};
    MultiplicativeExpression *expression2;

    explicit AdditiveExpression(MultiplicativeExpression *multiplicativeExpression) :
            Expression(),
            expression2(multiplicativeExpression) {}

    AdditiveExpression(AdditiveExpression *additiveExpression, Operator oper,
                       MultiplicativeExpression *multiplicativeExpression) :
            Expression(),
            expression1(additiveExpression), oper(oper), expression2(multiplicativeExpression) {}
};

struct ShiftExpression : Expression {
    ShiftExpression *expression1{};
    Operator oper{};
    AdditiveExpression *expression2;

    explicit ShiftExpression(AdditiveExpression *additiveExpression) : Expression(), expression2(additiveExpression) {}

    ShiftExpression(ShiftExpression *shiftExpression, Operator oper, AdditiveExpression *additiveExpression) :
            Expression(), expression1(shiftExpression), oper(oper), expression2(additiveExpression) {}

};

struct RelationalExpression : Expression {
    RelationalExpression *expression1{};
    Operator oper{};
    ShiftExpression *expression2;

    explicit RelationalExpression(ShiftExpression *shiftExpression) : Expression(), expression2(shiftExpression) {}

    RelationalExpression(RelationalExpression *relationalExpression, Operator oper, ShiftExpression *shiftExpression) :
            Expression(), expression1(relationalExpression), oper(oper), expression2(shiftExpression) {}

};

struct EqualityExpression : Expression {
    EqualityExpression *expression1{};
    Operator oper{};
    RelationalExpression *expression2;

    explicit EqualityExpression(RelationalExpression *relationalExpression)
            : Expression(), expression2(relationalExpression) {}

    EqualityExpression(EqualityExpression *equalityExpression, Operator oper,
                       RelationalExpression *relationalExpression) :
            Expression(), expression1(equalityExpression), oper(oper), expression2(relationalExpression) {}

};

struct AndExpression : Expression {
    AndExpression *expression1;
    EqualityExpression *expression2;

    AndExpression(AndExpression *andExpression, EqualityExpression *equalityExpression) :
            Expression(), expression1(andExpression), expression2(equalityExpression) {}
};

struct ExclusiveOrExpression : Expression {
    ExclusiveOrExpression *expression1;
    AndExpression *expression2;

    ExclusiveOrExpression(ExclusiveOrExpression *expression1, AndExpression *expression2) :
            Expression(), expression1(expression1), expression2(expression2) {}
};

struct InclusiveOrExpression : Expression {
    InclusiveOrExpression(InclusiveOrExpression *expression1, ExclusiveOrExpression *expression2) :
            Expression(), expression1(expression1), expression2(expression2) {}

    InclusiveOrExpression *expression1;
    ExclusiveOrExpression *expression2;
};

struct LogicalAndExpression : Expression {
    LogicalAndExpression(LogicalAndExpression *expression1, InclusiveOrExpression *expression2) :
            Expression(), expression1(expression1), expression2(expression2) {}

    LogicalAndExpression *expression1;
    InclusiveOrExpression *expression2;
};

struct LogicalOrExpression : Expression {
    LogicalOrExpression(LogicalOrExpression *expression1, LogicalAndExpression *expression2) :
            Expression(), expression1(expression1), expression2(expression2) {}

    LogicalOrExpression *expression1;
    LogicalAndExpression *expression2;
};

struct ConditionalExpression : Expression {
    ConditionalExpression(LogicalOrExpression *expression1, Expression *expression2, ConditionalExpression *expression3):
            expression1(expression1), expression2(expression2), expression3(expression3) {}

    LogicalOrExpression *expression1{};
    Expression *expression2{};
    ConditionalExpression *expression3{};
};


struct AssignmentExpression : Expression {
    ConditionalExpression *conditionalExpression;
    UnaryExpression *unaryExpression;
    Operator *oper;
    AssignmentExpression *assignmentExpression;

    AssignmentExpression(ConditionalExpression *expression) :
            conditionalExpression(expression) {};

    AssignmentExpression(UnaryExpression *unary, Operator *oper, AssignmentExpression *assignment) :
            unaryExpression(unary), oper(oper), assignmentExpression(assignment) {};
};

struct ArgumentExpressionList {
    AssignmentExpression *assignmentExpression1;
    AssignmentExpression *assignmentExpression2;

    ArgumentExpressionList(AssignmentExpression *assignmentExpression) :
            assignmentExpression1(assignmentExpression) {};

    ArgumentExpressionList(AssignmentExpression *assignmentExpression1, AssignmentExpression *assignmentExpression2) :
            assignmentExpression1(assignmentExpression1), assignmentExpression2(assignmentExpression2) {};
};

struct Enumerator {
    char *identifier;
    Constant *constant{};

    Enumerator(char *identifier) :
            identifier(identifier) {};

    Enumerator(char *identifier, Constant *constant) :
            identifier(identifier), constant(constant) {};
};

struct EnumeratorList {
    std::list<Enumerator> enumerators;

    EnumeratorList(Enumerator enumerator) {
        enumerators.push_back(enumerator);
    }

    EnumeratorList(EnumeratorList enumeratorList, Enumerator enumerator) : enumerators(enumeratorList.enumerators) {
        enumerators.push_back(enumerator);
    }
};

struct EnumSpecifier {
    char *identifier;
    EnumeratorList *enumeratorList;

    EnumSpecifier(char *identifier) : identifier(identifier) {};

    EnumSpecifier(EnumeratorList *enumeratorList) : enumeratorList(enumeratorList) {}

    EnumSpecifier(char *identifier, EnumeratorList *enumeratorList) :
            identifier(identifier), enumeratorList(enumeratorList) {};
};

struct InitializerList;

struct Initializer {
    AssignmentExpression *expression{};
    InitializerList *list{};

    explicit Initializer(AssignmentExpression *expression) :
            expression(expression) {};

    explicit Initializer(InitializerList *list) :
            list(list) {};
};

struct InitializerList {
    std::list<Initializer *> initializers;

    explicit InitializerList(Initializer *init) {
        initializers.push_back(init);
    }

    void addInitializer(Initializer *initializer) {
        initializers.push_back(initializer);
    }
};

struct IdentifierList {
    std::list<char *> identifiers;

    explicit IdentifierList(char *id) {
        identifiers.push_back(id);
    }

    void addIdentifier(char *id) {
        identifiers.push_back(id);
    }
};

struct TypeQualifierList {
    std::list<int> qualifiers;

    explicit TypeQualifierList(int typeQualifier) {
        qualifiers.push_back(typeQualifier);
    }

    void addTypeQualifier(int typeQualifier) {
        qualifiers.push_back(typeQualifier);
    }
};

struct StructDeclarationList;

//enum StructOrUnion {
//    STRUCT, UNION
//};

struct StructOrUnionSpecifier {
    bool isUnion;
    char *identifier{};
    StructDeclarationList *structDeclarationList{};

    StructOrUnionSpecifier(bool isUnion, char *identifier) :
            isUnion(isUnion), identifier(identifier) {};

    StructOrUnionSpecifier(bool isUnion, StructDeclarationList *structDeclarationList) :
            isUnion(isUnion), structDeclarationList(structDeclarationList) {};

    StructOrUnionSpecifier(bool isUnion, char *id, StructDeclarationList *structDeclarationList) :
            isUnion(isUnion), identifier(id), structDeclarationList(structDeclarationList) {};
};

struct TypeSpecifier {
    int type{};
    bool isUnion;
    EnumSpecifier *enumSpecifier{};

    TypeSpecifier(int type) : type(type) {}

    TypeSpecifier(bool isUnion) : isUnion(isUnion) {}

    TypeSpecifier(EnumSpecifier *enumSpecifier) : enumSpecifier(enumSpecifier) {}
};

struct SpecifierQualifier {
    TypeSpecifier *typeSpecifier{};
    int typeQualifier{};
    char *identifier{};

    SpecifierQualifier(TypeSpecifier *typeSpecifier) : typeSpecifier(typeSpecifier) {}

    SpecifierQualifier(int typeQualifier, char *identifier) :
            typeQualifier(typeQualifier), identifier(identifier) {}
};

struct Pointer {
    Pointer *pointer;
    TypeQualifierList *typeQualifierList;

    Pointer(Pointer *pointer) : pointer(pointer) {}

    Pointer(TypeQualifierList *typeQualifierList) : typeQualifierList(typeQualifierList) {};

    Pointer(Pointer *pointer, TypeQualifierList *typeQualifierList) :
            pointer(pointer), typeQualifierList(typeQualifierList) {};
};

struct DirectDeclarator;

struct Declarator : AstNode {
    Pointer *pointer;
    DirectDeclarator *directDeclarator;

    Declarator(Pointer *pointer, DirectDeclarator *declarator) :
            pointer(pointer), directDeclarator(declarator) {}
};

struct ParameterTypeList;
struct IdentifierList;

struct DirectDeclarator : AstNode {
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

struct DirectAbstractDeclarator : AstNode {
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

struct AbstractDeclarator : AstNode {
    Pointer *pointer{};
    DirectAbstractDeclarator *directAbstractDeclarator{};

    explicit AbstractDeclarator(Pointer *pointer) : pointer(pointer) {}

    explicit AbstractDeclarator(DirectAbstractDeclarator *directAbstractDeclarator1) : directAbstractDeclarator(
            directAbstractDeclarator) {}

    AbstractDeclarator(Pointer *pointer, DirectAbstractDeclarator *directAbstractDeclarator) :
            pointer(pointer), directAbstractDeclarator(directAbstractDeclarator) {}
};

struct Initializer;

struct InitDeclarator : AstNode {
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

struct TypeSpecifier;

struct DeclarationSpecifier : AstNode {
    StorageClassSpecifier *storageClassSpecifier;
    TypeSpecifier *typeSpecifier;
    int typeQualifier;

    explicit DeclarationSpecifier(StorageClassSpecifier *storageClassSpecifier) :
            storageClassSpecifier(storageClassSpecifier) {}

    explicit DeclarationSpecifier(TypeSpecifier *typeSpecifier) : typeSpecifier(typeSpecifier) {}

    explicit DeclarationSpecifier(int typeQualifier) : typeQualifier(typeQualifier) {}
};

struct DeclarationSpecifiers : AstNode {
    std::list<DeclarationSpecifier *> declarationSpecifierList;

    explicit DeclarationSpecifiers(DeclarationSpecifier *declarationSpecifier) {
        addDeclarationSpecifier(declarationSpecifier);
    }

    void addDeclarationSpecifier(DeclarationSpecifier *declarationSpecifier) {
        declarationSpecifierList.push_back(declarationSpecifier);
    }
};

struct Declaration : AstNode {
    DeclarationSpecifiers *declarationSpecifiers;
    InitDeclaratorList *initDeclaratorList{};

    explicit Declaration(DeclarationSpecifiers *declarationSpecifiers) : declarationSpecifiers(declarationSpecifiers) {}

    Declaration(DeclarationSpecifiers *declarationSpecifiers, InitDeclaratorList *initDeclaratorList) :
            declarationSpecifiers(declarationSpecifiers), initDeclaratorList(initDeclaratorList) {}
};

struct DeclarationList : AstNode {
    std::list<Declaration *> declarationList;

    explicit DeclarationList(Declaration *declaration) {
        addDeclaration(declaration);
    }

    void addDeclaration(Declaration *declaration) {
        declarationList.push_back(declaration);
    }
};

struct StructDeclarator : AstNode {
    Declarator *declarator{};
    Constant *constant{};

    explicit StructDeclarator(Declarator *declarator) : declarator(declarator) {}

    explicit StructDeclarator(Constant *constant) : constant(constant) {}

    StructDeclarator(Declarator *declarator, Constant *constant) :
            declarator(declarator), constant(constant) {}
};

struct StructDeclaratorList : AstNode {
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
    std::list<StructDeclaration *> structDeclarationList;

    explicit StructDeclarationList(StructDeclaration *structDeclaration) {
        addStructDeclaration(structDeclaration);
    }

    void addStructDeclaration(StructDeclaration *structDeclaration) {
        this->structDeclarationList.push_back(structDeclaration);
    }
};



//NonEmptyStructDeclarationList

struct ParameterDeclaration : AstNode {
    DeclarationSpecifiers *declarationSpecifiers;
    AbstractDeclarator *abstractDeclarator{};
    Declarator *declarator{};

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

struct Declaration;

struct TypeName : AstNode {
    SpecifierQualifierList *specifierQualifierList;
    AbstractDeclarator *declarator{};

    explicit TypeName(SpecifierQualifierList *list) :
            specifierQualifierList(list) {};

    explicit TypeName(SpecifierQualifierList *list, AbstractDeclarator *decl) :
            specifierQualifierList(list), declarator(decl) {};
};

struct Statement : AstNode {
};

struct ExpressionStatement : Statement {
    Expression expression{};

    ExpressionStatement() {};

    ExpressionStatement(Expression exp) :
            expression(exp) {};
};

struct StatementList : AstNode {
    std::list<Statement *> statements;

    StatementList() {};

    StatementList(Statement *stat) {
        statements.push_back(stat);
    }

    void addStatement(Statement *stat) {
        statements.push_back(stat);
    }
};

//enum JumpStatementType {
//    GOTO, CONTINUE, BREAK, RETURN
//};

struct JumpStatement : Statement {
    char *identifier;
    Expression *expression;
    int jumpStatementType;

    explicit JumpStatement(int t) :
            jumpStatementType(t) {};

    explicit JumpStatement(char *id, int t) :
            identifier(id), jumpStatementType(t) {};

    explicit JumpStatement(Expression *exp, int t) :
            expression(exp), jumpStatementType(t) {};
};

struct LabeledStatement : Statement {
    char *identifier{};
    Constant *constant{};
    Statement *statement;

    explicit LabeledStatement(Statement *stat) :
            statement(stat) {};

    explicit LabeledStatement(char *id, Statement *stat) :
            identifier(id), statement(stat) {};

    explicit LabeledStatement(Constant *c, Statement *stat, int t) :
            constant(c), statement(stat) {};
};

//enum SelectionStatementType {
//    IF = 0, IF_ELSE = 1, SWITCH = 2
//};

struct SelectionStatement : Statement {
    Expression *expression;
    Statement *statement1;
    Statement *statement2{};
    int selectionStatementType;

    explicit SelectionStatement(Expression *exp, Statement *stat, int t) :
            expression(exp), statement1(stat), selectionStatementType(t) {};

    explicit SelectionStatement(Expression *exp, Statement *stat1, Statement *stat2, int t) :
            expression(exp), statement1(stat1), statement2(stat2), selectionStatementType(t) {};
};

//enum IterationStatementType {
//    WHILE = 0, DO_WHILE = 1, FOREACH = 2, FORI = 3
//};

#define DO_WHILE 1
#define FOREACH  2
#define FORI     3

struct IterationStatement : Statement {
    Statement *statement;

    Expression *expression{};
    ExpressionStatement *expressionStatement1{};
    ExpressionStatement *expressionStatement2{};

    int iterationStatementType;

    IterationStatement(Expression *exp, Statement *stat, int t) :
            expression(exp), statement(stat), iterationStatementType(t) {};

    IterationStatement(Statement *stat, Expression *exp, int t) :
            statement(stat), expression(exp), iterationStatementType(t) {};

    IterationStatement(ExpressionStatement *exp1, ExpressionStatement *exp2, Statement *stat, int t) :
            expressionStatement1(exp1), expressionStatement2(exp2), statement(stat),
            iterationStatementType(t) {};

    IterationStatement(ExpressionStatement *expStat1, ExpressionStatement *expStat2, Expression *exp, Statement *stat,
                       int t) :
            expressionStatement1(expStat1), expressionStatement2(expStat2), expression(exp), statement(stat),
            iterationStatementType(t) {};
};

struct CompoundStatement : Statement {
    StatementList *statementList{};
    DeclarationList *declarationList{};

    CompoundStatement() = default;;

    explicit CompoundStatement(StatementList *list) :
            statementList(list) {};

    explicit CompoundStatement(DeclarationList *list) :
            declarationList(list) {};

    explicit CompoundStatement(DeclarationList *declList, StatementList *statList) :
            declarationList(declList), statementList(statList) {};
};

struct FunctionDefinition : AstNode {
    Declarator *declarator;

    CompoundStatement *compoundStatement{};
    DeclarationSpecifiers *declarationSpecifiers{};
    DeclarationList *declarationList{};

    FunctionDefinition(Declarator *decl, CompoundStatement *statement) :
            declarator(decl), compoundStatement(statement) {};

    FunctionDefinition(Declarator *decl, DeclarationList *declList, CompoundStatement *statement) :
            declarator(decl), declarationList(declList), compoundStatement(statement) {};

    FunctionDefinition(DeclarationSpecifiers *specifiers, Declarator *decl, CompoundStatement *statement) :
            declarator(decl), declarationSpecifiers(specifiers), compoundStatement(statement) {};

    FunctionDefinition(DeclarationSpecifiers *specifiers, Declarator *decl, DeclarationList *declList,
                       CompoundStatement *statement) :
            declarator(decl), declarationSpecifiers(specifiers), declarationList(declList),
            compoundStatement(statement) {};
};

struct TranslationUnit : AstNode {
    std::list<ExternalDeclaration *> declarations;

    explicit TranslationUnit(ExternalDeclaration *decl) {
        declarations.push_back(decl);
    }

    void addExternalDeclaration(ExternalDeclaration *decl) {
        declarations.push_back(decl);
    }
};