/**
 * Created by ilya on 9/27/19.
 */

#ifndef CC_LABS_PARSER_H
#define CC_LABS_PARSER_H

typedef void* AstNode;

#endif //CC_LABS_PARSER_H

struct PostfixExpression {
  PostfixExpression * expression1;
  Operator *oper;
  PrimaryExpression * expression2;
};

struct UnaryExpression {
 Operator *oper;
 Expression *expr;
};

struct MultiplicativeExpression {
  MultiplicativeExpression * expression1;
  Operator *oper;
  CastExpression expression2;
};

struct AdditiveExpression {
  AdditiveExpression * expression1;
  Operator *oper;
  MultiplicativeExpression * expression2;
};

struct ShiftExpression {
  ShiftExpression * expression1;
  Operator *oper;
  AdditiveExpression * expression2;
};

struct RelationalExpression {
    RelationalExpression * expression1;
    Operator * oper;
    ShiftExpression * expression2;
};

struct EqualityExpression {
    EqualityExpression * expression1;
    Operator * oper;
    RelationalExpression * expression2;
};

struct AndExpression {
    AndExpression * expression1;
    Operator * oper;
    EqualityExpression * expression2;
};

struct ExclusiveOrExpression {
    ExclusiveOrExpression * expression1;
    Operator * oper;
    AndExpression * expression2;
};

struct InclusiveOrExpression {
    InclusiveOrExpression * expression1;
    Operator * oper;
    ExclusiveOrExpression * expression2;
};

struct LogicalAndExpression {
    LogicalAndExpression * expression1;
    Operator * oper;
    InclusiveOrExpression * expression2;
};

struct LogicalOrExpression {
    LogicalOrExpression * expression1;
    Operator * oper;
    LogicalAndExpression* expression2;
};

struct ConditionalExpression {
    LogicalOrExpression * expression1;
    Operator * oper;
    Expression * expression2;
    ConditionalExpression * expression3;
};
