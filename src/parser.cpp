/**
 * Created by Ilya Potemin on 10/11/19.
 *
 * @author Daniil Dvoryanov
 * @author Nicola Novarlic
 * @author Ilya Potemin
 */

#include <cstdio>
#include <FlexLexer.h>
#include "parser.h"
#include "generated/y.tab.hpp"

//extern "C" {

int GetIdentType() {
    return 0;
}

void yyerror(const char *str) {
    fprintf(stdout, "Parsing error: %s\n", str);
}

yyFlexLexer lexer;

void yylex(){
    lexer.yylex();
}

int main(int argc, const char **argv) {
    yyparse();
}

//}