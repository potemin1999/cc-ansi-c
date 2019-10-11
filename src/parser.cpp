//
// Created by ilya on 10/11/19.
//

#include <cstdio>
#include "parser.h"
#include "generated/y.tab.hpp"

extern "C" {

int GetIdentType() {
    return 0;
}

void yyerror(const char *str) {
    fprintf(stdout, "Parsing error: %s\n", str);
}

int yywrap() {
    return 1;
}

void yylex();

int main(int argc, const char **argv) {
    yyparse();
}

}