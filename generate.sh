#!/usr/bin/env bash

mkdir src/generated || echo ""
cd src/generated || exit
lex -+ ../lexer.l
yacc -v -d ../parser.y -o y.tab.cpp

cd .. || exit
cd .. || exit