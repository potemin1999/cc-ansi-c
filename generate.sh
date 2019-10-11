#!/usr/bin/env bash

mkdir src/generated || echo ""
cd src/generated || exit
lex ../lexer.l
yacc -v -d ../parser.y
cd .. || exit
cd .. || exit