OCT     [0-7]
DEC     [0-9]
DEC_NZ  [1-9]
HEX     [a-fA-F0-9]
HEX_P   (0[xX])
L       [a-zA-Z_]
L_DEC   [a-zA-Z_0-9]
FP_E    ([Ee][+-]?{D}+)
FP_P    ([Pp][+-]?{D}+)
F_SPEC  (f|F|l|L)
I_SPEC  (((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))
C_SPEC  (u|U|L)
S_SPEC  (u8|u|U|L)
L_ESC   (\\(['"\?\\abfnrtv]|[0-7]{1,3}|x[a-fA-F0-9]+))
L_W     [ \t\v\n\f]

%{
#include "y.tab.h"
static int GetIdentType();
}%

%%

"/*"*^"*/"  { /*multiline comment*/ }
"//"[^\n]*  { /*  line comment   */ }

asm         { return ASM;       }
auto        { return AUTO;      }
break       { return BREAK;     }
case        { return CASE;      }
char        { return CHAR;      }
const       { return CONST;     }
continue    { return CONTINUE;  }
default     { return DEFAULT;   }
do          { return DO;        }
double      { return DOUBLE;    }
else        { return ELSE;      }
enum        { return ENUM;      }
extern      { return EXTERN;    }
float       { return FLOAT;     }
for         { return FOR;       }
goto        { return GOTO;      }
if          { return IF;        }
inline      { return INLINE;    }
int         { return INT;       }
long        { return LONG;      }
register    { return REGISTER;  }
restrict    { return RESTRICT;  }
return      { return RETURN;    }
short       { return SHORT;     }
signed      { return SIGNED;    }
sizeof      { return SIZEOF;    }
static      { return STATIC;    }
struct      { return STRUCT;    }
switch      { return SWITCH;    }
typedef     { return TYPEDEF;   }
union       { return UNION;     }
unsigned    { return UNSIGNED;  }
void        { return VOID;      }
volatile    { return VOLATILE;  }
while       { return WHILE;     }
_Bool       { return BOOL;      }
_Complex    { return COMPLEX;   }
_Imaginary  { return IMAGINARY; }

">>="       { return OP_R_SHIFT_ASSIGN; }
"<<="       { return OP_L_SHIFT_ASSIGN; }
"->"        { return OP_PTR_ACCESS;     }
"+="        { return OP_ADD_ASSIGN;     }
"-="        { return OP_SUB_ASSIGN;     }
"*="        { return OP_MUL_ASSIGN;     }
"/="        { return OP_DIV_ASSIGN;     }
"%="        { return OP_MOD_ASSIGN;     }
"&="        { return OP_AND_ASSIGN;     }
"^="        { return OP_XOR_ASSIGN;     }
"|="        { return OP_OR_ASSIGN;      }
">>"        { return OP_R_SHIFT;        }
"<<"        { return OP_L_SHIFT;        }
"<="        { return OP_LE_THAN;        }
">="        { return OP_GE_THAN;        }
"!="        { return OP_NEQ_TO;         }
"=="        { return OP_EQ_TO;          }
"++"        { return OP_INC;            }
"--"        { return OP_DEC;            }
"&&"        { return OP_AND;            }
"||"        { return OP_OR;             }

";"			{ return ';'; }
","			{ return ','; }
":"			{ return ':'; }
"."			{ return '.'; }
"{"			{ return '{'; }
"}"			{ return '}'; }
"("			{ return '('; }
")"			{ return ')'; }
"["			{ return '['; }
"]"			{ return ']'; }
"="			{ return '='; }
"+"			{ return '+'; }
"-"			{ return '-'; }
"*"			{ return '*'; }
"/"			{ return '/'; }
"%"			{ return '%'; }
"&"			{ return '&'; }
"~"			{ return '~'; }
"^"			{ return '^'; }
"|"			{ return '|'; }
"!"			{ return '!'; }
"<"			{ return '<'; }
">"			{ return '>'; }
"?"			{ return '?'; }

{L}{L_DEC   { return GetIdentType();    }

{HEX_P}{HEX}+{I_SPEC}?              { return INT_LITERAL;   }
{DEC_NZ}{DEC}*{I_SPEC}?             { return INT_LITERAL;   }
"0"{OCT}*{I_SPEC}?                  { return INT_LITERAL;   }
{C_SPEC}?"'"([^'\\\n]|{L_ESC})+"'"  { return INT_LITERAL;   }

{DEC}+{FP_E}{F_SPEC}?				    { return F_CONSTANT; }
{DEC}*"."{DEC}+{FP_E}?{F_SPEC}?			{ return F_CONSTANT; }
{DEC}+"."{FP_E}?{F_SPEC}?			    { return F_CONSTANT; }
{HEX_P}{HEX}+{FP_P}{F_SPEC}?			{ return F_CONSTANT; }
{HEX_P}{HEX}*"."{HEX}+{FP_P}{F_SPEC}?	{ return F_CONSTANT; }
{HEX_P}{HEX}+"."{FP_P}{F_SPEC}?			{ return F_CONSTANT; }

({S_SPEC}?\"([^"\\\n]|{L_ESC})*\"{L_W}*)+	{ return STRING_LITERAL; }

{WS}+       { /* skip whitespaces  */   }
.           { /* skip unrecognised */   }

<<EOF>>     { return EOF;               }

%%