%{
#include "y.tab.h"
}%

%%

"//"[^\n]*  { /**line comment*/ }

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

.           { return *yytext;           }

<<EOF>>     { return EOF;               }

%%