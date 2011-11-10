/* recognize tokens for the calculator and print them out. */
%{
#include "fb1-3.tab.h"
    /* enum yytokentype { */
    /* 	NUMBER = 258,  */
    /* 	ADD = 259,  */
    /* 	SUB = 260,  */
    /* 	MUL = 261,  */
    /* 	DIV = 262,  */
    /* 	ABS = 263,  */
    /* 	EOL = 264 */
    /* }; */
    /* int yylval;  */
%}

%%

"+" {printf("+"); return ADD;}
"-" {printf("-"); return SUB;}
"*" {printf("*"); return MUL;}
"/" {printf("/"); return DIV;}
"|" {printf("|"); return ABS;}
[0-9]+ {printf("%s", yytext); yylval = atoi(yytext); return NUMBER;}
\n {return EOL;}
[ \t] { /* Ignore white spaces. */}
. {ECHO ; printf("mysterious character %s\n", yytext);}

%%

yywrap(){return (1);}
