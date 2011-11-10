%{
#include<stdlib.h>
void yyerror(char *);
#include"y.tab.h"
%}

%% 

[0-9]+ { yylval = atoi(yytext); return INTEGER; }

[-+\n] return yytext;

[ \t] ;

. yyerror("invalid character. ");

%% 

void yyerror(char *s) {
     printf("%s\n", s);

}