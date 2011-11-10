/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

extern int comment_no = 0;
%}

/*
 * Define names for regular expressions here.
 */
/* multiple character operators. */
DARROW          "=>"
ASSIGN          "<-"
LE              "<="

/* 
 * Key words. 
 */

CLASS     [Cc][Ll][Aa][Ss][Ss]
IF        [Ii][Ff]
FI        [Ff][Ii]
ELSE      [Ee][Ll][Ss][Ee]
IN        [Ii][Nn]
INHERITS  [Ii][Nn][Hh][Ee][Rr][Ii][Tt][Ss]
LET       [Ll][Ee][Tt]
LOOP      [Ll][Oo][Oo][Pp]
POOL      [Pp][Oo][Oo][Ll]
THEN      [Tt][Hh][Ee][Nn]
WHILE     [Ww][Hh][Ii][Ll][Ee]
CASE      [Cc][Aa][Ss][Ee]	
ESAC      [Ee][Ss][Aa][Cc]
OF        [Oo][Ff]
NEW       [Nn][Ee][Ww]
ISVOID    [Ii][Ss][Vv][Oo][Ii][Dd]
TRUE      t[Rr][Uu][Ee]
FALSE     f[Aa][Uu][Ll][Ss][Ee]

/* ids */
OBJECTID [a-z][a-zA-Z0-9_]*
TYPEID   [A-Z][a-zA-Z0-9_]*

%x 	comment 

%%

 /*
  *  Nested comments
  */
"*)"                    { printf("Unbalanced comment: %s at line %d", yytext, curr_lineno); 
			  return(ERROR); }
"(*"			{ ++comment_no; BEGIN(comment);}
<comment>"("+"*"	{++comment_no;}	
<comment>"*"+")"	{--comment_no; if(comment_no == 0) BEGIN(INITIAL); }

<comment>[^*\n(]*
<comment>"*"+[^)|\n(]*
<comment>\n		{++curr_lineno;}


 /* The multiple-character operators. */
{DARROW}		return (DARROW);
{ASSIGN}                return (ASSIGN); 
{LE}                    return (LE); 
 /*
  * Keywords are case-54Ginsensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{CLASS}	return (CLASS);
{IF}	return (IF);
{FI}	return (FI);
{ELSE}	return (ELSE);
{IN} 	return (IN);
{INHERITS} return (INHERITS);
{LET}   return (LET);
{LOOP}  return (LOOP);
{POOL}  return (POOL); 
{THEN}  return (THEN);
{WHILE} return (WHILE);
{CASE}  return (CASE);
{ESAC}  return (ESAC);
{OF}    return (OF);
{NEW}   return (NEW);
{ISVOID} return (ISVOID);
{TRUE}   { yylval.boolean = 1; return (BOOL_CONST); }
{FALSE}	 { yylval.boolean = 0; return (BOOL_CONST); }

 /* test program. */
[-+*/{}().~,;:@=<] return *yytext; 

 /* integers. */
[0-9]+           { yylval.symbol = inttable.add_string(yytext,yyleng); return (INT_CONST); }

 /* ids. */
{OBJECTID}	 { yylval.symbol = idtable.add_string(yytext,yyleng); return (OBJECTID);}
{TYPEID}	 { yylval.symbol = idtable.add_string(yytext,yyleng); return (TYPEID);}

 /* string constants, now only handles strings within one line. */
\"([^\"\n])*\"	 { yylval.symbol = stringtable.add_string(yytext, strlen(yytext));
	           return (STR_CONST); }

 /* spaces and newlines. */
[\n]             { curr_lineno++;}
[\t ]		 ;

 /* catch other characters. */
.                { sprintf(yylval.error_msg, "Illegal token: '%s' at line %d.", yytext, curr_lineno); 
                   return (ERROR); }

%% %%
