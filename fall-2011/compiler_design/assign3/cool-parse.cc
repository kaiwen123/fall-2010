
/*  A Bison parser, made from cool.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define yyparse cool_yyparse
#define yylex cool_yylex
#define yyerror cool_yyerror
#define yylval cool_yylval
#define yychar cool_yychar
#define yydebug cool_yydebug
#define yynerrs cool_yynerrs
#define	CLASS	258
#define	ELSE	259
#define	FI	260
#define	IF	261
#define	IN	262
#define	INHERITS	263
#define	LET	264
#define	LOOP	265
#define	POOL	266
#define	THEN	267
#define	WHILE	268
#define	CASE	269
#define	ESAC	270
#define	OF	271
#define	DARROW	272
#define	NEW	273
#define	ISVOID	274
#define	STR_CONST	275
#define	INT_CONST	276
#define	BOOL_CONST	277
#define	TYPEID	278
#define	OBJECTID	279
#define	ASSIGN	280
#define	NOT	281
#define	LE	282
#define	ERROR	283

#line 10 "cool.y"

#include <iostream.h>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

extern char *curr_filename;

void yyerror(char *s);        /*  defined below; called for each parse error */
extern int yylex();           /*  the entry point to the lexer  */

 /************************************************************************/
 /*                DONT CHANGE ANYTHING IN THIS SECTION                  */

Program ast_root;	      /* the result of the parse  */
Classes parse_results;        /* for use in semantic analysis */
int omerrs = 0;               /* number of errors in lexing and parsing */

#line 30 "cool.y"
typedef union {
  Boolean boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;
  Features features;
  Formal formal;
  Formals formals;
  Case case_;
  Cases cases;
  Expression expression;
  Expressions expressions;
  char *error_msg;
} YYSTYPE;
#ifndef YYDEBUG
#define YYDEBUG 1
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		123
#define	YYFLAG		-32768
#define	YYNTBASE	44

#define YYTRANSLATE(x) ((unsigned)(x) <= 283 ? yytranslate[x] : 55)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    41,
    42,    32,    34,     2,    35,    29,    33,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    43,    40,    36,
    37,     2,     2,    30,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    38,     2,    39,    31,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     7,    14,    23,    24,    26,    29,    39,
    43,    49,    50,    52,    55,    59,    60,    62,    65,    71,
    72,    74,    77,    81,    88,    97,   102,   110,   116,   120,
   127,   136,   142,   145,   148,   152,   156,   160,   164,   167,
   171,   175,   179,   182,   186,   188,   190,   192
};

static const short yyrhs[] = {    45,
     0,    46,     0,    45,    46,     0,     3,    23,    38,    47,
    39,    40,     0,     3,    23,     8,    23,    38,    47,    39,
    40,     0,     0,    48,     0,    47,    48,     0,    24,    41,
    49,    42,    43,    23,    38,    54,    39,     0,    24,    43,
    23,     0,    24,    43,    23,    25,    54,     0,     0,    50,
     0,    49,    50,     0,    24,    43,    23,     0,     0,    52,
     0,    51,    52,     0,    24,    43,    23,    17,    54,     0,
     0,    54,     0,    53,    54,     0,    24,    25,    54,     0,
    54,    29,    24,    41,    53,    42,     0,    54,    30,    23,
    29,    24,    41,    53,    42,     0,    24,    41,    53,    42,
     0,     6,    54,    12,    54,     4,    54,     5,     0,    13,
    54,    10,    54,    11,     0,    38,    53,    39,     0,     9,
    24,    43,    23,     7,    54,     0,     9,    24,    43,    23,
    25,    54,     7,    54,     0,    14,    54,    16,    51,    15,
     0,    18,    23,     0,    19,    54,     0,    54,    34,    54,
     0,    54,    35,    54,     0,    54,    32,    54,     0,    54,
    33,    54,     0,    31,    54,     0,    54,    36,    54,     0,
    54,    27,    54,     0,    54,    37,    54,     0,    26,    54,
     0,    41,    54,    42,     0,    24,     0,    21,     0,    20,
     0,    22,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   108,   112,   115,   121,   124,   129,   131,   133,   137,   140,
   142,   147,   149,   151,   155,   160,   162,   164,   168,   174,
   176,   178,   182,   185,   187,   189,   191,   193,   195,   197,
   199,   201,   203,   205,   207,   209,   211,   213,   215,   217,
   219,   221,   223,   225,   227,   229,   231,   233
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","CLASS",
"ELSE","FI","IF","IN","INHERITS","LET","LOOP","POOL","THEN","WHILE","CASE","ESAC",
"OF","DARROW","NEW","ISVOID","STR_CONST","INT_CONST","BOOL_CONST","TYPEID","OBJECTID",
"ASSIGN","NOT","LE","ERROR","'.'","'@'","'~'","'*'","'/'","'+'","'-'","'<'",
"'='","'{'","'}'","';'","'('","')'","':'","program","class_list","class","feature_list",
"feature","formal_list","formal","case_list","case_","expr_list","expr", NULL
};
#endif

static const short yyr1[] = {     0,
    44,    45,    45,    46,    46,    47,    47,    47,    48,    48,
    48,    49,    49,    49,    50,    51,    51,    51,    52,    53,
    53,    53,    54,    54,    54,    54,    54,    54,    54,    54,
    54,    54,    54,    54,    54,    54,    54,    54,    54,    54,
    54,    54,    54,    54,    54,    54,    54,    54
};

static const short yyr2[] = {     0,
     1,     1,     2,     6,     8,     0,     1,     2,     9,     3,
     5,     0,     1,     2,     3,     0,     1,     2,     5,     0,
     1,     2,     3,     6,     8,     4,     7,     5,     3,     6,
     8,     5,     2,     2,     3,     3,     3,     3,     2,     3,
     3,     3,     2,     3,     1,     1,     1,     1
};

static const short yydefact[] = {     0,
     0,     1,     2,     0,     3,     0,     6,     0,     0,     0,
     7,     6,    12,     0,     0,     8,     0,     0,     0,    13,
    10,     4,     0,     0,     0,    14,     0,     5,    15,     0,
     0,     0,     0,     0,     0,     0,    47,    46,    48,    45,
     0,     0,    20,     0,    11,     0,     0,     0,     0,     0,
    33,    34,     0,    20,    43,    39,     0,    21,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    16,    23,     0,    29,    22,    44,    41,     0,
     0,    37,    38,    35,    36,    40,    42,     0,     0,     0,
     0,     0,     0,    17,    26,    20,     0,     9,     0,     0,
     0,    28,     0,    32,    18,     0,     0,     0,    30,     0,
     0,    24,    20,    27,     0,     0,     0,    31,    19,    25,
     0,     0,     0
};

static const short yydefgoto[] = {   121,
     2,     3,    10,    11,    19,    20,    93,    94,    57,    58
};

static const short yypact[] = {    10,
    -1,    10,-32768,    -7,-32768,     9,    11,     8,   -25,   -14,
-32768,    11,    25,    28,    -6,-32768,   -12,    12,   -21,-32768,
    27,-32768,    16,    36,    17,-32768,   161,-32768,-32768,    46,
   161,    43,   161,   161,    48,   161,-32768,-32768,-32768,   -17,
   161,   161,   161,   161,   284,    38,   239,    34,   213,   251,
-32768,   295,   161,   161,-32768,   295,   130,   284,   262,   161,
    54,    57,   161,   161,   161,   161,   161,   161,   161,   161,
    58,   161,    59,-32768,    44,-32768,   284,-32768,    56,    53,
    66,   128,   128,    -8,    -8,    56,    56,   273,   174,    -2,
   225,    60,    -4,-32768,-32768,   161,    77,-32768,   161,   161,
   161,-32768,    82,-32768,-32768,    78,    65,   185,   284,   198,
    90,-32768,   161,-32768,   161,   161,   104,   284,   284,-32768,
   108,   111,-32768
};

static const short yypgoto[] = {-32768,
-32768,   110,   102,    37,-32768,    96,-32768,    39,   -52,   -27
};


#define	YYLAST		332


static const short yytable[] = {    45,
     6,    75,    18,    47,   100,    49,    50,    53,    52,     9,
   104,     9,     1,    55,    56,    13,    59,    14,    60,    92,
    25,     4,   101,    54,    15,    74,    23,    67,    68,    77,
     7,     8,    79,    22,     9,    82,    83,    84,    85,    86,
    87,    88,    89,   106,    91,    12,    16,    77,    18,    31,
    21,    27,    32,    16,    24,    28,    33,    34,    29,    30,
   117,    35,    36,    37,    38,    39,    48,    40,    46,    41,
    51,   108,   109,   110,    42,    69,    71,    80,    77,    81,
    90,    43,    92,    31,    44,    95,    32,   118,   119,    77,
    33,    34,    68,    96,    97,    35,    36,    37,    38,    39,
   107,    40,   103,    41,   111,   113,   116,   122,    42,    31,
   123,     5,    32,    17,    26,    43,    33,    34,    44,   112,
     0,    35,    36,    37,    38,    39,     0,    40,     0,    41,
     0,   105,     0,     0,    42,    31,     0,     0,    32,     0,
     0,    43,    33,    34,    44,   120,     0,    35,    36,    37,
    38,    39,     0,    40,    60,    41,     0,     0,     0,     0,
    42,    65,    66,    67,    68,     0,    31,    43,    76,    32,
    44,     0,     0,    33,    34,     0,     0,    99,    35,    36,
    37,    38,    39,     0,    40,     0,    41,     0,     0,   114,
     0,    42,     0,     0,     0,     0,     0,     0,    43,     0,
    60,    44,    61,    62,   115,    63,    64,    65,    66,    67,
    68,    60,     0,    61,    62,     0,    63,    64,    65,    66,
    67,    68,    72,     0,    60,     0,    61,    62,     0,    63,
    64,    65,    66,    67,    68,   102,     0,     0,     0,    60,
     0,    61,    62,     0,    63,    64,    65,    66,    67,    68,
    70,    60,     0,    61,    62,     0,    63,    64,    65,    66,
    67,    68,     0,     0,     0,    60,    73,    61,    62,     0,
    63,    64,    65,    66,    67,    68,     0,    60,     0,    61,
    62,     0,    63,    64,    65,    66,    67,    68,    60,     0,
    61,    62,     0,    63,    64,    65,    66,    67,    68,    60,
     0,    61,    62,    78,    63,    64,    65,    66,    67,    68,
    60,    98,    61,    62,     0,    63,    64,    65,    66,    67,
    68,    60,     0,     0,     0,     0,    63,    64,    65,    66,
    67,    68
};

static const short yycheck[] = {    27,
     8,    54,    24,    31,     7,    33,    34,    25,    36,    24,
    15,    24,     3,    41,    42,    41,    44,    43,    27,    24,
    42,    23,    25,    41,    39,    53,    39,    36,    37,    57,
    38,    23,    60,    40,    24,    63,    64,    65,    66,    67,
    68,    69,    70,    96,    72,    38,    10,    75,    24,     6,
    23,    25,     9,    17,    43,    40,    13,    14,    23,    43,
   113,    18,    19,    20,    21,    22,    24,    24,    23,    26,
    23,    99,   100,   101,    31,    38,    43,    24,   106,    23,
    23,    38,    24,     6,    41,    42,     9,   115,   116,   117,
    13,    14,    37,    41,    29,    18,    19,    20,    21,    22,
    24,    24,    43,    26,    23,    41,    17,     0,    31,     6,
     0,     2,     9,    12,    19,    38,    13,    14,    41,    42,
    -1,    18,    19,    20,    21,    22,    -1,    24,    -1,    26,
    -1,    93,    -1,    -1,    31,     6,    -1,    -1,     9,    -1,
    -1,    38,    13,    14,    41,    42,    -1,    18,    19,    20,
    21,    22,    -1,    24,    27,    26,    -1,    -1,    -1,    -1,
    31,    34,    35,    36,    37,    -1,     6,    38,    39,     9,
    41,    -1,    -1,    13,    14,    -1,    -1,     4,    18,    19,
    20,    21,    22,    -1,    24,    -1,    26,    -1,    -1,     5,
    -1,    31,    -1,    -1,    -1,    -1,    -1,    -1,    38,    -1,
    27,    41,    29,    30,     7,    32,    33,    34,    35,    36,
    37,    27,    -1,    29,    30,    -1,    32,    33,    34,    35,
    36,    37,    10,    -1,    27,    -1,    29,    30,    -1,    32,
    33,    34,    35,    36,    37,    11,    -1,    -1,    -1,    27,
    -1,    29,    30,    -1,    32,    33,    34,    35,    36,    37,
    12,    27,    -1,    29,    30,    -1,    32,    33,    34,    35,
    36,    37,    -1,    -1,    -1,    27,    16,    29,    30,    -1,
    32,    33,    34,    35,    36,    37,    -1,    27,    -1,    29,
    30,    -1,    32,    33,    34,    35,    36,    37,    27,    -1,
    29,    30,    -1,    32,    33,    34,    35,    36,    37,    27,
    -1,    29,    30,    42,    32,    33,    34,    35,    36,    37,
    27,    39,    29,    30,    -1,    32,    33,    34,    35,    36,
    37,    27,    -1,    -1,    -1,    -1,    32,    33,    34,    35,
    36,    37
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/nfs/valhalla/users28/spots/mvanhorn/share/bison.simple"
/* This file comes from bison-1.28.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0 /* No need for malloc.h, which pollutes the namespace;
	 instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
 #pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux /* haible@ilog.fr says this works for HPUX 9.05 and up,
		 and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 217 "/nfs/valhalla/users28/spots/mvanhorn/share/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int yyparse (void *);
#else
int yyparse (void);
#endif
#endif

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;
  int yyfree_stacks = 0;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  if (yyfree_stacks)
	    {
	      free (yyss);
	      free (yyvs);
#ifdef YYLSP_NEEDED
	      free (yyls);
#endif
	    }
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      yyfree_stacks = 1;
#endif
      yyss = (short *) YYSTACK_ALLOC (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1,
		   size * (unsigned int) sizeof (*yyssp));
      yyvs = (YYSTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1,
		   size * (unsigned int) sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1,
		   size * (unsigned int) sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 108 "cool.y"
{ ast_root = program(yyvsp[0].classes); ;
    break;}
case 2:
#line 113 "cool.y"
{ yyval.classes = single_Classes(yyvsp[0].class_);
                  parse_results = yyval.classes; ;
    break;}
case 3:
#line 116 "cool.y"
{ yyval.classes = append_Classes(yyvsp[-1].classes,single_Classes(yyvsp[0].class_)); 
                  parse_results = yyval.classes; ;
    break;}
case 4:
#line 122 "cool.y"
{ yyval.class_ = class_(yyvsp[-4].symbol,idtable.add_string("Object"),yyvsp[-2].features,
			      stringtable.add_string(curr_filename)); ;
    break;}
case 5:
#line 125 "cool.y"
{ yyval.class_ = class_(yyvsp[-6].symbol,yyvsp[-4].symbol,yyvsp[-2].features,stringtable.add_string(curr_filename)); ;
    break;}
case 6:
#line 130 "cool.y"
{  yyval.features = nil_Features(); ;
    break;}
case 7:
#line 132 "cool.y"
{ yyval.features = single_Features(yyvsp[0].feature); ;
    break;}
case 8:
#line 134 "cool.y"
{ yyval.features = append_Features(yyvsp[-1].features,single_Features(yyvsp[0].feature)); ;
    break;}
case 9:
#line 139 "cool.y"
{ yyval.feature = method(yyvsp[-8].symbol, yyvsp[-6].formals, yyvsp[-3].symbol, yyvsp[-1].expression); ;
    break;}
case 10:
#line 141 "cool.y"
{ yyval.feature = attr(yyvsp[-2].symbol, yyvsp[0].symbol, no_expr()); ;
    break;}
case 11:
#line 143 "cool.y"
{ yyval.feature = attr(yyvsp[-4].symbol, yyvsp[-2].symbol, yyvsp[0].expression); ;
    break;}
case 12:
#line 148 "cool.y"
{ yyval.formals = nil_Formals(); ;
    break;}
case 13:
#line 150 "cool.y"
{ yyval.formals = single_Formals(yyvsp[0].formal); ;
    break;}
case 14:
#line 152 "cool.y"
{ yyval.formals = append_Formals(yyvsp[-1].formals,single_Formals(yyvsp[0].formal)); ;
    break;}
case 15:
#line 157 "cool.y"
{ yyval.formal = formal(yyvsp[-2].symbol, yyvsp[0].symbol); ;
    break;}
case 16:
#line 161 "cool.y"
{ yyval.cases = nil_Cases(); ;
    break;}
case 17:
#line 163 "cool.y"
{ yyval.cases = single_Cases(yyvsp[0].case_); ;
    break;}
case 18:
#line 165 "cool.y"
{ yyval.cases = append_Cases(yyvsp[-1].cases, single_Cases(yyvsp[0].case_)); ;
    break;}
case 19:
#line 170 "cool.y"
{ yyval.case_ = branch(yyvsp[-4].symbol, yyvsp[-2].symbol, yyvsp[0].expression); ;
    break;}
case 20:
#line 175 "cool.y"
{ yyval.expressions = nil_Expressions(); ;
    break;}
case 21:
#line 177 "cool.y"
{ yyval.expressions = single_Expressions(yyvsp[0].expression); ;
    break;}
case 22:
#line 179 "cool.y"
{ yyval.expressions = append_Expressions(yyvsp[-1].expressions, single_Expressions(yyvsp[0].expression)); ;
    break;}
case 23:
#line 184 "cool.y"
{ yyval.expression = assign(yyvsp[-2].symbol, yyvsp[0].expression); ;
    break;}
case 24:
#line 186 "cool.y"
{ yyval.expression = dispatch(yyvsp[-5].expression, yyvsp[-3].symbol, yyvsp[-1].expressions); ;
    break;}
case 25:
#line 188 "cool.y"
{ yyval.expression = static_dispatch(yyvsp[-7].expression, yyvsp[-5].symbol, yyvsp[-3].symbol, yyvsp[-1].expressions); ;
    break;}
case 26:
#line 190 "cool.y"
{ yyval.expression = block(yyvsp[-1].expressions); ;
    break;}
case 27:
#line 192 "cool.y"
{ yyval.expression = cond(yyvsp[-5].expression,yyvsp[-3].expression,yyvsp[-1].expression); ;
    break;}
case 28:
#line 194 "cool.y"
{ yyval.expression = loop(yyvsp[-3].expression,yyvsp[-1].expression); ;
    break;}
case 29:
#line 196 "cool.y"
{ yyval.expression = block(yyvsp[-1].expressions); ;
    break;}
case 30:
#line 198 "cool.y"
{ yyval.expression = let(yyvsp[-4].symbol,yyvsp[-2].symbol,no_expr(),yyvsp[0].expression); ;
    break;}
case 31:
#line 200 "cool.y"
{ yyval.expression = let(yyvsp[-6].symbol,yyvsp[-4].symbol,yyvsp[-2].expression,yyvsp[0].expression); ;
    break;}
case 32:
#line 202 "cool.y"
{ yyval.expression = typcase(yyvsp[-3].expression, yyvsp[-1].cases); ;
    break;}
case 33:
#line 204 "cool.y"
{ yyval.expression = new_(yyvsp[0].symbol); ;
    break;}
case 34:
#line 206 "cool.y"
{ yyval.expression = isvoid(yyvsp[0].expression); ;
    break;}
case 35:
#line 208 "cool.y"
{ yyval.expression = plus(yyvsp[-2].expression,yyvsp[0].expression); ;
    break;}
case 36:
#line 210 "cool.y"
{ yyval.expression = sub(yyvsp[-2].expression,yyvsp[0].expression); ;
    break;}
case 37:
#line 212 "cool.y"
{ yyval.expression = mul(yyvsp[-2].expression,yyvsp[0].expression); ;
    break;}
case 38:
#line 214 "cool.y"
{ yyval.expression = divide(yyvsp[-2].expression,yyvsp[0].expression); ;
    break;}
case 39:
#line 216 "cool.y"
{ yyval.expression = neg(yyvsp[0].expression); ;
    break;}
case 40:
#line 218 "cool.y"
{ yyval.expression = lt(yyvsp[-2].expression,yyvsp[0].expression); ;
    break;}
case 41:
#line 220 "cool.y"
{ yyval.expression = leq(yyvsp[-2].expression,yyvsp[0].expression); ;
    break;}
case 42:
#line 222 "cool.y"
{ yyval.expression = eq(yyvsp[-2].expression,yyvsp[0].expression); ;
    break;}
case 43:
#line 224 "cool.y"
{ yyval.expression = comp(yyvsp[0].expression); ;
    break;}
case 44:
#line 226 "cool.y"
{ yyval.expression = yyvsp[-1].expression; ;
    break;}
case 45:
#line 228 "cool.y"
{ yyval.expression = object(yyvsp[0].symbol); ;
    break;}
case 46:
#line 230 "cool.y"
{ yyval.expression = int_const(yyvsp[0].symbol); ;
    break;}
case 47:
#line 232 "cool.y"
{ yyval.expression = string_const(yyvsp[0].symbol); ;
    break;}
case 48:
#line 234 "cool.y"
{ yyval.expression = bool_const(yyvsp[0].boolean); ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 543 "/nfs/valhalla/users28/spots/mvanhorn/share/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;

 yyacceptlab:
  /* YYACCEPT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 0;

 yyabortlab:
  /* YYABORT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 1;
}
#line 238 "cool.y"


/* This function is called automatically when Bison detects a parse error. */
void yyerror(char *s)
{
  extern int curr_lineno;

  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
    << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++;

  if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
}

