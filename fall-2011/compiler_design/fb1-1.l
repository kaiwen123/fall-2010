/* 
 * This is tutorial lex file, which is used to calculate the number of
 * chars, lines and words. If multiple files are provided to the
 * command line, quantities of each file along with a total quantity 
 * will be printed as the final output. 
 * @author simon guo.
 * @revision 10/12/2011. 
 */
%{
#include <stdio.h>
#include <stdlib.h>
  int chars = 0; 
  int words = 0; 
  int lines = 0; 

  /* total statistics if multiple files. */
  int totchars = 0; 
  int totwords = 0; 
  int totlines = 0; 
%}

%% 

[a-zA-Z]+ { words++; chars += strlen(yytext); }
\n { chars++; lines++; }
. { chars++; }

%% 

main(int argc, char **argv) {
  printf("Number of args : %d\n", argc); 
  if (argc < 2) { 		/*read from stdin.*/
    yylex();
    printf("%8d%8d%8d\n", lines, words, chars);    
    return 0; 
  }
  int i; 
  for (i = 1; i < argc; i++) {
    FILE *f = fopen(argv[i], "r");
    if (! f) {
      perror(argv[i]);
      return (1); 
    }
    yyrestart(f);
    yylex();
    fclose(f);
    printf("%s: %8d%8d%8d\n", argv[i], lines, words, chars);
    totchars += chars; chars = 0; 
    totwords += words; words = 0; 
    totlines += lines; lines = 0;
  }
  /* print the total info if more than one file is provided. */
  if (argc > 1) {
    printf("%8d%8d%8d\n", totlines, totwords, totchars);
    return 0 ;
  }
}
