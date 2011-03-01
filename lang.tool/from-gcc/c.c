#include <stdio.h>

extern char yytext[];
extern int column;

yyerror(s)
char *s;
{
        fflush(stdout);
        printf("\n%*s\n%*s\n", column, "^", column, s);
}

main()
{
  int yyparse();
  return(yyparse());
}
