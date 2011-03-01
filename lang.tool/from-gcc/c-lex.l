D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*

%{
#include <stdio.h>
#include "c-parse.tab.h"

void count();
%}

%%
"/*"			{ comment(); }

"auto"			{ count(); return(SCSPEC); }
"break"			{ count(); return(BREAK); }
"case"			{ count(); return(CASE); }
"char"			{ count(); return(TYPESPEC); }
"const"			{ count(); return(TYPE_QUAL); }
"continue"		{ count(); return(CONTINUE); }
"default"		{ count(); return(DEFAULT); }
"do"			{ count(); return(DO); }
"double"		{ count(); return(TYPESPEC); }
"else"			{ count(); return(ELSE); }
"enum"			{ count(); return(ENUM); }
"extern"		{ count(); return(SCSPEC); }
"float"			{ count(); return(TYPESPEC); }
"for"			{ count(); return(FOR); }
"goto"			{ count(); return(GOTO); }
"if"			{ count(); return(IF); }
"int"			{ count(); return(TYPESPEC); }
"long"			{ count(); return(TYPESPEC); }
"register"		{ count(); return(SCSPEC); }
"return"		{ count(); return(RETURN); }
"short"			{ count(); return(TYPESPEC); }
"signed"		{ count(); return(TYPE_QUAL); }
"sizeof"		{ count(); return(SIZEOF); }
"static"		{ count(); return(STATIC); }
"struct"		{ count(); return(STRUCT); }
"switch"		{ count(); return(SWITCH); }
"typedef"		{ count(); return(TYPENAME); }
"union"			{ count(); return(UNION); }
"unsigned"		{ count(); return(TYPE_QUAL); }
"void"			{ count(); return(TYPESPEC); }
"volatile"		{ count(); return(TYPE_QUAL); }
"while"			{ count(); return(WHILE); }

{L}({L}|{D})*		{ count(); return(check_type()); }

0[xX]{H}+{IS}?		{ count(); return(CONSTANT); }
0[xX]{H}+{IS}?		{ count(); return(CONSTANT); }
0{D}+{IS}?		{ count(); return(CONSTANT); }
0{D}+{IS}?		{ count(); return(CONSTANT); }
{D}+{IS}?		{ count(); return(CONSTANT); }
{D}+{IS}?		{ count(); return(CONSTANT); }
'(\\.|[^\\'])+'		{ count(); return(CONSTANT); }

{D}+{E}{FS}?		{ count(); return(CONSTANT); }
{D}*"."{D}+({E})?{FS}?	{ count(); return(CONSTANT); }
{D}+"."{D}*({E})?{FS}?	{ count(); return(CONSTANT); }

\"(\\.|[^\\"])*\"	{ count(); return(STRING); }

">>="			{ count(); return(ASSIGN); }
"<<="			{ count(); return(ASSIGN); }
"+="			{ count(); return(ASSIGN); }
"-="			{ count(); return(ASSIGN); }
"*="			{ count(); return(ASSIGN); }
"/="			{ count(); return(ASSIGN); }
"%="			{ count(); return(ASSIGN); }
"&="			{ count(); return(ASSIGN); }
"^="			{ count(); return(ASSIGN); }
"|="			{ count(); return(ASSIGN); }
">>"			{ count(); return(RSHIFT); }
"<<"			{ count(); return(LSHIFT); }
"++"			{ count(); return(PLUSPLUS); }
"--"			{ count(); return(MINUSMINUS); }
"->"			{ count(); return(PTR_EXTENT); }
"&&"			{ count(); return(ANDAND); }
"||"			{ count(); return(OROR); }
"<="			{ count(); return(ARITHCOMPARE); }
">="			{ count(); return(ARITHCOMPARE); }
"=="			{ count(); return(EQCOMPARE); }
"!="			{ count(); return(EQCOMPARE); }
";"			{ count(); return(';'); }
"{"			{ count(); return('{'); }
"}"			{ count(); return('}'); }
","			{ count(); return(','); }
":"			{ count(); return(':'); }
"="			{ count(); return('='); }
"("			{ count(); return('('); }
")"			{ count(); return(')'); }
"["			{ count(); return('['); }
"]"			{ count(); return(']'); }
"."			{ count(); return('.'); }
"&"			{ count(); return('&'); }
"!"			{ count(); return('!'); }
"~"			{ count(); return('~'); }
"-"			{ count(); return('-'); }
"+"			{ count(); return('+'); }
"*"			{ count(); return('*'); }
"/"			{ count(); return('/'); }
"%"			{ count(); return('%'); }
"<"			{ count(); return(ARITHCOMPARE); }
">"			{ count(); return(ARITHCOMPARE); }
"^"			{ count(); return('^'); }
"|"			{ count(); return('|'); }
"?"			{ count(); return('?'); }

[ \t\v\n\f]		{ count(); }
.			{ /* ignore bad characters */ }

%%

yywrap()
{
	return(1);
}

comment()
{
  char c;
  
  printf("/*");
 loop:
  while ((c = input()) != '*' && c != 0)
    putchar(c);
  
  if (c != 0)
    putchar(c);
  else
    return;

  if ((c = input()) != '/' && c != 0) {
      unput(c);
      goto loop;
    }
  
  if (c != 0)
    putchar(c);
  else 
    return;
}


int column = 0;

void count()
{
	int i;

	for (i = 0; yytext[i] != '\0'; i++)
		if (yytext[i] == '\n')
			column = 0;
		else if (yytext[i] == '\t')
			column += 8 - (column % 8);
		else
			column++;

	ECHO;
}

int check_type()
{
/*
* pseudo code --- this is what it should check
*
*	if (yytext == type_name)
*		return(TYPE_NAME);
*
*	return(IDENTIFIER);
*/

/*
*	it actually will only return IDENTIFIER
*/

	return(IDENTIFIER);
}
