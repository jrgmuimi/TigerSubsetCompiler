%{
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TigerSubsetCompiler.tab.h"

const char *token_err_msg = "Lexical Error: Line %d\n";
int lex_err_flag = 0;

char* strdup(char *str)
{
    int len = (strlen(str) + 1);
    char *dup = calloc(len, sizeof(char));
    assert(dup);
    strcpy(dup, str);
    return dup;
}

%}

 /* regular definitions */
number 0|[1-9][0-9]*
id [a-zA-Z_][a-zA-Z_0-9]*
string \"(\\\"|[^"\n])*\"
comment "//"[^\n]*

%option yylineno
%option never-interactive

%%
 /* keywords */
let 	{ return(LET); }
in 	{ return(IN); }
end 	{ return(END); }
var 	{ return(VAR); }
function	{ return(FUNCTION); }
printint 	{ return(PRINTINT); }
printstring 	{ return(PRINTSTRING); }
getint {return(GETINT); }
return 	{ return(RETURN); }
if 	{ return(IF); }
then { return(THEN); }
else 	{ return(ELSE); }
for { return(FOR); }
to 	{ return(TO); }
do 	{ return(DO); }
void { return(VOID); }
int { return(INT_TYPE); }
string { return(STRING_TYPE);}

 /* operators */
\+ 	{ return(ADD); }
-	{ return(MINUS); }
\* 	{ return(MUL); }
\/ 	{ return(DIV); }
== 	{ return(EQ); }
\< 	{ return(LT); }
\<= 	{ return(LE); }
\> 	{ return(GT); }
\>= 	{ return(GE); }
:= 	{ return(ASSIGN); }
\<\> 	{ return(NE); }

 /* punctuations */
\( 	{ return(LP); }
\) 	{ return(RP); }
: 	{ return(COLON); }
, 	{ return(COMMA); }
=   { return(SEQ); }
;   { return(SEMICOLON); }

{id}	{ yylval.val = strdup(yytext); return(ID); }

{number} { yylval.val = strdup(yytext); return(NUMBER); } 
{string} { yylval.val = strdup(yytext); return(STRING); }
{comment} { ; }
[ \t\n]  { ; }

. 	{ 
		fprintf(stderr, token_err_msg, yylineno); 
		lex_err_flag = 1;
    	exit(-1);
	}

%%

