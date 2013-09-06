ID = [[:alpha:]_+\-*%$&][[:alnum:]_+\-*%$&]*
DEC = [0-9]
HEX = [0-9a-fA-f]
BIN = [01]
%{
  #include <string.h>
  int_to_str(const char* str,int base){strtol(str,NULL,base)};
  word_to_str(const char* str,int base){strtoul(str,NULL,base)};
  real_to_str(const char* str,int base){strtod(str,NULL,base)};
%}
%%
[\x20\t\n]/*whitespace*/
{ID} yylval = strdup(yytext); return TOK_ID;
/*literal numbers*/
{DEC}+"."{DEC}* yylval=real_to_str(yytext}; return TOK_REAL;
{DEC}+ yylval=int_to_str(yytext,10); return TOK_INT
"0x"{HEX}+ yylval=int_to_str(yytext[2],16); return TOK_INT;
"0b"{BIN}+ yylval=int_to_str(yytext[2],2); return TOK_INT;
"if" return TOK_IF;
"do" return TOK_DO;
"def"|"define" return TOK_DEF;
"progn" return TOK_PROGN;
"let" return TOK_LET;
"tagbody" return TOK_TAG;
"go" return TOK_GO;
"quote"|'\'' return TOK_QUOTE;
%%
