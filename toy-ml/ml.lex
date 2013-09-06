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
[\x20\t]
{ID} yylval = strdup(yytext); return TOK_ID;
/*literal numbers*/
{DEC}+"."{DEC}* yylval=real_to_str(yytext}; return TOK_REAL;
{DEC}+ yylval=int_to_str(yytext,10); return TOK_INT
"0x"{HEX}+ yylval=int_to_str(yytext[2],16); return TOK_INT;
"0b"{BIN}+ yylval=int_to_str(yytext[2],2); return TOK_INT;
/*keywords / reserved words*/
"val" return TOK_VAL_SIMPLE;
"val rec" return TOK_VAL_REC;
"fun" return TOK_FUN_DEF;
"if" return TOK_IF;
"then" return TOK_THEN;
"else" return TOK_ELSE;
"while" return TOK_WHILE;
"type" return TOK_TYPEDEF;
"datatype" return TOK_DATATYPE_DEF;
"end" return TOK_END;
"let" return TOK_LET;
"local" return TOK_LOCAL;
"struct" return TOK_STRUCT;
"case" return TOK_CASE;
/*types*/
"real" yylval = _real; return TOK_TYPE;
"word" yylval = _word; return TOK_TYPE;
"list"  yylval = _cons; return TOK_TYPE;
"ref" yylval = _ref; return TOK_TYPE;
"nil" yylval = _nil; return TOK_TYPE;
"bool" yylval = _bool; return TOK_TYPE;
(#|//).* /*one line comments*/
 /*c multiline comments*/
"/*" {int c,nest=1;
while(1){
while ((c=input()) != '*' && c != '/' && c != EOF);
if (c == '/'){
while((c=input()) == '/'){
if (c=='*'){
nest++;
continue;}}}
if (c == '*'){
while((c=input()) == '*'){
if (c=='/'){
nest--;
if (nest == 0){break}
continue;}}}
if (c == EOF){
error("EOF in comment");
break;}}}
 /*ml/pascal multiline comments*/
"(*" {int c,nest=1;
while(1){
while ((c=input()) != '*' && c != '(' && c != EOF);
if (c == '('){
while((c=input()) == '('){
if (c=='*'){
nest++;
continue;}}}
if (c == '*'){
while((c=input()) == '*'){
if (c==')'){
nest--;
if (nest == 0){break}
continue;}}}
if (c == EOF){
error("EOF in comment");
break;}}}
 /*base case*/
. yylval=yytext[0]; return yytext[0]
