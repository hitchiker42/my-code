%code requires{
#include "common.h"
 }
%union{
  datatype val;
  symrec* id;
 }
%token <id> TOK_VAL_SIMPLE;
%token <id> TOK_VAL_REC;
%token <id> TOK_FUN_DEF;
%token <id> TOK_ID;
%token <val> TOK_INT;
%token <val> TOK_REAL;
%token TOK_IF;
%token TOK_THEN;
%token TOK_ELSE;
%token TOK_WHILE;
%token TOK_TYPEDEF;
%token TOK_DATATYPE_DEF;
%token TOK_END;
%token TOK_LET;
%token TOK_LOCAL;
%token TOK_STRUCT;
%token TOK_CASE;
%%
type:
TOK_TYPE {$$=get_ctype(yylval)}
| type * TOK_TYPE
| '(' type '->' type ')'
stmt:
  TOK_VAL_SIMPLE ':' type '=' expr
  | TOK_VAL_REC ':' type '=' expr
| TOK_FUN_DEF ':' type '=' expr
type_def:
  TOK_TYPEDEF TOK_ID '=' TOK_TYPEDEF
datatype_tag:
  TOK_ID
| TOK_ID "of" TOK_TYPE
| datatype_tag '|' datatype_tag
datatype_def:
  TOK_DATATYPE_DEF TOK_ID '=' datatype_tag
args:
TOK_ID {symref id_sym = {yylval,nil};$$=id_sym}
| args ',' TOK_ID 
fun_call:
  /*function w/tuple arg*/
TOK_ID{symref* fun;getSym(yylval,fun);} '(' args ')'{symref** args;
  if(fun->val.tag != _fun){
    yyerror("no such function %s\n",fun.name);
    //if arg type and expected type don't match raise an error
  } else if (fun->val.val.fun->type[0] != arg.val.tag){
    yyerror("error type mismatch:\n got %s, expected %s\n",
            toString_type(fun->val.val.fun->type[0]),
            toString_type(arg->val.tag));
  } else {

| TOK_ID{symref* fun;getSym(yylval,fun);} TOK_ID {symref* arg;
  getSym(yylval,arg);
  //if this isn't a functon raise an error
  if(fun->val.tag != _fun){
    yyerror("no such function %s\n",fun.name);
    //if arg type and expected type don't match raise an error
  } else if (fun->val.val.fun->type[0] != arg.val.tag){
    yyerror("error type mismatch:\n got %s, expected %s\n",
            toString_type(fun->val.val.fun->type[0]),
            toString_type(arg->val.tag));
  } else {
    //call the function on the argument
    $$=fun->val.val.fun(arg->val.val);
    
  
expr:
    TOK_REAL {$$=yylval}
| TOK_INT {$$=yylval}
| TOK_ID 
| TOK_IF expr TOK_THEN expr TOK_ELSE

