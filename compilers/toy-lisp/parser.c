#include "common.h"
datatype *yylval;
int paren_cnt=0;
Cons* parse(FILE* input){
  char token;
  Cons* ast_node=xmalloc(sizeof(Cons));
  yylval=0;
  while(1){
    token=yylex(input);
    if (token == '('){
      ast_node->car=parse(input);
    } else if (token == ')'){
      return ast_node;
    } else if (token == 'i'){
      //if_expr
      ast_node->car=yylval;
      Cons* cond=xmalloc(sizeof(Cons));
      cond->car=parse(input);
      Cons* then=xmalloc(sizeof(Cons));
      then->car=parse(input);
      Cons* otherwise=xmalloc(sizeof(Cons));
      //evaluate as implict progn
      ast_node->cdr=cond;
      cond->cdr=then;
      then->cdr=otherwise;
    }
}
