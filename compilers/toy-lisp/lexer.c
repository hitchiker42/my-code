#include "common.h"
#include <regex.h>
#include <readline/readline.h>
//idea for scaning for parens(just to see if we need more input)
char lex(char* input){
  rl_set_signals (void)
    int i=0;
  char cur_char;
  char* input;//get line
  int paren;
  //a way to insure parantheses match, not the best
  while(1){
    while((cur_char=input[i++])!='\n'){
      if(cur_char == '('){parens+=1;}
      if(cur_char == ')'){parens-=1;}
    }
    if(parens != 0)
      {input == getline;}
    else{break;}
  }
#define str_case(lc,uc) if(strcmp(#lc,/*some var*/)){return TOK_##uc;} else
regex_t* ID = xmalloc_atomic(sizeof(regex_t));
regex_t* LIST = xmalloc_atomic(sizeof(regex_t));
regex_t* NUMBER = xmalloc_atomic(sizeof(regex_t));
regmatch_t match[100];
char* id_str="[[:alpha:]_+\-*%$&][[:alnum:]_+\-*%$&]*";
char* list_str="'?( ' '* ("id_str"' '+)*"id_str"' '*')'";
char* num_str="[0-9]+\.?[0-9]*"
regcomp(ID,id_str,REG_NEWLINE&REG_EXTENDED);
regcomp(LIST,list_str,REG_NEWLINE&REG_EXTENDED);
regcomp(NUMBER,num_str,REG_NEWLINE&REG_EXTENDED);
str_case(if,IF)//match if expr
str_case(do,DO)//match do expr
str_case(def,DEF)//match generic define
str_case(define,DEF)//match generic define
str_case(defun,DEFUN)//match function define, only way to define functions
                     //untill I add lambdas
str_case(quote,QUOTE)//match a quote
str_case(\0x27,QUOTE)//also match a quote \0x27 is '
           
