#include "common.h"
#include <regex.h>
#include <readline/readline.h>
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
str_case(if,IF)
str_case(do,DO)
str_case(def,DEF)
str_case(define,DEF)
str_case(defun,DEFUN)
str_case(quote,QUOTE)
str_case(\0x27,QUOTE)
           
