#include "common.h"
#include <regex.h>
#include <readline/readline.h>
#define get_char() cur_char=fgetc(input)
inline int isspecial(int c){
  return (c == '('||c==')'||c=='\'');
}
inline int isdelim(int c){
  return isspace(c)||isspecial(c);
}
char yylex(FILE* input){
  int i=0;
  char cur_char=fgetc(input);
  if (cur_char == EOF){
    return 'e';
  }
  //oneline comments 
  if(cur_char == ';'){
    while(cur_char != '\n'){
      get_char();
    }
    yylex(input);
  }
  //skip whitespace
  while(isspace(cur_char)){get_char();}
  //parantheses & quotes are their own tokens
  if(isspecial(cur_char)){
    if(cur_char == '\''){
      *yylval=(datatype){_special,'\''};
    }
    return cur_char;
  }
  //parse numbers
  int has_dot=0;
  if (isdigit(cur_char)){
    int size=20;
    char* buf=xmalloc(sizeof(int)*size);//
    while (isdigit(cur_char)){
      if(i>=(size-1)){
        size*=2;
        xrealloc(buf,sizeof(int)*size);
      }
      buf[i]=cur_char;
      cur_char=fgetc(input);
      i++;
      if(cur_char == '.'){
        if(has_dot){
          fprintf(stderr,"Lex error, multiple .'s in numeric literal\n");
          return '?';
        }
        has_dot=1;
        buf[i]=cur_char;
        cur_char=fget(input);
        i++;
      }
    }
    //see if next character is a delimiter for numbers
    if(isdelim(cur_char)){
      ungetc(input,cur_char);
      *yylval=(datatype){_double,strtod(buf)};
      GC_FREE(buf);
      return 'n';
    } else {
      fprintf(stderr,"Lex error, expected number, space or parens got %c\n",cur_char);
      return '?';
    }
  }
  //parse variable names and special forms
  if(isalpha(cur_char)){
    int size=50;
    char* buf=xmalloc(sizeof(int)*size);//
    while(!isdelim(cur_char) && cur_char != EOF){
      if(i==size){
        xrealloc(buf,(size*=2));
      }
      buf[i]=cur_char;
      fgetc(cur_char);
    }
    if(cur_char == EOF){
      fprintf(stderr,"Lex error, eof in identifier");
      return '?';
    }
    if(strcmp(buf,"if")){
      *yylval=(datatype){_special,'i'};
      return 'i';
    } else if (strcmp(buf,"do")){
      *yylval=(datatype){_special,'l'};
      return 'l';
    } else if (strcmp(buf,"def")){
      *yylval=(datatype){_special,'d'};
      return 'd';
    } else if (strcmp(buf,"define")){
      *yylval=(datatype){_special,'d'};
      return 'd';
    } else if (strcmp(buf,"defun")){
      *yylval=(datatype){_special,'f'};
      return 'f';
    } else if (strcmp(buf,"quote")){
      *yylval=(datatype){_special,'\''};
      return '\'';
    } else {
      symref *temp=xmalloc(sizeof(symref));
      temp->name=buf;
      addSym(temp);
      *yylval=(datatype){_sym,temp};
      return 's';
    }
    /*    
#define str_case(lc,uc) if(strcmp(#lc,)){return TOK_##uc;} else
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
<<<<<<< HEAD
str_case(if,IF)
str_case(do,DO)
str_case(def,DEF)
str_case(define,DEF)
str_case(defun,DEFUN)
str_case(quote,QUOTE)
str_case(\0x27,QUOTE)          */
/*while(1){
while((cur_char=input[i++])!='\n'){
  if(cur_char == '('){parens+=1;}
  if(cur_char == ')'){parens-=1;}
 }
if(parens != 0)
  {input == getline;}
  else{break;}}*/
=======
str_case(if,IF)//match if expr
str_case(do,DO)//match do expr
str_case(def,DEF)//match generic define
str_case(define,DEF)//match generic define
str_case(defun,DEFUN)//match function define, only way to define functions
                     //untill I add lambdas
str_case(quote,QUOTE)//match a quote
str_case(\0x27,QUOTE)//also match a quote \0x27 is '
           
>>>>>>> ae853769629b47badf605d22215166ff05ec817c
