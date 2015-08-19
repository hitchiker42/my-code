#include <readline/readline.h>
#include <readline/history.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
static void do_readline(void(*f)(char*,void*),void *user_data,char *prompt){
  if(!prompt){
    prompt=">";
  }
  char *line_read=NULL;
  while(1){
    line_read=readline(prompt);
    if(line_read){
      if(*line_read){
        add_history(line_read);
      } else {continue;}
    } else {fputs("\n",stderr);exit();}
    f(line_read,user_data);
    free(line_read);
  }
}
