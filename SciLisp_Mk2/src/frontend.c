#include "common.h"
int main(int argc, char *argv[]){
  return 0;
}
/* Read a line from standard input using the readline library if
   avaiable. A pointer to the line read is returned, with memory
   for the line allocated with the sl_malloc function.
   If readline is being used and the line is not empty it is
   added to the history.

   @arg prompt: The prompt to display to the user
*/
SL_INLINE char* sl_readline(char *prompt){
  char *line_read=NULL;
#if (defined HAVE_READLINE)
  line_read=readline(prompt);
  if(line_read && *line_read){
    add_history(line_read);
    char *temp = sl_strdup(line_read);
    free(line_read);
    line_read=temp;
  }
#else
  fputs(prompt,stdout);
  int num_chars = getline(&line_read,0,stdin);
  if(line_read && *line_read){
    char *temp = sl_malloc(num_chars*sizeof(char));
    memcpy(line_read, temp, num_chars);
    free(line_read);
    line_read=temp;
  }
#endif
  return line_read;
}
