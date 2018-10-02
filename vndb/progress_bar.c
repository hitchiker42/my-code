#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>
#include <stdarg.h>
#include <assert.h>
struct progress_bar {
  char *title;
  char *buf;
  size_t title_size;
  //number of colmuns in the window, also used as the size of buf.
  int termwidth;
  int offset;
  double current;
  double end;
  struct {
    char start;
    char done;
    char cur;
    char empty;
    char end;
    char endline;
  } display_chars;
  //used in C++
  int8_t finished;
};
int get_term_width(){
  //try to get the value using the COLUMNS env variable first,
  //since its much simpler than making an ioctl call. 
  int termwidth = 0;
  char *cols = getenv("COLUMNS");
  if(cols){
    termwidth = strtol(cols, NULL, 10);
  }
  if(termwidth == 0){
    struct winsize ws;
    ioctl(STDIN_FILENO, TIOCGWINSZ, &ws);
    termwidth = ws.ws_col;
  }
  return termwidth;
}
void get_term_info(struct progress_bar *bar){
  if(!isatty(STDOUT_FILENO)){ 
    bar->display_chars.endline = '\n'; 
    bar->termwidth = 120;
    return;
  }
  bar->termwidth = get_term_width();
  return;
}
/*
double float_time(){
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  return ts.tv_sec + (ts.tv_nsec / 1e9);
}
*/
void display_progress_bar(struct progress_bar *bar){
  assert(bar->end >= 1);
  double percent_done = bar->current / bar->end;
  if(percent_done > 1){ 
    percent_done = 1; 
  }
  //format: [title] '\s' '[' [progress] ']' '\s' ddd.dd%
  int bar_size = bar->termwidth - (2 +  bar->title_size + 8);
  int new_offset = (int)round(bar_size * percent_done) + 1;
  if(bar->offset != new_offset){
    bar->offset = new_offset;
    bar->buf[0] = bar->display_chars.start;
    for(int i = 1; i < bar->offset; i++){
      bar->buf[i] = bar->display_chars.done;
    }
    if(bar->offset < bar_size){
      bar->buf[bar->offset] = bar->display_chars.cur;
      memset(bar->buf + bar->offset + 1, bar->display_chars.empty,
             bar_size - (bar->offset + 1));
    }
    bar->buf[bar_size-1] = bar->display_chars.end;
    bar->buf[bar_size] = '\0';
  }
  fprintf(stdout, "%s %s %6.2f%%%c", bar->title, bar->buf, percent_done * 100, 
          bar->display_chars.endline);
  fflush(stdout);
}
