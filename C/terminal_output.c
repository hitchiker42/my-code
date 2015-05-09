#include <curses.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <signal.h>

int term_rows, term_cols;
int max_x, max_y;
int ncurses_mode;
int slang_mode;
static struct timespec sleep_timer = {.tv_sec = 0, .tv_nsec = (time_t)0.1*10e9};
static struct timespec sleep_timer_long = {.tv_sec = 1, .tv_nsec = (time_t)0.5*10e9};
void cleanup(int signo){
  endwin(); 
  raise(signo);
}
#define getxy(win,x,y) getyx(win,y,x)
static struct sigaction cleanup_act = {.sa_handler = cleanup,
                                       .sa_flags = SA_RESETHAND};
void ncurses_init(){
  initscr();//start curses mode
  cbreak();//disable line buffering, pass ^C,^Z,etc to terminal
  //raw();//disable line buffering, don't pass ctrl chars to terminal
  noecho();//don't echo keyboard input;
  //keypad(stdscr, 1);//enable non-typewriter keys (F1, arrows, etc..)
  //macro with args window, y, x; sets the y,x variables to the maximum
  //y,x values of the given window.
  getmaxyx(stdscr, max_y, max_x);//lets try with ROWS/COLs first
  term_rows = LINES;
  term_cols = COLS;
}
//draw a box on the screen and fill it with '#' characters
void draw_box(){
  //draw the boarder, args are w,e,n,s,nw,ne,sw,se where north == up
  wborder(stdscr, '|', '|', '-', '-', '+', '+', '+', '+');
  int i,j;
  for(i=1;i<max_y-1;i++){
    wmove(stdscr, i, 1);
    for(j=1;j<max_x-1;j++){
      waddch(stdscr, '#');
    }
  }
  wrefresh(stdscr);
}
void iterate(){
  int i,j,x,y;
  int rows = term_rows - 1;
  int cols = term_cols - 1;
  for(y=1;y<rows;y++){
    for(x=1;x<cols;x++){
      move(y,x);
      addch('*');
      refresh();
      i = y;
      j = x+1;
      for(;i<rows;i++){
        for(;j<cols;j++){
          wmove(stdscr, i, j);
          if(rand() > RAND_MAX/2){
            waddch(stdscr, '*');
          } else {
            waddch(stdscr, '#');
          }
        }
        wrefresh(stdscr);
        nanosleep(&sleep_timer, NULL);
        j=1;
      }
    }
  }
}
int main(int argc, char **argv){
  sigaction(SIGTERM, &cleanup_act, NULL);
  srand(time(NULL));
  ncurses_init();
  draw_box();
  nanosleep(&sleep_timer_long, NULL);
  iterate();
  endwin();
  return 0;
}
