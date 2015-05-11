#define _GNU_SOURCE
#include <curses.h>
#include "life.h"
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <signal.h>
void catch_interupt(int signo);
static int running_life;
static struct timespec wait_time = {.tv_sec = 0, .tv_nsec = (time_t)0.5*10e9};
static struct sigaction default_act = {.sa_handler = SIG_DFL};
static struct sigaction cleanup_act = {.sa_handler = catch_interupt};
int term_rows;
int term_cols;
//^C stops the current simulation, if no simulation is running it kills the program
void catch_interupt(int signo){
  if(running_life){
    running_life = 0;
    return;
  } else {
    endwin();
    sigaction(signo, &default_act, NULL);
    raise(signo);
  }
}
int life_rand(int max){
  return random() % max;
}
#define getxy(win,x,y) getyx(win,y,x)
void ncurses_init(){
  initscr();//start curses mode
  cbreak();//disable line buffering, pass ^C,^Z,etc to terminal
  //raw();//disable line buffering, don't pass ctrl chars to terminal
  noecho();//don't echo keyboard input;
  term_rows = LINES;
  term_cols = COLS;
}
static inline void* xmalloc(size_t sz){
  void *mem = malloc(sz);
  if(mem == NULL && sz){
    raise(SIGSEGV);
  }
  return mem;
}
void write_grid(world *w, WINDOW *win){
  int i;
  for(i=0;i<w->rows;i++){
    wmove(win, i, 0);
    waddnstr(win, w->grid + i*w->rows, w->cols);
  }
  wrefresh(win);
}
void run_life(world *w, WINDOW *win){
  write_grid(w, win);
  running_life = 1;
  while(running_life){//this could cause a delay in reacting to ^C
    nanosleep(&wait_time, NULL);
    step_world(w);
    write_grid(w, win);
  }
}
//interactively read initial conditons from the user
void setup_initial_conditons(world *w, WINDOW *win){
  keypad(win, 1);
  int c,y,x,ymax,xmax;
  getyx(win, y ,x);
  getmaxyx(win, ymax, xmax);
  while(c = wgetch(win)){
    switch(c){
      case KEY_UP:
        y = y > 0 ? y-1 : ymax;
        break;
      case KEY_DOWN:
        y = y < ymax ? y+1 : 0;
        break;
      case KEY_LEFT:
        x = x > 0 ? x-1 : xmax;
        break;
      case KEY_RIGHT:
        x = x < xmax ? x+1 : 0;
        break;
      case '\r':
      case '\n':
        waddch(win, '#');
        w->grid[point_to_offset(make_point(x,y),w->cols)] = 1;
        break;
      case ' ':

    }
    wmove(win, y ,x);
  }
}
/*
  Pointless Idea, it'd be cool to run a '###' across the grid, in such a way
  that it moved one block at a time, or something.
*/  
int main(int argc, char **argv){
  //add options later
  
}
