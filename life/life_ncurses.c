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
static struct sigaction interupt_act = {.sa_handler = catch_interupt};
int term_rows;
int term_cols;
//^C stops the current simulation, if no simulation is running it kills the program
void catch_interupt(int signo){
  fprintf(stderr, "Caught signal %d", signo);
  if(running_life){
    fprintf(stderr, "stopping simulation");
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
void draw_world(world *w, WINDOW *win){
  int i,j;
  //TODO: leave room for a border, maybe
  for(i=0;i<w->rows;i++){
    wmove(win, i, 0);
    for(j=0;j<w->cols;j++){
      waddch(win, w->grid[i*w->rows + j] ? '#' : ' ');
    }
  }
  wrefresh(win);
}
void run_life(world *w, WINDOW *win){
  draw_world(w, win);
  running_life = 1;
  nodelay(win, 1);
  while(running_life){//this could cause a delay in reacting to ^C
    nanosleep(&wait_time, NULL);
    step_world(w);
    draw_world(w, win);
    if(wgetch(win) != ERR){
      break;
    }
  }
  nodelay(win, 0);
  return;
}
void run_life_test(world *w, WINDOW *win){
  draw_world(w, win);
  running_life = 1;
  nodelay(win, 1);
  char *filename;
  int i=0;
  while(running_life){//this could cause a delay in reacting to ^C
    if(i>= 10){break;}
    asprintf(&filename, "grid_step_%d",i++);
    dump_world_to_file(w, filename);
    nanosleep(&wait_time, NULL);
    step_world(w);
    draw_world(w, win);
    if(wgetch(win) != ERR){
      break;
    }
  }
  nodelay(win, 0);
  return;
}
//interactively read initial conditons from the user
void setup_initial_conditons(world *w, WINDOW *win){
  keypad(win, 1);
  int c,y,x,ymax,xmax;
  getyx(win, y ,x);
  getmaxyx(win, ymax, xmax);
  wmove(win, 0, 0);
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
      case KEY_BACKSPACE:
        reset_grid(w);
        draw_world(w, win);
        x = y = 0;
        break;
      case 'r':
        randomize_grid(w);
        draw_world(w, win);
        x = y = 0;
        break;
      case 's':
        step_world(w);
        draw_world(w, win);
        break;
      case '\r':
      case '\n':
        waddch(win, '#');
        w->grid[y*w->cols + x] = 1;
        break;
      case ' ':
        return;
    }
    wmove(win, y ,x);
  }
}
/*
  Pointless Idea, it'd be cool to run a '###' across the grid, in such a way
  that it moved one block at a time, or something.
*/  
int main(int argc, char **argv){

  //  run_life_debug(w);
  //add options later
  ncurses_init();
  sigaction(SIGTERM, &interupt_act, NULL);
  world *w = init_world(term_rows, term_cols);
  randomize_grid(w);
  draw_world(w, stdscr);  
  while(1){
    setup_initial_conditons(w, stdscr);
    run_life_test(w, stdscr);
  }
}
