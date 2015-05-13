#define _GNU_SOURCE
#include <curses.h>
#include "life.h"
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <signal.h>
#include <sys/time.h>
#define NANO_SECONDS(x) (long)(x * 1e9)
#define MICRO_SECONDS(x) (long)(x * 1e6)
void catch_interupt(int signo);
static int running_life;
static struct timespec wait_time = {.tv_nsec = 2e8, .tv_sec = 0};
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
#define getxy(win,x,y) getyx(win,y,x)
void ncurses_init(){
  initscr();//start curses mode
  cbreak();//disable line buffering, pass ^C,^Z,etc to terminal
  //raw();//disable line buffering, don't pass ctrl chars to terminal
  noecho();//don't echo keyboard input;
  getmaxyx(stdscr, term_rows, term_cols);
}
static inline void* xmalloc(size_t sz){
  void *mem = malloc(sz);
  if(mem == NULL && sz){
    raise(SIGSEGV);
  }
  return mem;
}
void nsec_add(struct timespec *ts, long nsecs){
  ts->tv_nsec += nsecs;
  if(nsecs > 0){
    while(ts->tv_nsec > 1e9){
      ts->tv_sec += 1;
      ts->tv_nsec -= 1e9;
    }
  } else {
    while(ts->tv_nsec < 0 && ts->tv_sec > 0){
      ts->tv_sec -= 1;
      ts->tv_nsec += 1e9;
    }
    ts->tv_nsec = ts->tv_nsec < 0 ? 0 : ts->tv_nsec;
  }
}

void draw_world(world *w, WINDOW *win){
  int i,j;
  //TODO: leave room for a border, maybe
  for(i=0;i<w->rows;i++){
    for(j=0;j<w->cols;j++){
      wmove(win, i, j);
      //this draws a border of '*'s 
      if(i==0 | j == 0 | i == w->rows-1 || j == w->cols -1){
        waddch(win, '*');
      }
      waddch(win, w->grid[i*w->cols + j] ? '#' : ' ');
    }
  }
  wrefresh(win);
}
void run_life(world *w, WINDOW *win){

  draw_world(w, win);
  running_life = 1;
  nodelay(win, 1);
  curs_set(0);
  struct timeval tv1,tv2;
  while(running_life){//this could cause a delay in reacting to ^C
    gettimeofday(&tv1, NULL);
    int interupt = nanosleep(&wait_time, NULL);
    /*    if(interupt == -1){
      fprintf(stderr, "Nanosleep interupted\n");
    } else {
      gettimeofday(&tv2, NULL);
      assert(tv2.tv_usec - tv1.tv_usec >= MICRO_SECONDS(0.5));
      }*/
    step_world(w);
    draw_world(w, win);
    if(wgetch(win) != ERR){
      break;
    }
  }
  nodelay(win, 0);
  curs_set(1);
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
        //      case KEY_SUP:
        y = y > 0 ? y-1 : ymax;
        break;
      case KEY_DOWN:
        //      case KEY_SDOWN:
        y = y < ymax ? y+1 : 0;
        break;
      case KEY_LEFT:
        //      case KEY_SLEFT:
        x = x > 0 ? x-1 : xmax;
        break;
      case KEY_RIGHT:
        //      case KEY_SRIGHT:
        x = x < xmax ? x+1 : 0;
        break;
      case KEY_BACKSPACE:
        reset_grid(w);
        draw_world(w, win);
        x = y = 0;
        break;
      case '+':
        nsec_add(&wait_time, 5e7);
        break;
      case '-':
        nsec_add(&wait_time, -5e7);
        break;
      case 'q':
        endwin();
        exit(0);
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
    //    if(c == KEY_SUP || c == KEY_SDOWN | c == KEY_SLEFT || c = KEY_SRIGHT){
    //      waddch(win, '#');
    //      wmove(win, y ,x);
    //    }

  }
}
/*
  Pointless Idea, it'd be cool to run a '###' across the grid, in such a way
  that it moved one block at a time, or something.
*/
int main(int argc, char **argv){
  srandom(time(NULL));
  //  run_life_debug(w);
  //add options later
  ncurses_init();
  sigaction(SIGTERM, &interupt_act, NULL);
  world *w = init_world(term_rows, term_cols);
  randomize_grid(w);
  draw_world(w, stdscr);
  while(1){
    wmove(stdscr, 0, 0);
    setup_initial_conditons(w, stdscr);
    run_life(w, stdscr);
  }
  endwin();//presumably we'll never get here
}
