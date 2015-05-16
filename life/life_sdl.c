#include "life_sdl.h"
volatile int8_t running_life = 0;
static void end_life(void){
  running_life = -1;
}
/*
  Features:
   Run multiple steps of the simulation between frames
   set colors (preferably by user input of hex values)
   Interactivly manipulate the grid
  Options:
    set initial delay
    set cell size
    set window size
    set initial colors
    set inital grid
*/
int main(int argc, char **argv){
  //process options
  SDL_context c = {0};
  life_context lc = {0};
  //init structs
  c.life_ctx = &lc;
  lc.SDL_ctx = &c;
  c.width = c.height = 0xff;
  lc.width = lc.height = 0xff;
  lc.cell_height = lc.cell_width = 1;
  lc.w = init_world(lc.width/lc.cell_width, lc.height/lc.cell_height);
  c.w = lc.w;
  lc.live_color = make_rgb(0,0,0xff);//blue
  lc.dead_color = 0xffffffff;//white
  
  //create worker thread/ init semaphores
  SDL_Init(SDL_INIT_VIDEO);
  lc.sem = SDL_CreateSemaphore(0);
  c.sem = SDL_CreateSemaphore(0);
  SDL_Thread *t = SDL_CreateThread((int(*)(void*))run_worker_thread,NULL,&lc);
  lc.id = SDL_GetThreadID(t);
  c.id = SDL_ThreadID();
  SDL_DetachThread(t);
  
  //init SDL
  atexit(SDL_Quit);
  atexit(end_life);
  SDL_CreateWindowAndRenderer(c.width, c.height,
                              SDL_WINDOW_INPUT_FOCUS | SDL_WINDOW_RESIZABLE,
                              &c.window, &c.renderer);
  clear_screen(c.renderer, (union rgba)0xffffffff);
  SDL_SetWindowTitle(c.window, "The Game of Life SDL");
  SDL_Delay(1000);
  c.texture = SDL_CreateTexture(c.renderer, SDL_PIXELFORMAT_ABGR8888,
                                SDL_TEXTUREACCESS_STREAMING, c.width, c.height);
  c.delay = 100;
  randomize_grid(c.w);//should this get called from the other thread?
  draw_world(&c);
  run_event_loop(&c);
  return 0;//we never get here
}
