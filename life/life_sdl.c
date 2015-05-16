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
  pthread_attr_t attr;
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
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, 1);
  lc.sem = malloc(sizeof(sem_t));
  c.sem = malloc(sizeof(sem_t));
  sem_init(lc.sem, 0, 0);
  sem_init(c.sem, 0, 0);
  pthread_create(&lc.id, &attr, (void*(*)(void*))run_worker_thread, &lc);
  //init SDL
  SDL_Init(SDL_INIT_VIDEO);
  atexit(SDL_Quit);
  atexit(end_life);
  SDL_CreateWindowAndRenderer(c.width, c.height,
                              SDL_WINDOW_INPUT_FOCUS | SDL_WINDOW_RESIZABLE,
                              &c.window, &c.renderer);
  SDL_SetWindowTitle(c.window, "The Game of Life SDL");
  SDL_SetRenderDrawColor(c.renderer, 0xff, 0xff, 0xff, 0xff);//white
  SDL_RenderClear(c.renderer);
  SDL_RenderPresent(c.renderer);
  SDL_Delay(1000);
  c.texture = SDL_CreateTexture(c.renderer, SDL_PIXELFORMAT_ABGR8888,
                                SDL_TEXTUREACCESS_STREAMING, c.width, c.height);
  c.delay = 100;
  randomize_grid(c.w);//should this get called from the other thread?
  draw_world(&c);
  run_event_loop(&c);
  return 0;//we never get here
}
