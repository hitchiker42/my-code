#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if 0
//Silly little test program to make sure I can do video stuff on a thread
//than I create.
int sdl_main(void *data){
  SDL_Init(SDL_INIT_VIDEO);
  SDL_Window *win;
  SDL_Renderer *renderer;
  win = SDL_CreateWindow("test",SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                         640,480, SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIDDEN);
  renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_PRESENTVSYNC);
//  SDL_Delay(1000);
  SDL_ShowWindow(win);
  SDL_Event evt;
  for(int b = 0; b < 256; b+=16){
    for(int g = 255; g >= 0; g-=16){
      for(int r = 0; r < 256; r+=16){
        SDL_SetRenderDrawColor(renderer, r, g, b, SDL_ALPHA_OPAQUE);
        SDL_RenderClear(renderer);
        SDL_RenderPresent(renderer);
        while(SDL_PollEvent(&evt)){
          if(evt.type == SDL_QUIT){
            goto end;
          }
          if(evt.type == SDL_WINDOWEVENT &&
             evt.window.event == SDL_WINDOWEVENT_CLOSE){
            goto end;
          }
        }
      }
    }
  }
 end:
  SDL_DestroyWindow(win);
  SDL_DestroyRenderer(renderer);
  return 0;
}
int main(){
  SDL_Thread *thrd;
  thrd = SDL_CreateThread(sdl_main, "thread", NULL);
  int ret;
  printf("This is the `main` thread.\n");
  SDL_WaitThread(thrd, &ret);
  return ret;
}
#endif
