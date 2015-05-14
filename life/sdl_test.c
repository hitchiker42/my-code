#include <SDL2/SDL.h>
#include <SDL2/SDL_video.h>
#include <SDL2/SDL_render.h>
#include <SDL2/SDL_timer.h>

#define set_renderer_black(r) SDL_SetRenderDrawColor(r, 0x00, 0x00, 0x00, 0xff)
#define set_renderer_white(r) SDL_SetRenderDrawColor(r, 0xff, 0xff, 0xff, 0xff)
SDL_Window *win;
SDL_Renderer *rend;
void fill_screen(SDL_Renderer *renderer, int r, int g, int b){
  SDL_SetRenderDrawColor(renderer, r, g, b, 0xff);
  SDL_RenderClear(renderer);//set screen to given color
  SDL_RenderPresent(renderer);//update diplay
}
int main(){
  int x,y;
  x = y = 100;
  atexit(SDL_Quit);
  SDL_Init(SDL_INIT_VIDEO);
  SDL_CreateWindowAndRenderer(x, y, 0, &win, &rend);
  int i;
  //make window black, then red, then green, then blue, then white
  fill_screen(rend, 0x00, 0x00, 0x00);//black
  for(i=0;i<=0xff;i++){
    fill_screen(rend, i, 0x00, 0x00);//red
    SDL_Delay(10);
  }
  for(i=0xff;i>=0;i--){
    fill_screen(rend, i, 0x00, 0x00);//red
    SDL_Delay(10);
  }
  for(i=0;i<=0xff;i++){
    fill_screen(rend, 0x00, i, 0x00);//green
    SDL_Delay(10);
  }
  for(i=0xff;i>=0;i--){
    fill_screen(rend, 0x00, i, 0x00);//green
    SDL_Delay(10);
  }
  for(i=0;i<=0xff;i++){
    fill_screen(rend, 0x00, 0x00, i);//blue
    SDL_Delay(10);
  }
  for(i=0xff;i>=0;i--){
    fill_screen(rend, 0x00, 0x00, i);//blue
    SDL_Delay(10);
  }
  for(i=0;i<=0xff;i++){
    fill_screen(rend, i, i, i);//white
    SDL_Delay(10);
  }
  return 0;
}
