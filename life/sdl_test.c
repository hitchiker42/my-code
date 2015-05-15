#include <SDL2/SDL.h>
#include <SDL2/SDL_video.h>
#include <SDL2/SDL_render.h>
#include <SDL2/SDL_timer.h>
#include "sdl_util.h"

#define set_renderer_black(r) SDL_SetRenderDrawColor(r, 0x00, 0x00, 0x00, 0xff)
#define set_renderer_white(r) SDL_SetRenderDrawColor(r, 0xff, 0xff, 0xff, 0xff)
int clip_uint8(int x){
  //really inefficent
  return x > 0xff ? 0xff : x < 0 ? 0 : x;
}
void fill_screen(SDL_Renderer *renderer, int r, int g, int b){
  SDL_SetRenderDrawColor(renderer, r, g, b, 0xff);
  SDL_RenderClear(renderer);//set screen to given color
  SDL_RenderPresent(renderer);//update diplay
}
int main_yuv(){
  const int num_cells = 12;
  const int rows = 4;
  const int cols = 3;
  SDL_Window *window;
  SDL_Renderer *renderer;
  SDL_Texture *textures[num_cells];
  SDL_Rect cells[num_cells];
  int w,h;
  int i,j,k,step;
  w = h = 0xff;
  atexit(SDL_Quit);
  SDL_Init(SDL_INIT_VIDEO);
  SDL_CreateWindowAndRenderer(w*cols, h*rows, 0, &window, &renderer);
  for(i=0;i<num_cells;i++){
    textures[i] = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_IYUV,
                                    SDL_TEXTUREACCESS_STREAMING, 0xff, 0xff);
  }
  for(i=0;i<rows;i++){
    for(j=0;j<cols;j++){
      cells[i*cols + j] = (SDL_Rect){.w = w, .h = h, .x = j*w, .y = i*h};
    }
  }
  uint8_t *pixels[num_cells];
  int stride;//should be the same for all
  for(step=0;step<0xff;step++){
    for(i=0;i<num_cells;i++){
      SDL_LockTexture(textures[i], NULL, (void**)&pixels[i], &stride);
    }
    //just loop luma and chroma seperatly, it shouldn't be any slower
    //all 3 columns have the same U,V values
    for(i=0;i<h/2;i++){
      for(j=0;j<stride/2;j++){
        //change this if num rows/cols changes
        for(k=0;k<3;k++){
          IYUV_getV(pixels[k],stride,h)[i*(stride/2) + j] = i*4;
          IYUV_getU(pixels[k],stride,h)[i*(stride/2) + j] = j*4;

          IYUV_getV(pixels[k + 3],stride,h)[i*(stride/2) + j] = 0xff-i*4;
          IYUV_getU(pixels[k + 3],stride,h)[i*(stride/2) + j] = j*4;

          IYUV_getV(pixels[k + 6],stride,h)[i*(stride/2) + j] = i*4;
          IYUV_getU(pixels[k + 6],stride,h)[i*(stride/2) + j] = 0xff-j*4;

          IYUV_getV(pixels[k + 9],stride,h)[i*(stride/2) + j] = 0xff-i*4;
          IYUV_getU(pixels[k + 9],stride,h)[i*(stride/2) + j] = 0xff-j*4;
        }
      }
    }
    //each row has the same luma values
    for(i=0;i<0xff;i++){
      for(j=0;j<stride;j++){
        for(k=0;k<4;k++){
          pixels[3*k][i*stride + j] = i;
          pixels[3*k+1][i*stride + j] = j;
          pixels[3*k+2][i*stride + j] = step;
        }
      }
    }
    for(i=0;i<num_cells;i++){
      SDL_UnlockTexture(textures[i]);
      SDL_RenderCopy(renderer, textures[i], NULL, cells+i);
    }
    SDL_RenderPresent(renderer);
    SDL_Delay(20);
  }
  return 0;
}
int main_rgba(){
  const int num_cells = 12;
  const int rows = 4;
  const int cols = 3;
  SDL_Window *window;
  SDL_Renderer *renderer;
  SDL_Texture *textures[num_cells];
  SDL_Rect cells[num_cells];
  int w,h;
  int i,j,step;
  w = h = 0xff;
  atexit(SDL_Quit);
  SDL_Init(SDL_INIT_VIDEO);
  SDL_CreateWindowAndRenderer(w*cols, h*rows, SDL_WINDOW_BORDERLESS, &window, &renderer);
  for(i=0;i<num_cells;i++){
    textures[i] = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ABGR8888,
                                    SDL_TEXTUREACCESS_STREAMING, 0xff, 0xff);
  }
  for(i=0;i<rows;i++){
    for(j=0;j<cols;j++){
      cells[i*cols + j] = (SDL_Rect){.w = w, .h = h, .x = j*w, .y = i*h};
    }
  }
  uint32_t *pixels[num_cells];
  int stride;//should be the same for all
  for(step=0;step<0xff;step++){
    for(i=0;i<num_cells;i++){
      SDL_LockTexture(textures[i], NULL, (void**)&pixels[i], &stride);
    }
    stride/=4;
    for(i=0;i<0xff;i++){
      for(j=0;j<stride;j++){
        pixels[0x0][i*stride + j] = make_rgb(j,step,i);
        pixels[0x1][i*stride + j] = make_rgb(step,i,j);
        pixels[0x2][i*stride + j] = make_rgb(i,j,step);

        pixels[0x3][i*stride + j] = make_rgb(j,step,0xff-i);
        pixels[0x4][i*stride + j] = make_rgb(step,0xff-i,j);
        pixels[0x5][i*stride + j] = make_rgb(0xff-i,j,step);

        pixels[0x6][i*stride + j] = make_rgb(j,0xff-step,0xff-i);
        pixels[0x7][i*stride + j] = make_rgb(0xff-step,0xff-i,j);
        pixels[0x8][i*stride + j] = make_rgb(0xff-i,j,0xff-step);

        pixels[0x9][i*stride + j] = make_rgb(j,0xff-step,i);
        pixels[0xa][i*stride + j] = make_rgb(0xff-step,i,j);
        pixels[0xb][i*stride + j] = make_rgb(i,j,0xff-step);
      }
    }
    for(i=0;i<num_cells;i++){
      SDL_UnlockTexture(textures[i]);
      SDL_RenderCopy(renderer, textures[i], NULL, cells+i);
    }
    //make sure that the updates to the screen happen as close to eachother as possible
    SDL_RenderPresent(renderer);
    SDL_Delay(15);
  }
  return 0;
}

int main(){
   main_yuv();
}
/*
    Does same thing as current code, but uses multiple windows & renderers for some reason
    for(i=0;i<num_windows;i++){
    SDL_CreateWindowAndRenderer(x, y, SDL_WINDOW_BORDERLESS, &windows[i], &renderers[i]);
    textures[i] = SDL_CreateTexture(renderers[i], SDL_PIXELFORMAT_ABGR8888,
                                    SDL_TEXTUREACCESS_STREAMING, 0xff, 0xff);
  }
  SDL_GetWindowPosition(windows[0], &xpos, &ypos);//center window
  SDL_GetWindowSize(windows[0], &width, &height);
  int x_step = width/2;
  int y_step = height/2;
  int x_center = xpos + x_step;
  int y_center = ypos + y_step;
  for(i=0;i<rows;i++){
    for(j=0;j<cols;j++){
      SDL_SetWindowPosition(windows[i*3 + j], x_center - ((3-2*j)*x_step), y_center - ((4-2*i)*y_step));
    }
    }*/
  /*
  SDL_SetWindowPosition(windows[3], xpos - width, ypos);
  //SDL_SetWindowPosition(windows[4], xpos, ypos);
  SDL_SetWindowPosition(windows[5], xpos + width, ypos);

  SDL_SetWindowPosition(windows[6], xpos - width, ypos + height);
  SDL_SetWindowPosition(windows[7], xpos, ypos + height);
  SDL_SetWindowPosition(windows[8], xpos + width, ypos + height);*/

  /*
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
    }*/
