#ifndef __GUI_H__
#define __GUI_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <vector>

extern int my_sdl_event_type;//needs to be set using SDL_RegisterEvents
static const int default_window_width = 640;
static const int default_window_height = 480;
struct sdl_context {
  SDL_Window *window;
  SDL_Renderer *renderer;
  SDL_texture *texture;
  //This is a bit of a hack to prevent SDL from generating SDL_Quit when
  //I close the main window.
  SDL_Window *hack; 
  //Semaphore for synchronization (Passed to the initialization function)
  SDL_Semaphore *sem;
  //store an event just to make it eaiser to break event
  //handling into seperate functions
  SDL_Event evt;
  //Width and height of last image drawn, we need to save these so we can
  //redraw the image if the window is moved/resized/etc..
  int img_width;
  int img_height;
};

#ifdef __cplusplus
}
#endif
#endif /* __GUI_H__ */
