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

extern int jpeg_event_type;//needs to be set using SDL_RegisterEvents
//indicates if the sdl thread is running, mostly used to check if
//initialization was successful.
extern int sdl_running;
static const int default_window_width = 640;
static const int default_window_height = 480;
//This is internal to sdl_gui.c, comunication with the sdl thread is done
//using a semaphore and the sdl event queue.
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
//  I don't really have a use for this, but if I ever need to check if
//  I failed at rendering a jpeg I can uncomment this.
//  int err;
  //Width and height of last image drawn, we need to save these so we can
  //redraw the image if the window is moved/resized/etc..
  int img_width;
  int img_height;
};
static inline void init_jpeg_user_event(SDL_event *evt, void *data, size_t sz){
  memset(evt, 0, sizeof(SDL_Event));
  evt->type = my_sdl_event_type;
  evt->user.data1 = data;
  evt->user.data2 = sz;
}
//void destroy_sdl_context(sdl_context *ctx);
//sdl_context* create_sdl_context(SDL_Semaphore *sem);
SDL_Semaphore* launch_sdl_thread();
#ifdef __cplusplus
}
#endif
#endif /* __GUI_H__ */
