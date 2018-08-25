#ifndef __GUI_H__
#define __GUI_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "sqlite3.h"
extern SDL_EventType jpeg_event_type;//needs to be set using SDL_RegisterEvents
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
  SDL_Texture *texture;
  //This is a bit of a hack to prevent SDL from generating SDL_Quit when
  //I close the main window.
  SDL_Window *hack; 
  //Semaphore for synchronization (Passed to the initialization function)
  SDL_sem *sem;
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
void destroy_sdl_context(struct sdl_context *ctx);
struct sdl_context* create_sdl_context(SDL_sem *sem);
int sdl_main_loop(void *sem);
SDL_sem* launch_sdl_thread();
#ifdef __cplusplus
}
/*
  In the SDL_UserEvent struct we get a 32 bit integer and 2 void pointers to store data.
  We need more than this to store all the data we need, but we can use it to store 
  what we need to generate that information. We store a sqlite3_stmt* in one of the pointers
  and either a single id in the second or a pointer to an array of ids. We use the
  integer to store the length of this array or 0 if it is just an integer.
*/
inline void init_jpeg_user_event(SDL_Event *evt, 
                                 const void *data, uintptr_t size){
  memset(evt, 0, sizeof(SDL_Event));
  evt->type = jpeg_event_type;
  evt->user.data1 = (void*)data;
  evt->user.data2 = (void*)size;
  evt->user.code = 0;
}
inline void init_jpeg_user_event(SDL_Event *evt, sqlite3_stmt* stmt,
                                 int* ids, int num_ids){
  memset(evt, 0, sizeof(SDL_Event));
  evt->type = jpeg_event_type;
  evt->user.data1 = (void*)stmt;
  evt->user.data2 = (void*)ids;
  evt->user.code = num_ids;
}
#endif
#endif /* __GUI_H__ */
