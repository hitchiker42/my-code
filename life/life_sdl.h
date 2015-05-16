#ifndef _LIFE_SDL_H_
#define _LIFE_SDL_H_
#include "life.h"
#include "sdl_util.h"
#include <setjmp.h>
#include <pthread.h>
#include <semaphore.h>

extern jmp_buf event_loop;
//atomic flag indicating if a simulation is running
extern volatile int8_t running_life;
//holds state for the main thread
typedef struct SDL_context {
  SDL_Texture *texture;
  SDL_Renderer *renderer;
  SDL_Window *window;
  int width;
  int height;
  /*int cell_width;
  int cell_height;
  uint32_t live_color;
  uint32_t dead_color;*/
  struct life_context *life_ctx;
  sem_t *sem;
  pthread_t id;
  world *w;//Soley for convience
  uint32_t delay;
  int texture_locked;
} SDL_context;
//holds state for the worker thread(s)
typedef struct life_context {
  uint32_t *pixels;
  uint32_t width;
  uint32_t height;
  //the actual pixel updating happens in this thread
  uint32_t cell_width;
  uint32_t cell_height;
  uint32_t live_color;
  uint32_t dead_color;
  uint32_t stride;//passed to SDL_LockTexture, probably unused
  int err;//set if an error is encountered during processing
  world *w;
  SDL_context *SDL_ctx;
  sem_t *sem;
  pthread_t id;
} life_context;
typedef SDL_Event SDL_MouseEvent;
#define atomic_read(ptr) __atomic_load_n(ptr, __ATOMIC_SEQ_CST)
#define atomic_write(ptr, val) __atomic_store_n(ptr, val, __ATOMIC_SEQ_CST)
//entry point for all event handlers
int sdl_handle_event(SDL_Event *e, SDL_context *c);
int draw_world(SDL_context *c);//Don't use this more that once or twice
int handle_window_event(SDL_WindowEvent *e, SDL_context *c);
int handle_keyboard_event(SDL_KeyboardEvent *e, SDL_context *c);
int handle_mouse_event(SDL_MouseEvent *e, SDL_context *c);
void update_pixels(life_context *c);
void __attribute__((noreturn)) run_life(SDL_context *c);
void __attribute__((noreturn)) run_event_loop(SDL_context *c);
void /*__attribute__((noreturn))*/ run_worker_thread(life_context *c);
#endif
