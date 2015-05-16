#include "life_sdl.h"
#include <setjmp.h>
jmp_buf event_loop;
uint32_t keymod_state;
int handle_window_resize(SDL_WindowEvent *e, SDL_context *c){
  //make the screen look nice
  SDL_RenderClear(c->renderer);
  SDL_RenderPresent(c->renderer);
  //  if(atomic_read(&running_life)){
  if(running_life){
    //atomic_write(&running_life, 0);
    running_life = 0;
    sem_wait(c->life_ctx->sem);
    SDL_UnlockTexture(c->texture);
  }
  int new_width = e->data1;
  int new_height = e->data2;
  //clear the winow (make it white)
  c->life_ctx->width = c->width = new_width;
  c->life_ctx->height = c->height = new_height;
  resize_world(c->w, new_width, new_height);
  randomize_grid(c->w);
  /*
    calling SDL_RenderClear here would allow the existing renderer to
    continue working, but at the same size
  */
  //SDL_RenderClear(c->renderer);
  //this may be a bit drastic/unecessary, but maybe not
  SDL_DestroyTexture(c->texture);
  SDL_DestroyRenderer(c->renderer);
  c->renderer = SDL_CreateRenderer(c->window, -1, 0);
  c->texture = SDL_CreateTexture(c->renderer, SDL_PIXELFORMAT_ABGR8888,
                                 SDL_TEXTUREACCESS_STREAMING, c->width, c->height);
  longjmp(event_loop, 1);
}
int handle_window_event(SDL_WindowEvent *e, SDL_context *c){
  switch(e->event){
    case SDL_WINDOWEVENT_CLOSE:
      exit(0);
      //We need to do some processing after we stop the worker thread for
      //a resize event, so we stop it here, and longjmp back to the top event
      //loop at the end, rather than returning to run_life
    case SDL_WINDOWEVENT_RESIZED:
      return handle_window_resize(e,c);
    default:
      return -1;
  }
}
int handle_keyboard_event(SDL_KeyboardEvent *e, SDL_context *c){
  if(handle_keymod_event(e) || e->type == SDL_KEYDOWN){
    return 0;//modifier key, or keypress, events trigger on key release
  }
  uint32_t key = e->keysym.sym;
  //keys which work the same if life is running or not go here
  if(key == '+' || key == '-' ||
     (key == '=' && get_keymod_state() & keymod_shift)){
    if(key == '+' || key == '='){
      c->delay += 100;
    } else if (key == '-'){
      c->delay = MIN(c->delay - 100, 0);
    }
    return 0;
  }
  //any keypress stops the current simulation
  if(/*atomic_read(&*/running_life > 0){
    atomic_write(&running_life, 1);
    return 1;
  }
  switch(e->keysym.sym){
    case 'q':
      exit(0);
    case 'r':
      randomize_grid(c->w);
      draw_world(c);
      return 1;
    case ' ':
    case '\r':
      run_life(c);
      return 1;
  }
  return -1;
}
int handle_mouse_event(SDL_MouseEvent *e, SDL_context *c){
  return -1;
}
int sdl_handle_event(SDL_Event *e, SDL_context *c){
  switch(e->type){
    case SDL_WINDOWEVENT:
      return handle_window_event((SDL_WindowEvent*)e, c);
    case SDL_KEYUP:
    case SDL_KEYDOWN:
      return handle_keyboard_event((SDL_KeyboardEvent*)e, c);
    case SDL_MOUSEMOTION:
    case SDL_MOUSEBUTTONUP:
    case SDL_MOUSEBUTTONDOWN:
    case SDL_MOUSEWHEEL:
      return handle_mouse_event((SDL_MouseEvent*)e, c);
    case SDL_QUIT:
      exit(0);
    default:
      return -1;
  }
}
//this is a bit cheating, but it's only called once per run
int draw_world(SDL_context *c){
  int lock = SDL_LockTexture(c->texture, NULL,
                             (void**)&c->life_ctx->pixels, &c->life_ctx->stride);
  if(lock == -1){
    return -1;//couldn't lock texture
  }
  update_pixels(c->life_ctx);
  SDL_UnlockTexture(c->texture);
  SDL_RenderCopy(c->renderer, c->texture, NULL, NULL);
  SDL_RenderPresent(c->renderer);
}
void __attribute__((noreturn)) run_life(SDL_context *c){
  SDL_Event e;
  uint32_t ticks1, ticks2, stride;
  uint32_t *pixels;
  //  atomic_write(&running_life, 1);
  running_life = 1;
  //run one iteration in the main thread, this makes syncronizing much eaiser
  draw_world(c);
  step_world(c->w);
  while(running_life){
    //update pixels in worker thread    
    int lock = SDL_LockTexture(c->texture, NULL, (void**)&pixels, &stride);
    if(lock < 0){
      longjmp(event_loop, lock);
    } else {
      c->life_ctx->pixels = pixels;
    }
    sem_post(c->sem);//tell worker thread to...work
    //wait, other thread computes and renders next frame in the mean time
    ticks1 = SDL_GetTicks();//time since SDL started
    if(SDL_WaitEventTimeout(&e, c->delay)){
      int retval = sdl_handle_event(&e, c);
      if(retval > 0){
        while(SDL_PollEvent(&e) && retval >= 0){
          retval = sdl_handle_event(&e, c);
        }
        //        atomic_write(&running_life, 0);
        running_life = 0;
        sem_wait(c->life_ctx->sem);
        SDL_UnlockTexture(c->texture);
        longjmp(event_loop, retval);
      } else {
        ticks2 = SDL_GetTicks();
        if((ticks1 + c->delay) > ticks2){
          SDL_Delay((ticks1 + c->delay) - ticks2);
        }
      }
    }
    sem_wait(c->life_ctx->sem);//make sure the worker thread is done
    SDL_UnlockTexture(c->texture);
    SDL_RenderCopy(c->renderer, c->texture, NULL, NULL);
    SDL_RenderPresent(c->renderer);
  }
  __builtin_unreachable();
}
static int event_filter(void *userdata, SDL_Event *e){
  if(e->type == SDL_MOUSEMOTION || e->type == SDL_MOUSEBUTTONUP ||
     e->type == SDL_MOUSEBUTTONDOWN || e->type == SDL_MOUSEWHEEL){
    return 0;
  }
  if(e->type == SDL_WINDOWEVENT && e->window.event == SDL_WINDOWEVENT_MOVED){
    return 0;
  }
  return 1;
}
void run_event_loop(SDL_context *c){
  SDL_Event e;
  int jmp_retval;
  SDL_SetEventFilter(event_filter, NULL);
  while(1){
    if(jmp_retval = setjmp(event_loop)){
      fprintf(stderr, "Caught longjmp with value %d\n",jmp_retval);
    }
    SDL_WaitEvent(&e);
    sdl_handle_event(&e, c);
  }
  __builtin_unreachable();
}
