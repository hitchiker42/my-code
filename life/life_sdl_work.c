#include "life_sdl.h"
/*  sigset_t *sigmask;
  sigemptyset(sigmask);
  sigaddset(sigmask, SIGUSR1);
  sigaction(SIGUSR1, &interupt_work);
  if(setjmp(work_loop)){
    //got here from signal handler
    sem_post(c->sem);//
    }
void jmp_to_work_loop(int signo){
  sem_post(c->sem);//this tells the main loop it can unlock the texture
  longjmp(work_loop);
} 
static struct sigaction interupt_work = {.sa_handler = jmp_to_work_loop};
jmp_buf event_loop;*/
void run_worker_thread(life_context *c){
  //  while(atomic_read(&running_life) >= 0){//this should only happen at program termination
  while(running_life >= 0){
    SDL_SemWait(c->SDL_ctx->sem);
    //I could install a signal handler for SIGUSR1 to jump to the top of this loop
    //but that seems excessive, though this way seems excessive too
    update_pixels(c);
    //    if(atomic_read(&running_life) < 1){
    if(running_life < 1){
      SDL_SemPost(c->sem);
      continue;
    }
    step_world(c->w);
    //    if(atomic_read(&running_life) < 1){
    if(running_life < 1){
      step_world_back(c->w);//undo the last step
      SDL_SemPost(c->sem);
      continue;
    }
    SDL_SemPost(c->sem);
  }
}
  
void update_pixels(life_context *c){
  world *w = c->w;
  int i,j,k,l;
  for(i=0;i<(w->rows - (c->cell_height - 1));i++){
    for(j=0;j<(w->cols - (c->cell_width -1));j++){
      uint32_t color = w->grid[i*w->cols + j] ? c->live_color : c->dead_color;
      uint32_t *cell =
        c->pixels + (i * c->width * c->cell_height) + (j * c->cell_width);
      //not super efficent, but I can't think of another way to do this
      for(k=0;k<c->cell_height;k++){
        for(l=0;l<c->cell_width;l++){
          cell[k*c->cell_width + l] = color;
        }
      }
    }
  }
}
/*
  TODO: Put Code for rendering and steping world into a seperate thread.
  The code for rendering (in the seperate thread) should just deal
  with an array of pixels, the main thread needs to lock/unlock the 
  texture and actually draw to the screen.
  
  The eaisest way to syncronize, I think, is to have a semaphore in each thread,
  when the thread needs to syncronize it posts on it's own semaphore and waits
  on the other thread's semaphore

  This should happen concurrently with the main thread waiting, i.e
  main thread:
   render texture to screen;
   wait for timeout/event
     event:
      handle event, may involve communicating with other thread(s);
     timeout:
      SDL_SemPost(c->sem);//or something
      SDL_SemWait(w->sem);//or whatever
   loop
  worker
   //the order doesn't really matter
   Update pixel array for texture
   Step grid
   SDL_SemPost(w->sem);
   SDL_SemWait(c->sem);

*/
/*
void __attribute__((noreturn)) run_life(SDL_context *c){
  SDL_Event e;
  c->running_life = 1;
  while(c->running_life){
    if(draw_world(c) == -1){
      longjmp(event_loop, -1);
    }
    step_world(c->w);
    //If no events are posted this acts the same as SDL_Delay(c->delay);
    if(SDL_WaitEventTimeout(&e, c->delay)){
      int retval = sdl_handle_event(&e, c);
      if(retval != 0){
        c->running_life = 0;//probably unecessary
        longjmp(event_loop, retval);
      } else {
        SDL_Delay(c->delay/2);//eh, good enough
      }
    }
  }
  __builtin_unreachable();
  }*/
