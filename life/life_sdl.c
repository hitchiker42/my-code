#include "life.h"
#include "sdl_util.h"
typedef struct SDL_context {
  SDL_Texture *texture;
  SDL_Renderer *renderer;
  SDL_Window *window;
  int width;
  int height;
  int cell_width;
  int cell_height;
  uint32_t live_color;
  uint32_t dead_color;
  int delay;
} SDL_context;
int handle_window_event(SDL_WindowEvent *e){
  switch(e->event){
    case SDL_WINDOWEVENT_CLOSE:
      exit(0);
    default:
      return 0;
  }
}
int sdl_handle_event(SDL_Event *e){
  switch(e->type){
    case SDL_WINDOWEVENT:
      return handle_window_event((SDL_WindowEvent*)e);
    default:
      return 0;
  }
}

//TODO: figure out the best way to pass a cell size parameter
int draw_world(world *w, SDL_context *c){
  int i,j,k,l;
  //assume rgba pixels for now
  uint32_t *pixels;
  int stride;
  int lock = SDL_LockTexture(c->texture, NULL, (void**)&pixels, &stride);
  if(lock == -1){
    return -1;//couldn't lock texture
  }
  for(i=0;i<w->rows;i++){
    for(j=0;j<w->cols;j++){
      uint32_t color = w->grid[i*w->cols + j] ? c->live_color : c->dead_color;
      uint32_t *cell = (pixels+(i * c->width * c->cell_height) + (j * c->cell_width));
      //not super efficent, but I can't think of another way to do this
      for(k=0;k<c->cell_height;k++){
        for(l=0;l<c->cell_width;l++){
          cell[k*c->cell_width + l] = color;
        }
      }
    }
  }
  SDL_UnlockTexture(c->texture);
  SDL_RenderCopy(c->renderer, c->texture, NULL, NULL);
  SDL_RenderPresent(c->renderer);
}
int run_life(world *w, SDL_context *c){
  SDL_Event e;
  while(1){
    if(draw_world(w,c) == -1){
      return -1;
    }
    //If no events are posted this acts the same as SDL_Delay(c->delay);
    if(SDL_WaitEventTimeout(&e, c->delay)){
      int retval = sdl_handle_event(&e);
      if(retval != 0){
        return retval;
      }
    }
    //potentially wait more here if an event happened?
    step_world(w);
  }
  return 0;//never get here (for now);
}

int main(int argc, char **argv){
  SDL_context c;
  c.width = c.height = 0xff;
  SDL_Init(SDL_INIT_VIDEO);
  SDL_CreateWindowAndRenderer(c.width, c.height, 0, &c.window, &c.renderer);
  SDL_SetRenderDrawColor(c.renderer, 0xff, 0xff, 0xff, 0xff);//white
  SDL_RenderClear(c.renderer);
  SDL_RenderPresent(c.renderer);
  SDL_Delay(1000);
  c.texture = SDL_CreateTexture(c.renderer, SDL_PIXELFORMAT_ABGR8888,
                                SDL_TEXTUREACCESS_STREAMING, 0xff, 0xff);
  c.cell_height = c.cell_width = 1;
  c.live_color = make_rgb(0,0,0xff);//blue
  c.dead_color = 0xffffffff;
  c.delay = 100;
  world *w = init_world(c.width/c.cell_width, c.height/c.cell_height);
  randomize_grid(w);
  //this is a bit cheating
  run_life(w, &c);
}
