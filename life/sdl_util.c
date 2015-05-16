#include "life_sdl.h"
int
SDL_CreateWindowRendererAndTexture(int w, int h, uint32_t window_flags,
                                       uint32_t texture_format,
                                       uint32_t texture_access,
                                       SDL_Window **window, SDL_Renderer **renderer,
                                       SDL_Texture **texture){
  int err = SDL_CreateWindowAndRenderer(w, h, window_flags, window, renderer);
  if(err == -1){
    return -1;
  }
  *texture = SDL_CreateTexture(*renderer, texture_format, texture_access, w, h);
  if(*texture == NULL){
    return -2;
  }
  return 0;
}
int handle_keymod_event(SDL_KeyboardEvent *e){
  uint32_t mod = 0;
  switch(e->keysym.sym){
    case SDLK_LCTRL:
      mod = keymod_lctrl; break;
    case SDLK_RCTRL:
      mod = keymod_rctrl; break;
    case SDLK_LSHIFT:
      mod = keymod_lshift; break;
    case SDLK_RSHIFT:
      mod = keymod_rshift; break;
    case SDLK_LALT:
      mod = keymod_lalt; break;
    case SDLK_RALT:
      mod = keymod_ralt; break;
    case SDLK_LGUI:
      mod = keymod_lsuper; break;
    case SDLK_RGUI:
      mod = keymod_rsuper; break;
    default:
      return 0;//not a modifier key
  }
  if(e->type == SDL_KEYDOWN){
    keymod_state |= mod;
  } else {
    keymod_state &= (~mod);
  }
  return 1;
}
/* char *sdl_readline(TTF_Font *font, SDL_Renderer *renderer, SDL_Texture *texture, */
/*                    SDL_Rect TextBox, union rgba fg, union rgba bg){ */
  
