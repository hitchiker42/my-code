#ifndef _SDL_UTIL_H_
#define _SDL_UTIL_H_
#include <stdint.h>
#include <SDL2/SDL.h> //includes all other sdl headers
union rgba {
  struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;
  } rgba;
  uint32_t pixel;
};
#define make_rgb(r,g,b) make_rgba(r,g,b,0xff)
static inline uint32_t make_rgba(uint8_t r, uint8_t g, uint8_t b, uint8_t a){
  union rgba temp = {.rgba = {.r=r, .g=g, .b=b, .a=a}};
  return temp.pixel;
}
static inline uint8_t *IYUV_getY(uint8_t *p, int w, int h){
  return p;
}
static inline uint8_t *IYUV_getU(uint8_t *p, int w, int h){
  return p + w*h;
}
static inline uint8_t *IYUV_getV(uint8_t *p, int w, int h){
  return p + w*h + (w/2)*(h/2);
}

//Keyboard Input
//Note: Keycode == key in current layout, Scancode = Physical key
/*Since SDL (weirdly) doesn't translate modifer keycodes to scancodes
  we need to read key_up/down events for modifer keys and use them to
  maintain an application level modifer mask */
enum keymod {
  //there are no more than 512 scancodes (from SDL_scancode.h)
  //so if we leave 12 bits for the scan code that's more than enough
  keymod_lshift = 0x01000,
  keymod_rshift = 0x02000,
  keymod_shift  = 0x03000,//lshift|rshift
  keymod_lctrl  = 0x04000,
  keymod_rctrl  = 0x08000,
  keymod_ctrl   = 0x0C000,//lctrl|rctrl
  keymod_lalt   = 0x10000,
  keymod_ralt   = 0x20000,
  keymod_alt    = 0x30000,//lalt|ralt
  keymod_lsuper = 0x40000,
  keymod_rsuper = 0x80000,
  keymod_super  = 0xC0000,//lsuper|rsuper
};
static const int keyboard_modifier_mask = 0xff000;
static const int keyboard_keysym_mask   = 0x00fff;
#define get_keysym(key) (key & keyboard_keysym_mask)
#define get_key_modifers(key) (key & keyboard_modifer_mask)
extern uint32_t keymod_state;//needs to be declared by the application
//this should probaly go in a seperate file
static int handle_keymod_event(SDL_KeyboardEvent *e){
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
static uint32_t get_keymod_state(){
  return keymod_state;
}
//convience macros
#define get_keymod_ctrl_state() (get_keymod_state() & keymod_ctrl)
#define get_keymod_shift_state() (get_keymod_state() & keymod_shift)
#define get_keymod_alt_state() (get_keymod_state() & keymod_alt)
#define get_keymod_super_state() (get_keymod_state() & keymod_super)

static int
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
           
#endif
