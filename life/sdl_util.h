#ifndef _SDL_UTIL_H_
#define _SDL_UTIL_H_
#include <stdint.h>
#include <SDL2/SDL.h> //includes all other sdl headers
union rgba {
  uint32_t pixel;
  SDL_Color sdl;//literally the same thing as the struct below
  struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;
  };
} rgba_pixel;
#define make_rgb(r,g,b) make_rgba(r,g,b,0xff)
//assume little endian
#define expand_rgba(rgba) ((rgba << 24)&0xff), ((rgba << 16)&0xff),     \
    ((rgba << 8)&0xff), ((rgba)&0xff)
static inline uint32_t make_rgba(uint8_t r, uint8_t g, uint8_t b, uint8_t a){
  union rgba temp = {.r=r, .g=g, .b=b, .a=a};
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
int handle_keymod_event(SDL_KeyboardEvent *e);
static uint32_t get_keymod_state(){
  return keymod_state;
}
//convience macros
#define get_keymod_ctrl_state() (get_keymod_state() & keymod_ctrl)
#define get_keymod_shift_state() (get_keymod_state() & keymod_shift)
#define get_keymod_alt_state() (get_keymod_state() & keymod_alt)
#define get_keymod_super_state() (get_keymod_state() & keymod_super)

int
SDL_CreateWindowRendererAndTexture(int w, int h, uint32_t window_flags,
                                   uint32_t texture_format,
                                   uint32_t texture_access,
                                   SDL_Window **window, SDL_Renderer **renderer,
                                   SDL_Texture **texture);
static void clear_screen(SDL_Renderer *renderer, union rgba rgba){
  SDL_SetRenderDrawColor(renderer, rgba.a, rgba.g, rgba.b, rgba.a);
  SDL_RenderClear(renderer);
  SDL_RenderPresent(renderer);
}
#endif
