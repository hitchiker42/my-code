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
//Since SDL (weirdly) doesn't translate modifer keycodes to scancodes
//we need to read key_up/down events for modifer keys and use them to
//maintain an application level modifer mask
enum keymod {
  //there are no more than 512 scancodes (from SDL_scancode.h)
  //so
  keymod_lshift = 0x01000,
  keymod_rshift = 0x02000,
  keymod_shift  = 0x03000,//keymod_lshift|keymod_rshift
  keymod_lctrl  = 0x04000,
  keymod_rctrl  = 0x08000,
  keymod_ctrl   = 0x0C000,//lctrl|rctrl
  keymod_lalt   = 0x10000,
  keymod_ralt   = 0x20000,
  keymod_alt    = 0x30000,//lalt|ralt
  keymod_lsuper = 0x40000,
  keymod_rsuper = 0x80000,
  keymod_super  = 0xC0000,//lsuper|rsuper
  //There could be a keymod_esc which would work the way it does on terminals,
  //in that it modifies the next pressed key, but I'm going to leave that up
  //to the application code
};
static const int keyboard_modifier_mask = 0xff000;
static const int keyboard_keysym_mask = 0xfff;
#define get_keysym(key) (key & keyboard_keysym_mask)
#define get_key_modifers(key) (key & keyboard_modifer_mask)
