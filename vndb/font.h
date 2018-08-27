#ifndef __FONT_H__
#define __FONT_H__
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <SDL2/SDL.h>
//Kindof weird but ft2build.h defines macros for the other freetype2
//header files, we then use these macros in future include statements.
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H
#include <memory>
#include <string_view>
#include <utility>
#include "unicode.h"
#include "logger.h"
extern std::unique_ptr<util::logger> vndb_log;
struct cached_glyph {
  int32_t codepoint;
  FT_UInt index;
  FT_Bitmap bitmap;//8bit greyscale image.
  int minx;
  int maxx;
  int miny;
  int maxy;
  int yoffset;
  int advance;
};
void output_glyph(cached_glyph *glyph, FILE *out = stdout);
//simple RAII wrapper around FT_Library.
struct ft_library_wrapper {
  FT_Library lib = nullptr;
  ft_library_wrapper(){
    FT_Init_FreeType(&lib);
  }
  ft_library_wrapper(const ft_library_wrapper& other) = delete;
  ft_library_wrapper(ft_library_wrapper&& other)
    : lib{other.lib} {
    other.lib = nullptr;
  }
  ~ft_library_wrapper(){
    FT_Done_FreeType(lib);
  }
  FT_Library unwrap(){
    return lib;
  }
  operator FT_Library(){
    return lib;
  }
};
struct ft_face_wrapper {
  FT_Face face = nullptr;
  //Metrics of the font converted to pixel values and rounded to
  //the nearest integer >= the fractional value.
  int height;
  int ascent;
  int descent;
  int lineskip;
  //max size and advance for use in computing upper bounds of text size.
  int max_advance;
  int max_width;
  int max_height;
  //These cached values could be marked mutable if necessary.
  //Stats for the (non-ascii) cache, in theory with typical use 
  //cache_misses should be 1/2 of glyphs loaded, since we load each glyph
  //twice when rendering a string, once for sizing and once for rendering.  
  int glyphs_loaded = 0;
  int cache_misses = 0;
  //the glyph we are currently working with.
  cached_glyph *current = nullptr;
  //Cache for printable ascii characters the first 32 characters are control
  //or nul and 127 is delete. index = codepoint - 32
  static constexpr size_t ascii_cache_size = 95;
  cached_glyph ascii_cache[ascii_cache_size] = {};
  //Cache for non-ascii characters index = codepoint % size. size is 163 since
  //thats a prime and ensures equidistribution.
  static constexpr size_t non_ascii_cache_size = 163;
  cached_glyph non_ascii_cache[non_ascii_cache_size] = {};
  //Maximum number of lines that can be rendered at once, it's completely
  //arbitrary but allows allocating line widths on the stack.
  static constexpr size_t max_lines_rendered = 100;
  
  //A pair of functions for use in a more C-like style.
  static ft_face_wrapper* init_create(FT_Library lib, const char *font_file,
                                      int pt_size, int font_index = 0){
    ft_face_wrapper *ret = (ft_face_wrapper*)calloc(sizeof(ft_face_wrapper),1);
    int err = ret->init(lib, font_file, pt_size, font_index);
    if(err){
      free(ret);
      return nullptr;
    }
    return ret;
  }
  static void destroy_free(ft_face_wrapper *face){
    if(face){
      face->~ft_face_wrapper();
      free(face);
    }
  }    
  ft_face_wrapper(ft_face_wrapper &other) = delete;
  ~ft_face_wrapper(){
    //If face is null then we assume nothing else needs cleaning up,
    //this could be used to make a potential move constructor more efficent.
    if(!face){
      return;
    }
    flush_cache();
    FT_Done_Face(face);
  }
  //Function which actually does the work of loading/rendering the glyph.
  static FT_Error Load_Glyph(ft_face_wrapper *face, int32_t codepoint,
                             cached_glyph* cglyph);
  //free the bitmap associated with cglyph and zero the codepoint/index values
  static void flush_cached_glyph(cached_glyph *cglyph);

  int init(FT_Library lib, const char *font_file,
           int pt_size, int font_index = 0);
  int init_ascii_cache();

  FT_Error Load_Glyph(int32_t codepoint){
    return Load_Glyph(this, codepoint, this->current);
  }
  FT_Error Load_Glyph(int32_t codepoint, cached_glyph *cglyph){
    return Load_Glyph(this, codepoint, cglyph);
  }
  void flush_cache(){
    for(size_t i = 0; i < ascii_cache_size; i++){
      flush_cached_glyph(ascii_cache + i);
    }
    for(size_t i = 0; i < non_ascii_cache_size; i++){
      flush_cached_glyph(non_ascii_cache + i);
    }
  }
  int get_glyph(int32_t codepoint) {
    if(codepoint >= 32 && codepoint < 127){
      this->current = &this->ascii_cache[codepoint-32];
      return 0;
    } else {
      glyphs_loaded++;
      int idx = codepoint % non_ascii_cache_size;
      this->current = &this->non_ascii_cache[idx];
      if(non_ascii_cache[idx].codepoint == codepoint){
        return 0;
      } else {
        cache_misses++;
        int ret = Load_Glyph(codepoint, &this->non_ascii_cache[idx]);
        return ret;
      }
    }
  }
  //Compute an upper bound for the bounding box for rendering 'text'
  int size_text(std::string_view text, int *w, int *h);
  int size_text_multiline(std::string_view text, int *w, int *h,
                          int *line_widths);
  void size_text_quick(std::string_view text, int *w, int *h){
    int len = util::utf8_strlen(text);
    *w = ((len - 1) * (max_width + max_advance)) + max_width;
    *h = max_height;    
  }    
  std::pair<int,int> size_text(std::string_view text){
    std::pair<int,int> ret = {-1,-1};
    size_text(text, &ret.first, &ret.second);
    return ret;
  }
  std::pair<int,int> size_text_quick(std::string_view text){
    std::pair<int,int> ret = {-1,-1};
    size_text_quick(text, &ret.first, &ret.second);
    return ret;
  }
  /*
    Sizes the text, creates a bounding box, sets alpha to 0 and rgb to color then
    renders the text setting alpha to the grayscale value of the glyph bithmap.
  */
  SDL_Texture* render_utf8_text_argb(std::string_view text,
                                     int color, SDL_Renderer *renderer);
  SDL_Texture* render_utf8_text_argb_multiline(std::string_view text,
                                               int color, 
                                               SDL_Renderer *renderer);
  /*
    Sizes the text, creates a bounding box, sets the color to bg then
    renders the text setting the pixel to bg+(((fg-bg)*greyscale_value)/255).
    w and h are set to the actual size of the bounding box which may
    be slightly smaller than the total size of the texture.
  */
  SDL_Texture* render_utf8_text_rgb(std::string_view text,
                                    int fg, int bg, SDL_Renderer *renderer,
                                    int *w = nullptr, int *h = nullptr);
  SDL_Texture* render_utf8_text_rgb_multiline(std::string_view text,
                                              int fg, int bg, 
                                              SDL_Renderer *renderer,
                                              int *w = nullptr, 
                                              int *h = nullptr);
  /*
    Renders the text into the array 'pixels' which has dimensions
    (*w_ptr)x(*h_ptr) with each entry having num_components bytes.
    The greyscale value of the text will be written into the 'alpha_offset'th
    byte of each group of 'num_components' bytes. The size of the text
    will be written into w_ptr and h_ptr. Returns non-zero on error.
  */
  int render_utf8_text_alpha(std::string_view text,
                             void *pixels,
                             int *w_ptr, int *h_ptr,
                             int num_components = 4,
                             int alpha_offset = 0);
};
struct vndb_font_ctx* vndb_font_ctx_init(const char *sans_path,
                                         const char *serif_path,
                                         const char *mono_path);
struct vndb_font_ctx {
  static constexpr const char *sans_font_path = "fonts/NotoSansCJK.ttc";
  static constexpr const char *serif_font_path = "fonts/NotoSerifJP.otf";
  static constexpr const char *mono_font_path = "fonts/NotoSansMonoJP.otf";
  FT_Library lib;
  ft_face_wrapper sans_font;
  ft_face_wrapper serif_font;
  ft_face_wrapper mono_font;
};
#endif /* __FONT_H__ */

/* Local Variables: */
/* mode: c++ */
/* End: */
