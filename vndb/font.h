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
#include "unicode.h"
struct cached_glyph {
  uint32_t codepoint;
  FT_UInt index;
  FT_Bitmap bitmap;//8bit greyscale image.
  int minx;
  int maxx;
  int miny;
  int maxy;
  int yoffset;
  int advance;
};
//simple RAII wrapper around FT_Library.
struct ft_library_wrapper {
  FT_Library lib = nullptr;
  ft_library_wrapper(){
    FT_Init_FreeType(lib);
  }
  ft_library_wrapper(const ft_library_wrapper& other) = delete;
  ft_library_wrapper(const ft_library_wrapper&& other)
    : lib{other.lib} {
    other.lib = nullptr;
  }
  ~ft_library_wrapper(){
    FT_Done_FreeType(lib);
  }
  FT_Library unwrap(){
    return lib;
  }
  operator FT_Library*(){
    return lib;
  }
}
struct ft_face_wrapper {
  FT_Face face;
  //Metrics of the font converted to pixel values and rounded to
  //the nearest integer >= the fractional value.
  int height;
  int ascent;
  int descent;
  int lineskip;
  //Cache for ascii characters. We could/may use the first 31 entries
  //to cache other glyphs (using the codepoint % 31 to find the slot).
  cached_glyph *current;
  cached_glyph cache[128];  
};
ft_face_wrapper* ft_face_wrapper_init(FT_Library lib, const char *filename,
                                      int ptsize, int index);
struct vndb_font_ctx* vndb_font_ctx_init(const char *sans_path,
                                         const char *serif_path,
                                         const char *mono_path);
struct vndb_font_ctx {
  static constexpr const char *sans_font_path = "data/NotoSansJP.otf";
  static constexpr const char *serif_font_path = "data/NotoSerifJP.otf";
  static constexpr const char *mono_font_path = "data/NotoSansMonoJP.otf";
  FT_Library lib;
  ft_face_wrapper sans_font;
  ft_face_wrapper serif_font;
  ft_face_wrapper mono_font;
};
/*
  Sizes the text, creates a bounding box, sets alpha to 0 and rgb to color then
  renders the text setting alpha to the grayscale value of the glyph bithmap. 
*/
SDL_Texture* render_utf8_text_rgba(const char* text, int color,
                                   SDL_Renderer *renderer);

SDL_Texture* render_utf8_text_rgb(const char* text, int fg, int bg,
                                  SDL_Renderer *renderer, 
                                  int *w, int *h);


                                  
int render_glyph(void *renderer_data,
                 FT_Bitmap *bmp)
#endif
#endif /* __FONT_H__ */

/* Local Variables: */
/* mode: c++ */
/* End: */
