#include "font.h"
#include <assert.h>
//inline floor/ceil for 26.6 fixed point integers
static inline int ft_floor_26dot6(long x){
  return x >> 6;
}
static inline int ft_ceil_26dot6(long x){
  return (x + 0x3f) >> 6;
}
//fairly imprecise rounding, but better than just straight floor/ceil.
static inline int ft_round_26dot6(long x){
  return (x + 0x1f) >> 6;
}
void output_glyph(cached_glyph *glyph, FILE *out){
  for(unsigned int row = 0; row < glyph->bitmap.rows; row++){
    for(unsigned int column = 0; column < glyph->bitmap.width; column++){
      fprintf(out, "%02X",
              glyph->bitmap.buffer[row * glyph->bitmap.pitch + column]);
    }
    fprintf(out, "\n");
  }
}
void print_glyph_metrics(cached_glyph *glyph, FILE* out){
  fprintf(out, "(xmin,xmax) = (%d,%d), (ymin,ymax) = (%d,%d)\n"
          "yoffset = %d, advance = %d\n",
          glyph->minx, glyph->maxx, glyph->miny, glyph->maxy,
          glyph->yoffset, glyph->advance);
}
//index should usually (always for the fonts I'm using) be 0.
int ft_face_wrapper::init(FT_Library lib, const char *filename,
                          int ptsize, int index){
  vndb_log->log_debug("Loading font from %s at %d pt (index %d).\n",
                      filename, ptsize, index);
  FT_Error err;
  err = FT_New_Face(lib, filename, index, &this->face);
  if(err){ return err; }
  //Assume default unicode charmap exists, need to call this if not.
  //error = FT_Set_Charmap(this->face, ...)
  err = FT_Set_Char_Size(
    face,
    0, ptsize*64, /* width/height in 1/64th of points */
    0, 0 /* horizontal/vertical device resolution dpi/ppi (default 72)*/
  );
  if(err){ return err; }
  //Get scaled font metrics
  FT_Face face = this->face;
  FT_Fixed yscale = face->size->metrics.y_scale;
  FT_Fixed xscale = face->size->metrics.x_scale;
  //Convert metrics to 26.6 font units, then round up to an integer.
  this->ascent = ft_ceil_26dot6(FT_MulFix(face->ascender, yscale));
  this->descent = ft_ceil_26dot6(FT_MulFix(face->descender, yscale));
  this->lineskip = ft_ceil_26dot6(FT_MulFix(face->height, yscale));
  this->height = (this->ascent - this->descent) + 1;/*1 is the baseline*/
  this->max_advance = ft_ceil_26dot6(FT_MulFix(face->max_advance_width,
                                               xscale));
  this->max_width = ft_ceil_26dot6(FT_MulFix(face->bbox.xMax, xscale)-
                                   FT_MulFix(face->bbox.xMin, xscale));
  this->max_height = ft_ceil_26dot6(FT_MulFix(face->bbox.yMax, yscale)-
                                    FT_MulFix(face->bbox.yMin, yscale));
  vndb_log->log_debug("Font Metrics: ascent = %d, descent = %d\n"
                      "              lineskip = %d, height = %d\n"
                      "              max glyph (w,h,advance) = (%d,%d,%d)",
                      this->ascent, this->descent, this->lineskip,
                      this->height, this->max_advance,
                      this->max_width, this->max_height);
  return this->init_ascii_cache();
}
int ft_face_wrapper::init_ascii_cache(){
  vndb_log->log_debug("Initializing font cache of ascii characters");
  static constexpr int codepoint_offest = 32;
  for(size_t i = 0; i < this->ascii_cache_size; i++){
    int err = this->Load_Glyph(i + codepoint_offest, this->ascii_cache + i);
    if(err){
      fprintf(stderr, "Error loding glyph %c.\n", (int)i + codepoint_offest);
      return err;
    }
  }
  return 0;
}

void ft_face_wrapper::flush_cached_glyph(cached_glyph *cglyph){
  free(cglyph->bitmap.buffer);//buffer may already be null
  cglyph->bitmap.buffer = nullptr;
  cglyph->index = 0;
  cglyph->codepoint = 0;
}
FT_Error ft_face_wrapper::Load_Glyph(ft_face_wrapper *font,
                                     int32_t codepoint,
                                     cached_glyph* cglyph){
  cglyph->index = FT_Get_Char_Index(font->face, codepoint);
  FT_Error error = FT_Load_Glyph(font->face, cglyph->index, FT_LOAD_DEFAULT);
  if(error){
    return error;
  }
  FT_GlyphSlot glyph = font->face->glyph;
  FT_Glyph_Metrics* metrics = &glyph->metrics;
  /* Get the bounding box
     May be able to replace this with a call to FT_Glyph_Get_CBox
   */
  cglyph->minx = ft_floor_26dot6(metrics->horiBearingX);
  cglyph->maxx = ft_ceil_26dot6(metrics->horiBearingX + metrics->width);
  cglyph->maxy = ft_floor_26dot6(metrics->horiBearingY);
  cglyph->miny = cglyph->maxy - ft_ceil_26dot6(metrics->height);
  cglyph->yoffset = font->ascent - cglyph->maxy;
  cglyph->advance = ft_ceil_26dot6(metrics->horiAdvance);
  /* Render the glyph */
  error = FT_Render_Glyph(glyph, FT_RENDER_MODE_NORMAL);
  if(error){
    return error;
  }
  //I'm not sure if this is technically allowed by the freetype api,
  //but it's what SDL_TTF does. (Freetype provides FT_Bitmap_Init and
  //FT_Bitmap_Copy which presumably you should use, but this is eaiser).
  FT_Bitmap* src = &glyph->bitmap;
  FT_Bitmap* dst = &cglyph->bitmap;
  memcpy(dst, src, sizeof(*dst));
  dst->buffer = (unsigned char*)malloc(src->rows * src->pitch);
  //I see no reason this shouldn't work, but if it doesn't try copying
  //line by line (see below)
  memcpy(dst->buffer, src->buffer, src->rows * src->pitch);
  /*  for(int i = 0; i < src->rows; i++){
      memcpy(dst->buffer + i * dst->pitch,
      src->buffer + i * src->pitch, src->pitch);
      }*/
  return 0;
}

int ft_face_wrapper::size_text(std::string_view text, int *w_ptr, int *h_ptr){
  auto codepoints = util::utf8_string_iter::utf8_iter_range(text);
  int minx = 0, maxx = 0;
  int prev_index = -1;//index of last codepoint, for kerning.
  int penx = 0;
  cached_glyph *&glyph = this->current;
  //iterate over the codepoints
  for(auto cp : codepoints){
    int err = this->get_glyph(cp);
    //fprintf(stderr, "Metrics for codeponit %d:\n", cp);
    //print_glyph_metrics(this->current, stderr);
    if(err){ return err; }
    if(prev_index > 0){
      FT_Vector delta;
      FT_Get_Kerning(this->face, prev_index, glyph->index,
                     ft_kerning_default, &delta);
      penx += ft_round_26dot6(delta.x);
    }
    if(glyph->minx < 0){
      fprintf(stderr,"Glyph %s(%d) has a minx of %d.\n",
              util::utf8_encode_char(cp).data(), cp, glyph->minx);
    }
    minx = std::min(minx, penx + glyph->minx);
    //shouldn't glyph->advance always be > than glyph->maxx?
    maxx = std::max(maxx, penx + std::max(glyph->maxx, glyph->advance));
    penx += glyph->advance;
    prev_index = glyph->index;
  }
  *w_ptr = (maxx - minx);
  *h_ptr = this->height;
  return 0;
}
//May merge this with the previous function, if/when I start using
//fast text sizing (i.e just counting chracters and multiplying by
//the maximum glyph size).
//Knowing line widths is necessary for centering text.
int ft_face_wrapper::size_text_multiline(std::string_view text,
                                         int *w_ptr, int *h_ptr,
                                         int *line_widths){
  auto codepoints = util::utf8_string_iter::utf8_iter_range(text);
  int minx = 0, maxx = 0, maxy = this->height;
  int prev_index = -1;//index of last codepoint, for kerning.
  int penx = 0;
  int num_lines = 0;
  cached_glyph *&glyph = this->current;
  //iterate over the codepoints
  for(auto cp : codepoints){
    if(cp == '\r'){
      prev_index = 0;
      continue;
    }
    if(cp == '\n'){
      prev_index = 0;
      maxy += this->height;
      if(++num_lines >= this->max_lines_rendered){
        //return -1 for now, ultimately we should just return early,
        //but if we do we need to return the offset into the text.
        return -1;
      }
      *line_widths++ = penx;
      continue;
    }
    int err = this->get_glyph(cp);
    //fprintf(stderr, "Metrics for codeponit %d:\n", cp);
    //print_glyph_metrics(this->current, stderr);
    if(err){ return err; }
    if(prev_index > 0){
      FT_Vector delta;
      FT_Get_Kerning(this->face, prev_index, glyph->index,
                     ft_kerning_default, &delta);
      penx += ft_round_26dot6(delta.x);
    } else {
      penx = 0;
    }

    minx = std::min(minx, penx + glyph->minx);
    //shouldn't glyph->advance always be > than glyph->maxx?
    maxx = std::max(maxx, penx + std::max(glyph->maxx, glyph->advance));
    penx += glyph->advance;
    prev_index = glyph->index;
  }
  *w_ptr = maxx - minx;
  *h_ptr = maxy;
  return 0;
}
//A union for an argb tuplet stored in big-endian mode,
//when constructing from an integer it will unpack it in
//an endian indepent way.
union argb_color {
  constexpr argb_color(uint8_t a,uint8_t r,uint8_t g,uint8_t b)
    : a{a}, r{r}, g{g}, b{b} {}
  constexpr argb_color(uint8_t bytes[4])
    : a{bytes[0]}, r{bytes[1]}, g{bytes[2]}, b{bytes[3]} {}
  constexpr explicit argb_color(uint32_t packed, bool swap_bytes = false)
    : packed{packed} {
    if(swap_bytes){
      std::swap(bytes[0], bytes[3]);
      std::swap(bytes[1], bytes[2]);
    }
  }
  constexpr argb_color()
    : packed{0} {}
  uint32_t packed;
  uint8_t bytes[4];
  struct {
    uint8_t a;
    uint8_t r;
    uint8_t g;
    uint8_t b;
  };
};
static inline SDL_Texture*
size_text_and_init_texture(ft_face_wrapper *font,
                           std::string_view text,
                           int &w, int &h,
                           SDL_Renderer *renderer){
  vndb_log->log_debug("Rendering text: %.*s",
                      (int)text.size(), text.data());
  int err = font->size_text(text, &w, &h);
  if(err){
    return nullptr;
  }
  vndb_log->log_debug("Size of text is %d x %d", w, h);
  //We use STREAMING rather than STATIC since we need to access the pixels,
  //directly, not because we expect the texture to change.
  SDL_Texture* tex = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB32,
                                       SDL_TEXTUREACCESS_STREAMING, w, h);
  if(!tex){
    fprintf(stderr, "Error creating texture.\n");
    return nullptr;
  }
  SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);
  return tex;
}
SDL_Texture* ft_face_wrapper::render_utf8_text_argb(std::string_view text,
                                                    int rgb,
                                                    SDL_Renderer *renderer){

  int w, h;
  SDL_Texture *tex = size_text_and_init_texture(this, text, w, h, renderer);
  if(!tex){ return nullptr; }
  argb_color color(rgb, true);
  vndb_log->log_debug(
    "Text color is: a = %02X, r = %02X, g = %02X, b = %02X.\n"
    "packed = %08X\n", color.a, color.r, color.g, color.b, color.packed);
  void *pixels;
  int stride;
  int err = SDL_LockTexture(tex, nullptr, &pixels, &stride);
  if(err){
    fprintf(stderr, "Error locking texture.\n");
    //I'd use goto error here, but I can't because C++.
    SDL_DestroyTexture(tex);
    return nullptr;
  }
  //we only need to set the alpha channel to 0, but this is the eaisest way.
  memset(pixels, '\0', h*stride);

  auto codepoints = util::utf8_string_iter::utf8_iter_range(text);
  int prev_index = -1;
  int penx = 0;
  cached_glyph *&glyph = this->current;
  FT_Vector kerning_delta;
  for(auto cp : codepoints){
    int err = this->get_glyph(cp);
    if(err){
      fprintf(stderr, "Error %d in get_glyph.\n", err);
      goto error;
    }
    if(prev_index > 0){
      FT_Get_Kerning(this->face, prev_index, glyph->index,
                     ft_kerning_default, &kerning_delta);
      penx += ft_round_26dot6(kerning_delta.x);
    }
    for(unsigned int row = 0; row < glyph->bitmap.rows; row++){
      int _offset = row + glyph->yoffset;
      if(_offset < 0 || _offset > h){
        fprintf(stderr, "Offset out of range: offest = %d, h = %d.\n",
                _offset, h);
        continue;
      }
      size_t offset = ((row + glyph->yoffset) * (stride/4)) + penx + glyph->minx;
      uint32_t *dst = ((uint32_t*)pixels) + offset;
      uint8_t *src = glyph->bitmap.buffer + (row * glyph->bitmap.pitch);
      for(unsigned int col = 0; col < glyph->bitmap.width; col++){
        color.a = *src++;
        *dst++ = color.packed;
      }
    }
    penx += glyph->advance;
    prev_index = glyph->index;
  }
  SDL_UnlockTexture(tex);
  return tex;
 error:
  SDL_UnlockTexture(tex);
  SDL_DestroyTexture(tex);
  return nullptr;
}
static inline int center_line(int line_width, int max_width){
  return (max_width - line_width)/2;
}
static inline int right_justify(int line_width, int max_width){
  return (max_width - line_width);
}
static inline SDL_Texture*
size_text_and_init_texture_multiline(ft_face_wrapper *font,
                                     std::string_view text,
                                     int &w, int &h,
                                     int *line_widths,
                                     SDL_Renderer *renderer){
  vndb_log->log_debug("Rendering text: %.*s",
                      (int)text.size(), text.data());
  int err = font->size_text_multiline(text, &w, &h, line_widths);
  if(err){
    return nullptr;
  }
  vndb_log->log_debug("Size of text is %d x %d", w, h);
  //We use STREAMING rather than STATIC since we need to access the pixels,
  //directly, not because we expect the texture to change.
  SDL_Texture* tex = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB32,
                                       SDL_TEXTUREACCESS_STREAMING, w, h);
  if(!tex){
    fprintf(stderr, "Error creating texture.\n");
    return nullptr;
  }
  SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);
  return tex;
}
SDL_Texture*
ft_face_wrapper::render_utf8_text_argb_multiline(std::string_view text,
                                                 int rgb,
                                                 SDL_Renderer *renderer){

  int w, h;
  int line_widths[this->max_lines_rendered];
  SDL_Texture *tex = 
    size_text_and_init_texture_multiline(this, text, w, h, 
                                         line_widths, renderer);
  if(!tex){ return nullptr; }
  argb_color color(rgb, true);
  vndb_log->log_debug(
    "Text color is: a = %02X, r = %02X, g = %02X, b = %02X.\n"
    "packed = %08X\n", color.a, color.r, color.g, color.b, color.packed);
  void *pixels;
  int stride;
  int err = SDL_LockTexture(tex, nullptr, &pixels, &stride);
  if(err){
    fprintf(stderr, "Error locking texture.\n");
    SDL_DestroyTexture(tex);
    return nullptr;
  }
  //we only need to set the alpha channel to 0, but this is the eaisest way.
  memset(pixels, '\0', h*stride);

  //auto codepoints = util::utf8_string_iter::utf8_iter_range(text);
  const uint8_t *text_ptr = (const uint8_t*)text.data();
  size_t text_size = text.size();
  size_t text_index = 0;
  int prev_index = -1;
  int penx = 0, peny = 0;
  int line_index = 0;
  cached_glyph *&glyph = this->current;
  FT_Vector kerning_delta;
  //for(auto cp : codepoints){
  while(text_index < text_size){
    int32_t cp = util::utf8_next_char(text_ptr, &text_index);
    if(cp == '\r'){
      prev_index = -1;
      --line_index;
      continue;
    }
    if(cp == '\n'){
      if(prev_index < 0){ ++line_index; } //compensate for extra '\r'
      prev_index = -1;
      peny += this->height;
      continue;
    }
    int err = this->get_glyph(cp);
    if(err){
      fprintf(stderr, "Error %d in get_glyph.\n", err);
      goto error;
    }
    if(prev_index > 0){
      FT_Get_Kerning(this->face, prev_index, glyph->index,
                     ft_kerning_default, &kerning_delta);
      penx += ft_round_26dot6(kerning_delta.x);
    } else if(prev_index < 0){
      penx = center_line(line_widths[line_index++], w);
    }
    for(unsigned int row = 0; row < glyph->bitmap.rows; row++){     
      size_t yoffset = ((row + glyph->yoffset + peny) * (stride/4));
      uint32_t *dst = ((uint32_t*)pixels) + yoffset + penx + glyph->minx;
      uint8_t *src = glyph->bitmap.buffer + (row * glyph->bitmap.pitch);
      for(unsigned int col = 0; col < glyph->bitmap.width; col++){
        color.a = *src++;
        *dst++ = color.packed;
      }
    }
    penx += glyph->advance;
    prev_index = glyph->index;
  }
  SDL_UnlockTexture(tex);
  return tex;
 error:
  SDL_UnlockTexture(tex);
  SDL_DestroyTexture(tex);
  return nullptr;
}
//Create a table of colors where color_table[i] is created
//as if by alpha blending fg with a = i over bg with a = 0xff.
//We use this as a color pallette with the greyscale values of
//the bitmaps as indexes, since textures don't support palletized color.
static inline void build_blend_table(int fg_rgb, int bg_rgb,
                                     int *table){
  argb_color fg(fg_rgb, true);
  if(!fg.a){ fg.a = 0xff; }
  argb_color bg(bg_rgb, true);
  if(!bg.a){ bg.a = 0xff; }
  for(int i = 0; i < 256; i++){
    argb_color tmp;
    tmp.a = bg.a + (((fg.a - bg.a) * i) / 255);
    tmp.r = bg.r + (((fg.r - bg.r) * i) / 255);
    tmp.g = bg.g + (((fg.g - bg.g) * i) / 255);
    tmp.b = bg.b + (((fg.b - bg.b) * i) / 255);
    table[i] = tmp.packed;
  }
  return;
}
SDL_Texture* ft_face_wrapper::render_utf8_text_rgb(std::string_view text,
                                    int fg, int bg, SDL_Renderer *renderer,
                                    int *w_ptr, int *h_ptr){
  int w, h;
  SDL_Texture *tex = size_text_and_init_texture(this, text, w, h, renderer);
  if(!tex){ return nullptr; }

  int color_table[256];
  build_blend_table(fg, bg, color_table);

  void *pixels;
  int stride;
  int err = SDL_LockTexture(tex, nullptr, &pixels, &stride);
  if(err){
    SDL_DestroyTexture(tex);
    return nullptr;
  }
  //Fill texture with background color.
  for(int i = 0; i < h*(stride/4); i++){
    ((uint32_t*)pixels)[i] = color_table[0];
  }
  auto codepoints = util::utf8_string_iter::utf8_iter_range(text);
  int prev_index = -1;
  int penx = 0;
  cached_glyph *&glyph = this->current;
  FT_Vector kerning_delta;
  for(auto cp : codepoints){
    int err = this->get_glyph(cp);
    if(err){
      fprintf(stderr, "Error %d in get_glyph.\n", err);
      goto error;
    }
    if(prev_index > 0){
      FT_Get_Kerning(this->face, prev_index, glyph->index,
                     ft_kerning_default, &kerning_delta);
      penx += ft_round_26dot6(kerning_delta.x);
    }
    for(unsigned int row = 0; row < glyph->bitmap.rows; row++){
      int _offset = row + glyph->yoffset; //Hopefully to be removed
      if(_offset < 0 || _offset > h){
        fprintf(stderr, "Offset out of range: offest = %d, h = %d.\n",
                _offset, h);
        continue;
      }
      size_t offset = ((row + glyph->yoffset) * (stride/4)) + penx + glyph->minx;
      uint32_t *dst = ((uint32_t*)pixels) + offset;
      uint8_t *src = glyph->bitmap.buffer + row * glyph->bitmap.pitch;
      for(unsigned int col = 0; col < glyph->bitmap.width; col++){
        *dst++ = color_table[*src++];
      }
    }
    penx += glyph->advance;
    prev_index = glyph->index;
  }
  if(w_ptr){
    *w_ptr = penx - glyph->advance;
  }
  if(h_ptr){
    *h_ptr = h;
  }
  SDL_UnlockTexture(tex);
  return tex;
 error:
  SDL_UnlockTexture(tex);
  SDL_DestroyTexture(tex);
  return nullptr;
}

//It is assumed that pixels is an array of (*w)x(*h) 4 byte argb pixels.
int ft_face_wrapper::render_utf8_text_alpha(std::string_view text,
                                            void *pixels,
                                            int *w_ptr, int *h_ptr,
                                            int num_components,
                                            int alpha_offset){
  int w = *w_ptr, h = *h_ptr;
  auto codepoints = util::utf8_string_iter::utf8_iter_range(text);
  int prev_index = -1;
  int penx = 0;
  cached_glyph *&glyph = this->current;
  for(auto cp : codepoints){
    int err = this->get_glyph(cp);
    if(err){
      return err;
    }
    if(prev_index > 0){
      FT_Vector delta;
      FT_Get_Kerning(this->face, prev_index, glyph->index,
                     ft_kerning_default, &delta);
      penx += ft_round_26dot6(delta.x);
    }
    for(unsigned int row = 0; row < glyph->bitmap.rows; row++){
      int offset = row + glyph->yoffset;
      if(offset < 0){
        continue;
      }
      //Copy the greyscale value to the alpha component of the output.
      uint8_t *dst = (uint8_t*)pixels +
        (num_components * ((offset * w) + penx + glyph->minx)) + alpha_offset;
      uint8_t *src = glyph->bitmap.buffer + row * glyph->bitmap.pitch;
      for(unsigned int col = 0; col < glyph->bitmap.width; col++){
        *dst = *src++;
        dst += num_components;
      }
    }
    penx += glyph->advance;
    prev_index = glyph->index;
  }
  return 0;
}
