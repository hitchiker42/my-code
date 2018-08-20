#include "font.h"
#include "range.h"
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
//index should usually (always for the fonts I'm using) be 0.
int ft_face_wrapper::init(FT_Library lib, const char *filename,
                          int ptsize, int index){
  FT_Error error;
  error = FT_New_Face(lib, filename, index, &this->face);
  if(error){
    return error;
  }
  //Assume default unicode charmap exists, need to call this if not.
  //error = FT_Set_Charmap(this->face, ...)
  error = FT_Set_Char_Size(
    face,
    0, ptsize*64, /* width/height in 1/64th of points */
    0, 0 /* horizontal/vertical device resolution dpi/ppi (default 72)*/
  );
  if(error){
    return error;
  }
  //Get scaled font metrics
  FT_Fixed scale = this->size->metrics.y_scale;
  //Convert metrics to 26.6 font units, then round up to an integer.
  this->ascent = ft_ceil_26dot6(FT_MulFix(face->ascender, scale));
  this->descent = ft_ceil_26dot6(FT_MulFix(face->descender, scale));
  this->lineskip = ft_ceil_26dot6(FT_MulFix(face->height, scale));
  this->height = (this->ascent - this->descent) + 1;/*1 is the baseline*/

}
void ft_face_wrapper::flush_cached_glyph(cached_glyph *cglyph){
  free(cglyph->bitmap.buffer);//buffer may already be null
  cglyph->bitmap.bufer = nullptr;
  cglyph->index = 0;
  cglyph->codepoint = 0;
}
FT_Error ft_face_wrapper::Load_Glyph(FT_Face face,
                                     uint32_t codepoint,
                                     cached_glyph* cglyph){
  cglyph->index = FT_Get_Char_Index(face, codepoint);
  FT_Error error = FT_Load_Glyph(face, cglyph->index, FT_LOAD_DEFAULT);
  if(error){
    return error;
  }
  FT_GlyphSlot glyph = face->glyph;
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
  error = FT_Render_Glyph(glyph, FT_RENDER_MODE_DEFAULT);
  if(error){
    return error;
  }
  //I'm not sure if this is technically allowed by the freetype api,
  //but it's what SDL_TTF does. (Freetype provides FT_Bitmap_Init and
  //FT_Bitmap_Copy which presumably you should use, but this is eaiser).
  FT_Bitmap* src = &glyph->bitmap;
  FT_Bitmap* dst = &cglyph->bitmap;
  memcpy(src, dst, sizeof(*dst));
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
  auto codepoints = util::utf8_iterator::utf8_iter_range(text);
  int minx = 0, maxx = 0, miny = 0, maxy = 0;
  int prev_index = -1;//index of last codepoint, for kerning.
  int penx = 0;
  cached_glyph *&glyph = this->current;
  //iterate over the codepoints
  for(auto cp : codepoints){
    int err = this->get_glyph(cp);
    if(err){ return err; }
    if(prev_index > 0){
      FT_Vector delta;
      FT_Get_Kerning(this->face, prev_index, glyph->index,
                     ft_kerning_default, &delta);
      penx += ft_round_26dot6(delta.x);
    }
    //Can this ever be negitive?
    minx = std::min(minx, penx + glyph->minx);
    //shouldn't glyph->advance always be > than glyph->maxx?
    maxx = std::max(maxx, penx + std::max(glyph->maxx, glyph->advance));
    penx += glyph->advance;
    miny = std::min(miny, glyph->miny);
    maxy = std::max(maxy, glyph->maxy);
    prev_index = glyph->index;
  }
  *w = (maxx - minx);
  //SDL_TTF pretty much always sets this to 'this->height' so I'm not
  //sure if I should too.
  *h = (maxy - miny);
  return 0;  
}
union rgba_union {
  uint32_t packed;
  uint8_t bytes[4];
  struct {
    uint8_t a;
    uint8_t r;
    uint8_t g;
    uint8_t b;
  };
};
SDL_Texture* ft_face_wrapper::render_utf8_text_rgba(std::string_view text,
                                                    int rgb, 
                                                    SDL_Renderer *renderer){
  int w, h;
  int err = this->size_text(text, &w, &h);
  if(err){
    return nullptr;
  }
  //We use STREAMING rather than STATIC since we need to access the pixels,
  //directly, the texture shouldn't change after we return it.
  SDL_Texture* tex = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888,
                                       SDL_TEXTUREACCESS_STREAMING, w, h);
  if(!tex){
    return nullptr;
  }  
  rgba_union color;
  color.r = rgb & 0x00FF0000;
  color.g = rgb & 0x0000FF00;
  color.b = rgb & 0x000000FF;

  void *pixels;
  int stride;
  err = SDL_LockTexture(tex, nullptr, &pixels, &stride);
  if(err){
    SDL_DestroyTexture(tex);
    return nullptr;
  }
  //we only need to set the alpha channel to 0, but this is the eaisest way.
  memset(pixels, '\0', h*stride);

  auto codepoints = util::utf8_iterator::utf8_iter_range(text);
  int last_index = -1;
  int penx = 0;
  cached_glyph *&glyph = this->current;
  for(auto cp : codepoints){
    int err = this->get_glyph(cp);
    if(err){ 
      SDL_DestroyTexture(tex);
      return nullptr;
    }
    if(prev_index > 0){
      FT_Vector delta;
      FT_Get_Kerning(this->face, prev_index, glyph->index,
                     ft_kerning_default, &delta);
      penx += ft_round_26dot6(delta.x);
    }    
    for(int row = 0; row < glyph->bitmap.rows; row++){
      int offset = row + glyph->yoffset;
      //skip empty
      if(offset < 0 || offset > h){
        continue;
      }
      //copy each pixel as a single unit, hopefully this won't
      //come back to bite me in the ass due to enidaness issues.
      uint32_t *dst = 
        (uint32_t*)pixels + (offset * (stride/4)) + penx + glyph->minx;
      uint8_t *src = glyph->bitmap.buffer + row * glyph->bitmap.pitch;
      for(int col = 0; col < glyph->bitmap.width; col++){
        color.a = *src++;
        *dst++ = color.packed;
      }
    }
    penx += glyph->advance;
    prev_index = glyph->index;
  }
      
