#include "font.h"
//inline floor/ceil for 26.6 fixed point integers
static inline int ft_floor_26dot6(long x){
  return (x & ~0x3f) >> 6;
}
static inline int ft_ceil_26dot6(long x){
  return ((x + 0x3f) & ~0x3f) >> 6;
}
//index should usually (always for the fonts I'm using) be 0.
ft_face_wrapper* ft_face_wrapper_init(FT_Library lib, const char *filename,
                                      int ptsize, int index){
  ft_face_wrapper *ret = (ft_face_wrapper*)calloc(sizeof(ft_face_wrapper,1));
  if(!ret){
    return nullptr;
  }
  FT_Error error;
  error = FT_New_Face(lib, filename, index, &ret->face);
  if(error){
    free(ret);
    return nullptr;
  }
  //Assume default unicode charmap exists, need to call this if not.
  //error = FT_Set_Charmap(ret->face, ...)
  error = FT_Set_Char_Size(
    face,      
    0, ptsize*64, /* width/height in 1/64th of points */      
    0,0 /* horizontal/vertical device resolution dpi/ppi (default 72)*/
  );   
  //Get scaled font metrics
  FT_Fixed scale = ret->size->metrics.y_scale;
  //Convert metrics to 26.6 font units, then round up to an integer.
  ret->ascent = ft_ceil_26dot6(FT_MulFix(face->ascender, scale));
  ret->descent = ft_ceil_26dot6(FT_MulFix(face->descender, scale));
  ret->lineskip = ft_ceil_26dot6(FT_MulFix(face->height, scale));
  ret->height = (ret->ascent - ret->descent) + 1;/*1 is the baseline*/
  
}
static void free_cached_glyph(cached_glyph *cglyph){
  free(cglyph->bitmap.buffer);
  cglyph->bitmap.bufer = nullptr;
  cglyph->index = 0;
}
static FT_Error Load_Glyph(FT_Face face, 
                           uint32_t codepoint, cached_glyph* cglyph){  
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
  
