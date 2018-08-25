#include "image.h"
struct my_error_mgr {
  struct jpeg_error_mgr err_mgr;
  jmp_buf dest;
  FILE *outfile;
};
static void my_error_exit(j_common_ptr cinfo){
  struct my_error_mgr *err = (struct my_error_mgr*)cinfo->err;
  longjmp(err->dest, 1);
}
static void my_output_message(j_common_ptr cinfo){
  struct my_error_mgr *err = (struct my_error_mgr*)cinfo->err;
  if(err->outfile == NULL){
    return;
  }
  char buf[JMSG_LENGTH_MAX];
  err->err_mgr.format_message(cinfo, buf);
  fputs(buf, err->outfile);
}
//Just in case the version of libjpeg used doesn't provide jpeg_mem_src
//I've copied the implementation from the libjpeg-turbo.
#if !(JPEG_LIB_VERSION >= 80 || defined(MEM_SRCDST_SUPPORTED))
void init_mem_source(j_decompress_ptr cinfo){
  /* no work necessary here */
}
boolean fill_mem_input_buffer(j_decompress_ptr cinfo){
  static const JOCTET mybuffer[4] = {
    (JOCTET)0xFF, (JOCTET)JPEG_EOI, 0, 0
  };

  /* The whole JPEG data is expected to reside in the supplied memory
   * buffer, so any request for more data beyond the given buffer size
   * is treated as an error.
   */
  WARNMS(cinfo, JWRN_JPEG_EOF);

  /* Insert a fake EOI marker */

  cinfo->src->next_input_byte = mybuffer;
  cinfo->src->bytes_in_buffer = 2;

  return TRUE;
}
void skip_input_data(j_decompress_ptr cinfo, long num_bytes){
  if(num_bytes <= 0){ return; }
  cinfo->src->next_output_byte += num_bytes;
  cinfo->src->free_in_buffer -= num_bytes;
  return;
}
void term_source(j_decompress_ptr cinfo){
  /* no work necessary here */
}
void jpeg_mem_src(j_decompress_ptr cinfo, const unsigned char *inbuffer,
                  unsigned long insize){
  struct jpeg_source_mgr *src;

  if (inbuffer == NULL || insize == 0)  /* Treat empty input as fatal error */
    ERREXIT(cinfo, JERR_INPUT_EMPTY);

  /* The source object is made permanent so that a series of JPEG images
   * can be read from the same buffer by calling jpeg_mem_src only before
   * the first one.
   */
  if (cinfo->src == NULL) {     /* first time for this JPEG object? */
    cinfo->src = (struct jpeg_source_mgr *)
      (*cinfo->mem->alloc_small) ((j_common_ptr)cinfo, JPOOL_PERMANENT,
                                  sizeof(struct jpeg_source_mgr));
  } else if (cinfo->src->init_source != init_mem_source) {
    /* It is unsafe to reuse the existing source manager unless it was created
     * by this function.
     */
    ERREXIT(cinfo, JERR_BUFFER_SIZE);
  }

  src = cinfo->src;
  src->init_source = init_mem_source;
  src->fill_input_buffer = fill_mem_input_buffer;
  src->skip_input_data = skip_input_data;
  src->resync_to_restart = jpeg_resync_to_restart; /* use default method */
  src->term_source = term_source;
  src->bytes_in_buffer = (size_t)insize;
  src->next_input_byte = (const JOCTET *)inbuffer;
}
#endif
#if (defined JCS_ALPHA_EXTENSIONS)
static const int max_color_space = 16;
#elif (defined JCS_EXTENSIONS)
static const int max_color_space = 11;
#else
static const int max_color_space = 5;
#endif
int decompress_jpeg(uint8_t *src, size_t src_sz,
                    struct decompressed_image *dst,
                    J_COLOR_SPACE color_space){
  struct jpeg_decompress_struct cinfo;
  struct my_error_mgr jerr;
  cinfo.err = jpeg_std_error(&jerr.err_mgr);
  jerr.err_mgr.error_exit = my_error_exit;
  jerr.err_mgr.output_message = my_output_message;

  if(setjmp(jerr.dest) != 0){
    int ret = jerr.err_mgr.msg_code;
    jpeg_destroy_decompress(&cinfo);
    free(dst->img);//may be null but that's fine
    dst->img = NULL;
    return ret;
  }

  jpeg_create_decompress(&cinfo);
  jpeg_mem_src(&cinfo, src, src_sz);
  jpeg_read_header(&cinfo, TRUE);
  //  fprintf(stderr,"Read jpeg header: %dx%d, %d components, colorspace %d.\n",
  //               cinfo.image_width, cinfo.image_height, cinfo.num_components,
  //               cinfo.jpeg_color_space);

//  fprintf(stderr,"Set out color space to %d (%d components).\n",
//          cinfo.out_color_space, cinfo.out_color_components);
  cinfo.quantize_colors = FALSE;
  //By default libjpeg will set out_color_space to match the input.
  if(color_space != JCS_UNKNOWN && color_space <= max_color_space){
    cinfo.out_color_space = color_space;
  }
//  cinfo.scale_num = 2;
//  cinfo.scale_denom = 1;
  jpeg_calc_output_dimensions(&cinfo);
  dst->num_components = cinfo.output_components;
  dst->color_space = cinfo.out_color_space;
  dst->width = cinfo.output_width;
  dst->height = cinfo.output_height;
  dst->img = malloc(dst->width * dst->height * dst->num_components);

  jpeg_start_decompress(&cinfo);

  JSAMPROW rowptr[1];
  while(cinfo.output_scanline < cinfo.output_height){
    rowptr[0] = (JSAMPROW)dst->img +
      (cinfo.output_scanline * cinfo.output_width * dst->num_components);
    jpeg_read_scanlines(&cinfo, rowptr, 1);
  }
  jpeg_finish_decompress(&cinfo);
  jpeg_destroy_decompress(&cinfo);

  return 0;
}
//Mostly just copied from the above function, if I add another function
//I'll abstract away the common stuff.
SDL_Texture* decompress_jpeg_to_texture(SDL_Renderer *renderer,
                                        uint8_t *src, size_t src_sz,
                                        int use_alpha){
#ifndef JCS_ALPHA_EXTENSIONS
  if(use_alpha){ return NULL; }
#endif
  struct jpeg_decompress_struct cinfo;
  struct my_error_mgr jerr;
  SDL_Texture *tex = NULL;
  cinfo.err = jpeg_std_error(&jerr.err_mgr);
  jerr.err_mgr.error_exit = my_error_exit;
  jerr.err_mgr.output_message = my_output_message;

  if(setjmp(jerr.dest) != 0){
    //int ret = jerr.err_mgr.msg_code;
    jpeg_destroy_decompress(&cinfo);
    SDL_DestroyTexture(tex);
    return NULL;
  }

  jpeg_create_decompress(&cinfo);
  jpeg_mem_src(&cinfo, src, src_sz);
  jpeg_read_header(&cinfo, TRUE);
  //  fprintf(stderr,"Read jpeg header: %dx%d, %d components, colorspace %d.\n",
  //               cinfo.image_width, cinfo.image_height, cinfo.num_components,
  //               cinfo.jpeg_color_space);

//  fprintf(stderr,"Set out color space to %d (%d components).\n",
//          cinfo.out_color_space, cinfo.out_color_components);
  cinfo.quantize_colors = FALSE;
#ifdef JCS_ALPHA_EXTENSIONS
  cinfo.out_color_space = (use_alpha ? JCS_EXT_ARGB : JCS_EXT_RGB);
#else
  cinfo.out_color_space = JCS_RGB;
#endif
  cinfo.scale_num = 2;
  cinfo.scale_denom = 1;
  jpeg_calc_output_dimensions(&cinfo);
  tex = SDL_CreateTexture(renderer,
                                       (use_alpha ? SDL_PIXELFORMAT_ARGB8888 :
                                        SDL_PIXELFORMAT_RGB888),
                                       SDL_TEXTUREACCESS_STREAMING,
                                       cinfo.output_width, 
                                       cinfo.output_height);
  int err = 0;
  void *pixels;
  int stride;
  if(tex){
    err = SDL_LockTexture(tex, NULL, &pixels, &stride);
  }
  if(!tex || err){
    SDL_DestroyTexture(tex);
    jpeg_destroy_decompress(&cinfo);
    return NULL;
  }
  jpeg_start_decompress(&cinfo);

  JSAMPROW rowptr[1];
  while(cinfo.output_scanline < cinfo.output_height){
    rowptr[0] = (JSAMPROW) pixels +
      (cinfo.output_scanline * cinfo.output_width * cinfo.num_components);
    jpeg_read_scanlines(&cinfo, rowptr, 1);
  }
  jpeg_finish_decompress(&cinfo);
  jpeg_destroy_decompress(&cinfo);
  SDL_UnlockTexture(tex);
  return tex;
}
