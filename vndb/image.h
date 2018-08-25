#ifndef __IMAGE_H__
#define __IMAGE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <stdint.h>
#include <jpeglib.h>
#include <SDL2/SDL.h>

struct decompressed_image {
  uint8_t *img;
  uint32_t width;
  uint32_t height;
  int num_components;
  J_COLOR_SPACE color_space;
};
int decompress_jpeg(uint8_t *src, size_t src_sz,
                    struct decompressed_image *dst,
                    J_COLOR_SPACE color_space);
//Currently only possible color spaces are rgb and argb, with
//use_alpha indicating if the alpha channel is desired.
SDL_Texture* decompress_jpeg_to_texture(SDL_Renderer *renderer,
                                        uint8_t *src, size_t src_sz,
                                        int use_alpha);
#ifdef __cplusplus
}
#endif
#endif /* __IMAGE_H__ */
