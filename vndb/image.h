#ifndef __IMAGE_H__
#define __IMAGE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <jpeglib.h>
#include <setjmp.h>
#include <stdint.h>
struct decompressed_image {
  uint8_t *img;
  uint32_t width;
  uint32_t height;
  int num_componentns;
  J_COLOR_SPACE color_space;
};
int decompress_jpeg(uint8_t *src, size_t src_sz,
                    struct decompressed_image *dst);
#ifdef __cplusplus
}
#endif
#endif /* __IMAGE_H__ */
