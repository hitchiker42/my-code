
#define RENAME_DECLARATION(name) name
int  RENAME_DECLARATION(vertClassify)(const uint8_t src[], int stride, PPContext *c);
void RENAME_DECLARATION(doVertLowPass)(uint8_t *src, int stride, PPContext *c);
int  RENAME_DECLARATION(vertClassify)(const uint8_t src[], int stride, PPContext *c);
void RENAME_DECLARATION(doVertLowPass)(uint8_t *src, int stride, PPContext *c);
void RENAME_DECLARATION(vertX1Filter)(uint8_t *src, int stride, PPContext *co);
void RENAME_DECLARATION(doVertDefFilter)(uint8_t src[], int stride, PPContext *c);
void RENAME_DECLARATION(dering)(uint8_t src[], int stride, PPContext *c);
void RENAME_DECLARATION(deInterlaceInterpolateLinear)(uint8_t src[], int stride);
void RENAME_DECLARATION(deInterlaceInterpolateCubic)(uint8_t src[], int stride);
void RENAME_DECLARATION(deInterlaceFF)(uint8_t src[], int stride, uint8_t *tmp);
void RENAME_DECLARATION(deInterlaceL5)(uint8_t src[], int stride, uint8_t *tmp, uint8_t *tmp2);
void RENAME_DECLARATION(deInterlaceBlendLinear)(uint8_t src[], int stride, uint8_t *tmp);
void RENAME_DECLARATION(deInterlaceMedian)(uint8_t src[], int stride);
void RENAME_DECLARATION(transpose1)(uint8_t *dst1, uint8_t *dst2,
                                    const uint8_t *src, int srcStride);
void RENAME_DECLARATION(transpose2)(uint8_t *dst, int dstStride, const uint8_t *src);
void RENAME_DECLARATION(tempNoiseReducer)(uint8_t *src, int stride,;
void RENAME_DECLARATION(do_a_deblock)(uint8_t *src, int step, int stride, const PPContext *c, int mode){;
void RENAME_DECLARATION(postProcess)(const uint8_t src[], int srcStride, uint8_t dst[], int dstStride,
                                     int width, int height, const QP_STORE_T QPs[],
                                     int QPStride, int isColor, PPContext *c);
void RENAME_DECLARATION(blockCopy)(uint8_t dst[], int dstStride, const uint8_t src[], int srcStride,
                                   int levelFix, int64_t *packedOffsetAndScale);
void RENAME_DECLARATION(duplicate)(uint8_t src[], int stride);
 /*
  Explaination of terms:
  luma (luminance)(Y): Represents the black and white part of an image, think rods in the human eye
  chroma (chrominance)(U,V): Represenets the color portion of an image in terms of color differences,
    there are two chroma channels U and V, U is blue difference (blue - luma) and V is red difference
    (red -luma), think cones in the human eye.
  Because the human eye is much more sensitive to luma than chroma (we have ~90 million rods and
    ~5 million cones) the chroma portion is often compressed more than the luma.

  QP (quantization paramater): Exactly what it sounds like, it defines the resolution of some value,
    by turning any value into some range into a single value (i.e anything from 0.01-0.03 -> 0.02)
    thus quantizing that value.
  There are two types of quantization which are revelent, color quantization and frequency quantization.

  Color quantization is about reducing the number of colors available to use (think 16bit vs 24bit color)

  Frequency quantization is a bit more complicated, the human eye is bad at detecting the differences
  between high frequency signals (i.e the difference between say 10 and 12 is much more noticable than
  the difference between 100 and 102). So to compress images/video it makes sense to quantize high
  frequency signals. This can be done in a somewhat complex way by using a discrete cosine transform
  to calculate the frequencies of a block of pixels and multiplying it by a quantization matrix in order
  to reduce the resolution of heigher frequencies, allowing heigher compression.

  I'm not sure which of these forms of quantization is used, or if both are.

*/

