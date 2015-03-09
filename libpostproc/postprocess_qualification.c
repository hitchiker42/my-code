#include <stdint.h>
#include <x86intrin.h>
#define REG_a "rax"
#define REG_c "rcx"
/*
 * Assume this is operating on an 8x16 block, this is just a simple
 * reimplementation of the existing mmx code using sse2
 * This also assumes that src is 16 byte aligned
 */
void deInterlaceInterpolateLinear_SSE2(uint8_t src[], int stride){
  /* 
     I'll need to look at the code more to figure out exactly where this
     is being called, but it might be worth using non-temporal load/stores
     to prevent unecessary caching. Though I'm not sure if we want the data
     to be cached or not.
   */
  __asm__ volatile("leaq (%0, %1), %%"REG_a";\n\t"
                   "leaq (%%"REG_a", %1, 4), %%"REG_c";\n\t"
                   "movdqa (%0), %%xmm0;\n\t"
                   "movdqa (%%"REG_a", %1), %%xmm1;\n\t"
                   "pavgb %%xmm1, %%xmm0;\n\t"
                   "movdqa %%xmm0, (%%"REG_a");\n\t"//1st line
                   "movdqa (%0,%1,4), %%xmm0\n\t;"
                   "pavgb %%xmm0, %%xmm1;\n\t"
                   "movdqa %%xmm1, (%%"REG_a", %1, 2);\n\t"//3rd line
                   "movdqa (%%"REG_c", %1, 2), %%xmm1;\n\t"
                   "pavgb %%xmm1, %%xmm0;\n\t"
                   "movdqa %%xmm0, (%%"REG_c");\n\t"//5th line
                   "movdqa (%0, %1, 8), %%xmm0;\n\t"
                   "pavgb %%xmm0, %%xmm1;\n\t"
                   "movdqa %%xmm1, (%%"REG_c",%1, 4);\n\t"//7th line
                   : : "r"(src), "r" ((uint64_t)stride)
                     : "%"REG_a, "%"REG_c);
}
/* or */
void deInterlaceInterpolateLinear_SSE2_intrinsic(uint8_t *src, int stride){
  __m128i prev, next;
  __m128i *dest;
  int i;
  /* 
     It might be worth manually unrolling this loop,
     if the compiler doesn't.
   */  
  for(i=0; i < 4; i++){
    dest = (src+stride);
    prev = _mm_load_si128(src);
    next = _mm_load_si128(src+(2*stride));
    *dest = _mm_avg_epu8(prev,next);
    src += (3*stride);
  }
}
union m256 {
  __m256i m256;
  uint64_t uint64[4];
};
void deInterlaceInterpolateLinear_AVX2(uint8_t *src, int stride){
//this is really inefficent as is
  __m128i vindex = _mm_set_epi32(0,2*stride,4*stride,6*stride);
  __m256i prev, next;
  union m256 avg;
  prev = _mm256_i32gather_epi64((uint64_t*)src, vindex, 1);
  next = _mm256_i32gather_epi64((uint64_t*)(src+(2*stride)), vindex, 1);
  avg.m256 = _mm256_avg_epu8(prev, next);
  uint64_t *dest = (uint64_t)src;
  dest[stride] = avg.uint64[0];
  dest[stride*3] = avg.uint64[1];
  dest[stride*5] = avg.uint64[2];
  dest[stride*7] = avg.uint64[3];
}
  

