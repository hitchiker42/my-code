#ifndef __MATRIX_TRANSFORMS_H__
#define __MATRIX_TRANSFORMS_H__
#include "cblas_util.h"
#ifndef SINLINE
#define SINLINE static inline
#endif
#ifndef ZMALLOC
#define UNDEF_ZMALLOC
#define ZMALLOC(sz) calloc(sz,1)
#endif
#ifndef XMALLOC
#define UNDEF_XMALLOC
#define XMALLOC(sz) malloc(sz)
#endif
//literal for the 4x4 identity matrix
#define I_4 {1.0,0.0,0.0,0.0,                   \
             0.0,1.0,0.0,0.0,                   \
             0.0,0.0,1.0,0.0,                   \
             0.0,0.0,0.0,1.0}
/*
  Utility routines for creating and manipulating transformation matrices.
  All transformation matrices are 4x4 matrices unless otherwise stated, with indices
  |0 |1 |2 |3 |
  |4 |5 |6 |7 |
  |8 |9 |10|11|
  |12|13|14|15|
  Generalized rotation matrix around unit vector (l,m,n) (taken from wikipedia)
  |ll(1-cos(θ)) +  (cos(θ))|ml(1-cos(θ)) - n(sin(θ))|nl(1-cos(θ)) + m(sin(θ))|
  |lm(1-cos(θ)) + n(sin(θ))|mm(1-cos(θ)) -  (cos(θ))|nm(1-cos(θ)) - l(sin(θ))|
  |ln(1-cos(θ)) + m(sin(θ))|mn(1-cos(θ)) - l(sin(θ))|nn(1-cos(θ)) +  (cos(θ))|
*/
#define sincosd sincos
#define make_rot_fun(type, axis, suffix, a, b, c, d)            \
  SINLINE type *make_rot_##plane##_##suffix(type theta){        \
    type s,c;                                                   \
    sincos##suffix(theta, &s, &c);                              \
    type *rot = ZMALLOC(4*4*sizeof(type));                      \
    rot[a] = rot[b] = c;                                        \
    rot[c] = s; rot[d] = -s;                                    \
    return rot;                                                 \
  }
//clockwise rotation matrices
make_rot_fun(float, x, f, 5, 10, 1, 4);
make_rot_fun(float, y, f, 0, 10, 8, 3);
make_rot_fun(float, z, f, 0, 5,  1, 4);

make_rot_fun(double, x, d, 5, 10, 1, 4);
make_rot_fun(double, y, d, 0, 10, 8, 3);
make_rot_fun(double, z, d, 0, 5,  1, 4);
float *make_rot_xy_f(float theta){
  float sin,cos
double *make_rot_xy_d(double theta);
float *make_rot_yz_f(float theta);
double *make_rot_yz_d(double theta);
float *make_rot_zx_f(float theta);
double *make_rot_zx_d(double theta);

#endif /* __MATRIX_TRANSFORMS_H__ */
