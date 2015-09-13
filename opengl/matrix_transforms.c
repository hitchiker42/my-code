#include "cblas_util.h"
#ifndef ZMALLOC
#define UNDEF_ZMALLOC
#define ZMALLOC(sz) calloc(sz,1)
#endif
#ifndef XMALLOC
#define UNDEF_XMALLOC
#define XMALLOC(sz) malloc(sz)
#endif
/*
  Utility routines for creating and manipulating transformation matrices.
  All transformation matrices are 4x4 matrices unless otherwise stated.
*/
//literal for the 4x4 identity matrix
#define I_4 {1.0,0.0,0.0,0.0,                   \
             0.0,1.0,0.0,0.0,                   \
             0.0,0.0,1.0,0.0,                   \
             0.0,0.0,0.0,1.0}
#define I_4(x) {x,0.0,0.0,0.0,                  \
                0.0,x,0.0,0.0,                  \
                0.0,0.0,x,0.0,                  \
                0.0,0.0,0.0,x}
//only evaluates mat once
#define set_diagonal(mat, x)                            \
  ({__typeof(mat) _mat = mat;                           \
    _mat[0] = _mat[5] = _mat[10] = _mat[15] = x;        \
    _mat;})
/*
  Utility routines for creating and manipulating transformation matrices.
  All transformation matrices are 4x4 matrices unless otherwise stated, with indices
  |0 |1 |2 |3 |
  |4 |5 |6 |7 |
  |8 |9 |10|11|
  |12|13|14|15|
  Generalized rotation matrix R around unit vector (l,m,n) by θ(taken from wikipedia):
  |ll(1-cos(θ)) +  (cos(θ))|ml(1-cos(θ)) - n(sin(θ))|nl(1-cos(θ)) + m(sin(θ))|
  |lm(1-cos(θ)) + n(sin(θ))|mm(1-cos(θ)) -  (cos(θ))|nm(1-cos(θ)) - l(sin(θ))|
  |ln(1-cos(θ)) + m(sin(θ))|mn(1-cos(θ)) - l(sin(θ))|nn(1-cos(θ)) +  (cos(θ))|
  or R = cos(θ)I + sin(θ)[u]ₓ + (1-cos(θ))u * u
  where * is the tensor product, and [u]ₓ is the cross product matrix of u
*/
float *generalized_rotation_f(float theta, float l, float m, float n){
  float s,c;
  sincosf(theta, &s, &c);
  float unit_vec[4] = {l,m,n,1};
  cblas_svec unit = {.data = unit_vec,.inc = 1,.n = 4};
  //cross product matrix
  float scratch_data[16] = {[1] = -n*s, [2] = m*s, [4] = n*s,
                            [6] = -l*s, [8] = -m*s, [9] = l*s, [15] = 1};
  cblas_smat scratch = make_smat_square(scratch_data, 4);
  //cos * identity matrix
  float *rot_data = ZMALLOC(4*4*sizeof(float));
  rot_data[0] = rot_dat[5] = rot_data[10] = c;
  rot[15] = 1.0f;//this should be 1 right?
  cblas_smat rot = make_smat_square(rot_data, 4);

  rot = smat_addmul_square(1,scratch,1,rot);//cos(θ)I + sin(θ)[u]ₓ
  rot = sger(1-c, unit, unit, rot);

  return rot.data;//could eaisly return the cblas_smat instead
}

double *generalized_rotation_d(double theta, double l, double m, double n){
  double s,c;
  sincos(theta, &s, &c);
  double unit_vec[4] = {l,m,n,1};
  cblas_dvec unit = {.data = unit_vec,.inc = 1,.n = 4};
  //cross product matrix
  double scratch_data[16] = {[1] = -n*s, [2] = m*s, [4] = n*s,
                            [6] = -l*s, [8] = -m*s, [9] = l*s, [15] = 1};
  cblas_dmat scratch = make_dmat_square(scratch_data, 4);
  //cos * identity matrix
  double *rot_data = ZMALLOC(4*4*sizeof(double));
  rot_data[0] = rot_dat[5] = rot_data[10] = c;
  rot[15] = 1.0f;//this should be 1 right? or should it be c?
  cblas_dmat rot = make_dmat_square(rot_data, 4);

  rot = dmat_addmul_square(1,scratch,1,rot);//cos(θ)I + sin(θ)[u]ₓ
  rot = dger(1-c, unit, unit, rot);

  return rot.data;//could eaisly return the cblas_dmat instead
}

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
#define make_rot_funs(axis, a, b, c, d)         \
  make_rot_fun(float, axis, f, a, b, c, d);     \
  make_rot_fun(double, axis, d, a, b, c, d);
//clockwise rotation matrices
make_rot_funs(x, 5, 10, 6, 9);
make_rot_funs(y, 0, 10, 8, 2);
make_rot_funs(z, 0, 5,  1, 4);

static float *translation_matrix_internal_f(float *trans, 
                                            float x, float y, float z){
  set_diagonal(trans, 1.0);
  trans[3] = x;
  trans[7] = y;
  trans[11] = z;
  return trans;
}
static double *translation_matrix_internal_d(double *trans, 
                                             double x, double y, double z){
  set_diagonal(trans, 1.0);
  trans[3] = x;
  trans[7] = y;
  trans[11] = z;
  return trans;
}
float *make_translation_f(float x, float y, float z){
  float *trans = ZMALLOC(4*4*sizeof(float));
  return translation_matrix_internal_f(trans, x, y, z);
}
double *make_translation_d(double x, double y, double z){
  double *trans = ZMALLOC(4*4*sizeof(double));
  return translation_matrix_internal_d(trans, x, y, z);
}

float *add_translation_before_f(float *mat, float x, float y, float z){
  float *scratch = alloca(4*4*sizeof(float));
  memset(scratch,'\0', 4*4*sizeof(float));
  cblas_smat transform = make_smat_square(mat, 4);
  cblas_smat translate = make_smat_square(scratch, 4);
  transform = sgemm(1, translate, transform, 0 transform);
  return transform.data;
}
float *add_translation_after_f(float *mat, float x, float y, float z){
  float *scratch = alloca(4*4*sizeof(float));
  memset(scratch,'\0', 4*4*sizeof(float));
  cblas_smat transform = make_smat_square(mat, 4);
  cblas_smat translate = make_smat_square(scratch, 4);
  transform = sgemm(1, transform, translate, 0 transform);
  return transform.data;
}

double *add_translation_before_d(double *mat, double x, double y, double z){
  double *scratch = alloca(4*4*sizeof(double));
  memset(scratch,'\0', 4*4*sizeof(double));
  cblas_dmat transform = make_dmat_square(mat, 4);
  cblas_dmat translate = make_dmat_square(scratch, 4);
  transform = dgemm(1, translate, transform, 0 transform);
  return transform.data;
}
double *add_translation_after_d(double *mat, double x, double y, double z){
  double *scratch = alloca(4*4*sizeof(double));
  memset(scratch,'\0', 4*4*sizeof(double));
  cblas_dmat transform = make_dmat_square(mat, 4);
  cblas_dmat translate = make_dmat_square(scratch, 4);
  transform = dgemm(1, transform, translate, 0 transform);
  return transform.data;
}
#endif /* __MATRIX_TRANSFORMS_H__ */
