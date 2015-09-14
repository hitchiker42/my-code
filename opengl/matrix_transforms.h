#ifndef __MATRIX_TRANSFORMS_H__
#define __MATRIX_TRANSFORMS_H__
#include "cblas_util.h"
#include "matrix_transforms.h"
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
  All transformation matrices are 4x4 matrices unless otherwise stated
*/
/*
  Functions which create new transformation matrices
*/
//Create a rotation matrix that rotates a given point theta radians
//around the axis specified by the unit vector [l,m,n]
float *generalized_rotation_f(float theta, float l, float m, float n);
double *generalized_rotation_d(double theta, double l, double m, double n);

/*
  Create a rotation matrix to rotate a point theta radians around the
  x, y, or z axis. These are special cases of the generalized_rotation
  function, but are much faster since the axis is fixed at compile time.
*/
float *x_rotation_f(float theta);
float *y_rotation_f(float theta);
float *z_rotation_f(float theta);

float *add_x_rotation_after_f(float* data, float theta);
float *add_y_rotation_after_f(float* data, float theta);
float *add_z_rotation_after_f(float* data, float theta);

float *add_x_rotation_before_f(float* data, float theta);
float *add_y_rotation_before_f(float* data, float theta);
float *add_z_rotation_before_f(float* data, float theta);

double *x_rotation_d(double theta);
double *y_rotation_d(double theta);
double *z_rotation_d(double theta);

double *add_x_rotation_after_d(double* data, double theta);
double *add_y_rotation_after_d(double* data, double theta);
double *add_z_rotation_after_d(double* data, double theta);

double *add_x_rotation_before_d(double* data, double theta);
double *add_y_rotation_before_d(double* data, double theta);
double *add_z_rotation_before_d(double* data, double theta);

/*
  Create a Matrix which given a point (a,b,c,1) produces (a+x,b+y,c+z,1)
*/
float *make_translation_f(float x, float y, float z);
double *make_translation_d(double x, double y, double z);

/*
  Functions which add a transformation to an existing matrix
*/
/*
  create a translation matrix T using x, y, z, and return T*mat
*/
float *add_translation_before_f(float *mat, float x, float y, float z);
double *add_translation_before_d(double *mat, double x, double y, double z);
/*
  create a translation matrix T using x, y, z, and return mat*T
*/
float *add_translation_after_f(float *mat, float x, float y, float z);
double *add_translation_after_d(double *mat, double x, double y, double z);

#endif /* __MATRIX_TRANSFORMS_H__ */
