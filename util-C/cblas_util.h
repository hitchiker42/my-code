/*
  This file attempts to provide structs and functions that make using the
  cblas library easier.

  Limitations, complex types not supported. Not all the cblas functions are
  supported. Only row major matrices are supported.

  For now only generic NxM matrices are supported, I may add support for
  triangular, hermetian, etc... matrices later.

  I would like to make all these pure functions, but that's simply not practical
*/
#ifndef _CBLAS_UTIL_H_
#define _CBLAS_UTIL_H_
#include <cblas.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#define SINLINE static inline
#ifndef ZMALLOC
#define UNDEF_ZMALLOC
#define ZMALLOC(sz) calloc(sz,1)
#endif
typedef struct cblas_svector cblas_svector;
typedef cblas_svector cblas_svec;
typedef struct cblas_dvector cblas_dvector;
typedef cblas_svector cblas_dvec;
typedef struct cblas_smatrix cblas_smatrix;
typedef cblas_smatrix cblas_smat;
typedef struct cblas_dmatrix cblas_dmatrix;
typedef cblas_dmatrix cblas_dmat;
enum cblas_type {cblas_float, cblas_double, cblas_complex};
struct cblas_svector {
  float *data;
  int n;
  int inc;//aka stride
};
struct cblas_dvector {
  double *data;
  int n;
  int inc;
};
struct cblas_smatrix {
  float *data;
  int m;
  int n;
  int inc;
};
struct cblas_dmatrix {
  double *data;
  int m;
  int n;
  int inc;
};
/*
  A note on paramater names:
    -vectors use lowercase x,y,z
    -matrices use uppercase A,B,C
    -scalars use lowercase a;b;c, they should use α;β;γ, but
       you can't use unicode characters in C variables, and actually
       writing out alpha and beta is too annoying.
*/
/*
  Blas level 1 functions (vector, vector operations)
*/
/*
  Dot product: xᵀ · y
*/
SINLINE float sdot(const cblas_svec x, const cblas_svec y){
  assert(x.n == y.n);
  return cblas_sdot(x.n, x.data, x.inc, y.data, y.inc);
}

SINLINE double ddot(const cblas_dvec x, const cblas_dvec y){
  assert(x.n == y.n);
  return cblas_ddot(x.n, x.data, x.inc, y.data, y.inc);
}
SINLINE double sddot(const cblas_svec x, const cblas_svec y){
  assert(x.n == y.n);
  return cblas_sddot(x.n, x.data, x.inc, y.data, y.inc);
}
/*
  Scalar multiply: x = ax
*/
SINLINE cblas_svec ssmul(const float a, cblas_svec x){
  cblas_sscal(x.n, a, x.data, x.inc);
  return x;
}
SINLINE cblas_dvec dsmul(const double a, cblas_dvec x){
  cblas_sscal(x.n, a, x.data, x.inc);
  return x;
}
/*
  Scalar multiply and addition: y = axᵀ + y
*/
SINLINE cblas_svec saxpy(const float a, const cblas_svec x, cblas_svec y){
  assert(x.n == y.n);
  saxpy(x.n, a, x.data, x.inc, y.data, y.inc);
  return y;
}

SINLINE cblas_dvec daxpy(const double a, const cblas_dvec x, cblas_dvec y){
  assert(x.n == y.n);
  daxpy(x.n, a, x.data, x.inc, y.data, y.inc);
  return y;
}
/*
  Compute the Euclidian norm: ||x||₂ =
    (sqrt (reduce + (map (lambda (x) (pow x 2)) x)))
*/
SINLINE float snrm2(const cblast_svec x){
  return cblas_snrm2(x.n, x.data, x.inc);
}
SINLINE double dnrm2(const cblast_dvec x){
  return cblas_dnrm2(x.n, x.data, x.inc);
}
/*
  Compute the L1 norm: ||x||₁ = (reduce + (map abs x))
    equivlent to the blas function asum
*/
SINLINE float snrm1(const cblas_svec x){
  return cblas_sasum(x.n, x.data, x.inc);
}
SINLINE double dnrm1(const cblas_dvec x){
  return cblas_dasum(x.n, x.data, x.inc);
}
/*
  plane rotation: rotate the points (xᵢ,yᵢ),i=0,...,n-1 according to
    the rotation matrix [[c, -s],[s, c]]
  unfortunately these can't return a value, since C doesn't have (convient)
  ways to return multiple values
*/
SINLINE void srot(cblas_svec x, cblas_svec y, const float c, const float s){
  assert(x.n == y.n);
  cblas_srot(x.n, x.data, x.inc, y.data, y.inc, c, s);
  return;
}
SINLINE void drot(cblas_dvec x, cblas_dvec y, const double c, const double s){
  assert(x.n == y.n);
  cblas_drot(x.n, x.data, x.inc, y.data, y.inc, c, s);
  return;
}
/*
  Same as the above rotation functions, except c = cos(t) and s = sin(t).
  The gnu c function sincos is used to optimize the compution of these values.
    
*/
SINLINE void srot_by(cblas_svec x, cblas_svec y, const float t){
  assert(x.n == y.n);
  float c,s;
  sincosf(t, &s, &c);
  cblas_srot(x.n, x.data, x.inc, y.data, y.inc, c, s);
  return;
}
SINLINE void drot_by(cblas_dvec x, cblas_dvec y, const double t){
  assert(x.n == y.n);
  sincos(t, &s, &c);
  cblas_drot(x.n, x.data, x.inc, y.data, y.inc, c, s);
  return;
}
/*
  Blas level 2 functions (vector, matrix operations)
*/
/*
  General matrix vector multiplication: y = αAx + βy

*/
/*
  I'm pretty sure teh lda argument is effectively the scale parameter for
     matrices, but I'm not sure
*/
SINLINE cblas_svec* smvmul(const cblas_smat A, const float a,
                           const cblas_svec x, const float b, cblas_svec y){
  assert(A.m == x.n &&  A.n == y.n);

  cblas_sgemv(CblasRowMajor, CblasNoTrans, A.m, A.n, a, A.data, A.inc,
              x.data, x.inc, b, y.data, y.inc);
  return y;
}
SINLINE cblas_dvec* dmvmul(const cblas_smat A, const double a,
                           const cblas_svec x, const double b, cblas_svec y){
  assert(A.m == x.n &&  A.n == y.n);
  cblas_dgemv(CblasRowMajor, CblasNoTrans, A.m, A.n, a, A.data, A.inc,
              x.data, x.inc, b, y.data, y.inc);
  return y;
}
/*
  General transposed matrix vector multiplication: y = αAᵀx + βy

*/
SINLINE cblas_svec* smvmul_t(const cblas_smat A, const float a,
                             const cblas_svec x, const float b, cblas_svec y){
  assert(A.m == x.n &&  A.n == y.n);

  cblas_sgemv(CblasRowMajor, CblasTrans, A.m, A.n, a, A.data, A.inc,
              x.data, x.inc, b, y.data, y.inc);
  return y;
}
SINLINE cblas_svec* dmvmul_t(const cblas_smat A, const double a,
                             const cblas_svec x, const double b, cblas_svec y){
  assert(A.m == x.n &&  A.n == y.n);

  cblas_sgemv(CblasRowMajor, CblasTrans, A.m, A.n, a, A.data, A.inc,
              x.data, x.inc, b, y.data, y.inc);
  return y;
}

/*
  Cross product, scale and add: A = axyᵀ + A
*/
#define scross_add(A,a,x,y) sger(A,a,x,y)
SINLINE cblas_smat* sger(const float a, const cblas_svec x,
                         const cblas_svec y, cblas_smat A){
  assert(A.n == x.n && A.m == y.n);
  cblas_sger(CblasRowMajor, A.m, A.n, a, x.data, x.inc,
             y.data, y.inc, A.data, A.inc);
  return A;
}

#define dcross_add(A,a,x,y) dger(A,a,x,y)
SINLINE cblas_dmat* dger(const double a, const cblas_dvec x,
                         const cblas_dvec y, cblas_dmat A){
  assert(A.m == x.n && A.n == y.n);
  cblas_dger(CblasRowMajor, A.m, A.n, a, x.data, x.inc,
             y.data, y.inc, A.data, A.inc);
  return A;
}
/*
  Level 3 blas functions
*/
/*
  good old matrix multiplication: C = aA * B + bC
*/
SINLINE cblas_smat sgemm(const float a, const cblas_smat A, const cblas_smat B,
                         const float b, cblas_smat C){
  assert(A.m == C.m && A.n == B.m && B.n == C.n);
  cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, C.m, C.n, A.n,
              a, A.data, A.inc, B.data, B.inc, b, C.data, C.inc);
  return C;
}
SINLINE cblas_dmat dgemm(const double a, const cblas_dmat A, const cblas_dmat B,
                         const double b, cblas_dmat C){
  assert(A.m == C.m && A.n == B.m && B.n == C.n);
  cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, C.m, C.n, A.n,
              a, A.data, A.inc, B.data, B.inc, b, C.data, C.inc);
  return C;
}
/*
  Matrix multiplication w/transposition. The parameter t is a 2-bit bifield
  specifying what to transpose, bit 1 coorsponds to A and bit 2 to B
*/
enum gemm_trans {
  gemm_trans_none = 0x0,
  gemm_trans_A = 0x1,
  gemm_trans_B = 0x2,
  gemm_trans_AB = 0x3
};
SINLINE cblas_smat sgemm_t(const float a, const cblas_smat A, const cblas_smat B,
                           const float b, cblas_smat C, int trans){
  assert(A.m == C.m && A.n == B.m && B.n == C.n);
  enum CBLAS_TRANSPOSE trans_a = (trans & 0x1 ? CblasTrans : CblasNoTrans);
  enum CBLAS_TRANSPOSE trans_b = (trans & 0x2 ? CblasTrans : CblasNoTrans);
  cblas_sgemm(CblasRowMajor, trans_a, trans_b, C.m, C.n, A.n,
              a, A.data, A.inc, B.data, B.inc, b, C.data, C.inc);
  return C;
}
SINLINE cblas_dmat dgemm_t(const double a, const cblas_dmat A, const cblas_dmat B,
                           const double b, cblas_dmat C, int trans){
  assert(A.m == C.m && A.n == B.m && B.n == C.n);
  enum CBLAS_TRANSPOSE trans_a = (trans & 0x1 ? CblasTrans : CblasNoTrans);
  enum CBLAS_TRANSPOSE trans_b = (trans & 0x2 ? CblasTrans : CblasNoTrans);
  cblas_sgemm(CblasRowMajor, trans_a, trans_b, C.m, C.n, A.n,
              a, A.data, A.inc, B.data, B.inc, b, C.data, C.inc);
  return C;
}
              
/*
  Additional utility functions
*/
/*
  Functions which create new vectors/matrices.
  If memory allocation fails a vector/matrix with data set to NULL is returned.
  This is kinda clunky, but it'll do for now
*/
SINLINE cblas_svec smvmul_new(const cblas_smat A, const int a,
                              const cblas_svec x){
  assert(A.m == x.m);
  cblas_svec ret = {.n = A.n, .inc = 1};
  ret.data = ZMALLOC(A.n*sizeof(float), 1);
  if(ret.data == NULL){
    return ret;
  }
  cblas_sgemv(CblasRowMajor, CblasNoTrans, A.m, A.n, a, A.data, A.inc,
              x.data, x.inc, 1, ret.data, ret.inc);
  return y;
}
SINLINE cblas_dvec* dmvmul_new(const cblas_dmat A, const int a,
                               const cblas_dvec x){
  assert(A.m == x.m);
  cblas_dvec ret = {.n = A.n, .inc = 1};
  ret.data = ZMALLOC(A.n*sizeof(double), 1);
  if(ret.data == NULL){
    return ret;
  }
  cblas_dgemv(CblasRowMajor, CblasNoTrans, A.m, A.n, a, A.data, A.inc,
              x.data, x.inc, 1, ret.data, ret.inc);
  return y;
}
SINLINE cblas_smat sgemm_new(const float a, const cblas_smat A,
                             const cblas_smat B){
  assert(A.n == B.m);
  cblas_smat C = {.m = A.m, .n = B.n, .inc = 1};
  C.data = ZMALLOC(C.m*C.n*sizeof(float), 1);
  if(C.data == NULL){
    return C;
  }
  cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, C.m, C.n, A.n,
              a, A.data, A.inc, B.data, B.inc, b, C.data, C.inc);
  return C;
}
SINLINE cblas_dmat dgemm_new(const double a, const cblas_dmat A,
                             const cblas_dmat B){
  assert(A.n == B.m);
  cblas_dmat C = {.m = A.m, .n = B.n, .inc = 1};
  C.data = ZMALLOC(C.m*C.n*sizeof(double), 1);
  if(C.data == NULL){
    return C;
  }
  cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, C.m, C.n, A.n,
              a, A.data, A.inc, B.data, B.inc, b, C.data, C.inc);
  return C;
}
/*
  TODO: cross product, transposition/operation w/transposition.
*/
#ifdef UNDEF_ZMALLOC
#undef ZMALLOC
#endif
#endif
