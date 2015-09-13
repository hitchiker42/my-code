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
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <cblas.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
/*
  Add -DSINLINE to the command line flags to compile this as
  an object file usabale by c++
*/
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
#define column_major_order CblasColMajor
#define row_major_order CblasRowMajor
typedef struct cblas_svector cblas_svector;
typedef cblas_svector cblas_svec;
typedef struct cblas_dvector cblas_dvector;
typedef cblas_dvector cblas_dvec;
typedef struct cblas_smatrix cblas_smatrix;
typedef cblas_smatrix cblas_smat;
typedef struct cblas_dmatrix cblas_dmatrix;
typedef cblas_dmatrix cblas_dmat;
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
/*
  A matrix struct represents an m by n matrix stored in
  a lda x n block of memory. For our purposes a square matrix is defined
  as on where n == m == lda.
*/
struct cblas_smatrix {
  float *data;
  int order;
  int m;
  int n;
  int lda;
};
struct cblas_dmatrix {
  double *data;
  int order;
  int m;
  int n;
  int lda;
};
/*
  I'd use designated initializers, but I want to be C++ compatable
*/
//make a vector with implicit scale of 1
SINLINE cblas_svec make_svec_simple(float *data, int n){
  cblas_svec ret;
  ret.data = data; ret.n = n; ret.inc = 1;
  return ret;
}
SINLINE cblas_dvec make_dvec_simple(double *data, int n){
  cblas_dvec ret;
  ret.data = data; ret.n = n; ret.inc = 1;
  return ret;
}
//make a vector, specifying all parameters
SINLINE cblas_svec make_svec(float *data, int n, int inc){
  cblas_svec ret;
  ret.data = data; ret.n = n; ret.inc = inc;
  return ret;
}
SINLINE cblas_dvec make_dvec(double *data, int n, int inc){
  cblas_dvec ret;
  ret.data = data; ret.n = n; ret.inc = inc;
  return ret;
}
//make a square row major matrix with implicit lda = n
SINLINE cblas_smat make_smat_square(float *data, int n){
  cblas_smat ret;
  ret.data = data; ret.n = ret.m = ret.lda = n;
  ret.order = CblasRowMajor;
  return ret;
}
SINLINE cblas_dmat make_dmat_square(double *data, int n){
  cblas_dmat ret;
  ret.data = data; ret.n = ret.m = ret.lda = n;
  ret.order = CblasRowMajor;
  return ret;
}
//make a square column major matrix with implicit lda = n
SINLINE cblas_smat make_smat_square_col(float *data, int n){
  cblas_smat ret;
  ret.data = data; ret.n = ret.m = ret.lda = n;
  ret.order = CblasColMajor;
  return ret;
}
SINLINE cblas_dmat make_dmat_square_col(double *data, int n){
  cblas_dmat ret;
  ret.data = data; ret.n = ret.m = ret.lda = n;
  ret.order = CblasColMajor;
  return ret;
}
//make a row major matrix, specifying all parameters
SINLINE cblas_smat make_smat(float *data, int n, int m, int lda){
  cblas_smat ret;
  ret.data = data; ret.n = n; ret.m = m; ret.lda = lda;
  ret.order = CblasRowMajor;
  return ret;
}
SINLINE cblas_dmat make_dmat(double *data, int n, int m, int lda){
  cblas_dmat ret;
  ret.data = data; ret.n = n; ret.m = m; ret.lda = lda;
  ret.order = CblasRowMajor;
  return ret;
}
//make a column major matrix, specifying all parameters
SINLINE cblas_smat make_smat_col(float *data, int n, int m, int lda){
  cblas_smat ret;
  ret.data = data; ret.n = n; ret.m = m; ret.lda = lda;
  ret.order = CblasColMajor;
  return ret;
}
SINLINE cblas_dmat make_dmat_col(double *data, int n, int m, int lda){
  cblas_dmat ret;
  ret.data = data; ret.n = n; ret.m = m; ret.lda = lda;
  ret.order = CblasColMajor;
  return ret;
}
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
SINLINE double dsdot(const cblas_svec x, const cblas_svec y){
  assert(x.n == y.n);
  return cblas_dsdot(x.n, x.data, x.inc, y.data, y.inc);
}

/*
  Scalar multiply: x = ax
*/
SINLINE cblas_svec ssmul(const float a, cblas_svec x){
  cblas_sscal(x.n, a, x.data, x.inc);
  return x;
}
SINLINE cblas_dvec dsmul(const double a, cblas_dvec x){
  cblas_dscal(x.n, a, x.data, x.inc);
  return x;
}
/*
  Scalar multiply and addition: y = axᵀ + y
*/
SINLINE cblas_svec saxpy(const float a, const cblas_svec x, cblas_svec y){
  assert(x.n == y.n);
  cblas_saxpy(x.n, a, x.data, x.inc, y.data, y.inc);
  return y;
}

SINLINE cblas_dvec daxpy(const double a, const cblas_dvec x, cblas_dvec y){
  assert(x.n == y.n);
  cblas_daxpy(x.n, a, x.data, x.inc, y.data, y.inc);
  return y;
}
/*
  Compute the Euclidian norm: ||x||₂ =
    (sqrt (reduce + (map (lambda (x) (pow x 2)) x)))
*/
SINLINE float snrm2(const cblas_svec x){
  return cblas_snrm2(x.n, x.data, x.inc);
}
SINLINE double dnrm2(const cblas_dvec x){
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
  float s,c;
  sincosf(t, &s, &c);
  cblas_srot(x.n, x.data, x.inc, y.data, y.inc, c, s);
  return;
}
SINLINE void drot_by(cblas_dvec x, cblas_dvec y, const double t){
  assert(x.n == y.n);
  double s,c;
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
SINLINE cblas_svec smvmul(const cblas_smat A, const float a,
                          const cblas_svec x, const float b, cblas_svec y){
  assert(A.m == x.n &&  A.n == y.n);

  cblas_sgemv(CblasRowMajor, CblasNoTrans, A.m, A.n, a, A.data, A.lda,
              x.data, x.inc, b, y.data, y.inc);
  return y;
}
SINLINE cblas_dvec dmvmul(const cblas_dmat A, const double a,
                          const cblas_dvec x, const double b, cblas_dvec y){
  assert(A.m == x.n &&  A.n == y.n);
  cblas_dgemv(A.order, CblasNoTrans, A.m, A.n, a, A.data, A.lda,
              x.data, x.inc, b, y.data, y.inc);
  return y;
}
/*
  General transposed matrix vector multiplication: y = αAᵀx + βy

*/
SINLINE cblas_svec smvmul_t(const cblas_smat A, const float a,
                             const cblas_svec x, const float b, cblas_svec y){
  assert(A.m == x.n &&  A.n == y.n);

  cblas_sgemv(A.order, CblasTrans, A.m, A.n, a, A.data, A.lda,
              x.data, x.inc, b, y.data, y.inc);
  return y;
}
SINLINE cblas_svec dmvmul_t(const cblas_smat A, const double a,
                             const cblas_svec x, const double b, cblas_svec y){
  assert(A.m == x.n &&  A.n == y.n);

  cblas_sgemv(A.order, CblasTrans, A.m, A.n, a, A.data, A.lda,
              x.data, x.inc, b, y.data, y.inc);
  return y;
}

/*
  Matrix + scaled tensor product of 2 vectors: A = axyᵀ + A
*/
SINLINE cblas_smat sger(const float a, const cblas_svec x,
                         const cblas_svec y, cblas_smat A){
  assert(A.n == x.n && A.m == y.n);
  cblas_sger(A.order, A.m, A.n, a, x.data, x.inc,
             y.data, y.inc, A.data, A.lda);
  return A;
}

SINLINE cblas_dmat dger(const double a, const cblas_dvec x,
                         const cblas_dvec y, cblas_dmat A){
  assert(A.m == x.n && A.n == y.n);
  cblas_dger(A.order, A.m, A.n, a, x.data, x.inc,
             y.data, y.inc, A.data, A.lda);
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
  assert(A.order == B.order && (!b || (A.order == C.order)));
  cblas_sgemm(A.order, CblasNoTrans, CblasNoTrans, C.m, C.n, A.n,
              a, A.data, A.lda, B.data, B.lda, b, C.data, C.lda);
  return C;
}
SINLINE cblas_dmat dgemm(const double a, const cblas_dmat A, const cblas_dmat B,
                         const double b, cblas_dmat C){
  assert(A.m == C.m && A.n == B.m && B.n == C.n);
  assert(A.order == B.order && (!b || (A.order == C.order)));
  cblas_dgemm(A.order, CblasNoTrans, CblasNoTrans, C.m, C.n, A.n,
              a, A.data, A.lda, B.data, B.lda, b, C.data, C.lda);
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
  assert(A.order == B.order && (!b || (A.order == C.order)));
  enum CBLAS_TRANSPOSE trans_a = (trans & 0x1 ? CblasTrans : CblasNoTrans);
  enum CBLAS_TRANSPOSE trans_b = (trans & 0x2 ? CblasTrans : CblasNoTrans);
  cblas_sgemm(A.order, trans_a, trans_b, C.m, C.n, A.n,
              a, A.data, A.lda, B.data, B.lda, b, C.data, C.lda);
  return C;
}
SINLINE cblas_dmat dgemm_t(const double a, const cblas_dmat A, const cblas_dmat B,
                           const double b, cblas_dmat C, int trans){
  assert(A.m == C.m && A.n == B.m && B.n == C.n);
  assert(A.order == B.order && (!b || (A.order == C.order)));
  enum CBLAS_TRANSPOSE trans_a = (trans & 0x1 ? CblasTrans : CblasNoTrans);
  enum CBLAS_TRANSPOSE trans_b = (trans & 0x2 ? CblasTrans : CblasNoTrans);
  cblas_dgemm(A.order, trans_a, trans_b, C.m, C.n, A.n,
              a, A.data, A.lda, B.data, B.lda, b, C.data, C.lda);
  return C;
}
              
/*
  Additional utility functions
*/
/*
  Functions which create new vectors/matrices.
  Often we can avoid initializing the new vector/matrix by passing 0 for the
  scalar it's multiplied by, letting us use malloc instead of calloc.
*/
SINLINE cblas_svec* smvmul_new(const cblas_smat A, const int a,
                              const cblas_svec x){
  assert(A.m == x.n);
  cblas_svec *ret = XMALLOC(A.n*sizeof(float) + sizeof(cblas_svec));
  if(ret == NULL){
    return NULL;
  }
  ret->n = A.n;
  ret->inc = 1;
  //cast to uint8_t for pointer arithmatic
  ret->data = (float*)((uint8_t*)ret + sizeof(cblas_svec));
  cblas_sgemv(A.order, CblasNoTrans, A.m, A.n, a, A.data, A.lda,
              x.data, x.inc, 0, ret->data, ret->inc);
  return ret;
}
SINLINE cblas_dvec* dmvmul_new(const cblas_dmat A, const int a,
                               const cblas_dvec x){
  assert(A.m == x.n);
  cblas_dvec *ret = XMALLOC(A.n*sizeof(double) + sizeof(cblas_svec));
  if(ret == NULL){
    return NULL;
  }
  ret->n = A.n;
  ret->inc = 1;
  ret->data = (double*)((uint8_t*)ret + sizeof(cblas_svec));
  cblas_dgemv(A.order, CblasNoTrans, A.m, A.n, a, A.data, A.lda,
              x.data, x.inc, 0, ret->data, ret->inc);
  return ret;
}
SINLINE cblas_smat* sgemm_new(const float a, const cblas_smat A,
                             const cblas_smat B){
  assert(A.n == B.m);
  assert(A.order == B.order);
  cblas_smat *C = XMALLOC(A.m*B.n*sizeof(float) + sizeof(cblas_smat));
  if(C == NULL){
    return NULL;
  }
  C->m = A.m; C->n = B.n;
  C->lda = C->m;
  C->order = A.order;
  C->data = (float*)((uint8_t*)C + sizeof(cblas_smat));
  cblas_sgemm(A.order, CblasNoTrans, CblasNoTrans, A.m, B.n, A.n,
              a, A.data, A.lda, B.data, B.lda, 0, C->data, C->lda);
  return C;
}
SINLINE cblas_dmat* dgemm_new(const double a, const cblas_dmat A,
                             const cblas_dmat B){
  assert(A.n == B.m);
  assert(A.order == B.order);
  cblas_dmat *C = XMALLOC(A.m*B.n*sizeof(double) + sizeof(cblas_smat));
  if(C == NULL){
    return NULL;
  }
  C->m = A.m; C->n = B.n;
  C->lda = C->m;
  C->order = A.order;
  C->data = (double*)((uint8_t*)C + sizeof(cblas_smat));
  cblas_dgemm(A.order, CblasNoTrans, CblasNoTrans, A.m, B.n, A.n,
              a, A.data, A.lda, B.data, B.lda, 0, C->data, C->lda);
  return C;
}

SINLINE cblas_smat *make_smat_idenity(int n){
  int i;
  cblas_smat *ret = ZMALLOC(n*n*sizeof(float) + sizeof(cblas_smat));
  for(i=0;i<n;i++){
    ret->data[i*n + i] = 1.0f;
  }
  return ret;
}
SINLINE cblas_smat *make_dmat_idenity(int n){
  int i;
  cblas_smat *ret = ZMALLOC(n*n*sizeof(double) + sizeof(cblas_smat));
  for(i=0;i<n;i++){
    ret->data[i*n + i] = 1.0;
  }
  return ret;
}

SINLINE cblas_smat* stensor_mul(const cblas_svec x, const cblas_svec y){
  //need to explicitly zero this
  cblas_smat *ret = ZMALLOC(x.n*y.n*sizeof(float) + sizeof(cblas_smat));
  ret->m = x.n; ret->n = y.n; ret->lda = x.n; ret->CblasRowMajor;
  cblas_sger(ret->order, x.n, y.n, 1, x.data, x.inc,
             y.data, y.inc, ret->data, ret->lda);
  return ret;
}
SINLINE cblas_dmat* dtensor_mul(const cblas_dvec x, const cblas_dvec y){
  cblas_dmat *ret = ZMALLOC(x.n*y.n*sizeof(double) + sizeof(cblas_dmat));
  ret->m = x.n; ret->n = y.n; ret->lda = x.n; ret->CblasRowMajor;
  cblas_dger(ret->order, x.n, y.n, 1, x.data, x.inc,
             y.data, y.inc, ret->data, ret->lda);
  return ret;
}

SINLINE cblas_smat* stensor_mul_col(const cblas_svec x, const cblas_svec y){
  //need to explicitly zero this
  cblas_smat *ret = ZMALLOC(x.n*y.n*sizeof(float) + sizeof(cblas_smat));
  ret->m = x.n; ret->n = y.n; ret->lda = x.n; ret->CblasColMajor;
  cblas_sger(ret->order, x.n, y.n, 1, x.data, x.inc,
             y.data, y.inc, ret->data, ret->lda);
  return ret;
}
SINLINE cblas_dmat* dtensor_mul_col(const cblas_dvec x, const cblas_dvec y){
  cblas_dmat *ret = ZMALLOC(x.n*y.n*sizeof(double) + sizeof(cblas_dmat));
  ret->m = x.n; ret->n = y.n; ret->lda = x.n; ret->CblasColMajor;
  cblas_dger(ret->order, x.n, y.n, 1, x.data, x.inc,
             y.data, y.inc, ret->data, ret->lda);
  return ret;
}
/*
  Routines defined in cblas_aux.c
*/
cblas_smat smat_addmul(const float a, const cblas_smat A,
                       const float b, cblas_smat B);
cblas_smat smat_addmul_square(const float a, const cblas_smat A,
                       const float b, cblas_smat B);

cblas_dmat dmat_addmul(const double a, const cblas_dmat A,
                       const double b, cblas_dmat B);
cblas_dmat dmat_addmul_square(const double a, const cblas_dmat A,
                       const double b, cblas_dmat B);
/*
  TODO: cross product, transposition/operation w/transposition.
*/
#ifdef UNDEF_ZMALLOC
#undef ZMALLOC
#endif

#ifdef UNDEF_XMALLOC
#undef XMALLOC
#endif

#endif /* _CBLAS_UTIL_H_ */
