#ifndef _CBLAS_UTIL_H_
#define _CBLAS_UTIL_H_
#include <cblas.h>
extern "C" {
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
  a lda x n block of memory
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
cblas_svec make_svec_simple(float *data, int n);
cblas_dvec make_dvec_simple(double *data, int n);
//make a vector, specifying all parameters
cblas_svec make_svec(float *data, int n, int inc);
cblas_dvec make_dvec(double *data, int n, int inc);
//make a square row major matrix with implicit lda = n
cblas_smat make_smat_square(float *data, int n);
cblas_dmat make_dmat_square(double *data, int n);
//make a square column major matrix with implicit lda = n
cblas_smat make_smat_square_col(float *data, int n);
cblas_dmat make_dmat_square_col(double *data, int n);
//make a row major matrix, specifying all parameters
cblas_smat make_smat(float *data, int n, int m, int lda);
cblas_dmat make_dmat(double *data, int n, int m, int lda);
//make a column major matrix, specifying all parameters
cblas_smat make_smat_col(float *data, int n, int m, int lda);
cblas_dmat make_dmat_col(double *data, int n, int m, int lda);
/*
  A note on paramater names:
    -vectors use lowercase x,y,z
    -matrices use uppercase A,B,C
    -scalars use lowercase a;b;c, they should use α;β;γ, but
       you can't use unicode characters in C variables, and actually
       writing out alpha and beta is too annoying.
*/
/*
  Blas level 1 functions (vector, vector operations);
*/
/*
  Dot product: xᵀ · y
*/
float sdot(const cblas_svec x, const cblas_svec y);

double ddot(const cblas_dvec x, const cblas_dvec y);
double dsdot(const cblas_svec x, const cblas_svec y);

/*
  Scalar multiply: x = ax
*/
cblas_svec ssmul(const float a, cblas_svec x);
cblas_dvec dsmul(const double a, cblas_dvec x);
/*
  Scalar multiply and addition: y = axᵀ + y
*/
cblas_svec saxpy(const float a, const cblas_svec x, cblas_svec y);

cblas_dvec daxpy(const double a, const cblas_dvec x, cblas_dvec y);
/*
  Compute the Euclidian norm: ||x||₂ =
    (sqrt (reduce + (map (lambda (x); (pow x 2);); x);););
*/
float snrm2(const cblas_svec x);
double dnrm2(const cblas_dvec x);
/*
  Compute the L1 norm: ||x||₁ = (reduce + (map abs x););
    equivlent to the blas function asum
*/
float snrm1(const cblas_svec x);
double dnrm1(const cblas_dvec x);
/*
  plane rotation: rotate the points (xᵢ,yᵢ);,i=0,...,n-1 according to
    the rotation matrix [[c, -s],[s, c]]
  unfortunately these can't return a value, since C doesn't have (convient);
  ways to return multiple values
*/
void srot(cblas_svec x, cblas_svec y, const float c, const float s);
void drot(cblas_dvec x, cblas_dvec y, const double c, const double s);
/*
  Same as the above rotation functions, except c = cos(t); and s = sin(t);.
  The gnu c function sincos is used to optimize the compution of these values.
    
*/
void srot_by(cblas_svec x, cblas_svec y, const float t);
void drot_by(cblas_dvec x, cblas_dvec y, const double t);
/*
  Blas level 2 functions (vector, matrix operations);
*/
/*
  General matrix vector multiplication: y = αAx + βy

*/
cblas_svec smvmul(const cblas_smat A, const float a,
                           const cblas_svec x, const float b, cblas_svec y);
cblas_dvec dmvmul(const cblas_dmat A, const double a,
                          const cblas_dvec x, const double b, cblas_dvec y);
/*
  General transposed matrix vector multiplication: y = αAᵀx + βy

*/
cblas_svec smvmul_t(const cblas_smat A, const float a,
                             const cblas_svec x, const float b, cblas_svec y);
cblas_svec dmvmul_t(const cblas_smat A, const double a,
                             const cblas_svec x, const double b, cblas_svec y);

/*
  Cross product, scale and add: A = axyᵀ + A
*/
cblas_smat sger(const float a, const cblas_svec x,
                         const cblas_svec y, cblas_smat A);

cblas_dmat dger(const double a, const cblas_dvec x,
                         const cblas_dvec y, cblas_dmat A);
/*
  Level 3 blas functions
*/
/*
  good old matrix multiplication: C = aA * B + bC
*/
cblas_smat sgemm(const float a, const cblas_smat A, const cblas_smat B,
                         const float b, cblas_smat C);
cblas_dmat dgemm(const double a, const cblas_dmat A, const cblas_dmat B,
                         const double b, cblas_dmat C);
/*
  Matrix multiplication w/transposition. The parameter t is a 2-bit bifield
  specifying what to transpose, bit 1 coorsponds to A and bit 2 to B
*/
cblas_smat sgemm_t(const float a, const cblas_smat A, const cblas_smat B,
                           const float b, cblas_smat C, int trans);
cblas_dmat dgemm_t(const double a, const cblas_dmat A, const cblas_dmat B,
                           const double b, cblas_dmat C, int trans);
              
/*
  Additional utility functions
*/
/*
  Functions which create new vectors/matrices. Needed to be freed 
  explicitly using free.
*/
cblas_svec* smvmul_new(const cblas_smat A, const int a, 
                       const cblas_svec x);
cblas_dvec* dmvmul_new(const cblas_dmat A, const int a,
                       const cblas_dvec x);
cblas_smat* sgemm_new(const float a, const cblas_smat A,
                             const cblas_smat B);
cblas_dmat* dgemm_new(const double a, const cblas_dmat A,
                             const cblas_dmat B);
cblas_smat *make_smat_idenity(int n);
cblas_smat *make_dmat_idenity(int n);
cblas_smat* stensor_mul(const cblas_svec x, const cblas_svec y);
cblas_dmat* dtensor_mul(const cblas_dvec x, const cblas_dvec y);
cblas_smat* stensor_mul_col(const cblas_svec x, const cblas_svec y);
cblas_dmat* dtensor_mul_col(const cblas_dvec x, const cblas_dvec y);

} //extern "C"
#endif /* _CBLAS_UTIL_H_ */
