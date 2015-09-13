#include "cblas_util.h"
cblas_smat smat_addmul(const float a, const cblas_smat A,
                       const float b, cblas_smat B){
  int i,j;
  assert(A.n == B.n && A.m == B.m && A.order == B.order);
  if(A.order == CblasRowMajor){
    for(i=0;i<A.n;i++){      
      for(j=0;j<A.m;j++){
        int A_ij = i*A.lda + j;
        int B_ij = i*B.lda + j;
        B[B_ij] = a*A[A_ij] + b*B[B_ij];
      }
    }
  } else if(A.order == CblasColMajor){
    for(i=0;i<A.m;i++){
      for(j=0;j<A.n;j++){
        int A_ij = i*A.lda + j;
        int B_ij = i*B.lda + j;
        B[B_ij] = a*A[A_ij] + b*B[B_ij];
      }
    }
  }
  return B;
}
/*
  For NxN square matrices with lda = N the nested loop can be flattened.
*/
cblas_smat smat_addmul_square(const float a, const cblas_smat A,
                              const float b, cblas_smat B){
  assert(A.n == B.n == A.m == B.m == A.lda == B.lda);
  int i;
  for(i=0;i<A.n*A.n;i++){
    B[i] = a*A[i] + b*B[i];
    //could be unrolled for speed
  }
  return B;
}

cblas_dmat dmat_addmul(const double a, const cblas_dmat A,
                       const double b, cblas_dmat B){
  int i,j;
  assert(A.n == B.n && A.m == B.m && A.order == B.order);
  assert(A.order == B.order);
  if(A.order == CblasRowMajor){
    for(i=0;i<A.n;i++){      
      for(j=0;j<A.m;j++){
        int A_ij = i*A.lda + j;
        int B_ij = i*B.lda + j;
        B[B_ij] = a*A[A_ij] + b*B[B_ij];
      }
    }
  } else if(A.order == CblasColMajor){
    for(i=0;i<A.m;i++){
      for(j=0;j<A.n;j++){
        int A_ij = i*A.lda + j;
        int B_ij = i*B.lda + j;
        B[B_ij] = a*A[A_ij] + b*B[B_ij];
      }
    }
  }
  return B;
}
/*
  For NxN square matrices with lda = N the nested loop can be flattened.
*/
cblas_dmat dmat_addmul_square(const double a, const cblas_dmat A,
                              const double b, cblas_dmat B){
  assert(A.n == B.n == A.m == B.m == A.lda == B.lda);
  assert(A.order == B.order);
  int i;
  for(i=0;i<A.n*A.n;i++){
    B[i] = a*A[i] + b*B[i];
    //could be unrolled for speed
  }
  return B;
}
/*
  Sets y[0:3] to the cross product of x[0:3] and y[0:3], x and y
  can have more than 3 elements, but they are not used/modified.
*/
cblas_svec svec_cross(const cblas_svec x, cblas_svec y){
  assert(x.n >= 3 && y.n >= 3);
  float temp[3] = {y.data[0], y.data[y.inc], y.data[2*y.inc]};
  y.data[0] = x.data[x.inc]*temp[2] - x.data[2*x.inc]*temp[1];
  y.data[y.inc] = x.data[2*x.inc] + temp[0] - x.data[0] * temp[2];
  y.data[2*y.inc] = x.data[0]*temp[1] - x.data[x.inc] * temp[0];
  return y;
}
cblas_dvec dvec_cross(const cblas_dvec x, cblas_dvec y){
  assert(x.n >= 3 && y.n >= 3);
  double temp[3] = {y.data[0], y.data[y.inc], y.data[2*y.inc]};
  y.data[0] = x.data[x.inc]*temp[2] - x.data[2*x.inc]*temp[1];
  y.data[y.inc] = x.data[2*x.inc] + temp[0] - x.data[0] * temp[2];
  y.data[2*y.inc] = x.data[0]*temp[1] - x.data[x.inc] * temp[0];
  return y;
}
/*
  Sets A to a skew symmetric matrix such that Ax = x cross x;
*/
cblas_smat* svec_to_skew_symmetric_smat(const cblas_svec x,
                                        cblas_smat A){
  assert(x.n >= 3 && A.m >=3 && A.n >= 3);
  //row major, add column major later
  A.data[1] = -x.data[2*x.inc];
  A.data[2] = x.data[x.inc];
  A.data[A.lda + 2] = -x.data[0];
  A.data[A.lda] = x.data[2*x.inc];
  A.data[2*A.lda] = -x.data[x.inc];
  A.data[2*A.lda + 2] = x.data[0];
  return A;
}
cblas_dmat* dvec_to_skew_symmetric_dmat(const cblas_dvec x,
                                        cblas_dmat A){
  assert(x.n >= 3 && A.m >=3 && A.n >= 3);
  //row major, add column major later
  A.data[1] = -x.data[2*x.inc];
  A.data[2] = x.data[x.inc];
  A.data[A.lda + 2] = -x.data[0];
  A.data[A.lda] = x.data[2*x.inc];
  A.data[2*A.lda] = -x.data[x.inc];
  A.data[2*A.lda + 2] = x.data[0];
  return A;
}
  
