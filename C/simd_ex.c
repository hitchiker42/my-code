#include <immintrin.h>
#include <xmmintrin.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <limits.h>
/*For now N must = M and N%4=0*/
#define N 8192
#define M 8192
/*Macros to simplify timing*/
#define gettime(X)  gettimeofday(&X,NULL);
#define time_dbl(X) (double)(X.tv_sec+X.tv_usec*.000001)
struct timeval now;
int i,j,k,i2,j2;
struct {
  /*
   *Hold the Arrays/pointers we're working with in
   *one place for convience
   */
  double *dbl_a,*dbl_b,*dbl_c;
  double *double_a,*double_b,*double_c;
  float *flt_a,*flt_b,*flt_c;
  float *float_a,*float_b,*float_c;
} X;
/*
 *Initalizes the data structures,
 *allocates memory to hold all needed values
 *and initalizes the result to 0.
 *The values being multiplied are initalized with random
 *numbers.
 */
void init(){
  X.dbl_a=malloc(M*N*sizeof(double));
  X.dbl_b=malloc(M*N*sizeof(double));
  X.dbl_c=calloc(M*N,sizeof(double));
  X.double_a=malloc(M*N*sizeof(double));
  X.double_b=malloc(M*N*sizeof(double));
  X.double_c=calloc(M*N,sizeof(double));
  X.flt_a=malloc(M*N*sizeof(float));
  X.flt_b=malloc(M*N*sizeof(float));
  X.flt_c=calloc(M*N,sizeof(float));
  X.float_a=malloc(M*N*sizeof(float));
  X.float_b=malloc(M*N*sizeof(float));
  X.float_c=calloc(M*N,sizeof(float));
  /*dbl=drand48()*lrand48()
    flt=(float)drand48()*lrand48()
    long=lrand48()
    int=rand()
    shrt=rand()%USHRT_MAX
    byte=rand()%UCHAR_MAX
  */
  for(i=0;i<N;i++){
    for(j=0;j<M;j++){
      *(X.dbl_a+(i*M)+j)=(double)drand48()*lrand48();
      *(X.double_a+(i*M)+j)=(double)drand48()*lrand48();
      *(X.flt_a+(i*M)+j)=(float)drand48()*lrand48();
      *(X.float_a+(i*M)+j)=(float)drand48()*lrand48();
      /*if we want rectangular arrays then change
        values below to *(foo(j*N)+i*/
      *(X.dbl_b+(i*N)+j)=drand48()*lrand48();
      *(X.double_b+(i*N)+j)=drand48()*lrand48();
      *(X.flt_b+(i*N)+j)=(float)drand48()*lrand48();
      *(X.float_b+(i*N)+j)=(float)drand48()*lrand48();
    }
  }
}
int main(int argv,char** args){
  double start,stop;
  double a,b,c,d;
  srand48(-1);
  gettime(now);start=time_dbl(now);
  init();
  gettime(now);stop=time_dbl(now);
  a=stop-start;
  /*loops w/out simd*/
  gettime(now);start=time_dbl(now);
  for(i=0;i<N;i++){
    for(j=0;j<M;j++){
      /*
       *standard matrix multiplication loop, used to compare
       *times with the simd loop
       */
      *(X.double_c+(i*N)+j)+=
        *(X.double_a+(i*M)+j)*
        *(X.double_b+(j*N)+i);
      *(X.float_c+(i*N)+j)+=
        *(X.float_a+(i*M)+j)*
        *(X.float_b+(j*N)+i);
    }
  }
  gettime(now);stop=time_dbl(now);
  b=stop-start;
  /*loops w/simd*/
  gettime(now);start=time_dbl(now);
  for(i=0;i<N;i++){
    for(j=0;j<M;j+=4){
      /*
       *Store 4 doubles into the ymm registers
       *and multiply them
       */
      __v4df vecd_a={*(X.dbl_a+(i*M)+j),*(X.dbl_a+(i*M)+j+1),\
                   *(X.dbl_a+(i*M)+j+2),*(X.dbl_a+(i*M)+j+3)};
      __v4df vecd_b={*(X.dbl_b+(j*N)+i),*(X.dbl_b+(j*N)+i+1),\
                   *(X.dbl_b+(j*N)+i+2),*(X.dbl_b+(j*N)+i+3)};
      __v4df vecd_c=__builtin_ia32_mulpd256(vecd_a,vecd_b);
      for(k=0;k<4;k++){
        /*
         *Store the results from the above calculations
         *into memory
         */
        *(X.dbl_c+(i*M)+j+k)+=vecd_c[k];
      }
    }
  }
  gettime(now);stop=time_dbl(now);
  c=stop-start;
  gettime(now);start=time_dbl(now);
  for(i=0;i<N;i++){
    for(j=0;j<M;j+=8){
     /*
      *move 2 sets of 8 floats into ymm registers and multiply them
      */
      __v8sf vecf_a={*(X.flt_a+(i*N)+j),*(X.flt_a+(i*N)+j+1),
                     *(X.flt_a+(i*N)+j+2),*(X.flt_a+(i*N)+j+3),
                     *(X.flt_a+(i*N)+j+4),*(X.flt_a+(i*N)+j+5),
                     *(X.flt_a+(i*N)+j+6),*(X.flt_a+(i*N)+j+7),};
      __v8sf vecf_b={*(X.flt_b+(i*N)+j),*(X.flt_b+(i*N)+j+1),
                     *(X.flt_b+(i*N)+j+2),*(X.flt_b+(i*N)+j+3),
                     *(X.flt_b+(i*N)+j+4),*(X.flt_b+(i*N)+j+5),
                     *(X.flt_b+(i*N)+j+6),*(X.flt_b+(i*N)+j+7),};
      __v8sf vecf_c=__builtin_ia32_mulps256(vecf_a,vecf_b);
      for(k=0;k<8;k++){
        /*
         *Store the results from the above calculations
         *into memory
         */
        *(X.flt_c+(i*M)+j+k)+=vecf_c[k];
      }
    }
  }
  gettime(now);stop=time_dbl(now);
  d=stop-start;
  printf("Time to init: %f sec\n",a);
  printf("Without Simd: %f sec\n",b);
  printf("With Simd:\nDoubles: %f sec\n"\
         "Floats: %f sec\nTotal: %f sec\n",c,d,c+d);
  return 0;
}
/*typedef double __v4df __attribute__ ((__vector_size__ (32)));
typedef float __v8sf __attribute__ ((__vector_size__ (32)));
typedef long long __v4di __attribute__ ((__vector_size__ (32)));
typedef int __v8si __attribute__ ((__vector_size__ (32)));
typedef short __v16hi __attribute__ ((__vector_size__ (32)));
typedef char __v32qi __attribute__ ((__vector_size__ (32)));
typedef double __v2df __attribute__ ((__vector_size__ (16)));
typedef long long __v2di __attribute__ ((__vector_size__ (16)));
typedef int __v4si __attribute__ ((__vector_size__ (16)));
typedef short __v8hi __attribute__ ((__vector_size__ (16)));
typedef char __v16qi __attribute__ ((__vector_size__ (16)));*/
