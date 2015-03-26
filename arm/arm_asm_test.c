
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <alloca.h>
//factorials, test recursion, and 64 bit ints
int64_t fact_naive(int64_t n){
  if(n <= 1){
    return 1;
  } else {
    return n*fact_naive(n-1);
}
}


uint64_t ackerman(uint64_t m,uint64_t n){
  if(!m){ 
    return n+1;
  } else if(!n){
    return ackerman(m-1,1);
  } else {
    return ackerman(m-1,ackerman(m,n-1));
  }
}
//dot_product to test arrays/floating point
double dot_product_double(double *A,double *B,int n){
  //assume n correctly specifys the length of A and B
  double acc=0;
  int i;
  for(i=0;i<n;i++){
    acc+=(A[i]*B[i]);
  }
  return acc;
}
float dot_product_float(float *A,float *B,int n){
  //assume n correctly specifys the length of A and B
  float acc=0;
  int i;
  for(i=0;i<n;i++){
    acc+=(A[i]*B[i]);
  }
  return acc;
}
//see if we can get gcc to vectorize this
float __attribute__((optimize(3,"fast-math")))
dot_product_float_fast(float *A,float *B,int n){
  //assume n correctly specifys the length of A and B
  float acc=0;
  int i;
  for(i=0;i<n;i++){
    acc+=(A[i]*B[i]);
  }
  return acc;
}
int dot_product_int(int *A,int *B,int n){
  //assume n correctly specifys the length of A and B
  int acc=0;
  int i;
  for(i=0;i<n;i++){
    acc+=(A[i]*B[i]);
  }
  return acc;
}
int __attribute__((optimize(3)))
dot_product_int_fast(int *A,int *B,int n){
  //assume n correctly specifys the length of A and B
  int acc=0;
  int i;
  for(i=0;i<n;i++){
    acc+=(A[i]*B[i]);
  }
  return acc;
}
//test passing a bunch of arguments
double abi_test(uint8_t a, int16_t b, uint32_t c, uint64_t d, float e, double f){
  b+=a;
  c+=b;
  d+=c;
  e+=d;
  f+=e;
  return f;
}
//see how va_arg functions work
//# of args passed as first parameter, just sum them and return
int sum_vararg(int n, ...){
  va_list ap;
  va_start(ap, a);
  int acc = 0, cur;
  while(n--){
    cur = va_arg(ap, int);
    acc+=cur;
  }
  return acc;
}

//test function pointers + stack allocation
int map_reduce(int *arr, int len, int(*map)(int), int(*reduce)(int, int)){
  int *tmp = alloca(len * sizeof(int));
  memcpy(tmp, arr, len*sizeof(int));
  tmp[0] = map(tmp[0]);
  int i, acc = tmp[0]
  for(i=1;i<len;i++){
    tmp[i] = map(tmp[i]);
    acc = reduce(acc, tmp[i]);
  }
  
  return acc;
}
  
