#ifndef VEC_MATH_H
#define VEC_MATH_H
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <signal.h>
#define HERE fprintf(stderr, "HERE at %s:%d (%s)\n", __FILE__, __LINE__, __FUNCTION__)  
typedef struct {
  double* vals;
  int len;
} vector;
typedef struct {
  double** vals;
  int columns;
  int rows
} matrix;
//probably not the best practice to put this in the header file
//but I don't know enough about cpp macros to make this into one
inline void malloc_test(_Bool test){
  if  (test){
      fprintf(stderr,"Memory Allocation Failed");
      raise(SIGSEGV);//maybe not the most elegnt way of dealing with this
    }
}
#endif
