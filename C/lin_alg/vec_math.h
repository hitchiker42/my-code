#ifndef VEC_MATH_H
#define VEC_MATH_H
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <signal.h>
typedef struct {
  double* vals;
  int len;
} vector;
typedef struct {
  double** vals;
  int columns;
  int rows
} matrix;
double scalar_product(vector x, vector y);
vector vec_add(vector x,vector y);
vector mat_vec_mult(matrix A,vector x);
int add_test(int N,FILE* file);
int scalar_test(int N,FILE* file);
int vec_mult_test(int N,int M,FILE* file);
void vec_write(FILE* file,vector* vectors,int n);
//probably not the best practice to put this in the header file
//but I don't know enough about cpp macros to make this into one
inline void malloc_test(_Bool test){
  if  (test){
      fprintf(stderr,"Memory Allocation Failed");
      raise(SIGSEGV);//maybe not the most elegnt way of dealing with this
    }
}
#endif
