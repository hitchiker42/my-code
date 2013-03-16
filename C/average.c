#include "vector.h"
/*Write a function that takes the N + 1 values f_i (in a vector / array), and calculates the N values F_i by averaging: F_i = .5 * (f_i + f_{i+1}). */
vector
midpoints(vector x){
  int i=0;
  vector y={x.len,malloc(sizeof(double*)*x.len-1)}
  malloc_test(y.vals==NULL);
  for(i;i<x.len-1;i++){
    y.vals[i]=(x.vals[i+1]-x.vals[i])/2;
  }
  return y;
}
