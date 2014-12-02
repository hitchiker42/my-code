#include <stdio.h>
#include "vec_math.h"
// ----------------------------------------------------------------------
// scalar_product
//
// calculates the scalar product of the two vectors
// x: first vector
// y: second vector

double scalar_product(vector x, vector y) {
  if (x.len == y.len){
    double sum = 0;
    for (int i=0;i<x.len;i++) {
      sum += x.vals[i]*y.vals[i];
    }
    return sum;
  }
  else{
    printf("vectors must be the same length\n");
    exit(-1);
  }
}
