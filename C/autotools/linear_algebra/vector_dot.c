
#include "linear_algebra.h"

// ----------------------------------------------------------------------
// vector_dot
//
// calculates and returns the scalar product of the two vectors
// x: first vector
// y: second vector

double
vector_dot(struct vector *x, struct vector *y)
{
  // can only dot vectors of equal length
  assert(x->n == y->n);

  double sum = 0.;
  for (int i = 0; i < x->n; i++) {
    sum += VEC(x, i) * VEC(y, i);
  }
  return sum;
}

