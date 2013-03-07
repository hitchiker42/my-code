
#include "linear_algebra.h"

// ----------------------------------------------------------------------
// vector_add
//
// calculates the sum of two vectors z = x + y
// x: first vector
// y: second vector
// z: result vector

void
vector_add(struct vector *x, struct vector *y, struct vector *z)
{
  // can only add vectors of equal length
  assert(x->n == y->n);
  assert(x->n == z->n);

  for (int i = 0; i < x->n; i++) {
    VEC(z, i) = VEC(x, i) + VEC(y, i);
  }
}

