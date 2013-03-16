
#include <stdio.h>

#include "linear_algebra.h"

// ----------------------------------------------------------------------
// main
//
// test the vector_dot() function

int
main(int argc, char **argv)
{
  const int n = 3;

  // init our vectors of size n
  struct vector *x = vector_create(n);
  struct vector *y = vector_create(n);

  // initialize values for testing
  for (int i = 0; i < n; i++) {
    VEC(x, i) = i + 1.f;
    VEC(y, i) = i + 2.f;
  }

  // calculate scalar product and make sure it's correct
  assert(vector_dot(x, y) == 20.f);

  // clean up
  vector_destroy(x);
  vector_destroy(y);

  return 0;
}
