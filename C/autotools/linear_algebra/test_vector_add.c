
#include <stdio.h>

#include "linear_algebra.h"

// ----------------------------------------------------------------------
// main
//
// test the vector_add() function

int
main(int argc, char **argv)
{
  const int n = 3;

  // init our vectors of size n
  struct vector *x = vector_create(n);
  struct vector *y = vector_create(n);
  struct vector *z = vector_create(n);
  struct vector *z_ref = vector_create(n);

  // initialize values for testing
  for (int i = 0; i < n; i++) {
    VEC(x, i) = i + 1.f;
    VEC(y, i) = i + 2.f;
    VEC(z_ref, i) = 2*i + 3.f;
  }

  // test the vector_add()
  vector_add(x, y, z);

  // and make sure it equals the reference result
  assert(vector_is_equal(z, z_ref));

  // clean up
  vector_destroy(x);
  vector_destroy(y);
  vector_destroy(z);
  vector_destroy(z_ref);

  return 0;
}
