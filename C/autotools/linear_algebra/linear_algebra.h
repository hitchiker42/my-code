
// include guards: an idiom which ensures that it's okay to include
// the header more than once -- the 2nd time around, LINEAR_ALGEBRA_H will
// be defined already and so nothing will be included anymore.

#ifndef LINEAR_ALGEBRA_H
#define LINEAR_ALGEBRA_H

#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#define BOUNDSCHECK

// ----------------------------------------------------------------------
// struct vector

struct vector {
  double *vals;
  int n;
};

#ifdef BOUNDSCHECK
#define VEC(v, i) (*({				\
	assert((i) >= 0 && (i) < (v)->n);	\
	&((v)->vals[(i)]);			\
      })) 
#else
#define VEC(v, i) ((v)->vals[i])
#endif

struct vector *vector_create(int n);
void vector_destroy(struct vector *v);
bool vector_is_equal(struct vector *v1, struct vector *v2);
void vector_add(struct vector *x, struct vector *y, struct vector *z);
double vector_dot(struct vector *x, struct vector *y);

#endif
