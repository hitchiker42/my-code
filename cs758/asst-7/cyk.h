/**

   cyk parser header file.

 */

#ifndef CYK_H
#define CYK_H

/**
   This function is the core of the CYK parser.  It should produce a
   parse that looks like this:

   LHS(RHS1 RHS2)

   This is recursive, so you can have stuff like this:

   S(NP(N(fish) N(fish)) VP(V(fish) NP(fish)))

 */
void cyk_parse(void);
void *xmalloc(size_t sz);
#include <string.h>
static inline int string_equal(const char *a, const char *b){
  return !strcmp(a,b);
}
/*
 This function checks if the void* passed in is null.  If it is, then 
 it prints and error message and kills the program.
 */
void malloc_check(void* p);

#endif
