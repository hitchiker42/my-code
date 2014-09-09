#include "common.h"
/* Nothing outside of this file/cons.h should need to manipulate
   literal cons cells, that is to say only pointers to conses
   need to be dealt with.
*/
#define XCAR(obj) (XCONS(obj)->car)
#define XCDR(obj) (XCONS(obj)->cdr)

