#ifndef _CONS_H
#define _CONS_H
/*
 simple implementation of lisp style lists in c
*/
#define NIL NULL
#define XCAR(cons) cons->car
#define XCDR(cons) cons->cdr
#define XCAAR(_cons) ((cons*)_cons->car)->car
#define XCADR(_cons) ((cons*)_cons->car)->cdr
#define PUSH(obj,cons)                          \
  ({cons->cdr=cons;                             \
    cons->car=obj;                              \
    cons;})
#define POP(cons) \
  ({void *retval=XCAR(cons);                    \
    cons=XCDR(cons);                            \
    retval;})
#define Acons(_car,_cdr)                        \
  ({cons *retval=alloca(sizeof(cons));          \
    retval->car=_car;                           \
    retval->cdr=_cdr;                           \
    retval;})
#endif
