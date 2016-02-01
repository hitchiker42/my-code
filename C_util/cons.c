#include "C_util.h"
#define POP(x)                                  \
  ({void *ret = XCAR(x);                        \
    x = XCDR(x);                                \
    ret;})
cons_t split(cons_t *ls, int n){
  if(n == 0){
    return (cons_t){.car = NULL, .cdr = ls};
  }
  cons_t *ret = make_cons(POP(ls), NULL);
  cons_t *ptr = ret;
  while(--n && ls){
    XSETCDR(ptr, make_cons(POP(ls), NULL));
    ptr = XCDR(ptr);
  }
  return (cons_t){.car = ret, .cdr = ls};
}
static cons_t* merge_cons(cons_t *A, cons_t *B, cmp_fun cmp){
  cons_t *ret = NULL;
  while(A && B){
    if(cmp(XCAR(A),XCAR(B))){
      ret = make_cons(XCAR(A), ret);
      A = XCDR(A);
    } else {
      ret = make_cons(XCDR(B), ret);
      B = XCDR(B);
    }
  }
//Only one of these loops will run
  while(A){
    ret = make_cons(XCAR(A), ret);
  }
  while(B){
    ret = make_cons(XCAR(B), ret);
  }
}
static cons_t* mergesort_cons(cons_t *input, cmp_fun cmp){
  if(!input || !XCDR(input)){
    return input;
  }
  cons_t *head = NULL, *tail = NULL;
  while(input){
    head = make_cons(POP(input, head));
    if(!input){break;}
    tail = make_cons(POP(input, tail));
  }
  head = mergesort_cons(head, cmp);
  tail = mergesort_cons(tail, cmp);
  return merge_cons(head, tail, cmp);
}
