#include "cons.h"
//these 2 functions are for easily generating lisp lists in C
//without having to invoke the reader

//make a lisp list given a NULL terminated list of sl_objs
//it is assumed that there are atleast 2 non null arguments
//use make_list1/make_list2 for lists of 1 or 2 elements
sl_obj make_list(sl_obj car,...){
  va_list ap;
  sl_obj arg;
  sl_obj retval = make_cons(car, NULL);
  sl_obj tail = retval;
  va_start(ap, car);
  arg = va_arg(ap, sl_obj);
  do {
    tail->cdr=make_cons(arg, NULL);
    tail = tail->cdr;
  } while (arg = va_arg(ap, sl_obj) != NULL);
  return retval;
}
//for convience, make a list of lisp integers given c integers
//again a minimum of 2 elements is required
sl_obj make_int_list(uint64_t car,...){
  va_list ap;
  uint64_t arg;
  sl_obj retval = make_cons(car, NULL);
  sl_obj tail = retval;
  va_start(ap, car);
  do {
    tail->cdr=make_cons(make_int(arg), NULL);
    tail = tail->cdr;
  } while (arg = va_arg(ap, uint_64) != NULL);
  return retval;
}
//If another make_X_list function is needed extract the core code
//out into a macro

sl_obj make_list1(sl_obj car){
  return make_cons(car, NULL);
}
sl_obj make_list2(sl_obj car, sl_obj caar){
  return make_cons(car, make_cons(caar, NULL));
}
