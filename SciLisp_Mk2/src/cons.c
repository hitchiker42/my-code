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
DEFUN("cons",cons,2,2, "create a new cons cell with `car` and `cdr` as the\n"
      "car and cdr respectively")
  (sl_obj car, sl_obj cdr){
  return make_cons(car,cdr);
}


DEFUN("caar",caar,1,1,"return the caar of the given cons cell")
  (sl_obj obj){
  return car(car(obj));
}
DEFUN("cadr",cadr,1,1,"return the cadr of the given cons cell")
  (sl_obj obj){
  return car(cdr(obj));
}
DEFUN("cdar",cdar,1,1,"return the cdar of the given cons cell")
  (sl_obj obj){
  return cdr(car(obj));
}
DEFUN("cddr",cddr,1,1,"return the cddr of the given cons cell")
  (sl_obj obj){
  return cdr(cdr(obj));
}
DEFUN("caaar",caaar,1,1,"return the caaar of the given cons cell")
  (sl_obj obj){
  return car(car(car(obj)));
}
DEFUN("caadr",caadr,1,1,"return the caadr of the given cons cell")
  (sl_obj obj){
  return car(car(cdr(obj)));
}
DEFUN("cadar",cadar,1,1,"return the cadar of the given cons cell")
  (sl_obj obj){
  return car(cdr(car(obj)));
}
DEFUN("caddr",caddr,1,1,"return the caddr of the given cons cell")
  (sl_obj obj){
  return car(cdr(cdr(obj)));
}
DEFUN("cdaar",cdaar,1,1,"return the cdaar of the given cons cell")
  (sl_obj obj){
  return cdr(car(car(obj)));
}
DEFUN("cdadr",cdadr,1,1,"return the cdadr of the given cons cell")
  (sl_obj obj){
  return cdr(car(cdr(obj)));
}
DEFUN("cddar",cddar,1,1,"return the cddar of the given cons cell")
  (sl_obj obj){
  return cdr(cdr(car(obj)));
}
DEFUN("cdddr",cdddr,1,1,"return the cdddr of the given cons cell")
  (sl_obj obj){
  return cdr(cdr(cdr(obj)));
}
DEFUN("caaaar",caaaar,1,1,"return the caaaar of the given cons cell")
  (sl_obj obj){
  return car(car(car(car(obj))));
}
DEFUN("caaadr",caaadr,1,1,"return the caaadr of the given cons cell")
  (sl_obj obj){
  return car(car(car(cdr(obj))));
}
DEFUN("caadar",caadar,1,1,"return the caadar of the given cons cell")
  (sl_obj obj){
  return car(car(cdr(car(obj))));
}
DEFUN("caaddr",caaddr,1,1,"return the caaddr of the given cons cell")
  (sl_obj obj){
  return car(car(cdr(cdr(obj))));
}
DEFUN("cadaar",cadaar,1,1,"return the cadaar of the given cons cell")
  (sl_obj obj){
  return car(cdr(car(car(obj))));
}
DEFUN("cadadr",cadadr,1,1,"return the cadadr of the given cons cell")
  (sl_obj obj){
  return car(cdr(car(cdr(obj))));
}
DEFUN("caddar",caddar,1,1,"return the caddar of the given cons cell")
  (sl_obj obj){
  return car(cdr(cdr(car(obj))));
}
DEFUN("cadddr",cadddr,1,1,"return the cadddr of the given cons cell")
  (sl_obj obj){
  return car(cdr(cdr(cdr(obj))));
}
DEFUN("cdaaar",cdaaar,1,1,"return the cdaaar of the given cons cell")
  (sl_obj obj){
  return cdr(car(car(car(obj))));
}
DEFUN("cdaadr",cdaadr,1,1,"return the cdaadr of the given cons cell")
  (sl_obj obj){
  return cdr(car(car(cdr(obj))));
}
DEFUN("cdadar",cdadar,1,1,"return the cdadar of the given cons cell")
  (sl_obj obj){
  return cdr(car(cdr(car(obj))));
}
DEFUN("cdaddr",cdaddr,1,1,"return the cdaddr of the given cons cell")
  (sl_obj obj){
  return cdr(car(cdr(cdr(obj))));
}
DEFUN("cddaar",cddaar,1,1,"return the cddaar of the given cons cell")
  (sl_obj obj){
  return cdr(cdr(car(car(obj))));
}
DEFUN("cddadr",cddadr,1,1,"return the cddadr of the given cons cell")
  (sl_obj obj){
  return cdr(cdr(car(cdr(obj))));
}
DEFUN("cdddar",cdddar,1,1,"return the cdddar of the given cons cell")
  (sl_obj obj){
  return cdr(cdr(cdr(car(obj))));
}
DEFUN("cddddr",cddddr,1,1,"return the cddddr of the given cons cell")
  (sl_obj obj){
  return cdr(cdr(cdr(cdr(obj))));
}
