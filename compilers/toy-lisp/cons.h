#ifndef _MY_LISTS_H_
#define _MY_LISTS_H_
#include "common.h"
//extern Cons NIL;
//#define NIL_REF &NIL
//lisp->ml synonyms
#define hd car
#define tl cdr
typedef struct List List;
struct List{
  Cons* first;
  Cons* last;
  Cons* cur_loc;
  int len;
};
Cons* cdr(Cons* ls);
datatype car(Cons ls);
datatype Car(Cons* ls);
Cons first(Cons* ls);
Cons last(Cons* ls);
Cons cons(Cons l,Cons ls);
Cons* list(datatype head,...);
int length(Cons* ls);
Cons* mapcar(datatype(*map_fn)(datatype),Cons* ls);
Cons nreconc(Cons* ls,...);
Cons pop_fxn(Cons** ls);
datatype pop(Cons* ls);
Cons* push(Cons* ls,datatype l);
Cons* nrev(Cons* ls);
#define pop_cons(ls) pop_fxn(&ls)
#endif //_MY_LISTS_H_
