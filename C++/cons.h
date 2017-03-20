/*
  File to show examples of how to do some things in C++
*/
#include <stdint.h>
#include <stdlib.h>
static const void *NIL = NULL;
struct cons {
  void *car;
  void *cdr;
  //simple constructor, initializes car,cdr with the given arguments
  //think of the list after the parentheses like the type declarations
  //in a k&r c function, they initialize things, then the function body in run.
  cons(void *a, void *d) : car(a),//sets car to a
                           cdr(d)//sets cdr to d
  {} //empty function body
  //template constructor, initializes car and cdr with arguments of given types
  //by bitcasting them to void*s
  template<typename T1,typename T2>
    cons(T1 a,T2 d) : car(NIL), cdr(NIL) {
      //set the car field of this new cons object to a, a is converted
      //to a void*, without changing the underlying bits
      this->car = reinterpret_cast<void*>(a);
      this->cdr = reinterpret_cast<void*>(d);//ditto for cdr,d
    }
  //nonstatic template member functions to extract an object of a specific type
  //actual type is T car(cons *this); called like cons_obj->car<T>(),
  //with the cons_obj passed as the first argument
  template<typename T>
  T xcar(){return reinterpret_cast<T>(this->car);}
  template<typename T>
  T xcdr(){return reinterpret_cast<T>(this->cdr);}
  //static member function, called like cons->list2(args)
  static cons* list1(void * arg){
    cons *retval = new cons(arg, NIL);
    return retval;
  }
  static cons* list2(void *arg1, void *arg2){
    cons *retval = new cons(arg1, new cons(arg2, NIL));
    return retval;
  }
};


/* Local Variables: */
/* mode: c++ */
/* End: */
