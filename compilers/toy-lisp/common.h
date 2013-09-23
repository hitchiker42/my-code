#ifndef _LISP_COMMON_H_
#define _LISP_COMMON_H_
#include <stdio.h>
#include <stdlib.h>
#include <gc.h>
#include <uthash.h>
#include <signal.h>
#include <string.h>
#include <stdarg.h>
#define HERE() fprintf(stderr,"here at %s,line %d\n",__FILE__,__LINE__)
typedef struct symref symref;
typedef enum type type;
typedef union data data;
typedef struct datatype datatype;
typedef struct Cons Cons;
typedef datatype(*func_t)(datatype);
typedef enum{
  _lparen='(',
  _rparen=')',
  _quote='\'',
  _num='n',
  _defun='f',
  _id='s',
  _if='i',
  _def='d',
  _do='l',//l is for loop
  _err='?',
  _eof='e'
} token;
enum type{
  _nil = -1,
  _double = 0,
  _fun = 1,
  _sym = 2,
  _cons = 3,
  _str = 4,
  _special = 5
};
union data {
  double real;
  symref* sym;
  func_t fun;
  Cons* ls;
  char* string;
  char special;
};
struct datatype{
  type tag;
  data val;
};
struct symref{
  char* name;
  datatype val;
  UT_hash_handle hh;
};
struct Cons{
  datatype car;
  Cons* cdr;
};
extern symref* symbolTable;
extern datatype *yylval;
static const datatype nil={_nil,0};
static const Cons NIL={{_nil,0},NULL};
static const Cons* NIL_REF = &NIL;
static inline void addSym(symref* Var){
  HASH_ADD_KEYPTR(hh, symbolTable, Var->name, strlen(Var->name), Var);
}
static inline void getSym(const char* name,symref* Var){
  HASH_FIND_STR(symbolTable,name,Var);
}
static inline void* xmalloc(size_t size){
  register void* temp = GC_MALLOC(size);
  if(!temp && size){
    fprintf(stderr,"virtual memory exhausted\n");
    raise(SIGSEGV);
  } else{
    return temp;
  }
}
static inline void* xrealloc(void* mem,size_t size){
  register void* temp = GC_REALLOC(mem,size);
  if(!temp && size){
    fprintf(stderr,"virtual memory exhausted\n");
    raise(SIGSEGV);
  } else{
    return temp;
  }
}
static inline void* xmalloc_atomic(size_t size){
  register void* temp = GC_MALLOC_ATOMIC(size);
  if(!temp && size){
    fprintf(stderr,"virtual memory exhausted\n");
    raise(SIGSEGV);
  } else{
    return temp;
  }
}
#endif
