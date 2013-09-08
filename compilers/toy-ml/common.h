#ifndef __COMMON_H__
#define __COMMON_H__
//includes
#include <stdio.h>
#include <stdlib.h>
#include <uthash.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <gc/gc.h>
#incldue "list.h"
#include "option.h"
#include "ref.h"
//#defines
#define NIL _nil//not sure what nil should be
//type declarations
typedef enum type type;
typedef union data data;
typedef struct datatype datatype;
typedef struct cons cons;
typedef struct symref symref;
typedef struct function function
typedef datatype*(*func_t)(datatype*)
//type definitions
//general rule, limit structs passed by value to 3*64 bits
enum type {
//list of possible types, keep it simple for now(this is simple?)
  _nil = 0,_real = 1,_word = 2,
  _cons = 3,_ref = 4,_bool = 5,
  _fun = 6,_tuple = 7,_opt = 8};

enum boolean {true,false};
struct tuple{
  int length;
  datatype* vals;
}
union data{
  double real;
  unsigned long word;
  cons* list;
  datatype* ref;
  function* fun;
  enum boolean bool;
//  void* NIL;
};
struct function {
  int argc;
  types type[argc];
  func_t fun;
}
struct datatype {
  type tag;
  data val;
};
struct cons{
  datatype val;
  cons* next;
};
struct option{
  enum {SOME,NONE} state;
  datatype val;
};
struct symref{
  char* name;
  datatype val;
  UT_hash_handle hh;
}
//global functions/values
//I wrote things using xmalloc, so just make it a call to GC_MALLOC
extern inline void* xmalloc(size_t size){GC_MALLOC(size);}
extern const datatype nil = {_nil,0};
extern symref* symbolTable = NULL;
extern inline void addSym(symref* Var){
  HASH_ADD_KEYPTR(hh, symbolTable, Var->name, strlen(Var->name), Var);
}
extern inline void getSym(const char* name,symref* Var){
  HASH_FIND_STR(symbolTable,name,Var);
}
char* get_ctype(type ml_type);
char* toString_type(type ml_type);
#endif

