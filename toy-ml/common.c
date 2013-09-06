#ifndef __COMMON_H__
#define __COMMON_H__
#include <stdio.h>
#include <stdlib.h>
#include <uthash.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <gc/gc.h>
#define NIL _nil//not sure what nil should be
typedef enum types types;
typedef union data data;
typedef struct datatype datatype;
typedef struct cons cons;
enum types {
//list of possible types, keep it simple for now
  _nil = 0,
  _real = 1,
  _word = 2,
  _cons = 3,
  _ref = 4,
  _bool = 5,
  _NONE = 6};
enum boolean {true,false};
union data{
  double real;
  unsigned long word;
  cons* list;
  data* ref;
  enum boolean bool;
//  void* NIL;
};
struct datatype {
  types tag;
  data value;
};
struct cons{
  datatype value;
  cons* next;
};
struct option{
  enum {SOME,NONE} state;
  datatype value;
};
static const datatype nil = {_nil,0};
//I wrote things using xmalloc, so just make it a call to GC_MALLOC
extern inline void* xmalloc(size_t size){GC_MALLOC(size);}
#endif

