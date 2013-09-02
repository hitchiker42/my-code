#ifndef DYNARRAY_H_
#define DYNARRAY_H_
#include <stdlib.h>
#include <stdio.h>
#include "xmalloc.h"
typedef struct dynarray{
  unsigned char* data;//raw data
  int size;//size of the array, in bytes
  int elemsize;//size of each element, in bytes
} dynarray;
//set the value of name[index] to val, where index is the number of elemntes
//of size name.elemsize
#define dynarray_update(name,index,val)                                 \
  if (name.size <= (index*name.elemsize){                               \
    name.size *= 1.5;                                                   \
    xrealloc(name.array,(name.size*name.elemsize));}                    \
  name.data[index*name.elemsize]=val
//return the value of type type, using index as number of elements
//of size sizeof(type)
#define dynarray_sub(arr,index,type)            \
  (type)(*(arr.data + index*arr.elemsize))
//create a new dynamic array of type type and size initsize
#define dynarray_ofType(type,name,initsize)     \
  {xmalloc(initsize*sizeof(type)),initsize,sizeof(type)}
#endif
