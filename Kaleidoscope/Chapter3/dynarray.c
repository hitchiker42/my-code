#include <stdlib.h>
#include <stdio.h>
#ifndef DYNARRAY_H_
#define DYNARRAY_H_
#define fatal(message)                          \
  fprintf(stderr,"Fatal Error:" message "\n");  \
  abort()
inline void* xmalloc (size_t size){
  register void* value = malloc(size);
  if (value == 0){
    fatal("virtual memory exhausted");
  }
  return value;
}
inline void* xrealloc (void* ptr, size_t size){
  register void* value = realloc(ptr,size);
  if (value == 0){
    fatal ("Virtual memory exhausted");
  }
  return value;
}
typedef struct dynarray_common{
  int size;
  int threshold;
  int step;
  int elemsize;
} dynarray_common;
typedef struct dynarray{
  void* array;
  dynarray_common* metadata;
} dynarray;
#define mkdynarray_type(type,name)                   \
  typedef struct {                              \
  type* array;                                  \
  dynarray_common* metadata;                    \
  } name
#define dynarray_ref(name,index)                     \
  if (name.metadata->size-name.metadata->threshold <= index){    \
    name.metadata->size+=name.metadata->step;        \
    xrealloc(name.array,(name.metadata->size*name.metadata->elemsize));} \
  name.array[index]
dynarray* init_dynarray(int initsize,int threshold,int step,int element_size){
  if(initsize <= 0){
    initsize = 20;}
  if(threshold <=0){
    threshold=0;}
  if(step<=0){
    step = initsize/2;}
  dynarray_common* temp = xmalloc(sizeof(dynarray_common));
  temp->size=initsize,temp->threshold=threshold,temp->step=step;
  temp->elemsize=element_size;
  dynarray* retval = xmalloc(sizeof(dynarray));
  retval->array = xmalloc(element_size*initsize);
  retval->metadata=temp;
  return retval;}
      
#define mkmap_type(key_type,value_type)                   \
  typedef struct{                               \
  key_type key;                                 \
  value_type value;                             \
  } key_type##_##value_type##_map
#endif
