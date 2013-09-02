#include <stdlib.h>
#include <stdio.h>
#ifndef DYNARRAY_H_
#define DYNARRAY_H_
#define fatal(message)                          \
  fprintf(stderr,"Fatal Error:" message "\n");  \
  abort()
/*#define mkdynarray_type(type,name)            \
  typedef struct {                              \
  type* data;                                  \
  int size;                                     \
  int elemsize;                                 \
  } name*/
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
//#endif
//or more simply
typedef struct{
  unsigned char* data;
  unsigned int len;
  unsigned int elemsize;
} dynarray;
dynarray mk_dynarray(int initsize,int elemsize){
  initsize = (initsize == 0 ? 20 : initsize);
  unsigned char* temp=xmalloc(initsize*elemsize);
  dynarray retval = {temp,initsize,elemsize};
}
inline void* dynarray_sub(dynarray arr,int index){
  return (arr.data + index*arr.elemsize);
}
inline void dynarray_update(dynarray arr,int index,unsigned char* value){
  if (index >= arr.len){
    arr.len*=1.5;
    arr.data=xrealloc(arr.len);
  }
  *(arr.data+index*arr.elemsize)=&value;
}
#define dynarray_ofType(type,name,initsize)     \
  {xmalloc(initsize*sizeof(type)),initsize,sizeof(type)}
#endif
