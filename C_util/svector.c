#include "svector.h"
#include <signal.h>
#include <stdio.h>
#ifndef XMALLOC
#define XMALLOC
static inline void* xmalloc(size_t sz){
  void *mem = malloc(sz);
  if(!mem && sz != 0){
    fputs("Out of memory\n",stderr);
    raise(SIGABRT);
  }
  return mem;
}
#endif
typedef struct svector svector;
inline svector make_svector(int size){
  svector ret = {.size = (size || 10), .len = 0};
  ret.data = xmalloc(size*sizeof(void*));
  return ret;
}
inline svector copy_svector(const svector *svec){
  svector ret;
  ret.len = svec->len;
  ret.size = svec->size;
  ret.data = xmalloc(ret.size * sizeof(void*));
  ret.data = memcpy(ret.data, svec->data, ret.size*sizeof(void*));
  return ret;
}
svector init_svector(int size, int len, const void *data){
  svector ret = make_svector(size);
  ret.len = len;
  memcpy(ret.data, data, len*sizeof(void*));
  return ret;
}

svector svector_reverse(const svector *svec){
  svector ret = make_svector(svec->size);
  ret.len = svec->len;
  int i;
  for(i=0;i<svec->len;i++){
    ret.data[i] = svec->data[(svec->len-1)-i];
  }
  return ret;
}
svector svector_reverse_inplace(svector *svec){
  int i;
  for(i=0;i<svec->len/2;i++){
    svector_swap(svec, i, (svec->len-1)-i);
  }
  return *svec;
}
int svector_find(const struct svector *svec, void *elt){
  int i;
  for(i=0;i<svec->len;i++){
    if(svec->data[i] == elt){
      return i;
    }
  }
  return -1;
}
int svector_search(const struct svector *svec, int(*test)(void*)){
  int i;
  for(i=0;i<svec->len;i++){
    if(test(svec->data[i]) > 0){
      return i;
    }
  }
  return -1;
}
int svector_search2(const struct svector *svec,
                    int(*test)(void*,void*), void *data){
  int i;
  for(i=0;i<svec->len;i++){
    if(test(svec->data[i], data) > 0){
      return i;
    }
  }
  return -1;
}
void* svector_reduce(const svector *vec, void*(*f)(void*,void*)){
  int i;
  void *acc = vec->data[0];
  for(i=1;i<vec->len;i++){
    acc = f(acc, vec->data[i]);
  }
  return acc;
}
static inline svector svector_map_internal(const svector *vec,
                                           svector dest, void*(*f)(void*)){
  int i;
  for(i=0;i<vec->len;i++){
    dest.data[i] = f(vec->data[i]);
  }
  return dest;
}
svector svector_map(const svector *vec, void*(*f)(void*)){
  svector new_vec = make_svector(vec->size);
  new_vec.len = vec->len;
  return svector_map_internal(vec, new_vec, f);
}
svector svector_map_inplace(svector *vec, void*(*f)(void*)){
  return svector_map_internal(vec, *vec, f);
}
void svector_mapc(const svector *vec, void(*f)(void*)){
  int i;
  for(i=0;i<vec->len;i++){
    f(vec->data[i]);
  }
}
#ifdef _C_UTIL_H_
svector svector_sort(svector *svec, int(*cmp)(void*,void*),
                     int stable, int inplace){
  svector ret = *svec;
  if(!inplace){
    ret = copy_svector(svec);
  }
  void **arr = ret.data;
  int len = ret.len;
  if(svec->len < 64){
    insertion_sort_generic(arr, len, cmp);
  } else if(stable){
    mergesort_generic(arr, len, cmp);
  } else {
    qsort_generic(arr, len, cmp);
  }
  return ret;
}
#endif
