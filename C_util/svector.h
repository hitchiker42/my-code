#ifndef _SVECTOR_H_
#define _SVECTOR_H_
#include <stdlib.h>
#include <string.h>
/*
  TODO: create a 'template' for a typed svector
*/
struct svector {
  void **data;
  int len;
  int size;
};
/*
  Ensure svector vec can hold sz more elements
*/
#define svector_check_size(svec,sz)                                     \
  ({struct svector *tmp = svec;                                         \
    int needed = tmp->len + sz;                                         \
    while(tmp->size <= needed){                                         \
      tmp->data = realloc(tmp->data, tmp->size*2*sizeof(void*));        \
      tmp->size*=2;                                                     \
    };})
#define svector_data(x) (x.data)
/*
  Right now all functions take pointers to svectors for the sake of uniformity
  but only the functions that modify their arguments actually need to, should
  this be changed?
*/
#define svector_len(x) (x.len)
struct svector make_svector(int size);
struct svector copy_svector(const struct svector *svec);
struct svector init_svector(int size, int len, const void* data);
#define SVECTOR_POP(vec)                               \
  (vec.data[--vec.len])
static inline void *svector_pop(struct svector *vec){
  return vec->data[--vec->len];
}
#define SVECTOR_PUSH(elt, vec)                                          \
  svector_check_size(&vec);                                             \
  vec.data[vec.len++] = elt
static inline void svector_push(void *elt, struct svector* vec){
  svector_check_size(vec,1);
  vec->data[vec->len++] = elt;
}
//is an lvalue
#define SVECTOR_REF(vec, idx)                                           \
  (vec.data[idx])
static inline void* svector_ref(const struct svector *vec, int idx){
  return vec->data[idx];
}
#define SVECTOR_SET(vec, idx, elt)                                      \
  (vec.data[idx] = elt)
static inline void svector_set(struct svector *vec, int idx, void *elt){
  vec->data[idx] = elt;
}
#define SVECTOR_SWAP(vec, i, j)                 \
  __extension__                                 \
  ({__typeof(vec.data[i]) __temp = vec.data[i]; \
    vec.data[i] = vec.data[j];                  \
    vec.data[j] = __temp;})
static inline void svector_swap(struct svector *vec, int i, int j){
  void *temp = vec->data[i];
  vec->data[i] = vec->data[j];
  vec->data[j] = temp;
}

struct svector svector_reverse(const struct svector *svec);
struct svector svector_reverse_inplace(struct svector *svec);
int svector_find(const struct svector *svec, void *elt);
int svector_search(const struct svector *svec, int(*test)(void*));
//search with user provided data passed as second argument
int svector_search2(const struct svector *svec,
                    int(*test)(void*,void*), void *data);
struct svector svector_sort(struct svector *svec, int(*cmp)(void*,void*),
                            int stable, int inplace);
/*struct svector svector_sort_inplace(struct svector *svec, 
                                    int(*cmp)(void*,void*));*/
//these are less useful than they would be in a langage with lambdas
void* svector_reduce(const struct svector *vec, void*(*f)(void*,void*));
struct svector svector_map(const struct svector *vec, void*(*f)(void*));
struct svector svector_map_inplace(struct svector *vec, void*(*f)(void*));
//apply f to each element of vec for side effects only
void svector_mapc(const struct svector *vec, void(*f)(void*));

#endif
