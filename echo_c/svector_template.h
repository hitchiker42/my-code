/*
  Defines a typed svector named TYPE_svector, where TYPE is a macro defined to the
  name of the type of the elements of the svector.
*/
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#ifndef CAT
#define CAT(a,b) PRIMITIVE_CAT(a, b)
#define PRIMITIVE_CAT(a,b) a ## b
#endif

#ifdef svector
#undef svector
#endif
#define svector CAT(TYPE, _svector)

typedef struct svector svector;
struct svector {
  union {
    TYPE *data;
    uint8_t *bytes;
  };
  int len;
  int size;
};
/*
  Ensure svector vec can hold sz more elements.
  Used to go into an infinite loop for a svector with size 0, but I fixed that

  This doesn't check the return value of realloc.
*/
#define svector_check_size(svec,sz)                                     \
  ({struct svector *tmp = svec;                                         \
    int needed = tmp->len + sz;                                         \
    while(tmp->size <= needed){                                         \
      tmp->size = (tmp->size >= 1 ? tmp->size*2 : 2);                   \
      tmp->data = realloc(tmp->data, tmp->size*sizeof(TYPE));           \
    };})

/*
  Functions come in two versions, lowercase are functions and take pointers to
  svectors, uppercase are macros and take literal svectors. Functions can't take
  literal svectors since they often need to be modified.
*/
#define svector_data(x) (x->data)
#define SVECTOR_DATA(x) (x.data)
#define svector_len(x) (x->len)
#define SVECTOR_LEN(x) (x.len)
#define SVECTOR_POP(vec)                        \
  (vec.data[--vec.len])
static inline TYPE svector_pop(struct svector *vec){
  return vec->data[--vec->len];
}
#define SVECTOR_PUSH(elt, vec)                  \
  svector_check_size(&vec, 1);                  \
  vec.data[vec.len++] = elt
static inline void svector_push(TYPE elt, struct svector* vec){
  svector_check_size(vec,1);
  vec->data[vec->len++] = elt;
}
#define SVECTOR_MULTIPUSH(elts, nelts, vec)                     \
  svector_check_size(&vec, nelts);                              \
  memcpy(vec.data + vec.len, elts, nelts*sizeof(TYPE));         \
  vec.len += nelts
static inline void svector_multipush(TYPE *elts, size_t len, struct svector *vec){
  svector_check_size(vec, len);
  memcpy(vec->data + vec->len, elts, len*sizeof(TYPE));
  vec->len += len;
}
  
//is an lvalue
#define SVECTOR_REF(vec, idx)                   \
  (vec.data[idx])
static inline TYPE svector_ref(const struct svector *vec, int idx){
  return vec->data[idx];
}
#define SVECTOR_SET(vec, idx, elt)              \
  (vec.data[idx] = elt)
static inline void svector_set(struct svector *vec, int idx, TYPE elt){
  vec->data[idx] = elt;
}
#define SVECTOR_SWAP(vec, i, j)                 \
  __extension__                                 \
  ({__typeof(vec.data[i]) __temp = vec.data[i]; \
    vec.data[i] = vec.data[j];                  \
    vec.data[j] = __temp;})
static inline void svector_swap(struct svector *vec, int i, int j){
  TYPE temp = vec->data[i];
  vec->data[i] = vec->data[j];
  vec->data[j] = temp;
}
struct svector make_svector(int size);
struct svector copy_svector(const struct svector *svec);
struct svector init_svector(int size, int len, const TYPE data);
struct svector svector_reverse(const struct svector *svec);
struct svector svector_reverse_inplace(struct svector *svec);
int svector_find(const struct svector *svec, TYPE elt);
int svector_search(const struct svector *svec, int(*test)(TYPE));
//search with user provided data passed as second argument
int svector_search2(const struct svector *svec,
                    int(*test)(TYPE,void*), void *data);
struct svector svector_sort(struct svector *svec, int(*cmp)(TYPE, TYPE),
                            int stable, int inplace);
/*struct svector svector_sort_inplace(struct svector *svec, 
                                    int(*cmp)(void*,void*));*/
//these are less useful than they would be in a langage with lambdas
TYPE svector_reduce(const struct svector *vec, TYPE(*f)(TYPE, TYPE));
struct svector svector_map(const struct svector *vec, TYPE(*f)(TYPE));
struct svector svector_map_inplace(struct svector *vec, TYPE(*f)(TYPE));
//apply f to each element of vec for side effects only
void svector_mapc(const struct svector *vec, void(*f)(TYPE));

#undef svector
