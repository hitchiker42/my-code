#include "C_util.h"
typedef struct svector svector;
inline svector make_svector(int size){
  svector ret = {.size = size, .len = 0};
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
/*
   Sorting
   MAKE SURE TO TEST THESE
*/
#define MEDIAN(_x,_y,_z)                        \
  ({__typeof(_x) x = _x;                        \
    __typeof(_y) y = _y;                        \
    __typeof(_z) z = _z;                        \
    __typeof(_x) lg = MAX(x,y);                 \
    __typeof(_y) sm = MIN(x,y);                 \
    z > lg ? lg : (z < sm ? sm : z);})
static inline void insertion_sort_internal(void** arr, int len,
                                           int(*cmp)(void*,void*)){
  int i,j;
  for(i=1;i<len;i++){
    void *val = arr[i];
    for(j=i;j>0;j--){
      if(cmp(arr[j-1], val) < 0){
        break;
      } else {
        SWAP(arr[j], arr[j-1]);
      }
    }
  }  
}
/*
  Similar to the algorithm used by glibc, but glibc uses an explict stack
  while we use recursion, since I'm lazy.
*/
static inline int qsort_partition(void **arr, int left, int right,
                                  int(*cmp)(void*,void*)){
  int pivot_idx = MEDIAN(left, right, (left+right)/2);
  void* pivot = arr[pivot_idx];
  int i = left - 1, j = right - 1;
  do {
    do {
      i++;
    } while(cmp(arr[i], pivot) < 0);
    do {
      j--;
    } while(cmp(pivot, arr[j]) < 0);
    if(i<j){
      SWAP(arr[i], arr[j]);
    } else {
      return j;
    }
  } while(i<=j);
  return j;
}
//this is the size used by glibc
#define QSORT_MIN_LENGTH 4
static inline void qsort_internal(void **arr, int len, int(*cmp)(void*,void*)){
  if(len < QSORT_MIN_LENGTH){
    //putting a check for non-zero len here might speed things up
    insertion_sort_internal(arr, len, cmp);
  } else {
    int pivot_idx = qsort_partition(arr, 0, len-1, cmp);
    qsort_internal(arr, pivot_idx+1, cmp);
    qsort_internal(arr+(pivot_idx+1), len - (pivot_idx+1), cmp);
  }
  return;
}
static inline void mergesort_internal(void **arr, void **tmp, int len,
                                      int(*cmp)(void*,void*)){
  if(len<=1){
    return;
  }
  int n1,n2;
  void **A, **B;
  n1 = len/2;
  n2 = len-n1;
  A = arr;
  B = arr + n1;
  mergesort_internal(A, tmp, n1, cmp);
  mergesort_internal(B, tmp, n2, cmp);
  while(n1 > 0 && n2 > 0){
    if(cmp(*A,*B) <= 0){
      *tmp++ = *A;
      A++;
      n1--;
    } else {
      *tmp++ = *B;
      B++;
      n2--;
    }
  }
  if(n1>0){
    memcpy(tmp, A, n1);
  }
  memcpy(arr, tmp, len-n2);
}

svector svector_sort(svector *svec, int(*cmp)(void*,void*),
                     int stable, int inplace){
  svector ret = *svec;
  if(!inplace){
    ret = copy_svector(svec);
  }
  void **arr = ret.data;
  int len = ret.len;
  if(svec->len < 64){
    insertion_sort_internal(arr, len, cmp);
  } else if(stable){
    //maybe change to malloc if len is big
    void **tmp = alloca(len * sizeof(void*));
    mergesort_internal(arr, tmp, len, cmp);
  } else {
    qsort_internal(arr, len, cmp);
  }
  return ret;
}
