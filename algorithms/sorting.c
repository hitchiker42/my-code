#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#define NEED_XMALLOC
#include "C_util.h"
#define get_byte(val, byte)                     \
  (((val) >> ((byte)*CHAR_BIT)) & 0xff)
#define SWAP(x,y)                                    \
  ({__typeof(x) __temp = x;                          \
    x = y;                                           \
    y = __temp;})
//should return true if the 2 arguments are in the correct order
//and false otherwised
typedef int(*cmp_fun)(void*,void*);
void print_arr(ulong *arr, int len, FILE *out);
int cmp_gt(void *x, void *y){
  return ((uintptr_t)x) > ((uintptr_t) y);
}
int cmp_lt(void *x, void *y){
  return ((uintptr_t)x) < ((uintptr_t) y);
}
int is_sorted_generic(void **input, size_t len, cmp_fun cmp){
  int i;
  for(i=0;i<len-1;i++){
    if(!(cmp(input[i],input[i+1]))){
      return 0;
    }
  }
  return 1;
}
int is_sorted(uint64_t *input, size_t len){
  int i;
  for(i=0;i<len-1;i++){
    if(input[i] > input[i+1]){
      return 0;
    }
  }
  return 1;
}
void insertion_sort_generic(void **input, size_t len, cmp_fun cmp){
  int i,j;
  for(i=1;i<len;i++){
    void *temp = input[i];
    j=i;
    while(j>0 && !cmp(input[j-1],temp)){
      input[j] = input[j-1];
      j--;
    }
    input[j] = temp;
  }
}
#define MEDIAN(_x,_y,_z)                        \
  ({__typeof(_x) x = _x;                        \
    __typeof(_y) y = _y;                        \
    __typeof(_z) z = _z;                        \
    __typeof(_x) lg = MAX(x,y);                 \
    __typeof(_y) sm = MIN(x,y);                 \
    z > lg ? lg : (z < sm ? sm : z);})
/*
  Similar to the algorithm used by glibc, but glibc uses an explict stack
  while we use recursion, since I'm lazy.
*/
static inline int qsort_partition(void **arr, int left, int right,
                                  cmp_fun cmp){
  //int pivot_idx = MEDIAN(left, right, (left+right)/2);
  int pivot_idx = left;
  void* pivot = arr[pivot_idx];
  int i = left - 1, j = right + 1;
  do {
    do {
      i++;
    } while(cmp(arr[i], pivot));
    do {
      j--;
    } while(cmp(pivot, arr[j]));
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
void qsort_generic(void **arr, int len, cmp_fun cmp){
  if(len < QSORT_MIN_LENGTH){
    //putting a check for non-zero len here might speed things up
    insertion_sort_generic(arr, len, cmp);
  } else {
    int pivot_idx = qsort_partition(arr, 0, len-1, cmp);
    qsort_generic(arr, pivot_idx+1, cmp);
    qsort_generic(arr+(pivot_idx+1), len - (pivot_idx+1), cmp);
  }
  return;
}
/*void mergesort_internal(void **arr, void **tmp, size_t len,
                               cmp_fun cmp){
  if(len <= 1){
    return;
  }
  int n1, n2;
  void **A, **B;
  n1 = len/2;
  n2 = len-n1;
  A = arr;
  B = arr + n1;
  mergesort_internal(A, tmp, n1, cmp);
  mergesort_internal(B, tmp+n1, n2, cmp);
  while(n1 > 0 && n2 > 0){
    if(!cmp(*A,*B)){
      *tmp++ = *B;
      B++;
      n2--;
    } else {
      *tmp++ = *A;
      A++;
      n1--;
    }
  }
  if(n1 > 0){
    memcpy(tmp, A, n1);
  }
  if(n2 > 0){
    memcpy(tmp, B, n2);
  }
  memcpy(arr, tmp, len-n2);
}*/
void merge(void **A, void **B, void **tmp, 
           size_t n1, size_t n2, cmp_fun cmp){
  while(n1 > 0 & n2 > 0){
    if(cmp(*A,*B)){
      *tmp++ = *A++;
      n1--;
    } else {
      *tmp++ = *B++;
      n2--;
    }
  }
  if(n1 > 0){
    memcpy(tmp, A, n1*sizeof(void*));
  }
  if(n2 > 0){
    memcpy(tmp, B, n2*sizeof(void*));
  }
}
void mergesort_internal(void **arr, void **tmp, 
                        size_t len, cmp_fun cmp){
  if(len <= 1){
    return;
  }
  size_t mid = len/2;
//  size_t n2 = len - n1;
//  void **A = arr;
//  void **B = arr + n1;
  mergesort_internal(arr, tmp, mid, cmp);
  mergesort_internal(arr + mid, tmp, len - mid, cmp);
  merge(arr, arr + mid, tmp, mid, len - mid, cmp);
  memcpy(arr, tmp, len*sizeof(void*));
}
void mergesort_generic(void **arr, size_t len, cmp_fun cmp){
  //alloca would be faster, but limits len to 8M/sizeof(void*)
  //and I'm too lazy to test the length
  void **tmp = zmalloc(len*sizeof(void*));
  mergesort_internal(arr, tmp, len, cmp);
  free(tmp);
}
uint64_t* qsort_u64(uint64_t *input, size_t len){
  qsort_generic((void**)input, len, cmp_lt);
  return input;
}
uint64_t* insertion_sort_u64(uint64_t *input, size_t len){
  insertion_sort_generic((void**)input, len, cmp_lt);
  return input;
}
uint64_t* mergesort_u64(uint64_t *input, size_t len){
  mergesort_generic((void**)input, len, cmp_lt);
  return input;
}
/*
  Usually works, but sometimes doesn't, but if it does fail
  it's only by 1. I'm pretty sure what's happening is I'm
  sometimes getting indices equal to -1 or sz+1 somehow.
*/
uint64_t *radix_sort_u64(uint64_t *in, size_t sz){
  int64_t hist[8][0xff];
  int64_t total = 0, sum[8];
  size_t i, j;
  memset(sum, '\0', sizeof(sum));
  memset((uint64_t*)hist, '\0', sizeof(hist));
  for(i=0;i<sz;i++){
    hist[0][get_byte(in[i], 0)]++;
    hist[1][get_byte(in[i], 1)]++;
    hist[2][get_byte(in[i], 2)]++;
    hist[3][get_byte(in[i], 3)]++;
    hist[4][get_byte(in[i], 4)]++;
    hist[5][get_byte(in[i], 5)]++;
    hist[6][get_byte(in[i], 6)]++;
    hist[7][get_byte(in[i], 7)]++;
  }

  for(i=0;i<0xff;i++){
    for(j=0;j<8;j++){
      total = hist[j][i] + sum[j];
      hist[j][i] = sum[j];
      sum[j] = total;
    }
  }
  uint64_t *temp = zmalloc(sz*sizeof(uint64_t));
  uint64_t *a = in, *b = temp;
  size_t pos = 0;
  for(j=0;j<sz;j++){
    pos = get_byte(a[j], 0);
    b[hist[0][pos]++] = a[j];
  }
  SWAP(a, b);
  for(j=0;j<sz;j++){
    pos = get_byte(a[j], 1);
    b[hist[1][pos]++] = a[j];
  }
  SWAP(a, b);
  for(j=0;j<sz;j++){
    pos = get_byte(a[j], 2);
    b[hist[2][pos]++] = a[j];
  }
  SWAP(a, b);
  for(j=0;j<sz;j++){
    pos = get_byte(a[j], 3);
    b[hist[3][pos]++] = a[j];
  }
  SWAP(a, b);
  for(j=0;j<sz;j++){
    pos = get_byte(a[j], 4);
    b[hist[4][pos]++] = a[j];
  }
  SWAP(a, b);
  for(j=0;j<sz;j++){
    pos = get_byte(a[j], 5);
    b[hist[5][pos]++] = a[j];
  }
  SWAP(a, b);
  for(j=0;j<sz;j++){
    pos = get_byte(a[j], 6);
    b[hist[6][pos]++] = a[j];
  }
  SWAP(a, b);
  for(j=0;j<sz;j++){
    pos = get_byte(a[j], 7);
    b[hist[7][pos]++] = a[j];
  }
  SWAP(a, b);
  free(temp);
  return in;
}
/*
  in,out:out,in:in,out:out,in:in,out:out,in:in,out:out,in
   */
int strtoul_checked(char *str, ulong *ret, char **endptr){
  errno = 0;
  ulong temp = strtoul(str, endptr, 0);
  if(errno != 0){
    return errno;
  } else {
    *ret = temp;
    return 0;
  }
}
ulong *read_arr(int len, char *str){
  ulong *arr = zmalloc(len*sizeof(unsigned long));
  char *strptr = str;
  int i;
  for(i=0;i<len;i++){
    errno = 0;
    arr[i] = strtoul(strptr, &strptr, 0);
    if(errno != 0){
      goto fail;
    }    
  }
  return arr;
 fail:
  free(arr);
  return NULL;
}
void print_arr(ulong *arr, int len, FILE *out){
  //there are faster ways to do this, but who cares
  int i;
  //print the first element w/out space, then the rest with a leading space
  fprintf(out,"%lu", arr[0]);
  for(i=1;i<len;i++){
    fprintf(out," %lu", arr[i]);
  }
  fputs("\n", out);
  return;
}
enum sort_method {
  SORT_QUICK,
  SORT_MERGE,
  SORT_INSERTION,
  SORT_RADIX
};
//is a a prefix of b
static int is_prefix(char *a, char *b){
  int a_len = strlen(a);
  int b_len = strlen(b);
  if((a_len > b_len) || a_len == 0){
    return 0;
  } else {
    return !memcmp(a,b,a_len);
  }
}
int main(int argc, char *argv[]){
  if(argc < 3){
    printf("Usage: ./sort method array_length array\n");
    exit(1);
  }
  int method = SORT_QUICK;
  if(argc == 4){
    if(is_prefix(argv[1], "quick")){
      method = SORT_QUICK;
    } else if(is_prefix(argv[1], "merge")){
      method = SORT_MERGE;
    } else if(is_prefix(argv[1], "insertion")){
      method = SORT_INSERTION;
    } else if(is_prefix(argv[1], "radix")){
      method = SORT_RADIX;
    } else {
      fprintf(stderr, "Uknown sorting method %s, using default\n", argv[1]);
    }
    argv++;
  }
  ulong len;
  int err = strtoul_checked(argv[1], &len, NULL);
  if(err){
    perror("strtoul");
    exit(1);
  }
  ulong *arr = read_arr(len, argv[2]);
  if(!arr){
    perror("strtoul");
    exit(1);
  }
  //sort stuff
  switch(method){
    case(SORT_QUICK):      
      qsort_u64(arr,len); break;
    case(SORT_MERGE):
      mergesort_u64(arr,len);break;
    case(SORT_INSERTION):
      insertion_sort_u64(arr,len);break;
    case(SORT_RADIX):
      radix_sort_u64(arr,len);break;
  }
  if(is_sorted(arr,len)){
    printf("Array was sorted\n");
  } else {
    printf("Failed to sort array\n");
  }
  print_arr(arr, len, stdout);
  free(arr);
  return 0;
}
