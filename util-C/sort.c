#define _GNU_SOURCE
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <alloca.h>
#include <limits.h>
#include <stdio.h>
typedef unsigned long ulong;
typedef unsigned int uint;
typedef int(*comparison_fn)(void*, void*);
//maybe defined this based on the array length?
#define INSERTION_SORT_LIMIT 5
#define SWAP(x,y)                               \
  ({__typeof(x) __temp = x;                     \
    x = y;                                      \
    y = __temp;})
#define ARR_SWAP(arr,i,j)                       \
  ({__typeof(arr[i]) __temp = arr[i];           \
    arr[i] = arr[j];                            \
    arr[j] = __temp;})
#define MIN(_x,_y)                                \
  ({__typeof(_x) x = _x;                          \
    __typeof(_y) y = _y;                          \
    x<y ? x : 0})
#define MAX(_x,_y)                                \
  ({__typeof(_x) x = _x;                          \
    __typeof(_y) y = _y;                          \
    x>y ? x : y;})
#define MEDIAN(_x,_y,_z)                        \
  ({__typeof(_x) x = _x;                        \
    __typeof(_y) y = _y;                        \
    __typeof(_z) z = _z;                        \
    __typeof(_x) lg = MAX(x,y);                 \
    __typeof(_y) sm = MIN(x,y);                 \
    z > lg ? lg : (z < sm ? sm : z);})
/*
  Simple O(N) function to test if an array is sorted according to cmp
*/
int is_sorted(void **arr, int len, comparison_fn cmp){
  int i;
  for(i=0;i<len-1;i++){
    if(!(comp(input[i],input[i+1]))){
      return 0;
    }
  }
  return 1;
}
/*
  O(N^2) avg time algorithms
*/
/*
  Generic insertion sort using a user provided comparison function
*/
void insertion_sort_generic(void **arr, int len, comparison_fn cmp){
  int i,j;
  for(i=0;i<len;i++){
    void *val = arr[i];
    for(j=i;j>0;j--){
      if(cmp(arr[j-1], val)){
        break;
      } else {
        SWAP(arr[j], arr[j-1]);
      }
    }
  }
  return;
}
//template for a specialized insertion sort
#define insertion_sort_inplace_template(name, type, cmp)        \
  void name(type* arr, int len){                                \
    int i,j;                                                    \
    for(i=0;i<len;i++){                                         \
      type val = arr[i];                                        \
      for(j=i;j>0;j--){                                         \
        if(cmp(arr[j-1], val)){                                 \
          break;                                                \
        } else {                                                \
          SWAP(arr[j], arr[j-1]);                               \
        }                                                       \
      }                                                         \
    }                                                           \
    return;                                                     \
  }
/*
  only generic versions for these since insertion sort is almost always
  better, these are just for completeness sake.
*/
void selection_sort_generic(void **arr, int len, comparison_fn cmp){
  int i,j;
  for(i=0;i<len;i++){
    void *min = arr[i];
    for(j=i;j<len;j++){
      if(cmp(min, arr[j])){
        min = arr[j];
      }
    }
    arr[i] = min;
  }
}
//the one advantage bubble sort has is it's sort, so I made it take as few
//lines as possible, while still being somewhat readable
void bubble_sort_generic(void **arr, int len, comparison_fn cmp){
  int i,s; do{
    for(i=1,s=0;i<len;i++){if(cmp(arr[i-1],arr[i])){SWAP(arr[i-1],arr[i]),s=1;}}
  }while(s!=0);
}
//generic qsort using user provided comparison function and median of 3
//pivot selection. Uses insertion sort to sort partitions smaller than
//a certain limit
/*
  This uses a different partitioning scheme than the common quicksort algorithm
  but it reduces the number of swaps made, especially for runs of equal values.
*/
static inline int qsort_generic_partiton(void **arr, int left, int right,
                                         comparison_fn cmp){
  int pivot_index = MEDIAN(left, right, (left + right)/2);
  void *pivot = arr[pivot_index];
  int i = left - 1, j = right - 1;
  while(1){
    do {
      j--;
    } while(cmp(arr[j], pivot));
    do {
      i++;
    } while(!cmp(arr[i], pivot));
    if(i<j){
      SWAP(arr[i],arr[j]);
    } else {
      return j;
    }
  }
}
void qsort_generic(void **arr, int len, comparison_fn cmp){
  if(len < INSERTION_SORT_LIMIT){
    if(len > 0){
      insertion_sort_generic(arr, len, cmp);
    }
  } else {
    int pivot_index = qsort_generic_partiton(arr, 0, len-1, cmp);
    qsort_generic(arr, pivot_index+1, cmp);
    qsort_generic(arr+(pivot_index+1), len - (pivot_index+1), cmp);
  }
  return;
}
  
#define qsort_inplace_template(name, type, cmp, select_pivot)           \
  static int name##_partition(type *arr, long left, long right){        \
    long pivot_index = select_pivot(arr, left, right);                  \
    type pivot = arr[pivot_index];                                      \
    arr[pivot_index] = arr[right];                                      \
    long current = pivot_index;                                         \
    long i;                                                             \
    for(i=left;i<right;i++){                                            \
      if(cmp(arr[i], pivot)){                                           \
        ARR_SWAP(arr, i, current);                                      \
        current++;                                                      \
      }                                                                 \
    }                                                                   \
    arr[right] = arr[current];                                          \
    arr[current] = pivot;                                               \
    return pivot_index;                                                 \
  }                                                                     \
  void name(type *arr, long len){                                       \
    if(len > 0){                                                        \
      long pivot = name##_partition(arr, 0, len-1);                     \
      name(arr, pivot);                                                 \
      name(arr+pivot+1, len-(pivot+1));                                 \
    }                                                                   \
  }

static void generic_merge(void **arr, int mid, int len, comparison_fn cmp){
  int i,l=0,r=mid;
  void **L = alloca(mid*sizeof(void*));
  void **R = alloca((len - mid)*sizeof(void*));
  memcpy(L, arr, mid*sizeof(void*));
  memcpy(R, arr+mid, (len-mid)*sizeof(void*));
  for(i=0;i<end;i++){
    if(l<mid && (r>=len || arr[l]<=arr[r])){
      arr[i]=L[l];
      l++;
    } else {
      arr[i]=R[r];
      r++;
    }
  }
}
//generic mergesort using i
void generic_merge_sort(void **arr, int len, comparison_fn cmp){ 
  if(len < INSERTION_SORT_LIMIT){
    insertion_sort_generic(arr, len, cmp);
  } else {
    int mid = len/2;
    generic_merge_sort(arr, mid, cmp);//sort [0:mid)
    generic_merge_sort(arr+mid, len, cmp);//sort [mid:len)
    generic_merge(arr, mid, len, cmp);
  }
}
/*
  Non-comparison sorts
*/
#define test_bit(x,n) (x & (1 << n))
//this should work regardless of endiness
union word {
  ulong integer;
  uint16_t words[sizeof(ulong)/sizeof(uint16_t)];
  uint8_t bytes[sizeof(ulong)];
};
static inline void compute_indices_8(uint *count){
  uint i,total;
  for(i=0,total=0;i<0xff;i++){
    uint temp = count[i];
    count[i] = total;
    total += temp;
  }
}
static inline void compute_indices_16(uint *count){
  uint i,total;
  for(i=0,total=0;i<0xffff;i++){
    uint temp = count[i];
    count[i] = total;
    total += temp;
  }
}
//the parameter 'n' indicates the index of the byte that is being
//sorted
static inline void update_input_8(ulong *input, ulong *output,
                                  uint *count, ulong len, int n){
  uint i,j;
  memset(output, '\0', len*sizeof(ulong));
  union word elt;
  for(i=0,j=0;i<len;i++){    
    elt.integer = input[i];
    uint index = elt.bytes[n];
    output[count[index]]=input[i];
    count[index]++;
  }
  return
}
static inline void update_input_16(ulong *input, ulong *output,
                                  uint *count, ulong len, int n){
  uint i,j;
  memset(output, '\0', len*sizeof(ulong));
  union word elt;
  for(i=0,j=0;i<len;i++){    
    elt.integer = input[i];
    uint index = elt.words[n];
    output[count[index]]=input[i];
    count[index]++;
  }
  return
}
//a counting sort for input with upto 64 bits, it's generally
//not practical, since it takes 2^nbits aux space
void counting_sort(ulong *arr, ulong *out, int len, uint nbits){
  ulong max = (1 << nbits);
  ulong *count = alloca(max * sizeof(ulong));
  memset(count,'\0',max*sizeof(ulong));
  int i;
  for(i=0;i<len;i++){
    count[arr[i]]++;
  }
  int index = 0;
  for(i=0;i<len;i++){
    int temp = count[i];
    count[i] = index;
    index += temp;
  }
  for(i=0;i<len;i++){
    out[count[arr[i]]] = arr[i];
    count[arr[i]]++;
  }
  return;
}
/*
  apperently I don't actually use the functions above.
*/
//this sorts from least significant byte to most significant byte
int radix_sort_8(ulong *arr, uint len, uint nbits){
  uint count = alloca(0xff*sizeof(uint));
  uint i,n;
  union word elt;
  ulong *output = alloca(sizeof(ulong)*len);
  for(n=0;n<(nbits/8);n++){
    memset(count, '\0', 0xff*sizeof(uint));
    for(i=0;i<len;i++){
      elt.integer = arr[i];
      count[elt.bytes[n]]++;
    }
    uint total = 0;
    //convert count into indices
    for(i=0;i<0xff;i++){
      uint temp = count[i];
      count[i] = total;
      total += temp;
    }
    //It'd be best swap between input and output
    //rather than zeroing output and copying back
    //to input each time, but that requires keeping
    //track of which is current and making sure input
    //has the right values at the end.
    memset(output,'\0',sizeof(ulong)*len);
    for(i=0;i<len;i++){
      elt.integer = arr[i];
      output[count[elt.bytes[n]]] = arr[i];
      count[elt.bytes[n]]++;
    }
    memcpy(input, output, sizeof(ulong)*len);
  }
  return 0;
}
int radix_sort_16(ulong *arr, uint len, uint nbits){
  uint count = alloca(0xffff*sizeof(uint));
  uint i,n;
  union word elt;
  ulong *output = alloca(sizeof(ulong)*len);
  for(n=0;n<(nbits/16);n++){
    memset(count, '\0', 0xffff*sizeof(uint));
    for(i=0;i<len;i++){
      elt.integer = arr[i];
      count[elt.words[n]]++;
    }
    uint total = 0;
    //convert count into indices
    for(i=0;i<0xffff;i++){
      uint temp = count[i];
      count[i] = total;
      total += temp;
    }
    //It'd be best swap between input and output
    //rather than zeroing output and copying back
    //to input each time, but that requires keeping
    //track of which is current and makeing sure input
    //has the right values at the end.
    memset(output,'\0',sizeof(ulong)*len);
    for(i=0;i<len;i++){
      elt.integer = arr[i];
      output[count[elt.words[n]]] = arr[i];
      count[elt.words[n]]++;
    }
    memcpy(input, output, sizeof(ulong)*len);
  }
  return 0;
}
//just for lulz
void fisher_yates(void **arr, int len){
  int i;
  for(i=len-1;i>0;i--){
    SWAP(arr[i], arr[lrand48() % i]);
  }
}
#define shuffle_array fisher_yates
void bogo_sort_generic(void **arr, int len, comparison_fn cmp){
  while(!is_sorted(arr, len, cmp)){
    shuffle_array(arr, len);
  }
}
/*
  My personal favorite sorting algorithm, it has a running time of O(n^lg(n)).
  The great thing about it though, is that in every step it always makes progress.
*/
static void slowsort_generic(void **arr, int i, int j, comparison_fn cmp){
  if(i>=j){
    return;
  }
  int mid = (i + j)/2;
  slowsort_generic(arr, i, mid, cmp);
  slowsort_generic(arr, mid+1, j, cmp);
  if(cmp(arr[j],arr[m])){
    SWAP(arr[j],arr[m]);
  }
  slowsort(arr, i, j-1);
}
                   
void slow_sort_generic(void **arr, int len, comparison_fn cmp){
  int mid = len / 2;
  slowsort(arr, mid);
  slowsort(arr+mid, len);
