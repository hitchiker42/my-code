#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <alloca.h>
#include <limits.h>
#include <stdio.h>
typedef unsigned long ulong;
typedef unsigned int uint;
#define ARR_SWAP(arr,i,j)                                               \
  __extension__ ({__typeof__(arr[i]) __temp = arr[i];                   \
  arr[i]=arr[j];                                                        \
  arr[j]=__temp;})


#define insertion_sort_inplace_template(name, type, cmp)        \
  void name(type* arr, int len){                                \
    int i,j;                                                    \
    for(i=0;i<len;i++){                                         \
      type val = arr[i];                                        \
      for(j=i;j>0;j--){                                         \
        if(cmp(arr[j-1], val)){                                 \
          break;                                                \
        } else {                                                \
          arr[j] = arr[j-1];                                    \
        }                                                       \
      }                                                         \
    }                                                           \
    return;                                                     \
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
//this sorts from least significant byte to most significant byte
int radix_sort_8(ulong *arr, uint len, uint nbits){
  uint count = alloca(0xff*sizeof(uint));
  uint i,n;
  union word elt;
  ulong *output = alloca(sizeof(ulong)*len);
  for(n=0;n<(nbits/16);n++){
    memset(count, '\0', 0xffff*sizeof(uint16_t));
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
    //track of which is current and makeing sure input
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
