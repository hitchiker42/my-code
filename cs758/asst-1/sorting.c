#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <alloca.h>
#include <stdint.h>
#include <limits.h>
//type of a comparison function returns 1 if the comparison is tru
typedef int(comp_fun)(void*,void*);
typedef unsigned int uint;
typedef unsigned long ulong;
int is_sorted(void *input, ulong len, comp_fun *comp){
  int i;
  for(i=0;i<len-1;i++){
    if(!(comp(input[i],input[i+1]))){
      return 0;
    }
  }
  return 1;
}
int insertion_sort_generic(void *input, ulong len, comp_fun *comp){
  int i,j;
  for(i=1;i<len;i++){
    void *temp = input[i];
    j=i;
    while(j>0 && comp(input[j-1],x)){
      input[j]=input[j-1];
      j--;
    }
    input[j]=temp;
  }
}
#define count_keys(arr,len,max,key)             \
  {(ulong *count = alloca(sizeof(ulong) *max);  \
    memset(count, '\0', sizeof(ulong)*max);     \
    int i;                                      \
    for (i=0;i<len;i++) {                       \
      count[key(arr[i])]++;                     \
    }                                           \
    count)}

static inline void compute_indices(ulong *count, ulong len){
  int i,total;
  for(i=0,total=0;i<len;i++){
    ulong temp=count[i];
    count[i]=total;
    total+=temp;
  }
}
static inline void update_input(ulong *input, ulong *count, ulong len){
  int i;
  ulong *output = alloca(len * sizeof(ulong));
  for(i=0;i<len;i++){
    int ind = arr[i];
    output[count[ind]]=arr[i];
    count[ind]++;
  }
  memcpy(input, output, len);
}
//generic counting sort suitable for sorting aribitary data
//indexed by integer keys
void *counting_sort(void *arr,int len,int min,int max,int (*key)(void*)){
  //sort an array via integer keys, keys are obtained by calling the key
  //function on the given array
  //len is the length of the array, min and max are the minimum and maximum
  //values of the integer keys
  int *count = alloca(max-mix * sizeof(int));
  memset(count, '\0', (max-mix * sizeof(int));
  void *output = malloc(len * sizeof(void*));
  int i,total;
  for(i=0;i<len;i++){
    //count the number of times each integer key appears
    count[key(arr[i])]++;
  }
    //change the values in the count array to the starting index
  //of the elements with that key in the output array
  compute_indices(count,max-min);

  //fill the output array with the sorted elements
  for(i=0;i<len;i++){
    int ind=key(arr[i]);
    /* 
       because of the second loop count[ind] contains the index
       that the current element should be placed
    */
    output[count[ind]] = arr[i];
    count[ind]++;//make sure other elements with the same key don't overwrite
    //the element we just added;
  }
  //two ways to return the sorted output
  //1. just return the sorted array, this allocates new memory that the user
  //needs to deal with
  //2. overwrite the input array with the sorted output
  //for now use 1.
  return output;
}
int 
