#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#include <stdint.h>
#include <limits.h>
typedef unsigned int uint;
typedef unsigned long ulong;
//generic counting sort suitable for sorting aribitary data
//indexed by integer keys
#define count_occurances(arr,len,max,key)       \
  {(ulong *count = alloca(sizeof(ulong) *max);  \
    int i;                                      \
    for (i=0;i<len;i++) {                       \
      count[key(arr[i])]++;                     \
    }                                           \
    count)}
static inline ulong *count_occurances(ulong *arr, ulong len, ulong max){
  ulong *count = 
void *counting_sort(void *arr,int len,int min,int max,int (*key)(void*)){
  //sort an array via integer keys, keys are obtained by calling the key
  //function on the given array
  //len is the length of the array, min and max are the minimum and maximum
  //values of the integer keys
  int *count = alloca(max-mix * sizeof(int));
  void *output = malloc(len * sizeof(void*));
  int i,total;
  for(i=0;i<len;i++){
    //count the number of times each integer key appears
    count[key(arr[i])]++;
  }
  //change the values in the count array to the starting index
  //of the elements with that key in the output array
  for(i=0,total=0;i<(max-mix);i++){
    int temp=count[i];
    count[i]=total;
    total+=temp;
  }
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
struct min_max {
  ulong min;
  ulong max;
};
struct min_max min_max(ulong *arr, ulong n){
  int i;
  ulong min=arr[0],max=arr[0];
  for(i=1;i<n;i++){
    if(arr[i] > max){
      max = arr[i];
      continue;
    }
    if(arr[i] < min){
      min = arr[i];
      continue;
    }
  }
  struct min_max = {.min = min, .max = max};
  return min_max;
}
uint integer_counting_sort(ulong *arr, ulong n, ulong nbits){
  //could be more efficent, but still not bad
  //need use lists to store count or find max/min values of arr;
  struct min_max mm = min_max(arr,n);
  ulong min = mm.min, max = mm.max;
  int *count = alloca(max-mix * sizeof(int));
  int i,j;
  for(i=0;i<len;i++){
    //count the number of times each integer key appears
    count[arr[i]]++;
  }
  for(i=0,j=0;i<max-min;){
    //count[i] is a count of the number of element in arr
    //equal to i. if it's 0 move to the next number otherwise
    //add to the output and decrese count[i];
    //j keeps track of where we are in the output
    if(count[i]){
      arr[j++]=i;
      count[i]--;
    } else {
      i++;
    }
  }
  return 0;
}
#define ARR_SWAP(arr,i,j)                                               \
  ({__typeof__(arr[i]) __temp = arr[i];                                 \
  arr[i]=arr[j];                                                        \
  arr[j]=temp;})
/* Its a shame C doesn't have nested functions, because most of the
   parameters to the partiton function are in the containing scope
   of the qsort function where it's called
*/
static int qsort_partiton(ulong *arr, ulong left,
                          ulong right, ulong pivot_ind,
                          int(*comp)(ulong,ulong)){
  ulong pivot = arr[pivot_ind];
  arr[pivot_ind] = arr[right];
  int i;
  ulong index = pivot_ind;
  for(i=left;i<right-1;i++){
    if(comp(arr[i],pivot)){
      ARR_SWAP(arr,i,index);
      index++;
    }
  }
  arr[right]=arr[index];
  arr[index]=pivot;
  return pivot_ind;
}
//call this at somepoint srandom(time(NULL));
static void qsort_inplace(ulong *arr, ulong left, ulong right,
                          int(*comp)(ulong,ulong)){
  if(left < right){
    int pivot = (random() % right) + left;
    pivot = qsort_partiton(arr, left, right, pivot, comp);
    qsort_inplace(arr, left, pivot-1,comp);
    qsort_inplace(arr, pivot +1, right ,comp);
  }
}
static int gt(ulong x,ulong y){
  return x>y;
}
uint qsort(ulong *arr, ulong n, ulong nbits){
  qsort_inplace(arr, 0, n, gt);
  return 0;
}

ulong bytes[8]={0xff<<56,0xff<<48,0xff<<40,0xff<<32,
                0xff<<24,0xff<<16,0xff<<8,0xff};
//radix sort of base 8
uint radix_sort(ulong *arr, ulong len, ulong nbits){
  ulong buckets[256] = {0};
  int i,n,total;
  for(n=0;n<nbits;n+=8){
    for(i=0;i<len;i++){
      buckets[arr[i]&bytes[n]]++;
    }
    for(i=0,total=0;i<256;i++){
      int temp=buckets[i];
      buckets[i]=total;
      total+=temp;
    }
  }
}
