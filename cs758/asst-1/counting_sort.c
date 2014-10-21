#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
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
