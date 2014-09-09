#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#include <string.h>
#include <stdint.h>
#include <limits.h>
#include <unistd.h>
#include <sys/mman.h>
typedef unsigned int uint;
typedef unsigned long ulong;

static const radix_base = 8;
//Key is a function to get keys from array elements
//if array elements are the keys leave key empty
#define count_keys(arr,len,max,key)             \
  {(ulong *count = alloca(sizeof(ulong) *max);  \
    memset(count, '\0', sizeof(ulong)*max);     \
    int i;                                      \
    for (i=0;i<len;i++) {                       \
      count[key(arr[i])]++;                     \
    }                                           \
    count;)}

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
  memset(count, '\0', (max-mix * sizeof(int)));
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
uint integer_counting_sort(ulong *arr, ulong len, ulong nbits){
  //could be more efficent, but still not bad
  //need use lists to store count or find max/min values of arr;
  struct min_max mm = min_max(arr,n);
  ulong min = mm.min, max = mm.max;
  int *count = count_keys(arr, len, max-min,)
    //because we only have integer keys and don't need to have a stable
    //sort we can optimize here by sorting in place and not computing
    //indices.
  int i,j;
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
//in order to be able to use an arbitary radix
//the line: count[arr[i]&bytes[n]]++; should be replaced by
//count[(arr[i]/count_size)%radix_base]++;
uint radix_sort(ulong *arr, ulong len, ulong nbits){
  int count_size = 1<<radix_base;
  ulong count[count_size] = {0};
  int i,n,total;
  for(n=0;n<nbits;n+=radix_base){
    /* Can't use the count keys macro because we can't close
       over the value of n in C*/
    for(i=0;i<len;i++){
      count[arr[i]&bytes[n]]++;
    }
    compute_indices(count, count_size);
    update_input(arr, count, len);
  }
}
struct input_data {
  ulong len;
  ulong nbits;
  ulong min;
  ulong max;
  ulong *data;
};
char *mmap_file(const char *input_file){
  int fd = open(input_file, O_RDONLY);
  const char *err_msg;
  if(fd < 0){
    handle_error("read");
  }
  //be lazy and just use mmap
  off_t len = lseek(fd, 0, SEEK_END);
  if(len == (off_t)-1){
    handle_error("lseek");
  }
  char *mem = mmap(NULL, len+1, PROT_READ, MAP_PRIVATE, fd, 0);
  if(mem == MAP_FAILED){
    handle_error("mmap");
  }
  if(close(fd) < 0){
    munmap(mem);
    handle_error("close");
  }
}
#define handle_error(msg)                               \
  perror(msg);                                          \
  return NULL;
#define strtoul_err(val)                            \
  ((errno == ERANGE && (val == ULONG_MAX))            \
   || (errno != 0 && val == 0))

struct input_data* read_input(char *data) {
  int num_values = strtoul(data, &data, 0);
  if(strtoul_err(num_values)){return NULL;}
  int nbits = strtoul(data, &data, 0);
  if(strtoul_err(nbits) || nbits > (CHAR_BIT * sizeof(ulong))){
    return NULL;
  }

  struct input_data *retval=malloc(sizeof(struct input_data));
  if(!retval){return NULL;}

  retval.data = malloc(sizeof(ulong)*num_values);
  if(!retval.data){goto CLEANUP;}

  retval.len = num_values;
  retval.nbits = nbits;
  
  retval.data[0] = strtoul(data, &data, 0);
  if(strtoul_err(retval.data[i])){
    goto CLEANUP;
  }
  retval.min = retval.max = retval.data[0];
  int i;
  for(i=1;i<num_values;i++){
    retval.data[i] = strtoul(data, &data, 0);
    if(strtoul_err(retval.data[i])){
      goto CLEANUP;
    }
    if(retval.data[i] > retval.max){
      retval.max = retval.data[i];
    } else if (retval.data[i] < retval.min){
      retval.min = retval.data[i];
    }
  }
  return retval; 
 CLEANUP:
  free(retval.data);
  free(retval);
  return NULL;
}
