#define NEED_MALLOC
#include "C_util.h"
#include "assert.h"
//for the lulz
//This really is as bad as it's made out to be, it's sooo slow
void bubble_sort_u64(uint64_t *in, size_t n){
  int i, new_n;
  while(n>0){
    new_n = 0;
    for(i=1;i<n;i++){
      if(in[i] < in[i-1]){
        SWAP(in[i], in[i-1]);
        new_n = i;
      }
    }
    n = new_n;
  }
}
int is_sorted_generic(void **input, size_t len, cmp_fun cmp){
  uint i;
  for(i=0;i<len-1;i++){
    if(!(cmp(input[i],input[i+1]))){
      return 0;
    }
  }
  return 1;
}
int is_sorted(uint64_t *input, size_t len){
  uint i;
  for(i=0;i<len-1;i++){
    if(input[i] > input[i+1]){
      return 0;
    }
  }
  return 1;
}
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
double time_sort(uint64_t *arr, size_t len, int_sort_fn sort){
  double start_time = float_time();
  sort(arr,len);
  double end_time = float_time();
  assert(is_sorted(arr, len));
  return (end_time - start_time);
}
enum sort_method {
  SORT_QUICK,
  SORT_BUBBLE,
  SORT_MERGE,
  SORT_INSERTION,
  SORT_RADIX,
  SORT_HEAP,
  SORT_ALL
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
  int method = SORT_ALL;
  if(argc == 4){
    if(is_prefix(argv[1], "quick")){
      method = SORT_QUICK;
    } else if(is_prefix(argv[1], "merge")){
      method = SORT_MERGE;
    } else if(is_prefix(argv[1], "insertion")){
      method = SORT_INSERTION;
    } else if(is_prefix(argv[1], "radix")){
      method = SORT_RADIX;
    } else if(is_prefix(argv[1], "heap")){
      method = SORT_HEAP;
    } else if(is_prefix(argv[1], "all")){
      method = SORT_ALL;
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
  ulong *arr;
  int fd = open(argv[2], O_RDONLY);
  if(fd>0){
    size_t sz;
    char *file = mmap_file(fd, 1, PROT_READ, &sz);
    if(!file){exit(1);}
    //char *file = read_file_to_string(fd, NULL);
    close(fd);
    arr = read_arr(len, file);
    munmap(file, sz);
    //free(file);
  } else {
    arr = read_arr(len, argv[2]);
  }
  if(!arr){
    perror("strtoul");
    exit(1);
  }
  //sort stuff
  double time;
  switch(method){
    case(SORT_INSERTION):
      time = time_sort(arr, len, insertion_sort_u64);break;
    case(SORT_MERGE):
      time = time_sort(arr, len, mergesort_u64);break;
    case(SORT_HEAP):      
      time = time_sort(arr, len, heapsort_u64);break;      
    case(SORT_QUICK):
      time = time_sort(arr, len, qsort_u64);break;
    case(SORT_RADIX):
      time = time_sort(arr, len, radix_sort_u8_u64);
      double time2 = time_sort(arr, len, radix_sort_u16_u64);
      printf("Radix sort with byte histograms: %f\n"
             "Radix sort with word histograms: %f\n", time, time2);
      return 0;
    case(SORT_ALL):{
      double times[6];
      uint64_t *arr_bkup = xmalloc(len * sizeof(uint64_t)); 
      memcpy(arr_bkup, arr, (len * sizeof(uint64_t)));
      times[0] = time_sort(arr, len, bubble_sort_u64);
      memcpy(arr, arr_bkup, (len * sizeof(uint64_t)));
      times[1] = time_sort(arr, len, insertion_sort_u64);
      memcpy(arr, arr_bkup, (len * sizeof(uint64_t)));
      times[2] = time_sort(arr, len, mergesort_u64);
      memcpy(arr, arr_bkup, (len * sizeof(uint64_t)));
      times[3] = time_sort(arr, len, heapsort_u64);
      memcpy(arr, arr_bkup, (len * sizeof(uint64_t)));
      times[4] = time_sort(arr, len, qsort_u64);
      memcpy(arr, arr_bkup, (len * sizeof(uint64_t)));
      times[5] = time_sort(arr, len, radix_sort_u64);
      printf("Times taken to sort:\n"
             "Bubble sort: %f\n"
             "Insertion sort: %f\n"
             "Merge sort: %f\n"
             "Heap sort: %f\n"
             "Quick sort: %f\n"
             "Radix sort: %f\n",
             times[0], times[1], times[2], times[3], times[4], times[5]);
      return 0;
    }
  }
  printf("Time taken to sort: %f\n", time);
  return 0;
}
