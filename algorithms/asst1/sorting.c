#define NEED_XMALLOC
#include "C_util.h"
#define get_byte(val, byte)                     \
  (((val) >> ((byte)*CHAR_BIT)) & 0xffu)
#define SWAP(x,y)                                    \
  __extension__ ({__typeof(x) __temp = x;                          \
      x = y;                                                       \
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
void insertion_sort_generic(void **input, size_t len, cmp_fun cmp){
  uint i,j;
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
  __extension__ ({__typeof(_x) x = _x;         \
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
  while(n1 > 0 && n2 > 0){
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
  This used to use arrays for hist and sum, and loops, but it sometimes failed.
  I manually unrolled all the loops and used seperate variables and it worked.
  I could probably figure out what was wrong and  put back in the loops and arrays,
  but this works and I'm lazy.
*/
/*
  Note for posterity:
  I made a mistake when I first wrote this, I set size of the histograms to 
  be 255 (i.e 0xff) bytes each, however a byte can take on 256 (i.e 0x100) possible
  values. This caused a subtle bug, the function would work so long as the byte 0xff didn't
  appear in the input, but if it did then memory out of bounds would be written (generally
  writing to hist_n[0xff] would write to hist_n+1[0] instead). It took me a while
  to realize what was going on.
*/
uint64_t *radix_sort_u64(uint64_t *in, size_t sz){
  uint64_t *hist = alloca(8 * 0x100 * sizeof(uint64_t));
  uint64_t *hist0 = hist;
  uint64_t *hist1 = hist0 + 0x100;
  uint64_t *hist2 = hist1 + 0x100;
  uint64_t *hist3 = hist2 + 0x100;
  uint64_t *hist4 = hist3 + 0x100;
  uint64_t *hist5 = hist4 + 0x100;
  uint64_t *hist6 = hist5 + 0x100;
  uint64_t *hist7 = hist6 + 0x100;
  uint64_t total = 0, sum0 = 0, sum1 = 0, sum2 = 0, sum3 = 0;
  uint64_t sum4 = 0, sum5 = 0, sum6 = 0 , sum7 = 0;
  size_t i;
  memset(hist, '\0', 8*0xff*sizeof(uint64_t));
  for(i=0;i<sz;i++){
    hist0[get_byte(in[i], 0)]++;
    hist1[get_byte(in[i], 1)]++;
    hist2[get_byte(in[i], 2)]++;
    hist3[get_byte(in[i], 3)]++;
    hist4[get_byte(in[i], 4)]++;
    hist5[get_byte(in[i], 5)]++;
    hist6[get_byte(in[i], 6)]++;
    hist7[get_byte(in[i], 7)]++;
  }  
#define set_index(byte)                         \
  total = CAT(hist,byte)[i] + CAT(sum,byte);    \
  CAT(hist,byte)[i] = CAT(sum, byte);           \
  CAT(sum, byte) = total;
  
  for(i=0;i<0x100;i++){
    set_index(0);
    set_index(1);
    set_index(2);
    set_index(3);
    set_index(4);
    set_index(5);
    set_index(6);
    set_index(7);
  }
  
  uint64_t *temp = zmalloc(sz*sizeof(uint64_t));
  uint64_t *a = in, *b = temp;
  size_t pos = 0;
  for(i=0;i<sz;i++){
    pos = get_byte(a[i], 0);
    b[hist0[pos]++] = a[i];
  }
  SWAP(a, b);
  for(i=0;i<sz;i++){
    pos = get_byte(a[i], 1);
    b[hist1[pos]++] = a[i];
  }
  SWAP(a, b);
  for(i=0;i<sz;i++){
    pos = get_byte(a[i], 2);
    b[hist2[pos]++] = a[i];
  }
  SWAP(a, b);
  for(i=0;i<sz;i++){
    pos = get_byte(a[i], 3);
    b[hist3[pos]++] = a[i];
  }
  SWAP(a, b);
  for(i=0;i<sz;i++){
    pos = get_byte(a[i], 4);
    b[hist4[pos]++] = a[i];
  }
  SWAP(a, b);
  for(i=0;i<sz;i++){
    pos = get_byte(a[i], 5);
    b[hist5[pos]++] = a[i];
  }
  SWAP(a, b);
  for(i=0;i<sz;i++){
    pos = get_byte(a[i], 6);
    b[hist6[pos]++] = a[i];
  }
  SWAP(a, b);
  for(i=0;i<sz;i++){
    pos = get_byte(a[i], 7);
    b[hist7[pos]++] = a[i];
  }
  SWAP(a, b);
  free(temp);
  return in;
}
/*
  same as above but using histograms of words instead of bytes. This will be
  faster for a large enough size (I'm not totally sure what the threshold is), but
  it uses a lot more memory
*/
#define get_word(val, word)                     \
  (((val) >> ((2*word)*CHAR_BIT)) & 0xffffu)
uint64_t *radix_sort_u16_u64(uint64_t *in, size_t sz){
  uint64_t *hist = alloca(4 * 0x10000 * sizeof(uint64_t));
  uint64_t *hist0 = hist;
  uint64_t *hist1 = hist0 + 0x10000;
  uint64_t *hist2 = hist1 + 0x10000;
  uint64_t *hist3 = hist2 + 0x10000;
  uint64_t total = 0, sum0 = 0, sum1 = 0, sum2 = 0, sum3 = 0;
  size_t i;
  memset(hist, '\0', 4*0x10000*sizeof(uint64_t));
  for(i=0;i<sz;i++){
    hist0[get_word(in[i], 0)]++;
    hist1[get_word(in[i], 1)]++;
    hist2[get_word(in[i], 2)]++;
    hist3[get_word(in[i], 3)]++;
  }
  for(i=0;i<0x10000;i++){
    total = hist0[i] + sum0;
    hist0[i] = sum0;
    sum0 = total;

    total = hist1[i] + sum1;
    hist1[i] = sum1;
    sum1 = total;

    total = hist2[i] + sum2;
    hist2[i] = sum2;
    sum2 = total;
      
    total = hist3[i] + sum3;
    hist3[i] = sum3;
    sum3 = total;
  }
  fprintf(stderr, "%lu, %lu, %lu, %lu\n", sum0, sum1, sum2, sum3);
  uint64_t *temp = zmalloc(sz*sizeof(uint64_t));
  uint64_t *a = in, *b = temp;
  size_t pos = 0;
  for(i=0;i<sz;i++){
    pos = get_word(a[i], 0);
    b[(hist0[pos]++)] = a[i];
  }
  SWAP(a, b);
  for(i=0;i<sz;i++){
    pos = get_word(a[i], 1);
    b[(hist1[pos]++)] = a[i];
  }
  SWAP(a, b);
  for(i=0;i<sz;i++){
    pos = get_word(a[i], 2);
    b[(hist2[pos]++)] = a[i];
  }
  SWAP(a, b);
  for(i=0;i<sz;i++){
    pos = get_word(a[i], 3);
    b[(hist3[pos]++)] = a[i];
  }
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
