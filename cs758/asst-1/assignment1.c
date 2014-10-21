#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#include <string.h>
#include <stdint.h>
#include <limits.h>
#include <unistd.h>
#include <sys/time.h>
long int random(void);
/* The various sorting algorithm names */
static const char *counting_sort_str = "counting";
static const char *radix_sort_str = "radix";
static const char *quick_sort_str = "quick";
static const char *insertion_sort_str = "insertion";
static const char *system_quick_sort_str = "system_quick";

/* Number of bits in an unsigned long. */
static const unsigned long ulong_bits = 8 * sizeof(unsigned long);


typedef unsigned int uint;
typedef unsigned long ulong;

static const int radix_base = 8;
//Key is a function to get keys from array elements
//if array elements are the keys leave key empty
#define count_keys(arr,len,max,key)             \
  __extension__ ({uint *count = alloca(sizeof(uint) *max);    \
    memset(count, '\0', sizeof(uint)*max);      \
    uint i;                                      \
    for (i=0;i<len;i++) {                       \
      count[key(arr[i])]++;                     \
    }                                           \
    count;})

static inline void compute_indices(uint *count, ulong len){
  uint i,total;
  for(i=0,total=0;i<len;i++){
    ulong temp=count[i];
    count[i]=total;
    total+=temp;
  }
}
static inline void update_input(ulong *input, uint *count, ulong len){
  uint i;
  ulong *output = alloca(len * sizeof(ulong));
  for(i=0;i<len;i++){
    int ind = input[i];
    output[count[ind]]=input[i];
    count[ind]++;
  }
  memcpy(input, output, len);
}
struct min_max {
  ulong min;
  ulong max;
};
struct min_max min_max(ulong *arr, ulong n){
  uint i;
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
  struct min_max mm= {.min = min, .max = max};
  return mm;
}
uint integer_counting_sort(ulong *arr, ulong len,
                           ulong nbits __attribute__((unused))){
  //could be more efficent, but still not bad
  //need use lists to store count or find max/min values of arr;
  struct min_max mm = min_max(arr,len);
  ulong min = mm.min, max = mm.max;
  uint *count = count_keys(arr, len, max-min,(ulong));
    //because we only have integer keys and don't need to have a stable
    //sort we can optimize here by sorting in place and not computing
    //indices.
  uint i,j;
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
  __extension__ ({__typeof__(arr[i]) __temp = arr[i];                   \
  arr[i]=arr[j];                                                        \
  arr[j]=__temp;})
/* Its a shame C doesn't have nested functions, because most of the
   parameters to the partiton function are in the containing scope
   of the qsort function where it's called
*/
static int qsort_partiton(ulong *arr, ulong left,
                          ulong right, ulong pivot_ind,
                          int(*comp)(ulong,ulong)){
  ulong pivot = arr[pivot_ind];
  arr[pivot_ind] = arr[right];
  uint i;
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
uint quick_sort(ulong *arr, ulong n){
  qsort_inplace(arr, 0, n, gt);
  return 0;
}

ulong bytes[8]={(ulong)0xff<<56,(ulong)0xff<<48,(ulong)0xff<<40,
                (ulong)0xff<<32,(ulong)0xff<<24,(ulong)0xff<<16,
                (ulong)0xff<<8,(ulong)0xff};

//radix sort of base 8
//in order to be able to use an arbitary radix
//the line: count[arr[i]&bytes[n]]++; should be replaced by
//count[(arr[i]/count_size)%radix_base]++;
uint radix_sort(ulong *arr, ulong len, ulong nbits){
  int count_size = 1<<radix_base;
  uint *count = alloca(sizeof(uint)*len);
  memset(count, '\0',len*sizeof(uint));
  uint i,n;
  for(n=0;n<nbits;n+=radix_base){
    /* Can't use the count keys macro because we can't close
       over the value of n in C*/
    for(i=0;i<len;i++){
      count[arr[i]&bytes[n]]++;
    }
    compute_indices(count, count_size);
    update_input(arr, count, len);
  }
  return 0;
}
/************************************************************
 * Functions that you must implement
 ************************************************************/

/*
 * Radix sort
 * Return 0 on success and 1 on failure.
*/
static unsigned int do_radix_sort(unsigned long *arr,
				  unsigned long n, unsigned long nbits)
{
  return radix_sort(arr, n, nbits);
}


/*
 * Counting sort
 * Return 0 on success and 1 on failure.
 */
static unsigned int do_counting_sort(unsigned long *arr,
				     unsigned long n, unsigned long nbits)
{
  if (nbits > 20){
    exit(EXIT_FAILURE);
  }
  return integer_counting_sort(arr, n, nbits);
}


/*
 * Quicksort
 * Return 0 on success and 1 on failure.
 */
static unsigned int do_quicksort(unsigned long *arr,
				  unsigned long n,
                                 __attribute__((unused)) unsigned long nbits)
{
  return quick_sort(arr, n);
}

/************************************************************
 *
 * You probably don't need to modify anything beyond here.
 *
 ************************************************************/


/************************************************************
 * Example: Using the standard library's qsort() routine.
 ************************************************************/

/* The comparison function for qsort(). */
static int compare(const void *_a, const void *_b)
{
    unsigned long *a = (unsigned long *) _a;
    unsigned long *b = (unsigned long *) _b;

    return *a - *b;
}


/*
 * Uses the standard library quicksort function.
 * Return 0 on success and 1 on failure.
 */
static unsigned int do_system_quicksort(unsigned long ary[],
					unsigned long n,
                                        __attribute__((unused)) ulong nbits)
{
    /* This is the system's quick sort function. */
    qsort(ary, n, sizeof(*ary), compare);

    return 0;
}


/************************************************************
 * Example: insertion sort
 ************************************************************/

/*
 * Insertion sort.
 * Return 0 on success and 1 on failure.
 */
static unsigned int do_insertion_sort(unsigned long ary[], unsigned long n,
                                      __attribute__((unused)) ulong nbits)
{
    unsigned long j;

    for (j = 1; j < n; j += 1) {
	unsigned long key = ary[j];
	unsigned long i = j;
	while (i > 0 && ary[i-1] > key) {
	    ary[i] = ary[i-1];
	    i -= 1;
	}
	ary[i] = key;
    }

    return 0;
}

/*
 * Read the header from the input file and returts the number of
 * values in the input using the 'nvalues' argument and the number of
 * bits for each number using the 'nbits' argument.
 *
p * Returns 0 on success and 1 on failure.
 */
static unsigned int read_file_header(FILE * infile, unsigned long *nvalues,
				     unsigned long *nbits)
{
    int ret;
    unsigned long _nvalues, _nbits;

    ret = fscanf(infile, " %lu %lu", &_nvalues, &_nbits);
    if (ret == EOF) {
	fprintf(stderr, "Unexpected end of file\n");
	return 1;
    }
    if (ret != 2) {
	fprintf(stderr, "Malformed file header\n");
	return 1;
    }

    if (_nbits > ulong_bits) {
	fprintf(stderr, "%lu bits input values are too big\n", _nbits);
	fprintf(stderr, "Word size seems to be %lu bits\n", ulong_bits);
	return 1;
    }

    if (nvalues)
	*nvalues = _nvalues;
    if (nbits)
	*nbits = _nbits;

    return 0;
}

/*
 * Reads the next number from the input file into the 'num' argument.
 * If the end of the file is reached then 'num' is left as is and
 * 'eof' is set to 1, otherwise 'eof' is set to zero.
 *
 * Returns 0 on success and 1 on failure.
 */
static unsigned int read_next_number(FILE * infile, unsigned long *num,
				     unsigned int *eof)
{
    int ret;
    unsigned long _num;

    ret = fscanf(infile, " %lu", &_num);
    if (ret == EOF) {
	*eof = 1;
	return 0;
    }
    if (ret != 1) {
	perror("fscanf failed");
	return 1;
    }

    *num = _num;
    *eof = 0;

    return 0;
}

/*
 * Reads 'n' numbers from the given input file into the provided
 * array.
 *
 * Returns 0 on success and 1 on failure.  The state of 'ary' on
 * failure is unspecified.
 */
static unsigned int read_into_array(FILE * infile, unsigned long n,
				    unsigned long ary[])
{
    unsigned long i;

    for (i = 0; i < n; i += 1) {
	unsigned int err, eof;
	err = read_next_number(infile, &ary[i], &eof);
	if (err)
	    return 1;
	if (eof) {
	    fprintf(stderr, "Unexpected EOF when reading %lu values", n);
	    return 1;
	}
    }

    return 0;
}

/* Writes the given number to the output file. */
static void output_number(FILE * outfile, unsigned long num)
{
    fprintf(outfile, "%lu\n", num);
}

/* Output the given array to the output file. */
static void output_from_array(FILE * outfile, unsigned long ary[],
			      unsigned long n)
{
    unsigned long i;

    for (i = 0; i < n; i += 1)
	output_number(outfile, ary[i]);
}

/* Print the usage string */
static void usage(void)
{
    fprintf(stderr, "usage: sort <algorithm> <infile> <outfile>\n");
    fprintf(stderr,
	    "Where <algorithm> is one of: counting, radix, quick,\n"
	    "                             insertion or system_quick\n"
	    "and <infile> and/or <outfile> may be '-' to indicate that\n"
	    "the standard input and/or output stream should be used\n");
}


/*
 * Reads the file header and the values.  The return value is an array
 * of values that must be freed by the caller.
 */
static unsigned long *get_values(FILE * infile, unsigned long *n,
				 unsigned long *nbits)
{
    unsigned int err;
    unsigned long _n, _nbits;
    unsigned long *ary;

    err = read_file_header(infile, &_n, &_nbits);
    if (err)
	return NULL;

    ary = malloc(sizeof(*ary) * _n);
    if (!ary) {
	perror("Failed to allocate array");
	return NULL;
    }

    err = read_into_array(infile, _n, ary);
    if (err) {
	free(ary);
	return NULL;
    }

    if (n)
	*n = _n;
    if (nbits)
	*nbits = _nbits;

    return ary;
}

/* Gets the time of day in seconds. */
static double get_current_seconds(void)
{
    double sec, usec;
    struct timeval tv;

    if (gettimeofday(&tv, NULL) < 0) {
	perror("gettimeofday failed");
	exit(EXIT_FAILURE);
    }

    sec = tv.tv_sec;
    usec = tv.tv_usec;

    return sec + (usec / 1000000);
}

/*
 * Reads the values, begins the timer, calls the sorting algorithm,
 * stops the timer and outputs the values.  The time taken is printed
 * to standard error.
 */
static unsigned int do_sort(const char *const algorithm, FILE * infile,
			    FILE * outfile)
{
    int err = 0;
    double start, end;
    unsigned long n, nbits;
    unsigned long *ary;

    ary = get_values(infile, &n, &nbits);
    if (!ary)
	return 1;

    start = get_current_seconds();

    if (strcmp(algorithm, counting_sort_str) == 0) {
	err = do_counting_sort(ary, n, nbits);

    } else if (strcmp(algorithm, radix_sort_str) == 0) {
	err = do_radix_sort(ary, n, nbits);

    } else if (strcmp(algorithm, quick_sort_str) == 0) {
	err = do_quicksort(ary, n, nbits);

    } else if (strcmp(algorithm, insertion_sort_str) == 0) {
	err = do_insertion_sort(ary, n, nbits);

    } else if (strcmp(algorithm, system_quick_sort_str) == 0) {
	err = do_system_quicksort(ary, n, nbits);

    } else {
	fprintf(stderr, "Impossible\n");
	exit(EXIT_FAILURE);
    }

    end = get_current_seconds();

    output_from_array(outfile, ary, n);
    fprintf(stderr, "%f\n", end - start);

    free(ary);

    return err;
}


int main(int argc, char *const argv[])
{
    int ret = EXIT_SUCCESS;
    unsigned int err;
    FILE *infile = stdin, *outfile = stdout;

    if (argc < 4 || (strcmp(argv[1], counting_sort_str) != 0
		     && strcmp(argv[1], radix_sort_str) != 0
		     && strcmp(argv[1], quick_sort_str) != 0
		     && strcmp(argv[1], insertion_sort_str) != 0
		     && strcmp(argv[1], system_quick_sort_str) != 0)) {
	usage();
	return EXIT_FAILURE;
    }


    if (strcmp(argv[2], "-") != 0) {
	infile = fopen(argv[2], "r");
	if (!infile) {
	    perror("Failed to open input file for reading");
	    ret = EXIT_FAILURE;
	    goto out;
	}
    }

    if (strcmp(argv[3], "-") != 0) {
	outfile = fopen(argv[3], "w");
	if (!outfile) {
	    perror("Failed to open output file for writing");
	    ret = EXIT_FAILURE;
	    goto out;
	}
    }

    err = do_sort(argv[1], infile, outfile);
    if (err)
	ret = EXIT_FAILURE;

  out:

    if (outfile && outfile != stdout)
	fclose(outfile);
    if (infile && infile != stdin)
	fclose(infile);

    return ret;
}
