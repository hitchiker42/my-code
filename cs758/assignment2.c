#include "heap.h"
#include <stdlib.h>
/* Read in floats from stdin one per line, do something with them:
   three things;
   seq, sequential sum in order read in.
   sort, sort numbers min->max then add them sequentially
   min2_scan, find 2 minimum numbers, using linear scan,
     remove them from the 'list' and add them to the sum, repeat
     until no numbers left.
   heap, find 2 min numbers using a heap, remove from heap, add
     to sum, repeat
*/

binary_heap * read_floats_heap(FILE *input){
  
