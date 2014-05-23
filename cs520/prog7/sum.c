//
// simple test of the simulated virtual memory system
//

#include <stdio.h>
#include <stdlib.h>
#include "simVM.h"

#define VMEMSIZE 64*64
#define PMEMSIZE 4*64
#define PAGESIZE 4*64
#define TLBSIZE 2*64
#define MEMSIZE (VMEMSIZE*PAGESIZE)

int main(void)
{
  void *h = createVM(VMEMSIZE, PMEMSIZE, PAGESIZE, TLBSIZE, 0, 0);
  if (h == NULL)
  {
    fprintf(stderr, "createVM failed!\n");
    exit(-1);
  }

  float i, sum, sum2;

  // initialize the array
  for (i = 0; i < MEMSIZE; i++)
  {
    writeFloat(h, i, i);
  }

  // now sum it
  sum = 0;
  sum2 = 0;
  for (i = 0; i < MEMSIZE; i++)
  {
    float tmp = readFloat(h, i);
    // printf("# %d %d\n", tmp, i);
    sum += tmp;
    sum2 += i;
  }
  printf("sum is %f (should be %f4)\n", sum, sum2);

  // print stats
  printf("\n");
  printStatistics(h);
  printf("\n");

  return 0;
}


