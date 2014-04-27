//
// simple test of the simulated virtual memory system
//

#include <stdio.h>
#include <stdlib.h>
#include "simVM.h"

#define VMEMSIZE 64*4
#define PMEMSIZE 4*4
#define PAGESIZE 4*4
#define TLBSIZE 2*4
#define MEMSIZE (VMEMSIZE*PAGESIZE)

int main(void)
{
  void *h = createVM(VMEMSIZE, PMEMSIZE, PAGESIZE, TLBSIZE, 1, 0);
  if (h == NULL)
  {
    fprintf(stderr, "createVM failed!\n");
    exit(-1);
  }

  int i, sum, sum2;

  // initialize the array
  for (i = 0; i < MEMSIZE; i++)
  {
    writeInt(h, i, i);
  }

  // now sum it
  sum = 0;
  sum2 = 0;
  for (i = 0; i < MEMSIZE; i++)
  {
    int tmp = readInt(h, i);
    // printf("# %d %d\n", tmp, i);
    sum += tmp;
    sum2 += i;
  }
  printf("sum is %d (should be %d)\n", sum, sum2);

  // print stats
  printf("\n");
  printStatistics(h);
  printf("\n");

  return 0;
}


