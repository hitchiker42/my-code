#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>      
#include <errno.h>  

// NOTE: this file cannot be compiled with the -std=c99 flag.

// allocate a buffer that instructions can be placed in
//   by default malloc-ed memory is enabled for having instructions fetched
//     and executed from it
//   so need to go to some more trouble to get memory and configure it to
//     allow instructions to be stored in it and then fetched/executed.
//   it returns the length of the buffer through an output parameter
//     right now it always returns a buffer that is one page in length
//
void *allocateInstructionBuffer(unsigned int *length)
{
  //finds pagesize
  unsigned int pagesize;
  pagesize = sysconf(_SC_PAGE_SIZE);
  *length = pagesize;
 
  //pointer to buffer to receive machine code
  unsigned char *buffer;
  
  //allocate buffer aligned on page boundary
  int ret;
  ret = posix_memalign((void **) &buffer, pagesize, pagesize);
  if (ret == EINVAL) 
  {
    fprintf(stderr,
            "posix_memalign failed: pagesize is not a power of two?\n");
    exit(-1);
  }
  if (ret == ENOMEM) 
  {
    fprintf(stderr, "posix_memalign failed: insufficient memory\n");
    exit(-1);
  }

  // enable memory for read, write and execution
  if(mprotect(buffer, pagesize, PROT_EXEC | PROT_WRITE | PROT_READ))
  {
    perror("mprotect failed: ");
    exit(-1);
  }

  return buffer;
}
 
