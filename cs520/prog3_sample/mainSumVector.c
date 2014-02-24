#include <stdio.h>

//
// this main program simply allocates a buffer, fills it in
// with code implementing sumVector.obj and then executes it
//

// allocates a buffer that instructions can be written to and then
// executed
extern void *allocateInstructionBuffer(unsigned int *length);

// translates encoded vm520 instructions to encoded Intel 64 instructions
extern void translateBinary(char *vm520ObjFile,
                     unsigned char *buffer,
                     unsigned int length);

int main()
{
  unsigned int bufferLength;
  unsigned char *buffer = allocateInstructionBuffer(&bufferLength);

  // generate the Intel code for sumVector.obj
  translateBinary("not used!", buffer, bufferLength);
  
  // the trick is to pass the code the address of the buffer,
  // which will be placed into %rsi
  ((void (*)(void*)) buffer)(buffer); 
 
  // "sum" starts in second quadword of buffer
  printf("The result is %ld\n", *(((long *) buffer) + 1)); 

  return 0;
}
