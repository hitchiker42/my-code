#include "hello.h"
#include <stdio.h>
#include <stdlib.h>
int main (int argc, char **argv)
{
  greeting();
  if (argc==1 || atoi(argv[1])==0) {
    printf("10 factorial is %d\n", factorial(10));
  } else {
      int num=atoi(argv[1]);
      printf("%d factorial is %d\n", num,factorial(num));
  }
}
