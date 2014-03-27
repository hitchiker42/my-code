// test for exception handler

#include <stdio.h>

void throwException(int);
void cancelCatchException(void);
int catchException(void);
int caughtException;
int temp=340;

void f3()
{
  fprintf(stderr, "f3 entered\n");
  throwException(1999);
  fprintf(stderr, "f3 exiting\n");
}

void f2()
{
  fprintf(stderr, "f2 entered\n");
  if ((caughtException = catchException()))
  {
    temp=temp-1;
    fprintf(stderr, "f2 catch clause entered\n");
    fprintf(stderr, "temp %d\n", temp);
    fprintf(stderr, "f2 catch clause exiting\n");
    throwException(caughtException>>1);
  }
  f3();
  fprintf(stderr, "f2 exiting\n");
}

void f1()
{
  fprintf(stderr, "f1 entered\n");
  if ((caughtException = catchException()))
  {
    temp=temp-1;
    fprintf(stderr, "f1 catch clause entered\n");
    fprintf(stderr, "temp = %d\n", temp);
    fprintf(stderr, "f1 catch clause exiting\n");
    throwException(caughtException>>1);
  }
  f2();
  fprintf(stderr, "f1 exiting\n");
}

int main(int argc, char*argv[])
{
  fprintf(stderr, "main entered\n");
 TOP:
  if ((caughtException = catchException()))
  {
    fprintf(stderr, "catch clause entered\n");
    fprintf(stderr, "temp = %d\n", temp);
    if(temp<=1){
      return 0;
    }
    goto TOP;
    fprintf(stderr, "catch clause exiting\n");
  }
  f1();
  cancelCatchException();
  fprintf(stderr, "main exiting\n");
  cancelCatchException();
  cancelCatchException();
  cancelCatchException();
  cancelCatchException();
  cancelCatchException();
  cancelCatchException();
  throwException(caughtException>>1);
  return 0;
}

