#include <stdio.h>
#include <stdlib.h>
void test_args(float x,int i,int[] array){
  printf("Before in fxn:\nfloat x=%f,int i=%d and int[3] array=%d %d %d\n"\
         ,x,i,array[0],array[1],array[2]);
  x=3.14159;
  i=65536;
  printf("After in fxn:\nfloat x=%f,int i=%d and int[3] array=%d %d %d\n"\
         ,x,i,array[0],array[1],array[2]);
}
int main (int argv, char** args){
  float x=2.71828;
  int i=1729;
  int[3] array={34,55,89};
  printf("Before,in main:\nfloat x=%f,int i=%d and int[3] array=%d %d %d\n"\
         ,x,i,array[0],array[1],array[2]);
  test_args(x,i,array);
  printf("After in main:\nfloat x=%f,int i=%d and int[3] array=%d %d %d\n" \
         ,x,i,array[0],array[1],array[2]);
  return 0;
}
