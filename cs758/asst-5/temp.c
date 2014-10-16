#include <stdlib.h>
#include <stdio.h>
int main(){
  unsigned long  *a = NULL, *b=NULL, *c=NULL;
  int i=0,j=0,k=0;
  a = alloca(128);
  b = alloca(64);
  c = alloca(32);
  printf("a = %p, a - 128 = %p, a + 128 = %p\n"
         "b = %p, b - 64 = %p, b + 64 = %p\n"
         "c = %p, c + 32 = %p, c + 48 = %p\n",
         a, a-128, a+128, b, b-64, b+64, c, c + 32, c +48);
  printf("*a + 128 = %#x, *b + 64 = %#x, *c + 32 = %#x\n",
         *(a+128),*(b+64),*(c+32));
  *(a+128) = 0xcab;
  *(b+64) = 0xabc;
  *(c+32) = 0xfff;
  printf("*a + 128 = %#x, *b + 64 = %#x, *c + 32 = %#x\n"
         "*(a - 8) = %#x, *(b + 72) = %#x\n"
         "*(a - 16) = %#x, *(b + 80) = %#x\n",
         *(a+128),*(b+64),*(c+32),*(a-8),*(b+72),*(a-16),*(b+80));
         
}
