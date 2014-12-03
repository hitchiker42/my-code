#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include "bitvector.h"
typedef unsigned long ulong;
static const ulong ulong_bits = sizeof(ulong)*CHAR_BIT;
#define nth_bit(x,n) (x & (1UL << n))
#define nth_value(x,i) (nth_bit((x[i%ulong_bits]),(i/ulong_bits))
int* seq(int start,int end){
  int* s = malloc(sizeof(int)*(end-start));
  int n,i;
  for (n=start,i=0;n<=end;n++,i++){
    s[i]=n;
  }
  return s;
}
uint8_t* prime_sieve_bytes(int max){
  uint8_t *sieve = malloc(sizeof(uint8_t)*max);
  if(!sieve){return NULL;}
  memset(sieve,1,max);
  int limit = (int)ceil(pow(max,0.5));
  int i,j;
  for(i=0;i<limit;i++){
    if(sieve[i]){
      for(j=(i*i);j<max;j+=i){
        sieve[j]=0;
      }
    }
  }
}             
byte* prime_sieve_bitvector(int max){
  byte* primes = bitvector(max);
  memset(primes,0xff,(max/8));
  int i,j,cnt=0;
  int limit = (int)ceil(pow(max,0.5));
  for (i=0;i<limit;i++){
    if (getbit(primes,i)){
      for(j=pow(i,2);j<max;j+=i){
        clearbit(primes,j);
      }
    }
  }
  return primes;
}
int* prime_list (int max){
  byte* primes = prime_sieve(max);
  int* list = malloc(sizeof(int)*max/2);
  int cnt=0,i;
  for (i=0;i<max;i++){
    if(getbit(primes,i)){
      list[cnt]=i;
      cnt++;
    }
  }
  return list;
}
int main(void){
  byte* primes = prime_sieve(10000);
  for(int i=0;i<10000;i++){
    if (getbit(primes,i)){
      printf("%d\n",i);
    }
  }
}
