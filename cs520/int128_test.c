#include <stdio.h>
#include <stdint.h>
typedef __int128 int128_t;
typedef unsigned __int128 uint128_t;
typedef union word128 word128;
union word128 {
  uint128_t uint128;
  struct {
    uint64_t low;
    uint64_t high;
  };
  struct {
    uint32_t one;
    uint32_t two;
    uint32_t three;
    uint32_t four;
  };
  uint16_t words[8];
  uint8_t bytes[16];
};
int print_uint128(uint128_t n) {
  char str[40] = {0}; // log10(1 << 128) + '\0'
  char *s = str + sizeof(str) - 1; // start at the end
  while (n != 0) {
    if (s == str) return -1; // never happens

    *--s = "0123456789abcdef"[n % 16]; // save last digit
    n >>= 4;                     // drop it
  }
  while(s-str>8){
    *--s='0';
  }
  *--s='x';
  *--s='0';
  return printf("%s\n", s);
}
int main(){
  word128 test;
  int i;
  for(i=0;i<8;i++){
    test.bytes[i]=1<<i;
    test.bytes[i+8]=-(1<<i);
  }
  //should set test to
  //0x1 0x2 0x4 0x8 0x10 0x20 0x40 0x80
  //0xfe 0xfd 0xfb 0xf7 0xef 0xdf 0xbf 0x7f
  print_uint128(test.uint128);
  printf("\n%#016lx %#016lx\n",test.high,test.low);
  printf("%#08x %#08x %#08x %#08x\n",test.four,test.three,test.two,test.one);
  printf("%#06hx %#06hx %#06hx %#06hx %#06hx %#06hx %#06hx %#06hx\n",
         test.words[0],test.words[1],test.words[2],test.words[3],
         test.words[4],test.words[5],test.words[6],test.words[7]);
  printf("%#04hhx %#04hhx %#04hhx %#04hhx %#04hhx %#04hhx %#04hhx %#04hhx "
         "%#04hhx %#04hhx %#04hhx %#04hhx %#04hhx %#04hhx %#04hhx %#04hhx\n",
         test.bytes[0],test.bytes[1],test.bytes[2],test.bytes[3],
         test.bytes[4],test.bytes[5],test.bytes[6],test.bytes[7],
         test.bytes[8],test.bytes[9],test.bytes[10],test.bytes[11],
         test.bytes[12],test.bytes[13],test.bytes[14],test.bytes[15]);
  return 0;
}
