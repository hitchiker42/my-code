#include <stdlib.h>
#include <string.h>
typedef unsigned char byte;
//return the byte in which only the nth bit is set
//where n is the arguement
inline byte bitmask(int bit){
  return (1 << (bit % 8));
}
//get the # of the byte that contains the nth bit
inline int bitslot(int bit){
  return bit/8;
}
inline byte* clearall(byte* bit_vector,int bits){
  bzero(bit_vector,bits/8);
  return bit_vector;
}
inline byte* setall(byte* bit_vector,int bits){
  memset(bit_vector,0xff,bits/8);
  return bit_vector;
}
//set the nth bit of bit_vector and return bit_vector
inline byte* setbit(byte* bit_vector,int bit){
  bit_vector[bitslot(bit)]|=bitmask(bit);
  return bit_vector;
}
//clear the nth bit of bit_vector and return bit_vector
inline byte* clearbit(byte* bit_vector,int bit){
  bit_vector[bitslot(bit)]&= ~bitmask(bit);//remember ~ = bitwise not
  return bit_vector;
}
inline byte* flipbit(byte* bit_vector,int bit){
  bit_vector[bitslot(bit)]^=bitmask(bit);
  return bit_vector;
}
inline byte getbit(byte* bit_vector,int bit){
  return bit_vector[bitslot(bit)] & bitmask(bit);
}
inline byte* bitvector(int bits){
  byte* bytes=(byte*)calloc((bits/8),8);
  return bytes;
}
