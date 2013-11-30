#include <stdint.h>
#include <stdlib.h>
typedef union mix_word mix_word;
typedef union mix_index mix_index;
typedef struct mix MIX;
MIX *mix;//global variable for the actual `computer`
#define make_mix_word(_sign,b1,b2,b3,b4,b5)      \
  (mix_word){.sign=_sign,.byte1=b1,.byte2=b2,.byte3=b3,.byte4=b4,.byte5=b5}
#define make_mix_index(_sign,b1,b2)             \
  (mix_index){.sign=_sign,.byte1=b1,.byte2=b2};
union mix_word {
  struct {
    int sign :1;
    int fill :1;//this is just ignored
    unsigned byte1 :6;
    unsigned byte2 :6;
    unsigned byte3 :6;
    unsigned byte4 :6;
    unsigned byte5 :6;
  };
  uint32_t word;
};
enum comparison_indicator{
  _eq,
  _gt,
  _lt,
};
union mix_index {
  struct {
    int sign :1;
    int fill :3;
    unsigned byte1:6;
    unsigned byte2:6;
  };
  int16_t val;
};
struct mix {
  mix_word a;
  mix_word x;
  mix_index I[6];
  union {
    struct {
      int fill :4;
      unsigned byte1:6;
      unsigned byte2:6;
    }
      uint16_t val;
  } J;
  int overflow :1;
  enum comparision_indicator comparison;
  mix_word memory[4000];
};
//an mix instruction is:
//[+/- sign,[A,A] address,I index,F opcode modifier,C opcode id] 
//let M = [A,A] indexed by I and *M be the contents of M
enum mix_opcode {
//opcode = opcode id //num cycles, description
  NOP=0,//1 do nothing
  ADD=1,//2 
  SUB=2,//2
  MUL=3,//10
  DIV=4,//12
  LDA=8,//2 load *M into a, using the bytes of *M specified by F
  LDI1=9,//2 load bytes 4,5 from *M into I<N>(for LDI1-LDI6)
  LDI2=10,
  LDI3=11,
  LDI4=12,
  LDI5=13,
  LDI6=14,
  LDX=15,//2 like LDA but use X register
  LDAN=16,//2 load *M into a and flip the sign
  LDI1N=8,//2 load bytes 4,5 from *M into I<N> and flip sign
  LDI2N=9,
  LDI3N=10,
  LDI4N=11,
  LDI5N=12,
  LDI6N=13,
  LDXN=15,//2 like LDAN but use X register
};
void mix_operation(mix_word instruction){
  
  
