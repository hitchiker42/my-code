//static const uint64_t PAGESIZE=4096;
static const uint64_t buf_size=128*(1<<10);//128kb
static const uint64_t min_buf_size=8*(1<<10);//8Kb
static const uint64_t max_buf_size=136*(1<<10);//128+8Kb
//global initialized arrays
/*1 if char is in the set [A-Za-z] zero otherwise */
static const uint8_t eng_accept[256]=
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
   1,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
   1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
/* Quite a few of the following lines are well over 80 characters per line
   that's why this is a seperate file, trying to fit all of these constants
   into 80 characters per line would be needlessly difficult
*/
//the Nth entry consists of a 64 bit integer with the least significant
//N+1 bits set and the rest of the bits unset
static const uint64_t file_bit_strings64[64] =
  {0x1, 0x3, 0x7, 0xf,
   0x1f, 0x3f, 0x7f, 0xff,
   0x1ff, 0x3ff, 0x7ff, 0xfff,
   0x1fff, 0x3fff, 0x7fff, 0xffff,
   0x1ffff, 0x3ffff, 0x7ffff, 0xfffff,
   0x1fffff, 0x3fffff, 0x7fffff, 0xffffff,
   0x1ffffff, 0x3ffffff, 0x7ffffff, 0xfffffff,
   0x1fffffff, 0x3fffffff, 0x7fffffff, 0xffffffff,
   0x1ffffffff, 0x3ffffffff, 0x7ffffffff, 0xfffffffff,
   0x1fffffffff, 0x3fffffffff, 0x7fffffffff, 0xffffffffff,
   0x1ffffffffff, 0x3ffffffffff, 0x7ffffffffff, 0xfffffffffff,
   0x1fffffffffff, 0x3fffffffffff, 0x7fffffffffff, 0xffffffffffff,
   0x1ffffffffffff, 0x3ffffffffffff, 0x7ffffffffffff, 0xfffffffffffff,
   0x1fffffffffffff, 0x3fffffffffffff, 0x7fffffffffffff, 0xffffffffffffff,
   0x1ffffffffffffff, 0x3ffffffffffffff, 0x7ffffffffffffff, 0xfffffffffffffff,
   0x1fffffffffffffff, 0x3fffffffffffffff, 0x7fffffffffffffff, 0xffffffffffffffff};
static const file_bitfield file_bit_strings[128] ={
  {.low=0x1},{.low=0x3},
  {.low=0x7},{.low=0xf},
  {.low=0x1f},{.low=0x3f},
  {.low=0x7f},{.low=0xff},
  {.low=0x1ff},{.low=0x3ff},
  {.low=0x7ff},{.low=0xfff},
  {.low=0x1fff},{.low=0x3fff},
  {.low=0x7fff},{.low=0xffff},
  {.low=0x1ffff},{.low=0x3ffff},
  {.low=0x7ffff},{.low=0xfffff},
  {.low=0x1fffff},{.low=0x3fffff},
  {.low=0x7fffff},{.low=0xffffff},
  {.low=0x1ffffff},{.low=0x3ffffff},
  {.low=0x7ffffff},{.low=0xfffffff},
  {.low=0x1fffffff},{.low=0x3fffffff},
  {.low=0x7fffffff},{.low=0xffffffff},
  {.low=0x1ffffffff},{.low=0x3ffffffff},
  {.low=0x7ffffffff},{.low=0xfffffffff},
  {.low=0x1fffffffff},{.low=0x3fffffffff},
  {.low=0x7fffffffff},{.low=0xffffffffff},
  {.low=0x1ffffffffff},{.low=0x3ffffffffff},
  {.low=0x7ffffffffff},{.low=0xfffffffffff},
  {.low=0x1fffffffffff},{.low=0x3fffffffffff},
  {.low=0x7fffffffffff},{.low=0xffffffffffff},
  {.low=0x1ffffffffffff},{.low=0x3ffffffffffff},
  {.low=0x7ffffffffffff},{.low=0xfffffffffffff},
  {.low=0x1fffffffffffff},{.low=0x3fffffffffffff},
  {.low=0x7fffffffffffff},{.low=0xffffffffffffff},
  {.high=0x1,.low=0xffffffffffffffff},{.high=0x3,.low=0xffffffffffffffff},
  {.high=0x7,.low=0xffffffffffffffff},{.high=0xf,.low=0xffffffffffffffff},
  {.high=0x1f,.low=0xffffffffffffffff},{.high=0x3f,.low=0xffffffffffffffff},
  {.high=0x7f,.low=0xffffffffffffffff},{.high=0xff,.low=0xffffffffffffffff},
  {.high=0x1ff,.low=0xffffffffffffffff},{.high=0x3ff,.low=0xffffffffffffffff},
  {.high=0x7ff,.low=0xffffffffffffffff},{.high=0xfff,.low=0xffffffffffffffff},
  {.high=0x1fff,.low=0xffffffffffffffff},{.high=0x3fff,.low=0xffffffffffffffff},
  {.high=0x7fff,.low=0xffffffffffffffff},{.high=0xffff,.low=0xffffffffffffffff},
  {.high=0x1ffff,.low=0xffffffffffffffff},{.high=0x3ffff,.low=0xffffffffffffffff},
  {.high=0x7ffff,.low=0xffffffffffffffff},{.high=0xfffff,.low=0xffffffffffffffff},
  {.high=0x1fffff,.low=0xffffffffffffffff},{.high=0x3fffff,.low=0xffffffffffffffff},
  {.high=0x7fffff,.low=0xffffffffffffffff},{.high=0xffffff,.low=0xffffffffffffffff},
  {.high=0x1ffffff,.low=0xffffffffffffffff},{.high=0x3ffffff,.low=0xffffffffffffffff},
  {.high=0x7ffffff,.low=0xffffffffffffffff},{.high=0xfffffff,.low=0xffffffffffffffff},
  {.high=0x1fffffff,.low=0xffffffffffffffff},{.high=0x3fffffff,.low=0xffffffffffffffff},
  {.high=0x7fffffff,.low=0xffffffffffffffff},{.high=0xffffffff,.low=0xffffffffffffffff},
  {.high=0x1ffffffff,.low=0xffffffffffffffff},{.high=0x3ffffffff,.low=0xffffffffffffffff},
  {.high=0x7ffffffff,.low=0xffffffffffffffff},{.high=0xfffffffff,.low=0xffffffffffffffff},
  {.high=0x1fffffffff,.low=0xffffffffffffffff},{.high=0x3fffffffff,.low=0xffffffffffffffff},
  {.high=0x7fffffffff,.low=0xffffffffffffffff},{.high=0xffffffffff,.low=0xffffffffffffffff},
  {.high=0x1ffffffffff,.low=0xffffffffffffffff},{.high=0x3ffffffffff,.low=0xffffffffffffffff},
  {.high=0x7ffffffffff,.low=0xffffffffffffffff},{.high=0xfffffffffff,.low=0xffffffffffffffff},
  {.high=0x1fffffffffff,.low=0xffffffffffffffff},{.high=0x3fffffffffff,.low=0xffffffffffffffff},
  {.high=0x7fffffffffff,.low=0xffffffffffffffff},{.high=0xffffffffffff,.low=0xffffffffffffffff},
  {.high=0x1ffffffffffff,.low=0xffffffffffffffff},{.high=0x3ffffffffffff,.low=0xffffffffffffffff},
  {.high=0x7ffffffffffff,.low=0xffffffffffffffff},{.high=0xfffffffffffff,.low=0xffffffffffffffff},
  {.high=0x1fffffffffffff,.low=0xffffffffffffffff},{.high=0x3fffffffffffff,.low=0xffffffffffffffff},
  {.high=0x7fffffffffffff,.low=0xffffffffffffffff},{.high=0xffffffffffffff,.low=0xffffffffffffffff}};
//the Nth entry is a 64 bit integer with only the Nth bit set
static const uint64_t file_bit_masks64[64] = {
  0x1,0x2,0x4,0x8,//4
  0x10,0x20,0x40,0x80,//8
  0x100,0x200,0x400,0x800,//12
  0x1000,0x2000,0x4000,0x8000,//16
  0x10000,0x20000,0x40000,0x80000,//20
  0x100000,0x200000,0x400000,0x800000,//24
  0x1000000,0x2000000,0x4000000,0x8000000,//28
  0x10000000,0x20000000,0x40000000,0x80000000,//32
  0x100000000,0x200000000,0x400000000,0x800000000,//36
  0x1000000000,0x2000000000,0x4000000000,0x8000000000,//40
  0x10000000000,0x20000000000,0x40000000000,0x80000000000,//44
  0x100000000000,0x200000000000,0x400000000000,0x800000000000,//48
  0x1000000000000,0x2000000000000,0x4000000000000,0x8000000000000,//52
  0x10000000000000,0x20000000000000,0x40000000000000,0x80000000000000,//56
  0x100000000000000,0x200000000000000,0x400000000000000,0x800000000000000,//60
  0x1000000000000000,0x2000000000000000,0x4000000000000000,0x8000000000000000};
static const file_bitfield file_bit_masks[128]={
  {.low=(long)1<<0}, {.low=(long)1<<1},{.low=(long)1<<2},{.low=(long)1<<3},
  {.low=(long)1<<4}, {.low=(long)1<<5}, {.low=(long)1<<6}, {.low=(long)1<<7},
  {.low=(long)1<<8}, {.low=(long)1<<9}, {.low=(long)1<<10},{.low=(long)1<<11},
  {.low=(long)1<<12},{.low=(long)1<<13},{.low=(long)1<<14},{.low=(long)1<<15},
  {.low=(long)1<<16},{.low=(long)1<<17},{.low=(long)1<<18},{.low=(long)1<<19},
  {.low=(long)1<<20},{.low=(long)1<<21},{.low=(long)1<<22},{.low=(long)1<<23},
  {.low=(long)1<<24},{.low=(long)1<<25},{.low=(long)1<<26},{.low=(long)1<<27},
  {.low=(long)1<<28},{.low=(long)1<<29},{.low=(long)1<<30},{.low=(long)1<<31},
  {.low=(long)1<<32},{.low=(long)1<<33},{.low=(long)1<<34},{.low=(long)1<<35},
  {.low=(long)1<<36},{.low=(long)1<<37},{.low=(long)1<<38},{.low=(long)1<<39},
  {.low=(long)1<<40},{.low=(long)1<<41},{.low=(long)1<<42},{.low=(long)1<<43},
  {.low=(long)1<<44},{.low=(long)1<<45},{.low=(long)1<<46},{.low=(long)1<<47},
  {.low=(long)1<<48},{.low=(long)1<<49},{.low=(long)1<<50},{.low=(long)1<<51},
  {.low=(long)1<<52},{.low=(long)1<<53},{.low=(long)1<<54},{.low=(long)1<<55},
  {.low=(long)1<<56},{.low=(long)1<<57},{.low=(long)1<<58},{.low=(long)1<<59},
  {.low=(long)1<<60},{.low=(long)1<<61},{.low=(long)1<<62},{.low=(long)1<<63},
  {.high=(long)1<<0}, {.high=(long)1<<1},{.high=(long)1<<2},{.high=(long)1<<3},
  {.high=(long)1<<4}, {.high=(long)1<<5}, {.high=(long)1<<6}, {.high=(long)1<<7},
  {.high=(long)1<<8}, {.high=(long)1<<9}, {.high=(long)1<<10},{.high=(long)1<<11},
  {.high=(long)1<<12},{.high=(long)1<<13},{.high=(long)1<<14},{.high=(long)1<<15},
  {.high=(long)1<<16},{.high=(long)1<<17},{.high=(long)1<<18},{.high=(long)1<<19},
  {.high=(long)1<<20},{.high=(long)1<<21},{.high=(long)1<<22},{.high=(long)1<<23},
  {.high=(long)1<<24},{.high=(long)1<<25},{.high=(long)1<<26},{.high=(long)1<<27},
  {.high=(long)1<<28},{.high=(long)1<<29},{.high=(long)1<<30},{.high=(long)1<<31},
  {.high=(long)1<<32},{.high=(long)1<<33},{.high=(long)1<<34},{.high=(long)1<<35},
  {.high=(long)1<<36},{.high=(long)1<<37},{.high=(long)1<<38},{.high=(long)1<<39},
  {.high=(long)1<<40},{.high=(long)1<<41},{.high=(long)1<<42},{.high=(long)1<<43},
  {.high=(long)1<<44},{.high=(long)1<<45},{.high=(long)1<<46},{.high=(long)1<<47},
  {.high=(long)1<<48},{.high=(long)1<<49},{.high=(long)1<<50},{.high=(long)1<<51},
  {.high=(long)1<<52},{.high=(long)1<<53},{.high=(long)1<<54},{.high=(long)1<<55},
  {.high=(long)1<<56},{.high=(long)1<<57},{.high=(long)1<<58},{.high=(long)1<<59},
  {.high=(long)1<<60},{.high=(long)1<<61},{.high=(long)1<<62},{.high=(long)1<<63}};
