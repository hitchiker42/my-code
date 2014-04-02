static const uint64_t PAGESIZE=4096;
static const uint64_t buf_size=128*(1<<10);//128kb
static const uint64_t min_buf_size=8*(1<<10);//8Kb
static const uint64_t max_buf_size=136*(1<<10);//128+8Kb
static const uint64_t const_zero_64=0;
static const uint32_t const_zero_32=0;
static const uint64_t const_one_64=1;
static const uint32_t const_one_32=1;
#define MAX_BUF_SIZE (136*(1<<10))
//global initialized arrays
/*1 if char is in the set [A-Za-z] zero otherwise */
static const uint8_t eng_accept[256]=
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
   0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
   1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
//the Nth entry consists of a 64 bit integer with the least significant
//N+1 bits set and the rest of the bits unset
static const uint64_t file_bit_strings[64] =
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
//the Nth entry is a 64 bit integer with only the Nth bit set
static const uint64_t file_bit_masks[64] = 
  {0x1, 0x2, 0x4, 0x8,
   0x10, 0x20, 0x40, 0x80,
   0x100, 0x200, 0x400, 0x800,
   0x1000, 0x2000, 0x4000, 0x8000,
   0x10000, 0x20000, 0x40000, 0x80000,
   0x100000, 0x200000, 0x400000, 0x800000,
   0x1000000, 0x2000000, 0x4000000, 0x8000000,
   0x10000000, 0x20000000, 0x40000000, 0x80000000,
   0x100000000, 0x200000000, 0x400000000, 0x800000000,
   0x1000000000, 0x2000000000, 0x4000000000, 0x8000000000,
   0x10000000000, 0x20000000000, 0x40000000000, 0x80000000000,
   0x100000000000, 0x200000000000, 0x400000000000, 0x800000000000,
   0x1000000000000, 0x2000000000000, 0x4000000000000, 0x8000000000000,
   0x10000000000000, 0x20000000000000, 0x40000000000000, 0x80000000000000,
   0x100000000000000, 0x200000000000000, 0x400000000000000, 0x800000000000000,
   0x1000000000000000, 0x2000000000000000, 0x4000000000000000, 0x8000000000000000};
