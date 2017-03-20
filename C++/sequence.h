#include <stdint.h>
#define make_obj_constructor(name, type, field)         \
  static obj make_##name##_obj(type x){                 \
    obj y;                                              \
    y.field = x;                                        \
    return y;                                           \
  }
//type for anything that fits in 64 bits or less
union obj {
  void *ptr;
  int64_t int64;
  uint64_t uint64;
  double dbl;
  int32_t int32;
  uint32_t uint32;
  float flt;
  int16_t int16;
  uint16_t uint16;
  int8_t int8;e
  uint8_t uint8;
  char chr;
};
make_obj_constructor(int64, int64_t, int64);
make_obj_constructor(uint64, uint64_t, uint64);
make_obj_constructor(double, double, dbl);
//type for anything that fits in exactly 64 bits;
union obj_64 {
  void *ptr;
  int64_t int64;
  uint64_t uint64;
  double dbl;
  int32_t int32[2];
  uint32_t uint32[2];
  float flt[2];
  int16_t int16[4];
  uint16_t uint16[4];
  int8_t int8[8];
  uint8_t uint8[8];
  char chr[8];
};

  
template<typename T> struct sequence {
  virtual long length() = 0;
  virtual T elt() = 0;
  virtual sequence<T> head(int n) = 0;
  virtual sequence<T> tail(int n) = 0;
  virtual sequence<T> subseq(int n, int m) = 0;
  virtual sequence<T> map(T(*fp)(sequence<T>)) = 0;
};

// Local Variables:
// mode: c++
// End:
