#include <stdint.h>
#include <array>
size_t utf8_encode_char_unchecked(int32_t c, uint8_t *buf){  
  if(c < 0x80) {
    *buf++ = c;
    return 1;
  } else if(c < 0x800) {
    *buf++ = 0xC0 | ((c >> 6) & 0x1F);
    *buf++ = 0x80 | (c & 0x3F);
    return 2;
  } else if(c < 0x10000) {
    *buf++ = 0xE0 | ((c >> 12) & 0xF);
    *buf++ = 0x80 | ((c >> 6) & 0x3F);
    *buf++ = 0x80 | (c & 0x3F);
    return 3;
  } else if(c < 0x200000) {
    *buf++ = 0xF0 | ((c >> 18) & 0x7);
    *buf++ = 0x80 | ((c >> 12) & 0x3F);
    *buf++ = 0x80 | ((c >> 6) & 0x3F);
    *buf++ = 0x80 | (c & 0x3F);
    return 4;
  }
}
size_t utf8_encode_char_unchecked_2(int32_t c, uint8_t *buf){  
  int shift = -6;
  if(c < 0x80) {
    *buf++ = c;
  } else if(c < 0x800) {
    *buf++ = 0xC0 | ((c >> 6) & 0x1F);
    shift = 0;
  } else if(c < 0x10000) {
    *buf++ = 0xE0 | ((c >> 12) & 0xF);
    shift = 6;
  } else if(c < 0x200000) {
    *buf++ = 0xF0 | ((c >> 18) & 0x7);
    shift = 12;
  }
  int ret = 1;
  while(shift >= 0){
    *buf++ = 0x80 | ((c >> shift) & 0x3F);
    shift-=6;
    ret++;
  }
  return ret;
}
#if 0
static inline constexpr size_t max_utf8_char_size = 4;
inline constexpr std::array<int8_t, 256> utf8_char_size_table = {{
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4,    
  //5, 5, 5, 5, 6, 6, //These are possible but not used
   -1,-1,-1,-1,-1,-1,-1,-1
  }};
int32_t utf8_decode_char_unchecked(const uint8_t *src){
  static constexpr std::array<uint8_t, max_utf8_char_size> utf8_lead_char_mask =
    {{0x7f, 0x1F, 0x0F, 0x07}};
  int remain = utf8_char_size_table[*src] - 1;
  int32_t ret = (*src++) & utf8_lead_char_mask[remain];
  while(remain--){
    ret <<= 6;
    ret |= (*src++) & 0x3F;
  }
  return static_cast<int32_t>(ret);
}
#if 0
#include "templates.h"
template<typename T>
using alloc_traits_ptr = typename std::allocator_traits<T>::pointer;
template<typename T>
using alloc_traits_size_t = typename std::allocator_traits<T>::size_type;


template <class Allocator>
alloc_traits_ptr<Allocator>
reallocate(alloc_traits_ptr<Allocator> ptr,
           alloc_traits_size_t<Allocator> new_size,
           alloc_traits_size_t<Allocator> old_length = -1,
           alloc_traits_size_t<Allocator> old_size = -1){
  using pointer = alloc_traits_ptr<Allocator>;
  using size_type = alloc_traits_size_t<Allocator>;
  //I'm too lazy to support stateful allocators
  static_assert(std::allocator_traits<Allocator>::is_always_equal::value,
                "Allocator is not stateless");
  Allocator a;
  if(old_size = (size_type)-1){
    old_size = old_length;
  }
  if constexpr(std::is_invokable_r_v<pointer, a.reallocate, 
                                     pointer, size_type, size_type, size_type>){
    return a.reallocate(ptr, new_size, old_length, old_size);
  } else {
    pointer ret = a.allocate(new_size);//a.allocate has to be defined
    for(size_type i = 0; i < old_length; i++){
      std::allocator_traits<Allocator>::construct(a, ret + i, 
                                                  std::move(*(pointer + i)));
      std::allocator_traits<Allocator>::destroy(a, pointer + i);
    }
    a.deallocate(pointer);
    return ret;
  }
}
#endif
#endif
