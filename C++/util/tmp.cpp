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
