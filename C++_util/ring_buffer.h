#ifndef __RING_BUFFER_H__
#define __RING_BUFFER_H__
#include <utility>
#include <initializer_list>
#include <iterator>
#include <type_traits>
#include <array>
#include <assert.h>
#include <stdlib.h>
/*
  WIP implementaion of a ring buffer using a contiguous block
  of memory as storage. Using a linked list is much eaiser, but 
  obviously slower.
*/
namespace util {
//Writing an allocator aware container is way too much work.
template<typename T>
struct ring_buffer {
  typedef T                                       value_type;
  typedef value_type*                             pointer;
  typedef const value_type*                       const_pointer;
  typedef value_type&                             reference;
  typedef const value_type&                       const_reference;
  typedef std::size_t                             size_type;
  typedef std::ptrdiff_t                          difference_type;
  typedef std::reverse_iterator<iterator>	  reverse_iterator;
  typedef std::reverse_iterator<const_iterator>   const_reverse_iterator;
  typedef Alloc allocator;
  static constexpr next_index(size_t idx, size_t sz){
    //should be much more efficent than: return (idx + 1) % sz;
    return (idx == (sz - 1) ? 0 : idx + 1);
  } 
  static constexpr prev_index(size_t idx, size_t sz){
    return (idx == 0 ? sz-1 : idx - 1);
  }  
  static constexpr add_to_index(size_t idx, size_t amt, size_t sz){
    if(amt > sz){//unlikely
      amt %= sz;
    }
    idx += amt;
    return (idx >= sz ? idx - sz : idx);
  }
  T* buf;//points to start of underlying memory.
  size_type sz;//size of underlying memory.
  
  size_type oldest;//index of oldest element
  size_type newest;//index of to newest element.
  size_type len;//number of elements, computable from oldest, newest and sz,
                //but the math is eaiser if we keep a copy of it.
#if 0 
  //this iterator iterates from the oldest element to the newest.
  struct iterator {
    const ring_buffer *buf;
    size_type idx = -1;
    iterator(const ring_buffer* rb, size_type idx = -1)
      : buf{rb}, idx{idx} {}
    iterator(const ring_buffer& rb, size_type idx = -1)
      : buf{&rb}, idx{idx} {}
    //The end iterator is represented by an index of -1, it represents
    //the point inbetween the beginning and the end of the buffer, so
    //incrementing it gives you the begin iterator.
    iterator& operator++(){
      if(idx == -1){
        idx = buf->oldest;
        return (*this);
      } else if(idx == buf->newest){
        idx = -1;
      } else {
        idx = next_index(idx, buf->sz);
      }
      return (*this);
    }
    iterator& operator--(){
      if(idx == -1){
        idx = buf->newest;
      } else if(idx == buf->oldest){
        idx = -1;
      } else {
        idx = next_index(idx, buf->sz);
      }
      return (*this);
    } 
    
  };
  size_type get_insertion_index(){
    if(len == 0){
      len++;
      return newest;
    } else if(len < sz){//if the underlying buffer isn't full.
      len++;      
      return (newest = next_index(newest, sz));
    } else {
      destruct(oldest);
      newest = oldest;
      oldest = next_index(oldest, sz);
      return newest;
    }
  }
#endif
  
  template<typename... Ts>
  void emplace_back(Ts&&... Args) {
    size_t idx = get_insertion_index();
    new (buf + idx) T(std::forward<Ts>(Args)...);
  }
  void push(const T& val){
    size_t idx = get_insertion_index();
    new(vec + idx) T(val);
  }
  void push_back(const T& val){
    push(val);
  } 
  //remove the oldest element
  void pop_back(){
    assert(len > 0);
    len--;
    destruct(oldest);
    oldest = next_index(oldest, sz);
  }
  //remove the newest element
  void pop_front(){
    assert(len > 0);
    len--;
    destruct(newest);
    newest = prev_index(newest, sz);
  }
  //Indexing is a bit weird, the way I'm doing it is that 0 is the
  //oldest element and len-1 is the newest.
  T& operator[](size_t idx){
    idx += len;
    
}
  
#endif /* __RING_BUFFER_H__ */
