#ifndef __RING_BUFFER_H__
#define __RING_BUFFER_H__
#include "typedefs.h"
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
  struct  iterator; //Forward declaration (not strictly necessary)
  typedef std::reverse_iterator<iterator>	  reverse_iterator;
  typedef std::reverse_iterator<const_iterator>   const_reverse_iterator;
  //  typedef Alloc allocator;
  T* buf = nullptr;//points to start of underlying memory.
  size_type sz = 0;//size of underlying memory.

  size_type oldest_idx;//index of oldest element
  size_type newest_idx;//index of to newest element.
  size_type len = 0;   //number of elements, computable from other members
                       //but stored for convenience

  ring_buffer(size_t sz) :
    buf((T*)malloc(sz)), sz{sz} {}
  //Initializer list constructor, still requires an explicit size
  //Assumes ls goes from newest_idx to oldest_idx
  ring_buffer(size_t sz, std::initializer_list<T> ls)
    : ring_buffer(sz), len{ls.size()} {
    T* ptr = buf + len;
    for(auto &x : ls){
      new (--ptr) T(std::move(x));
    }
  }
  constexpr size_t next_index(size_t idx) const {
    //should be much more efficent than: return (idx + 1) % sz;
    return (idx == (sz - 1) ? 0 : idx + 1);
  }
  constexpr size_t prev_index(size_t idx, size_t sz) const {
    return (idx == 0 ? sz-1 : idx - 1);
  }
  constexpr size_t add_to_index(size_t idx, size_t amt) const {
    idx += amt;
    return (idx >= sz ? idx - sz : idx);
  }
  constexpr size_t add_to_index_checked(size_t idx, size_t amt) const {
    if(amt > sz){ amt %= sz; }
    return add_to_index(idx, amt);
  }
  //Subtraction still uses unsigned integers, so we check for wraparound
  //rather than negative indexes
  constexpr size_t sub_from_index(size_t idx, size_t amt) const {
    idx -= amt;
    return (idx > sz ? sz + idx : idx);
  }
  constexpr size_t sub_from_index_checked(size_t idx, size_t amt) const {
    if(amt > sz){ amt %= sz; }
    return sub_from_index(idx, amt);
  }
  constexpr size_type get_insertion_index() const {
    if(len == 0){
      newest_idx = oldest_idx = 0;
      len++;
    } else if(len < sz){//if the underlying buffer isn't full.
      newest_idx = next_index(newest_idx, sz);
      len++;
    } else {
      destruct(oldest_idx);
      newest_idx = oldest_idx;
      oldest_idx = next_index(oldest_idx, sz);
    }
    return newest_idx;
  }
  //Returns the number of elements between newest_idx and idx.
  constexpr size_t normalize_idx(size_t idx) const {
    return (idx < newest_idx ? (idx + sz) - newest_idx : idx - newest_idx);
  }


  bool is_full() const {
    return len == sz;
  }
  void destruct([[maybe_unused]] size_type idx){
    if constexpr(std::is_destructible_v<T> &&
                 !std::is_trivially_destructible_v<T>) {
      buf[idx].~T();
    }
  }
  template<typename... Ts>
  void emplace(Ts&&... Args) {
    size_t idx = get_insertion_index();
    new (buf + idx) T(std::forward<Ts>(Args)...);
  }
  void push(const T& val){
    size_t idx = get_insertion_index();
    new(vec + idx) T(val);
  }
  //remove the oldest_idx element and return it
  T pop(){
    assert(len > 0);
    len--;
    T ret = std::move(buf[oldest_idx]);
    oldest_idx = next_index(oldest_idx, sz);
    return ret;
  }
  void pop_void(){
    assert(len > 0);
    len--;
    destruct(oldest_idx);
    oldest_idx = next_index(oldest_idx, sz);
  }
  //Return the newest/oldest elment
  T& oldest(){
    return buf[oldest_idx];
  }
  const T& oldest() const {
    return buf[oldest_idx];
  }
  T& newest(){
    return buf[newest_idx];
  }
  const T& newest() const {
    return buf[newest_idx];
  }

  //Indexing is a bit weird, the way I'm doing it is that 0 is the
  //newest_idx element and len-1 is the oldest_idx.
  T& operator[](size_t idx){
    real_idx = add_to_index(newest_idx, idx);
    return buf[idx];
  }
  const T& operator[](size_t idx) const {
    real_idx = add_to_index(newest_idx, idx);
    return buf[idx];
  }
  constexpr size_type size() const noexcept {
    return len;
  }
  constexpr size_type capacity() const noexcept {
    return sz;
  }
  //Grow or shrink the maximum number of elements the buffer holds,
  //if shrinking to less than the current length the oldest objects
  //are removed from the buffer.
  void resize(size_type new_sz){
    //Could optimize when shrinking the buffer, but the code would
    //be fairly complicated and it doesn't seem worth it.
    if(new_sz < sz){
      T *new_buf = (T*)malloc(new_sz * sizeof(T));      
      for(int i = (new_sz-1), j = newest_idx; i >= 0; i++, j = prev_index(j)){
        new (new_buf + i) T(std::move(*(buf + j)));
        destruct(j); // Unecessary if ring_buffer is going to follow svector rules
        if(j == oldest_idx){ break; }
      }
      free(buf);
      buf = new_buf;
      len = std::min(len, new_sz);
    } else if(new_sz > sz){
      buf = realloc(buf, new_sz * sizeof(T));
    }
    sz = new_sz;
  }        
  /*
    Since ring buffers are circular data structures iterators behave a little
    strangely. A special iterator contceptually representing the value
    before the begining or after the end is represted by -1. Incrementing
    this iterator gives the begin iterator and decrementing it gives the end
    iterator. This is necessary to allow iterator to work with standard
    algorithms, but also allows circular iteraton (with a bit of extra code).
  */
  //iterates from newest_idx to oldest_idx
  struct iterator {
    using value_type = T;
    using pointer = value_type*;
    using reference = value_type&;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::random_access_iterator;

    const ring_buffer *buf;
    size_type idx = -1;
    iterator(const ring_buffer* rb, size_type idx = -1)
      : buf{rb}, idx{idx} {}
    iterator(const ring_buffer& rb, size_type idx = -1)
      : buf{&rb}, idx{idx} {}
    //Distance from newest_index to idx, if idx == -1 return len.
    difference_type normalize_index() const {
      if(idx == -1){
        return buf->len;
      } else {
        return buf->normalize_index(idx);
      }
    }    
    iterator& operator++(){
      if(idx == -1){
        idx = buf->newest_idx;
      } else if(idx == buf->oldest_idx){
        idx = -1;
      } else {
        idx = buf->next_index(idx);
      }
      return (*this);
    }
    iterator& operator--(){
      if(idx == -1){
        idx = buf->oldest_idx;
      } else if(idx == buf->newest_idx){
        idx = -1;
      } else {
        idx = buf->prev_index(idx);
      }
      return (*this);
    }
    //Unlike incrementing / decrementing adding / subtracting
    //to a valid iterator ignores the 'gap' between the start and the end.
    //An iterator pointing to the 'gap' is unchanged by addition/subtraction.
    iterator& operator+=(size_t amt){
      if(idx != -1){
        idx = add_to_index(idx, amt);
      }
      return *this;
    }
    friend iterator operator+(iterator it, size_t amt){
      return it += amt;
    }
    iterator& operator-=(size_t amt){
      if(idx != -1){
        idx = sub_from_index(idx, amt);
      }
      return *this;
    }
    friend iterator operator-(iterator it, size_t amt){
      return it -= amt;
    }    
    difference_type operator-(const iterator &other) const {
      return normalize_index(idx) - other.normlize_index();
    }
    bool operator==(const iterator &other) const {
      return normalize_index() == other.normalize_index();
    }
    bool operator!=(const iterator &other) const {
      return normalize_index() != other.normalize_index();
    }
    bool operator<(const iterator &other) const {
      return normalize_index() < other.normalize_index();
    }
  };
  iterator begin(){
    return iterator(this, newest_idx);
  }
  iterator end(){
    return iterator(this, -1);
  }
  //[push/pop_]front/back provided to allow using a ring buffer
  //as a std::queue / std::stack, the semantics of front/back
  //are a little weird (back is the newest element and
  //front is teh oldest) so these shouldn't be used explicitly.
  T& back(){
    return buf[newest_idx];
  }
  const T& back() const {
    return buf[newest_idx];
  }
  T& front(){
    return buf[oldest_idx];
  }
  const T& back() const {
    return buf[oldest_idx];
  }
  void push_back(const T& val){
    push(val);
  }
  void pop_back(){
    assert(len > 0);
    len--;
    destruct(newest_idx);
    newest_idx = prev_index(newest_idx, sz);
  }
  void pop_front(){
    pop_void();
  }
}
}

#endif /* __RING_BUFFER_H__ */
