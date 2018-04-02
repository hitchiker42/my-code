#ifndef __SVECTOR_H__
#define __SVECTOR_H__
#include "my_array.h"
#include "macros.h"
#include <utility>
#include <type_traits>
namespace util {
/*
  A compromise between a vector which treats it's contents as just bytes
  and std::vector.

  T doesn't need to be trivially destructable, but if a value of type T
    is moved from a to b, (i.e T a = std::move(b)), b needs to be trivially
    destructable afterward.

  Given 2 pointers to T, a & b,
    memcpy(a, b, sizeof(T)); free(b); //should be identical to
    a = std::move(b); free(b);

  When resizing the vector the old contents are moved into new memory, and
   the old memory is freed without calling any destructors.

  When the vector itself is destroyed it will destruct all of its elements.

  For example std::unique_ptr could be used safely with an svector.

  Offers various unsafe operations:
    Giving / Setting the underlying block of memory.
    directly setting the length, without constructing/destructing
      any elements added/removed.

  Memory is managed using malloc/free, not new/delete.
*/
template<typename T>
struct svector {
  typedef T                                       value_type;
  typedef value_type*                             pointer;
  typedef const value_type*                       const_pointer;
  typedef value_type&                             reference;
  typedef const value_type&                       const_reference;
  typedef value_type*                             iterator;
  typedef const value_type*			  const_iterator;
  typedef std::size_t                             size_type;
  typedef std::ptrdiff_t                          difference_type;
  typedef std::reverse_iterator<iterator>	  reverse_iterator;
  typedef std::reverse_iterator<const_iterator>   const_reverse_iterator;

  T *vec = nullptr;
  size_t len = 0;
  size_t sz = 0;

  svector() = default;
  //Create an svector from an existing block of memory
  svector(T *vec, size_t len, size_t sz) noexcept
    : vec{vec}, len{len}, sz{sz} {}
  //Allocate an svector that can hold at least cnt elements of type T
  svector(size_t cnt) noexcept {
    sz = NEAREST_POW_OF_2(cnt);
    len = 0;
    vec = (T*)calloc(sz,sizeof(T));
  }
  svector(size_t cnt, T val) noexcept : svector(cnt) {
    len = cnt;
    for(size_t i = 0; i < len; ++i){
      vec[i] = val;
    }
  }
  svector(const svector& other) noexcept : svector(other.size()) {
    len = other.size();
    for(size_t i = 0; i < len; i++){
      vec[i] = other[i];
    }
  }
  svector(svector&& other) noexcept : svector(other.vec, other.len, other.sz) {
    other.len = 0;
  }
  svector shallow_copy(){
    return svector(vec, len, sz);
  }
  //Destruct the element at idx if it has a non-trivial destructor,
  //otherwise do nothing.
  void destruct([[maybe_unused]] size_type idx){
    if constexpr(std::is_destructible_v<T> &&
                 !std::is_trivially_destructible_v<T>) {
      vec[idx].~T();
    }
  }
  //Destructs all the elements in the range [start, end)
  void destruct(size_type start, size_type end){
    for(size_t i = start; i < end; i++){
      destruct(i);
    }
  }

  ~svector(){
    clear();
    free((void*)vec);
  }

  template<typename... Ts>
  void emplace_back(Ts&&... Args) {
    check_length();
    new (vec+len) T(std::forward<Ts>(Args)...);
    ++len;
  }
  void push(T val){
    check_length();
    vec[len++] = val;
  }
  void push_back(T val){push(val);}
  //Moves the last element out of the vector and returns it
  T pop(){
    //len < sz checks to see if len has wrapped around
    assert(len > 0 && len < sz);
    return std::move(vec[--len]);
  }
  void pop_back(){
    assert(len > 0 && len < sz);
    len--;
    destruct(len);
  }
  void clear(){
    if(len > 0) {
      destruct(0, len);
      len = 0;
    }
  }
  //Calling resize with no specific value will zero the new elements bytewise,
  void resize(size_type new_len){
    if(new_len < len){
      destruct(new_len, len);
    } else if(new_len > len) {
      //Only zero if we didn't need to reallocate, since we use calloc.
      if(!check_length(new_len-len)){
        zero_elements(len, new_len);
      }
    }
    len = new_len;
  } 
  void resize(size_type new_len, const T& val){
    if(new_len < len){
      destruct(new_len, len);
    } else if(new_len > len) {
      check_length(new_len-len);
      set_elements(len, new_len, val);
    }
    len = new_len;
  }

  //zero the memory from vec+start to vec+end, does not destruct
  //any values that may be in this memory.
  void zero_elements(size_type start, size_type end){
    memset(vec + start, '\0', end*sizeof(T));
  }
  //set [start,end) to val, does not destruct any of the current elements
  void set_elements(size_type start, size_type end, const T& val){
    for(size_type i = start; i < end; ++i){
      vec[i] = val;
    }
  }
  //Make sure we can add another element, returns true if memory was reallocated
  bool check_length(){
    return check_length(1);
  }
  bool check_length(size_t count){
    if((len + count-1) < sz){ return false; }
    //if vector is empty allocate space for 8 elements, NEXT_POW_OF_2
    //doesn't work with a 0 argument.
    size_type new_sz = (sz ? NEXT_POW_OF_2(sz+count)
                           : std::max(count,size_type(8)));
    reallocate(new_sz);
    return true;
  }

  //Sets sz to new_sz, moves the first len elements of vec to a new block
  //of memory large enough to hold sz elements, and frees the old memory.
  void reallocate(size_type new_sz){
    sz = new_sz;
    T* new_vec = (T*)calloc(sz, sizeof(T));
    memcpy(new_vec, vec, len*sizeof(T));
    free(vec);
    vec = new_vec;
  }
//Unsafe Functions
  T* take_memory(){
    T* ret = vec;
    vec = nullptr;
    len = sz = 0;
    return ret;
  }
  //This function seems kinda silly, now that I look at it.
  void give_memory(T *mem, ssize_t new_len, ssize_t new_sz = -1){
    if(new_sz == -1){ new_sz = new_len; }
    if(len > 0) { this->~svector(); }
    new(this) svector(mem, new_len, new_sz);
  }
  //Explictly change the length without modifying the contents.
  //new_len must be < capacity() && any elements added/removed due to
  //the length change are not initialized/destructed.
  bool set_length(size_t new_len){
    if(new_len > sz){
      return false;
    } else {
      len = new_len;
    }
  }

  //Standard container functions
  iterator begin() noexcept {
    return iterator(data());
  }
  const_iterator begin() const noexcept {
    return const_iterator(data());
  }
  iterator end() noexcept {
    return iterator(data() + len);
  }
  const_iterator end() const noexcept {
    return const_iterator(data() + len);
  }

  reverse_iterator rbegin() noexcept {
    return reverse_iterator(end());
  }
  const_reverse_iterator rbegin() const noexcept {
    return const_reverse_iterator(end());
  }
  reverse_iterator rend() noexcept {
    return reverse_iterator(begin());
  }
  const_reverse_iterator rend() const noexcept {
    return const_reverse_iterator(begin());
  }

  const_iterator cbegin() const noexcept {
    return const_iterator(data());
  }
  const_iterator cend() const noexcept {
    return const_iterator(data() + len);
  }
  const_reverse_iterator crbegin() const noexcept {
    return const_reverse_iterator(end());
  }
  const_reverse_iterator crend() const noexcept {
    return const_reverse_iterator(begin());
  }

  // Length
  size_type size() const noexcept {
    return len;
  }
  size_type capacity() const noexcept {
    return sz;
  }
  size_type max_size() const noexcept {
    return std::numeric_limits<size_type>::max();
  }
  void reserve(size_type new_sz){
    if(new_sz > sz){
      reallocate(new_sz);
    }
  }


  bool empty() const noexcept {
    return size() == 0;
  }
  bool full() const noexcept {
    return size() == capacity();
  }

  // Element access.
  reference operator[](size_type n) noexcept {
    return vec[n];
  }
  const_reference operator[](size_type n) const noexcept {
    return vec[n];
  }

  //Ommited, since I think exceptions are bad
  //reference at(size_type __n)

  reference front() noexcept {
    return *begin();
  }
  const_reference front() const noexcept {
    return *cbegin();
  }

  reference back() noexcept {
    return (len ? *(end() - 1) : *end());
  }
  const_reference back() const noexcept {
    return (len ? *(cend() - 1) : *cend());
  }

  pointer data() noexcept {
    return vec;
  }
  const_pointer data() const noexcept {
    return vec;
  }
};
template <typename T>
using svector_2d = svector<svector<T>>;
}

#endif /* __SVECTOR_H__ */
