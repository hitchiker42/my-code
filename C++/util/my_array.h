//#include "common.h"
#ifndef _MY_ARRAY_H_
#define _MY_ARRAY_H_
#include <utility>
#include <initializer_list>
#include <iterator>
#include <type_traits>
#include <array>
#include <assert.h>
#include <stdlib.h>
namespace util {
template<typename T>
constexpr void constexpr_fill_n(T *ptr, size_t n, T val){
  while(n--){
    ptr[n] = val;
  }
}
template<typename T, typename U,
         std::enable_if_t<std::is_convertible_v<U,T>,int> = 0>
constexpr void constexpr_va_init(T* ptr, U val){
  *ptr = val;
}
template<typename T, typename U, typename ... Us,
         std::enable_if_t<std::is_convertible_v<std::common_type_t<U,Us...>,T>,
                        int> = 0>
constexpr void constexpr_va_init(T *ptr, U val, Us ... Args){
  *ptr++ = val;
  constexpr_va_init(ptr, Args...);
}
/*
  Static array with seperate size and length, an initalizer list constructor,
  and everything that std::array has.

  There are two main use cases:
   Creating a array of arrays of unequal length that is easy to use (using
     c-style or std::arrays you'd need a sentinal value or something similar
     to determine when an array ended).
   Emulating a vector, except with a fixed maximum size and allocated on
     the stack. Note however that regardless of the type it will be
     treated as just bytes.
*/
template<class T, size_t N>
struct array {
  //we need to provide a default initializer in order for the
  //initializer_list and variadic constructors to be constexpr.
  T arr[N] = {T(0)};
  size_t length = N;
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

  constexpr array() : length(0) {};
  constexpr array(T val) : length(N) {
    constexpr_fill_n(arr, N, val);
  }
  constexpr array(T val, size_t len) : length{len} {
    constexpr_fill_n(arr, len, val);
  }
  constexpr array(const std::initializer_list<T> v) : length(v.size()){
    T *ptr = arr;
    for(auto x : v){
      *ptr++ = x;
    }
  }
  /*
  template<typename ...Ts>
  explicit constexpr array(const Ts ... Args) : length(sizeof...(Ts)) {
    constexpr_va_init(arr, Args...);
    }*/
    
  constexpr array(array&) = default;
  array& operator= (const array&) = default;

  //Copy constructor from a type compatable array of equal or smaller size
  template<typename U, size_t M,
           std::enable_if_t<(M <= N) && std::is_convertible_v<U,T>, int> = 0>
  constexpr array(const array<U,M>& a) : length{M} {
    T *ptr = arr;
    for(auto x : a){
      *ptr++ = x;
    }
  }
  template<typename U, size_t M,
           std::enable_if_t<(M <= N) && std::is_convertible_v<U,T>, int> = 0>
  constexpr array(const std::array<U,M>& a) : length{M} {
    T *ptr = arr;
    for(auto x : a){
      *ptr++ = x;
    }
  }
  template<typename U, size_t M,
           std::enable_if_t<(M <= N) && std::is_convertible_v<U,T>, int> = 0>
  constexpr array(U const (&c_arr)[M]) : length{M} {
    T *ptr = arr;
    for(auto x : c_arr){
      *ptr++ = x;
    }
  }
  static constexpr array iota(T start , T stop, T step) {
    size_t sz = static_cast<size_t>((stop - start) / step);
    assert(sz <= N);
    array ret = array(T(), sz);
    T *ptr = ret.arr;
    T val = start;
    while (val < stop) {
      *ptr++ = val;
      val += step;
    }
    return ret;
  }
  static constexpr array iota(T stop) {
    return iota(T(0), stop, T(1));
  }
  static constexpr array iota(T start, T stop) {
    return iota(start, stop, T(1));
  }

  //Most of what is below is adapted from the stdlibc++ array header
  // Iterators.
  constexpr iterator begin() noexcept {
    return iterator(data());
  }

  constexpr const_iterator begin() const noexcept {
    return const_iterator(data());
  }

  constexpr iterator end() noexcept {
    return iterator(data() + size());
  }

  constexpr const_iterator end() const noexcept {
    return const_iterator(data() + size());
  }

  constexpr reverse_iterator rbegin() noexcept {
    return reverse_iterator(end());
  }

  constexpr const_reverse_iterator rbegin() const noexcept {
    return const_reverse_iterator(end());
  }

  constexpr reverse_iterator rend() noexcept {
    return reverse_iterator(begin());
  }

  constexpr const_reverse_iterator rend() const noexcept {
    return const_reverse_iterator(begin());
  }

  constexpr const_iterator cbegin() const noexcept {
    return const_iterator(data());
  }

  constexpr const_iterator cend() const noexcept {
    return const_iterator(data() + length);
  }

  constexpr const_reverse_iterator crbegin() const noexcept {
    return const_reverse_iterator(end());
  }

  constexpr const_reverse_iterator crend() const noexcept {
    return const_reverse_iterator(begin());
  }

  // Length
  constexpr size_type size() const noexcept {
    return length;
  }

  constexpr size_type max_size() const noexcept {
    return N;
  }
  //Different idea of capacity than std::vector.
  constexpr size_type capacity() const noexcept {
    return (max_size() - size());
  }
  constexpr bool empty() const noexcept {
    return size() == 0;
  }

  // Element access.
  constexpr reference operator[](size_type n) noexcept {
    return arr[n];
  }

  constexpr const_reference operator[](size_type n) const noexcept {
    return arr[n];
  }

  //Ommited, since I think exceptions are bad
  //constexpr reference at(size_type __n)

  constexpr reference front() noexcept {
    return *begin();
  }

  constexpr const_reference front() const noexcept {
    return *cbegin();
  }
  //back
  constexpr reference back() noexcept {
    return (length ? *(end() - 1) : *end());
  }

  constexpr const_reference back() const noexcept {
    return (length ? *(cend() - 1) : *cend());
  }

  constexpr pointer data() noexcept {
    return arr;
  }

  constexpr const_pointer data() const noexcept {
    return arr;
  }
  constexpr void clear() noexcept {
    length = 0;
  }
  //Returns false if val could not be inserted because length == N
  bool push_back(const T &val) {
    if(length == N){
      return false;
    } else {
      arr[length++] = val;
      return true;
    }
  }
  //As with a std::vector calling pop_back on an empty array is undefined
  void pop_back(){
    length--;
  }
  //push & pop functions, basically the same as push_back and pop_back
  //but return values of type T. Only really meant for simple types
  T push(const T val) {
    push_back(val);
    return val;
  }
  T pop(){
    return arr[--length];
  }
  //explicitly set length.
  bool set_length(size_t len){
    assert(len <= max_size());
    length = len;
    
    // if(len > max_size()){
    //   return false;
    // } else {
    //   length = len;
    //   return true;
    // }
  }
};
template<typename ... Ts>
explicit array(const Ts ...) -> array<std::common_type_t<Ts...>, sizeof...(Ts)>;

template <typename T, size_t N, size_t M = N>
using array_2D = array<array<T,N>,M>;
};
#endif

