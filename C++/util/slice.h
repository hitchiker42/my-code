#ifndef __SLICE_H__
#define __SLICE_H__
#include <iterator>
#include <type_traits>
#include <utility>
namespace util {
/*
  A fixed size view of some memory region, it dosen't own any memory
  so whatever it is a slice of needs to be kept in scope. Its like
  a modifiable string_view, the size of the slice can't be changed,
  but the actual data can be.
  
  I'm not sure how useful this actually is.
*/
template<class T>
struct  slice {
  T* ptr = nullptr;
  size_t length = 0;
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

  constexpr slice() = default;
  constexpr slice(T* ptr, size_t length)
    : ptr{ptr}, length{length} {}
  constexpr slice(T* ptr, size_t start, size_t stop)
    : ptr{ptr + start}, length{stop - start} {}
  //Make a complete slice of any container that supports std::data.
  template<typename T, std::enable_if_t<is_raw_data_t<T>, int> = 0>
  constexpr slice(const T& val)
    : slice(std::data(val), std::size(val)) {}
  //Make a slice of any container that supports std::data using substr syntax.
  template<typename T, std::enable_if_t<is_raw_data_t<T>, int> = 0>
  constexpr slice(const T& val, size_t start)
    : slice(std::data(val), start, std::size(val)) {}
  template<typename T, std::enable_if_t<is_raw_data_t<T>, int> = 0>
  constexpr slice(const T& val, size_t start, size_t stop)
    : slice(std::data(val), start, stop) {}
  //Copying a slice is just a memcpy
  constexpr slice(slice&) = default;
  slice& operator= (const slice&) = default;

  //Most of what is below is adapted from the stdlibc++ array/vector headers
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
    return length;
  }
  constexpr bool empty() const noexcept {
    return size() == 0;
  }

  // Element access.
  constexpr reference operator[](size_type n) noexcept {
    return ptr[n];
  }

  constexpr const_reference operator[](size_type n) const noexcept {
    return ptr[n];
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
    return ptr;
  }

  constexpr const_pointer data() const noexcept {
    return ptr;
  }
};
}
#endif /* __SLICE_H__ */
