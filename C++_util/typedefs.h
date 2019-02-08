#ifndef __TYPEDEFS_H__
#define __TYPEDEFS_H__
//Maybe rename this to something like type_utils.h
#include <type_traits>
#include <iterator> //iterator_traits
#include <utility> //declval
#include <stdint.h> //typedefs
/*
  Typedefs and a couple simple templates.
  This is included by templates.h and is mostly for use by other
  headers to avoid including all the template functions defined in templates.h
*/
namespace util {
inline //on a seperate line since 'inline namespace' confuses emacs
namespace templates {
//An actual reinterpret cast, interprets the bits of source as type Dest.
template <typename Dest, typename Source>
inline Dest bit_cast(const Source& source) {
  static_assert(sizeof(Dest) == sizeof(Source),
                "bitcast requires source and destination to be the same size");
  //Could probably get rid of these next two asserts
  static_assert(std::is_trivially_copyable_v<Dest>,
                "bitcast requires the destination type to be copyable");
  static_assert(std::is_trivially_copyable_v<Source>,
                "bitcast requires the source type to be copyable");
  Dest dest;
  memcpy(&dest, &source, sizeof(dest));
  return dest;
}
/*
  convience typedefs for iterator traits (avoids having to add 'typename')
*/
template <class It>
using iter_traits_value_type = typename std::iterator_traits<It>::value_type;
template <class It>
using iter_traits_difference_type =
  typename std::iterator_traits<It>::difference_type;
template <class It>
using iter_traits_reference = typename std::iterator_traits<It>::reference;
template <class It>
using iter_traits_pointer = typename std::iterator_traits<It>::pointer;
template <class It>
using iter_traits_iterator_category =
  typename std::iterator_traits<It>::iterator_category;
template <class It>
using iter_traits_category = iter_traits_iterator_category<It>;


template <class It>
struct is_input_iterator :
    std::is_base_of<std::input_iterator_tag,
                    iter_traits_iterator_category<It>> {};
template <class It>
inline constexpr bool is_input_iterator_v = is_input_iterator<It>::value;

template <class It>
struct is_forward_iterator :
    std::is_base_of<std::forward_iterator_tag,
                    iter_traits_iterator_category<It>> {};
template <class It>
inline constexpr bool is_forward_iterator_v = is_forward_iterator<It>::value;

template <class It>
struct is_bidirectional_iterator :
    std::is_base_of<std::bidirectional_iterator_tag,
                    iter_traits_iterator_category<It>> {};
template <class It>
inline constexpr bool is_bidirectional_iterator_v = is_bidirectional_iterator<It>::value;

template <class It>
struct is_random_access_iterator :
    std::is_base_of<std::random_access_iterator_tag,
                    iter_traits_iterator_category<It>> {};
template <class It>
inline constexpr bool is_random_access_iterator_v = is_random_access_iterator<It>::value;

/*
  If T is a pointer type provides the member typedef type T otherwise
  provides the member typedef type which is std::add_pointer_t<T>.
*/
template <typename T>
struct make_pointer {
  using type = std::conditional_t<std::is_pointer_v<T>, T, 
                                  std::add_pointer_t<T>>;
};
template <typename T>
using make_pointer_t = typename make_pointer<T>::type;

template<int N,
         std::enable_if_t<N <= sizeof(uintmax_t)/CHAR_BIT, int> = 0>
struct nbyte_int {
  using signed_type = 
    std::conditional_t<N <= 1, int8_t,
      std::conditional_t<N <= 2, int16_t,
        std::conditional_t<N <= 4, int32_t,
          std::conditional_t<N <= 8, int64_t, intmax_t>>>>;
  using unsigned_type = 
    std::conditional_t<N <= 1, uint8_t,
      std::conditional_t<N <= 2, uint16_t,
        std::conditional_t<N <= 4, uint32_t,
          std::conditional_t<N <= 8, uint64_t, intmax_t>>>>;
};
template<int N>
using nbyte_sint_t = typename nbyte_int<N>::signed_type;
template<int N>
using nbyte_uint_t = typename nbyte_int<N>::unsigned_type;
//Convert an enum to its underlying representation
template <typename T>
constexpr auto to_underlying(T e){
  return static_cast<std::underlying_type_t<T>>(e);
}
/*
  Functions for mapping over stl containers.
*/
/*
  Template functions for containers / Iterators.
*/
//is_iterable_v<T> is true if std::begin, std::end can be called with T.
template<typename T, typename = void>
struct is_iterable : std::false_type {};
template<typename T>
struct is_iterable<
  T, std::void_t<decltype(std::begin(std::declval<T>())),
                 decltype(std::end(std::declval<T>()))>> : std::true_type {};
template<typename T>
inline constexpr bool is_iterable_v = is_iterable<T>::value;

//is_raw_data_v<T> is true if std::data can be called with T
template<typename T, typename = void>
struct is_raw_data : std::false_type {};
template<typename T>
struct is_raw_data<
  T, std::void_t<decltype(std::data(std::declval<T>()))>> : std::true_type {};
template<typename T>
inline constexpr bool is_raw_data_v = is_raw_data<T>::value;
}
}
#endif /* __TYPEDEFS_H__ */
