//Heavily modified by me.
/*
    __ _____ _____ _____
 __|  |   __|     |   | |  JSON for Modern C++
|  |  |__   |  |  | | | |  version 3.1.2
|_____|_____|_____|_|___|  https://github.com/nlohmann/json

Licensed under the MIT License <http://opensource.org/licenses/MIT>.
Copyright (c) 2013-2018 Niels Lohmann <http://nlohmann.me>.

Permission is hereby  granted, free of charge, to any  person obtaining a copy
of this software and associated  documentation files (the "Software"), to deal
in the Software  without restriction, including without  limitation the rights
to  use, copy,  modify, merge,  publish, distribute,  sublicense, and/or  sell
copies  of  the Software,  and  to  permit persons  to  whom  the Software  is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE  IS PROVIDED "AS  IS", WITHOUT WARRANTY  OF ANY KIND,  EXPRESS OR
IMPLIED,  INCLUDING BUT  NOT  LIMITED TO  THE  WARRANTIES OF  MERCHANTABILITY,
FITNESS FOR  A PARTICULAR PURPOSE AND  NONINFRINGEMENT. IN NO EVENT  SHALL THE
AUTHORS  OR COPYRIGHT  HOLDERS  BE  LIABLE FOR  ANY  CLAIM,  DAMAGES OR  OTHER
LIABILITY, WHETHER IN AN ACTION OF  CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#ifndef __MY_JSON_HPP__
#define __MY_JSON_HPP__

#define MY_JSON_VERSION_MAJOR 3
#define MY_JSON_VERSION_MINOR 1
#define MY_JSON_VERSION_PATCH 2

#include <algorithm>
#include <array>
#include <assert.h>
#include <ciso646>
#include <clocale>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <exception>
#include <forward_list>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <map>
#include <memory>
#include <numeric>
#include <stdexcept>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <valarray>
#include <vector>

#ifndef MY_JSON_JSON_FWD_HPP
#define MY_JSON_JSON_FWD_HPP

namespace my_json {
template <typename = void, typename = void> struct adl_serializer;

//The type of a json value.
struct basic_json;

//A JSON pointer defines a string syntax for identifying a specific value
//within a JSON document
struct json_pointer;
} // namespace my_json

// manual branch prediction
#if defined(__clang__) || defined(__GNUC__) || defined(__GNUG__)
#define JSON_LIKELY(x) __builtin_expect(!!(x), 1)
#define JSON_UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define JSON_LIKELY(x) x
#define JSON_UNLIKELY(x) x
#endif

/*!
@brief Helper to determine whether there's a key_type for T.

This helper is used to tell associative containers apart from other containers
such as sequence containers. For instance, `std::map` passes the test as it
contains a `mapped_type`, whereas `std::vector` fails the test.

@sa http://stackoverflow.com/a/7728728/266378
@since version 1.0.0, overworked in version 2.0.6
*/
#define MY_JSON_JSON_HAS_HELPER(type)                                          \
  template <typename T> struct has_##type {                                    \
  private:                                                                     \
    template <typename U, typename = typename U::type> static int detect(U&&); \
    static void detect(...);                                                   \
                                                                               \
  public:                                                                      \
    static constexpr bool value =                                              \
        std::is_integral<decltype(detect(std::declval<T>()))>::value;          \
  }

// <my_json/detail/meta.hpp>

namespace my_json {
/*!
@brief detail namespace with internal helper functions

This namespace collects functions that should not be exposed,
implementations of some @ref basic_json methods, and meta-programming helpers.

@since version 2.1.0
*/
namespace detail {
/////////////
// helpers //
/////////////

template <typename T>
using uncvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

template <class Sequence1, class Sequence2> struct merge_and_renumber;

template <std::size_t... I1, std::size_t... I2>
struct merge_and_renumber<index_sequence<I1...>, index_sequence<I2...>>
    : index_sequence<I1..., (sizeof...(I1) + I2)...> {};

template <std::size_t N>
struct make_index_sequence
    : merge_and_renumber<typename make_index_sequence<N / 2>::type,
                         typename make_index_sequence<N - N / 2>::type> {};

template <> struct make_index_sequence<0> : index_sequence<> {};
template <> struct make_index_sequence<1> : index_sequence<0> {};

template <typename... Ts>
using index_sequence_for = make_index_sequence<sizeof...(Ts)>;

/*
Implementation of two C++17 constructs: conjunction, negation. This is needed
to avoid evaluating all the traits in a condition

For example: not std::is_same<void, T>::value and has_value_type<T>::value
will not compile when T = void (on MSVC at least). Whereas
conjunction<negation<std::is_same<void, T>>, has_value_type<T>>::value will
stop evaluating if negation<...>::value == false

Please note that those constructs must be used with caution, since symbols can
become very long quickly (which can slow down compilation and cause MSVC
internal compiler errors). Only use it when you have to (see example ahead).
*/
template <class...> struct conjunction : std::true_type {};
template <class B1> struct conjunction<B1> : B1 {};
template <class B1, class... Bn>
struct conjunction<B1, Bn...>
    : std::conditional<bool(B1::value), conjunction<Bn...>, B1>::type {};

template <class B> struct negation : std::integral_constant<bool, !B::value> {};

// dispatch utility (taken from ranges-v3)
template <unsigned N> struct priority_tag : priority_tag<N - 1> {};
template <> struct priority_tag<0> {};

////////////////////////
// has_/is_ functions //
////////////////////////

// source: https://stackoverflow.com/a/37193089/4116453

template <typename T, typename = void>
struct is_complete_type : std::false_type {};

template <typename T>
struct is_complete_type<T, decltype(void(sizeof(T)))> : std::true_type {};

MY_JSON_JSON_HAS_HELPER(mapped_type);
MY_JSON_JSON_HAS_HELPER(key_type);
MY_JSON_JSON_HAS_HELPER(value_type);
MY_JSON_JSON_HAS_HELPER(iterator);

template <bool B, class RealType, class CompatibleObjectType>
struct is_compatible_object_type_impl : std::false_type {};

template <class RealType, class CompatibleObjectType>
struct is_compatible_object_type_impl<true, RealType, CompatibleObjectType> {
  static constexpr auto value =
      std::is_constructible<typename RealType::key_type,
                            typename CompatibleObjectType::key_type>::value &&
      std::is_constructible<typename RealType::mapped_type,
                            typename CompatibleObjectType::mapped_type>::value;
};

template <class basic_json, class CompatibleObjectType>
struct is_compatible_object_type {
  static auto constexpr value = is_compatible_object_type_impl<
      conjunction<negation<std::is_same<void, CompatibleObjectType>>,
                  has_mapped_type<CompatibleObjectType>,
                  has_key_type<CompatibleObjectType>>::value,
      typename basic_json::object_t, CompatibleObjectType>::value;
};

template <typename basic_json, typename T> struct is_basic_json_nested_type {
  static auto constexpr value =
      std::is_same<T, typename basic_json::iterator>::value ||
      std::is_same<T, typename basic_json::const_iterator>::value ||
      std::is_same<T, typename basic_json::reverse_iterator>::value ||
      std::is_same<T, typename basic_json::const_reverse_iterator>::value;
};

template <class basic_json, class CompatibleArrayType>
struct is_compatible_array_type {
  static auto constexpr value = conjunction<
      negation<std::is_same<void, CompatibleArrayType>>,
      negation<is_compatible_object_type<basic_json, CompatibleArrayType>>,
      negation<std::is_constructible<typename basic_json::string_t,
                                     CompatibleArrayType>>,
      negation<is_basic_json_nested_type<basic_json, CompatibleArrayType>>,
      has_value_type<CompatibleArrayType>,
      has_iterator<CompatibleArrayType>>::value;
};

template <bool, typename, typename>
struct is_compatible_integer_type_impl : std::false_type {};

template <typename RealIntegerType, typename CompatibleNumberIntegerType>
struct is_compatible_integer_type_impl<true, RealIntegerType,
                                       CompatibleNumberIntegerType> {
  // is there an assert somewhere on overflows?
  using RealLimits = std::numeric_limits<RealIntegerType>;
  using CompatibleLimits = std::numeric_limits<CompatibleNumberIntegerType>;

  static constexpr auto value =
      std::is_constructible<RealIntegerType,
                            CompatibleNumberIntegerType>::value &&
      CompatibleLimits::is_integer &&
      RealLimits::is_signed == CompatibleLimits::is_signed;
};

template <typename RealIntegerType, typename CompatibleNumberIntegerType>
struct is_compatible_integer_type {
  static constexpr auto
      value = is_compatible_integer_type_impl <
                  std::is_integral<CompatibleNumberIntegerType>::value &&
              !std::is_same<bool, CompatibleNumberIntegerType>::value,
      RealIntegerType, CompatibleNumberIntegerType > ::value;
};

// trait checking if JSONSerializer<T>::from_json(json const&, udt&) exists
template <typename basic_json, typename T> struct has_from_json {
  // also check the return type of from_json
  template <
      typename U,
      typename = enable_if_t<std::is_same<
          void, decltype(uncvref_t<U>::from_json(std::declval<basic_json>(),
                                                 std::declval<T&>()))>::value>>
  static int detect(U&&);
  static void detect(...);

  static constexpr bool value = std::is_integral<decltype(
      detect(std::declval<typename basic_json::template json_serializer<
                 T, void>>()))>::value;
};

// This trait checks if JSONSerializer<T>::from_json(json const&) exists
// this overload is used for non-default-constructible user-defined-types
template <typename basic_json, typename T> struct has_non_default_from_json {
  template <typename U, typename = enable_if_t<std::is_same<
                            T, decltype(uncvref_t<U>::from_json(
                                   std::declval<basic_json>()))>::value>>
  static int detect(U&&);
  static void detect(...);

  static constexpr bool value = std::is_integral<decltype(
      detect(std::declval<typename basic_json::template json_serializer<
                 T, void>>()))>::value;
};

// This trait checks if basic_json::json_serializer<T>::to_json exists
template <typename basic_json, typename T> struct has_to_json {
  template <typename U, typename = decltype(uncvref_t<U>::to_json(
                            std::declval<basic_json&>(), std::declval<T>()))>
  static int detect(U&&);
  static void detect(...);

  static constexpr bool value = std::is_integral<decltype(
      detect(std::declval<typename basic_json::template json_serializer<
                 T, void>>()))>::value;
};

template <typename basic_json, typename CompatibleCompleteType>
struct is_compatible_complete_type {
  static constexpr bool value =
      !is_basic_json<CompatibleCompleteType>::value &&
      !is_basic_json_nested_type<basic_json,
                                 CompatibleCompleteType>::value &&
      has_to_json<basic_json, CompatibleCompleteType>::value;
};

template <typename basic_json, typename CompatibleType>
struct is_compatible_type
    : conjunction<is_complete_type<CompatibleType>,
                  is_compatible_complete_type<basic_json, CompatibleType>> {
};

// taken from ranges-v3
template <typename T> struct static_const { static constexpr T value{}; };

template <typename T> constexpr T static_const<T>::value;
} // namespace detail
} // namespace my_json

// <my_json/detail/exceptions.hpp>

namespace my_json {
namespace detail {
struct exception : public std::exception {
  const char* what() const noexcept override { return m.what(); }
  const int id;

  exception(int id_, const char* what_arg) : id(id_), m(what_arg) {}

  static std::string name(const std::string& ename, int id_) {
    return "[json.exception." + ename + "." + std::to_string(id_) + "] ";
  }

  /// an exception object as storage for error messages
  std::runtime_error m;
};
struct parse_error : public exception {
  /*!
  @brief create a parse error exception
  @param[in] id_       the id of the exception
  @param[in] byte_     the byte index where the error occurred (or 0 if the
                       position cannot be determined)
  @param[in] what_arg  the explanatory string
  @return parse_error object
  */
  static parse_error create(int id_, std::size_t byte_,
                            const std::string& what_arg) {
    std::string w = exception::name("parse_error", id_) + "parse error" +
                    (byte_ != 0 ? (" at " + std::to_string(byte_)) : "") +
                    ": " + what_arg;
    return parse_error(id_, byte_, w.c_str());
  }

  /*!
  @brief byte index of the parse error

  The byte index of the last read character in the input file.

  @note For an input with n bytes, 1 is the index of the first character and
        n+1 is the index of the terminating null byte or the end of file.
        This also holds true when reading a byte vector (CBOR or MessagePack).
  */
  const std::size_t byte;

  parse_error(int id_, std::size_t byte_, const char* what_arg)
      : exception(id_, what_arg), byte(byte_) {}
};

struct invalid_iterator : public exception {
  static invalid_iterator create(int id_, const std::string& what_arg) {
    std::string w = exception::name("invalid_iterator", id_) + what_arg;
    return invalid_iterator(id_, w.c_str());
  }

  invalid_iterator(int id_, const char* what_arg) : exception(id_, what_arg) {}
};

struct type_error : public exception {
  static type_error create(int id_, const std::string& what_arg) {
    std::string w = exception::name("type_error", id_) + what_arg;
    return type_error(id_, w.c_str());
  }

  type_error(int id_, const char* what_arg) : exception(id_, what_arg) {}
};

struct out_of_range : public exception {
  static out_of_range create(int id_, const std::string& what_arg) {
    std::string w = exception::name("out_of_range", id_) + what_arg;
    return out_of_range(id_, w.c_str());
  }

  out_of_range(int id_, const char* what_arg) : exception(id_, what_arg) {}
};

struct other_error : public exception {
  static other_error create(int id_, const std::string& what_arg) {
    std::string w = exception::name("other_error", id_) + what_arg;
    return other_error(id_, w.c_str());
  }

  other_error(int id_, const char* what_arg) : exception(id_, what_arg) {}
};
} // namespace detail
} // namespace my_json

// <my_json/detail/value_t.hpp>

namespace my_json {
namespace detail {
///////////////////////////
// JSON type enumeration //
///////////////////////////

//Enum used to identify what the type of a json value is.
enum class value_t : uint8_t {
  null,            ///< null value
  object,          ///< object (unordered set of name/value pairs)
  array,           ///< array (ordered collection of values)
  string,          ///< string value
  boolean,         ///< boolean value
  number_integer,  ///< number value (signed integer)
  number_unsigned, ///< number value (unsigned integer)
  number_float,    ///< number value (floating-point)
  blob,            ///< binary blob (non-standard extension)
  discarded        ///< discarded by the the parser callback function
};
static constexpr value_t_max = 10;
/*!
@brief comparison operator for JSON types

Returns an ordering that is similar to Python:
- order: null < boolean < number < object < array < string < blob
- furthermore, each type is not smaller than itself
- discarded values are not comparable

@since version 1.0.0
*/
inline bool operator<(const value_t lhs, const value_t rhs) noexcept {
  static constexpr std::array<std::uint8_t, value_t_max - 1> order = {{
      0 /* null */, 3 /* object */, 4 /* array */, 5 /* string */,
      1 /* boolean */, 2 /* integer */, 2 /* unsigned */, 2 /* float */, 6 /* blob */
  }};
  const auto l_index = static_cast<std::size_t>(lhs);
  const auto r_index = static_cast<std::size_t>(rhs);
  return l_index < order.size() && r_index < order.size() &&
         order[l_index] < order[r_index];
}
} // namespace detail
} // namespace my_json

// <my_json/detail/conversions/from_json.hpp>

namespace my_json {
namespace detail {
// overloads for basic_json template parameters
template <typename ArithmeticType,
    enable_if_t<std::is_arithmetic<ArithmeticType>::value &&
                    !std::is_same<ArithmeticType,
                                  typename basic_json::boolean_t>::value,
                int> = 0>
void get_arithmetic_value(const basic_json& j, ArithmeticType& val) {
  switch (static_cast<value_t>(j)) {
  case value_t::number_unsigned: {
    val = static_cast<ArithmeticType>(
      *j.template get_ptr<
        const typename basic_json::number_unsigned_t*>());
    break;
  }
  case value_t::number_integer: {
    val = static_cast<ArithmeticType>(
        *j.template get_ptr<const typename basic_json::number_integer_t*>());
    break;
  }
  case value_t::number_float: {
    val = static_cast<ArithmeticType>(
        *j.template get_ptr<const typename basic_json::number_float_t*>());
    break;
  }

  default:
    JSON_THROW(type_error::create(302, "type must be number, but is " +
                                           std::string(j.type_name())));
  }
}
void from_json(const basic_json& j, typename basic_json::boolean_t& b) {
  if (JSON_UNLIKELY(!j.is_boolean())) {
    JSON_THROW(type_error::create(302, "type must be boolean, but is " +
                                           std::string(j.type_name())));
  }
  b = *j.template get_ptr<const typename basic_json::boolean_t*>();
}

void from_json(const basic_json& j, typename basic_json::string_t& s) {
  if (JSON_UNLIKELY(!j.is_string())) {
    JSON_THROW(type_error::create(302, "type must be string, but is " +
                                           std::string(j.type_name())));
  }
  s = *j.template get_ptr<const typename basic_json::string_t*>();
}
// Add string view conversion
void from_json(const basic_json& j,
               typename basic_json::string_view_t& sv) {
  if (JSON_UNLIKELY(!j.is_string())) {
    JSON_THROW(type_error::create(302, "type must be string, but is " +
                                           std::string(j.type_name())));
  }
  auto* s = j.template get_ptr<const typename basic_json::string_t*>();
  sv = typename basic_json::string_view_t(s->data(), s->size());
}

void from_json(const basic_json& j,
               typename basic_json::number_float_t& val) {
  get_arithmetic_value(j, val);
}

void from_json(const basic_json& j,
               typename basic_json::number_unsigned_t& val) {
  get_arithmetic_value(j, val);
}


void from_json(const basic_json& j,
               typename basic_json::number_integer_t& val) {
  get_arithmetic_value(j, val);
}

template <typename EnumType,
          enable_if_t<std::is_enum<EnumType>::value, int> = 0>
void from_json(const basic_json& j, EnumType& e) {
  typename std::underlying_type<EnumType>::type val;
  get_arithmetic_value(j, val);
  e = static_cast<EnumType>(val);
}

void from_json(const basic_json& j, typename basic_json::array_t& arr) {
  if (JSON_UNLIKELY(!j.is_array())) {
    JSON_THROW(type_error::create(302, "type must be array, but is " +
                                           std::string(j.type_name())));
  }
  arr = *j.template get_ptr<const typename basic_json::array_t*>();
}

// forward_list doesn't have an insert method
template <typename T, typename Allocator,
          enable_if_t<std::is_convertible<basic_json, T>::value, int> = 0>
void from_json(const basic_json& j, std::forward_list<T, Allocator>& l) {
  if (JSON_UNLIKELY(!j.is_array())) {
    JSON_THROW(type_error::create(302, "type must be array, but is " +
                                           std::string(j.type_name())));
  }
  std::transform(j.rbegin(), j.rend(), std::front_inserter(l),
                 [](const basic_json& i) { return i.template get<T>(); });
}

// valarray doesn't have an insert method
template <typename T,
          enable_if_t<std::is_convertible<basic_json, T>::value, int> = 0>
void from_json(const basic_json& j, std::valarray<T>& l) {
  if (JSON_UNLIKELY(!j.is_array())) {
    JSON_THROW(type_error::create(302, "type must be array, but is " +
                                           std::string(j.type_name())));
  }
  l.resize(j.size());
  std::copy(j.m_value.array->begin(), j.m_value.array->end(), std::begin(l));
}

template <typename CompatibleArrayType>
void from_json_array_impl(const basic_json& j, CompatibleArrayType& arr,
                          priority_tag<0> /*unused*/) {
  using std::end;

  std::transform(
      j.begin(), j.end(), std::inserter(arr, end(arr)),
      [](const basic_json& i) {
        // get<basic_json>() returns *this, this won't call a from_json
        // method when value_type is basic_json
        return i.template get<typename CompatibleArrayType::value_type>();
      });
}

template <typename CompatibleArrayType>
auto from_json_array_impl(const basic_json& j, CompatibleArrayType& arr,
                          priority_tag<1> /*unused*/)
    -> decltype(
        arr.reserve(std::declval<typename CompatibleArrayType::size_type>()),
        void()) {
  using std::end;

  arr.reserve(j.size());
  std::transform(
      j.begin(), j.end(), std::inserter(arr, end(arr)),
      [](const basic_json& i) {
        // get<basic_json>() returns *this, this won't call a from_json
        // method when value_type is basic_json
        return i.template get<typename CompatibleArrayType::value_type>();
      });
}

template <typename T, std::size_t N>
void from_json_array_impl(const basic_json& j, std::array<T, N>& arr,
                          priority_tag<2> /*unused*/) {
  for (std::size_t i = 0; i < N; ++i) {
    arr[i] = j.at(i).template get<T>();
  }
}

template <typename CompatibleArrayType,
          enable_if_t<
            is_compatible_array_type<basic_json, CompatibleArrayType>::value &&
            !std::is_same<typename basic_json::array_t,
                          CompatibleArrayType>::value &&
            std::is_constructible<
              basic_json, typename CompatibleArrayType::value_type>::value,
            int> = 0>
void from_json(const basic_json& j, CompatibleArrayType& arr) {
  if (JSON_UNLIKELY(!j.is_array())) {
    JSON_THROW(type_error::create(302, "type must be array, but is " +
                                           std::string(j.type_name())));
  }

  from_json_array_impl(j, arr, priority_tag<2>{});
}

template <typename CompatibleObjectType,
          enable_if_t<is_compatible_object_type<basic_json,
                                                CompatibleObjectType>::value,
                      int> = 0>
void from_json(const basic_json& j, CompatibleObjectType& obj) {
  if (JSON_UNLIKELY(!j.is_object())) {
    JSON_THROW(type_error::create(302, "type must be object, but is " +
                                           std::string(j.type_name())));
  }

  auto inner_object =
      j.template get_ptr<const typename basic_json::object_t*>();
  using value_type = typename CompatibleObjectType::value_type;
  std::transform(
      inner_object->begin(), inner_object->end(),
      std::inserter(obj, obj.begin()),
      [](typename basic_json::object_t::value_type const& p) {
        return value_type(
            p.first,
            p.second
                .template get<typename CompatibleObjectType::mapped_type>());
      });
}

// overload for arithmetic types, not chosen for basic_json template arguments
// (BooleanType, etc..); note: Is it really necessary to provide explicit
// overloads for boolean_t etc. in case of a custom BooleanType which is not
// an arithmetic type?
template <typename ArithmeticType,
    enable_if_t<
        std::is_arithmetic<ArithmeticType>::value &&
            !std::is_same<ArithmeticType,
                          typename basic_json::number_unsigned_t>::value &&
            !std::is_same<ArithmeticType,
                          typename basic_json::number_integer_t>::value &&
            !std::is_same<ArithmeticType,
                          typename basic_json::number_float_t>::value &&
            !std::is_same<ArithmeticType,
                          typename basic_json::boolean_t>::value,
        int> = 0>
void from_json(const basic_json& j, ArithmeticType& val) {
  switch (static_cast<value_t>(j)) {
  case value_t::number_unsigned: {
    val = static_cast<ArithmeticType>(
        *j.template get_ptr<
            const typename basic_json::number_unsigned_t*>());
    break;
  }
  case value_t::number_integer: {
    val = static_cast<ArithmeticType>(
        *j.template get_ptr<const typename basic_json::number_integer_t*>());
    break;
  }
  case value_t::number_float: {
    val = static_cast<ArithmeticType>(
        *j.template get_ptr<const typename basic_json::number_float_t*>());
    break;
  }
  case value_t::boolean: {
    val = static_cast<ArithmeticType>(
        *j.template get_ptr<const typename basic_json::boolean_t*>());
    break;
  }

  default:
    JSON_THROW(type_error::create(302, "type must be number, but is " +
                                           std::string(j.type_name())));
  }
}

template <typename A1, typename A2>
void from_json(const basic_json& j, std::pair<A1, A2>& p) {
  p = {j.at(0).template get<A1>(), j.at(1).template get<A2>()};
}

template <typename Tuple, std::size_t... Idx>
void from_json_tuple_impl(const basic_json& j, Tuple& t,
                          index_sequence<Idx...>) {
  t = std::make_tuple(
      j.at(Idx)
          .template get<typename std::tuple_element<Idx, Tuple>::type>()...);
}

template <typename... Args>
void from_json(const basic_json& j, std::tuple<Args...>& t) {
  from_json_tuple_impl(j, t, index_sequence_for<Args...>{});
}

struct from_json_fn {
  template <typename T>
  auto call(const basic_json& j, T& val, priority_tag<1> /*unused*/) const
      noexcept(noexcept(from_json(j, val)))
          -> decltype(from_json(j, val), void()) {
    return from_json(j, val);
  }

  template  typename T>
  void call(const basic_json& /*unused*/, T& /*unused*/,
            priority_tag<0> /*unused*/) const noexcept {
    static_assert(sizeof(basic_json) == 0,
                  "could !find from_json() method in T's namespace");
#ifdef _MSC_VER
    // MSVC does not show a stacktrace for the above assert
    using decayed = uncvref_t<T>;
    static_assert(
        sizeof(typename decayed::force_msvc_stacktrace) == 0,
        "forcing MSVC stacktrace to show which T we're talking about.");
#endif
  }

  template <typename T>
  void operator()(const basic_json& j, T& val) const
      noexcept(noexcept(std::declval<from_json_fn>().call(j, val,
                                                          priority_tag<1>{}))) {
    return call(j, val, priority_tag<1>{});
  }
};
} // namespace detail

/// namespace to hold default `from_json` function
/// to see why this is required:
/// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/n4381.html
namespace {
constexpr const auto& from_json =
    detail::static_const<detail::from_json_fn>::value;
}
} // namespace my_json

// <my_json/detail/conversions/to_json.hpp>

namespace my_json {
namespace detail {
//////////////////
// constructors //
//////////////////

template <value_t> struct external_constructor;

template <> struct external_constructor<value_t::boolean> {
  static void construct(basic_json& j,
                        typename basic_json::boolean_t b) noexcept {
    j.m_type = value_t::boolean;
    j.m_value = b;
    j.assert_invariant();
  }
};

template <> struct external_constructor<value_t::string> {
  static void construct(basic_json& j,
                        const typename basic_json::string_t& s) {
    j.m_type = value_t::string;
    j.m_value = s;
    j.assert_invariant();
  }
  static void construct(basic_json& j, const std::string_view& s) {
    j.m_type = value_t::string;
    j.m_value = typename basic_json::string_t(s);
    j.assert_invariant();
  }
  static void construct(basic_json& j,
                        typename basic_json::string_t&& s) {
    j.m_type = value_t::string;
    j.m_value = std::move(s);
    j.assert_invariant();
  }
};

template <> struct external_constructor<value_t::number_float> {
  static void construct(basic_json& j,
                        typename basic_json::number_float_t val) noexcept {
    j.m_type = value_t::number_float;
    j.m_value = val;
    j.assert_invariant();
  }
};

template <> struct external_constructor<value_t::number_unsigned> {
  static void
  construct(basic_json& j,
            typename basic_json::number_unsigned_t val) noexcept {
    j.m_type = value_t::number_unsigned;
    j.m_value = val;
    j.assert_invariant();
  }
};

template <> struct external_constructor<value_t::number_integer> {
  static void construct(basic_json& j,
                        typename basic_json::number_integer_t val) noexcept {
    j.m_type = value_t::number_integer;
    j.m_value = val;
    j.assert_invariant();
  }
};

template <> struct external_constructor<value_t::array> {
  static void construct(basic_json& j,
                        const typename basic_json::array_t& arr) {
    j.m_type = value_t::array;
    j.m_value = arr;
    j.assert_invariant();
  }
  static void construct(basic_json& j,
                        typename basic_json::array_t&& arr) {
    j.m_type = value_t::array;
    j.m_value = std::move(arr);
    j.assert_invariant();
  }

  template <typename basic_json, typename CompatibleArrayType,
            enable_if_t<!std::is_same<CompatibleArrayType,
                                      typename basic_json::array_t>::value,
                        int> = 0>
  static void construct(basic_json& j, const CompatibleArrayType& arr) {
    using std::begin;
    using std::end;
    j.m_type = value_t::array;
    j.m_value.array = j.template create<typename basic_json::array_t>(
        begin(arr), end(arr));
    j.assert_invariant();
  }

  template <typename basic_json>
  static void construct(basic_json& j, const std::vector<bool>& arr) {
    j.m_type = value_t::array;
    j.m_value = value_t::array;
    j.m_value.array->reserve(arr.size());
    for (const bool x : arr) {
      j.m_value.array->push_back(x);
    }
    j.assert_invariant();
  }

  template <typename basic_json, typename T,
            enable_if_t<std::is_convertible<T, basic_json>::value, int> = 0>
  static void construct(basic_json& j, const std::valarray<T>& arr) {
    j.m_type = value_t::array;
    j.m_value = value_t::array;
    j.m_value.array->resize(arr.size());
    std::copy(std::begin(arr), std::end(arr), j.m_value.array->begin());
    j.assert_invariant();
  }
};

template <> struct external_constructor<value_t::object> {
  template <typename basic_json>
  static void construct(basic_json& j,
                        const typename basic_json::object_t& obj) {
    j.m_type = value_t::object;
    j.m_value = obj;
    j.assert_invariant();
  }

  template <typename basic_json>
  static void construct(basic_json& j,
                        typename basic_json::object_t&& obj) {
    j.m_type = value_t::object;
    j.m_value = std::move(obj);
    j.assert_invariant();
  }

  template <typename CompatibleObjectType,
            enable_if_t<!std::is_same<CompatibleObjectType,
                                      typename basic_json::object_t>::value,
                        int> = 0>
  static void construct(basic_json& j, const CompatibleObjectType& obj) {
    using std::begin;
    using std::end;

    j.m_type = value_t::object;
    j.m_value.object = j.template create<typename basic_json::object_t>(
        begin(obj), end(obj));
    j.assert_invariant();
  }
};

/////////////
// to_json //
/////////////

template <typename T,
          enable_if_t<std::is_same<T, typename basic_json::boolean_t>::value,
                      int> = 0>
void to_json(basic_json& j, T b) noexcept {
  external_constructor<value_t::boolean>::construct(j, b);
}

template <typename CompatibleString,
          enable_if_t<std::is_constructible<typename basic_json::string_t,
                                            CompatibleString>::value,
                      int> = 0>
void to_json(basic_json& j, const CompatibleString& s) {
  external_constructor<value_t::string>::construct(j, s);
}

void to_json(basic_json& j, typename basic_json::string_t&& s) {
  external_constructor<value_t::string>::construct(j, std::move(s));
}

template <typename FloatType,
          enable_if_t<std::is_floating_point<FloatType>::value, int> = 0>
void to_json(basic_json& j, FloatType val) noexcept {
  external_constructor<value_t::number_float>::construct(
      j, static_cast<typename basic_json::number_float_t>(val));
}

template <typename CompatibleNumberUnsignedType,
          enable_if_t<is_compatible_integer_type<
                          typename basic_json::number_unsigned_t,
                          CompatibleNumberUnsignedType>::value,
                      int> = 0>
void to_json(basic_json& j, CompatibleNumberUnsignedType val) noexcept {
  external_constructor<value_t::number_unsigned>::construct(
      j, static_cast<typename basic_json::number_unsigned_t>(val));
}

template <typename CompatibleNumberIntegerType,
          enable_if_t<is_compatible_integer_type<
                          typename basic_json::number_integer_t,
                          CompatibleNumberIntegerType>::value,
                      int> = 0>
void to_json(basic_json& j, CompatibleNumberIntegerType val) noexcept {
  external_constructor<value_t::number_integer>::construct(
      j, static_cast<typename basic_json::number_integer_t>(val));
}

template <typename EnumType,
          enable_if_t<std::is_enum<EnumType>::value, int> = 0>
void to_json(basic_json& j, EnumType e) noexcept {
  using underlying_type = typename std::underlying_type<EnumType>::type;
  external_constructor<value_t::number_integer>::construct(
      j, static_cast<underlying_type>(e));
}
void to_json(basic_json& j, const std::vector<bool>& e) {
  external_constructor<value_t::array>::construct(j, e);
}

template <typename CompatibleArrayType,
          enable_if_t<is_compatible_array_type<basic_json,
                                               CompatibleArrayType>::value ||
                          std::is_same<typename basic_json::array_t,
                                       CompatibleArrayType>::value,
                      int> = 0>
void to_json(basic_json& j, const CompatibleArrayType& arr) {
  external_constructor<value_t::array>::construct(j, arr);
}

template <typename T,
          enable_if_t<std::is_convertible<T, basic_json>::value, int> = 0>
void to_json(basic_json& j, std::valarray<T> arr) {
  external_constructor<value_t::array>::construct(j, std::move(arr));
}

void to_json(basic_json& j, typename basic_json::array_t&& arr) {
  external_constructor<value_t::array>::construct(j, std::move(arr));
}

template <typename CompatibleObjectType,
          enable_if_t<is_compatible_object_type<basic_json,
                                                CompatibleObjectType>::value,
                      int> = 0>
void to_json(basic_json& j, const CompatibleObjectType& obj) {
  external_constructor<value_t::object>::construct(j, obj);
}

void to_json(basic_json& j, typename basic_json::object_t&& obj) {
  external_constructor<value_t::object>::construct(j, std::move(obj));
}

template <typename T, std::size_t N,
          enable_if_t<!std::is_constructible<typename basic_json::string_t,
                                             T (&)[N]>::value,
                      int> = 0>
void to_json(basic_json& j, T (&arr)[N]) {
  external_constructor<value_t::array>::construct(j, arr);
}

template <typename... Args>
void to_json(basic_json& j, const std::pair<Args...>& p) {
  j = {p.first, p.second};
}

template <typename Tuple, std::size_t... Idx>
void to_json_tuple_impl(basic_json& j, const Tuple& t,
                        index_sequence<Idx...>) {
  j = {std::get<Idx>(t)...};
}

template <typename... Args>
void to_json(basic_json& j, const std::tuple<Args...>& t) {
  to_json_tuple_impl(j, t, index_sequence_for<Args...>{});
}

struct to_json_fn {
  template <typename T>
  auto call(basic_json& j, T&& val, priority_tag<1> /*unused*/) const
      noexcept(noexcept(to_json(j, std::forward<T>(val))))
          -> decltype(to_json(j, std::forward<T>(val)), void()) {
    return to_json(j, std::forward<T>(val));
  }

  template <typename T>
  void call(basic_json& /*unused*/, T&& /*unused*/,
            priority_tag<0> /*unused*/) const noexcept {
    static_assert(sizeof(basic_json) == 0,
                  "could !find to_json() method in T's namespace");

#ifdef _MSC_VER
    // MSVC does not show a stacktrace for the above assert
    using decayed = uncvref_t<T>;
    static_assert(
        sizeof(typename decayed::force_msvc_stacktrace) == 0,
        "forcing MSVC stacktrace to show which T we're talking about.");
#endif
  }

  template <typename T>
  void operator()(basic_json& j, T&& val) const
      noexcept(noexcept(std::declval<to_json_fn>().call(j, std::forward<T>(val),
                                                        priority_tag<1>{}))) {
    return call(j, std::forward<T>(val), priority_tag<1>{});
  }
};
} // namespace detail

/// namespace to hold default `to_json` function
namespace {
constexpr const auto& to_json = detail::static_const<detail::to_json_fn>::value;
}
} // namespace my_json

// <my_json/detail/input/input_adapters.hpp>

namespace my_json {
namespace detail {
////////////////////
// input adapters //
////////////////////

/*!
@brief abstract input adapter interface

Produces a stream of std::char_traits<char>::int_type characters from a
std::istream, a buffer, or some other input type.  Accepts the return of exactly
one non-EOF character for future input.  The int_type characters returned
consist of all valid char values as positive values (typically unsigned char),
plus an EOF value outside that range, specified by the value of the function
std::char_traits<char>::eof().  This value is typically -1, but could be any
arbitrary value which is not a valid char value.
*/
struct input_adapter_protocol {
  /// get a character [0,255] or std::char_traits<char>::eof().
  virtual std::char_traits<char>::int_type get_character() = 0;
  /// restore the last non-eof() character to input
  virtual void unget_character() = 0;
  virtual ~input_adapter_protocol() = default;
};

/// a type to simplify interfaces
using input_adapter_t = std::shared_ptr<input_adapter_protocol>;


struct input_FILE_adapter : input_adapter_protocol {
  std::FILE* f;
  int c;
  input_FILE_adapter(std::FILE* f) : f{f} {};

  // delete because of pointer members
  input_FILE_adapter(const input_FILE_adapter&) = delete;
  input_FILE_adapter& operator=(input_FILE_adapter&) = delete;
  std::char_traits<char>::int_type get_character() override {
    return (c = std::fgetc(f));
  }
  void unget_character() override { std::ungetc(last, f); }
};

/// input adapter for buffer input
struct input_buffer_adapter : input_adapter_protocol {
  input_buffer_adapter(const char* b, const std::size_t l)
      : cursor(b), limit(b + l), start(b) {
    // skip byte order mark
    if (l >= 3 && b[0] == '\xEF' && b[1] == '\xBB' && b[2] == '\xBF') {
      cursor += 3;
    }
  }

  // delete because of pointer members
  input_buffer_adapter(const input_buffer_adapter&) = delete;
  input_buffer_adapter& operator=(input_buffer_adapter&) = delete;

  std::char_traits<char>::int_type get_character() noexcept override {
    if (JSON_LIKELY(cursor < limit)) {
      return std::char_traits<char>::to_int_type(*(cursor++));
    }

    return std::char_traits<char>::eof();
  }

  void unget_character() noexcept override {
    if (JSON_LIKELY(cursor > start)) {
      --cursor;
    }
  }
  /// pointer to the current character
  const char* cursor;
  /// pointer past the last character
  const char* limit;
  /// pointer to the first character
  const char* start;
};

struct input_adapter {

  /// input adapter for FILE
  input_adapter(std::FILE* f) : ia(std::make_shared<input_FILE_adapter>(f)) {}

  /// input adapter for buffer
  input_adapter(char* b, std::size_t l)
      : ia(std::make_shared<input_buffer_adapter>(
             reinterpret_cast<const char*>(b), l)) {}
    input_adapter(unsigned char* b, std::size_t l)
      : ia(std::make_shared<input_buffer_adapter>(
            reinterpret_cast<const char*>(b), l)) {}

  /// input adapter for string literal
  input_adapter(const char *b)
    : input_adapter(b, strlen(b)) {}

  /// input adapter for iterator range with contiguous storage
  template <class IteratorType,
            typename std::enable_if_t<
                std::is_same_v<typename std::iterator_traits<
                                 IteratorType>::iterator_category,
                               std::random_access_iterator_tag> &&
              sizeof(typename std::iterator_traits<IteratorType>::value_type) == 1,
                  int> = 0>
  input_adapter(IteratorType first, IteratorType last) {
    const size_t len = static_cast<size_t>(std::distance(first, last));
    if (JSON_LIKELY(len > 0)) {
      // there is at least one element: use the address of first
      ia = std::make_shared<input_buffer_adapter>(
          reinterpret_cast<const char*>(&(*first)), len);
    } else {
      // the address of first cannot be used: use nullptr
      ia = std::make_shared<input_buffer_adapter>(nullptr, len);
    }
  }

  /// input adapter for array
  template <class T, std::size_t N>
  input_adapter(T (&array)[N]) //this is weird
      : input_adapter(std::begin(array), std::end(array)) {}

  /// input adapter for contiguous container
  template <
      class ContiguousContainer,
      typename std::enable_if<
          !std::is_pointer<ContiguousContainer>::value &&
              std::is_base_of<std::random_access_iterator_tag,
                              typename std::iterator_traits<decltype(std::begin(
                                  std::declval<ContiguousContainer const>()))>::
                                  iterator_category>::value,
          int>::type = 0>
  input_adapter(const ContiguousContainer& c)
      : input_adapter(std::begin(c), std::end(c)) {}

  operator input_adapter_t() { return ia; }

  /// the actual adapter
  input_adapter_t ia = nullptr;
};
} // namespace detail
} // namespace my_json

// <my_json/detail/input/lexer.hpp>

namespace my_json {
namespace detail {
///////////
// lexer //
///////////

/*!
@brief lexical analysis

This class organizes the lexical analysis during JSON deserialization.
*/
struct lexer {
  using number_integer_t = typename basic_json::number_integer_t;
  using number_unsigned_t = typename basic_json::number_unsigned_t;
  using number_float_t = typename basic_json::number_float_t;
  using string_t = typename basic_json::string_t;

  /// token types for the parser
  enum class token_type {
    uninitialized,  ///< indicating the scanner is uninitialized
    literal_true,   ///< the `true` literal
    literal_false,  ///< the `false` literal
    literal_null,   ///< the `null` literal
    value_string,   ///< a string -- use get_string() for actual value
    value_unsigned, ///< an unsigned integer -- use get_number_unsigned() for
                    ///< actual value
    value_integer,  ///< a signed integer -- use get_number_integer() for actual
                    ///< value
    value_float,    ///< an floating point number -- use get_number_float() for
                    ///< actual value
    begin_array,    ///< the character for array begin `[`
    begin_object,   ///< the character for object begin `{`
    end_array,      ///< the character for array end `]`
    end_object,     ///< the character for object end `}`
    name_separator, ///< the name separator `:`
    value_separator, ///< the value separator `,`
    parse_error,     ///< indicating a parse error
    end_of_input,    ///< indicating the end of the input buffer
    literal_or_value ///< a literal or the begin of a value (only for
                     ///< diagnostics)
  };

  /// return name of values of type token_type (only used for errors)
  static const char* token_type_name(const token_type t) noexcept {
    switch (t) {
    case token_type::uninitialized: return "<uninitialized>";
    case token_type::literal_true: return "true literal";
    case token_type::literal_false: return "false literal";
    case token_type::literal_null: return "null literal";
    case token_type::value_string: return "string literal";
    case lexer::token_type::value_unsigned:
    case lexer::token_type::value_integer:
    case lexer::token_type::value_float: return "number literal";
    case token_type::begin_array: return "'['";
    case token_type::begin_object: return "'{'";
    case token_type::end_array: return "']'";
    case token_type::end_object: return "'}'";
    case token_type::name_separator: return "':'";
    case token_type::value_separator: return "','";
    case token_type::parse_error: return "<parse error>";
    case token_type::end_of_input: return "end of input";
    case token_type::literal_or_value: return "'[', '{', || a literal";
    default:                  // catch non-enum values
      return "unknown token"; // LCOV_EXCL_LINE
    }
  }

  explicit lexer(detail::input_adapter_t adapter, bool validate_utf8 = true)
      : ia(std::move(adapter)), decimal_point_char(get_decimal_point()),
        validate_utf8{validate_utf8} {}

  // delete because of pointer members
  lexer(const lexer&) = delete;
  lexer& operator=(lexer&) = delete;
  detail::input_adapter_t ia = nullptr;

  /// the current character
  std::char_traits<char>::int_type current = std::char_traits<char>::eof();

  /// the number of characters read
  std::size_t chars_read = 0;

  /// raw input token string (for error messages)
  std::vector<char> token_string{};

  /// buffer for variable-length tokens (numbers, strings)
  string_t token_buffer{};

  /// a description of occurred lexer errors
  std::string error_message = "";

  // number values
  union {
    number_integer_t value_integer = 0;
    number_unsigned_t value_unsigned;
    number_float_t value_float;
  };
  bool validate_utf8;
  /// the decimal point
  const char decimal_point_char = '.';
  //scan a string literal
  token_type scan_string();
  //scan a number, the value will be stored in the appropiate value_XXX member.
  token_type scan_number() {
  token_type scan_literal(const char* literal_text, const std::size_t length,
                          token_type return_type);
    assert(current == literal_text[0]);
    for (std::size_t i = 1; i < length; ++i) {
      if (JSON_UNLIKELY(get() != literal_text[i])) {
        error_message = "invalid literal";
        return token_type::parse_error;
      }
    }
    return return_type;
  }

  /////////////////////
  // input management
  /////////////////////

  /// reset token_buffer; current character is beginning of token
  void reset() noexcept {
    token_buffer.clear();
    token_string.clear();
    token_string.push_back(std::char_traits<char>::to_char_type(current));
  }

  //get next character from the input
  std::char_traits<char>::int_type get() {
    ++chars_read;
    current = ia->get_character();
    if (JSON_LIKELY(current != std::char_traits<char>::eof())) {
      token_string.push_back(std::char_traits<char>::to_char_type(current));
    }
    return current;
  }

  /// unget current character (return it again on next get)
  void unget() {
    --chars_read;
    if (JSON_LIKELY(current != std::char_traits<char>::eof())) {
      ia->unget_character();
      assert(token_string.size() != 0);
      token_string.pop_back();
    }
  }

  /// add a character to token_buffer
  void add(int c) {
    token_buffer.push_back(std::char_traits<char>::to_char_type(c));
  }
  void add_current() {
    token_buffer.push_back(std::char_traits<char>::to_char_type(current));
  }
  bool eof(){
    return current == std::char_traits<char>::eof();
  }

  /////////////////////
  // value getters
  /////////////////////

  /// return integer value
  constexpr number_integer_t get_number_integer() const noexcept {
    return value_integer;
  }

  /// return unsigned integer value
  constexpr number_unsigned_t get_number_unsigned() const noexcept {
    return value_unsigned;
  }

  /// return floating-point value
  constexpr number_float_t get_number_float() const noexcept {
    return value_float;
  }

  /// return current string value (implicitly resets the token; useful only
  /// once)
  string_t&& move_string() { return std::move(token_buffer); }

  /////////////////////
  // diagnostics
  /////////////////////

  /// return position of last read token
  constexpr std::size_t get_position() const noexcept { return chars_read; }

  /// return the last read token (for errors only).  Will never contain EOF
  /// (an arbitrary value that is not a valid char value, often -1), because
  /// 255 may legitimately occur.  May contain NUL, which should be escaped.
  std::string get_token_string() const {
    // escape control characters
    std::string result;
    for (const auto c : token_string) {
      if ('\x00' <= c && c <= '\x1F') {
        // escape control characters
        char buf[16];
        snprintf(buf, 16, "<U+%04X>", static_cast<int>(c));
        result += buf;
      } else {
        // add character as is
        result.push_back(c);
      }
    }

    return result;
  }

  /// return syntax error message
  constexpr const char* get_error_message() const noexcept {
    return error_message;
  }

  /////////////////////
  // actual scanner
  /////////////////////

  token_type scan() {
    // read next character and ignore whitespace
    do {
      get();
    } while (current == ' ' || current == '\t' || current == '\n' ||
             current == '\r');

    switch (current) {
    // structural characters
    case '[': return token_type::begin_array;
    case ']': return token_type::end_array;
    case '{': return token_type::begin_object;
    case '}': return token_type::end_object;
    case ':': return token_type::name_separator;
    case ',':
      return token_type::value_separator;

    // literals
    case 't': return scan_literal("true", 4, token_type::literal_true);
    case 'f': return scan_literal("false", 5, token_type::literal_false);
    case 'n':
      return scan_literal("null", 4, token_type::literal_null);

    // string
    case '\"':
      return scan_string();

    // number
    case '-':
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      return scan_number();

    // end of input (the null byte is needed when parsing from
    // string literals)
    case '\0':
    case std::char_traits<char>::eof():
      return token_type::end_of_input;

    // error
    default: error_message = "invalid literal"; return token_type::parse_error;
    }
  }

};
} // namespace detail
} // namespace my_json

namespace my_json {
namespace detail {
////////////
// parser //
////////////

/*!
@brief syntax analysis

This class implements a recursive decent parser.
*/
template class parser {
  using number_integer_t = typename basic_json::number_integer_t;
  using number_unsigned_t = typename basic_json::number_unsigned_t;
  using number_float_t = typename basic_json::number_float_t;
  using string_t = typename basic_json::string_t;
  using lexer_t = lexer<basic_json>;
  using token_type = typename lexer_t::token_type;

  enum class parse_event_t : uint8_t {
    /// the parser read `{` and started to process a JSON object
    object_start,
    /// the parser read `}` and finished processing a JSON object
    object_end,
    /// the parser read `[` and started to process a JSON array
    array_start,
    /// the parser read `]` and finished processing a JSON array
    array_end,
    /// the parser read a key of a value in an object
    key,
    /// the parser finished reading a JSON value
    value
  };

  using parser_callback_t = std::function<bool(int depth, parse_event_t event,
                                               basic_json& parsed)>;

  /// a parser reading from an input adapter
  explicit parser(detail::input_adapter_t adapter,
                  const parser_callback_t cb = nullptr,
                  const bool allow_exceptions_ = false)
      : callback(cb), m_lexer(adapter), allow_exceptions(allow_exceptions_) {}
  explicit parser(detail::input_adapter adapter,
                  const parser_callback_t cb = nullptr,
                  const bool allow_exceptions_ = false)
      : parser(static_cast<detail::input_adapter_t>(adapter), cb,
               allow_exceptions_) {}

  /*!
  @brief public parser interface

  @param[in] strict      whether to expect the last token to be EOF
  @param[in,out] result  parsed JSON value

  @throw parse_error.101 in case of an unexpected token
  @throw parse_error.102 if to_unicode fails or surrogate error
  @throw parse_error.103 if to_unicode fails
  */
  void parse(const bool strict, basic_json& result) {
    // read first token
    get_token();

    parse_internal(true, result);
    result.assert_invariant();

    // in strict mode, input must be completely read
    if (strict) {
      get_token();
      expect(token_type::end_of_input);
    }

    // in case of an error, return discarded value
    if (errored) {
      result = value_t::discarded;
      return;
    }

    // set top-level value to null if it was discarded by the callback
    // function
    if (result.is_discarded()) {
      result = nullptr;
    }
  }
  // Code Added to allow resuing a parser to parse multiple json values
  // from on input stream.
  bool at_eof() { return last_token == token_type::end_of_input; }
  bool test_eof() {
    get_token();
    return last_token == token_type::end_of_input;
  }
  void init() { get_token(); }
  // Try to parse a value, return false if parsing failed, use when
  // you're not sure if the input is json and it's ok if it's not.
  bool try_parse(basic_json& result, const bool strict = true) {
    get_token();
    parse_internal(true, result);
    if (errored) {
      goto error;
    }
    if (strict && test_eof()) {
      goto error;
    }
    return true;
  error:
    result = value_t::discarded;
    return false;
  }

  basic_json parse(const bool strict = true) {
    basic_json ret;
    parse(strict, ret);
    return ret;
  }
  basic_json parse_next() {
    if (at_eof()) {
      errored = true;
    }
    if (errored) {
      return value_t::discarded;
    }
    basic_json ret;
    parse_internal(true, ret);
    ret.assert_invariant();
    get_token();
    return ret;
  }

  /*!
  @brief public accept interface

  @param[in] strict  whether to expect the last token to be EOF
  @return whether the input is a proper JSON text
  */
  bool accept(const bool strict = true) {
    // read first token
    get_token();

    if (!accept_internal()) {
      return false;
    }

    // strict => last token must be EOF
    return !strict || (get_token() == token_type::end_of_input);
  }

  /*!
  @brief the actual parser
  @throw parse_error.101 in case of an unexpected token
  @throw parse_error.102 if to_unicode fails or surrogate error
  @throw parse_error.103 if to_unicode fails
  */
  void parse_internal(bool keep, basic_json& result) {
    // never parse after a parse error was detected
    assert(!errored);

    // start with a discarded value
    if (!result.is_discarded()) {
      result.m_value.destroy(result.m_type);
      result.m_type = value_t::discarded;
    }

    switch (last_token) {
    case token_type::begin_object: {
      if (keep) {
        if (callback) {
          keep = callback(depth++, parse_event_t::object_start, result);
        }

        if (!callback || keep) {
          // explicitly set result to object to cope with {}
          result.m_type = value_t::object;
          result.m_value = value_t::object;
        }
      }

      // read next token
      get_token();

      // closing } -> we are done
      if (last_token == token_type::end_object) {
        if (keep && callback &&
            !callback(--depth, parse_event_t::object_end, result)) {
          result.m_value.destroy(result.m_type);
          result.m_type = value_t::discarded;
        }
        break;
      }

      // parse values
      string_t key;
      basic_json value;
      while (true) {
        // store key
        if (!expect(token_type::value_string)) {
          return;
        }
        key = m_lexer.move_string();

        bool keep_tag = false;
        if (keep) {
          if (callback) {
            basic_json k(key);
            keep_tag = callback(depth, parse_event_t::key, k);
          } else {
            keep_tag = true;
          }
        }

        // parse separator (:)
        get_token();
        if (!expect(token_type::name_separator)) {
          return;
        }

        // parse and add value
        get_token();
        value.m_value.destroy(value.m_type);
        value.m_type = value_t::discarded;
        parse_internal(keep, value);

        if (JSON_UNLIKELY(errored)) {
          return;
        }

        if (keep && keep_tag && !value.is_discarded()) {
          result.m_value.object->emplace(std::move(key), std::move(value));
        }

        // comma -> next value
        get_token();
        if (last_token == token_type::value_separator) {
          get_token();
          continue;
        }

        // closing }
        if (!expect(token_type::end_object)) {
          return;
        }
        break;
      }

      if (keep && callback &&
          !callback(--depth, parse_event_t::object_end, result)) {
        result.m_value.destroy(result.m_type);
        result.m_type = value_t::discarded;
      }
      break;
    }

    case token_type::begin_array: {
      if (keep) {
        if (callback) {
          keep = callback(depth++, parse_event_t::array_start, result);
        }

        if (!callback || keep) {
          // explicitly set result to array to cope with []
          result.m_type = value_t::array;
          result.m_value = value_t::array;
        }
      }

      // read next token
      get_token();

      // closing ] -> we are done
      if (last_token == token_type::end_array) {
        if (callback && !callback(--depth, parse_event_t::array_end, result)) {
          result.m_value.destroy(result.m_type);
          result.m_type = value_t::discarded;
        }
        break;
      }

      // parse values
      basic_json value;
      while (true) {
        // parse value
        value.m_value.destroy(value.m_type);
        value.m_type = value_t::discarded;
        parse_internal(keep, value);

        if (JSON_UNLIKELY(errored)) {
          return;
        }

        if (keep && !value.is_discarded()) {
          result.m_value.array->push_back(std::move(value));
        }

        // comma -> next value
        get_token();
        if (last_token == token_type::value_separator) {
          get_token();
          continue;
        }

        // closing ]
        if (!expect(token_type::end_array)) {
          return;
        }
        break;
      }

      if (keep && callback &&
          !callback(--depth, parse_event_t::array_end, result)) {
        result.m_value.destroy(result.m_type);
        result.m_type = value_t::discarded;
      }
      break;
    }

    case token_type::literal_null: {
      result.m_type = value_t::null;
      break;
    }

    case token_type::value_string: {
      result.m_type = value_t::string;
      result.m_value = m_lexer.move_string();
      break;
    }

    case token_type::literal_true: {
      result.m_type = value_t::boolean;
      result.m_value = true;
      break;
    }

    case token_type::literal_false: {
      result.m_type = value_t::boolean;
      result.m_value = false;
      break;
    }

    case token_type::value_unsigned: {
      result.m_type = value_t::number_unsigned;
      result.m_value = m_lexer.get_number_unsigned();
      break;
    }

    case token_type::value_integer: {
      result.m_type = value_t::number_integer;
      result.m_value = m_lexer.get_number_integer();
      break;
    }

    case token_type::value_float: {
      result.m_type = value_t::number_float;
      result.m_value = m_lexer.get_number_float();

      // throw in case of infinity or NAN
      if (JSON_UNLIKELY(!std::isfinite(result.m_value.number_float))) {
        if (allow_exceptions) {
          JSON_THROW(out_of_range::create(406, "number overflow parsing '" +
                                                   m_lexer.get_token_string() +
                                                   "'"));
        }
        expect(token_type::uninitialized);
      }
      break;
    }

    case token_type::parse_error: {
      // using "uninitialized" to avoid "expected" message
      if (!expect(token_type::uninitialized)) {
        return;
      }
      break; // LCOV_EXCL_LINE
    }

    default: {
      // the last token was unexpected; we expected a value
      if (!expect(token_type::literal_or_value)) {
        return;
      }
      break; // LCOV_EXCL_LINE
    }
    }

    if (keep && callback && !callback(depth, parse_event_t::value, result)) {
      result.m_value.destroy(result.m_type);
      result.m_type = value_t::discarded;
    }
  }

  /*!
  @brief the actual acceptor

  @invariant 1. The last token is not yet processed. Therefore, the caller
                of this function must make sure a token has been read.
             2. When this function returns, the last token is processed.
                That is, the last read character was already considered.

  This invariant makes sure that no token needs to be "unput".
  */
  bool accept_internal() {
    switch (last_token) {
    case token_type::begin_object: {
      // read next token
      get_token();

      // closing } -> we are done
      if (last_token == token_type::end_object) {
        return true;
      }

      // parse values
      while (true) {
        // parse key
        if (last_token != token_type::value_string) {
          return false;
        }

        // parse separator (:)
        get_token();
        if (last_token != token_type::name_separator) {
          return false;
        }

        // parse value
        get_token();
        if (!accept_internal()) {
          return false;
        }

        // comma -> next value
        get_token();
        if (last_token == token_type::value_separator) {
          get_token();
          continue;
        }

        // closing }
        return (last_token == token_type::end_object);
      }
    }

    case token_type::begin_array: {
      // read next token
      get_token();

      // closing ] -> we are done
      if (last_token == token_type::end_array) {
        return true;
      }

      // parse values
      while (true) {
        // parse value
        if (!accept_internal()) {
          return false;
        }

        // comma -> next value
        get_token();
        if (last_token == token_type::value_separator) {
          get_token();
          continue;
        }

        // closing ]
        return (last_token == token_type::end_array);
      }
    }

    case token_type::value_float: {
      // reject infinity or NAN
      return std::isfinite(m_lexer.get_number_float());
    }

    case token_type::literal_false:
    case token_type::literal_null:
    case token_type::literal_true:
    case token_type::value_integer:
    case token_type::value_string:
    case token_type::value_unsigned: return true;

    default: // the last token was unexpected
      return false;
    }
  }

  /// get next token from lexer
  token_type get_token() { return (last_token = m_lexer.scan()); }

  /*!
  @throw parse_error.101 if expected token did not occur
  */
  bool expect(token_type t) {
    if (JSON_UNLIKELY(t != last_token)) {
      errored = true;
      expected = t;
      if (allow_exceptions) {
        throw_exception();
      } else {
        return false;
      }
    }

    return true;
  }

  [[noreturn]] void throw_exception() const {
    std::string error_msg = "syntax error - ";
    if (last_token == token_type::parse_error) {
      error_msg += std::string(m_lexer.get_error_message()) + "; last read: '" +
                   m_lexer.get_token_string() + "'";
    } else {
      error_msg +=
          "unexpected " + std::string(lexer_t::token_type_name(last_token));
    }

    if (expected != token_type::uninitialized) {
      error_msg +=
          "; expected " + std::string(lexer_t::token_type_name(expected));
    }

    JSON_THROW(parse_error::create(101, m_lexer.get_position(), error_msg));
  }

  /// current level of recursion
  int depth = 0;
  /// callback function
  const parser_callback_t callback = nullptr;
  /// the type of the last read token
  token_type last_token = token_type::uninitialized;
  /// the lexer
  lexer_t m_lexer;
  /// whether a syntax error occurred
  bool errored = false;
  /// possible reason for the syntax error
  token_type expected = token_type::uninitialized;
  /// whether to throw exceptions in case of errors
  const bool allow_exceptions = true;
};
} // namespace detail
} // namespace my_json

// <my_json/detail/iterators/primitive_iterator.hpp>

namespace my_json {
namespace detail {
/*
@brief an iterator for primitive JSON types

This class models an iterator for primitive JSON types (boolean, number,
string). It's only purpose is to allow the iterator/const_iterator classes
to "iterate" over primitive values. Internally, the iterator is modeled by
a `difference_type` variable. Value begin_value (`0`) models the begin,
end_value (`1`) models past the end.
*/
struct primitive_iterator_t {
  using difference_type = std::ptrdiff_t;
  static constexpr difference_type begin_value = 0;
  static constexpr difference_type end_value = begin_value + 1;

  /// iterator as signed integer type
  difference_type m_it = (std::numeric_limits<std::ptrdiff_t>::min)();

  constexpr difference_type get_value() const noexcept { return m_it; }

  /// set iterator to a defined beginning
  void set_begin() noexcept { m_it = begin_value; }

  /// set iterator to a defined past the end
  void set_end() noexcept { m_it = end_value; }

  /// return whether the iterator can be dereferenced
  constexpr bool is_begin() const noexcept { return m_it == begin_value; }

  /// return whether the iterator is at end
  constexpr bool is_end() const noexcept { return m_it == end_value; }

  friend constexpr bool operator==(primitive_iterator_t lhs,
                                   primitive_iterator_t rhs) noexcept {
    return lhs.m_it == rhs.m_it;
  }

  friend constexpr bool operator<(primitive_iterator_t lhs,
                                  primitive_iterator_t rhs) noexcept {
    return lhs.m_it < rhs.m_it;
  }

  primitive_iterator_t operator+(difference_type n) noexcept {
    auto result = *this;
    result += n;
    return result;
  }

  friend constexpr difference_type
  operator-(primitive_iterator_t lhs, primitive_iterator_t rhs) noexcept {
    return lhs.m_it - rhs.m_it;
  }

  primitive_iterator_t& operator++() noexcept {
    ++m_it;
    return *this;
  }

  primitive_iterator_t const operator++(int)noexcept {
    auto result = *this;
    m_it++;
    return result;
  }

  primitive_iterator_t& operator--() noexcept {
    --m_it;
    return *this;
  }

  primitive_iterator_t const operator--(int)noexcept {
    auto result = *this;
    m_it--;
    return result;
  }

  primitive_iterator_t& operator+=(difference_type n) noexcept {
    m_it += n;
    return *this;
  }

  primitive_iterator_t& operator-=(difference_type n) noexcept {
    m_it -= n;
    return *this;
  }
};
} // namespace detail
} // namespace my_json

// <my_json/detail/iterators/internal_iterator.hpp>

namespace my_json {
namespace detail {
/*!
@brief an iterator value

@note This structure could easily be a union, but MSVC currently does not allow
unions members with complex constructors, see
https://github.com/my_json/json/pull/105.
*/
template <typename basic_json> struct internal_iterator {
  /// iterator for JSON objects
  typename basic_json::object_t::iterator object_iterator{};
  /// iterator for JSON arrays
  typename basic_json::array_t::iterator array_iterator{};
  /// generic iterator for all other types
  primitive_iterator_t primitive_iterator{};
};
} // namespace detail
} // namespace my_json

//  <my_json/detail/iterators/iter_impl.hpp>

namespace my_json {
namespace detail {
// forward declare, to be able to friend it later on
template <typename IteratorType> class iteration_proxy;

/*!
@brief a template for a bidirectional iterator for the @ref basic_json class

This class implements a both iterators (iterator and const_iterator) for the
@ref basic_json class.

@note An iterator is called *initialized* when a pointer to a JSON value has
      been set (e.g., by a constructor or a copy assignment). If the iterator is
      default-constructed, it is *uninitialized* and most methods are undefined.
      **The library uses assertions to detect calls on uninitialized
iterators.**

@requirement The class satisfies the following concept requirements:
-
[BidirectionalIterator](http://en.cppreference.com/w/cpp/concept/BidirectionalIterator):
  The iterator that can be moved can be moved in both directions (i.e.
  incremented and decremented).

@since version 1.0.0, simplified in version 2.0.9, change to bidirectional
       iterators in version 3.0.0 (see
https://github.com/my_json/json/issues/593)
*/
template <typename basic_json> class iter_impl {
  /// allow basic_json to access private members
  friend iter_impl<
      typename std::conditional<std::is_const<basic_json>::value,
                                typename std::remove_const<basic_json>::type,
                                const basic_json>::type>;
  friend basic_json;
  friend iteration_proxy<iter_impl>;

  using object_t = typename basic_json::object_t;
  using array_t = typename basic_json::array_t;
  // make sure basic_json is basic_json or const basic_json
  static_assert(
      is_basic_json<typename std::remove_const<basic_json>::type>::value,
      "iter_impl only accepts (const) basic_json");

  /// The std::iterator class template (used as a base class to provide
  /// typedefs) is deprecated in C++17. The C++ Standard has never required
  /// user-defined iterators to derive from std::iterator. A user-defined
  /// iterator should provide publicly accessible typedefs named
  /// iterator_category, value_type, difference_type, pointer, and reference.
  /// Note that value_type is required to be non-const, even for constant
  /// iterators.
  using iterator_category = std::bidirectional_iterator_tag;

  /// the type of the values when the iterator is dereferenced
  using value_type = typename basic_json::value_type;
  /// a type to represent differences between iterators
  using difference_type = typename basic_json::difference_type;
  /// defines a pointer to the type iterated over (value_type)
  using pointer =
      typename std::conditional<std::is_const<basic_json>::value,
                                typename basic_json::const_pointer,
                                typename basic_json::pointer>::type;
  /// defines a reference to the type iterated over (value_type)
  using reference =
      typename std::conditional<std::is_const<basic_json>::value,
                                typename basic_json::const_reference,
                                typename basic_json::reference>::type;

  /// default constructor
  iter_impl() = default;

  /*!
  @brief constructor for a given JSON instance
  @param[in] object  pointer to a JSON object for this iterator
  @pre object != nullptr
  @post The iterator is initialized; i.e. `m_object != nullptr`.
  */
  explicit iter_impl(pointer object) noexcept : m_object(object) {
    assert(m_object != nullptr);

    switch (m_object->m_type) {
    case value_t::object: {
      m_it.object_iterator = typename object_t::iterator();
      break;
    }

    case value_t::array: {
      m_it.array_iterator = typename array_t::iterator();
      break;
    }

    default: {
      m_it.primitive_iterator = primitive_iterator_t();
      break;
    }
    }
  }

  /*!
  @note The conventional copy constructor and copy assignment are implicitly
        defined. Combined with the following converting constructor and
        assignment, they support: (1) copy from iterator to iterator, (2)
        copy from const iterator to const iterator, and (3) conversion from
        iterator to const iterator. However conversion from const iterator
        to iterator is not defined.
  */

  /*!
  @brief converting constructor
  @param[in] other  non-const iterator to copy from
  @note It is not checked whether @a other is initialized.
  */
  iter_impl(const iter_impl<typename std::remove_const<basic_json>::type>&
                other) noexcept
      : m_object(other.m_object), m_it(other.m_it) {}

  /*!
  @brief converting assignment
  @param[in,out] other  non-const iterator to copy from
  @return const/non-const iterator
  @note It is not checked whether @a other is initialized.
  */
  iter_impl&
  operator=(const iter_impl<typename std::remove_const<basic_json>::type>&
                other) noexcept {
    m_object = other.m_object;
    m_it = other.m_it;
    return *this;
  }

  /*!
  @brief set the iterator to the first value
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  void set_begin() noexcept {
    assert(m_object != nullptr);

    switch (m_object->m_type) {
    case value_t::object: {
      m_it.object_iterator = m_object->m_value.object->begin();
      break;
    }

    case value_t::array: {
      m_it.array_iterator = m_object->m_value.array->begin();
      break;
    }

    case value_t::null: {
      // set to end so begin()==end() is true: null is empty
      m_it.primitive_iterator.set_end();
      break;
    }

    default: {
      m_it.primitive_iterator.set_begin();
      break;
    }
    }
  }

  /*!
  @brief set the iterator past the last value
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  void set_end() noexcept {
    assert(m_object != nullptr);

    switch (m_object->m_type) {
    case value_t::object: {
      m_it.object_iterator = m_object->m_value.object->end();
      break;
    }

    case value_t::array: {
      m_it.array_iterator = m_object->m_value.array->end();
      break;
    }

    default: {
      m_it.primitive_iterator.set_end();
      break;
    }
    }
  }

  /*!
  @brief return a reference to the value pointed to by the iterator
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  reference operator*() const {
    assert(m_object != nullptr);

    switch (m_object->m_type) {
    case value_t::object: {
      assert(m_it.object_iterator != m_object->m_value.object->end());
      return m_it.object_iterator->second;
    }

    case value_t::array: {
      assert(m_it.array_iterator != m_object->m_value.array->end());
      return *m_it.array_iterator;
    }

    case value_t::null:
      JSON_THROW(invalid_iterator::create(214, "can!get value"));

    default: {
      if (JSON_LIKELY(m_it.primitive_iterator.is_begin())) {
        return *m_object;
      }

      JSON_THROW(invalid_iterator::create(214, "can!get value"));
    }
    }
  }

  /*!
  @brief dereference the iterator
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  pointer operator->() const {
    assert(m_object != nullptr);

    switch (m_object->m_type) {
    case value_t::object: {
      assert(m_it.object_iterator != m_object->m_value.object->end());
      return &(m_it.object_iterator->second);
    }

    case value_t::array: {
      assert(m_it.array_iterator != m_object->m_value.array->end());
      return &*m_it.array_iterator;
    }

    default: {
      if (JSON_LIKELY(m_it.primitive_iterator.is_begin())) {
        return m_object;
      }

      JSON_THROW(invalid_iterator::create(214, "can!get value"));
    }
    }
  }

  /*!
  @brief post-increment (it++)
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  iter_impl const operator++(int) {
    auto result = *this;
    ++(*this);
    return result;
  }

  /*!
  @brief pre-increment (++it)
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  iter_impl& operator++() {
    assert(m_object != nullptr);

    switch (m_object->m_type) {
    case value_t::object: {
      std::advance(m_it.object_iterator, 1);
      break;
    }

    case value_t::array: {
      std::advance(m_it.array_iterator, 1);
      break;
    }

    default: {
      ++m_it.primitive_iterator;
      break;
    }
    }

    return *this;
  }

  /*!
  @brief post-decrement (it--)
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  iter_impl const operator--(int) {
    auto result = *this;
    --(*this);
    return result;
  }

  /*!
  @brief pre-decrement (--it)
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  iter_impl& operator--() {
    assert(m_object != nullptr);

    switch (m_object->m_type) {
    case value_t::object: {
      std::advance(m_it.object_iterator, -1);
      break;
    }

    case value_t::array: {
      std::advance(m_it.array_iterator, -1);
      break;
    }

    default: {
      --m_it.primitive_iterator;
      break;
    }
    }

    return *this;
  }

  /*!
  @brief  comparison: equal
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  bool operator==(const iter_impl& other) const {
    // if objects are not the same, the comparison is undefined
    if (JSON_UNLIKELY(m_object != other.m_object)) {
      JSON_THROW(invalid_iterator::create(
          212, "can!compare iterators of different containers"));
    }

    assert(m_object != nullptr);

    switch (m_object->m_type) {
    case value_t::object:
      return (m_it.object_iterator == other.m_it.object_iterator);

    case value_t::array:
      return (m_it.array_iterator == other.m_it.array_iterator);

    default: return (m_it.primitive_iterator == other.m_it.primitive_iterator);
    }
  }

  /*!
  @brief  comparison: not equal
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  bool operator!=(const iter_impl& other) const { return !operator==(other); }

  /*!
  @brief  comparison: smaller
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  bool operator<(const iter_impl& other) const {
    // if objects are not the same, the comparison is undefined
    if (JSON_UNLIKELY(m_object != other.m_object)) {
      JSON_THROW(invalid_iterator::create(
          212, "can!compare iterators of different containers"));
    }

    assert(m_object != nullptr);

    switch (m_object->m_type) {
    case value_t::object:
      JSON_THROW(invalid_iterator::create(
          213, "can!compare order of object iterators"));

    case value_t::array:
      return (m_it.array_iterator < other.m_it.array_iterator);

    default: return (m_it.primitive_iterator < other.m_it.primitive_iterator);
    }
  }

  /*!
  @brief  comparison: less than or equal
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  bool operator<=(const iter_impl& other) const {
    return !other.operator<(*this);
  }

  /*!
  @brief  comparison: greater than
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  bool operator>(const iter_impl& other) const { return !operator<=(other); }

  /*!
  @brief  comparison: greater than or equal
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  bool operator>=(const iter_impl& other) const { return !operator<(other); }

  /*!
  @brief  add to iterator
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  iter_impl& operator+=(difference_type i) {
    assert(m_object != nullptr);

    switch (m_object->m_type) {
    case value_t::object:
      JSON_THROW(invalid_iterator::create(
          209, "can!use offsets with object iterators"));

    case value_t::array: {
      std::advance(m_it.array_iterator, i);
      break;
    }

    default: {
      m_it.primitive_iterator += i;
      break;
    }
    }

    return *this;
  }

  /*!
  @brief  subtract from iterator
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  iter_impl& operator-=(difference_type i) { return operator+=(-i); }

  /*!
  @brief  add to iterator
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  iter_impl operator+(difference_type i) const {
    auto result = *this;
    result += i;
    return result;
  }

  /*!
  @brief  addition of distance and iterator
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  friend iter_impl operator+(difference_type i, const iter_impl& it) {
    auto result = it;
    result += i;
    return result;
  }

  /*!
  @brief  subtract from iterator
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  iter_impl operator-(difference_type i) const {
    auto result = *this;
    result -= i;
    return result;
  }

  /*!
  @brief  return difference
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  difference_type operator-(const iter_impl& other) const {
    assert(m_object != nullptr);

    switch (m_object->m_type) {
    case value_t::object:
      JSON_THROW(invalid_iterator::create(
          209, "can!use offsets with object iterators"));

    case value_t::array: return m_it.array_iterator - other.m_it.array_iterator;

    default: return m_it.primitive_iterator - other.m_it.primitive_iterator;
    }
  }

  /*!
  @brief  access to successor
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  reference operator[](difference_type n) const {
    assert(m_object != nullptr);

    switch (m_object->m_type) {
    case value_t::object:
      JSON_THROW(invalid_iterator::create(
          208, "can!use operator[] for object iterators"));

    case value_t::array: return *std::next(m_it.array_iterator, n);

    case value_t::null:
      JSON_THROW(invalid_iterator::create(214, "can!get value"));

    default: {
      if (JSON_LIKELY(m_it.primitive_iterator.get_value() == -n)) {
        return *m_object;
      }

      JSON_THROW(invalid_iterator::create(214, "can!get value"));
    }
    }
  }

  /*!
  @brief  return the key of an object iterator
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  typename object_t::key_type key() const {
    assert(m_object != nullptr);

    if (JSON_LIKELY(m_object->is_object())) {
      return m_it.object_iterator->first;
    }

    JSON_THROW(invalid_iterator::create(
        207, "can!use key() for non-object iterators"));
  }

  /*!
  @brief  return the value of an iterator
  @pre The iterator is initialized; i.e. `m_object != nullptr`.
  */
  reference value() const { return operator*(); }

  /// associated JSON instance
  pointer m_object = nullptr;
  /// the actual iterator of the associated instance
  internal_iterator<typename std::remove_const<basic_json>::type> m_it;
};
} // namespace detail
} // namespace my_json

// <my_json/detail/iterators/iteration_proxy.hpp>

namespace my_json {
namespace detail {
/// proxy class for the items() function
template <typename IteratorType> class iteration_proxy {
  /// helper class for iteration
  class iteration_proxy_internal {
  private:
    /// the iterator
    IteratorType anchor;
    /// an index for arrays (used to create key names)
    std::size_t array_index = 0;

  public:
    explicit iteration_proxy_internal(IteratorType it) noexcept : anchor(it) {}

    /// dereference operator (needed for range-based for)
    iteration_proxy_internal& operator*() { return *this; }

    /// increment operator (needed for range-based for)
    iteration_proxy_internal& operator++() {
      ++anchor;
      ++array_index;

      return *this;
    }

    /// inequality operator (needed for range-based for)
    bool operator!=(const iteration_proxy_internal& o) const noexcept {
      return anchor != o.anchor;
    }

    /// return key of the iterator
    std::string key() const {
      assert(anchor.m_object != nullptr);

      switch (anchor.m_object->type()) {
      // use integer array index as key
      case value_t::array:
        return std::to_string(array_index);

      // use key from the object
      case value_t::object:
        return anchor.key();

      // use an empty key for all primitive types
      default: return "";
      }
    }

    /// return value of the iterator
    typename IteratorType::reference value() const { return anchor.value(); }
  };

  /// the container to iterate
  typename IteratorType::reference container;

  /// construct iteration proxy from a container
  explicit iteration_proxy(typename IteratorType::reference cont) noexcept
      : container(cont) {}

  /// return iterator begin (needed for range-based for)
  iteration_proxy_internal begin() noexcept {
    return iteration_proxy_internal(container.begin());
  }

  /// return iterator end (needed for range-based for)
  iteration_proxy_internal end() noexcept {
    return iteration_proxy_internal(container.end());
  }
};
} // namespace detail
} // namespace my_json

// <my_json/detail/iterators/json_reverse_iterator.hpp>
namespace my_json {
namespace detail {
//////////////////////
// reverse_iterator //
//////////////////////

/*!
@brief a template for a reverse iterator class

@tparam Base the base iterator type to reverse. Valid types are @ref
iterator (to create @ref reverse_iterator) and @ref const_iterator (to
create @ref const_reverse_iterator).

@requirement The class satisfies the following concept requirements:
-
[BidirectionalIterator](http://en.cppreference.com/w/cpp/concept/BidirectionalIterator):
  The iterator that can be moved can be moved in both directions (i.e.
  incremented and decremented).
- [OutputIterator](http://en.cppreference.com/w/cpp/concept/OutputIterator):
  It is possible to write to the pointed-to element (only if @a Base is
  @ref iterator).

@since version 1.0.0
*/
template <typename Base>
struct json_reverse_iterator : public std::reverse_iterator<Base> {
  using difference_type = std::ptrdiff_t;
  /// shortcut to the reverse iterator adapter
  using base_iterator = std::reverse_iterator<Base>;
  /// the reference type for the pointed-to element
  using reference = typename Base::reference;

  /// create reverse iterator from iterator
  json_reverse_iterator(
      const typename base_iterator::iterator_type& it) noexcept
      : base_iterator(it) {}

  /// create reverse iterator from base class
  json_reverse_iterator(const base_iterator& it) noexcept : base_iterator(it) {}

  /// post-increment (it++)
  json_reverse_iterator const operator++(int) {
    return static_cast<json_reverse_iterator>(base_iterator::operator++(1));
  }

  /// pre-increment (++it)
  json_reverse_iterator& operator++() {
    return static_cast<json_reverse_iterator&>(base_iterator::operator++());
  }

  /// post-decrement (it--)
  json_reverse_iterator const operator--(int) {
    return static_cast<json_reverse_iterator>(base_iterator::operator--(1));
  }

  /// pre-decrement (--it)
  json_reverse_iterator& operator--() {
    return static_cast<json_reverse_iterator&>(base_iterator::operator--());
  }

  /// add to iterator
  json_reverse_iterator& operator+=(difference_type i) {
    return static_cast<json_reverse_iterator&>(base_iterator::operator+=(i));
  }

  /// add to iterator
  json_reverse_iterator operator+(difference_type i) const {
    return static_cast<json_reverse_iterator>(base_iterator::operator+(i));
  }

  /// subtract from iterator
  json_reverse_iterator operator-(difference_type i) const {
    return static_cast<json_reverse_iterator>(base_iterator::operator-(i));
  }

  /// return difference
  difference_type operator-(const json_reverse_iterator& other) const {
    return base_iterator(*this) - base_iterator(other);
  }

  /// access to successor
  reference operator[](difference_type n) const {
    return *(this->operator+(n));
  }

  /// return the key of an object iterator
  auto key() const -> decltype(std::declval<Base>().key()) {
    auto it = --this->base();
    return it.key();
  }

  /// return the value of an iterator
  reference value() const {
    auto it = --this->base();
    return it.operator*();
  }
};
} // namespace detail
} // namespace my_json

// <my_json/detail/output/output_adapters.hpp>
namespace my_json {
namespace detail {
/// abstract output adapter interface
template <typename CharType> struct output_adapter_protocol {
  virtual void write_character(CharType c) = 0;
  virtual void write_characters(const CharType* s, std::size_t length) = 0;
  virtual ~output_adapter_protocol() = default;
};

/// a type to simplify interfaces
template <typename CharType>
using output_adapter_t = std::shared_ptr<output_adapter_protocol<CharType>>;

/// output adapter for byte vectors
template <typename CharType>
struct output_vector_adapter : public output_adapter_protocol<CharType> {
  explicit output_vector_adapter(std::vector<CharType>& vec) : v(vec) {}

  void write_character(CharType c) override { v.push_back(c); }

  void write_characters(const CharType* s, std::size_t length) override {
    std::copy(s, s + length, std::back_inserter(v));
  }

  std::vector<CharType>& v;
};

#if USE_IOSTREAMS
/// output adapter for output streams
template <typename CharType>
struct output_stream_adapter : public output_adapter_protocol<CharType> {
  explicit output_stream_adapter(std::basic_ostream<CharType>& s) : stream(s) {}

  void write_character(CharType c) override { stream.put(c); }

  void write_characters(const CharType* s, std::size_t length) override {
    stream.write(s, static_cast<std::streamsize>(length));
  }

  std::basic_ostream<CharType>& stream;
};
#endif // USE_IO_STREAMS
/// output adapter for basic_string
template <typename CharType, typename StringType = std::basic_string<CharType>>
struct output_string_adapter : public output_adapter_protocol<CharType> {
  explicit output_string_adapter(StringType& s) : str(s) {}

  void write_character(CharType c) override { str.push_back(c); }

  void write_characters(const CharType* s, std::size_t length) override {
    str.append(s, length);
  }

  StringType& str;
};
template <typename CharType, typename OutputIt,
          typename std::enable_if<
              std::is_base_of<std::output_iterator_tag,
                              typename std::iterator_traits<
                                  OutputIt>::iterator_category>::value,
              int>::type = 0>
struct output_iter_adapter : public output_adapter_protocol<CharType> {
  explicit output_iter_adapter(OutputIt& it) : it{it} {}
  void write_character(CharType c) override {
    *it = c;
    ++it;
  }
  void write_characters(const CharType* s, std::size_t length) override {
    it = std::copy_n(s, length, it);
  }
  OutputIt it;
};
template <typename CharType>
struct output_FILE_adapter : public output_adapter_protocol<CharType> {
  explicit output_FILE_adapter(std::FILE* f) : f{f} {};
  void write_character(CharType c) override { std::fputc(c, f); }
  void write_characters(const CharType* s, std::size_t length) override {
    std::fwrite(s, sizeof(CharType), length, f);
  }
  std::FILE* f;
};

template <typename CharType, typename StringType = std::basic_string<CharType>>
struct output_adapter {
  output_adapter(std::vector<CharType>& vec)
      : oa(std::make_shared<output_vector_adapter<CharType>>(vec)) {}
#if USE_IOSTREAMS
  output_adapter(std::basic_ostream<CharType>& s)
      : oa(std::make_shared<output_stream_adapter<CharType>>(s)) {}
#endif
  output_adapter(StringType& s)
      : oa(std::make_shared<output_string_adapter<CharType, StringType>>(s)) {}
  output_adapter(std::FILE* f)
      : oa(std::make_shared<output_FILE_adapter<CharType>>(f)) {}
  template <typename OutputIt,
            typename std::enable_if<
                std::is_base_of<std::output_iterator_tag,
                                typename std::iterator_traits<
                                    OutputIt>::iterator_category>::value,
                int>::type = 0>
  output_adapter(OutputIt& it)
      : oa(std::make_shared<output_iter_adapter<CharType, OutputIt>>(it)) {}

  operator output_adapter_t<CharType>() { return oa; }

  output_adapter_t<CharType> oa = nullptr;
};
} // namespace detail
} // namespace my_json

// <my_json/detail/output/serializer.hpp>

namespace my_json {
namespace detail {

/*!
@brief implements the Grisu2 algorithm for binary to decimal floating-point
conversion.

This implementation is a slightly modified version of the reference
implementation which may be obtained from
http://florian.loitsch.com/publications (bench.tar.gz).

The code is distributed under the MIT license, Copyright (c) 2009 Florian
Loitsch.

For a detailed description of the algorithm see:

[1] Loitsch, "Printing Floating-Point Numbers Quickly and Accurately with
    Integers", Proceedings of the ACM SIGPLAN 2010 Conference on Programming
    Language Design and Implementation, PLDI 2010
[2] Burger, Dybvig, "Printing Floating-Point Numbers Quickly and Accurately",
    Proceedings of the ACM SIGPLAN 1996 Conference on Programming Language
    Design and Implementation, PLDI 1996
*/
namespace dtoa_impl {

template <typename Target, typename Source>
Target reinterpret_bits(const Source source) {
  static_assert(sizeof(Target) == sizeof(Source), "size mismatch");

  Target target;
  std::memcpy(&target, &source, sizeof(Source));
  return target;
}

struct diyfp // f * 2^e
{
  static constexpr int kPrecision = 64; // = q

  uint64_t f;
  int e;

  constexpr diyfp() noexcept : f(0), e(0) {}
  constexpr diyfp(uint64_t f_, int e_) noexcept : f(f_), e(e_) {}

  /*!
  @brief returns x - y
  @pre x.e == y.e and x.f >= y.f
  */
  static diyfp sub(const diyfp& x, const diyfp& y) noexcept {
    assert(x.e == y.e);
    assert(x.f >= y.f);

    return diyfp(x.f - y.f, x.e);
  }

  /*!
  @brief returns x * y
  @note The result is rounded. (Only the upper q bits are returned.)
  */
  static diyfp mul(const diyfp& x, const diyfp& y) noexcept {
    static_assert(kPrecision == 64, "internal error");

    // Computes:
    //  f = round((x.f * y.f) / 2^q)
    //  e = x.e + y.e + q

    // Emulate the 64-bit * 64-bit multiplication:
    //
    // p = u * v
    //   = (u_lo + 2^32 u_hi) (v_lo + 2^32 v_hi)
    //   = (u_lo v_lo         ) + 2^32 ((u_lo v_hi         ) + (u_hi v_lo
    //   )) + 2^64 (u_hi v_hi         ) = (p0                ) + 2^32 ((p1
    //   ) + (p2                )) + 2^64 (p3                ) = (p0_lo + 2^32
    //   p0_hi) + 2^32 ((p1_lo + 2^32 p1_hi) + (p2_lo + 2^32 p2_hi)) + 2^64 (p3
    //   ) = (p0_lo             ) + 2^32 (p0_hi + p1_lo + p2_lo
    //   ) + 2^64 (p1_hi + p2_hi + p3) = (p0_lo             ) + 2^32 (Q
    //   ) + 2^64 (H                 ) = (p0_lo             ) + 2^32 (Q_lo +
    //   2^32 Q_hi                           ) + 2^64 (H                 )
    //
    // (Since Q might be larger than 2^32 - 1)
    //
    //   = (p0_lo + 2^32 Q_lo) + 2^64 (Q_hi + H)
    //
    // (Q_hi + H does not overflow a 64-bit int)
    //
    //   = p_lo + 2^64 p_hi

    const uint64_t u_lo = x.f & 0xFFFFFFFF;
    const uint64_t u_hi = x.f >> 32;
    const uint64_t v_lo = y.f & 0xFFFFFFFF;
    const uint64_t v_hi = y.f >> 32;

    const uint64_t p0 = u_lo * v_lo;
    const uint64_t p1 = u_lo * v_hi;
    const uint64_t p2 = u_hi * v_lo;
    const uint64_t p3 = u_hi * v_hi;

    const uint64_t p0_hi = p0 >> 32;
    const uint64_t p1_lo = p1 & 0xFFFFFFFF;
    const uint64_t p1_hi = p1 >> 32;
    const uint64_t p2_lo = p2 & 0xFFFFFFFF;
    const uint64_t p2_hi = p2 >> 32;

    uint64_t Q = p0_hi + p1_lo + p2_lo;

    // The full product might now be computed as
    //
    // p_hi = p3 + p2_hi + p1_hi + (Q >> 32)
    // p_lo = p0_lo + (Q << 32)
    //
    // But in this particular case here, the full p_lo is not required.
    // Effectively we only need to add the highest bit in p_lo to p_hi (and
    // Q_hi + 1 does not overflow).

    Q += uint64_t{1} << (64 - 32 - 1); // round, ties up

    const uint64_t h = p3 + p2_hi + p1_hi + (Q >> 32);

    return diyfp(h, x.e + y.e + 64);
  }

  /*!
  @brief normalize x such that the significand is >= 2^(q-1)
  @pre x.f != 0
  */
  static diyfp normalize(diyfp x) noexcept {
    assert(x.f != 0);

    while ((x.f >> 63) == 0) {
      x.f <<= 1;
      x.e--;
    }

    return x;
  }

  /*!
  @brief normalize x such that the result has the exponent E
  @pre e >= x.e and the upper e - x.e bits of x.f must be zero.
  */
  static diyfp normalize_to(const diyfp& x,
                            const int target_exponent) noexcept {
    const int delta = x.e - target_exponent;

    assert(delta >= 0);
    assert(((x.f << delta) >> delta) == x.f);

    return diyfp(x.f << delta, target_exponent);
  }
};

struct boundaries {
  diyfp w;
  diyfp minus;
  diyfp plus;
};

/*!
Compute the (normalized) diyfp representing the input number 'value' and its
boundaries.

@pre value must be finite and positive
*/
template <typename FloatType> boundaries compute_boundaries(FloatType value) {
  assert(std::isfinite(value));
  assert(value > 0);

  // Convert the IEEE representation into a diyfp.
  //
  // If v is denormal:
  //      value = 0.F * 2^(1 - bias) = (          F) * 2^(1 - bias - (p-1))
  // If v is normalized:
  //      value = 1.F * 2^(E - bias) = (2^(p-1) + F) * 2^(E - bias - (p-1))

  static_assert(std::numeric_limits<FloatType>::is_iec559,
                "internal error: dtoa_short requires an IEEE-754 "
                "floating-point implementation");

  constexpr int kPrecision =
      std::numeric_limits<FloatType>::digits; // = p (includes the hidden bit)
  constexpr int kBias =
      std::numeric_limits<FloatType>::max_exponent - 1 + (kPrecision - 1);
  constexpr int kMinExp = 1 - kBias;
  constexpr uint64_t kHiddenBit = uint64_t{1} << (kPrecision - 1); // = 2^(p-1)

  using bits_type =
      typename std::conditional<kPrecision == 24, uint32_t, uint64_t>::type;

  const uint64_t bits = reinterpret_bits<bits_type>(value);
  const uint64_t E = bits >> (kPrecision - 1);
  const uint64_t F = bits & (kHiddenBit - 1);

  const bool is_denormal = (E == 0);
  const diyfp v = is_denormal ?
                      diyfp(F, kMinExp) :
                      diyfp(F + kHiddenBit, static_cast<int>(E) - kBias);

  // Compute the boundaries m- and m+ of the floating-point value
  // v = f * 2^e.
  //
  // Determine v- and v+, the floating-point predecessor and successor if v,
  // respectively.
  //
  //      v- = v - 2^e        if f != 2^(p-1) or e == e_min                (A)
  //         = v - 2^(e-1)    if f == 2^(p-1) and e > e_min                (B)
  //
  //      v+ = v + 2^e
  //
  // Let m- = (v- + v) / 2 and m+ = (v + v+) / 2. All real numbers _strictly_
  // between m- and m+ round to v, regardless of how the input rounding
  // algorithm breaks ties.
  //
  //      ---+-------------+-------------+-------------+-------------+---  (A)
  //         v-            m-            v             m+            v+
  //
  //      -----------------+------+------+-------------+-------------+---  (B)
  //                       v-     m-     v             m+            v+

  const bool lower_boundary_is_closer = (F == 0 && E > 1);
  const diyfp m_plus = diyfp(2 * v.f + 1, v.e - 1);
  const diyfp m_minus = lower_boundary_is_closer ?
                            diyfp(4 * v.f - 1, v.e - 2) // (B)
                            :
                            diyfp(2 * v.f - 1, v.e - 1); // (A)

  // Determine the normalized w+ = m+.
  const diyfp w_plus = diyfp::normalize(m_plus);

  // Determine w- = m- such that e_(w-) = e_(w+).
  const diyfp w_minus = diyfp::normalize_to(m_minus, w_plus.e);

  return {diyfp::normalize(v), w_minus, w_plus};
}

// Given normalized diyfp w, Grisu needs to find a (normalized) cached
// power-of-ten c, such that the exponent of the product c * w = f * 2^e lies
// within a certain range [alpha, gamma] (Definition 3.2 from [1])
//
//      alpha <= e = e_c + e_w + q <= gamma
//
// or
//
//      f_c * f_w * 2^alpha <= f_c 2^(e_c) * f_w 2^(e_w) * 2^q
//                          <= f_c * f_w * 2^gamma
//
// Since c and w are normalized, i.e. 2^(q-1) <= f < 2^q, this implies
//
//      2^(q-1) * 2^(q-1) * 2^alpha <= c * w * 2^q < 2^q * 2^q * 2^gamma
//
// or
//
//      2^(q - 2 + alpha) <= c * w < 2^(q + gamma)
//
// The choice of (alpha,gamma) determines the size of the table and the form of
// the digit generation procedure. Using (alpha,gamma)=(-60,-32) works out well
// in practice:
//
// The idea is to cut the number c * w = f * 2^e into two parts, which can be
// processed independently: An integral part p1, and a fractional part p2:
//
//      f * 2^e = ( (f div 2^-e) * 2^-e + (f mod 2^-e) ) * 2^e
//              = (f div 2^-e) + (f mod 2^-e) * 2^e
//              = p1 + p2 * 2^e
//
// The conversion of p1 into decimal form requires a series of divisions and
// modulos by (a power of) 10. These operations are faster for 32-bit than for
// 64-bit integers, so p1 should ideally fit into a 32-bit integer. This can be
// achieved by choosing
//
//      -e >= 32   or   e <= -32 := gamma
//
// In order to convert the fractional part
//
//      p2 * 2^e = p2 / 2^-e = d[-1] / 10^1 + d[-2] / 10^2 + ...
//
// into decimal form, the fraction is repeatedly multiplied by 10 and the digits
// d[-i] are extracted in order:
//
//      (10 * p2) div 2^-e = d[-1]
//      (10 * p2) mod 2^-e = d[-2] / 10^1 + ...
//
// The multiplication by 10 must not overflow. It is sufficient to choose
//
//      10 * p2 < 16 * p2 = 2^4 * p2 <= 2^64.
//
// Since p2 = f mod 2^-e < 2^-e,
//
//      -e <= 60   or   e >= -60 := alpha

constexpr int kAlpha = -60;
constexpr int kGamma = -32;

struct cached_power // c = f * 2^e ~= 10^k
{
  uint64_t f;
  int e;
  int k;
};

/*!
For a normalized diyfp w = f * 2^e, this function returns a (normalized) cached
power-of-ten c = f_c * 2^e_c, such that the exponent of the product w * c
satisfies (Definition 3.2 from [1])

     alpha <= e_c + e + q <= gamma.
*/
inline cached_power get_cached_power_for_binary_exponent(int e) {
  // Now
  //
  //      alpha <= e_c + e + q <= gamma                                    (1)
  //      ==> f_c * 2^alpha <= c * 2^e * 2^q
  //
  // and since the c's are normalized, 2^(q-1) <= f_c,
  //
  //      ==> 2^(q - 1 + alpha) <= c * 2^(e + q)
  //      ==> 2^(alpha - e - 1) <= c
  //
  // If c were an exakt power of ten, i.e. c = 10^k, one may determine k as
  //
  //      k = ceil( log_10( 2^(alpha - e - 1) ) )
  //        = ceil( (alpha - e - 1) * log_10(2) )
  //
  // From the paper:
  // "In theory the result of the procedure could be wrong since c is rounded,
  //  and the computation itself is approximated [...]. In practice, however,
  //  this simple function is sufficient."
  //
  // For IEEE double precision floating-point numbers converted into
  // normalized diyfp's w = f * 2^e, with q = 64,
  //
  //      e >= -1022      (min IEEE exponent)
  //           -52        (p - 1)
  //           -52        (p - 1, possibly normalize denormal IEEE numbers)
  //           -11        (normalize the diyfp)
  //         = -1137
  //
  // and
  //
  //      e <= +1023      (max IEEE exponent)
  //           -52        (p - 1)
  //           -11        (normalize the diyfp)
  //         = 960
  //
  // This binary exponent range [-1137,960] results in a decimal exponent
  // range [-307,324]. One does not need to store a cached power for each
  // k in this range. For each such k it suffices to find a cached power
  // such that the exponent of the product lies in [alpha,gamma].
  // This implies that the difference of the decimal exponents of adjacent
  // table entries must be less than or equal to
  //
  //      floor( (gamma - alpha) * log_10(2) ) = 8.
  //
  // (A smaller distance gamma-alpha would require a larger table.)

  // NB:
  // Actually this function returns c, such that -60 <= e_c + e + 64 <= -34.

  constexpr int kCachedPowersSize = 79;
  constexpr int kCachedPowersMinDecExp = -300;
  constexpr int kCachedPowersDecStep = 8;

  static constexpr cached_power kCachedPowers[] = {
      {0xAB70FE17C79AC6CA, -1060, -300}, {0xFF77B1FCBEBCDC4F, -1034, -292},
      {0xBE5691EF416BD60C, -1007, -284}, {0x8DD01FAD907FFC3C, -980, -276},
      {0xD3515C2831559A83, -954, -268},  {0x9D71AC8FADA6C9B5, -927, -260},
      {0xEA9C227723EE8BCB, -901, -252},  {0xAECC49914078536D, -874, -244},
      {0x823C12795DB6CE57, -847, -236},  {0xC21094364DFB5637, -821, -228},
      {0x9096EA6F3848984F, -794, -220},  {0xD77485CB25823AC7, -768, -212},
      {0xA086CFCD97BF97F4, -741, -204},  {0xEF340A98172AACE5, -715, -196},
      {0xB23867FB2A35B28E, -688, -188},  {0x84C8D4DFD2C63F3B, -661, -180},
      {0xC5DD44271AD3CDBA, -635, -172},  {0x936B9FCEBB25C996, -608, -164},
      {0xDBAC6C247D62A584, -582, -156},  {0xA3AB66580D5FDAF6, -555, -148},
      {0xF3E2F893DEC3F126, -529, -140},  {0xB5B5ADA8AAFF80B8, -502, -132},
      {0x87625F056C7C4A8B, -475, -124},  {0xC9BCFF6034C13053, -449, -116},
      {0x964E858C91BA2655, -422, -108},  {0xDFF9772470297EBD, -396, -100},
      {0xA6DFBD9FB8E5B88F, -369, -92},   {0xF8A95FCF88747D94, -343, -84},
      {0xB94470938FA89BCF, -316, -76},   {0x8A08F0F8BF0F156B, -289, -68},
      {0xCDB02555653131B6, -263, -60},   {0x993FE2C6D07B7FAC, -236, -52},
      {0xE45C10C42A2B3B06, -210, -44},   {0xAA242499697392D3, -183, -36},
      {0xFD87B5F28300CA0E, -157, -28},   {0xBCE5086492111AEB, -130, -20},
      {0x8CBCCC096F5088CC, -103, -12},   {0xD1B71758E219652C, -77, -4},
      {0x9C40000000000000, -50, 4},      {0xE8D4A51000000000, -24, 12},
      {0xAD78EBC5AC620000, 3, 20},       {0x813F3978F8940984, 30, 28},
      {0xC097CE7BC90715B3, 56, 36},      {0x8F7E32CE7BEA5C70, 83, 44},
      {0xD5D238A4ABE98068, 109, 52},     {0x9F4F2726179A2245, 136, 60},
      {0xED63A231D4C4FB27, 162, 68},     {0xB0DE65388CC8ADA8, 189, 76},
      {0x83C7088E1AAB65DB, 216, 84},     {0xC45D1DF942711D9A, 242, 92},
      {0x924D692CA61BE758, 269, 100},    {0xDA01EE641A708DEA, 295, 108},
      {0xA26DA3999AEF774A, 322, 116},    {0xF209787BB47D6B85, 348, 124},
      {0xB454E4A179DD1877, 375, 132},    {0x865B86925B9BC5C2, 402, 140},
      {0xC83553C5C8965D3D, 428, 148},    {0x952AB45CFA97A0B3, 455, 156},
      {0xDE469FBD99A05FE3, 481, 164},    {0xA59BC234DB398C25, 508, 172},
      {0xF6C69A72A3989F5C, 534, 180},    {0xB7DCBF5354E9BECE, 561, 188},
      {0x88FCF317F22241E2, 588, 196},    {0xCC20CE9BD35C78A5, 614, 204},
      {0x98165AF37B2153DF, 641, 212},    {0xE2A0B5DC971F303A, 667, 220},
      {0xA8D9D1535CE3B396, 694, 228},    {0xFB9B7CD9A4A7443C, 720, 236},
      {0xBB764C4CA7A44410, 747, 244},    {0x8BAB8EEFB6409C1A, 774, 252},
      {0xD01FEF10A657842C, 800, 260},    {0x9B10A4E5E9913129, 827, 268},
      {0xE7109BFBA19C0C9D, 853, 276},    {0xAC2820D9623BF429, 880, 284},
      {0x80444B5E7AA7CF85, 907, 292},    {0xBF21E44003ACDD2D, 933, 300},
      {0x8E679C2F5E44FF8F, 960, 308},    {0xD433179D9C8CB841, 986, 316},
      {0x9E19DB92B4E31BA9, 1013, 324},
  };

  // This computation gives exactly the same results for k as
  //      k = ceil((kAlpha - e - 1) * 0.30102999566398114)
  // for |e| <= 1500, but doesn't require floating-point operations.
  // NB: log_10(2) ~= 78913 / 2^18
  assert(e >= -1500);
  assert(e <= 1500);
  const int f = kAlpha - e - 1;
  const int k = (f * 78913) / (1 << 18) + (f > 0);

  const int index = (-kCachedPowersMinDecExp + k + (kCachedPowersDecStep - 1)) /
                    kCachedPowersDecStep;
  assert(index >= 0);
  assert(index < kCachedPowersSize);
  static_cast<void>(kCachedPowersSize); // Fix warning.

  const cached_power cached = kCachedPowers[index];
  assert(kAlpha <= cached.e + e + 64);
  assert(kGamma >= cached.e + e + 64);

  return cached;
}

/*!
For n != 0, returns k, such that pow10 := 10^(k-1) <= n < 10^k.
For n == 0, returns 1 and sets pow10 := 1.
*/
inline int find_largest_pow10(const uint32_t n, uint32_t& pow10) {
  // LCOV_EXCL_START
  if (n >= 1000000000) {
    pow10 = 1000000000;
    return 10;
  }
  // LCOV_EXCL_STOP
  else if (n >= 100000000) {
    pow10 = 100000000;
    return 9;
  } else if (n >= 10000000) {
    pow10 = 10000000;
    return 8;
  } else if (n >= 1000000) {
    pow10 = 1000000;
    return 7;
  } else if (n >= 100000) {
    pow10 = 100000;
    return 6;
  } else if (n >= 10000) {
    pow10 = 10000;
    return 5;
  } else if (n >= 1000) {
    pow10 = 1000;
    return 4;
  } else if (n >= 100) {
    pow10 = 100;
    return 3;
  } else if (n >= 10) {
    pow10 = 10;
    return 2;
  } else {
    pow10 = 1;
    return 1;
  }
}

inline void grisu2_round(char* buf, int len, uint64_t dist, uint64_t delta,
                         uint64_t rest, uint64_t ten_k) {
  assert(len >= 1);
  assert(dist <= delta);
  assert(rest <= delta);
  assert(ten_k > 0);

  //               <--------------------------- delta ---->
  //                                  <---- dist --------->
  // --------------[------------------+-------------------]--------------
  //               M-                 w                   M+
  //
  //                                  ten_k
  //                                <------>
  //                                       <---- rest ---->
  // --------------[------------------+----+--------------]--------------
  //                                  w    V
  //                                       = buf * 10^k
  //
  // ten_k represents a unit-in-the-last-place in the decimal representation
  // stored in buf.
  // Decrement buf by ten_k while this takes buf closer to w.

  // The tests are written in this order to avoid overflow in unsigned
  // integer arithmetic.

  while (rest < dist && delta - rest >= ten_k &&
         (rest + ten_k < dist || dist - rest > rest + ten_k - dist)) {
    assert(buf[len - 1] != '0');
    buf[len - 1]--;
    rest += ten_k;
  }
}

/*!
Generates V = buffer * 10^decimal_exponent, such that M- <= V <= M+.
M- and M+ must be normalized and share the same exponent -60 <= e <= -32.
*/
inline void grisu2_digit_gen(char* buffer, int& length, int& decimal_exponent,
                             diyfp M_minus, diyfp w, diyfp M_plus) {
  static_assert(kAlpha >= -60, "internal error");
  static_assert(kGamma <= -32, "internal error");

  // Generates the digits (and the exponent) of a decimal floating-point
  // number V = buffer * 10^decimal_exponent in the range [M-, M+]. The diyfp's
  // w, M- and M+ share the same exponent e, which satisfies alpha <= e <=
  // gamma.
  //
  //               <--------------------------- delta ---->
  //                                  <---- dist --------->
  // --------------[------------------+-------------------]--------------
  //               M-                 w                   M+
  //
  // Grisu2 generates the digits of M+ from left to right and stops as soon as
  // V is in [M-,M+].

  assert(M_plus.e >= kAlpha);
  assert(M_plus.e <= kGamma);

  uint64_t delta = diyfp::sub(M_plus, M_minus)
                       .f; // (significand of (M+ - M-), implicit exponent is e)
  uint64_t dist = diyfp::sub(M_plus, w)
                      .f; // (significand of (M+ - w ), implicit exponent is e)

  // Split M+ = f * 2^e into two parts p1 and p2 (note: e < 0):
  //
  //      M+ = f * 2^e
  //         = ((f div 2^-e) * 2^-e + (f mod 2^-e)) * 2^e
  //         = ((p1        ) * 2^-e + (p2        )) * 2^e
  //         = p1 + p2 * 2^e

  const diyfp one(uint64_t{1} << -M_plus.e, M_plus.e);

  uint32_t p1 = static_cast<uint32_t>(
      M_plus.f >>
      -one.e); // p1 = f div 2^-e (Since -e >= 32, p1 fits into a 32-bit int.)
  uint64_t p2 = M_plus.f & (one.f - 1); // p2 = f mod 2^-e

  // 1)
  //
  // Generate the digits of the integral part p1 = d[n-1]...d[1]d[0]

  assert(p1 > 0);

  uint32_t pow10;
  const int k = find_largest_pow10(p1, pow10);

  //      10^(k-1) <= p1 < 10^k, pow10 = 10^(k-1)
  //
  //      p1 = (p1 div 10^(k-1)) * 10^(k-1) + (p1 mod 10^(k-1))
  //         = (d[k-1]         ) * 10^(k-1) + (p1 mod 10^(k-1))
  //
  //      M+ = p1                                             + p2 * 2^e
  //         = d[k-1] * 10^(k-1) + (p1 mod 10^(k-1))          + p2 * 2^e
  //         = d[k-1] * 10^(k-1) + ((p1 mod 10^(k-1)) * 2^-e + p2) * 2^e
  //         = d[k-1] * 10^(k-1) + (                         rest) * 2^e
  //
  // Now generate the digits d[n] of p1 from left to right (n = k-1,...,0)
  //
  //      p1 = d[k-1]...d[n] * 10^n + d[n-1]...d[0]
  //
  // but stop as soon as
  //
  //      rest * 2^e = (d[n-1]...d[0] * 2^-e + p2) * 2^e <= delta * 2^e

  int n = k;
  while (n > 0) {
    // Invariants:
    //      M+ = buffer * 10^n + (p1 + p2 * 2^e)    (buffer = 0 for n = k)
    //      pow10 = 10^(n-1) <= p1 < 10^n
    //
    const uint32_t d = p1 / pow10; // d = p1 div 10^(n-1)
    const uint32_t r = p1 % pow10; // r = p1 mod 10^(n-1)
    //
    //      M+ = buffer * 10^n + (d * 10^(n-1) + r) + p2 * 2^e
    //         = (buffer * 10 + d) * 10^(n-1) + (r + p2 * 2^e)
    //
    assert(d <= 9);
    buffer[length++] = static_cast<char>('0' + d); // buffer := buffer * 10 + d
    //
    //      M+ = buffer * 10^(n-1) + (r + p2 * 2^e)
    //
    p1 = r;
    n--;
    //
    //      M+ = buffer * 10^n + (p1 + p2 * 2^e)
    //      pow10 = 10^n
    //

    // Now check if enough digits have been generated.
    // Compute
    //
    //      p1 + p2 * 2^e = (p1 * 2^-e + p2) * 2^e = rest * 2^e
    //
    // Note:
    // Since rest and delta share the same exponent e, it suffices to
    // compare the significands.
    const uint64_t rest = (uint64_t{p1} << -one.e) + p2;
    if (rest <= delta) {
      // V = buffer * 10^n, with M- <= V <= M+.

      decimal_exponent += n;

      // We may now just stop. But instead look if the buffer could be
      // decremented to bring V closer to w.
      //
      // pow10 = 10^n is now 1 ulp in the decimal representation V.
      // The rounding procedure works with diyfp's with an implicit
      // exponent of e.
      //
      //      10^n = (10^n * 2^-e) * 2^e = ulp * 2^e
      //
      const uint64_t ten_n = uint64_t{pow10} << -one.e;
      grisu2_round(buffer, length, dist, delta, rest, ten_n);

      return;
    }

    pow10 /= 10;
    //
    //      pow10 = 10^(n-1) <= p1 < 10^n
    // Invariants restored.
  }

  // 2)
  //
  // The digits of the integral part have been generated:
  //
  //      M+ = d[k-1]...d[1]d[0] + p2 * 2^e
  //         = buffer            + p2 * 2^e
  //
  // Now generate the digits of the fractional part p2 * 2^e.
  //
  // Note:
  // No decimal point is generated: the exponent is adjusted instead.
  //
  // p2 actually represents the fraction
  //
  //      p2 * 2^e
  //          = p2 / 2^-e
  //          = d[-1] / 10^1 + d[-2] / 10^2 + ...
  //
  // Now generate the digits d[-m] of p1 from left to right (m = 1,2,...)
  //
  //      p2 * 2^e = d[-1]d[-2]...d[-m] * 10^-m
  //                      + 10^-m * (d[-m-1] / 10^1 + d[-m-2] / 10^2 + ...)
  //
  // using
  //
  //      10^m * p2 = ((10^m * p2) div 2^-e) * 2^-e + ((10^m * p2) mod 2^-e)
  //                = (                   d) * 2^-e + (                   r)
  //
  // or
  //      10^m * p2 * 2^e = d + r * 2^e
  //
  // i.e.
  //
  //      M+ = buffer + p2 * 2^e
  //         = buffer + 10^-m * (d + r * 2^e)
  //         = (buffer * 10^m + d) * 10^-m + 10^-m * r * 2^e
  //
  // and stop as soon as 10^-m * r * 2^e <= delta * 2^e

  assert(p2 > delta);

  int m = 0;
  for (;;) {
    // Invariant:
    //      M+ = buffer * 10^-m + 10^-m * (d[-m-1] / 10 + d[-m-2] / 10^2 + ...)
    //      * 2^e
    //         = buffer * 10^-m + 10^-m * (p2                                 )
    //         * 2^e = buffer * 10^-m + 10^-m * (1/10 * (10 * p2)
    //         ) * 2^e = buffer * 10^-m + 10^-m * (1/10 * ((10*p2 div 2^-e) *
    //         2^-e + (10*p2 mod 2^-e)) * 2^e
    //
    assert(p2 <= UINT64_MAX / 10);
    p2 *= 10;
    const uint64_t d = p2 >> -one.e;     // d = (10 * p2) div 2^-e
    const uint64_t r = p2 & (one.f - 1); // r = (10 * p2) mod 2^-e
    //
    //      M+ = buffer * 10^-m + 10^-m * (1/10 * (d * 2^-e + r) * 2^e
    //         = buffer * 10^-m + 10^-m * (1/10 * (d + r * 2^e))
    //         = (buffer * 10 + d) * 10^(-m-1) + 10^(-m-1) * r * 2^e
    //
    assert(d <= 9);
    buffer[length++] = static_cast<char>('0' + d); // buffer := buffer * 10 + d
    //
    //      M+ = buffer * 10^(-m-1) + 10^(-m-1) * r * 2^e
    //
    p2 = r;
    m++;
    //
    //      M+ = buffer * 10^-m + 10^-m * p2 * 2^e
    // Invariant restored.

    // Check if enough digits have been generated.
    //
    //      10^-m * p2 * 2^e <= delta * 2^e
    //              p2 * 2^e <= 10^m * delta * 2^e
    //                    p2 <= 10^m * delta
    delta *= 10;
    dist *= 10;
    if (p2 <= delta) {
      break;
    }
  }

  // V = buffer * 10^-m, with M- <= V <= M+.

  decimal_exponent -= m;

  // 1 ulp in the decimal representation is now 10^-m.
  // Since delta and dist are now scaled by 10^m, we need to do the
  // same with ulp in order to keep the units in sync.
  //
  //      10^m * 10^-m = 1 = 2^-e * 2^e = ten_m * 2^e
  //
  const uint64_t ten_m = one.f;
  grisu2_round(buffer, length, dist, delta, p2, ten_m);

  // By construction this algorithm generates the shortest possible decimal
  // number (Loitsch, Theorem 6.2) which rounds back to w.
  // For an input number of precision p, at least
  //
  //      N = 1 + ceil(p * log_10(2))
  //
  // decimal digits are sufficient to identify all binary floating-point
  // numbers (Matula, "In-and-Out conversions").
  // This implies that the algorithm does not produce more than N decimal
  // digits.
  //
  //      N = 17 for p = 53 (IEEE double precision)
  //      N = 9  for p = 24 (IEEE single precision)
}

/*!
v = buf * 10^decimal_exponent
len is the length of the buffer (number of decimal digits)
The buffer must be large enough, i.e. >= max_digits10.
*/
inline void grisu2(char* buf, int& len, int& decimal_exponent, diyfp m_minus,
                   diyfp v, diyfp m_plus) {
  assert(m_plus.e == m_minus.e);
  assert(m_plus.e == v.e);

  //  --------(-----------------------+-----------------------)--------    (A)
  //          m-                      v                       m+
  //
  //  --------------------(-----------+-----------------------)--------    (B)
  //                      m-          v                       m+
  //
  // First scale v (and m- and m+) such that the exponent is in the range
  // [alpha, gamma].

  const cached_power cached = get_cached_power_for_binary_exponent(m_plus.e);

  const diyfp c_minus_k(cached.f, cached.e); // = c ~= 10^-k

  // The exponent of the products is = v.e + c_minus_k.e + q and is in the range
  // [alpha,gamma]
  const diyfp w = diyfp::mul(v, c_minus_k);
  const diyfp w_minus = diyfp::mul(m_minus, c_minus_k);
  const diyfp w_plus = diyfp::mul(m_plus, c_minus_k);

  //  ----(---+---)---------------(---+---)---------------(---+---)----
  //          w-                      w                       w+
  //          = c*m-                  = c*v                   = c*m+
  //
  // diyfp::mul rounds its result and c_minus_k is approximated too. w, w- and
  // w+ are now off by a small amount.
  // In fact:
  //
  //      w - v * 10^k < 1 ulp
  //
  // To account for this inaccuracy, add resp. subtract 1 ulp.
  //
  //  --------+---[---------------(---+---)---------------]---+--------
  //          w-  M-                  w                   M+  w+
  //
  // Now any number in [M-, M+] (bounds included) will round to w when input,
  // regardless of how the input rounding algorithm breaks ties.
  //
  // And digit_gen generates the shortest possible such number in [M-, M+].
  // Note that this does not mean that Grisu2 always generates the shortest
  // possible number in the interval (m-, m+).
  const diyfp M_minus(w_minus.f + 1, w_minus.e);
  const diyfp M_plus(w_plus.f - 1, w_plus.e);

  decimal_exponent = -cached.k; // = -(-k) = k

  grisu2_digit_gen(buf, len, decimal_exponent, M_minus, w, M_plus);
}

/*!
v = buf * 10^decimal_exponent
len is the length of the buffer (number of decimal digits)
The buffer must be large enough, i.e. >= max_digits10.
*/
template <typename FloatType>
void grisu2(char* buf, int& len, int& decimal_exponent, FloatType value) {
  static_assert(diyfp::kPrecision >= std::numeric_limits<FloatType>::digits + 3,
                "internal error: !enough precision");

  assert(std::isfinite(value));
  assert(value > 0);

  // If the neighbors (and boundaries) of 'value' are always computed for
  // double-precision numbers, all float's can be recovered using strtod (and
  // strtof). However, the resulting decimal representations are not exactly
  // "short".
  //
  // The documentation for 'std::to_chars'
  // (http://en.cppreference.com/w/cpp/utility/to_chars) says "value is
  // converted to a string as if by std::sprintf in the default ("C") locale"
  // and since sprintf promotes float's to double's, I think this is exactly
  // what 'std::to_chars' does. On the other hand, the documentation for
  // 'std::to_chars' requires that "parsing the representation using the
  // corresponding std::from_chars function recovers value exactly". That
  // indicates that single precision floating-point numbers should be recovered
  // using 'std::strtof'.
  //
  // NB: If the neighbors are computed for single-precision numbers, there is a
  // single float
  //     (7.0385307e-26f) which can't be recovered using strtod. The resulting
  //     double precision value is off by 1 ulp.
#if 0
    const boundaries w = compute_boundaries(static_cast<double>(value));
#else
  const boundaries w = compute_boundaries(value);
#endif

  grisu2(buf, len, decimal_exponent, w.minus, w.w, w.plus);
}

/*!
@brief appends a decimal representation of e to buf
@return a pointer to the element following the exponent.
@pre -1000 < e < 1000
*/
inline char* append_exponent(char* buf, int e) {
  assert(e > -1000);
  assert(e < 1000);

  if (e < 0) {
    e = -e;
    *buf++ = '-';
  } else {
    *buf++ = '+';
  }

  uint32_t k = static_cast<uint32_t>(e);
  if (k < 10) {
    // Always print at least two digits in the exponent.
    // This is for compatibility with printf("%g").
    *buf++ = '0';
    *buf++ = static_cast<char>('0' + k);
  } else if (k < 100) {
    *buf++ = static_cast<char>('0' + k / 10);
    k %= 10;
    *buf++ = static_cast<char>('0' + k);
  } else {
    *buf++ = static_cast<char>('0' + k / 100);
    k %= 100;
    *buf++ = static_cast<char>('0' + k / 10);
    k %= 10;
    *buf++ = static_cast<char>('0' + k);
  }

  return buf;
}

/*!
@brief prettify v = buf * 10^decimal_exponent

If v is in the range [10^min_exp, 10^max_exp) it will be printed in fixed-point
notation. Otherwise it will be printed in exponential notation.

@pre min_exp < 0
@pre max_exp > 0
*/
inline char* format_buffer(char* buf, int len, int decimal_exponent,
                           int min_exp, int max_exp) {
  assert(min_exp < 0);
  assert(max_exp > 0);

  const int k = len;
  const int n = len + decimal_exponent;

  // v = buf * 10^(n-k)
  // k is the length of the buffer (number of decimal digits)
  // n is the position of the decimal point relative to the start of the buffer.

  if (k <= n && n <= max_exp) {
    // digits[000]
    // len <= max_exp + 2

    std::memset(buf + k, '0', static_cast<size_t>(n - k));
    // Make it look like a floating-point number (#362, #378)
    buf[n + 0] = '.';
    buf[n + 1] = '0';
    return buf + (n + 2);
  }

  if (0 < n && n <= max_exp) {
    // dig.its
    // len <= max_digits10 + 1

    assert(k > n);

    std::memmove(buf + (n + 1), buf + n, static_cast<size_t>(k - n));
    buf[n] = '.';
    return buf + (k + 1);
  }

  if (min_exp < n && n <= 0) {
    // 0.[000]digits
    // len <= 2 + (-min_exp - 1) + max_digits10

    std::memmove(buf + (2 + -n), buf, static_cast<size_t>(k));
    buf[0] = '0';
    buf[1] = '.';
    std::memset(buf + 2, '0', static_cast<size_t>(-n));
    return buf + (2 + (-n) + k);
  }

  if (k == 1) {
    // dE+123
    // len <= 1 + 5

    buf += 1;
  } else {
    // d.igitsE+123
    // len <= max_digits10 + 1 + 5

    std::memmove(buf + 2, buf + 1, static_cast<size_t>(k - 1));
    buf[1] = '.';
    buf += 1 + k;
  }

  *buf++ = 'e';
  return append_exponent(buf, n - 1);
}

} // namespace dtoa_impl

/*!
@brief generates a decimal representation of the floating-point number value in
[first, last).

The format of the resulting decimal representation is similar to printf's %g
format. Returns an iterator pointing past-the-end of the decimal representation.

@note The input number must be finite, i.e. NaN's and Inf's are not supported.
@note The buffer must be large enough.
@note The result is NOT null-terminated.
*/
template <typename FloatType>
char* to_chars(char* first, char* last, FloatType value) {
  static_cast<void>(last); // maybe unused - fix warning
  assert(std::isfinite(value));

  // Use signbit(value) instead of (value < 0) since signbit works for -0.
  if (std::signbit(value)) {
    value = -value;
    *first++ = '-';
  }

  if (value == 0) // +-0
  {
    *first++ = '0';
    // Make it look like a floating-point number (#362, #378)
    *first++ = '.';
    *first++ = '0';
    return first;
  }

  assert(last - first >= std::numeric_limits<FloatType>::max_digits10);

  // Compute v = buffer * 10^decimal_exponent.
  // The decimal digits are stored in the buffer, which needs to be interpreted
  // as an unsigned decimal integer.
  // len is the length of the buffer, i.e. the number of decimal digits.
  int len = 0;
  int decimal_exponent = 0;
  dtoa_impl::grisu2(first, len, decimal_exponent, value);

  assert(len <= std::numeric_limits<FloatType>::max_digits10);

  // Format the buffer like printf("%.*g", prec, value)
  constexpr int kMinExp = -4;
  // Use digits10 here to increase compatibility with version 2.
  constexpr int kMaxExp = std::numeric_limits<FloatType>::digits10;

  assert(last - first >= kMaxExp + 2);
  assert(last - first >=
         2 + (-kMinExp - 1) + std::numeric_limits<FloatType>::max_digits10);
  assert(last - first >= std::numeric_limits<FloatType>::max_digits10 + 6);

  return dtoa_impl::format_buffer(first, len, decimal_exponent, kMinExp,
                                  kMaxExp);
}

} // namespace detail
} // namespace my_json

namespace my_json {
namespace detail {
///////////////////
// serialization //
///////////////////

template <typename basic_json> class serializer {
  using string_t = typename basic_json::string_t;
  using number_float_t = typename basic_json::number_float_t;
  using number_integer_t = typename basic_json::number_integer_t;
  using number_unsigned_t = typename basic_json::number_unsigned_t;
  static constexpr uint8_t UTF8_ACCEPT = 0;
  static constexpr uint8_t UTF8_REJECT = 1;

  /*!
  @param[in] s  output stream to serialize to
  @param[in] ichar  indentation character to use
  */
  serializer(output_adapter_t<char> s, const char ichar)
      : o(std::move(s)), loc(std::localeconv()),
        thousands_sep(loc->thousands_sep == nullptr ? '\0' :
                                                      *(loc->thousands_sep)),
        decimal_point(loc->decimal_point == nullptr ? '\0' :
                                                      *(loc->decimal_point)),
        indent_char(ichar), indent_string(512, indent_char) {}

  // delete because of pointer members
  serializer(const serializer&) = delete;
  serializer& operator=(const serializer&) = delete;

  // Names borrowed from lisp/scheme
  // print & pprint are the same, except for their default parameters,
  // both are provided as a convenience.

  // Print pretty prints with no extra indentation
  void print(const basic_json& val, int indent = 0) {
    return my_dump(val, true, false, std::max(indent, 0));
  }
  // pprint/display pretty print with indentation (defaults to 2)
  void pprint(const basic_json& val, int indent = 2) {
    // ensure positive indentation
    return my_dump(val, true, true, std::max(indent, 0));
  }
  void display(const basic_json& val, int indent = 2) {
    return pprint(val, indent);
  }
  // write prints with no extra indentation
  void write(const basic_json& val) { return my_dump(val, false, false, 0); }
  void my_dump(const basic_json& val, const bool pretty_print_obj,
               const bool pretty_print_arr, const unsigned int indent_step,
               const unsigned int current_indent = 0) {
    switch (val.m_type) {
    case value_t::object: {
      if (val.m_value.object->empty()) {
        o->write_characters("{}", 2);
        return;
      }
      if (pretty_print_obj) {
        o->write_characters("{\n", 2);

        // variable to hold indentation for recursive calls
        const auto new_indent = current_indent + indent_step;
        if (JSON_UNLIKELY(indent_string.size() < new_indent)) {
          indent_string.resize(indent_string.size() * 2, ' ');
        }

        // first n-1 elements
        auto i = val.m_value.object->cbegin();
        for (std::size_t cnt = 0; cnt < val.m_value.object->size() - 1;
             ++cnt, ++i) {
          o->write_characters(indent_string.c_str(), new_indent);
          o->write_character('\"');
          dump_escaped(i->first, false);
          o->write_characters("\": ", 3);
          my_dump(i->second, pretty_print_obj, pretty_print_arr, indent_step,
                  new_indent);
          o->write_characters(",\n", 2);
        }

        // last element
        assert(i != val.m_value.object->cend());
        assert(std::next(i) == val.m_value.object->cend());
        o->write_characters(indent_string.c_str(), new_indent);
        o->write_character('\"');
        dump_escaped(i->first, false);
        o->write_characters("\": ", 3);
        my_dump(i->second, pretty_print_obj, pretty_print_arr, indent_step,
                new_indent);

        o->write_character('\n');
        o->write_characters(indent_string.c_str(), current_indent);
        o->write_character('}');
      } else {
        o->write_character('{');

        // first n-1 elements
        auto i = val.m_value.object->cbegin();
        for (std::size_t cnt = 0; cnt < val.m_value.object->size() - 1;
             ++cnt, ++i) {
          o->write_character('\"');
          dump_escaped(i->first, false);
          o->write_characters("\":", 2);
          my_dump(i->second, pretty_print_obj, pretty_print_arr, indent_step,
                  current_indent);
          o->write_character(',');
        }

        // last element
        assert(i != val.m_value.object->cend());
        assert(std::next(i) == val.m_value.object->cend());
        o->write_character('\"');
        dump_escaped(i->first, false);
        o->write_characters("\":", 2);
        my_dump(i->second, pretty_print_obj, pretty_print_arr, indent_step,
                current_indent);

        o->write_character('}');
      }

      return;
    }

    case value_t::array: {
      if (val.m_value.array->empty()) {
        o->write_characters("[]", 2);
        return;
      }

      if (pretty_print_arr) {
        o->write_characters("[\n", 2);

        // variable to hold indentation for recursive calls
        const auto new_indent = current_indent + indent_step;
        if (JSON_UNLIKELY(indent_string.size() < new_indent)) {
          indent_string.resize(indent_string.size() * 2, ' ');
        }

        // first n-1 elements
        for (auto i = val.m_value.array->cbegin();
             i != val.m_value.array->cend() - 1; ++i) {
          o->write_characters(indent_string.c_str(), new_indent);
          my_dump(*i, pretty_print_obj, pretty_print_arr, indent_step,
                  new_indent);
          o->write_characters(",\n", 2);
        }

        // last element
        assert(!val.m_value.array->empty());
        o->write_characters(indent_string.c_str(), new_indent);
        my_dump(val.m_value.array->back(), pretty_print_obj, pretty_print_arr,
                indent_step, new_indent);

        o->write_character('\n');
        o->write_characters(indent_string.c_str(), current_indent);
        o->write_character(']');
      } else {
        o->write_character('[');

        // first n-1 elements
        for (auto i = val.m_value.array->cbegin();
             i != val.m_value.array->cend() - 1; ++i) {
          my_dump(*i, pretty_print_obj, pretty_print_arr, indent_step,
                  current_indent);
          o->write_character(',');
        }

        // last element
        assert(!val.m_value.array->empty());
        my_dump(val.m_value.array->back(), pretty_print_obj, pretty_print_arr,
                indent_step, current_indent);

        o->write_character(']');
      }

      return;
    }

    case value_t::string: {
      o->write_character('\"');
      dump_escaped(*val.m_value.string, false);
      o->write_character('\"');
      return;
    }

    case value_t::boolean: {
      if (val.m_value.boolean) {
        o->write_characters("true", 4);
      } else {
        o->write_characters("false", 5);
      }
      return;
    }

    case value_t::number_integer: {
      dump_integer(val.m_value.number_integer);
      return;
    }

    case value_t::number_unsigned: {
      dump_integer(val.m_value.number_unsigned);
      return;
    }

    case value_t::number_float: {
      dump_float(val.m_value.number_float);
      return;
    }

    case value_t::discarded: {
      o->write_characters("<discarded>", 11);
      return;
    }

    case value_t::null: {
      o->write_characters("null", 4);
      return;
    }
    }
  }

  /*!
  @brief internal implementation of the serialization function

  This function is called by the public member function dump and organizes
  the serialization internally. The indentation level is propagated as
  additional parameter. In case of arrays and objects, the function is
  called recursively.

  - strings and object keys are escaped using `escape_string()`
  - integer numbers are converted implicitly via `operator<<`
  - floating-point numbers are converted to a string using `"%g"` format

  @param[in] val             value to serialize
  @param[in] pretty_print    whether the output shall be pretty-printed
  @param[in] indent_step     the indent level
  @param[in] current_indent  the current indent level (only used internally)
  */
  void dump(const basic_json& val, const bool pretty_print,
            const bool ensure_ascii, const unsigned int indent_step,
            const unsigned int current_indent = 0) {
    switch (val.m_type) {
    case value_t::object: {
      if (val.m_value.object->empty()) {
        o->write_characters("{}", 2);
        return;
      }

      if (pretty_print) {
        o->write_characters("{\n", 2);

        // variable to hold indentation for recursive calls
        const auto new_indent = current_indent + indent_step;
        if (JSON_UNLIKELY(indent_string.size() < new_indent)) {
          indent_string.resize(indent_string.size() * 2, ' ');
        }

        // first n-1 elements
        auto i = val.m_value.object->cbegin();
        for (std::size_t cnt = 0; cnt < val.m_value.object->size() - 1;
             ++cnt, ++i) {
          o->write_characters(indent_string.c_str(), new_indent);
          o->write_character('\"');
          dump_escaped(i->first, ensure_ascii);
          o->write_characters("\": ", 3);
          dump(i->second, true, ensure_ascii, indent_step, new_indent);
          o->write_characters(",\n", 2);
        }

        // last element
        assert(i != val.m_value.object->cend());
        assert(std::next(i) == val.m_value.object->cend());
        o->write_characters(indent_string.c_str(), new_indent);
        o->write_character('\"');
        dump_escaped(i->first, ensure_ascii);
        o->write_characters("\": ", 3);
        dump(i->second, true, ensure_ascii, indent_step, new_indent);

        o->write_character('\n');
        o->write_characters(indent_string.c_str(), current_indent);
        o->write_character('}');
      } else {
        o->write_character('{');

        // first n-1 elements
        auto i = val.m_value.object->cbegin();
        for (std::size_t cnt = 0; cnt < val.m_value.object->size() - 1;
             ++cnt, ++i) {
          o->write_character('\"');
          dump_escaped(i->first, ensure_ascii);
          o->write_characters("\":", 2);
          dump(i->second, false, ensure_ascii, indent_step, current_indent);
          o->write_character(',');
        }

        // last element
        assert(i != val.m_value.object->cend());
        assert(std::next(i) == val.m_value.object->cend());
        o->write_character('\"');
        dump_escaped(i->first, ensure_ascii);
        o->write_characters("\":", 2);
        dump(i->second, false, ensure_ascii, indent_step, current_indent);

        o->write_character('}');
      }

      return;
    }

    case value_t::array: {
      if (val.m_value.array->empty()) {
        o->write_characters("[]", 2);
        return;
      }

      if (pretty_print) {
        o->write_characters("[\n", 2);

        // variable to hold indentation for recursive calls
        const auto new_indent = current_indent + indent_step;
        if (JSON_UNLIKELY(indent_string.size() < new_indent)) {
          indent_string.resize(indent_string.size() * 2, ' ');
        }

        // first n-1 elements
        for (auto i = val.m_value.array->cbegin();
             i != val.m_value.array->cend() - 1; ++i) {
          o->write_characters(indent_string.c_str(), new_indent);
          dump(*i, true, ensure_ascii, indent_step, new_indent);
          o->write_characters(",\n", 2);
        }

        // last element
        assert(!val.m_value.array->empty());
        o->write_characters(indent_string.c_str(), new_indent);
        dump(val.m_value.array->back(), true, ensure_ascii, indent_step,
             new_indent);

        o->write_character('\n');
        o->write_characters(indent_string.c_str(), current_indent);
        o->write_character(']');
      } else {
        o->write_character('[');

        // first n-1 elements
        for (auto i = val.m_value.array->cbegin();
             i != val.m_value.array->cend() - 1; ++i) {
          dump(*i, false, ensure_ascii, indent_step, current_indent);
          o->write_character(',');
        }

        // last element
        assert(!val.m_value.array->empty());
        dump(val.m_value.array->back(), false, ensure_ascii, indent_step,
             current_indent);

        o->write_character(']');
      }

      return;
    }

    case value_t::string: {
      o->write_character('\"');
      dump_escaped(*val.m_value.string, ensure_ascii);
      o->write_character('\"');
      return;
    }

    case value_t::boolean: {
      if (val.m_value.boolean) {
        o->write_characters("true", 4);
      } else {
        o->write_characters("false", 5);
      }
      return;
    }

    case value_t::number_integer: {
      dump_integer(val.m_value.number_integer);
      return;
    }

    case value_t::number_unsigned: {
      dump_integer(val.m_value.number_unsigned);
      return;
    }

    case value_t::number_float: {
      dump_float(val.m_value.number_float);
      return;
    }

    case value_t::discarded: {
      o->write_characters("<discarded>", 11);
      return;
    }

    case value_t::null: {
      o->write_characters("null", 4);
      return;
    }
    }
  }

  /*!
  @brief dump escaped string

  Escape a string by replacing certain special characters by a sequence of an
  escape character (backslash) and another character and other control
  characters by a sequence of "\u" followed by a four-digit hex
  representation. The escaped string is written to output stream @a o.

  @param[in] s  the string to escape
  @param[in] ensure_ascii  whether to escape non-ASCII characters with
                           \uXXXX sequences

  @complexity Linear in the length of string @a s.
  */
  void dump_escaped(const string_t& s, const bool ensure_ascii) {
    uint32_t codepoint;
    uint8_t state = UTF8_ACCEPT;
    std::size_t bytes = 0; // number of bytes written to string_buffer

    for (std::size_t i = 0; i < s.size(); ++i) {
      const auto byte = static_cast<uint8_t>(s[i]);

      switch (decode(state, codepoint, byte)) {
      case UTF8_ACCEPT: // decode found a new code point
      {
        switch (codepoint) {
        case 0x08: // backspace
        {
          string_buffer[bytes++] = '\\';
          string_buffer[bytes++] = 'b';
          break;
        }

        case 0x09: // horizontal tab
        {
          string_buffer[bytes++] = '\\';
          string_buffer[bytes++] = 't';
          break;
        }

        case 0x0A: // newline
        {
          string_buffer[bytes++] = '\\';
          string_buffer[bytes++] = 'n';
          break;
        }

        case 0x0C: // formfeed
        {
          string_buffer[bytes++] = '\\';
          string_buffer[bytes++] = 'f';
          break;
        }

        case 0x0D: // carriage return
        {
          string_buffer[bytes++] = '\\';
          string_buffer[bytes++] = 'r';
          break;
        }

        case 0x22: // quotation mark
        {
          string_buffer[bytes++] = '\\';
          string_buffer[bytes++] = '\"';
          break;
        }

        case 0x5C: // reverse solidus
        {
          string_buffer[bytes++] = '\\';
          string_buffer[bytes++] = '\\';
          break;
        }

        default: {
          // escape control characters (0x00..0x1F) or, if
          // ensure_ascii parameter is used, non-ASCII characters
          if ((codepoint <= 0x1F) || (ensure_ascii && (codepoint >= 0x7F))) {
            if (codepoint <= 0xFFFF) {
              std::snprintf(string_buffer.data() + bytes, 7, "\\u%04x",
                            static_cast<uint16_t>(codepoint));
              bytes += 6;
            } else {
              std::snprintf(
                  string_buffer.data() + bytes, 13, "\\u%04x\\u%04x",
                  static_cast<uint16_t>(0xD7C0 + (codepoint >> 10)),
                  static_cast<uint16_t>(0xDC00 + (codepoint & 0x3FF)));
              bytes += 12;
            }
          } else {
            // copy byte to buffer (all previous bytes
            // been copied have in default case above)
            string_buffer[bytes++] = s[i];
          }
          break;
        }
        }

        // write buffer and reset index; there must be 13 bytes
        // left, as this is the maximal number of bytes to be
        // written ("\uxxxx\uxxxx\0") for one code point
        if (string_buffer.size() - bytes < 13) {
          o->write_characters(string_buffer.data(), bytes);
          bytes = 0;
        }
        break;
      }

      case UTF8_REJECT: // decode found invalid UTF-8 byte
      {
        char buf[8];
        snprintf(buf, 8, "%02X", byte);
        JSON_THROW(type_error::create(316, "invalid UTF-8 byte at index " +
                                               std::to_string(i) + ": 0x" +
                                               std::string(buf)));
      }

      default: // decode found yet incomplete multi-byte code point
      {
        if (!ensure_ascii) {
          // code point will not be escaped - copy byte to buffer
          string_buffer[bytes++] = s[i];
        }
        break;
      }
      }
    }

    if (JSON_LIKELY(state == UTF8_ACCEPT)) {
      // write buffer
      if (bytes > 0) {
        o->write_characters(string_buffer.data(), bytes);
      }
    } else {
      // we finish reading, but do not accept: string was incomplete
      char buf[8];
      snprintf(buf, 8, "%02X", static_cast<uint8_t>(s.back()));
      JSON_THROW(type_error::create(
          316, "incomplete UTF-8 string; last byte: 0x" + std::string(buf)));
    }
  }

  /*!
  @brief dump an integer

  Dump a given integer to output stream @a o. Works internally with
  @a number_buffer.

  @param[in] x  integer number (signed or unsigned) to dump
  @tparam NumberType either @a number_integer_t or @a number_unsigned_t
  */
  template <
      typename NumberType,
      detail::enable_if_t<std::is_same<NumberType, number_unsigned_t>::value ||
                              std::is_same<NumberType, number_integer_t>::value,
                          int> = 0>
  void dump_integer(NumberType x) {
    // special case for "0"
    if (x == 0) {
      o->write_character('0');
      return;
    }

    const bool is_negative = (x <= 0) && (x != 0); // see issue #755
    std::size_t i = 0;

    while (x != 0) {
      // spare 1 byte for '\0'
      assert(i < number_buffer.size() - 1);

      const auto digit = std::labs(static_cast<long>(x % 10));
      number_buffer[i++] = static_cast<char>('0' + digit);
      x /= 10;
    }

    if (is_negative) {
      // make sure there is capacity for the '-'
      assert(i < number_buffer.size() - 2);
      number_buffer[i++] = '-';
    }

    std::reverse(number_buffer.begin(), number_buffer.begin() + i);
    o->write_characters(number_buffer.data(), i);
  }

  /*!
  @brief dump a floating-point number

  Dump a given floating-point number to output stream @a o. Works internally
  with @a number_buffer.

  @param[in] x  floating-point number to dump
  */
  void dump_float(number_float_t x) {
    // NaN / inf
    if (!std::isfinite(x)) {
      o->write_characters("null", 4);
      return;
    }

    // If number_float_t is an IEEE-754 single or double precision number,
    // use the Grisu2 algorithm to produce short numbers which are
    // guaranteed to round-trip, using strtof and strtod, resp.
    //
    // NB: The test below works if <long double> == <double>.
    static constexpr bool is_ieee_single_or_double =
        (std::numeric_limits<number_float_t>::is_iec559 &&
         std::numeric_limits<number_float_t>::digits == 24 &&
         std::numeric_limits<number_float_t>::max_exponent == 128) ||
        (std::numeric_limits<number_float_t>::is_iec559 &&
         std::numeric_limits<number_float_t>::digits == 53 &&
         std::numeric_limits<number_float_t>::max_exponent == 1024);

    dump_float(x, std::integral_constant<bool, is_ieee_single_or_double>());
  }

  void dump_float(number_float_t x,
                  std::true_type /*is_ieee_single_or_double*/) {
    char* begin = number_buffer.data();
    char* end =
        ::my_json::detail::to_chars(begin, begin + number_buffer.size(), x);

    o->write_characters(begin, static_cast<size_t>(end - begin));
  }

  void dump_float(number_float_t x,
                  std::false_type /*is_ieee_single_or_double*/) {
    // get number of digits for a float -> text -> float round-trip
    static constexpr auto d = std::numeric_limits<number_float_t>::max_digits10;

    // the actual conversion
    std::ptrdiff_t len =
        snprintf(number_buffer.data(), number_buffer.size(), "%.*g", d, x);

    // negative value indicates an error
    assert(len > 0);
    // check if buffer was large enough
    assert(static_cast<std::size_t>(len) < number_buffer.size());

    // erase thousands separator
    if (thousands_sep != '\0') {
      const auto end = std::remove(number_buffer.begin(),
                                   number_buffer.begin() + len, thousands_sep);
      std::fill(end, number_buffer.end(), '\0');
      assert((end - number_buffer.begin()) <= len);
      len = (end - number_buffer.begin());
    }

    // convert decimal point to '.'
    if (decimal_point != '\0' && decimal_point != '.') {
      const auto dec_pos =
          std::find(number_buffer.begin(), number_buffer.end(), decimal_point);
      if (dec_pos != number_buffer.end()) {
        *dec_pos = '.';
      }
    }

    o->write_characters(number_buffer.data(), static_cast<std::size_t>(len));

    // determine if need to append ".0"
    const bool value_is_int_like =
        std::none_of(number_buffer.begin(), number_buffer.begin() + len + 1,
                     [](char c) { return (c == '.' || c == 'e'); });

    if (value_is_int_like) {
      o->write_characters(".0", 2);
    }
  }

  /*!
  @brief check whether a string is UTF-8 encoded

  The function checks each byte of a string whether it is UTF-8 encoded. The
  result of the check is stored in the @a state parameter. The function must
  be called initially with state 0 (accept). State 1 means the string must
  be rejected, because the current byte is not allowed. If the string is
  completely processed, but the state is non-zero, the string ended
  prematurely; that is, the last byte indicated more bytes should have
  followed.

  @param[in,out] state  the state of the decoding
  @param[in,out] codep  codepoint (valid only if resulting state is UTF8_ACCEPT)
  @param[in] byte       next byte to decode
  @return               new state

  @note The function has been edited: a std::array is used.

  @copyright Copyright (c) 2008-2009 Bjoern Hoehrmann <bjoern@hoehrmann.de>
  @sa http://bjoern.hoehrmann.de/utf-8/decoder/dfa/
  */
  static uint8_t decode(uint8_t& state, uint32_t& codep,
                        const uint8_t byte) noexcept {
    static const std::array<uint8_t, 400> utf8d = {{
        0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,   0,   0, // 00..1F
        0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,   0,   0, // 20..3F
        0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,   0,   0, // 40..5F
        0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,   0,   0, // 60..7F
        1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
        1,   1,   1,   1,   1,   9,   9,   9,   9,   9,   9,
        9,   9,   9,   9,   9,   9,   9,   9,   9,   9, // 80..9F
        7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
        7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
        7,   7,   7,   7,   7,   7,   7,   7,   7,   7, // A0..BF
        8,   8,   2,   2,   2,   2,   2,   2,   2,   2,   2,
        2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
        2,   2,   2,   2,   2,   2,   2,   2,   2,   2, // C0..DF
        0xA, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3,
        0x3, 0x3, 0x4, 0x3, 0x3, // E0..EF
        0xB, 0x6, 0x6, 0x6, 0x5, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8,
        0x8, 0x8, 0x8, 0x8, 0x8, // F0..FF
        0x0, 0x1, 0x2, 0x3, 0x5, 0x8, 0x7, 0x1, 0x1, 0x1, 0x4,
        0x6, 0x1, 0x1, 0x1, 0x1, // s0..s0
        1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
        1,   1,   1,   1,   1,   1,   0,   1,   1,   1,   1,
        1,   0,   1,   0,   1,   1,   1,   1,   1,   1, // s1..s2
        1,   2,   1,   1,   1,   1,   1,   2,   1,   2,   1,
        1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
        1,   2,   1,   1,   1,   1,   1,   1,   1,   1, // s3..s4
        1,   2,   1,   1,   1,   1,   1,   1,   1,   2,   1,
        1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
        1,   3,   1,   3,   1,   1,   1,   1,   1,   1, // s5..s6
        1,   3,   1,   1,   1,   1,   1,   3,   1,   3,   1,
        1,   1,   1,   1,   1,   1,   3,   1,   1,   1,   1,
        1,   1,   1,   1,   1,   1,   1,   1,   1,   1 // s7..s8
    }};

    const uint8_t type = utf8d[byte];

    codep = (state != UTF8_ACCEPT) ?
                (byte & 0x3fu) | (codep << 6) :
                static_cast<uint32_t>(0xff >> type) & (byte);

    state = utf8d[256u + state * 16u + type];
    return state;
  }

  /// the output of the serializer
  output_adapter_t<char> o = nullptr;

  /// a (hopefully) large enough character buffer
  std::array<char, 64> number_buffer{{}};

  /// the locale
  const std::lconv* loc = nullptr;
  /// the locale's thousand separator character
  const char thousands_sep = '\0';
  /// the locale's decimal point character
  const char decimal_point = '\0';

  /// string buffer
  std::array<char, 512> string_buffer{{}};

  /// the indentation character
  const char indent_char;
  /// the indentation string
  string_t indent_string;
};
} // namespace detail
} // namespace my_json

// <my_json/detail/json_ref.hpp>

namespace my_json {
namespace detail {
template <typename basic_json> class json_ref {
  using value_type = basic_json;

  json_ref(value_type&& value)
      : owned_value(std::move(value)), value_ref(&owned_value),
        is_rvalue(true) {}

  json_ref(const value_type& value)
      : value_ref(const_cast<value_type*>(&value)), is_rvalue(false) {}

  json_ref(std::initializer_list<json_ref> init)
      : owned_value(init), value_ref(&owned_value), is_rvalue(true) {}

  template <class... Args>
  json_ref(Args&&... args)
      : owned_value(std::forward<Args>(args)...), value_ref(&owned_value),
        is_rvalue(true) {}

  // class should be movable only
  json_ref(json_ref&&) = default;
  json_ref(const json_ref&) = delete;
  json_ref& operator=(const json_ref&) = delete;

  value_type moved_or_copied() const {
    if (is_rvalue) {
      return std::move(*value_ref);
    }
    return *value_ref;
  }

  value_type const& operator*() const {
    return *static_cast<value_type const*>(value_ref);
  }

  value_type const* operator->() const {
    return static_cast<value_type const*>(value_ref);
  }

  mutable value_type owned_value = nullptr;
  value_type* value_ref = nullptr;
  const bool is_rvalue;
};
} // namespace detail
} // namespace my_json

// <my_json/detail/json_pointer.hpp>

namespace my_json {
template <typename basic_json> class json_pointer {
  // allow basic_json to access private members
  MY_JSON_BASIC_JSON_TPL_DECLARATION
  friend class basic_json;

  /*!
  @brief create JSON pointer

  Create a JSON pointer according to the syntax described in
  [Section 3 of RFC6901](https://tools.ietf.org/html/rfc6901#section-3).

  @param[in] s  string representing the JSON pointer; if omitted, the empty
                string is assumed which references the whole JSON value

  @throw parse_error.107 if the given JSON pointer @a s is nonempty and does
                         not begin with a slash (`/`); see example below

  @throw parse_error.108 if a tilde (`~`) in the given JSON pointer @a s is
  not followed by `0` (representing `~`) or `1` (representing `/`); see
  example below

  @liveexample{The example shows the construction several valid JSON pointers
  as well as the exceptional behavior.,json_pointer}

  @since version 2.0.0
  */
  explicit json_pointer(const std::string& s = "")
      : reference_tokens(split(s)) {}

  /*!
  @brief return a string representation of the JSON pointer

  @invariant For each JSON pointer `ptr`, it holds:
  @code {.cpp}
  ptr == json_pointer(ptr.to_string());
  @endcode

  @return a string representation of the JSON pointer

  @liveexample{The example shows the result of `to_string`.,
  json_pointer__to_string}

  @since version 2.0.0
  */
  std::string to_string() const noexcept {
    return std::accumulate(reference_tokens.begin(), reference_tokens.end(),
                           std::string{},
                           [](const std::string& a, const std::string& b) {
                             return a + "/" + escape(b);
                           });
  }

  /// @copydoc to_string()
  operator std::string() const { return to_string(); }

  /*!
  @param[in] s  reference token to be converted into an array index

  @return integer representation of @a s

  @throw out_of_range.404 if string @a s could not be converted to an integer
  */
  static int array_index(const std::string& s) {
    std::size_t processed_chars = 0;
    const int res = std::stoi(s, &processed_chars);

    // check if the string was completely read
    if (JSON_UNLIKELY(processed_chars != s.size())) {
      JSON_THROW(detail::out_of_range::create(
          404, "unresolved reference token '" + s + "'"));
    }

    return res;
  }

  /*!
  @brief remove and return last reference pointer
  @throw out_of_range.405 if JSON pointer has no parent
  */
  std::string pop_back() {
    if (JSON_UNLIKELY(is_root())) {
      JSON_THROW(
          detail::out_of_range::create(405, "JSON pointer has no parent"));
    }

    auto last = reference_tokens.back();
    reference_tokens.pop_back();
    return last;
  }

  /// return whether pointer points to the root document
  bool is_root() const { return reference_tokens.empty(); }

  json_pointer top() const {
    if (JSON_UNLIKELY(is_root())) {
      JSON_THROW(
          detail::out_of_range::create(405, "JSON pointer has no parent"));
    }

    json_pointer result = *this;
    result.reference_tokens = {reference_tokens[0]};
    return result;
  }

  /*!
  @brief create and return a reference to the pointed to value

  @complexity Linear in the number of reference tokens.

  @throw parse_error.109 if array index is not a number
  @throw type_error.313 if value cannot be unflattened
  */
  basic_json& get_and_create(basic_json& j) const {
    using size_type = typename basic_json::size_type;
    auto result = &j;

    // in case no reference tokens exist, return a reference to the JSON value
    // j which will be overwritten by a primitive value
    for (const auto& reference_token : reference_tokens) {
      switch (result->m_type) {
      case detail::value_t::null: {
        if (reference_token == "0") {
          // start a new array if reference token is 0
          result = &result->operator[](0);
        } else {
          // start a new object otherwise
          result = &result->operator[](reference_token);
        }
        break;
      }

      case detail::value_t::object: {
        // create an entry in the object
        result = &result->operator[](reference_token);
        break;
      }

      case detail::value_t::array: {
        // create an entry in the array
        JSON_TRY {
          result = &result->operator[](
              static_cast<size_type>(array_index(reference_token)));
        }
        JSON_CATCH(std::invalid_argument&) {
          JSON_THROW(detail::parse_error::create(
              109, 0, "array index '" + reference_token + "' is !a number"));
        }
        break;
      }

      /*
      The following code is only reached if there exists a reference
      token _and_ the current value is primitive. In this case, we have
      an error situation, because primitive values may only occur as
      single value; that is, with an empty list of reference tokens.
      */
      default:
        JSON_THROW(
            detail::type_error::create(313, "invalid value to unflatten"));
      }
    }

    return *result;
  }

  /*!
  @brief return a reference to the pointed to value

  @note This version does not throw if a value is not present, but tries to
        create nested values instead. For instance, calling this function
        with pointer `"/this/that"` on a null value is equivalent to calling
        `operator[]("this").operator[]("that")` on that value, effectively
        changing the null value to an object.

  @param[in] ptr  a JSON value

  @return reference to the JSON value pointed to by the JSON pointer

  @complexity Linear in the length of the JSON pointer.

  @throw parse_error.106   if an array index begins with '0'
  @throw parse_error.109   if an array index was not a number
  @throw out_of_range.404  if the JSON pointer can not be resolved
  */
  basic_json& get_unchecked(basic_json* ptr) const {
    using size_type = typename basic_json::size_type;
    for (const auto& reference_token : reference_tokens) {
      // convert null values to arrays or objects before continuing
      if (ptr->m_type == detail::value_t::null) {
        // check if reference token is a number
        const bool nums =
            std::all_of(reference_token.begin(), reference_token.end(),
                        [](const char x) { return (x >= '0' && x <= '9'); });

        // change value to array for numbers or "-" or to object otherwise
        *ptr = (nums || reference_token == "-") ? detail::value_t::array :
                                                  detail::value_t::object;
      }

      switch (ptr->m_type) {
      case detail::value_t::object: {
        // use unchecked object access
        ptr = &ptr->operator[](reference_token);
        break;
      }

      case detail::value_t::array: {
        // error condition (cf. RFC 6901, Sect. 4)
        if (JSON_UNLIKELY(reference_token.size() > 1 &&
                          reference_token[0] == '0')) {
          JSON_THROW(detail::parse_error::create(
              106, 0,
              "array index '" + reference_token + "' must !begin with '0'"));
        }

        if (reference_token == "-") {
          // explicitly treat "-" as index beyond the end
          ptr = &ptr->operator[](ptr->m_value.array->size());
        } else {
          // convert array index to number; unchecked access
          JSON_TRY {
            ptr = &ptr->operator[](
                static_cast<size_type>(array_index(reference_token)));
          }
          JSON_CATCH(std::invalid_argument&) {
            JSON_THROW(detail::parse_error::create(
                109, 0, "array index '" + reference_token + "' is !a number"));
          }
        }
        break;
      }

      default:
        JSON_THROW(detail::out_of_range::create(
            404, "unresolved reference token '" + reference_token + "'"));
      }
    }

    return *ptr;
  }

  /*!
  @throw parse_error.106   if an array index begins with '0'
  @throw parse_error.109   if an array index was not a number
  @throw out_of_range.402  if the array index '-' is used
  @throw out_of_range.404  if the JSON pointer can not be resolved
  */
  basic_json& get_checked(basic_json* ptr) const {
    using size_type = typename basic_json::size_type;
    for (const auto& reference_token : reference_tokens) {
      switch (ptr->m_type) {
      case detail::value_t::object: {
        // note: at performs range check
        ptr = &ptr->at(reference_token);
        break;
      }

      case detail::value_t::array: {
        if (JSON_UNLIKELY(reference_token == "-")) {
          // "-" always fails the range check
          JSON_THROW(detail::out_of_range::create(
              402, "array index '-' (" +
                       std::to_string(ptr->m_value.array->size()) +
                       ") is out of range"));
        }

        // error condition (cf. RFC 6901, Sect. 4)
        if (JSON_UNLIKELY(reference_token.size() > 1 &&
                          reference_token[0] == '0')) {
          JSON_THROW(detail::parse_error::create(
              106, 0,
              "array index '" + reference_token + "' must !begin with '0'"));
        }

        // note: at performs range check
        JSON_TRY {
          ptr = &ptr->at(static_cast<size_type>(array_index(reference_token)));
        }
        JSON_CATCH(std::invalid_argument&) {
          JSON_THROW(detail::parse_error::create(
              109, 0, "array index '" + reference_token + "' is !a number"));
        }
        break;
      }

      default:
        JSON_THROW(detail::out_of_range::create(
            404, "unresolved reference token '" + reference_token + "'"));
      }
    }

    return *ptr;
  }

  /*!
  @brief return a const reference to the pointed to value

  @param[in] ptr  a JSON value

  @return const reference to the JSON value pointed to by the JSON
  pointer

  @throw parse_error.106   if an array index begins with '0'
  @throw parse_error.109   if an array index was not a number
  @throw out_of_range.402  if the array index '-' is used
  @throw out_of_range.404  if the JSON pointer can not be resolved
  */
  const basic_json& get_unchecked(const basic_json* ptr) const {
    using size_type = typename basic_json::size_type;
    for (const auto& reference_token : reference_tokens) {
      switch (ptr->m_type) {
      case detail::value_t::object: {
        // use unchecked object access
        ptr = &ptr->operator[](reference_token);
        break;
      }

      case detail::value_t::array: {
        if (JSON_UNLIKELY(reference_token == "-")) {
          // "-" cannot be used for const access
          JSON_THROW(detail::out_of_range::create(
              402, "array index '-' (" +
                       std::to_string(ptr->m_value.array->size()) +
                       ") is out of range"));
        }

        // error condition (cf. RFC 6901, Sect. 4)
        if (JSON_UNLIKELY(reference_token.size() > 1 &&
                          reference_token[0] == '0')) {
          JSON_THROW(detail::parse_error::create(
              106, 0,
              "array index '" + reference_token + "' must !begin with '0'"));
        }

        // use unchecked array access
        JSON_TRY {
          ptr = &ptr->operator[](
              static_cast<size_type>(array_index(reference_token)));
        }
        JSON_CATCH(std::invalid_argument&) {
          JSON_THROW(detail::parse_error::create(
              109, 0, "array index '" + reference_token + "' is !a number"));
        }
        break;
      }

      default:
        JSON_THROW(detail::out_of_range::create(
            404, "unresolved reference token '" + reference_token + "'"));
      }
    }

    return *ptr;
  }

  /*!
  @throw parse_error.106   if an array index begins with '0'
  @throw parse_error.109   if an array index was not a number
  @throw out_of_range.402  if the array index '-' is used
  @throw out_of_range.404  if the JSON pointer can not be resolved
  */
  const basic_json& get_checked(const basic_json* ptr) const {
    using size_type = typename basic_json::size_type;
    for (const auto& reference_token : reference_tokens) {
      switch (ptr->m_type) {
      case detail::value_t::object: {
        // note: at performs range check
        ptr = &ptr->at(reference_token);
        break;
      }

      case detail::value_t::array: {
        if (JSON_UNLIKELY(reference_token == "-")) {
          // "-" always fails the range check
          JSON_THROW(detail::out_of_range::create(
              402, "array index '-' (" +
                       std::to_string(ptr->m_value.array->size()) +
                       ") is out of range"));
        }

        // error condition (cf. RFC 6901, Sect. 4)
        if (JSON_UNLIKELY(reference_token.size() > 1 &&
                          reference_token[0] == '0')) {
          JSON_THROW(detail::parse_error::create(
              106, 0,
              "array index '" + reference_token + "' must !begin with '0'"));
        }

        // note: at performs range check
        JSON_TRY {
          ptr = &ptr->at(static_cast<size_type>(array_index(reference_token)));
        }
        JSON_CATCH(std::invalid_argument&) {
          JSON_THROW(detail::parse_error::create(
              109, 0, "array index '" + reference_token + "' is !a number"));
        }
        break;
      }

      default:
        JSON_THROW(detail::out_of_range::create(
            404, "unresolved reference token '" + reference_token + "'"));
      }
    }

    return *ptr;
  }

  /*!
  @brief split the string input to reference tokens

  @note This function is only called by the json_pointer constructor.
        All exceptions below are documented there.

  @throw parse_error.107  if the pointer is not empty or begins with '/'
  @throw parse_error.108  if character '~' is not followed by '0' or '1'
  */
  static std::vector<std::string> split(const std::string& reference_string) {
    std::vector<std::string> result;

    // special case: empty reference string -> no reference tokens
    if (reference_string.empty()) {
      return result;
    }

    // check if nonempty reference string begins with slash
    if (JSON_UNLIKELY(reference_string[0] != '/')) {
      JSON_THROW(detail::parse_error::create(
          107, 1,
          "JSON pointer must be empty || begin with '/' - was: '" +
              reference_string + "'"));
    }

    // extract the reference tokens:
    // - slash: position of the last read slash (or end of string)
    // - start: position after the previous slash
    for (
        // search for the first slash after the first character
        std::size_t slash = reference_string.find_first_of('/', 1),
                    // set the beginning of the first reference token
        start = 1;
        // we can stop if start == string::npos+1 = 0
        start != 0;
        // set the beginning of the next reference token
        // (will eventually be 0 if slash == std::string::npos)
        start = slash + 1,
                    // find next slash
        slash = reference_string.find_first_of('/', start)) {
      // use the text between the beginning of the reference token
      // (start) and the last slash (slash).
      auto reference_token = reference_string.substr(start, slash - start);

      // check reference tokens are properly escaped
      for (std::size_t pos = reference_token.find_first_of('~');
           pos != std::string::npos;
           pos = reference_token.find_first_of('~', pos + 1)) {
        assert(reference_token[pos] == '~');

        // ~ must be followed by 0 or 1
        if (JSON_UNLIKELY(pos == reference_token.size() - 1 ||
                          (reference_token[pos + 1] != '0' &&
                           reference_token[pos + 1] != '1'))) {
          JSON_THROW(detail::parse_error::create(
              108, 0, "escape character '~' must be followed with '0' || '1'"));
        }
      }

      // finally, store the reference token
      unescape(reference_token);
      result.push_back(reference_token);
    }

    return result;
  }

  /*!
  @brief replace all occurrences of a substring by another string

  @param[in,out] s  the string to manipulate; changed so that all
                 occurrences of @a f are replaced with @a t
  @param[in]     f  the substring to replace with @a t
  @param[in]     t  the string to replace @a f

  @pre The search string @a f must not be empty. **This precondition is
  enforced with an assertion.**

  @since version 2.0.0
  */
  static void replace_substring(std::string& s, const std::string& f,
                                const std::string& t) {
    assert(!f.empty());
    for (auto pos = s.find(f);            // find first occurrence of f
         pos != std::string::npos;        // make sure f was found
         s.replace(pos, f.size(), t),     // replace with t, &&
         pos = s.find(f, pos + t.size())) // find next occurrence of f
    {
    }
  }

  /// escape "~"" to "~0" and "/" to "~1"
  static std::string escape(std::string s) {
    replace_substring(s, "~", "~0");
    replace_substring(s, "/", "~1");
    return s;
  }

  /// unescape "~1" to tilde and "~0" to slash (order is important!)
  static void unescape(std::string& s) {
    replace_substring(s, "~1", "/");
    replace_substring(s, "~0", "~");
  }

  /*!
  @param[in] reference_string  the reference string to the current value
  @param[in] value             the value to consider
  @param[in,out] result        the result object to insert values to

  @note Empty objects or arrays are flattened to `null`.
  */
  static void flatten(const std::string& reference_string,
                      const basic_json& value, basic_json& result) {
    switch (value.m_type) {
    case detail::value_t::array: {
      if (value.m_value.array->empty()) {
        // flatten empty array as null
        result[reference_string] = nullptr;
      } else {
        // iterate array and use index as reference string
        for (std::size_t i = 0; i < value.m_value.array->size(); ++i) {
          flatten(reference_string + "/" + std::to_string(i),
                  value.m_value.array->operator[](i), result);
        }
      }
      break;
    }

    case detail::value_t::object: {
      if (value.m_value.object->empty()) {
        // flatten empty object as null
        result[reference_string] = nullptr;
      } else {
        // iterate object and use keys as reference string
        for (const auto& element : *value.m_value.object) {
          flatten(reference_string + "/" + escape(element.first),
                  element.second, result);
        }
      }
      break;
    }

    default: {
      // add primitive value with its reference string
      result[reference_string] = value;
      break;
    }
    }
  }

  /*!
  @param[in] value  flattened JSON

  @return unflattened JSON

  @throw parse_error.109 if array index is not a number
  @throw type_error.314  if value is not an object
  @throw type_error.315  if object values are not primitive
  @throw type_error.313  if value cannot be unflattened
  */
  static basic_json unflatten(const basic_json& value) {
    if (JSON_UNLIKELY(!value.is_object())) {
      JSON_THROW(
          detail::type_error::create(314, "only objects can be unflattened"));
    }

    basic_json result;

    // iterate the JSON object values
    for (const auto& element : *value.m_value.object) {
      if (JSON_UNLIKELY(!element.second.is_primitive())) {
        JSON_THROW(detail::type_error::create(
            315, "values in object must be primitive"));
      }

      // assign value to reference pointed to by JSON pointer; Note that if
      // the JSON pointer is "" (i.e., points to the whole value), function
      // get_and_create returns a reference to result itself. An assignment
      // will then create a primitive value.
      json_pointer(element.first).get_and_create(result) = element.second;
    }

    return result;
  }

  friend bool operator==(json_pointer const& lhs,
                         json_pointer const& rhs) noexcept {
    return (lhs.reference_tokens == rhs.reference_tokens);
  }

  friend bool operator!=(json_pointer const& lhs,
                         json_pointer const& rhs) noexcept {
    return !(lhs == rhs);
  }

  /// the reference tokens
  std::vector<std::string> reference_tokens;
};
} // namespace my_json

// <my_json/adl_serializer.hpp>

namespace my_json {
template <typename, typename> struct adl_serializer {
  /*!
  @brief convert a JSON value to any value type

  This function is usually called by the `get()` function of the
  @ref basic_json class (either explicit or via conversion operators).

  @param[in] j         JSON value to read from
  @param[in,out] val  value to write to
  */
  template <typename basic_json, typename ValueType>
  static void from_json(basic_json&& j, ValueType& val) noexcept(
      noexcept(::my_json::from_json(std::forward<basic_json>(j), val))) {
    ::my_json::from_json(std::forward<basic_json>(j), val);
  }

  /*!
  @brief convert any value type to a JSON value

  This function is usually called by the constructors of the @ref basic_json
  class.

  @param[in,out] j  JSON value to write to
  @param[in] val     value to read from
  */
  template <typename basic_json, typename ValueType>
  static void to_json(basic_json& j, ValueType&& val) noexcept(
      noexcept(::my_json::to_json(j, std::forward<ValueType>(val)))) {
    ::my_json::to_json(j, std::forward<ValueType>(val));
  }
};
} // namespace my_json

/*!
@brief namespace for Niels Lohmann
@see https://github.com/my_json
@since version 1.0.0
*/
namespace my_json {

// The actual json class.
struct basic_json {
  template <detail::value_t> friend struct detail::external_constructor;
  friend ::my_json::json_pointer<basic_json>;
  friend ::my_json::detail::parser<basic_json>;
  friend ::my_json::detail::serializer<basic_json>;
  template <typename basic_json> friend class ::my_json::detail::iter_impl;

  // convenience aliases for types residing in namespace detail;
  using lexer = ::my_json::detail::lexer<basic_json>;
  using parser = ::my_json::detail::parser<basic_json>;

  using primitive_iterator_t = ::my_json::detail::primitive_iterator_t;
  template <typename basic_json>
  using internal_iterator = ::my_json::detail::internal_iterator<basic_json>;
  template <typename basic_json>
  using iter_impl = ::my_json::detail::iter_impl<basic_json>;
  template <typename Iterator>
  using iteration_proxy = ::my_json::detail::iteration_proxy<Iterator>;
  template <typename Base>
  using json_reverse_iterator = ::my_json::detail::json_reverse_iterator<Base>;

  template <typename CharType>
  using output_adapter_t = ::my_json::detail::output_adapter_t<CharType>;
#if USE_BINARY_FORMATS
  using binary_reader = ::my_json::detail::binary_reader<basic_json>;
  template <typename CharType>
  using binary_writer = ::my_json::detail::binary_writer<basic_json, CharType>;
#endif

  using serializer = ::my_json::detail::serializer<basic_json>;

  using value_t = detail::value_t;
  /// @copydoc my_json::json_pointer
  using json_pointer = ::my_json::json_pointer<basic_json>;
  template <typename T, typename SFINAE>
  using json_serializer = JSONSerializer<T, SFINAE>;
  /// helper type for initializer lists of basic_json values
  using initializer_list_t =
      std::initializer_list<detail::json_ref<basic_json>>;

  ////////////////
  // exceptions //
  ////////////////

  /// @name exceptions
  /// Classes to implement user-defined exceptions.
  /// @{

  /// @copydoc detail::exception
  using exception = detail::exception;
  /// @copydoc detail::parse_error
  using parse_error = detail::parse_error;
  /// @copydoc detail::invalid_iterator
  using invalid_iterator = detail::invalid_iterator;
  /// @copydoc detail::type_error
  using type_error = detail::type_error;
  /// @copydoc detail::out_of_range
  using out_of_range = detail::out_of_range;
  /// @copydoc detail::other_error
  using other_error = detail::other_error;

  /// @}

  /////////////////////
  // container types //
  /////////////////////

  /// @name container types
  /// The canonic container types to use @ref basic_json like any other STL
  /// container.
  /// @{

  /// the type of elements in a basic_json container
  using value_type = basic_json;

  /// the type of an element reference
  using reference = value_type&;
  /// the type of an element const reference
  using const_reference = const value_type&;

  /// a type to represent differences between iterators
  using difference_type = std::ptrdiff_t;
  /// a type to represent container sizes
  using size_type = std::size_t;

  /// the allocator type
  using allocator_type = AllocatorType<basic_json>;

  /// the type of an element pointer
  using pointer = typename std::allocator_traits<allocator_type>::pointer;
  /// the type of an element const pointer
  using const_pointer =
      typename std::allocator_traits<allocator_type>::const_pointer;

  /// an iterator for a basic_json container
  using iterator = iter_impl<basic_json>;
  /// a const iterator for a basic_json container
  using const_iterator = iter_impl<const basic_json>;
  /// a reverse iterator for a basic_json container
  using reverse_iterator = json_reverse_iterator<typename basic_json::iterator>;
  /// a const reverse iterator for a basic_json container
  using const_reverse_iterator =
      json_reverse_iterator<typename basic_json::const_iterator>;

  /// @}

  /*!
  @brief returns the allocator associated with the container
  */
  static allocator_type get_allocator() { return allocator_type(); }

  /// the type of the current element
  value_t m_type = value_t::null;

  /// the value of the current element
  json_value m_value = {};

  using boolean_t = bool;

  //use intptr_t as the integer so that we get the largest possible integer
  //without causing the union to be larger than a pointer.
  using number_integer_t = intptr_t;

  using number_unsigned_t = uintptr_t;

  //Use double if its the same size as a pointer, otherwise use float.
  using number_float_t =
    std::conditional_t<sizeof(double) <= sizeof(void*), double, float>;
  //the underlying type of a json array.
  using array_t = std::vector<basic_json>;

  //The underlying type of a json string.
  using string_t = std::string;

  // Hack to add string_view type.
  using string_view_t = std::string_view;

  using object_comparator_t = std::less<>;

  //We use maps to represent objects for now since it allows looking
  //up values using compatible types, whereas an unordered map doesn't.
  //I plan on replacing this with my own hash table eventually.
  using object_t = std::map<string_t, basic_json, object_comparator_t>;

  //blobs are completely opaque.
  using blob_t = void;




  template <typename T, typename... Args> static T* create(Args&&... args) {
    AllocatorType<T> alloc;
    using AllocatorTraits = std::allocator_traits<AllocatorType<T>>;

    auto deleter = [&](T* object) {
      AllocatorTraits::deallocate(alloc, object, 1);
    };
    std::unique_ptr<T, decltype(deleter)> object(
        AllocatorTraits::allocate(alloc, 1), deleter);
    AllocatorTraits::construct(alloc, object.get(),
                               std::forward<Args>(args)...);
    assert(object != nullptr);
    return object.release();
  }

  ////////////////////////
  // JSON value storage //
  ////////////////////////

  //The actual storage for the JSON value.
  union json_value {
    object_t* object;
    array_t* array;
    string_t* string;
    blob_t* blob;
    boolean_t boolean;
    number_integer_t number_integer;
    number_unsigned_t number_unsigned;
    number_float_t number_float;

    /// default constructor (for null values)
    json_value() = default;
    /// constructor for booleans
    json_value(boolean_t v) noexcept : boolean(v) {}
    /// constructor for numbers (integer)
    json_value(number_integer_t v) noexcept : number_integer(v) {}
    /// constructor for numbers (unsigned)
    json_value(number_unsigned_t v) noexcept : number_unsigned(v) {}
    /// constructor for numbers (floating-point)
    json_value(number_float_t v) noexcept : number_float(v) {}
    /// constructor for empty values of a given type
    json_value(value_t t) {
      switch (t) {
      case value_t::object: {
        object = create<object_t>();
        break;
      }

      case value_t::array: {
        array = create<array_t>();
        break;
      }

      case value_t::string: {
        string = create<string_t>("");
        break;
      }

      case value_t::boolean: {
        boolean = boolean_t(false);
        break;
      }

      case value_t::number_integer: {
        number_integer = number_integer_t(0);
        break;
      }

      case value_t::number_unsigned: {
        number_unsigned = number_unsigned_t(0);
        break;
      }

      case value_t::number_float: {
        number_float = number_float_t(0.0);
        break;
      }

      case value_t::null: {
        object = nullptr; // silence warning, see #821
        break;
      }

      default: {
        object = nullptr; // silence warning, see #821
        if (JSON_UNLIKELY(t == value_t::null)) {
          JSON_THROW(
              other_error::create(500, "961c151d2e87f2686a955a9be24d316f1362bf2"
                                       "1 3.1.2")); // LCOV_EXCL_LINE
        }
        break;
      }
      }
    }

    /// constructor for strings
    json_value(const string_t& value) { string = create<string_t>(value); }

    /// constructor for rvalue strings
    json_value(string_t&& value) {
      string = create<string_t>(std::move(value));
    }

    /// constructor for objects
    json_value(const object_t& value) { object = create<object_t>(value); }

    /// constructor for rvalue objects
    json_value(object_t&& value) {
      object = create<object_t>(std::move(value));
    }

    /// constructor for arrays
    json_value(const array_t& value) { array = create<array_t>(value); }

    /// constructor for rvalue arrays
    json_value(array_t&& value) { array = create<array_t>(std::move(value)); }

    void destroy(value_t t) noexcept {
      switch (t) {
      case value_t::object: {
        AllocatorType<object_t> alloc;
        std::allocator_traits<decltype(alloc)>::destroy(alloc, object);
        std::allocator_traits<decltype(alloc)>::deallocate(alloc, object, 1);
        break;
      }

      case value_t::array: {
        AllocatorType<array_t> alloc;
        std::allocator_traits<decltype(alloc)>::destroy(alloc, array);
        std::allocator_traits<decltype(alloc)>::deallocate(alloc, array, 1);
        break;
      }

      case value_t::string: {
        AllocatorType<string_t> alloc;
        std::allocator_traits<decltype(alloc)>::destroy(alloc, string);
        std::allocator_traits<decltype(alloc)>::deallocate(alloc, string, 1);
        break;
      }

      default: { break; }
      }
    }
  };

  /*!
  @brief checks the class invariants

  This function asserts the class invariants. It needs to be called at the
  end of every constructor to make sure that created objects respect the
  invariant. Furthermore, it has to be called each time the type of a JSON
  value is changed, because the invariant expresses a relationship between
  @a m_type and @a m_value.
  */
  void assert_invariant() const noexcept {
    assert(m_type != value_t::object || m_value.object != nullptr);
    assert(m_type != value_t::array || m_value.array != nullptr);
    assert(m_type != value_t::string || m_value.string != nullptr);
  }

  //////////////////////////
  // JSON parser callback //
  //////////////////////////

  /*!
  @brief parser event types

  The parser callback distinguishes the following events:
  - `object_start`: the parser read `{` and started to process a JSON object
  - `key`: the parser read a key of a value in an object
  - `object_end`: the parser read `}` and finished processing a JSON object
  - `array_start`: the parser read `[` and started to process a JSON array
  - `array_end`: the parser read `]` and finished processing a JSON array
  - `value`: the parser finished reading a JSON value

  @image html callback_events.png "Example when certain parse events are
  triggered"

  @sa @ref parser_callback_t for more information and examples
  */
  using parse_event_t = typename parser::parse_event_t;

  /*!
  @brief per-element parser callback type

  With a parser callback function, the result of parsing a JSON text can be
  influenced. When passed to @ref parse, it is called on certain events
  (passed as @ref parse_event_t via parameter @a event) with a set recursion
  depth @a depth and context JSON value @a parsed. The return value of the
  callback function is a boolean indicating whether the element that emitted
  the callback shall be kept or not.

  We distinguish six scenarios (determined by the event type) in which the
  callback function can be called. The following table describes the values
  of the parameters @a depth, @a event, and @a parsed.

  parameter @a event | description | parameter @a depth | parameter @a parsed
  ------------------ | ----------- | ------------------ | -------------------
  parse_event_t::object_start | the parser read `{` and started to process a
  JSON object | depth of the parent of the JSON object | a JSON value with type
  discarded parse_event_t::key | the parser read a key of a value in an object |
  depth of the currently parsed JSON object | a JSON string containing the key
  parse_event_t::object_end | the parser read `}` and finished processing a JSON
  object | depth of the parent of the JSON object | the parsed JSON object
  parse_event_t::array_start | the parser read `[` and started to process a JSON
  array | depth of the parent of the JSON array | a JSON value with type
  discarded parse_event_t::array_end | the parser read `]` and finished
  processing a JSON array | depth of the parent of the JSON array | the parsed
  JSON array parse_event_t::value | the parser finished reading a JSON value |
  depth of the value | the parsed JSON value

  @image html callback_events.png "Example when certain parse events are
  triggered"

  Discarding a value (i.e., returning `false`) has different effects
  depending on the context in which function was called:

  - Discarded values in structured types are skipped. That is, the parser
    will behave as if the discarded value was never read.
  - In case a value outside a structured type is skipped, it is replaced
    with `null`. This case happens if the top-level element is skipped.

  @param[in] depth  the depth of the recursion during parsing

  @param[in] event  an event of type parse_event_t indicating the context in
  the callback function has been called

  @param[in,out] parsed  the current intermediate parse result; note that
  writing to this value has no effect for parse_event_t::key events

  @return Whether the JSON value which called the function during parsing
  should be kept (`true`) or not (`false`). In the latter case, it is either
  skipped completely or replaced by an empty discarded object.

  @sa @ref parse for examples

  @since version 1.0.0
  */
  using parser_callback_t = typename parser::parser_callback_t;

  //////////////////
  // constructors //
  //////////////////

  /// @name constructors and destructors
  /// Constructors of class @ref basic_json, copy/move constructor, copy
  /// assignment, static functions creating objects, and the destructor.
  /// @{

  /*!
  @brief create an empty value with a given type

  Create an empty JSON value with a given type. The value will be default
  initialized with an empty value which depends on the type:

  Value type  | initial value
  ----------- | -------------
  null        | `null`
  boolean     | `false`
  string      | `""`
  number      | `0`
  object      | `{}`
  array       | `[]`

  @param[in] v  the type of the value to create

  @complexity Constant.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes to any JSON value.

  @liveexample{The following code shows the constructor for different @ref
  value_t values,basic_json__value_t}

  @sa @ref clear() -- restores the postcondition of this constructor

  @since version 1.0.0
  */
  basic_json(const value_t v) : m_type(v), m_value(v) { assert_invariant(); }

  /*!
  @brief create a null object

  Create a `null` JSON value. It either takes a null pointer as parameter
  (explicitly creating `null`) or no parameter (implicitly creating `null`).
  The passed null pointer itself is not read -- it is only used to choose
  the right constructor.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this constructor never throws
  exceptions.

  @liveexample{The following code shows the constructor with and without a
  null pointer parameter.,basic_json__nullptr_t}

  @since version 1.0.0
  */
  basic_json(std::nullptr_t = nullptr) noexcept : basic_json(value_t::null) {
    assert_invariant();
  }

  /*!
  @brief create a JSON value

  This is a "catch all" constructor for all compatible JSON types; that is,
  types for which a `to_json()` method exists. The constructor forwards the
  parameter @a val to that method (to `json_serializer<U>::to_json` method
  with `U = uncvref_t<CompatibleType>`, to be exact).

  Template type @a CompatibleType includes, but is not limited to, the
  following types:
  - **arrays**: @ref array_t and all kinds of compatible containers such as
    `std::vector`, `std::deque`, `std::list`, `std::forward_list`,
    `std::array`, `std::valarray`, `std::set`, `std::unordered_set`,
    `std::multiset`, and `std::unordered_multiset` with a `value_type` from
    which a @ref basic_json value can be constructed.
  - **objects**: @ref object_t and all kinds of compatible associative
    containers such as `std::map`, `std::unordered_map`, `std::multimap`,
    and `std::unordered_multimap` with a `key_type` compatible to
    @ref string_t and a `value_type` from which a @ref basic_json value can
    be constructed.
  - **strings**: @ref string_t, string literals, and all compatible string
    containers can be used.
  - **numbers**: @ref number_integer_t, @ref number_unsigned_t,
    @ref number_float_t, and all convertible number types such as `int`,
    `size_t`, `int64_t`, `float` or `double` can be used.
  - **boolean**: @ref boolean_t / `bool` can be used.

  See the examples below.

  @tparam CompatibleType a type such that:
  - @a CompatibleType is not derived from `std::istream`,
  - @a CompatibleType is not @ref basic_json (to avoid hijacking copy/move
       constructors),
  - @a CompatibleType is not a different @ref basic_json type (i.e. with
  different template arguments)
  - @a CompatibleType is not a @ref basic_json nested type (e.g.,
       @ref json_pointer, @ref iterator, etc ...)
  - @ref @ref json_serializer<U> has a
       `to_json(basic_json_t&, CompatibleType&&)` method

  @tparam U = `uncvref_t<CompatibleType>`

  @param[in] val the value to be forwarded to the respective constructor

  @complexity Usually linear in the size of the passed @a val, also
              depending on the implementation of the called `to_json()`
              method.

  @exceptionsafety Depends on the called constructor. For types directly
  supported by the library (i.e., all types for which no `to_json()` function
  was provided), strong guarantee holds: if an exception is thrown, there are
  no changes to any JSON value.

  @liveexample{The following code shows the constructor with several
  compatible types.,basic_json__CompatibleType}

  @since version 2.1.0
  */
  template <typename CompatibleType,
            typename U = detail::uncvref_t<CompatibleType>,
            detail::enable_if_t<
                detail::is_compatible_type<basic_json_t, U>::value, int> = 0>
  basic_json(CompatibleType&& val) noexcept(noexcept(JSONSerializer<U>::to_json(
      std::declval<basic_json_t&>(), std::forward<CompatibleType>(val)))) {
    JSONSerializer<U>::to_json(*this, std::forward<CompatibleType>(val));
    assert_invariant();
  }

  /*!
  @brief create a JSON value from an existing one

  This is a constructor for existing @ref basic_json types.
  It does not hijack copy/move constructors, since the parameter has different
  template arguments than the current ones.

  The constructor tries to convert the internal @ref m_value of the parameter.

  @tparam basic_json a type such that:
  - @a basic_json is a @ref basic_json type.
  - @a basic_json has different template arguments than @ref basic_json_t.

  @param[in] val the @ref basic_json value to be converted.

  @complexity Usually linear in the size of the passed @a val, also
              depending on the implementation of the called `to_json()`
              method.

  @exceptionsafety Depends on the called constructor. For types directly
  supported by the library (i.e., all types for which no `to_json()` function
  was provided), strong guarantee holds: if an exception is thrown, there are
  no changes to any JSON value.

  @since version 3.1.2
  */
  template <
      typename basic_json,
      detail::enable_if_t<detail::is_basic_json<basic_json>::value &&
                              !std::is_same<basic_json, basic_json>::value,
                          int> = 0>
  basic_json(const basic_json& val) {
    using other_boolean_t = typename basic_json::boolean_t;
    using other_number_float_t = typename basic_json::number_float_t;
    using other_number_integer_t = typename basic_json::number_integer_t;
    using other_number_unsigned_t = typename basic_json::number_unsigned_t;
    using other_string_t = typename basic_json::string_t;
    using other_object_t = typename basic_json::object_t;
    using other_array_t = typename basic_json::array_t;

    switch (val.type()) {
    case value_t::boolean:
      JSONSerializer<other_boolean_t>::to_json(
          *this, val.template get<other_boolean_t>());
      break;
    case value_t::number_float:
      JSONSerializer<other_number_float_t>::to_json(
          *this, val.template get<other_number_float_t>());
      break;
    case value_t::number_integer:
      JSONSerializer<other_number_integer_t>::to_json(
          *this, val.template get<other_number_integer_t>());
      break;
    case value_t::number_unsigned:
      JSONSerializer<other_number_unsigned_t>::to_json(
          *this, val.template get<other_number_unsigned_t>());
      break;
    case value_t::string:
      JSONSerializer<other_string_t>::to_json(
          *this, val.template get_ref<const other_string_t&>());
      break;
    case value_t::object:
      JSONSerializer<other_object_t>::to_json(
          *this, val.template get_ref<const other_object_t&>());
      break;
    case value_t::array:
      JSONSerializer<other_array_t>::to_json(
          *this, val.template get_ref<const other_array_t&>());
      break;
    case value_t::null: *this = nullptr; break;
    case value_t::discarded: m_type = value_t::discarded; break;
    }
    assert_invariant();
  }

  /*!
  @brief create a container (array or object) from an initializer list

  Creates a JSON value of type array or object from the passed initializer
  list @a init. In case @a type_deduction is `true` (default), the type of
  the JSON value to be created is deducted from the initializer list @a init
  according to the following rules:

  1. If the list is empty, an empty JSON object value `{}` is created.
  2. If the list consists of pairs whose first element is a string, a JSON
     object value is created where the first elements of the pairs are
     treated as keys and the second elements are as values.
  3. In all other cases, an array is created.

  The rules aim to create the best fit between a C++ initializer list and
  JSON values. The rationale is as follows:

  1. The empty initializer list is written as `{}` which is exactly an empty
     JSON object.
  2. C++ has no way of describing mapped types other than to list a list of
     pairs. As JSON requires that keys must be of type string, rule 2 is the
     weakest constraint one can pose on initializer lists to interpret them
     as an object.
  3. In all other cases, the initializer list could not be interpreted as
     JSON object type, so interpreting it as JSON array type is safe.

  With the rules described above, the following JSON values cannot be
  expressed by an initializer list:

  - the empty array (`[]`): use @ref array(initializer_list_t)
    with an empty initializer list in this case
  - arrays whose elements satisfy rule 2: use @ref
    array(initializer_list_t) with the same initializer list
    in this case

  @note When used without parentheses around an empty initializer list, @ref
  basic_json() is called instead of this function, yielding the JSON null
  value.

  @param[in] init  initializer list with JSON values

  @param[in] type_deduction internal parameter; when set to `true`, the type
  of the JSON value is deducted from the initializer list @a init; when set
  to `false`, the type provided via @a manual_type is forced. This mode is
  used by the functions @ref array(initializer_list_t) and
  @ref object(initializer_list_t).

  @param[in] manual_type internal parameter; when @a type_deduction is set
  to `false`, the created JSON value will use the provided type (only @ref
  value_t::array and @ref value_t::object are valid); when @a type_deduction
  is set to `true`, this parameter has no effect

  @throw type_error.301 if @a type_deduction is `false`, @a manual_type is
  `value_t::object`, but @a init contains an element which is not a pair
  whose first element is a string. In this case, the constructor could not
  create an object. If @a type_deduction would have be `true`, an array
  would have been created. See @ref object(initializer_list_t)
  for an example.

  @complexity Linear in the size of the initializer list @a init.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes to any JSON value.

  @liveexample{The example below shows how JSON values are created from
  initializer lists.,basic_json__list_init_t}

  @sa @ref array(initializer_list_t) -- create a JSON array
  value from an initializer list
  @sa @ref object(initializer_list_t) -- create a JSON object
  value from an initializer list

  @since version 1.0.0
  */
  basic_json(initializer_list_t init, bool type_deduction = true,
             value_t manual_type = value_t::array) {
    // check if each element is an array with two elements whose first
    // element is a string
    bool is_an_object = std::all_of(
        init.begin(), init.end(),
        [](const detail::json_ref<basic_json>& element_ref) {
          return (element_ref->is_array() && element_ref->size() == 2 &&
                  (*element_ref)[0].is_string());
        });

    // adjust type if type deduction is not wanted
    if (!type_deduction) {
      // if array is wanted, do not create an object though possible
      if (manual_type == value_t::array) {
        is_an_object = false;
      }

      // if object is wanted but impossible, throw an exception
      if (JSON_UNLIKELY(manual_type == value_t::object && !is_an_object)) {
        JSON_THROW(
            type_error::create(301, "can!create object from initializer list"));
      }
    }

    if (is_an_object) {
      // the initializer list is a list of pairs -> create object
      m_type = value_t::object;
      m_value = value_t::object;

      std::for_each(
          init.begin(), init.end(),
          [this](const detail::json_ref<basic_json>& element_ref) {
            auto element = element_ref.moved_or_copied();
            m_value.object->emplace(
                std::move(*((*element.m_value.array)[0].m_value.string)),
                std::move((*element.m_value.array)[1]));
          });
    } else {
      // the initializer list describes an array -> create array
      m_type = value_t::array;
      m_value.array = create<array_t>(init.begin(), init.end());
    }

    assert_invariant();
  }

  /*!
  @brief explicitly create an array from an initializer list

  Creates a JSON array value from a given initializer list. That is, given a
  list of values `a, b, c`, creates the JSON value `[a, b, c]`. If the
  initializer list is empty, the empty array `[]` is created.

  @note This function is only needed to express two edge cases that cannot
  be realized with the initializer list constructor (@ref
  basic_json(initializer_list_t, bool, value_t)). These cases
  are:
  1. creating an array whose elements are all pairs whose first element is a
  string -- in this case, the initializer list constructor would create an
  object, taking the first elements as keys
  2. creating an empty array -- passing the empty initializer list to the
  initializer list constructor yields an empty object

  @param[in] init  initializer list with JSON values to create an array from
  (optional)

  @return JSON array value

  @complexity Linear in the size of @a init.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes to any JSON value.

  @liveexample{The following code shows an example for the `array`
  function.,array}

  @sa @ref basic_json(initializer_list_t, bool, value_t) --
  create a JSON value from an initializer list
  @sa @ref object(initializer_list_t) -- create a JSON object
  value from an initializer list

  @since version 1.0.0
  */
  static basic_json array(initializer_list_t init = {}) {
    return basic_json(init, false, value_t::array);
  }

  /*!
  @brief explicitly create an object from an initializer list

  Creates a JSON object value from a given initializer list. The initializer
  lists elements must be pairs, and their first elements must be strings. If
  the initializer list is empty, the empty object `{}` is created.

  @note This function is only added for symmetry reasons. In contrast to the
  related function @ref array(initializer_list_t), there are
  no cases which can only be expressed by this function. That is, any
  initializer list @a init can also be passed to the initializer list
  constructor @ref basic_json(initializer_list_t, bool, value_t).

  @param[in] init  initializer list to create an object from (optional)

  @return JSON object value

  @throw type_error.301 if @a init is not a list of pairs whose first
  elements are strings. In this case, no object can be created. When such a
  value is passed to @ref basic_json(initializer_list_t, bool, value_t),
  an array would have been created from the passed initializer list @a init.
  See example below.

  @complexity Linear in the size of @a init.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes to any JSON value.

  @liveexample{The following code shows an example for the `object`
  function.,object}

  @sa @ref basic_json(initializer_list_t, bool, value_t) --
  create a JSON value from an initializer list
  @sa @ref array(initializer_list_t) -- create a JSON array
  value from an initializer list

  @since version 1.0.0
  */
  static basic_json object(initializer_list_t init = {}) {
    return basic_json(init, false, value_t::object);
  }

  /*!
  @brief construct an array with count copies of given value

  Constructs a JSON array value by creating @a cnt copies of a passed value.
  In case @a cnt is `0`, an empty array is created.

  @param[in] cnt  the number of JSON copies of @a val to create
  @param[in] val  the JSON value to copy

  @post `std::distance(begin(),end()) == cnt` holds.

  @complexity Linear in @a cnt.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes to any JSON value.

  @liveexample{The following code shows examples for the @ref
  basic_json(size_type\, const basic_json&)
  constructor.,basic_json__size_type_basic_json}

  @since version 1.0.0
  */
  basic_json(size_type cnt, const basic_json& val) : m_type(value_t::array) {
    m_value.array = create<array_t>(cnt, val);
    assert_invariant();
  }

  /*!
  @brief construct a JSON container given an iterator range

  Constructs the JSON value with the contents of the range `[first, last)`.
  The semantics depends on the different types a JSON value can have:
  - In case of a null type, invalid_iterator.206 is thrown.
  - In case of other primitive types (number, boolean, or string), @a first
    must be `begin()` and @a last must be `end()`. In this case, the value is
    copied. Otherwise, invalid_iterator.204 is thrown.
  - In case of structured types (array, object), the constructor behaves as
    similar versions for `std::vector` or `std::map`; that is, a JSON array
    or object is constructed from the values in the range.

  @tparam InputIT an input iterator type (@ref iterator or @ref
  const_iterator)

  @param[in] first begin of the range to copy from (included)
  @param[in] last end of the range to copy from (excluded)

  @pre Iterators @a first and @a last must be initialized. **This
       precondition is enforced with an assertion (see warning).** If
       assertions are switched off, a violation of this precondition yields
       undefined behavior.

  @pre Range `[first, last)` is valid. Usually, this precondition cannot be
       checked efficiently. Only certain edge cases are detected; see the
       description of the exceptions below. A violation of this precondition
       yields undefined behavior.

  @warning A precondition is enforced with a runtime assertion that will
           result in calling `std::abort` if this precondition is not met.
           Assertions can be disabled by defining `NDEBUG` at compile time.
           See http://en.cppreference.com/w/cpp/error/assert for more
           information.

  @throw invalid_iterator.201 if iterators @a first and @a last are not
  compatible (i.e., do not belong to the same JSON value). In this case,
  the range `[first, last)` is undefined.
  @throw invalid_iterator.204 if iterators @a first and @a last belong to a
  primitive type (number, boolean, or string), but @a first does not point
  to the first element any more. In this case, the range `[first, last)` is
  undefined. See example code below.
  @throw invalid_iterator.206 if iterators @a first and @a last belong to a
  null value. In this case, the range `[first, last)` is undefined.

  @complexity Linear in distance between @a first and @a last.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes to any JSON value.

  @liveexample{The example below shows several ways to create JSON values by
  specifying a subrange with iterators.,basic_json__InputIt_InputIt}

  @since version 1.0.0
  */
  template <class InputIT,
            typename std::enable_if<
                std::is_same<InputIT, typename basic_json_t::iterator>::value ||
                    std::is_same<InputIT,
                                 typename basic_json_t::const_iterator>::value,
                int>::type = 0>
  basic_json(InputIT first, InputIT last) {
    assert(first.m_object != nullptr);
    assert(last.m_object != nullptr);

    // make sure iterator fits the current value
    if (JSON_UNLIKELY(first.m_object != last.m_object)) {
      JSON_THROW(invalid_iterator::create(201, "iterators are !compatible"));
    }

    // copy type from first iterator
    m_type = first.m_object->m_type;

    // check if iterator range is complete for primitive values
    switch (m_type) {
    case value_t::boolean:
    case value_t::number_float:
    case value_t::number_integer:
    case value_t::number_unsigned:
    case value_t::string: {
      if (JSON_UNLIKELY(!first.m_it.primitive_iterator.is_begin() ||
                        !last.m_it.primitive_iterator.is_end())) {
        JSON_THROW(invalid_iterator::create(204, "iterators out of range"));
      }
      break;
    }

    default: break;
    }

    switch (m_type) {
    case value_t::number_integer: {
      m_value.number_integer = first.m_object->m_value.number_integer;
      break;
    }

    case value_t::number_unsigned: {
      m_value.number_unsigned = first.m_object->m_value.number_unsigned;
      break;
    }

    case value_t::number_float: {
      m_value.number_float = first.m_object->m_value.number_float;
      break;
    }

    case value_t::boolean: {
      m_value.boolean = first.m_object->m_value.boolean;
      break;
    }

    case value_t::string: {
      m_value = *first.m_object->m_value.string;
      break;
    }

    case value_t::object: {
      m_value.object = create<object_t>(first.m_it.object_iterator,
                                        last.m_it.object_iterator);
      break;
    }

    case value_t::array: {
      m_value.array =
          create<array_t>(first.m_it.array_iterator, last.m_it.array_iterator);
      break;
    }

    default:
      JSON_THROW(invalid_iterator::create(
          206, "can!construct with iterators from " +
                   std::string(first.m_object->type_name())));
    }

    assert_invariant();
  }

  ///////////////////////////////////////
  // other constructors and destructor //
  ///////////////////////////////////////

  /// @private
  basic_json(const detail::json_ref<basic_json>& ref)
      : basic_json(ref.moved_or_copied()) {}

  /*!
  @brief copy constructor

  Creates a copy of a given JSON value.

  @param[in] other  the JSON value to copy

  @post `*this == other`

  @complexity Linear in the size of @a other.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes to any JSON value.

  @requirement This function helps `basic_json` satisfying the
  [Container](http://en.cppreference.com/w/cpp/concept/Container)
  requirements:
  - The complexity is linear.
  - As postcondition, it holds: `other == basic_json(other)`.

  @liveexample{The following code shows an example for the copy
  constructor.,basic_json__basic_json}

  @since version 1.0.0
  */
  basic_json(const basic_json& other) : m_type(other.m_type) {
    // check of passed value is valid
    other.assert_invariant();

    switch (m_type) {
    case value_t::object: {
      m_value = *other.m_value.object;
      break;
    }

    case value_t::array: {
      m_value = *other.m_value.array;
      break;
    }

    case value_t::string: {
      m_value = *other.m_value.string;
      break;
    }

    case value_t::boolean: {
      m_value = other.m_value.boolean;
      break;
    }

    case value_t::number_integer: {
      m_value = other.m_value.number_integer;
      break;
    }

    case value_t::number_unsigned: {
      m_value = other.m_value.number_unsigned;
      break;
    }

    case value_t::number_float: {
      m_value = other.m_value.number_float;
      break;
    }

    default: break;
    }

    assert_invariant();
  }

  /*!
  @brief move constructor

  Move constructor. Constructs a JSON value with the contents of the given
  value @a other using move semantics. It "steals" the resources from @a
  other and leaves it as JSON null value.

  @param[in,out] other  value to move to this object

  @post `*this` has the same value as @a other before the call.
  @post @a other is a JSON null value.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this constructor never throws
  exceptions.

  @requirement This function helps `basic_json` satisfying the
  [MoveConstructible](http://en.cppreference.com/w/cpp/concept/MoveConstructible)
  requirements.

  @liveexample{The code below shows the move constructor explicitly called
  via std::move.,basic_json__moveconstructor}

  @since version 1.0.0
  */
  basic_json(basic_json&& other) noexcept
      : m_type(std::move(other.m_type)), m_value(std::move(other.m_value)) {
    // check that passed value is valid
    other.assert_invariant();

    // invalidate payload
    other.m_type = value_t::null;
    other.m_value = {};

    assert_invariant();
  }

  /*!
  @brief copy assignment

  Copy assignment operator. Copies a JSON value via the "copy and swap"
  strategy: It is expressed in terms of the copy constructor, destructor,
  and the `swap()` member function.

  @param[in] other  value to copy from

  @complexity Linear.

  @requirement This function helps `basic_json` satisfying the
  [Container](http://en.cppreference.com/w/cpp/concept/Container)
  requirements:
  - The complexity is linear.

  @liveexample{The code below shows and example for the copy assignment. It
  creates a copy of value `a` which is then swapped with `b`. Finally\, the
  copy of `a` (which is the null value after the swap) is
  destroyed.,basic_json__copyassignment}

  @since version 1.0.0
  */
  reference& operator=(basic_json other) noexcept(
      std::is_nothrow_move_constructible<value_t>::value &&
          std::is_nothrow_move_assignable<value_t>::value &&
              std::is_nothrow_move_constructible<json_value>::value &&
                  std::is_nothrow_move_assignable<json_value>::value) {
    // check that passed value is valid
    other.assert_invariant();

    using std::swap;
    swap(m_type, other.m_type);
    swap(m_value, other.m_value);

    assert_invariant();
    return *this;
  }

  /*!
  @brief destructor

  Destroys the JSON value and frees all allocated memory.

  @complexity Linear.

  @requirement This function helps `basic_json` satisfying the
  [Container](http://en.cppreference.com/w/cpp/concept/Container)
  requirements:
  - The complexity is linear.
  - All stored elements are destroyed and all memory is freed.

  @since version 1.0.0
  */
  ~basic_json() noexcept {
    assert_invariant();
    m_value.destroy(m_type);
  }

  /// @}

  ///////////////////////
  // object inspection //
  ///////////////////////

  /// @name object inspection
  /// Functions to inspect the type of a JSON value.
  /// @{

  /*!
  @brief serialization

  Serialization function for JSON values. The function tries to mimic
  Python's `json.dumps()` function, and currently supports its @a indent
  and @a ensure_ascii parameters.

  @param[in] indent If indent is nonnegative, then array elements and object
  members will be pretty-printed with that indent level. An indent level of
  `0` will only insert newlines. `-1` (the default) selects the most compact
  representation.
  @param[in] indent_char The character to use for indentation if @a indent is
  greater than `0`. The default is ` ` (space).
  @param[in] ensure_ascii If @a ensure_ascii is true, all non-ASCII characters
  in the output are escaped with `\uXXXX` sequences, and the result consists
  of ASCII characters only.

  @return string containing the serialization of the JSON value

  @throw type_error.316 if a string stored inside the JSON value is not
                        UTF-8 encoded

  @complexity Linear.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes in the JSON value.

  @liveexample{The following example shows the effect of different @a indent\,
  @a indent_char\, and @a ensure_ascii parameters to the result of the
  serialization.,dump}

  @see https://docs.python.org/2/library/json.html#json.dump

  @since version 1.0.0; indentation character @a indent_char, option
         @a ensure_ascii and exceptions added in version 3.0.0
  */
  string_t dump(const int indent = -1, const char indent_char = ' ',
                const bool ensure_ascii = false) const {
    string_t result;
    serializer s(detail::output_adapter<char, string_t>(result), indent_char);

    if (indent >= 0) {
      s.dump(*this, true, ensure_ascii, static_cast<unsigned int>(indent));
    } else {
      s.dump(*this, false, ensure_ascii, 0);
    }

    return result;
  }
  void print(detail::output_adapter<char, string_t> output_it,
             const char indent_char = ' ', const int indent = 0) const {
    serializer s(detail::output_adapter<char, string_t>(output_it),
                 indent_char);
    s.print(*this, indent);
  }
  void pprint(detail::output_adapter<char, string_t> output_it,
              const char indent_char = ' ', const int indent = 2) const {
    serializer s(detail::output_adapter<char, string_t>(output_it),
                 indent_char);
    s.pprint(*this, indent);
  }
  void write(detail::output_adapter<char, string_t> output_it) const {
    serializer s(detail::output_adapter<char, string_t>(output_it), ' ');
    s.write(*this);
  }

  /*!
  @brief return the type of the JSON value (explicit)

  Return the type of the JSON value as a value from the @ref value_t
  enumeration.

  @return the type of the JSON value
          Value type                | return value
          ------------------------- | -------------------------
          null                      | value_t::null
          boolean                   | value_t::boolean
          string                    | value_t::string
          number (integer)          | value_t::number_integer
          number (unsigned integer) | value_t::number_unsigned
          number (floating-point)   | value_t::number_float
          object                    | value_t::object
          array                     | value_t::array
          discarded                 | value_t::discarded

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies `type()` for all JSON
  types.,type}

  @sa @ref operator value_t() -- return the type of the JSON value (implicit)
  @sa @ref type_name() -- return the type as string

  @since version 1.0.0
  */
  constexpr value_t type() const noexcept { return m_type; }

  /*!
  @brief return whether type is primitive

  This function returns true if and only if the JSON type is primitive
  (string, number, boolean, or null).

  @return `true` if type is primitive (string, number, boolean, or null),
  `false` otherwise.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies `is_primitive()` for all JSON
  types.,is_primitive}

  @sa @ref is_structured() -- returns whether JSON value is structured
  @sa @ref is_null() -- returns whether JSON value is `null`
  @sa @ref is_string() -- returns whether JSON value is a string
  @sa @ref is_boolean() -- returns whether JSON value is a boolean
  @sa @ref is_number() -- returns whether JSON value is a number

  @since version 1.0.0
  */
  constexpr bool is_primitive() const noexcept {
    return is_null() || is_string() || is_boolean() || is_number();
  }

  /*!
  @brief return whether type is structured

  This function returns true if and only if the JSON type is structured
  (array or object).

  @return `true` if type is structured (array or object), `false` otherwise.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies `is_structured()` for all JSON
  types.,is_structured}

  @sa @ref is_primitive() -- returns whether value is primitive
  @sa @ref is_array() -- returns whether value is an array
  @sa @ref is_object() -- returns whether value is an object

  @since version 1.0.0
  */
  constexpr bool is_structured() const noexcept {
    return is_array() || is_object();
  }

  /*!
  @brief return whether value is null

  This function returns true if and only if the JSON value is null.

  @return `true` if type is null, `false` otherwise.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies `is_null()` for all JSON
  types.,is_null}

  @since version 1.0.0
  */
  constexpr bool is_null() const noexcept { return (m_type == value_t::null); }

  /*!
  @brief return whether value is a boolean

  This function returns true if and only if the JSON value is a boolean.

  @return `true` if type is boolean, `false` otherwise.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies `is_boolean()` for all JSON
  types.,is_boolean}

  @since version 1.0.0
  */
  constexpr bool is_boolean() const noexcept {
    return (m_type == value_t::boolean);
  }

  /*!
  @brief return whether value is a number

  This function returns true if and only if the JSON value is a number. This
  includes both integer (signed and unsigned) and floating-point values.

  @return `true` if type is number (regardless whether integer, unsigned
  integer or floating-type), `false` otherwise.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies `is_number()` for all JSON
  types.,is_number}

  @sa @ref is_number_integer() -- check if value is an integer or unsigned
  integer number
  @sa @ref is_number_unsigned() -- check if value is an unsigned integer
  number
  @sa @ref is_number_float() -- check if value is a floating-point number

  @since version 1.0.0
  */
  constexpr bool is_number() const noexcept {
    return is_number_integer() || is_number_float();
  }

  /*!
  @brief return whether value is an integer number

  This function returns true if and only if the JSON value is a signed or
  unsigned integer number. This excludes floating-point values.

  @return `true` if type is an integer or unsigned integer number, `false`
  otherwise.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies `is_number_integer()` for all
  JSON types.,is_number_integer}

  @sa @ref is_number() -- check if value is a number
  @sa @ref is_number_unsigned() -- check if value is an unsigned integer
  number
  @sa @ref is_number_float() -- check if value is a floating-point number

  @since version 1.0.0
  */
  constexpr bool is_number_integer() const noexcept {
    return (m_type == value_t::number_integer ||
            m_type == value_t::number_unsigned);
  }

  /*!
  @brief return whether value is an unsigned integer number

  This function returns true if and only if the JSON value is an unsigned
  integer number. This excludes floating-point and signed integer values.

  @return `true` if type is an unsigned integer number, `false` otherwise.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies `is_number_unsigned()` for all
  JSON types.,is_number_unsigned}

  @sa @ref is_number() -- check if value is a number
  @sa @ref is_number_integer() -- check if value is an integer or unsigned
  integer number
  @sa @ref is_number_float() -- check if value is a floating-point number

  @since version 2.0.0
  */
  constexpr bool is_number_unsigned() const noexcept {
    return (m_type == value_t::number_unsigned);
  }

  /*!
  @brief return whether value is a floating-point number

  This function returns true if and only if the JSON value is a
  floating-point number. This excludes signed and unsigned integer values.

  @return `true` if type is a floating-point number, `false` otherwise.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies `is_number_float()` for all
  JSON types.,is_number_float}

  @sa @ref is_number() -- check if value is number
  @sa @ref is_number_integer() -- check if value is an integer number
  @sa @ref is_number_unsigned() -- check if value is an unsigned integer
  number

  @since version 1.0.0
  */
  constexpr bool is_number_float() const noexcept {
    return (m_type == value_t::number_float);
  }

  /*!
  @brief return whether value is an object

  This function returns true if and only if the JSON value is an object.

  @return `true` if type is object, `false` otherwise.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies `is_object()` for all JSON
  types.,is_object}

  @since version 1.0.0
  */
  constexpr bool is_object() const noexcept {
    return (m_type == value_t::object);
  }

  /*!
  @brief return whether value is an array

  This function returns true if and only if the JSON value is an array.

  @return `true` if type is array, `false` otherwise.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies `is_array()` for all JSON
  types.,is_array}

  @since version 1.0.0
  */
  constexpr bool is_array() const noexcept {
    return (m_type == value_t::array);
  }

  /*!
  @brief return whether value is a string

  This function returns true if and only if the JSON value is a string.

  @return `true` if type is string, `false` otherwise.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies `is_string()` for all JSON
  types.,is_string}

  @since version 1.0.0
  */
  constexpr bool is_string() const noexcept {
    return (m_type == value_t::string);
  }

  /*!
  @brief return whether value is discarded

  This function returns true if and only if the JSON value was discarded
  during parsing with a callback function (see @ref parser_callback_t).

  @note This function will always be `false` for JSON values after parsing.
  That is, discarded values can only occur during parsing, but will be
  removed when inside a structured value or replaced by null in other cases.

  @return `true` if type is discarded, `false` otherwise.

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies `is_discarded()` for all JSON
  types.,is_discarded}

  @since version 1.0.0
  */
  constexpr bool is_discarded() const noexcept {
    return (m_type == value_t::discarded);
  }

  /*!
  @brief return the type of the JSON value (implicit)

  Implicitly return the type of the JSON value as a value from the @ref
  value_t enumeration.

  @return the type of the JSON value

  @complexity Constant.

  @exceptionsafety No-throw guarantee: this member function never throws
  exceptions.

  @liveexample{The following code exemplifies the @ref value_t operator for
  all JSON types.,operator__value_t}

  @sa @ref type() -- return the type of the JSON value (explicit)
  @sa @ref type_name() -- return the type as string

  @since version 1.0.0
  */
  constexpr operator value_t() const noexcept { return m_type; }

  /// @}

  //////////////////
  // value access //
  //////////////////

  /// get a boolean (explicit)
  boolean_t get_impl(boolean_t* /*unused*/) const {
    if (JSON_LIKELY(is_boolean())) {
      return m_value.boolean;
    }

    JSON_THROW(type_error::create(302, "type must be boolean, but is " +
                                           std::string(type_name())));
  }

  /// get a pointer to the value (object)
  object_t* get_impl_ptr(object_t* /*unused*/) noexcept {
    return is_object() ? m_value.object : nullptr;
  }

  /// get a pointer to the value (object)
  constexpr const object_t* get_impl_ptr(const object_t* /*unused*/) const
      noexcept {
    return is_object() ? m_value.object : nullptr;
  }

  /// get a pointer to the value (array)
  array_t* get_impl_ptr(array_t* /*unused*/) noexcept {
    return is_array() ? m_value.array : nullptr;
  }

  /// get a pointer to the value (array)
  constexpr const array_t* get_impl_ptr(const array_t* /*unused*/) const
      noexcept {
    return is_array() ? m_value.array : nullptr;
  }

  /// get a pointer to the value (string)
  string_t* get_impl_ptr(string_t* /*unused*/) noexcept {
    return is_string() ? m_value.string : nullptr;
  }

  /// get a pointer to the value (string)
  constexpr const string_t* get_impl_ptr(const string_t* /*unused*/) const
      noexcept {
    return is_string() ? m_value.string : nullptr;
  }
  /// get a pointer to the value (boolean)
  boolean_t* get_impl_ptr(boolean_t* /*unused*/) noexcept {
    return is_boolean() ? &m_value.boolean : nullptr;
  }

  /// get a pointer to the value (boolean)
  constexpr const boolean_t* get_impl_ptr(const boolean_t* /*unused*/) const
      noexcept {
    return is_boolean() ? &m_value.boolean : nullptr;
  }

  /// get a pointer to the value (integer number)
  number_integer_t* get_impl_ptr(number_integer_t* /*unused*/) noexcept {
    return is_number_integer() ? &m_value.number_integer : nullptr;
  }

  /// get a pointer to the value (integer number)
  constexpr const number_integer_t*
  get_impl_ptr(const number_integer_t* /*unused*/) const noexcept {
    return is_number_integer() ? &m_value.number_integer : nullptr;
  }

  /// get a pointer to the value (unsigned number)
  number_unsigned_t* get_impl_ptr(number_unsigned_t* /*unused*/) noexcept {
    return is_number_unsigned() ? &m_value.number_unsigned : nullptr;
  }

  /// get a pointer to the value (unsigned number)
  constexpr const number_unsigned_t*
  get_impl_ptr(const number_unsigned_t* /*unused*/) const noexcept {
    return is_number_unsigned() ? &m_value.number_unsigned : nullptr;
  }

  /// get a pointer to the value (floating-point number)
  number_float_t* get_impl_ptr(number_float_t* /*unused*/) noexcept {
    return is_number_float() ? &m_value.number_float : nullptr;
  }

  /// get a pointer to the value (floating-point number)
  constexpr const number_float_t*
  get_impl_ptr(const number_float_t* /*unused*/) const noexcept {
    return is_number_float() ? &m_value.number_float : nullptr;
  }

  /*!
  @brief helper function to implement get_ref()

  This function helps to implement get_ref() without code duplication for
  const and non-const overloads

  @tparam ThisType will be deduced as `basic_json` or `const basic_json`

  @throw type_error.303 if ReferenceType does not match underlying value
  type of the current JSON
  */
  template <typename ReferenceType, typename ThisType>
  static ReferenceType get_ref_impl(ThisType& obj) {
    // delegate the call to get_ptr<>()
    auto ptr =
        obj.template get_ptr<typename std::add_pointer<ReferenceType>::type>();

    if (JSON_LIKELY(ptr != nullptr)) {
      return *ptr;
    }

    JSON_THROW(type_error::create(
        303, "incompatible ReferenceType for get_ref, actual type is " +
                 std::string(obj.type_name())));
  }

  /// @name value access
  /// Direct access to the stored value of a JSON value.
  /// @{

  /*!
  @brief get special-case overload

  This overloads avoids a lot of template boilerplate, it can be seen as the
  identity method

  @tparam basic_json == @ref basic_json

  @return a copy of *this

  @complexity Constant.

  @since version 2.1.0
  */
  template <typename basic_json,
            detail::enable_if_t<
                std::is_same<typename std::remove_const<basic_json>::type,
                             basic_json_t>::value,
                int> = 0>
  basic_json get() const {
    return *this;
  }

  /*!
  @brief get special-case overload

  This overloads converts the current @ref basic_json in a different
  @ref basic_json type

  @tparam basic_json == @ref basic_json

  @return a copy of *this, converted into @tparam basic_json

  @complexity Depending on the implementation of the called `from_json()`
              method.

  @since version 3.1.2
  */
  template <
      typename basic_json,
      detail::enable_if_t<!std::is_same<basic_json, basic_json>::value &&
                              detail::is_basic_json<basic_json>::value,
                          int> = 0>
  basic_json get() const {
    return *this;
  }

  /*!
  @brief get a value (explicit)

  Explicit type conversion between the JSON value and a compatible value
  which is
  [CopyConstructible](http://en.cppreference.com/w/cpp/concept/CopyConstructible)
  and
  [DefaultConstructible](http://en.cppreference.com/w/cpp/concept/DefaultConstructible).
  The value is converted by calling the @ref json_serializer<ValueType>
  `from_json()` method.

  The function is equivalent to executing
  @code {.cpp}
  ValueType ret;
  JSONSerializer<ValueType>::from_json(*this, ret);
  return ret;
  @endcode

  This overloads is chosen if:
  - @a ValueType is not @ref basic_json,
  - @ref json_serializer<ValueType> has a `from_json()` method of the form
    `void from_json(const basic_json&, ValueType&)`, and
  - @ref json_serializer<ValueType> does not have a `from_json()` method of
    the form `ValueType from_json(const basic_json&)`

  @tparam ValueTypeCV the provided value type
  @tparam ValueType the returned value type

  @return copy of the JSON value, converted to @a ValueType

  @throw what @ref json_serializer<ValueType> `from_json()` method throws

  @liveexample{The example below shows several conversions from JSON values
  to other types. There a few things to note: (1) Floating-point numbers can
  be converted to integers\, (2) A JSON array can be converted to a standard
  `std::vector<short>`\, (3) A JSON object can be converted to C++
  associative containers such as `std::unordered_map<std::string\,
  json>`.,get__ValueType_const}

  @since version 2.1.0
  */
  template <typename ValueTypeCV,
            typename ValueType = detail::uncvref_t<ValueTypeCV>,
            detail::enable_if_t<
                !detail::is_basic_json<ValueType>::value &&
                    detail::has_from_json<basic_json_t, ValueType>::value &&
                    !detail::has_non_default_from_json<basic_json_t,
                                                       ValueType>::value,
                int> = 0>
  ValueType get() const noexcept(noexcept(JSONSerializer<ValueType>::from_json(
      std::declval<const basic_json_t&>(), std::declval<ValueType&>()))) {
    // we cannot static_assert on ValueTypeCV being non-const, because
    // there is support for get<const basic_json_t>(), which is why we
    // still need the uncvref
    static_assert(!std::is_reference<ValueTypeCV>::value,
                  "get() can!be used with reference types, you might want to "
                  "use get_ref()");
    static_assert(std::is_default_constructible<ValueType>::value,
                  "types must be DefaultConstructible when used with get()");

    ValueType ret;
    JSONSerializer<ValueType>::from_json(*this, ret);
    return ret;
  }

  /*!
  @brief get a value (explicit); special case

  Explicit type conversion between the JSON value and a compatible value
  which is **not**
  [CopyConstructible](http://en.cppreference.com/w/cpp/concept/CopyConstructible)
  and **not**
  [DefaultConstructible](http://en.cppreference.com/w/cpp/concept/DefaultConstructible).
  The value is converted by calling the @ref json_serializer<ValueType>
  `from_json()` method.

  The function is equivalent to executing
  @code {.cpp}
  return JSONSerializer<ValueTypeCV>::from_json(*this);
  @endcode

  This overloads is chosen if:
  - @a ValueType is not @ref basic_json and
  - @ref json_serializer<ValueType> has a `from_json()` method of the form
    `ValueType from_json(const basic_json&)`

  @note If @ref json_serializer<ValueType> has both overloads of
  `from_json()`, this one is chosen.

  @tparam ValueTypeCV the provided value type
  @tparam ValueType the returned value type

  @return copy of the JSON value, converted to @a ValueType

  @throw what @ref json_serializer<ValueType> `from_json()` method throws

  @since version 2.1.0
  */
  template <
      typename ValueTypeCV, typename ValueType = detail::uncvref_t<ValueTypeCV>,
      detail::enable_if_t<
          !std::is_same<basic_json_t, ValueType>::value &&
              detail::has_non_default_from_json<basic_json_t, ValueType>::value,
          int> = 0>
  ValueType get() const
      noexcept(noexcept(JSONSerializer<ValueTypeCV>::from_json(
          std::declval<const basic_json_t&>()))) {
    static_assert(!std::is_reference<ValueTypeCV>::value,
                  "get() can!be used with reference types, you might want to "
                  "use get_ref()");
    return JSONSerializer<ValueTypeCV>::from_json(*this);
  }

  /*!
  @brief get a pointer value (explicit)

  Explicit pointer access to the internally stored JSON value. No copies are
  made.

  @warning The pointer becomes invalid if the underlying JSON object
  changes.

  @tparam PointerType pointer type; must be a pointer to @ref array_t, @ref
  object_t, @ref string_t, @ref boolean_t, @ref number_integer_t,
  @ref number_unsigned_t, or @ref number_float_t.

  @return pointer to the internally stored JSON value if the requested
  pointer type @a PointerType fits to the JSON value; `nullptr` otherwise

  @complexity Constant.

  @liveexample{The example below shows how pointers to internal values of a
  JSON value can be requested. Note that no type conversions are made and a
  `nullptr` is returned if the value and the requested pointer type does not
  match.,get__PointerType}

  @sa @ref get_ptr() for explicit pointer-member access

  @since version 1.0.0
  */
  template <typename PointerType,
            typename std::enable_if<std::is_pointer<PointerType>::value,
                                    int>::type = 0>
  PointerType get() noexcept {
    // delegate the call to get_ptr
    return get_ptr<PointerType>();
  }

  /*!
  @brief get a pointer value (explicit)
  @copydoc get()
  */
  template <typename PointerType,
            typename std::enable_if<std::is_pointer<PointerType>::value,
                                    int>::type = 0>
  constexpr const PointerType get() const noexcept {
    // delegate the call to get_ptr
    return get_ptr<PointerType>();
  }

  /*!
  @brief get a pointer value (implicit)

  Implicit pointer access to the internally stored JSON value. No copies are
  made.

  @warning Writing data to the pointee of the result yields an undefined
  state.

  @tparam PointerType pointer type; must be a pointer to @ref array_t, @ref
  object_t, @ref string_t, @ref boolean_t, @ref number_integer_t,
  @ref number_unsigned_t, or @ref number_float_t. Enforced by a static
  assertion.

  @return pointer to the internally stored JSON value if the requested
  pointer type @a PointerType fits to the JSON value; `nullptr` otherwise

  @complexity Constant.

  @liveexample{The example below shows how pointers to internal values of a
  JSON value can be requested. Note that no type conversions are made and a
  `nullptr` is returned if the value and the requested pointer type does not
  match.,get_ptr}

  @since version 1.0.0
  */
  template <typename T,
            typename PointerType = typename std::conditional<
                std::is_pointer<T>::value, T,
                typename std::add_pointer<T>::type>::type,
            typename std::enable_if<std::is_pointer<PointerType>::value,
                                    int>::type = 0>
  PointerType get_ptr() noexcept {
    // get the type of the PointerType (remove pointer and const)
    using pointee_t = typename std::remove_const<typename std::remove_pointer<
        typename std::remove_const<PointerType>::type>::type>::type;
    // make sure the type matches the allowed types
    static_assert(std::is_same<object_t, pointee_t>::value ||
                      std::is_same<array_t, pointee_t>::value ||
                      std::is_same<string_t, pointee_t>::value ||
                      std::is_same<boolean_t, pointee_t>::value ||
                      std::is_same<number_integer_t, pointee_t>::value ||
                      std::is_same<number_unsigned_t, pointee_t>::value ||
                      std::is_same<number_float_t, pointee_t>::value,
                  "incompatible pointer type");

    // delegate the call to get_impl_ptr<>()
    return get_impl_ptr(static_cast<PointerType>(nullptr));
  }

  /*!
  @brief get a pointer value (implicit)
  @copydoc get_ptr()
  */
  template <
      typename T,
      typename PointerType =
          std::conditional_t<std::is_pointer<T>::value, T,
                             std::add_pointer_t<std::add_const_t<T>>>,
      typename std::enable_if<std::is_pointer<PointerType>::value &&
                                  std::is_const<typename std::remove_pointer<
                                      PointerType>::type>::value,
                              int>::type = 0>
  constexpr const PointerType get_ptr() const noexcept {
    // get the type of the PointerType (remove pointer and const)
    using pointee_t = typename std::remove_const<typename std::remove_pointer<
        typename std::remove_const<PointerType>::type>::type>::type;
    // make sure the type matches the allowed types
    static_assert(std::is_same<object_t, pointee_t>::value ||
                      std::is_same<array_t, pointee_t>::value ||
                      std::is_same<string_t, pointee_t>::value ||
                      std::is_same<boolean_t, pointee_t>::value ||
                      std::is_same<number_integer_t, pointee_t>::value ||
                      std::is_same<number_unsigned_t, pointee_t>::value ||
                      std::is_same<number_float_t, pointee_t>::value,
                  "incompatible pointer type");

    // delegate the call to get_impl_ptr<>() const
    return get_impl_ptr(static_cast<PointerType>(nullptr));
  }

  // Modified get ref to not require the template type to be a reference itself
  /*!
  @brief get a reference value (implicit)

  Implicit reference access to the internally stored JSON value. No copies
  are made.

  @warning Writing data to the referee of the result yields an undefined
  state.

  @tparam ReferenceType reference type; must be a reference to @ref array_t,
  @ref object_t, @ref string_t, @ref boolean_t, @ref number_integer_t, or
  @ref number_float_t. Enforced by static assertion.

  @return reference to the internally stored JSON value if the requested
  reference type @a ReferenceType fits to the JSON value; throws
  type_error.303 otherwise

  @throw type_error.303 in case passed type @a ReferenceType is incompatible
  with the stored JSON value; see example below

  @complexity Constant.

  @liveexample{The example shows several calls to `get_ref()`.,get_ref}

  @since version 1.1.0
  */
  template <typename T, typename ReferenceType = std::add_lvalue_reference_t<T>,
            typename std::enable_if<std::is_reference<ReferenceType>::value,
                                    int>::type = 0>
  ReferenceType get_ref() {
    // delegate call to get_ref_impl
    return get_ref_impl<ReferenceType>(*this);
  }

  /*!
  @brief get a reference value (implicit)
  @copydoc get_ref()
  */
  template <
      typename T,
      typename ReferenceType = std::add_lvalue_reference_t<std::add_const_t<T>>,
      typename std::enable_if<std::is_reference<ReferenceType>::value &&
                                  std::is_const<typename std::remove_reference<
                                      ReferenceType>::type>::value,
                              int>::type = 0>
  ReferenceType get_ref() const {
    // delegate call to get_ref_impl
    return get_ref_impl<ReferenceType>(*this);
  }

  /*!
  @brief get a value (implicit)

  Implicit type conversion between the JSON value and a compatible value.
  The call is realized by calling @ref get() const.

  @tparam ValueType non-pointer type compatible to the JSON value, for
  instance `int` for JSON integer numbers, `bool` for JSON booleans, or
  `std::vector` types for JSON arrays. The character type of @ref string_t
  as well as an initializer list of this type is excluded to avoid
  ambiguities as these types implicitly convert to `std::string`.

  @return copy of the JSON value, converted to type @a ValueType

  @throw type_error.302 in case passed type @a ValueType is incompatible
  to the JSON value type (e.g., the JSON value is of type boolean, but a
  string is requested); see example below

  @complexity Linear in the size of the JSON value.

  @liveexample{The example below shows several conversions from JSON values
  to other types. There a few things to note: (1) Floating-point numbers can
  be converted to integers\, (2) A JSON array can be converted to a standard
  `std::vector<short>`\, (3) A JSON object can be converted to C++
  associative containers such as `std::unordered_map<std::string\,
  json>`.,operator__ValueType}

  @since version 1.0.0
  */
  template <
      typename ValueType,
      typename std::enable_if<
          !std::is_pointer<ValueType>::value &&
              !std::is_same<ValueType, detail::json_ref<basic_json>>::value &&
              !std::is_same<ValueType, typename string_t::value_type>::value &&
              !detail::is_basic_json<ValueType>::value &&
#ifndef _MSC_VER // fix for issue #167 operator<< ambiguity under VS2015
              !std::is_same<ValueType,
                            std::initializer_list<
                                typename string_t::value_type>>::value &&
#endif
#if defined(JSON_HAS_CPP_17)
              !std::is_same<ValueType, typename std::string_view>::value &&
#endif
              true,
          int>::type = 0>
  operator ValueType() const {
    // delegate the call to get<>() const
    return get<ValueType>();
  }

  // Take the json value and set the current object to null, super unsafe.
  std::pair<void*, value_t> take() {
    std::pair<void*, value_t> ret = {reinterpret_cast<void*>(m_value), m_type};
    memset(m_value, '\0', sizeof(m_value));
    m_type = value_t::null;
    return ret;
  }

  /// @}

  ////////////////////
  // element access //
  ////////////////////

  /// @name element access
  /// Access to the JSON value.
  /// @{

  /*!
  @brief access specified array element with bounds checking

  Returns a reference to the element at specified location @a idx, with
  bounds checking.

  @param[in] idx  index of the element to access

  @return reference to the element at index @a idx

  @throw type_error.304 if the JSON value is not an array; in this case,
  calling `at` with an index makes no sense. See example below.
  @throw out_of_range.401 if the index @a idx is out of range of the array;
  that is, `idx >= size()`. See example below.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes in the JSON value.

  @complexity Constant.

  @since version 1.0.0

  @liveexample{The example below shows how array elements can be read and
  written using `at()`. It also demonstrates the different exceptions that
  can be thrown.,at__size_type}
  */
  reference at(size_type idx) {
    // at only works for arrays
    if (JSON_LIKELY(is_array())) {
      JSON_TRY { return m_value.array->at(idx); }
      JSON_CATCH(std::out_of_range&) {
        // create better exception explanation
        JSON_THROW(out_of_range::create(
            401, "array index " + std::to_string(idx) + " is out of range"));
      }
    } else {
      JSON_THROW(type_error::create(304, "can!use at() with " +
                                             std::string(type_name())));
    }
  }

  /*!
  @brief access specified array element with bounds checking

  Returns a const reference to the element at specified location @a idx,
  with bounds checking.

  @param[in] idx  index of the element to access

  @return const reference to the element at index @a idx

  @throw type_error.304 if the JSON value is not an array; in this case,
  calling `at` with an index makes no sense. See example below.
  @throw out_of_range.401 if the index @a idx is out of range of the array;
  that is, `idx >= size()`. See example below.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes in the JSON value.

  @complexity Constant.

  @since version 1.0.0

  @liveexample{The example below shows how array elements can be read using
  `at()`. It also demonstrates the different exceptions that can be thrown.,
  at__size_type_const}
  */
  const_reference at(size_type idx) const {
    // at only works for arrays
    if (JSON_LIKELY(is_array())) {
      JSON_TRY { return m_value.array->at(idx); }
      JSON_CATCH(std::out_of_range&) {
        // create better exception explanation
        JSON_THROW(out_of_range::create(
            401, "array index " + std::to_string(idx) + " is out of range"));
      }
    } else {
      JSON_THROW(type_error::create(304, "can!use at() with " +
                                             std::string(type_name())));
    }
  }

  /*!
  @brief access specified object element with bounds checking

  Returns a reference to the element at with specified key @a key, with
  bounds checking.

  @param[in] key  key of the element to access

  @return reference to the element at key @a key

  @throw type_error.304 if the JSON value is not an object; in this case,
  calling `at` with a key makes no sense. See example below.
  @throw out_of_range.403 if the key @a key is is not stored in the object;
  that is, `find(key) == end()`. See example below.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes in the JSON value.

  @complexity Logarithmic in the size of the container.

  @sa @ref operator[](const typename object_t::key_type&) for unchecked
  access by reference
  @sa @ref value() for access by value with a default value

  @since version 1.0.0

  @liveexample{The example below shows how object elements can be read and
  written using `at()`. It also demonstrates the different exceptions that
  can be thrown.,at__object_t_key_type}
  */
  reference at(const typename object_t::key_type& key) {
    // at only works for objects
    if (JSON_LIKELY(is_object())) {
      JSON_TRY { return m_value.object->at(key); }
      JSON_CATCH(std::out_of_range&) {
        // create better exception explanation
        JSON_THROW(out_of_range::create(403, "key '" + key + "' !found"));
      }
    } else {
      JSON_THROW(type_error::create(304, "can!use at() with " +
                                             std::string(type_name())));
    }
  }

  /*!
  @brief access specified object element with bounds checking

  Returns a const reference to the element at with specified key @a key,
  with bounds checking.

  @param[in] key  key of the element to access

  @return const reference to the element at key @a key

  @throw type_error.304 if the JSON value is not an object; in this case,
  calling `at` with a key makes no sense. See example below.
  @throw out_of_range.403 if the key @a key is is not stored in the object;
  that is, `find(key) == end()`. See example below.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes in the JSON value.

  @complexity Logarithmic in the size of the container.

  @sa @ref operator[](const typename object_t::key_type&) for unchecked
  access by reference
  @sa @ref value() for access by value with a default value

  @since version 1.0.0

  @liveexample{The example below shows how object elements can be read using
  `at()`. It also demonstrates the different exceptions that can be thrown.,
  at__object_t_key_type_const}
  */
  const_reference at(const typename object_t::key_type& key) const {
    // at only works for objects
    if (JSON_LIKELY(is_object())) {
      JSON_TRY { return m_value.object->at(key); }
      JSON_CATCH(std::out_of_range&) {
        // create better exception explanation
        JSON_THROW(out_of_range::create(403, "key '" + key + "' !found"));
      }
    } else {
      JSON_THROW(type_error::create(304, "can!use at() with " +
                                             std::string(type_name())));
    }
  }

  /*!
  @brief access specified array element

  Returns a reference to the element at specified location @a idx.

  @note If @a idx is beyond the range of the array (i.e., `idx >= size()`),
  then the array is silently filled up with `null` values to make `idx` a
  valid reference to the last stored element.

  @param[in] idx  index of the element to access

  @return reference to the element at index @a idx

  @throw type_error.305 if the JSON value is not an array or null; in that
  cases, using the [] operator with an index makes no sense.

  @complexity Constant if @a idx is in the range of the array. Otherwise
  linear in `idx - size()`.

  @liveexample{The example below shows how array elements can be read and
  written using `[]` operator. Note the addition of `null`
  values.,operatorarray__size_type}

  @since version 1.0.0
  */
  reference operator[](size_type idx) {
    // implicitly convert null value to an empty array
    if (is_null()) {
      m_type = value_t::array;
      m_value.array = create<array_t>();
      assert_invariant();
    }

    // operator[] only works for arrays
    if (JSON_LIKELY(is_array())) {
      // fill up array with null values if given idx is outside range
      if (idx >= m_value.array->size()) {
        m_value.array->insert(m_value.array->end(),
                              idx - m_value.array->size() + 1, basic_json());
      }

      return m_value.array->operator[](idx);
    }

    JSON_THROW(type_error::create(305, "can!use operator[] with " +
                                           std::string(type_name())));
  }

  /*!
  @brief access specified array element

  Returns a const reference to the element at specified location @a idx.

  @param[in] idx  index of the element to access

  @return const reference to the element at index @a idx

  @throw type_error.305 if the JSON value is not an array; in that case,
  using the [] operator with an index makes no sense.

  @complexity Constant.

  @liveexample{The example below shows how array elements can be read using
  the `[]` operator.,operatorarray__size_type_const}

  @since version 1.0.0
  */
  const_reference operator[](size_type idx) const {
    // const operator[] only works for arrays
    if (JSON_LIKELY(is_array())) {
      return m_value.array->operator[](idx);
    }

    JSON_THROW(type_error::create(305, "can!use operator[] with " +
                                           std::string(type_name())));
  }

  /*!
  @brief access specified object element

  Returns a reference to the element at with specified key @a key.

  @note If @a key is not found in the object, then it is silently added to
  the object and filled with a `null` value to make `key` a valid reference.
  In case the value was `null` before, it is converted to an object.

  @param[in] key  key of the element to access

  @return reference to the element at key @a key

  @throw type_error.305 if the JSON value is not an object or null; in that
  cases, using the [] operator with a key makes no sense.

  @complexity Logarithmic in the size of the container.

  @liveexample{The example below shows how object elements can be read and
  written using the `[]` operator.,operatorarray__key_type}

  @sa @ref at(const typename object_t::key_type&) for access by reference
  with range checking
  @sa @ref value() for access by value with a default value

  @since version 1.0.0
  */
  reference operator[](const typename object_t::key_type& key) {
    // implicitly convert null value to an empty object
    if (is_null()) {
      m_type = value_t::object;
      m_value.object = create<object_t>();
      assert_invariant();
    }

    // operator[] only works for objects
    if (JSON_LIKELY(is_object())) {
      return m_value.object->operator[](key);
    }

    JSON_THROW(type_error::create(305, "can!use operator[] with " +
                                           std::string(type_name())));
  }

  /*!
  @brief read-only access specified object element

  Returns a const reference to the element at with specified key @a key. No
  bounds checking is performed.

  @warning If the element with key @a key does not exist, the behavior is
  undefined.

  @param[in] key  key of the element to access

  @return const reference to the element at key @a key

  @pre The element with key @a key must exist. **This precondition is
       enforced with an assertion.**

  @throw type_error.305 if the JSON value is not an object; in that case,
  using the [] operator with a key makes no sense.

  @complexity Logarithmic in the size of the container.

  @liveexample{The example below shows how object elements can be read using
  the `[]` operator.,operatorarray__key_type_const}

  @sa @ref at(const typename object_t::key_type&) for access by reference
  with range checking
  @sa @ref value() for access by value with a default value

  @since version 1.0.0
  */
  const_reference operator[](const typename object_t::key_type& key) const {
    // const operator[] only works for objects
    if (JSON_LIKELY(is_object())) {
      assert(m_value.object->find(key) != m_value.object->end());
      return m_value.object->find(key)->second;
    }

    JSON_THROW(type_error::create(305, "can!use operator[] with " +
                                           std::string(type_name())));
  }

  /*!
  @brief access specified object element

  Returns a reference to the element at with specified key @a key.

  @note If @a key is not found in the object, then it is silently added to
  the object and filled with a `null` value to make `key` a valid reference.
  In case the value was `null` before, it is converted to an object.

  @param[in] key  key of the element to access

  @return reference to the element at key @a key

  @throw type_error.305 if the JSON value is not an object or null; in that
  cases, using the [] operator with a key makes no sense.

  @complexity Logarithmic in the size of the container.

  @liveexample{The example below shows how object elements can be read and
  written using the `[]` operator.,operatorarray__key_type}

  @sa @ref at(const typename object_t::key_type&) for access by reference
  with range checking
  @sa @ref value() for access by value with a default value

  @since version 1.1.0
  */
  template <typename T> reference operator[](T* key) {
    // implicitly convert null to object
    if (is_null()) {
      m_type = value_t::object;
      m_value = value_t::object;
      assert_invariant();
    }

    // at only works for objects
    if (JSON_LIKELY(is_object())) {
      return m_value.object->operator[](key);
    }

    JSON_THROW(type_error::create(305, "can!use operator[] with " +
                                           std::string(type_name())));
  }

  /*!
  @brief read-only access specified object element

  Returns a const reference to the element at with specified key @a key. No
  bounds checking is performed.

  @warning If the element with key @a key does not exist, the behavior is
  undefined.

  @param[in] key  key of the element to access

  @return const reference to the element at key @a key

  @pre The element with key @a key must exist. **This precondition is
       enforced with an assertion.**

  @throw type_error.305 if the JSON value is not an object; in that case,
  using the [] operator with a key makes no sense.

  @complexity Logarithmic in the size of the container.

  @liveexample{The example below shows how object elements can be read using
  the `[]` operator.,operatorarray__key_type_const}

  @sa @ref at(const typename object_t::key_type&) for access by reference
  with range checking
  @sa @ref value() for access by value with a default value

  @since version 1.1.0
  */
  template <typename T> const_reference operator[](T* key) const {
    // at only works for objects
    if (JSON_LIKELY(is_object())) {
      assert(m_value.object->find(key) != m_value.object->end());
      return m_value.object->find(key)->second;
    }

    JSON_THROW(type_error::create(305, "can!use operator[] with " +
                                           std::string(type_name())));
  }

  /*!
  @brief access specified object element with default value

  Returns either a copy of an object's element at the specified key @a key
  or a given default value if no element with key @a key exists.

  The function is basically equivalent to executing
  @code {.cpp}
  try {
      return at(key);
  } catch(out_of_range) {
      return default_value;
  }
  @endcode

  @note Unlike @ref at(const typename object_t::key_type&), this function
  does not throw if the given key @a key was not found.

  @note Unlike @ref operator[](const typename object_t::key_type& key), this
  function does not implicitly add an element to the position defined by @a
  key. This function is furthermore also applicable to const objects.

  @param[in] key  key of the element to access
  @param[in] default_value  the value to return if @a key is not found

  @tparam ValueType type compatible to JSON values, for instance `int` for
  JSON integer numbers, `bool` for JSON booleans, or `std::vector` types for
  JSON arrays. Note the type of the expected value at @a key and the default
  value @a default_value must be compatible.

  @return copy of the element at key @a key or @a default_value if @a key
  is not found

  @throw type_error.306 if the JSON value is not an object; in that case,
  using `value()` with a key makes no sense.

  @complexity Logarithmic in the size of the container.

  @liveexample{The example below shows how object elements can be queried
  with a default value.,basic_json__value}

  @sa @ref at(const typename object_t::key_type&) for access by reference
  with range checking
  @sa @ref operator[](const typename object_t::key_type&) for unchecked
  access by reference

  @since version 1.0.0
  */
  template <
      class ValueType,
      typename std::enable_if<
          std::is_convertible<basic_json_t, ValueType>::value, int>::type = 0>
  ValueType value(const typename object_t::key_type& key,
                  const ValueType& default_value) const {
    // at only works for objects
    if (JSON_LIKELY(is_object())) {
      // if key is found, return value and given default value otherwise
      const auto it = find(key);
      if (it != end()) {
        return *it;
      }

      return default_value;
    }

    JSON_THROW(type_error::create(306, "can!use value() with " +
                                           std::string(type_name())));
  }

  /*!
  @brief overload for a default value of type const char*
  @copydoc basic_json::value(const typename object_t::key_type&, ValueType)
  const
  */
  string_t value(const typename object_t::key_type& key,
                 const char* default_value) const {
    return value(key, string_t(default_value));
  }

  /*!
  @brief access specified object element via JSON Pointer with default value

  Returns either a copy of an object's element at the specified key @a key
  or a given default value if no element with key @a key exists.

  The function is basically equivalent to executing
  @code {.cpp}
  try {
      return at(ptr);
  } catch(out_of_range) {
      return default_value;
  }
  @endcode

  @note Unlike @ref at(const json_pointer&), this function does not throw
  if the given key @a key was not found.

  @param[in] ptr  a JSON pointer to the element to access
  @param[in] default_value  the value to return if @a ptr found no value

  @tparam ValueType type compatible to JSON values, for instance `int` for
  JSON integer numbers, `bool` for JSON booleans, or `std::vector` types for
  JSON arrays. Note the type of the expected value at @a key and the default
  value @a default_value must be compatible.

  @return copy of the element at key @a key or @a default_value if @a key
  is not found

  @throw type_error.306 if the JSON value is not an objec; in that case,
  using `value()` with a key makes no sense.

  @complexity Logarithmic in the size of the container.

  @liveexample{The example below shows how object elements can be queried
  with a default value.,basic_json__value_ptr}

  @sa @ref operator[](const json_pointer&) for unchecked access by reference

  @since version 2.0.2
  */
  template <
      class ValueType,
      typename std::enable_if<
          std::is_convertible<basic_json_t, ValueType>::value, int>::type = 0>
  ValueType value(const json_pointer& ptr,
                  const ValueType& default_value) const {
    // at only works for objects
    if (JSON_LIKELY(is_object())) {
      // if pointer resolves a value, return it or use default value
      JSON_TRY { return ptr.get_checked(this); }
      JSON_CATCH(out_of_range&) { return default_value; }
    }

    JSON_THROW(type_error::create(306, "can!use value() with " +
                                           std::string(type_name())));
  }

  /*!
  @brief overload for a default value of type const char*
  @copydoc basic_json::value(const json_pointer&, ValueType) const
  */
  string_t value(const json_pointer& ptr, const char* default_value) const {
    return value(ptr, string_t(default_value));
  }

  /*!
  @brief access the first element

  Returns a reference to the first element in the container. For a JSON
  container `c`, the expression `c.front()` is equivalent to `*c.begin()`.

  @return In case of a structured type (array or object), a reference to the
  first element is returned. In case of number, string, or boolean values, a
  reference to the value is returned.

  @complexity Constant.

  @pre The JSON value must not be `null` (would throw `std::out_of_range`)
  or an empty array or object (undefined behavior, **guarded by
  assertions**).
  @post The JSON value remains unchanged.

  @throw invalid_iterator.214 when called on `null` value

  @liveexample{The following code shows an example for `front()`.,front}

  @sa @ref back() -- access the last element

  @since version 1.0.0
  */
  reference front() { return *begin(); }

  /*!
  @copydoc basic_json::front()
  */
  const_reference front() const { return *cbegin(); }

  /*!
  @brief access the last element

  Returns a reference to the last element in the container. For a JSON
  container `c`, the expression `c.back()` is equivalent to
  @code {.cpp}
  auto tmp = c.end();
  --tmp;
  return *tmp;
  @endcode

  @return In case of a structured type (array or object), a reference to the
  last element is returned. In case of number, string, or boolean values, a
  reference to the value is returned.

  @complexity Constant.

  @pre The JSON value must not be `null` (would throw `std::out_of_range`)
  or an empty array or object (undefined behavior, **guarded by
  assertions**).
  @post The JSON value remains unchanged.

  @throw invalid_iterator.214 when called on a `null` value. See example
  below.

  @liveexample{The following code shows an example for `back()`.,back}

  @sa @ref front() -- access the first element

  @since version 1.0.0
  */
  reference back() {
    auto tmp = end();
    --tmp;
    return *tmp;
  }

  /*!
  @copydoc basic_json::back()
  */
  const_reference back() const {
    auto tmp = cend();
    --tmp;
    return *tmp;
  }

  /*!
  @brief remove element given an iterator

  Removes the element specified by iterator @a pos. The iterator @a pos must
  be valid and dereferenceable. Thus the `end()` iterator (which is valid,
  but is not dereferenceable) cannot be used as a value for @a pos.

  If called on a primitive type other than `null`, the resulting JSON value
  will be `null`.

  @param[in] pos iterator to the element to remove
  @return Iterator following the last removed element. If the iterator @a
  pos refers to the last element, the `end()` iterator is returned.

  @tparam IteratorType an @ref iterator or @ref const_iterator

  @post Invalidates iterators and references at or after the point of the
  erase, including the `end()` iterator.

  @throw type_error.307 if called on a `null` value; example: `"cannot use
  erase() with null"`
  @throw invalid_iterator.202 if called on an iterator which does not belong
  to the current JSON value; example: `"iterator does not fit current
  value"`
  @throw invalid_iterator.205 if called on a primitive type with invalid
  iterator (i.e., any iterator which is not `begin()`); example: `"iterator
  out of range"`

  @complexity The complexity depends on the type:
  - objects: amortized constant
  - arrays: linear in distance between @a pos and the end of the container
  - strings: linear in the length of the string
  - other types: constant

  @liveexample{The example shows the result of `erase()` for different JSON
  types.,erase__IteratorType}

  @sa @ref erase(IteratorType, IteratorType) -- removes the elements in
  the given range
  @sa @ref erase(const typename object_t::key_type&) -- removes the element
  from an object at the given key
  @sa @ref erase(const size_type) -- removes the element from an array at
  the given index

  @since version 1.0.0
  */
  template <
      class IteratorType,
      typename std::enable_if<
          std::is_same<IteratorType, typename basic_json_t::iterator>::value ||
              std::is_same<IteratorType,
                           typename basic_json_t::const_iterator>::value,
          int>::type = 0>
  IteratorType erase(IteratorType pos) {
    // make sure iterator fits the current value
    if (JSON_UNLIKELY(this != pos.m_object)) {
      JSON_THROW(
          invalid_iterator::create(202, "iterator does !fit current value"));
    }

    IteratorType result = end();

    switch (m_type) {
    case value_t::boolean:
    case value_t::number_float:
    case value_t::number_integer:
    case value_t::number_unsigned:
    case value_t::string: {
      if (JSON_UNLIKELY(!pos.m_it.primitive_iterator.is_begin())) {
        JSON_THROW(invalid_iterator::create(205, "iterator out of range"));
      }

      if (is_string()) {
        AllocatorType<string_t> alloc;
        std::allocator_traits<decltype(alloc)>::destroy(alloc, m_value.string);
        std::allocator_traits<decltype(alloc)>::deallocate(alloc,
                                                           m_value.string, 1);
        m_value.string = nullptr;
      }

      m_type = value_t::null;
      assert_invariant();
      break;
    }

    case value_t::object: {
      result.m_it.object_iterator =
          m_value.object->erase(pos.m_it.object_iterator);
      break;
    }

    case value_t::array: {
      result.m_it.array_iterator =
          m_value.array->erase(pos.m_it.array_iterator);
      break;
    }

    default:
      JSON_THROW(type_error::create(307, "can!use erase() with " +
                                             std::string(type_name())));
    }

    return result;
  }

  /*!
  @brief remove elements given an iterator range

  Removes the element specified by the range `[first; last)`. The iterator
  @a first does not need to be dereferenceable if `first == last`: erasing
  an empty range is a no-op.

  If called on a primitive type other than `null`, the resulting JSON value
  will be `null`.

  @param[in] first iterator to the beginning of the range to remove
  @param[in] last iterator past the end of the range to remove
  @return Iterator following the last removed element. If the iterator @a
  second refers to the last element, the `end()` iterator is returned.

  @tparam IteratorType an @ref iterator or @ref const_iterator

  @post Invalidates iterators and references at or after the point of the
  erase, including the `end()` iterator.

  @throw type_error.307 if called on a `null` value; example: `"cannot use
  erase() with null"`
  @throw invalid_iterator.203 if called on iterators which does not belong
  to the current JSON value; example: `"iterators do not fit current value"`
  @throw invalid_iterator.204 if called on a primitive type with invalid
  iterators (i.e., if `first != begin()` and `last != end()`); example:
  `"iterators out of range"`

  @complexity The complexity depends on the type:
  - objects: `log(size()) + std::distance(first, last)`
  - arrays: linear in the distance between @a first and @a last, plus linear
    in the distance between @a last and end of the container
  - strings: linear in the length of the string
  - other types: constant

  @liveexample{The example shows the result of `erase()` for different JSON
  types.,erase__IteratorType_IteratorType}

  @sa @ref erase(IteratorType) -- removes the element at a given position
  @sa @ref erase(const typename object_t::key_type&) -- removes the element
  from an object at the given key
  @sa @ref erase(const size_type) -- removes the element from an array at
  the given index

  @since version 1.0.0
  */
  template <
      class IteratorType,
      typename std::enable_if<
          std::is_same<IteratorType, typename basic_json_t::iterator>::value ||
              std::is_same<IteratorType,
                           typename basic_json_t::const_iterator>::value,
          int>::type = 0>
  IteratorType erase(IteratorType first, IteratorType last) {
    // make sure iterator fits the current value
    if (JSON_UNLIKELY(this != first.m_object || this != last.m_object)) {
      JSON_THROW(
          invalid_iterator::create(203, "iterators do !fit current value"));
    }

    IteratorType result = end();

    switch (m_type) {
    case value_t::boolean:
    case value_t::number_float:
    case value_t::number_integer:
    case value_t::number_unsigned:
    case value_t::string: {
      if (JSON_LIKELY(!first.m_it.primitive_iterator.is_begin() ||
                      !last.m_it.primitive_iterator.is_end())) {
        JSON_THROW(invalid_iterator::create(204, "iterators out of range"));
      }

      if (is_string()) {
        AllocatorType<string_t> alloc;
        std::allocator_traits<decltype(alloc)>::destroy(alloc, m_value.string);
        std::allocator_traits<decltype(alloc)>::deallocate(alloc,
                                                           m_value.string, 1);
        m_value.string = nullptr;
      }

      m_type = value_t::null;
      assert_invariant();
      break;
    }

    case value_t::object: {
      result.m_it.object_iterator = m_value.object->erase(
          first.m_it.object_iterator, last.m_it.object_iterator);
      break;
    }

    case value_t::array: {
      result.m_it.array_iterator = m_value.array->erase(
          first.m_it.array_iterator, last.m_it.array_iterator);
      break;
    }

    default:
      JSON_THROW(type_error::create(307, "can!use erase() with " +
                                             std::string(type_name())));
    }

    return result;
  }

  /*!
  @brief remove element from a JSON object given a key

  Removes elements from a JSON object with the key value @a key.

  @param[in] key value of the elements to remove

  @return Number of elements removed. If @a ObjectType is the default
  `std::map` type, the return value will always be `0` (@a key was not
  found) or `1` (@a key was found).

  @post References and iterators to the erased elements are invalidated.
  Other references and iterators are not affected.

  @throw type_error.307 when called on a type other than JSON object;
  example: `"cannot use erase() with null"`

  @complexity `log(size()) + count(key)`

  @liveexample{The example shows the effect of `erase()`.,erase__key_type}

  @sa @ref erase(IteratorType) -- removes the element at a given position
  @sa @ref erase(IteratorType, IteratorType) -- removes the elements in
  the given range
  @sa @ref erase(const size_type) -- removes the element from an array at
  the given index

  @since version 1.0.0
  */
  size_type erase(const typename object_t::key_type& key) {
    // this erase only works for objects
    if (JSON_LIKELY(is_object())) {
      return m_value.object->erase(key);
    }

    JSON_THROW(type_error::create(307, "can!use erase() with " +
                                           std::string(type_name())));
  }

  /*!
  @brief remove element from a JSON array given an index

  Removes element from a JSON array at the index @a idx.

  @param[in] idx index of the element to remove

  @throw type_error.307 when called on a type other than JSON object;
  example: `"cannot use erase() with null"`
  @throw out_of_range.401 when `idx >= size()`; example: `"array index 17
  is out of range"`

  @complexity Linear in distance between @a idx and the end of the container.

  @liveexample{The example shows the effect of `erase()`.,erase__size_type}

  @sa @ref erase(IteratorType) -- removes the element at a given position
  @sa @ref erase(IteratorType, IteratorType) -- removes the elements in
  the given range
  @sa @ref erase(const typename object_t::key_type&) -- removes the element
  from an object at the given key

  @since version 1.0.0
  */
  void erase(const size_type idx) {
    // this erase only works for arrays
    if (JSON_LIKELY(is_array())) {
      if (JSON_UNLIKELY(idx >= size())) {
        JSON_THROW(out_of_range::create(
            401, "array index " + std::to_string(idx) + " is out of range"));
      }

      m_value.array->erase(m_value.array->begin() +
                           static_cast<difference_type>(idx));
    } else {
      JSON_THROW(type_error::create(307, "can!use erase() with " +
                                             std::string(type_name())));
    }
  }

  /// @}

  ////////////
  // lookup //
  ////////////

  /// @name lookup
  /// @{

  /*!
  @brief find an element in a JSON object

  Finds an element in a JSON object with key equivalent to @a key. If the
  element is not found or the JSON value is not an object, end() is
  returned.

  @note This method always returns @ref end() when executed on a JSON type
        that is not an object.

  @param[in] key key value of the element to search for.

  @return Iterator to an element with key equivalent to @a key. If no such
  element is found or the JSON value is not an object, past-the-end (see
  @ref end()) iterator is returned.

  @complexity Logarithmic in the size of the JSON object.

  @liveexample{The example shows how `find()` is used.,find__key_type}

  @since version 1.0.0
  */
  template <typename KeyT> iterator find(KeyT&& key) {
    auto result = end();

    if (is_object()) {
      result.m_it.object_iterator =
          m_value.object->find(std::forward<KeyT>(key));
    }

    return result;
  }

  /*!
  @brief find an element in a JSON object
  @copydoc find(KeyT&&)
  */
  template <typename KeyT> const_iterator find(KeyT&& key) const {
    auto result = cend();

    if (is_object()) {
      result.m_it.object_iterator =
          m_value.object->find(std::forward<KeyT>(key));
    }

    return result;
  }

  /*!
  @brief returns the number of occurrences of a key in a JSON object

  Returns the number of elements with key @a key. If ObjectType is the
  default `std::map` type, the return value will always be `0` (@a key was
  not found) or `1` (@a key was found).

  @note This method always returns `0` when executed on a JSON type that is
        not an object.

  @param[in] key key value of the element to count

  @return Number of elements with key @a key. If the JSON value is not an
  object, the return value will be `0`.

  @complexity Logarithmic in the size of the JSON object.

  @liveexample{The example shows how `count()` is used.,count}

  @since version 1.0.0
  */
  template <typename KeyT> size_type count(KeyT&& key) const {
    // return 0 for all nonobject types
    return is_object() ? m_value.object->count(std::forward<KeyT>(key)) : 0;
  }

  /// @}

  ///////////////
  // iterators //
  ///////////////

  /// @name iterators
  /// @{

  /*!
  @brief returns an iterator to the first element

  Returns an iterator to the first element.

  @image html range-begin-end.svg "Illustration from cppreference.com"

  @return iterator to the first element

  @complexity Constant.

  @requirement This function helps `basic_json` satisfying the
  [Container](http://en.cppreference.com/w/cpp/concept/Container)
  requirements:
  - The complexity is constant.

  @liveexample{The following code shows an example for `begin()`.,begin}

  @sa @ref cbegin() -- returns a const iterator to the beginning
  @sa @ref end() -- returns an iterator to the end
  @sa @ref cend() -- returns a const iterator to the end

  @since version 1.0.0
  */
  iterator begin() noexcept {
    iterator result(this);
    result.set_begin();
    return result;
  }

  /*!
  @copydoc basic_json::cbegin()
  */
  const_iterator begin() const noexcept { return cbegin(); }

  /*!
  @brief returns a const iterator to the first element

  Returns a const iterator to the first element.

  @image html range-begin-end.svg "Illustration from cppreference.com"

  @return const iterator to the first element

  @complexity Constant.

  @requirement This function helps `basic_json` satisfying the
  [Container](http://en.cppreference.com/w/cpp/concept/Container)
  requirements:
  - The complexity is constant.
  - Has the semantics of `const_cast<const basic_json&>(*this).begin()`.

  @liveexample{The following code shows an example for `cbegin()`.,cbegin}

  @sa @ref begin() -- returns an iterator to the beginning
  @sa @ref end() -- returns an iterator to the end
  @sa @ref cend() -- returns a const iterator to the end

  @since version 1.0.0
  */
  const_iterator cbegin() const noexcept {
    const_iterator result(this);
    result.set_begin();
    return result;
  }

  /*!
  @brief returns an iterator to one past the last element

  Returns an iterator to one past the last element.

  @image html range-begin-end.svg "Illustration from cppreference.com"

  @return iterator one past the last element

  @complexity Constant.

  @requirement This function helps `basic_json` satisfying the
  [Container](http://en.cppreference.com/w/cpp/concept/Container)
  requirements:
  - The complexity is constant.

  @liveexample{The following code shows an example for `end()`.,end}

  @sa @ref cend() -- returns a const iterator to the end
  @sa @ref begin() -- returns an iterator to the beginning
  @sa @ref cbegin() -- returns a const iterator to the beginning

  @since version 1.0.0
  */
  iterator end() noexcept {
    iterator result(this);
    result.set_end();
    return result;
  }

  /*!
  @copydoc basic_json::cend()
  */
  const_iterator end() const noexcept { return cend(); }

  /*!
  @brief returns a const iterator to one past the last element

  Returns a const iterator to one past the last element.

  @image html range-begin-end.svg "Illustration from cppreference.com"

  @return const iterator one past the last element

  @complexity Constant.

  @requirement This function helps `basic_json` satisfying the
  [Container](http://en.cppreference.com/w/cpp/concept/Container)
  requirements:
  - The complexity is constant.
  - Has the semantics of `const_cast<const basic_json&>(*this).end()`.

  @liveexample{The following code shows an example for `cend()`.,cend}

  @sa @ref end() -- returns an iterator to the end
  @sa @ref begin() -- returns an iterator to the beginning
  @sa @ref cbegin() -- returns a const iterator to the beginning

  @since version 1.0.0
  */
  const_iterator cend() const noexcept {
    const_iterator result(this);
    result.set_end();
    return result;
  }

  /*!
  @brief returns an iterator to the reverse-beginning

  Returns an iterator to the reverse-beginning; that is, the last element.

  @image html range-rbegin-rend.svg "Illustration from cppreference.com"

  @complexity Constant.

  @requirement This function helps `basic_json` satisfying the
  [ReversibleContainer](http://en.cppreference.com/w/cpp/concept/ReversibleContainer)
  requirements:
  - The complexity is constant.
  - Has the semantics of `reverse_iterator(end())`.

  @liveexample{The following code shows an example for `rbegin()`.,rbegin}

  @sa @ref crbegin() -- returns a const reverse iterator to the beginning
  @sa @ref rend() -- returns a reverse iterator to the end
  @sa @ref crend() -- returns a const reverse iterator to the end

  @since version 1.0.0
  */
  reverse_iterator rbegin() noexcept { return reverse_iterator(end()); }

  /*!
  @copydoc basic_json::crbegin()
  */
  const_reverse_iterator rbegin() const noexcept { return crbegin(); }

  /*!
  @brief returns an iterator to the reverse-end

  Returns an iterator to the reverse-end; that is, one before the first
  element.

  @image html range-rbegin-rend.svg "Illustration from cppreference.com"

  @complexity Constant.

  @requirement This function helps `basic_json` satisfying the
  [ReversibleContainer](http://en.cppreference.com/w/cpp/concept/ReversibleContainer)
  requirements:
  - The complexity is constant.
  - Has the semantics of `reverse_iterator(begin())`.

  @liveexample{The following code shows an example for `rend()`.,rend}

  @sa @ref crend() -- returns a const reverse iterator to the end
  @sa @ref rbegin() -- returns a reverse iterator to the beginning
  @sa @ref crbegin() -- returns a const reverse iterator to the beginning

  @since version 1.0.0
  */
  reverse_iterator rend() noexcept { return reverse_iterator(begin()); }

  /*!
  @copydoc basic_json::crend()
  */
  const_reverse_iterator rend() const noexcept { return crend(); }

  /*!
  @brief returns a const reverse iterator to the last element

  Returns a const iterator to the reverse-beginning; that is, the last
  element.

  @image html range-rbegin-rend.svg "Illustration from cppreference.com"

  @complexity Constant.

  @requirement This function helps `basic_json` satisfying the
  [ReversibleContainer](http://en.cppreference.com/w/cpp/concept/ReversibleContainer)
  requirements:
  - The complexity is constant.
  - Has the semantics of `const_cast<const basic_json&>(*this).rbegin()`.

  @liveexample{The following code shows an example for `crbegin()`.,crbegin}

  @sa @ref rbegin() -- returns a reverse iterator to the beginning
  @sa @ref rend() -- returns a reverse iterator to the end
  @sa @ref crend() -- returns a const reverse iterator to the end

  @since version 1.0.0
  */
  const_reverse_iterator crbegin() const noexcept {
    return const_reverse_iterator(cend());
  }

  /*!
  @brief returns a const reverse iterator to one before the first

  Returns a const reverse iterator to the reverse-end; that is, one before
  the first element.

  @image html range-rbegin-rend.svg "Illustration from cppreference.com"

  @complexity Constant.

  @requirement This function helps `basic_json` satisfying the
  [ReversibleContainer](http://en.cppreference.com/w/cpp/concept/ReversibleContainer)
  requirements:
  - The complexity is constant.
  - Has the semantics of `const_cast<const basic_json&>(*this).rend()`.

  @liveexample{The following code shows an example for `crend()`.,crend}

  @sa @ref rend() -- returns a reverse iterator to the end
  @sa @ref rbegin() -- returns a reverse iterator to the beginning
  @sa @ref crbegin() -- returns a const reverse iterator to the beginning

  @since version 1.0.0
  */
  const_reverse_iterator crend() const noexcept {
    return const_reverse_iterator(cbegin());
  }

  /*!
  @brief wrapper to access iterator member functions in range-based for

  This function allows to access @ref iterator::key() and @ref
  iterator::value() during range-based for loops. In these loops, a
  reference to the JSON values is returned, so there is no access to the
  underlying iterator.

  For loop without iterator_wrapper:

  @code{cpp}
  for (auto it = j_object.begin(); it != j_object.end(); ++it)
  {
      std::cout << "key: " << it.key() << ", value:" << it.value() << '\n';
  }
  @endcode

  Range-based for loop without iterator proxy:

  @code{cpp}
  for (auto it : j_object)
  {
      // "it" is of type json::reference and has no key() member
      std::cout << "value: " << it << '\n';
  }
  @endcode

  Range-based for loop with iterator proxy:

  @code{cpp}
  for (auto it : json::iterator_wrapper(j_object))
  {
      std::cout << "key: " << it.key() << ", value:" << it.value() << '\n';
  }
  @endcode

  @note When iterating over an array, `key()` will return the index of the
        element as string (see example).

  @param[in] ref  reference to a JSON value
  @return iteration proxy object wrapping @a ref with an interface to use in
          range-based for loops

  @liveexample{The following code shows how the wrapper is
  used,iterator_wrapper}

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes in the JSON value.

  @complexity Constant.

  @note The name of this function is not yet final and may change in the
  future.

  @deprecated This stream operator is deprecated and will be removed in
              future 4.0.0 of the library. Please use @ref items() instead;
              that is, replace `json::iterator_wrapper(j)` with `j.items()`.
  */
  JSON_DEPRECATED
  static iteration_proxy<iterator> iterator_wrapper(reference ref) noexcept {
    return ref.items();
  }

  /*!
  @copydoc iterator_wrapper(reference)
  */
  JSON_DEPRECATED
  static iteration_proxy<const_iterator>
  iterator_wrapper(const_reference ref) noexcept {
    return ref.items();
  }

  /*!
  @brief helper to access iterator member functions in range-based for

  This function allows to access @ref iterator::key() and @ref
  iterator::value() during range-based for loops. In these loops, a
  reference to the JSON values is returned, so there is no access to the
  underlying iterator.

  For loop without `items()` function:

  @code{cpp}
  for (auto it = j_object.begin(); it != j_object.end(); ++it)
  {
      std::cout << "key: " << it.key() << ", value:" << it.value() << '\n';
  }
  @endcode

  Range-based for loop without `items()` function:

  @code{cpp}
  for (auto it : j_object)
  {
      // "it" is of type json::reference and has no key() member
      std::cout << "value: " << it << '\n';
  }
  @endcode

  Range-based for loop with `items()` function:

  @code{cpp}
  for (auto it : j_object.items())
  {
      std::cout << "key: " << it.key() << ", value:" << it.value() << '\n';
  }
  @endcode

  @note When iterating over an array, `key()` will return the index of the
        element as string (see example). For primitive types (e.g., numbers),
        `key()` returns an empty string.

  @return iteration proxy object wrapping @a ref with an interface to use in
          range-based for loops

  @liveexample{The following code shows how the function is used.,items}

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes in the JSON value.

  @complexity Constant.

  @since version 3.x.x.
  */
  iteration_proxy<iterator> items() noexcept {
    return iteration_proxy<iterator>(*this);
  }

  /*!
  @copydoc items()
  */
  iteration_proxy<const_iterator> items() const noexcept {
    return iteration_proxy<const_iterator>(*this);
  }

  /// @}

  //////////////
  // capacity //
  //////////////

  /// @name capacity
  /// @{

  /*!
  @brief checks whether the container is empty.

  Checks if a JSON value has no elements (i.e. whether its @ref size is `0`).

  @return The return value depends on the different types and is
          defined as follows:
          Value type  | return value
          ----------- | -------------
          null        | `true`
          boolean     | `false`
          string      | `false`
          number      | `false`
          object      | result of function `object_t::empty()`
          array       | result of function `array_t::empty()`

  @liveexample{The following code uses `empty()` to check if a JSON
  object contains any elements.,empty}

  @complexity Constant, as long as @ref array_t and @ref object_t satisfy
  the Container concept; that is, their `empty()` functions have constant
  complexity.

  @iterators No changes.

  @exceptionsafety No-throw guarantee: this function never throws exceptions.

  @note This function does not return whether a string stored as JSON value
  is empty - it returns whether the JSON container itself is empty which is
  false in the case of a string.

  @requirement This function helps `basic_json` satisfying the
  [Container](http://en.cppreference.com/w/cpp/concept/Container)
  requirements:
  - The complexity is constant.
  - Has the semantics of `begin() == end()`.

  @sa @ref size() -- returns the number of elements

  @since version 1.0.0
  */
  bool empty() const noexcept {
    switch (m_type) {
    case value_t::null: {
      // null values are empty
      return true;
    }

    case value_t::array: {
      // delegate call to array_t::empty()
      return m_value.array->empty();
    }

    case value_t::object: {
      // delegate call to object_t::empty()
      return m_value.object->empty();
    }

    default: {
      // all other types are nonempty
      return false;
    }
    }
  }

  /*!
  @brief returns the number of elements

  Returns the number of elements in a JSON value.

  @return The return value depends on the different types and is
          defined as follows:
          Value type  | return value
          ----------- | -------------
          null        | `0`
          boolean     | `1`
          string      | `1`
          number      | `1`
          object      | result of function object_t::size()
          array       | result of function array_t::size()

  @liveexample{The following code calls `size()` on the different value
  types.,size}

  @complexity Constant, as long as @ref array_t and @ref object_t satisfy
  the Container concept; that is, their size() functions have constant
  complexity.

  @iterators No changes.

  @exceptionsafety No-throw guarantee: this function never throws exceptions.

  @note This function does not return the length of a string stored as JSON
  value - it returns the number of elements in the JSON value which is 1 in
  the case of a string.

  @requirement This function helps `basic_json` satisfying the
  [Container](http://en.cppreference.com/w/cpp/concept/Container)
  requirements:
  - The complexity is constant.
  - Has the semantics of `std::distance(begin(), end())`.

  @sa @ref empty() -- checks whether the container is empty
  @sa @ref max_size() -- returns the maximal number of elements

  @since version 1.0.0
  */
  size_type size() const noexcept {
    switch (m_type) {
    case value_t::null: {
      // null values are empty
      return 0;
    }

    case value_t::array: {
      // delegate call to array_t::size()
      return m_value.array->size();
    }

    case value_t::object: {
      // delegate call to object_t::size()
      return m_value.object->size();
    }

    default: {
      // all other types have size 1
      return 1;
    }
    }
  }

  /*!
  @brief returns the maximum possible number of elements

  Returns the maximum number of elements a JSON value is able to hold due to
  system or library implementation limitations, i.e. `std::distance(begin(),
  end())` for the JSON value.

  @return The return value depends on the different types and is
          defined as follows:
          Value type  | return value
          ----------- | -------------
          null        | `0` (same as `size()`)
          boolean     | `1` (same as `size()`)
          string      | `1` (same as `size()`)
          number      | `1` (same as `size()`)
          object      | result of function `object_t::max_size()`
          array       | result of function `array_t::max_size()`

  @liveexample{The following code calls `max_size()` on the different value
  types. Note the output is implementation specific.,max_size}

  @complexity Constant, as long as @ref array_t and @ref object_t satisfy
  the Container concept; that is, their `max_size()` functions have constant
  complexity.

  @iterators No changes.

  @exceptionsafety No-throw guarantee: this function never throws exceptions.

  @requirement This function helps `basic_json` satisfying the
  [Container](http://en.cppreference.com/w/cpp/concept/Container)
  requirements:
  - The complexity is constant.
  - Has the semantics of returning `b.size()` where `b` is the largest
    possible JSON value.

  @sa @ref size() -- returns the number of elements

  @since version 1.0.0
  */
  size_type max_size() const noexcept {
    switch (m_type) {
    case value_t::array: {
      // delegate call to array_t::max_size()
      return m_value.array->max_size();
    }

    case value_t::object: {
      // delegate call to object_t::max_size()
      return m_value.object->max_size();
    }

    default: {
      // all other types have max_size() == size()
      return size();
    }
    }
  }

  /// @}

  ///////////////
  // modifiers //
  ///////////////

  /// @name modifiers
  /// @{

  /*!
  @brief clears the contents

  Clears the content of a JSON value and resets it to the default value as
  if @ref basic_json(value_t) would have been called with the current value
  type from @ref type():

  Value type  | initial value
  ----------- | -------------
  null        | `null`
  boolean     | `false`
  string      | `""`
  number      | `0`
  object      | `{}`
  array       | `[]`

  @post Has the same effect as calling
  @code {.cpp}
  *this = basic_json(type());
  @endcode

  @liveexample{The example below shows the effect of `clear()` to different
  JSON types.,clear}

  @complexity Linear in the size of the JSON value.

  @iterators All iterators, pointers and references related to this container
             are invalidated.

  @exceptionsafety No-throw guarantee: this function never throws exceptions.

  @sa @ref basic_json(value_t) -- constructor that creates an object with the
      same value than calling `clear()`

  @since version 1.0.0
  */
  void clear() noexcept {
    switch (m_type) {
    case value_t::number_integer: {
      m_value.number_integer = 0;
      break;
    }

    case value_t::number_unsigned: {
      m_value.number_unsigned = 0;
      break;
    }

    case value_t::number_float: {
      m_value.number_float = 0.0;
      break;
    }

    case value_t::boolean: {
      m_value.boolean = false;
      break;
    }

    case value_t::string: {
      m_value.string->clear();
      break;
    }

    case value_t::array: {
      m_value.array->clear();
      break;
    }

    case value_t::object: {
      m_value.object->clear();
      break;
    }

    default: break;
    }
  }

  /*!
  @brief add an object to an array

  Appends the given element @a val to the end of the JSON value. If the
  function is called on a JSON null value, an empty array is created before
  appending @a val.

  @param[in] val the value to add to the JSON array

  @throw type_error.308 when called on a type other than JSON array or
  null; example: `"cannot use push_back() with number"`

  @complexity Amortized constant.

  @liveexample{The example shows how `push_back()` and `+=` can be used to
  add elements to a JSON array. Note how the `null` value was silently
  converted to a JSON array.,push_back}

  @since version 1.0.0
  */
  void push_back(basic_json&& val) {
    // push_back only works for null objects or arrays
    if (JSON_UNLIKELY(not(is_null() || is_array()))) {
      JSON_THROW(type_error::create(308, "can!use push_back() with " +
                                             std::string(type_name())));
    }

    // transform null object into an array
    if (is_null()) {
      m_type = value_t::array;
      m_value = value_t::array;
      assert_invariant();
    }

    // add element to array (move semantics)
    m_value.array->push_back(std::move(val));
    // invalidate object
    val.m_type = value_t::null;
  }

  /*!
  @brief add an object to an array
  @copydoc push_back(basic_json&&)
  */
  reference operator+=(basic_json&& val) {
    push_back(std::move(val));
    return *this;
  }

  /*!
  @brief add an object to an array
  @copydoc push_back(basic_json&&)
  */
  void push_back(const basic_json& val) {
    // push_back only works for null objects or arrays
    if (JSON_UNLIKELY(not(is_null() || is_array()))) {
      JSON_THROW(type_error::create(308, "can!use push_back() with " +
                                             std::string(type_name())));
    }

    // transform null object into an array
    if (is_null()) {
      m_type = value_t::array;
      m_value = value_t::array;
      assert_invariant();
    }

    // add element to array
    m_value.array->push_back(val);
  }

  /*!
  @brief add an object to an array
  @copydoc push_back(basic_json&&)
  */
  reference operator+=(const basic_json& val) {
    push_back(val);
    return *this;
  }

  /*!
  @brief add an object to an object

  Inserts the given element @a val to the JSON object. If the function is
  called on a JSON null value, an empty object is created before inserting
  @a val.

  @param[in] val the value to add to the JSON object

  @throw type_error.308 when called on a type other than JSON object or
  null; example: `"cannot use push_back() with number"`

  @complexity Logarithmic in the size of the container, O(log(`size()`)).

  @liveexample{The example shows how `push_back()` and `+=` can be used to
  add elements to a JSON object. Note how the `null` value was silently
  converted to a JSON object.,push_back__object_t__value}

  @since version 1.0.0
  */
  void push_back(const typename object_t::value_type& val) {
    // push_back only works for null objects or objects
    if (JSON_UNLIKELY(not(is_null() || is_object()))) {
      JSON_THROW(type_error::create(308, "can!use push_back() with " +
                                             std::string(type_name())));
    }

    // transform null object into an object
    if (is_null()) {
      m_type = value_t::object;
      m_value = value_t::object;
      assert_invariant();
    }

    // add element to array
    m_value.object->insert(val);
  }

  /*!
  @brief add an object to an object
  @copydoc push_back(const typename object_t::value_type&)
  */
  reference operator+=(const typename object_t::value_type& val) {
    push_back(val);
    return *this;
  }

  /*!
  @brief add an object to an object

  This function allows to use `push_back` with an initializer list. In case

  1. the current value is an object,
  2. the initializer list @a init contains only two elements, and
  3. the first element of @a init is a string,

  @a init is converted into an object element and added using
  @ref push_back(const typename object_t::value_type&). Otherwise, @a init
  is converted to a JSON value and added using @ref push_back(basic_json&&).

  @param[in] init  an initializer list

  @complexity Linear in the size of the initializer list @a init.

  @note This function is required to resolve an ambiguous overload error,
        because pairs like `{"key", "value"}` can be both interpreted as
        `object_t::value_type` or `std::initializer_list<basic_json>`, see
        https://github.com/my_json/json/issues/235 for more information.

  @liveexample{The example shows how initializer lists are treated as
  objects when possible.,push_back__initializer_list}
  */
  void push_back(initializer_list_t init) {
    if (is_object() && init.size() == 2 && (*init.begin())->is_string()) {
      basic_json&& key = init.begin()->moved_or_copied();
      push_back(
          typename object_t::value_type(std::move(key.get_ref<string_t&>()),
                                        (init.begin() + 1)->moved_or_copied()));
    } else {
      push_back(basic_json(init));
    }
  }

  /*!
  @brief add an object to an object
  @copydoc push_back(initializer_list_t)
  */
  reference operator+=(initializer_list_t init) {
    push_back(init);
    return *this;
  }

  /*!
  @brief add an object to an array

  Creates a JSON value from the passed parameters @a args to the end of the
  JSON value. If the function is called on a JSON null value, an empty array
  is created before appending the value created from @a args.

  @param[in] args arguments to forward to a constructor of @ref basic_json
  @tparam Args compatible types to create a @ref basic_json object

  @throw type_error.311 when called on a type other than JSON array or
  null; example: `"cannot use emplace_back() with number"`

  @complexity Amortized constant.

  @liveexample{The example shows how `push_back()` can be used to add
  elements to a JSON array. Note how the `null` value was silently converted
  to a JSON array.,emplace_back}

  @since version 2.0.8
  */
  template <class... Args> void emplace_back(Args&&... args) {
    // emplace_back only works for null objects or arrays
    if (JSON_UNLIKELY(not(is_null() || is_array()))) {
      JSON_THROW(type_error::create(311, "can!use emplace_back() with " +
                                             std::string(type_name())));
    }

    // transform null object into an array
    if (is_null()) {
      m_type = value_t::array;
      m_value = value_t::array;
      assert_invariant();
    }

    // add element to array (perfect forwarding)
    m_value.array->emplace_back(std::forward<Args>(args)...);
  }

  /*!
  @brief add an object to an object if key does not exist

  Inserts a new element into a JSON object constructed in-place with the
  given @a args if there is no element with the key in the container. If the
  function is called on a JSON null value, an empty object is created before
  appending the value created from @a args.

  @param[in] args arguments to forward to a constructor of @ref basic_json
  @tparam Args compatible types to create a @ref basic_json object

  @return a pair consisting of an iterator to the inserted element, or the
          already-existing element if no insertion happened, and a bool
          denoting whether the insertion took place.

  @throw type_error.311 when called on a type other than JSON object or
  null; example: `"cannot use emplace() with number"`

  @complexity Logarithmic in the size of the container, O(log(`size()`)).

  @liveexample{The example shows how `emplace()` can be used to add elements
  to a JSON object. Note how the `null` value was silently converted to a
  JSON object. Further note how no value is added if there was already one
  value stored with the same key.,emplace}

  @since version 2.0.8
  */
  template <class... Args> std::pair<iterator, bool> emplace(Args&&... args) {
    // emplace only works for null objects or arrays
    if (JSON_UNLIKELY(not(is_null() || is_object()))) {
      JSON_THROW(type_error::create(311, "can!use emplace() with " +
                                             std::string(type_name())));
    }

    // transform null object into an object
    if (is_null()) {
      m_type = value_t::object;
      m_value = value_t::object;
      assert_invariant();
    }

    // add element to array (perfect forwarding)
    auto res = m_value.object->emplace(std::forward<Args>(args)...);
    // create result iterator and set iterator to the result of emplace
    auto it = begin();
    it.m_it.object_iterator = res.first;

    // return pair of iterator and boolean
    return {it, res.second};
  }

  /*!
  @brief inserts element

  Inserts element @a val before iterator @a pos.

  @param[in] pos iterator before which the content will be inserted; may be
  the end() iterator
  @param[in] val element to insert
  @return iterator pointing to the inserted @a val.

  @throw type_error.309 if called on JSON values other than arrays;
  example: `"cannot use insert() with string"`
  @throw invalid_iterator.202 if @a pos is not an iterator of *this;
  example: `"iterator does not fit current value"`

  @complexity Constant plus linear in the distance between @a pos and end of
  the container.

  @liveexample{The example shows how `insert()` is used.,insert}

  @since version 1.0.0
  */
  iterator insert(const_iterator pos, const basic_json& val) {
    // insert only works for arrays
    if (JSON_LIKELY(is_array())) {
      // check if iterator pos fits to this JSON value
      if (JSON_UNLIKELY(pos.m_object != this)) {
        JSON_THROW(
            invalid_iterator::create(202, "iterator does !fit current value"));
      }

      // insert to array and return iterator
      iterator result(this);
      result.m_it.array_iterator =
          m_value.array->insert(pos.m_it.array_iterator, val);
      return result;
    }

    JSON_THROW(type_error::create(309, "can!use insert() with " +
                                           std::string(type_name())));
  }

  /*!
  @brief inserts element
  @copydoc insert(const_iterator, const basic_json&)
  */
  iterator insert(const_iterator pos, basic_json&& val) {
    return insert(pos, val);
  }

  /*!
  @brief inserts elements

  Inserts @a cnt copies of @a val before iterator @a pos.

  @param[in] pos iterator before which the content will be inserted; may be
  the end() iterator
  @param[in] cnt number of copies of @a val to insert
  @param[in] val element to insert
  @return iterator pointing to the first element inserted, or @a pos if
  `cnt==0`

  @throw type_error.309 if called on JSON values other than arrays; example:
  `"cannot use insert() with string"`
  @throw invalid_iterator.202 if @a pos is not an iterator of *this;
  example: `"iterator does not fit current value"`

  @complexity Linear in @a cnt plus linear in the distance between @a pos
  and end of the container.

  @liveexample{The example shows how `insert()` is used.,insert__count}

  @since version 1.0.0
  */
  iterator insert(const_iterator pos, size_type cnt, const basic_json& val) {
    // insert only works for arrays
    if (JSON_LIKELY(is_array())) {
      // check if iterator pos fits to this JSON value
      if (JSON_UNLIKELY(pos.m_object != this)) {
        JSON_THROW(
            invalid_iterator::create(202, "iterator does !fit current value"));
      }

      // insert to array and return iterator
      iterator result(this);
      result.m_it.array_iterator =
          m_value.array->insert(pos.m_it.array_iterator, cnt, val);
      return result;
    }

    JSON_THROW(type_error::create(309, "can!use insert() with " +
                                           std::string(type_name())));
  }

  /*!
  @brief inserts elements

  Inserts elements from range `[first, last)` before iterator @a pos.

  @param[in] pos iterator before which the content will be inserted; may be
  the end() iterator
  @param[in] first begin of the range of elements to insert
  @param[in] last end of the range of elements to insert

  @throw type_error.309 if called on JSON values other than arrays; example:
  `"cannot use insert() with string"`
  @throw invalid_iterator.202 if @a pos is not an iterator of *this;
  example: `"iterator does not fit current value"`
  @throw invalid_iterator.210 if @a first and @a last do not belong to the
  same JSON value; example: `"iterators do not fit"`
  @throw invalid_iterator.211 if @a first or @a last are iterators into
  container for which insert is called; example: `"passed iterators may not
  belong to container"`

  @return iterator pointing to the first element inserted, or @a pos if
  `first==last`

  @complexity Linear in `std::distance(first, last)` plus linear in the
  distance between @a pos and end of the container.

  @liveexample{The example shows how `insert()` is used.,insert__range}

  @since version 1.0.0
  */
  iterator insert(const_iterator pos, const_iterator first,
                  const_iterator last) {
    // insert only works for arrays
    if (JSON_UNLIKELY(!is_array())) {
      JSON_THROW(type_error::create(309, "can!use insert() with " +
                                             std::string(type_name())));
    }

    // check if iterator pos fits to this JSON value
    if (JSON_UNLIKELY(pos.m_object != this)) {
      JSON_THROW(
          invalid_iterator::create(202, "iterator does !fit current value"));
    }

    // check if range iterators belong to the same JSON object
    if (JSON_UNLIKELY(first.m_object != last.m_object)) {
      JSON_THROW(invalid_iterator::create(210, "iterators do !fit"));
    }

    if (JSON_UNLIKELY(first.m_object == this)) {
      JSON_THROW(invalid_iterator::create(
          211, "passed iterators may !belong to container"));
    }

    // insert to array and return iterator
    iterator result(this);
    result.m_it.array_iterator = m_value.array->insert(
        pos.m_it.array_iterator, first.m_it.array_iterator,
        last.m_it.array_iterator);
    return result;
  }

  /*!
  @brief inserts elements

  Inserts elements from initializer list @a ilist before iterator @a pos.

  @param[in] pos iterator before which the content will be inserted; may be
  the end() iterator
  @param[in] ilist initializer list to insert the values from

  @throw type_error.309 if called on JSON values other than arrays; example:
  `"cannot use insert() with string"`
  @throw invalid_iterator.202 if @a pos is not an iterator of *this;
  example: `"iterator does not fit current value"`

  @return iterator pointing to the first element inserted, or @a pos if
  `ilist` is empty

  @complexity Linear in `ilist.size()` plus linear in the distance between
  @a pos and end of the container.

  @liveexample{The example shows how `insert()` is used.,insert__ilist}

  @since version 1.0.0
  */
  iterator insert(const_iterator pos, initializer_list_t ilist) {
    // insert only works for arrays
    if (JSON_UNLIKELY(!is_array())) {
      JSON_THROW(type_error::create(309, "can!use insert() with " +
                                             std::string(type_name())));
    }

    // check if iterator pos fits to this JSON value
    if (JSON_UNLIKELY(pos.m_object != this)) {
      JSON_THROW(
          invalid_iterator::create(202, "iterator does !fit current value"));
    }

    // insert to array and return iterator
    iterator result(this);
    result.m_it.array_iterator = m_value.array->insert(
        pos.m_it.array_iterator, ilist.begin(), ilist.end());
    return result;
  }

  /*!
  @brief inserts elements

  Inserts elements from range `[first, last)`.

  @param[in] first begin of the range of elements to insert
  @param[in] last end of the range of elements to insert

  @throw type_error.309 if called on JSON values other than objects; example:
  `"cannot use insert() with string"`
  @throw invalid_iterator.202 if iterator @a first or @a last does does not
  point to an object; example: `"iterators first and last must point to
  objects"`
  @throw invalid_iterator.210 if @a first and @a last do not belong to the
  same JSON value; example: `"iterators do not fit"`

  @complexity Logarithmic: `O(N*log(size() + N))`, where `N` is the number
  of elements to insert.

  @liveexample{The example shows how `insert()` is used.,insert__range_object}

  @since version 3.0.0
  */
  void insert(const_iterator first, const_iterator last) {
    // insert only works for objects
    if (JSON_UNLIKELY(!is_object())) {
      JSON_THROW(type_error::create(309, "can!use insert() with " +
                                             std::string(type_name())));
    }

    // check if range iterators belong to the same JSON object
    if (JSON_UNLIKELY(first.m_object != last.m_object)) {
      JSON_THROW(invalid_iterator::create(210, "iterators do !fit"));
    }

    // passed iterators must belong to objects
    if (JSON_UNLIKELY(!first.m_object->is_object())) {
      JSON_THROW(invalid_iterator::create(
          202, "iterators first && last must point to objects"));
    }

    m_value.object->insert(first.m_it.object_iterator,
                           last.m_it.object_iterator);
  }

  /*!
  @brief updates a JSON object from another object, overwriting existing keys

  Inserts all values from JSON object @a j and overwrites existing keys.

  @param[in] j  JSON object to read values from

  @throw type_error.312 if called on JSON values other than objects; example:
  `"cannot use update() with string"`

  @complexity O(N*log(size() + N)), where N is the number of elements to
              insert.

  @liveexample{The example shows how `update()` is used.,update}

  @sa https://docs.python.org/3.6/library/stdtypes.html#dict.update

  @since version 3.0.0
  */
  void update(const_reference j) {
    // implicitly convert null value to an empty object
    if (is_null()) {
      m_type = value_t::object;
      m_value.object = create<object_t>();
      assert_invariant();
    }

    if (JSON_UNLIKELY(!is_object())) {
      JSON_THROW(type_error::create(312, "can!use update() with " +
                                             std::string(type_name())));
    }
    if (JSON_UNLIKELY(!j.is_object())) {
      JSON_THROW(type_error::create(312, "can!use update() with " +
                                             std::string(j.type_name())));
    }

    for (auto it = j.cbegin(); it != j.cend(); ++it) {
      m_value.object->operator[](it.key()) = it.value();
    }
  }

  /*!
  @brief updates a JSON object from another object, overwriting existing keys

  Inserts all values from from range `[first, last)` and overwrites existing
  keys.

  @param[in] first begin of the range of elements to insert
  @param[in] last end of the range of elements to insert

  @throw type_error.312 if called on JSON values other than objects; example:
  `"cannot use update() with string"`
  @throw invalid_iterator.202 if iterator @a first or @a last does does not
  point to an object; example: `"iterators first and last must point to
  objects"`
  @throw invalid_iterator.210 if @a first and @a last do not belong to the
  same JSON value; example: `"iterators do not fit"`

  @complexity O(N*log(size() + N)), where N is the number of elements to
              insert.

  @liveexample{The example shows how `update()` is used__range.,update}

  @sa https://docs.python.org/3.6/library/stdtypes.html#dict.update

  @since version 3.0.0
  */
  void update(const_iterator first, const_iterator last) {
    // implicitly convert null value to an empty object
    if (is_null()) {
      m_type = value_t::object;
      m_value.object = create<object_t>();
      assert_invariant();
    }

    if (JSON_UNLIKELY(!is_object())) {
      JSON_THROW(type_error::create(312, "can!use update() with " +
                                             std::string(type_name())));
    }

    // check if range iterators belong to the same JSON object
    if (JSON_UNLIKELY(first.m_object != last.m_object)) {
      JSON_THROW(invalid_iterator::create(210, "iterators do !fit"));
    }

    // passed iterators must belong to objects
    if (JSON_UNLIKELY(!first.m_object->is_object() ||
                      !last.m_object->is_object())) {
      JSON_THROW(invalid_iterator::create(
          202, "iterators first && last must point to objects"));
    }

    for (auto it = first; it != last; ++it) {
      m_value.object->operator[](it.key()) = it.value();
    }
  }

  /*!
  @brief exchanges the values

  Exchanges the contents of the JSON value with those of @a other. Does not
  invoke any move, copy, or swap operations on individual elements. All
  iterators and references remain valid. The past-the-end iterator is
  invalidated.

  @param[in,out] other JSON value to exchange the contents with

  @complexity Constant.

  @liveexample{The example below shows how JSON values can be swapped with
  `swap()`.,swap__reference}

  @since version 1.0.0
  */
  void swap(reference other) noexcept(
      std::is_nothrow_move_constructible<value_t>::value &&
          std::is_nothrow_move_assignable<value_t>::value &&
              std::is_nothrow_move_constructible<json_value>::value &&
                  std::is_nothrow_move_assignable<json_value>::value) {
    std::swap(m_type, other.m_type);
    std::swap(m_value, other.m_value);
    assert_invariant();
  }

  /*!
  @brief exchanges the values

  Exchanges the contents of a JSON array with those of @a other. Does not
  invoke any move, copy, or swap operations on individual elements. All
  iterators and references remain valid. The past-the-end iterator is
  invalidated.

  @param[in,out] other array to exchange the contents with

  @throw type_error.310 when JSON value is not an array; example: `"cannot
  use swap() with string"`

  @complexity Constant.

  @liveexample{The example below shows how arrays can be swapped with
  `swap()`.,swap__array_t}

  @since version 1.0.0
  */
  void swap(array_t& other) {
    // swap only works for arrays
    if (JSON_LIKELY(is_array())) {
      std::swap(*(m_value.array), other);
    } else {
      JSON_THROW(type_error::create(310, "can!use swap() with " +
                                             std::string(type_name())));
    }
  }

  /*!
  @brief exchanges the values

  Exchanges the contents of a JSON object with those of @a other. Does not
  invoke any move, copy, or swap operations on individual elements. All
  iterators and references remain valid. The past-the-end iterator is
  invalidated.

  @param[in,out] other object to exchange the contents with

  @throw type_error.310 when JSON value is not an object; example:
  `"cannot use swap() with string"`

  @complexity Constant.

  @liveexample{The example below shows how objects can be swapped with
  `swap()`.,swap__object_t}

  @since version 1.0.0
  */
  void swap(object_t& other) {
    // swap only works for objects
    if (JSON_LIKELY(is_object())) {
      std::swap(*(m_value.object), other);
    } else {
      JSON_THROW(type_error::create(310, "can!use swap() with " +
                                             std::string(type_name())));
    }
  }

  /*!
  @brief exchanges the values

  Exchanges the contents of a JSON string with those of @a other. Does not
  invoke any move, copy, or swap operations on individual elements. All
  iterators and references remain valid. The past-the-end iterator is
  invalidated.

  @param[in,out] other string to exchange the contents with

  @throw type_error.310 when JSON value is not a string; example: `"cannot
  use swap() with boolean"`

  @complexity Constant.

  @liveexample{The example below shows how strings can be swapped with
  `swap()`.,swap__string_t}

  @since version 1.0.0
  */
  void swap(string_t& other) {
    // swap only works for strings
    if (JSON_LIKELY(is_string())) {
      std::swap(*(m_value.string), other);
    } else {
      JSON_THROW(type_error::create(310, "can!use swap() with " +
                                             std::string(type_name())));
    }
  }

  /// @}

  //////////////////////////////////////////
  // lexicographical comparison operators //
  //////////////////////////////////////////

  /// @name lexicographical comparison operators
  /// @{

  /*!
  @brief comparison: equal

  Compares two JSON values for equality according to the following rules:
  - Two JSON values are equal if (1) they are from the same type and (2)
    their stored values are the same according to their respective
    `operator==`.
  - Integer and floating-point numbers are automatically converted before
    comparison. Note than two NaN values are always treated as unequal.
  - Two JSON null values are equal.

  @note Floating-point inside JSON values numbers are compared with
  `json::number_float_t::operator==` which is `double::operator==` by
  default. To compare floating-point while respecting an epsilon, an alternative
  [comparison
  function](https://github.com/mariokonrad/marnav/blob/master/src/marnav/math/floatingpoint.hpp#L34-#L39)
  could be used, for instance
  @code {.cpp}
  template<typename T, typename = typename
  std::enable_if<std::is_floating_point<T>::value, T>::type> inline bool
  is_same(T a, T b, T epsilon = std::numeric_limits<T>::epsilon()) noexcept
  {
      return std::abs(a - b) <= epsilon;
  }
  @endcode

  @note NaN values never compare equal to themselves or to other NaN values.

  @param[in] lhs  first JSON value to consider
  @param[in] rhs  second JSON value to consider
  @return whether the values @a lhs and @a rhs are equal

  @exceptionsafety No-throw guarantee: this function never throws exceptions.

  @complexity Linear.

  @liveexample{The example demonstrates comparing several JSON
  types.,operator__equal}

  @since version 1.0.0
  */
  friend bool operator==(const_reference lhs, const_reference rhs) noexcept {
    const auto lhs_type = lhs.type();
    const auto rhs_type = rhs.type();

    if (lhs_type == rhs_type) {
      switch (lhs_type) {
      case value_t::array: return (*lhs.m_value.array == *rhs.m_value.array);

      case value_t::object: return (*lhs.m_value.object == *rhs.m_value.object);

      case value_t::null: return true;

      case value_t::string: return (*lhs.m_value.string == *rhs.m_value.string);

      case value_t::boolean:
        return (lhs.m_value.boolean == rhs.m_value.boolean);

      case value_t::number_integer:
        return (lhs.m_value.number_integer == rhs.m_value.number_integer);

      case value_t::number_unsigned:
        return (lhs.m_value.number_unsigned == rhs.m_value.number_unsigned);

      case value_t::number_float:
        return (lhs.m_value.number_float == rhs.m_value.number_float);

      default: return false;
      }
    } else if (lhs_type == value_t::number_integer &&
               rhs_type == value_t::number_float) {
      return (static_cast<number_float_t>(lhs.m_value.number_integer) ==
              rhs.m_value.number_float);
    } else if (lhs_type == value_t::number_float &&
               rhs_type == value_t::number_integer) {
      return (lhs.m_value.number_float ==
              static_cast<number_float_t>(rhs.m_value.number_integer));
    } else if (lhs_type == value_t::number_unsigned &&
               rhs_type == value_t::number_float) {
      return (static_cast<number_float_t>(lhs.m_value.number_unsigned) ==
              rhs.m_value.number_float);
    } else if (lhs_type == value_t::number_float &&
               rhs_type == value_t::number_unsigned) {
      return (lhs.m_value.number_float ==
              static_cast<number_float_t>(rhs.m_value.number_unsigned));
    } else if (lhs_type == value_t::number_unsigned &&
               rhs_type == value_t::number_integer) {
      return (static_cast<number_integer_t>(lhs.m_value.number_unsigned) ==
              rhs.m_value.number_integer);
    } else if (lhs_type == value_t::number_integer &&
               rhs_type == value_t::number_unsigned) {
      return (lhs.m_value.number_integer ==
              static_cast<number_integer_t>(rhs.m_value.number_unsigned));
    }

    return false;
  }

  /*!
  @brief comparison: equal
  @copydoc operator==(const_reference, const_reference)
  */
  template <
      typename ScalarType,
      typename std::enable_if<std::is_scalar<ScalarType>::value, int>::type = 0>
  friend bool operator==(const_reference lhs, const ScalarType rhs) noexcept {
    return (lhs == basic_json(rhs));
  }

  /*!
  @brief comparison: equal
  @copydoc operator==(const_reference, const_reference)
  */
  template <
      typename ScalarType,
      typename std::enable_if<std::is_scalar<ScalarType>::value, int>::type = 0>
  friend bool operator==(const ScalarType lhs, const_reference rhs) noexcept {
    return (basic_json(lhs) == rhs);
  }

  /*!
  @brief comparison: not equal

  Compares two JSON values for inequality by calculating `not (lhs == rhs)`.

  @param[in] lhs  first JSON value to consider
  @param[in] rhs  second JSON value to consider
  @return whether the values @a lhs and @a rhs are not equal

  @complexity Linear.

  @exceptionsafety No-throw guarantee: this function never throws exceptions.

  @liveexample{The example demonstrates comparing several JSON
  types.,operator__notequal}

  @since version 1.0.0
  */
  friend bool operator!=(const_reference lhs, const_reference rhs) noexcept {
    return !(lhs == rhs);
  }

  /*!
  @brief comparison: not equal
  @copydoc operator!=(const_reference, const_reference)
  */
  template <
      typename ScalarType,
      typename std::enable_if<std::is_scalar<ScalarType>::value, int>::type = 0>
  friend bool operator!=(const_reference lhs, const ScalarType rhs) noexcept {
    return (lhs != basic_json(rhs));
  }

  /*!
  @brief comparison: not equal
  @copydoc operator!=(const_reference, const_reference)
  */
  template <
      typename ScalarType,
      typename std::enable_if<std::is_scalar<ScalarType>::value, int>::type = 0>
  friend bool operator!=(const ScalarType lhs, const_reference rhs) noexcept {
    return (basic_json(lhs) != rhs);
  }

  /*!
  @brief comparison: less than

  Compares whether one JSON value @a lhs is less than another JSON value @a
  rhs according to the following rules:
  - If @a lhs and @a rhs have the same type, the values are compared using
    the default `<` operator.
  - Integer and floating-point numbers are automatically converted before
    comparison
  - In case @a lhs and @a rhs have different types, the values are ignored
    and the order of the types is considered, see
    @ref operator<(const value_t, const value_t).

  @param[in] lhs  first JSON value to consider
  @param[in] rhs  second JSON value to consider
  @return whether @a lhs is less than @a rhs

  @complexity Linear.

  @exceptionsafety No-throw guarantee: this function never throws exceptions.

  @liveexample{The example demonstrates comparing several JSON
  types.,operator__less}

  @since version 1.0.0
  */
  friend bool operator<(const_reference lhs, const_reference rhs) noexcept {
    const auto lhs_type = lhs.type();
    const auto rhs_type = rhs.type();

    if (lhs_type == rhs_type) {
      switch (lhs_type) {
      case value_t::array: return (*lhs.m_value.array) < (*rhs.m_value.array);

      case value_t::object: return *lhs.m_value.object < *rhs.m_value.object;

      case value_t::null: return false;

      case value_t::string: return *lhs.m_value.string < *rhs.m_value.string;

      case value_t::boolean: return lhs.m_value.boolean < rhs.m_value.boolean;

      case value_t::number_integer:
        return lhs.m_value.number_integer < rhs.m_value.number_integer;

      case value_t::number_unsigned:
        return lhs.m_value.number_unsigned < rhs.m_value.number_unsigned;

      case value_t::number_float:
        return lhs.m_value.number_float < rhs.m_value.number_float;

      default: return false;
      }
    } else if (lhs_type == value_t::number_integer &&
               rhs_type == value_t::number_float) {
      return static_cast<number_float_t>(lhs.m_value.number_integer) <
             rhs.m_value.number_float;
    } else if (lhs_type == value_t::number_float &&
               rhs_type == value_t::number_integer) {
      return lhs.m_value.number_float <
             static_cast<number_float_t>(rhs.m_value.number_integer);
    } else if (lhs_type == value_t::number_unsigned &&
               rhs_type == value_t::number_float) {
      return static_cast<number_float_t>(lhs.m_value.number_unsigned) <
             rhs.m_value.number_float;
    } else if (lhs_type == value_t::number_float &&
               rhs_type == value_t::number_unsigned) {
      return lhs.m_value.number_float <
             static_cast<number_float_t>(rhs.m_value.number_unsigned);
    } else if (lhs_type == value_t::number_integer &&
               rhs_type == value_t::number_unsigned) {
      return lhs.m_value.number_integer <
             static_cast<number_integer_t>(rhs.m_value.number_unsigned);
    } else if (lhs_type == value_t::number_unsigned &&
               rhs_type == value_t::number_integer) {
      return static_cast<number_integer_t>(lhs.m_value.number_unsigned) <
             rhs.m_value.number_integer;
    }

    // We only reach this line if we cannot compare values. In that case,
    // we compare types. Note we have to call the operator explicitly,
    // because MSVC has problems otherwise.
    return operator<(lhs_type, rhs_type);
  }

  /*!
  @brief comparison: less than
  @copydoc operator<(const_reference, const_reference)
  */
  template <
      typename ScalarType,
      typename std::enable_if<std::is_scalar<ScalarType>::value, int>::type = 0>
  friend bool operator<(const_reference lhs, const ScalarType rhs) noexcept {
    return (lhs < basic_json(rhs));
  }

  /*!
  @brief comparison: less than
  @copydoc operator<(const_reference, const_reference)
  */
  template <
      typename ScalarType,
      typename std::enable_if<std::is_scalar<ScalarType>::value, int>::type = 0>
  friend bool operator<(const ScalarType lhs, const_reference rhs) noexcept {
    return (basic_json(lhs) < rhs);
  }

  /*!
  @brief comparison: less than or equal

  Compares whether one JSON value @a lhs is less than or equal to another
  JSON value by calculating `not (rhs < lhs)`.

  @param[in] lhs  first JSON value to consider
  @param[in] rhs  second JSON value to consider
  @return whether @a lhs is less than or equal to @a rhs

  @complexity Linear.

  @exceptionsafety No-throw guarantee: this function never throws exceptions.

  @liveexample{The example demonstrates comparing several JSON
  types.,operator__greater}

  @since version 1.0.0
  */
  friend bool operator<=(const_reference lhs, const_reference rhs) noexcept {
    return !(rhs < lhs);
  }

  /*!
  @brief comparison: less than or equal
  @copydoc operator<=(const_reference, const_reference)
  */
  template <
      typename ScalarType,
      typename std::enable_if<std::is_scalar<ScalarType>::value, int>::type = 0>
  friend bool operator<=(const_reference lhs, const ScalarType rhs) noexcept {
    return (lhs <= basic_json(rhs));
  }

  /*!
  @brief comparison: less than or equal
  @copydoc operator<=(const_reference, const_reference)
  */
  template <
      typename ScalarType,
      typename std::enable_if<std::is_scalar<ScalarType>::value, int>::type = 0>
  friend bool operator<=(const ScalarType lhs, const_reference rhs) noexcept {
    return (basic_json(lhs) <= rhs);
  }

  /*!
  @brief comparison: greater than

  Compares whether one JSON value @a lhs is greater than another
  JSON value by calculating `not (lhs <= rhs)`.

  @param[in] lhs  first JSON value to consider
  @param[in] rhs  second JSON value to consider
  @return whether @a lhs is greater than to @a rhs

  @complexity Linear.

  @exceptionsafety No-throw guarantee: this function never throws exceptions.

  @liveexample{The example demonstrates comparing several JSON
  types.,operator__lessequal}

  @since version 1.0.0
  */
  friend bool operator>(const_reference lhs, const_reference rhs) noexcept {
    return !(lhs <= rhs);
  }

  /*!
  @brief comparison: greater than
  @copydoc operator>(const_reference, const_reference)
  */
  template <
      typename ScalarType,
      typename std::enable_if<std::is_scalar<ScalarType>::value, int>::type = 0>
  friend bool operator>(const_reference lhs, const ScalarType rhs) noexcept {
    return (lhs > basic_json(rhs));
  }

  /*!
  @brief comparison: greater than
  @copydoc operator>(const_reference, const_reference)
  */
  template <
      typename ScalarType,
      typename std::enable_if<std::is_scalar<ScalarType>::value, int>::type = 0>
  friend bool operator>(const ScalarType lhs, const_reference rhs) noexcept {
    return (basic_json(lhs) > rhs);
  }

  /*!
  @brief comparison: greater than or equal

  Compares whether one JSON value @a lhs is greater than or equal to another
  JSON value by calculating `not (lhs < rhs)`.

  @param[in] lhs  first JSON value to consider
  @param[in] rhs  second JSON value to consider
  @return whether @a lhs is greater than or equal to @a rhs

  @complexity Linear.

  @exceptionsafety No-throw guarantee: this function never throws exceptions.

  @liveexample{The example demonstrates comparing several JSON
  types.,operator__greaterequal}

  @since version 1.0.0
  */
  friend bool operator>=(const_reference lhs, const_reference rhs) noexcept {
    return !(lhs < rhs);
  }

  /*!
  @brief comparison: greater than or equal
  @copydoc operator>=(const_reference, const_reference)
  */
  template <
      typename ScalarType,
      typename std::enable_if<std::is_scalar<ScalarType>::value, int>::type = 0>
  friend bool operator>=(const_reference lhs, const ScalarType rhs) noexcept {
    return (lhs >= basic_json(rhs));
  }

  /*!
  @brief comparison: greater than or equal
  @copydoc operator>=(const_reference, const_reference)
  */
  template <
      typename ScalarType,
      typename std::enable_if<std::is_scalar<ScalarType>::value, int>::type = 0>
  friend bool operator>=(const ScalarType lhs, const_reference rhs) noexcept {
    return (basic_json(lhs) >= rhs);
  }

    /// @}

    ///////////////////
    // serialization //
    ///////////////////

    /// @name serialization
    /// @{

    /*!
    @brief serialize to stream

    Serialize the given JSON value @a j to the output stream @a o. The JSON
    value will be serialized using the @ref dump member function.

    - The indentation of the output can be controlled with the member variable
      `width` of the output stream @a o. For instance, using the manipulator
      `std::setw(4)` on @a o sets the indentation level to `4` and the
      serialization result is the same as calling `dump(4)`.

    - The indentation character can be controlled with the member variable
      `fill` of the output stream @a o. For instance, the manipulator
      `std::setfill('\\t')` sets indentation to use a tab character rather than
      the default space character.

    @param[in,out] o  stream to serialize to
    @param[in] j  JSON value to serialize

    @return the stream @a o

    @throw type_error.316 if a string stored inside the JSON value is not
                          UTF-8 encoded

    @complexity Linear.

    @liveexample{The example below shows the serialization with different
    parameters to `width` to adjust the indentation level.,operator_serialize}

    @since version 1.0.0; indentation character added in version 3.0.0
    */
#if USE_IOSTREAMS
  friend std::ostream& operator<<(std::ostream& o, const basic_json& j) {
    // read width member and use it as indentation parameter if nonzero
    const bool pretty_print = (o.width() > 0);
    const auto indentation = (pretty_print ? o.width() : 0);

    // reset width to 0 for subsequent calls to this stream
    o.width(0);

    // do the actual serialization
    serializer s(detail::output_adapter<char>(o), o.fill());
    s.dump(j, pretty_print, false, static_cast<unsigned int>(indentation));
    return o;
  }

  /*!
  @brief serialize to stream
  @deprecated This stream operator is deprecated and will be removed in
              future 4.0.0 of the library. Please use
              @ref operator<<(std::ostream&, const basic_json&)
              instead; that is, replace calls like `j >> o;` with `o << j;`.
  @since version 1.0.0; deprecated since version 3.0.0
  */
  JSON_DEPRECATED
  friend std::ostream& operator>>(const basic_json& j, std::ostream& o) {
    return o << j;
  }
#endif // USE_IOSTREAMS
  /// @}

  /////////////////////
  // deserialization //
  /////////////////////

  /// @name deserialization
  /// @{

  /*!
  @brief deserialize from a compatible input

  This function reads from a compatible input. Examples are:
  - an array of 1-byte values
  - strings with character/literal type with size of 1 byte
  - input streams
  - container with contiguous storage of 1-byte values. Compatible container
    types include `std::vector`, `std::string`, `std::array`,
    `std::valarray`, and `std::initializer_list`. Furthermore, C-style
    arrays can be used with `std::begin()`/`std::end()`. User-defined
    containers can be used as long as they implement random-access iterators
    and a contiguous storage.

  @pre Each element of the container has a size of 1 byte. Violating this
  precondition yields undefined behavior. **This precondition is enforced
  with a static assertion.**

  @pre The container storage is contiguous. Violating this precondition
  yields undefined behavior. **This precondition is enforced with an
  assertion.**
  @pre Each element of the container has a size of 1 byte. Violating this
  precondition yields undefined behavior. **This precondition is enforced
  with a static assertion.**

  @warning There is no way to enforce all preconditions at compile-time. If
           the function is called with a noncompliant container and with
           assertions switched off, the behavior is undefined and will most
           likely yield segmentation violation.

  @param[in] i  input to read from
  @param[in] cb  a parser callback function of type @ref parser_callback_t
  which is used to control the deserialization by filtering unwanted values
  (optional)

  @return result of the deserialization

  @throw parse_error.101 if a parse error occurs; example: `""unexpected end
  of input; expected string literal""`
  @throw parse_error.102 if to_unicode fails or surrogate error
  @throw parse_error.103 if to_unicode fails

  @complexity Linear in the length of the input. The parser is a predictive
  LL(1) parser. The complexity can be higher if the parser callback function
  @a cb has a super-linear complexity.

  @note A UTF-8 byte order mark is silently ignored.

  @liveexample{The example below demonstrates the `parse()` function reading
  from an array.,parse__array__parser_callback_t}

  @liveexample{The example below demonstrates the `parse()` function with
  and without callback function.,parse__string__parser_callback_t}

  @liveexample{The example below demonstrates the `parse()` function with
  and without callback function.,parse__istream__parser_callback_t}

  @liveexample{The example below demonstrates the `parse()` function reading
  from a contiguous container.,parse__contiguouscontainer__parser_callback_t}

  @since version 2.0.3 (contiguous containers)
  */
  static basic_json parse(detail::input_adapter i,
                          const parser_callback_t cb = nullptr,
                          const bool allow_exceptions = true) {
    basic_json result;
    parser(i, cb, allow_exceptions).parse(true, result);
    return result;
  }

  /*!
  @copydoc basic_json parse(detail::input_adapter, const parser_callback_t)
  */
  static basic_json parse(detail::input_adapter& i,
                          const parser_callback_t cb = nullptr,
                          const bool allow_exceptions = true) {
    basic_json result;
    parser(i, cb, allow_exceptions).parse(true, result);
    return result;
  }

  static bool accept(detail::input_adapter i) { return parser(i).accept(true); }

  static bool accept(detail::input_adapter& i) {
    return parser(i).accept(true);
  }

  /*!
  @brief deserialize from an iterator range with contiguous storage

  This function reads from an iterator range of a container with contiguous
  storage of 1-byte values. Compatible container types include
  `std::vector`, `std::string`, `std::array`, `std::valarray`, and
  `std::initializer_list`. Furthermore, C-style arrays can be used with
  `std::begin()`/`std::end()`. User-defined containers can be used as long
  as they implement random-access iterators and a contiguous storage.

  @pre The iterator range is contiguous. Violating this precondition yields
  undefined behavior. **This precondition is enforced with an assertion.**
  @pre Each element in the range has a size of 1 byte. Violating this
  precondition yields undefined behavior. **This precondition is enforced
  with a static assertion.**

  @warning There is no way to enforce all preconditions at compile-time. If
           the function is called with noncompliant iterators and with
           assertions switched off, the behavior is undefined and will most
           likely yield segmentation violation.

  @tparam IteratorType iterator of container with contiguous storage
  @param[in] first  begin of the range to parse (included)
  @param[in] last  end of the range to parse (excluded)
  @param[in] cb  a parser callback function of type @ref parser_callback_t
  which is used to control the deserialization by filtering unwanted values
  (optional)
  @param[in] allow_exceptions  whether to throw exceptions in case of a
  parse error (optional, true by default)

  @return result of the deserialization

  @throw parse_error.101 in case of an unexpected token
  @throw parse_error.102 if to_unicode fails or surrogate error
  @throw parse_error.103 if to_unicode fails

  @complexity Linear in the length of the input. The parser is a predictive
  LL(1) parser. The complexity can be higher if the parser callback function
  @a cb has a super-linear complexity.

  @note A UTF-8 byte order mark is silently ignored.

  @liveexample{The example below demonstrates the `parse()` function reading
  from an iterator range.,parse__iteratortype__parser_callback_t}

  @since version 2.0.3
  */
  template <class IteratorType,
            typename std::enable_if<
                std::is_base_of<std::random_access_iterator_tag,
                                typename std::iterator_traits<
                                    IteratorType>::iterator_category>::value,
                int>::type = 0>
  static basic_json parse(IteratorType first, IteratorType last,
                          const parser_callback_t cb = nullptr,
                          const bool allow_exceptions = true) {
    basic_json result;
    parser(detail::input_adapter(first, last), cb, allow_exceptions)
        .parse(true, result);
    return result;
  }

  template <class IteratorType,
            typename std::enable_if<
                std::is_base_of<std::random_access_iterator_tag,
                                typename std::iterator_traits<
                                    IteratorType>::iterator_category>::value,
                int>::type = 0>
  static bool accept(IteratorType first, IteratorType last) {
    return parser(detail::input_adapter(first, last)).accept(true);
  }

    /*!
    @brief deserialize from stream
    @deprecated This stream operator is deprecated and will be removed in
                version 4.0.0 of the library. Please use
                @ref operator>>(std::istream&, basic_json&)
                instead; that is, replace calls like `j << i;` with `i >> j;`.
    @since version 1.0.0; deprecated since version 3.0.0
    */
#if USE_IOSTREAMS
  JSON_DEPRECATED
  friend std::istream& operator<<(basic_json& j, std::istream& i) {
    return operator>>(i, j);
  }
#endif
    /*!
    @brief deserialize from stream

    Deserializes an input stream to a JSON value.

    @param[in,out] i  input stream to read a serialized JSON value from
    @param[in,out] j  JSON value to write the deserialized input to

    @throw parse_error.101 in case of an unexpected token
    @throw parse_error.102 if to_unicode fails or surrogate error
    @throw parse_error.103 if to_unicode fails

    @complexity Linear in the length of the input. The parser is a predictive
    LL(1) parser.

    @note A UTF-8 byte order mark is silently ignored.

    @liveexample{The example below shows how a JSON value is constructed by
    reading a serialization from a stream.,operator_deserialize}

    @sa parse(std::istream&, const parser_callback_t) for a variant with a
    parser callback function to filter values while parsing

    @since version 1.0.0
    */
#if USE_IOSTREAMS
  friend std::istream& operator>>(std::istream& i, basic_json& j) {
    parser(detail::input_adapter(i)).parse(false, j);
    return i;
  }
#endif
  /// @}

  ///////////////////////////
  // convenience functions //
  ///////////////////////////
  const char* type_name() const noexcept {
    switch (m_type) {
      case value_t::null: return "null";
      case value_t::object: return "object";
      case value_t::array: return "array";
      case value_t::string: return "string";
      case value_t::boolean: return "boolean";
      case value_t::discarded: return "discarded";
      default: return "number";
    }
  }

  //////////////////////
  // member variables //
  //////////////////////


  /// @}

  //////////////////////////
  // JSON Pointer support //
  //////////////////////////

  /// @name JSON Pointer functions
  /// @{

  /*!
  @brief access specified element via JSON Pointer

  Uses a JSON pointer to retrieve a reference to the respective JSON value.
  No bound checking is performed. Similar to @ref operator[](const typename
  object_t::key_type&), `null` values are created in arrays and objects if
  necessary.

  In particular:
  - If the JSON pointer points to an object key that does not exist, it
    is created an filled with a `null` value before a reference to it
    is returned.
  - If the JSON pointer points to an array index that does not exist, it
    is created an filled with a `null` value before a reference to it
    is returned. All indices between the current maximum and the given
    index are also filled with `null`.
  - The special value `-` is treated as a synonym for the index past the
    end.

  @param[in] ptr  a JSON pointer

  @return reference to the element pointed to by @a ptr

  @complexity Constant.

  @throw parse_error.106   if an array index begins with '0'
  @throw parse_error.109   if an array index was not a number
  @throw out_of_range.404  if the JSON pointer can not be resolved

  @liveexample{The behavior is shown in the example.,operatorjson_pointer}

  @since version 2.0.0
  */
  reference operator[](const json_pointer& ptr) {
    return ptr.get_unchecked(this);
  }

  /*!
  @brief access specified element via JSON Pointer

  Uses a JSON pointer to retrieve a reference to the respective JSON value.
  No bound checking is performed. The function does not change the JSON
  value; no `null` values are created. In particular, the the special value
  `-` yields an exception.

  @param[in] ptr  JSON pointer to the desired element

  @return const reference to the element pointed to by @a ptr

  @complexity Constant.

  @throw parse_error.106   if an array index begins with '0'
  @throw parse_error.109   if an array index was not a number
  @throw out_of_range.402  if the array index '-' is used
  @throw out_of_range.404  if the JSON pointer can not be resolved

  @liveexample{The behavior is shown in the example.,operatorjson_pointer_const}

  @since version 2.0.0
  */
  const_reference operator[](const json_pointer& ptr) const {
    return ptr.get_unchecked(this);
  }

  /*!
  @brief access specified element via JSON Pointer

  Returns a reference to the element at with specified JSON pointer @a ptr,
  with bounds checking.

  @param[in] ptr  JSON pointer to the desired element

  @return reference to the element pointed to by @a ptr

  @throw parse_error.106 if an array index in the passed JSON pointer @a ptr
  begins with '0'. See example below.

  @throw parse_error.109 if an array index in the passed JSON pointer @a ptr
  is not a number. See example below.

  @throw out_of_range.401 if an array index in the passed JSON pointer @a ptr
  is out of range. See example below.

  @throw out_of_range.402 if the array index '-' is used in the passed JSON
  pointer @a ptr. As `at` provides checked access (and no elements are
  implicitly inserted), the index '-' is always invalid. See example below.

  @throw out_of_range.403 if the JSON pointer describes a key of an object
  which cannot be found. See example below.

  @throw out_of_range.404 if the JSON pointer @a ptr can not be resolved.
  See example below.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes in the JSON value.

  @complexity Constant.

  @since version 2.0.0

  @liveexample{The behavior is shown in the example.,at_json_pointer}
  */
  reference at(const json_pointer& ptr) { return ptr.get_checked(this); }

  /*!
  @brief access specified element via JSON Pointer

  Returns a const reference to the element at with specified JSON pointer @a
  ptr, with bounds checking.

  @param[in] ptr  JSON pointer to the desired element

  @return reference to the element pointed to by @a ptr

  @throw parse_error.106 if an array index in the passed JSON pointer @a ptr
  begins with '0'. See example below.

  @throw parse_error.109 if an array index in the passed JSON pointer @a ptr
  is not a number. See example below.

  @throw out_of_range.401 if an array index in the passed JSON pointer @a ptr
  is out of range. See example below.

  @throw out_of_range.402 if the array index '-' is used in the passed JSON
  pointer @a ptr. As `at` provides checked access (and no elements are
  implicitly inserted), the index '-' is always invalid. See example below.

  @throw out_of_range.403 if the JSON pointer describes a key of an object
  which cannot be found. See example below.

  @throw out_of_range.404 if the JSON pointer @a ptr can not be resolved.
  See example below.

  @exceptionsafety Strong guarantee: if an exception is thrown, there are no
  changes in the JSON value.

  @complexity Constant.

  @since version 2.0.0

  @liveexample{The behavior is shown in the example.,at_json_pointer_const}
  */
  const_reference at(const json_pointer& ptr) const {
    return ptr.get_checked(this);
  }

  /*!
  @brief return flattened JSON value

  The function creates a JSON object whose keys are JSON pointers (see [RFC
  6901](https://tools.ietf.org/html/rfc6901)) and whose values are all
  primitive. The original JSON value can be restored using the @ref
  unflatten() function.

  @return an object that maps JSON pointers to primitive values

  @note Empty objects and arrays are flattened to `null` and will not be
        reconstructed correctly by the @ref unflatten() function.

  @complexity Linear in the size the JSON value.

  @liveexample{The following code shows how a JSON object is flattened to an
  object whose keys consist of JSON pointers.,flatten}

  @sa @ref unflatten() for the reverse function

  @since version 2.0.0
  */
  basic_json flatten() const {
    basic_json result(value_t::object);
    json_pointer::flatten("", *this, result);
    return result;
  }

  /*!
  @brief unflatten a previously flattened JSON value

  The function restores the arbitrary nesting of a JSON value that has been
  flattened before using the @ref flatten() function. The JSON value must
  meet certain constraints:
  1. The value must be an object.
  2. The keys must be JSON pointers (see
     [RFC 6901](https://tools.ietf.org/html/rfc6901))
  3. The mapped values must be primitive JSON types.

  @return the original JSON from a flattened version

  @note Empty objects and arrays are flattened by @ref flatten() to `null`
        values and can not unflattened to their original type. Apart from
        this example, for a JSON value `j`, the following is always true:
        `j == j.flatten().unflatten()`.

  @complexity Linear in the size the JSON value.

  @throw type_error.314  if value is not an object
  @throw type_error.315  if object values are not primitive

  @liveexample{The following code shows how a flattened JSON object is
  unflattened into the original nested JSON object.,unflatten}

  @sa @ref flatten() for the reverse function

  @since version 2.0.0
  */
  basic_json unflatten() const { return json_pointer::unflatten(*this); }

  /// @}

  //////////////////////////
  // JSON Patch functions //
  //////////////////////////

  /// @name JSON Patch functions
  /// @{

  /*!
  @brief applies a JSON patch

  [JSON Patch](http://jsonpatch.com) defines a JSON document structure for
  expressing a sequence of operations to apply to a JSON) document. With
  this function, a JSON Patch is applied to the current JSON value by
  executing all operations from the patch.

  @param[in] json_patch  JSON patch document
  @return patched document

  @note The application of a patch is atomic: Either all operations succeed
        and the patched document is returned or an exception is thrown. In
        any case, the original value is not changed: the patch is applied
        to a copy of the value.

  @throw parse_error.104 if the JSON patch does not consist of an array of
  objects

  @throw parse_error.105 if the JSON patch is malformed (e.g., mandatory
  attributes are missing); example: `"operation add must have member path"`

  @throw out_of_range.401 if an array index is out of range.

  @throw out_of_range.403 if a JSON pointer inside the patch could not be
  resolved successfully in the current JSON value; example: `"key baz not
  found"`

  @throw out_of_range.405 if JSON pointer has no parent ("add", "remove",
  "move")

  @throw other_error.501 if "test" operation was unsuccessful

  @complexity Linear in the size of the JSON value and the length of the
  JSON patch. As usually only a fraction of the JSON value is affected by
  the patch, the complexity can usually be neglected.

  @liveexample{The following code shows how a JSON patch is applied to a
  value.,patch}

  @sa @ref diff -- create a JSON patch by comparing two JSON values

  @sa [RFC 6902 (JSON Patch)](https://tools.ietf.org/html/rfc6902)
  @sa [RFC 6901 (JSON Pointer)](https://tools.ietf.org/html/rfc6901)

  @since version 2.0.0
  */
  basic_json patch(const basic_json& json_patch) const {
    // make a working copy to apply the patch to
    basic_json result = *this;

    // the valid JSON Patch operations
    enum class patch_operations {
      add,
      remove,
      replace,
      move,
      copy,
      test,
      invalid
    };

    const auto get_op = [](const std::string& op) {
      if (op == "add") {
        return patch_operations::add;
      }
      if (op == "remove") {
        return patch_operations::remove;
      }
      if (op == "replace") {
        return patch_operations::replace;
      }
      if (op == "move") {
        return patch_operations::move;
      }
      if (op == "copy") {
        return patch_operations::copy;
      }
      if (op == "test") {
        return patch_operations::test;
      }

      return patch_operations::invalid;
    };

    // wrapper for "add" operation; add value at ptr
    const auto operation_add = [&result](json_pointer& ptr, basic_json val) {
      // adding to the root of the target document means replacing it
      if (ptr.is_root()) {
        result = val;
      } else {
        // make sure the top element of the pointer exists
        json_pointer top_pointer = ptr.top();
        if (top_pointer != ptr) {
          result.at(top_pointer);
        }

        // get reference to parent of JSON pointer ptr
        const auto last_path = ptr.pop_back();
        basic_json& parent = result[ptr];

        switch (parent.m_type) {
        case value_t::null:
        case value_t::object: {
          // use operator[] to add value
          parent[last_path] = val;
          break;
        }

        case value_t::array: {
          if (last_path == "-") {
            // special case: append to back
            parent.push_back(val);
          } else {
            const auto idx = json_pointer::array_index(last_path);
            if (JSON_UNLIKELY(static_cast<size_type>(idx) > parent.size())) {
              // avoid undefined behavior
              JSON_THROW(out_of_range::create(401, "array index " +
                                                       std::to_string(idx) +
                                                       " is out of range"));
            } else {
              // default case: insert add offset
              parent.insert(parent.begin() + static_cast<difference_type>(idx),
                            val);
            }
          }
          break;
        }

        default: {
          // if there exists a parent it cannot be primitive
          assert(false); // LCOV_EXCL_LINE
        }
        }
      }
    };

    // wrapper for "remove" operation; remove value at ptr
    const auto operation_remove = [&result](json_pointer& ptr) {
      // get reference to parent of JSON pointer ptr
      const auto last_path = ptr.pop_back();
      basic_json& parent = result.at(ptr);

      // remove child
      if (parent.is_object()) {
        // perform range check
        auto it = parent.find(last_path);
        if (JSON_LIKELY(it != parent.end())) {
          parent.erase(it);
        } else {
          JSON_THROW(
              out_of_range::create(403, "key '" + last_path + "' !found"));
        }
      } else if (parent.is_array()) {
        // note erase performs range check
        parent.erase(
            static_cast<size_type>(json_pointer::array_index(last_path)));
      }
    };

    // type check: top level value must be an array
    if (JSON_UNLIKELY(!json_patch.is_array())) {
      JSON_THROW(parse_error::create(104, 0,
                                     "JSON patch must be an array of objects"));
    }

    // iterate and apply the operations
    for (const auto& val : json_patch) {
      // wrapper to get a value for an operation
      const auto get_value = [&val](const std::string& op,
                                    const std::string& member,
                                    bool string_type) -> basic_json& {
        // find value
        auto it = val.m_value.object->find(member);

        // context-sensitive error message
        const auto error_msg =
            (op == "op") ? "operation" : "operation '" + op + "'";

        // check if desired value is present
        if (JSON_UNLIKELY(it == val.m_value.object->end())) {
          JSON_THROW(parse_error::create(
              105, 0, error_msg + " must have member '" + member + "'"));
        }

        // check if result is of type string
        if (JSON_UNLIKELY(string_type && !it->second.is_string())) {
          JSON_THROW(parse_error::create(
              105, 0, error_msg + " must have string member '" + member + "'"));
        }

        // no error: return value
        return it->second;
      };

      // type check: every element of the array must be an object
      if (JSON_UNLIKELY(!val.is_object())) {
        JSON_THROW(parse_error::create(
            104, 0, "JSON patch must be an array of objects"));
      }

      // collect mandatory members
      const std::string op = get_value("op", "op", true);
      const std::string path = get_value(op, "path", true);
      json_pointer ptr(path);

      switch (get_op(op)) {
      case patch_operations::add: {
        operation_add(ptr, get_value("add", "value", false));
        break;
      }

      case patch_operations::remove: {
        operation_remove(ptr);
        break;
      }

      case patch_operations::replace: {
        // the "path" location must exist - use at()
        result.at(ptr) = get_value("replace", "value", false);
        break;
      }

      case patch_operations::move: {
        const std::string from_path = get_value("move", "from", true);
        json_pointer from_ptr(from_path);

        // the "from" location must exist - use at()
        basic_json v = result.at(from_ptr);

        // The move operation is functionally identical to a
        // "remove" operation on the "from" location, followed
        // immediately by an "add" operation at the target
        // location with the value that was just removed.
        operation_remove(from_ptr);
        operation_add(ptr, v);
        break;
      }

      case patch_operations::copy: {
        const std::string from_path = get_value("copy", "from", true);
        const json_pointer from_ptr(from_path);

        // the "from" location must exist - use at()
        basic_json v = result.at(from_ptr);

        // The copy is functionally identical to an "add"
        // operation at the target location using the value
        // specified in the "from" member.
        operation_add(ptr, v);
        break;
      }

      case patch_operations::test: {
        bool success = false;
        JSON_TRY {
          // check if "value" matches the one at "path"
          // the "path" location must exist - use at()
          success = (result.at(ptr) == get_value("test", "value", false));
        }
        JSON_CATCH(out_of_range&) {
          // ignore out of range errors: success remains false
        }

        // throw an exception if test fails
        if (JSON_UNLIKELY(!success)) {
          JSON_THROW(other_error::create(501, "unsuccessful: " + val.dump()));
        }

        break;
      }

      case patch_operations::invalid: {
        // op must be "add", "remove", "replace", "move", "copy", or
        // "test"
        JSON_THROW(parse_error::create(
            105, 0, "operation value '" + op + "' is invalid"));
      }
      }
    }

    return result;
  }

  /*!
  @brief creates a diff as a JSON patch

  Creates a [JSON Patch](http://jsonpatch.com) so that value @a source can
  be changed into the value @a target by calling @ref patch function.

  @invariant For two JSON values @a source and @a target, the following code
  yields always `true`:
  @code {.cpp}
  source.patch(diff(source, target)) == target;
  @endcode

  @note Currently, only `remove`, `add`, and `replace` operations are
        generated.

  @param[in] source  JSON value to compare from
  @param[in] target  JSON value to compare against
  @param[in] path    helper value to create JSON pointers

  @return a JSON patch to convert the @a source to @a target

  @complexity Linear in the lengths of @a source and @a target.

  @liveexample{The following code shows how a JSON patch is created as a
  diff for two JSON values.,diff}

  @sa @ref patch -- apply a JSON patch
  @sa @ref merge_patch -- apply a JSON Merge Patch

  @sa [RFC 6902 (JSON Patch)](https://tools.ietf.org/html/rfc6902)

  @since version 2.0.0
  */
  static basic_json diff(const basic_json& source, const basic_json& target,
                         const std::string& path = "") {
    // the patch
    basic_json result(value_t::array);

    // if the values are the same, return empty patch
    if (source == target) {
      return result;
    }

    if (source.type() != target.type()) {
      // different types: replace value
      result.push_back({{"op", "replace"}, {"path", path}, {"value", target}});
    } else {
      switch (source.type()) {
      case value_t::array: {
        // first pass: traverse common elements
        std::size_t i = 0;
        while (i < source.size() && i < target.size()) {
          // recursive call to compare array values at index i
          auto temp_diff =
              diff(source[i], target[i], path + "/" + std::to_string(i));
          result.insert(result.end(), temp_diff.begin(), temp_diff.end());
          ++i;
        }

        // i now reached the end of at least one array
        // in a second pass, traverse the remaining elements

        // remove my remaining elements
        const auto end_index = static_cast<difference_type>(result.size());
        while (i < source.size()) {
          // add operations in reverse order to avoid invalid
          // indices
          result.insert(result.begin() + end_index,
                        object({{"op", "remove"},
                                {"path", path + "/" + std::to_string(i)}}));
          ++i;
        }

        // add other remaining elements
        while (i < target.size()) {
          result.push_back({{"op", "add"},
                            {"path", path + "/" + std::to_string(i)},
                            {"value", target[i]}});
          ++i;
        }

        break;
      }

      case value_t::object: {
        // first pass: traverse this object's elements
        for (auto it = source.cbegin(); it != source.cend(); ++it) {
          // escape the key name to be used in a JSON patch
          const auto key = json_pointer::escape(it.key());

          if (target.find(it.key()) != target.end()) {
            // recursive call to compare object values at key it
            auto temp_diff =
                diff(it.value(), target[it.key()], path + "/" + key);
            result.insert(result.end(), temp_diff.begin(), temp_diff.end());
          } else {
            // found a key that is not in o -> remove it
            result.push_back(
                object({{"op", "remove"}, {"path", path + "/" + key}}));
          }
        }

        // second pass: traverse other object's elements
        for (auto it = target.cbegin(); it != target.cend(); ++it) {
          if (source.find(it.key()) == source.end()) {
            // found a key that is not in this -> add it
            const auto key = json_pointer::escape(it.key());
            result.push_back({{"op", "add"},
                              {"path", path + "/" + key},
                              {"value", it.value()}});
          }
        }

        break;
      }

      default: {
        // both primitive type: replace value
        result.push_back(
            {{"op", "replace"}, {"path", path}, {"value", target}});
        break;
      }
      }
    }

    return result;
  }

  /// @}

  ////////////////////////////////
  // JSON Merge Patch functions //
  ////////////////////////////////

  /// @name JSON Merge Patch functions
  /// @{

  /*!
  @brief applies a JSON Merge Patch

  The merge patch format is primarily intended for use with the HTTP PATCH
  method as a means of describing a set of modifications to a target
  resource's content. This function applies a merge patch to the current
  JSON value.

  The function implements the following algorithm from Section 2 of
  [RFC 7396 (JSON Merge Patch)](https://tools.ietf.org/html/rfc7396):

  ```
  define MergePatch(Target, Patch):
    if Patch is an Object:
      if Target is not an Object:
        Target = {} // Ignore the contents and set it to an empty Object
      for each Name/Value pair in Patch:
        if Value is null:
          if Name exists in Target:
            remove the Name/Value pair from Target
        else:
          Target[Name] = MergePatch(Target[Name], Value)
      return Target
    else:
      return Patch
  ```

  Thereby, `Target` is the current object; that is, the patch is applied to
  the current value.

  @param[in] patch  the patch to apply

  @complexity Linear in the lengths of @a patch.

  @liveexample{The following code shows how a JSON Merge Patch is applied to
  a JSON document.,merge_patch}

  @sa @ref patch -- apply a JSON patch
  @sa [RFC 7396 (JSON Merge Patch)](https://tools.ietf.org/html/rfc7396)

  @since version 3.0.0
  */
  void merge_patch(const basic_json& patch) {
    if (patch.is_object()) {
      if (!is_object()) {
        *this = object();
      }
      for (auto it = patch.begin(); it != patch.end(); ++it) {
        if (it.value().is_null()) {
          erase(it.key());
        } else {
          operator[](it.key()).merge_patch(it.value());
        }
      }
    } else {
      *this = patch;
    }
  }

  /// @}
};
} // namespace my_json

///////////////////////
// nonmember support //
///////////////////////

// specialization of std::swap, and std::hash
namespace std {
/*!
@brief exchanges the values of two JSON objects

@since version 1.0.0
*/
template <>
inline void swap(my_json::json& j1, my_json::json& j2) noexcept(
    is_nothrow_move_constructible<my_json::json>::value &&
        is_nothrow_move_assignable<my_json::json>::value) {
  j1.swap(j2);
}

/// hash value for JSON objects
template <> struct hash<my_json::json> {
  /*!
  @brief return a hash value for a JSON object

  @since version 1.0.0
  */
  std::size_t operator()(const my_json::json& j) const {
    // a naive hashing via the string representation
    const auto& h = hash<my_json::json::string_t>();
    return h(j.dump());
  }
};

/// specialization for std::less<value_t>
/// @note: do not remove the space after '<',
///        see https://github.com/my_json/json/pull/679
template <> struct less<::my_json::detail::value_t> {
  /*!
  @brief compare two value_t enum values
  @since version 3.0.0
  */
  bool operator()(my_json::detail::value_t lhs,
                  my_json::detail::value_t rhs) const noexcept {
    return my_json::detail::operator<(lhs, rhs);
  }
};

} // namespace std

/*!
@brief user-defined string literal for JSON values

This operator implements a user-defined string literal for JSON objects. It
can be used by adding `"_json"` to a string literal and returns a JSON object
if no parse error occurred.

@param[in] s  a string representation of a JSON object
@param[in] n  the length of string @a s
@return a JSON object

@since version 1.0.0
*/
inline my_json::json operator"" _json(const char* s, std::size_t n) {
  return my_json::json::parse(s, s + n);
}

/*!
@brief user-defined string literal for JSON pointer

This operator implements a user-defined string literal for JSON Pointers. It
can be used by adding `"_json_pointer"` to a string literal and returns a JSON
pointer object if no parse error occurred.

@param[in] s  a string representation of a JSON Pointer
@param[in] n  the length of string @a s
@return a JSON pointer object

@since version 2.0.0
*/
inline my_json::json::json_pointer operator"" _json_pointer(const char* s,
                                                            std::size_t n) {
  return my_json::json::json_pointer(std::string(s, n));
}

// <my_json/detail/macro_unscope.hpp>

// restore GCC/clang diagnostic settings
#if defined(__clang__) || defined(__GNUC__) || defined(__GNUG__)
#pragma GCC diagnostic pop
#endif
#if defined(__clang__)
#pragma GCC diagnostic pop
#endif

// clean up
#undef JSON_CATCH
#undef JSON_THROW
#undef JSON_TRY
#undef JSON_LIKELY
#undef JSON_UNLIKELY
#undef JSON_DEPRECATED
#undef JSON_HAS_CPP_14
#undef JSON_HAS_CPP_17
#undef MY_JSON_BASIC_JSON_TPL_DECLARATION
#undef MY_JSON_BASIC_JSON_TPL
#undef MY_JSON_JSON_HAS_HELPER

#endif
