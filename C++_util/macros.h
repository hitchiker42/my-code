#ifndef _MACROS_H_
#define _MACROS_H_
/**
   Macros usable in C or C++
*/
//Some Macros
#define SET_BIT(val, bit)                       \
  ((val) |= (1ul << (bit)))
#define UNSET_BIT(val, bit)                     \
  ((val) &= ~(1ul << (bit)))
#define FLIP_BIT(val, bit)                      \
  ((val) ^= (1ul << (bit)))


//will fail for 0
//To exclude 0 use (num && (!(num & (num-1))))
#define IS_POW_OF_2(x) (!(x & (x-1)))

//This will fail for 0
#if (defined __GNUC__)
#define NEXT_POW_OF_2(num)                                      \
  (1UL << ((sizeof(long)*CHAR_BIT) - __builtin_clzl(num)))
#else //sorry windows, but you kinda suck when it comes to intrinsics.
#define NEXT_POW_OF_2(x)                                \
  (sizeof(x) <= 4 ?                                     \
   (x |= x >> 1; x |= x >> 2; x |= x >> 4;              \
    x |= x >> 8; x |= x >> 16; ++x) :                   \
   (x |= x >> 1; x |= x >> 2; x |= x >> 4;              \
    x |= x >> 8; x |= x >> 16; x |= x >> 32; ++x))
#endif



//If num is a power of 2 return it, otherwise return
//the nearest power of 2 larger than num.
#define NEAREST_POW_OF_2(num)                   \
  (IS_POW_OF_2(num) ? num : NEXT_POW_OF_2(num))

#define DOWNCASE_ASCII(c) (c > 0x40 && c < 0x5B ? c | 0x20 : c)
#define UPCASE_ASCII(c) (c > 0x60 && c < 0x7B ? c & (~0x20) : c)
/*
  CPP Funtimes
*/
#define CAT(a,b) PRIMITIVE_CAT(a, b)
#define PRIMITIVE_CAT(a,b) a ## b
#define CAT2(a,b) PRIMITIVE_CAT2(a,b)
#define PRIMITIVE_CAT2(a,b) a ## b
#define CAT3(a, b, c) PRIMITIVE_CAT3(a, b, c)
#define PRIMITIVE_CAT3(a, b, c) a ## b ##c
#define CAT4(a, b, c, d) PRIMITIVE_CAT4(a, b, c, d)
#define PRIMITIVE_CAT4(a, b, c, d) a ## b ## c ## d
//these concatenate their arguments but insert an underscore between them
//they're mainly for use in constructing function names in macros
#define CAT_2(a,b) PRIMITIVE_CAT_2(a, b)
#define PRIMITIVE_CAT_2(a,b) a ## _ ## b
#define CAT_3(a, b, c) PRIMITIVE_CAT_3(a, b, c)
#define PRIMITIVE_CAT_3(a, b, c) a ## _ ## b ## _ ## c
#define CAT_4(a, b, c, d) PRIMITIVE_CAT_4(a, b, c, d)
#define PRIMITIVE_CAT_4(a, b, c, d) a ## _ b ## _ ## c ## _ ## d
//Count number of vaargs upto 64
#define __NARG__(...)  __NARG_I_(__VA_ARGS__,__RSEQ_N())
#define __NARG_I_(...) __ARG_N(__VA_ARGS__)
#define __ARG_N(_1, _2, _3, _4, _5, _6, _7, _8, _9,_10,         \
                _11,_12,_13,_14,_15,_16,_17,_18,_19,_20,        \
                _21,_22,_23,_24,_25,_26,_27,_28,_29,_30,        \
                _31,_32,_33,_34,_35,_36,_37,_38,_39,_40,        \
                _41,_42,_43,_44,_45,_46,_47,_48,_49,_50,        \
                _51,_52,_53,_54,_55,_56,_57,_58,_59,_60,        \
                _61,_62,_63,n,...) n
#define __RSEQ_N() 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53,  \
                   52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42,  \
                   41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31,  \
                   30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20,  \
                   19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9,   \
                   8, 7, 6, 5, 4, 3, 2, 1, 0
//Macro recursion
#define EVAL0(...) __VA_ARGS__
#define EVAL1(...) EVAL0 (EVAL0 (EVAL0 (__VA_ARGS__)))
#define EVAL2(...) EVAL1 (EVAL1 (EVAL1 (__VA_ARGS__)))
#define EVAL3(...) EVAL2 (EVAL2 (EVAL2 (__VA_ARGS__)))
#define EVAL4(...) EVAL3 (EVAL3 (EVAL3 (__VA_ARGS__)))
#define EVAL(...)  EVAL4 (EVAL4 (EVAL4 (__VA_ARGS__)))

#define MAP_END(...)
#define MAP_OUT

#define MAP_GET_END() 0, MAP_END
#define MAP_NEXT0(test, next, ...) next MAP_OUT
#define MAP_NEXT1(test, next) MAP_NEXT0 (test, next, 0)
#define MAP_NEXT(test, next)  MAP_NEXT1 (MAP_GET_END test, next)

#define MACRO_MAP0(f, x, peek, ...) f(x) MAP_NEXT (peek, MACRO_MAP1) (f, peek, __VA_ARGS__)
#define MACRO_MAP1(f, x, peek, ...) f(x) MAP_NEXT (peek, MACRO_MAP0) (f, peek, __VA_ARGS__)
#define MACRO_MAP(f, ...) EVAL (MACRO_MAP1(f, __VA_ARGS__ ,(), 0))

/**
   C++ specific macros.
 */
/*
  Macros to make C++ I/O look a bit nicer, can also use variadic templates
*/
#define STREAM0(op, arg, peek, ...)                             \
  op arg MAP_NEXT(peek, STREAM1) (op, peek, __VA_ARGS__)
#define STREAM1(op, arg, peek, ...)                             \
  op arg MAP_NEXT(peek, STREAM0) (op, peek, __VA_ARGS__)
#define STREAM_OUT(out, ...)                    \
  out EVAL(STREAM1(<<, __VA_ARGS__, (), 0))
#define STREAM_IN(in, ...)                      \
  in EVAL(STREAM1(>>, __VA_ARGS__, (), 0))

//Macros for automatically generating operators
#include "operators.h"
/*
  Macros for SFINAE
*/
/*
  Simplifies enable_if, if is_'what' is a template in <type_traits>
  enable_if_is(what, T) will expand to T if is_'what'<T>::value is true,
  and nothing if it isn't.
  ex. enable_if_is(unsigned, T) will expand to T for unsigned types and
  nothing for signed types
*/
#define enable_if_is(what, T)                                   \
typename std::enable_if<CAT(std::is_, what)<T>::value, T>::type
#define enable_if_is_not(what, T)                               \
typename std::enable_if<!CAT(std::is_, what)<T>::value, T>::type
#define enable_int_if_is(what, T)                                   \
std::enable_if_t<CAT(std::is_, what)<T>::value, int>
#define enable_int_if_is_not(what, T)                               \
std::enable_if_t<!CAT(std::is_, what)<T>::value, int>


//Check if a class/struct has a member of the given name.
//The macro takes the member name and its type, it produces a template
//which takes a type T and constructs a struct with a member 'value' which
//indicates if T has a member named 'name' with type 'type'. This could be
//modified so the type of the member was passed to the macro rather
//than the template.
//The (type of th) class being tested is available for use in type as T.
#define define_has_member(name)                 \
  define_has_member_(name, CAT(has_, name))
#define define_has_member_(name, struct_name)                           \
  template <typename T, typename = void>                                \
  struct struct_name : std::false_type {};                              \
  template<typename T>                                                  \
  struct struct_name<T, std::void_t<decltype(T::name)>> : std::true_type {}; \
  template <typename T>                                                 \
  inline constexpr bool CAT(struct_name, _v) = struct_name<T>::value;

#define define_has_member_typed(name, type)            \
  define_has_member_typed_(name, type, CAT(has_, name))
#define define_has_member_typed_(name, struct_name, type)               \
  template <typename T, typename = void>                                \
  struct struct_name : std::false_type {};                              \
  template<typename T>                                                  \
  struct struct_name<T,                                                 \
    std::enable_if_t<std::is_same_v<decltype(T::name), type>, void>>    \
    : std::true_type {};                                                \
  template <typename T>                                                 \
  inline constexpr bool CAT(struct_name, _v) = struct_name<T>::value;

#define define_has_member_function(name)                \
  define_has_member_function_(name, CAT(has_, name))
#define define_has_member_function_(name, struct_name)                  \
  template <typename T, typename = void>                                \
  struct struct_name : std::false_type {};                              \
  template<typename T>                                                  \
  struct struct_name<                                                   \
   T, std::enable_if_t<std::is_member_function_pointer_v<decltype(&T::name)>, \
                       void>> : std::true_type {};                      \
  template <typename T>                                                 \
  inline constexpr bool CAT(struct_name, _v) = struct_name<T>::value;
/*
  Type manipulation with overload sets is tricky, so to test if a struct
  has a member function taking specific types we just use decltype on
  an invocation of the function. This is less elegent that the previous 3
  ways to test for members, but this works and trying to do it more elegently
  doesn't work.
*/
#define define_has_member_overloaded(name)              \
  define_has_member_overloaded_(name, CAT(has_, name))
#define define_has_member_overloaded_(name, struct_name)                \
  template <typename T, typename... Args> struct struct_name {          \
    template <typename N,                                               \
              typename = decltype(std::declval<N>().name(std::declval<Args>()...))> \
    static int8_t check(int);                                            \
    /* This will catch anything */                                      \
    template <typename N> static int16_t check(...);/*{return 0;};*/     \
    /* This is the flag that indicates if the member exists */          \
    static constexpr bool value = sizeof(check<T>(0)) == sizeof(int8_t); \
  };                                                                    \
  template<typename T, typename... Args>                                \
  inline constexpr bool CAT(struct_name, _v) = struct_name<T,Args...>::value;

//Needs to be called from the top level namespace and type should
//have a member function get
#define define_tuple_info(type, sz)                                     \
  namespace std {                                                       \
  template<size_t N> struct tuple_element<N,type> {                     \
    using type = decltype(std::declval<type>().get<N>());               \
  };                                                                    \
  template<>                                                            \
  struct tuple_size<type> : std::integral_constant<size_t, sz> {};      \
  };

//Explicitly instantiates a template in a header, while also
//preventing any explicit instantiations. Likely won't shorten
//compilation times since the template is still instianteted in
//each translation file.
#define extern_template_header(ret, name, ...)      \
  extern template inline ret name<__VA_ARGS__>(__VA_ARGS__);\
  template inline ret name<__VA_ARGS__>(__VA_ARGS__);
  
//Macro for portably using possibly empty __VA_ARGS__
//Likely to eventually be replaced by C++20 VA_OPT
#ifndef MAYBE_VA_ARGS
#if __cplusplus > 202000L //C++20
#define MAYBE_VA_ARGS(...) __VA_OPT__(,) __VA_ARGS__
#elif (defined __GNUC__)
#define MAYBE_VA_ARGS(...) ,##__VA_ARGS__
#else
#define MAYBE_VA_ARGS(...) ,__VA_ARGS__
#endif
#endif

#if (defined DEBUG)
#define DEBUG_PRINTF(fmt, ...) fprintf(stderr, fmt MAYBE_VA_ARGS(__VA_ARGS__))
#else
#define DEBUG_PRINTF(...)
#endif

//portable version of gcc's builtin_unreachable
#if (defined __GNUC__)
#define unreachable() __builtin_unreachable()
#elif (defined _MSC_VER)
//MVSC has assume, which is like assert but used by the compiler.
#define unreachable() __assume(0)
#else
//Will only work when debugging is enabled but it's better than nothing.
#define unreachable() assert(false);
#endif

//some macros for cleaner attribute syntax
#define ATTRIBUTE(...) [[__VA_ARGS__]]
#define attribute_unused ATTRIBUTE(maybe_unused)
#define attribute_noreturn ATTRIBUTE(noreturn)
#define attribute_fallthrough ATTRIBUTE(fallthrough)
#if __cplusplus > 202000L //C++20
#define attribute_likely(...) [[likely]] __VA_ARGS__
#define attribute_unlikely(...) [[unlikely]] __VA_ARGS__
#elif (defined __GNUC__)
#define attribute_likely(...) __builtin_expect(__VA_ARGS__, 1)
#define attribute_unlikely(...) __builtin_expect(__VA_ARGS__, 0)
#else
#define attribute_likely(...) __VA_ARGS__
#define attribute_unlikely(...) __VA_ARGS__
#endif

//Shut up a super annoying g++ warning.
#if (defined __GNUC__) && (defined __GNUC_MINOR__) && (defined __GNUC_PATCHLEVEL__)
#if __GNUC__ >= 8
#pragma GCC diagnostic ignored "-Wclass-memaccess"
#endif
#endif
/*
  Super tricky way to test if something is a string literal.

  Not actually used, since it doesn't work for the main thing
  I wanted to use it for, detecting if something is a string literal
  in order to concatenate it with another string literal. what I'd like
  to do is define a macro like
  #define print(X)
  if constexpr(is_literal(X)){
    printf("literal" X)
  } else {
    printf(("literal"s + X).c_str());
  }
  however code in both branches of a constexpr if need to be syntactically
  valid. So if X is a pointer it results in: "literal" ptr, which is invalid.
*/
#if 0

//The way to do this is to use macro stringification (the # character)

//If x isn't a const char* return false, otherwise compare x and #x
//#define is_literal_(x)
//  (std::is_same_v<std::decay_t<decltype(x)>, const char *> ?
//   is_literal_f(#x, sizeof(#x) - 1) : false)
#define is_literal_(x)  is_literal_f(#x, sizeof(#x) - 1)
#define is_literal(x) is_literal_(x)

static constexpr const char* constexpr_strchr(const char *s, char c){
  return (*s == '\0' ? nullptr :
          (*s == c ? s : constexpr_strchr(s+1, c)));
}
//test if s is formed by strigifying a string literal. len is the
//size of s, excluding the terminating '\0'.
static constexpr bool is_literal_f(const char *s, size_t len){
  const char *e = s + len;
  //quick test to eliminate most non-literals
  if(s[0] != '"') return false;
  //We need this loop to detect literals formed by contatenation
  //i.e "hello" "world" stringifies to ""hello" "world""
  while(s != e){
    s = constexpr_strchr(s + 1, '"');
    if(s == NULL){ return false; }
    ++s;
    if(*s == ' '){ ++s; } //potentially skip a space
  }
  return true;
}
#endif

#endif
