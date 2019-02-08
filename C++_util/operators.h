#ifndef _OPERATORS_H_
#define _OPERATORS_H_

/*
  Macros to automatically generate various operators.
*/
//Requires type_traits to be included at the point of macro expansion
//since there is no other way to get the underlying
#define enum_cast_macro(T,x) static_cast<std::underlying_type_t<T>>(x)
#define gen_enum_binop(T,op)                            \
  constexpr inline T operator op(T lhs, T rhs){                               \
    return T(enum_cast_macro(T, lhs) op enum_cast_macro(T, rhs));   \
  }
#define gen_enum_binop_equals(T,op)                             \
  constexpr inline T& operator op(T& lhs, T rhs){                                 \
    return (lhs = T(enum_cast_macro(T, lhs) op enum_cast_macro(T, rhs)));   \
  }
#define gen_enum_unop(T,op)                     \
  constexpr inline T operator op(T val){                        \
    return T(op(enum_cast_macro(T, val)));            \
  }
#define gen_enum_bitwise_operators(E)           \
  gen_enum_binop(&);                            \
  gen_enum_binop(|);                            \
  gen_enum_binop(^);                            \
  gen_enum_binop_equals(&);                     \
  gen_enum_binop_equals(|);                     \
  gen_enum_binop_equals(^);                     \
  gen_enum_unop(~)
//only generate and, or and not, and only the non-assignment versions.
#define gen_simple_enum_bitwise_operators(E)    \
  gen_enum_binop(E, &);                         \
  gen_enum_binop(E, |);                         \
  gen_enum_unop(E, ~)

//Generate == / != operators which do bytewise comparison.
#define generate_bytewise_equality_operators(name)              \
  friend bool operator==(const name &lhs, const name &rhs){     \
    return !memcmp(&lhs, &rhs, sizeof(name));                   \
  }                                                             \
  friend bool operator!=(const name &lhs, const name &rhs){     \
    return memcmp(&lhs, &rhs, sizeof(name));                    \
  }

//Generate a set of comparison operators for 'name' which compare
//two 'name's by comparing the member varible 'member_name'
#define gen_rel_ops_via_member_generic(name, member_name, specifier)              \
  specifier bool operator<(const name& a, const name& b){                  \
    return a.member_name < b.member_name;                               \
  }                                                                     \
  specifier bool operator==(const name& a, const name& b){                 \
    return a.member_name == b.member_name;                              \
  }                                                                     \
  specifier bool operator<=(const name& a, const name& b){                 \
    return a.member_name <= b.member_name;                              \
  }                                                                     \
  specifier bool operator>=(const name& a, const name& b){                 \
    return a.member_name >= b.member_name;                              \
  }                                                                     \
  specifier bool operator>(const name& a, const name& b){                  \
    return a.member_name > b.member_name;                               \
  }                                                                     \
  specifier bool operator!=(const name& a, const name& b){                 \
    return a.member_name != b.member_name;                              \
  }
#define gen_rel_ops_via_member(name, member_name, specifier)    \
  gen_rel_ops_via_member_generic(name, member_name, friend)

//same as above but only define '<' and '=='
#define gen_basic_rel_ops_via_member_generic(name, member_name, specifier) \
  specifier bool operator<(const name& a, const name& b){                  \
    return a.member_name < b.member_name;                               \
  }                                                                     \
  specifier bool operator==(const name& a, const name& b){                 \
    return a.member_name == b.member_name;                              \
  }
#define gen_basic_rel_ops_via_member(name, member_name, specifier) \
  gen_basic_rel_ops_via_member_generic(name, member_name, friend)
//Generate all 6 relational operators from a 3 way comparison operator
#define gen_rel_ops_via_compare(type1, type2, cmp)    \
  friend bool operator==(const type1& lhs, const type2 &rhs){           \
    return cmp(lhs,rhs) == 0;                                           \
  }                                                                     \
  friend bool operator!=(const type1& lhs, const type2 &rhs){           \
    return cmp(lhs,rhs) != 0;                                           \
  }                                                                     \
  friend bool operator< (const type1& lhs, const type2 &rhs){           \
    return cmp(lhs,rhs) <  0;                                           \
  }                                                                     \
  friend bool operator> (const type1& lhs, const type2 &rhs){           \
    return cmp(lhs,rhs) >  0;                                           \
  }                                                                     \
  friend bool operator<=(const type1& lhs, const type2 &rhs){           \
    return cmp(lhs,rhs) <= 0;                                           \
  }                                                                     \
  friend bool operator>=(const type1& lhs, const type2 &rhs){           \
    return cmp(lhs,rhs) >= 0;                                           \
  }
//same as above but only define '<' and '=='
#define gen_basic_rel_ops_via_compare_generic(type1, type2, cmp, specifier) \
  friend bool operator< (const type1& lhs, const type2 &rhs){           \
    return cmp(lhs,rhs) <  0;                                           \
  }                                                                     \
  friend bool operator==(const type1& lhs, const type2 &rhs){           \
    return cmp(lhs,rhs) == 0;                                           \
  }
//Generate all 6 relational operators from a 3 way comparison operator that
//is a member function of type1
#define generate_comparison_operators_via_compare_member(type1, type2, cmp) \
  friend bool operator==(const type1& lhs, const type2 &rhs){           \
    return lhs.cmp(rhs) == 0;                                           \
  }                                                                     \
  friend bool operator!=(const type1& lhs, const type2 &rhs){           \
    return lhs.cmp(rhs) != 0;                                           \
  }                                                                     \
  friend bool operator< (const type1& lhs, const type2 &rhs){           \
    return lhs.cmp(rhs) <  0;                                           \
  }                                                                     \
  friend bool operator> (const type1& lhs, const type2 &rhs){           \
    return lhs.cmp(rhs) >  0;                                           \
  }                                                                     \
  friend bool operator<=(const type1& lhs, const type2 &rhs){           \
    return lhs.cmp(rhs) <= 0;                                           \
  }                                                                     \
  friend bool operator>=(const type1& lhs, const type2 &rhs){           \
    return lhs.cmp(rhs) >= 0;                                           \
  }
//same as above but only define '<' and '=='
#define generate_basic_comparison_operators_via_compare_member(type1, type2, cmp) \
  friend bool operator< (const type1& lhs, const type2 &rhs){           \
    return lhs.cmp(rhs) <  0;                                           \
  }                                                                     \
  friend bool operator==(const type1& lhs, const type2 &rhs){           \
    return lhs.cmp(rhs) == 0;                                           \
  }
//Given operator[<,==](type1, type2) define operator[<,==](type2, type1)
#define generate_recip_rel_ops2(type1, type2, specifier)                \
  specifier bool operator==(const type2& a, const type1 &b){ return b == a; } \
  specifier bool operator<(const type2& a, const type1 &b){             \
    return !((a < b) || (b == a));                                      \
  }
//Generate >,!=,<=, >= for two types with defined < and == operators
#define generate_rel_ops_12_generic(type1, type2, specifier)            \
  specifier bool operator>(const type1& a, const type2& b){ return b < a; } \
  specifier bool operator<=(const type1& a, const type2& b){ return !(a > b); } \
  specifier bool operator!=(const type1& a, const type2& b){ return !(a == b); } \
  specifier bool operator>=(const type1& a, const type2& b){ return !(a < b); }
#define generate_rel_ops_21_generic(type1, type2, specifier)    \
  generate_rel_ops_12_generic(type2, type1, specifier)
#define generate_rel_ops2_generic(type1, type2, specifier)      \
  generate_rel_ops_12_generic(type1, type2, specifier)          \
  generate_rel_ops_21_generic(type1, type2, specifier)
#define generate_rel_ops1_generic(type1, specifier)     \
  generate_rel_ops_12_generic(type1, type1, specifier)
#define generate_friend_rel_ops1(type) generate_rel_ops1_generic(type, friend)
#define generate_inline_rel_ops1(type) generate_rel_ops1_generic(type, inline)
#define generate_friend_rel_ops2(type1,type2)           \
  generate_rel_ops2_generic(type1, type2, friend)
#define generate_inline_rel_ops2(type)                  \
  generate_rel_ops1_generic(type1, type2, inline)

#if 0
// From boost, uses templates and inheritance rather than macros
namespace util {
template <typename T> class empty_base {};
template <class T, class U, class B = empty_base<T>>
struct less_than_comparable2 : B {
  friend bool operator<=(const T& x, const U& y) { return !(x > y); }
  friend bool operator>=(const T& x, const U& y) { return !(x < y); }
  friend bool operator>(const U& x, const T& y)  { return y < x; }
  friend bool operator<(const U& x, const T& y)  { return y > x; }
  friend bool operator<=(const U& x, const T& y) { return !(y < x); }
  friend bool operator>=(const U& x, const T& y) { return !(y > x); }
};

template <class T, class B = empty_base<T>>
struct less_than_comparable1 : B {
  friend bool operator>(const T& x, const T& y)  { return y < x; }
  friend bool operator<=(const T& x, const T& y) { return !(y < x); }
  friend bool operator>=(const T& x, const T& y) { return !(x < y); }
};

template <class T, class U, class B = empty_base<T>>
struct equality_comparable2 : B {
  friend bool operator==(const U& y, const T& x) { return x == y; }
  friend bool operator!=(const U& y, const T& x) { return !(x == y); }
  friend bool operator!=(const T& y, const U& x) { return !(y == x); }
};

template <class T, class B = empty_base<T>>
struct equality_comparable1 : B {
  friend bool operator!=(const T& x, const T& y) { return !(x == y); }
};

template <class T, class U, class B = empty_base<T>>
struct totally_ordered2
  : less_than_comparable2<T, U, equality_comparable2<T, U, B>> {};

template <class T, class B = empty_base<T>>
struct totally_ordered1
  : less_than_comparable1<T, equality_comparable1<T, B>> {};

#define SAFE_TYPEDEF(T, D)                                      \
  struct D                                                      \
    : totally_ordered1<D, totally_ordered2<D, T>> {             \
    T t;                                                        \
    explicit D(const T& t_) : t(t_) {};                         \
    explicit D(T&& t_) : t(std::move(t_)) {};                   \
    D() = default;                                              \
    D(const D& t_) = default;                                   \
    D(D&&) = default;                                           \
    D& operator=(const D& rhs) = default;                       \
    D& operator=(D&&) = default;                                \
    operator T& () { return t; }                                \
    bool operator==(const D& rhs) const { return t == rhs.t; }  \
    bool operator<(const D& rhs) const { return t < rhs.t; }    \
  };
#endif 
//was in util.h, moved here for posterity.
#if 0
#define define_rel_ops_generic(T, qualifier)            \
  qualifier bool operator !=(const T& a, const T& b){   \
    return !(a == b);                                   \
  }                                                     \
  qualifier bool operator <=(const T& a, const T& b){   \
    return a == b || a < b;                             \
  }                                                     \
  qualifier bool operator >(const T& a, const T& b){    \
    return !(a <= b);                                   \
  }                                                     \
  qualifier bool operator >=(const T& a, const T& b){   \
    return !(a < b);                                    \
  }
#define define_rel_ops_in_class(T)              \
  define_rel_ops_generic(T, friend)
#endif
#define generate_binop_from_comp_assign_op(name, op)                    \
  friend name CAT(operator,op)(const name &lhs, const name &rhs){       \
    return name(lhs) CAT(op,=) rhs;                                     \
  }
//Requires: rhs unary operator-, lhs operator+=, lhs copy constructor
//Provides: binary operator-, operator-=, operator+
//Note: Will likely be less efficent than a manual implementation.
#define generate_additive_ops(lhs_type , rhs_type)                      \
  friend name operator+(const lhs_type &lhs, const rhs_type &rhs){      \
    return name(lhs) += rhs;                                            \
  }                                                                     \
  friend name& operator-=(lhs_type &lhs, const rhs_type &rhs){          \
    return lhs += (-rhs);                                               \
  }                                                                     \
  friend name operator-(const lhs_type &lhs, const rhs_type &rhs){      \
    return name(lhs) -= rhs;                                            \
  }                                                                     \

//Requires: operators +=, -=, *= and /=
//#define generate_arithmetic_binops(name)

//Oh boy is this ugly
//You could also do a partial specialization for just one type, but
//that seems like overkill
#define gen_binop_function_object(name, op)     \
  template<typename T = void, typename U = void>                        \
  struct name;                                                          \
  template<typename T, typename U>                                      \
  struct name {                                                         \
    using result_type = decltype(std::declval<T>() op std::declval<U>()); \
    constexpr result_type operator()(const T& lhs, const U& rhs) const { \
      return lhs op rhs;                                                \
    }                                                                   \
  };                                                                    \
  template<>                                                            \
  struct name<void,void> {                                              \
  template<typename T, typename U>                                      \
  constexpr auto operator()(T&& lhs, U&& rhs) const                     \
    noexcept(noexcept(std::forward<T>(lhs) op std::forward<U>(rhs))) \
    -> decltype(std::forward<T>(lhs) op std::forward<U>(rhs))           \
  { return std::forward<T>(lhs) op std::forward<U>(rhs); }              \
    using is_transparent = std::true_type;                              \
  };

/*
#define gen_binop_equal_function_object(name, op)       \
  template<typename T = void, typename U = void>        \
  struct name;                                          \
  template<typename T, typename U>                      \
  struct name {                                         \
    T& operator()(T& lhs, const U& rhs) const {         \
      return lhs op rhs;                                \
    }                                                   \
  };                                                    \
  template<>                                            \
  struct name<void,void> {                              \
    template<typename T, typename U>                    \
    T& operator()(T& lhs, U&& rhs) const                \
      noexcept(noexcept(lhs op std::forward<U>(rhs)))   \
    { return lhs op std::forward<U>(rhs); }             \
  };
*/

#endif
