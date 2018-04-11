#ifndef _OPERATORS_H_
#define _OPERATORS_H_

/*
  Macros to automatically generate various operators.
  Generally the operators are defined as friend functions, but they can
  be made non-member functions by doing
#define friend inline
macro goes here
#undef friend
even if this sorta cheating.
*/

#define gen_enum_binop(T,op)                            \
  inline T operator op(T lhs, T rhs){                   \
    return T(to_underlying(lhs) op to_underlying(rhs)); \
  }
#define gen_enum_binop_equals(T,op)                             \
  inline T& operator op(T& lhs, T rhs){                         \
    return (lhs = T(to_underlying(lhs) op to_underlying(rhs))); \
  }
#define gen_enum_unop(T,op)                     \
  inline T operator op(T val){                  \
    return T(op(to_underlying(val)));           \
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
  gen_enum_binop(E, &);                            \
  gen_enum_binop(E, |);                            \
  gen_enum_unop(E, ~)

//Generate == / != operators which do bytewise comparison.
#define generate_bytewise_equality_operators(name)               \
  friend bool operator==(const name &lhs, const name &rhs){     \
    return !memcmp(&lhs, &rhs, sizeof(name));                   \
  }                                                             \
  friend bool operator!=(const name &lhs, const name &rhs){     \
    return memcmp(&lhs, &rhs, sizeof(name));                    \
  }

//Generate a set of comparison operators for 'name' which compare
//two 'name's by comparing the member varible 'member_name'
#define generate_comparison_operators_via_member(name, member_name)     \
  friend bool operator<(const name& a, const name& b){                  \
    return a.member_name < b.member_name;                               \
  }                                                                     \
  friend bool operator==(const name& a, const name& b){                 \
    return a.member_name == b.member_name;                              \
  }                                                                     \
  friend bool operator<=(const name& a, const name& b){                 \
    return a.member_name <= b.member_name;                              \
  }                                                                     \
  friend bool operator>=(const name& a, const name& b){                 \
    return a.member_name >= b.member_name;                              \
  }                                                                     \
  friend bool operator>(const name& a, const name& b){                  \
    return a.member_name > b.member_name;                               \
  }                                                                     \
  friend bool operator!=(const name& a, const name& b){                 \
    return a.member_name != b.member_name;                              \
  }
//Generate all 6 relational operators from a 3 way comparison operator
#define generate_comparison_operators_via_compare(name, cmp)    \
  friend bool operator==(const name& lhs, const name& rhs){     \
    return cmp(lhs,rhs) == 0;                                   \
  }                                                             \
  friend bool operator!=(const name& lhs, const name& rhs){     \
    return cmp(lhs,rhs) != 0;                                   \
  }                                                             \
  friend bool operator< (const name& lhs, const name& rhs){     \
    return cmp(lhs,rhs) <  0;                                   \
  }                                                             \
  friend bool operator> (const name& lhs, const name& rhs){     \
    return cmp(lhs,rhs) >  0;                                   \
  }                                                             \
  friend bool operator<=(const name& lhs, const name& rhs){     \
    return cmp(lhs,rhs) <= 0;                                   \
  }                                                             \
  friend bool operator>=(const name& lhs, const name& rhs){     \
    return cmp(lhs,rhs) >= 0;                                   \
  }
//Requires: operator< and operator==
//Provides: operator>, operator<=, operator>=, operator!=
#define generate_rel_ops(name)                                          \
  friend bool operator>(const name& a, const name& b){                  \
    return !((a < b) || (a == b));                                      \
  }                                                                     \
  friend bool operator<=(const name& a, const name& b){                 \
    return !(a > b);                                                    \
  }                                                                     \
  friend bool operator>=(const name& a, const name& b){                 \
    return !(a < b);                                                    \
  }                                                                     \
  friend bool operator!=(const name& a, const name& b){                 \
    return !(a == b);                                                   \
  }
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
