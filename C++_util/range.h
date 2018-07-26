#ifndef __RANGE_H__
#define __RANGE_H__
#include <iterator>
#include <algorithm>
#include <type_traits>
#include "templates.h"
namespace util {
//Range class, structurally it's just a pair of iterators, but thank's
//to c++17's template class deduction rules any object which can be
//passed to std::begin and std::end can be implicitly converted to a range.
template <class It>
struct range {
  //type aliases for templates.
  using value_type = iter_traits_value_type<It>;
  using iter_type = It;
  It start;
  It stop;
  range(It start, It stop)
    : start{start}, stop{stop} {};
  range(std::pair<It,It> start_stop)
    : start{start_stop.first}, stop{start_stop.second} {};
  template<typename T>
  range(const T& container)
    : range(std::begin(container), std::end(container)) {};

  It begin(){ return start; }
  const It begin() const {return const_cast<const It>(start); }

  It end(){ return stop; }
  const It end() const {return const_cast<const It>(stop); }
};
//template deduction rule for ranges from containers.
template <typename T>
range(const T& container) ->
  range<decltype(std::begin(container))>;

//Range wrappers around std algorithms.
template<class Range, class T = typename Range::value_type>
auto find(const Range r, const T& what) {
  return std::find(r.begin(), r.end(), what);
}
template<class Range, class T = typename Range::value_type>
T find_default(const Range r, const T& value,
               const T& deflt = T()){
  auto it = std::find(r.begin(), r.end(), value);
  if(it == r.end()){
    return deflt;
  } else {
    return (*it);
  }
}
template<class Range, class Generator,
         std::enable_if_t<
           std::is_invocable_r_v<Range::value_type,
                                 Generator>, int> = 0>
void generate(Range r, const Generator &gen){
  std::generate(r.begin(), r.end(), gen);
}
template<class Range, class Fn,
         std::enable_if_t<
           std::is_invocable_r_v<Range::value_type,
                                 Fn, Range::value_type>, int> = 0>
void fmap(const Fn &f, Range r){
  std::transform(r.begin(), r.end(), r.begin(), f);
}
template<class Range, class Fn,
         std::enable_if_t<
           std::is_invocable_r_v<Range::value_type,
                                 Fn, Range::value_type>, int> = 0>
void fmap(const Fn &f, const Range in, Range out){
  std::transform(in.begin(), in.end(), out.begin(), f);
}
template<class Range, class Fn, class OutputIt>
void fmap(const Fn &f, const Range in, OutputIt out){
  std::transform(in.begin(), in.end(), out, f);
}
template<class Range, class Fn, class U = typename Range::value_type,
         std::enable_if_t<std::is_invocable_r_v<U, Fn, Range::value_type>, int> = 0>
U reduce(const Fn &f, const Range in){
  return std::transform(in.begin(0, in.end(), f));
}
}
#endif /* __RANGE_H__ */
