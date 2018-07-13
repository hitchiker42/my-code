#ifndef __TEMPLATES_H__
#define __TEMPLATES_H__
#include <type_traits>
//Macros, most notably  macros for defining templates to test for the existance
//of a member function/variable.
#include "macros.h"
namespace util {
namespace templates {

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
  Functions for mapping over stl containers.
*/
/*
  I wish there was a nicer way to do this, but if there is I don't know it .
*/
define_has_member_function(reserve);
define_has_member_overloaded(push_back);
define_has_member_overloaded(insert);
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

//Reserve space if possible, do nothing if not
template<typename T,
         std::enable_if_t<!has_reserve_v<T>, int> = 0>
void maybe_reserve([[maybe_unused]] T &x, [[maybe_unused]] size_t sz){
  return;
}
template<typename T,
         std::enable_if_t<has_reserve_v<T>, int> = 0>
void maybe_reserve(T &x, size_t sz){
  x.reserve(sz);
}
/*
  Destructive map function
*/
/*
  Define both a pointer and reference version.
*/
template<class Container, class Fn>
void Fmap_into(const Fn &f, Container& seq){
  std::transform(seq.begin(), seq.end(), seq.begin(), f);
}
template<class Container, class Fn>
void Fmap_into(const Fn &f, Container* seq){
  std::transform(seq->begin(), seq->end(), seq->begin(), f);
}
template<class Container, class Fn>
void Fmapc(const Container &seq, Fn f){
  std::for_each(seq.begin(), seq.end(), f);
}
/*
  Functional map function (i.e non-destructive)
*/
//Version for sequence containers (eg vectors/lists)
template<class Fn, class T,
         typename
         std::enable_if_t<
           has_push_back_v<T, const typename T::value_type&>, int> = 0>
T Fmap(const Fn &f, const T &seq){
  T ret;
  //reserve the necessary space if possible
  maybe_reserve(ret, seq.size());
  std::transform(seq.begin(), seq.end(), std::back_inserter(ret), f);
  return ret;
}
//version for associative containers (eg map/set)
template<class Fn, class T,
         typename std::enable_if_t<
           !has_push_back<T, const typename T::value_type&>::value &&
           has_insert<T, const typename T::value_type&>::value, int> = 0>
T Fmap(const Fn &f, const T &seq){
  T ret;
  maybe_reserve(ret, seq.size());//likely does nothing
  std::transform(seq.begin(), seq.end(), std::inserter(ret, ret.end()), f);
  return ret;
}
//Compile time fold.
template<typename T, typename U = T>
constexpr T constexpr_foldl(const U *ptr, size_t n, const T init,
                            T(*f)(const T,const U)){
  return (n == 0 ? init : constexpr_foldl(ptr+1, n-1, f(init, *ptr), f));
}
template<typename T, typename U = T>
constexpr T constexpr_sum(const U *ptr, size_t n, const T init){
  return (n == 0 ? init : constexpr_sum(ptr+1, n-1, init + (*ptr)));
}
/*
  Version of std::find/find_if which returns an index rather than an iterator.
*/
template<class InputIt, class T>
ssize_t position(InputIt first, InputIt last, const T& value){
  ssize_t cnt = 0;
  for(;first != last; ++first){
    if(*first == value){
      return cnt;
    }
    ++cnt;
  }
  return -1;
}
template<class InputIt, class UnaryPredicate>
ssize_t position_if(InputIt first, InputIt last, UnaryPredicate p){
  ssize_t cnt = 0;
  for(;first != last; ++first){
    if(p(*first)){
      return cnt;
    }
    ++cnt;
  }
  return -1;
}
template<typename T, template <typename> class Container>
T find_or_default(const Container<T>& container, const T& val,
                  const T& deflt =  T()){
  auto it = std::find(container.begin(), container.end(), val);
  if(it == container.end()){
    return deflt;
  } else {
    return (*it);
  }
}
//Version which takes a container
template<class T, class U = typename T::value_type>
ssize_t position(const T& container, const U& value){
  return position(std::begin(container), std::end(container), value);
}
//version for initializer list
template<typename T>
ssize_t position(const std::initializer_list<T>&& lst, const T& value){
  return position(lst.begin(), lst.end(), value);
}
//Just a wrapper around std::find witch compares its return value to
//the end iterator and returns a boolean
template<typename T, typename U,
         std::enable_if_t<std::is_same_v<typename T::value_type,U>, int> = 0>
bool in_container(const T &seq, const U &val){
  return std::find(std::begin(seq), std::end(seq), val) != std::end(seq);
}
template<typename T, typename U,
         std::enable_if_t<!std::is_same_v<typename T::value_type,U>,
                          int> = 0>
bool in_container(const T &seq, const U &val){
  //if U != T::value_type try using the containers find method
  return seq.find(val) != seq.end();
}
template<typename InputIt,
         typename U = iter_traits_value_type<InputIt>>
bool in_sequence(const InputIt begin, const InputIt end, const U &val){
  return std::find(begin, end, val) != end;
}

//Simple templates
#if __cplusplus < 201703L
template<typename T>
constexpr const T clamp(const T value, const T lo, const T hi){
  return std::min(hi, std::max(value, lo));
}
template<typename T>
constexpr const T clamp(const T value, std::pair<const T, const T> lohi){
  return std::min(lohi.first, std::max(value, lohi.second));
}
#else
using std::clamp;
#endif
template<typename T>
constexpr bool in_range(const T val, const T min, const T max){
  return (val >= min && val <= max);
}
template<typename T>
constexpr std::pair<T,T> quot_rem(T num, T denom){
  return {num / denom, num % denom};
}
//Specialize for floating point numbers, since division is exact
//the remanider is always 0.
template<>
constexpr std::pair<double,double> quot_rem(double num, double denom){
  return {num / denom, 0.0};
}
template<>
constexpr std::pair<float,float> quot_rem(float num, float denom){
  return {num / denom, 0.0f};
}
/*
  This is a lot harder that you would think since signed integer
  overflow is undefined.
*/
#ifdef __GNUC__
template<typename T, typename U = T, typename V = T,
         std::enable_if_t<
           std::is_integral_v<std::common_type_t<T,U,V>>, int> = 0>
bool checked_add(T x, U y, V *sum){
  return __builtin_add_overflow(x, y, sum);
}
template<typename T, typename U = T, typename V = T,
         std::enable_if_t<
           std::is_integral_v<std::common_type_t<T,U,V>>, int> = 0>
bool checked_sub(T x, U y, V *sum){
  return __builtin_sub_overflow(x, y, sum);
}
template<typename T, typename U = T, typename V = T,
         std::enable_if_t<
           std::is_integral_v<std::common_type_t<T,U,V>>, int> = 0>
bool checked_mul(T x, U y, V *sum){
  return __builtin_mul_overflow(x, y, sum);
}
#else
/*
  Assumes 2s compliment representation, because I'm not crazy enough
  to try and write these functions for 1s compliment/sign and magintude. 

  These could be optimized, but I'm not going to optimize for non gcc 
  compatible compilers.
*/
template<typename T, typename U = T, typename V = T,
         std::enable_if_t<
           std::is_integral_v<std::common_type_t<T,U,V>>, int> = 0>
bool checked_add(T x_, U y_, V *sum){
  uintmax_t x = (uintmax_t)x_;
  uintmax_t y = (uintmax_t)y_;
  uintmax_t result = x + y;
  //This is always defined (although it may be implimentation defined)
  *sum = (V)result;
  //If we overflow a uintmax_t we'd overflow on any other type.
  if(result < x){    
    return true;
  } else {
    if(result > std::max((uintptr_t)std::numeric_limits<V>::min,
                         (uintptr_t)std::numeric_limits<V>::max)){
      return true;
    }
  }
  return false;
}
#endif
template<typename T,
         std::enable_if_t<std::is_signed<T>::value &&
                          std::is_integral<T>::value, int> = 0>
constexpr T sadd(const T a, const T b,
                 const T min = std::numeric_limits<T>::lowest(),
                 const T max = std::numeric_limits<T>::max()){
  T res;
  if(checked_add(a,b,&res)){
    return ((a < 0 && b < 0) ? min : max);
  } else {
    return std::clamp(*res, min, max);
  }
}
template<typename T,
         std::enable_if_t<std::is_unsigned<T>::value, int> = 0>
constexpr T sadd(const T a, const T b,
                 const T min = std::numeric_limits<T>::lowest(),//will be 0
                 const T max = std::numeric_limits<T>::max()){
  T sum = a + b;
  if(sum < std::max(a,b)){//check for overflow
    return max;
  } else {
    return clamp(sum, min, max);
  }
}
// template<typename T,
//          std::enable_if_t<std::is_signed<T>::value, int> = 0>
// constexpr T sadd(const T a, const T b,
//                  const T min = std::numeric_limits<T>::lowest(),
//                  const T max = std::numeric_limits<T>::max()){
//   //If you were just checking for over/underflow you should
//   //use < inplace of <=
//   if((a > 0 && b > 0) && (max - b <= a)){
//     return max;
//   } else if((a < 0 && b < 0) && (a <= min - b)){
//     return min;
//   } else {
//     //if min and max are the default this is identical to a + b;
//     return clamp(a + b,min,max);
//   }
// }
template<typename T,
         std::enable_if_t<std::is_unsigned<T>::value, int> = 0>
constexpr T ssub(const T a, const T b,
                 const T min = std::numeric_limits<T>::lowest(),
                 const T max = std::numeric_limits<T>::max()){
  //unsigned subtraction can't overflow
  return clamp(a - b, min, max);
}
template<typename T,
         std::enable_if_t<std::is_signed<T>::value, int> = 0>
constexpr T ssub(const T a, const T b,
                 const T min = std::numeric_limits<T>::lowest(),
                 const T max = std::numeric_limits<T>::max()){
  return sadd(a,-b,min,max);
}
//Specialization for unsigned types to avoid compiler warnings for
//always true comparisons
template<typename T>
constexpr T sgn(const enable_if_is(unsigned, T)& val){
  return (val > T(0));
}
template<typename T>
constexpr T sgn(const T& value){
  return (value >= T(0) ? value > T(0)  : -1);
}
//Compare lhs to rhs
//return -1 if lhs < rhs, 0 if lhs == rhs and 1 if lhs > rhs
template <typename T>
int three_way_compare(const T& lhs,const T& rhs){
  if(lhs < rhs) {
    return -1;
  } else if (lhs == rhs) {
    return 0;
  } else {
    return 1;
  }
}
/*
  Conditional C++ style formatted output.
  This also necssitates / provides a functional form of C++ formated output.
*/
template <typename Os, typename ...Args>
Os& stream_out(Os &stream, Args&&... args) {
  (stream << ... << args);
  return stream;
}
template <typename Os, typename ...Args>
Os& maybe_stream_out(bool conditional, Os &stream, Args&&... args) {
  if(conditional){
    (stream << ... << args);
  }
  return stream;
}
/*
  templates for working with enumerated types
*/
//Convert an enum to its underlying representation
template <typename T>
constexpr auto to_underlying(T e){
  return static_cast<std::underlying_type_t<T>>(e);
}



//taken from a part of city hash
static constexpr uint64_t hash_combine(uint64_t x, uint64_t y){
  constexpr uint64_t kMul = 0x9ddfea08eb382d69ULL;
  uint64_t a = (y ^ x) * kMul;
  a ^= (a >> 47);
  uint64_t b = (x ^ a) * kMul;
  b ^= (b >> 47);
  return b * kMul;
}
template <class T>
static inline void hash_combine(std::uint64_t *seed, const T& v){
  std::hash<T> hasher;
  (*seed) = hash_combine((*seed), hasher(v));
}
//32 bit version from boost
/*
template <class T>
static inline size_t hash_combine(std::size_t *seed, const T& v){
    std::hash<T> hasher;
    (*seed) ^= hasher(v) + 0x9e3779b9 + ((*seed)<<6) + (((*seed)>>2);
}
static constexpr size_t hash_combine(size_t x, size_t y){
  return (x ^ (y + 0x9e3779b9 + (x<<6) + (x>>2)));
}
*/
template<typename MapType, typename K,
         std::enable_if_t<
           std::is_convertible_v<K, typename MapType::key_type>, int> = 0>
auto find_or_null(const MapType& map, const K& key){
  auto it = map.find(key);
  return (it == map.end() ? nullptr : &(it->second));
}
template<typename MapType, typename K,
         std::enable_if_t<
           std::is_convertible_v<K, typename MapType::key_type>, int> = 0>

auto find_or_null(MapType& map, const K& key){
  auto it = map.find(key);
  return (it == map.end() ? nullptr : &(it->second));
}
template <typename MapType, typename K, typename V,
          std::enable_if_t<
            std::is_convertible_v<K, typename MapType::key_type> &&
            std::is_convertible_v<typename MapType::value_type, V>, int> = 0>
const V& find_or_default(const MapType &map,
                         const K &key, const V& deflt){
  auto it = map.find(key);
  return (it == map.end() ? deflt : it->second);
}
template<typename K, typename V>
auto hash_find_or_null(const std::unordered_map<K,V> &hash, const K &key){
  auto it = hash.find(key);
  return (it == hash.end() ? nullptr : &(it->second));
}
template<typename K, typename V>
auto hash_find_or_null(std::unordered_map<K,V> &hash, const K &key){
  auto it = hash.find(key);
  return (it == hash.end() ? nullptr : &(it->second));
}
template<typename K, typename V>
V hash_find_or_default(const std::unordered_map<K,V> &hash,
                       const K &key, const V deflt){
  auto it = hash.find(key);
  return (V)(it == hash.end() ? deflt : it->second);
}
//I need to make seperate templates for a multimap since
//you can't partially specialize function templates.
template<typename K, typename V>
auto hash_find_or_null(const std::unordered_multimap<K,V> &hash, const K &key){
  auto it = hash.find(key);
  return (it == hash.end() ? nullptr : &(it->second));
}
template<typename K, typename V>
auto hash_find_or_null(std::unordered_multimap<K,V> &hash, const K &key){
  auto it = hash.find(key);
  return (it == hash.end() ? nullptr : &(it->second));
}
template<typename K, typename V>
V hash_find_or_default(const std::unordered_multimap<K,V> &hash,
                       const K &key, const V deflt){
  auto it = hash.find(key);
  return (V)(it == hash.end() ? deflt : it->second);
}
template<typename K, typename V>
V* map_find_or_null(std::map<K,V> m, const K &key){
  auto it = m.find(key);
  if(it == m.end()){
    return nullptr;
  } else {
    return &(it->second);
  }
}
/*
  Searches the map for a element with the given key returns
  a pair of an iterator an a boolean, if the boolean is true
  the iterator pointes to the element with the given key, if
  it is false it points to the first element with a key greater
  that the given key, such that when passed to emplace_hint it will
  result in amortized constant time insertion.
*/
template<typename K, typename V>
std::pair<typename std::map<K,V>::iterator, bool>
map_find_location(const std::map<K,V> &m, const K& key){
  auto it = m.begin();
  auto cmp = m.key_comp();
  while(it != m.end()){
    if(!cmp(it->first, key)){//element is not less than key
      if(!cmp(key, it->first)){
        return {it, true};//we found the element
      } else {
        return {it, false};
      }
    }
  }
  return {it, false};//element is greater than all elements in the map
}

/*
  Returns a pair of bucket iterators, the first points to the
  first instance of key, the second is the end iterator of the bucket.
*/
template<typename K, typename V>
decltype(auto) //since the return type is a mess
unordered_multimap_find_first(std::unordered_multimap<K,V> &ht, const K& key){
  auto idx = ht.bucket(key);
  auto start = ht.begin(idx);
  auto stop = ht.end(idx);
  auto cmp = ht.key_eq();
  while(!cmp(start->first, key) && start != stop){
    ++start;
  }
  return std::make_pair(start,stop);
}
/*
  Returns the equivlent of:
  std::make_pair(ht.equal_range(key).first, ht.count(key));
  without having to iterate across the range multiple times.
*/
template<typename K, typename V>
decltype(auto)
unordered_multimap_equal_range_count(std::unordered_multimap<K,V> &ht, const K& key){
  auto [start, stop] = unordered_multimap_find_first(ht, key);
  decltype(ht.count(key)) count = 0;
  auto cmp = ht.key_eq();
  if(start != stop){
    auto next = start;
    do {
      ++count;
    } while(++next != stop && cmp(key, next->first));
  }
  return std::make_pair(start, count);
}
/*
  An allocator using malloc, does not initialize/construct anything
  nor does it destruct anything, using this with anything but POD
  types will cause serious issues.
*/
template <typename T>
struct malloc_allocator {
  using pointer = T*;
  using const_pointer = const T*;
  using void_pointer = void*;
  using const_void_pointer =const void* ;
  using value_type = T;
  using size_type = size_t;
  using difference_type = ptrdiff_t;
  //introduced in C++17
  using is_always_equal = std::true_type;
  static pointer allocate(size_type n){
    return (pointer)malloc(n * sizeof(value_type));
  }
  static void deallocate(pointer ptr, [[maybe_unused]] size_type n){
    free(ptr);
  }
  static pointer reallocate(pointer ptr, size_type n,
                            [[maybe_unused]] size_type old_length = -1,
                            [[maybe_unused]] size_type old_size = -1){
    return realloc(ptr, n*sizeof(value_type));
  }
  //does nothing
  template<class... Args>
  static void construct([[maybe_unused]] Args&&... args){
    return;
  }
  //also does nothing
  template<class... Args>
  static void destroy([[maybe_unused]] Args&&... args){
    return;
  }
};
/*
  An allocator using calloc, same as the above malloc_allocator but
  zeros all allocated memory.
*/
template <typename T>
struct calloc_allocator {
  using pointer = T*;
  using const_pointer = const T*;
  using void_pointer = void*;
  using const_void_pointer =const void* ;
  using value_type = T;
  using size_type = size_t;
  using difference_type = ptrdiff_t;
  //introduced in C++17
  using is_always_equal = std::true_type;
  static pointer allocate(size_type n){
    return (pointer)calloc(n,sizeof(value_type));
  }
  static void deallocate(pointer ptr, [[maybe_unused]] size_type n){
    free(ptr);
  }
  //Newly allocated memory will not be zeroed, if old_length 
  //is not explicitly given.
  static pointer reallocate(pointer ptr, size_type n,
                            [[maybe_unused]] size_type old_length = -1,
                            [[maybe_unused]] size_type old_size = -1){
    pointer ret =  realloc(ptr, n*sizeof(value_type));
    if(old_length != (size_type)-1){
      memset(ret + (old_length * sizeof(value_type)), '\0',
             (n * sizeof(value_type)) - (old_length * sizeof(value_type)));
    }
    return ret;
  }
  //does nothing
  template<class... Args>
  static void construct([[maybe_unused]] Args&&... args){
    return;
  }
  //also does nothing
  template<class... Args>
  static void destroy([[maybe_unused]] Args&&... args){
    return;
  }
};
template <typename T,
          std::enable_if_t<std::is_pod_v<T>, int> = 0>
using raw_vector = std::vector<T, malloc_allocator<T>>;

template <typename T>
T* typed_malloc(size_t sz){
  return (T*)malloc(sz);
}
template <typename T>
T* typed_calloc(size_t sz, size_t nmemb){
  return (T*)calloc(sz,nmemb);
}

//Templates to simplify placement new & related functions
template<class T, class... Ts>
T* placement_new(void *mem, Ts&&... Args){
  return new(mem) T(std::forward<Ts>(Args)...);
}
template<class T, class... Ts>
T* reconstruct(T& obj, Ts&&... Args){
  obj.~T();
  return new(&obj) T(std::forward<Ts>(Args)...);
}

// template <typename T>
// svector<T> vector_iota(T start, T stop, T step = T(1)){
//   T range = stop - start;
//   size_t len;
//   if constexpr (std::is_floating_point_v<T>){
//     len = (size_t)ceil(range/step);
//   } else {
//     len = (range/step) + (range % step ? 1 : 0);
//   }
//   T val;
//   size_t i;
//   svector<T> ret = svector<T>(len);
//   for(val = start, i = 0; val < stop && i < len; val += step, ++i){
//     ret[i] = val;
//   }
//   return ret;
// }
// template <typename T>
// svector<T> vector_iota(T stop){
//   return vector_iota(T(0), stop, T(1));
// }
/*
  if we define a template
  template<typename T>
  void foo(T&& t){
    for(auto x : t){
      do something with x;
    }
  }
  foo(std::vector({1,2,3})) //works
  foo<std::initializer_list<int>>({1,2,3}) //works
  foo({1,2,3}) //fails
  if we add
  template<typename T>
  void foo(std::initializer_list<T> &&t){
    for(auto x : t){
      do something with x;
    }
  }
  everything works fine, its best to define a template that implements the
  function then 2 templates to use, one for containers and one for
  initializer_lists.
*/
#define initializer_list_specialization(fn_name, impl_name)     \
  template<typename T>                                          \
  decltype(auto) fn_name(std::initializer_list<T>&& t){         \
    return impl_name(std::forward<std::initializer_list<T>> t); \
  }
#define container_specialization(fn_name, impl_name)     \
  template<typename T>                                          \
  decltype(auto) fn_name(T&& t){                                \
    return impl_name(std::forward<T> t);                        \
  }
//Delete copy constructor and copy assignment operator of class name
#define delete_copy_constructors(name)          \
  name& operator=(const name&) = delete;        \
  name(const name&) = delete;

#if (defined __GNUC__)
#define object_to_string(obj)                   \
  ({auto ss = std::stringstream();              \
    ss.operator<<(obj);                         \
    ss.str();})
#else
//Making this a template is a horrible idea, but if we can't use
//gnu extensions it'll have to do.
template <typename T>
std::string object_to_string(const T& obj){
  auto ss = std::stringstream();
  ss.operator<<(obj);
  return ss.str();
}
#endif
/*
  Variadic templates (parameter packs)
*/
//Extention of C++17 fold expresssions to arbitrary binary functions.
template <typename Fn, typename U>
U variadic_fold_impl(Fn &&f, U acc){
  return acc;
}
template <typename Fn, typename U, typename T>
U variadic_fold_impl(Fn &&f, U acc, T val){
    return f(acc, val);
}
template <typename Fn, typename U, typename T1, typename T2, typename ...Ts>
U variadic_fold_impl(Fn &&f, U acc, T1 val1, T2 val2, Ts&&... Rest){
  return variadic_fold_impl(f, f(acc, val1), val2, Rest...);
};
template <typename Fn, typename U, typename ...Ts,
          std::enable_if_t<std::is_convertible_v<
                           U, std::invoke_result_t<
                                Fn, U, std::common_type_t<Ts...>>>, int> = 0>
U variadic_fold(Fn &&f, U acc, Ts&& ... Args){
  return variadic_fold_impl(f, acc, Args...);
}
//special case for when sizeof...(Ts) == 0
template <typename Fn, typename U, typename ...Ts,
          std::enable_if_t<sizeof...(Ts) == 0, int> = 0>
U variadic_fold(Fn &&f, U acc, Ts&& ... Args){
  return variadic_fold_impl(f, acc, Args...);
}
template <typename Fn, typename T,
          std::enable_if_t<std::is_invocable_v<Fn, T>, int> = 0>
void variadic_for_each(Fn &&f, T val){
  f(val);
  return;
}
template <typename Fn, typename T, typename ...Ts,
          std::enable_if_t<std::is_invocable_v<Fn, T>, int> = 0>
void variadic_for_each(Fn &&f, T val, Ts&&... vals){
  f(val);
  return variadic_for_each_impl(f, vals...);
};
/*
  Useful generic function templates
*/
template<typename T>
const T identity(const T arg){
  return arg;
}
template<typename T>
T identity(T arg){
  return arg;
}
template<typename T, typename U,
         std::enable_if_t<std::is_convertible_v<T,U>, int> = 0>
const U convert(const T arg){
  return static_cast<U>(arg);
}
template<typename T, typename U, typename V>
std::function<T(V,U)> flip(const std::function<T(U,V)> &fn){
  return [&fn](V arg1, U arg2){return fn(arg2, arg1);};
}
/*
namespace util {
  namespace detail {
  template<typename F1, typename F2>
  struct composition {
    const F1 &f1;
    const F2 &f2;
    template<typename ... Ts>
    auto operator()(Ts&&... Args){
      return f1(f2(std::forward<Ts>(Args)...));
    }
  };
  };
}
template<typename F1, typename F2>
decltype(auto) compose(const F1& f1, const F2& f2){
  util::detail::composition<F1,F2> ret = {f1, f2};
  return std::function(ret);
}
*/
/*
  Iterators
*/
//Simple iterator wrappers for iterating over the keys/values of a map

template<class Map>
struct key_iter {
  //Iterator Traits
  typedef typename Map::difference_type difference_type;
  typedef const typename Map::key_type value_type;
  typedef value_type* pointer;
  typedef value_type& reference;
  typedef std::forward_iterator_tag iterator_category;
  typedef typename Map::iterator iterator_type;
  iterator_type it;
  key_iter() = default;
  explicit key_iter(iterator_type iter) : it{iter} {};

  key_iter& operator++(){//pre-increment
    ++it;
    return *this;
  }
  key_iter& operator++(int){//post increment
    key_iter ret = key_iter(it);
    ++it;
    return ret;
  }
  template<typename T>
  bool operator ==(const key_iter<T>&rhs){
    return it == rhs.it;
  }
  template<typename T>
  bool operator !=(const key_iter<T>&rhs){
    return it != rhs.it;
  }
  reference operator*(){
    return it->first;
  }
  pointer operator->(){
    return &(it->first);
  }
};
template<class Map>
struct value_iter {
  //Iterator Traits
  typedef typename Map::difference_type difference_type;
  typedef typename Map::mapped_type value_type;
  typedef value_type* pointer;
  typedef value_type& reference;
  typedef std::forward_iterator_tag iterator_category;
  typedef typename Map::iterator iterator_type;
  iterator_type it;
  value_iter() = default;
  explicit value_iter(iterator_type iter) : it{iter} {};

  value_iter& operator++(){//pre-increment
    ++it;
    return *this;
  }
  value_iter& operator++(int){//post increment
    value_iter ret = value_iter(it);
    ++it;
    return ret;
  }
  template<typename T>
  bool operator ==(const value_iter<T>&rhs){
    return it == rhs.it;
  }
  template<typename T>
  bool operator !=(const value_iter<T>&rhs){
    return it != rhs.it;
  }
  reference operator*(){
    return it->first;
  }
  pointer operator->(){
    return &(it->first);
  }
};

//Iterator adaptor which applies a function to the result of the underlying
//iterator.
template <class Fn, class It>
struct transform_iter {
  using difference_type = iter_traits_difference_type<It>;
  using input_type = iter_traits_value_type<It>;
  using output_type = std::invoke_result_t<Fn, decltype(*std::declval<It>())>;
  using value_type = std::remove_reference_t<output_type>;
  using pointer = std::add_pointer_t<value_type>;
  //Unless the function we're given returns a refernce this can't be an
  //output iterator, and the reference type needs to be const.
  using reference = output_type;
/*    std::conditional_t<std::is_reference_v<output_type>,
                       output_type,
                       std::add_lvalue_reference_t<
                         std::add_const_t<value_type>>>;*/
  using iterator_category = iter_traits_category<It>;

  transform_iter(Fn f, It iter)
    : f(f), iter(iter) {};
  Fn f;
  It iter;


  //Two transform_iters with different template parameters always compare unequal, should
  //they even be comparable?
  template<class It2, class Fn2,
           std::enable_if_t<std::is_same_v<It,It2> && std::is_same_v<Fn,Fn2>, int> = 0>
  friend bool operator==(const transform_iter &lhs, const transform_iter<It2,Fn2> &rhs){
    return (lhs.iter == rhs.iter) && (lhs.f == rhs.f);
  }
  template<class It2, class Fn2,
           std::enable_if_t<!std::is_same_v<It,It2> || !std::is_same_v<Fn,Fn2>, int> = 0>
  friend bool operator==([[maybe_unused]] const transform_iter &lhs,
                         [[maybe_unused]] const transform_iter<It2,Fn2> &rhs){
    return false;
  }
  template<class It2, class Fn2>
  friend bool operator!=(const transform_iter &lhs, const transform_iter<It2,Fn2> &rhs){
    return !(lhs == rhs);
  }
  //To avoid having to construct a seperate iterator in foreach loops we
  //define a != operator with the underlying It type
  friend bool operator!=(const transform_iter &lhs, const It& rhs){
    return lhs.iter != rhs;
  }

  reference operator*() const {
    return f(*iter);
  }
  pointer operator->() const {
    return &(f(*iter));
  }
  transform_iter& operator++(){
    ++iter;
    return (*this);
  }
  //Need to add a dummy type paramater to cause template type substitution
  //to take place and allow SFINAE, in order to avoid generating operators
  //that the iterator type doesn't support.
  template<typename T = iter_traits_category<It>,
           std::enable_if_t<std::is_base_of_v<std::forward_iterator_tag,
                                              T>, int> = 0>
  transform_iter& operator++(int){
    auto ret = *this;
    ++iter;
    return ret;
  }
//  template<std::enable_if_t<std::is_base_of_v<std::bidirectional_iterator_tag,
//                                              iter_traits_category<It>>, int> = 0>
  template<typename T = iter_traits_category<It>,
           std::enable_if_t<std::is_base_of_v<std::bidirectional_iterator_tag,
                                              T>, int> = 0>
  transform_iter& operator--(){
    --iter;
    return (*this);
  }
//  template<std::enable_if_t<std::is_base_of_v<std::bidirectional_iterator_tag,
//                                              iter_traits_category<It>>, int> = 0>
  template<typename T = iter_traits_category<It>,
           std::enable_if_t<std::is_base_of_v<std::bidirectional_iterator_tag,
                                              T>, int> = 0>
  transform_iter& operator--(int){
    auto ret = *this;
    --iter;
    return ret;
  }
//  template<std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag,
//                                              iter_traits_category<It>>, int> = 0>
  template<typename T = iter_traits_category<It>,
           std::enable_if_t<std::is_base_of_v<std::bidirectional_iterator_tag,
                                              T>, int> = 0>
  transform_iter& operator+=(difference_type n){
    iter += n;
    return (*this);
  }
//  template<std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag,
//                                              iter_traits_category<It>>, int> = 0>
  template<typename T = iter_traits_category<It>,
           std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag,
                                              T>, int> = 0>
  transform_iter& operator-=(difference_type n){
    iter -= n;
    return (*this);
  }
//  template<std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag,
//                                              iter_traits_category<It>>, int> = 0>
  template<typename T = iter_traits_category<It>,
           std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag,
                                              T>, int> = 0>
  transform_iter& operator+(difference_type n){
    auto ret = (*this);
    ret += n;
    return ret;
  }
//  template<std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag,
//                                              iter_traits_category<It>>, int> = 0>
  template<typename T = iter_traits_category<It>,
           std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag,
                                              T>, int> = 0>
  transform_iter& operator-(difference_type n){
    auto ret = (*this);
    ret -= n;
    return ret;
  }
  //Convience function to create a pair of iterators
  static std::pair<transform_iter,transform_iter>
  range(Fn f, It start, It stop) {
    return {transform_iter(f, start), transform_iter(f, stop)};
  }
};
//Merge this with my actual range class?
template <class Fn, class It>
struct transform_range {
  transform_range(Fn f, It start, It stop)
    : start(f, start), stop{stop} {};
  template<typename T>
  transform_range(Fn f, const T& container)
    : transform_range(f, std::begin(container), std::end(container)) {};
  transform_iter<Fn,It> start;
  It stop;
  transform_iter<Fn,It>& begin() { return start; }
  It& end() { return stop; }
};
//Template deduction guide for range via contianer
template <class Fn, typename T>
transform_range(Fn f, const T& container) ->
  transform_range<Fn, decltype(std::begin(container))>;
/*
template <typename Fn, typename T>
decltype(auto) transform_range(Fn fun, const T& val){
  using iter =  transform_iter<Fn, decltype(std::begin(val))>;
  return iter::range(fun, std::begin(val), std::end(val));
}
*/
//the complicated SFINAE stuff checks that calling Fn on *It returns a value
//than can be cast to a boolean.
template <class Pred, class It,
          std::enable_if_t<
            std::is_convertible_v<
              std::invoke_result_t<Pred, iter_traits_value_type<It>>,
              bool>, int> = 0>
struct filter_iter {
  using difference_type = iter_traits_difference_type<It>;
  using value_type = iter_traits_value_type<It>;
  using reference = iter_traits_reference<It>;
  using pointer = iter_traits_pointer<It>;
//  using iterator_category = iter_traits_category<It>;

  //this will be a bidirectional iterator if the original iterator was
  //random access, otherwise it will be the same as the original.
  using iterator_category = std::common_type_t<iter_traits_category<It>,
                                               std::bidirectional_iterator_tag>;

  //If T(*)(T) isn't convertible to Pred then p needs to be given explicitly.
  //end defaults to iter so that it's easy to construct an end iterator
  //ex: for(auto i = filter_iter(start, end); i != filter_iter(start); ++i);
/*  filter_iter(It iter, It end_iter, Pred pred = &identity<value_type>)
    : iter{iter}, end_iter{end_iter}, pred{pred} {
    satisfy_pred();
    };*/
  filter_iter(Pred pred, It iter)
    : filter_iter(pred, iter, iter) {};
  filter_iter(Pred pred, It iter, It end_iter)
    : iter{iter}, end_iter{end_iter}, pred{pred} {
      satisfy_pred();
    };

  It iter;
  It end_iter;
  Pred pred;

  template<class It2, class Pred2,
           std::enable_if_t<std::is_same_v<It,It2> &&
                            std::is_same_v<Pred,Pred2>, int> = 0>
  friend bool operator==(const filter_iter &lhs, const filter_iter<It2,Pred2> &rhs){
    return (lhs.iter == rhs.iter) && (lhs.f == rhs.f);
  }
  template<class It2, class Pred2,
           std::enable_if_t<!std::is_same_v<It,It2> ||
                            !std::is_same_v<Pred,Pred2>, int> = 0>
  friend bool operator==(const filter_iter &lhs, const filter_iter<It2,Pred2> &rhs){
    return false;
  }
  template<class It2, class Pred2>
  friend bool operator!=(const filter_iter &lhs, const filter_iter<It2,Pred2> &rhs){
    return !(lhs == rhs);
  }
  //foreach loops require comparing with an end value, to avoid constructing
  //a seperate iterator we can (since C++17) compare with end_iter
  friend bool operator!=(const filter_iter &lhs, const It& rhs){
    return (lhs.iter != rhs);
  }
  void satisfy_pred(){
    while((iter != end_iter) && !pred(*iter)){
      ++iter;
    }
  }
  void find_next(){
    ++iter;
    satisfy_pred();
  }
  //We don't store the starting iterator so if you try to decrment
  //an iterator for a sequence with no values for whech pred is true
  //then bad things will happen
  void find_prev(){
    while(!pred(*(--iter)));
  }

  reference operator*() const {
    return *iter;
  }
  pointer operator->() const {
    return &(*iter);
  }
  filter_iter& operator++(){
    find_next();
    return (*this);
  }
  template<std::enable_if_t<std::is_base_of_v<std::forward_iterator_tag,
                                              iter_traits_category<It>>, int> = 0>
  filter_iter& operator++(int){
    auto ret = *this;
    ++(*this);
    return ret;
  }
  template<std::enable_if_t<std::is_base_of_v<std::bidirectional_iterator_tag,
                                              iter_traits_category<It>>, int> = 0>
  filter_iter& operator--(){
    find_prev();
    return (*this);
  }
  template<std::enable_if_t<std::is_base_of_v<std::bidirectional_iterator_tag,
                                              iter_traits_category<It>>, int> = 0>
  filter_iter& operator--(int){
    auto ret = *this;
    --(*this);
    return ret;
  }
  //begin and end functions to allow this to be used in for each loops
  filter_iter& begin(){
    return *this;
  }
  //Since C++17 the end expression doesn't need to be the same type as the
  //begin expression, so we can avoid constructing a seprate iterator.
  It& end(){
    return end_iter;
  }
};
template <class Map>
decltype(auto) value_iterator(const typename Map::Iterator& m){
  return transform_iter([](auto& p){ return p.second; }, m);
}
template <class Map>
decltype(auto) value_iterator_range(const Map& m){
  return transform_range([](auto& p){ return p.second; }, m.begin(), m.end());
}
template <class Map>
decltype(auto) key_iterator(const typename Map::Iterator& m){
  using rettype = typename Map::value_type::first_type;
  return transform_iter([](auto& p)->const rettype& { return p.first; }, m);
}
template <class Map>
decltype(auto) key_iterator_range(const Map& m){
  using rettype = typename Map::value_type::first_type;
  return transform_range([](auto& p)->const rettype& { return p.first; },
                         m.begin(), m.end());
}
// template<typename F>
// struct fix_type {
//   F fn;
//   template<typename ...Args>
//   decltype(auto) operator()(Args&&... args){
//     return F(*this,std::forward<Args>(args)...);
//   }
// }

namespace details {
  template<class>
  struct is_ref_wrapper : std::false_type {};
  template<class T>
  struct is_ref_wrapper<std::reference_wrapper<T>> : std::true_type {};

  template<class T>
  using not_ref_wrapper = std::negation<is_ref_wrapper<std::decay_t<T>>>;

  template <class D, class...>
  struct return_type_helper { using type = D; };
  template <class... Types>
  struct return_type_helper<void, Types...> : std::common_type<Types...> {
      static_assert(std::conjunction_v<not_ref_wrapper<Types>...>,
                    "Types cannot contain reference_wrappers when D is void");
  };

  template <class D, class... Types>
  using return_type = std::array<typename return_type_helper<D, Types...>::type,
                                 sizeof...(Types)>;
}
template <class D = void, class... Types>
constexpr details::return_type<D, Types...> make_array(Types&&... t) {
  return {std::forward<Types>(t)... };
}
}
using namespace util::templates;
}//namespace util
//originally all the above functions were in the global namespace,
//and I'm fine with them staying there, it's just useful to have them
//in a namespace as well. In generally 'using namespace XXX' is not a good idea.
using namespace util::templates;

#endif /* __TEMPLATES_H__ */
