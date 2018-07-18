#ifndef __HASHTABLE_H__
#define __HASHTABLE_H__
namespace util {
inline constexpr size_t prime_list_size =
  (sizeof(unsigned long) == 8 ? 256 + 48 : 256) + 1;
extern const std::array<unsigned long, prime_list_size> prime_list;
template<typename T>
struct hash : std::hash<T> {}
template<>
struct hash<void> {
  template <typename T>
  constexpr auto operator()(T&& t) const
    -> decltype(std::invoke_result_t<std::hash<T>, T>) {
     return std::hash<T>()(std::forward<T>(t)); 
  }
};

/*
  Hashtable using robin hood hashing.
*/
template<typename K, typename V,
         class HashFn = std::hash<K>,
         class KeyCmp = std::equal_to<>,
         class Allocator = std::allocator< std::pair<const Key, T>>>
struct HashTable : Allocator {
  using hash_code_t = std::invoke_result_t<HashFn, K>;
  using size_type = size_t;
  using difference_type = ptrdiff_t;
  using key_type = K;
  using value_type = V;
  struct HashEntry {
    K key;
    V value;
    hash_code_t hash_value;
  };
  
} 
}   
#endif /* __HASHTABLE_H__ */
