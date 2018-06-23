#ifndef __HASHTABLE_H__
#define __HASHTABLE_H__
namespace Util {
template<typename K, typename v>
struct HashEntry {
  K key;
  V value;
  uint64_t hash_value;
  hash_entry *next;
};
template<typename K, typename V>
struct HashTable {
  HashEntry<K,V> **data;
  struct iterator {
    e
    HashEntry<K,V> *ptr;
    
} 
}   
#endif /* __HASHTABLE_H__ */
