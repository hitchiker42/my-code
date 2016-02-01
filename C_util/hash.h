#ifndef __HASH_H__
#define __HASH_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <stdlib.h>
#include <stdint.h>
#ifdef ATOMIC_HASHTABLE
#include "atomic.h"
#include <semaphore.h>
#endif
typedef uint64_t(*hash_fun_t)(void*,size_t);
typedef int(*hash_cmp_fun_t)(void*,void*,size_t,size_t);
struct hashtable {
  struct hash_entry **table;
  uint32_t size;//number of elements in table
  uint32_t entries;//number of actual hash entries
  hash_fun_t hash;
  hash_cmp_fun_t cmp;
  float load_factor;
#ifdef ATOMIC_HASHTABLE
  sem_t sem;
  void *freelist;
  atomic_int32_t rehash_pending;
  atomic_int32_t num_threads_using;  
#endif
};
struct hashtable *make_hashtable(uint32_t size, float load_factor,
                                 hash_fun_t hash, hash_cmp_fun_t cmp);
struct hashtable *make_hashtable_default();
/*
  Add an entry to hash table ht, returns value if a new entry was added,
  or the value of key in the table if one already existed.
*/
int hashtable_add(struct hashtable *ht, 
                  void *key, size_t key_sz, void *value);
int hashtable_add_string(struct hashtable *ht, char *key, void *value);
/*
  Add an entry to hashtable ht, or set the value of a prexisting entry.
*/
void* hashtable_set(struct hashtable *ht, 
                    void *key, size_t key_sz, void *value);
void* hashtable_set_string(struct hashtable *ht, char *key, void *value);
/*
  Remove an entry from hash table ht, if it is in the hashtable
  when this function is called.
*/
void* hashtable_remove(struct hashtable *ht, void *key, size_t key_sz);
void* hashtable_remove_string(struct hashtable *ht, char *str);
/*
  Return the value of key has in the hashtable ht at the time
  this function is called. It may have a different value when the
  function returns.
*/
void* hashtable_find(struct hashtable *ht, void *data, size_t sz);
void* hashtable_find_string(struct hashtable *ht, char *str);
//It is not safe for any thread to start using the table after this 
//function has been called. Threads using the table at the time this
//function is called are fine.
void destroy_hashtable(struct hashtable *ht);
#ifdef __cplusplus
}
#endif
#endif /* __HASH_H__ */
