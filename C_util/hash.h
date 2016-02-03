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
typedef uint64_t(*hash_fun_t)(const void*,size_t);
typedef int(*hash_cmp_fun_t)(const void*,const void*,size_t,size_t);
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
  Add an entry to hash table ht, true if a value was added, otherwise false.
*/
int hashtable_add(struct hashtable *ht,
                  void *key, size_t key_sz, void *value);
int hashtable_add_string(struct hashtable *ht, char *key, void *value);
/*
  Add an entry to hashtable ht, or set the value of a prexisting entry.
  Returns the old value (or NULL if there was no old value)
*/
void* hashtable_set(struct hashtable *ht,
                    void *key, size_t key_sz, void *value);
void* hashtable_set_string(struct hashtable *ht, char *key, void *value);
/*
  Set the value of key in hashtable ht to the result of calling 
  update_fun with the current value of key. This is really only
  useful for the atomic hashtable (other than optimization).
  A simple example would be if the value of each key was a counter
  (i.e counting word frequency). In a single threaded program you
  could read the value, increment it and then set it. With the atomic
  version the value could change after reading it, in which case
  the new value would need to be recalculated. To do this you would
  call this with a function like: void*inc(void*val){return val+1;}.
*/
void* hashtable_update(struct hashtable *ht,
                    void *key, size_t key_sz, void *(*update_fun)(void*));
void* hashtable_set_string(struct hashtable *ht, char *key, 
                           void *(*update_fun)(void*));
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
void* hashtable_find(struct hashtable *ht, void *key, size_t sz);
void* hashtable_find_string(struct hashtable *ht, char *str);
/*
  Call the provided function on every key/value pair in the hashtable ht.
  The key shouldn't be modified, and any changes to the value (assuming
  it's actually a pointer) will modify the value in the table, so be
  careful.

  A note of warning, when the table needs to be rehashed the thread
  doing the rehash waits for all other threads to finish using a spinlock.
  Most functions are quick enough that this isn't a problem. But this
  one can take a while, just keep that in mind.
*/
void hashtable_iter(struct hashtable *ht, void(*fun)(void*,size_t,void*));
//It is not safe for any thread to start using the table after this
//function has been called. Threads using the table at the time this
//function is called are fine.
void destroy_hashtable(struct hashtable *ht);
#ifdef __cplusplus
}
#endif
#endif /* __HASH_H__ */
