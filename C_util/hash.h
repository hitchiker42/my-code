#ifndef __HASH_H__
#define __HASH_H__
#ifdef __cplusplus
extern "C" {
#endif
//#define ATOMIC_HASHTABLE
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
  atomic_int32_t rehash_pending;
  atomic_int32_t num_threads_using;
  atomic_int32_t num_threads_waiting;
  sem_t sem;
  void *freelist;
#endif
};
struct hashtable *make_hashtable(uint32_t size, float load_factor,
                                 hash_fun_t hash, hash_cmp_fun_t cmp);
/*
  The default size is 128, default load factor is 0.8,
  default hash function is fnv hash, and default comparison
  function is a wrapper around memcmp.
*/
struct hashtable *make_hashtable_default();
/*
  If key is already in the table return its value, otherwise set the
  value of key to the given value.
*/
void* hashtable_find_add(struct hashtable *ht,
                    void *key, size_t key_sz, void *value);
static void * hashtable_find_add_string(struct hashtable *ht,
                                        char *str, void *value){
  return hashtable_find_add(ht, str, strlen(str), value);
}
/*
  Add an entry to hash table ht, true if a value was added, otherwise false.
  Just a wrapper around hashtable_find_add. Hash table find add will
  return NULL for an existing entry with a null value and for a non
  existing entry. This will always tell you if you added an entry
  or not.
*/
int hashtable_add(struct hashtable *ht, void *key,
                  size_t keylen, void *value);
static int hashtable_add_string(struct hashtable *ht,
                                char *str, void *value){
  return hashtable_add(ht, str, strlen(str), value);
}
/*
  Add an entry to hashtable ht, or set the value of a prexisting entry.
  Returns the old value (or NULL if there was no old value)
*/
void* hashtable_set(struct hashtable *ht,
                    void *key, size_t key_sz, void *value);
static void* hashtable_set_string(struct hashtable *ht,
                                  char *str, void *value){
  return hashtable_set(ht, str, strlen(str), value);
}
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
static void* hashtable_update_string(struct hashtable *ht, char *key,
                                     void *(*update_fun)(void*)){
  return hashtable_update(ht, key, strlen(key), update_fun);
}
/*
  Remove an entry from hash table ht, if it is in the hashtable
  when this function is called.
*/
void* hashtable_remove(struct hashtable *ht, void *key, size_t key_sz);
static void* hashtable_remove_string(struct hashtable *ht, char *str){
  return hashtable_remove(ht, str, strlen(str));
}
/*
  Return the value of key has in the hashtable ht at the time
  this function is called. It may have a different value when the
  function returns.
*/
void* hashtable_find(struct hashtable *ht, void *key, size_t sz);
static void* hashtable_find_string(struct hashtable *ht, char *str){
  return hashtable_find(ht, str, strlen(str));
}
/*
  Similar to hashtable_find, but mearly checks for existance, resolves
  the ambiguity of a NULL return from hashtable_find. For an atomic hashtable
  the return value may not be accurate.
*/
int hashtable_exists(struct hashtable *ht, void *key, size_t sz);
static int hashtable_exists_string(struct hashtable *ht, char *str){
  return hashtable_exists(ht, str, strlen(str));
}
/*
  Call the provided function on every key/value pair in the hashtable ht.
  The key shouldn't be modified, and any changes to the value (assuming
  it's actually a pointer) will modify the value in the table, so be
  careful.

  If called in a multithreaded context some sort of external locking
  should probably be used. When the table needs to rehash it uses a spinlock
  to wait for all threads to finish using the table, since this function is
  O(N) whereas all the others are O(1) it could cause a lot of wasted cycles.
  It will still work, but it'll be awful for performance

  fun has the prototype: 
    void fun(void *key, size_t key_sz, void *value, void *userdata);
*/
void hashtable_iter(struct hashtable *ht, 
                    void(*fun)(void*,size_t,void*,void*), void *userdata);
//These work the way you'd expect
void keys_iter(struct hashtable *ht,
               void(*fun)(void*,size_t,void*), void *userdata);
void values_iter(struct hashtable *ht,
               void(*fun)(void*, void*), void *userdata);
/*
  Print the number of entries in each bucket in ht to out.
  Useful for profiling hash functions.

  The same warning as hashtable_iter applies to this, also bucket
  counts may be inaccurate if the the hashtable is in use when
  this function is running.
*/
void hashtable_bucket_counts(struct hashtable *ht, FILE *out);
//It is not safe for any thread to start using the table after this
//function has been called. Threads using the table at the time this
//function is called are fine.
void destroy_hashtable(struct hashtable *ht);
#ifdef __cplusplus
}
#endif
#endif /* __HASH_H__ */
