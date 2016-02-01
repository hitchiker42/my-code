#include "hash.h"
#include <stdio.h>
/*
  Implementation of a basic hashtable, with optional atomicity.

  Most write operations on the hash table can be done atomically using
  cmpxchg, only a rehash requires fully locking the table.
*/

typedef struct hashtable htable;
typedef struct hash_entry hentry;
struct hash_entry {
  void *key;
  size_t key_sz;
  uint64_t hv;
  void *value;
  struct hash_entry *next;
};
static void *xmalloc(size_t sz){
  void *mem = calloc(sz, 1);
  if(!mem && sz != 0){
    fprintf(stderr, "Error, out of memory\n");
    abort();
  }
  return mem;
}
#ifndef ATOMIC_HASHTABLE
#define atomic_inc(ptr)
#define atomic_dec(ptr)
#define atomic_cmpxchg(a,b,c)
#define atomic_compare_exchange(a,b,c)
#define atomic_load(ptr) *ptr
#define inc_hashtable_threadcount(ht)
#else
/*
  Needed for freelist. We use a freelist so we don't need to lock the
  table to remove an entry.
*/
typedef struct freelist_entry fl_entry;
struct freelist_entry {
  void *data;
  fl_entry *next;
};
static inline void freelist_push(struct hashtable *ht, void *data);
static inline void* freelist_pop(struct hashtable *ht);
static inline void* freelist_pop_unsafe(htable *ht);
static inline void inc_hashtable_threadcount(htable *ht){
  /*
    We need to increment the thread count before testing if a rehash is
    pending, then if there is a rehash pending we need to decrement it
    to avoid a deadlock, then increment it again when the rehash is done.

    We can't just increment the thread count after checking for a rehash
    because there is a (really tiny) possibility that a rehash could start
    between the two instructions.
   */
  atomic_inc(&ht->num_threads_using);
  if(atomic_load(&ht->rehash_pending)){
    atomic_dec(&ht->num_threads_using);
    sem_wait(&ht->sem);
    atomic_inc(&ht->num_threads_using);
  }
  return;
}
#endif

static inline hentry* make_hentry(void *key, size_t key_sz, uint64_t hv,
                                  void *value, hentry *next){
  hentry *new_entry = xmalloc(sizeof(hentry));
  new_entry->key_sz = key_sz;
  new_entry->key = key;
  new_entry->hv = hv;
  new_entry->next = next;
  new_entry->value = value;
  return new_entry;
}
static void rehash(htable *ht);
static void maybe_rehash(htable *ht);

#define fnv_prime_64 1099511628211UL
#define fnv_offset_basis_64 14695981039346656037UL
static uint64_t fnv_hash(const void *key,int keylen){
  const uint8_t *raw_data=(const uint8_t *)key;
  int i;
  uint64_t hash=fnv_offset_basis_64;
  for(i=0; i < keylen; i++){
    hash = (hash ^ raw_data[i])*fnv_prime_64;
  }
  return hash;
}
static int bool_memcmp(void *x, void *y, size_t x_sz, size_t y_sz){
  if(x_sz != y_sz){
    return 0;
  } else {
    return !memcmp(x,y,x_sz);
  }
}
htable *make_hashtable(uint32_t size, float load_factor, 
                       hash_fun_t hash, hash_cmp_fun_t cmp){
  htable *ht = xmalloc(sizeof(htable));
  size = size ? size : 128;//default size of 128
  load_factor = load_factor ? load_factor : 0.8;
  ht->size = size;
  ht->hash = hash ? hash : fnv_hash;
  ht->cmp = cmp ? cmp : bool_memcmp;
  ht->table = xmalloc(size*sizeof(hentry*));
#ifdef ATOMIC_HASHTABLE
  sem_init(&ht->sem, 0, 0);
#endif
  return ht;
}
htable *make_hashtable_default(){
  return make_hashtable(0,0,NULL,NULL);
}
static void rehash_internal(htable *ht){
  uint32_t new_size = ht->size*2;
  hentry **new_table = xmalloc(new_size*sizeof(hentry*));
  int i;
  for(i=0;i<ht->size;i++){
    hentry *entry = ht->table[i];
    while(entry){
      int index = entry->hv % new_size;
      hentry *next = entry->next;
      if(new_table[index]){
        entry->next = new_table[index];
      }
      new_table[index] = entry;
      entry = next;
    }
  }
  free(ht->table);
  ht->size = new_size;
  ht->table = new_table;
}
#ifdef ATOMIC_HASHTABLE
static void rehash(htable *ht){
  if(!(atomic_cmpxchg(&ht->rehash_pending, 0, 1))){
    return;//another thread is already doing the rehash
  }
  //spin until no other threads are using the table
  while(atomic_load(&ht->num_threads_using)){
    spin_pause();
  }
  rehash_internal(ht);
  void *temp;
  while((temp = freelist_pop_unsafe(ht))){
    free(temp);
  }
  ht->rehash_pending = 0;
  int cnt;
  sem_getval(&ht->sem, &cnt);
  while(cnt--){
    sem_post(&ht->sem);
  }
}
#else
static void rehash(htable *ht){
  rehash_internal(ht);
}
static void maybe_rehash(htable *ht){
  //we make a check for a rehash which will usually catch
  //a pending rehash, but we also do a cmpxchg to actually set the
  //flag, meaning this check can be non-atomic for speed.
  if(((float)ht->entries/(float)ht->size) > ht->load_factor &&
     !ht->rehash_pending){
    rehash(ht);
  }
}
#endif

void* hashtable_add(htable *ht, void *key, size_t key_sz, void *value){
  inc_hashtable_threadcount(ht);
  uint64_t hv = ht->hash(key, key_sz);
  uint32_t index = hv % ht->size;
  /*
    bucket = head of linked list, entry = current entry
  */
  hentry *new_entry = NULL;
  hentry *entry, *bucket;
  bucket = ht->table[index];
#ifdef ATOMIC_HASHTABLE
  //use a label to indicate this isn't a normal loop
 retry:
  entry = bucket;
  while(entry){
    if(entry->hv == hv){
      if(ht->cmp(key, entry->key, key_sz, entry->key_sz)){
        atomic_dec(&ht->num_threads_using);
        return entry->value;
      }
    }
    entry = entry->next;
  }
  if(!new_entry){
    new_entry = make_hentry(key, key_sz, hv, value, bucket);
  } else {
    new_entry->next = bucket;
  }
  if(atomic_compare_exchange(ht->table+index, &bucket, new_entry)){
    atomic_inc(&ht->entries);
    atomic_dec(&ht->num_threads_using);
    maybe_rehash(ht);
    return value;
  }
  goto retry;
#else
  while(entry){
    if(entry->hv == hv){
      if(ht->cmp(key, entry->key, key_sz, entry->key_sz)){
        return entry->value;
      }
    }
    entry = entry->next;
  }
  hentry *new_entry = make_hentry(key, key_sz, hv, value, bucket);
  ht->table[index] = new_entry;
  ht->entries++;
  return value;
#endif
}
/*
  with an atomic hash table this function can return NULL
  even if the key is in the table, and non-NULL even if it is not
  (but only in the case where key was in the table, but removed).
*/
void* hashtable_find(htable *ht, void *key, size_t key_sz){
  uint64_t hv = ht->hash(key, key_sz);
  uint32_t index = hv % ht->size;
  inc_hashtable_threadcount(ht);
  /*
    bucket = head of linked list, entry = current entry
   */
  hentry *entry, *bucket;
  bucket = ht->table[index];
  entry = bucket;
  while(entry){
    if(entry->hv == hv){
      if(ht->cmp(key, entry->key, key_sz, entry->key_sz)){
        atomic_dec(&ht->num_threads_using);
        return entry->value;
      }
    }
    entry = entry->next;
  }
  return NULL;
}
#ifdef ATOMIC_HASHTABLE
void *hashtable_remove(htable *ht, void *key, size_t key_sz){
  void *ret = NULL;
  inc_hashtable_threadcount(ht);
  uint64_t hv = ht->hash(key, key_sz);
  uint32_t index = hv % ht->size;
  hentry *entry, *start;
  hentry **loc = ht->table + index;
#ifdef ATOMIC_HASHTABLE
  /*
    I'm pretty sure there's a more elegent way to do this, which
    avoids the retry, but I'm not sure how to do that.
   */
  //use a label to indicate this isn't a normal loop
  //we only need to retry if the entry we're removing was the
  //head of a bucket, and another entry was added to that bucket
 retry:
  entry = start = ht->table[index];
  while(entry){
    if(entry->hv == hv){
      if(ht->cmp(data, entry->data, sz, entry->sz)){
        if(atomic_cmpxchg(loc, entry, entry->next)){
          atomic_dec(&ht->entries);
          freelist_push(ht, entry);
          atomic_dec(&ht->num_threads_using);
          ret = entry->value;
          return ret;
        } else {
          if(loc == ht->table + index){
            goto retry;
          } else {
            return NULL;
          }
        }
      }
    }
    loc = &entry->next;
    entry = entry->next;
  }
  return ret;
#else
  while(entry){
    if(entry->hv == hv){
      if(ht->cmp(key, entry->key, key_sz, entry->key_sz)){
        ret = entry->value;
        *loc = entry->next;
        ht->entries--;
        free(entry);
        return ret;
      }
    }
    loc = &entry->next;
    entry = entry->next;
  }
  return ret;
#endif
}
/*
  It is up to the caller of this function to insure no threads
  start using the table after it has been called.
*/
void destroy_hashtable(struct hashtable *ht){
#ifdef ATOMIC_HASHTABLE
  if(atomic_load(ht->rehash_pending)){
    sem_wait(&ht->sem);
  }
  while(atomic_load(ht->num_threads_using)){
    spin_pause();
  }
  sem_destroy(&ht->sem);
#endif
  int i;
  for(i=0;i<ht->size;i++){
    hentry *entry = ht->table[i];
    while(entry){
      hentry *temp = entry->next;
      free(entry);
      entry = temp;
    }
  }
  free(ht->table);
  free(ht);
  return;
}

void* hashtable_add_string(htable *ht, char *str, void *value){
  return hashtable_add(ht, str, strlen(str), value);
}
void hashtable_remove_string(htable *ht, char *str){
  return hashtable_remove(ht, str, strlen(str));
}
void* hashtable_find_string(htable *ht, char *str){
  return hashtable_find(ht, str, strlen(str));
}

#ifdef ATOMIC_HASHTABLE
static inline void freelist_push(htable *ht, void *data){
  fl_entry *entry = xmalloc(sizeof(fl_entry));
  entry->data = data;
  entry->next = ht->freelist;
 retry:
  if(atomic_compare_exchange(&(ht->freelist), &(entry->next), entry)){
    return;
  } else {
    goto retry;
  }
}
static inline void* freelist_pop(htable *ht){
  fl_entry *entry = ht->freelist;
 retry:
  if(entry){
    if(atomic_compare_exchange(ht->freelist, &entry, entry->next)){
      void *ret = entry->data;
      free(entry);
      return ret;
    } else {
      goto retry;
    }
  }
  return NULL;
}
static inline void* freelist_pop_unsafe(htable *ht){
  fl_entry *entry = ht->freelist;
  if(entry){
    void *ret = entry->data;
    ht->freelist = entry->next;
    free(entry);
    return ret;
  } else {
    return NULL;
  }
}
#endif
