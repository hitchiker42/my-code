#include "C_util.h"
#include "hash.h"
#include <stdio.h>
/*
  Implementation of a basic hashtable, with optional atomicity.

  Most write operations on the hash table can be done atomically using
  cmpxchg, only a rehash requires fully locking the table.

  TODO: Refactor, there is a lot of similar code for looking up entries
  and such that could be consolidated.
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
#ifndef ATOMIC_HASHTABLE
#define atomic_inc(ptr)
#define atomic_dec(ptr)
#define atomic_cmpxchg(a,b,c)
#define atomic_compare_exchange(a,b,c)
#define atomic_load(ptr) *(ptr)
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

  //  Also we use the rehash_pending field as a counter for the number of threads
  //  waiting.
   */
  atomic_inc(&ht->num_threads_using);
  if(atomic_load(&ht->rehash_pending)){
    atomic_inc(&ht->num_threads_waiting);
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

static int bool_memcmp(const void *x, const void *y,
                       size_t x_sz, size_t y_sz){
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
  ht->load_factor = load_factor ? load_factor : 0.8;
  ht->size = size;
  ht->hash = hash ? hash : fnv_hash;
  ht->cmp = cmp ? cmp : bool_memcmp;
  ht->table = zmalloc(size*sizeof(hentry*));
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
  hentry **new_table = zmalloc(new_size*sizeof(hentry*));
  int i;
  for(i=0;i<ht->size;i++){
    hentry *entry = ht->table[i];
    while(entry){
      int index = entry->hv % new_size;
      hentry *next = entry->next;
      /*
        Update the next pointer, I forgot to clear the next pointer for the
        first entry before and it caused an infinite loop
      */
      if(new_table[index]){
        entry->next = new_table[index];
      } else {
        entry->next = NULL;
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
//    DEBUG_PRINTF("spinning\n");
    spin_pause();
  }
  rehash_internal(ht);
  void *temp;
  while((temp = freelist_pop_unsafe(ht))){
    free(temp);
  }
  atomic_store(&ht->rehash_pending, 0);
  while(atomic_load(&ht->num_threads_waiting)){
    sem_post(&ht->sem);
    atomic_dec(&ht->num_threads_waiting);
  }
}
static void maybe_rehash(htable *ht){
  //we make a check for a rehash which will usually catch
  //a pending rehash, but we also do a cmpxchg to actually set the
  //flag, meaning this check can be non-atomic for speed.
  if((((float)ht->entries/(float)ht->size) > ht->load_factor) &&
     !ht->rehash_pending){
    rehash(ht);
  }
}
#else
static void rehash(htable *ht){
  rehash_internal(ht);
}
static void maybe_rehash(htable *ht){
  if(((float)ht->entries/(float)ht->size) > ht->load_factor){
    rehash(ht);
  }
}
#endif

void* hashtable_find_add(htable *ht, void *key, size_t key_sz, void *value){
  inc_hashtable_threadcount(ht);
  uint64_t hv = ht->hash(key, key_sz);
  uint32_t index = hv % ht->size;
  /*
    bucket = head of linked list, entry = current entry
  */
  hentry *new_entry = NULL;
  hentry *entry, *bucket;
#ifdef ATOMIC_HASHTABLE
  //use a label to indicate this isn't a normal loop
 retry:
  bucket = ht->table[index];
  entry = bucket;
  while(entry){
    if(entry->hv == hv){
      if(ht->cmp(key, entry->key, key_sz, entry->key_sz)){
        atomic_dec(&ht->num_threads_using);
        free(new_entry);
        return atomic_load(&entry->value);
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
    return NULL;
  }
  goto retry;
#else
  bucket = ht->table[index];
  entry = bucket;
  while(entry){
    if(entry->hv == hv){
      if(ht->cmp(key, entry->key, key_sz, entry->key_sz)){
        return entry->value;
      }
    }
    entry = entry->next;
  }
  new_entry = make_hentry(key, key_sz, hv, value, bucket);
  ht->table[index] = new_entry;
  ht->entries++;
  return NULL;
#endif
}
int hashtable_add(htable *ht, void *key, size_t key_sz, void *value){
  inc_hashtable_threadcount(ht);
  uint64_t hv = ht->hash(key, key_sz);
  uint32_t index = hv % ht->size;
  /*
    bucket = head of linked list, entry = current entry
  */
  hentry *new_entry = NULL;
  hentry *entry, *bucket;
#ifdef ATOMIC_HASHTABLE
  //use a label to indicate this isn't a normal loop
 retry:
  bucket = ht->table[index];
  entry = bucket;
  while(entry){
    if(entry->hv == hv){
      if(ht->cmp(key, entry->key, key_sz, entry->key_sz)){
        atomic_dec(&ht->num_threads_using);
        free(new_entry);
        return 0;
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
    return 1;
  }
  goto retry;
#else
  bucket = ht->table[index];
  entry = bucket;
  while(entry){
    if(entry->hv == hv){
      if(ht->cmp(key, entry->key, key_sz, entry->key_sz)){
        return 0;
      }
    }
    entry = entry->next;
  }
  new_entry = make_hentry(key, key_sz, hv, value, bucket);
  ht->table[index] = new_entry;
  ht->entries++;
  return 1;
#endif
}
/*
  I Really wish C marcos were more lispy, then I wouldn't have to
  write basically the same function several times.
 */
void* hashtable_set(htable *ht, void *key, size_t key_sz, void *value){
  inc_hashtable_threadcount(ht);
  uint64_t hv = ht->hash(key, key_sz);
  uint32_t index = hv % ht->size;
  /*
    bucket = head of linked list, entry = current entry
  */
  hentry *new_entry = NULL;
  hentry *entry, *bucket;
#ifdef ATOMIC_HASHTABLE
  //use a label to indicate this isn't a normal loop
 retry:
  bucket = ht->table[index];
  entry = bucket;
  while(entry){
    if(entry->hv == hv){
      if(ht->cmp(key, entry->key, key_sz, entry->key_sz)){
        void *retval = atomic_load(&entry->value);
        if(atomic_cmpxchg(&entry->value, retval, value)){
          atomic_dec(&ht->num_threads_using);
          free(new_entry);
          return retval;
        } else {
          goto retry;
        }
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
    return NULL;
  }
  goto retry;
#else
  bucket = ht->table[index];
  entry = bucket;
  while(entry){
    if(entry->hv == hv){
      if(ht->cmp(key, entry->key, key_sz, entry->key_sz)){
        void *retval = entry->value;
        entry->value = value;
        return retval;
      }
    }
    entry = entry->next;
  }
  new_entry = make_hentry(key, key_sz, hv, value, bucket);
  ht->table[index] = new_entry;
  ht->entries++;
  return NULL;
#endif
}
/*
  There really isn't any C abstraction I could use to simplify
  this. C really needs more compile time features.
*/
void* hashtable_update(htable *ht, void *key, size_t key_sz,
                       void *(*update)(void*)){
  inc_hashtable_threadcount(ht);
  uint64_t hv = ht->hash(key, key_sz);
  uint32_t index = hv % ht->size;
  /*
    bucket = head of linked list, entry = current entry
  */
  hentry *new_entry = NULL;
  hentry *entry, *bucket;
#ifdef ATOMIC_HASHTABLE
  //use a label to indicate this isn't a normal loop
 retry:
  bucket = ht->table[index];
  entry = bucket;
  while(entry){
    if(entry->hv == hv){
      if(ht->cmp(key, entry->key, key_sz, entry->key_sz)){
        void *oldval = atomic_load(&entry->value);
        void *newval = update(oldval);
        if(atomic_cmpxchg(&entry->value, oldval, newval)){
          atomic_dec(&ht->num_threads_using);
          free(new_entry);
          return oldval;
        } else {
          //DEBUG_PRINTF("retrying\n");
          goto retry;
        }
      }
    }
    entry = entry->next;
  }
  if(!new_entry){
    new_entry = make_hentry(key, key_sz, hv, update(NULL), bucket);
  } else {
    new_entry->next = bucket;
  }
  if(atomic_compare_exchange(ht->table+index, &bucket, new_entry)){
    atomic_inc(&ht->entries);
    atomic_dec(&ht->num_threads_using);
    maybe_rehash(ht);
    return NULL;
  }
//  DEBUG_PRINTF("Retrying\n");
  goto retry;
#else
  bucket = ht->table[index];
  entry = bucket;
  while(entry){
    if(entry->hv == hv){
      if(ht->cmp(key, entry->key, key_sz, entry->key_sz)){
        void *oldval = entry->value;
        entry->value = update(oldval);
        return oldval;
      }
    }
    entry = entry->next;
  }
  new_entry = make_hentry(key, key_sz, hv, update(NULL), bucket);
  ht->table[index] = new_entry;
  ht->entries++;
  return NULL;
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
#ifdef ATOMIC_HASHTABLE
        void *retval = atomic_load(&entry->value);
        atomic_dec(&ht->num_threads_using);
        return retval;
#else
        return entry->value;
#endif
      }
    }
    entry = entry->next;
  }
  return NULL;
}

int hashtable_exists(htable *ht, void *key, size_t key_sz){
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
        void *retval = atomic_load(&entry->value);
        atomic_dec(&ht->num_threads_using);
        return 1;
      }
    }
    entry = entry->next;
  }
  return 0;
}
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
      if(ht->cmp(key, entry->key, key_sz, entry->key_sz)){
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
  entry = start = ht->table[index];
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
#define HTABLE_ITERATE(ht, body)                \
  ({int i;                                      \
    hentry *entry;                              \
    for(i=0;i<ht->size;i++){                    \
      entry = ht->table[i];                     \
      while(entry){                             \
        body;                                   \
        entry = entry->next;                    \
      }                                         \
    };})
void hashtable_iter(struct hashtable *ht, 
                    void(*fun)(void*,size_t,void*,void*), void *userdata){
  inc_hashtable_threadcount(ht);
  HTABLE_ITERATE(ht, fun(entry->key, entry->key_sz, entry->value, userdata));
  atomic_dec(&ht->num_threads_using);
}
void keys_iter(struct hashtable *ht,
               void(*fun)(void*,size_t,void*), void *userdata){
  inc_hashtable_threadcount(ht);
  HTABLE_ITERATE(ht, fun(entry->key, entry->key_sz, userdata));
  atomic_dec(&ht->num_threads_using);
}
void values_iter(struct hashtable *ht,
               void(*fun)(void*, void*), void *userdata){
  inc_hashtable_threadcount(ht);
  HTABLE_ITERATE(ht, fun(entry->value, userdata));
  atomic_dec(&ht->num_threads_using);
}
void hashtable_bucket_counts(struct hashtable *ht, FILE* out){
  inc_hashtable_threadcount(ht);
  int i, count;
  hentry *entry;
  fprintf(out, "Printing bucket lengths for hashtable with"
          " %u buckets and %u entries\n", ht->size, ht->entries);
  for(i=0;i<ht->size;i++){
    count = 0;
    entry = ht->table[i];
    while(entry){
      count++;
      entry = atomic_load(&entry->next);
    }
    fprintf(out, "Bucket %d: %d entries\n", i, count);
  }
}


/*
  It is up to the caller of this function to insure no threads
  start using the table after it has been called.
*/
void destroy_hashtable(struct hashtable *ht){
  if(!ht){return;}
#ifdef ATOMIC_HASHTABLE
  if(atomic_load(&ht->rehash_pending)){
    sem_wait(&ht->sem);
  }
  while(atomic_load(&ht->num_threads_using)){
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
    if(atomic_compare_exchange(&ht->freelist, &entry, entry->next)){
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
