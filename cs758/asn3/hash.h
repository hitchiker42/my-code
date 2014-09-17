#ifndef _MY_HASH_H
#define _MY_HASH_H
#include <stdint.h>
#include <stdlib.h>
#ifdef HASH_MALLOC
#define xmalloc(sz) HASH_MALLOC(sz)
#else
#define xmalloc(sz) malloc(sz)
#endif
typedef uint32_t (*hash_fn)(void*,uint32_t);
typedef int(*comp_fn)(void*,void*,uint32_t);
typedef struct hashtable {
  int len;
  float load_factor;
  int buckets_used;
  int num_entries;
  float growth_factor;
  int (*comp)(void*,void*,uint32_t);
  uint32_t(*hash)(void*,uint32_t);
  struct ht_entry *table;
} hashtable;
typedef struct ht_entry {
  uint32_t hv;
  struct ht_entry *next;
  void *data;
  int len;
} ht_entry;
hashtable *make_hashtable(float load_factor, int growth_factor,
                          int(*comp)(void*,void*,int),
                          uint32_t(*hash)(void*,int),
                          uint32_t initial_length);
int addhash(hashtable *ht, void *data, uint32_t len);
int gethash(hashtable *ht, void *data, uint32_t len);
int remhash(hashtable *ht, void *data, uint32_t len);
#endif
