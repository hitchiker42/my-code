#ifndef _MY_HASH_H
#define _MY_HASH_H
#include <stdint.h>
#include <stdlib.h>
#include "state.h"
#ifdef HASH_MALLOC
#define xmalloc(sz) HASH_MALLOC(sz)
#else
#define xmalloc(sz) malloc(sz)
#endif
typedef struct hashtable {
  int len;
  float load_factor;
  int buckets_used;
  int num_entries;
  float growth_factor;
  int (*comp)(void*,void*);
  uint32_t(*hash)(void*);
  struct ht_entry *table;
} hashtable;
typedef struct ht_entry {
  uint32_t hv;
  struct ht_entry *next;
  State *data;
} ht_entry;
hashtable *make_hashtable(float load_factor, int growth_factor,
                          int(*comp)(void*,void*),
                          uint32_t(*hash)(void*),
                          uint32_t initial_length);
int addhash(hashtable *ht, void *data);
void* gethash(hashtable *ht, void *data);
int remhash(hashtable *ht, void *data);
#endif
