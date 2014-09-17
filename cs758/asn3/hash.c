#include "hash.h"
#define fnv_prime_32 16777619
#define fnv_prime_64 1099511628211UL
#define offset_basis_32 2166136261
#define offset_basis_64 14695981039346656037UL
uint32_t fnv_hash32(const void *key, int keylen){
  const uint8_t *raw_data=(const uint8_t*)key;
  int i;
  uint64_t hash=offset_basis_32;
  for(i=0;i<keylen;i++){
    hash=(hash^raw_data[i])*fnv_prime_32;
  }
}
uint64_t fnv_hash(const void *key,int keylen){
  const uint8_t *raw_data=(const uint8_t *)key;
  int i;
  uint64_t hash=offset_basis_64;
  for(i=0; i < keylen; i++){
    hash = (hash ^ raw_data[i])*fnv_prime_64;
  }
  return hash;
}
int default_comp(void *a, void *b, uint64_t len){
  return (memcmp(a,b,len) == 0 ? 1 : 0);
}
uint64_t default_hash
hashtable *make_hashtable(float load_factor, int growth_factor,
                          int(*comp)(void*,void*,int),
                          uint32_t(*hash)(void*,int),
                          uint64_t initial_length){
  //set default values for any values not given
  if(!load_factor){load_factor = 0.8;}
  if(!growth_factor){growth_factor = 2;}
  if(!comp){comp = default_comp;}
  if(!hash){hash = fnv_hash32;}
  if(!initial_length){initial_length = 64;}
  hashtable *retval = xmalloc(sizeof(struct hashtable));
  retval->table = xmalloc(initial_length * sizeof(void*));
  retval->load_factor = load_factor;
  retval->growth_factor = growth_factor;
  retval->comp = comp;
  retval->hash = hash;
  retval->len = initial_length;
  retval->num_entries = retval->buckets_used = 0;
  return retval;
}
int addhash(hashtable *ht, void *data, uint32_t len){
  uint32_t hv=ht->hash(data,len);
  int ind = hv % ht->len;
  ht_entry *temp = ht->table[ind];
  if(temp){
    do {
      if (temp->len == len && ht->comp(temp->data,data,len)){
        return 0;
      }
    } while (temp->next && temp = temp->next);
    temp->next = xmalloc(sizeof(ht_entry));
    temp->next->data = data;
    temp->next->hv = hv;
    temp->next->next = NULL;
    temp->next->len = len;
  } else {
    temp = xmalloc(sizeof(ht_entry));
    temp->data = data;
    temp->hv = hv;
    temp->next = NULL;
    temp->len = len;
  }
  ht->num_entries++;
  if((ht->num_entries/(float)ht->len) > ht->load_factor){
    ht=rehash(ht);
  }
  return 1;
}
int gethash(hashtable *ht, void *data, uint32_t len){
  uint32_t hv=ht->hash(data,len);
  int ind = hv % ht->len;
  ht_entry *temp = ht->table[ind];
  while(temp){
    if(ht->comp(temp,data)){
      return 1;
    }
    temp=temp->next;
  }
  return 0;
}
int remhash(hashtable *ht, void *data, uint32_t len){
  uint32_t hv=ht->hash(data,len);
  int ind = hv % ht->len;
  ht_entry *temp = ht->table[ind];
  if(comp(temp,data)){
    free(comp);
    ht->num_entries--;
    return 1;
  } else {
  while(temp->next){
    if(temp->next->len == len && ht->comp(temp->next,data,len)){
      goto FOUND;
    }
    temp=temp->next;
  }
  return 0;
 FOUND:
  old = temp->next;
  temp->next = temp->next->next;
  free(old);
  ht->num_entries--;
  return 1;
  }
}
static void addhash_internal(hashtable *ht, ht_entry *entry){
  int ind = entry->hv % ht->len;
  ht_entry *temp = ht->table[ind];
  if(temp){
    while(temp->next){temp=temp->next;}
    temp->next = entry;
    entry->next = NULL;
  } else {
    temp = entry;

  }
}
static hashtable* rehash(hashtable *ht){
  hashtable *new = xmalloc(ht->len * ht->growth_factor + sizeof(hashtable));
  new->len = ht->len *ht->growth_factor;
  memcpy(new, ht, sizeof(hashtable));
  new->table = new + sizeof(hashtable);
  //lazy way of doing this
  int i;
  ht_entry *entry;
  for(i=0;i<ht->len;i++){
    entry = ht->table[i];
    if(entry){
      do {
        addhash_internal(new, entry);
      } while((entry=entry->next));
    }
  }
  free(ht);
  return new;
}
