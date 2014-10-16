/**
Code file for hash tables.

This is primarily where you will need to add things.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <alloca.h>
#include <assert.h>

#include "storer.h"
#include "htable.h"
#include "eprintf.h"
typedef struct hash_table {
  State **table;
  int len;
  int entries;
  float load_factor;
} hash_table;

void srandom(unsigned int seed);
long int random();

static const int NHASH = 5000;

static State *ht_lookup(char *prefix[], int create, PrefixStorer* ps);
static void ht_add(char *prefix[], char *word, PrefixStorer* ps);
static void cleanup(PrefixStorer* ps);
static void printCounts(PrefixStorer* ps);
unsigned int badHash(char *s[NPREF]);
unsigned int krHash(char *s[NPREF]);
unsigned int add3Hash(char *s[NPREF]);
unsigned int cwHash(char *s[NPREF]);

unsigned int my_bad_hash(void *key, unsigned int keylen);
unsigned int my_kr_hash(void *key, unsigned int keylen);
unsigned int my_add3_hash(void *key, unsigned int keylen);
unsigned int my_cw_hash(void *key, unsigned int keylen);
//allocate zeroed memory
#define xmalloc(sz) calloc(sz,1)
uint8_t *rand_arr_256(){
  uint8_t *retval = emalloc(256);
  memset(retval, '\0', 256);
  int i,j;
  for(i=1;i<256;i++){
    do {
      j = random() % 256;
    } while (retval[j]);
    retval[j]=i;
  }
  return retval;
}
int prefix_compare(void *a, void *b){
  char **strs_1 = a;
  char **strs_2 = b;
  int i;
  for(i=0;i<NPREF;i++){
    if(strcmp(strs_1[i],strs_2[i])){
      return 0;
    }
  }
  return 1;
}

PrefixStorer* makeHashPrefixStorer(enum hashType htype){
    int i;
    State **statetab;

    PrefixStorer* p = emalloc(sizeof(struct PrefixStorer));
    p->lookup = &ht_lookup;
    p->add = &ht_add;
    p->cleanup = &cleanup;
    p->printCounts = &printCounts;
    switch(htype){
    case BAD:
        p->hash = &badHash;
        break;
    case KR:
        p->hash = &krHash;
        break;
    case CW:
        p->hash = &add3Hash;
        break;
    case ADD3:
        p->hash = &cwHash;
        break;
    }
    hash_table *ht = xmalloc(sizeof(hash_table));
    ht->load_factor = 0.8;
    ht->len = 100;
    ht->table =  xmalloc(100*sizeof(State*));
    p->tableBase = ht;
    return p;
}

static unsigned int length(State* s){
    unsigned int l = 0;
    while(s != NULL){
        s = s->next;
        l++;
    }
    return l;
}

static void printCounts(PrefixStorer* ps){
    /*
      You will need to make sure this is implemented to work with your
      hash table.  Once again, the precise mechanics of this function
      will depend on how you decide to implement your hash table.

    for(every bucket in your table){
        printf("bucket %i:\t%i\n", i, length(statetab[i]));
    }
    */
  hash_table *ht = ps->tableBase;
  if(ht){
    int i, len;
    State *temp;
    for(i=0;i<ht->len;i++){
      len = 0;
      temp = ht->table[i];
      if (temp) {
        do {
          len++;
        } while(temp = temp->next);
      }
      printf("bucket %i:\t%i\n", i, len);
    }
  }
}

static void cleanup(PrefixStorer* ps){
    /*
      This should delete all of the nodes in your hash table.
     */
  hash_table *ht = ps->tableBase;
  State *temp, *next;
  if(ht){
    int i;
    for(i=0;i<ht->len;i++){
      temp = ht->table[i];
      if(temp){
        do {
          next = temp ->next;
          free(temp);
        } while (temp = next);
      }
    }
    free(ht);
  }
}


/**
   Takes an array of strings and flattens it, so a string hash
   function can be uesd on it.  It does so by just concatenating the
   strings together.

   optionally returns the length of the returned string in the given
   integer.

   I fixed this, it was bad.
 */
static char* flatten(char*s[NPREF], int *len){
  int size = 0, offset=0;
  char *long_string;
  int i;
  int *lengths = alloca(NPREF*sizeof(int));
  for(i = 0; i < NPREF; i++){
    int len = strlen(s[i]);
    lengths[i] = len;
    size += len;
  }
  long_string = emalloc(size + 1);
  for(i = 0; i < NPREF; i++){
    memcpy(long_string+offset, s[i], lengths[i]);
    offset += lengths[i];
  }
  *(long_string+offset) = '\0';
  if(len){
    *len = size;
  }
  return long_string;

}
uint32_t my_bad_hash(void *key, uint32_t keylen){
  char **strs=key;
  return strs[0][0] %NHASH;
}
/* hash: compute hash value for array of NPREF strings */
unsigned int badHash(char *s[NPREF])
{
  return my_bad_hash(s, 0);
}
#define KR_MULTIPLE 127
uint32_t kr_hash(uint8_t *key, uint32_t keylen){
  uint32_t hv = 0;
  int i=0;
  for(i=0;i<keylen;i++){
    hv=((hv*KR_MULTIPLE)+key[i]);
  }
  return hv;
}
/* hash: compute hash value for array of NPREF strings */
unsigned int krHash(char *s[NPREF])
{
  int key_len;
  char* key = flatten(s,&key_len);
  uint32_t hv = kr_hash(key,key_len);
  free(key);
  return hv;
}
uint32_t pearson_hash8(const void *key, int keylen){
  int h, i, j, k;
  uint8_t *x=(uint8_t*)key,ch;
  uint8_t *lookup_table = rand_arr_256();
  union {
    char hh[4];
    uint32_t hash_val;
  } hash;
  ch=x[0]; // save first byte
  for (j=0; j<4; j++) {
    // standard Pearson hash (output is h)
    h=0;
    for (i=0; i<keylen; i++) {
      k=h^x[i];
      h=lookup_table[k];
    }
    hash.hh[j]=h; // store result
    x[0]=x[0]+1; // increment first data byte by 1
  }
  x[0]=ch; // restore first byte...why?
  // concatenate the 4 stored values of h;
  return hash.hash_val; // output 32-bit 8 hex bytes hash
}
/* hash: compute hash value for array of NPREF strings */
unsigned int cwHash(char *s[NPREF]){
  int keylen;
  char* toHash = flatten(s,&keylen);
  uint32_t hv = pearson_hash8(toHash, keylen);
  free(toHash);
  return hv;
}

/* hash: compute hash value for array of NPREF strings */
unsigned int add3Hash(char *s[NPREF])
{
  uint32_t hv = 0;
  int i,keylen;
  char* toHash = flatten(s,&keylen);
  for(i=0;i<keylen;i++){
    hv += toHash[i]+3;
  }
  free(toHash);
  return hv;
}

static hash_table* rehash(hash_table *ht){
  int old_len = ht->len, new_len = ht->len *2;
  State **new_table = xmalloc(new_len *sizeof(State*));
  memcpy(new_table, ht->table, old_len *sizeof(State*));
  int i;
  State *entry,*next,*temp;
  for(i=0;i<old_len;i++){
    entry = ht->table[i];
    while(entry){
      int ind = entry->hv % new_len;
      if(new_table[ind]){
        temp = new_table[ind];
        while(temp->next){temp = temp->next;}
        temp->next = entry;
      } else {
        new_table[ind] = entry;
      }
      next = entry->next;
      entry->next = NULL;
      entry = next;
    }
  }
  free(ht->table);
  ht->table = new_table;
  return ht;
}
static State *addhash(hash_table *ht, char* prefix[NPREF], uint32_t hv){
  int ind = hv % ht->len;
  State *temp;
  if(temp = ht->table[ind]){
    do {
      if(prefix_compare(temp->pref, prefix)){
        return temp;
      }
    } while (temp->next && (temp = temp->next));
  }
  if(!temp){
    ht->table[ind] = emalloc(sizeof(State));
    temp = ht->table[ind];
  } else {
    temp->next = emalloc(sizeof(State));
    temp = temp->next;
  }
  memcpy(temp->pref,prefix,NPREF*sizeof(char*));
  temp->next = NULL;
  temp->hv = hv;
  ht->entries++;
  if((float)ht->entries/ht->len > ht->load_factor){
    rehash(ht);
  }
  return temp;
}
/* lookup: search for prefix; create if requested. */
/*  returns pointer if present or created; NULL if not. */
/*  creation doesn't strdup so strings mustn't change later. */
static State* ht_lookup(char *prefix[NPREF], int create, PrefixStorer* ps)
{
  State *temp;
  hash_table *ht = ps->tableBase;
  uint32_t hv = ps->hash(prefix);
  if(create){
    return addhash(ht,prefix,hv);
  }
  temp = ht->table[hv % ht->len];
  if(temp){
    do {
      if(temp->hv == hv){
        if(prefix_compare(temp->pref, prefix)){
          return temp;
        }
      }
    } while (temp->next && (temp = temp->next));
  }
  return NULL;
}


/**
   This calls your other functions you already implemented, so as long
   as the other functions are done correctly, this should require no
   changes.
 */

/* add: add word to suffix list, update prefix */
static void ht_add(char *prefix[NPREF], char *suffix, PrefixStorer* ps)
{
        State *sp;
        sp = (ps->lookup)(prefix, 1, ps);  /* create if not found */
        addsuffix(sp, suffix);

        /* move the words down the prefix */
        memmove(prefix, prefix+1, (NPREF-1)*sizeof(prefix[0]));
        prefix[NPREF-1] = suffix;
}
