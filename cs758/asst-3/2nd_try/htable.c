


/**
   Code file for hash tables.

   This is primarily where you will need to add things.
*/
/*
  The skeleton code is not very good, but I'm not going to rewrite it, so
  if my code isn't the most efficent don't blame me.
*/
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>


#include "storer.h"
#include "htable.h"
#include "eprintf.h"
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
typedef struct State ht_entry;
typedef struct hash_table hash_table;
struct hash_table {
  ht_entry **table;
  uint32_t entries;
  uint32_t size;
  float load_factor;
};
hash_table *make_hash_table(uint32_t size, float load_factor){
  if(!size){size = 128;}
  if(!load_factor){load_factor = 0.8;}
  hash_table *ht = emalloc(sizeof(hash_table));
  ht->table = emalloc(sizeof(ht_entry*)*size);
  memset(ht->table,'\0',size*sizeof(ht_entry*));
  ht->size = size;
  ht->entries = 0;
  ht->load_factor = load_factor;
  return ht;
}

PrefixStorer* makeHashPrefixStorer(enum hashType ht){
  PrefixStorer* p = emalloc(sizeof(PrefixStorer));
  p->lookup = &ht_lookup;
  p->add = &ht_add;
  p->cleanup = &cleanup;
  p->printCounts = &printCounts;
  p->tableBase = make_hash_table(0,0);
  srandom(time(NULL));
  switch (ht) {
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

  return p;
}

/*static unsigned int length(State* s){
  unsigned int l = 0;
  while (s != NULL) {
    s = s->next;
    l++;
  }
  return l;
  }*/
static int bucket_len(ht_entry *bucket){
  int len=0;
  while(bucket){
    len++;
    bucket = bucket->next;
  }
  return len;
}
static void printCounts(PrefixStorer* ps){
  /*
    You will need to make sure this is implemented to work with your
    hash table.  Once again, the precise mechanics of this function
    will depend on how you decide to implement your hash table.

    for (every bucket in your table) {
    printf("bucket %i:\t%i\n", i, length(statetab[i]));
    }
  */
  hash_table *ht = ps->tableBase;
  uint32_t i;
  for(i=0;i<ht->size;i++){
    printf("bucket %d:\t%d\n", i, bucket_len(ht->table[i]));
  }
  return;
  
}

static void cleanup(PrefixStorer* ps){
  hash_table *ht = ps->tableBase;
  uint32_t i;
  for(i=0;i<ht->size;i++){
    ht_entry *bucket = ht->table[i];
    while(bucket){
      ht_entry *temp = bucket->next;
      free(bucket);
      bucket = temp;
    }
  }
  free(ht->table);
  free(ht);
}


/**
   Takes an array of strings and flattens it, so a string hash
   function can be uesd on it.  It does so by just concatenating the
   strings together.
*/
static char* flatten(char*s[NPREF]){
  int size = 0;
  char* longString;
  int i;
  for (i = 0; i < NPREF; i++) {
    size += strlen(s[i]);
  }
  longString = emalloc(size + 1);
  size = 0;
  for (i = 0; i < NPREF; i++) {
    strcpy(longString + size, s[i]);
    size += strlen(s[i]);
  }
  return longString;
    
}
/* hash: compute hash value for array of NPREF strings */
unsigned int badHash(char *s[NPREF]){
  return **s % NHASH;
}

/* hash: compute hash value for array of NPREF strings */
#define KR_MULTIPLE 127
#define KR_START 2166136261
unsigned int krHash(char *s[NPREF]){
  char* to_hash = flatten(s);
  uint32_t len = strlen(to_hash); //bleh, I hate doing this
  uint32_t hv = KR_START, i;
  for (i=0;i<len;i++) {
    hv = ((hv * KR_MULTIPLE) + to_hash[i]);
  }
  free(to_hash);
  return hv;
}
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
/* hash: compute hash value for array of NPREF strings */
unsigned int cwHash(char *s[NPREF]){
  char* to_hash = flatten(s);
  int keylen = strlen(to_hash);//again I wish I didn't have to do this
  int h, i, j, k;
  uint8_t *x=(uint8_t*)to_hash,ch;
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
  x[0]=ch; // restore first byte (not really necessary since we're using a copy)
  free(to_hash);
  free(lookup_table);
  return hash.hash_val; // output 32-bit 8 hex bytes hash
}

/* hash: compute hash value for array of NPREF strings */
unsigned int add3Hash(char *s[NPREF]){
  char* to_hash = flatten(s);
  char* ptr = to_hash;
  uint32_t hv = KR_START;
  while(*ptr){
    hv += (3 + *ptr++);
  }
  free(to_hash);
  return hv;
}
int string_eq(char *a, char *b){
  return !strcmp(a,b);
}
int string_arr_eq(char **a, char **b, int len){
  int i=0;
  while(i<len){
    if(!string_eq(a[i],b[i])){
      return 0;
    }
    i++;
  }
  return 1;
}
static void rehash(hash_table *ht){
  int new_size = ht->size*2;
  ht_entry **new_table = emalloc(new_size*sizeof(ht_entry*));
  memset(new_table, '\0',(sizeof(ht_entry*)*new_size));
  uint32_t i;
  for(i=0;i<ht->size;i++){
    ht_entry *entry = ht->table[i];
    while(entry){
      int index = entry->hv % new_size;
      ht_entry *temp = entry->next;
      entry->next = new_table[index];
      new_table[index] = entry;
      entry = temp;
    }
  }
  free(ht->table);
  ht->table = new_table;
  ht->size = new_size;
}
    
static void maybe_rehash(hash_table *ht){
  if(((float)ht->entries / (float)ht->size) > ht->load_factor){
    rehash(ht);
  }
  return;
}
/* lookup: search for prefix; create if requested. */
/*  returns pointer if present or created; NULL if not. */
/*  creation doesn't strdup so strings mustn't change later. */
static State* ht_lookup(char *prefix[NPREF], int create, PrefixStorer* ps){
  /*
    Looks up the state, and optionally creates a new node depending
    on what create has been set to.
  */
  uint32_t hv = ps->hash(prefix);
  hash_table *ht = ps->tableBase;
  uint32_t index = hv % ht->size;
  ht_entry *bucket = ht->table[index];
  if(bucket != NULL){
    ht_entry *temp = bucket;
    while(temp != NULL){
      if(temp->hv == hv){
        if(string_arr_eq(temp->pref, prefix,NPREF)){
          return temp;
        }
      }
      temp = temp->next;
    }
  }
  if(!create){
    return NULL;
  }
  ht_entry *new = emalloc(sizeof(ht_entry));
  //Since we're not multithreaded we can just change the hash table
  memcpy(new->pref,prefix,NPREF*sizeof(char*));
  new->next = bucket;
  new->hv = hv;
  new->suf = NULL;
  ht->table[index] = new;
  ht->entries++;
  maybe_rehash(ht);
  return new;
}


/**
   This calls your other functions you already implemented, so as long
   as the other functions are done correctly, this should require no
   changes.
*/

/* add: add word to suffix list, update prefix */
static void ht_add(char *prefix[NPREF], char *suffix, PrefixStorer* ps){
  State *sp;
  sp = ps->lookup(prefix, 1, ps);  /* create if not found */
  /*  if(sp->suf == NULL){
    sp->suf = emalloc(sizeof(Suffix));
    sp->suf->word = suffix;
    sp->suf->next = NULL;
    sp->suf->count = 1;
  } else {
    sp->suf->count++;
    }*/
  addsuffix(sp, suffix);

  /* move the words down the prefix */
  memmove(prefix, prefix+1, (NPREF-1)*sizeof(prefix[0]));
  prefix[NPREF-1] = suffix;
}
