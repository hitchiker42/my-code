


/**
Code file for hash tables.

This is primarily where you will need to add things.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "storer.h"
#include "htable.h"
#include "eprintf.h"

#define NEED_XMALLOC
#include "C_util.h"
#include "hash.h"

static const int NHASH = 5000;

static State *ht_lookup(char *prefix[], int create, PrefixStorer* ps);
static void ht_add(char *prefix[], char *word, PrefixStorer* ps);
static void cleanup(PrefixStorer* ps);
static void printCounts(PrefixStorer* ps);
unsigned int badHash(char *s[NPREF]);
unsigned int krHash(char *s[NPREF]);
unsigned int add3Hash(char *s[NPREF]);
unsigned int cwHash(char *s[NPREF]);

PrefixStorer* makeHashPrefixStorer(enum hashType ht){
  int i;
  State **statetab;

  /*
    You will have to decide exactly what you want to malloc here, as
    it will depend on how you decide to implement your hash table.

    PrefixStorer* p = emalloc(?);
  */
  PrefixStorer *p = xmalloc(sizeof(PrefixStorer));
  p->lookup = &ht_lookup;
  p->add = &ht_add;
  p->cleanup = &cleanup;
  p->printCounts = &printCounts;
  switch(ht){
    case BAD:
      p->htable = make_hashtable(NHASH, 0.8, bad_hash, NULL);
      p->hash = &badHash;
      break;
    case KR:
      p->htable = make_hashtable(NHASH, 0.8, kr_hash, NULL);
      p->hash = &krHash;
      break;
    case CW:
      p->htable = make_hashtable(NHASH, 0.8, pearson_hash, NULL);
      p->hash = &cwHash;
      break;
    case ADD3:
      p->htable = make_hashtable(NHASH, 0.8, add3_hash, NULL);

      p->hash = &add3Hash;
      break;
  }

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
}

static void cleanup(PrefixStorer* ps){
    /*
      This should delete all of the nodes in your hash table.
     */
  
}


/**
   Takes an array of strings and flattens it, so a string hash
   function can be uesd on it.  It does so by just concatenating the
   strings together.
*/
//At the very least this should cache the string lengths to avoid
//having to call strlen twice on each string
static int flatten(char*s[NPREF], char **ret){
    int size = 0;
    char* longString;
    int i;
    int ix = 0;
    for(i = 0; i < NPREF; i++){
	size += strlen(s[i]);
    }
    longString = emalloc(size + 1);
    size = 0;
    for(i = 0; i < NPREF; i++){
	strcpy(longString + size, s[i]);
	size += strlen(s[i]);
    }
    *ret = longString;
    return size;
}
/* hash: compute hash value for array of NPREF strings */
uint32_t bad_hash(uint8_t *key, int keylen){
  return key[0] % NHASH;
}
unsigned int badHash(char *s[NPREF]){
  return *(*s) % NHASH;
}
#define KR_MULTIPLE 127
#define INITIAL_HV 0xaabbccddee
/* hash: compute hash value for array of NPREF strings */
uint32_t kr_hash(uint8_t *key, int keylen){
  uint32_t hv = INITIAL_HV;
  for(i=0;i<keylen;i++){
    hv = ((hv * KR_MULTIPLE) + key[i]);
  }
  return hv;
}
unsigned int krHash(char *s[NPREF]){
  uint8_t *key;
  int size = flatten(s, (char**)&key);
  int i;
  uint32_t hv = kr_hash(key, size);
  free(key);
  return hv;
}
//generate this by randomly shuffling an array of 0-255
static uint8_t lookup_table[0x100];
/* hash: compute hash value for array of NPREF strings */
uint32_t pearson_hash8(uint8_t *key, int keylen){
  int i,j;
/*
  Compute 4 8 bit hash values, add 1 to the
  first value of the key to make sure the 4 values
  are different, then return the 32 bit integer formed
  by concatenating the 4 bytes together;
*/
  union {
    char hh[4];
    uint32_t hv;
  } hv;
  uint8_t ch = key[0];//save 1st byte (could also just subtract 4 at the end)
  for(j=0;j<4;j++){//unroll this?
    uint8_t h = 0x7f;
    for(i=0;i<keylen;i++){
      uint8_t k = h^key[i];
      h = lookup_table[k];
    }
    hv.hh[j] = h;
    x[0]++;//increment 1st data byte
  }
  x[0] = ch;
  return hv.hv;
}
unsigned int cwHash(char *s[NPREF]){  
  uint8_t *key;
  int size = flatten(s, (char**)&key);
  uint32_t hv = pearson_hash8(key, size);
  free(key);
  return hv;
}
/* hash: compute hash value for array of NPREF strings */
unsigned int add3Hash(char *s[NPREF]){
  uint8_t *key;
  int size = flatten(s, (char**)&key);
  int i;
  uint32_t hv;
  for(i=0;i<size;i++){
    hv += key[i]+3;
  }
  free(key);
  return hv;
}
    free(toHash);
    return 0;
}


/* lookup: search for prefix; create if requested. */
/*  returns pointer if present or created; NULL if not. */
/*  creation doesn't strdup so strings mustn't change later. */
static State* ht_lookup(char *prefix[NPREF], int create, PrefixStorer* ps)
{
    /*
      Looks up the state, and optionally creates a new node depending
      on what create has been set to.
     */
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
