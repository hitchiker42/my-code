


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

    PrefixStorer* p = NULL;
    /*
      You will have to decide exactly what you want to malloc here, as
      it will depend on how you decide to implement your hash table.

    PrefixStorer* p = emalloc(?);
    */
    p->lookup = &ht_lookup;
    p->add = &ht_add;
    p->cleanup = &cleanup;
    p->printCounts = &printCounts;

    switch(ht){
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
static char* flatten(char*s[NPREF]){
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
    return longString;
    
}
/* hash: compute hash value for array of NPREF strings */
unsigned int badHash(char *s[NPREF])
{
    char* flat = flatten(s);
    unsigned int h;
    h = flat[0];
    free(flat);
    return h % NHASH;
}

/* hash: compute hash value for array of NPREF strings */
unsigned int krHash(char *s[NPREF])
{
    char* toHash = flatten(s);
    
    free(toHash);
    return 0;
}
/* hash: compute hash value for array of NPREF strings */
unsigned int cwHash(char *s[NPREF])
{
    char* toHash = flatten(s);
    
    free(toHash);
    return 0;
}

/* hash: compute hash value for array of NPREF strings */
unsigned int add3Hash(char *s[NPREF])
{
    char* toHash = flatten(s);
    
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
