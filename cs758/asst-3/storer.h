/**

Defines an interface for PrefixStorers.

IMPORTANT NOTE!!!!  This data structure uses function pointers.  If
these are a bit rusty for you, I would STRONGLY recommend finding a
quick tutorial to review what a function pointer is, how to use them,
etc.

 */

#ifndef STORER_H
#define STORER_H

typedef struct PrefixStorer PrefixStorer;


typedef struct State State;
typedef struct Suffix Suffix;

enum {
	NPREF	= 2,	/* number of prefix words */
};

/**
   States store a prefix, as well as all suffixes that are valid after
   that prefix.
 */
struct State {	/* prefix + suffix list */
  char	*pref[NPREF];	/* prefix words */
  unsigned int hv;
  Suffix	*suf;			/* list of suffixes */
  State	*next;			/* next in hash table */

};

/**
   Suffixes store 1 suffix, as well as a pointer to the next valid
   suffix.
 */
struct Suffix {	/* list of suffixes */
    char	*word;			/* suffix */
    int count;
    Suffix	*next;			/* next in list of suffixes */
};

/**
   Function that can be called to clean up a state.
 */
void cleanupState(State* s);


/**
   Struct with function pointers and a bit of data at the end.  This
   is kind of like a primitive version of object oriented programming,
   and was used before the invention of C++.  The function pointers allow
   the data structure to be used to do something.

   The void* at the end is space for whatever this thing actually
   needs to function.

   In the linked list implementation, it serves as the head pointer.
   In the hash table, it can either be made to be larger to form the
   base of the table, or it can point to the base of the hash table
   which would then be malloc'd elsewhere.  Which approach up to
   programmer discretion.

   A number of the functions have a PrefixStorer* argument.  This is
   roughly equivalent to a this pointer in function and purpose.
 */
struct PrefixStorer {
    /**
       Looks for the prefix in the table.  If create is set, adds to
       the collection, otherwise doesn't do anything.
     */
    State* (*lookup)(char* prefix[NPREF], int create, PrefixStorer* ps);
    /**
       Adds the thing to the collection.
     */
    void (*add)(char* prefix[NPREF], char* suffix, PrefixStorer* ps);
    /**
       Doens't actually get called, but printing can be useful
       depending on what you are doing.  Gives you a place to put a
       print function if you feel like writing one.
     */
    void (*print)(PrefixStorer* ps);
    /**
       For hash tables, prints the count of items in each bucket.
     */
    void (*printCounts)(PrefixStorer* ps);
    /**
       When everything is done, the data structure needs to be cleaned
       up.  This function provides that.  It is roughly equivalent to
       a destructor.
     */
    void (*cleanup)(PrefixStorer* ps);
    /**
       Which hash function to use.  It is important you use the hash
       function from here, as opposed to elsewhere, because this one
       will match the command line argument hash function.
     */
    unsigned int (*hash)(char* s[]);
    void* tableBase;
};


/* addsuffix: add to state. suffix must not change later Adds the
 * suffix to the list of suffixes for this tuple.*/
void addsuffix(State *sp, char *suffix);

#endif
