


/**
Code file for hash tables.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "storer.h"
#include "eprintf.h"

static State *linked_lookup(char *prefix[], int create, PrefixStorer* ps);
static void linked_add(char *prefix[], char *word, PrefixStorer* ps);
static void print(PrefixStorer* ps);
static void cleanup(PrefixStorer* ps);

/**
   Called to create a new linked list.
 */
PrefixStorer* makeLinkedPrefixStorer(){
    /*
      Malloc a new PrefixStorer
     */
    PrefixStorer* p = emalloc(sizeof(PrefixStorer));
    /*
      Set which functions it will contain
     */
    p->lookup = &linked_lookup;
    p->add = &linked_add;
    p->print = &print;
    p->cleanup = &cleanup;
    /*
      Initialize p to NULL because the linked list initially contains nothing.
     */
    p->tableBase = NULL;
    return p;
}

/**
   Called to delete the contents of this PrefixStorer.
 */
static void cleanup(PrefixStorer* ps){
    State *statehead = (State*) (ps->tableBase);
    while(statehead != NULL){
	State* current = statehead;
	statehead = statehead->next;
	cleanupState(current);
	free(current);
    }
}

/**
   Prints the contents of the PrefixStorer out to standard out.
 */
static void print(PrefixStorer* ps){
    State* sp;
    int i;
    State *statehead = (State*) (ps->tableBase);

    assert(statehead != NULL);

    for (sp = statehead; sp  != NULL; sp = sp->next) {
	printf("Tuple: ");
	for (i = 0; i < NPREF; i++){
	    printf("<%s>", sp->pref[i]);
	}
	printf("\n");
    }
}

/* lookup: search for prefix; create if requested. */
/*  returns pointer if present or created; NULL if not. */
/*  creation doesn't strdup so strings mustn't change later. */
static State* linked_lookup(char *prefix[NPREF], int create, PrefixStorer* ps)
{
    int i;
    State *sp;
    State *statehead = (State*) (ps->tableBase);
    for (sp = statehead; sp  != NULL; sp = sp->next) {
	for (i = 0; i < NPREF; i++)
	    if (strcmp(prefix[i], sp->pref[i]) != 0)
		break;
	if (i == NPREF)		/* found it */
	    return sp;
    }
    if (create) {
	sp = (State *) emalloc(sizeof(State));
	for (i = 0; i < NPREF; i++)
	    sp->pref[i] = prefix[i];
	sp->suf = NULL;
	sp->next = statehead;
	ps->tableBase = sp;
    }
    return sp;
}


/* add: add word to suffix list, update prefix */
static void linked_add(char *prefix[NPREF], char *suffix, PrefixStorer* ps)
{
    State *sp;
    sp = (ps->lookup)(prefix, 1, ps);  /* create if not found */
    addsuffix(sp, suffix);
    /* move the words down the prefix */
    memmove(prefix, prefix+1, (NPREF-1)*sizeof(prefix[0]));
    prefix[NPREF-1] = suffix;
}
