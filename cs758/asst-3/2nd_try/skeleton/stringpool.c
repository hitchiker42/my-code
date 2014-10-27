/**
   String Pool Allocator
 */

#include <stdlib.h>

#include "stringpool.h"
#include "eprintf.h"

#define SIZE 1024

struct poolNode {
    struct poolNode* next;
    char strings[SIZE];
};

static struct poolNode* base;
static int nextFree;

void freeStringPool(){
    while(base != NULL){
	struct poolNode* old = base;
	base = base->next;
	free(old);
    }
}

void initStringPool(){
    base = emalloc(sizeof(struct poolNode));
    base->next = NULL;
    nextFree = 0;
}

char* requestSpace(unsigned int size){
    if(nextFree + size >= SIZE){
	struct poolNode* oldBase = base;
	base = emalloc(sizeof(struct poolNode));
	base->next = oldBase;
	nextFree = 0;
    }
    nextFree += size;
    return base->strings + nextFree - size;
}
