/**
   Utility functions
*/

#include <string.h>
#include <stdlib.h>

#include "storer.h"
#include "eprintf.h"

void cleanupState(State* s){
  if (s == NULL) {
    return;
  }
  /*
    This memory has to be released, but doing it in this manner
    results in double deletions.  Should consider using a pool to
    allocate it all at once and free it all at once.
  */
  /*
    for (i = 0; i < NPREF; i++) {
    free(s->pref[i]);
    }
  */

  Suffix* nextSuffix = s->suf;
  while (nextSuffix != NULL) {
    Suffix* current = nextSuffix;
    nextSuffix = nextSuffix->next;
    free(current);
  }
}
/* addsuffix: add to state. suffix must not change later */
void addsuffix(State *sp, char *suffix){
  /*
    look for the suffix in the list
  */
  Suffix *suf = sp->suf;
  while (suf != NULL && strcmp(suf->word, suffix) != 0) {
    suf = suf->next;
  }
  if (suf == NULL) {
    suf = (Suffix *) emalloc(sizeof(Suffix));
    suf->word = suffix;
    suf->next = sp->suf;
    suf->next = NULL;
    suf->count = 1;
    sp->suf = suf;
  } else {
    suf->count ++;
  }
}
