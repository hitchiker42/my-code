/* Copyright (C) 1999 Lucent Technologies */
/* Excerpted from 'The Practice of Programming' */
/* by Brian W. Kernighan and Rob Pike */

/**
   Extensively Modified by Christopher Wilt as assignment 3 for
   University of New Hampshire Compuer Science 758/858.
*/

/*
 * Markov chain random text generator.
 */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <stdbool.h>
#include "eprintf.h"
#include "storer.h"
#include "htable.h"
#include "linked.h"
#include "stringpool.h"

void build(char *prefix[], FILE*, PrefixStorer* ps);
void generate(int nwords, PrefixStorer* ps);
void printWord(char *c);


char NONWORD[] = "\n";  /* cannot appear as real word */


/* Print the usage string and exit. */
static void usage(){
  fprintf(stderr, "Usage:\n");
  fprintf(stderr,
          "babbler <structure> <opt_args> <wordcount> <c>\n"
          "where <structure> is one of: hashtable or linkedlist\n"
          "if you use a hashtable, <opt> determins the hash function\n"
          "<wordcount> must be greater than or equal to zero.\n"
          "<opt> is one of: bad, kr, cw, add3\n"
          "If <c> is used, and the data structre is hash table, it \n"
          "will output the bucket lengths.\n"
          " Outputs a babbled text on standard out\n");
  exit(EXIT_FAILURE);
}


/* markov main: markov-chain random text generation */
int main(int argc, char** argv){
  PrefixStorer* ps;
  int i, nwords;
  char *prefix[NPREF];		/* current input prefix */
  long seed;
  bool printCounts = false;
  /*
    Command Line Argument parsing
  */
  if (argc < 3) {
    usage();
  }
  if (argc > 5) {
    usage();
  }

  if (strcmp(argv[1], "linkedlist") == 0) {
    ps = makeLinkedPrefixStorer();
    nwords = atoi(argv[2]);
    if (nwords <= 0 || argc >= 4) {
      usage();
    }
  }
  else if (strcmp(argv[1], "hashtable") == 0) {

	
    if (argc == 3) {
      usage();
    }
    //have to set the hash function.

    nwords = atoi(argv[3]);
    if (nwords <= 0) {
      usage();
    }
    if (argc == 5 && strcmp(argv[4], "c") == 0) {
      printCounts = true;
    } else if (argc == 5) {
      usage();
    }
	
    enum hashType ht;

    if (strcmp(argv[2], "bad") == 0) {
      ht = BAD;
    }
    else if (strcmp(argv[2], "kr") == 0) {
      ht = KR;
    }
    else if (strcmp(argv[2], "cw") == 0) {
      ht = CW;
    }
    else if (strcmp(argv[2], "add3") == 0) {
      ht = ADD3;
    }
    else 
      usage();
    ps = makeHashPrefixStorer(ht);
	
  } else {
    usage();
  }

  /*
    Done command line argument parsing, actually time to start up.
  */

  initStringPool();

  seed = time(NULL);
  srand(seed);
  for (i = 0; i < NPREF; i++) {	/* set up initial prefix */
    prefix[i] = NONWORD;
  }
  build(prefix, stdin, ps);
  (ps->add)(prefix, NONWORD, ps);
  if (printCounts) {
    (ps->printCounts)(ps);
  }

  generate(nwords, ps);
  fprintf(stderr,"\n");

  (ps->cleanup)(ps);
  free(ps);
  freeStringPool();
  return 0;
}

/* build: read input, build prefix table */
void build(char *prefix[NPREF], FILE *f, PrefixStorer* ps){
  char buf[100], fmt[10];

  /* create a format string; %s could overflow buf */
  sprintf(fmt, "%%%lds", sizeof(buf)-1);
  while (fscanf(f, fmt, buf) != EOF) {
    (ps->add)(prefix, estrdup(buf), ps);
  }
}

/* generate: produce output, one word per line */
void generate(int nwords, PrefixStorer* ps){

  State *sp;
  Suffix *suf;
  char *prefix[NPREF], *w = NULL;
  int i, j, nmatch;

  for (i = 0; i < NPREF; i++) {	/* reset initial prefix */
    prefix[i] = NONWORD;
  }

  for (i = 0; i < nwords; i++) {
    sp = (ps->lookup)(prefix, 0, ps);
    if (sp == NULL) {
      fprintf(stderr, "\nwas looking for this");
      for (j = 0; j < NPREF; j++) {
        fprintf(stderr, "<%s>", prefix[i]);
      }
      eprintf("internal error: lookup failed");
		
    }
    nmatch = 0;
    for (suf = sp->suf; suf != NULL; suf = suf->next) {
      for (j = 0; j < suf->count; j++) {
        if (rand() % ++nmatch == 0) {/* prob = 1/nmatch */
          w = suf->word;
        }
      }
    }
    if (nmatch == 0) {
      eprintf("internal error: no suffix %d %s", i, prefix[0]);
    }
    if (strcmp(w, NONWORD) == 0) {
      break;
    }
    printWord(w);
    memmove(prefix, prefix+1, (NPREF-1)*sizeof(prefix[0]));
    prefix[NPREF-1] = w;
  }
}



/* returns 1 if c is puntutaion, 0 if not */
int isPunctuation(char c){
  if(c == ',' || c == ';' || c == '.' || c == '!' ||
     c == '?' || c == '\"'|| c == '\'' || c == ':'||
     c == '(' || c == ')' || c == '{' || c == '}'){
    return 1;
  } else {
    return 0;
  }
}

int output = 0;
const int TERM_WIDTH = 80;

/* Makes sure the output doesn't look terrible, that it fits on a standard      
   terminal, that punctuation isn't preceded by a space, and so on. */
void printWord(char *c){
  int len = strlen(c);
  output += len;
  if (output >= TERM_WIDTH) {
    fprintf(stderr, "\n%s", c);
    output = len;
  } else {
    if (isPunctuation(*c)) {
      fprintf(stderr, "%s", c);
    } else {
      output += 1;
      fprintf(stderr, " %s", c);
    }
  }
}


