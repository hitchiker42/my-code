#ifndef MY_PARSER_H
#define MY_PARSER_H
#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include "uthash.h"
#include <ctype.h>
#define LINEMAX 100
#define MAX_INPUT 1000
#define MAX_CHARS 10000
#define MAX_SYMBOLS 500

/**
   Structure for storing a grammatical production.
   
   lhs = left hand side

   rhs1 = right hand side 1, or terminal for terminal productions
   rhs2 = right hand side 2, or null for terminal productions
 */
typedef struct rule rule;
typedef struct rule_vector rule_vector;
typedef struct token token;
struct rule{
  const rule* next;
  token* lhs;
  token* rhs1;
  token* rhs2;
  double probability;
};

struct token {
  char *token;
  UT_hash_handle hh;//this is kinda big, but it makes for an easy hash table
  uint32_t len;
  uint32_t index;
};
struct rule_vector {
  rule **rules;
  size_t len;
  size_t size;
};
void *xmalloc(size_t sz);
static inline int string_equal(const char *a, const char *b){
  return !strcmp(a,b);
}
void malloc_check(void* p);
rule_vector* lex_rules(FILE* f);
void parse(FILE *input, int algorithm, rule_vector *rules);
void cyk_parse(rule_vector *rules, char **input, int input_len);
int token_equal(token *tok1, token *tok2);

static struct token *tokens = NULL;//hash table
static struct token **token_array;
#endif
