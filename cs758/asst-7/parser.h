#ifndef PARSER_H
#define PARSER_H

#include <stdlib.h>

#define LINEMAX 100
#define MAX_INPUT 1000
#define MAX_CHARS 10000
#define MAX_SYMBOLS 500



/**
   Structure for storing a grammatical production.  This only stores
   the nonterminal productions.
   
   lhs = left hand side

   rhs1 = right hand side 1
   rhs2 = right hand side 2
 */
typedef struct r {
  const struct r* next;
  const char* lhs;
  const char* rhs1;
  const char* rhs2;
  double probability;
} rule;

/**
   Structure for storing a grammatical production.  This only stores
   terminal productions.

   lhs = left hand side

   word = terminal
 */
typedef struct wr{
  const struct wr* next;
  double probability;
  const char*  lhs;
  const char* word;

} word_rule;
struct rule_header {
  struct rule_header *next;
  void *contents[];
};
typedef struct token token;
struct token {
  char *token;
  int len;
};
typedef struct token_table token_table;
struct token_table {  
  token *tokens;
  size_t num_tokens;
  size_t size;
};
/**
   non terminal symbols are stored in an array, and this function is
   used to search for the index of a particular non terminal symbol.
 */
int find_symbol_index(const char* looking_for);


#endif
