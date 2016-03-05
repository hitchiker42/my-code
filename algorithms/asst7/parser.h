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
    double probability;
    const char* lhs;
    const char* rhs1;
    const char* rhs2;
    const struct r* next;
} rule;

/**
   Structure for storing a grammatical production.  This only stores
   terminal productions.

   lhs = left hand side

   word = terminal
 */
typedef struct wr{
    double probability;
    const char*  lhs;
    const char* word;
    const struct wr* next;
} word_rule;
/**
   non terminal symbols are stored in an array, and this function is
   used to search for the index of a particular non terminal symbol.
 */
int find_symbol_index(const char* looking_for);

struct parser {
  const rule *rule_list;
  const word_rule **word_list;
  string *tokens;
  int rule_count;
  int word_count;
  int num_tokens;
};
  

#endif
