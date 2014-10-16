#include "my_parser.h"
#include "cyk.h" //for xmalloc
#include "uthash.h"
#include <ctype.h>
typedef struct rule_vector rule_vector;
typedef struct token token;
struct token {
  char *token;
  UT_hash_handle hh;//this is kinda big, but it makes for an easy hash table
  uint32_t len;
};
struct rule_vector {
  rule **rules;
  size_t len;
  size_t size;
};
static struct token *tokens = NULL;//hash table
rule_vector* lex_rules(FILE* f);
void parse(FILE *input, int algorithm, rule_vector *rules);
