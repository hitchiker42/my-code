#ifndef _TRIE_H_
#define _TRIE_H_
//the skeleton code uses gnu source so I assume I can too
#define _GNU_SOURCE
#include <alloc.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>
typedef struct trie_node trie_node;
typedef struct trie_head trie_head;
typedef struct sparse_trie_node sparse_trie_node;
enum trie_case {
  TRIE_NOCASE = 0,
  TRIE_LCASE = 1,
  TRIE_UCASE = 2,
  TRIE_LUCASE = 3
};
struct trie_root {
  trie_node *root;
  uint32_t longest;
  /*
    when traversing the trie to find nearest words
    store the current string in an array of size 'longest' elements
   */
  uint32_t num_entries;
};
static inline void *xmalloc(size_t sz){
  void *temp = calloc(sz, 1);
  if(!temp && sz){
    perror("malloc");
    abort();
  } else {
    return temp;
  }
}
#define upcase(c) (c & ~0x20)
#define downcase(c) (c | 0x20)
#define char_to_index(c) (upcase(c) - 0x40)
#define index_to_char(i) (i + 0x40)
static inline int lcase_exists(trie_node *node, int index){
  return XNODE(node)->letters[index] & 0x1;
}
static inline int ucase_exists(trie_node *node, int index){
  return XNODE(node)->letters[index] & 0x2;
}
static inline int char_exists(trie_node *node, char c){
  if(upcase(c) == c){
    return ucase_exists(trie_node, char_to_index(c));
  } else {
    return lcase_exists(trie_node, char_to_index(c));
  }
}
#define XNODE(node) (NODE & ~3)
struct trie_node {
  //this way we automatically allocate 26 chars, but can still
  //manipulate them via a pointer instead of an array
  union {
    trie_node **letters;
    trie_node **letter_mem[26];
  };
};
static inline trie_node *make_node(enum trie_case c){
  trie_node *node = xmalloc(sizeof(trie_node));
  node |= c;
  return node;
}
static inline trie_node *make_node_from_char(char c){
  if(upcase(c) == c){
    return make_node(TRIE_UCASE);
  } else {
    return make_node(TRIE_LCASE);
  }
}
unsigned int trie_check_word(trie_root *trie, char *str, int len,
                             FILE *outfile, unsigned int num_edits);  
#endif
