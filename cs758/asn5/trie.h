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
  TRIE_LCASE = 1,
  TRIE_UCASE = 2,
  TRIE_LUCASE = 3
};
struct trie_head {
  trie_node *head;
  uint32_t longest;
  /*
    when traversing the trie to find nearest words
    store the current string in an array of size 'longest' elements
   */
  uint32_t entries;
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
  return node->letters[index] & 0x1;
}
static inline int ucase_exists(trie_node *node){
  return node->letters[indeb] & 0x2;
}
static inline int letter_exists(trie_code *node, uint8_t 
#define XNODE(node) (NODE & ~3)
struct trie_node {
  //this way we automatically allocate 26 chars, but can still
  //manipulate them via a pointer instead of an array
  union {
    trie_node **letters;
    trie_node **letter_mem[26];
  };nn
};
static inline trie_node *make_node(enum trie_case c){
  trie_node *node = xmalloc(sizeof(trie_node));
  node |= c;
  return node;
}
int trie_lookup(trie_node *trie, char *str, int len);
int trie_add(trie_node *trie, char *str, int len);
trie_node *find_nearest(trie_node *trie, char *str, int len);
#endif
