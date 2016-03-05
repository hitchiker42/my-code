#ifndef __TRIE_H__
#define __TRIE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "C_util.h"
#include "util_string.h"
/*
  TODO: Either make trie a proper type and add a starting node argument to
  all the functions, or just have the functions take trie_nodes.
*/
/*
  Two types of tries, a generic trie which uses arbitary bitstrings as keys,
  and a trie specialized to use english words as keys.
*/
typedef struct trie_dict_node trie_dict_node;
typedef struct trie_node trie_node;

typedef trie_dict_node trie_dict;
typedef trie_node trie;

/*
  4 bit trie. Requires O(2m) time to lookup an m byte key, but uses
  significantly less space than an 8 bit trie. Really an 8 bit trie wastes
  way too much space to be efficent.
*/
#define GET_NIBBLE(str, idx)                    \
  __extension__                                 \
  ({__typeof(idx) i = idx;                      \
    (str[i/2] & (0xf << (4 * (i % 2))));})
struct trie {
  struct trie_node *head;
};
struct trie_node {
  struct trie_node *children[16];
  struct trie_node *parent;
  void *value;
  uint16_t bitmask;//bitmask of valid values in children
  uint16_t added;//1 if this node was explicitly added to the trie
  //32 bits of padding (for a 64 bit target)
};
static inline trie_node* make_trie_node(void *val, trie_node *parent){
  trie_node *node = zmalloc(sizeof(trie_node));
  node->value = val;
  node->parent = parent;
  return node;
}
trie *make_trie(void);
/*
  add str to the trie with the given value, returns 0 if str was added,
  and < 0 if it was already in the trie.
*/
int trie_add(trie *t, uint8_t *str, int len, void *value);
static inline int trie_add_str(trie *t, char *str, void *value){
  return trie_add(t, (uint8_t*)str, strlen(str), value);
}
/*
  Return 1 if str is in the trie, 0 if it is not.
  If ptr is not NULL store the value of str in ptr. NULL is stored in ptr
  if str is not in the trie. Keep in mind NULL is a perfectly valid value for
  a key to have in the trie.
*/
int trie_lookup(trie *t, uint8_t *str, int len, void **ptr);
static inline int trie_lookup_str(trie *t, char *str, void **ptr){
  return trie_lookup(t, (uint8_t*)str, strlen(str), ptr);
}
//provided to make adding additional trie functions eaiser
trie_node *trie_lookup_node(trie *t, uint8_t *str, int len);
/*
  Remove str from the trie, return 1 if str was actually removed.
*/
int trie_remove(trie *t, uint8_t *str, int len);
static inline int trie_remove_str(trie *t, char *str){
  return trie_remove(t, (uint8_t*)str, strlen(str));
}
/*
  I tried using a fancy bitmask to keep track of case, but it ended up not
  working. I'm sure theres a ways to do it, I just didn't have the time
  to figure it out.
*/
/*
  Trie designed to be used as a dictionary for english words. Instead of
  a key each entry (effectively) has 2 flags per english letter which indicate 
  if there is a word with that letter in either lower or upper case, along with
  a flag indicating if the node itself represents a word.
*/
struct trie_dict {
  struct trie_dict_node *head;
};
struct trie_dict_node {
  struct trie_dict_node *children[52];
  struct trie_dict_node *parent;
  //2 bits per letter, one for upper/lower case
  //this only uses 52 bits, so we can use the last byte to
  //indicate if this node represents a word
//  union {
    uint64_t bitmask;
//    struct {
//      uint8_t bitmask_bytes[7];
//      uint8_t added;
//    };
//  };
  uint8_t added;
};
/*enum trie_dict_case {
  UPPER_CASE = 1,
  LOWER_CASE = 2,
  BOTH_CASES = 3,
};*/
static inline trie_dict_node* make_trie_dict_node(trie_dict_node *parent){
  trie_dict_node *node = zmalloc(sizeof(trie_dict_node));
  node->parent = parent;
  return node;
}
trie_dict *make_trie_dict(void);
/*
  add str to the trie with the given value, returns 0 if str was added,
  and < 0 if it was already in the trie.
*/
int trie_dict_add(trie_dict *t, uint8_t *str, int len);
static inline int trie_dict_add_cstr(trie_dict *t, 
                                     char *str){
  return trie_dict_add(t, (uint8_t*)str, strlen(str));
}
/*
  Return 1 if str is in the trie, 0 if it is not.
  If ptr is not NULL store the value of str in ptr. NULL is stored in ptr
  if str is not in the trie. Keep in mind NULL is a perfectly valid value for
  a key to have in the trie.
*/
int trie_dict_lookup(trie_dict *t, uint8_t *str, int len);
static inline int trie_dict_lookup_cstr(trie_dict *t, 
                                        char *str){
  return trie_dict_lookup(t, (uint8_t*)str, strlen(str));
}
/*
  If str in in the tree, either as a word, or the prefix of another word
  return the node coorsponding to str.
*/
trie_dict_node *trie_dict_lookup_node(trie_dict *t, uint8_t *str, int len);
/*
    Remove str from the trie, return 1 if str was actually removed.
*/
int trie_dict_remove(trie_dict *t, uint8_t *str, int len);
static inline int trie_dict_remove_cstr(trie_dict *t, char *str){
  return trie_dict_remove(t, (uint8_t*)str, strlen(str));
}
void destroy_trie_dict(trie_dict *t);
void print_trie_dict(trie_dict *t, FILE *out);
#ifdef __cplusplus
}
#endif
#endif /* __TRIE_H__ */
