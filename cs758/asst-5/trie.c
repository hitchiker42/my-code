//the skeleton code uses gnu source so I assume I can too
#define _GNU_SOURCE
#include <alloca.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
typedef struct trie_node trie_node;
typedef struct trie_root trie_root;
typedef struct sparse_trie_node sparse_trie_node;
enum trie_case {
  TRIE_NOCASE = 0,
  TRIE_LCASE = 1,
  TRIE_UCASE = 2,
  TRIE_LUCASE = 3
};
struct trie_root {
  trie_node *root;
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
__extension__ struct trie_node {
  //this way we automatically allocate 26 chars, but can still
  //manipulate them via a pointer instead of an array
  union {
    trie_node **letters;
    trie_node **letter_mem[26];
    uint64_t val;
  };
  
};
#define XNODE(node) ((trie_node*)((uint64_t)node & ~3UL))
#define upcase(c) (c & ~0x20)
#define downcase(c) (c | 0x20)
#define char_to_index(c) (upcase(c) - 0x40)
#define index_to_char(i) (i + 0x40)
static inline int lcase_exists(trie_node *node, int index){
  return ((uint64_t)(XNODE(node)->letters[index]) & 0x1UL);
}
static inline int ucase_exists(trie_node *node, int index){
  return ((uint64_t)(XNODE(node)->letters[index]) & 0x2UL);
}
static inline int char_exists(trie_node *node, char c){
  if(upcase(c) == c){
    return ucase_exists(node, char_to_index(c));
  } else {
    return lcase_exists(node, char_to_index(c));
  }
}

static inline trie_node *make_node(enum trie_case c){
  trie_node *node = xmalloc(sizeof(trie_node));
  node->val |= (unsigned long)c;
  return node;
}
static inline trie_node *make_node_from_char(char c){
  if(upcase(c) == c){
    return make_node(TRIE_UCASE);
  } else {
    return make_node(TRIE_LCASE);
  }
}
static inline int trie_add_new(trie_node *trie, char *str, int start, int len){
  int i=start;
  while(i < len){
    trie->letters[char_to_index(str[i])] = make_node_from_char(str[i]);
    trie = XNODE(trie->letters[char_to_index(str[i])]);
    i++;
  }
  return 0;
}
int trie_add(trie_root *trie, char *str, int len){
  if(!trie->root){
    trie->root = make_node(TRIE_NOCASE);
    return trie_add_new(trie->root, str, 0, len);
  } else {
    trie_node *node = trie->root;
    int i=0;
    while(i < len &&  char_exists(node, str[i])){
      node = XNODE(node->letters[char_to_index(str[i])]);
      i++;
    }
    if(i == len){
      return 1;
    } else {
      return trie_add_new(node, str, i, len);
    }
  }
}


int trie_lookup(trie_node *trie, char *str, int start, int len){
  trie_node *node = trie;
  int i=start;
  while(i < len &&  char_exists(node, str[i])){
    node = XNODE(node->letters[char_to_index(str[i])]);
    i++;
  }
  if(i == len){
    return 1;
  } else {
    return 0;
  }
}
trie_node *find_nearest(trie_root *trie, char *str, int len){
  if(!trie->root){
    return NULL;
  }
  trie_node *node = trie->root;
  int i=0;
  while(i < len &&  char_exists(node, str[i])){
    node = XNODE(node->letters[char_to_index(str[i])]);
    i++;
  }
  return node;
}
typedef struct sugg_node sugg_node;
struct sugg_node {
  char *word;
  int len;
  struct sugg_node *left;
  struct sugg_node *right;
};
#define create_sugg_node(_word, _len)                     \
  __extension__ ({struct sugg_node *n = alloca(sizeof(sugg_node));     \
  n->word = (strndupa(_word, _len));                      \
  n->left = NULL;                                        \
  n->right = NULL;                                       \
  n->len = _len;                                         \
  n;})

static sugg_node* add_sugg(struct sugg_node *root, char *word, int len);
static void output_suggs(struct sugg_node *root,FILE *outfile);
/*
  This will probably break if adding to the end of the string;
*/
//stupid preprosser  won't do recursion, well not without a buch of work
#define do_edits_1(__word, __sugg, __i, __len)  \
  __extension__ ({int k = __i;                                \
    char c;                                     \
    for(;k<__len;i++){                          \
      char *_edit = strndupa(__word, __len);     \
      for(c = 'a'; c < 'z'; c++){               \
        _edit[k] = c;                            \
        if(trie_lookup(node, _edit, k, __len)){  \
          add_sugg(__sugg, _edit, __len);        \
        }                                       \
        _edit[k] = upcase(c);                    \
        if(trie_lookup(node, _edit, k, __len)){  \
          add_sugg(__sugg, _edit, __len);        \
        }                                       \
      }                                         \
    }                                           \
    __sugg;})
#define do_edits(_word, _sugg, _i, _len, _n)            \
  __extension__({int j = _i;                                         \
    char c;                                             \
    for(;j<_len;i++){                                   \
      char *edit = strndupa(_word, _len);               \
      for(c = 'a'; c < 'z'; c++){                       \
        edit[j] = c;                                    \
        if(trie_lookup(node, edit, j, _len)){           \
          add_sugg(_sugg, edit, _len);                  \
        }                                               \
        if(_n){                                         \
          do_edits_1(edit, _sugg, _i+1, _len);          \
        }                                               \
        edit[i] = upcase(c);                            \
        if(trie_lookup(node, edit, j, _len)){           \
          add_sugg(_sugg, edit, _len);                  \
        }                                               \
        if(_n){                                         \
          do_edits_1(edit, _sugg, _i+1, _len);          \
        }                                               \
      }                                                 \
    }                                                   \
    _sugg;})
unsigned int trie_check_word(trie_root *trie, char *str, int len,
                        FILE *outfile, unsigned int num_edits){
  if(num_edits >2){
    return -1;
  }
  int recurse = (num_edits == 2 ? 1 : 0);
  if(trie_lookup(trie->root, str, 0, len)){
      fprintf(outfile, "correct: %s\n", str);
      return 0;
  }
  if(!trie->root){
    return -1;
  }
  fprintf(outfile, "incorrect: %s\n", str);
  trie_node *node = trie->root;
  int i=0;
  while(i < len &&  char_exists(node, str[i])){
    node = XNODE(node->letters[char_to_index(str[i])]);
    i++;
  }
  char *word = alloca(len+1);
  memcpy(word, str, len);
  sugg_node *suggestions = create_sugg_node(str, len);
  do_edits(word, suggestions, i, len, recurse);
  fprintf(outfile, "suggestions:\n");
  output_suggs(suggestions->left, outfile);
  output_suggs(suggestions->right, outfile);
  fprintf(outfile, "\t----\n");
  return 0;
}

static sugg_node *add_sugg(struct sugg_node *root, char *word, int len){
  int cmp;
  cmp = strncmp(word, root->word, len);
  if (cmp < 0){
    if(!root->left){
      root->left = create_sugg_node(word,len);
      return root->left;
    } else
      return add_sugg(root->left, word, len);
  } else if(cmp > 0) {
    if(!root->right){
      root->right = create_sugg_node(word,len);
      return root->right;
    } else {
      return add_sugg(root->right, word, len);
    }
  }
  return NULL;
}


/* Outputs the suggestions to the given file. */
static void output_suggs(struct sugg_node *root,FILE *outfile){
  if(root){
    output_suggs(root->left,outfile);
    fprintf(outfile, "\t%.*s\n", root->len,root->word);
    output_suggs(root->right,outfile);
  }
}
