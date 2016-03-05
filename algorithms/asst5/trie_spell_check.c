#include "C_util.h"
#include "trie.h"
#include "rbtree.h"
/*
  3 types of edits:
  Substutions, change one letter of word
  Additions, add one letter to word, at an arbitary position
  Deletions, delete one letter from word, at an arbitary position
*/
/*
  Some utility functions for the trie.
*/
extern int add_delete;
static int check_edits(trie_dict *t, string word, int edits, rb_tree *suggs);
//is t an empty trie
#define is_empty(t) ((t->bitmask == 0) && (t->parent == NULL))
static void add_suggestion(rb_tree *suggs, string word){
  if(!(rb_lookup(suggs, &word))){
    DEBUG_PRINTF("adding suggestion %.*s\n", (int)word.len, word.str);
    string *ptr = xmalloc(sizeof(string) + word.sz);
    ptr->sz = word.sz; ptr->mem = (((void*)ptr)+sizeof(string));
    memcpy(ptr->mem, word.mem, word.sz);
    rb_insert(suggs, ptr);
  }
}
//(loop for i from #x41 to #x60 doing (insert (format "'%c', '%c'," (+ i #x20) i)))

static char eng_alphabet[52] =
  {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
   'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
   'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
   'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'};

#define UNCASE(c) (c & ~0x20)
#define TO_INDEX(c)                                     \
  __extension__({__typeof(c) _c = c;                    \
      ((UNCASE(_c)-0x41) + 26*GET_BIT(_c, 5));})

static int check_subs(trie_dict *t, string word, int edits, rb_tree *suggs){
  int i, j;
  char c, ch;
  string tmp;
  tmp.len = word.len;
  tmp.str = alloca(tmp.len);
  memcpy(tmp.mem, word.mem, word.len);
  for(i=0;i<word.len;i++){
    trie_dict_node *node = trie_dict_lookup_node(t, word.mem, i);
    if(!node){
      continue;
    }
    ch = word.str[i];
    for(j=0;j<52;j++){
      if(ch == eng_alphabet[j]){continue;}
      tmp.str[i] = eng_alphabet[j];
      if(trie_dict_lookup(node->children[j], word.mem + (i+1),
                          word.len - (i+1))){
        add_suggestion(suggs, tmp);
      }
      check_edits(t, tmp, edits-1, suggs);
    }
    tmp.str[i] = ch;
  }
  return 0;
}
static int check_adds(trie_dict *t, string word, int edits, rb_tree *suggs){
  int i, j;
  char c, ch;
  string tmp;
  tmp.len = word.len+1;
  tmp.str = alloca(tmp.len);
  for(i=0;i<=word.len;i++){
/*
  I might be off on the indexing here
*/
    trie_dict_node *node = trie_dict_lookup_node(t, word.mem, i);
    if(!node){
      continue;
    }
    memcpy(tmp.str, word.str, i);
    memcpy(tmp.str + i + 1, word.str + i, word.len - i);
    for(j=0;j<52;j++){
      tmp.str[i] = eng_alphabet[j];
      if(trie_dict_lookup(node->children[j], word.mem + i,
                          word.len - i)){
        add_suggestion(suggs, tmp);
      }
      check_edits(t, tmp, edits-1, suggs);
    }
  }
  return 0;
}
static int check_dels(trie_dict *t, string word, int edits, rb_tree *suggs){
  int i, j;
  char c, ch;
  string tmp;
  tmp.len = word.len-1;
  tmp.str = alloca(tmp.len);
//it may be possible to optimize this using the trie, But it doesn't seem
//like it'd be worth it
  for(i=0;i<word.len;i++){
    memcpy(tmp.str, word.str, i);
    memcpy(tmp.str + i, word.str + i + 1, word.len - (i+1));
    if(trie_dict_lookup(t, tmp.mem, tmp.len)){
      //      DEBUG_PRINTF("adding suggestion (del) %.*s\n", (int)tmp.len, tmp.str);
      add_suggestion(suggs, tmp);
    }
    check_edits(t, tmp, edits-1, suggs);
  }
  return 0;
}
static int check_edits(trie_dict *t, string word, int edits, rb_tree *suggs){
  if(edits > 0){
    //    DEBUG_PRINTF("checking %.*s (%d edits)\n",(int)word.len,word.str,edits);
    check_subs(t, word, edits, suggs);
    if(add_delete){
      DEBUG_PRINTF("checking adds and deletes");
      check_adds(t, word, edits, suggs);
      check_dels(t, word, edits, suggs);
    }
  }
  return 0;
}
static void output_suggest_visit(rb_node *node, void *data){
  FILE *out = data;
  string *word = node->data;
  DEBUG_PRINTF("\t%.*s\n", (int)word->sz, word->str);
  fprintf(out, "\t%.*s\n", (int)word->sz, word->str);
}
/*
  Potential optimization, have a static variable for a freelist of rbtree nodes
*/
int trie_check_word(trie_dict *t, string word, FILE *out, int n_edits){
  char *tmp = string_to_cstra(word);
  //  DEBUG_PRINTF("Checking %s\n", tmp);
  if(trie_dict_lookup(t, word.mem, word.len)){
    fprintf(out, "correct: %s\n", tmp);
  } else {
    rb_tree *suggs = make_empty_rbtree((int(*)(void*,void*))string_ptr_cmp);
    fprintf(out, "incorrect: %s\n", tmp);
    check_edits(t, word, n_edits, suggs);
    fprintf(out, "suggestions:\n");
    rb_traverse(suggs, out, output_suggest_visit);
    fprintf(out, "\t----\n");
    destroy_rbtree_custom(suggs, free);
  }
  return 0;
}
