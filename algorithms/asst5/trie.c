#include "trie.h"
trie *make_trie(void){
  return make_trie_node(NULL, NULL);
}
int trie_add(trie *t, uint8_t *str, int len, void *value){
  if(len <= 0){
    return -1;
  }
  int len2 = len*2, idx = 0;
  trie_node *last = t;
  trie_node *next = t->children[GET_NIBBLE(str,0)];
/*
  By pre-incrementing idx at the start of the loop, we insure that idx == len2
  only happens when next is a valid pointer.
*/
  while(next && ((++idx) < len2)){
    last = next;
    next = next->children[GET_NIBBLE(str, idx)];
  }
  if(idx == len2){
    //value to add was already there
    if(next->added){
      return -1;
    } else {
      next->added = 1;
      next->value = value;
      return 0;
    }
  }
  for(;idx<len;idx++){
    last->children[GET_NIBBLE(str, idx)] = make_trie_node(NULL, last);
    SET_BIT(last->bitmask, GET_NIBBLE(str, idx));
    last = last->children[GET_NIBBLE(str, idx)];
  }
  last->value = value;
  last->added = 1;
  return 0;
}
trie_node *trie_lookup_node(trie *t, uint8_t *str, int len){
  if(len <= 0){
    return t;
  }
  int len2 = len*2, idx = 0;
  trie_node *next = t->children[GET_NIBBLE(str,0)];
  while(next && ((++idx) < len2)){
    next = next->children[GET_NIBBLE(str, idx++)];
  }
  if(idx == len2 && next){
    return next;
  } else {
    return NULL;
  }
}
int trie_lookup(trie *t, uint8_t *str, int len, void **ptr){
  trie_node *node = trie_lookup_node(t,str,len);
  if(node && node->added){
    if(ptr){*ptr = node->value;}
    return 1;
  } else {
    if(ptr){*ptr = NULL;}
    return 0;
  }
}
int trie_remove(trie *t, uint8_t *str, int len){
  int len2 = len*2, idx = 0;
  trie_node *last = t;
  trie_node *next = t->children[GET_NIBBLE(str,0)];
  while(next && ((++idx) < len2)){
    last = next;
    next = next->children[GET_NIBBLE(str, idx)];
  }
  //if len == 0 we skip this whole thing and return 0
  if(idx == len2 && next->added){
    UNSET_BIT(last->bitmask, GET_NIBBLE(str, idx));
    next->added = 0;
    /*
      Free any nodes that are no longer necessary.
      Specifically, the node we're removing if it has no children,
      and all ancestors of that node that only have one child and
      weren't explicitly added.
     */
    while((next->bitmask == 0) && (next->added == 0) && (next->parent)){
      free(next);
      last->children[GET_NIBBLE(str, idx)] = NULL;
      UNSET_BIT(last->bitmask, GET_NIBBLE(str, idx));
      next = last;
      last = next->parent;
    }
  }
  return 0;
}

#define UNCASE(c) (c & ~0x20)
#define TO_INDEX_2(s, i) (TO_INDEX_1(s[i]))
#define TO_INDEX_1(c)                                   \
  __extension__({__typeof(c) _c = c;                    \
      ((UNCASE(_c)-0x41) + 26*GET_BIT(_c, 5));})
#define TO_INDEX(...) VFUNC(TO_INDEX_, __VA_ARGS__)

#define TO_CHAR(i) (i >= 26 ? (i + (0x61 - 26)) : (i + 0x41))

#define TO_BIT(c) (2*(UNCASE(c)-0x41ul) + (c & 0x20 ? 1 : 0))

#define HAS_CASE(t, c) (GET_BIT(t->bitmask, TO_BIT(c)))
#define HAS_ANY(t, c) (HAS_CASE(t,(c & ~0x20)) || HAS_CASE(t,(c | 0x20)))

#define SET_CASE(t, c) (SET_BIT(t->bitmask, TO_BIT(c)))
#define UNSET_CASE(t, c) (UNSET_BIT(t->bitmask, TO_BIT(c)))

trie_dict *make_trie_dict(void){
  return make_trie_dict_node(NULL);
}
int trie_dict_add(trie_dict *t, uint8_t *str, int len){
  if(len <= 0 || !t){
    return -1;
  }
  int idx = 0;
  trie_dict_node *last = t;
  trie_dict_node *next = t->children[TO_INDEX(str, idx)];
  while(last->children[TO_INDEX(str[idx])] && ((++idx) < len)){
    last = next;
    next = next->children[TO_INDEX(str, idx)];
  }
  if(idx == len){
    //value to add was already there
    if(next->added){
      return -1;
    } else {
      next->added = 1;
      return 0;
    }
  }
  for(;idx<len;idx++){
    last->children[TO_INDEX(str,idx)] = make_trie_dict_node(NULL);
    SET_BIT(last->bitmask, TO_INDEX(str[idx]));
    last = last->children[TO_INDEX(str, idx)];
  }
  last->added = 1;
  return 0;
}
trie_dict_node *trie_dict_lookup_node(trie_dict *t, uint8_t *str, int len){
  if(len <= 0 || !t){
    return t;
  }
  int idx = 0;
  trie_dict_node *node = t;
  trie_dict_node *last = NULL;
  while((idx < len) && node->children[TO_INDEX(str[idx])]){
    last = node;
    node = node->children[TO_INDEX(str, idx++)];
  }
  if((idx == len) && node){
    return node;
  } else {
    return NULL;
  }
}
int trie_dict_lookup(trie_dict *t, uint8_t *str, int len){
  trie_dict_node *node = trie_dict_lookup_node(t,str,len);
  return (node && node->added);
}

int trie_dict_remove(trie_dict *t, uint8_t *str, int len){
  if(len <= 0 || !t){
    return 0;
  }
  int idx = 0;
  trie_dict_node *last = t;
  trie_dict_node *next = t->children[TO_INDEX(str,0)];
  while(last->children[TO_INDEX(str[idx])] && ((++idx) < len)){
    last = next;
    next = next->children[TO_INDEX(str, idx)];
  }
  if(idx == len && next->added){
    UNSET_BIT(last->bitmask, TO_INDEX(str[idx]));
    next->added = 0;
    /*
      Free any nodes that are no longer necessary.
      Specifically, the node we're removing if it has no children,
      and all ancestors of that node that only have one child and
      weren't explicitly added.
     */
    while((next->bitmask == 0) && (next->added == 0) && (next->parent)){
      free(next);
      last->children[TO_INDEX(str, idx)] = NULL;
      UNSET_CASE(last, str[idx]);
      next = last;
      last = next->parent;
    }
  }
  return 0;
}
static void print_trie_dict_recurse(trie_dict_node *t, char *str,
                                    char *strptr, FILE *out){
  int i;
  if(t->added){
    fprintf(out, "%s\n", str);
  }
  for(i=0;i<52;i++){
    if(t->children[i]){
      *strptr++ = TO_CHAR(i);
      print_trie_dict_recurse(t->children[i], str, strptr, out);
      *strptr-- = '\0';
    }
  }

}
void print_trie_dict(trie_dict *t, FILE *out){
  //this is unsafe, but I'm mostly writing this function for debugging
  //so it doesn't really matter
  char str[4096];
  memset(str, '\0', sizeof(str));
  if(t){
    print_trie_dict_recurse(t, str, str, out);
  }
}


void destroy_trie_dict(trie_dict *t){
  if(t){
    int i;
    for(i=0;i<52;i++){
      destroy_trie_dict(t->children[i]);
    }
    free(t);
  }
}
