#include "C_util.h"
#include "trie.h"

int test_trie(string **values, int n){
  trie *t = make_trie();
  int i = 0;
  for(i=0;i<n;i++){
    trie_add(t, values[i]->mem, values[i]->len, (void*)i);
  }
}
