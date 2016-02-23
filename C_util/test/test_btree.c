#include "C_util.h"
#include "btree.h"
#include "util_string.h"
#include "svector.h"
static int min_keys = ((btree_min_degree)-1);
static int max_keys = ((2*btree_min_degree)-1);
//Since the root can violate some properties that other nodes can't
//we have seperate functions to check the entire tree, and to check
//any node that isn't the root.
int check_node(btree_node *node, uint64_t min, uint64_t max);
int check_internal(btree_node *node, uint64_t min, uint64_t max);
int check_leaf(btree_node *node, uint64_t min, uint64_t max);
int check_tree(btree *tree){
  /*
    We need to treat the root specially since it can violate some properties
  */
  btree_node *root = tree->root;
  if(root->n_keys == 0){
    return 0;
  }
  if(root->n_keys > max_keys){
    HERE_FMT("Too many keys in the root\n");
    return -1;
  }
  if(is_leaf(root)){
    uint64_t min = 0;
    int i;
    for(i=0;i<root->n_keys;i++){
      if(root->keys[i] < min){
        HERE_FMT("key %d = %lu < key %d = %lu\n",
                 i, root->keys[i], i-1, root->keys[i-1]);
        return -1;
      }
      min = root->keys[i];
    }
    return 0 ;
  }
  int i = 0;
  //we use i<=root->n_keys since btree nodes have one child more
  //then they do keys
  uint64_t min = 0;
  uint64_t max = root->keys[0];
  //Check to make sure all nodes on the same level are either
  //leaves or internal nodes, not a mix of the two
  int leaf_status = is_leaf(root->children[i]);
  for(i=0;i<=root->n_keys;i++){
    WARN_ONCE(min > max, "key %d > key %d\n",i,i-1);
    if(leaf_status != is_leaf(root->children[i])){
      HERE_FMT("All leaves are not the same depth");
      return -1;
    }
    int err = check_node(root->children[i], min, max);
    if(err){
      return err;
    }
    min = root->keys[i];
    max = (i+1 == root->n_keys ? UINT64_MAX : root->keys[i+1]);
  }
  return 0;
}
int check_node(btree_node *node, uint64_t min, uint64_t max){
  if(node->n_keys < min_keys){
    HERE_FMT_ONCE("Too few keys in a node\n");
    BREAKPOINT();
    return -1;
  }
  if(node->n_keys > max_keys){
    HERE_FMT_ONCE("Too many keys in a node\n");
    BREAKPOINT();
    return -1;
  }
  if(is_leaf(node)){
    return check_leaf(node, min, max);
  } else {
    return check_internal(node, min, max);
  }
}
int check_internal(btree_node *node, uint64_t min, uint64_t max){
  int i;
  int leaf_status = is_leaf(node->children[0]);
  uint64_t true_max = max;
  max = node->keys[0];
  for(i=0;i<node->n_keys+1;i++){
    WARN_ON_ONCE(min > max);
    if(max > true_max){
      HERE();
      BREAKPOINT();
    }
    if(leaf_status != is_leaf(node->children[i])){
      HERE_FMT("All leaves are not the same depth");
      return -1;
    }
    int err = check_node(node->children[i], min, max);
    if(err){
      return err;
    }
    min = max;
    max = (i+1 == node->n_keys ? true_max : node->keys[i+1]);
  }
  return 0;
}
int check_leaf(btree_node *node, uint64_t min, uint64_t max){
  int i;
  if(node->n_keys > max_keys || node->n_keys < min_keys){
    HERE_FMT("Invalid number of keys %ld in leaf node\n",node->n_keys);
    return -1;
  }
  for(i=0;i<node->n_keys;i++){
    if(node->keys[i] < min){
      HERE_FMT_ONCE("key %d = %lu < min = %lu\n", i, node->keys[i], min);
      BREAKPOINT();
      return -1;
    }
    if(node->keys[i] > max){
      HERE_FMT_ONCE("key %d = %lu > max = %lu\n", i, node->keys[i], max);
      BREAKPOINT();
      return -1;
    }
    min = node->keys[i];
  }
  return 0;
}
uint64_t count_num_keys(btree_node *node){
  if(is_leaf(node)){
    return node->n_keys;
  } else {
    int i;
    uint64_t acc = node->n_keys;
    for(i=0;i<=node->n_keys;i++){
      acc += count_num_keys(node->children[i]);
    }
    return acc;
  }
}
      
int test_btree(string **values, int n){
  int i;
  btree *tree = make_btree();
  uint64_t *hashes = xmalloc(n*sizeof(uint64_t));
  uint64_t *indices = xmalloc(n*sizeof(uint64_t));
  //nfor now I'm just using sequential ints for keys
  DEBUG_PRINTF("Testing btree insertion\n");
  for(i=0;i<n;i++){
    assert(check_tree(tree) >= 0);
    indices[i] = i;
    hashes[i] = fnv_hash(values[i]->mem, values[i]->len);
    btree_insert(tree, hashes[indices[i]], values[i]);
  }
  assert(check_tree(tree) >= 0);
  shuffle_array((void**)indices, n);
  DEBUG_PRINTF("Testing btree lookup\n");
  for(i=0;i<n;i++){
    string *str = btree_lookup(tree, hashes[indices[i]]);
    WARN_ON_ONCE(!string_ptr_eq(str, values[indices[i]]));
  }
  DEBUG_PRINTF("Testing btree deletion\n");  
  for(i=0;i<n;i++){
    uint64_t num_keys = count_num_keys(tree->root);
    if(num_keys != (n-i)){
      DEBUG_PRINTF("Tree has %lu keys, expected %lu\n",
                   num_keys, n-i);
      exit(1);
    }
    assert(check_tree(tree) >= 0);
    if(btree_lookup(tree, hashes[indices[i]])){
      if(!btree_delete(tree, hashes[indices[i]])){
        exit(1);
      }
    } else {
      DEBUG_PRINTF("Couldn't find key %lu\n",indices[i]);
    }
  }
  WARN_ON(check_tree(tree) < 0);
  WARN_ON(tree->root->n_keys != 0);
  return 0;
}
int main(int argc, char *argv[]){
  if(argc < 2){
    printf("usage: test_btree file\n");
    exit(0);
  }
  size_t sz;
  uint8_t *buf = mmap_filename(argv[1], 0, PROT_READ, &sz);
  if(!buf){
    exit(1);
  }
  uint8_t *bufptr = buf;
  struct svector strings = make_svector(100);
  while(bufptr - buf < sz){
    uint8_t *start = bufptr;
    while(!isspace(*bufptr)){
      bufptr++;
    }
    string *str = make_string_ptr(start, bufptr-start);
    svector_push(str, &strings);
    do {
      bufptr++;
    } while((bufptr-buf < sz) && isspace(*bufptr));
  }
  enable_backtraces();
  test_btree((string**)svector_data(strings), svector_len(strings));
}
