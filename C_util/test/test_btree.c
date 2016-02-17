#include "btree.h"
static int min_keys = ((btree_min_degree)-1);
static int max_keys = ((2*btree_min_degree)-1);
//Since the root can violate some properties that other nodes can't
//we have seperate functions to check the entire tree, and to check
//any node that isn't the root.
int check_node(btree_node *node, uint64_t min, uint64_t max);
int check_internal(btree_node *node, uint64_t min, uint64_t max);
int check_leaf(btree_node *node, uint64_t min, uint64_t max);
int check_tree(btree *tree){
  btree_node *root = tree->root;
  if(root->n_keys == 0){
    return 0;
  }
  if(root->n_keys > max_keys){
    HERE_FMT("Too many keys in the root\n");
    return -1;
  }
  int i;
  //we use i<=root->n_keys since btree nodes have one child
  //then they do keys
  uint64_t min = 0, max = root->keys[i];
  //Check to make sure all nodes on the same level are either
  //leaves or internal nodes, not a mix of the two
  int leaf_status = is_leaf(root->children[i]);
  for(i=0;i<=root->n_keys;i++){
    WARN_ON_ONCE(min > max);
    if(leaf_status != is_leaf(root->children[i])){
      HERE_FMT("All leaves are not the same depth");
      return -1;
    }
    int err = check_node(root->children[i], min, max);
    if(err){
      return err;
    }
    min = max;
    max = (i+1 == root->n_keys ? UINT64_T_MAX : root->keys[i+1]);
  }
  return 0;
}
int check_node(btree_node *node, uint64_t min, uint64_t max){
  if(node->n_keys < min_keys){
    HERE_FMT("Too few keys in a node");
    return -1;
  }
  if(node->n_keys > max_keys){
    HERE_FMT("Too many keys in a node");
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
    if(leaf_status != is_leaf(node->children[i])){
      HERE_FMT("All leaves are not the same depth");
      return -1;
    }
    int err = check_node(node->children[i], min, max);
    if(err){
      return err;
    }
    min = max;
    max = (i+1 == node->n_keys ? UINT64_T_MAX : node->keys[i+1]);
  }
  WARN_ON_ONCE(max > true_max);
  return 0;
}
int check_leaf(btree_node *node, uint64_t min, uint64_t max){
  int i;
  if(node->n_keys > max_keys || node->n_keys < min_keys){
    HERE_FMT("Invalid number of keys %ld in leaf node\n",node->n_keys);
    return -1;
  }
  for(i=0;i<node->n_keys;i++){
    if(node->keys[i] < min || node->keys[i] > max){
      HERE();
      return -1;
    }
    min = node->keys[i];
  }
