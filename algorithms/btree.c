#include "btree.h"
#include "farmhash.h"
//uint128_t btree_hash(uint8_t *key, size_t key_sz);
//currently the tree argument is unused, but if I change to using a
//contiguous block of memory I'll need to store metadata in the root
btree_node *allocate_node(btree *tree){
  btree_node *node;
  posix_memalign(&node, PAGE_SIZE, PAGE_SIZE);
  return node;
} 
void *btree_lookup(btree *tree, uint128_t hv){
  int i;
  btree_node *node = tree;
  while(1){
    i = 0;
    while(i<node->n_keys && node->keys[i] < hv){
      i++;
    }
    if(i < node->n_keys && node->keys[i] == hv){
      return node->values[i];
    } else if(is_leaf(node)){
      return NULL;
    } else {
      //disk read
      node = node->children[i];
    }
  }
}
//shift the arrays in the node so that a new entry can be placed 
//at the given index
static void node_create_space(btree_node *node, int index){
  memmove(node->keys + (index+1), node->keys + (index+2),
          index*sizeof(node->keys[0]));
  memmove(node->values + (index+1), node->values + (index+2),
          index*sizeof(node->values[0]));
  if(!is_leaf(node)){
    memmove(node->children + (index+1), node->children + (index+2),
            index*sizeof(node->children[0]));
  }
  node->n_keys++;
} 
static void btree_split_child(btree *tree, btree_node *node, int split){
  btree_node *child = node->children[split];
  btree_node *child2 = allocate_node(tree);
  child2->parent_leaf = child->parent_leaf;
  child2->num_keys = btree_min_degree-1;
  memcpy(child2->keys, child->keys+btree_min_degree, 
         (btree_min_degree-1)*sizeof(child->keys[0]));
  memcpy(child2->values, child->values+btree_min_degree,
         (btree_min_degree-1)*sizeof(child->values[0]));
  if(!is_leaf(child)){
    memcpy(child2->children, child->children+btree_min_degree,
           (btree_min_degree-1)*sizeof(child->children[0]));
  }
  child->n_keys = btree_min_degree-1;
  node_create_space(node, split);
  node->children[split] = child2;
  node->keys[split] = child->keys[btree_min_degree-1];
  node->values[split] = child->values[btree_min_degree-1];
  //write node, child, child2 to disk
}
static void btree_split_root(btree *tree){
  btree_node *child1 = allocate_node(tree);
  btree_node *child2 = allocate_node(tree);
  child1->n_keys = child2->n_keys = (btree_min_degree-1);

  memcpy(child1->keys, tree->root->keys, 
         (btree_min_degree-1)*sizeof(child1->keys[0]));
  memcpy(child1->values, tree->root->values, 
         (btree_min_degree-1)*sizeof(child1->values[0]));

  memcpy(child2->keys, tree->root->keys+btree_min_degree, 
         (btree_min_degree-1)*sizeof(child2->keys[0]));
  memcpy(child2->values, tree->root->values+btree_min_degree, 
         (btree_min_degree-1)*sizeof(child2->values[0]));
  /*
    This is always true, execpt when the root is the only node.
   */
  if(!is_leaf(tree->root)){
    memcpy(child1->children, tree->root->children, 
           (btree_min_degree-1)*sizeof(child1->children[0]));
    memcpy(child2->children, tree->root->children+btree_min_degree, 
           (btree_min_degree-1)*sizeof(child2->children[0]));
  }
  tree->root->keys[0] = child1->keys[0];
  tree->root->values[0] = child1->values[0];
  tree->root->children[0] = child1;

  tree->root->keys[1] = child2->keys[0];
  tree->root->values[1] = child2->values[0];
  tree->root->children[1] = child2;

  root->n_keys = 2;
  //write child1,2 to disk
}
    

void btree_insert(btree *tree, uint64_t key, void *value){
  if(tree->root->n_keys == (2*btree_min_degree)-1){
    btree_split_root(tree);
  }
  btree_node *node = tree->root;
  while(1){
    int i = node->n_keys-1;
    while(i>=0 && key < node->keys[i]){
        i--;
    }
    i++;
    if(is_leaf(node)){      
      //We could copy key/values as we look for where to put the new key
      node_create_space(node, i);
      node->keys[i] = key;
      node->values[i] = value;
      //write node to disk
      return;
    } else {
      //read node->children[i] from disk && split it if it's full
      if(node->children[i].n_keys == ((2*btree_min_degree)-1)){
        btree_split_child(tree, node, i);
        if(key > node->keys[i]){
          i++;
        }
      }
      node = node->children[i];
    }
  }
}
