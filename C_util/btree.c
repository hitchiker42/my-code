#include "btree.h"
/*
  Utility functions
*/
//Allocate/free a node, the tree structure is passed since in the future
//it may contain metadata
btree_node *allocate_node(btree *tree){
  btree_node *node;
  posix_memalign((void**)&node, PAGE_SIZE, PAGE_SIZE);
  return node;
}
void free_node(btree *tree, btree_node *node){
  free(node);
}
/*
  Copy the key at src_index in src to dest_index in dest.
  This is just to save typing.
*/
static void copy_key(btree_node *src, int src_index,
                     btree_node *dest, int dest_index){
  dest->keys[dest_index] = src->keys[src_index];
  dest->values[dest_index] = src->values[src_index];
  return;
}
//Remove the key at the given index from node by shifting arrays
static void remove_key(btree_node *node, int index){
  memmove(node->keys+(index-1), node->keys+index, node->n_keys-index);
  memmove(node->values+(index-1), node->values+index, node->n_keys-index);
  node->n_keys--;
}
/*
  Lookup key in tree and return its value, or NULL if it isn't in the tree
*/
void *btree_lookup(btree *tree, uint64_t key){
  int i;
  btree_node *node = tree->root;
  while(1){
    i = 0;
    while(i<node->n_keys && node->keys[i] < key){
      i++;
    }
    if(i < node->n_keys && node->keys[i] == key){
      return node->values[i];
    } else if(is_leaf(node)){
      return NULL;
    } else {
      //disk read
      node = node->children[i];
    }
  }
}
/*
  Insertion + needed auxiliary functions
*/
/*
  Shift the arrays in the node so that a new entry can be placed
  at the given index.
*/
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
/*
  Split node->children[split] into two nodes.
*/
static void btree_split_child(btree *tree, btree_node *node, int split){
  btree_node *child = node->children[split];
  btree_node *child2 = allocate_node(tree);
  child2->parent_leaf = child->parent_leaf;
  child2->n_keys = btree_min_degree-1;
  memcpy(child2->keys, child->keys+(btree_min_degree-1),
         (btree_min_degree-1)*sizeof(child->keys[0]));
  memcpy(child2->values, child->values+(btree_min_degree-1),
         (btree_min_degree-1)*sizeof(child->values[0]));
  if(!is_leaf(child)){
    memcpy(child2->children, child->children+(btree_min_degree-1),
           (btree_min_degree-1)*sizeof(child->children[0]));
  }
  child->n_keys = btree_min_degree-1;
  node_create_space(node, split);
  node->children[split] = child2;
  copy_key(node, split, child, btree_min_degree-1);
  //write node, child, child2 to disk
}
/*
  Split the root into two children, leaving the root with only 1 key
  and 2 children.
*/
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
    Also, we move all of the children, since the new nodes should
    be the only children of the root.
   */
  if(!is_leaf(tree->root)){
    memcpy(child1->children, tree->root->children,
           btree_min_degree*sizeof(child1->children[0]));
    memcpy(child2->children, tree->root->children+btree_min_degree,
           btree_min_degree*sizeof(child2->children[0]));
  }
  copy_key(tree->root, 0, tree->root, btree_min_degree-1);
  tree->root->children[0] = child1;
  tree->root->children[1] = child2;

  tree->root->n_keys = 1;
  //write child1,2 to disk
}
/*
  Add key into the tree with the given value.
*/
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
      if(node->children[i]->n_keys == ((2*btree_min_degree)-1)){
        btree_split_child(tree, node, i);
        if(key > node->keys[i]){
          i++;
        }
      }
      node = node->children[i];
    }
  }
}
//forward declarations
static void *remove_from_leaf(btree *tree, btree_node *node, int index);
static void *remove_from_internal(btree *tree, btree_node *node, int index);
/*
  Remove the key at index i from node, node must have at least
  btree_min_degree keys when this is called.
*/
static void *remove_from_node(btree *tree, btree_node *node, int i){
  if(is_leaf(node)){
    return remove_from_leaf(tree, node, i);
  } else {
    return remove_from_internal(tree, node, i);
  }
}
/*
  Combine a and b by moving all of the keys in b into a, and freeing b
*/
static void merge_nodes(btree *tree, btree_node *a, btree_node *b){
  //when this gets call a has btree_min_degree keys, since a key
  //from it's parent has already been added to it.
  memcpy(a->keys+btree_min_degree, b->keys,
          sizeof(a->keys[0])*(btree_min_degree-1));
  memcpy(a->values+btree_min_degree, b->values,
          sizeof(a->values[0])*(btree_min_degree-1));
  if(!is_leaf(a)){
    memcpy(a->children+(btree_min_degree), b->children,
            sizeof(a->children[0])*(btree_min_degree));
  }
  free_node(tree, b);
}
/*
  Merge child1 and child2, setting the key at parent[i] to the median
  key of the merged node. parent must have at least btree_min_degree keys when
  this is called, to avoid violating the properties of btrees
*/
static void merge_children(btree *tree, btree_node *parent,
                           btree_node *child1, btree_node *child2, int i){
  //move the key at index i in parent into child1
  copy_key(child1, btree_min_degree-1, parent, i);
  //remove the key at index i from parent
  remove_key(parent, i);
  //merge child1 and child2, and free child2
  merge_nodes(tree, child1, child2);
  //If parent is the root, and we just removed the last key from it we
  //need to make the newly merged node the root
  if(tree->root == parent && (parent->n_keys == 0)){
    tree->root = child1;
    free_node(tree, parent);
  } else {
    //delete child2 from parent
    memmove(parent->children+(i+1), parent->children+(i+2),
            parent->n_keys-i);
  }
  return;
}
/*
  Remove the key at the given index from a leaf node, the node needs
  to have at least btree_min_degree keys when this is called
*/
static void *remove_from_leaf(btree *tree, btree_node *node, int index){
  void *value = node->values[index];
  remove_key(node, index);
  return value;
}
/*
  Remove the key at the given index from an internal node, the node needs
  to have at least btree_min_degree keys when this is called
*/
static void *remove_from_internal(btree *tree, btree_node *node,  int i){
  btree_node *child1, *child2;
  void *ret = node->values[i];
  child1 = node->children[i];//keys < the key being removed
  if(child1->n_keys >= btree_min_degree){
    child1->n_keys--;
    copy_key(node, i, child1, child1->n_keys);
    return ret;
  }
  //this is ok to do, since a node always has one more child than it does keys
  child2 = node->children[i+1];//keys > the key being removed
  if(child2->n_keys >= btree_min_degree){
    child2->n_keys--;
    copy_key(node, i, child2, child2->n_keys);
    return ret;
  }
  //This moves the key we're removing down, so we don't need to return
  //the value we saved
  merge_children(tree, node, child1, child2, i);
  return remove_from_node(tree, child1, (btree_min_degree-1));
}
/*
  If key is in the tree remove it and return the value it had, otherwise
  return NULL.
*/
void *btree_delete(btree *tree, uint64_t key){
  int i;
  btree_node *node = tree->root;
  while(1){
    i = 0;
    while(i<node->n_keys && node->keys[i] < key){
      i++;
    }
    if(i < node->n_keys && node->keys[i] == key){
      return remove_from_node(tree, node, i);
    } else if(is_leaf(node)){
      //We can't remove key, since it's not in the tree
      return NULL;
    } else {
      btree_node *child = node->children[i];
      if(child->n_keys < btree_min_degree){
        //If one of child's siblings has enough keys move a key from
        //node into child, and then a key from the sibling into node
        if(node->children[i+1]->n_keys >= btree_min_degree){
          btree_node *sibling = node->children[i+1];
          //move key from node to child
          copy_key(child, child->n_keys, node, i);
          child->n_keys++;
          //move key from sibling to node
          copy_key(node, i, sibling, 0);
          remove_key(sibling, 0);
          //if necessary move a child from sibling to child
          if(!is_leaf(child)){
            child->children[child->n_keys] = sibling->children[0];
            memmove(sibling->children, sibling->children+1,
                    sibling->n_keys+1);
          }
          //Insure we don't try to access a negitive index
        } else if(i != 0 && (node->children[i-1]->n_keys >= btree_min_degree)){
          btree_node *sibling = node->children[i-1];
          //move key from node to child
          copy_key(child, child->n_keys, node, i);
          child->n_keys++;
          //move key from sibling to node
          copy_key(node, i, sibling, sibling->n_keys-1);
          sibling->n_keys--;//removing the last key is easy
          //if necessary move a child from sibling to child
          if(!is_leaf(child)){
            child->children[child->n_keys] = sibling->children[sibling->n_keys];
          }
        } else {
          //Combine childa and one of it's siblings, we always use the right
          //child, since it's guaranteed to exist
          merge_children(tree, node, node->children[i], node->children[i+1], i);
        }
        //recurse on the child node
        node = node->children[i];
      }
    }
  }
}
