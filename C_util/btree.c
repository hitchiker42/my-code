#include "btree.h"
/*
  Utility functions
*/
  
//Allocate/free a node, the tree structure is passed since in the future
//it may contain metadata
btree_node *allocate_node(btree *tree){
  btree_node *node;
  posix_memalign((void**)&node, PAGE_SIZE, PAGE_SIZE);
  node->n_keys = 0;
  return node;
}
void free_node(btree *tree, btree_node *node){
  free(node);
}
btree *make_btree(void){
  btree *tree = xmalloc(sizeof(btree));
  tree->root = allocate_node(tree);
  set_leaf(tree->root);
  return tree;
}
/*
  Copy the key at src_index in src to dest_index in dest.
  This is just to save typing.
*/
static void copy_key(btree_node *dest, int dest_index,
                     btree_node *src, int src_index){
  dest->keys[dest_index] = src->keys[src_index];
  dest->values[dest_index] = src->values[src_index];
  return;
}
//Remove the key at the given index from node by shifting arrays
//doesn't deal with children
static void remove_key(btree_node *node, int index){
  node->n_keys--;
  memmove(node->keys+index, node->keys+(index+1), 
          (node->n_keys-index)*sizeof(node->keys[0]));
  memmove(node->values+index, node->values+(index+1), 
          (node->n_keys-index)*sizeof(node->values[0]));
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
  if(index < node->n_keys){
    memmove(node->keys + (index+1), node->keys + (index),
            (node->n_keys-index)*sizeof(node->keys[0]));
    memmove(node->values + (index+1), node->values + (index),
            (node->n_keys-index)*sizeof(node->values[0]));
    if(!is_leaf(node)){
      memmove(node->children + (index+1), node->children + (index),
              ((node->n_keys+1)-index)*sizeof(node->children[0]));
    }
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
  memcpy(child2->keys, child->keys+(btree_min_degree),
         (btree_min_degree-1)*sizeof(child->keys[0]));
  memcpy(child2->values, child->values+(btree_min_degree),
         (btree_min_degree-1)*sizeof(child->values[0]));
  if(!is_leaf(child)){
    memcpy(child2->children, child->children+(btree_min_degree),
           (btree_min_degree)*sizeof(child->children[0]));
  }
  child->n_keys = btree_min_degree-1;
  memmove(node->keys + (split+1), node->keys + (split),
            (node->n_keys-split)*sizeof(node->keys[0]));
  memmove(node->values + (split+1), node->values + (split),
          (node->n_keys-split)*sizeof(node->values[0]));
  memmove(node->children + (split+2), node->children + (split+1),
          ((node->n_keys+1)-split)*sizeof(node->children[0]));
//  node_create_space(node, split);
  node->children[split+1] = child2;
  copy_key(node, split, child, btree_min_degree-1);
  node->n_keys++;
  //write node, child, child2 to disk

}
/*
  Split the root into two children, leaving the root with only 1 key
  and 2 children.
*/
static void btree_split_root(btree *tree){
//  DEBUG_PRINTF("Splitting root\n");
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
  } else {
    set_leaf(child1);
    set_leaf(child2);
    unset_leaf(tree->root);
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
        assert(node->children[i+1]);
        if(key > node->keys[i]){
          i++;
        }
      }
      node = node->children[i];
    }
  }
}
//forward declarations
static void *btree_delete_internal(btree *tree, 
                                   btree_node *node, uint64_t key);
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
  //when this gets called 'a' has btree_min_degree keys, since a key
  //from it's parent has already been added to it. If it is an internal
  //node it now has an equal number of children and keys, so when we
  //merge 'b's children we don't end up with an extra child
  memcpy(a->keys+(btree_min_degree), b->keys,
          sizeof(a->keys[0])*(btree_min_degree-1));
  memcpy(a->values+(btree_min_degree), b->values,
          sizeof(a->values[0])*(btree_min_degree-1));
  if(!is_leaf(a)){
    memcpy(a->children+(btree_min_degree), b->children,
            sizeof(a->children[0])*(btree_min_degree));
  }
  a->n_keys += btree_min_degree;
  free_node(tree, b);
}
/*
  Merge child1 and child2, setting the key at parent[i] to the median
  key of the merged node. parent must have at least btree_min_degree keys when
  this is called, to avoid violating the properties of btrees.

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
    child1->parent_leaf = is_leaf(child1);
    tree->root = child1;
    free_node(tree, parent);
  } else {
    //delete child2 from parent
    memmove(parent->children+(i+1), parent->children+(i+2),
            (parent->n_keys-i)*sizeof(parent->children[0]));
  }  
}
/*
  Remove the key at the given index from a leaf node, the node needs
  to have at least btree_min_degree keys when this is called
*/
static void *remove_from_leaf(btree *tree, btree_node *node, int index){
  int i;
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
  //this is ok to do, since a node always has one more child than it does keys
  child2 = node->children[i+1];//keys > the key being removed
  /*
    We need to find the successor/predecessor of node->keys[i], to
    replace it. If the children of node aren't leaves, we need to decend
    the tree untill we get to a leaf to find the predecessor/successor.

    I currently call btree_delete_internal recursively to do this,
    there are probably more efficent ways, but they would involve reorginizing
    my deletion code.
  */
  if(child1->n_keys >= btree_min_degree){
    btree_node *tmp = child1;
    if(!is_leaf(tmp)){
      do {
        tmp = tmp->children[tmp->n_keys];
      } while(!is_leaf(tmp));
      copy_key(node, i, tmp, tmp->n_keys-1);
      btree_delete_internal(tree, child1, tmp->keys[tmp->n_keys-1]);
    } else {
      copy_key(node, i, child1, child1->n_keys-1);
      remove_from_leaf(tree, child1, child1->n_keys-1);
    }
    return ret;
  } else if(child2->n_keys >= btree_min_degree){
    btree_node *tmp = child2;
    if(!is_leaf(tmp)){
      do {
        tmp = tmp->children[0];
      } while(!is_leaf(tmp));
      copy_key(node, i, tmp, 0);
      btree_delete_internal(tree, child1, tmp->keys[0]);
    } else {
      copy_key(node, i, child2, 0);
      remove_from_leaf(tree, child2, 0);
    }
    return ret;
  } else {
    //This moves the key we're removing down, so we don't need to return
    //the value we saved
    merge_children(tree, node, child1, child2, i);
    return remove_from_node(tree, child1, (btree_min_degree-1));
  }
}
/*
  Remove key from the subtree of tree, rooted at node.
*/
static void *btree_delete_internal(btree *tree, 
                                   btree_node *node, uint64_t key){
  int i;
  while(1){
    i = 0;
    while(i<node->n_keys && node->keys[i] < key){
      i++;
    }
    if(i < node->n_keys && node->keys[i] == key){
      //We found the key, now delete it
      return remove_from_node(tree, node, i);
    } else if(is_leaf(node)){
      DEBUG_PRINTF("Failed to remove key %lu\n",key);
      //We can't remove key, since it's not in the tree
      return NULL;
    } else {
      //Make sure the child that could have the key has at least
      //btree_min_degree keys, so removing it doesn't violate
      //the minimum number of keys property
      btree_node *child = node->children[i];
      if(child->n_keys < btree_min_degree){
        /*
          The child doesn't have enough keys, we need to fix that.
          If one of child's siblings has enough keys move a key from
          node into child, and then a key from the sibling into node
        */
        //take a key from the right sibling
        if(i != node->n_keys && node->children[i+1]->n_keys >= btree_min_degree){
//          DEBUG_PRINTF("Moving key from right child\n");
          btree_node *sibling = node->children[i+1];
          //move key from node to child
          copy_key(child, child->n_keys, node, i);
          child->n_keys++;
          //move key from sibling to node
          copy_key(node, i, sibling, 0);
          remove_key(sibling, 0);
          //if necessary move a child from sibling to child
          if(!is_leaf(child)){
//            DEBUG_PRINTF("Moving child from right child\n");
            child->children[child->n_keys] = sibling->children[0];
            memmove(sibling->children, sibling->children+1,
                    (sibling->n_keys+1)*sizeof(sibling->children[0]));
          }
          //Take a key from the left sibling
        } else if(i != 0 && (node->children[i-1]->n_keys >= btree_min_degree)){
//          DEBUG_PRINTF("Moving key from left child\n");
          btree_node *sibling = node->children[i-1];
          //move key from node to child
          node_create_space(child, 0);
          copy_key(child, 0, node, i-1);
          //move key from sibling to node
          copy_key(node, i-1, sibling, sibling->n_keys-1);
          sibling->n_keys--;//removing the last key is easy
          //if necessary move a child from sibling to child
          if(!is_leaf(child)){
            //node_create_space already moved the existing children
           // DEBUG_PRINTF("Moving child from left child\n");
            child->children[0] = sibling->children[sibling->n_keys+1];
          }
        } else {
          if(EXPR_LIKELY(node->n_keys > 1)){
            //Combine childa and one of it's siblings
            if(i < node->n_keys){
              merge_children(tree, node, node->children[i], 
                             node->children[i+1], i);
            } else {//i == node->n_keys
              //   DEBUG_PRINTF("Merging left child\n");
              merge_children(tree, node, node->children[i-1], 
                             node->children[i], i-1);
              i--;
            } 
          } else {
            assert(tree->root == node && i <= 1 && node->n_keys == 1);
            merge_children(tree, node, node->children[0], 
                           node->children[1], 0);
            node = tree->root;
            continue;
          }
        }
      }
    }
    //recurse on the child node
    node = node->children[i];
  }
}
/*
  If key is in the tree remove it and return the value it had, otherwise
  return NULL.
*/
void *btree_delete(btree *tree, uint64_t key){
  return btree_delete_internal(tree, tree->root, key);
}
