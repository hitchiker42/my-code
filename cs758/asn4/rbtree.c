/*
  I feel like I should mention I used the code for rbtrees in the
  linux kernel as a reference, though it's not as if I actually
  copied it or anything. Just citing my sources. 
  
  If you look at the linux rb tree implemntation it is actually
  quite different from a normal rb tree implemenation anyway
*/
#include "rbtree.h"
//the uppercase macros are defined in the header, but it's eaiser
//to use lower case, so define these here.
#define rb_empty(node) RB_PARENT(node)
#define rb_parent(node) RB_PARENT(node)
//part of me really wants to make macros to generate code
//since there's a lot of similar stuff with the only difference
//being right/left flipped around, but there's not enough code
//duplication to warrent the use of macros (at least not C macros)
rb_node *rb_prev(rb_node *node){
  if(rb_empty(node)){
    return NULL;
  }
  if(node->left){
    node = node->left;
    while (node->right) {
      node = node->right
    }
    return node;
  } else {
    rb_node *parent;
    while (parent = rb_parent(node) && node == parent->left) {
      node = parent;
    }
    return parent;
  }
}
rb_node *rb_next(rb_node *node){
  if(rb_empty(node)){
    return NULL;
  }
  if(node->right){
    node = node->right;
    while(node->left){
      node=node->left;
    }
    return node;
  } else {
    while (parent = rb_parent(node) && node == parent->right) {
      node = parent;
    }
    return parent;
  }
}
rb_node *rb_root(rb_node *node){
  while(rb_parent(node)){
    node = rb_parent_node;
  }
}
#define RB_ROOT(node) (RB_IS_ROOT(node) ? node : rb_root(node))
rb_node *rb_first(rb_node *node){
  node = RB_ROOT(node);
  while(node->left){
    node = node->left;
  }
  return node;
}
rb_node *rb_last(rb_node *node){
  node = RB_ROOT(node);
  while(node->right){
    node = node->right;
  }
  return node;
}
#define compare(a,b) (compare_locations(a,b) <= 0 ? 0 : 1)
rb_node *__insert(void *data, rb_node *node){
  if(compare(data, node->data)){
    if(root->right){
      __insert(data, node->right);
    } else {
      rb_node *new_node = make_node(data);
      new_node->parent = node;
    }
  } else {
    if(root->left){
      __insert(data, node->right);
    } else {
      rb_node *new_node = make_node(data);
      new_node->parent = node;
    }
  }
}
//replace old with new in the tree
static void replace_node(rb_node *old, rb_node *new){
  if(old->parent->left == node){
      old->parent->left = new;
    } else {
      old->parent->right = new;
    }
}
  
static rb_node *__delete(rb_node *node, rb_node *root){
  node *retval = node->parent;
  if(!node->left || !node->right){
    if(!node->left && !node->right){
      node->parent = NULL;
    } else {
      node *temp;
      if(!node->left){
        temp = node->right;
      } else {
        temp = node->left;
      }
      rb_replace(node, temp);
    }
    free_node(node);
    return retval;
  }
  node *succ = rb_next(node);
  rb_replace(succ, succ->right);
  rb_replace(node, succ);
  succ->right = node->right;
  free_node(node);
}
  
  
  
static inline rb_node *rb_sibling(rb_node *node, int *direction){
  rb_node *parent=rb_parent(node);
  if(parent){
    if(node == parent->left){
      if(direction){*direction = RIGHT;}
      return parent->right;
    } else {
      if(direction){*direction = LEFT;}
      return parent->left;
    }
  }
}
/*
  rotate parent and node in direction direction, they keep their colors
*/
void rotate(rb_node *parent, rb_node *node, int direction){
  if(direction == LEFT){
    parent->right = node->left;
    if(parent->right){
      RB_SET_PARENT(parent->right, parent);
    }
    node->left = parent;
    RB_SET_PARENT(node, parent->parent);
    RB_SET_PARENT(parent, node);
  } else {
    parent->left = node->right;
    if(parent->left){
      RB_SET_PARENT(parent->left, parent);
    }
    node->right = parent;
    RB_SET_PARENT(node, parent->parent);
    RB_SET_PARENT(parent, node);
  }
}
/*
  rotate parent and node in direction direction and swap thier colors
*/
void rotate_colors(rb_node *parent, rb_node *node, int direction){
  if(direction == LEFT){
    parent->right = node->left;
    if(parent->right){
      RB_SET_PARENT(parent->right, parent);
    }
    node->left = parent;
    node->parent = parent->parent;
    parent->parent = node
  } else {
    parent->left = node->right;
    if(parent->left){
      RB_SET_PARENT(parent->left, parent);
    }
    node->right = parent;
    node->parent = parent->parent;
    parent->parent = node;
  }
}

void rb_insert(void *data, rb_node *root){
  rb_node *node = __insert(data, root);
  RB_MARK_RED(node);
  rb_node *parent = rb_parent(node), *gparent, *uncle;
  int parent_direction,node_direction;
  /*
    rather than having 6 cases, I abstract away direction,
    or atleast try to
   */
  while(parent && RB_COLOR(parent) = RB_RED){
    gparent = rb_parent(parent);
    uncle = rb_sibling(parent, &parent_direction);
    if(RB_COLOR(uncle) == RB_RED){
      RB_MARK_BLACK(uncle);
      RB_MARK_BLACK(parent);
      node = gparent;
      parent = rb_parent(node);
      RB_MARK_RED(node);
      continue;
    }
    node_direction = (node == parent->left ? LEFT : RIGHT);
    if (node_direction != parent_direction){
      rotate(parent, node, parent_direction);
      parent = node;
    }
    rotate(gparent, parent, node_direction);
  }
  RB_MARK_BLACK(root);
}
void rb_delete(rb_node *node, rb_node *root){
  rb_node *parent = __delete(node,root);//assume this frees node
  node = NULL;
  rb_node *sibling, *temp1,*temp2;
  int sibling_direction, node_direction;
  while(1){
    sibling = parent->right;
    if(node == sibling){
      sibling_direction = LEFT;
      node_direction = RIGHT;
      sibling = parent->left;
    } else {
      sibling_direction = RIGHT;
      node_direction = LEFT;
    }
    if(RB_COLOR(sibling) == RB_RED){ //case 1
      //swaps the colors of parent and sibling
      rotate_colors(parent, sibling, node_direction);
    }
    temp1 = RB_CHILD(sibling, sibling_direction);
    if(!temp1 || RB_COLOR(temp1) == RB_BLACK){
      temp2 = RB_CHILD(sibling, node_direction);
      if(!temp2 || RB_COLOR(temp2) == RB_BLACK){//case 2
        RB_MARK_RED(sibling);
        if(RB_COLOR(parent) == RB_RED){
          MARK_BLACK(parent);
          break;
        } else {
          node = parent;
          parent = RB_PARENT(parent);
          if(parent == NULL){
            break;
          }
        }
      } else { //temp2 is red (case 3)
        rotate(temp2, sibling, sibling_direction);
        temp1 = sibling;
        sibling = temp2;
      } //case 4     
      rotate_color(sibling, parent, node_direction);
      RB_MARK_BLACK(temp1);
      break;
    }
  }
}
      
      
      
