#include "C_util.h"
#include "rbtree.h"
/*
 * red-black trees properties:  http://en.wikipedia.org/wiki/Rbtree
 *
 *  1) A node is either red or black
 *  2) The root is black
 *  3) All leaves (NULL) are black
 *  4) Both children of every red node are black
 *  5) Every simple path from root to leaves contains the same number
 *     of black nodes.
 *  Note: upper case = Black, lower case = red, parenthesized = unknown
 */
/*
  We always use this macro to get a nodes parent, but it could be optimized
  by avoiding the 'and' in cases where we know the parent is red, or we
  don't actually need the parent to be a valid pointer.
*/
#define rb_parent(node) ((rb_node*)(node->parent & ~3)
#define rb_color(node) (node->parent & 1)
#define rb_is_red(node) (rb_color(node) == RB_RED)
#define rb_is_black(node) (rb_color(node) == RB_BLACK)
#define rb_is_black_safe(node) (!node || rb_is_black(node))
/*
  Structure of the rbtree node taken from the rbtree implementation in
  the linux kernel (well the parent_color part of it at least).

  Some of the rbtree code and accompaning comments (specifically the illustrations)
  is also adapted from the linux kernel implementation.
  However the way the kernel does rb trees is a bit weird, so I still had to write 
  the code myself.
*/
static inline void rb_set_parent(rb_node *x, rb_node *p){
  x->parent_color = rb_color(x) | (ulong)p;
}
static inline void rb_set_parent_color(rb_node *x, rb_node *p, uint color){
  x->parent_color = (ulong)p | color;
}
static inline rb_node* make_rb_node(rb_node *parent, uint color, void *data){
  rb_node *node = xmalloc(sizeof(rb_node));
  rb_set_parent_color(node, parent, color);
  node->data = data;
  node->left = node->right = NULL;
}
/*
  We don't have functions for actual rotations, since in general we
  know information that can eliminate some of the conditionals needed
  in a generic rotation function.
*/
//change child of parent from old to new
static inline void rb_change_child(rb_tree *tree, rb_node *old,
                                   rb_node *new,  rb_node *parent){
  if(parent){
    if(parent->left == old){
      parent->left = new;
    } else {
      parent->right = new;
    }
  } else {
    tree->root = new;
  }
}
//swap old with new, and recolor old to color
static inline void rb_rotate_set_parents(rb_tree *tree, rb_node *old,
                                         rb_node *new, int color){
  rb_node *parent = rb_parent(old);
  new->parent_color = old->parent_color;
  rb_set_parent_color(old, new, color);
  rb_change_child(old, new, parent, root);
}
static void rb_insert_node(rb_tree *tree, rb_node *node);
/*
  Figures out where in the tree val should be placed, then calls rb_insert_node
  to actually insert a node and fix the tree.
*/
void rb_insert(rb_tree *tree, void *val){
  rb_node *x = tree->root;
  rb_node *y = NULL;
  while(x){
    y = x;
    if(tree->cmp(val, x->key)){
      x = x->left;
    } else {
      x = x->right;
    }
  }
  rb_node *new = make_rb_node(y, RB_RED, val);
  if(y == NULL){
    tree->root = new;
  } else if(tree->cmp(val, y->key)){
    y->left = new;
  } else {
    y->right = new;
  }
  rb_insert_node(tree, new);
}
//equivlent to the RB-Insert-Fixup function from clrs
static void rb_insert_node(rb_tree *tree, rb_node *node){
  rb_node *parent = rb_parent(node), *gparent, *tmp;
  while(1){
    //if node is the root, or has a black parent return
    if(!parent){
      rb_set_parent_color(node, NULL, RB_BLACK);
      break;
    } else if(rb_is_black(parent)){
      break;
    }
    gparent = rb_parent(parent);
    tmp = gparent->right;
    if(parent == gparent->left){
      if(tmp && rb_is_red(tmp))
        /*
         * Case 1 - color flips
         *
         *       G            g
         *      / \          / \
         *     p   u  -->   P   U
         *    /            /
         *   n            n
         *
         * However, since g's parent might be red, and
         * 4) does not allow this, we need to recurse
         * at g.
         */
        rb_set_parent_color(tmp, gparent, RB_BLACK);
        rb_set_parent_color(parent, gparent, RB_BLACK);
        node = gparent;
        parent = rb_parent(node);
        rb_set_parent_color(node, parent, RB_RED);
        continue;
    }
    tmp = parent->right;
    if(node == parent->right){
      /*
       * Case 2 - left rotate at parent
       *
       *      G             G
       *     / \           / \
       *    p   U  -->    n   U
       *     \           /
       *      n         p
       *
       * This still leaves us in violation of 4), the
       * continuation into Case 3 will fix that.
       */
      tmp = node->left;
      parent->right = tmp;
      node->left = parent;
      if(tmp){
        rb_set_parent_color(tmp, parent, RB_BLACK);
      }
      rb_set_parent_color(parent, node, RB_RED);
      parent = node;
      tmp = node->right;
    }
    /*
     * Case 3 - right rotate at gparent
     *
     *        G           P
     *       / \         / \
     *      p   U  -->  n   g
     *     /                 \
     *    n                   U
     */
    gparent->left = tmp; /* == parent->right; */
    parent->right = gparent;
    if(tmp){
      rb_set_parent_color(tmp, gparent, RB_BLACK);
    }
    rb_rotate_set_parents(tree, gparent, parent, RB_RED);
    break;
  } else {
    //same as above, but swap left and right
    tmp = gparent->left;
    if(tmp && rb_is_red(tmp)){
      /* Case 1 - color flips */
      rb_set_parent_color(tmp, gparent, RB_BLACK);
      rb_set_parent_color(parent, gparent, RB_BLACK);
      node = gparent;
      parent = rb_parent(node);
      rb_set_parent_color(node, parent, RB_RED);
      continue;
    }
    tmp = gparent->left;
    if(node == parent->left){
      /* Case 2 - left rotate at parent */
      tmp = node->right;
      parent->left = tmp;
      node->right = parent;
      if(tmp){
        rb_set_parent_color(tmp, parent, RB_BLACK);
      }
      rb_set_parent_color(parent, node, RB_RED);
      parent = node;
      tmp = node->left;
    }
    /* Case 3 - right rotate at gparent */
    gparent->right = tmp; /* == parent->left;*/
    parent->left = gparent;
    if(tmp){
      rb_set_parent_color(tmp, gparent, RB_BLACK);
    }
    rb_rotate_set_parents(tree, gparent, parent, RB_RED);
    break;
  }
}
static void* rb_delete_node(rb_tree *tree, rb_node *node);
static void rb_recolor(rb_tree *tree, rb_node *parent);
void rb_delete(rb_tree *tree, rb_node *node){
  void *rebalance = rb_delete_node(tree, node);
  if(rebalance){
    rb_recolor(tree, rebalance);
  }
}
/*
  Oh boy, deletion.
*/
//removes node from tree, may leave the tree unbalanced
void* rb_delete_node(rb_tree *tree, rb_node *node){
  rb_node *child = node->right, *tmp = node->left;
  rb_node *parent, *rebalance;
  ulong pc;
  if(!node->left){
    /*
     * Case 1: node to delete has no more than 1 child (easy!)
     *
     * Note that if there is one child it must be red due to 5)
     * and node must be black due to 4). We adjust colors locally
     * so as to bypass rb_recolor() later on.
     *
     * In this case the child is on the right (or doesn't exist)
     */
    pc = node->parent_color;
    parent = rb_parent(node->parent_color);
    rb_change_child(tree, node, child, parent);
    if(child){
      child->parent_color = pc;
      rebalance = NULL;
    }
      rebalance = rb_is_black(pc) ? parent : NULL;
  } else if (!child){
    //Still case 1, but w/child on the left
    tmp->parent_color = pc = node->parent_color ;
    parent = rb_parent(node->parent_color);
    rb_change_child(tree, node, tmp, parent);
    rebalance = NULL;
    tmp = parent;
  } else {
    //two children
    rb_node *successor = child, *child2;
    tmp = child->left;
    if(!child->left){
      /*
       * Case 2: node's successor is its right child
       *
       *    (n)          (s)
       *    / \          / \
       *  (x) (s)  ->  (x) (c)
       *        \
       *        (c)
       */
      parent = successor;
      child2 = successor->right;
    } else {
      /*
       * Case 3: node's successor is leftmost under
       * node's right child subtree
       *
       *    (n)          (s)
       *    / \          / \
       *  (x) (y)  ->  (x) (y)
       *      /            /
       *    (p)          (p)
       *    /            /
       *  (s)          (c)
       *    \
       *    (c)
       */      
      while(tmp){  //find successor
        parent = successor;
        successor = tmp;
        tmp = tmp->left;
      }
      child2 = successor->right;
      parent->left = child2;
      successor->right = child;
      rb_set_parent(child, successor);
    }
    /*
      replace node with successor
    */
    successor->left = node->left;
    rb_set_parent(node->left, successor);

    pc = node->parent_color;
    tmp = rb_parent(pc);
    rb_change_child(tree, node, successor, tmp);
    
    if(child2){
      successor->parent_color = pc;
      rb_set_parent_color(chuld2, parent, RB_BLACK);
      rebalance = NULL;
    } else {
      ulong pc2 = successor->parent_color;
      successor->parent_color = pc;
      rebalance = rb_is_black(pc2) ? parent : NULL;
    }
  }
  return rebalance
}
//fix any color issues caused by removing a node
void rb_recolor(rb_tree *tree, rb_node *parent){
  rb_node *node = NULL, *sibling, *tmp1, *tmp2;
  while(1){
    /*
     * Loop invariants:
     * - node is black (or NULL on first iteration)
     * - node is not the root (parent is not NULL)
     * - All leaf paths going through parent and node have a
     *   black node count that is 1 lower than other leaf paths.
     */  
    sibling = parent->right;
    if(node != sibling){ /* node is left child*/
      if(rb_is_red(sibling)){
        /*
         * Case 1 - left rotate at parent
         *
         *     P               S
         *    / \             / \
         *   N   s    -->    p   Sr
         *      / \         / \
         *     Sl  Sr      N   Sl
         */
        tmp1 = sibling->left;
        parent->right = sibling->left;
        sibling->left = parent;
        rb_set_parent_color(tmp1, parent, RB_BLACK);
        rb_rotate_set_parents(tree, parent, sibling, RB_RED);
        sibling = tmp1;
      } 
      tmp1 = sibling->right;
      if(rb_is_black_safe(sibling->right)){
        tmp2 = sibling->left;
        if(rb_is_black_safe(sibling->left)){
          /*
           * Case 2 - sibling color flip
           * (p could be either color here)
           *
           *    (p)           (p)
           *    / \           / \
           *   N   S    -->  N   s
           *      / \           / \
           *     Sl  Sr        Sl  Sr
           *
           * This leaves us violating 5) which
           * can be fixed by flipping p to black
           * if it was red, or by recursing at p.
           * p is red when coming from Case 1.
           */
          rb_set_parent_color(sibling, parent, RB_RED);
          if(rb_is_red(parent)){//set parent to black, and we're done
            //this could be optimized by just doing parent->parent_color |= RB_BLACK
            //but that relies on RB_RED being 0
            rb_set_parent_color(parent, rb_parent(parent), RB_BLACK);
          } else if(rb_parent(parent)){
            node = parent;
            parent = rb_parent(node);
            continue;
          }
          break;
        } else {
          /*
           * Case 3 - right rotate at sibling
           * (p could be either color here)
           *
           *   (p)           (p)
           *   / \           / \
           *  N   S    -->  N   Sl
           *     / \             \
           *    sl  Sr            s
           *                       \
           *                        Sr
           */
          tmp1 = tmp2->right;//== sibling->left->right;
          sibling->left = tmp1;
          tmp2->right = sibling;
          parent->right = tmp2;
          if(tmp1){
            rb_set_parent_color(tmp1, sibling, RB_BLACK);
          }
          tmp1 = sibling;
          sibling = tmp2;
        }
      }
      /*
       * Case 4 - left rotate at parent + color flips
       * (p and sl could be either color here.
       *  After rotation, p becomes black, s acquires
       *  p's color, and sl keeps its color)
       *
       *      (p)             (s)
       *      / \             / \
       *     N   S     -->   P   Sr
       *        / \         / \
       *      (sl) sr      N  (sl)
       */
      tmp2 = sibling->left;
      parent->right = tmp2;
      sibling->left = parent;
      rb_set_parent_color(tmp1, sibling, RB_BLACK);
      if(tmp2){
        rb_set_parent(tmp2, parent);
      }
      rb_rotate_set_parents(tree, parent, sibling, RB_BLACK):
      break;
    } else {//node is right child
      sibling = parent->left;
      if(rb_is_red(sibling)){
        /* Case 1 - left rotate at parent*/
        tmp1 = sibling->right;
        parent->left = sibling->right;
        sibling->right = parent;
        rb_set_parent_color(tmp1, parent, RB_BLACK);
        rb_rotate_set_parents(tree, parent, sibling, RB_RED);
        sibling = tmp1;
      } 
      tmp1 = sibling->left;
      if(rb_is_black_safe(sibling->left)){
        tmp2 = sibling->right;
        if(rb_is_black_safe(sibling->right)){
           /* Case 2 - sibling color flip */
          rb_set_parent_color(sibling, parent, RB_RED);
          if(rb_is_red(parent)){
            rb_set_parent_color(parent, rb_parent(parent), RB_BLACK);
          } else if(rb_parent(parent)){
            node = parent;
            parent = rb_parent(node);
            continue;
          }
          break;
        } else {
          /* Case 3 - right rotate at sibling */
          tmp1 = tmp2->left;//== sibling->right->left;
          sibling->right = tmp1;
          tmp2->left = sibling;
          parent->left = tmp2;
          if(tmp1){
            rb_set_parent_color(tmp1, sibling, RB_BLACK);
          }
          tmp1 = sibling;
          sibling = tmp2;
        }
      }
       /* Case 4 - left rotate at parent + color flips */
      tmp2 = sibling->right;
      parent->left = tmp2;
      sibling->right = parent;
      rb_set_parent_color(tmp1, sibling, RB_BLACK);
      if(tmp2){
        rb_set_parent(tmp2, parent);
      }
      rb_rotate_set_parents(tree, parent, sibling, RB_BLACK):
      break;
    }
  }
}

/*
  Now the simple stuff, traversal, lookup, etc...
*/
/*
  Oh how I wish C had lambda functions
 */
static inline void rb_traverse_recurse(rb_node *node, void(*visit)(rb_node*)){
  if(node){
    rb_traverse_recurse(node->left, visit);
    visit(node);
    rb_traverse_recurse(node->right, visit);
  }
}
void rb_traverse(rb_tree *tree, void(*visit)(rb_node*)){
  rb_traverse_recurse(tree->root, visit);
}
rb_node *rb_lookup(rb_tree *tree, void *val){
  rb_node *node = tree->root;
  //tail recursive version...kinda
 RECURSE:
  if(!node || !(tree->cmp(node->data, val))){
    return node;
  }
  if(tree->cmp(node->data, val) > 0){
    node = node->right;
  } else {
    node = node->left;
  }
  goto RECURSE;
/*
  iterative version
  while(node && tree->cmp(node->data, val)){
    if(tree->cmp(node->data, val) > 0){
      node = node->right;
    } else {
      node = node->left;
    }
  }
  return node;
*/
}
static inline rb_node *rb_min(rb_node *node){
  while(node->left){
    node = node->left;
  }
  return node;
}
static inline rb_node *rb_max(rb_node *node){
  while(node->right){
    node = node->right;
  }
  return node;
}
rb_node *rb_first(rb_tree *tree){
  return rb_min(tree->root);
}
rb_node *rb_last(rb_tree *tree){
  return rb_max(tree->root);
}
rb_node *rb_next(rb_node *node){
  if(node->right){
    return rb_min(node->right);
  } else {
    rb_node *next = rb_parent(node);
    while(next && node == next->right){
      node = next;
      next = next->parent;
    }
    return successor;
  }
}
rb_node *rb_prev(rb_node *node){
  if(node->left){
    return rb_max(node->left);
  } else {
    rb_node *prev = rb_parent(node);
    while(prev && node = prev->left){
      node = prev;
      prev = prev->parent;
    }
    return prev;
  }
}
void destroy_rbtree(rb_tree *tree){
  rb_traverse(tree, free);
}
