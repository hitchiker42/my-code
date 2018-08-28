#ifndef __RBTREE_H__
#define __RBTREE_H__
#include "util.h"
namespace util {
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
template<typename T, typename Compare = std::less>
struct rb_tree {
  enum rb_color {
    RB_RED = 0,
    RB_BLACK = 1
  };
  struct rb_node {
    union {
      uintptr_t rb_parent_color;
      rb_node *next; //for use in free list
    };
    rb_node *left = nullptr;
    rb_node *right = nullptr;
    T data;

    rb_node(rb_node *parent, rb_color color, T data = T())
      : rb_parent_color(reinterpret_cast<uintptr_t>(parent) | color),
        data(data) {}
    //It is important to note that quite often this is called without
    //actually deallocating the node afterward, which is why we zero it.
    ~rb_node(){
      std::destroy_at(&data);
      memset(this, '\0', sizeof(*this));
    } 
    rb_node* parent() const {
      return reinterpret_cast<rb_node*>(rb_parent_color &
                                        tagged_pointer_bitmask);
    }
    void set_parent(rb_node *parent) {
      rb_parent_color = color() | reinterpret_cast<uintptr_t>(parent);
    }
    rb_color color() const {
      return rb_parent_color & pointer_tag_bitmask;
    }
    void set_parent_color(rb_node *parent, rb_color color) {
      rb_parent_color = color | reinterpret_cast<uintptr_t>(parent);
    }
    void set_parent_color(uintptr_t parent_color){
      rb_parent_color = parent_color;
    }
    void make_black(){
      parent_color |= 1;
    }
    void make_red(){
      parent_color &= ~1;
    }
    void set_color(rb_color color){
      (color == rb_black ? make_black() : make_red());
    }
    bool is_red() const {
      return color() == rb_red;
    }
    bool is_black() const {
      return color() == rb_black;
    }
    
  };
  rb_node *root = nullptr;
  rb_node *freelist = nullptr;
  size_t sz = 0;
  Compare cmp = Compare();

  rb_node* alloc_node(){
    if(freelist){
      rb_node *ret = freelist;
      freelist = ret->next;
      ret->next = nullptr;
      return ret;
    } else {
      return (rb_node*)malloc(sizeof(rb_node));
    }
  }
  void free_node(rb_node *n){
    memset(n, '\0', sizeof(rb_node));  
    n->next = freelist;
    freelist = n;
  }
  T cleanup_node(rb_node *n){
    T ret(std::move(n->data));
    n->~rb_node();
    n->next = freelist;
    freelist = n;
    return ret;
  }

  //NULL safe color checks
  static bool is_node_black(rb_node *n){
    return !n || r->is_black();
  }
  static bool is_node_red(rb_node *n){
    return n && r->is_red();
  }
/*
  Structure of the rbtree node taken from the rbtree implementation in
  the linux kernel (well the parent_color part of it at least).

  Some of the rbtree code and accompaning comments (specifically the illustrations)
  is also adapted from the linux kernel implementation.
*/
  rb_node* make_rb_node(rb_node *parent, rb_color color, T data){
    rb_node *node = alloc_node();
    new (node)(parent, color, data);
    return node;
  }
/*
  We don't have functions for generic rotations, since in general we
  know information that can eliminate some of the conditionals needed
  in a generic rotation function.
*/
//change child of parent from old to new
  void rb_change_child(rb_node *old_node,
                       rb_node *new_node,  rb_node *parent){
    if(parent){
      if(parent->left == old_node){
        parent->left = new_node;
      } else {
        parent->right = new_node;
      }
    } else {
      this->root = new_node;
    }
  }
  //swap old with new, and recolor old to color
  void rb_rotate_set_parents(rb_node *old_node,
                             rb_node *new_node, rb_color color){
    rb_node *parent = old_node->parent();
    new_node->set_parent_color(old_node->parent_color);
    old_node->set_parent_color(old_node, new_node, color);
    rb_change_child(old_node, new_node, parent);
  }
  void rb_insert_node(rb_tree *tree, rb_node *node);
  /*
    Figures out where in the tree val should be placed, then calls rb_insert_node
    to actually insert a node and fix the tree.
  */
  void rb_insert(T val){
    rb_node *x = this->root;
    rb_node *y = nullptr;
    while(x){
      y = x;
      if(this->cmp(val, x->data) <= 0){
        x = x->left;
      } else {
        x = x->right;
      }
    }
    rb_node *new_node = make_rb_node(y, RB_RED, val);
    if(y == nullptr){
      this->root = new_node;
    } else if(this->cmp(val, y->data) <= 0){
      y->left = new_node;
    } else {
      y->right = new_node;
    }
    rb_insert_node(tree, new_node);
    this->sz++;
  }
  //equivlent to the RB-Insert-Fixup function from clrs
  bool rb_insert_node(rb_node *node){
    rb_node *parent = node->parent(), *gparent, *tmp;
    while(1){
      //if node is the root, or has a black parent return
      if(!parent){
        node->set_parent_color(nullptr, RB_BLACK);
        break;
      } else if(parent->is_black()){
        break;
      }
      gparent = parent->parent();
      tmp = gparent->right;
      if(parent != tmp){//parent == gparent->left
        if(this->is_node_red(tmp)){
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
          tmp->set_parent_color(gparent, RB_BLACK);
          parent->set_parent_color(gparent, RB_BLACK);
          node = gparent;
          parent = node->parent();
          node->set_parent_color(parent, RB_RED);
          continue;
        }
        tmp = parent->right;
        if(node == tmp){
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
            tmp->set_parent_color(parent, RB_BLACK);
          }
          parent->set_parent_color(node, RB_RED);
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
          tmp->set_parent_color(gparent, RB_BLACK);
        }
        this->rotate_set_parents(gparent, parent, RB_RED);
        break;
      } else {
        //same as above, but swap left and right
        tmp = gparent->left;
        if(this->is_node_red(tmp)){
          /* Case 1 - color flips */
          tmp->set_parent_color(gparent, RB_BLACK);
          parent->set_parent_color(gparent, RB_BLACK);
          node = gparent;
          parent = node->parent();
          node->set_parent_color(parent, RB_RED);
          continue;
        }
        tmp = parent->left;
        if(node == tmp){
          /* Case 2 - left rotate at parent */
          tmp = node->right;
          parent->left = tmp;
          node->right = parent;
          if(tmp){
            tmp->set_parent_color(parent, RB_BLACK);
          }
          parent->set_parent_color(node, RB_RED);
          parent = node;
          tmp = node->left;
        }
        /* Case 3 - right rotate at gparent */
        gparent->right = tmp; /* == parent->left;*/
        parent->left = gparent;
        if(tmp){
          tmp->set_parent_color(gparent, RB_BLACK);
        }
        this->rotate_set_parents(gparent, parent, RB_RED);
        break;
      }
    }
  }
  T delete_node(rb_node *node);
  void recolor(rb_node *parent);
  void erase(rb_node *node){
    rb_node *rebalance = this->delete_node(node);
    if(rebalance){
      this->recolor(rebalance);
    }
    free(node);
    this->sz--;
  }
  void erase_custom(rb_tree *tree, rb_node *node,
                        void(*cleanup)(void*)){
    rb_node *rebalance = this->delete_node(node);
    if(rebalance){
      this->recolor(rebalance);
    }
    cleanup(node->data);
    free(node);
    this->sz--;
  }
  /*
    Oh boy, deletion.
  */
  //removes node from tree, may leave the tree unbalanced
  void* delete_node(rb_node *node){
    rb_node *child = node->right;
    rb_node *tmp = node->left;
    rb_node *parent, *rebalance;
    //We do a lot of changing around parents in this function, so
    //we store some parent pointers in their tagged form to make
    //setting the parent field eaiser.
    uintptr pc;
    if(!tmp){
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
      parent = node->parent();
      this->change_child(node, child, parent);
      if(child){
        child->parent_color = pc;
        rebalance = nullptr;
      } else {
        rebalance = (pc & RB_BLACK) ? parent : nullptr;
      }
      tmp = parent;
    } else if (!child){
      //Still case 1, but w/child on the left
      tmp->parent_color = node->parent_color;
      parent = node->parent();
      this->change_child(node, tmp, parent);
      rebalance = nullptr;
      tmp = parent;
    } else {
      //two children
      rb_node *successor = child, *child2;
      tmp = child->left;
      if(!tmp){
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
        do {  //find successor
          parent = successor;
          successor = tmp;
          tmp = tmp->left;
        } while(tmp);

        child2 = successor->right;
        parent->left = child2;
        successor->right = child;
        child->set_parent(successor);
      }
      /*
        replace node with successor
      */
      tmp = node->left;
      successor->left = tmp;
      tmp->set_parent(successor);

      pc = node->parent_color;
      tmp = node->parent();
      this->change_child(node, successor, tmp);

      if(child2){
        successor->parent_color = pc;
        child2->set_parent_color(parent, RB_BLACK);
        rebalance = nullptr;
      } else {
        rb_color succ_color = successor->color();
        successor->parent_color = pc;
        rebalance = (succ_color ==  RB_BLACK) ? parent : nullptr;
      }
      tmp = successor;
    }
    return rebalance;
  }
  //fix any color issues caused by removing a node
  void recolor(rb_node *parent){
    rb_node *node = nullptr, *sibling, *tmp1, *tmp2;
    while(1){
      /*
       * Loop invariants:
       * - node is black (or nullptr on first iteration)
       * - node is not the root (parent is not nullptr)
       * - All leaf paths going through parent and node have a
       *   black node count that is 1 lower than other leaf paths.
       */
      sibling = parent->right;
      if(node != sibling){ /* node is left child*/
        if(sibling->is_red()){
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
          parent->right = tmp1;
          sibling->left = parent;
          tmp1->set_parent_color(parent, RB_BLACK);
          this->rotate_set_parents(parent, sibling, RB_RED);
          sibling = tmp1;
        }
        tmp1 = sibling->right;
        if(this->is_node_black(tmp1)){
          tmp2 = sibling->left;
          if(this->is_node_black(tmp2)){
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
            sibling->set_parent_color(parent, RB_RED);
            if(parent->is_red()){//set parent to black, and we're done
              parent->set_black();
            } else {
              node = parent;
              parent = node->parent();
              if(parent){ //recurse
                continue;
              }
            }
            break;
          }
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
            tmp1->set_parent_color(sibling, RB_BLACK);
          }
          tmp1 = sibling;
          sibling = tmp2;
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
        tmp1->set_parent_color(sibling, RB_BLACK);
        if(tmp2){
          tmp2->set_parent(parent);
        }
        this->rotate_set_parents(parent, sibling, RB_BLACK);
        break;
      } else {//node is right child
        sibling = parent->left;
        if(sibling->is_red()){
          /* Case 1 - right rotate at parent*/
          tmp1 = sibling->right;
          parent->left = tmp1;
          sibling->right = parent;
          tmp1->set_parent_color(parent, RB_BLACK);
          this->rotate_set_parents(parent, sibling, RB_RED);
          sibling = tmp1;
        }
        tmp1 = sibling->left;
        if(this->is_node_black(tmp1)){
          tmp2 = sibling->right;
          if(this->is_node_black(tmp2)){
            /* Case 2 - sibling color flip */
            sibling->set_parent_color(parent, RB_RED);
            if(parent->is_red()){
              parent->set_black();
            } else {
              node = parent;
              parent = node->parent();
              if(parent){
                continue;
              }
            }
            break;
          }
          /* Case 3 - right rotate at sibling */
          tmp1 = tmp2->left;//== sibling->right->left;
          sibling->right = tmp1;
          tmp2->left = sibling;
          parent->left = tmp2;
          if(tmp1){
            tmp1->set_parent_color(sibling, RB_BLACK);
          }
          tmp1 = sibling;
          sibling = tmp2;
        }
        /* Case 4 - left rotate at parent + color flips */
        tmp2 = sibling->right;
        parent->left = tmp2;
        sibling->right = parent;
        tmp1->rb_set_parent_color(sibling, RB_BLACK);
        if(tmp2){
          tmp2->set_parent(parent);
        }
        this->rotate_set_parents(parent, sibling, RB_BLACK);
        break;
      }
    }
  }

  /*
    Now the simple stuff, traversal, lookup, etc...
  */
  //Much nicer in C++ due to std::function / lambdas
  void rb_traverse_recurse(rb_node *node,
                           std::function<void(rb_node*)> &visit){
    if(node){
      rb_traverse_recurse(node->left, visit);
      visit(node);
      rb_traverse_recurse(node->right, visit);
    }
  }
  void rb_traverse(std::function<void(rb_node*)> visit){
    rb_traverse_recurse(this->root, visit);
  }
  void rb_traverse_postorder_recurse(rb_node *node,
                           std::function<void(rb_node*)> &visit){
    if(node){
      rb_traverse_recurse(node->left, visit);
      rb_traverse_recurse(node->right, visit);
      visit(node);
    }
  }
  void rb_traverse_postorder(std::function<void(rb_node*)> visit){
    rb_traverse_postorder_recurse(this->root, visit);
  }
  void rb_traverse_preorder_recurse(rb_node *node,
                           std::function<void(rb_node*)> &visit){
    if(node){
      visit(node);
      rb_traverse_recurse(node->left, visit);
      rb_traverse_recurse(node->right, visit);
    }
  }
  void rb_traverse_preorder(std::function<void(rb_node*)> visit){
    rb_traverse_preorder_recurse(this->root, visit);
  }
  rb_node *lookup(rb_node *node, const T& val){
    if(!node || !(this->cmp(node->data, val))){
      return node;
    }
    if(this->cmp(node->data, val) < 0){
      lookup(node->right, val);
    } else {
      lookup(node->left, val);
    }
  }
  rb_node *lookup(const T& val){
    return lookup(this->root, val);
  }
  rb_node *rb_min(rb_node *node){
    while(node->left){
      node = node->left;
    }
    return node;
  }
  rb_node *rb_max(rb_node *node){
    while(node->right){
      node = node->right;
    }
    return node;
  }
  rb_node *rb_first(){
    if(this->root){
      return rb_min(this->root);
    } else {
      return nullptr;
    }
  }
  rb_node *rb_last(){
    if(this->root){
      return rb_max(this->root);
    } else {
      return nullptr;
    }
  }
  rb_node *rb_next(rb_node *node){
    if(node->right){
      return rb_min(node->right);
    } else {
      rb_node *next = node->parent();
      while(next && (node == next->right)){
        node = next;
        next = next->parent();
      }
      return next;
    }
  }
  rb_node *rb_prev(rb_node *node){
    if(node->left){
      return rb_max(node->left);
    } else {
      rb_node *prev = node->parent();
      while(prev && (node == prev->left)){
        node = prev;
        prev = prev->parent();
      }
      return prev;
    }
  }
  void destroy_rbtree(){
    rb_traverse_postorder(tree, nullptr, free);
    this->~rb_tree();
  }
#if 0
  static void destroy_custom_fun(rb_node *node, void(*fun)(void*)){
    fun(node->data);
    free(node);
  }
  void destroy_rbtree_custom(rb_tree *tree, void(*fun)(void*)){
    rb_traverse_postorder(tree, (void*)fun, (void*)destroy_custom_fun);
    free(tree);
  }
  
  static inline void print_rbtree_recurse(rb_node *node, void **userdata){
    FILE *out = userdata[0];
    void(*print_val)(void*,FILE*) = userdata[1];
    if(node){
      fputc('(',out);
      print_val(node->data, out);
      fputc(' ',out);
      print_rbtree_recurse(node->left, userdata);
      fputs(" . ",out);
      print_rbtree_recurse(node->right, userdata);
      fputc(')',out);
    } else {
      fputs("()",out);
    }
  }
  static void default_print_val(void *val, FILE *out){
    char buf[22];//enough to hold any 64 bit integer + 2 for the 0x
    snprintf(buf, 22, "%#0lx", (uintptr_t)val);
    fputs(buf, out);
  }
  void print_rbtree_sexp(rb_tree *tree, FILE *out, void(*print_val)(void*,FILE*)){
    void *userdata[2] = {(out ? out : stdout),
      (print_val ? print_val : default_print_val)};
    print_rbtree_recurse(this->root, userdata);
  }
  //This could be used to implement a generic breadth first
  //traversal, but I don't see much use for that aside from
  //seralizing a tree
  void print_rbtree(rb_tree *tree, FILE *out, void(*print_node)(rb_node*,FILE*)){
    if(!this->root){
      return;
    }
    /*
      This prints a full binary tree, with empty nodes indicated by NIL
    */
    struct queue *q = alloca(sizeof(struct queue));
    memset(q,'\0', sizeof(struct queue));
    rb_node *node;
    queue_push(q, this->root);
    while(!(queue_is_empty(q))){
      node = queue_pop(q);
      print_node(node, out);
      queue_push(q, node->left);
      queue_push(q, node->right);
    }
  }


  //make an rbtree from (potentially sorted) data
  rb_tree *construct_rbtree(cmp_fun cmp, void **data,
                            size_t len, int sorted){
    size_t i;
    rb_tree *tree = xmalloc(sizeof(rb_tree));
    this->root = nullptr;
    this->cmp = cmp;
    for(i=0;i<len;i++){
      rb_insert(tree, data[i]);
    }
    return tree;
  }
  static void construct_rbtree_recurse(rb_node *node, void **data, size_t len){
    if(len == 1){
      return;
    } else {
      size_t mid = len/2;
      size_t lmid = mid/2;
      size_t rmid = (len - mid)/2;
      node->data = data[len];
      if(len ==  2){
        node->left = make_rb_node();
      }
    }
  }
#endif
};
} //namespace util

#endif /* __RBTREE_H__ */
