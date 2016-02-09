#ifndef __RBTREE_H__
#define __RBTREE_H__
#ifdef __cplusplus
extern "C" {
#endif
#define RB_RED 0
#define RB_BLACK 1
//rb_node details hidden, really it's best this way
typedef struct rb_node rb_node;
typedef struct rb_tree rb_tree;
/*
  cmp(a,b) should return -1 if a is less than b, 0 if they are equal
  and 1 if a > b.
*/
struct rb_tree {
  rb_node *root;
  cmp_fun cmp;
};
//the structure of an rb_node should be concidered an
//implementation detail, it is exposed to avoid the overhead of a function
//call when getting the value of a node
struct rb_node {
  /*
    The color is stored in the low order bit of the parent pointer. We can
    do this because alignment guarantees that at least the lowest 2 bits of
    a malloc'd pointer are 0.
  */
  unsigned long parent_color;
  union {
    rb_node *children[2];
    struct {
      rb_node *left;
      rb_node *right;
    };
  };
  void *data;
};
/*
  We always use this macro to get a nodes parent, but it could be optimized
  by avoiding the 'and' in cases where we know the parent is red, or we
  don't actually need the parent to be a valid pointer.
*/
#define rb_parent(node) ((rb_node*)(((rb_node*)node)->parent_color & ~3))
#define rb_color(node) (((rb_node*)node)->parent_color & 1)

static inline void *rb_node_value(rb_node *node){
  return node->data;
}
void rb_insert(rb_tree *tree, void *val);
void rb_delete(rb_tree *tree, rb_node *node);
rb_node *rb_lookup(rb_tree *tree, void *val);
rb_node *rb_first(rb_tree *tree);
rb_node *rb_last(rb_tree *tree);
rb_node *rb_next(rb_node *node);
rb_node *rb_prev(rb_node *node);

//erase val from the tree, if it's in there in the first place
static inline int rb_remove(rb_tree *tree, void *val){
  rb_node *node = rb_lookup(tree, val);
  if(node){
    rb_delete(tree, node);
    return 1;
  } else {
    return 0;
  }
}
void rb_traverse(rb_tree *tree, void *userdata,
                 void(*visit)(rb_node*, void*));
void rb_traverse_postorder(rb_tree *tree, void *userdata,
                           void(*visit)(rb_node*, void*));
void rb_traverse_preorder(rb_tree *tree, void *userdata,
                          void(*visit)(rb_node*, void*));
rb_tree *make_empty_rbtree(cmp_fun cmp);
//make an rbtree from (potentially sorted) data
rb_tree *construct_rbtree(cmp_fun cmp, void **data, 
                          size_t len, int sorted);
/*
  Free all the memory associated with tree, Optionally a function
  can be provided which will be called on the value of each node before
  it gets freed.
*/
void destroy_rbtree(rb_tree *tree);
void destroy_rbtree_custom(rb_tree *tree, void(*fun)(void*));

void fprint_node(rb_node *node, FILE *out);
void print_node(rb_node *node);
#ifdef __cplusplus
}
#endif
#endif /* __RBTREE_H__ */
