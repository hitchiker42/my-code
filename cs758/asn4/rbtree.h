#ifndef _RB_TREE_H_
#define _RB_TREE_H_
#include <stdlib.h>
#include <assert.h>
#include "alloca.h"
#include "disk_loc.h"
#include <string.h>
#include <stdint.h>
#if (defined __GNUC__)
#define inline __inline
#elif (defined __STDC_VERSION__ && __STDC_VERSION__ >= 199901L)
#define inline inline
#else
#define inline
#endif
typedef struct rb_node rb_node;
enum RB_color {
  RB_RED = 0,
  RB_BLACK = 1
};
enum RB_children {
  LEFT = 0,
  RIGHT = 1
};
struct free_list_entry {
  rb_node *node;
  struct free_list_entry *next;
};
struct free_list {
  struct free_list_entry *head; //list of useable free list nodes
  struct free_list_entry *list_nodes;//free list of free list nodes
};
/*
  I'm sure you know how this works but I'll explain it anyway.
  Memory allocated by malloc is aligned to word sized boundries (4 bytes on a 
  32 bit machine and 8 bytes on a 64 bit machine). This means the lowest 2/3 bits
  of a (malloc'ed) pointer will always be 0, knowing this we can use those bits
  to store information, and mask them away when we want to get at the actual pointer.
  In this case the color of a node is stored in the least significant bit of the
  pointer to its parent. doing this means a red black tree has the same space complexity
  as a normal binary tree, if we allocated seperate space for the color we would need
  O(N) more memory than a normal binary tree.
*/
struct rb_node {
  uint64_t parent;
  void *data;
  struct rb_node *left;
  struct rb_node *right;
};

/*
  Because we store information in the parent pointer it's much eaiser to
  use macros for any access to the parent, to prevent the code from
  getting cluttered.
*/
#define RB_PARENT(node) ((struct rb_node *)(node->parent & (~1UL)))
#define RB_EMPTY(node) (node == NULL)
#define RB_IS_ROOT(node) (rb_parent(node) == NULL)
#define RB_LEFT_CHILD(node) (node->left(
#define RB_RIGHT_CHILD(node) (node->right)
#define RB_CHILD(node, dir) (dir == RIGHT ? node->right : node->left)
#define RB_GRANDPARENT(node) (RB_PARENT(node) ? RB_PARENT(RB_PARENT(node)) :\
                              NULL)
#define RB_SIBLING(node)                                \
  (RB_PARENT(node) ? RB_LEFT_CHILD(RB_PARENT(node)) == node ?           \
   RB_RIGHT_CHILD(RB_PARENT(node)) : RB_LEFT_CHILD(RB_PARENT(node)) :   \
   NULL)
#define RB_UNCLE(node)    \
  (RB_GRANDPARENT(node) ? \
      RB_LEFT_CHILD(RB_GRANDPARENT(node)) == RB_PARENT(node) ? \
    RB_RIGHT_CHILD(RB_GRANDPARENT(node)) :                     \
   RB_LEFT_CHILD(RB_GRANDPARENT(node)) : NULL)
#define RB_COLOR(node) (node ? node->parent & 1 : RB_BLACK)
#define RB_MARK_BLACK(node) (node->parent |=  RB_BLACK)
#define RB_MARK_RED(node) (node->parent &= ~RB_BLACK)
#define rb_set_color(node, parent)                      \
  (node->parent = (uint64_t)node->parent | color)


rb_node *rb_insert(void *data, rb_node *root);
void rb_delete(rb_node *node, rb_node *root);
rb_node *rb_prev(rb_node *node);
rb_node *rb_next(rb_node *node);
int rb_scan(rb_node **node, int direction, struct disk_location *loc,
                 struct disk_location **arr, unsigned int n);
int rb_add(rb_node **node, struct disk_location *loc);
int rb_del(rb_node *node, struct disk_location *loc);
rb_node *make_rb_node(void *data);
void rb_cleanup(rb_node **node);
#endif
