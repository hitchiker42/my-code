#include <stdlib.h>
#include "disc_loc.h"
#include <string.h>
#include <stdint.h>
typedef struct rb_node rb_node;
enum RB_color {
  RB_RED = 0,
  RB_BLACK = 1,
};
enum RB_children {
  LEFT = 0,
  RIGHT = 1,
};
struct free_list_entry {
  rb_node *node;
  struct free_list_entry *next;
};
struct free_list {
  struct free_list_entry *head;
  struct free_list_entry *list_nodes;
};
struct rb_node {
  uint64_t parent;//lsb is the node's color
  void *data;
  struct rb_node *left;
  struct rb_node *right;
};

static struct free_list *free_list;
static inline void *xmalloc(size_t sz){  
  void *temp = calloc(sz,1);
  if(!temp && sz){
    abort();
  }
  return temp;
}
static inline rb_node *malloc_node(){
  if(free_list.head){
    rb_node *node = free_list->head->node;
    memset(node, '\0', sizeof(rb_node));
    struct free_list_node *temp = free_list->head;
    free_list->head = free_list->head->next;
    temp->next = free_list->list_nodes;
    free_list->list_nodes = temp;
    return node;
  } else {
    return xmalloc(sizeof(rb_node));
  }
}
static inline void free_node(rb_node *node){
  struct free_list_node *temp;
  if(free_list->list_nodes){
    temp = free_list->list_nodes;
    free_list->list_nodes = temp->next;
  } else {
    temp = xmalloc(sizeof(struct free_list_node));
  }
    temp->node = node;
    temp->next = free_list->head;
    free_list->head = temp;
  }
}
static inline rb_node *make_node  
#define RB_PARENT(node) ((struct rb_node *)(node->parent & ~3))
#define RB_SET_PARENT(node, parent)\
  (node->parent = (parent & ~3) | RB_COLOR(node)
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
#define RB_COLOR(node) (node->parent & 1)
#define RB_MARK_BLACK(node) (node->parent |=  RB_BLACK)
#define RB_MARK_RED(node) (node->parent &= ~RB_BLACK)
#define rb_set_color(node, parent)                      \
  (node->parent = (uint64_t)node->parent | color)
static inline rb_node *make_node(void *data){
  rb_node *new_node = malloc_node();
  new_node->data = data;
  return new_node;
}

