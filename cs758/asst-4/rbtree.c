/*
  I feel like I should mention I used the code for rbtrees in the
  linux kernel as a reference, though it's not as if I actually
  copied it or anything. Just citing my sources.

  If you look at the linux rb tree implemntation it is actually
  quite different from a normal rb tree implemenation anyway
*/
#include "rbtree.h"
/*
  A free list is used to allocate rb tree nodes. Every time a node is freed
  it's memory is added to the free list, rather than being released. When a
  node is allocated we use space from the free list if possible and only
  allocate new space if the free list is empty. The free list itself
  is a simple FIFO queue implemnted using a linked list.
  Nodes of the freelist are themselves allocated from a free list, which
  is the only thing that might cause confusion;

*/

static struct free_list free_list_mem = {NULL, NULL};
static struct free_list *free_list = &free_list_mem;
static inline void *xmalloc(size_t sz){
  void *temp = calloc(sz,1);
  if(!temp && sz){
    abort();
  }
  return temp;
}
static inline __attribute__((unused)) rb_node *malloc_node(){
  if(free_list->head){
    rb_node *node = free_list->head->node;
    memset(node, '\0', sizeof(rb_node));
    struct free_list_entry *temp = free_list->head;
    free_list->head = free_list->head->next;
    temp->next = free_list->list_nodes;
    free_list->list_nodes = temp;
    return node;
  } else {
    return xmalloc(sizeof(rb_node));
  }
}
static inline __attribute__((unused)) void free_node(rb_node *node){
  struct free_list_entry *temp;
  if(free_list->list_nodes){
    temp = free_list->list_nodes;
    free_list->list_nodes = temp->next;
  } else {
    temp = xmalloc(sizeof(struct free_list_entry));
  }
  temp->node = node;
  temp->next = free_list->head;
  free_list->head = temp;
}
static __attribute__((unused)) void release_free_list(){
  struct free_list_entry *next, *head;
  head = free_list->head;
  while(head){
    next = head->next;
    free(head->node);
    free(head);
    head = next;
  }
  head = free_list->list_nodes;
  while(head){
    next = head->next;
    free(head);
    head = next;
  }
  return;
}
/*#define malloc_node() calloc(sizeof(rb_node),1)
#define free_node(node) free(node)
#define release_free_list()*/
rb_node *make_node(void *data){
  rb_node *new_node = malloc_node();
  new_node->data = data;
  return new_node;
}
rb_node *make_rb_node(void *data){
  return make_node(data);
}
#define RB_SET_PARENT(n,p) rb_set_parent(n,(uint64_t)p)
static void rb_set_parent(rb_node *node, uint64_t new_parent){
  int color = RB_COLOR(node);
  node->parent = new_parent & ~1;
  if(color == RB_RED){
    RB_MARK_RED(node);
  } else {
    RB_MARK_BLACK(node);
  }
}
//the uppercase macros are defined in the header, but it's eaiser
//to use lower case, so define these here.
#define rb_parent(node) RB_PARENT(node)
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
rb_node *rb_prev(rb_node *node){
  if(node == NULL){
    return NULL;
  }
  if(node->left){
    node = node->left;
    while (node->right) {
      node = node->right;
    }
    return node;
  } else {
    rb_node *parent;
    while ((parent = rb_parent(node)) && node == parent->left) {
      node = parent;
    }
    return parent;
  }
}
rb_node *rb_next(rb_node *node){
  rb_node *parent;
  if(node == NULL){
    return NULL;
  } /*  */
  if(node->right){
    node = node->right;
    while(node->left){
      node=node->left;
    }
    return node;
  } else {
    while ((parent = rb_parent(node)) && node == parent->right) {
      node = parent;
    }
    return parent;
  }
}
rb_node *rb_root(rb_node *node){
  while(rb_parent(node)){
    node = rb_parent(node);
  }
  return node;
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
  rb_node *new_node=NULL;
  if(node == NULL){
    return make_node(data);
  }
  if(compare(data, node->data) == 1){
    if(node->right){
      return __insert(data, node->right);
    } else {
      new_node = make_node(data);
      RB_SET_PARENT(new_node, node);
      node->right = new_node;
    }
  } else {//compare(data, node->date) == 0
    if(node->left){
      return __insert(data, node->left);
    } else {
      new_node = make_node(data);
      RB_SET_PARENT(new_node, node);
      node->left = new_node;
    }
  }
  assert(new_node != NULL);
  return new_node;
}
//return -1 if data1 < data2, 0 if data1 ==  data2 and 1 if data1 > data2
int compare_eq(void *data1, void *data2){
  int x = compare_locations(data1, data2);
  if (x<0) {
    return -1;
  } else {
    return (x == 0 ? 0 : 1);
  }
}

rb_node *rb_locate(void *data, rb_node *node){
  if(!node){
    return NULL;
  }
  switch(compare_eq(data, node->data)){
    case -1:{
      if(node->left){
        return rb_locate(data, node->left);
      } else{
        return node;
      }
    }
      case 0:
        return node;
    case 1:{
        if(node->right){
          return rb_locate(data, node->right);
        } else{
          return node;
        }
    }
    }
  __builtin_unreachable();
}
//replace old with new in the tree
static void replace_node(rb_node *old, rb_node *new){
  if(rb_parent(old)->left == old){
    rb_parent(old)->left = new;
  } else {
    rb_parent(old)->right = new;
  }
}

static rb_node *__delete(rb_node *node, rb_node *root){
  rb_node *retval = rb_parent(node);
  if(!node->left || !node->right){
    if(!node->left && !node->right){
      RB_SET_PARENT(node, NULL);
    } else {
      rb_node *temp;
      if(!node->left){
        temp = node->right;
      } else {
        temp = node->left;
      }
      replace_node(node, temp);
    }
    free_node(node);
    return retval;
  }
  rb_node *succ = rb_next(node);
  replace_node(succ, succ->right);
  replace_node(node, succ);
  succ->right = node->right;
  free_node(node);
  return retval;
}



static inline rb_node *rb_sibling(rb_node *node, int *direction){
  rb_node *parent=rb_parent(node);
  if(parent){
    if(node == parent->left){
      if(direction){
        *direction = LEFT;
      }
      return parent->right;
    } else {
      if(direction){
        *direction = RIGHT;
      }
      return parent->left;
    }
  } else {//no parent
    return NULL;
  }
}

/*
  rotate parent and node in direction direction, they keep their colors
*/
void rotate(rb_node *parent, rb_node *node, int direction){
  if(direction == LEFT){
    assert(parent->right == node);
    parent->right = node->left;
    node->left = parent;
    if(parent->right){
      RB_SET_PARENT(parent->right, parent);
    }
    RB_SET_PARENT(node, parent->parent);
    RB_SET_PARENT(parent, node);
  } else {
    assert(parent->left == node);
    parent->left = node->right;
    node->right = parent;
    if(parent->left){
      RB_SET_PARENT(parent->left, parent);
    }
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
    RB_SET_PARENT(parent, node);
  } else {
    parent->left = node->right;
    if(parent->left){
      RB_SET_PARENT(parent->left, parent);
    }
    node->right = parent;
    node->parent = parent->parent;
    RB_SET_PARENT(parent, node);
  }
}
/*
  Rather than having the same code repeated several times with
  left and right switched around I store the direction of relevent nodes
  in variables and use those.
*/
rb_node *rb_insert(void *data, rb_node *root){
  if(root == NULL){
    rb_node *node = make_node(data);
    RB_MARK_BLACK(node);
    return node;
  }
  assert(RB_COLOR(root) == RB_BLACK);
  rb_node *node = __insert(data, root);
  RB_MARK_RED(node);
  rb_node *parent = rb_parent(node), *gparent, *uncle;
  int parent_direction=0,node_direction;
  while(parent && (RB_COLOR(parent) == RB_RED)){
    gparent = rb_parent(parent);
    uncle = rb_sibling(parent, &parent_direction);
    if(RB_COLOR(uncle) == RB_RED){//case 1/4
      RB_MARK_BLACK(uncle);
      RB_MARK_BLACK(parent);
      node = gparent;
      parent = rb_parent(node);
      RB_MARK_RED(node);
      continue;
    }
    node_direction = (node == parent->left ? LEFT : RIGHT);
    if (node_direction != parent_direction){//case 2/5
      rotate(parent, node, parent_direction);
      parent = node;
    }
    //case 3/6
    if(parent_direction == LEFT){
      rotate(gparent, parent, RIGHT);
    } else {
      rotate(gparent, parent, LEFT);
    }
    break;
  }
  while(rb_parent(root)){
    root=rb_parent(root);
  }
  RB_MARK_BLACK(root);
  return root;
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
          RB_MARK_BLACK(parent);
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
      rotate_colors(sibling, parent, node_direction);
      RB_MARK_BLACK(temp1);
      break;
    }
  }
}



int rb_scan(rb_node **root_ptr, int direction, struct disk_location *loc,
        struct disk_location **arr, unsigned int n){
  if(n == 0){return 0;}
  rb_node *start = rb_locate(loc, *root_ptr);
  rb_node *node = start;
  if(start == NULL){
    return 0;
  }
  rb_node* (*next_node)(rb_node *) = (direction == UP ? rb_next : rb_prev);
  int i=0;
  do {
    arr[i++] = node->data;
  } while (i < n && (node = next_node(node)));
  fprintf(stderr,"scanned %d nodes\n",i);
  return i;
}
int rb_add(rb_node **root_ptr, struct disk_location *loc){
  rb_node *root = rb_insert(loc, *root_ptr);
  *root_ptr = root;
  return 0;
}
int rb_del(rb_node *node, struct disk_location *loc){
  rb_node *to_delete = rb_locate(loc, node);
  if(!to_delete){
    return 1;
  }
  rb_delete(to_delete, node);
  return 0;
}
void rbtree_walk(rb_node *head, void(*fn)(void *)){
 rb_node *node, *prev;
  node = rb_last(head);
  while(node){
    prev = rb_prev(node);
    fn(node);
    node = prev;
  }
  return;
}
int free_count=0;
static void free_rbtree(rb_node *head){
  if(head){
    //    fprintf(stderr,"free_count = %d\n",free_count++);
    free_rbtree(head->left);
    free_rbtree(head->right);
    free(head);
  }
}



void rb_cleanup(rb_node **root_ptr){
  free_rbtree(*root_ptr);
  release_free_list();
}
