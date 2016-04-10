#include "roadmap.h"
#define heap_left_child(i) (2*i+1)
#define heap_right_child(i) (2*i+2)
#define heap_parent(i) ((i-1)/2)
#define HEAP_SWAP(_heap, _i, _j)                                \
  __extension__ ({ARR_SWAP(_heap->nodes,_i,_j);                        \
  SWAP(_heap->nodes[_i]->heap_index,_heap->nodes[_j]->heap_index);     \
  ;})
#define MIN(_x,_y)                              \
  __extension__                                 \
  ({__typeof(_x) x = _x;                        \
    __typeof(_y) y = _y;                        \
    x<y ? x : y;})
#define MAX(_x,_y)                              \
  __extension__                                 \
  ({__typeof(_x) x = _x;                        \
    __typeof(_y) y = _y;                        \
    x>y ? x : y;})
  
static void heap_sift_down(struct heap *heap, int root){
  int left,right,swap;
  left = right = swap = 0;  
  while ((left = heap_left_child(root)) < heap->len){
    right = heap_right_child(root);
    swap = root;
    if(heap->nodes[left]->dist < heap->nodes[swap]->dist){
      swap = left;
    }
    if(right < heap->len &&
       (heap->nodes[right]->dist < heap->nodes[swap]->dist)){
      swap = right;
    }
    if(swap == root){
      return;
    } else {
      HEAP_SWAP(heap, root, swap);
      root = swap;
    }
  }
}

void heap_sift_up(struct heap *heap, int index){
  int parent = heap_parent(index);
  //this will terminate at the root since heap_parent(0) = 0
  while(heap->nodes[index]->dist < heap->nodes[parent]->dist){
    HEAP_SWAP(heap, index, parent);
    index = parent;
    parent = heap_parent(index);
  }
}
/*void heap_sift_up_element(struct heap *heap, struct node *element){
  int heap_index = (element - heap->nodes)>>3;//divite by sizeof struct node*
  heap_sift_up(heap, heap_index);
}*/
void heapify(struct heap *heap){
  int i = heap->len/2;
  while (i >= 0) {
    heap_sift_down(heap, i--);
  }
}

void heap_add(struct heap *heap, struct node *edge){
  if(heap->len >= heap->size){
    heap->size *= 2;
    heap->nodes = xrealloc(heap->nodes, heap->size*sizeof(struct node));
  }
  heap->nodes[heap->len] = edge;
  edge->heap_index = heap->len;
  heap_sift_up(heap, heap->len);
  heap->len++;
}
struct node *heap_pop(struct heap *heap){
  if(heap->len < 0){
    return NULL;
  }
  struct node *retval = heap->nodes[0];
  heap->nodes[0] = heap->nodes[--heap->len];
  heap_sift_down(heap, 0);
  return retval;
}
struct node** heapsort_heap(struct heap *heap){
  int i;
  for(i=heap->len-1;i>0;i--){
    HEAP_SWAP(heap,i,0);
    heap_sift_down(heap, 0);
  }
  return heap->nodes;
}
struct heap *build_heap(struct node **nodes, size_t sz){
  struct heap *heap = xmalloc(sizeof(struct heap));
  heap->len = heap->size = sz;
  heap->nodes = nodes;
//  heapify(heap);
  return heap;
}
struct heap *make_heap(size_t initial_size){
  if(initial_size == 0){
    initial_size = 128;
  }
  DEBUG_PRINTF("Creating heap with size %lu\n",initial_size);
  struct heap *heap = xmalloc(sizeof(struct heap));
  heap->size = initial_size;
  heap->len = 0;
  heap->nodes = xmalloc(initial_size*sizeof(struct node));
  return heap;
}
    
void free_heap(struct heap *heap){
  free(heap->nodes);
  free(heap);
}


#define SWAP(x,y)                               \
  __extension__                                 \
  ({__typeof(x) __temp = x;                     \
    x = y;                                      \
    y = __temp;})

#define fib_parent(node) ((fib_node*)(((fib_node*)node)->parent_mark & ~3))

#define fib_mark(node) (((fib_node*)node)->parent_mark |= 1)
#define fib_unmark(node) (((fib_node*)node)->parent_mark &= ~1)
#define fib_marked(node) (((fib_node*)node)->parent_mark & 1)

//Iterate over the list begining at start, setting the current node
//to 'var' each iteration, the count variable is used to insure we do
//one iteration in the where case we have only one node
#define FOR_EACH_NODE(var, start)                       \
  int CAT(count, __LINE__) = 0;                         \
  for(fib_node *var = start;                            \
      var != start || CAT(count, __LINE__) <= 0; \
      CAT(count, __LINE__)++, var = var->right)

static inline void fib_set_parent(fib_node *x, fib_node *p){
  x->parent_mark = fib_marked(x) | (ulong)p;
}
static inline void fib_set_parent_mark(fib_node *x, fib_node *p, uint mark){
  x->parent_mark = (ulong)p | mark;
}

fib_heap *make_fib_heap(){
  fib_heap *heap = zmalloc(sizeof(fib_heap));
  return heap;
}
//link node into the list contating the node 'ls'
static inline void fib_list_insert(fib_node *ls, fib_node *node){
  fib_node *left = ls->left;
  fib_node *right = ls;

  left->right = node;
  right->left = node;

  node->left = left;
  node->right = right;

  return;
}
//Make the circular linked list containing node into a linear doubly
//linked list starting at node (i.e node->left will be NULL)
static inline void fib_list_break(fib_node *node){
  fib_node *tmp = node->left;
  node->right = NULL;
  tmp->left = NULL;
}
static inline void fib_list_remove(fib_node **listptr, fib_node *node){
  if(node == node->right){
    if(listptr){
      //if node == node->right and *listptr != node that means node isn't
      //in the list pointed to by listptr, which is an error
      assert(*listptr == node);
      *listptr = NULL;
    }
  } else {
    node->left->right = node->right;
    node->right->left = node->left;
  }
  return;
}
//Merges 2 doubly linked lists by connecting b to the left of a
//(which is the same as connecting a to the right of b), and updating
//the sibling pointers
static inline void fib_list_merge(fib_node *a, fib_node *b){
    /*
    ???
    <- A2 - A1 - A0 -> + <- B0 - B1 - B2 -> =>
    <- A2 - A1 - A0 - B0 - B1 - B2 ->
   */
  fib_node *a_right = a->right;
  fib_node *b_left = b->left;

  a->right = b;
  b->left = a;

  a_right->left = b_left; //was a
  b_left->right = a_right; //was b
  return;
}

fib_node* fib_heap_insert(fib_heap *heap, struct node *data){
  fib_node *node;
  if(heap->free_list != NULL){
    node = heap->free_list;
    heap->free_list = heap->free_list->right;
  } else {
    node = zmalloc(sizeof(fib_node));
  }
  node->left = node->right = node;
  node->data = data;

  if(heap->min == NULL){
    heap->min = node;
  } else {
    fib_list_insert(heap->min, node);

    if(node->data->dist < heap->min->data->dist){
      heap->min = node;
    }
  }
  data->heap_node = node;
  heap->num_nodes++;
  heap->num_roots++;
  return node;
}

//Merges a and b, Following this operation both a and b should be considered
//invalid.
//Internally one of them will be repurposed into the new heap,
//but this is an implementation detail.
fib_heap *fib_heap_merge(fib_heap *a, fib_heap *b){
  if(a->min == NULL){
    free(a);
    return b;
  }
  if(b->min == NULL){
    free(b);
    return a;
  }

  fib_node *a_min = a->min;
  fib_node *b_min = b->min;
  fib_list_merge(a_min, b_min);


  if(a_min->data->dist < b_min->data->dist){
    a->min = b_min;
  }

  a->num_nodes += b->num_nodes;
  a->num_roots += b->num_roots;

  free(b);
  return a;
}


static void fib_heap_consolidate(fib_heap *heap);

struct node *fib_heap_pop(fib_heap *heap){
  if(heap->min == NULL){
    return NULL;
  }
  fib_node *min = heap->min;
  struct node *ret = min->data;
  fib_node *min_children = min->children;
  if(min_children){
    FOR_EACH_NODE(tmp, min_children){
      tmp->parent_mark = 0;
      heap->num_roots++;
    }
    if(min == min->right){
      assert(min == min->left);
      //this will cause heap->min to be set to min_children
      min->right = min_children;
    } else {
      fib_node *min_children_left = min_children->left;
      //link children into the root list
      min_children->left = min->left;
      min_children_left->right = min->right;
      //remove min from root list
      min->left->right = min_children;
      min->right->left = min_children_left;
    }
  } else {
    if(min == min->right){
      assert(min == min->left && heap->num_nodes == 1);
      heap->num_nodes = heap->num_roots = 0;
      heap->min = NULL;
      goto FINISH;
    } else {
      min->left->right = min->right;
      min->right->left = min->left;
    }
  }
  heap->num_roots--;
  heap->num_nodes--;
  heap->min = min->right;
  fib_heap_consolidate(heap);

 FINISH:
  memset(min, '\0', sizeof(fib_node));
  min->right = heap->free_list;
  heap->free_list = min;
  ret->heap_node = NULL;
  return ret;
}

static inline void fib_heap_link(fib_node *x, fib_node *y){
  //remove y from root list
  //  y->left->right = y->right;
  //  y->right->left = y->left;

  if(!x->children){
    x->children = y;
    y->left = y->right = y;
  } else {
    fib_list_insert(x->children, y);
  }
  x->degree++;
  fib_set_parent_mark(y, x, 0);
}

static void fib_heap_consolidate(fib_heap *heap){
  int max_degree = LOG_2(heap->num_nodes);
  int num_roots = heap->num_roots;
  fib_node **arr = zmalloc((max_degree+1) * sizeof(fib_node*));
  fib_node **ls = xmalloc(num_roots * sizeof(fib_node*));

  int i = 0;
  int count329 = 0;
  for(fib_node *tmp = heap->min;
      tmp != heap->min || count329 <= 0;
      count329++, tmp = tmp->right){
    //  FOR_EACH_NODE(tmp, heap->min){
    ls[i++] = tmp;
  }
  if(i != heap->num_roots) {
    free(arr);
    free(ls);
    abort();
  }
  for(i=0;i<num_roots;i++){
    fib_node *x = ls[i];
    int degree = x->degree;
    while(arr[degree] != NULL){
      fib_node *y = arr[degree];
      //make sure x < y
      if(y->data->dist < x->data->dist){
        SWAP(x,y);
      }
      fib_heap_link(x, y);
      arr[degree] = NULL;
      degree += 1;
    }
    arr[degree] = x;
  }

  heap->min = NULL;
  heap->num_roots = 0;

  for(i=0;i<=max_degree;i++){
    if(arr[i] != NULL){
      heap->num_roots++;
      if(heap->min == NULL){
        heap->min = arr[i];
        arr[i]->left = arr[i]->right = arr[i];
      } else {
        fib_list_insert(heap->min, arr[i]);
        if(arr[i]->data->dist < heap->min->data->dist){
          heap->min = arr[i];
        }
      }
    }
  }
  free(arr);
  free(ls);
}
static void fib_cut(fib_heap *heap, fib_node *x, fib_node *y){
  if(x == x->right){
    y->children = NULL;
  } else {
    x->left->right = x->right;
    x->right->left = x->left;
    y->children = x->right;
  }
  y->degree--;
  fib_list_insert(heap->min, x);
  fib_set_parent(x, NULL);
  heap->num_roots++;
}
static void fib_cascading_cut(fib_heap *heap, fib_node *x){
  fib_node *y = fib_parent(x);
  if(y){
    if(!fib_marked(y)){
      fib_mark(y);
    } else {
      fib_cut(heap, x, y);
      fib_cascading_cut(heap, y);
    }
  }
}
int fib_heap_decrease_key(fib_heap *heap, fib_node *x, double dist){
  if(x->data->dist < dist){
    return -1;//new key is greater than current key, fail
  }
  x->data->dist = dist;
  fib_node *y = fib_parent(x);
  if(y != NULL && x->data->dist < y->data->dist){
    fib_cut(heap, x, y);
    fib_cascading_cut(heap, y);
  }
  if(x->data->dist < heap->min->data->dist){
    heap->min = x;
  }
  return 0;
}
int fib_heap_delete(fib_heap *heap, fib_node *x){
  int err = fib_heap_decrease_key(heap, x, -1.0 * INFINITY);
  if(err){return -1;}
  assert(heap->min == x);
  fib_heap_pop(heap);
  return 0;
}

static inline void fib_node_free(fib_node *node){
  if(node->children){
    fib_list_break(node->children);
    fib_node *cur = node->children;
    fib_node *next = cur->right;
    while(next){
      fib_node_free(cur);
      cur = next;
      next = cur->right;
    }
    fib_node_free(cur);
  } else {
    free(node);
  }
}
    
void fib_heap_free(fib_heap *heap){
  if(heap->min == NULL){
    free(heap);
    return;
  }
  fib_list_break(heap->min);
  fib_node *cur = heap->min;
  fib_node *next = cur->right;
  while(next){
    fib_node_free(cur);
    cur = next;
    next = cur->right;
  }
  fib_node_free(cur);

  cur = heap->free_list;
  next = cur->right;
  while(next){
    free(cur);
    cur = next;
    next = cur->right;
  }
  free(cur);

  free(heap);
}
