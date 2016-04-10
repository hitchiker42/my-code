#ifndef __FIB_HEAP_H__
#define __FIB_HEAP_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "C_util.h"
typedef struct fib_heap fib_heap;
typedef struct fib_node fib_node;
struct fib_heap {
  cmp_fun cmp;
  fib_node *min;
  fib_node *free_list;
  //a value smaller than any that will be inserted into the heap
  void *min_val;//this is needed for deletion
  size_t num_nodes;
  size_t num_roots;
};
struct fib_node {
  void *data;
  ulong parent_mark;
  fib_node *children;
  fib_node *left;
  fib_node *right;
  int degree;
};
fib_heap *make_fib_heap(cmp_fun cmp, void *min_val);
//returns the node because you need to pass a node to decrease_key
fib_node* fib_heap_insert(fib_heap *heap, void *data);
fib_heap *fib_heap_merge(fib_heap *a, fib_heap *b);
//Returns NULL if the heap is empty, so you need to check that the heap
//is non-empty before calling if you have a node which might have a
//value of NULL
void *fib_heap_pop(fib_heap *heap);
//these next 2 return -1 on error and 0 on success. Decrease key will
//fail if the new key is less than the current one.
int fib_heap_decrease_key(fib_heap *heap, fib_node *x, void *data);
int fib_heap_delete(fib_heap *heap, fib_node *x);
//Free the heap and assoicated nodes, this is actually pretty slow since
//it needs to traverse the entire tree
void fib_heap_free(fib_heap *heap);
#ifdef __cplusplus
}
#endif
#endif /* __FIB_HEAP_H__ */
