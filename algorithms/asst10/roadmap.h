/**
 * \file roadmap.h
 *
 * Reads a roadmap in binary format into memory.  The nodes of the
 * roadmap are stored in an array where the index in the array is the
 * number of the node minus 1.
 *
 * \author eaburns
 * \date 18-08-2010
 */

#if !defined(_ROADMAP_H_)
#define _ROADMAP_H_

#include "util.h"
#include <stdio.h>
#include "debug.h"

struct arc {
  /* Target of the arc. */
  unsigned int target;

  /* The weight of the arc. */
  float weight;
};

struct node {
  /* Node's ID number. */
  unsigned int num;
  /* The x and y coordinate of the given node. */
  int x;
  int y;
  
  /* Number of arcs. */
  unsigned int num_arcs;
  /* Index of the node in the heap */
  //used as a pointer when using a fib_heap
  union {
    long heap_index;
    struct fib_node *heap_node;
  };
  
  /* Pointer to the arc array for this node. */
  struct arc *arcv;
  //Pointer to previous node on the shortest path
  struct node *parent;
  double dist;
};

struct roadmap {
  struct node *nodes;
  struct arc *arcs;
  size_t num_nodes;
  size_t num_arcs;
};

struct heap {
  struct node **nodes;
  int len;//len may be negitive if there are no elements in the heap
  unsigned int size;
};
typedef struct fib_heap fib_heap;
typedef struct fib_node fib_node;
struct fib_heap {
  fib_node *min;
  fib_node *free_list;
  size_t num_nodes;
  size_t num_roots;
};
struct fib_node {
  struct node *data;
  ulong parent_mark;
  fib_node *children;
  fib_node *left;
  fib_node *right;
  int degree;
};
struct roadmap *read_input(const char *input_filename);
void free_roadmap(struct roadmap *map);


/* Gets the square distance between two points. */
double sq_dist(int x, int y, int u, int v);


/*
 * Loads the map from the given file.  The nodes are returned in an
 * array via the 'nodesp' argument and the number of nodes in the
 * array is returned via the 'nnodes' argument.  If this function
 * returns successufully then the caller must free the nodes by using
 * the free_map() function.
 *
 * Return 1 on success and 0 on failure.
 */
int load_map(FILE * f, struct node **nodesp, unsigned int *nnodes);


/*
 * Frees the road map memory.
 */
void free_map(struct node *nodes, unsigned int nnodes);

struct heap *build_heap(struct node **nodes, size_t sz);
struct node *heap_pop(struct heap *heap);
void free_heap(struct heap *heap);
void heap_sift_up(struct heap *heap, int index);
void heap_sift_up_element(struct heap *heap, struct node *element);
void heap_add(struct heap *heap, struct node *edge);
struct heap *make_heap(size_t initial_size);

fib_heap *make_fib_heap();
fib_node *fib_heap_insert(fib_heap *heap, struct node *data);
fib_heap *fib_heap_merge(fib_heap *a, fib_heap *b);
struct node *fib_heap_pop(fib_heap *heap);
int fib_heap_decrease_key(fib_heap *heap, fib_node *x, double dist);
int fib_heap_delete(fib_heap *heap, fib_node *x);
void fib_heap_free(fib_heap *heap);
#endif				/* !_ROADMAP_H_ */
