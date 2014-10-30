/**
 * \file graph.h
 1 *
 *
 *
 * \author jtd7,
 * \date 20-10-2011
 */

#ifndef GRAPH_H
#define GRAPH_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <math.h>
#ifdef DEBUG
#define DEBUG_PRINTF(fmt,args...) fprintf(stderr,fmt,##args)
#else
#define DEBUG_PRINTF(...)
#endif
typedef struct vertex vertex;
struct vertex {
  float x;
  float y;
  int index;
  vertex *parent;
};
typedef struct edge graph_edge;
struct heap {
  graph_edge *edges;
  size_t size;
  size_t len;
  size_t max_size;
};
//x86_64 has 2 return registers so we can use this by value
//and have it be pretty fast
struct edge {
  int start;
  int end;
  double weight;  
};
graph_edge heap_pop(struct heap *heap);
void heap_add(struct heap *heap, graph_edge edge);
graph_edge* heap_sort_heap(struct heap *heap);
struct heap *make_heap(size_t initial_size, size_t max_size);
void edge_qsort(graph_edge *edges, size_t len);
static __attribute__((unused))  void *xmalloc(size_t sz){
  void *temp = calloc(sz,1);
  if(!temp && sz){
    fprintf(stderr,"Error, out of memory\n");
    exit(1);
  }
  return temp;
}
static  __attribute__((unused)) void *xmalloc_atomic(size_t sz){
  void *temp = malloc(sz);
  if(!temp && sz){
    fprintf(stderr,"Error, out of memory\n");
    exit(1);
  }
  return temp;
}
static __attribute__((unused))  void *xrealloc(void *ptr, size_t sz){
  void *temp = realloc(ptr, sz);
  if(!temp && sz){
    fprintf(stderr,"Error, out of memory\n");
    exit(1);
  }
  return temp;
}
static __attribute__((unused))  int string_eq(const char *x, const char *y){
  return !strcmp(x,y);
}
int make_spanning_tree(graph_edge *tree,vertex *vertices,
                       int num_vertices,double max_distance);
#endif
