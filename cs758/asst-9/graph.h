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
typedef struct point point;
struct point {
  float x;
  float y;
};
/* rather than storing an array of tuples, I store two arrays,
   one for x, and one for y. */
struct points {
  float *xs;
  float *ys;
  int num_points;
};
typedef struct vertex vertex;
typedef struct graph graph;
struct vertex {
  float x;
  float y;
  int index;
  vertex *parent;
};
struct graph {
  /* your fields here */
  edge *edges;
  
};
typedef struct edge graph_edge;
struct heap {
  graph_edge *edges;
  size_t size;
  size_t len;
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
/* an auxilliary data structure that has nothing to do with your code,
   used for making all of the valid edges efficiently */
typedef struct vector vector;
struct vector {
  int* elms;
  unsigned int size;
  unsigned int index;
};

static void *xmalloc(size_t sz){
  void *temp = calloc(sz,1);
  if(!temp && sz){
    fprintf(stderr,"Error, out of memory\n");
    exit(1);
  }
  return temp;
}
static void *xmalloc_atomic(size_t sz){
  void *temp = malloc(sz);
  if(!temp && sz){
    fprintf(stderr,"Error, out of memory\n");
    exit(1);
  }
  return temp;
}
static void *xrealloc(void *ptr, size_t sz){
  temp = realloc(ptr, sz);
  if(!temp && sz){
    fprintf(stderr,"Error, out of memory\n");
    exit(1);
  }
  return temp;
}
static int string_eq(char *x, char *y){
  return !strcmp(x,y);
}
struct points* newPoints(float *xs, float *ys, int np);
struct edges* makeEdges(struct points *p, double max_dist);
struct graph* newGraph(struct points* p, double max_dist);
int addEdge(struct edges* e, double cost, int start, int end);

#endif
