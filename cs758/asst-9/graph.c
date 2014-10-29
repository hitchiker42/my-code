/**
 * \file graph.c
 *
 *
 *
 * \author tucker dinapoli
 * \date 29-10-2014
 */

#define _POSIX_C_SOURCE 200112L

#include "graph.h"
#include <math.h>
#include <stdio.h>
#define pow2(x) (x*x)
static inline float dist(vertex v1, vertex v2){
  return sqrt((pow2(v1.x)-pow2(v2.x))+(pow2(v1.y)-pow2(v2.y)));
}

static struct heap* make_edges(vertex *v, int num_vertices, double max_dist){
  //make an initial guess that we have no more edges than vertices
  struct heap* retval = make_heap(num_vertices);
  int num_edges = 0;
  int i,j;
  for(i=0;i<num_vertices;i++){
    for(j=0;j<num_vertices;j++){
      if(i==j){continue;}//avoid comuputing the distance unecessarly
      double d = dist(v[i],v[j]);
      if(d <= max_distance and i!=j){
        struct edge e = {.start = i,.end = j, .weight = d};
        heap_add(retval, e);
      }
    }
  }
  return retval;
}
static vertex *find_set(vertex *v){
  vertex *p = v;
  while(p->parent != p){
    p = p->parent;
  }
  v->parent = p;
  return p;
}
static vertex *union_sets(vertex *v1, vertex *v2){
  v1 = find_set(v1);
  v2 = find_set(v2);
  //meh, ignore rank
  v2->parent = v1;
  return v1;
}
/*tree T;
for v in vector
make_set(v)
     for e in edges
     if(find_set(e.start) != find_set(e.end)){
       add_to_tree(e);
       union(u,v);
*/
struct node {
  graph_edge e;
  struct node *next;
};
int create_spanning_tree(struct heap *edges, graph_edge *tree,
                         vertex *vertices, int num_vertices){
  //we'll never have more edges in the spanning tree than vertices
  graph_edge *tree = xmalloc_atomic(sizeof(edge)*num_vertices);
  int tree_index = 0;
  graph_edge e;
  //There's probably a way to avoid checking all edges, but oh well
  while(edges->len > 0){
    e = heap_pop(edges);
    if(find_set(vertices+e.start) != find_set(vertices+e.end)){
      tree[tree_index++] = e;
      union_sets(vertices+e.start,vertices+e.end);
    }
  }
  return tree_index;
}
  
int make_spanning_tree(graph_edge *tree, vertex *vertices,
                       int num_vertices, double max_distance){
  struct heap* heap = make_edges(vertices, num_vertices, max_distance);
  graph_edge *retval = create_spanning_tree(heap, vertices, num_vertices);
  free(heap->edges);
  free(heap);
  return retval;
}
