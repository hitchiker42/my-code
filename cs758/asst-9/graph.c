/**
 * \file graph.c
 *
 *
 *
 * \author tucker dinapoli
 * \date 29-10-2014
 */
#define _GNU_SOURCE

#include "graph.h"
#include <math.h>
#include <stdio.h>
#define pow2(x) ((x)*(x))
static inline float dist4(float x1, float x2, float y1, float y2){
  float dist = sqrt(pow2(x2-x1)+pow2(v2-v1));
  return dist
}
static inline float dist(vertex v1, vertex v2){
  float dist = sqrt(pow2(v2.x-v1.x)+pow2(v2.y-v1.y));
  DEBUG_PRINTF("Distance between (%f,%f) and (%f,%f) is %f\n",
               v1.x,v1.y,v2.x,v2.y,dist);
  return dist;
}
typedef struct vector vector;
struct vector {
  vertex **elems;
  int len;
  int size;
};
vector *make_grid(vertex *vertices, int num_vertices, double max_dist){
  //only called if ceil(1/max_dist) > 2
  int num_sectors = ceil(1/max_dist);
  vector *retval = xmalloc_atomic(sizeof(vector)*num_sectors*num_sectors);
  for(i=0;i<num_vertices;i++){
    vertex v = vertices[i];
    int x = v.x;
    int y = v.y;
    push(v, retval);
  }
  return retval;
}
static size_t max_edges(vector *grid, int N){
  int i,j,k,l;
  size_t num_edges = 0;
  int s1_elems, s2_elems;
  for(i=0;i<N;i++){
    for(j=0;j<N;j++){
      s1_elems = grid[i*N+j].len;      
      for(k=i;k<i+2 && k<N;k++){
        for(l=j;l<j+2 && l<N;l++){
          s2_elems = grid[k*N+l].len;
          num_edges += s1_elems*s2_elems;
        }
      }
    }
  }
  return num_edges;
}
struct edges {
  graph_edge *edges;
  size_t len;
};
static struct edges make_edges_grid(vector *grid, vertex *vertices,
                                    double max_dist){
  struct heap* retval = make_heap(num_vertices, num_vertices*num_vertices);
  int i,j,k,l,m,n;
  vertex *v1,*v2;
  vector sector1,sector2;
  int N = (1/max_dist);
  for(i=0;i<N;i++){
    for(j=0;j<N;j++){/*for each grid section*/
      sector1 = grid[i*N+j];
      for(k=i;k<i+2 && k<N;k++){/*for the negihbors/the section itself*/
        for(l=j;l<j+2 && l<N;l++){
          sector2 = grid[k*N+l];
          int sector2_max = ((i==k) && (j==l) ? sector1.len/2 : sector2.len);
          for(n=0;n<sector1.len;n++){/*For the points in the section*/
            for(m=0;m<sector2_max;m++){/*For the points in the other section*/
              v1 = sector1.elems[n];
              v2 = sector2.elems[m];
              float distance = dist4(v1->x,v2->x,v1->y,v2->y);
              if(distance <= max_dist){
                struct edge e = {.start = i, .end = j, .weight = d};
                heap_add(retval,e);
              }
            }
          }
        }
      }
    }
  }
  graph_edge *sorted_edges = heapsort_heap(retval);
  return (struct edges){.edges = sorted_edges, .len = retval.len};
}
#define atomic_inc(x)  __atomic_add_fetch(x,1, __ATOMIC_SEQ_CST)
static struct edges make_edges_parallel(vector *grid, vertex *vertices,
                                       double max_dist){
  int N = ceil(1/max_dist);
  size_t num_edges = max_edges(grid,N);//upper bound on number of edges
  volatile size_t edge_index = -1;//start at -1 since we use an 
  //atomic operation akin to ++edge_index
  graph_edge *edges = xmalloc_atomic(sizeof(graph_edge)*num_edges);
  int h,i,j,k,l,m,n;
  vertex *v1,*v2;
  vector sector1,sector2;
  //break the top loop into as many iterations as possible to maximize
  //the gains from doing things in parallel
#pragma omp parallel for
  for(h=0;h<N*N;h++){
    i = h/N;
    j = h%N;
    sector1 = grid[i*N+j];
    for(k=i;k<i+2 && k<N;k++){
      for(l=j;l<j+2 && l<N;l++){
        sector2 = grid[k*N+l];
        int sector2_max = ((i==k) && (j==l) ? sector1.len/2 : sector2.len);
        for(n=0;n<sector1.len;n++){
          for(m=0;m<sector2_max;m++){
            v1 = sector1.elems[n];
            v2 = sector2.elems[m];
            float distance = dist4(v1->x,v2->x,v1->y,v2->y);
            if(distance <= max_dist){
              struct edge e = {.start = i, .end = j, .weight = d};
              size_t index = atomic_inc(edge_index);
            }
          }
        }
      }
    }
  }
  edge_qsort(retval, edge_index);
  return (struct edges){.edges = retval, .len = edge_index};
}
  
static struct edges make_edges_naieve(vertex *v, int num_vertices,
                                     double max_dist){
  //make an initial guess that we have no more edges than vertices
  DEBUG_PRINTF("Constructing edges\n");
  struct heap* retval = make_heap(num_vertices, num_vertices*num_vertices);
  int i,j; 
  for(i=0;i<num_vertices;i++){
    for(j=0;j<num_vertices;j++){
      if(j>=i){continue;}
      double d = dist(v[i],v[j]);
      if(d <= max_dist){

        struct edge e = {.start = i, .end = j, .weight = d};
        DEBUG_PRINTF("adding edge from %d to %d with weight %f to heap\n",
                     e.start,e.end,e.weight);
        heap_add(retval, e);
      }
    }
  }
  graph_edge *sorted_edges = heapsort_heap(retval);
  return (struct edges){.edges = sorted_edges, .len = retval.len};
}
/*
  Instead of implementing a seperate structure for use with
  union find I just have a parent pointer in the vertex struct
  that I use will union/find 
*/
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
  DEBUG_PRINTF("Creating spanning tree\n");
  int tree_index = 0;
  graph_edge e;
  //There's probably a way to avoid checking all edges, but oh well
  while(edges->len > 0){
    e = heap_pop(edges);
    if(find_set(vertices+e.start) != find_set(vertices+e.end)){
      DEBUG_PRINTF("adding edge from %d to %d with weight %f\n",
                   e.start,e.end,e.weight);
      tree[tree_index++] = e;
      union_sets(vertices+e.start,vertices+e.end);
    }
  }
  return tree_index;
}
  
int make_spanning_tree(graph_edge *tree, vertex *vertices,
                       int num_vertices, double max_distance){
  struct heap* heap = make_edges(vertices, num_vertices, max_distance);
  int num_edges = create_spanning_tree(heap, tree, vertices, num_vertices);
  free(heap->edges);
  free(heap);
  return num_edges;
}
