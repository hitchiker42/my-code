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
#define pow2(x) __extension__ ({__typeof(x) __x = x; (__x*__x);})
static inline float dist4(float x1, float x2, float y1, float y2){
  float dist = sqrt(pow2(x2-x1)+pow2(y2-y1));
  DEBUG_PRINTF("Distance between (%f,%f) and (%f,%f) is %f\n",
               x1,y1,x2,y2,dist);
  return dist;
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
//add an element to a vector, the vector may or may not have been initialized, but
//if not initialized it needs to have been zeroed, also it's not really push, but eh
void push(void *elt, vector *place){
  if(place->len >= place->size){
    if(place->size == 0){
      place->size = 32;//this is a arbitary default size
    } else {
      place->size *=2;
    }
    place->elems = xrealloc(place->elems, place->size*sizeof(void*));
  }
  place->elems[place->len++] = elt;
}
vector *make_grid(vertex *vertices, int num_vertices, double max_dist){
  //only called if ceil(1/max_dist) > 2
  int num_sectors = ceil(1/max_dist);
  vector *retval = xmalloc(sizeof(vector)*pow2(num_sectors));
  int i;
  for(i=0;i<num_vertices;i++){
    vertex *v = vertices+i;
    int x = v->x;
    int y = v->y;
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
struct heap *make_edges_grid(vector *grid, vertex *vertices,
                                    size_t num_vertices, double max_dist){
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
//          int sector2_max = ((i==k) && (j==l) ? sector1.len/2 : sector2.len);
          for(n=0;n<sector1.len;n++){/*For the points in the section*/
            for(m=0;m<sector2.len;m++){/*For the points in the other section*/
              v1 = sector1.elems[n];
              v2 = sector2.elems[m];
              float distance = dist4(v1->x,v2->x,v1->y,v2->y);
              if(distance <= max_dist){
                struct edge e = {.start = n, .end = m, .weight = distance};
                DEBUG_PRINTF("Adding Edge from %d to %d\n",n,m);
                heap_add(retval,e);
              }
            }
          }
        }
      }
    }
  }
  return retval;
//  graph_edge *sorted_edges = heapsort_heap(retval);
//  edge_qsort(retval->edges, retval->len);
//  return (struct edges){.edges = retval->edges, .len = retval->len};
}
#if 0
/*
  This creates the edges in parallel, but after doing some
  profiling I realized that the sorting took up far more time
  so I switched to using a heap so I didn't need to sort all the edges
*/
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
//#pragma omp parallel for
  for(h=0;h<(N*N);h++){
    i = h/N;
    j = h%N;
    sector1 = grid[i*N+j];
    for(k=i;k<i+2 && k<N;k++){
      for(l=j;l<j+2 && l<N;l++){
        sector2 = grid[k*N+l];
        for(n=0;n<sector1.len;n++){
          for(m=0;m<sector2.len;m++){
            v1 = sector1.elems[n];
            v2 = sector2.elems[m];
            float distance = dist4(v1->x,v2->x,v1->y,v2->y);
            if(distance <= max_dist){
              struct edge e = {.start = n, .end = m, .weight = distance};
              size_t index = atomic_inc(&edge_index);
              DEBUG_PRINTF("Adding Edge from %d to %d\n",n,m);
              edges[index] = e;
            }
          }
        }
      }
    }
  }
  edge_qsort(edges, edge_index+1);
  return (struct edges){.edges = edges, .len = edge_index};
}
#endif   
static struct heap *make_edges_naieve(vertex *v, int num_vertices,
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
  return retval;
  //  graph_edge *sorted_edges = heapsort_heap(retval);
  //  return (struct edges){.edges = sorted_edges, .len = retval->len};
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
double create_spanning_tree(struct heap *edges, graph_edge *tree,
                         vertex *vertices, int num_vertices){
  DEBUG_PRINTF("Creating spanning tree\n");
  int tree_index = 0;
  graph_edge e;
  int i;
  double weight = 0;
  for(i=0;i<edges->len;i++){
    e = heap_pop(edges);
    if(find_set(vertices+e.start) != find_set(vertices+e.end)){
      DEBUG_PRINTF("adding edge from %d to %d with weight %f\n",
                   e.start,e.end,e.weight);
      tree[tree_index++] = e;
      weight += e.weight;
      if(tree_index >= num_vertices-1){
        break;
      }
      union_sets(vertices+e.start,vertices+e.end);
    }
  }
  return weight;
}
#define GRID_MIN_VERTICES 0
#define GRID_MIN_SECTORS 0
static struct heap* make_edges(vertex *vertices, size_t num_vertices, double max_dist){
/*the functions which subdivide the grid are only asymptotically faster
  than the naive functions for a max distance < 0.5, if the max distance is >= 0.5 
  they are O(n^2), the same as the naive ones. I'm not sure (and I'm too
  lazy to figure out) the actual running time of these functions. Also since they
  have a non trival startup time compared to the naive functions they really only 
  have an advantage for a sizable number of vertices */
 int num_sectors = ceil(1/max_dist);
 if(num_sectors < GRID_MIN_SECTORS || num_vertices < GRID_MIN_VERTICES){
   return make_edges_naieve(vertices, num_vertices, max_dist);
 }
 vector *grid = make_grid(vertices, num_vertices, max_dist);
 struct heap *retval = make_edges_grid(grid, vertices, num_vertices, max_dist);
 free(grid);
 return retval;
}
double make_spanning_tree(graph_edge *tree, vertex *vertices,
                       int num_vertices, double max_distance){
  struct heap *heap = make_edges(vertices, num_vertices, max_distance);
  double weight = create_spanning_tree(heap, tree, vertices, num_vertices);
  free_heap(heap);
  return weight;
}
