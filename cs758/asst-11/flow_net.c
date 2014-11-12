/**
 * \file flow_net.c
 *
 *
 *
 * \author jtd7
 * \date 07-11-2011
 */

#include "flow_net.h"
#include <limits.h>
#include <stdlib.h>
#include <assert.h>

extern int dest, source;


int sumFlowIn(struct net* n, int id);
int sumFlowOut(struct net* n, int id);
void checkAllFlow(struct net* n);


int net_add_arc(struct net *n, int antip, unsigned int s, unsigned int t,
		int cap){
  return 1;
}

int net_free(struct net *n){
  return 1;
}

int init_flow_net(struct net* n, int n_nodes, int n_edges){
  net->num_edges = n_edges;
  net->num_nodes = n_nodes;
  net->nodes = xmalloc_atomic(sizeof(struct node)*n_nodes);
  net->edges = xmalloc_atomic(sizeof(struct edge)*n_edges);
  return 0;
}

static void find_path_init(struct net *n){
  int i;
  for(i=0;i<n->num_nodes;i++){
    n->nodes[i].visited = 0;
    n->nodes[i].to_parent = NULL;
  }
}
//returns an array of pointers to edges that lead from start to end
//or NULL if no such path exists
static svector *find_path(struct net *N, int start, int end, svector *queue){
  if(queue == NULL){
    queue = xmalloc(sizeof(svector));
    *queue = (svector){.size = 64, .len = 0, 
                       .pointers = xmalloc_atomic(sizeof(struct node *)*64)};
  } else {
    queue->len = 0;
  }
  find_path_init(N);
  N->nodes[start].visited = 1;
  svector_push((N->nodes+start), queue);
  struct edge *e;
  struct edgeList *edges;
  while(queue->len > 0){
    struct node *u = svector_pop(queue, struct node*);
    edges = u->out;
    while((e = edges->ed) != NULL){
      if(e->remaining <= 0){
        continue;
      }
      struct node *v = N->nodes + e->end;
      if (v->visited == 0) {
        v->visited = 1;
        v->to_parent = e;
        svector_push(v, queue);
      }
    }
  }
  struct node *tail = N->nodes+end;
  if(tail->to_parent == NULL){
    return NULL;
  }
  svector_push(tail->to_parent,queue);
  while(tail->to_parent != NULL){
    svector_push(tail->to_parent,queue);
    tail = N->nodes + tail->to_parent->start;
  }
  //shrink queue if it's significantly larger than it needs to be 
  if (queue->size > nearest_power_of_2(queue->len)) {
    queue->pointers = xrealloc(queue->pointers, nearest_power_of_2(queue->len));
  }
  return queue;
}
#define min_capacity(e1,e2)                             \
  ((e1->remaining) < (e2->remaining) ? 1 : 0)
static int find_remaining_capacity(svector *path){
  struct edge **edges = (struct edge **)path->pointers;
  struct edge *e = ARR_MINMAX(edges, path->len, min_capacity);
  return e->remaining;
}
static void augment_path(struct net *n, svector *path){
  int flow = find_remaining_capacity(path);
  int i;
  for(i=0;i<path->len;i++){
    struct edge *e = ((struct edge**)(path->pointers))[i];
    e->remaining -= flow;
    e->used += flow;
  }
} 
int compute_max_flow(struct net* n){
  svector *path = NULL;
  while((path = find_path(n, 0, n->num_nodes, path)) != NULL){
    augment_path(n, path);    
  }
  int max_flow=0;
  struct edgeList *edges = (n->nodes + n->num_nodes-1)->in;
  while(edges != NULL){
    struct edge *e = edges->ed;
    edges = edges->next;
    max_flow += e->used;
  }
  free(path->pointers);
  free(path);
  return max_flow;
}
