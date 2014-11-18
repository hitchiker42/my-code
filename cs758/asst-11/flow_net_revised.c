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

static inline void print_edge(struct edge *e, FILE *out){
  fprintf(out, "%d %d %d\n", e->start, e->end, e->capacity);
}
void print_results(struct net *n);

static struct edge_list *edge_list_cons(struct edge *e, struct edge_list *el){
  struct edge_list *temp = xmalloc(sizeof(struct edge_list));
  temp->e = e;
  temp->next = el;
  return temp;
}
void add_to_in_edges(struct net *N, int node, struct edge *e){
  struct node *n = N->nodes + node;
  n->in = edge_list_cons(e, n->in);
}
void add_to_out_edges(struct net *N, int node, struct edge *e){
  struct node *n = N->nodes + node;
  n->out = edge_list_cons(e, n->out);
}
//gets a pointer to memory for a new edge
//I don't ever free them, but it doesn't really matter
#define next_edge(n)                                                    \
  __extension__({                                                       \
      struct edge *e = xmalloc(sizeof(struct edge));                    \
      n->cur_edge++;                                                    \
      e;})
#define next_node(n)                                                    \
  __extension__({                                                       \
    if(n->num_nodes <= n->cur_node){                                    \
      n->nodes = xrealloc(n->nodes, n->num_nodes *sizeof(struct node) *2); \
      n->num_nodes *= 2;                                                \
    }                                                                   \
    n->nodes + n->cur_node;})

//edge, in, out , capacity
#define init_edge_simple(e, i, o, c)                            \
  {                                                             \
    e->start = i;                                               \
    e->end = o;                                                 \
    e->capacity = c;                                            \
  }
/*
  when we create an edge (u,v) we also create its reciprocal (v,u),
  thus if an edge we're adding already exists we know it's an antiparallel edge.
*/
void init_edge(struct edge *e, int start, int end, int cap, struct net *n){
  init_edge_simple(e, start, end, cap);
  struct edge *recip = next_edge(n);
  init_edge_simple(recip, end, start, 0);
  e->recip = recip;
  recip->recip = e;
  recip->capacity = 0;
  add_to_out_edges(n, end, recip);
  //  add_to_in_edges(n, start, recip);

}

int net_add_arc(struct net *net, unsigned int s, unsigned int t, int cap){
  if(s == 1){
    return 0;
  }
  struct node n = net->nodes[s];
  struct edge_list *el = n.out;
  while(el != NULL){
    if(el->e->end == t){
      break;
    }
    el = el->next;
  }
  if(el != NULL){//we need to add a dummy node
    struct node *dummy = next_node(net);
    *dummy = (struct node){.id = net->cur_node, .dummy = 1,
                           .out = edge_list_cons(next_edge(net), NULL),
                           .in = edge_list_cons(next_edge(net), NULL)};
    init_edge(dummy->out->e, net->cur_node, t, cap, net);
    //    add_to_in_edges(net, t, dummy->out);
    init_edge(dummy->in->e, s, net->cur_node, cap, net);
    add_to_out_edges(net, s, dummy->in->e);
    net->cur_node++;
  } else {//no need to add a dummy node
    struct edge *e = next_edge(net);
    init_edge(e, s, t, cap, net);
    add_to_out_edges(net, s, e);
    add_to_in_edges(net, t, e);
  }
  return 0;
}

int net_free(struct net *n){
  free(n->nodes);
  int i;
  for(i=0;i<n->num_nodes;i++){
    struct edge_list *el = n->nodes[i].out;
    while(el != NULL){
      struct edge_list *temp;
      free(el->e);
      temp = el->next;
      free(el);
      el = el->next;
    }
  }    
  return 0;
}

int init_flow_net(struct net* net, int n_nodes, int n_edges){
  net->num_edges = n_edges;
  net->num_nodes = n_nodes;
  net->cur_node = n_nodes;
  net->cur_edge = 0;
  net->nodes = xmalloc(sizeof(struct node)*n_nodes);
  return 0;
}

static void find_path_init(struct net *n){
  int i;
  for(i=0;i<n->num_nodes;i++){
    n->nodes[i].visited = 0;
    n->nodes[i].to_parent = NULL;
  }
}
int test_capacity(struct edge *e){
  return e->capacity > 0;
}
int test_used(struct edge *e){
  return e->used > 0;
}
//returns an array of pointers to edges that lead from start to end
//or NULL if no such path exists
static svector *find_path(struct net *N, int start,
                          int end, svector *queue,
                          int (*test)(struct edge *)){
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
  struct edge_list *el;
  while(queue->len > 0){
    struct node *u = svector_pop(queue, struct node*);
    el = u->out;
    while(el != NULL){
      e = el->e;
      if(test(e)){
        struct node *v = N->nodes + e->end;
        if (v->visited == 0) {
          v->visited = 1;
          v->to_parent = e;
          svector_push(v, queue);
        }
      }
      el = el->next;
    }
  }
  struct node *tail = N->nodes+end;//pointer to the end node
  if(tail->to_parent == NULL){
    return NULL;//no path exists
  }
  //  svector_push(tail->to_parent,queue);
  while(tail->to_parent != NULL){
    svector_push(tail->to_parent,queue);
    tail = N->nodes + tail->to_parent->start;
  }
#ifdef DEBUG
  int i;
  fprintf(stderr,"Printing path:\n");
  for(i=0;i<queue->len;i++){
    struct edge *e = (struct edge *)queue->pointers[i];
    fprintf(stderr,"%d %d %d\n", e->start, e->end, e->capacity);
  }
  fprintf(stderr,"End of path\n");
#endif
  return queue;
}

#define min_capacity(e1,e2)                             \
  ((e1->capacity) < (e2->capacity) ? 1 : 0)

static int find_remaining_capacity(svector *path){
  struct edge **edges = (struct edge **)path->pointers;
  //this finds 'e' such that e->capacity is the minimum amongst all edges
  struct edge *e = ARR_MINMAX(edges, path->len, min_capacity);
  return e->capacity;
}
static void augment_path(struct net *n, svector *path){
  int remaining = find_remaining_capacity(path);
  int i;
  for(i=0;i<path->len;i++){
    struct edge *e = (struct edge*)path->pointers[i];
    e->capacity -= remaining;
    e->used += remaining;
    struct edge *r = e->recip;
    r->capacity += remaining;
    r->used -= remaining;
  }
}
int compute_max_flow(struct net* n){
  n->num_nodes = n->cur_node;
  svector *path = NULL;
  svector *last_path = NULL;
  while((path = find_path(n, 0, 1, path, test_capacity)) != NULL){
    augment_path(n, path);
    last_path = path;
  }
  path = last_path;
  int max_flow=0;
  print_results(n);
  if(last_path){
    free(last_path->pointers);
    free(last_path);
  }
  return max_flow;
}
#define min_used(e1,e2)                                 \
  ((e1->used) < (e2->used) ? 1 : 0)


void print_result_edge(struct edge *e, struct net *n){
  int start = e->start;
  int end = e->end;
  if(n->nodes[start].dummy){
    struct edge *dummy_start = n->nodes[start].in->e;
    start = dummy_start->start;
    dummy_start->used = 0;
  }
  if(n->nodes[end].dummy){
    struct edge *dummy_end = n->nodes[end].out->e;
    end = dummy_end->end;
    dummy_end->used = 0;
  }
  fprintf(stdout, "%d %d %d\n", start, end, e->used);
  e->used = 0;
}
void print_results_recur(struct edge_list *el, struct net *n){  
  print_result_edge(el->e,  n);
  while(el->next != NULL){
    el = el->next;
    if(el->e->used > 0){
      struct node *next = n->nodes + (el->e->start);
      print_results_recur(next->in, n);
    }
  }
}
void print_results(struct net *n){
  int total_flow;
  struct edge_list *el = n->nodes[1].in;
  struct edge_list *temp = el;
  while(temp != NULL){
    if(temp->e->used>0){
      total_flow += temp->e->used;
    }
    temp = temp->next;
  }
  print_results_recur(el,n);
  printf("%d\n", total_flow);
}
/*void print_results(struct net *n, svector *path){
  int i;
  for(i=0;i<n->num_nodes;i++){
    struct edge *e = n->nodes[i].out;
    while(e!=NULL){
      if(e->used > 0){
        print_result_node(e,n);
      }
      e = e->next;
    }
  }
  }*/
/*
void print_results(struct net *n, svector *path){
  svector *last_path=  path;
  int i;
  int total_flow = 0;
  while((path = find_path(n, 0, 1, path, test_used))){
    int min_flow = ARR_MINMAX(((struct edge **)path->pointers), path->len,
                              min_used)->used;
    for(i=0;i<path->len;i++){
      struct edge *e = path->pointers[i];
      if(e->used == 0){
        continue;//this can happen onlty because of dummy nodes
      }
      if(e->recip != NULL){
        e->recip = NULL;
        if(e->end == 1){
          total_flow += e->used;
        }
        print_result_edge(e, n, min_flow);
      }
      E->Used -= Min_Flow;
    }
  }
  Printf("%d\n", total_flow);
  return;
}
*/
