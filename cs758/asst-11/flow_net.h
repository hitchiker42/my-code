/**
 * \file flow_net.h
 *
 * A simple network for computing max flow.
 *
 * \author Christopher Wilt
 */

#if !defined(_FLOW_NET_H_)
#define _FLOW_NET_H_

#include "util.h"
#include <stdio.h>

struct edge {
  struct edge *recip;//if this is an edge (u,v) this points to (v,u)
  struct edge *next;
  //I thought I could simplify things by adding this field, but all it
  //did was overcomplicate things
  //  struct edge *next;
  int start;
  int end;
  int capacity;
  int used;
};
struct edge_list {
  struct edge *e;
  struct edge_list *next;
};
struct node {
  int id;
  int visited; //for BFS  
  int dummy;
  struct edge* to_parent;
  struct edge* out;
  struct edge* in;
};

struct net {
  int num_nodes;
  int num_edges;
  //only used while building the flow network
  int cur_edge;
  int cur_node;
  //arrays for the edges/nodes
  struct node* nodes;
};

/*
 * Adds an arc between 's' and 't' with a capacity of 'cap' in the
 * given network. If an arc between 't' and 's' already exists a dummy node
 * will be added to prevent antiparallel edges.
 *
 * Return 0 on success and 1 on failure.
 */
int net_add_arc(struct net *n, unsigned int s, unsigned int t, int cap);

/* free the network, put not the pointer to it */
int net_free(struct net *n);

/*
  Should initialize the flow network.
*/
int init_flow_net(struct net* n, int nodes, int edges);

/*
  Stub that should calculate the max flow of a network.
*/
int compute_max_flow(struct net* n);

#endif /* !_FLOW_NET_H_ */

