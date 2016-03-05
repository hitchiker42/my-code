/**
 * \file flow_net.h
 *
 * A simple network for computing max flow.
 *
 * \author Christopher Wilt
 */

#if !defined(_FLOW_NET_H_)
#define _FLOW_NET_H_

#include <stdio.h>

struct edge {
    int startNode;
    int endNode;
    int capacity;
};

struct edgeList {
    struct edge* ed;
    struct edgeList* next;
};

struct node {
    int id;
    int parent;
    struct edgeList* out;
    struct edgeList* in;
};

struct net {
    int regular_nodes;
    struct node* nodes;
};

/*
 * Adds an arc between 's' and 't' with a capacity of 'cap' in the
 * given network.  If 'antip' is non-zero then antiparallel edges may
 * be added to the network.  If 'antip' is zero then a dummy node will
 * be placed in the network to prevent antiparallel edges.
 *
 * Return 0 on success and 1 on failure.
 */
int net_add_arc(struct net *n, int antip, unsigned int s, unsigned int t,
		int cap);

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

