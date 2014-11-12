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
void  checkAllFlow(struct net* n);


int net_add_arc(struct net *n, int antip, unsigned int s, unsigned int t,
		int cap){
    return 1;
}

int net_free(struct net *n){
    return 1;
}

int init_flow_net(struct net* n, int n_nodes, int n_edges){
    return 1;
}



int compute_max_flow(struct net* n){
    return 1;
}

