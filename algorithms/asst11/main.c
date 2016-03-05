/**

Christopher Wilt
Computer Science 758 Skeleton

Flow Networks

This program is supposed to output the max flow for the network.
Networks come in on standard in, in the following format:

<num_nodes>
<num_edges>
<edges, 1 per line>

edges are as follows:
start end weight

As an example:

4
6
0 1 1
1 2 3
2 3 2
3 1 2
2 1 3
0 3 2

It is expected to output the flow in the following format:

<edge utilization, 1 per line>
<cost>

Edge utilization is formatted as follows:
start end flow

where start is the start vertex, end is the end vertex, and flow is
the amount of flow through that edge.

As an example:

0 3 2
0 1 1
3 1 2
3

Node 0 is defined as the source, and node 1 is defined as the sink.

IMPORTANT NOTE:

You will probably want to eliminate antiparallel edges.  This is fine,
but you will need to make sure your program prints out something that
corresponds to the ORIGINAL graph.  This means that if you elect to
add nodes to the graph, if your final flow goes through nodes you
added, you will need to eliminate the extra nodes.

 */

#include <stdlib.h>
#include <stdio.h>

#include "flow_net.h"


const int source = 0;
const int dest = 1;
int node_count = 0; 
int edge_count = 0;

void malloc_check(void* p){
    if(p == NULL){
	fprintf(stderr, "malloc failed\n");
	exit(1);
    }
}

int main(int argc, char** argv){
    if(argc > 1){
	fprintf(stderr, "this program does not take command line arguments\n");
	return 1;
    }
    if(!scanf("%d\n", &node_count)){
	fprintf(stderr, "failed to read number of vertices\n");
	return 1;
    }
    if(!scanf("%d\n", &edge_count)){
	fprintf(stderr, "failed to read number of edges\n");
	return 1;
    }
    struct net* f_net = malloc(sizeof(struct net));
    init_flow_net(f_net, node_count, edge_count);


    malloc_check(f_net);

    int edgeStart, edgeEnd, edgeWeight;
    int num_scanned = 3;

    while(num_scanned == 3){

	num_scanned = scanf("%d %d %d\n", &edgeStart, &edgeEnd, &edgeWeight);
	if(num_scanned != 3){
	    break;
	}
	int ret = net_add_arc(f_net, 0, edgeStart, edgeEnd, edgeWeight);
	if(ret != 0){
	    fprintf(stderr, "error readng graph\n");
	    return 1;
	}
    }

    int err;
    err = compute_max_flow(f_net);


    net_free(f_net);
    free(f_net);

    return err;
}
