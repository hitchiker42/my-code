/**
 * \file roadmap.h
 *
 * Reads a roadmap in binary format into memory.  The nodes of the
 * roadmap are stored in an array where the index in the array is the
 * number of the node minus 1.
 *
 * \author eaburns
 * \date 18-08-2010
 */

#if !defined(_ROADMAP_H_)
#define _ROADMAP_H_

#include <stdio.h>

struct arc {
    /* Target of the arc. */
    unsigned int target;

    /* The weight of the arc. */
    unsigned int wt;
};

struct node {
    /* Node's ID number. */
    unsigned int num;

    /* The x and y coordinate of the given node. */
    int x, y;

    /* Number of arcs. */
    unsigned int narcs;

    /* Pointer to the arc array for this node. */
    struct arc *arcv;
};


/* Gets the square distance between two points. */
double sq_dist(int x, int y, int u, int v);


/*
 * Loads the map from the given file.  The nodes are returned in an
 * array via the 'nodesp' argument and the number of nodes in the
 * array is returned via the 'nnodes' argument.  If this function
 * returns successufully then the caller must free the nodes by using
 * the free_map() function.
 *
 * Return 1 on success and 0 on failure.
 */
int load_map(FILE * f, struct node **nodesp, unsigned int *nnodes);


/*
 * Frees the road map memory.
 */
void free_map(struct node *nodes, unsigned int nnodes);


#endif				/* !_ROADMAP_H_ */
