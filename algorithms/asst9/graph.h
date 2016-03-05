/**
 * \file graph.h
 *
 *
 *
 * \author jtd7
 * \date 20-10-2011
 */

#ifndef GRAPH_H
#define GRAPH_H

#include <stdlib.h>
#include <math.h>

/* I do a lot of dynamic resizing arrays in my code.
   I used 1.5 instead of 2 to avoid overshooting my end memory too much. */
const static float ar_scale = 1.5;

/* rather than storing an array of tuples, I store two arrays,
   one for x, and one for y. */
struct points {
  float *xs;
  float *ys;
  int num_points;
};


struct graph {
/* your fields here */
};

struct edges {
/* your fields here */
};

/* an auxilliary data structure that has nothing to do with your code,
   used for making all of the valid edges efficiently */
struct vect {
	int* elms;
	int num_elms;
	int index;
};

struct points* newPoints(float *xs, float *ys, int np);
struct edges* makeEdges(struct points *p, double max_dist);
struct graph* newGraph(struct points* p, double max_dist);
int addEdge(struct edges* e, double cost, int start, int end);

#endif
