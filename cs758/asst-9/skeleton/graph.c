/**
 * \file graph.c
 *
 *
 *
 * \author jtd7
 * \date 20-10-2011
 */

#define _POSIX_C_SOURCE 200112L

#include "graph.h"
#include <math.h>
#include <stdio.h>

/* creates a new point structure */
struct points* newPoints(float *xs, float*ys, int np){
	struct points *p = malloc(sizeof(struct points));
	p->xs = xs;
	p->ys = ys;
	p->num_points = np;
	return p;
}


/* computes the distance between two points */
float distance(struct points* p, int i, int j){
	float x1 = p->xs[i];
	float x2 = p->xs[j];
	float y1 = p->ys[i];
	float y2 = p->ys[j];
	return sqrt(pow((x1 - x2),2) + pow((y1 - y2),2));
}

/* constructs a graph from a set of points.
   once we have the graph, we don't need the points,
   so maybe I can free it here? */
struct graph* newGraph(struct points* p, double max_distance){
	struct graph *g = malloc(sizeof(struct graph));
	/* probably want to fill in your graph fields */
	return g;
}

/* adds an edge (weight or cost, starting vertex, and ending vertex)
   to the set of all edges, returns the index of the next
   edge, or the number of edges once the last edge has been added */
int addEdge(struct edges* edges, double cost, int start, int end){
	/* probably want to make an edge and add it here. */
	return 0;
}


/* this is code for adding to a vector.  I'm using it to pair down on the
   number of points that need compared in order to construct the set of
   all edges. You're more than welcome to change this, but you shouldn't
   need to. */
void add(struct vect* v, int i){
	static int next_size = 0;
	if (v->index >= v->num_elms){
		next_size = ceil(v->num_elms * ar_scale);
		v->elms = realloc(v->elms, sizeof(int) * next_size);
		if(v->elms == 0) exit(EXIT_FAILURE);
		v->num_elms = next_size;
	}
	v->elms[v->index] = i;
	v->index++;
}

/* this constructs a 2-d array of vectors for containing points.  Basically
   I'm splitting up the entire graph into sectors s.t. there can only be edges
   between adjacent sectors.  This converts then n^2 cost of making all edges
   into something much more palatable */
struct vect **make_buckets(struct points *p,
			   double max_dist,
			   int num_buckets){
	int x = 0;
	int y = 0;
	int i = 0;
	struct vect **to_ret = malloc(sizeof(struct vect*) *
					   num_buckets);
	for(i = 0; i < num_buckets; i++){
		to_ret[i] = malloc(sizeof(struct vect) * num_buckets);
		for(x = 0; x < num_buckets; x++){
			to_ret[i][x].num_elms = 1;
			to_ret[i][x].elms = malloc(sizeof(int));
			if(to_ret[i][x].elms == 0) exit(EXIT_FAILURE);
			to_ret[i][x].index = 0;
		}
	}

	for(i = 0; i < p->num_points; i++){
		x = (int)(p->xs[i] / max_dist);
		y = (int)(p->ys[i] / max_dist);
		if (x == num_buckets) x -= 1;
		if (y == num_buckets) y -= 1;
		add(&to_ret[x][y], i);
	}
	for(x = 0; x < num_buckets; x++)
		for(y = 0; y < num_buckets; y++){
			/* I probably over-alloced for my buckets, so
			   now I'm resizing them to save memory */
			to_ret[x][y].elms = realloc(to_ret[x][y].elms,
						    to_ret[x][y].index *
						    sizeof(int));
			to_ret[x][y].num_elms = to_ret[x][y].index;
		}
	return to_ret;
}

/* This builds all of the edges in the graph by calling addEdge a bunch of
   times. It's an unholy mess because I refused to use c99, so I'm declaring a
   boatload of values up front.  In short, it iterates over the buckets created
   by the preceeding function, calling add edge on every valid edge in the
   graph. The horribly nested for loops save you a bunch of work and let you
   tackle truly huge graphs in reasonable times. */
struct edges* makeEdges(struct points* p, double max_dist){
	struct edges* to_ret = 0;
	int x = 0, y = 0, dx = 0, dy = 0, xp = 0, yp = 0, num_edges = 0,
	    i = 0, j = 0, p1 = 0, p2 = 0, num_buckets = ceil(1 / max_dist);
	double d = 0;
	struct vect **buckets = make_buckets(p,max_dist,num_buckets);
	for(x = 0; x < num_buckets; x++){
	   for(y = 0; y < num_buckets; y++){  /* for each grid cell */
	      for(dx = 0; dx <= 1; dx++){
		 for(dy = 0; dy <= 1; dy++){  /* for each neighbor */
		    xp = x + dx;
		    yp = y + dy;
		    if(xp < num_buckets && yp < num_buckets){
		      for(i = 0; i < buckets[x][y].num_elms; i++){
		  	 for(j = 0; j < buckets[xp][yp].num_elms; j++){
			    /* this next one cuts down the points to compare
			       incase we're calculating edges in a region */
			    if(xp != x || yp != y || (j > i)){
			      p1 = buckets[x][y].elms[i];
			      p2 = buckets[xp][yp].elms[j];
			      d = distance(p,p1,p2);
			      if(d <= max_dist && p1 != p2){
				      /* this is the important bit */
				      num_edges = addEdge(to_ret,d,p1,p2);
			      }
			    }
			 }
		      }
		    }
		 }
	      }
	   }
	}
	/* free the buckets, you don't need them */
	for(i = 0; i < num_buckets; i++){
		for(j = 0; j < num_buckets; j++)
			free(buckets[i][j].elms);
		free(buckets[i]);
	}
	free(buckets);
	/* depending on what you do in add edge, you may want to do
	   some additional work on to_ret */
	return to_ret;
}

