/**
 * \file map_route.c
 *
 * Plans a route in a roadmap.
 *
 * \author eaburns
 * \date 18-08-2010
 */
#include "roadmap.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>



#include "system.h"
/*
struct arc {
  //Target of the arc.
  unsigned int target;

  //The weight of the arc.
  unsigned int wt;
};

struct node {
  //Node's ID number. 
  unsigned int num;

  //The x and y coordinate of the given node. 
  int x;
  int y;

  //Number of arcs. 
  unsigned int narcs;

//Pointer to the arc array for this node. 
  struct arc *arcv;
};
*/
/*
 * Single source shortest path solver using Djikstra's algorithm.
 * 'source' is the ID value of the source node (its node structure can
 * be found at nodes[source - 1]) and 'target' is the ID value of the
 * target node.  The 'costp' argument should be used to return the
 * final path cost value.
 *
 * Return 0 on success and 1 on failure.
 */
int djikstra(FILE * outfile, struct node *nodes, unsigned int num_nodes,
	     unsigned int source, unsigned int target, unsigned int *costp,
             struct node **nodes_location){
  /* Find the shortest path and output it.  Return the cost via
   * 'costp'. */
  int i;
  /* The heap is composeed of pointers to the actual nodes which are
   in the nodes array. Each node has a field which keeps track of it's 
   index in the heap so that it can be sifted up the heap if it's distance
   decreases*/
/*  struct node **nodes_location = xmalloc_atomic(sizeof(struct node*)*num_nodes);
  for(i=0;i<num_nodes;i++){
    nodes_location[i] = nodes +i;
    nodes[i].dist = INFINITY;
    nodes[i].heap_index = nodes[i].num;
  };*/
  struct node *start = nodes_location[source];
  struct node *last = nodes_location[target];
  start->dist=0;
  //this just creates the heap struct, since all the nodes but the first
  //have an infinite distance it already obeys the heap property
  struct heap *heap = build_heap(nodes_location, num_nodes);
  heap_sift_up(heap, start->heap_index);
  struct node *n;
  while((n=heap_pop(heap)) != NULL){//loop a max of |nodes| times
    if(n == last){break;}
    for(i=0;i<n->num_arcs;i++){
      struct arc a = n->arcv[i];
      struct node *m = nodes + a.target;
      if(m->dist > n->dist + a.weight){
        m->dist = n->dist +a.weight;
        m->parent = n;
        heap_sift_up(heap, m->heap_index);
      }
    }

  }
  if(last->dist == INFINITY){
    fprintf(stderr,"Error target node is not connected to start node\n");
    free_heap(heap);
    return 1;
  }
  *costp = (int)last->dist;
  i = 0;
  while(last->parent != start){
    //reuse the space from the heap to store the nodes on the shortest path
    nodes_location[i++] = last;
    last = last->parent;
  }
  while(i-->0){
    fprintf(outfile, "%d\n", nodes_location[i]->num);
  }
  free_heap(heap);
  return 0;
}

/* Read the user's input and call the search. */
static int input_and_search(FILE * infile, struct node nodes[],
			    unsigned int nnodes){
  int err = 0;
  unsigned int s, t;
  unsigned int cost = 0;
  double start, end;
  int i;

  while (fscanf(infile, "%u %u", &s, &t) == 2) {
    s = s - 1; /* avoiding 1-indexing */
    t = t - 1;
    if (s >= nnodes) {
      fprintf(stderr, "Start node is invalid\n");
      continue;
    }
    if (t >= nnodes) {
      fprintf(stderr, "Target node is invalid\n");
      continue;
    }
    //this isn't techically part of the algorithm so don't count the
    //time it takes, I figure the reference does the same since my program
    //consistantly runs about 2 seconds faster than the reference, from
    //start to finish, but when I this stuff in my dijkstra code I get
    //slower times for actually finding the shortest path, despite
    //running faster overall
    
    struct node **nodes_location = xmalloc_atomic(sizeof(struct node*)*nnodes);
    for(i=0;i<nnodes;i++){
      nodes_location[i] = nodes +i;
      nodes[i].dist = INFINITY;
      nodes[i].heap_index = nodes[i].num;
    };
    printf("finding a route from %d to %d\n", s, t);
    start = get_current_seconds();
    err = djikstra(stdout, nodes, nnodes, s, t, &cost, nodes_location);
    end = get_current_seconds();
    if (err) {
      break;
    }
    printf("cost: %u\n", cost);
    printf("time: %f seconds\n", end - start);
  }

  return err;
}
static int my_input_and_search(FILE * infile, struct roadmap *map){
  int err = 0;
  unsigned int s, t;
  unsigned int cost = 0;
  double start, end;
  int i;
  struct node *nodes = map->nodes;
  int num_nodes = map->num_nodes;
  while (fscanf(infile, "%u %u", &s, &t) == 2) {
    s = s - 1; /* avoiding 1-indexing */
    t = t - 1;
    if (s >= num_nodes) {
      fprintf(stderr, "Start node is invalid\n");
      continue;
    }
    if (t >= num_nodes) {
      fprintf(stderr, "Target node is invalid\n");
      continue;
    }
    //this isn't techically part of the algorithm so don't count the
    //time it takes, I figure the reference does the same since my program
    //consistantly runs about 2 seconds faster than the reference, from
    //start to finish, but when I this stuff in my dijkstra code I get
    //slower times for actually finding the shortest path, despite
    //running faster overall    
    struct node **nodes_location = xmalloc_atomic(sizeof(struct node*)*num_nodes);
    for(i=0;i<num_nodes;i++){
      nodes_location[i] = nodes +i;
      nodes[i].dist = INFINITY;
      nodes[i].heap_index = nodes[i].num;
    };
    printf("finding a route from %d to %d\n", s, t);
    start = get_current_seconds();
    err = djikstra(stdout, nodes, num_nodes, s, t, &cost, nodes_location);
    end = get_current_seconds();
    if (err) {
      break;
    }
    printf("cost: %u\n", cost);
    printf("time: %f seconds\n", end - start);
  }

  return err;
}


/* Print the usage string. */
static void usage(void){
  fprintf(stderr, "Usage:\nmap-route <datafile> <file>\n");
  exit(EXIT_FAILURE);
}


int main(int argc, const char *const argv[]){
  int err, ret = EXIT_FAILURE;
  FILE *infile = stdin;
  double start, end;
  unsigned int nnodes;
  struct node *nodes;

  if (argc != 3) {
    usage();
  }

  if (strcmp(argv[2], "-") != 0) {
    infile = fopen(argv[2], "r");
    if (!infile) {
      perror("Error opening input file");
      return EXIT_FAILURE;
    }
  }
  struct roadmap *map;
  start = get_current_seconds();
  map = read_input(argv[1]);
/*
  The order of code here was weird, so I fixed it, gotos are fine
  for error handling but at the same time if you don't really need
  them don't use them
*/
  end = get_current_seconds();
  if(map == NULL){
    return EXIT_FAILURE;
  }
  printf("Loaded %ld nodes in %f seconds\n", map->num_nodes, end - start);
  printf("Using %d MB\n", peak_memory_usage());

  err = my_input_and_search(infile, map);
  /* Again weird order, the was it was was that if there was an error
     it jumped to a place to gree the map, but the map wasn't
     used in any other calculations so there was no need
*/
  printf("Peak memory usage %d MB\n", peak_memory_usage());
  free_roadmap(map);
  fclose(infile);
  return err;
}
