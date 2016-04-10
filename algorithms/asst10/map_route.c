/**
 * \file map_route.c
 *
 * Plans a route in a roadmap.
 *
 * \author eaburns
 * \date 18-08-2010
 */

#include "roadmap.h"
#include "system.h"
#include "debug.h"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <math.h> //for INFINITY
#include <sys/times.h>
#include <sys/resource.h>

/*
 * Single source shortest path solver using Djikstra's algorithm.
 * 'source' is the ID value of the source node (its node structure can
 * be found at nodes[source - 1]) and 'target' is the ID value of the
 * target node.  The 'costp' argument should be used to return the
 * final path cost value.
 *
 * Return 0 on success and 1 on failure.
 */
//nodes_location is an array containing the memory locations of the nodes
int djikstra(FILE * outfile, struct node *nodes, unsigned int num_nodes,
             unsigned int source, unsigned int target,
             unsigned int *costp){
  /* Find the shortest path and output it.  Return the cost via
   * 'costp'. */
  struct node *start = nodes + source;
  struct node *last = nodes + target;
  struct node *n;
  int i;
  start->dist = 0;
  //this uses the nodes_location array as the memory for the heap
  struct heap *heap = make_heap(num_nodes);
  heap_add(heap, start);
  while((n = heap_pop(heap)) != NULL){
    if(n == last){break;}
    for(i=0;i<n->num_arcs;i++){
      struct arc a = n->arcv[i];
      struct node *m = nodes + a.target;
      if(m->dist > n->dist + a.weight){
        m->dist = n->dist + a.weight;
        m->parent = n;
        if(m->heap_index != -1){
          heap_sift_up(heap, m->heap_index);
        } else {
          heap_add(heap, m);
        }
      }
    }
  }
  if(last->dist == INFINITY){
    fprintf(stderr, "Can not reach target from source\n");
    free_heap(heap);
    return 1;
  }
  *costp = (int)last->dist;

  i = 0;
  //We don't need the heap any more, so store the nodes for the shortest
  //path in the memory used for the heap
  while(last != start){
#ifdef DEBUG
    if(i >= num_nodes){
      DEBUG_PRINTF("Error i = %d > num_nodes = %d\n", i, num_nodes);
      raise(SIGKILL);
    }
#endif
    heap->nodes[i++] = last;
    last = last->parent;
  }
  heap->nodes[i] = start;
  for(;i >= 0;i--){
    fprintf(outfile, "%d\n", heap->nodes[i]->num);
  }
  free_heap(heap);
  return 0;
}
int djikstra_fib(FILE * outfile, struct node *nodes, unsigned int num_nodes,
                 unsigned int source, unsigned int target,
                 unsigned int *costp){
  /* Find the shortest path and output it.  Return the cost via
   * 'costp'. */
  struct node *start = nodes + source;
  struct node *last = nodes + target;
  struct node *n;

  int i;
  start->dist = 0;

  fib_heap *heap = make_fib_heap();
  fib_heap_insert(heap, start);
  int __attribute__((unused)) count = 0;
  int __attribute__((unused)) num_fib_nodes = 1;

  while((n = fib_heap_pop(heap)) != NULL){
#if DEBUG
    count++;
    num_fib_nodes--;
#endif
    if(n == last){break;}
    for(i=0;i<n->num_arcs;i++){
      struct arc a = n->arcv[i];
      struct node *m = nodes + a.target;
      if(m->dist > n->dist + a.weight){
        m->parent = n;
        double dist = n->dist + a.weight;
        if(m->heap_node != NULL){
          fib_heap_decrease_key(heap, m->heap_node, dist);
        } else {
          m->dist = dist;
          num_fib_nodes++;
          fib_heap_insert(heap, m);
        }
      }
    }
  }
  if(last->dist == INFINITY){
    fprintf(stderr, "Can not reach target from source\n");
    fib_heap_free(heap);
    return 1;
  }
  *costp = (int)last->dist;

  i = 0;
  if(last->parent == start){
    fprintf(outfile, "%d\n", last->num);
  } else {
    /*
      Destructively reverse the list of parent pointers in the nodes,
      then traverse it printing each node.
    */
    struct node *prev = NULL;
    struct node *cur = last;

    while(cur != start){
      struct node *tmp = cur->parent;
      cur->parent = prev;
      prev = cur;
      cur = tmp;
    }
    cur->parent = prev;

    while(cur != NULL){
      fprintf(outfile, "%d\n", cur->num);
      cur = cur->parent;
    }
  }
  fib_heap_free(heap);
  return 0;
}
double get_seconds(){
  struct timespec ts = {0};
  clock_gettime(CLOCK_REALTIME, &ts);
  return ts.tv_sec + (ts.tv_nsec / 1e9);
}

/* Read the user's input and call the search. */
static int input_and_search(FILE * infile, FILE *outfile,
                            struct node nodes[],
                            unsigned int nnodes, int fib){
  int err = 0, i;
  unsigned int s, t;
  unsigned int cost = 0;
  double start, end;

  while (fscanf(infile, "%u %u", &s, &t) == 2) {
    s = s - 1; /* avoiding 1-indexing */
    t = t - 1;
    if (s >= nnodes) {
      fprintf(stderr, "Start node %d is greater than number of nodes %d\n", 
              s, nnodes);
      continue;
    }
    if (t >= nnodes) {
      fprintf(stderr, "Target node %d is greater than number of nodes %d\n",
              t, nnodes);
      continue;
    }
    fprintf(outfile, "finding a route from %d to %d\n", s, t);

    if(fib){
      for(i=0;i<nnodes;i++){
        nodes[i].dist = INFINITY;
        nodes[i].heap_node = NULL;
      }
      start = get_seconds();
      err = djikstra_fib(outfile, nodes, nnodes, s, t, &cost);
      end = get_seconds();
    } else {
      for(i=0;i<nnodes;i++){
        //I would do this when reading the input, if I wrote that
        nodes[i].dist = INFINITY;
        nodes[i].heap_index = -1;
      }
      start = get_seconds();
      err = djikstra(outfile, nodes, nnodes, s, t, &cost);
      end = get_seconds();
    }
    if(err){
      break;
    }
    fprintf(outfile, "cost: %u\n", cost);
    fprintf(outfile, "time: %f seconds\n", end - start);
  }

  return err;
}


/* Print the usage string. */
static void usage(void){
    fprintf(stderr, "Usage:\nmap-route [-fib] <datafile> <file | input> [outfile]\n");
    exit(EXIT_FAILURE);
}

#define USE_MY_READ_MAP 0
int main(int argc, const char *const argv[]){
    int err;
    FILE *infile = stdin, *outfile = stdout;
    double start, end;
    int fib = 0;

    if (argc < 3 || argc > 4){
      usage();
    }
    /*    if(argc >= 4){
      if(argv[1][0] == '-' && argv[1][1] == 'f'){
        fib = 1;
        argv = argv + 1;
        argc = argc - 1;
      } else {
        if(argc > 4){
          usage();
        }
      }
      }*/
#if USE_MY_READ_MAP
    err = access(argv[1], R_OK);
    if(err){
      fprintf(stderr, "Can't read file %s\n", argv[1]);
      perror("access");
      return EXIT_FAILURE;
    }
#else
    FILE *f = fopen(argv[1], "r");
    if(!f){
      perror("fopen");
      return EXIT_FAILURE;
    }
#endif
    if (strcmp(argv[2], "-") != 0) {
      /*
        If the second argument looks like numbers interpret it
        as giving the points directly, rather than in a file.
        this probably should check more than the first number,
        but it's not that important.
      */
      errno = 0;
      char *tmp = NULL;
      strtol(argv[2], &tmp, 0);
      if(errno == 0 && tmp != argv[2]){//strtol worked
        infile = fmemopen((void*)argv[2], strlen(argv[2]), "r");
      } else {
        infile = fopen(argv[2], "r");
      }
      if(!infile){
        perror("Error opening data file");
        return EXIT_FAILURE;
      }
    }
    if(argc == 4){
      outfile = fopen(argv[3], "w");
      if(!outfile){
        perror("Error opening output file");
        return EXIT_FAILURE;
      }
    }
   start = get_seconds();
#if USE_MY_READ_MAP
    struct roadmap *map = read_input(argv[1]);
    if(!map){
      fprintf(stderr, "Failed to read data file\n");
      return EXIT_FAILURE;
    }
#else
    unsigned int nnodes;
    struct node *nodes;
    if(load_map(f, &nodes, &nnodes)){
      return EXIT_FAILURE;
    }
#endif
    end = get_seconds();
#if USE_MY_READ_MAP
    printf("Loaded %ld nodes in %f seconds\n", map->num_nodes, end - start);
    printf("Using %d MB\n", peak_memory_usage());

    err = input_and_search(infile, outfile, map->nodes, map->num_nodes, fib);
#else
    printf("Loaded %d nodes in %f seconds\n", nnodes, end - start);
    printf("Using %d MB\n", peak_memory_usage());

    err = input_and_search(infile, outfile, nodes, nnodes, fib);
#endif

    if(err){
      return EXIT_FAILURE;
    }

    printf("Peak memory usage %d MB\n", peak_memory_usage());

    return EXIT_SUCCESS;
}
