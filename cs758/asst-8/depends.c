/**
 * \file depends.c
 *
 * Computes a linearization of a dependency graph.
 *
 * \author eaburns
 * \date 25-08-2010
 */

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/time.h>

#include "trie.h"
#include "depgraph.h"
#include "nodelist.h"
#include "depfile.h"
#include "debug.h"
#define _GNU_SOURCE
#include <getopt.h>
#ifdef DEBUG
#define DEBUG_PRINTF(fmt,args...) fprintf(stderr,fmt,##args)
#else
#define DEBUG_PRINTF(...)
#endif
typedef struct dep_node dep_node;
typedef struct node_info node_info;
enum state {
  unfinished,
  finished
};
static inline dep_node *pop(struct node_list *list){
  
  dep_node *node = (dep_node*)list->n;
  //or *list = *list->next;
  if(list->next){
      list->n = list->next->n;
      list->next = list->next->next;
  } else {
    list->next = NULL;
    list->n = NULL;
  }
  return node;
}
#define pop(list)                                                       \
  __extension__({                                                       \
      dep_node *temp= (list == NULL ? NULL : (dep_node*)list->n);       \
      if(temp){list = list->next;}                                      \
      temp;})
#define push(elt, place) node_list_add(place, (struct node*)elt)
int t_val = 1;
int *t = &t_val;
struct dep_node {
  /* The current file. */
  const char *file;

  /* Nodes that depend on this node. */
  struct node_list *kids;

  /* A unique ID number of this node.  Nodes are numbered from 0
   * so that the ID field can safely be used as an array
   * index. */
  unsigned int id;

  /* This field is initialized to NULL and remains unused by the
   * rest of the code.  Feel free to store any data in here that
   * you think that you will need. */
  /*  node_info *info;*/
  int *state;
  
};
/*struct node_info {
  dep_node *parent;
  enum state state;
  };*/
/* The type of the sorting function. */
typedef int (*sort_t) (struct node_list ** sorted,
		       struct node_list * to_sort);

/*
 * You should implement this function.  You are given a list of graph
 * nodes 'nodes' that have their dependencies linked up (see
 * depgraph.h for 'struct node').  This function should build a linked
 * list using the node_list_add() (see nodelist.h) function with
 * 'headp' as the head pointer.
 */
static void dfs_recur(dep_node *node, struct node_list **sorted_list){
  dep_node *next_node;
  struct node_list *list = node->kids;
  while((next_node = pop(list))){
    if(next_node->state == NULL){
      next_node->state = t;
      dfs_recur(next_node, sorted_list);
    }
  }
  push(node, sorted_list);
  return;
}
  
  
static int topological_sort(struct node_list **headp,
			    struct node_list *nodes){
  DEBUG_PRINTF("Calling topological_sort\n");
  dep_node *cur_node;
  while((cur_node = pop(nodes))){
    if(cur_node->state == NULL){
      cur_node->state = t;
      dfs_recur(cur_node, headp);
    }
  }
  int retval = 0;
  return retval;
  
}

/*
 * You should implement this function If you are a graduate student.
 * You are given a list of graph nodes 'nodes' that have their
 * dependencies linked up (see depgraph.h for 'struct node').  The
 * function should break the nodes into their connected components.
 * Each component should be a node_list built using node_list_add()
 * (see nodelist.h) and its head should be pointed to by an element of
 * 'comps'.  The number of components used in the 'comps' array should
 * be returned via the 'ncompsp' argument.
 *
 * i.e. Upon return '*ncompsp' will be the number of elemens in
 * 'comps' that were filled in.  comps[i] will be the ith node list
 * for 0 <= i < *ncompsp.
 */
static int sorted_components(struct node_list *comps[], int *ncompsp,
			     sort_t sort, struct node_list *nodes)
{
    fprintf(stderr, "Unimplemented\n");
    return 1;
}

/************************************************************
 * A naive implementation of topological sort.  This implementation
 * uses the 'priv' field of a node to maintain a count of the number
 * of unresolved dependencies.  Then there is a double-for loop over
 * all of the nodes each time adding a node with 0 unresolved
 * dependencies to the reverse sorted list.  Finally when all nodes
 * are added to the list it is reversed and returned.
 ************************************************************/

/*
 * Increments the unhandled dependency counter on all dependies of n.
 */
static unsigned int mark_dependies(struct node_list *nodes)
{
    unsigned int nnodes;
    struct node_list *l;

    nnodes = 0;
    for (l = nodes; l; l = l->next) {
	l->n->priv = (void *) 0l;
	nnodes += 1;
    }

    for (l = nodes; l; l = l->next) {
	struct node_list *j;
	for (j = l->n->kids; j; j = j->next) {
	    long p = (long) j->n->priv;
	    j->n->priv = (void *) (p + 1);
	}
    }

    return nnodes;
}


/*
 * Decrements the unhandled dependency counter on all dependies of n.
 */
static int unmark_dependies(struct node *n)
{
    struct node_list *l;

    for (l = n->kids; l; l = l->next) {
	long p = (long) l->n->priv;
	assert(p > 0);
	if (p > 0)
	    l->n->priv = (void *) (p - 1);
    }

    return 0;
}


/*
 * A naive implementation of topological sort.  This function
 * implements the sort_t interface.
 *
 * Destructive on the 'priv' field of nodes.
 */
static int naive_sort(struct node_list **headp, struct node_list *nodes)
{
    int err;
    unsigned int nnodes, nfinished;
    struct node_list *l;
    struct node_list *rev = NULL;

    nnodes = mark_dependies(nodes);
    nfinished = 0;
    while (nfinished < nnodes) {
	for (l = nodes; l; l = l->next) {
	    struct node *n = l->n;
	    if ((long) n->priv == 0) {
		unmark_dependies(n);
		n->priv = (void *) -1l;
		nfinished += 1;
		err = node_list_add(&rev, n);
		if (err) {
		    free_node_list(rev);
		    return 1;
		}
	    }
	}
    }


    err = reverse_node_list(headp, rev);
    free_node_list(rev);

    return err;
}

/************************************************************
 * No need to change anything belowe here!
 ************************************************************/

/*
 * Outputs the list of nodes to the file 'f'.
 */
#ifdef DEBUG
static void output_nodes(FILE * f, struct node_list *head){
  dep_node *n;
  int num_chars = 0;
  while((n = pop(head))){
    if(num_chars > 70){
      fputs("\n",f);
      num_chars = 0;
    }
    num_chars += output_node(f, n);
  }
  fputs("\n",f);
}
#else
static void output_nodes(FILE * f, struct node_list *head)
{
    if (head) {
	output_node(f, head->n);
	output_nodes(f, head->next);
    }
}
#endif

/* Get the time. */
static double get_current_seconds(void)
{
    double sec, usec;
    struct timeval tv;

    if (gettimeofday(&tv, NULL) < 0) {
	perror("gettimeofday failed");
	exit(EXIT_FAILURE);
    }

    sec = tv.tv_sec;
    usec = tv.tv_usec;

    return sec + (usec / 1000000);
}


/*
 * Do a serial linerization.
 *
 * Return 0 on success and 1 on failure.
 */
static int do_serial(struct node_list *nodes, sort_t sort)
{
    int err;
    double start, end;
    struct node_list *head = NULL;

    start = get_current_seconds();
    err = sort(&head, nodes);
    end = get_current_seconds();    
    if (!err) {
      fprintf(stderr,"Dependency Graph:\n");
	output_nodes(stdout, head);
	fprintf(stdout, "time: %f seconds\n", end - start);
    } else {
      fprintf(stderr,"Error in sorting nodes\n");
    }
    free_node_list(head);

    return err;
}


/*
 * Do a parallel linerization.
 *
 * Return 0 on success and 1 on failure.
 */
static int do_parallel(struct node_list *nodes, sort_t sort)
{
    int i, err, ncomps = 0;
    double start, end;
    struct node_list **comps;
    int nnodes = node_list_length(nodes);

    comps = malloc(nnodes * sizeof(*comps));
    if (!comps) {
	perror("malloc failed");
	return 1;
    }

    start = get_current_seconds();
    err = sorted_components(comps, &ncomps, sort, nodes);
    end = get_current_seconds();
    fprintf(stderr,"Dependency Graph:\n");
    for (i = 0; i < ncomps; i += 1) {
	if (!err) {
	    if (i > 0)
		fprintf(stdout, "\n");
	    fprintf(stdout, "component:\n");
	    output_nodes(stdout, comps[i]);
	}
	free_node_list(comps[i]);
    }
    if (!err)
	fprintf(stdout, "time: %f seconds\n", end - start);

    free(comps);
    return 0;
}


/*
 * Build and linearize the dependency graph.
 *
 * Return 0 on success and 1 on failure.
 */
static int dependency_graph(int parallel, sort_t sort, const char *depfile){
    int err, nnodes;
    struct node_list *nodes = NULL;
    struct node_list *l;

    debug_printf(DEBUG_MINOR, "parallel: %s\n", parallel ? "true" : "false");
    debug_printf(DEBUG_MINOR, "dependency file: %s\n", depfile);
    nnodes = build_graph(depfile, &nodes);
    if (nnodes < 0) {
      return 1;
    }
    debug_printf(DEBUG_MINOR, "%d nodes in the graph\n", nnodes);

    if (parallel){
	err = do_parallel(nodes, sort);
    } else {
	err = do_serial(nodes, sort);
    }
    for (l = nodes; l; l = l->next)
	free_node(l->n);
    free_node_list(nodes);

    return err;
}


/* Print the usage string and exit with failure status. */
static void usage(void){
  fprintf(stderr,
          "Usage:\ndepends [-ph] [-a] <alg> [-i] <depfile> [-o] <outfile>\n");
  exit(EXIT_FAILURE);
}


/*
 * Get the sorting algorithm by name.
 *
 * Exits with failure if the algorithm name is unsupported.
 */
static sort_t get_alg(const char *alg)
{
    if (strcmp(alg, "naive") == 0)
	return naive_sort;
    else if (strcmp(alg, "tsort") == 0)
	return topological_sort;
    usage();
    return NULL;
}
int main(int argc, char *argv[]){
  int parallel = 0;
  int err;
  sort_t sort = topological_sort;
  char *input_filename;
  if(argc < 2){
    usage();
  }
  if(!strcmp("-p",argv[optind])){
    parallel = 1;
    optind++;
  }
  sort = get_alg(argv[optind++]);
  input_filename = argv[optind];
  //    } 
  err = dependency_graph(parallel, sort, input_filename);
  if (err) {
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

/*
int main(int argc, char* const argv[])
{
    int parallel = 0;
    int err;
    sort_t sort = topological_sort;
    char *input_filename = "/proc/self/fd/0", *output_filename;
    FILE *input = stdin;
    FILE *output = stdout;
    int opt;
    /    if(argc == 0){
      usage();
    }
    while((opt = getopt(argc, argv, "hpa:o:i:"))){
      switch(opt) {
        case 'h':
          usage();
        case 'a':
          sort = get_alg(optarg);
          break;
        case 'i': {
          input_filename = optarg;
          input = fopen(input_filename, "r");
          if(input == NULL){
            perror("fopen");
            exit(EXIT_FAILURE);
          }
          break;
        }
        case 'o': {
          output_filename = optarg;
          output = fopen(output_filename, "w");
          if(output == NULL){
            perror("fopen");
            exit(EXIT_FAILURE);
          }
          break;
        }
        case 'p':
          parallel = 1;
          break;
      }
    }
    if(optind == 1){ /
      if(argc < 2){
        usage();
      }
      if(!strcmp("-p",argv[optind])){
        parallel = 1;
        optind++;
      }
      sort = get_alg(argv[optind++]);
      input_filename = argv[optind];
      //    } 
    err = dependency_graph(parallel, sort, input_filename);
    if (err) {
	return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
*/
