k#ifndef __DEPENDS_H__
#define __DEPENDS_H__
#ifdef __cplusplus
extern "C" {
#endif
#define _GNU_SOURCE //for str[n]dup
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define NELMS 256
typedef struct node dep_node;
typedef node_list node_list;
struct trie {
    void *data;
    const char *key;
    struct trie *kids[NELMS];
};
/* A dependency graph node. */
struct node {
  /* The current file. */
  const char *file;

  /* Nodes that depend on this node. */
  struct node_list *kids;

  /* A unique ID number of this node.  Nodes are numbered from 0
   * so that the ID field can safely be used as an array
   * index. */
  unsigned int id;

  //32 bits of padding regardless, so we may as well put something here
  int state;

  /* This field is initialized to NULL and remains unused by the
   * rest of the code.  Feel free to store any data in here that
   * you think that you will need. */
  union {
    void *priv;
    struct {
      int index;
      int link;
    };
  };
};
  /* A list of nodes. */
struct node_list {
    struct node *n;
    struct node_list *next;
};
//push/pop a node from a nodelist
#define pop(list)                               \
  __extension__                                 \
  ({dep_node *tmp = (list ? list->n : NULL);    \
    if(tmp){list = list->next;}                 \
    tmp;})
#define push(elt, place) node_list_add(place, elt)

/* Initialize a new trie. */
void trie_init(struct trie *t);

/* Free the memory associated with the given trie. */
void trie_free(struct trie *t);

/* Insert a binding of key=data into the trie. */
int trie_insert(struct trie *t, const char *key, void *data);

/* Look for the data associated with the given key. */
void *trie_lookup(struct trie *t, const char *key);

/* Iterator the function 'f' over each element of the trie. */
int trie_iter(struct trie *t, int (*f) (const char *, void *d, void *aux),
	      void *aux);
/*
 * Frees the memory associated with a node list.
 */
void free_node_list(struct node_list *nodes);

/*
 * Add an element to a node list.
 *
 * Return 1 on failure and 0 on success.
 */
int node_list_add(struct node_list **head, struct node *n);


/*
 * Get the length of the node list.
 */
int node_list_length(struct node_list *head);

/*
 * Reverses the nodes in 'list' into 'revd.
 */
int reverse_node_list(struct node_list **revd, struct node_list *list);

/*
 * Builds the node graph from the dependency file and puts all of the
 * nodes onto the list pointed to by 'headp'.
 */
int build_graph(const char *depfile, struct node_list **headp);

/*
 * Outputs the node to the given file.
 */
void output_node(FILE * f, struct node *n);

/*
 * Allocate a new node for the given string.
 *
 * Return the node on success and NULL on failure.
 */
struct node *create_node(const char *str, unsigned int id);

/*
 * Frees the memory associated with the given node.
 */
void free_node(struct node *n);

/*
 * Adds an arc in the graph marking 'dst' as a dependency of 'src'.
 *
 * Return 0 on success and 1 on failure.
 */
int add_dependency(struct node *src, struct node *dst);

/*
 * Get the node for the given string.  This uses the trie to ensure
 * that each string has a unique node associated with it.
 *
 * Returns the node on success and NULL on failure.
 */
struct node *get_node(struct trie *nodes, const char *str,
		      unsigned int *nnodesp);
/*
 * Read the dependency file and add dependencies between the nodes.
 * 'nnodesp' is used to count the nodes as they are encountered and to
 * assign their ID values.  Upon return 'nnodesp' points to an integer
 * that has the final node count.
 *
 * Return 0 on success and 1 on error.
 */
int read_depfile(FILE * f, struct trie *nodes, unsigned int *nnodesp);

#define XMALLOC
static inline void* xmalloc(size_t sz){
  void *mem = malloc(sz);
  if(!mem && sz != 0){
    perror("malloc");
    exit(EXIT_FAILURE);
  }
  return mem;
}
#ifdef __cplusplus
}
#endif
#endif /* __DEPENDS_H__ */
