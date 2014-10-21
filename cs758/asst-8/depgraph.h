/**
 * \file depgraph.h
 *
 * A dependency graph.
 *
 * \author eaburns
 * \date 25-08-2010
 */

#if !defined(_DEPGRAPH_H_)
#define _DEPGRAPH_H_

#include <stdio.h>
#include "nodelist.h"
#include "trie.h"


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

    /* This field is initialized to NULL and remains unused by the
     * rest of the code.  Feel free to store any data in here that
     * you think that you will need. */
    void *priv;
};

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

#endif				/* !_DEPGRAPH_H_ */
