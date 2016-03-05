/**
 * \file nodelist.h
 *
 * A linked list of dependency graph nodes.
 *
 * \author eaburns
 * \date 26-08-2010
 */

#if !defined(_NODELIST_H_)
#define _NODELIST_H_

struct node;

/* A list of nodes. */
struct node_list {
    struct node *n;
    struct node_list *next;
};

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


#endif				/* !_NODELIST_H_ */
