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


#endif				/* !_NODELIST_H_ */
