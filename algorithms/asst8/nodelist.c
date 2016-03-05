/**
 * \file nodelist.c
 *
 *
 *
 * \author eaburns
 * \date 26-08-2010
 */

#include "depends.h"

void free_node_list(struct node_list *nodes){
    struct node_list *l;

    l = nodes;
    while (l) {
	struct node_list *n = l;
	l = l->next;
	free(n);
    }
}


int node_list_add(struct node_list **head, struct node *n){
    struct node_list *l;

    l = malloc(sizeof(*l));
    if (!l) {
	perror("malloc failed");
	return 1;
    }

    l->n = n;
    l->next = *head;
    *head = l;

    return 0;
}


int node_list_length(struct node_list *head){
    int count = 0;
    struct node_list *l;

    for (l = head; l; l = l->next)
	count += 1;

    return count;
}


int reverse_node_list(struct node_list **revd, struct node_list *list)
{
    if (list) {
	int err = node_list_add(revd, list->n);
	if (err)
	    return 1;
	err = reverse_node_list(revd, list->next);
	if (err)
	    return 1;
    }

    return 0;
}
