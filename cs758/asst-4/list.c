/**
 * \file list.c
 *
 * A linked list of disk locations.
 *
 * \author eaburns
 * \date 10-08-2010
 */

#include <assert.h>
#include <stdlib.h>

#include "list.h"

/*
 * Makes a list node.
 *
 * Return 0 on success and 1 on error.
 */
static int make_list_node(struct disk_location *loc, struct list_node **n)
{
	struct list_node *node;

	node = malloc(sizeof(*node));
	if (!node) {
		perror("malloc failed");
		return 1;
	}

	node->loc = *loc;
	node->next = NULL;

	*n = node;

	return 0;
}


int insert_list_location(struct list_node **node, struct disk_location *loc)
{
	struct list_node *n;

	assert(node);

	n = *node;
	if (!n)
		return make_list_node(loc, node);

	if (compare_locations(loc, &n->loc) <= 0) {
		int err = make_list_node(loc, node);
		if (err)
			return 1;
		(*node)->next = n;
	} else {
		return insert_list_location(&n->next, loc);
	}

	return 0;
}


void free_list(struct list_node *head)
{
	if (head) {
		free_list(head->next);
		free(head);
	}
}


int remove_list_location(struct list_node **node, struct disk_location *loc)
{
	int c;
	struct list_node *n;

	assert(node);

	n = *node;
	if (!n)
		return 0;

	c = compare_locations(loc, &n->loc);
	if (c == 0) {
		*node = n->next;
		free(n);
		return 1;
	} else if (c > 0) {
		return remove_list_location(&n->next, loc);
	}

	return 0;
}


void output_list(FILE *outfile, struct list_node *head)
{
	struct list_node *n;

	for (n = head; n; n = n->next) {
		output_location(outfile, &n->loc);
		fprintf(outfile, "\n");
	}
}


int list_n_before(struct list_node *node, struct disk_location *loc,
		  struct disk_location *locs[], unsigned int n)
{
	if (!node)
		return n;

	if (compare_locations(&node->loc, loc) > 0) {
		return n;
	} else {
		int left = list_n_before(node->next, loc, locs, n);
		if (left > 0)
			locs[n - left] = &node->loc;
		left -= 1;
		return left > 0 ? left : 0;
	}
}


int list_n_after(struct list_node *node, struct disk_location *loc,
		 struct disk_location *locs[], unsigned int n,
		 unsigned int left)
{
	if (!node)
		return left;

	if (left <= 0)
		return 0;

	if (compare_locations(&node->loc, loc) < 0) {
		return list_n_after(node->next, loc, locs, n, left);
	} else {
		assert (left > 0);
		locs[n - left] = &node->loc;
		return list_n_after(node->next, loc, locs, n, left - 1);
	}
}
