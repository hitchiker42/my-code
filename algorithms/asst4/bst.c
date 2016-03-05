/**
 * \file bst.c
 *
 * A simple binary search tree of disk locations.
 *
 * \author eaburns
 * \date 10-08-2010
 */

#include <assert.h>
#include <stdlib.h>

#include "bst.h"
#include "disk_loc.h"

/*
 * Allocates a new node in the binary tree and returns a pointer to it
 * via 'n'.
 *
 * Return 0 on success and 1 on failure.
 */
static int make_tree_node(struct disk_location *loc, struct tree_node **n)
{
	struct tree_node *node;

	node = calloc(1, sizeof(*node));
	if (!node) {
		perror("calloc failed");
		return 1;
	}

	node->loc = *loc;
	*n = node;

	return 0;
}


int insert_tree_location(struct tree_node **root, struct disk_location *loc)
{
	struct tree_node *r;

	assert (root);

	r = *root;
	if (!r)
		return make_tree_node(loc, root);

	if (compare_locations(loc, &r->loc) <= 0)
		return insert_tree_location(&r->left, loc);

	return insert_tree_location(&r->right, loc);
}


void free_tree(struct tree_node *root)
{
	if (root) {
		free_tree(root->left);
		free_tree(root->right);
		free(root);
	}
}

/*
 * Returns the left-most child and its parent is returned via the
 * 'parent' argument
 */
static struct tree_node *minimum_with_parent(struct tree_node *node,
					     struct tree_node *prev,
					     struct tree_node **parent)
{
	if (!node)
		return NULL;

	while (node->left) {
		prev = node;
		node = node->left;
	}

	*parent = prev;

	return node;
}


void remove_tree_node(struct tree_node **nodep)
{
	struct tree_node *z, *pz, *y, *py, *x;

	z = *nodep;
	pz = NULL;

	if (!z->left || !z->right) {
		y = z;
		py = pz;
	} else {
		/* 'minimum' is sufficient instead of 'successor'
		 * because this branch only happens if both the left
		 * and right children are non NULL. */
		y = minimum_with_parent(z->right, z, &py);
	}

	if (y->left)
		x = y->left;
	else
		x = y->right;

	if (!py) {
		*nodep = x;
	} else {
		if (py->left == y)
			py->left = x;
		else
			py->right = x;
	}

	if (y != z)
		z->loc = y->loc;

	free(y);
}


int remove_tree_location(struct tree_node **root, struct disk_location *loc)
{
	int c;
	struct tree_node *r;

	assert(root);
	r = *root;
	if (!r)
		return 0;

	c = compare_locations(loc, &r->loc);
	if (c == 0) {
		remove_tree_node(root);
		return 1;
	} else if (c < 0) {
		return remove_tree_location(&r->left, loc);
	}

	return remove_tree_location(&r->right, loc);
}


int tree_n_after(struct tree_node *node, struct disk_location *loc,
		 struct disk_location *locs[], unsigned int n,
		 unsigned int fill)
{
	int c;

	if (!node)
		return fill;

	c = compare_locations(&node->loc, loc);
	if (c < 0) {
		return tree_n_after(node->right, loc, locs, n, fill);
	} else if (c >= 0) {
		int f = tree_n_after(node->left, loc, locs, n, fill);
		if (f < n) {
			locs[f] = &node->loc;
			f += 1;
			f = tree_n_after(node->right, loc, locs, n, f);
		}

		return f;
	}

	return fill;
}


int tree_n_before(struct tree_node *node, struct disk_location *loc,
		  struct disk_location *locs[], unsigned int n,
		  unsigned int fill)
{
	int c;

	if (!node)
		return fill;

	c = compare_locations(&node->loc, loc);
	if (c > 0) {
		return tree_n_before(node->left, loc, locs, n, fill);
	} else if (c <= 0) {
		int f = tree_n_before(node->right, loc, locs, n, fill);
		if (f < n) {
			locs[f] = &node->loc;
			f += 1;
			f = tree_n_before(node->left, loc, locs, n, f);
		}

		return f;
	}

	return fill;
}


void output_tree(FILE *outfile, int depth, struct tree_node *root)
{
	unsigned int i;
	for (i = 0; i < depth; i += 1)
		fprintf(outfile, " ");
	if (root) {
		output_location(outfile, &root->loc);
		fprintf(outfile, "\n");
		output_tree(outfile, depth + 1, root->left);
		output_tree(outfile, depth + 1, root->right);
	} else {
		fprintf(outfile, "(nil)\n");
	}
}
