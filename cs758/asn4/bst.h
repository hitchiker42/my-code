/**
 * \file bst.h
 *
 * A binary search tree on disk locations.
 *
 * \author eaburns
 * \date 10-08-2010
 */

#if !defined(_BST_H_)
#define _BST_H_

#include "disk_loc.h"

/* A simple binary tree node of disk locations. */
struct tree_node {
	struct disk_location loc;
	struct tree_node *left;
	struct tree_node *right;
};


/*
 * Inserts a location into the binary tree.
 *
 * Returns 0 on success and 1 on failure.
 */
int insert_tree_location(struct tree_node **root, struct disk_location *loc);

/* Frees the memory allocated for the location tree. */
void free_tree(struct tree_node *root);

/*
 * Removes the given node from the tree.
 *
 * Blame Cormen, Leiserson, Rivest and Stein for the awful variable
 * names.
 */
void remove_tree_node(struct tree_node **nodep);

/*
 * Remove the given location from the tree if it is there.
 *
 * Returns 1 if the location was removed and 0 if not (because it was
 * not found).
 */
int remove_tree_location(struct tree_node **root, struct disk_location *loc);

/*
 * Gets up to 'n' elements that come directly after (and including)
 * 'loc'.
 */
int tree_n_after(struct tree_node *node, struct disk_location *loc,
		 struct disk_location *locs[], unsigned int n,
		 unsigned int fill);

/*
 * Gets up to 'n' elements that come directly before (and including)
 * 'loc'.
 */
int tree_n_before(struct tree_node *node, struct disk_location *loc,
		  struct disk_location *locs[], unsigned int n,
		  unsigned int fill);

/*
 * Output the tree to the given file.
 */
void output_tree(FILE *outfile, int depth, struct tree_node *root);

#endif /* !_BST_H_ */
