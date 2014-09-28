/**
 * \file list.h
 *
 * A linked list of disk locations.
 *
 * \author eaburns
 * \date 10-08-2010
 */

#if !defined(_LIST_H_)
#define _LIST_H_

#include "disk_loc.h"

/* A linked list node. */
struct list_node {
	struct disk_location loc;
	struct list_node *next;
};

/*
 * Inserts a new location into the list (in sorted order).
 *
 * Returns 0 on success and 1 or error.
 */
int insert_list_location(struct list_node **node, struct disk_location *loc);

/* Frees the memory for the linked list. */
void free_list(struct list_node *head);

/*
 * Removes the disk location from the list.
 *
 * Return 1 if the location was removed and 0 if it was not found.
 */
int remove_list_location(struct list_node **node, struct disk_location *loc);

/* Prints the list to the output file. */
void output_list(FILE *outfile, struct list_node *head);

/*
 * Adds the 'n' locations before (and equal to) 'loc' to the 'locs'
 * array.
 *
 * The return value is the number of elements that were *not* added to
 * the array.
 */
int list_n_before(struct list_node *node, struct disk_location *loc,
		  struct disk_location *locs[], unsigned int n);

/*
 * Adds the 'n' locations after (and equal to) 'loc' to the 'locs'
 * array.
 *
 * The return value is the number of elements that were *not* added to
 * the array.
 */
int list_n_after(struct list_node *node, struct disk_location *loc,
		 struct disk_location *locs[], unsigned int n,
		 unsigned int left);

#endif /* !_LIST_H_ */
