/**
 * \file depfile.h
 *
 * Reading a dependency file.
 *
 * \author eaburns
 * \date 25-08-2010
 */

#if !defined(_DEPFILE_H_)
#define _DEPFILE_H_

#include <stdio.h>

#include "trie.h"

/*
 * Read the dependency file and add dependencies between the nodes.
 * 'nnodesp' is used to count the nodes as they are encountered and to
 * assign their ID values.  Upon return 'nnodesp' points to an integer
 * that has the final node count.
 *
 * Return 0 on success and 1 on error.
 */
int read_depfile(FILE * f, struct trie *nodes, unsigned int *nnodesp);

#endif				/* !_DEPFILE_H_ */
