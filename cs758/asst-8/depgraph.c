/**
 * \file depgraph.c
 *
 * A dependency graph.
 *
 * \author eaburns
 * \date 25-08-2010
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "depgraph.h"
#include "nodelist.h"
#include "depfile.h"
#include "trie.h"
#include "debug.h"

static char *string_dup(const char *s)
{
    size_t len = strlen(s);
    char *copy;

    copy = malloc(sizeof(*copy) * (len + 1));
    if (!copy)
	return NULL;

    strncpy(copy, s, len + 1);

    return copy;
}


struct node *create_node(const char *str, unsigned int id)
{
    struct node *n;

    n = malloc(sizeof(*n));
    if (!n) {
	perror("malloc failed");
	return NULL;
    }
    n->file = string_dup(str);
    if (!n->file) {
	free(n);
	return NULL;
    }
    n->id = id;
    n->kids = NULL;
    n->priv = NULL;

    return n;
}

#ifdef DEBUG
int output_node(FILE * f, struct node *n){
  int len = strlen(n->file);
  fprintf(f, "%.*s ", len, n->file);
  return len+1;
}
#else
void output_node(FILE * f, struct node *n){
    fprintf(f, "%s\n", n->file);
}
#endif

void free_node(struct node *n)
{
    struct node_list *link;
    if(!n){
      return;
    }

    free((void *) n->file);

    link = n->kids;
    while (link) {
	struct node_list *l = link;
	link = link->next;
	free(l);
    }

    free(n);
}


/* Test if the given dependency exists. */
static int has_dependency(struct node *n, struct node *dep)
{
    struct node_list *link;

    for (link = dep->kids; link; link = link->next) {
	if (link->n == n)
	    return 1;
    }

    return 0;
}


int add_dependency(struct node *n, struct node *dep)
{
    struct node_list *link;

    if (has_dependency(n, dep))
	return 0;

    link = malloc(sizeof(*link));
    if (!link) {
	perror("malloc failed");
	return 1;
    }

    debug_printf(DEBUG_LOTS, "adding dependency %s -> %s\n",
	    n->file, dep->file);

    link->n = n;
    link->next = dep->kids;
    dep->kids = link;

    return 0;
}


struct node *get_node(struct trie *nodes, const char *str,
		      unsigned int *nnodesp)
{
    struct node *n = trie_lookup(nodes, str);

    if (!n) {
	int err;

	n = create_node(str, *nnodesp);
	if (!n)
	    return NULL;
	*nnodesp += 1;

	err = trie_insert(nodes, str, n);
	if (err) {
	    free_node(n);
	    return NULL;
	}

	debug_printf(DEBUG_LOTS, "new node for %s\n", str);
    }

    return n;
}

/************************************************************/
/* Building the node graph.                                 */
/************************************************************/

/*
 * Adds a node to the list.  This is for iterating over the trie.
 */
static int add_to_list_from_trie(const char *key, void *_n, void *_headp)
{
    struct node *n = _n;
    struct node_list **headp = _headp;
    return node_list_add(headp, n);
}


int build_graph(const char *depfile, struct node_list **headp)
{
    FILE *f;
    int err;
    unsigned int nnodes = 0;
    struct trie nodes;

    trie_init(&nodes);
    f = fopen(depfile, "r");
    if (!f) {
	perror("failed to open dependency file");
	return -1;
    }
    err = read_depfile(f, &nodes, &nnodes);
    fclose(f);
    if (err) {
	trie_free(&nodes);
	return -1;
    }

    err = trie_iter(&nodes, add_to_list_from_trie, headp);
    if (err) {
	free_node_list(*headp);
	nnodes = -1;
    }
    trie_free(&nodes);

    return nnodes;
}
