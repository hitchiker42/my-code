/**
 * \file depfile.c
 *
 * Reading of dependency files.
 *
 * \author eaburns
 * \date 25-08-2010
 */

#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "depfile.h"
#include "depgraph.h"
#include "trie.h"
#include "debug.h"

#if !defined(LINE_MAX)
#if !defined(_POSIX2_LINE_MAX)
#define LINE_MAX 4096		/* should be large enough. */
#else
#define LINE_MAX _POSIX2_LINE_MAX
#endif				/* !_POSIX2_LINE_MAX */
#endif				/* !LINE_MAX */


/* Eat whitespace (not counting newlines). */
static int eat_whitespace(FILE * f)
{
    int c = fgetc(f);

    while (isspace(c) && c != '\n')
	c = fgetc(f);

    return c;
}


/* Eat all remaining non-whitespace characters in this token. */
static int eat_non_whitespace(FILE * f)
{
    int c = fgetc(f);

    while (!isspace(c))
	c = fgetc(f);

    return c;
}


/*
 * Read the next token into the 'token' argument.  If there is a
 * newline after the token then 'newline' is set to 1.  The return
 * value is the number of characters added to 'token' or EOF if the
 * end of file is reached.
 */
static int next_token(FILE * f, char token[], int size, int *newline)
{
    unsigned int i = 0;
    int c = eat_whitespace(f);

    if (c == EOF)
	return EOF;

    if (c == '\n')
	goto out;

    for (i = 0; i < size - 1; i += 1) {
	token[i] = c;
	c = fgetc(f);
	if (isspace(c) || c == EOF) {
	    i += 1;
	    break;
	}
    }

    if (!isspace(c) && c != EOF) {
	fprintf(stderr, "Warning: token is too long. Truncating!\n");
	c = eat_non_whitespace(f);
    }

  out:
    token[i] = '\0';
    *newline = c == '\n';
    debug_printf(DEBUG_SPAM, "token: [%s], newline=%s\n", token,
	    *newline ? "true" : "false");
    return i;
}


/*
 * Read the dependencies of 'n'.
 */
static int read_depends(FILE * f, struct trie *nodes, struct node *n,
			unsigned int *nnodesp)
{
    int err, len, newline, escaped = 0;
    int nnodes = 0;
    char token[LINE_MAX + 1];

    do {
	len = next_token(f, token, LINE_MAX + 1, &newline);
	if (len > 0 && strcmp(token, "\\") == 0)
	    escaped = 1;
	else if (len > 0) {
	    struct node *d;
	    escaped = 0;
	    d = get_node(nodes, token, nnodesp);
	    if (!d)
		return -1;
	    err = add_dependency(n, d);
	    if (err)
		return -1;
	}
    } while (len != EOF && (!newline || escaped));

    return nnodes;
}


static int _read_depfile(FILE * f, struct trie *nodes,
			 unsigned int *nnodesp)
{
    int len, newline, err;
    char token[LINE_MAX + 1];
    struct node *n;

    len = next_token(f, token, LINE_MAX + 1, &newline);
    while (len != EOF) {
	if (len == 0) {
	    fprintf(stderr, "Malformed dependency file\n");
	    return 1;
	}

	if (token[len - 1] != ':') {
	    fprintf(stderr, "Expected ':', got '%c'\n", token[len - 1]);
	    return 1;
	}
	token[len - 1] = '\0';

	n = get_node(nodes, token, nnodesp);
	if (!n)
	    return -1;

	if (!newline) {
	    err = read_depends(f, nodes, n, nnodesp);
	    if (err)
		return 1;
	}

	len = next_token(f, token, LINE_MAX + 1, &newline);
    }

    return 0;
}


int read_depfile(FILE * f, struct trie *nodes, unsigned int *nnodesp)
{
    *nnodesp = 0;
    return _read_depfile(f, nodes, nnodesp);
}
