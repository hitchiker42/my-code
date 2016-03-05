/**
 * \file align_io.c
 *
 *
 *
 * \author jtd7
 * \date 03-10-2011
 */

#include "align_io.h"

#if !defined(LINE_MAX)
#if !defined(_POSIX2_LINE_MAX)
#define LINE_MAX 4096		/* should be large enough. */
#else
#define LINE_MAX _POSIX2_LINE_MAX
#endif				/* !_POSIX2_LINE_MAX */
#endif				/* !LINE_MAX */

/* A list of 'tokens' in a file. */
struct list_node {
    char val;
    struct list_node *next;
};

/* Frees a list of tokens. */
static void free_token_list(struct list_node *n){
    while (n) {
	struct list_node *p = n;
	n = n->next;
	free(p);
    }
}


/*
 * Reads the tokens from a file into a linked list.  Returns the size
 * of the list via the 'num' parameter.  The caller is responsable for
 * freeing the list.
 *
 * Return NULL on failure;
 */
static struct list_node *read_token_list(FILE *f,
				  unsigned int *num){
    int _num;
    char c = '\0';
    struct list_node *head = NULL;

    _num = 0;
    while ((c = getc(f)) != EOF) {
	    if (c != '\n' && c != '\t' && c != ' '){
		    struct list_node *n;
		    n = malloc(sizeof(*n));
		    if (!n) {
			    perror("malloc failed");
			    free_token_list(head);
			    return NULL;
		    }
		    n->val = c;
		    _num += 1;
		    n->next = head;
		    head = n;
	    }
    }
    *num = _num;
    return head;
}


/*
 * Reads the tokens from the file into an array which is returned via
 * the 'ary' parameter.  The caller is responsable for freeing the
 * array.
 *
 * Returns 0 on success and 1 on failure.
 */
static int read_token_array(FILE * f, char **ary, unsigned int *n){
    struct list_node *head, *p;
    unsigned int i;
    char *a;

    head = read_token_list(f, n);
    if (!head)
	return 1;

    a = malloc(sizeof(*a) * *n);
    if (!a) {
	perror("malloc failed");
	free_token_list(head);
	return 1;
    }

    i = *n - 1;
    p = head;
    while (p) {
	a[i] = p->val;
	p = p->next;
	i -= 1;
    }

    *ary = a;
    free_token_list(head);
    return 0;
}



/*
 * Loads an array of the tokens from the given file (this actually
 * opens and closes the file).  The caller is responsable for freeing
 * the array.
 *
 * Return 0 on success and 1 on error.
 */
static int load_token_array(char *fname, char **ary, unsigned int *n){
    int err;
    FILE *f;

    f = fopen(fname, "r");
    if (!f) {
	perror(fname);
	return 1;
    }

    err = read_token_array(f, ary, n);

    fclose(f);

    return err;
}


int load_token_arrays(char *f0, char *f1,
			     char **ary0, unsigned int *n0,
			     char **ary1, unsigned int *n1){
    int err;

    err = load_token_array(f0, ary0, n0);
    if (err)
	return 1;

    err = load_token_array(f1, ary1, n1);
    if (err) {
	free(*ary0);
	return 1;
    }

    return 0;
}


void output_alignment(char *str1, char  *str2){
	int index = 0;
	while(*str1 != '\0'){
		while(*str1 != '\0' && index < 75){
			printf("%c", *str1++);
			index++;
		}
		if(index < 75) printf("\n");
		else printf("...\n");
		index = 0;
		while(*str2 != '\0' && index < 75){
			printf("%c", *str2++);
			index++;
		}
		if(index < 75) printf("\n\n");
		else printf("...\n\n");
		index = 0;
	}
}
