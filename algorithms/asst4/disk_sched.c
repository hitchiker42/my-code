/**
 * \file disk_sched.c
 *
 * A simple disk scheduler.
 *
 * \author eaburns
 * \date 09-08-2010
 */

#include "C_util.h"
#include "rbtree.h"

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/time.h>

#include "disk_loc.h"
#include "list.h"
#include "bst.h"

#if !defined(LINE_MAX)
#if !defined(_POSIX2_LINE_MAX)
#define LINE_MAX 4096 		/* should be large enough. */
#else
#define LINE_MAX _POSIX2_LINE_MAX
#endif	/* !_POSIX2_LINE_MAX */
#endif	/* !LINE_MAX */

/*
 * Reads at most 'len - 1' bytes of the next token from the input file
 * into the buffer as a string with a null terminator on the end.
 *
 * Returns 0 on success and 1 on failure.  This function fails if a
 * space character of EOF is not read in the next 'len - 1' bytes.
 */
static int next_token(FILE *infile, char buf[], unsigned int len)
{
	unsigned int i;
	char c;

	for (i = 0; i < len - 1; i += 1) {
		c = getc(infile);
		if (c == EOF || isspace(c))
			break;
		buf[i] = c;
	}
	buf[i] = '\0';

	if (c == EOF && i == 0)
		return EOF;

	if (!isspace(c)) {
		fprintf(stderr, "Buffer is too small\n");
		return 1;
	}

	return 0;
}

/* Gets the time of day in seconds. */
static double get_current_seconds(void)
{
    double sec, usec;
    struct timeval tv;

    if (gettimeofday(&tv, NULL) < 0) {
	perror("gettimeofday failed");
	exit(EXIT_FAILURE);
    }

    sec = tv.tv_sec;
    usec = tv.tv_usec;

    return sec + (usec / 1000000);
}


/************************************************************
 * A simple interface for adding, removing and scanning requests on
 * the disk.
 ************************************************************/

/*
 * Adds a request to the given structure.
 *
 * The first paramater is a pointer to the structure cast into a void*
 * and the second parameter is the location to add.  This function
 * should return 0 on success and 1 on error.
 */
typedef int (*add_request_t)(void *, struct disk_location *);

/*
 * Removes a request from the given structure.
 *
 * The first paramater is a pointer to the structure cast into a void*
 * and the second parameter is the location to delete.  This function
 * should return 0 on success and 1 on error.
 */
typedef int (*del_request_t)(void *, struct disk_location *);

/*
 * Scans the structure in the given direction and fills in the array
 * with a given number of elements.
 *
 * The first parameter is a pointer to the structure cast into a
 * void*, the second is the scan direction, the third is the starting
 * location, the fourth is the array to fill with the scanned
 * locations and the final is the number of locations to fill in in
 * the 4th parameter array.  This function should return the number of
 * elements scanned on success or a negative number on error.
 */
typedef int (*scan_t)(void *, enum direction, struct disk_location *,
		      struct disk_location *[], unsigned int);


/* Handles an incoming request by adding it to the data structure. */
static int handle_request(FILE *infile, FILE *outfile, add_request_t add_req,
			  void *data)
{
	int err, ret;
	struct disk_location loc;

	ret = read_location(infile, &loc);
	if (ret == EOF) {
		fprintf(stderr, "Unexpected end of file\n");
		return 1;
	} else if (ret) {
		return 1;
	}

#if !defined(NDEBUG)
	printf("Adding request ");
	output_location(stdout, &loc);
	printf("\n");
#endif	/* !NDEBUG */

	err = add_req(data, &loc);
	if (err) {
		fprintf(outfile, "Failed to cancel request ");
		output_location(outfile, &loc);
		fprintf(outfile, "\n");
		return 1;
	}

	return 0;
}

/* Handles the cancelation of a request. */
static int handle_cancel(FILE *infile, FILE *outfile, del_request_t del_req,
			 void *data)
{
	int err, ret;
	struct disk_location loc;

	ret = read_location(infile, &loc);
	if (ret == EOF) {
		fprintf(stderr, "Unexpected end of file\n");
		return 1;
	} else if (ret) {
		return 1;
	}

#if !defined(NDEBUG)
	printf("Deleting request ");
	output_location(stdout, &loc);
	printf("\n");
#endif	/* !NDEBUG */

	if (!del_req) {
		fprintf(stderr, "WARNENG: Request removal is not supported\n");
		return 0;
	}

	err = del_req(data, &loc);
	if (err) {
		fprintf(outfile, "Failed to cancel request ");
		output_location(outfile, &loc);
		fprintf(outfile, "\n");
		return 1;
	}

	return 0;
}

/* Allocates a scan array, performs the scan and outputs the scan. */
static int do_scan(FILE *outfile, void *data, scan_t scan, enum direction dir,
		   unsigned int num, struct disk_location *loc)
{
	int nscanned;
	struct disk_location **locs;

	locs = malloc(num * sizeof(*locs));
	if (!locs) {
		perror("malloc failed");
		return 1;
	}

	nscanned = scan(data, dir, loc, locs, num);
	if (nscanned >= 0) {
		int i;
		for (i = 0; i < nscanned; i += 1) {
			output_location(outfile, locs[i]);
			fprintf(outfile, "\n");
		}
	}

	free(locs);

	return nscanned < 0;
}

/* Outputs the information about the scan. */
static void output_scan(FILE *outfile, enum direction dir, unsigned int num,
			struct disk_location *loc)
{
	fprintf(outfile, "scanning %s by %d from ",
		(dir == DOWN ? "down" : "up"), num);
	output_location(outfile, loc);
	fprintf(outfile, "\n");
}


/* Handles a scan. */
static int handle_scan(FILE *infile, FILE *outfile, scan_t scan, void *data)
{
	int ret, err, num;
	char buf[LINE_MAX + 1];
	enum direction dir = UP;
	struct disk_location loc;

	ret = next_token(infile, buf, LINE_MAX + 1);
	if (ret == EOF) {
		fprintf(stderr, "Unexpected end of file\n");
		return 1;
	} else if (ret != 0) {
		return 1;
	}

	if (strncmp("down", buf, LINE_MAX) == 0) {
		dir = DOWN;
	} else if (strncmp("up", buf, LINE_MAX) != 0) {
		fprintf(stderr, "Unknown direction [%s]\n", buf);
		return 1;
	}

	ret = fscanf(infile, " %d", &num);
	if (ret == EOF) {
		fprintf(stderr, "Unexpected end of file\n");
		return 1;
	} else if (ret != 1) {
		fprintf(stderr, "Failed to read length of scan\n");
		return 1;
	}

	ret = read_location(infile, &loc);
	if (ret == EOF) {
		fprintf(stderr, "Unexpected end of file\n");
		return 1;
	} else if (ret) {
		return 1;
	}

	output_scan(outfile, dir, num, &loc);

	err = do_scan(outfile, data, scan, dir, num, &loc);
	if (err) {
		fprintf(stderr, "Scan failed\n");
		return 1;
	}

	return 0;
}

/*
 * Schedule the I/O requests using a sorted linked list.
 *
 * Return 0 on success and 1 on failure.
 */
static int schedule(FILE *infile, FILE *outfile, add_request_t add_req,
		    del_request_t del_req, scan_t scan, void *data)
{
	int ret, err;
	char buf[LINE_MAX + 1];

	ret = next_token(infile, buf, LINE_MAX + 1);
	while (ret == 0) {
		if (strncmp("request", buf, LINE_MAX) == 0) {
			err = handle_request(infile, outfile, add_req, data);
			if (err)
				return 1;
		} else if (strncmp("cancel", buf, LINE_MAX) == 0) {
			err = handle_cancel(infile, outfile, del_req, data);
			if (err)
				return 1;
		} else if (strncmp("scan", buf, LINE_MAX) == 0) {
			err = handle_scan(infile, outfile, scan, data);
			if (err)
				return 1;
		} else if (strncmp("time", buf, LINE_MAX) == 0) {
			fprintf(outfile, "time: %f seconds\n",
                                get_current_seconds());
		} else if (buf[0] != '\0') {
			fprintf(stderr, "Unknown command [%s]\n", buf);
			return 1;
		}
		ret = next_token(infile, buf, LINE_MAX + 1);
	}

	return 0;
}


/************************************************************
 * Interface for the linked list.
 ************************************************************/

/*
 * Adds a request to the list.  This adhers to the add_request_t type.
 */
static int list_add_request(void *data, struct disk_location *loc)
{
	return insert_list_location((struct list_node **)data, loc);
}


/*
 * Deletes a request from the list.  This adhers to the del_request_t
 * type.
 */
static int list_del_request(void *data, struct disk_location *loc)
{
	int removed = remove_list_location((struct list_node **) data, loc);

	return !removed;
}


/*
 * Scans the list for a set of locations.  This adhers to the scan_t
 * type.
 */
static int list_scan(void *data, enum direction dir, struct disk_location *loc,
		     struct disk_location *locs[], unsigned int n)
{
	int left = -1;
	struct list_node **head = data;

	assert(head);

	switch (dir) {
	case UP:
		left = list_n_after(*head, loc, locs, n, n);
		break;
	case DOWN:
		left = list_n_before(*head, loc, locs, n);
		break;
	}

	if (left < 0)
		return left;

	return n - left;
}


/*
 * The scheduler function that uses linked lists.
 */
static int schedule_list(FILE *infile, FILE *outfile)
{
	int err;
	struct list_node *head = NULL;

	err = schedule(infile, outfile, list_add_request,
		       list_del_request, list_scan, (void*) &head);

	free_list(head);
	return err;
}

/************************************************************
 * Interface for the binary search tree.
 ************************************************************/


static int bst_add_request(void *data, struct disk_location *loc)
{
	return insert_tree_location((struct tree_node **) data, loc);
}

static int bst_del_request(void *data, struct disk_location *loc)
{
	int removed;

	removed = remove_tree_location((struct tree_node **) data, loc);

	return !removed;
}

static int bst_scan(void *data, enum direction dir, struct disk_location *loc,
		    struct disk_location *locs[], unsigned int n)
{
	int fill = -1;
	struct tree_node **root = data;

	assert(root);

	switch (dir) {
	case UP:
		fill = tree_n_after(*root, loc, locs, n, 0);
		break;
	case DOWN:
		fill = tree_n_before(*root, loc, locs, n, 0);
		break;
	}

	return fill;
}

/*
 * The scheduler function that uses a binary search tree.
 */
static int schedule_bst(FILE *infile, FILE *outfile)
{
	int err;
	struct tree_node *root = NULL;

	err = schedule(infile, outfile, bst_add_request,
		       bst_del_request, bst_scan, (void*) &root);

	free_tree(root);
	return err;
}

/************************************************************
 * The following functions conform to the interface for add_request_t,
 * del_request_t and scan_t (see above for comments).  You should
 * implement these functions using a red/black tree.
 *
 * There are two samples above using a linked list and using an
 * unbalanced binary tree.
 ************************************************************/

static int rbtree_add_request(void *data, struct disk_location *loc){
  struct disk_location *loc_copy = xmalloc(sizeof(struct disk_location));
  memcpy(loc_copy, loc, sizeof(struct disk_location));
  rb_insert(data, loc_copy);
  //rb_insert(data, loc);
  return 0;
}

static int rbtree_del_request(void *data, struct disk_location *loc){
  rb_node *node = rb_lookup(data, loc);
  if(!node){
    return 1;
  } else { 
    rb_delete_custom(data, node, free);
    return 0;
  }
}

void print_location(struct disk_location *loc, FILE *out){
  output_location(out, loc);
}
static int rbscan_recur(rb_tree *tree, rb_node *node,
                        struct disk_location *loc,
                        struct disk_location **locs,
                        uint n, uint fill, int dir){
  if(!node){
    return fill;
  }
  int c = tree->cmp(node->data, loc);
  if(dir == UP){
    if(c < 0){
      return rbscan_recur(tree, node->right, loc, locs, n, fill, dir);
    } else {
      int f = rbscan_recur(tree, node->left, loc, locs, n, fill, dir);
      if(f<n){
        locs[f++] = node->data;
        f = rbscan_recur(tree, node->right, loc, locs, n, f, dir);
      }
      return f;
    }
  } else {
    if(c > 0){
      return rbscan_recur(tree, node->left, loc, locs, n, fill, dir);
    } else {
      int f = rbscan_recur(tree, node->right, loc, locs, n, fill, dir);
      if(f<n){
        locs[f++] = node->data;
        f = rbscan_recur(tree, node->left, loc, locs, n, f, dir);
      }
      return f;
    }
  }
  return fill;
}
        
static int rbtree_scan(void *data, enum direction dir,
		       struct disk_location *loc,
		       struct disk_location *locs[], unsigned int n){
  rb_tree *tree = data;
  return rbscan_recur(tree, tree->root, loc, locs, n, 0, dir);
}

/*
 * The scheduler function that uses a binary search tree.
 */
static int schedule_rbtree(FILE *infile, FILE *outfile)
{
	int err;

	/*
	 * Undergraduates can set this to NULL because you do not need
	 * to implement request cancellation, however, graduate
	 * students should delete the following line once the
	 * rbtree_del_request() function has been implemented.
	 */
        rb_tree *tree = make_empty_rbtree((cmp_fun)compare_locations);
	err = schedule(infile, outfile, rbtree_add_request,
		       rbtree_del_request,
		       rbtree_scan, tree);
        
//        destroy_rbtree(tree);//???
	return err;
}

/************************************************************
 * The main function and friends.
 ************************************************************/


static void usage(void)
{
	fprintf(stderr, "Usage:\n"
		"disk-sched <data structure> <infile> <outfile>\n");
	exit(EXIT_FAILURE);
}

int main(int argc, const char * argv[])
{
	int err;
	FILE *infile = stdin;
	FILE *outfile = stdout;

	if (argc != 4)
		usage ();

	if (strcmp(argv[2], "-") != 0) {
		infile = fopen(argv[2], "r");
		if (!infile) {
			perror("failed to open input file");
			err = 1;
			goto out;
		}
	}
	if (strcmp(argv[3], "-") != 0) {
		outfile = fopen(argv[3], "w");
		if (!outfile) {
			perror("failed to open output file");
			err = 1;
			goto out;
		}
	}

	if (strcmp("list", argv[1]) == 0) {
		err = schedule_list(infile, outfile);
	} else if (strcmp("bst", argv[1]) == 0) {
		err = schedule_bst(infile, outfile);
	} else if (strcmp("rbtree", argv[1]) == 0) {
		err = schedule_rbtree(infile, outfile);
	} else {
		fprintf(stderr, "Unknown data structure: %s\n", argv[1]);
		err = 1;
	}

out:
	if (outfile != stdout)
		fclose(outfile);
	if (infile != stdin)
		fclose(infile);

	if (err)
		return EXIT_FAILURE;

	return EXIT_SUCCESS;
}
