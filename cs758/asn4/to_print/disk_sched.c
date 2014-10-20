/*
  Code that was added to disk_sched.c
*/
/************************************************************
 * The following functions conform to the interface for add_request_t,
 * del_request_t and scan_t (see above for comments).  You should
 * implement these functions using a red/black tree.
 *
 * There are two samples above using a linked list and using an
 * unbalanced binary tree.
 ************************************************************/

static int __rbtree_add_request(rb_node **data, struct disk_location *loc){
  return rb_add(data,loc);
}
static int rbtree_add_request(void *data, struct disk_location *loc){
  return __rbtree_add_request(data, loc);
}
static int __rbtree_del_request(rb_node **data, struct disk_location *loc){
  if(*data == NULL){
    return 1;
  } else {
    return rb_del(*data, loc);
  }
    
}
static int rbtree_del_request(void *data, struct disk_location *loc){
  return __rbtree_del_request(data, loc);
}

static int __rbtree_scan(rb_node **data, enum direction dir,
		       struct disk_location *loc,
		       struct disk_location **locs, unsigned int n){
  if(*data == NULL){
    return -1;
  } else {
    return rb_scan(data, dir, loc, locs, n);
  }
}
static int rbtree_scan(void *data, enum direction dir,
		       struct disk_location *loc,
		       struct disk_location *locs[], unsigned int n){
  return __rbtree_scan(data,dir,loc,locs,n);
}

/*
 * The scheduler function that uses a binary search tree.
 */
static int schedule_rbtree(FILE *infile, FILE *outfile)
{
	int err;
	del_request_t _rbtree_del_request = rbtree_del_request;;
        rb_node **root = alloca(sizeof(rb_node*));
        *root = NULL;
	/*
	 * Undergraduates can set this to NULL because you do not need
	 * to implement request cancellation, however, graduate
	 * students should delete the following line once the
	 * rbtree_del_request() function has been implemented.
	 */
	_rbtree_del_request = NULL;

	err = schedule(infile, outfile, rbtree_add_request,
		       _rbtree_del_request, rbtree_scan, root);
        rb_cleanup(root);
	return err;
}

