#include "C_util.h"
/*
  This is a template for testing a generic map data structure (i.e one
  that maps a set of keys to a set of values), this can be anything
  from an alist, to a hashtable, to a binary tree.
*/

//function called after every insertion/deletion used to insure
//the data structure is still correct
typedef int(*check_structure)(void *structure);
