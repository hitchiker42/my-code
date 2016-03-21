#ifndef __BPTREE_H__
#define __BPTREE_H__
#ifdef __cplusplus
extern "C" {
#endif
/*
  b+tree, designed to be stored in a file.
*/
struct bptree {
  bptree_node *root;
  void *mem_base;
  char filename[256];
  size_t n_nodes;
  size_t n_pages;
  size_t pagesize;
  //These 2 are malloc'd and free pages are consolidated before
  //the tree is written to disk. Should probably limit the number
  //of free pages to a certain ammount.
  int *free_pages;
  size_t num_free_pages;

};
bptree* make_new_bptree(const char *filename, uint32_t initial_pages,
                        uint32_t pagesize);
void btree_insert(btree *tree, uint64_t key, void *value);
void *btree_lookup(btree *tree, uint64_t key);
void *btree_delete(btree *tree, uint64_t key);

#ifdef __cplusplus
}
#endif
#endif /* __BPTREE_H__ */
