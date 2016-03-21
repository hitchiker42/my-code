//#include "btreee.h"
//The size of the tree is limited to (2**32)-1 pages
struct bptree_record {
  uint32_t offset;
  uint32_t size;
  void *data;
};
struct bptree_node {
  uint32_t n_keys;
  uint32_t page_no;
  uint32_t parent;
  uint32_t is_leaf;//can shrink this to store more metadata if needed
//We still have 64 bit keys
  uint64_t keys[(2*bptree_min_degree)-1];
  union {
    //may need to treat these as integers not pointers
    uint32_t children[2*bptree_min_degree];//internal node
    struct {//leaf node
      void *values[(2*bptree_min_degree)-1];
      bptree_node *next;//pointer to next leaf node
    };
  };
};
/*
  b+tree, designed to be stored in a file.
*/
struct bptree {
  bptree_node *root;
  void *mem_base;
  void *data_base;
  char filename[256];
  /*
    n_nodes is the number of nodes currently in the tree, n_pages
    is the number of pages available to use as nodes.
  */
  uint32_t n_nodes;
  uint32_t n_pages;
  uint32_t data_size;//Data is addressed semi-normally
  /*
    There are a few reasonable choices for pagesize, either the size
    used by the virtual memory system (usually 4K), the size used
    by the disk, usually 512B or 4K for newer harddrives. Or just use
    some power of two (2**16 is a good one). This could be smaller and
    be read as power of 2.
  */
  uint32_t pagesize;
  /*
    These is are malloc'd and free pages are consolidated before
    the tree is written to disk. Should probably limit the number
    of free pages to a certain ammount.
  */
  uint32_t *free_pages;
  uint32_t n_free_pages;

};
int bptree_write_to_disk(bptree *tree);
bptree *bptree_read_from_disk(int fd);
static inline void* compute_page_address(bptree *tree, size_t page){
  size_t offset = page * tree->pagesize;
  return tree->mem_base + offset;
}
static inline uint32_t allocate_node(bptree *tree){
  if(tree->n_free_pages > 0){
    return tree->free_pages[--tree->n_free_pages];
  } else {
    if(tree->n_pages_used >= tree->n_pages){
      //allocate some more pages;
    }
