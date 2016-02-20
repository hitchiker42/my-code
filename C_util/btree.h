#ifndef __BTREE_H__
#define __BTREE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "C_util.h"
/*
  B and B+ tree implementations, written mostly as a learning experience.

  B trees stores keys as 64 bit integers (These could be for example, hashes
  of the data / a string key). and data as void*'s.

  B+ trees store keys as 64 bit hashes and values in leaves.
*/
typedef struct btree btree;
typedef struct bptree bptree;
typedef struct btree_node btree_node;
typedef struct bptree_node bptree_node;
/*
  A node holds at most (2*min_degree)-1 keys/values and 2*min_degree children
  We require space for 2*min_degree keys and  4*min_degree pointers,
  This gives us sizeof(key)+sizeof(value) space to store metadata.
  For 4K pages, 64 bit keys and 64 bit pointers this gives a min_degree of 48
*/
#define btree_parent(node) ((btree_node*)((node->parent_leaf) & ~1))
#define bptree_parent(node) ((bptree_node*)((node->parent_leaf) & ~1))
#define is_leaf(node) (node->parent_leaf & 1)
#define set_leaf(node) (node->parent_leaf |= 1)
#define unset_leaf(node) (node->parent_leaf &= ~1)
//Change this to change the size of the btree nodes
#define btree_min_degree  (PAGE_SIZE/48)
/*
  OPTIMIZATIONS:
  -Use a seperate type for internal nodes and leaves, to allow leaves to 
    store more key/values, since they don't need pointers to children.
  -Use an index to speed up lookups
*/
/*
  Features:
    Allow saving the btree as a file. This means all nodes of the tree
    need to be stored on a contigious set of pages, obtained either from
    reading a prexisting btree from a file, or allocating pages via mmap.
*/
struct btree_node {
  uint64_t n_keys;
  uint64_t parent_leaf;//parent pointer + leaf flag
//Interleaving these might change the performance (I'm not sure how though)
  uint64_t keys[(2*btree_min_degree)-1];
  void *values[(2*btree_min_degree)-1];
  btree_node *children[(2*btree_min_degree)];
};
//The btree struct is the root node + some metadata (currently there isn't
//actually any metadata)
struct btree {
  btree_node *root;
};

#define bptree_min_degree (PAGE_SIZE/32)
struct bptree_node {
  uint64_t n_keys;
  uint64_t parent_leaf;
  uint64_t keys[(2*bptree_min_degree)-1];
  union {
    bptree_node *children[2*bptree_min_degree];//internal node
    struct {//leaf node
      void *values[(2*bptree_min_degree)-1];
      bptree_node *next;//pointer to next leaf node
    };
  };
};
struct bptree {
  bptree_node *root;
};
/*
  For now I'm only doing insertion, lookup and deletion.
*/
//uint128_t btree_hash(uint8_t *key, size_t key_sz);
btree* make_btree(void);
void btree_insert(btree *tree, uint64_t key, void *value);
void *btree_lookup(btree *tree, uint64_t key);
void *btree_delete(btree *tree, uint64_t key);

/*
void *bptree_find_add_hv(bptree *tree, uint64_t hv, void *value);
static void *bptree_find_add(bptree *tree, uint8_t *key, 
                            size_t key_sz, void *value){
  uint64_t hv = bptree_hash(key, key_sz);
  return bptree_find_add_hv(tree, hv, value);
}

void *bptree_lookup_hv(bptree *tree, uint64_t hv);
static void *bptree_lookup(bptree *tree, uint8_t *key, size_t key_sz){
  uint64_t hv = bptree_hash(key, key_sz);
  return bptree_lookup_hv(tree, hv);
}

void *bptree_delete_hv(bptree *tree, uint64_t hv);
void *bptree_delete(bptree *tree, uint8_t *key, size_t key_sz){
  uint64_t hv = bptree_hash(key, key_sz);
  return bptree_delete_hv(tree, hv);
}
*/
#ifdef __cplusplus
}
#endif
#endif /* __BTREE_H__ */
