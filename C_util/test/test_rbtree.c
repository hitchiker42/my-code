#include "C_util.h"
#include "rbtree.h"
/*
  A lot of the basic code comes from the linux tree linux/lib/rbtree_test.c
*/
/*
  Testing entails filling a tree with N random 64 bit integers, and then
  validating all the rb tree properties.
*/
static int is_red(rb_node *node){
  return !(node->parent_color & 1);
}
static int black_path_count(rb_node *node){
  int count;
  for(count=0; node; node = rb_parent(node)){
    count += !(is_red(node));
  }
  return count;
}
static void check_postorder_recurse(rb_node *node, int *count){
  (*count)++;
}
static int check_postorder(rb_tree *tree, int num_nodes){
  int count = 0;
  rb_traverse_postorder(tree, (void*)&count, (void*)check_postorder_recurse);
  WARN(num_nodes != count, "count = %d, num_nodes = %d\n", count, num_nodes);
  return count;
}
static void rb_check(rb_tree *tree, int num_nodes){
  rb_node *node;
  int count = 0, blacks = 0;
  int64_t prev_key = INT64_MIN;
  for(node = rb_first(tree);node;node = rb_next(node)){
    WARN_ONCE(((int64_t)node->data) < prev_key, 
              "key = %ld, prev = %ld\n", (int64_t)node->data, prev_key);
    WARN_ONCE(is_red(node) && (!rb_parent(node) || is_red(rb_parent(node))),
              "count = %d\n", count);
    if(!count){
      blacks = black_path_count(node);
    } else {
      WARN_ONCE((!node->left || !node->right) &&
                blacks != black_path_count(node), 
                "blacks = %d, path count = %d\n", blacks, black_path_count(node));
    }
    prev_key = (int64_t)node->data;
    count++;
  }
  //(count == num_nodes) || raise(SIGTRAP);
  WARN_ONCE(count != num_nodes,
            "count = %d, num_nodes = %d\n", count, num_nodes);
  WARN_ON_ONCE(count < (1 << black_path_count(rb_last(tree)))-1);
}
#define PERF_LOOPS 100
static void rbtree_test(int num_nodes){
  srand48(0);
  int i,j;
  rb_tree *tree = make_empty_rbtree(NULL);
  volatile int64_t nodes[num_nodes];
  double time1, time2, time3;
  for(i=0;i<num_nodes;i++){
    nodes[i] = lrand48();
  }
  DEBUG_PRINTF("Checking rbtree\n");
  for(i=0;i<num_nodes;i++){
    rb_check(tree, i);
    rb_insert(tree, (void*)nodes[i]);
    if(!rb_lookup(tree, (void*)nodes[i])){
      raise(SIGTRAP);
    }
  }
  check_postorder(tree, num_nodes);
  DEBUG_PRINTF("Deleteing nodes\n");
  for(i=(num_nodes-1);i>=0;i--){
    rb_check(tree, i+1);
    if(!rb_lookup(tree, (void*)nodes[i])){
      raise(SIGTRAP);
    }
    rb_remove(tree, (void*)nodes[i]);
  }
  check_postorder(tree, 0);
  DEBUG_PRINTF("Testing rbtree performance\n");
  time1 = float_time();
  for(i=0;i<PERF_LOOPS;i++){
    for(j=0;j<num_nodes;j++){
      rb_insert(tree, (void*)nodes[j]);
    }
    for(j=0;j<num_nodes;j++){
      rb_remove(tree, (void*)nodes[j]);
    }
  }
  time2 = float_time();
  printf("Time to insert and delete %d nodes %d times: %f\n",
         num_nodes, PERF_LOOPS, time2-time1);
//  free(nodes);
  return;
}
int main(int argc, char *argv[]){
//  enable_backtraces();
  rbtree_test(10000);
}
