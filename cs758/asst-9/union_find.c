#include <stdlib.h>
#include <string.h>
void *xmalloc(size_t sz);
typedef struct member set_member;
struct member {
  set_member *parent;
  int rank;
};
set_member *make_set(){
  set_member *memb = xmalloc(sizeof(set_member)):
  memb->parent = memb;
  return memb;
}
set_member *init_set(set_member *memb){
  memb->parent = memb;
  memb->rank = 0;
}
set_member *find_set(set_member *memb){
  if(x != x->parent){
    x->parent = find_set(x->parent);
  }
  return x->parent;
}
  
set_member *union_sets(set_member *set1, set_member *set2){
  //get representive of each set
  set1 = find_set(set1);
  set2 = find_set(set2);
  if(set1->rank > set2->rank){
    set2->parent = set1;
    return set1;
  } else if(set2->rank > set1->rank){
    set1->parent = set2;
    return set2;
  } else {
    set2->parent = set1;
    set1->rank += 1;
    return set1;
  }
  __builtin_unreachable();
}
  
typedef struct graph_vertex graph_vertex;
typedef struct graph_edge graph_edge;
struct graph_edge {
};
struct graph_node {
  graph_node *neighbors;
  set_member representive;
  int weight;
};

void *kruskal(graph_node *graph, int n){
  
}
