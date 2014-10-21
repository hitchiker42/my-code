typedef struct tree_node tree_node;
enum {
  left = 0,
  right = 1,
};
struct tree_node {
  tree_node *children[2];
  tree_node *parent;
  void *data;
};
struct binary_tree {
  tree_node *root;
  int size;
  int depth;
  //return 1 if 1st arg is greater than 2nd arg, otherwise 0
  int(*comp)(void*,void*);
};
tree_node *make_leaf(void *data, tree_node *parent){
  tree_node *node = malloc(sizeof(tree_node));
  node->data=data;
  node->parent=parent;
  return node;
}
  
tree_node *follow_route(tree_node *node, int side){
  while(node->children[side]){
    node = node->children[side);
  }
  return node;
}
tree_node *find_parent(void *data, tree_node *curr, tree_node *parent){
  if(!curr){
    return parent;
  } else {
    return find_parent(data, curr->children[comp(data,cur->data)], curr);
  }
}
    
  
tree_node *successor(tree_node *node){
  if(node->children[right]){
    return follow_route(node->children[right],left);
  } else {
    return up(node);
  }
}
tree_node *up(tree_node *node){
  tree_node *p=node->parent;
  if(!p || p->children[left] == x){
    return p;
  } else {
    return up(p);
  }
}
void insert(binary_tree *tree, void *data){
  if(!tree->root){
    tree->root = make_leaf(data, NULL);
  } else {
    tree_node *p = find_parent(data, tree->root, NULL);
    tree_node *new_node = make_leaf(data,p);
    p->children[tree->comp(data,p->data)] = new_node;
  }
}
    
//deletion is complicated
void substitute(tree_node *old, tree_node *new){}
