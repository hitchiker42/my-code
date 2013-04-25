#ifndef DBL_LIST_H
#define DBL_LIST_H
typedef struct node node;
struct node {
  double val;
  struct node* next;
};
typedef struct{
  node* car;
  node* cdr;
} list;
node* blank_node();
node* new_node(double val);
double car(node* cell);
node* cdr(node* cell);
node* cons(node* head,node* tail);
#endif
