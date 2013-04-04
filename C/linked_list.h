#include <stdlib.h>
#include <stdbool.h>
typedef struct dbl_node dbl_node;
struct dbl_node {
  dbl_node* back;
  void* val;
  dbl_node* next;
};
typedef struct node node;
struct node {
  void* val;
  struct node* next;
};
typedef node* list;
list cons(node* head,node* tail);
int length(list l);
node* cdr(node* cell);
void* car(node* cell);
