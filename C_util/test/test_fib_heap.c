#include "C_util.h"
#include "fib_heap.h"
//#include "util_string.h"
//#include "svector.h"

//Iterate over the list begining at start, setting the current node
//to 'var' each iteration, the count variable is used to insure we do
//one iteration in the where case we have only one node
#define FOR_EACH_NODE(var, start)                       \
  int CAT(count, __LINE__) = 0;                         \
  for(fib_node *var = start;                            \
      var != start || CAT(count, __LINE__) <= 0; \
      CAT(count, __LINE__)++, var = var->right)

GEN_ENUM(heap_error,
         ERR_NONE,
         ERR_HEAP,
         ERR_MIN,
         ERR_NUM_NODES,
         ERR_NUM_ROOTS,
         ERR_DEGREE,
         ERR_FIBONACCI);
long fibonacci(int n){
  static const double sqrt_5 = sqrt(5);
  static const double phi = (1 + sqrt_5)/2;
  double fib = floor((pow(phi, n)/sqrt(5)) + 0.5);  
  return (long)fib;
}
int verify_heap_rec(fib_heap *heap, fib_node *node, int *count){
  *count = *count + 1;
  //this is really unlikely so I'm just using assert
  assert(node->degree <= LOG_2_CEIL(heap->num_nodes));
  if(!node->children){
    return ERR_NONE;
  } else {
    int i = 0;
    fib_node *children = node->children;
    FOR_EACH_NODE(child, children){
      if(!(heap->cmp(node->data, child->data))){
        HERE_FMT("Heap property violated, child %p of node %p "
             "is smaller than its parent\n", child, node);
        return ERR_HEAP;
      }
      /*      if(!(child->degree >= MAX(0, i-2))){
        HERE_FMT("Fibonacci heap property violated: "
                 "child %d of node %p has degree %d, expected %d or greater\n",
                 i, node, child->degree, MAX(0, i-2));
        return ERR_FIBONACCI;
        }*/
      i++;
      int err = verify_heap_rec(heap, child, count);
      if(err){return err;}        
    }
    if(node->degree != i){
      HERE_FMT("Error, node %p has degree %d, but has %d children\n",
           node, node->degree, i);
      return ERR_DEGREE;
    }
    return ERR_NONE;
  }
}
int verify_heap(fib_heap *heap){
  if(heap->min == NULL){
    return 0;
  }
  fib_node *min = heap->min;
  int num_roots = 0;
  int count = 0;
  FOR_EACH_NODE(root, min){
    num_roots++;
    if(root != min && !(heap->cmp(min->data, root->data))){
      HERE_FMT("Node %p (val = %f) is smaller than heap->min (%p, val = %f)\n",
               root, BITCAST(root->data, double), min, BITCAST(min->data, double));
      return ERR_MIN;
    }
    assert(root->parent_mark <= 1);
    int err = verify_heap_rec(heap, root, &count);
    if(err){
      return err;
    }
  }
  if(num_roots != heap->num_roots){
    HERE_FMT("Expected %ld roots in heap, found %d\n", heap->num_roots, num_roots);
    return ERR_NUM_ROOTS;
  }
  if(count != heap->num_nodes){
    HERE_FMT("Expected %ld nodes in heap, found %d\n", heap->num_nodes, count);
    return ERR_NUM_NODES;
  }
  return ERR_NONE;
}
int verify_heap_post_delete(fib_heap *heap){
  int max_degree = LOG_2(heap->num_nodes);
  uint8_t have_degree[max_degree];
  memset(have_degree, '\0', max_degree);
  FOR_EACH_NODE(x, heap->min){
    assert(x->degree <= max_degree);
    if(have_degree[x->degree]){
      HERE_FMT("Error, more than one node of degree %d following a heap pop\n",
           x->degree);
      return ERR_DEGREE;
    }
    have_degree[x->degree] = 1;
  }
  int err = verify_heap(heap);
  return err;
}

int cmp_doubles(void *x, void *y){
  return (BITCAST(x, double) < BITCAST(y, double));
}
//DOESN'T test deletion or merging
#define NUM_NODES 30000
int test_heap(fib_heap *heap){
  printf("Building fibonacci heap with %d nodes\n", NUM_NODES);
  fib_node *nodes[NUM_NODES];
  fib_node *min = NULL;
  int min_index = 0;
  double min_val = 100;
  int i, err;
  srand48(nano_time());
  for(i=0;i<NUM_NODES;i++){
    //    DEBUG_PRINTF("%d\t", i);
    //every 10 nodes decrease a key
    if(i && !(i % 10)){
      int idx = i * drand48();
      fib_node *node = nodes[idx];
      double key = BITCAST(node->data, double);
      if((key/2) < min_val){
        min_val = key/2;
        min = node;
        min_index = idx;
      }
      fib_heap_decrease_key(heap, node, BITCAST(key/2, void*));
      err = verify_heap(heap);
      if(err){
        DEBUG_PRINTF("Error decreasing key on node %d on iteration %d, with error %s\n",
                     idx, i, heap_error_to_string(err));
        return err;
      }
    }
    //every 20 nodes pop the heap
    if(i && !(i % 20)){
      assert(min == heap->min);
      double actual_min = BITCAST(fib_heap_pop(heap), double);
      if(actual_min != min_val){
        DEBUG_PRINTF("Smallest value inserted into the heap was %f,\n"
                     "but value popped was %f\n", min_val, actual_min);
        return ERR_MIN;
      }


      err = verify_heap(heap);
      if(err){
        DEBUG_PRINTF("Failure popping node %d, on iteration %d with error %s\n",
                     min_index, i, heap_error_to_string(err));
        return err;
      }

      nodes[min_index] = fib_heap_insert(heap, BITCAST(min_val/2, void*));
      min_val /= 2;
      err = verify_heap(heap);
      if(err || nodes[min_index] != heap->min){
        DEBUG_PRINTF("Error adding new min node after deletion on iteration %d,\n"
                     "with error %s\n", i, heap_error_to_string(err));
        return err;
      }
    }
    double val = drand48()/drand48();
    fib_node *node = fib_heap_insert(heap, BITCAST(val, void*));
    //    DEBUG_PRINTF("Heap has %ld nodes and %ld roots on iteration %d, just added %f\n",
                 //                 heap->num_nodes, heap->num_roots, i, val);
    err = verify_heap(heap);
    if(err){
      DEBUG_PRINTF("Error adding new node on iteration %d, with error %s\n",
                   i, heap_error_to_string(err));
      return err;
    }
    nodes[i] = node;
    if(val < min_val){
      min_val = val;
      min = node;
      min_index = i;
    }
  }
  printf("Finished building heap, now emptying it\n");
  i = 0;
  //empty the heap
  while(heap->min != NULL){
    double val = BITCAST(fib_heap_pop(heap), double);
    assert(val >= min_val);
    min_val = val;
    err = verify_heap(heap);
    if(err){
      DEBUG_PRINTF("Error popping node iteration %d, with error %s\n",
                   i, heap_error_to_string(err));
      return err;
    }
    i++;
  }
  assert(heap->num_nodes == 0);
  //fib_heap_free(heap);
  return 0;
}
int main(int argc, char *argv[]){
  //all of the values we add are > 0
  fib_heap *heap = make_fib_heap(cmp_doubles, BITCAST(-1.0, void*));
  int err = test_heap(heap);
  return err;
}
