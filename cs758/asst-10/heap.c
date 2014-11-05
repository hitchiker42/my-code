#include "roadmap.h"
#define heap_left_child(i) (2*i+1)
#define heap_right_child(i) (2*i+2)
#define heap_parent(i) ((i-1)/2)
#define HEAP_SWAP(_heap, _i, _j)                                \
  __extension__ ({ARR_SWAP(_heap->nodes,_i,_j);                        \
  SWAP(_heap->nodes[_i]->heap_index,_heap->nodes[_j]->heap_index);     \
  ;})
  
static void heap_sift_down(struct heap *heap, int root){
  int left,right,swap;
  left = right = swap = 0;  
  while ((left = heap_left_child(root)) < heap->len){
    right = heap_right_child(root);
    swap = root;
    if(heap->nodes[left]->dist < heap->nodes[swap]->dist){
      swap = left;
    }
    if(right < heap->len &&
       (heap->nodes[right]->dist < heap->nodes[swap]->dist)){
      swap = right;
    }
    if(swap == root){
      return;
    } else {
      HEAP_SWAP(heap, root, swap);
      root = swap;
    }
  }
}

void heap_sift_up(struct heap *heap, int index){
  int parent = heap_parent(index);
  //this will terminate at the root since heap_parent(0) = 0
  while(heap->nodes[index]->dist < heap->nodes[parent]->dist){
    HEAP_SWAP(heap, index, parent);
    index = parent;
    parent = heap_parent(index);
  }
}
/*void heap_sift_up_element(struct heap *heap, struct node *element){
  int heap_index = (element - heap->nodes)>>3;//divite by sizeof struct node*
  heap_sift_up(heap, heap_index);
}*/
void heapify(struct heap *heap){
  int i = heap->len/2;
  while (i >= 0) {
    heap_sift_down(heap, i--);
  }
}

void heap_add(struct heap *heap, struct node *edge){
  if(heap->len >= heap->size){
    heap->size *= 2;
    heap->nodes = xrealloc(heap->nodes, heap->size*sizeof(struct node));
  }
  heap->nodes[heap->len] = edge;
  heap_sift_up(heap, heap->len);
  heap->len++;
}
struct node *heap_pop(struct heap *heap){
  if(heap->len < 0){
    return NULL;
  }
  struct node *retval = heap->nodes[0];
  heap->nodes[0] = heap->nodes[--heap->len];
  heap_sift_down(heap, 0);
  return retval;
}
struct node** heapsort_heap(struct heap *heap){
  int i;
  for(i=heap->len-1;i>0;i--){
    HEAP_SWAP(heap,i,0);
    heap_sift_down(heap, 0);
  }
  return heap->nodes;
}
struct heap *build_heap(struct node **nodes, size_t sz){
  struct heap *heap = xmalloc(sizeof(struct heap));
  heap->len = heap->size = sz;
  heap->nodes = nodes;
//  heapify(heap);
  return heap;
}
struct heap *make_heap(size_t initial_size){
  if(initial_size == 0){
    initial_size = 128;
  }
  DEBUG_PRINTF("Creating heap with size %lu\n",initial_size);
  struct heap *heap = xmalloc(sizeof(struct heap));
  heap->size = initial_size;
  heap->len = 0;
  heap->nodes = xmalloc_atomic(initial_size*sizeof(struct node));
  return heap;
}
    
void free_heap(struct heap *heap){
  free(heap->nodes);
  free(heap);
}
