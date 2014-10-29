#include "graph.h"
#define heap_left_child(i) (2*i+1)
#define heap_right_child(i) (2*i+2)
#define heap_parent(i) ((i-1)/2)
#define ARR_SWAP(arr,i,j)                                       \
  { __typeof__(arr[i]) __temp = arr[i];                         \
    arr[i] = arr[j];                                            \
    arr[j] = __temp;                                            \
  }
static void heap_sift_down(struct heap *heap, int root){
  int left,right,swap;
  while ((left = heap_left_child(root)) < heap->size){
    right = heap_right_child(root);
    swap = root;
    if(heap->edges[left].weight < heap->edges[swap].weight){
      swap = left;
    }
    if(right < heap->size &&
       (heap->edges[right].weight < heap->edges[swap].weight)){
      swap = right;
    }
    if(swap == root){
      return;
    } else {
      ARR_SWAP(heap, root, swap);
      root = swap;
    }
  }
}

static void heap_sift_up(double *heap, int index){
  int parent = heap_parent(index);
  //this will terminate at the root since heap_parent(0) = 0
  while(heap->edges[index].weight < heap->edges[parent].weight){
    ARR_SWAP(heap, index, parent);
    index = parent;
    parent = heap_parent(index);
  }
}
//turns an array of doubles into a min heap
static void heapify(struct heap *heap){
  int i = heap->len/2;
  while (i >= 0) {
    heap_sift_down(heap, i--);
  }
}

void heap_add(struct heap *heap, graph_edge edge){
  if(heap->len >= heap->size){
    heap->edges = xrealloc(heap->edges, heap->size*2);
    heap->size*=2;
  }
  heap->edges[heap->len] = val;
  heap_sift_up(heap, heap->len);
  heap->len++;
}
graph_edge heap_pop(struct heap *heap){
  graph_edge retval = heap->edges[0];
  heap->edges[0] = heap->edges[heap->len--];
  heap_sift_down(heap, 0);
  return retval;
}
graph_edge* heap_sort_heap(struct heap *heap){
  int i;
  for(i=heap->len-1;i>0;i--){
    ARR_SWAP(heap,i,0);
    heap_sift_down(heap, 0);
  }
  return heap->edges;
}
struct heap *make_heap(int initial_size){
  if(initial_size == 0){
    initial_size = 128;
  }
  struct heap *heap = xmalloc(sizeof(structheap));
  heap->size = initial_size;
  heap->edges = xmalloc(initial_size*sizeof(graph_edge));
  return heap;
}
    
