#include "graph.h"
#define heap_left_child(i) (2*i+1)
#define heap_right_child(i) (2*i+2)
#define heap_parent(i) ((i-1)/2)
#define ARR_SWAP(arr,i,j)                                       \
  __extension__({ __typeof__(arr[i]) __temp = arr[i];           \
    arr[i] = arr[j];                                            \
    arr[j] = __temp;                                            \
    ;})
#define MAX(x,y)                                                 \
  __extension__({ __typeof(x) __x = x;                           \
      __typeof(y) __y = y;                                       \
      (__x > __y ? __x : __y);})
#define MIN(x,y)                                                 \
  __extension__({ __typeof(x) __x = x;                           \
      __typeof(y) __y = y;                                       \
      (__x < __y ? __x : __y);})
static void heap_sift_down(struct heap *heap, int root){
  int left,right,swap;
  left = right = swap = 0;  
  while ((left = heap_left_child(root)) < heap->len){
    right = heap_right_child(root);
    swap = root;
    if(heap->edges[left].weight < heap->edges[swap].weight){
      swap = left;
    }
    if(right < heap->len &&
       (heap->edges[right].weight < heap->edges[swap].weight)){
      swap = right;
    }
    if(swap == root){
      return;
    } else {
      ARR_SWAP(heap->edges, root, swap);
      root = swap;
    }
  }
}

static void heap_sift_up(struct heap *heap, int index){
  int parent = heap_parent(index);
  //this will terminate at the root since heap_parent(0) = 0
  while(heap->edges[index].weight < heap->edges[parent].weight){
    ARR_SWAP(heap->edges, index, parent);
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
    heap->size = MIN(heap->size*2,heap->max_size);
    heap->edges = xrealloc(heap->edges, heap->size*sizeof(graph_edge));
  }
  heap->edges[heap->len] = edge;
  heap_sift_up(heap, heap->len);
  heap->len++;
}
graph_edge heap_pop(struct heap *heap){
  graph_edge retval = heap->edges[0];
  heap->edges[0] = heap->edges[--heap->len];
  heap_sift_down(heap, 0);
  return retval;
}
graph_edge* heapsort_heap(struct heap *heap){
  int i;
  for(i=heap->len-1;i>0;i--){
    ARR_SWAP(heap->edges,i,0);
    heap_sift_down(heap, 0);
  }
  return heap->edges;
}
struct heap *make_heap(size_t initial_size, size_t max_size){
  if(initial_size == 0){
    initial_size = 128;
  }
  DEBUG_PRINTF("Creating heap with size %lu, and max size %lu\n",
               initial_size, max_size);
  struct heap *heap = xmalloc(sizeof(struct heap));
  heap->size = initial_size;
  heap->len = 0;
  heap->edges = xmalloc_atomic(initial_size*sizeof(graph_edge));
  if(max_size == 0){
    heap->max_size = -1;
  } else {
    heap->max_size = max_size;
  }
  return heap;
}
    
void free_heap(struct heap *heap){
  free(heap->edges);
  free(heap);
}
