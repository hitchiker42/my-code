#include "C_util.h"
#define heap_left_child(i) (2*i+1)
#define heap_right_child(i) (2*i+2)
#define heap_parent(i) ((i-1)/2)
/*
  Implemented so that end should be the length of the heap, rather
  than the index of the last element. Swap the '<' to '<=' to make
  end the index of the last element.
*/
static void sift_down(binary_heap *heap, long root, long end){
  long l,r,swap;
  while((l = heap_left_child(root)) < end){
    r = heap_right_child(root);
    swap = root;
    //should we swap with the left 
    if(heap->cmp(heap->heap[l],heap->heap[swap])){
      swap = l;
    }//can I make this next if and else if?
    if(r < end && heap->cmp(heap->heap[r],heap->heap[swap])){
      swap = r;
    }
    if(swap != root){
      SWAP(heap->heap[root],heap->heap[swap]);
      root = swap;
    } else {
      return;
    }
  }
}
static void sift_up(binary_heap *heap, int index){
  int parent = heap_parent(index);
  while(heap->cmp(heap->heap[index], heap->heap[parent])){
    SWAP(heap->heap[index],heap->heap[parent]);
    index = parent;
    parent = heap_parent(index);
  }
}
static void heapify(binary_heap *heap){
  long start;
  for(start=heap_parent((heap->len-1));start>=0;start--){
    sift_down(heap, start, heap->len);
  }
}
int cmp_default(void *x, void *y){
  return x < y;
}
binary_heap *make_new_heap(void *arr, int len, cmp_fun cmp){
  int mem = (len & (len -1)) << 1;
  binary_heap *heap = xmalloc(sizeof(binary_heap));
  heap->len = len;
  heap->mem = mem;
  heap->heap = xmalloc(mem*sizeof(void*));
  heap->cmp = cmp ? cmp : cmp_default;
  memcpy(heap->heap, arr, len*sizeof(void*));
  heapify(heap);
  return heap;
}  
void destroy_heap(binary_heap *heap){
  free(heap->heap);
  free(heap);
}
void heap_add(binary_heap *heap, void *new_element){
  if(heap->len == heap->mem){
    heap->heap = realloc(heap->heap, heap->mem * 2);
    heap->mem *= 2;
  }
  heap->heap[heap->len++] = new_element;
  sift_up(heap, heap->len-1);
}
void* heap_pop(binary_heap *heap){
  void *retval = *heap->heap;
  heap->heap[0] = heap->heap[--heap->len];
  sift_down(heap, 0, heap->len);
  return retval;
}
//works by modifying memory, but returns a value for convience
binary_heap *heap_sort(binary_heap *heap){
  if(heap->len <= 1){
    return heap;
  }
  int end = heap->len-1;
  heapify(heap);
  do {
    SWAP(heap->heap[end], heap->heap[0]);
    sift_down(heap, 0, end);
  } while(--end > 0);
  return heap;
}
void heap_sort_generic(void **input, size_t len, cmp_fun cmp){
  binary_heap *heap = alloca(sizeof(binary_heap));
  heap->heap = input;
  heap->cmp = cmp;
  heap->len = len;
  heap->mem = len;
  heap_sort(heap);
  return;
}
