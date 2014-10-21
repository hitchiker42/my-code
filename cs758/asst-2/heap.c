#include "heap.h"
//this comes from the psudeocode on the wikipedia heapsort artical
void sift_down(binary_heap *heap,int root){
  int l,r;
  while((l=heap_left_child(root)) <= end){
    r = heap_right_child(root);
    swap=root;
    //is left child bigger?
    if(heap->compare_fn(heap->heap[l],heap->heap[swap])){
      swap=l;
    }
    //is right child bigger?
    if(r<=end && heap->compare_fn(heap->heap[r],heap->heap[swap])){
      swap=r;
    }
    if(swap != root){
      SWAP(heap->heap[root],heap->heap[swap]);
      root=swap;
    } else {
      return;
    }
  }
}
void sift_up(binary_heap *heap,int index){
  int parent=heap_parent(index);
  while(heap->compare_fn(heap->heap[index],heap->heap[parent])){
    SWAP(heap->heap[index],heap->heap[parent]);
    index=parent;
    parent=heap_parent(index);
  }
}
void heapify(binary_heap *heap){
  int start;
  for(start=heap_parent((heap->len-1));start>0;start--){
    sift_down(heap,start,heap->len-1);
  }
}
binary_heap *make_new_heap(void *arr, int len){
  int mem = (len &(len -1)) << 1;
  binary_heap *heap=malloc(sizeof(binary_heap));
  heap->len=len;
  heap->mem=mem;
  heap->heap = malloc(mem*sizeof(void*));
  memcpy(heap->heap, arr, len);
  return heap;
}
  
void heap_add(binary_heap *heap, void *new_element){
  if(heap->len == heap->mem){
    heap->heap=realloc(heap->heap, heap->mem * 2);
    heap->mem *= 2;
  }
  heap->heap[heap->len++];
  sift_up(heap, heap->len);
}
void* heap_pop(binary_heap *heap){
  void *retval=*heap->heap;
  heap->heap[0]=heap->heap[heap->len-1];
  sift_down(heap,0,heap->len-2);
  return retval;
}
//works by modifying memory, but returns a value for convience
binary_heap *heap_sort(binary_heap *heap){
  int end=heap->len-1;
  heapify(heap);
  while(end>0){
    SWAP(heap->heap[end],heap->heap[0]);
    end--;
    sift_down(heap,0,end);
  }
  return heap;
}
