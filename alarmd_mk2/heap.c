#include "heap.h"
//this comes from the psudeocode on the wikipedia heapsort artical
void sift_down(binary_heap *heap,int start,int end){
  int child,swap,root;
  root=start;
  while((child=heap_left_child(root)) <= end){
    swap=root;
    //is left child bigger?
    if(heap->compare_fn(heap->heap[child],heap->heap[swap])){
      swap=child;
    }
    //is right child bigger?
    if(child+1<=end && heap->compare_fn(heap->heap[child+1],heap->heap[swap])){
      swap=child+1;
    }
    if(swap!=root){
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
heap_insert(binary_heap *heap,void *new_element,int heap_index){
  heap[heap_index]=new_element;
  sift_up(heap,heap_index);
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
