#ifndef _MY_HEAP_H
#define _MY_HEAP_H
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
typedef struct heap binary_heap;
struct heap {
  void **heap;
  int (*cmp)(void*,void*);
  int len;
  int mem;
};
binary_heap* make_new_heap(void *arr, int size);
binary_heap* heap_sort(binary_heap *heap);
void heap_sift_down(binary_heap **heap, int start, int end);
void heap_sift_up(english_word **heap, int index);
void heapify(binary_heap *heap);
void* heap_pop(binary_heap *heap);
void heap_add(binary_heap *heap,void *new_element);
#endif
