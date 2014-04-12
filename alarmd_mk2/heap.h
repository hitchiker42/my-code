#ifndef ALARMD_HEAP_H
#define ALARMD_HEAP_H
typedef struct heap binary_heap;
struct heap {
  void **heap;
  int (*compare_fn)(void*,void*);
  int len;
};
#define heap_left_child(i) (2*i+1)
#define heap_right_child(i) (2*i+2)
#define heap_parent(i) ((i-1)/2)

binary_heap* heap_sort(binary_heap *heap);
void heap_sift_down(english_word **heap, int start, int end);
void heap_sift_up(english_word **heap, int index);
void heapify(binary_heap *heap);
void* heap_pop(binary_heap *heap);
void heap_insert(binary_heap *heap,void *new_element,int heap_index);
#endif
