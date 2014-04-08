//auxiliary routines for sorting
#define heap_left_child(i) (2*i+1)
#define heap_right_child(i) (2*i+2)
#define heap_parent(i) ((i-1)/2)
static english_word** heap_sort(english_word **heap, uint32_t size);
static void sift_down(english_word **heap, uint32_t start, uint32_t end);
static void heapify(english_word **heap, uint32_t heap_length);
static english_word *heap_pop(english_word **heap,uint32_t heap_length);

//this comes from the psudeocode on the wikipedia heapsort artical
static inline void sift_down(english_word **heap,uint32_t start,uint32_t end){
  uint32_t root=start,child,swap;
  while((child=heap_left_child(root)) <= end){
    swap=root;
    //is left child bigger?
    if(heap[child]->count > heap[swap]->count  ){
      swap=child;
    }
    //is right child bigger?
    if(child+1<=end && heap[child+1]->count > heap[swap]->count){
      swap=child+1;
    }
    if(swap!=root){
      SWAP(heap[root],heap[swap]);
      root=swap;
    } else {
      return;
    }
  }
}
static inline void sift_up(english_word **heap,uint32_t index){
  uint32_t parent=heap_parent(index);
  while(heap[index]->count > heap[parent]->count){
    SWAP(heap[index],heap[parent]);
    index=parent;
    parent=heap_parent(index);
  }
}
static inline void heapify(english_word **heap,uint32_t heap_length){
  uint32_t start=heap_parent((heap_length-1));
  do {
    sift_down(heap,start,heap_length-1);
  } while(start--);
}
static inline void heap_insert(english_word **heap,english_word *new_element,
                               uint32_t heap_index){
  heap[heap_index]=new_element;
  sift_up(heap,heap_index);
}
static inline english_word *heap_pop(english_word **heap,uint32_t heap_length){
  english_word *retval=*heap;
  heap[0]=heap[heap_length-1];
  sift_down(heap,0,heap_length-2);
  return retval;
}
//this is pretty inefficent, but it should work for now
static inline int heap_swap_min(english_word **heap,english_word *new){
  int i,heap_index=15,min_count[2]={heap[15]->count,heap[15]->count};
  //find the current smallest value (scan the bottom row of the heap)
  for(i=16;i<30;i++){
    if(heap[i]->count < min_count[1]){
      min_count[0]=min_count[1];
      min_count[1]=heap[i]->count;
      heap_index=i;
    }
  }
  heap[heap_index]=new;
  sift_up(heap,heap_index);
  return min_count[0];
}
int compare(const english_word *a,const english_word *b){
  if(a->count==b->count){
    return 0;
  } else if(a->count > b->count){
    return 1;
  } else {
    return -1;
  }
}
//no args because all the data used is global
struct heap sort_words(){
  //maybe this should be thread local
  //this is 32 instead of 20 so we have a full 5 level binary tree
  //which needs 31 blocks and 32 is a much nicer value than 31
  english_word *most_common[30];
  uint32_t i=0,j=0,minimum=-1;
  //first get twenty words used in every file, if there are less then twenty
  //then this is all we need to do
  file_bitfield max_file_bits={.uint128=0};
  PRINT_MSG("start of sort_words\n");
  while(i<30 && j<indices_index){
    english_word *cur_word=global_hash_table[hash_table_indices[j++]];
    if(cur_word->file_bits.uint128>max_file_bits.uint128){
      max_file_bits=cur_word->file_bits;
    }
    if(cur_word->file_bits.uint128 == all_file_bits.uint128){
      if(cur_word->count < minimum){
        minimum=cur_word->count;
      }
      heap_insert(most_common,cur_word,i);
      i++;
    }
  }
  PRINT_FMT("max file_bits=\n%#016lx %#016lx\n",max_file_bits.high,
            max_file_bits.low);
  if(i<20){
    if(i==0){
      fprintf(stderr,"No words common to all files\n");
      exit(0);
    }
    qsort(most_common,i,sizeof(english_word*),(comparison_fn_t)compare);
    return (struct heap){.heap=most_common,.size=i};
  }
  PRINT_MSG("added initial 20 words\n");
  i=30;
  while(j<indices_index){
    english_word *cur_word=global_hash_table[hash_table_indices[j++]];
    if(cur_word->file_bits.uint128 == all_file_bits.uint128){
      if(cur_word->count > minimum){
        minimum=heap_swap_min(most_common,cur_word);
      }
    }
  }
  PRINT_MSG("added all words\n");
  i=30;
  for(j=0;j<i;j++){
    fprintf(stderr,"%d\n",most_common[j]->count);
  }
  qsort(most_common,i,sizeof(english_word*),(comparison_fn_t)compare);
  for(j=0;j<i;j++){
    fprintf(stderr,"%d\n",most_common[j]->count);
  }
  return (struct heap){.heap=most_common,.size=i};
}
#if 0
struct heap sort_words_2(){
  //maybe this should be thread local
  //this is 32 instead of 20 so we have a full 5 level binary tree
  //which needs 31 blocks and 32 is a much nicer value than 31
  english_word **most_common=thread_stacks;
  uint32_t i=0,j=0;
  //first get twenty words used in every file, if there are less then twenty
  //then this is all we need to do
  PRINT_MSG("start of sort_words\n");
  while(j<indices_index){
    english_word *cur_word=global_hash_table[hash_table_indices[j++]];
    if(cur_word->file_bits.uint128 == all_file_bits.uint128){
    heap_insert(most_common,cur_word,i);
    i++;
    }
  }
  return (struct heap){.heap=heap_sort(most_common,i),.size=i};
}
#endif
//works by modifying memory, but returns a value for convience
static inline english_word** heap_sort(english_word **heap,uint32_t size){
  uint32_t end=size-1;
  fprintf(stderr,"started heapify");
  heapify(heap,size);
  fprintf(stderr,"finished heapify");
  while(end>0){
    SWAP(heap[end],heap[0]);
    end--;
    sift_down(heap,0,end);
  }
  return heap;
}
static int is_sorted(english_word **arr,uint32_t size){
  uint32_t i;
  uint32_t count=-1;
  for(i=size-1;i>0;i--){
    if(arr[i]->count > count){
      return 0;
    } else {
      count=arr[i]->count;
    }
  }
  return 1;
}
