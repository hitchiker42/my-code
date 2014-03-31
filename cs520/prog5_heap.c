//auxiliary routines for sorting
#define heap_left_child(i) (2*i+1)
#define heap_right_child(i) (2*i+2)
#define heap_parent(i) (i?(i-1)/2:0)
static english_word** heap_sort(english_word **heap, uint32_t size);
static void sift_down(english_word **heap, uint32_t start, uint32_t end);
static void heapify(english_word **heap, uint32_t index, uint32_t heap_length);
static english_word *heap_pop(english_word **heap,uint32_t heap_length);

static inline void heap_insert(english_word **heap,english_word *new_element,
                               uint32_t heap_index){
  heap[heap_index]=new_element;
  //will always terminate when heap_index = 0 because heap_parent returns 0
  //when given 0
  while(heap[heap_index]->count > heap[heap_parent(heap_index)]->count){
    SWAP(heap[heap_index],heap[heap_parent(heap_index)]);
    heap_index=heap_parent(heap_index);
  }
}
static inline void heapify(english_word **heap,uint32_t index,
                           uint32_t heap_length){
  uint32_t left,right,cur_max;
  while(index<heap_length){
    left=heap_left_child(index);
    right=heap_right_child(index);
    cur_max=index;
    if(heap[left]->count > heap[cur_max]->count){
      cur_max=left;
    }
    if(heap[right]->count > heap[cur_max]->count){
      cur_max=right;
    }
    if(cur_max == index){
      break;
    }
    SWAP(heap[index],heap[cur_max]);
    index=cur_max;
  }
}
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

static inline english_word *heap_pop(english_word **heap,uint32_t heap_length){
  english_word *retval=*heap;
  heap[0]=heap[heap_length-1];
  heapify(heap,0,heap_length-2);
  return retval;
}
//this is pretty inefficent, but it should work for now
static inline int heap_swap_min(english_word **heap,english_word *new){
  int i,heap_index=15,min_count[2]={heap[15]->count,heap[15]->count};
  //find the current smallest value
  for(i=16;i<=30;i++){
    if(heap[i]->count < min_count[1]){
      min_count[0]=min_count[1];
      min_count[1]=heap[i]->count;
      heap_index=i;
    }
  }
  heap[heap_index]=new;
  while(heap[heap_index]->count > heap[heap_parent(heap_index)]->count){
    SWAP(heap[heap_index],heap[heap_parent(heap_index)]);
    heap_index=heap_parent(heap_index);
  }
  return min_count[0];
}

//no args because all the data used is global
struct heap sort_words(){
  //maybe this should be thread local
  //this is 32 instead of 20 so we have a full 5 level binary tree
  //which needs 31 blocks and 32 is a much nicer value than 31
  static english_word *most_common[32];
  uint32_t i=0,j=0,minimum=-1;
  //first get twenty words used in every file, if there are less then twenty
  //then this is all we need to do
  PRINT_MSG("start of sort_words\n");
  while(i<31 && j<indices_index){
//    PRINT_FMT("Loop %d\n",j);
    english_word *cur_word=global_hash_table[hash_table_indices[j++]];
//    if(cur_word->file_bits.uint128 == all_file_bits.uint128){
      if(cur_word->count < minimum){
        minimum=cur_word->count;
      }
      heap_insert(most_common,cur_word,i);
      i++;
//    }
  }
  if(i<20){
    PRINT_MSG("Less than twenty words found\n");
    return (struct heap){.heap=heap_sort(most_common,i),.size=i};
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
  i=31;
  return (struct heap){.heap=heap_sort(most_common,i),.size=i};
}
struct heap sort_words_2(){
  //maybe this should be thread local
  //this is 32 instead of 20 so we have a full 5 level binary tree
  //which needs 31 blocks and 32 is a much nicer value than 31
  english_word **most_common=xmalloc(indices_index*sizeof(english_word*));
  uint32_t i=0,j=0;
  //first get twenty words used in every file, if there are less then twenty
  //then this is all we need to do
  PRINT_MSG("start of sort_words\n");
  while(j<indices_index){
    english_word *cur_word=global_hash_table[hash_table_indices[j++]];
//    if(cur_word->file_bits.uint128 == all_file_bits.uint128){
    heap_insert(most_common,cur_word,i);
    i++;
//    }
  }
  return (struct heap){.heap=heap_sort(most_common,i),.size=i};
}
//works by modifying memory, but returns a value for convience
static inline english_word** heap_sort(english_word **heap,uint32_t size){
  PRINT_MSG("Running heapsort\n");
  uint32_t end=size-1;
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
