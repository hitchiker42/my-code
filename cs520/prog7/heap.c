//auxiliary routines for sorting

#define heap_left_child(i) (2*i+1)
#define heap_right_child(i) (2*i+2)
#define heap_parent(i) ((i-1)/2)
static english_word** heap_sort(english_word **heap, int32_t size);
static void sift_down(english_word **heap, int32_t start, int32_t end);
static void heapify(english_word **heap, int32_t heap_length);
static english_word *heap_pop(english_word **heap, int32_t heap_length);

//this comes from the psudeocode on the wikipedia heapsort artical
static inline void sift_down(english_word **heap,int32_t start,int32_t end){
  int32_t root=start,child,swap;
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
static inline void sift_up(english_word **heap,int32_t index){
  int32_t parent=heap_parent(index);
  volatile english_word a=*heap[index];
  while(heap[index]->count > heap[parent]->count){
    SWAP(heap[index],heap[parent]);
    index=parent;
    parent=heap_parent(index);
  }
}
static inline void heapify(english_word **heap,int32_t heap_length){
  int32_t start=heap_parent((heap_length-1));
  while(start-->0){
    sift_down(heap,start,heap_length-1);
  }
}
static inline void heap_insert(english_word **heap,english_word *new_element,
                               int32_t heap_index){
  heap[heap_index]=new_element;
  sift_up(heap,heap_index);
}
static inline english_word *heap_pop(english_word **heap,int32_t heap_length){
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
//no args because all the data used is global
struct heap sort_words(){
  //reuse some static memory we don't need anymore
  static english_word *most_common[8000];
  //  memset(thread_bufs,'\0',100*sizeof(english_word*));
  int32_t i=0,j=0;
  PRINT_MSG("start of sort_words\n");
  english_word *cur_word,*last_word=NULL;
  //if I had more time I would've liked to parallize this
  //but I'm not sure how I would've done it, probably
  //divide indices_index by NUM_PROCS have each thread
  //pick out the words that are in every file for the range
  //indices_index/NUM_PROCS*thread_no - indices_index/NUM_PROCS+thread_no+1
  //then go back to one thread to sort it
  //it's 10PM on wednesday as I write this, if I had another day I would
  //probably do that, then again I've spent enough time on this as it is
  while(j<indices_index){
    cur_word=global_hash_table[hash_table_indices[j++]];
//    if(last_word==cur_word){
  //    fprintf(stderr,"HERE\n");
//      continue;
//    }
//    last_word=cur_word;
    if(cur_word->file_bits.uint128 == all_file_bits.uint128){
      most_common[i]=cur_word;
      sift_up(most_common,i++);
    }
  }
  if(i==0){
    fprintf(stderr,"No words common to all files\n");
    exit(EXIT_SUCCESS);
  }
  return (struct heap){.heap=most_common,.size=i};
}
//works by modifying memory, but returns a value for convience
static inline english_word** heap_sort(english_word **heap,int32_t size){
  int32_t end=size-1;
  heapify(heap,size);
  while(end>0){
    SWAP(heap[end],heap[0]);
    end--;
    sift_down(heap,0,end);
  }
  return heap;
}
static int is_sorted(english_word **arr,int32_t size){
  int32_t i;
  int32_t count=-1;
  for(i=size-1;i>0;i--){
    if(arr[i]->count > count){
      return 0;
    } else {
      count=arr[i]->count;
    }
  }
  return 1;
}
static inline void print_nth(english_word *word,int i){
  //this is the way I would print it, but
  //looking at the description of the program we are only supposed
  //to print one word per line, and I figure it'll be eaiser
  //to parse if  it's just a word follower by a count
  //  printf("The %2d%s most common word was ",i,ordinal_suffix(i));
  print_word(word);
  //  printf(" with %d occurances\n",word->count);
  printf(" %d\n",word->count);
}
void print_results_heap(struct heap heap_){
  //heap satisifies the heap property, but isn't sorted
  english_word **heap=heap_.heap;
  int count=0,i,j;
  int32_t size=heap_.size-1;
//  heapify(heap,size);
  english_word *nth=*heap,*last=heap[1],*twentieth;
  english_word *most_common[20];
  while(count<20 && size >= 0 ){
    nth=*heap;
/*    if(nth==most_common[count-1]){
      heap[0]=heap[size--];
      sift_down(heap,0,size);
      nth=*heap;
      most_common[count-1]->count+=1;
    }*/
    most_common[count]=nth;
    
/*    for(i=0;i<count;i++){
      for(j=count;j>i;j--){
        assert(most_common[i]->count>=most_common[j]->count);
        assert((i == j) || !string_compare(most_common[i],most_common[j]));
      }
    }*/
/*    last=nth;*/
    ++count;
    heap[0]=heap[size--];
    sift_down(heap,0,size);
  }
  for(i=0;i<count;i++){
    print_nth(most_common[i],i+1);
  }
  if(count == 20){
    twentieth=nth;
    while((nth=*heap)->count==twentieth->count){
      print_nth(nth,++count);
      heap[0]=heap[size--];
      sift_down(heap,0,size);
    }
  }
}
//would probably be faster but doesn't work
#if 0
struct heap sort_words(){
  //this is 32 instead of 20 so we have a full 5 level binary tree
  //which needs 31 blocks and 32 is a much nicer value than 31
  english_word *most_common[30];
  int32_t i=0,j=0,minimum=-1;
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
  //  qsort(most_common,i,sizeof(english_word*),(comparison_fn_t)compare);
  return (struct heap){.heap=heap_sort(most_common,i),.size=i};
}
#endif
