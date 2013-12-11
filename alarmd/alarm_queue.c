#include "alarmd.h"
static my_alarm _temp_;
static my_alarm *_temp_ptr=&_temp_;
#define left_child(i) ((i<<1)+1)
#define right_child(i) ((i<<1)+2)
#define parent(i) ((i-1)>>1)
#define queue_compare(i,j) (alarm_queue[i]->alarm_time>alarm_queue[j]->alarm_time)
#define queue_swap(i,j) _temp_ptr=alarm_queue[i];\
  alarm_queue[i]=alarm_queue[j];              \
  alarm_queue[j]=_temp_ptr
void alarm_queue_heapify(int index){
  int left,right,cur_max;
  while(index<queue_length){
    left=left_child(index);
    right=right_child(index);
    cur_max=index;
    if(queue_compare(left,cur_max)){
      cur_max=left;
    }
    if(queue_compare(right,cur_max)){
      cur_max=right;
    }
    if(cur_max == index){
      break;
    }
    queue_swap(index,cur_max);
    index=cur_max;
  }
}

//pop's head of alarm queue, sets queue length and next_alarm to
//appropiate values
my_alarm* alarm_head_pop(){
  my_alarm* retval=alarm_queue[0];
  alarm_queue[0]=alarm_queue[queue_length-1];
  queue_length--;
  alarm_queue_heapify(0);
  return retval;
}
void alarm_heap_add(my_alarm *alarm){
  uint64_t alarm_time=(uint64_t)alarm->alarm_time;
  if(queue_size<=queue_length){
    xrealloc(alarm_queue,(queue_size*=2));
    memset(alarm_queue+queue_length,'\0',(queue_size-queue_length));
  }
  alarm_queue[queue_length]=alarm;
  int i=queue_length;
  queue_length++;
  while(i>0){
    if(alarm_queue[parent(i)]->alarm_time>alarm->alarm_time){
      queue_swap(parent(i),i);
      i=parent(i);
    }
  }
}
