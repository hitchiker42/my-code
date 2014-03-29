#include "alarmd.h"
//all these functions require external locking
static my_alarm _temp_;
static my_alarm *_temp_ptr=&_temp_;
#define left_child(i) ((i*2)+1)
#define right_child(i) ((i*2)+2)
#define parent(i) ((i-1)/2)
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
my_alarm* alarm_heap_pop(){
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
void alarm_heap_delete(int index){
  my_alarm *deleted_node=alarm_queue[index];
  xfree(deleted_node);
  alarm_queue[index]=alarm_queue[queue_length-1];
  queue_length--;
  alarm_queue_heapify(index);
}
int alarm_heap_try_delete(uint32_t alarm_id){
  int i;
  for(i=0;i<queue_length;i++){
    if(alarm_id==alarm_queue[i]->alarm_id){
      alarm_heap_delete(i);
      return i;
    }
  }
  return -1;
}
/* The string form of an alarm is
   "alarm set for: " time\n
   "running command: " command\n
   ["repeats: " days\n
*/
static char *music_command(my_alarm *alarm){
  //not sure if this'll work
  char *str=xmalloc(7+9+alarm->command_len);
  memcpy(str,"mplayer",7);
  memcpy(str+7,repeat_opt,9);
  memcpy(str+16,alarm->command,alarm->command_len);
  return str;
}
#define do_day(day,arr)                         \
  if(ALARM_##day(alarm)){                       \
    memcpy(str_acc+len,arr,4);                  \
    len+=4;                                     \
  }
//assume we check repeat before we call this
//returns a string of the form
//'repeats: [mon,][tue,][wed,][thu,][fri,][sat,][sun,]' with the last , ommited
static char str_acc[28];//room for three letters + , for each day of the week
const char mon_abbr[4]={'m','o','n',','};
const char tue_abbr[4]={'t','u','e',','};
const char wed_abbr[4]={'w','e','d',','};
const char thu_abbr[4]={'t','h','u',','};
const char fri_abbr[4]={'f','r','i',','};
const char sat_abbr[4]={'s','a','t',','};
const char sun_abbr[4]={'s','u','u',','};
static char *repeat_str(my_alarm *alarm){
  int len=0;
  do_day(MONDAY,mon_abbr);
  do_day(TUESDAY,tue_abbr);
  do_day(WEDNESDAY,wed_abbr);
  do_day(THURSDAY,thu_abbr);
  do_day(FRIDAY,fri_abbr);
  do_day(SATURDAY,sat_abbr);
  do_day(SUNDAY,sun_abbr);
  char* retval=xmalloc(9+len-1*sizeof(char));
  memcpy(retval,"repeats: ",9);
  memcpy(retval+9,str_acc,len-1);
  return retval;
}
static int alarm_string(char *str,int index){}
int alarm_heap_list(char **str_loc){}
  
