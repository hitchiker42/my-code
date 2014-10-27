#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdint.h>
#include <time.h>
#include <limits.h>
#define zmalloc(sz) calloc(sz,1)
#define binary_byte_string(byte)\
  ({char str[8];                                \
  memcpy(str,nibble_strings[byte&0xf],4);       \
  memcpy(str,nibble_strings[byte&0xf0],4);\
  })
typedef struct binary_clock binary_clock;
struct binary_clock {
  int size;
  int state;
  uint64_t time;
  pthread_t thread;
  pthread_rwlock_t lock;
  uint8_t clock[];
};
static uint8_t bitmasks[8]={0x1,0x2,0x4,0x8,0x10,0x20,0x40,0x80};
static char *nibble_strings[16]=
  {"0000","0001","0010","0011","0100","0101","0110","0111",
   "1000","1001","1010","1011","1100","1101","1110","1111"};
extern inline void set_time(binary_clock *clock,uint64_t time){
  pthread_rwlock_wrlock(&clock->lock);
  memcpy(clock->clock,time,8);
  pthread_rwlock_unlock(&clock->lock);
}
static void* clock_main(binary_clock *clock);
binary_clock *make_binary_clock(int size,uint64_t time){
  if(!size){
    size=64;
  }
  binary_clock *clock=zmalloc(sizeof(binary_clock)+size);
  if(!clock){
    perror("malloc failed");
    exit(1);
  }
  clock->size=size;
  clock->time=time;
  pthread_rwlock_init(&clock->lock);
  if(time){
    memcpy(clock->clock,time,8);
  }
  return clock;
}
void start_binary_clock(binary_clock *clock){
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
  pthread_create(&clock->thread,&attr,clock_main,clock);
  return;
}
static void* clock_main(binary_clock *clock){
  //not sure how to insure time is correct, since it takes time to
  //actually update the time 
  struct timespec wait_time={.tv_sec=0,.tv_nsec=(long)9.99e9};
  while(clock->state>0){//no need to lock here, doing an extra loop doesn't hurt
    
}
  
static inline void step_clock(binary_clock *clock){
  int i;
  pthread_rwlock_wrlock(&clock->lock);
  clock->time++;
  for(i=0;i<(clock->size/8);i++){
    clock->clock[i]++;
    if(clock->clock[i]){
      break;
    }
  }
  pthread_rwlock_unlock(&clock->lock);
}
void *clock_to_byte_array(binary_clock *clock){
  pthread_rwlock_rdlock(&clock->lock);
  uint8_t *byte_arry=malloc(clock->size);
  
  pthread_rwlock_unlock(&clock->lock);
