#include "C_util.h"
static uint64_t internal_rand_state[2];
//The version we use is called xorshift+
uint64_t util_rand_r(util_rand_state state){
  uint64_t x = state[0], const y = state[1];
  state[0] = y;
  x ^= x << 23;
  state[1] = (x ^ y) ^ (x >> 17) ^ (y >> 26);
  return state[1] + y;
}
uint64_t util_rand(){
  return util_rand_r(internal_rand_state);
}
/*
  There's an obvious way to do this (util_rand() % (max-min) + min), and
  a correct way to do this, which is what follows
*/
int64_t util_rand_range_r(int64_t min, int64_t max, util_rand_state state);
int64_t util_rand_range(int64_t min, int64_t max, util_rand_state state);
  
  
//This is 2^-64
static const double int_to_float_multiplier =  (1.0/18446744073709551616.0);
double util_drand_r(util_rand_state state){
  return util_rand_r(state)*int_to_float_multiplier;
}
uint64_t util_drand(){
  return util_drand_r(internal_rand_state);
}
util_rand_state util_get_rand_state(void){
  util_rand_state ret = internal_rand_state;
  return ret;
}
util_rand_state util_set_rand_state(util_rand_state state){
  util_rand_state ret = internal_rand_state;
  internal_rand_state[0] = state[0];
  internal_rand_state[1] = state[1];
  return ret;
}
util_rand_state util_auto_rand_state(void){
  util_rand_state state[2];
#if (defined USE_SEED_URANDOM) && (USE_SEED_URANDOM > 0)
  int fd = open("/dev/urandom", O_RDWR);
  if(fd > 0){
    int err = read(fd, state, 2*sizeof(uint64_t));
    if(err == 16){
      return state;
    }
    close(fd);
  }
#endif
#if (defined USE_SEED_TIME) && (USE_SEED_TIME > 0)
  struct timespec ts = get_current_time;
  state[0] =  ts.tv_sec;
  state[1] = ts.tv_nsec;
  return state;
#else
//It's better than leaving it 0
  state[0] = 0x8a5cd789635d2dffUL;
  state[1] = 0x121fd2155c472f96UL;
  return state;
}
void util_srand(uint64_t a, uint64_t b){
  internal_rand_state[0] = a;
  internal_rand_state[1] = b;
  return;
}
void util_srand_auto(uint64_t a, uint64_t b){
  util_rand_state tmp = util_auto_rand_state();
  internal_rand_state[0] = tmp[0];
  internal_rand_state[1] = tmp[1];
  return;
}    
void shuffle_array(void **arr, size_t len){
  if(len <= 1){//avoid underflow if len==1
    return;
  }
  //be threadsafe
  util_rand_state state = util_get_rand_state();
  size_t i;
  for(i=0;i<n-1;i++){
    size_t j = util_rand_range_r(0, n-i, state);
    SWAP(arr[i], arr[i+j]);
  }
}
//take a min, max an count and print random numbers.
int main(int argc, char *argv[]){
}
