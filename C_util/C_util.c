/*
  Utility C routines
*/
#include "C_util.h"
#include <sys/types.h>
char * __attribute__((const)) filemode_bits_to_string(int mode){
  if(get_access_mode(mode) == O_RDWR){
    if(mode & O_TRUNC){
      return "w+";
    } else if(mode & O_APPEND){
      return "a+";
    } else {
      return "r+";
    }
  } else if(get_access_mode(mode) == O_WRONLY){
    if(mode & O_APPEND){
      return "a";
    } else {
      return "w";
    }
  } else if(get_access_mode(mode) == O_RDONLY){
    return "r";
  } else {
    return NULL;
  }
}

off_t file_len_by_fd(int fd){
  struct stat buf;
  fstat(fd, &buf);
  return buf.st_size;
}
off_t file_len_by_name(const char *filename){
  struct stat buf;
  stat(filename, &buf);
  return buf.st_size;
}
off_t file_len_by_FILE(FILE *file){
  off_t pos = ftello(file);//to get back to where we awere
  if(fseeko(file, 0, SEEK_END) == (off_t)-1){
    return (off_t)-1;
  }
  off_t end = ftello(file);
  fseeko(file, pos, SEEK_SET);
  return end;
}
int regular_filep_fd(long fd){
  struct stat buf;
  fstat(fd, &buf);
  return S_ISREG(buf.st_mode);
}
int regular_filep_filename(const char *filename){
  struct stat buf;
  stat((char *)filename, &buf);
  return S_ISREG(buf.st_mode);
}
int regular_filep_FILE(FILE* file){
  return regular_filep_fd(fileno(file));
}
char* read_file_to_string_fd(int fd, size_t *szptr){
  off_t len = file_len_by_fd(fd);
  char *buf = xmalloc(len + 1);
  read(fd, buf, len+1);
  buf[len] = '\0';
  if(szptr){
    *szptr = len;
  }
  return buf;
}
char* read_file_to_string_FILE(FILE *file, size_t *szptr){
  off_t len = file_len_by_FILE(file);
  char *buf = xmalloc(len + 1);
  fread(buf, 1, len, file);
  buf[len] = '\0';
  if(szptr){
    *szptr = len;
  }
  return buf;
}
char* read_file_to_string_filename(const char *filename, size_t *szptr){
  int fd = open(filename, O_RDONLY);
  char *ret = read_file_to_string_fd(fd, szptr);
  close(fd);
  return ret;
}
int __regular_filep(void *arg, int is_fd){
  struct stat buf;
  if(is_fd){
    fstat((long)arg, &buf);
  } else {
    stat((char *)arg, &buf);
  }
  return S_ISREG(buf.st_mode);
}
//Test if the file described by buf is accessable by the current
//process for reading/writing/executing (controled by access_mode)
//this is fairly simple conceptually, but cumbersome to implement
static int test_access(struct stat buf, int access_mode){
  uid_t euid = geteuid();
  gid_t egid = getegid();
  mode_t grp_mode;
  /*
    We need to test the files group against all the groups the
    process belongs to, but we avoid doing that untill we've done
    every other test, since it's slow.
  */
  if(access_mode == R_OK){
    if((euid == buf.st_uid && (buf.st_mode & S_IRUSR)) ||
       (egid == buf.st_gid && (buf.st_mode & S_IRGRP)) ||
       (buf.st_mode & S_IROTH)){
      return 1;
    } else {
      grp_mode = S_IRGRP;
    }
  } else if(access_mode == W_OK){
    if((euid == buf.st_uid && (buf.st_mode & S_IWUSR)) ||
       (egid == buf.st_gid && (buf.st_mode & S_IWGRP)) ||
       (buf.st_mode & S_IWOTH)){
      return 1;
    } else {
      grp_mode = S_IWGRP;
    }
  } else if(access_mode == X_OK){
    if((euid == buf.st_uid && (buf.st_mode & S_IXUSR)) ||
       (egid == buf.st_gid && (buf.st_mode & S_IXGRP)) ||
       (buf.st_mode & S_IXOTH)){
      return 1;
    } else {
      grp_mode = S_IXGRP;
    }
  } else {
    //unknown access mode
    return 0;
  }
  if(!(buf.st_mode & grp_mode)){
    return 0;
  }
  int i;
  int ngroups = getgroups(0, NULL);
  gid_t *groups = alloca(ngroups * sizeof(gid_t));
  getgroups(ngroups, groups);
  for(i=0;i<ngroups;i++){
    if(buf.st_gid == groups[i]){
      return 1;
    }
  }
  return 0;
}
static int filetest_core(struct stat buf, char test){
  switch(test){
    case 'b':
      return S_ISBLK(buf.st_mode);
    case 'c':
      return S_ISCHR(buf.st_mode);
    case 'd':
      return S_ISDIR(buf.st_mode);
    case 'e':
      //if this gets called stat succeded so the file must exist
      return 1;
    case 'f':
      return S_ISREG(buf.st_mode);
    case 'g':
      return (buf.st_mode & S_ISGID);
    case 'G':
      return (buf.st_gid == getegid());
    case 'h':
    case 'L':
      return S_ISLNK(buf.st_mode);
    case 'k':
      return (buf.st_mode & S_ISVTX);
    case 'O':
      return (buf.st_uid == geteuid());
    case 'p':
      return S_ISFIFO(buf.st_mode);
    case 'r':
      return test_access(buf, R_OK);
    case 's':
      return (buf.st_size > 0);
    case 'S':
      return S_ISSOCK(buf.st_mode);
    case 'u':
      return (buf.st_mode & S_ISUID);
    case 'w':
      return test_access(buf, W_OK);
    case 'x':
      return test_access(buf, X_OK);
    default:
      return 0;
  }
}
int filetest_filename(const char *filename, char test){
  struct stat buf;
  int err = stat(filename, &buf);
  //assume an error means the file doesn't exist, this may not
  //always be the case, but it's good enough
  if(err != 0){
    return 0;
  }
  return filetest_core(buf, test);
}
int filetest_fd(int fd, char test){
  struct stat buf;
  int err = fstat(fd, &buf);
  //assume an error means the file doesn't exist, this may not
  //always be the case, but it's good enough
  if(err != 0){
    return 0;
  }
  return filetest_core(buf, test);
}
int filetest_FILE(FILE *file, char test){
  return filetest_fd(fileno(file), test);
}
/*
  TODO: make memory protection part of the arguments to mmap functions
*/
void* mmap_file(int fd, int shared, int prot, size_t *sz){
  *sz= file_len_by_fd(fd);
  int visibility = (shared ? MAP_SHARED : MAP_PRIVATE);
  void *retval = mmap(NULL, *sz+1, prot, visibility,
                      fd, 0);
  if(retval == MAP_FAILED){
    perror("mmap");
    return NULL;
  }
  return retval;
}
void *mmap_filename(const char *file, int shared, int prot, size_t *sz){
  int fd = open(file, O_RDONLY);
  if(fd < 0){
    perror("open");
    return NULL;
  }
  void *buf = mmap_file(fd, shared, prot, sz);
  close(fd);
  return buf;
}
void *mmap_anon(size_t sz){
  void *mem = mmap(0, sz, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
  if(mem == MAP_FAILED){
    perror("mmap");
    return NULL;
  }
  return mem;
}
/*
int stream_at_end(mem_stream *stream){
  return(stream->off >= stream->sz);
}
void free_stream(mem_stream *stream){
  munmap(stream->mem, stream->sz);
  free(stream);
}
string stream_next_word(mem_stream *stream){
  size_t start = stream->off;
  while(stream->off < stream->sz && 
        !isspace(stream->mem[stream->off])){
    stream->off++;
  }
  string ret = {.mem = stream->mem + start,
                 .sz = stream->off - start};
  while(isspace(stream->mem[stream->off])){
    stream->off++;
  }
  return ret;
}
mem_stream *FILE_to_stream(FILE *f){
  int fd = fileno(f);
  mem_stream *stream = xmalloc(sizeof(mem_stream));
  stream->mem = mmap_file(fd, 0, PROT_READ | PROT_WRITE, &stream->sz);
  if(!stream->mem){
    free(stream);
    return NULL;
  }
  stream->off = 0;
  return stream;
}
*/
uint32_t strspn_table(const uint8_t *str, const uint8_t accept[256]){
  int i=0;

  //accept['\0'] == 0)
  //We assume (or we could force by uncommenting above) that '\0' is not
  //in the accept set, this means we'll always return the length of the
  //string if all characters are in the accept set
  while(1){
    if(!accept[str[i]]){return i;}
    if(!accept[str[i+1]]){return i+1;}
    if(!accept[str[i+2]]){return i+2;}
    if(!accept[str[i+3]]){return i+3;}
    i+=4;
  }
}
uint32_t strcspn_table(const uint8_t *str, const uint8_t reject[256]){
  int i=0;
  reject['\0'] == 1;//force the null byte to be in the reject set
  while(1){
    if(reject[str[i]]){return i;}
    if(reject[str[i+1]]){return i+1;}
    if(reject[str[i+2]]){return i+2;}
    if(reject[str[i+3]]){return i+3;}
    i+=4;
  }
}
uint32_t memspn_table(const uint8_t *str, uint32_t len,
                      const uint8_t accept[256]){
  unsigned int i=0;
  //this is for speed, but I'm not sure how much it's worth it
  while(i+4 < len){
    if(!accept[str[i]]){return i;}
    if(!accept[str[i+1]]){return i+1;}
    if(!accept[str[i+2]]){return i+2;}
    if(!accept[str[i+3]]){return i+3;}
    i+=4;
  }
  switch(len-i){
    case 3: if(!accept[str[i]]){return i;} i++;
    case 2: if(!accept[str[i]]){return i;} i++;
    case 1: if(!accept[str[i]]){return i;}
  }
  return len;
}
uint32_t memcspn_table(const uint8_t *str, uint32_t len,
                       const uint8_t reject[256]){
  unsigned int i=0;
  while(i+4 < len){
    if(reject[str[i]]){return i;}
    if(reject[str[i+1]]){return i+1;}
    if(reject[str[i+2]]){return i+2;}
    if(reject[str[i+3]]){return i+3;}
    i+=4;
  }
  switch(len-i){
    case 3: if(reject[str[i]]){return i;} i++;
    case 2: if(reject[str[i]]){return i;} i++;
    case 1: if(reject[str[i]]){return i;}
  }
  return len;
}
uint32_t memspn(const uint8_t *buf, uint32_t len,
                const uint8_t *accept, uint32_t len2){
  uint8_t bytes[256] = {0};
  unsigned int i;
  for(i = 0; i < len2; i++){
    bytes[accept[i]] = 1;
  }
  return memspn_table(buf, len, bytes);
}
uint32_t memcspn(const uint8_t *buf, uint32_t len,
                 const uint8_t *reject, uint32_t len2){
  uint8_t bytes[256] = {0};
  unsigned int i;
  for(i = 0; i < len2; i++){
    bytes[reject[i]] = 1;
  }
  return memcspn_table(buf, len, bytes);
}
void *memdup(const void *src, size_t sz){
  uint8_t *dest = xmalloc(sz);
  memcpy(dest, src, sz);
  return dest;
}

struct queue_node {
  struct queue_node *next;
  struct queue_node *prev;
  void *data;
};
struct queue* make_queue(void){
  struct queue *q = zmalloc(sizeof(struct queue));
  return q;
}
void queue_push(struct queue *q, void *data){
  struct queue_node *node = xmalloc(sizeof(struct queue_node));
  node->next = q->head;
  node->prev = NULL;
  node->data = data;
  if(q->tail == NULL){
    q->tail = node;
  }
  if(q->head){
    q->head->prev = node;
  }
  q->head = node;

};
void *queue_pop(struct queue *q){
  struct queue_node *node = q->tail;
  void *ret = NULL;
  if(node){
    if(q->head == q->tail){
      void *ret = node->data;
      q->head = q->tail = NULL;
    } else {
      q->tail = node->prev;
      q->tail->next = NULL;
      void *ret = node->data;
    }
  }
  free(node);
  return ret;
}
int queue_is_empty(struct queue *q){
  return !(q->head || q->tail);
}
#define NANO_SCALE 1000000000
#define NANO_SCALE_FLOAT 1e9
#define MICRO_SCALE 1000000
#define MICRO_SCALE_FLOAT 1e6

struct timespec float_to_timespec(double t){
  struct timespec ts;
  double sec, fsec;
  fsec = modf(t, &sec);
    //it'd be best to use trunc here, but this way we avoid linking libm
  ts.tv_sec = sec;
  ts.tv_nsec = fsec * NANO_SCALE_FLOAT;
  return ts;
}
double timespec_to_float(struct timespec t){
  return t.tv_sec + (t.tv_nsec / NANO_SCALE_FLOAT);
}
int64_t float_to_nsec(double t){
  return trunc(t * NANO_SCALE_FLOAT);
}
double nsec_to_float(int64_t t){
  //nanoseconds to fractions of a second
  double fsec = (t % NANO_SCALE)/NANO_SCALE_FLOAT;
  int64_t sec = t/NANO_SCALE;//integer number of seconds
  return (sec + fsec);
}

int64_t timespec_to_nsec(struct timespec t){
  return t.tv_nsec + t.tv_sec * NANO_SCALE;
}
struct timespec nsec_to_timespec(int64_t t){
  struct timespec ts;
  ts.tv_nsec = t % NANO_SCALE;
  ts.tv_sec = t / NANO_SCALE;
  return ts;
}

struct timespec timeval_to_timespec(struct timeval tv){
  struct timespec ts;
  ts.tv_sec = tv.tv_sec;
  ts.tv_nsec = (tv.tv_usec * (NANO_SCALE/MICRO_SCALE));
  return ts;
}

struct timespec get_current_time(){
  struct timespec ts;
#if (defined _POSIX_TIMERS) && (_POSIX_TIMERS > 0)
  clock_gettime(CLOCK_REALTIME, &ts);
#else
  struct timeval tv;
  gettimeofday(&tv, NULL);
  ts =  timeval_to_timespec(tv);
#endif
  return ts;
}

double float_time(){
  return timespec_to_float(get_current_time());
}

int64_t nano_time(){
  return timespec_to_nsec(get_current_time());
}

double float_sleep(double sleep_time){
  struct timespec ts;
  if(sleep_time < 0){//we can't turn back time...
    return 0;
  }
  ts = float_to_timespec(sleep_time);
  int err = nanosleep(&ts, &ts);
  if(err == -1){
    //if we passed an invalid time it's an issue with this function
    assert(errno != EINVAL);
    return timespec_to_float(ts);
  }
  return 0;
}
void float_sleep_full(double sleep_time){
  struct timespec ts, tr;
  if(sleep_time < 0){//we can't turn back time...
    return;
  }
  ts = float_to_timespec(sleep_time);
  int err;
  do {
    errno = 0;
    err = nanosleep(&ts, &ts);
    assert(errno != EINVAL);
  } while (err == -1);
  return;
}
/*
  Basic xorshift algorithm (has some linear artifacts)
  uint32_t xorshift(uint32_t state[4]){
    uint32_t tmp = state[0];
    tmp ^= tmp << 11;
    tmp ^= tmp >> 8;
    memmove(state, state+1, 3*sizeof(uint32_t));//shift the state vector
    state[3] ^= state[3] >> 19;
    state[4] ^= tmp;
    return state[4];
  }
  Slighly better version, still has some issues (also only 64 bit state)
  uint64_t xorshift_star(uint64_t state[1]){
    state[0] ^= state[0] >> 12;
    state[0] ^= state[0] << 25;
    state[0] ^= state[0] >> 27;
    return state[0] * 2685821657736338717ul;
  }
*/
static util_rand_state internal_rand_state;
//The version we use is called xorshift+
uint64_t util_rand_r(util_rand_state state){
  uint64_t x = state.state[0];
  const uint64_t y = state.state[1];
  state.state[0] = y;
  x ^= x << 23;
  state.state[1] = (x ^ y) ^ (x >> 17) ^ (y >> 26);
  return state.state[1] + y;
}
uint64_t util_rand(){
  return util_rand_r(internal_rand_state);
}
/*
  There's an obvious way to do this (util_rand() % (max-min) + min), and
  a correct way to do this, which is what follows.

  Effictively we restrict the range to be a multiple of (max-min) and
  if rand returns a number outside this range we discard it and try again.

  This is slower, but not by all that much. In most cases the first number
  we generate will work, and in the worst case we still have a 50% chance
  of getting a number on the first try;
*/
int64_t util_rand_range_r(int64_t min, int64_t max, util_rand_state state){
  if(min > max){
    errno = ERANGE;
    return 0;
  }
  uint64_t true_max = max-min;
  uint64_t n_buckets = UINT64_MAX/true_max;
  uint64_t limit = n_buckets * true_max;
  uint64_t r;
  do {
    r = util_rand_r(state);
  } while(r > limit);
  return ((r % true_max) + min);
}  
int64_t util_rand_range(int64_t min, int64_t max){
  return util_rand_range_r(min, max, internal_rand_state);
}  
//This is 2^-64
static const double int_to_float_multiplier =  (1.0/18446744073709551616.0);
double util_drand_r(util_rand_state state){
  return util_rand_r(state)*int_to_float_multiplier;
}
double util_drand(void){
  return util_drand_r(internal_rand_state);
}
util_rand_state util_get_rand_state(void){
  util_rand_state ret = internal_rand_state;
  return ret;
}
util_rand_state util_set_rand_state(util_rand_state state){
  util_rand_state ret = internal_rand_state;
  internal_rand_state = state;
  return ret;
}
util_rand_state util_auto_rand_state(void){
  util_rand_state state;
#if (defined USE_SEED_URANDOM) && (USE_SEED_URANDOM > 0)
  int fd = open("/dev/urandom", O_RDWR);
  if(fd > 0){
    int err = read(fd, state.state, 2*sizeof(uint64_t));
    if(err == 16){
      return state;
    }
    close(fd);
  }
#endif
#if (defined USE_SEED_TIME) && (USE_SEED_TIME > 0)
  struct timespec ts = get_current_time();
  state.state[0] =  ts.tv_sec;
  state.state[1] = ts.tv_nsec;
  return state;
#endif
//It's better than leaving it 0
  state.state[0] = 0x8a5cd789635d2dffUL;
  state.state[1] = 0x121fd2155c472f96UL;
  return state;
}
void util_srand(uint64_t a, uint64_t b){
  internal_rand_state.state[0] = a;
  internal_rand_state.state[1] = b;
  return;
}
void util_srand_auto(){
  util_rand_state tmp = util_auto_rand_state();
  internal_rand_state = tmp;
  return;
}    
void shuffle_array(void **arr, size_t n){
  if(n <= 1){//avoid underflow if n==1
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
/*
  Could add this, effectively fills an array by setting successive
  elements with the results of calling fn, then shuffles it, but only
  loops over the data once.
  void** rand_array(size_t n, void*(*fn)(void*), void *userdata){
    void **ret = xmalloc(n*sizeof(void*));
    size_t i;
    for(i=0;i<n;i++){
      size_t j = util_rand_range(0, i);
      SWAP(ret[i], ret[j]);
      a[j] = fn(userdata);
    }
*/

//need to do a check for start > stop, or start < 0 and stop == 0,
//both of which should work, but currently don't
int *iota(int start, int stop, int step){
  if(stop == 0 && step == 0){
    stop = start;
    start = 0;
  }
  if(step == 0){
    step = SIGNBIT(stop - start);
  }
  int count = (stop-start)/step;
  int *ret = xmalloc(count*sizeof(int));
  int i, x;
  for(i = 0, x = start; i<count; i++, x += step){
    ret[i] = x;
  }
  return ret;
}

static const char *nibble_strings[16] =
  {"0000","0001","0010","0011","0100","0101","0111","1000",
   "0111","1000","1001","1010","1011","1100","1101","1111"};
char* binary_int_to_string(uint64_t x, int bitdepth, char *buf){
  int i;
  char *ret = buf;
  for(i=bitdepth-3;i>3;i-=4){
    int nibble = ((x & (0xful << i))>>i);
    memcpy(buf, nibble_strings[nibble],4);
    buf+=4;
  }
  switch(i){
    case 3:
      *buf++ = (x & 4 ? '1' : '0');
    case 2:
      *buf++ = (x & 2 ? '1' : '0');
    case 1:
      *buf++ = (x & 1 ? '1' : '0');
    case 0:
      *buf = '\0';
  }
  return ret;
}
#define is_binary_digit(c) (c == '0' || c == '1')
#define is_oct_digit(c) (c >= '0' && c <= '8')
static const uint8_t valid_digits[] =
  "0123456789AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz";
//max_length_per_base[n] is the length of the longest string in base n
//than can possibly fit in a 64 bit integer
//the formula for m bits in base n is just ceil(m/lg(n))
static int max_length_per_base[35] =
  {64, 41, 32, 28, 25, 23, 22, 21, 20, 19, 18, 18,
   17, 17, 16, 16, 16, 16, 15, 15, 15, 15, 14, 14,
   14, 14 ,14, 14, 14, 13, 13, 13, 13, 13, 13};

static inline int char_to_number(char c){
  if(isdigit(c)){
    return c-0x30;
  } else {
    /*
      in a number a=0xa-z=0x24, the ascii codes for a-z are 0x61-0x7A
      0x56 is just 0x61 - 0xa/0x7A-0x24
    */
    return DOWNCASE_ASCII(c)-0x56;
  }
}
long parse_integer(char *str, char **endptr,
                   int len, int base, int *err){
  if(len == 0){
    len = strlen(str);
  }
  //since signed overflow is undefined and I don't really
  //want to use gmp for this, use an unsigned long to hold the value
  uint64_t num = 0;
  int i = 0, negitive = 0, overflow = 0;
  //ignore leading space
  while(isspace(str[i]) && ++i < len);
  if(i < len){
    if(str[i] == '-'){
      negitive = 1;
      i++;
    }
  }
  if(i == len){
    if(endptr){*endptr = str;}
    return 0;
  }
#define TO_NUMBER(start, end, base)                     \
  ({uint64_t number = 0, next = 0;                      \
    int j;                                              \
    for(j = start; j < end; j++){                       \
      next = (number * base) + char_to_number(str[j]);  \
      if(next < number){/*overflow*/                    \
        overflow = j;                                   \
        break;                                          \
      } else {                                          \
        number = next;                                  \
      }                                                 \
    }                                                   \
    number;})
//I'm pretty sure this will cause an error if the string is something like
//0x abcdefg. It should be read as a 0, but I'm not sure what will happen
  if(base == 0 && ((i+1) < len)){
    if(str[i] == '0' && str[i+1] == 'x'){
      base = 16;
      i+=2;
    } else if(str[i] == '0' && str[i+1] == 'o'){
      base = 8;
      i+=2;
    } else if(str[i] == '0' && str[i+1] == 'b'){
      base = 2;
      i+=2;
    } else {
      base = 10;
    }
  }
  while(str[i] == '0' && ++i < len);//read leading zeros
  if(i == len){
    if(endptr){*endptr = (str+i);}
    return 0;
  }
   /*
    Use special cases for 2, 8, 10, and 16 to speed them up.
    Multiplies in base 2, 8, or 16 become shifts, and for x86 at least
    a multiply by 10 becomes two lea instructions.
   */
#define DO_CASE(base)                                                   \
  if(i <= max_length_per_base[base]){                                   \
    num = TO_NUMBER(start, i, base);                                    \
  }                                                                     \
  if(i > max_length_per_base[base] || overflow > 0 || num>LONG_MAX){    \
    *err = errno = ERANGE;                                              \
    *endptr = (str+i);                                                  \
    return (negitive ? LONG_MIN : LONG_MAX);                            \
  }
  int start = i;
  if(base == 10){
    while(isdigit(str[i]) && ++i < len);
    DO_CASE(base);
  } else if(base == 16){
    while(isxdigit(str[i]) && ++i < len);
    DO_CASE(base);
  } else if(base == 8){
    while(is_oct_digit(str[i]) && ++i < len);
    DO_CASE(base);
  } else if(base == 2){
    while(is_binary_digit(str[i]) && ++i < len);
    DO_CASE(base);
  } else if(base < 10){
    i = memspn((uint8_t*)str + i, len - i, valid_digits, base);
    DO_CASE(base);
  } else {
    i = memspn((uint8_t*)str + i, len -i, valid_digits, 10 + (base-10)*2);
    DO_CASE(base);
  }
  if(endptr){*endptr = (str + i);}
  return (long)(negitive ? -num : num);
}
//TEMPORARY HACK
//#include "svector.c"
