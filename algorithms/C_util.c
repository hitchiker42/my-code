/*
  Utility C routines
*/
#include "C_util.h"
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <assert.h>
#include <errno.h>
#ifndef DEBUG_PRINTF
#define DEBUG_PRINTF(fmt,args...)               \
  fprintf(stderr,fmt,##args)
#endif
void * __attribute__((weak, malloc)) xmalloc(size_t sz){
  void *mem = malloc(sz);
  if(mem == NULL && sz != 0){
    fprintf(stderr, "out of memory\n");
    raise(SIGABRT);
  }
  return mem;
}
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
  DEBUG_PRINTF("Calling file_len_by_fd\n");
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
  ((char*)retval)[*sz] = EOF;
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
#include "svector.c"
