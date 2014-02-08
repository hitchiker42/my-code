#define UTF8_MAX_LEN 8 
/*This code is copied from another project of mine (a lisp compilier)
  and hacked to make it fit the assignment*/
//minimal includes so this can stand on it's own
#include <wchar.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#define xmalloc(sz)                             \
  ({void *temp=malloc(sz);                      \
    if(!temp && sz){                            \
    raise(SIGUSR1);                             \
    }                                           \
    temp;})
static const int utf8_max[6]={0x7F,0x7FF,0xFFFF,0x1FFFFF,0x3FFFFFF,0x7FFFFFFF};
static const int utf8_min[6]={0x9,0x80,0x800,0x10000,0x200000,0x4000000};
static const uint8_t utf8_initial_mask[6]={0x0,0b00011111,0b00001111,
                                           0b00000111,0b00000011,0b00000001};
static const uint8_t utf8_rest_mask=0b00111111;
static const int utf8_len_max=8;//really it's 6, but setting this to 8 makess life eaiser
#define UTF8_LEN_MAX 8
/*
  Simple UTF-8 spec
  bits of    | First      | Last         | Num Bytes | Leading    |
  code point | code point | code point   |           | byte       |
  ----------------------------------------------------------------|
  |    7     |  0x0       |  0x7F        |    1      | 0b0xxxxxxx |
  |    11    |  0x80      |  0x7FF       |    2      | 0b110xxxxx |
  |    16    |  0x800     |  0xFFFF      |    3      | 0b1110xxxx |
  |    21    |  0x10000   |  0x1FFFFF    |    4      | 0b11110xxx |
  |    26    |  0x200000  |  0x3FFFFFF   |    5      | 0b111110xx |
  |    31    |  0x4000000 |  0x7FFFFFFF  |    6      | 0b1111110x |

  All bytes following the leading byte in a multibyte sequence have the form
  0b10xxxxxx
  in any instance the bytes 0b11111110 and 0b11111111 are illegal
 */
//given the first byte in a possible utf8 sequence
//return 
int utf8_char_len(char mb_char);
int utf8_mb_char_len(char mb_char);
//same semantics as mbrtowc
//convert a valid utf-8 sequence of upto size bytes from src
//and store the result in dest, if src contains a possibly valid
//sequence that cannot be resolved in size bytes -2 is returned
//and dest is set to NULL for any other error dest is set to NULL
//errno is set and -1 is returned. otherwise dest is set to the 
//result and the number of bytes used is returned
size_t utf8_decode_char(const char* src, wchar_t *dest, size_t size,int*err_offset);
size_t utf8_encode_char(char* dest, wchar_t src);
typedef struct encode_state utf8_encode_state;
typedef struct decode_state utf8_decode_state;
struct decode_state_simple {//indivual char state
  wchar_t result;//where to store the partial result
  uint8_t state;//0 if in initial state
  uint8_t total_bytes;//total bytes in current char
  uint8_t bytes_remaning;//bytes left in current char
};
struct encode_state {
  //state for the string
  wchar_t *src;
  char *dest;
  int maxchars;//0 to scan untill null
  int src_offset;//current widechar to decode is src[offset]
  int dest_offset;
  int dest_size;
  int resize;
  int complete;
  //no indivual char state
};
struct decode_state {
  //state for the string 
  char *src;
  wchar_t *dest;
  int maxchars;//0 to scan untill null
  int src_offset;//offset in src to the start of the current mbchar
  int dest_offset;//
  //state for indivual chararcter
  uint8_t state;//0 if in initial state
  uint8_t total_bytes;//total bytes in current char
  uint8_t bytes_consumed;//bytes used so far in current char
  wchar_t result;//where to store the partial result
};
//from libutf8
//using binary to make things a bit more clear
#define IS_VALID_CONT_BYTE(byte) ((byte & 0b11000000) == 0b10000000)
#define IS_INVALID_CONT_BYTE(byte) ((byte & 0b11000000) != 0b10000000)
//from the linux kernel
//not sure why I couldn't think to do bsf !val but eh
static inline uint64_t ffz(uint64_t word){
#ifdef __x86_64__
  __asm__("bsf %1,%0"
      : "=r" (word)
      : "r" (~word));
  return word;
#else
  return ffsl(~word);
#endif
}
int utf8_isucs4(wchar_t ch){
    return !(ch & (~((wchar_t)0x7FFFFFFF)))
        && (ch < 0xD800 || ch > 0xDFFF)
        && (ch != 0xFFFE) && (ch != 0xFFFF);
}
int utf8_char_len(char mb_char){
  if(mb_char<0x80){
    return 1;
  } else if (mb_char>=0xFE){//invalid leading byte
    return -1;
  } else {
    return ffz(mb_char)-1;
  }
}
int utf8_mb_char_len(char mb_char){
  if (mb_char>=0xFE){//invalid leading byte
    return -1;
  } else {
    return ffz(mb_char)-1;
  }
}
//internal use only, really unsafe, need to check
//for 0xFE/0xFF and >0x80 yourself
static inline int utf8_char_len_unsafe(char mb_char){
  return 1-ffz(mb_char);
}
/*
  modified quite a bit from libutf8, libutf8 takes a parameter giving the length of dest
  and checks it, which I don't, and the value returned is different, it returns a pointer
  to the dest+bytes_used whereas I just return bytes used
 */
size_t utf8_encode_char(char* dest, wchar_t src){
  if(!dest){
    dest = xmalloc(utf8_len_max);
  }
  if(!utf8_isucs4(src)) {//test if ch is a valid codepoint
    errno = EILSEQ;
    return (size_t)-1;
  }
  int i=0;
  if(src < 0x80) {
    dest[i++] = src;
  } else if(src < 0x800) {
    dest[i++] = 0xC0 | ((src >> 6) & 0x1F);
    dest[i++] = 0x80 | (src & 0x3F);
  } else if(src < 0x10000) {
    dest[i++] = 0xE0 | ((src >> 12) & 0xF);
    dest[i++] = 0x80 | ((src >> 6) & 0x3F);
    dest[i++] = 0x80 | (src & 0x3F);
  } else if(src < 0x200000) {
    dest[i++] = 0xF0 | ((src >> 18) & 0x7);
    dest[i++] = 0x80 | ((src >> 12) & 0x3F);
    dest[i++] = 0x80 | ((src >> 6) & 0x3F);
    dest[i++] = 0x80 | (src & 0x3F);
  } else if(src < 0x4000000) {
    dest[i++] = 0xF8 | ((src >> 24) & 0x3);
    dest[i++] = 0x80 | ((src >> 18) & 0x3F);
    dest[i++] = 0x80 | ((src >> 12) & 0x3F);
    dest[i++] = 0x80 | ((src >> 6) & 0x3F);
    dest[i++] = 0x80 | (src & 0x3F);
  } else {
    dest[i++] = 0xFC | ((src >> 30) & 0x1);
    dest[i++] = 0x80 | ((src >> 24) & 0x3F);
    dest[i++] = 0x80 | ((src >> 18) & 0x3F);
    dest[i++] = 0x80 | ((src >> 12) & 0x3F);
    dest[i++] = 0x80 | ((src >> 6) & 0x3F);
    dest[i++] = 0x80 | (src & 0x3F);
  }
  return i;
}
/*
  modified from libutf8 in a similar way to encode_char
 */
size_t utf8_decode_char(const char* src, wchar_t *dest, size_t size,
                        /*extra arg for this assignment*/int *err_offset){
  int i=0;
  wchar_t retval,min;
  //deal with invaid arguments and ascii chars
  if(!size){
    return (size_t)-2;
  } else if(!src && size) {
    errno = EINVAL;
    return (size_t)-1;
  } else if(src[0]>=0XFE){
    errno=EILSEQ;
    *err_offset=1;
    return (size_t)-1;
  } else if (src[0]<0x80){
    *dest=src[0];
    return 1;
  }
  int needed=ffz(src[0])-1;
  min=utf8_min[needed];
  retval=src[0] & utf8_initial_mask[needed-1];
  while(i++&& --size){//i is first so we can return it regardless
    if(IS_INVALID_CONT_BYTE(src[i])){
      errno = EILSEQ;
      *err_offset=i;
      return (size_t)-1;
    }
    //0x3f == 0b00111111
    /*ex retval = 0b00011111
      retval <<=6 retval = 0b00000111 0b11000000
    //ok I guess this works I'm not 100% sure on how though
     */
    retval <<= 6;
    retval |= src[i] & 0x3F;
  }
  if(retval < min) {
    errno = EILSEQ;
    return (size_t)-1;
  }
  if(needed>size){
    return (size_t)-2;
  }
  *dest=retval;
  return i;
}
//size_t utf8_encode_char(char* dest, wchar_t src){
//decode the next utf-8 char from src and store it in dest,
//current_offset is for reporting the file offset in error messages
//returns the number of bytes read
size_t cs520_encode_wrapper(const wchar_t *src,char *out,int current_offset){
  size_t nbytes=utf8_encode_char(out,*src);
  if((size_t)-1 == nbytes){
    fprintf(stderr,"Invaid UTF-32 sequence found at offset %d\n"
            ,current_offset);
    exit(-1);
  }
  return nbytes;
}
//there was a bunch of more complicated stuff here, using an encode/decode struct
//to decode files/streams in a re-entrent manner, but I don't think I need that
//here
//honestly it's really 6, but 48 bytes is a bit silly
int main(int argc,char *argv[]){
  //argv[1]==input file
  //argv[2]==output file
  int fd_in=open(argv[1],O_RDONLY);
  //int fd_out=open(argv[2],O_CREAT|O_WRONLY|O_TRUNC);
  //  FILE *input_file=fopen(argv[1],"r");
  FILE *output_file=fopen(argv[2],"w");
  if(fd_in == -1){
    fprintf(stderr,"Can not open input file %s for reading\n",argv[1]);
    exit(1);
  } else if(!output_file){
    fprintf(stderr,"Can not open output file %s for writing\n",argv[2]);
    exit(1);
  }
  //fseek(input_file,0,SEEK_END);
  //size_t file_size=ftell(input_file);
  //read input file into an internal buffer
  off_t file_size=lseek(fd_in,0,SEEK_END);
  if((off_t)-1 == file_size){
    perror("Error in lseek");
    exit(1);
  }
  lseek(fd_in,0,SEEK_SET);
  wchar_t *in_buf=xmalloc(file_size);
  size_t buf_size=file_size>>2;
  read(fd_in,in_buf,file_size);
  size_t i=0;
  char *out_buf=xmalloc(UTF8_MAX_LEN);
  size_t nbytes;
  while(i<buf_size){
    memset(out_buf,'\0',UTF8_MAX_LEN);
    nbytes=cs520_encode_wrapper(in_buf+i,out_buf,i);
    fwrite(out_buf,sizeof(char),nbytes,output_file);
    i++;
  }
  return 0;
}
