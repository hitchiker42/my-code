#include "trie.h"
/*
  read a word from infile and store in in w, return the length of the
  word read or -1 on EOF or ERROR. Doesn't append a nul to w.
*/
#define BUF_SIZE 128;
static int next_word(FILE *infile, char **w){
{
  int c = 0;
  unsigned int i = 0, bufsize = BUF_SIZE;
  //since buf-size just moves the stack pointer it we don't
  //call anyother functions or allocate any more memory after
  //allocating buf
  char *bufptr;
  char *buf = xmalloc(bufsize);
  bufptr = buf;
  while(isspace(c = fgetc(infile)));
  do {
    if(i>= bufsize){
      bufsize*=2;
      realloc(buf,bufsize);
    }
    if (isalpha(c)) {
      *bufptr++=c
      i += 1;
    } else if (isspace(c)) {
      break;
    } else {
      fprintf(stderr, "Non-alpha '%c', skipping word\n", c);
      while(!isspace(fgetc(infile)));
      return next_word(infile, w, n); 
    }
  } while ((c = fgetc(infile)) != EOF);
  if (c == EOF){
    free(buf);
    return -1;
  }
  *w = buf;
  return i;
}
