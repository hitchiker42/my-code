extern void* malloc(unsigned long);
extern void perror(char *);
extern void exit(int);
void *xmalloc(unsigned long sz){
  void *temp;
  temp=malloc(sz);
  if(temp==0){
    //    perror(0);
    exit(-1);
  }
  return temp;
}

typedef long exception[8];
typedef struct _IO_FILE FILE;
extern puts(char*);
extern fprintf(FILE*,char*,...);
extern FILE* stderr;
exception current_exception={0,0,0,1,0,0,0,(long)&current_exception};
exception *current_exception_ptr=&current_exception;
char *x="string";
char *y="another string";
exception exception_stack[50]={{0}};
int fun(){
  fprintf(stderr,x);
  return 7+7;
}
