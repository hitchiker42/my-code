int my_strlen(char *str){
  int len=0;
  while(str[len++]);
  return len;
}
void strrev(char *str){
  int len=my_strlen(str);
  char c;
  int i=0;
  while(i<len/2){
    c=str[len-i-1];
    str[len-i-1]=str[i];
    str[i]=c;
  }
}
void write(void *s,unsigned long l){
  asm ("movq %rsi,%rdx\n"
       "movq %rdi,%rsi\n"
       "movq $1,%rax\n"
       "movq $1,%rdi\n"
       "syscall\n");
}


int main(int argc,char **argv){
  int i=0;
  while(i<argc){
    strrev(argv[i++]);
  }
  i=0;
  while(i<argc){
    write(argv[i],my_strlen(argv[i]));
    i++;
  }
  return 0;
}
