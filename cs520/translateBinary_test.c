#include "vm_translate.h"
void translateBinary(char *vm_objfile_name,void *buf_ptr,int64_t len);
#define pow2_roundup(num,multiple) ((uint64_t)(num+multiple-1)&(uint64_t)(~(multiple-1)))
#define round_to_next_page(num) (pow2_roundup(num,sys_pagesize))
void *allocate_executable_buffer(uint64_t *length){
  uint64_t sys_pagesize=sysconf(_SC_PAGESIZE);
  uint64_t real_length=round_to_next_page(*length);//round size to nearest page size
  uint8_t *buf=mmap(NULL,real_length,PROT_READ|PROT_WRITE|PROT_EXEC,
                    MAP_ANONYMOUS|MAP_SHARED,-1,0);
  if(buf == MAP_FAILED){
    perror("Error allocating memory\n");
    return (void*)-1;//a function shouldn't exit the main program by itself
  }
  *length=real_length;
  return buf;
}
int main(int argc,char *argv[]){
  uint64_t length=4096*5;
  uint8_t *buf=allocate_executable_buffer(&length);
  if(length==(void*)-1){
    return -1;
  }
  translateBinary(argv[1],buf,length);
  FILE* outfile;
  if(argv[2]){
    FILE* outfile=fopen(argv[2],"w");
  } else {
    FILE* outfile=fopen("a.out","w");
  }
  if(fwrite(buf,1,length,outfile)==(size_t)-1){
    return -2;
  }
  return 0;
}
