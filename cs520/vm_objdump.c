#include "vm.h"
/* vm objfile format
   Insymbol section length, 32bits
   Outsymbol section length, 32bits
   Object-Code section length 32bits
   Insymbol section
   Outsymbol section
   Object code section
 */
#define file_size(fd)                           \
  ({off_t len=lseek(fd,0,SEEK_END);             \
    lseek(fd,0,SEEK_SET);                       \
    len;})
static void print_header(vm_word in,vm_word out,vm_word obj);
/* Translate the file given by the file descriptor fd into a
   vm_objfile struct and verify that the sum of the header words of the
   file is the same as the file length.
   
   The file descriptor fd is closed, and any errors from library calls
   result in program termination
 */
vm_objfile *verify_file(int fd){
  off_t len=file_size(fd);
  uint8_t *objfile=xmalloc_atomic(sizeof(struct vm_objfile)+len);
  ssize_t nbytes=read(fd,objfile+sizeof(vm_objfile),len);
  if(nbytes == (ssize_t)-1 /*|| nbytes != len*/){
    perror("Error reading from file");
    exit(EXIT_FAILURE);
  }
  if(close(fd) == -1){
    perror("Error closing file");
    exit(EXIT_FAILURE);
  }
  vm_objfile *obj=(void*)objfile;
  obj->objfile=(void*)objfile+sizeof(vm_objfile);
  obj->file_len=(vm_word)(len>>2);
  obj->insymbol_len=obj->objfile[0];
  obj->outsymbol_len=obj->objfile[1];
  obj->objcode_len=obj->objfile[2];
  if((obj->file_len-3) !=
     (obj->insymbol_len + obj->outsymbol_len + obj->objcode_len)){
    fprintf(stderr,
            "Error, object file size %d is not equal to sum of header words\n",
            obj->file_len-3);
    print_header(obj->insymbol_len,obj->outsymbol_len,obj->objcode_len);
    exit(EXIT_FAILURE);
  }
  obj->insymbols=obj->objfile+3;
  obj->outsymbols=obj->insymbols+obj->insymbol_len;
  obj->objcode=obj->outsymbols+obj->outsymbol_len;
  return obj;
}
static inline void print_header(vm_word in,vm_word out,vm_word obj){
  printf("Insymbol Length %d\nOutSymbol Length %d\nObject-Code Length %d\n",
         in,out,obj);
}
static inline void print_instr(vm_word instr){
  vm_op op;
  op.bits=instr;
  uint8_t opcode=op.op.op;//that's pretty great right there
  if(opcode>0x19){
    printf("%x unknown\n",opcode);
  } else {
    printf("%02x %s\n",opcode,instr_names[opcode]);
  }
}
  
int main(int argc,char *argv[]){
  if(!argv[1]){
    fprintf(stderr,"Usage, vm_objdump <objfile>\n");
    return 0;
  }
  int fd = open(argv[1],O_RDONLY);
  if(fd == -1){
    perror("Error opening object file");
    return 1;
  }
  vm_objfile *obj=verify_file(fd);
  print_header(obj->insymbol_len,obj->outsymbol_len,obj->objcode_len);
  vm_word *objcode=obj->objcode;
  int i;
  for(i=0;i<obj->objcode_len;i++){
    print_instr(objcode[i]);
  }
  return 0;
}



