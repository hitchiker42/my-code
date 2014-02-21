#include "vm.h"
//I can't concatenate stuff
#include "cord.h"
//quick little hash function, to hash lables and such
#define offset_basis_32 2166136261
#define offset_basis_64 14695981039346656037UL
#define fnv_prime_32 16777619
#define fnv_prime_64 1099511628211UL
static uint64_t fnv_hash(const void *key,int keylen){
  const uint8_t *raw_data=(const uint8_t *)key;
  int i;
  uint64_t hash=offset_basis_64;
  for(i=0; i < keylen; i++){
    hash = (hash ^ raw_data[i])*fnv_prime_64;
  }
  return hash;
}
//laziest symbol table ever
struct symbol_table_entry {
  const char *label;
  vm_word addr;
  struct symbol_table_entry *next;
};
typedef struct symbol_table_entry *symref;
typedef struct symbol_table_entry *symbol_table;
#define PUSH(sym,sym_table)                     \
  (sym->next=sym_table;sym_table=sym;)
FILE *vm_preprocess(FILE *input){
  symbol_table ext_symbols=xmalloc(sizeof(struct symbol_table_entry));
  symbol_table internal_symbols=xmalloc(sizeof(struct symbol_table_entry));
  char tmpfile_name[10]={'a','s','m','-','X','X','X','X','X','X'};
  int temp_fd=mkstemp(tmpfile_name);
  size_t line_buf_size=128;  
  ssize_t line_size;
  char *line_buf=xmalloc(line_size);//should be enough for most
  char *out_buf=xmalloc(line_size);
  char *char_ptr,*comment_ptr,*end_ptr,*colon_ptr;
  vm_word addr=0;//current adress
  //lines, but getline will realloc it if necessary
  while(line_size=getline(*line_buf,line_buf_size,input)>0){
    char_ptr=line_buf;
    end_ptr=char_ptr+line_size;
    //strip leading ws and comment lines
    while(is_blank(*char_ptr++));//strip leading whitespace
    if(!(*char_ptr)){//blank line
      continue;
    } else if(*char_ptr == '#'){//deal with comment lines quickly
      continue;
    }
    //strip rest of line following a non full line comment
    comment_ptr=memchr(char_ptr,'#',end_ptr-char_ptr);
    if(comment_ptr){
      memset(comment_ptr,'\0',end_ptr-comment_ptr);
      end_ptr=comment_ptr;
    }
    //check for directives
    switch(*char_ptr){
      case 'i':
        if(!memcmp(char_ptr),"import",sizeof("import")){
          //get lable, check length and push onto external labels stack
        }
    colon_ptr=memchr(char_ptr,':',end_ptr-char_ptr);
    if(!colon_ptr){
      if(write(temp_fd,char_ptr,(end_ptr-char_ptr))<0){
        exit(EXIT_FAILURE);
      }
      
}
vm_word assemble_instruction(const char *instr,int len){
}
