/* three steps, 
   1. preprocess input file: strip comments / whitespace,process directives,
                             and built up lists of imported/exported symbols
   2. validate symbols: insure all exported symbols exist, make sure no symbols
      are defined twice, no symbols are imported and exported,etc...
   3. assemble file
 */
#include "vm.h"
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
void *vm_preprocess(FILE *input){
  symbol_table ext_symbols=xmalloc(sizeof(struct symbol_table_entry));
  symbol_table internal_symbols=xmalloc(sizeof(struct symbol_table_entry));
  size_t line_buf_size=128;
  ssize_t line_size;
  char *line_buf=xmalloc_atomic(line_buf_size);
  char *out_buf=xmalloc_atomic(line_buf_size);
  char *char_ptr,*comment_ptr,*end_ptr,*colon_ptr;
  vm_word addr=0;//current adress
  //lines, but getline will realloc it if necessary

  while(1){
  LOOP:
    line_size=getline(*line_buf,line_buf_size,input);
    if(line_size<0){break;}
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
    //there are 4: word <const>, alloc <len>, import <sym>, export <sym>
    switch(*char_ptr){
      case 'a':
        if(!memcmp(char_ptr,"alloc",5)){
          //reserve len words in the objcode section
          uint32_t len =strtol(char_ptr+5,NULL,0);
          addr+=len;
          goto LOOP;
        } else {break;}
      case 'e':
        if(!memcmp(char_ptr,"export",6)){
          //export symbol
          goto LOOP;
        } else {break;}
      case 'i':
        if(!memcmp(char_ptr,"import",6)){
          //add imported symbol to insymbols
          goto LOOP;
        } else {break;}

      case 'w':
        if(!memcmp(char_ptr,"word",4)){          
          //read a constant and stick it int the objcode
          int64_t num=strtol(char_ptr+4,NULL,0);
          if(num != (int32_t)num){
            //constant too big;
          }
          addr++;
          goto LOOP;
        } else {break;}
    }
    colon_ptr=memchr(char_ptr,':',end_ptr-char_ptr);
    if(colon_ptr){
      //get label, check length and push onto external labels stack
    } else {
      //normal instruction
      addr++;
    }
  }
}
//if there were more than 32 instructions I might do this differently
//but since there are so few I can afford to do this slowly
vm_word assemble_instruction(const char *instr,int len){
  switch(*instr){//basically make a trie out of switches
    case 'a'://addf/addi
      if(!strcmp(instr,"add",3)){
      } else {
        goto INVALID_INSTR;
      }
    case 'b'://blt/beq/bgt
    case 'c'://call/cmpxchg
    case 'd'://divi/divf
      if(!strcmp(instr,"div",3)){
      } else {
        goto INVALID_INSTR;
      }
    case 'g'://getpid/getpn
    case 'h'://halt
    case 'j'://jmp
    case 'l'://load/ldimm/ldaddr/ldind
    case 'm'://muli/mulf
      if(!strcmp(instr,"mul",3)){
      } else {
        goto INVALID_INSTR;
      }
    case 'p'://pop/push
    case 's'://store/stind/subi/subf
      if(*(instr+1) == 't'){
        //might as well just do the strcmps
        if(!strcmp(instr,"store",5)){
        } else if (!strcmp(instr,"stind",5)){
        } else {
          goto INVALID_INSTR;
        }
      } else if(!strcmp(instr,"sub",3)){
      } else {
        goto INVALID_INSTR;
      }
    default:{
    INVALID_INSTR:
      fprintf(stderr,"Invalid instruction %s",instr);
      return -1;
    }
  }
}
