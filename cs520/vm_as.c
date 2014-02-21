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
/* C code produced by gperf version 3.0.4 */
/* Command-line: gperf -G instructions  */
/* Computed positions: -k'1-2,$' */
#define TOTAL_KEYWORDS 26
#define MIN_WORD_LENGTH 3
#define MAX_WORD_LENGTH 7
#define MIN_HASH_VALUE 3
#define MAX_HASH_VALUE 64
/* maximum key range = 62, duplicates = 0 */
static inline unsigned int hash (register const char *str,
                                 register unsigned int len){
  static unsigned char asso_values[] =
    {
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 30,  0, 20,
       5, 10,  5,  5, 30,  0, 20, 65,  0,  0,
      10,  3,  0,  5,  0, 10,  0, 15, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
      65, 65, 65, 65, 65, 65
    };
  return len + asso_values[(unsigned char)str[1]] +
    asso_values[(unsigned char)str[0]] + asso_values[(unsigned char)str[len - 1]];
}

static const char * wordlist[] =
  {
    "", "", "",
    "blt",//3
    "", "",
    "pop",//6
    "",
    "bgt",//8
    "divi",//9
    "ldimm",//10
    "ldaddr",//11
    "load",//12
    "ret",//13
    "divf",//14
    "ldind",//15
    "", "",
    "beq",//18
    "muli",//19
    "stind",//20
    "", "",
    "jmp",//23
    "mulf",//24
    "store",//25
    "getpid",//26
    "", "",
    "subi",//29
    "getpn",//30
    "",
    "cmpxchg",//32
    "",
    "subf",//34
    "", "", "", "",
    "addi",//39
    "", "", "", "",
    "addf",//44
    "", "", "", "",
    "push",//49
    "", "", "", "",
    "call",//54
    "", "", "", "", "", "", "", "", "",
    "halt"//64
  };
//be careful with this as it returns from whatever function it's used in
//if there's anything but whitespace in str
//this should probably print an error message
#define check_rest_of_line(str)                 \
  while(*str){                                  \
    if(!__builtin_isspace(*instr++)){           \
      return -1;                                \
    }                                           \
  }
//for all the assemble_format<N> instr should point to the first character
//following the opcode
static inline vm_word assemble_format1(int key,const char *instr){
  if(check_rest_of_line(instr)<0){
    return -1;
  }
  vm_op retval=0;
  if(key == 13){//ret
    retval.op.op=0x10;
  } else if (key==64){//halt
    retval.op.op==0x00;
  }
  return retval.bits;
}
static inline vm_word assemble_format2(int key,const char *instr){
  char *endptr;
  uint32_t addr;
  int64_t lit_addr=strtol(instr,&endptr,0);
  if(is_space(*endptr)){
    check_rest_of_line(endptr);
    if(lit_addr>>20 || lit_addr<0){
      fprintf(stderr,"Invaild constant address %#0lx",lit_addr);
      return -1;
    }
  } else {
    //lookup symbol;
  }
  vm_op retval=0;
  if(key == 23){//jmp
    retval.op_addr.op=0x14;
  } else if(key == 54){//call
    retval.op_addr.op=0x0f;
  }
  retval.op_addr.addr=addr;
  return retval
}
static inline vm_word assemble_format3(int key,char *instr){
  while(is_space(*instr)){instr++;}
  int reg;
  if(*instr != 'r'){
    if(*instr++ == 'f' && *instr++ == 'p'){
      reg=13;
    } else if (*instr++ == 's' && *instr++ == 'p'){
      ret=14;
    } else if (*instr++ == 'p' && *instr++ == 'c'){
      reg=15;
    } else {
      goto INVALID_REG;
    }    
  } else {
    if(*instr == '1'){
      //i'm a little bit dissapointed ++instr++ doesn't work
      reg=10+((*(++instr)++)-0x30);
    } else {
      reg=*instr++-0x30;
    }
    if(reg>12){
      goto INVALID_REG;
    }
  }  
}
//if there were more than 32 instructions I might do this differently
//but since there are so few I can afford to do this slowly
vm_word assemble_instruction(const char *instr,int len){
  char *first_space=memchr(instr,' ',len);
  uint32_t opcode_len;
  if(!first_space){
    opcode_len=len;//possibly valid for halt or ret
  } else {
    opcode_len=first_space-instr;
  }
  uint32_t key=hash(instr,opcode_len);
  if (key <= MAX_HASH_VALUE && key >= 0){
    register const char *s = wordlist[key];
    if (!(*str == *s && !strcmp (str + 1, s + 1))){
      goto INVALID_INSTRUCTION;
    }
  }
  switch(
 INVALID_INSTRUCTION:
