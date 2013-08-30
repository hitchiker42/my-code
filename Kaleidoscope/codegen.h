#include <llvm-c/core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Initialization.h>
#include "parser.h"
#include <uthash.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
typedef struct{
  char* name;
  LLVMValueRef value;
  UT_hash_handle hh;
} LLVMVar;
extern LLVMVar* symbolTable;
/*unhygenic macro, relies on an existing hash table named symbolTable*/
#define addToSyntaxTable(/*LLVMVar*/ Var)\
  HASH_ADD_KEYPTR(hh, symbolTable, name, strlen(Var->name), Var)
//may not work, not sure
inline LLVMVar* LookupSymbol(char* name){
  LLVMVar *temp;
  HASH_FIND_STR(symbolTable,name,temp);
  return temp;
}
#ifndef __CODEGEN_H__
