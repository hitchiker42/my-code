#ifndef _CODEGEN_H_
#define _CODEGEN_H_
#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Transforms/PassManagerBuilder.h>
#include "parser.h"
#include "xmalloc.h"
#include "uthash.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <signal.h>
typedef struct{
  char* name;
  LLVMValueRef value;
  UT_hash_handle hh;
} LLVMVar;
/*unhygenic macro, relies on an existing hash table named symbolTable*/
#define addToSyntaxTable(/*LLVMVar*/ Var)\
  HASH_ADD_KEYPTR(hh, symbolTable, Var->name, strlen(Var->name), Var)
//may not work, not sure
LLVMValueRef GenerateValFromName(ExprAST Var);
LLVMValueRef GenerateBinaryExpr(ExprAST op);
LLVMValueRef GenerateFxnCall(ExprAST Call);
LLVMValueRef Codegen(ExprAST expr);
LLVMValueRef GeneratePrototype(Prototype *proto);
LLVMValueRef GenerateFunction(FunctionAST *fxn);
LLVMValueRef ErrorV(const char *Str);
#endif
