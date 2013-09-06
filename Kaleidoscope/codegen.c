#include "codegen.h"
//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//
jmp_buf proto,top_level,definition;
LLVMModuleRef MainModule;
LLVMBuilderRef Builder;
LLVMExecutionEngineRef JITEngine;
LLVMPassManagerRef OurFPM;
LLVMExecutionEngineRef JITEngine;
LLVMPassManagerRef OurFPM;
LLVMVar* symbolTable = NULL;
LLVMVar* LookupSymbol(const char* name){
  LLVMVar *temp;
  HASH_FIND_STR(symbolTable,name,temp);
  return temp;
}
LLVMValueRef ErrorV(const char *Str) {
  fputs(Str,stderr);
  return 0;
}
LLVMValueRef Codegen(ExprAST expr) {
  switch(expr.Tag){
    case Real:
      return LLVMConstReal(LLVMDoubleType(),expr.Expr.real.dblVal);
    case Name:
      return GenerateValFromName(expr);
    case Op:
      return GenerateBinaryExpr(expr);
    case fxnCall:
      return GenerateFxnCall(expr);
    default :
      return ErrorV("Invalid Tag");
  }
}
LLVMValueRef GenerateValFromName(ExprAST Var){
  if (Var.Tag != Name){
    return ErrorV("Expected a Variable Name");
  }
  LLVMVar* temp = LookupSymbol(Var.Expr.Name);
  LLVMValueRef  V = temp->value;
  return V ? V : ErrorV("Unknown variable name");
}

LLVMValueRef GenerateBinaryExpr(ExprAST op) {
  if (op.Tag != Op || op.Expr.BinOp == NULL){
    return ErrorV("Expected a Binary Operator");
  }
  LLVMValueRef L = Codegen(op.Expr.BinOp->LHS);
  LLVMValueRef R = Codegen(op.Expr.BinOp->RHS);
  if (L == NULL || R == NULL){return NULL;}
  
  switch (op.Expr.BinOp->op) {
    case '+': return LLVMBuildFAdd(Builder,L, R, "addtmp");
    case '-': return LLVMBuildFSub(Builder,L, R, "subtmp");
    case '*': return LLVMBuildFMul(Builder,L, R, "multmp");
    case '<':
      L = LLVMBuildFCmp(Builder,LLVMRealULT, R, L, "cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      return LLVMBuildUIToFP(Builder, L, LLVMDoubleType(),"booltmp");
    default: return ErrorV("invalid binary operator");
  }
}

LLVMValueRef GenerateFxnCall(ExprAST Call) {
  if (Call.Tag != fxnCall){
    return ErrorV("Expected a function Call");
  }
  int numArgs = Call.Expr.fxnCall->Argc;
  // Look up the name in the global module table.
  LLVMValueRef CalleeFxn = 
    LLVMGetNamedFunction(MainModule,Call.Expr.fxnCall->Name);
  if (CalleeFxn == NULL){
    return ErrorV("Unknown function referenced");
  }
  // If argument mismatch error.
  if ((LLVMCountParams(CalleeFxn)) != numArgs){
    fprintf(stderr,"%d args passed, wanted%d",(LLVMCountParams(CalleeFxn)),numArgs);
    return ErrorV("Incorrect # arguments passed");
  }
  LLVMValueRef* Args=xmalloc(numArgs*sizeof(LLVMValueRef));//not sure about size
  LLVMGetParams(CalleeFxn,Args);
  unsigned int i,e=numArgs;
  for (i=0; i!=e; i++) {
    Args[i]=Codegen(Call.Expr.fxnCall->args[i]);
    if (Args[i] == NULL) {return 0;}
  }
  return LLVMBuildCall(Builder,CalleeFxn, Args, numArgs,"calltmp");
}
LLVMValueRef GeneratePrototype(Prototype *prototype) {
  // Make the function type:  double(double,double) etc.
  int numArgs=prototype->Argc,i;
  LLVMValueRef Params[numArgs],F;
  LLVMTypeRef ArgTypes[numArgs],DoubleRef = LLVMDoubleType(),FT;
  //for now all args are doubles;
  for (i=0;i<numArgs;i++){
    ArgTypes[i]=DoubleRef;
  }
  FT = LLVMFunctionType(DoubleRef,ArgTypes,numArgs,0);  
  // If F conflicted, there was already something named 'Name'.  If it has a
  // body, don't allow redefinition or reextern.
  F=NULL;
  signal(SIGSEGV,SIG_IGN);
  F=LLVMGetNamedFunction(MainModule,prototype->Name);
  if (F != NULL) {
    // If F already has a body, reject this.
    if (LLVMCountBasicBlocks(F) > 0) {
      ErrorV("redefinition of function");
      return NULL;
    }    
    // If F took a different number of args, reject.
    if (LLVMCountParams/*or LLVMGetNumOperands*/(F) != numArgs) {
      ErrorV("redefinition of function with different # args");
      return 0;
    }
  } else {
    F=LLVMAddFunction(MainModule,prototype->Name,FT);
  }
  signal(SIGSEGV,SIG_DFL);
  // Set names for all arguments.
  LLVMGetParams(F,Params);
  LLVMVar* curParam;
  for(i=0;i<numArgs;i++){
    curParam = xmalloc(sizeof(LLVMVar));//create variable
    curParam->value = Params[i];//set variable value
    curParam->name=prototype->Argv[i];//set variable name
    LLVMSetValueName(curParam->value,curParam->name);//this seems redundant    
    addToSyntaxTable(curParam);//add to syntax table, duh
  }
  return F;
}

LLVMValueRef GenerateFunction(FunctionAST *fxn){
  LLVMValueRef RetVal,NewFxn = GeneratePrototype(&(fxn->Proto));
  if (NewFxn == NULL){
    return NULL;
  }
  // Create a new basic block to start insertion into.
  LLVMBasicBlockRef BB = LLVMAppendBasicBlock(NewFxn,fxn->Proto.Name);
  LLVMPositionBuilderAtEnd(Builder,BB);
  RetVal = Codegen(fxn->Body);
  if (RetVal != NULL) {
    // what do I do with this value
    LLVMBuildRet(Builder,RetVal);
    // Validate the generated code, checking for consistency.
    if (LLVMVerifyFunction(NewFxn,LLVMPrintMessageAction)){
      return ErrorV("Invalid Function");} 
    LLVMRunFunctionPassManager(OurFPM,NewFxn);
    return NewFxn;
  }
  LLVMDeleteFunction(NewFxn);
  return ErrorV("Body of function is NULL");
}
//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

/// putchard - putchar that takes a double and returns 0.
double putchard(double X) {
  putchar((char)X);
  return 0;
}
//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//
FunctionAST*  ParseDefn() {
  if (setjmp(definition)){
    return 0;
  } else {
    FunctionAST* curFxn = xmalloc(sizeof(FunctionAST));
    *curFxn = ParseDefinition(definition);
    return curFxn;
  }
}
Prototype*  ParseProto() {
  if (setjmp(proto)){
    return 0;
  } else {
    Prototype* curProto = xmalloc(sizeof(Prototype));
    *curProto = ParsePrototype(proto);
    return curProto;
  }
}
FunctionAST*  ParseTopLevel() {
  if (setjmp(top_level)){
    return 0;
  } else {
    FunctionAST* curFxn = xmalloc(sizeof(FunctionAST));
    *curFxn = ParseTopLevelExpr(top_level);
    return curFxn;
  }
}
void HandleDefinition() {
  FunctionAST* F = ParseDefn();
  if (F){
    LLVMValueRef LF = GenerateFunction(F);
    free(F);
    if (LF){
      fprintf(stderr, "Read function definition:");
      LLVMDumpValue(LF);
    } else {
      getNextToken();
    }
  } else {
    getNextToken();
  }
}
//needs some work still
void HandleExtern() {
  getNextToken();
  Prototype* P = ParseProto();
  if (P){
    LLVMValueRef LP = GeneratePrototype(P);
    free(P);
    if (LP){
      fprintf(stderr, "Read extern: ");
      LLVMDumpValue(LP);
    } else {
      getNextToken();
    } 
  } else {
    getNextToken();
  }
}

void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  FunctionAST *F = ParseTopLevel();
  if(F){
    LLVMValueRef LF = GenerateFunction(F);
    //free(F);
    if(LF){
      fprintf(stderr, "Read top-level expression:");
      LLVMDumpValue(LF);
      //toplevel expr, function is void
      LLVMGenericValueRef void_star=LLVMCreateGenericValueOfPointer(NULL);
      double retval = 
        LLVMGenericValueToFloat(LLVMDoubleType(),
                                LLVMRunFunction(JITEngine,LF,0,&void_star));
      fprintf(stderr,"Evaluated to %f\n",retval);
    } else {
      getNextToken();
    } 
  } else {
    getNextToken();
  }
}

/// top ::= definition | external | expression | ';'
//todo add readline support
static void MainLoop() {
  signal(SIGSEGV,SIG_IGN);
  while (1) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
      case tok_eof:    return;
      case ';':        getNextToken(); break;  // ignore top-level semicolons.
      case tok_def:    HandleDefinition(); break;
      case tok_extern: HandleExtern(); break;
      default:         HandleTopLevelExpression(); break;
    }
  }
}
//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//
int main() {
  char* error = xmalloc(100*sizeof(char));
  //struct LLVMMCJITCompilerOptions  options;
  LLVMPassManagerBuilderRef FPMBuilder = LLVMPassManagerBuilderCreate();
  JITEngine=xmalloc(sizeof(LLVMExecutionEngineRef));
  OurFPM=xmalloc(sizeof(LLVMPassManagerRef));
  //create module & builder
  MainModule = LLVMModuleCreateWithName("my cool jit");
  Builder = LLVMCreateBuilder();
  //create function pass manager for optimizations
  OurFPM = LLVMCreateFunctionPassManagerForModule(MainModule);
  LLVMPassManagerBuilderSetOptLevel(FPMBuilder,2);
  LLVMPassManagerBuilderPopulateFunctionPassManager(FPMBuilder,OurFPM);
  /*LLVMInitializeMCJITCompilerOptions(&options,2);
  options.OptLevel=2;
  options.EnableFastISel=0;
  LLVMCreateMCJITCompilerForModule(&JITEngine,MainModule,&options,2,&error);*/
  // LLVMCreateMCJITCompilerForModule(&JITEngine,MainModule,0,0,&error);
  LLVMCreateInterpreterForModule(&JITEngine,MainModule,&error);
  LLVMInitializeFunctionPassManager(OurFPM);
  LLVMInitializeNativeTarget();
  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();
  // Run the main "interpreter loop" now.
  MainLoop();
  // Print out all of the generated code.
  LLVMDumpModule(MainModule);
  return 0;
}
