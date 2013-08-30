#include "codegen.h"



//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

LLVMModuleRef MainModule;
LLVMBuilderRef Builder = LLVMCreateBuilderInContex(LLVMGetGlobalContex());
LLVMVar* symbolTable = NULL;

LLVMValueRef ErrorV(const char *Str) { Error(Str); return 0; }

LLVMValueRef Codegen(ExprAST expr) {
  switch(expr.tag){
    case: Number
      return LLVMConstReal(LLVMDoubleType(),expr.Expr.Number);
    case: Name 
  return ConstantFP::get(getGlobalContext(), APFloat(Val));
}

LLVMValueRef GenerateValFromName(ExprAST Var){
  if (Var.Tag != Name){
    return ErrorV("Expected a Variable Name");
  }
  LLVMValueRef  V = (LookupSymbol(Var.Name))->value;
  return V ? V : ErrorV("Unknown variable name");
}

LLVMValueRef GenerateBinaryExpr(ExprAST op) {
  if (op.Tag != Op){
    return ErrorV("Expected a Binary Operator");
  }
  LLVMValueRef L = Codegen(op.Expr->LHS);
  LLVMValueRef R = Codegen(op.Expr->RHS);
  if (L == 0 || R == 0){return 0;}
  
  switch (op.Expr->op) {
    case '+': return LLVMBuildFAdd(Builder,L, R, "addtmp");
    case '-': return LLVMBuildFSub(Builder,L, R, "subtmp");
    case '*': return LLVMBuildFMul(Builder,L, R, "multmp");
    case '<':
      L = LLVMBuildFCmpULT(Builder,LLVMRealULT, R, "cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      return Builder.CreateUIToFP(Builder, L,LLVMDoubleType(),"booltmp");
    default: return ErrorV("invalid binary operator");
  }
}

LLVMValueRef GenerateFxnCall(ExprAST Call) {
  if (Call.Tag != fnxCall){
    return ErrorV("Expected a function Call");
  }
  // Look up the name in the global module table.
  LLVMValueRef CalleeFxn = LLVMGetNamedFunction(MainModule,Call.Expr->Name);
  if (CalleeFxn == 0){
    return ErrorV("Unknown function referenced");
  }
  int numArgs = Call.fxnCall->Argv;
  // If argument mismatch error.
  if (LLVMCountParams(CalleeFxn) != numArgs){
    return ErrorV("Incorrect # arguments passed");
  }
  LLVMValueRef* Args=xmalloc(numArgs*sizeof(LLVMValueRef));//not sure on the size
  unsigned int i,e;
  for (i=0,e=numArgs;i != e; i++) {
    Args[i]=Codegen(Args[i]));
    if (ArgsV.back() == 0) return 0;
  }
  
  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

LLVMValueRef GeneratePrototype() {
  // Make the function type:  double(double,double) etc.
  LLVMTypeRef* ArgTypes;
  std::vector<Type*> Doubles(Args.size(),
                             Type::getDoubleTy(getGlobalContext()));
  FunctionType *FT = FunctionType::get(Type::getDoubleTy(getGlobalContext()),
                                       Doubles, false);
  
  Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule);
  
  // If F conflicted, there was already something named 'Name'.  If it has a
  // body, don't allow redefinition or reextern.
  if (F->getName() != Name) {
    // Delete the one we just made and get the existing one.
    F->eraseFromParent();
    F = TheModule->getFunction(Name);
    
    // If F already has a body, reject this.
    if (!F->empty()) {
      ErrorF("redefinition of function");
      return 0;
    }
    
    // If F took a different number of args, reject.
    if (F->arg_size() != Args.size()) {
      ErrorF("redefinition of function with different # args");
      return 0;
    }
  }
  
  // Set names for all arguments.
  unsigned Idx = 0;
  for (Function::arg_iterator AI = F->arg_begin(); Idx != Args.size();
       ++AI, ++Idx) {
    AI->setName(Args[Idx]);
    
    // Add arguments to variable symbol table.
    NamedValues[Args[Idx]] = AI;
  }
  
  return F;
}

Function *FunctionAST::Codegen() {
  NamedValues.clear();
  
  Function *TheFunction = Proto->Codegen();
  if (TheFunction == 0)
    return 0;
  
  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", TheFunction);
  Builder.SetInsertPoint(BB);
  
  if (LLVMValueRef RetVal = Body->Codegen()) {
    // Finish off the function.
    Builder.CreateRet(RetVal);

    // Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);

    return TheFunction;
  }
  
  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return 0;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void HandleDefinition() {
  if (FunctionAST *F = ParseDefinition()) {
    if (Function *LF = F->Codegen()) {
      fprintf(stderr, "Read function definition:");
      LF->dump();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (PrototypeAST *P = ParseExtern()) {
    if (Function *F = P->Codegen()) {
      fprintf(stderr, "Read extern: ");
      F->dump();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (FunctionAST *F = ParseTopLevelExpr()) {
    if (Function *LF = F->Codegen()) {
      fprintf(stderr, "Read top-level expression:");
      LF->dump();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
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
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

/// putchard - putchar that takes a double and returns 0.
extern "C" 
double putchard(double X) {
  putchar((char)X);
  return 0;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
  LLVMContext &Context = getGlobalContext();

  // Install standard binary operators.
  // 1 is lowest precedence.
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40;  // highest.

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();

  // Make the module, which holds all the code.
  TheModule = new Module("my cool jit", Context);

  // Run the main "interpreter loop" now.
  MainLoop();

  // Print out all of the generated code.
  TheModule->dump();

  return 0;
}
