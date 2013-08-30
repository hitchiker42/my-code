#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <setjmp.h>
#include "parser.h"
#incldue "dynarray.h"

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

unsigned char* IdStr;  // Filled in if tok_identifier
double NumVal;              // Filled in if tok_number
int CurTok;
jmp_buf main_loop;
/// gettok - Return the next token from standard input.
//on error break to main
int Error(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  longjmp(main_loop,-1);
  return 1;
}
int gettok() {
  int i=0,haveDot=0;
  int LastChar = ' ';
  // Skip any whitespace.
  while (isspace(LastChar)){
    LastChar = getchar();
  }
  // identifier: [a-zA-Z][a-zA-Z0-9]*
  if (isalpha(LastChar)) {
    IdStr[i] = LastChar;
    while (isalnum((LastChar = getchar())) && i<100){
      IdStr[i++] = LastChar;
    }

    if (!(strcmp(IdStr,"def"))){
      return tok_def;
    } else if (!(strcmp(IdStr,"extern"))){
      return tok_extern;
    } else {
      return tok_identifier;
    }
  // Number: [0-9.]+
  } else if (isdigit(LastChar) || (LastChar == '.' && haveDot++)){
    char* NumStr = alloca(100);
    do {
      NumStr[i++] = LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || (LastChar == '.' && haveDot++) && i< 100);
    if (haveDot > 1){
      Error(strcat("Parse Error found in",strcat(NumStr,"\n")));
    } else {
      NumVal = strtod(NumStr, 0);
      return tok_number;
    }
  // Comment until end of line.
  } else if (LastChar == '#') {
    do {
      LastChar = getchar();
    } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
    if (LastChar != EOF){
      return gettok();
    }
  // Check for end of file.  Don't eat the EOF.
  } else if (LastChar == EOF){
    return tok_eof;
  // Otherwise, just return the character as its ascii value.
  } else {  
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
  }
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//



//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
int BinOpPrecedence(char Tok){
  switch (Tok) {
    case '+':
      return 20;
    case '-': return 20;
    case '*': return 40;
    case '<': return 10;
    default: return -1;
  }
}

/// identifierexpr = identifier
///                | identifier '(' expression* ')'
ExprAST ParseIdentifierExpr(const char* id) {
  ExprAST retval;
  getNextToken();  // eat identifier.
  if (CurTok != '('){ // Simple variable ref.
    retval.Expr.Name = id;
    retval.Tag = 3;
    return retval;
  } else {  // Call.
    int i;
    ExprAST* Args;
    //this imposes an arbitrary limit on number of arguements
    Args = xmalloc(20*sizeof(ExprAST))
    getNextToken();  // eat (
    if (CurTok != ')') {
      for (i=0;;i++) {
        ExprAST Arg = ParseExpression();
        Args[i] = Arg;
        if (CurTok == ')'){
          break;
        } else if (CurTok != ',') {
          Error("Expected ')' or ',' in argument list");
        } else {
          getNextToken();
        }
      }
    }
  // Eat the ')'.
  getNextToken();
  CallExpr* temp = xmalloc(sizeof(CallExpr));
  *temp={id,Args};
  retval.Expr.fxnCall=temp;
  retval.Tag = 5;
  return retval;
  }
}

// numberexpr = number
ExprAST ParseNumberExpr() {
  ExprAST retval;
  retval.Expr.Number = NumVal;
  retval.Tag = 2;
  getNextToken(); // consume the number
  return retval;
}

/// parenexpr = '(' expression ')'
ExprAST ParseParenExpr() {
  getNextToken();  // eat (.
  ExprAST retval = ParseExpression();
  //  if (!V){return 0;} //if null, return 0
  if (CurTok != ')'){
    Error("expected ')'");
  }
  getNextToken();  // eat ).
  return retval;
}

/// primary = identiferExpr | numberExpr | parenExpr
ExprAST ParsePrimary() {
  switch (CurTok) {
    case tok_identifier:
      return ParseIdentifierExpr(strdup(IdStr));
    case tok_number:     
      return ParseNumberExpr();
    case '(':
      return ParseParenExpr();
    default:
      Error("unknown token when expecting an expression");
  }
}

/// binopRHS = op primary | op primary op primary
///   ::= ('+' primary)*
ExprAST ParseBinOpRHS(int ExprPrec, ExprAST LHS) {
  // If this is a binop, find its precedence.
  BinExp* temp;
  ExprAST RHS;
  temp = xmalloc(sizeof(BinExp))
  while (1) {
    int TokPrec = BinOpPrecedence(CurTok);
    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec){
      return LHS;
    }
    // Okay, we know this is a binop.
    int BinOp = CurTok;
    getNextToken();  // eat binop
    // Parse the primary expression after the binary operator.
    RHS = ParsePrimary();
    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int NextPrec = BinOpPrecedence(CurTok);
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec+1, RHS);
    }
    // Merge LHS/RHS.
    BinExp temp2 = {LHS, RHS, BinOp};
    *temp = temp2;
    LHS.tag=4;
    LHS.Expr.Op=temp;
  }
}

/// expression = primary binopRHS
ExprAST ParseExpression() {
  ExprAST LHS = ParsePrimary();
  ExprAST retval;
  retval = ParseBinOpRHS(0, LHS);
  retval.tag = 4;
  return retval;
}

/// prototype = id ( args )
/// args = identifier | identifier , identifier
Prototype ParsePrototype() {
  int i=0;
  if (CurTok != tok_identifier){
    Error("Expected function name in prototype");
  }
  char* FnName = strdup(IdStr);
  getNextToken();

  if (CurTok != '('){
    Error("Expected '(' in prototype");
  }
  char** ArgNames;
  ArgNames = xmalloc(20*sizeof(char*))
  while(getNextToken() == tok_identifier){
    ArgNames[i] = strdup(IdStr);
  }
  if (CurTok != ')'){
    Error("Expected ')' in prototype");
  }
  // success.
  getNextToken();  // eat ')'.
  Prototype retval={FnName,ArgNames};
  return retval;
}

/// definition = 'def' prototype expression
FunctionAST ParseDefinition() {
  getNextToken();  // eat def.
  Prototype proto = ParsePrototype();
  ExprAST body = ParseExpression();
  FunctionAST fxn={proto,body};
  return fxn;
}

/// toplevelexpr = expression
FunctionAST ParseTopLevelExpr() {
  ExprAST body = ParseExpression();
  // Make an anonymous proto.
  Prototype proto = {"",0};
  FunctionAST fxn = {proto,body};
  return fxn;
}

/// external ::= 'extern' prototype
Prototype ParseExtern() {
  getNextToken();  // eat extern.
  return ParsePrototype();
}

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//
/*
void HandleDefinition() {
  if (ParseDefinition()) {
    fprintf(stderr, "Parsed a function definition.\n");
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleExtern() {
  if (ParseExtern()) {
    fprintf(stderr, "Parsed an extern\n");
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (ParseTopLevelExpr()) {
    fprintf(stderr, "Parsed a top-level expr\n");
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}
*/
/// top ::= definition | external | expression | ';'
static void MainLoop() {
  while (1) {
    fprintf(stderr, "ready> ");
    if (setjmp (main_loop)){
      continue;
    } else {
      switch (CurTok) {
        case tok_eof:
          return;
        case ';':  // ignore top-level semicolons.
          getNextToken();
          break;
        case tok_def:
          ParseDefinition();
          break;
        case tok_extern:
          ParseExtern();
          break;
        default:
          ParseTopLevelExpr();
          break;
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
  if (!(IdStr = calloc(100,sizeof(char)))){return -0xff;}
  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();
  // Run the main "interpreter loop" now.
  MainLoop();
  return 0;
}
