#include "parser.h"
//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

char IdStr[1000];  // Filled in if tok_identifier
double NumVal;              // Filled in if tok_number
int CurTok;
int LastTok;
int gensym_count=0;
jmp_buf main_loop,external,top_level,definition;
/// gettok - Return the next token from standard input.
//on error break to main
int Error(const char *Str,jmp_buf label) {
  fprintf(stderr, "Error: %s\n", Str);
  longjmp(label,-1);
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
    IdStr[i++] = LastChar;
    while (isalnum((LastChar = getchar())) && i<100){
      IdStr[i++] = LastChar;
    }
    ungetc(LastChar,stdin);
    if (!(strcmp(IdStr,"def"))){
      return tok_def;
    } else if (!(strcmp(IdStr,"extern"))){
      return tok_extern;
    } else {
      return tok_identifier;
    }
  // Number: [0-9.]+
  } else if (isdigit(LastChar) || (LastChar == '.' && ++haveDot)){
    char* NumStr = alloca(100);
    do {
      NumStr[i++] = LastChar;
      LastChar = getchar();
    } while ((isdigit(LastChar) || (LastChar == '.' && haveDot++)) && i< 100);
    if (haveDot > 1){
      Error(strcat("Parse Error found in",strcat(NumStr,"\n")),main_loop);
    } else {
      NumVal = strtod(NumStr, 0);
      ungetc(LastChar,stdin);
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
    return LastChar;
  }
  return 0;
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
    case '+': return 20;
    case '-': return 20;
    case '*': return 40;
    case '<': return 10;
    default: return -1;
  }
}

/// identifierexpr = identifier
///                | identifier '(' expression* ')'
ExprAST ParseIdentifierExpr(const char* id,jmp_buf label) {
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
    Args = xmalloc(20*sizeof(ExprAST));
    getNextToken();  // eat (
    if (CurTok != ')') {
      for (i=0;i<20;i++) {
        ExprAST Arg = ParseExpression(label);
        Args[i] = Arg;
        if (CurTok == ')'){
          break;
        } else if (CurTok != ',') {
          Error("Expected ')' or ',' in argument list",label);
        } else {
          getNextToken();
        }
      }
    }
  // Eat the ')'.
  getNextToken();
  CallExpr* temp = xmalloc(sizeof(CallExpr));
  CallExpr temp2={id,Args,i+1};
  *temp=temp2;
  retval.Expr.fxnCall=temp;
  retval.Tag = 5;
  return retval;
  }
}

// numberexpr = number
ExprAST ParseNumberExpr() {
  ExprAST retval;
  retval.Expr.real.dblVal = NumVal;
  retval.Tag = 2;
  getNextToken(); // consume the number
  return retval;
}

/// parenexpr = '(' expression ')'
ExprAST ParseParenExpr(jmp_buf label) {
  getNextToken();  // eat (.
  ExprAST retval = ParseExpression(label);
  //  if (!V){return 0;} //if null, return 0
  if (CurTok != ')'){
    Error("expected ')'",label);
  }
  getNextToken();  // eat ).
  return retval;
}

/// primary = identiferExpr | numberExpr | parenExpr
ExprAST ParsePrimary(jmp_buf label) {
  switch (CurTok) {
    case tok_identifier:
      return ParseIdentifierExpr(strdup(IdStr),label);
    case tok_number:     
      return ParseNumberExpr();
    case '(':
      return ParseParenExpr(label);
    default:
      Error("unknown token when expecting an expression",label);
  }
}

/// binopRHS = op primary | op primary op primary
///   ::= ('+' primary)*
ExprAST ParseBinOpRHS(int ExprPrec, ExprAST LHS,jmp_buf label) {
  // If this is a binop, find its precedence.
  BinExp* temp;
  ExprAST RHS;
  temp = xmalloc(sizeof(BinExp));
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
    RHS = ParsePrimary(label);
    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int NextPrec = BinOpPrecedence(CurTok);
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec+1, RHS,label);
    }
    // Merge LHS/RHS.
    BinExp temp2 = {LHS, RHS, BinOp};
    *temp = temp2;
    RHS.Tag=4;
    RHS.Expr.BinOp=temp;
  }
}

/// expression = primary binopRHS
ExprAST ParseExpression(jmp_buf label) {
  ExprAST LHS = ParsePrimary(label);
  ExprAST retval;
  retval = ParseBinOpRHS(0, LHS,label);
  return retval;
  }


/// prototype = id ( args )
/// args = identifier | identifier , identifier
Prototype ParsePrototype(jmp_buf label) {
  int i=0;
  if (CurTok != tok_identifier){
    Error("Expected function name in prototype",label);
  }
  char* FnName = strdup(IdStr);
  getNextToken();

  if (CurTok != '('){
    fprintf(stderr,"Expected '(' in prototype, got %c",CurTok);
    Error("",label);
  }
  char** ArgNames;
  ArgNames = xmalloc(20*sizeof(char*));
 parse: while (getNextToken() == tok_identifier && i <20){
    ArgNames[i] = strdup(IdStr);
    i++;
  }
  if (CurTok != ')'){
    if (CurTok == ','){
      goto parse;}
    fprintf(stderr,"Expected ')' in prototype, got %c",CurTok);
    Error("",label);
  }
  // success.
  getNextToken();  // eat ')'.
  Prototype retval={FnName,ArgNames,i};
  return retval;
}

/// definition = 'def' prototype expression
FunctionAST ParseDefinition(jmp_buf label) {
  getNextToken();  // eat def.
  Prototype proto = ParsePrototype(label);
  ExprAST body = ParseExpression(label);
  FunctionAST fxn={proto,body};
  return fxn;
}

/// toplevelexpr = expression
FunctionAST ParseTopLevelExpr(jmp_buf label) {
  ExprAST body = ParseExpression(label);
  // Make an anonymous proto.
  char* top_level_id = malloc(15*sizeof(char));
  snprintf(top_level_id,14,"top_level_%d",gensym_count++);
  Prototype proto = {top_level_id,0};
  FunctionAST fxn = {proto,body};
  return fxn;
}

/// external ::= 'extern' prototype
Prototype ParseExtern(jmp_buf label) {
  getNextToken();  // eat extern.
  return ParsePrototype(label);
}

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//
/*
void HandleDefinition() {
  if (setjmp(definition)){
    getNextToken();//error recovery
  } else {
    ParseDefinition(definition);
    fprintf(stderr, "Parssed a function definition.\n");
  }
}

void HandleExtern() {
  if (setjmp(external)){
      getNextToken();//error recovery
  } else {
      ParseExtern(external);
      fprintf(stderr, "Parsed an extern\n");
  }
}

static void HandleTopLevelExpr() {
  // Evaluate a top-level expression into an anonymous function.
  if(setjmp(top_level)){
    getNextToken(); //error recovery
  } else{
    ParseTopLevelExpr(top_level);
    fprintf(stderr, "Parsed a top-level expr\n");
  }
}

/// top ::= definition | external | expression | ';'
static void Parse() {
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
          HandleDefinition();
          break;
        case tok_extern:
          HandleExtern();
          break;
        default:
          HandleTopLevelExpr();
          break;
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
  IdStr = xcalloc(100,sizeof(char));
  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();
  // Run the main "interpreter loop" now.
  MainLoop();
  return 0;
}
*/
