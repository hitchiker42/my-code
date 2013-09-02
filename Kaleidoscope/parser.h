#ifndef PARSER_H_
#define PARSER_H_
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <setjmp.h>
#include "xmalloc.h"
// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
  tok_eof = -1,
  // commands
  tok_def = -2,
  tok_extern = -3,
  // primary
  tok_identifier = -4,
  tok_number = -5
};
//TODO: add in integers
typedef struct CallExpr CallExpr;
typedef struct BinExp BinExp;
typedef struct Prototype Prototype;
typedef struct FunctionAST FunctionAST;
typedef union untaggedExprAST untaggedExprAST;
typedef union floatAST floatAST;
typedef struct wordAST wordAST;
typedef struct valueAST valueAST;
typedef union numberAST numberAST;
typedef struct ExprAST ExprAST;
typedef enum ExprASTtag ExprASTtag;

struct CallExpr{//function call
  const char* Name;
  ExprAST* args;
  int Argc;
};//3 qwords
union floatAST {//floating pt value
  float fltFal;
  double dblVal;
};
struct wordAST {//integral value
  union {
  char byteVal;
  short shortVal;
  int intVal;
  long longVal;
  } value;
  unsigned char sign;
};
union numberAST{
  wordAST word;
  floatAST real;
};
struct valueAST {//any numerical value
  numberAST value;
  unsigned char sign;
};
union untaggedExprAST {//any expression
  wordAST word;
  floatAST real;
  const char* Name; //Identifiers
  BinExp* BinOp; //Binary Expressions
  CallExpr* fxnCall; //Function Calls
};//1 qwords
enum ExprASTtag {//identifies the type of exprssions
  Word = 1,
  Real = 2,
  Name = 3,
  Op = 4,
  fxnCall = 5
};
struct ExprAST {//basic ast node
  untaggedExprAST Expr;
  ExprASTtag Tag;
};//2 qwords
struct BinExp {//Binary operation
  ExprAST LHS;
  ExprAST RHS;
  char op;
};//3 qwords
struct Prototype {//function prototype
  const char* Name;
  char** Argv;
  int Argc;
};//3 qwords
struct FunctionAST {//function body
  Prototype Proto;
  ExprAST Body;
};//128 bits
int Error(const char *Str,jmp_buf label);
ExprAST ParseExpression(jmp_buf label);
Prototype ParseExtern(jmp_buf label);
FunctionAST ParseTopLevelExpr(jmp_buf label);
FunctionAST ParseDefinition(jmp_buf label);
Prototype ParsePrototype(jmp_buf label);
ExprAST ParseBinOpRHS(int ExprPrec, ExprAST LHS,jmp_buf label);
ExprAST ParsePrimary(jmp_buf label);
int gettok();
extern int CurTok;
inline int getNextToken(){
  return CurTok = gettok();
}
#endif
