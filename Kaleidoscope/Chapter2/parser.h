#ifndef PARSER_H_
#define PARSER_H_
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
typedef struct fxn FunctionAST;
typedef union ExprAST ExprAST;

struct CallExpr{
  const char* Name;
  ExprAST* args;
};//128 bits(char* is 64 bits)
union ExprAST {
  double Number; //Numbers, double precision only
  const char* Name; //Identifiers
  BinExp* Op; //Binary Expressions
  CallExpr fxnCall; //Function Calls
};//bits = 128
struct BinExp {
  ExprAST LHS;
  ExprAST RHS;
  char op;
};//172 bits(char is most likely padded to 64 bits)
struct Prototype {
  const char* Name;
  char** Args;
};//128 bits
struct fxn {
  Prototype Proto;
  ExprAST Body;
};//128 bits
ExprAST ParseExpression();
#define getNextToken() (CurTok = gettok())
#endif
