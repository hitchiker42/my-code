%token TOK_ID
%token TOK_REAL
%token TOK_INT
%token TOK_IF
%token TOK_DO
%token TOK_DEF
%token TOK_LAMBDA
%token TOK_PROGN
%token TOK_LET
%token TOK_TAG
%token TOK_GO
%token TOK_QUOTE
%%
sexp:

atom:
  TOK_REAL {$$=yylval;}
| TOK_ID {symref* var;
    getSym(yylval,var);
    $$=var;}
def:
TOK_DEF{char* name=yylval} sexp{datatype value=yylval;
  symref* sym=xmalloc(sizeof(symref));
  sym.name=name;
  sym.val=value;
  addSym(sym);
  $$=value}
args:
'(' TOK_ID ')' {$$=yylval}
'(' args TOK_ID {char** arglist
    | TOK_ID{char* name=yylval} ')' {$$=name}
defun:
TOK_DEFUN

