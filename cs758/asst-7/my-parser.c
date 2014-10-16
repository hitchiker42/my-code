#include "lexer.h"
#include "parser.h"
#include "rdp.h"
/*
  simple main function
*/
void usage(void){
  printf("usage: ./parser [-h|--help] grammar.cnf <alg> [input]\n"
         "parse input using the grammer in grammar.cnf and the alorithm alg\n"
         "If input is not given, or is '-' it is read from stdin\n"
         "<alg> may be either rdp (recursive descent parser) or cyk,\n"
         "which is the name of the dynamic programming parsing algorithm\n");
  exit(0);
}
static FILE *fopen_checked(const char *pathname, const char *mode){
  FILE *f = fopen(pathname, mode);
  if(f == NULL){
    perror("fopen");
    exit(1);    
  }
  return f;
}
void cleanup_rules(rule_vector *rules){
  int i;
  for(i=0;i<rules->len;i++){
    free(rules->rules[i]);
  }
  free(rules);
}
void cleanup_tokens(){
  token *current_tok, *temp;
  HASH_ITER(hh, tokens, current_tok, temp){
    HASH_DEL(tokens, current_tok);
    free(current_tok->token);
    free(current_tok);
  }
}
int main(int argc, const char * argv[]){
  FILE* grammar, input;
  if(argc < 3){
    usage();
  }
  if(argv[1][0] == '-'){
    if(string_equal(argv[1],"-h") || string_equal(argv[1],"--help")){
      usage();
    }
  }
  grammar = fopen_checked(argv[1], "r");
  read_rules(grammar);
  if(argv[3] == NULL || string_equal(argv[3],"-")){
    input = STDIN;
  } else {
    input = fopen_checked(argv[3], "r");
  }
  rule_vector *rules = lex(grammar);
  int algorithm;
  if(strcmp(argv[2], "rdp") == 0){
    algorithm = 0;
  } else if(strcmp(argv[2], "cyk") == 0){
    algorithm = 1;
  } else {
    usage();
  }
  parse(input, algorithm, rules);
  cleanup_rules();
  return 0;
}

