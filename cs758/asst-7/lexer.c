#include "my_parser.h"
static uint32_t current_index = 0;
token* find_add_token(char *str, int len);
void PUSH(rule *r, rule_vector *vec){
  if(vec->len >= vec->size){
    vec->rules=realloc(vec->rules, vec->size*2);
    vec->size *= 2;
  }
  vec->rules[vec->len++] = r;
}
/*#define PUSH(obj, place)                                              \
  obj->next = place;                                                    \
  place = obj*/
token *gettok(char **line){
  char *pos = *line;
  while(*pos && !isspace(*pos));
  if(!*pos){
    return NULL;
  }
  token *tok = find_add_token(*line, pos - *line);
  *line = pos;
  return tok;
}
token* find_add_token(char *str, int len){
  token *existent_tok = NULL;
  HASH_FIND(hh, tokens, str, len, existent_tok);
  if(existent_tok != NULL){
    return existent_tok;
  }
  token *new_tok = xmalloc(len+sizeof(token));
  memcpy(new_tok->token, str, len);
  new_tok->len = len;
  new_tok->index = current_index++;
  HASH_ADD_KEYPTR(hh, tokens, new_tok->token, new_tok->len, new_tok);
  return new_tok;
}
int token_exists_p(char *str, int len){
  token *tok = NULL;
  HASH_FIND(hh, tokens, str, len, tok);
  return (tok == NULL ? 0 : 1);
}
rule *make_rule(token *lhs, token *rhs1, token *rhs2, double prob){
  rule *new_rule = xmalloc(sizeof(rule));
  new_rule->lhs = lhs;
  new_rule->rhs1 = rhs1;
  new_rule->rhs2 = rhs2;
  new_rule->probability = prob;
  return new_rule;
}
rule_vector* lex_rules(FILE* f){
  char* line;
  size_t line_size;
  rule_vector *retval=malloc(sizeof(rule_vector));
  retval->rules = malloc(32 * sizeof(rule));
  retval->len = 0;
  retval->size = 32;
  while (getline(&line,&line_size,f)) {
    if(line == (char*)-1){
      perror("getline");
      exit(1);
    }
    token *lhs = gettok(&line);
    token *rhs1 = gettok(&line);
    if(lhs == NULL || rhs1 == NULL){
      fprintf(stderr,"Malformed input\n");
      exit(1);
    }
    token *maybe_rhs2 = gettok(&line);
    if(maybe_rhs2 == NULL){
      rule *new_rule = make_rule(lhs,rhs1, NULL, 0);
      PUSH(new_rule, retval);
    } else {
      char **endptr;
      double temp = strtod(maybe_rhs2->token, endptr);
      if(maybe_rhs2->token != *endptr){
        rule *new_rule = make_rule(lhs, rhs1, NULL, temp);
        PUSH(new_rule, retval);
      } else {
        rule *new_rule = make_rule(lhs, rhs1, maybe_rhs2, 0);
        endptr = NULL;
        token *prob = gettok(&line);
        temp = strtod(prob->token, endptr);
        if(*endptr != prob->token){
          new_rule->probability = temp;
        }
        PUSH(new_rule,retval);
      }
    }
  }
  if(!token_exists_p("S",1)){
    fprintf(stderr,"Error no start symbol \"S\" found in the input grammar\n");
    exit(1);
  }
  token_array = xmalloc(sizeof(token*)* HASH_COUNT(tokens));
  return retval;
}
char * read_word(FILE *input){
  char *word = NULL;
  int word_size = 0;
  int buf_size = 128;
  char buf[128];
  char *bufptr = buf;
  char c;
  while(!isspace(c = fgetc(input)) && c != EOF){
    if(bufptr-buf >= buf_size){
      word = realloc(word, word_size + buf_size);
      memcpy(word + word_size, buf, buf_size);
      word_size +=buf_size;
      bufptr = buf;
    }
    *bufptr++ = c;
  }
  if(c == EOF){
    free(word);
    return NULL;
  }
  word = realloc(word, word_size + bufptr-buf);
  memcpy(word+word_size,buf, bufptr-buf);
  return word;
}

void parse(FILE *input, int algorithm, rule_vector *rules){
  size_t input_size = 64, input_len = 0;
  char **tokenized_input = xmalloc(input_size * sizeof(char*));
  char c;
  //eatup leading whitespace
  while(isspace((c=fgetc(input))) && c != EOF);
  if(c==EOF){
    fprintf(stderr, "error reading input\n");
    exit(1);
  }
  ungetc(c, input);
  char *word;
  while((word = read_word(input))){
    if(input_len >= input_size){
      tokenized_input=realloc(tokenized_input, input_size*2*sizeof(char*));
      input_size*=2;
    }
    tokenized_input[input_len++] = word;
    while(isspace((c=fgetc(input))) && c != EOF);
    if(c==EOF){break;}
    ungetc(c, input);
  }
  if(algorithm == 0){//use recursive decent parser
  } else {
    cyk_parse(rules, tokenized_input, input_len);
  }
}
