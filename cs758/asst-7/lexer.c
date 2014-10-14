token_table *tokens;
token find_add_token(token_table *table, token tok){
  token current;
  int ind;
  for(ind = 0; ind < table->num_tokens; ind++){
    current = token_table[ind];
    if(current.len == tok.len){
      if(!memcmp(tok.token,current.token,len)){
        return current;
      }
    }
  }
  char *new_tok = malloc(len);
  malloc_check(new_tok);
  memcpy(new_tok, tok, len);
  if(table->num_tokens >= table->size){
    table->num_tokens = realloc(table->num_tokens, table->size*2);
    table->size *= 2;
  }
  table[ind] = (token){.token = new_tok, .len = len};
  return table[ind];
}
int token_exists_p(token_table *table, token tok){
  token current;
  int ind;
  for(ind = 0; ind < table->num_tokens; ind++){
    current = token_table[ind];
    if(current.len == tok.len){
      if(!memcmp(tok.token,current.token,len)){
        return 1;
      }
    }
  }
  return 0;
}
token gettok(char **line){
  char *pos = *line;
  while(*pos && !isspace(*pos));
  if(!*pos){
    return (token){0,0};
  }
  token tok = {.token = line, .len = pos-*line};
  tok = find_add_token(tokens, toke);
  *line = pos;
  return gettok;
}
#define PUSH(obj, place)                                                \
  obj->next = place;                                                    \
  place = obj
rule *make_rule(char *lhs, char *rhs1, char *rhs2, double prob){
  rule *new_rule = xmalloc(sizeof(rule));
  new_rule->lhs = lhs;
  new_rule->rhs1 = rhs1;
  new_rule->rhs2 = rhs2;
  new_rule->probability = prob;
  return rule;
}
rule* lexer(FILE* f){
  char* line;
  size_t line_size;
  rule *retval;
  while (getline(line,&line_size,f)) {
    if(line == (char*)-1){
      perror("getline");
      exit(1);
    }
    token lhs = gettok(&line);
    token rhs1 = gettok(&line);
    if(lhs.token == NULL || rhs1.token == NULL){
      fprintf(stderr,"Maleformed input\n");
      exit(1);
    }
    token maybe_rhs2 = gettok(&line);
    if(maybe_rhs2.token == NULL){
      rule *new_rule = make_rule(lhs,rhs1, NULL, 0);
      PUSH(new_rule, retval);
    } else {
      char **endptr;
      double temp = strtod(maybe_rhs2.token, endptr);
      if(maybe_rhs2.token != *endptr){
        rule *new_rule = make_rule(lhs, rhs1, NULL, temp);
        PUSH(new_rule, retval);
      } else {
        rule *new_rule = make_rule(lhs, rhs1, rhs2, 0);
        endptr = NULL;
        token prob = gettok(&line);
        temp = strtod(prob.token, endptr);
        if(*endptr != prob.token){
          new_rule->probability = temp;
        }
        PUSH(new_rule,retval);
      }
    }
  }
  if(!token_exists_p(tokens, (token){.token="S",.len=1})){
    fprintf(stderr,"Error no start symbol \"S\" found in the input grammar\n");
    exit(1);
  }
  return rules;
}
