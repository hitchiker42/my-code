
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <alloca.h>

#include "cyk.h"
#include "parser.h"

/*
  pick up the globals from the parser
*/
extern const rule* rule_list;
extern const word_rule* word_list;
extern int rule_count;
extern int word_count;
extern char* tokenized_input [MAX_INPUT];
extern char input[MAX_CHARS];
extern int input_length;
extern int symbol_count;
extern char** symbol_table;
extern int start_index;
struct parser_state {
  const rule *nonterminals_list;
  const word_rule *terminals_list;
  char **input_tokens;
  int nonterminals_len;
  int terminals_len;
  int input_len;
  int num_symbols;
};
static void internal_parse(struct parser_state *state);
static int find_start_index(const rule *rules){
  int i=0;
  const rule *r = rules;
  while(r){
    if(r->lhs[0]=='S' && r->lhs[1]=='\0'){
      return i;
    }
    i++;
    r=r->next;
  }
  return -1;
}
void cyk_parse(void){
  /*struct parser_state {
  const rule *nonterminals_list;
  const word_rule *terminals_list;
  int nonterminals_len;
  int terminals_len;
  char **input_tokens;
  int input_len;
};*/
  struct parser_state state = {.nonterminals_list = rule_list,
                               .terminals_list = word_list,
                               .nonterminals_len = rule_len,
                               .terminals_len = word_len,
                               .input_tokens = tokenized_input,
                               .input_len = input_length};
  internal_parse(&state);
}

void malloc_check(void* p){
  if(p == NULL) {
      fprintf(stderr, "Malloc failed\n");
      exit(1);
  }
}
inline void *xmalloc(size_t sz){
  void *temp = malloc(sz);
  malloc_check(temp);
  memset(temp,'\0',sz);
  return temp;
}
//assumes len1, len2 and len3 are the lengths of some 3d array, stored as
//a 1-D array in row major order, given an index i,j,k into that array
//return the offset from the start of the array to the current element
#define index_3d(_i,_j,_k)                           \
  (_i*(len2*len3)+_j*(len3)+_k)
#define list_to_array(head, arr)                \
  {                                             \
    __typeof(head) temp = head;                 \
    int i=0;                                    \
    while(head){                                \
      arr[i] = temp;                            \
      temp = temp->next;                        \
    }                                           \
  }
void cyk_parse(rule_vector *rules, char **input, int input_len){
  int num_symbols = HASH_COUNT(tokens);
  int len1 = len2 = input_len;
  int len3 = num_symbols;
  int i,j,k,l;
  struct node {
    int32_t exists;
    int32_t parent;
  };
  struct node *grid = xmalloc(len1*len2*len3*sizeof(struct node));
  for(i=0;i<len1;i++){ 
    for(k=0;k<;k++){
     if(string_eq(tokens[i],term_rules[k]->rhs)){
       grid[index_3d(i,i,k)] = (struct node){.exists =1};
     }
   }
  }
  for(i=1;i<len1;i++){
    for(j=0;j<i+(len1-1);j++){
      for(k=0;k<i-1;k++){
        for(l=0;l<num_rules;l++){
          int A = find_symbol_index(rules[l]->lhs);
          int B = find_symbol_index(rules[l]->rhs1);
          int C = find_symbol_index(rules[l]->rhs2);
          if(grid[index_3d(j,k,B)].exists & grid[index_3d(j+k,i-k,C)].exits){
            grid[index_3d(j,i,A)].exists = 1;
            grid[index_3d(j,k,B)].parent = index_3d(j,i,A);
            grid[index_3d(j+k,i-k,C)].parent = index_3d(j,i,A);
          }
        }
      }
    }
  }
  int s = find_start_index(rules_list);
  if(grid[index_3d[0,len1-1,s].exists]){
    //input has a valid parse
  } else {
    //input was invalid;
  }
}  
static void internal_parse(struct parser_state *state){
  const rule *rules_list = state->nonterminals_list;
  const int num_rules = state->nonterminals_len;
  const word_rule *term_rules_list = state->terminals_list;
  const int num_term_rules = state->terminals_len;
  const char **tokens = state->input_tokens;
  const int num_tokens = state->input_len;
  rule *rules = alloca(sizeof(rule*)*num_rules);
  rule *term_rules = alloca(sizeof(word_rule*)*num_term_rules);
  list_to_array(rules_list,rules);
  list_to_array(term_rules_list,term_rules);
  int len1 = len2 = num_tokens;
  int len3 = state->num_symbols;  
  int i,j,k,l;
  struct node {
    int32_t exists;
    int32_t parent;
  };
  struct node *grid = xmalloc(len1*len2*len3*sizeof(struct node));
  for(i=0;i<len1;i++){ 
   for(k=0;k<num_term_rules;k++){
     if(string_eq(tokens[i],term_rules[k]->rhs)){
       grid[index_3d(i,i,k)] = (struct node){.exists =1};
     }
   }
  }
  for(i=1;i<len1;i++){
    for(j=0;j<i+(len1-1);j++){
      for(k=0;k<i-1;k++){
        for(l=0;l<num_rules;l++){
          int A = find_symbol_index(rules[l]->lhs);
          int B = find_symbol_index(rules[l]->rhs1);
          int C = find_symbol_index(rules[l]->rhs2);
          if(grid[index_3d(j,k,B)].exists & grid[index_3d(j+k,i-k,C)].exits){
            grid[index_3d(j,i,A)].exists = 1;
            grid[index_3d(j,k,B)].parent = index_3d(j,i,A);
            grid[index_3d(j+k,i-k,C)].parent = index_3d(j,i,A);
          }
        }
      }
    }
  }
  int s = find_start_index(rules_list);
  if(grid[index_3d[0,len1-1,s].exists]){
    //input has a valid parse
  } else {
    //input was invalid;
  }
}
  
  
