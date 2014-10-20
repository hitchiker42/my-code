#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <alloca.h>
#ifndef MY_PARSER
#include "cyk.h"
#include "parser.h"
#else
#include "my_parser.h"
#endif
char *strdup(const char *);
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
int len1, len2, len3;
#define index_3d(_i,_j,_k)                           \
  (_i*(len2*len3)+_j*(len3)+_k)
#define extract_index_3d(_ind,_i,_j,_k)         \
  _i = (_ind / (len2 *len3));                   \
  _j = (_ind - _i / len3);                      \
  _k = (_ind - _i - _j);
#define list_to_array(head, arr, len)           \
  {                                             \
    __typeof(head) temp = head;                 \
    int i=0;                                    \
    while(temp && i<len){                       \
      arr[i++] = *temp;                         \
      temp = temp->next;                        \
    }                                           \
    if(temp){                                                           \
      fprintf(stderr,"Excess elements in list not copied to array\n");  \
    }                                                                   \
  }
#ifndef MY_PARSER
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
/*static int find_start_index(const rule *rules){
  int i=0;
  const rule *r = rules;
  while(r){
    if(string_equal(r->lhs, "S")){
      return i;
    }
    i++;
    r=r->next;
  }
  return -1;
  }*/

void cyk_parse(void){
  fprintf(stderr,"starting cyk parse\n");
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
                               .nonterminals_len = rule_count,
                               .terminals_len = word_count,
                               .input_tokens = tokenized_input,
                               .input_len = input_length,
                               .num_symbols = symbol_count};
  internal_parse(&state);
}
struct backptr {
  int32_t left;
  int32_t right;
  struct backptr *next;
};
struct node {
  int32_t exists;
  struct backptr *backptrs;
};
struct backptr* make_backptr(int left,int right){
  struct backptr *temp = xmalloc(sizeof(struct backptr));
  temp->left = left;
  temp->right = right;
  return temp;
}
typedef struct str str;
struct str {
  char *str;
  int len;
};
void print_str(str string){
  fprintf(stderr,"%.*s\n",string.len,string.str);
}
str make_str(char *string, int len){
  return (str){.str = string, .len = len};
}
str my_strcat(str lstr, str rstr){
  char *temp = malloc(lstr.len + rstr.len + 8);
  malloc_check(temp);
  memcpy(temp, lstr.str, lstr.len);
  memcpy(temp+lstr.len, rstr.str, rstr.len);
  str retval  = make_str(temp, lstr.len+rstr.len);
  free(lstr.str);
  free(rstr.str);
  return retval;
}
str cstr_to_str(char *cstr){
  char *temp = strdup(cstr);
  return make_str(temp, strlen(temp));
}
#define str_append_char(_s,_c)                                          \
  _s.str[_s.len] = _c;                                                  \
  _s.len++;
str build_tree_rec(struct node *grid, int32_t start);
str build_tree_rec(struct node *grid, int32_t start){
  struct node n = grid[start];
  if(n.backptrs == NULL){
    return cstr_to_str("fish");
  } else {
    int left = n.backptrs->left;
    int right = n.backptrs->right;
    str cur = cstr_to_str(symbol_table[start % len3]);
    str_append_char(cur, '(');
    str lsym = cstr_to_str(symbol_table[left % len3]);
    str_append_char(lsym, '(');
    str lstr = build_tree_rec(grid, left);
    lstr = my_strcat(lsym, lstr);
    str_append_char(lstr,')');
    str_append_char(lstr,' ');
    
    str rsym = cstr_to_str(symbol_table[right % len3]);
    str_append_char(rsym, '(');
    str rstr = build_tree_rec(grid, right);
    rstr = my_strcat(rsym, rstr);
    str_append_char(rstr,')');
    str_append_char(rstr,')');
    str retval = my_strcat(cur, lstr);
    retval = my_strcat(retval, rstr);
    return retval;
  }
}

str build_tree(struct node *grid, int32_t start){
  struct node n = grid[start];
  if(n.backptrs == NULL){
    return (str){NULL,0};
  } else {
    return build_tree_rec(grid, start);
  }
}

void print_grid(struct node *grid, int len1, int len2, int len3){
  int i,j,k;
  for(i=0;i<len1;i++){
    fprintf(stderr,"\n\n");
    for(j=0;j<len2;j++){
       fprintf(stderr,"\n");
      for(k=0;k<len3;k++){
        fprintf(stderr,"%d ",(grid[index_3d(i,j,k)].exists ? 1 : 0));
      }
    }
  }
  fprintf(stderr,"\n\n");
}
static void internal_parse(struct parser_state *state){
  const rule *rules_list = state->nonterminals_list;
  const int num_rules = state->nonterminals_len;
  const word_rule *term_rules_list = state->terminals_list;
  const int num_term_rules = state->terminals_len;
  char **tokens = state->input_tokens;
  const int num_tokens = state->input_len;
  rule *rules = xmalloc(sizeof(rule)*num_rules);
  word_rule *term_rules = xmalloc(sizeof(word_rule)*num_term_rules);
  list_to_array(rules_list,rules,num_rules);
  list_to_array(term_rules_list,term_rules,num_term_rules);

  len1 = len2 = num_tokens;
  len3 = state->num_symbols;
  assert(len1 && len2 && len3);
  int i,j,k,l;
  fprintf(stderr,"grid size = %dx%dx%d (%lu)\n",len1,len2,len3,(unsigned long)len1*len2*len3);
  struct node *grid = xmalloc(len1*len2*len3*sizeof(struct node));
  fprintf(stderr,"starting initialization loop\n");
  for(i=0;i<len1;i++){
   for(k=0;k<num_term_rules;k++){
     //a span of length 1(0) can be generated from A iff A produces a terminal
     int A = find_symbol_index(term_rules[k].lhs);
     if(string_equal(tokens[i],term_rules[k].word)){
       grid[index_3d(i,0,A)] = (struct node){.exists = 1};
     }
   }
  }
  print_grid(grid,len1,len2,len3);
  fprintf(stderr,"starting main loop\n");
  for(i=1;i<len1;i++){//length of span -1
    for(j=0;j<len1-i+1;j++){//start point
      for(k=0;k<i-1;k++){//point to split
        for(l=0;l<num_rules;l++){
          int A = find_symbol_index(rules[l].lhs);
          int B = find_symbol_index(rules[l].rhs1);
          int C = find_symbol_index(rules[l].rhs2);
          /*
            if B can represent the tokens from j->k and
            C can represent the tokens from j+k -> i then
            A can represent the tokens from j -> i
           */
          if(grid[index_3d(j,k,B)].exists && grid[index_3d(j+k,i-k,C)].exists){
            fprintf(stderr,"Rule from %s -> %s %s, exists\n",rules[l].lhs, rules[l].rhs1,rules[1].rhs2);
            grid[index_3d(j,i,A)].exists = 1;
            struct backptr *temp = make_backptr(index_3d(j,k,B), index_3d(j+k,i-k,C));
            temp->next = grid[index_3d(j,i,A)].backptrs;
            grid[index_3d(j,i,A)].backptrs = temp;
          }
        }
      }
    }
  }
  fprintf(stderr,"finished main loop\n");
  print_grid(grid,len1,len2,len3);
  int s = find_symbol_index("S");
  fprintf(stderr,"start symbol index = %d\n",s);
  if(s<0){
    fprintf(stderr,"Error, start symbol not found\n");
    return;
  }
  int index = index_3d(0,(len2-1),s);
  if(grid[index].exists){
    if(grid[index].backptrs == NULL){
      fprintf(stderr,"no backptrs\n");
      abort();
    }
    fprintf(stderr,"Valid parse exists\n");
    str tree;
    tree = build_tree(grid, index);
    while(tree.str){
      fprintf(stderr,"%.*s\n",tree.len,tree.str);
      grid[index].backptrs = grid[index].backptrs->next;
      tree = build_tree(grid, index);
    }
  } else {
    fprintf(stderr,"no valid parse exists\n");
    //input was invalid;
  }
}
#else
void cyk_parse(rule_vector *rules, char **input, int input_len){
  int num_symbols = HASH_COUNT(tokens);
  int len1,len2;
  len1 = len2 = input_len;
  int len3 = num_symbols;
  int i,j,k,l;
  struct node {
    int32_t exists;
    int32_t parent;
  };
  struct node *grid = xmalloc(len1*len2*len3*sizeof(struct node));
  for(i=0;i<len1;i++){
    for(k=0;k<len3;k++){
      for(l=0;l<num_rules;l++){
        if(tokens[i] == rules->rules[k]->rhs1) &&
          rules->rules[k]->rhs2 == NULL){
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
          if(grid[index_3d(j,k,B)].exists & grid[index_3d(j+k,i-k,C)].exists){
            grid[index_3d(j,i,A)].exists = 1;
            grid[index_3d(j,k,B)].parent = index_3d(j,i,A);
            grid[index_3d(j+k,i-k,C)].parent = index_3d(j,i,A);
          }
        }
      }
    }
  }
  int s = find_start_index(rules_list);
  if(grid[index_3d(0,len1-1,s)].exists){
    //input has a valid parse
  } else {
    //input was invalid;
  }
}
#endif
