#include "C_util.h"
#include "hash.h"
#define STRING_BUF_MACROS
#include "string_buf.h"
#include "util_string.h"
typedef struct hashtable hashtable;
typedef struct mem_stream mem_stream;
struct mem_stream {
  uint8_t *mem;
  size_t sz;
  size_t off;
};
int stream_at_end(mem_stream *stream){
  return(stream->off >= stream->sz);
}
void free_stream(mem_stream *stream){
  munmap(stream->mem, stream->sz);
  free(stream);
}
string stream_next_word(mem_stream *stream){
  size_t start = stream->off;
  while(stream->off < stream->sz &&
        !isspace(stream->mem[stream->off])){
    stream->off++;
  }
  string ret = {.mem = stream->mem + start,
                 .sz = stream->off - start};
  while(isspace(stream->mem[stream->off])){
    stream->off++;
  }
  return ret;
}
//returned string includes '\n' character (unless we reach the eof)
string stream_next_line(mem_stream *stream){
  size_t start = stream->off;
  while(stream->off < stream->sz &&
        stream->mem[stream->off++] != '\n');
  string ret = {.mem = stream->mem + start,
                 .sz = stream->off - start};
  return ret;
}
mem_stream *FILE_to_stream(FILE *f){
  int fd = fileno(f);
  mem_stream *stream = xmalloc(sizeof(mem_stream));
  stream->mem = mmap_file(fd, 0, PROT_READ | PROT_WRITE, &stream->sz);
  if(!stream->mem){
    free(stream);
    return NULL;
  }
  stream->off = 0;
  return stream;
}

typedef struct grammar grammar;
typedef struct symbol symbol;
typedef struct non_terminal_rule non_terminal_rule;
typedef struct terminal_rule terminal_rule;
struct non_terminal_rule {
  double prob;
  string lhs;
  string rhs1;
  string rhs2;
};
struct terminal_rule {
  double prob;
  string lhs;
  string rhs;
};
struct grammar {
  non_terminal_rule **non_terminals;
  terminal_rule **terminals;
  hashtable *symbol_table;
  int num_non_terminals;
  int num_terminals;
  void *file;
  size_t file_sz;
};
void destroy_grammar(grammar *g){
  int i;
  for(i = 0; i < g->num_non_terminals; i++){
    free(g->non_terminals[i]);
  }
  for(i = 0; i < g->num_terminals; i++){
    free(g->terminals[i]);
  }
  free(g->non_terminals);
  free(g->terminals);
  if(g->symbol_table){
    destroy_hashtable(g->symbol_table);
  }
  if(g->file){
    munmap(g->file, g->file_sz);
  }
  free(g);
}
non_terminal_rule *make_non_terminal(string lhs, string rhs,
                                     string rhs2, double prob){
  non_terminal_rule *rule = xmalloc(sizeof(non_terminal_rule));
  rule->lhs = lhs;
  rule->rhs1 = rhs;
  rule->rhs2 = rhs2;
  rule->prob = prob;
  return rule;
}
terminal_rule *make_terminal(string lhs, string rhs, double prob){
  terminal_rule *rule = xmalloc(sizeof(terminal_rule));
  rule->lhs = lhs;
  rule->rhs = rhs;
  rule->prob = prob;
  return rule;
}
//if symbol is not in the table add it with the value of *symbol_count
//and incrmement symbol_count, otherwise do nothing
void add_symbol(hashtable *symbol_table, string symbol,
                long *symbol_count){
  if(hashtable_add(symbol_table, symbol.mem,
                   symbol.len, (void*)(*symbol_count))){
    *symbol_count++;
  }
  return;
}
//make an array from the values in symbol_table
void set_symbol_list_value(void *sym, size_t sym_len, void *idx, void *strings){
  string *arr = strings;
  long index = (long)idx;
  arr[index] = make_string(sym, sym_len);
}
string *make_symbol_list(hashtable *symbol_table){
  string *arr = xmalloc(symbol_table->entries * sizeof(string));
  hashtable_iter(symbol_table, set_symbol_list_value, arr);
  return arr;
}

grammar* parse_grammar(FILE *f){
  mem_stream * stream = FILE_to_stream(f);
  hashtable *symbol_table = make_hashtable_default();
  long symbol_count = 0;
  struct svector non_terminals = make_svector(32);
  struct svector terminals = make_svector(32);
  //we need to malloc this since tokenize might reallocate it
  string *tokens = xmalloc(4 * sizeof(string));
  string delim = make_string_lit(" \t\n");
  int tokens_size = 4;
  while(!stream_at_end(stream)){
    string line = stream_next_line(stream);
    int num_tokens = tokenize(line, delim, &tokens, &tokens_size);
/*
  Lines need to have 0, 3 or 4 tokens, blank lines are skipped
*/
//    DEBUG_PRINTF("Read %d tokens from line '%.*s'\n", num_tokens, 
//                 (int)line.len, line.str);
    if(num_tokens == 0){
      continue;
    }
    if(num_tokens != 3 && num_tokens != 4){
      fprintf(stderr, "Malformed line in grammar: %.*s",
              (int)line.len, line.str);
      goto ERROR;
    }

    add_symbol(symbol_table, tokens[0], &symbol_count);
    errno = 0;
    double prob = strtod(tokens[num_tokens-1].str, NULL);
    if(errno){
      perror("strtod");
      goto ERROR;
    }
    if(num_tokens == 3){
      terminal_rule *rule = make_terminal(tokens[0], tokens[1], prob);
      SVECTOR_PUSH(rule, terminals);
    } else {
      add_symbol(symbol_table, tokens[1], &symbol_count);
      add_symbol(symbol_table, tokens[2], &symbol_count);
      non_terminal_rule *rule = make_non_terminal(tokens[0], tokens[1],
                                                  tokens[2], prob);
      SVECTOR_PUSH(rule, non_terminals);
    }
  }
  struct grammar *ret = xmalloc(sizeof(struct grammar));
  ret->symbol_table = symbol_table;

  ret->non_terminals = (void*)svector_data(non_terminals);
  ret->num_non_terminals = svector_len(non_terminals);

  ret->terminals = (void*)svector_data(terminals);
  ret->num_terminals = svector_len(terminals);
  ret->file = stream->mem;
  ret->file_sz = stream->sz;
  free(stream);
  free(tokens);
//  free_stream(stream);
  return ret;

 ERROR:
  //cleanup stuff
  free(tokens);
  free_stream(stream);
  destroy_hashtable(symbol_table);
  free(svector_data(non_terminals));
  free(svector_data(terminals));
  return NULL;
}

/*
  Grid nodes. For non terminal productions left and right are
  the grid indices of the left and right hand sides of the parse tree.

  For terminals left is the grid index of the node itself, right is a
  negitve number, and the literal values is stored in terminal.

  The prob field is the probability for weighted grammars.
*/
struct node {
  int32_t left;
  int32_t right;
  double prob;
  union {
    struct node *next;
    string *terminal;
  };
};
struct node *make_node(int l, int r, double prob, void *next){
  struct node *ptr = xmalloc(sizeof(struct node));
  ptr->left = l; ptr->right = r;
  ptr->prob = prob;
  ptr->next = next;
  return ptr;
}
/*
  A 3d grid + a functions to index into it.
  This is a convience, it might hurt performance a bit due to
  indirection, but it makes the code eaiser to read.
*/
struct grid {
  struct node **nodes;
  int32_t len1;
  int32_t len2;
  int32_t len3;
};
//Obtains the element i,j,k in the grid as an lvalue
#define grid_val(g, i, j, k)                            \
  g->nodes[(i*g->len2*g->len3) + (j*g->len3) + k]

struct node *grid_read(struct grid *g, int i, int j, int k){
  return g->nodes[(i*g->len2*g->len3) + (j*g->len3) + k];
}
void grid_write(struct grid *g, int i, int j, int k, struct node *val){
  g->nodes[(i*g->len2*g->len3) + (j*g->len3) + k] = val;
}
//returns the 1d index in the array of nodes, corrspoinding to
//the 3d index i,j,k.
int grid_index(struct grid *g, int i, int j, int k){
  return ((i*g->len2*g->len3)+(j*g->len3)+k);
}
struct grid *make_grid(int len1, int len2, int len3){
  struct grid *grid = xmalloc(sizeof(struct grid));
  grid->len1 = len1;
  grid->len2 = len2;
  grid->len3 = len3;
  //zero the memory, it's slow but better safe than sorry
  grid->nodes = zmalloc(len1*len2*len3*sizeof(struct node));
  return grid;
}
static inline int find_symbol_index(hashtable *symbol_table, string sym){
  return (long)hashtable_find(symbol_table, sym.mem, sym.len);
}
/*
  To generate symbol_list from a symbol table 'symbols', allocate an array
  'arr' symbols->entries elements long. Then iterate over the buckets of the
  hash table and for each entry 'e' set arr[e.value] to e.key.
*/
void build_parse_tree_rec(struct grid *g, int start, string_buf *buf,
                          string *symbol_list, double *prob){
  struct node *n = g->nodes[start];
  if(prob){*prob *= n->prob;}
  if(n->right == -1){
    buf_append(buf, *n->terminal);
    return;
  }
  /*
    Due to how string buffers work it's faster to append a string literal
    than a character (or 2), if another string is going to be appended
    immediately afterwords
  */
  //add current symbol to buf
  buf_append(buf, symbol_list[start % g->len3]);
  buf_append_literal(buf, "(");
  //add left hand symbol to buf
  buf_append(buf, symbol_list[n->left % g->len3]);
  buf_append_literal(buf, "(");
  //recursively bulid the left hand side of the parse tree
  build_parse_tree_rec(g, n->left, buf, symbol_list, prob);
  buf_append_literal(buf, ") ");
  //add right hand symbol to buf
  buf_append(buf, symbol_list[n->right % g->len3]);
  buf_append_literal(buf, "(");
  //recursively build the right hand side
  build_parse_tree_rec(g, n->right, buf, symbol_list, prob);
  buf_append_literal(buf, "))");
  return;
}
string* build_parse_tree(struct grid *g, int start, 
                         string *symbol_list, double *prob){
  string_buf *buf = make_string_buf();
  build_parse_tree_rec(g, start, buf, symbol_list, prob);
  return string_buf_to_string_ptr(buf);
}


/*
  Parse the 'input' according to the given grammar and
  return a vector of strings containing the parse trees of
  all the valid ways to parse the input.
*/
static struct svector* parse(grammar *gr, string input){
  string *tokens = NULL;
  string delim = make_string_lit(" \t");
  int tokens_size = 0;
  int num_tokens = tokenize(input, delim, &tokens, &tokens_size);
  if(num_tokens <= 0){
    return NULL;
  }
  hashtable *symbols = gr->symbol_table;
  struct grid *g = make_grid(num_tokens, num_tokens,
                             symbols->entries);
  int i, j, k, l, A, B, C;
  //initalize the grid with terminal rules
  for(i=0;i<g->len1;i++){
    for(j=0;j<gr->num_terminals;j++){
      terminal_rule *rule = gr->terminals[j];
      A = find_symbol_index(symbols, rule->lhs);
      if(string_eq(tokens[i], rule->rhs)){
        grid_val(g, 0, A, j) = make_node(j, -1, rule->prob, &rule->rhs);
      }
    }
  }
  for(i = 1; i < g->len1; i++){ //length of span
    for(j = 0; j < (g->len1-(i+1)); j++){//start of span
      for(k = 0; k < (i-1); k++){//partition of span
        for(l = 0; l < gr->num_non_terminals; l++){//iterate over productions
          non_terminal_rule *rule = gr->non_terminals[l];
          A = find_symbol_index(symbols, rule->lhs);
          B = find_symbol_index(symbols, rule->rhs1);
          C = find_symbol_index(symbols, rule->rhs2);

          if(grid_read(g, k, j, B) && grid_read(g, i-k, j+k, C)){
            grid_val(g, i, j, A) = make_node(grid_index(g, k, j, B),
                                             grid_index(g, i-k, j+k, C),
                                             rule->prob,
                                             grid_read(g, i, j, A));
          }
        }
      }
    }
  }

  int start_symbol_index = (long)hashtable_find(symbols, "S", 1);

  int start_index = grid_index(g, g->len1-1, 0, start_symbol_index);

  struct node *start = g->nodes[start_index];
  struct svector *ret = zmalloc(sizeof(struct svector));
  string *symbol_list = make_symbol_list(gr->symbol_table);
  if(!start){
    fprintf(stderr, "Couldn't find start symbol\n");
    return NULL;
  }
  while(start){
    if(start->right < 0){
      fprintf(stderr, "start symbol is a terminal symbol");
      svector_push(&g->nodes[start_index]->terminal, ret);
      break;
    }
    string *parse = build_parse_tree(g, start_index, symbol_list, NULL);
    svector_push(parse, ret);
    start = start->next;
    g->nodes[start_index] = start;
  }

  free(g->nodes);
  free(g);
  free(tokens);
  free(symbol_list);
  
  return ret;
}
void cyk_parse(FILE* grammar_file, FILE* in, FILE* out){
  enable_backtraces();
  //parse grammar file
  DEBUG_PRINTF("Parsing grammar file\n");
  grammar *gr = parse_grammar(grammar_file);

  //insure there's a start symbol in the symbol_table
  char start_str[1] = {'S'};
  int has_start = hashtable_exists(gr->symbol_table, start_str, 1);
  if(!has_start){
    destroy_grammar(gr);
    return;
  }

  //read input string from stdin
  char *input_line = NULL;
  size_t input_size = 0;
  long input_len = getline(&input_line, &input_size, in);

  string input = make_string((uint8_t*)input_line, input_len);

  DEBUG_PRINTF("Parsing input string\n");
  
  svector *parses = parse(gr, input);
  if(!parses){
    return;
  }
  DEBUG_PRINTF("Printing results\n");
  while(parses->len > 0){
    string *parse = svector_pop(parses);
    fprintf(out, "%.*s\n", (int)parse->len, parse->str);
  }

  destroy_grammar(gr);
  free(parses->data);
  free(parses);
  free(input_line);
  return;
}
