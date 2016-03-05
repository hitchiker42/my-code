
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <stdbool.h>

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


void cyk_parse(void){
    fprintf(stderr, "No dynamic programming solution implemented");
    exit(1);
}

void malloc_check(void* p){
    if(p == NULL)
    {
	fprintf(stderr, "Malloc failed\n");
	exit(1);
    }
}
