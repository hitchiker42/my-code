/*
//
//  parser.c
//  Parser
//
//  Created by Christopher Wilt on 10/5/12.
//  Copyright (c) 2012 Christopher Wilt. All rights reserved.
//
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "parser.h"
#include "cyk.h"
#include "rdp.h"

/**
   index of the start symbol in the symbol table
 */
int start_index = -1;


/**
   rules are stored in a linked list, this is the head pointer.
 */
const rule* rule_list = NULL;
/**
   parts of speech are stored in a linked list, this is the head pointer.
 */
const word_rule* word_list = NULL;
/*
  Count of all rules
 */
int rule_count = 0;
/*
  count of add word -> speech rules
 */
int word_count = 0;
/*
  Total number of chracters in the input
 */
int input_length = 0;
/*
  Array of the tokens in the input sentence.
 */
char* tokenized_input [MAX_INPUT] = {NULL};
/*
  Raw data from input
 */
char input[MAX_CHARS] = {0};

/*
  Symbol table
 */
char* symbol_table[MAX_SYMBOLS] = {NULL};
/*
  Count of all nontermial symbols in the grammar.
 */
int symbol_count = 0;


/*
  prints all the terminal productions
 */
void print_words(void){
    const word_rule* next_word = word_list;
    while(next_word != NULL){
        printf("word %s %s\n", next_word->lhs, next_word->word);
        next_word = next_word->next;
    }
}
/*
  prints all the nontermial productions
 */
void print_rules(void){
    const rule* next_rule = rule_list;
    while(next_rule != NULL){
        printf("rule %s %s %s\n", next_rule->lhs, next_rule->rhs1, next_rule->rhs2);
        next_rule = next_rule->next;
    }
}

/*
  reads the sentence from standard in in, and puts it in the tokenized
  input array.
 */
void read_input(){
    int current = 0;
    char* next_word;
    if(fgets(input, MAX_CHARS, stdin) == 0){
	fprintf(stderr, "error reading standard in\n");
	exit(1);
    }
    next_word = strtok(input, " \n");
    while(next_word != NULL){
	if(current >= MAX_INPUT){
	    fprintf(stderr, "can't read in more than %d words\n", MAX_INPUT);
	    exit(1);
	}
	tokenized_input[current] = next_word;

	current ++;
	next_word = strtok(NULL, " \n");
    }
    input_length = current;
}

/*
    looks for the index of the specified symbol.
 */
int find_symbol_index(const char* looking_for){
    int current_ix = 0;
    char* current = symbol_table[current_ix];
    while(current != NULL){
	if(strcmp(current, looking_for) == 0)
	    return current_ix;
	current_ix ++;
	current = symbol_table[current_ix];
    }
    return -1;
}

/*
    prints out a list of all the symbols, along with their indices.
 */
void print_symbols(){
    for(int i = 0; i < symbol_count; i++){
	printf("symbol: %s\n", symbol_table[i]);
    }
}

/**
 reads in the rules from the specified file, and creates both the
 rule list and the word list.
 */
void read_rules(FILE* f){
    char* line = malloc(LINEMAX);
    if(line == NULL){
        fprintf(stderr, "malloc failed");
        exit(1);
    }
    while ( fgets ( line, LINEMAX, f ) != NULL ) /* read a line */
    {
        char* char1 = strtok(line, " \n");
	int new_index1 = find_symbol_index(char1);
	char* char2 = strtok(NULL, " \n");
        char* char3 = strtok(NULL, " \n");
        char* char4 = strtok(NULL, " \n");
        char* junk = strtok(NULL, " \n");
	
	if(new_index1 == -1){
	    assert(symbol_table[symbol_count] == NULL);
	    symbol_table[symbol_count] = char1;
	    symbol_count ++;
	}

	
        assert(junk == NULL);
        if(char4 != NULL){
            rule* new_rule = malloc(sizeof(rule));
            rule_count ++;
            new_rule->lhs = char1;
            new_rule->rhs1 = char2;
            new_rule->rhs2 = char3;
            new_rule->next = rule_list;
            rule_list = new_rule;
	    int new_index2 = find_symbol_index(char2);
	    int new_index3 = find_symbol_index(char3);
	    if(new_index2 == -1){
		assert(symbol_table[symbol_count] == NULL);
		symbol_table[symbol_count] = char2;
		symbol_count ++;
	    }
	    if(new_index3 == -1){
		assert(symbol_table[symbol_count] == NULL);
		symbol_table[symbol_count] = char3;
		symbol_count ++;
	    }
	    sscanf(char4, "%lf", &(new_rule->probability));

        }
        else {
            word_rule* new_word = malloc(sizeof(word_rule));
            word_count ++;
            new_word->lhs = char1;
            new_word->word = char2;
            new_word->next = word_list;
	    sscanf(char3, "%lf", &(new_word->probability));
	    word_list = new_word;
        }
        line = malloc(LINEMAX);
        if(line == NULL){
            fprintf(stderr, "malloc failed");
            exit(1);
        }
    }
    free(line);
    fclose ( f );
    start_index = find_symbol_index("S");
    if(start_index == -1){
	fprintf(stderr, "Error - no start symbol (S is the expected start symbol)\n");
	exit(1);
    }
}

void usage(void){
    printf("usage: ./parser grammar.cnf <alg>\n");
    printf("<alg> may be either rdp (recursive descent parser) or cyk,\n");
    printf("which is the name of the dynamic programming parsing algorithm\n");
}

/*
 deletes all of the rules, and the words.
 */
void cleanup_rules(void){
    while( word_list != NULL){
	const word_rule* to_free = word_list;
	free((void*) to_free->lhs);
	word_list = word_list->next;
	free((void*)to_free);
    }
    while(rule_list != NULL){
	const rule* to_free = rule_list;
	free((void*) to_free->lhs);
	rule_list = rule_list->next;
	free((void*) to_free);
    }
}

/*
 simple main function
 */
int main(int argc, const char * argv[])
{
    FILE* f;
    if(argc != 3){
        usage();
    }
    f = fopen(argv[1], "r");

    if(f == NULL){
        fprintf(stderr, "Error - no such file %s", argv[1]);
        return 1;
    }
    
    read_rules(f);
/*
    print_words();
    print_rules();
    print_symbols();
*/
    read_input();

    if(strcmp(argv[2], "rdp") == 0)
v        rdp_parse();
    else if(strcmp(argv[2], "cyk") == 0)
        cyk_parse();
    else
        usage();

    cleanup_rules();

    return 0;
}

