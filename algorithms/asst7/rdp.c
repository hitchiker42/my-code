/**
    Recursive descent parser
 */
#include <stdio.h>
#include <assert.h>
#include <string.h>
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

typedef struct {
    int start_ix;
    int end_ix;
    int subparse_count;
    char** subparses;
    double* probabilities;
    void** rules;
} rdp_node;

static void print(rdp_node* n){
    fprintf(stderr, "start %d end %d subparses: %d\n", n->start_ix, n->end_ix, n->subparse_count);
    for(int i = 0; i < n->subparse_count; i++){
        assert(n != NULL);
        assert(n->subparses != NULL);
        assert(n->subparses[i] != NULL);
        const rule** r = (const rule**)n->rules;
        fprintf(stderr, "subparse %d: <", i);
        fprintf(stderr, "%s> using rule %s->%s %s\n", n->subparses[i],
                r[i]->lhs,
                r[i]->rhs1,
                r[i]->rhs2);
    }
}

/**
 Adds the contents of victim to recipient
 */
static void add_rdp_nodes(rdp_node* recipient, rdp_node* victim){
    if(recipient->start_ix != victim->start_ix){
        fprintf(stderr, "rec start %d vic start %d\n", recipient->start_ix, victim->start_ix);
        assert(false);
        
    }
/*
    if(recipient->end_ix != victim->end_ix){
        fprintf(stderr, "rec end %d vic end %d\n", recipient->end_ix, victim->end_ix);
        assert(false);
    }
 */
    int n_items = recipient->subparse_count + victim->subparse_count;
/*
    fprintf(stderr, "recipient: ");
    print(recipient);
    fprintf(stderr, "victim: ");
    print(victim);
  */  
    assert(n_items > 0);
    
    char** new_subparses = xmalloc(sizeof(char*) * n_items);
    const rule** new_rules = xmalloc(sizeof(rule*) * n_items);
    double* new_probabilities = xmalloc(sizeof(double*) * n_items);

    assert(recipient != NULL);

    int set_count = 0;
    
    for(int i = 0; i < recipient->subparse_count; i++){
        set_count ++;
        new_subparses[i] = recipient->subparses[i];
        new_probabilities[i] = recipient->probabilities[i];
        new_rules[i] = recipient->rules[i];
    }
    for(int i = 0; i < victim->subparse_count; i++){
        set_count ++;
        new_subparses[i + recipient->subparse_count] = victim->subparses[i];
        new_probabilities[i + recipient->subparse_count] = victim->probabilities[i];
        new_rules[i + recipient->subparse_count] = victim->rules[i];
    }
    
    free(recipient->probabilities);
    free(recipient->subparses);
    free(recipient->rules);
    free(victim->probabilities);
    free(victim->subparses);
    free(victim->rules);
    recipient->subparses = new_subparses;
    recipient->probabilities = new_probabilities;
    recipient->rules = (void*) new_rules;
    recipient->subparse_count = n_items;/*
    fprintf(stderr, "completed");
    print(recipient);
 */
}

/**
 combines two rdp_nodes
 */
static void mult_rdp_nodes(rdp_node* recipient, rdp_node* victim, const char* nt){
    /* make sure they're adjacent to eachother */
    assert(recipient->end_ix == victim->start_ix - 1);
    assert(recipient->subparse_count > 0);
    assert(victim->subparse_count > 0);
    
    int n_items = recipient->subparse_count * victim->subparse_count;
    char** new_subparses = xmalloc(sizeof(char*) * n_items);
    double* new_probabilities = xmalloc(sizeof(double*) * n_items);
    const rule** const new_rules = xmalloc(sizeof(rule*) * n_items);


/*
    fprintf(stderr, "mult rec :");
    print(recipient);
    fprintf(stderr, "mult vic :");
    print(victim);
 */
    for(int i = 0; i < recipient->subparse_count; i++){
        for(int j = 0; j < victim->subparse_count; j++){
            int new_length = strlen(victim->subparses[j]);
            new_length += strlen(recipient->subparses[i]);
            const rule* r = recipient->rules[i];
            new_length += strlen(r->lhs) + 5;
            char* new_string = xmalloc(new_length);
            
            if(strcmp(nt, r->lhs) != 0){
                fprintf(stderr, "error using %s->%s %s to produce %s\n",
                        r->lhs, r->rhs1, r->rhs2, nt);
                assert(false);
            }
            
            
            
            sprintf(new_string, "%s(%s %s)", r->lhs, recipient->subparses[i], victim->subparses[j]);

            int sub_index = i * victim->subparse_count + j;
            new_subparses[sub_index] = new_string;
            new_probabilities[sub_index] = r->probability * recipient->probabilities[i] * victim->probabilities[j];
            new_rules[sub_index] = recipient->rules[i];
        }
    }

    
    for(int i = 0; i < recipient->subparse_count; i++){
        free(recipient->subparses[i]);
    }
    for(int i = 0; i < victim->subparse_count; i++){
        free(victim->subparses[i]);
    }

    free(recipient->probabilities);
    free(recipient->subparses);
    free(victim->probabilities);
    free(victim->subparses);
    recipient->subparses = new_subparses;
    recipient->probabilities = new_probabilities;
    recipient->rules = (void*) new_rules;
    recipient->subparse_count = n_items;
    recipient->end_ix = victim->end_ix;
    /*
    fprintf(stderr, "mult completed using :");
    print(recipient);
*/
}

/**
 deletes a rdp_node
 */
static void junk_rdp_node(rdp_node* junk){
    for(int i = 0; i < junk->subparse_count; i++){
        free(junk->subparses[i]);
    }
    free(junk->subparses);
    free(junk->probabilities);
}

static rdp_node parse(const char* nt, const int start, 
                      const int end, const rule* parent){
    assert(start <= end);
    rdp_node to_return;
    to_return.start_ix = start;
    to_return.end_ix = end;
    to_return.subparse_count = 0;
    to_return.probabilities = NULL;
    to_return.subparses = NULL;
    to_return.rules = NULL;
    
    rdp_node left_parses[RDP_MAX];
 //   rdp_node right_parses[RDP_MAX];

    for(int i = 0; i < RDP_MAX; i++){
        left_parses[i] = to_return;
 //       right_parses[i] = to_return;
    }
    
    /*
     base case
     */
    if(end - start == 0){
        const word_rule* current_word = word_list;
        while(current_word != NULL)
        {
            if(strcmp(nt, current_word->lhs) == 0){
                if(strcmp(current_word->word, tokenized_input[start]) != 0){
                    current_word = current_word->next;
                    continue;
                }
                char* copy = xmalloc(strlen(current_word->word) + strlen(nt) + 3);

                sprintf(copy, "%s(%s)", nt, current_word->word);
                to_return.subparses = xmalloc(sizeof(char*));
                to_return.probabilities = xmalloc(sizeof(double));
                to_return.rules = xmalloc(sizeof(void*));

                to_return.subparses[0] = copy;
                to_return.probabilities[0] = current_word->probability;
                to_return.rules[0] = (void*) parent;
                to_return.subparse_count = 1;
                break;
            }
            current_word = current_word->next;
        }
    } else {
        /*
         
         */
        for(int split_point = start; split_point < end; split_point++){
/*
            fprintf(stderr, "split_point: %d (%d - %d)\n", split_point, left_parses[split_point - start].start_ix, start);
*/
            left_parses[split_point - start].end_ix = split_point;
//            right_parses[split_point - start].start_ix = split_point + 1;
            const rule* next_rule = rule_list;
            assert(left_parses[split_point - start].start_ix == start);
            while(next_rule != NULL){
                assert(left_parses[split_point - start].start_ix == start);

                if(strcmp(next_rule->lhs, nt) != 0){
                    next_rule = next_rule->next;
                    continue;
                }
                rdp_node left = parse(next_rule->rhs1, start, split_point, next_rule);
                rdp_node right = parse(next_rule->rhs2, split_point + 1, end, next_rule);
                assert(left.start_ix == start);
                assert(right.end_ix == end);
                
                if(left.subparse_count == 0 && right.subparse_count == 0){
                    next_rule = next_rule->next;
                    continue;
                }
                if(left.subparse_count != 0 && right.subparse_count == 0){
                    junk_rdp_node(&left);
                    next_rule = next_rule->next;
                    continue;
                }
                if(left.subparse_count == 0 && right.subparse_count != 0){
                    junk_rdp_node(&right);
                    next_rule = next_rule->next;
                    continue;
                }
                assert(left.rules != NULL);
                assert(right.rules != NULL);

                /*
                 fix the rules for left and right
                 */
                const rule** right_rules = (const rule**) right.rules;
                for(int i = 0; i < right.subparse_count; i++){
                    right_rules[i] = next_rule;
                }
                const rule** left_rules = (const rule**) left.rules;
                for(int i = 0; i < left.subparse_count; i++){
                    left_rules[i] = next_rule;
                }
                
                mult_rdp_nodes(&left, &right, next_rule->lhs);
                add_rdp_nodes(&left_parses[split_point - start], &left);

//                add_rdp_nodes(&right_parses[split_point - start], &right);
                next_rule = next_rule->next;
            }
        }
        for(int i = 0; i < end - start; i++){
            if(left_parses[i].subparse_count == 0){
 //               assert(right_parses[i].subparse_count == 0);
                continue;
            }
/*
            mult_rdp_nodes(&left_parses[i], &right_parses[i], nt);
  */          
        }
        for(int i = 0; i < end - start; i++){
            if(left_parses[i].subparse_count == 0){
                continue;
            }
            add_rdp_nodes(&to_return, &left_parses[i]);
        }
    }
    /*
    fprintf(stderr, "about to return this:");
    print(&to_return);
    */
    return to_return;
}

void rdp_parse(void){
    if(input_length > RDP_MAX){
        fprintf(stderr, "can't recursive descent parse something with more than %d input symbols", RDP_MAX);
        exit(1);
    }
    
    const rule* start = NULL;
    for(const rule* r = rule_list; r != NULL; r = r->next){
        if(strcmp(r->lhs, "S") == 0)
        {
            start = r;
            break;
        }
    }

    if(start == NULL){
        fprintf(stderr, "Error - could not find start symbol rule");
        exit(1);
    }
    
    rdp_node n = parse("S", 0, input_length - 1, start);
    
    for(int i = 0; i < n.subparse_count; i++){
        printf("%s %lf\n", n.subparses[i], n.probabilities
               [i]);
    }
}
