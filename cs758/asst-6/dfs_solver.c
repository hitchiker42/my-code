/**
 * \file dfs_solver.c
 *
 * Solves the alignment problem using depth first search
 *
 * \author jtd7
 * \date 03-10-2011
 */
#include "dfs_solver.h"

static unsigned int best_cost = 0;
static char* sol_str1 = NULL;
static char* sol_str2 = NULL;


struct problem {
	char* string1;
	char* string2;
	unsigned int len1;
	unsigned int len2;
};

unsigned int max(unsigned int a, unsigned int b){
	if (a >= b) return a;
	else return b;
}

void handle_sol(char** str1, char** str2, unsigned int len){
	memcpy(sol_str1, *str1, len * sizeof(char));
	memcpy(sol_str2, *str2, len * sizeof(char));
}

void dfs_solve(struct problem *p,
	       unsigned int index1, unsigned int index2,
	       char** str1, char** str2,
	       unsigned int score, unsigned int index){

	/*consume match*/
	if ((index1 >= p->len1) && (index2 >= p->len2)){
		if (score > best_cost){
			(*str1)[index] = '\0';
			(*str2)[index] = '\0';
			best_cost = score;
			handle_sol(str1, str2, p->len1 + p->len2 + 1);
		}
	}else if((index1 >= p->len1) && (index2 < p->len2)){
		 /*insert gap (*str1)*/
		(*str1)[index] = '-';
		(*str2)[index] = p->string2[index2];
		dfs_solve(p, index1, index2+1, str1, str2, score, index+1);
	}else if((index1 < p->len1) && (index2 >= p->len2)){
		/*insert gap (*str2)*/
		(*str1)[index] = p->string1[index1];
		(*str2)[index] = '-';
		dfs_solve(p, index1+1, index2, str1, str2, score, index+1);
	}else{ /* both strings have characters that can be consumed */
		if (p->string1[index1] == p->string2[index2]){
			(*str1)[index] = p->string1[index1];
			(*str2)[index] = p->string2[index2];
			dfs_solve(p, index1+1, index2+1,
				  str1, str2, score+1, index+1);
		}
		/*consume str2 only*/
		(*str1)[index] = '-';
		(*str2)[index] = p->string2[index2];
		dfs_solve(p, index1, index2+1, str1, str2, score, index+1);
		/*consume str1 only*/
		(*str1)[index] = p->string1[index1];
		(*str2)[index] = '-';
		dfs_solve(p, index1+1, index2, str1, str2, score, index+1);
	}
}


int call_dfs_solver(char *string1, unsigned int len1,
		    char *string2, unsigned int len2){
	struct problem *p = malloc(sizeof(struct problem));
	unsigned int max_leng = len1 + len2 + 1;
	char* output_1 = calloc(max_leng,sizeof(char));
	char* output_2 = calloc(max_leng,sizeof(char));
	int err = 0;
	sol_str1 = calloc(max_leng,sizeof(char));
	sol_str2 = calloc(max_leng, sizeof(char));
	p->string1 = string1;
	p->string2 = string2;
	p->len1 = len1;
	p->len2 = len2;
	dfs_solve(p, 0, 0, &output_1, &output_2, 0, 0);
	printf("Alignment Score: %i\n", best_cost);
	output_alignment(sol_str1, sol_str2);
	free(p);
	free(sol_str1);
	free(sol_str2);
	free(output_1);
	free(output_2);
	return err;
}
