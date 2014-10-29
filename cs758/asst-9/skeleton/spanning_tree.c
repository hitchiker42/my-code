/**
 * \file spanning_tree.c
 *
 *
 *
 * \author jtd7
 * \date 20-10-2011
 */

#define _POSIX_C_SOURCE 200112L
#include <assert.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>
#include "graph.h"

void usage(){
	fprintf(stderr, "Usage:\ntree <infile> <max_distance>\n");
	exit(EXIT_FAILURE);
}

int solve(struct graph* g){
	/* You should fill this in! */
}


int process(const char* infile, double max_distance){
	float *x_array;
	float *y_array;
	int num_points = -1;
	FILE *ifile = fopen(infile, "r");
	char buf[256];
	char c = '\0';
	int b_index = 0;
	int p_index = 0;
	float x = 0.;
	float y = 0.;
	int on_x = 1;
	struct points* p;
	struct graph* g;
	int err = 0;

	if(ifile == NULL){ /* fopen failed */
		fprintf(stderr, "Failed to open %s\n", infile);
		return EXIT_FAILURE;
	}else{  /* get input size */
		while((c = getc(ifile)) != '\n'){
			buf[b_index] = c;
			b_index++;
		}
		num_points = atoi(buf);
		x_array = malloc(sizeof(float) * num_points);
		y_array = malloc(sizeof(float) * num_points);
		b_index = 0;
		while((c = getc(ifile)) != EOF){
			buf[b_index] = c;
			if(c == '\n' || c == ' '){
				if (on_x){
					x = atof(buf);
					on_x = 0;
				}else{
					y = atof(buf);
					x_array[p_index] = x;
					y_array[p_index] = y;
					p_index ++;
					on_x = 1;
				}
				b_index = 0;
			}else b_index++;
		}
	}
	p = newPoints(x_array, y_array, num_points);
	g = newGraph(p, max_distance);

	/* solve the graph */
	err = solve(g);
	/* cleanup here */
	fclose(ifile);
	return err;
}

int main(int argc, const char *const argv[]){
	double d;
	if (argc != 3)	usage();
	d = atof(argv[2]);
	return process(argv[1], d);
}
