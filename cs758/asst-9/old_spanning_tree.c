/**
 * \file spanning_tree.c
 *
 *
 *
 * \author tucker dinapoli
 * \date 29-10-2013
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
//this could be a bit better if it took an optional buffer ala getline
double read_double(FILE *infile){
  //this doesn't need to be thread safe
  static size_t buf_size = 256;
  //this buffer will never get freed, oh well
  static char *buf = xmalloc_atomic(buf_size*sizeof(char));
  while(isspace(c=fgetc(infile))); //do nothing
  if(c == EOF){
    errno = ERANGE;
    return 0.0;
  }
  ungetc(c,infile);
  unsigned int buf_ind = 0;
  while (!(isspace(c = fgetc(infile)))) {
    if(buf_ind >= (buf_size-1)){//buf_size-1 for trailing null byte
      buf = xrealloc(buf, buf_size*2);
      buf_size *=2;
    }
    buf[buf_ind++] = c;
  }
  buf[buf_ind] = '\0';
  errno = 0;
  char *endptr;
  double d = strtod(buf, &entptr);
  if(endptr != NULL || errno = ERANGE){
    fprintf(stderr, "invalid number %s\n", buf);
    errno = EILSEQ;
    return 0.0;
  } else {
    errno = 0;
    return d;
  }
}
int process(FILE *infile, double max_distance){
  char *line;
  int err = getline(&line, NULL, infile);
  if(errno == -1){
    fprintf(stderr, "Unable to read from file\n");
    return 1;
  }
  errno = 0;
  long num_points = strtol(line, NULL, 0);
  if(num_points <= 0 || errno == ERANGE){
    fprintf(stderr,"invalid number of points%s\n",line);
    free(line);
    return 1;
  }
  free(line);
  float *x_array = xmalloc(sizeof(float) * num_points);
  float *y_array = xmalloc(sizeof(float) * num_points);
  int p_index = 0;
  int i;
  for(i=0;i<num_points;i+=2){
    double x = read_double(infile);
    if(errno == EILSEQ){return 1;}
    double y = read_double(infile);
    if(errno == EILSEQ){return 1;}
    if(errno == ERANGE){
      fprintf(stderr,"Unexpected end of file\n");
      return 1;
    }
    x_array[p_index] = x;
    y_array[p_index] = y;
    p_index ++;
  }
  struct points *p = newPoints(x_array, y_array, num_points);
  struct graph *g = newGraph(p, max_distance);
  /* solve the graph */
  err = solve(g);
  /* cleanup here */
  fclose(infile);
  return err;
}

int main(int argc, const char *argv[]){
  if (string_eq(argv[1],"--help") || string_eq(argv[1],"-h")){
    usage();
  }
  if (argc != 3){
    usage();
  }
  double d = strtod(argv[2],NULL);
  if(d > 1.5){
    fprintf(stderr,"Invalid maximum distance %f\n "
            "maximum distance must be between 0 and 1.5\n");
    return 1;
  }
  FILE *infile = fopen(argv[1]);
  if(!infile){
    perror("fopen");
    return 1;
  }
  return process(infile, d);
}
int determine_decimal_digits(unsigned long x){
  int binary_digits = 64 - __builtin_ctzl(x);
  //0.30103 is roughly equal to log(2)/log(10)
  return 0.30103 * binary_digits;
}
int my_procss(FILE *infile, double max_dist){
  char *line;
  int err = getline(&line, NULL, infile);
  if(errno == -1){
    fprintf(stderr, "Unable to read from file\n");
    return 1;
  }
  errno = 0;
  long num_points = strtol(line, NULL, 0);
  if(num_points <= 0 || errno == ERANGE){
    fprintf(stderr,"invalid number of points%s\n",line);
    free(line);
    return 1;
  }
  free(line);
  vertex *points = xmalloc_atomic(sizeof(vertex)*num_points);
  int i;
  for(i=0;i<num_points;i++){
    double x = read_double(infile);
    if(errno == EILSEQ){return 1;}
    double y = read_double(infile);
    if(errno == EILSEQ){return 1;}
    if(errno == ERANGE){
      fprintf(stderr,"Unexpected end of file\n");
      return 1;
    }
    points[i] = (vertex){.x = x,.y = y, .index = i, .parent = points+i};
  }
  graph_edge *edges = xmalloc_atomic(sizeof(edge) * num_points);
  int tree_size = make_spanning_tree(edges, points, num_points, max_dist);
  double weight = 0.0;
  int decimal_digits = determine_decimal_digits(num_points);
  for(i=0;i<tree_size;i++){
    weight += edges[i].weight;
    printf("%1$0*3$ld $2$0*$3ld\n", edges[i].start,
           edges[i].end,decimal_digits);
  }
  printf("%lf", weight);
  return 0;
}
