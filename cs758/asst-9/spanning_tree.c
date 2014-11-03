/**
 * \file spanning_tree.c
 *
 *
 *
 * \author tucker dinapoli
 * \date 29-10-2013
 */

#define _GNU_SOURCE
#include <assert.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>
#include "graph.h"
#include <ctype.h>

void usage(){
  fprintf(stderr, "Usage:\ntree <infile> <max_distance>\n");
  exit(EXIT_FAILURE);
}
//this could be a bit better if it took an optional buffer ala getline
double read_double(FILE *infile){
  //this doesn't need to be thread safe
  static size_t buf_size = 256;
  //this buffer will never get freed, oh well
  static char *buf = NULL;
  if(!buf){
    buf = xmalloc_atomic(buf_size*sizeof(char));
  }
  char c;
  while(isspace(c=fgetc(infile))); //do nothing
  if(c == EOF){
    errno = ERANGE;
    return 0.0;
  }
  unsigned int buf_ind = 0;
  do {
    if(buf_ind >= (buf_size-1)){//buf_size-1 for trailing null byte
      buf = xrealloc(buf, buf_size*2);
      buf_size *=2;
    }
    buf[buf_ind++] = c;
  } while (!(isspace(c = fgetc(infile))));
  buf[buf_ind] = '\0';
  errno = 0;
  char *endptr = NULL;
  double d = strtod(buf, &endptr);
  if(*endptr != '\0'){
    fprintf(stderr, "invalid number: %s\n", buf);
    errno = EILSEQ;
    return 0.0;
  } else if(errno == ERANGE){
    fprintf(stderr, "Number out of range: %s\n",buf);
    errno = EILSEQ;
    return 0.0;
  } else {
    errno = 0;
    return d;
  }
}

int determine_decimal_digits(unsigned long x){
  int binary_digits = 64 - __builtin_clzl(x);
  //0.30102999566 is roughly equal to log(2)/log(10)
  int decimal_digits = ceil(0.30102999566 * binary_digits);
  return decimal_digits;
}
int find_spanning_tree(FILE *infile, double max_dist){
  DEBUG_PRINTF("Begining Program\n");
  char *line = NULL;
  size_t line_size = 0;
  int err = getline(&line, &line_size , infile);
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
  DEBUG_PRINTF("Expecting %ld number of points with max distance %f\n",
               num_points, max_dist);
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
  graph_edge *edges = xmalloc_atomic(sizeof(graph_edge) * num_points);
  double weight = make_spanning_tree(edges, points, num_points, max_dist);
  int decimal_digits = determine_decimal_digits(num_points);
  for(i=0;i<num_points-1;i++){
    __extension__ printf("%2$*1$u %3$*1$u\n", decimal_digits,
                         edges[i].start,edges[i].end);
  }
  printf("%lf\n", weight);
  fclose(infile);
  free(edges);
  free(points);
  return 0;
}
int main(int argc, const char *argv[]){
  if (argc != 3){
    usage();
  }  
  if (string_eq(argv[1],"--help") || string_eq(argv[1],"-h")){
    usage();
  }
  double d = strtod(argv[2],NULL);
  if(d > 1.5){
    fprintf(stderr,"Invalid maximum distance %f\n "
            "maximum distance must be between 0 and 1.5\n",d);
    return 1;
  }
  FILE *infile = fopen(argv[1],"r");
  if(!infile){
    perror("fopen");
    return 1;
  }
  return find_spanning_tree(infile, d);
}
