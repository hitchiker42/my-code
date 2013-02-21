#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include "vec_math.h"
#define N 3
#define M 4
//Write argparser here
int main(int argv,char** args){
  //Argparse stuff
  int c;
  int result;
  int n=3;
  int m=3;
  FILE* file=stdout;
  //  file output:
  opterr = 0;
  while ((c = getopt (argv, args, "asmhf:")) != -1){
    switch(c) {
    case 'f':
      file=fopen(optarg,"w");
      break;
    case 'a':
      result=add_test(n,file);
      return result;
      break;//totally unnecessary but eh
    case 's':
      result=scalar_test(n,file);
      return result;
      break;
    case 'm':
      result=vec_mult_test(n,m,file);
      return result;
      break;
    case 'h':
    HELP:
      printf("usage:\n-f file,write output to given file\n"
             "-a,vector additon\n"
             "-s,dot product\n"
             "-m,matrix multiplication\n"
             "-h,print this help and exit\n");
      return 0;
    case '?':
      printf ("Unknown option `-%c'\n", optopt);
      return 2;
      break;
    default:
      return 3;
    }
  }
  goto HELP;//GASP goto
  //printf ("call each option with no further parameters to run the tests");
  return 99;//Really can't ever get here
}
