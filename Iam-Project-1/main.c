#include "kdv.h"
#include <getopt.h>
#include <unistd.h>
#include <ctype.h>
int main(int argc,char** argv){
  //argument parsing, just layout for now
  //kdv -h [-d](0.1) [-i](0) [-s](0.1) [-r](40)
  //-h=help -d delta x,x step -f function -i=inital x val,
  //-s = time step -r = x range
  int arg;
  int r = 40,i=0;
  double d = 0.1,s=0.1;
  while((arg=getopt(argc,argv,"hd:i:s:r:"))!=-1){
    switch (arg) {
    case 'h':
      help();
    case 'd':
      d=(double)atof(optarg);
      break;
    case 'i':
      i=atoi(optarg);
      break;
    case 's':
      s=(double)atof(optarg);
    case 'r':
      r=atoi(optarg);
      if(r<=0){
        fprintf(stderr,"Range must be positive, ignoring range arguement\n");
        r=40;
      }
    case '?':
      if (isprint (optopt)){
        fprintf (stderr, "Unknown option `-%c'.\n", optopt);
        return 2;
      }
      else{
        fprintf (stderr,"Unknown option character `\\x%x'.\n",optopt);
        return 1;
      }
    }
    double *x=seq(i,r+i,d);
    double *u=map(x,r,(*inital_u));
    //call something 
  }
}
