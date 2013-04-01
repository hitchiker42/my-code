#include "kdv.h"
#include <getopt.h>
#include <unistd.h>
int main(int argc,char** argv[]){
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
      d=optarg;
      break;
    case 'i':
      i=optarg;
      break;
    case 's':
      s=optarg;
    case 'r':
      if(r<=0){
        fprintf(stderr,"Range must be positive, ignoring range arguement\n");
      }
      r=optarg;
    case '?':
      if (isprint (optopt)){
        fprintf (stderr, "Unknown option `-%c'.\n", optopt);
        return;
      }
      else{
        fprintf (stderr,"Unknown option character `\\x%x'.\n",optopt);
        return;
      }
    }
  }
}
void help (){
  const char * help = "kdv -h [-d(0.1)] [-f(0)] [-i(0)] [-s(0.1)] [-r(40)]\
Command line interface to a Korteweg de Vires equation\
solver\nWhen run with no options runs the solver with:\n\tinital condition\
 u=sin(pi*x)\n\tx inital value 0, final value 40 and step 0.1\n\t time step 0.1\
\n\tand using the gsl qgas interation method.
Options:
\t-h (--help,to be added) display this help and exit
\t-d (--delta_x,tba) set the difference between successive x values
\t-f (--function,tba) set the function to use for numerical integration
\t-i (--xinit,tba) set the inital x value
\t-s (--delta_t,tba) set the time step
\t-r (--range,tba) set the range of x values (ie x max = x inital + range)";
  printf(help);
}
