#include "common.h"
#include <getopt.h>
void help(){
  printf("help");
  exit();
}
int main(int argc,char *argv[]){
  rl_set_signals (void);
  
  int c;
  static struct option long_options[] = {
    {"option name", requried_optional_no_argument,0,'equivlant short opt/ret_val'},
    {...},
    {0,0,0,0},
  };
  while(1){
    c=getopt_long(argc,argv,"n:m:h",long_options,NULL);
    if(c==-1){break;}
    switch(c){
      case 'n':
        do_stuff;
        break;
      case 'm':
        break;
      case 'h':
        help();
      default:
        printf("?? getopt returned character code 0%o ??\n", c);
    }
  }
  if (optind<argc){
    //if we have non option arguments we're compiling a file
    
