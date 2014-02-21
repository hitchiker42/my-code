#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
typedef union float32 float32;
typedef struct float_add_args float_add_args;
union float32 {
  float real32;
  uint32_t int32;
};
struct float_add_args {
  float32 a;
  float32 b;
};
int32_t FloatAdd (int32_t a_bits, int32_t b_bits);
int32_t call_FloatAdd(float_add_args args){
  return FloatAdd(args.a.int32,args.b.int32);
}
float_add_args parse_interactive_input(char *input){
  char *endptr;
  float a,b;
  a=strtof(input,&endptr);
  while(*endptr++!='+');
  b=strtof(endptr,NULL);
  float_add_args retval;
  retval.a.real32=a;
  retval.b.real32=b;
  return retval;
} 
float_add_args parse_commandline_input(char *argv[]){
  uint32_t a,b;
  a=strtoul(argv[1],NULL,0);
  b=strtoul(argv[3],NULL,0);
  float_add_args retval;
  retval.a.int32=a;
  retval.b.int32=b;
  return retval;
}
int main(int argc,char *argv[]){
  if(argc<3){
    fprintf(stderr,"to few inputs, input is of the form 'float + float'\n");
    exit(EXIT_FAILURE);
  }
  float_add_args args=parse_commandline_input(argv);
  float32 float_ans;
  float_ans.real32=args.a.real32+args.b.real32;
  float32 ans;
  ans.int32=call_FloatAdd(args);
  printf("result using float add was %#0x, or %f as a float\n" 
         "using normal floating point arithmatic the answer was %#0x or %f as a float\n",
         ans.int32,ans.real32,float_ans.int32,float_ans.real32);
}
