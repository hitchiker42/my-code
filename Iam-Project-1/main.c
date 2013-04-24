#include "kdv.h"
#include <getopt.h>
#include <unistd.h>
#include <ctype.h>
int main(int argc,char** argv){
  int arg;
  int r = (20 * M_PI),i=(-10 * M_PI);
  double d =(M_PI/8),s=pow(d,3),t_max=10;
    const char *name="kdv_data";
    //d=x step,s=time step, i=x inital, r+i=x max and t_max=time_max
  double *x=seq(i,r+i,d);
  double *u=map(x,r,(*inital_u));
  double *u_p=malloc(r*sizeof(double));
  double t=0;
  int j;
  /*open file here*/
  FILE* file=fopen(name,"w");
  mem_check(file);
  fprintf(file,"#KdV data for use with gnu plot\n");
  clone(u,u_p,r);
  MAIN_LOOP:while(t<t_max){
    //#pragma omp parallel num_threads(2)
    //   {
    //#pragma omp single 
    //  {
        fprintf(file,"#data for time %.2f\n",t);
        for(j=0;j<r;j++){
          fprintf(file,"Pt %d:%10f\t%10f\n",j,x[j],u_p[j]);
        }
        //  }
        //#pragma omp single
      update(u,x,t,d,s,r);
      //#pragma omp barrier
        //   }
    t+=s;
    clone(u,u_p,r);//copy elements of u to u_p by value 
  }
  fprintf(file,"#data for time %.2f",t);
  for(j=0;j<r;j++){
    fprintf(file,"%10.3f\t%10.3f\n",x[j],u[j]);
  }
  fclose(file);
  return 0;
}
  //argument parsing, just layout for now
  //kdv -h [-d](0.1) [-i](0) [-s](0.1) [-r](40) [-t](10)
  //-h=help -d delta x,x step -f function -i=inital x val,
  //-s = time step -r = x range -t = time max
    /*while((arg=getopt(argc,argv,"hd:i:s:r:t:"))!=-1){
    switch (arg) {
    case 'h':
      help();
      return 0;
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
    case 't':
      t_max=(double)atof(optarg);
      if(t_max<=0){
        fprintf(stderr,"Time must be positive, ignoring time arguement\n");
        t_max=10;
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
    }*/
