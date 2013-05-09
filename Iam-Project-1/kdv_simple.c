#include <ctype.h>
#include <string.h>
#include "calculus.h"
#include <omp.h>
double
 inital_u(double y){
  return -12*pow((1/cosh(y)),2);
}
double*
u_discrete(double* u,int len,double h){
  int i;
  double *u_x=malloc(len*sizeof(double));
  //core parallel bit
  //#pragma omp parallel for
  for(i=0;i<len;i++){
    u_x[i]=-1*Stencil_5pt(u,i,len,3,h);
    u_x[i]+= 6*u[i]*Stencil_5pt(u,i,len,1,h);
  }
  return u_x;
}
double* vec_add_kdv (double* x,double* y,int len,double scale){
  int i;
  double *xy=malloc(len*sizeof(double));
  //#pragma omp parallel for
  for(i=0;i<len;i++){
    xy[i]=x[i]+(y[i]*scale);
  }
  return xy;
}
double*
rk4_array_kdv (double* u,int len,double dt,double dx){
  int i;
  double *result = malloc(len*sizeof(double));
  double *k_1=u_discrete(u,len,dx);
  double *k_2=u_discrete(vec_add_kdv(u,k_1,len,(dt*0.5)),len,dx);
  double *k_3=u_discrete(vec_add_kdv(u,k_2,len,(dt*0.5)),len,dx);
  double *k_4=u_discrete(vec_add_kdv(u,k_3,len,(dt)),len,dx);
  for(i=0;i<len;i++){
    result[i]=(dt*(1.0/6))*(k_1[i]+2*k_2[i]+2*k_3[i]+k_4[i]);
  }
  free(k_1);free(k_2);free(k_3);free(k_4);
  return result;
}
int main(int argc,char** argv) {
  int range = (4 * M_PI),x_0=(-2.0 * M_PI);
  double dx=(M_PI/32),dt=pow(dx,3),t_max=50;
  int len = 128;
  char name[50];
  double *x=malloc(len*sizeof(double));
  double *u=malloc(len*sizeof(double));
  double *u_p=malloc(len*sizeof(double));
  double q=x_0;int q2;
  for (q2=0;q2<len;q2++){
    x[q2]=q;
    u[q2]=-12*pow((1/cosh(x[q2])),2);
    u_p[q2]=u[q2];
    q+=dx;
  }
  //for printing to a file
  double time=0;
  int j;int cnt=0;
  /*  while (time<t_max) {
    u=vec_add_kdv(u,rk4_array_kdv(u,len,dt,dx),len,1);
    time+=dt;
  } return 0;
  }*/
  /*open file here*/
  MAIN_LOOP:while(time<t_max){
    //#pragma omp parallel num_threads(2)
    //   {
    //#pragma omp single
    if(cnt % 5000 ==0){
      snprintf(name,50,"%06d.txt",cnt);
      //fprintf(stderr,name);
      FILE* file=fopen(name,"w");
      mem_check(file);
      //  {
      fprintf(file,"#data for time %.2f\n",time);
      for(j=0;j<len;j++){
        fprintf(file,"%10f\t%10f\n",j,x[j],(fabs(u[j])));
      }
      fclose(file);
    }
    //  }
    //#pragma omp single
    u=vec_add_kdv(u,rk4_array_kdv(u,len,dt,dx),len,1);
    //#pragma omp barrier
    //   }
    time+=dt;
    for(q2=0;q2<len;q2++){
      u_p[q2]=u[q2];
    }
    cnt++;
  }
  snprintf(name,50,"%06d.txt",cnt);
  FILE* file=fopen(name,"w");
  fprintf(file,"#data for time %.2f",time);
  for(j=0;j<len;j++){
    fprintf(file," %10f\t%10f\n",j,x[j],(fabs(u[j])));
  }
  fclose(file);
  system("mkdir -p kdv-data && rm -f kdv-data/*");
  system("mv *.txt kdv-data");
  return 0;
}
