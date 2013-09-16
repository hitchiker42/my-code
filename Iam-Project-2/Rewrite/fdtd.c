#include "fdtd_consts.h"
#include "fdtd.h"
//constants
inline void updateHx(double* Hx,double* Hn,field E,point loc){
#ifndef __AVX__
  int i;
    for(i=0;i<4;i++){
      *(get_ptr_xyz(Hn,loc.x+i,loc.y,loc.z))=
        get_value_xyz(Hx,loc.x+i,loc.y,loc.z) +
        dt*mu*((get_value_xyz(E.y,loc.x+i,loc.y,loc.z+1)-
                get_value_xyz(E.y,loc.x+i,loc.y,loc.z)) +
               (get_value_xyz(E.z,loc.x+i,loc.y,loc.z)-
                get_vaule_xyz(E.z,loc.x+i,loc.y+1,loc.z)));
    }
#endif
#ifdef __AVX__
  register m256d E_y=_mm256_load_pd(get_ptr_xyz(E.y,loc.x,loc.y,loc.z));
  /*                              get_value_xyz(E.y,loc.x+0,loc.y,loc.z),
                                get_value_xyz(E.y,loc.x+1,loc.y,loc.z),
                                get_value_xyz(E.y,loc.x+2,loc.y,loc.z),
                                get_value_xyz(E.y,loc.x+3,loc.y,loc.z));*/
  register m256d E_y1=_mm256_load_pd(get_ptr_xyz(E.y,loc.x,loc.y,loc.z+1));
                           /* get_value_xyz(E.y,loc.x+0,loc.y,loc.z+1),
                              get_value_xyz(E.y,loc.x+1,loc.y,loc.z+1),
                              get_value_xyz(E.y,loc.x+2,loc.y,loc.z+1),
                              get_value_xyz(E.y,loc.x+3,loc.y,loc.z+1));*/
  register m256d E_z=_mm256_load_pd(get_ptr_xyz(E.z,loc.x,loc.y,loc.z));
                              /*get_value_xyz(E.y,loc.x+0,loc.y,loc.z),
                                get_value_xyz(E.y,loc.x+1,loc.y,loc.z),
                                get_value_xyz(E.y,loc.x+2,loc.y,loc.z),
                                get_value_xyz(E.y,loc.x+3,loc.y,loc.z));*/
  register m256d E_z1=_mm256_load_pd(get_ptr_xyz(E.z,loc.x,loc.y,loc.z+1));
                               /*get_value_xyz(E.y,loc.x+0,loc.y+1,loc.z),
                                get_value_xyz(E.y,loc.x+1,loc.y+1,loc.z),
                                get_value_xyz(E.y,loc.x+2,loc.y+1,loc.z),
                                get_value_xyz(E.y,loc.x+3,loc.y+1,loc.z));*/
  register m256d H_x=_mm256_load_pd(get_ptr_xyz(Hx,loc.x,loc.y,loc.z));
                          /*get_value_xyz(Hx,loc.x+0,loc.y,loc.z),
                            get_value_xyz(Hx,loc.x+1,loc.y,loc.z),
                            get_value_xyz(Hx,loc.x+2,loc.y,loc.z),
                            get_value_xyz(Hx,loc.x+3,loc.y,loc.z));*/
  E_y=_mm256_sub_pd(E_y1,E_y);
  E_z=_mm256_sub_pd(E_z,E_z1);
  E_z=_mm256_add_pd(E_y,E_z);
  E_z=_mm256_mul_pd(E_z,mu_dt_simd);
  H_x=_mm256_add_pd(H_x,E_z);
  _mm256_store_pd(get_ptr(Hn,loc),H_x);
#endif
  return;
}
inline void updateHy(double *Hy,double* Hn,field E,point loc){
#ifndef __AVX__
  int i;
  for(i=0;i<4;i++){
    *(get_ptr_xyz(Hn,loc.x+i,loc.y,loc.z))=
      get_value_xyz(Hy,loc.x+i,loc.y,loc.z) +
      dt*mu*((get_value_xyz(E.z,loc.x+1+i,loc.y,loc.z)-
              get_value_xyz(E.z,loc.x+i,loc.y,loc.z)) +
             (get_value_xyz(E.x,loc.x+i,loc.y,loc.z)-
              get_value_xyz(E.x,loc.x+i,loc.y,loc.z+1)));
  }
#endif
#ifdef __AVX__
  register m256d E_z=_mm256_load_pd(get_ptr_xyz(E.z,loc.x,loc.y,loc.z));
  register m256d E_z1=_mm256_load_pd(get_ptr_xyz(E.z,loc.x+1,loc.y,loc.z));
  register m256d E_x=_mm256_load_pd(get_ptr_xyz(E.x,loc.x,loc.y,loc.z));
  register m256d E_x1=_mm256_load_pd(get_ptr_xyz(E.x,loc.x,loc.y,loc.z+1));
  register m256d H_y=_mm256_load_pd(get_ptr_xyz(Hy,loc.x,loc.y,loc.z));
  E_z=_mm256_sub_pd(E_z1,E_z);
  E_x=_mm256_sub_pd(E_x,E_x1);
  E_x=_mm256_add_pd(E_z,E_x);
  E_x=_mm256_mul_pd(E_x,mu_dt_simd);
  H_y=_mm256_add_pd(H_y,E_x);
  _mm256_store_pd(get_ptr(Hn,loc),H_y);
#endif
  return;
}
inline void updateHz(double* Hz,double* Hn,field E,point loc){
#ifndef __AVX__
  int i;
  for(i=0;i<4;i++){
    *(get_ptr_xyz(Hn,loc.x+i,loc.y,loc.z)) =
      get_value_xyz(Hz,loc.x+i,loc.y,loc.z) +
      dt*mu*((get_value_xyz(E.x,loc.x+i,loc.y+1,loc.z)-
              get_value_xyz(E.x,loc.x+i,loc.y,loc.z)) +
             (get_value_xyz(E.y,loc.x+i,loc.y,loc.z)-
              get_value_xyz(E.y,loc.x+i+1,loc.y,loc.z)));
  }
#endif
#ifdef __AVX__
  register m256d E_x=_mm256_load_pd(get_ptr_xyz(E.x,loc.x,loc.y,loc.z));
  register m256d E_x1=_mm256_load_pd(get_ptr_xyz(E.x,loc.x,loc.y+1,loc.z));
  register m256d E_y=_mm256_load_pd(get_ptr_xyz(E.y,loc.x,loc.y,loc.z));
  register m256d E_y1=_mm256_load_pd(get_ptr_xyz(E.y,loc.x+1,loc.y,loc.z));
  register m256d H_z=_mm256_load_pd(get_ptr_xyz(Hz,loc.x,loc.y,loc.z));
  E_x=_mm256_sub_pd(E_x1,E_x);
  E_y=_mm256_sub_pd(E_y,E_y1);
  E_y=_mm256_add_pd(E_x,E_y);
  E_y=_mm256_mul_pd(E_y,mu_dt_simd);
  H_z=_mm256_add_pd(H_z,E_y);
  _mm256_store_pd(get_ptr(Hn,loc),H_z);
#endif
  return;
}
inline void updateEx(double* Ex,double* En,field H,point loc){
#ifndef __AVX__
  for(i=0;i<4;i++){
    *(get_ptr_xyz(En,loc.x+i,loc.y,loc.z))=
      (1-(sigma*dt/episilon))*get_value_xyz(Ex,loc.x+i,loc.y,loc.z)+
        dt*episilon*((get_value_xyz(H.z,loc.x+i,loc.y,loc.z)-
                     get_value_xyz(H.z,loc.x+i,loc.y-1,loc.z))+
                    (get_value_xyz(H.y,loc.x+i,loc.y,loc.z-1)-
                     get_value_xyz(H.y,loc.x+i,loc.y,loc.z)));
  }
#endif
#ifdef __AVX__
  register m256d H_z=_mm256_load_pd(get_ptr_xyz(H.z,loc.x,loc.y,loc.z));
  register m256d H_z1=_mm256_load_pd(get_ptr_xyz(H.z,loc.x,loc.y-1,loc.z));
  register m256d H_y=_mm256_load_pd(get_ptr_xyz(H.y,loc.x,loc.y,loc.z));
  register m256d H_y1=_mm256_load_pd(get_ptr_xyz(H.y,loc.x,loc.y,loc.z-1));
  register m256d E_x=_mm256_load_pd(get_ptr_xyz(Ex,loc.x,loc.y,loc.z));
  H_z=_mm256_sub_pd(H_z,H_z1);
  H_y=_mm256_sub_pd(H_y1,H_y);
  H_y=_mm256_add_pd(H_z,H_y);
  H_y=_mm256_mul_pd(H_y,mu_dt_simd);
  E_x=_mm256_add_pd(H_y,E_x);
  _mm256_store_pd(get_ptr(En,loc),E_x);
#endif
  return;
}
 inline void updateEy(double* Ey,double* En,field H,point loc){
#ifndef __AVX__
   int i;
  for(i=0;i<4;i++){
    *(get_ptr_xyz(En,loc.x+i,loc.y,loc.z))=
      (1-(sigma*dt/episilon))*get_value_xyz(Ey,loc.x+i,loc.y,loc.z)+
      dt*episilon*((get_value_xyz(H.x,loc.x+i,loc.y,loc.z)-
                   get_value_xyz(H.x,loc.x+i,loc.y,loc.z-1))+
                  (get_value_xyz(H.z,loc.x+i-1,loc.y,loc.z)-
                   get_value_xyz(H.z,loc.x+i,loc.y,loc.z)));
  }
#endif
#ifdef __AVX__
  register m256d H_x=_mm256_load_pd(get_ptr_xyz(H.x,loc.x,loc.y,loc.z));
  register m256d H_x1=_mm256_load_pd(get_ptr_xyz(H.x,loc.x,loc.y,loc.z-1));
  register m256d H_z=_mm256_load_pd(get_ptr_xyz(H.z,loc.x,loc.y,loc.z));
  register m256d H_z1=_mm256_load_pd(get_ptr_xyz(H.z,loc.x-1,loc.y,loc.z));
  register m256d E_y=_mm256_load_pd(get_ptr_xyz(Ey,loc.x,loc.y,loc.z));
  H_x=_mm256_sub_pd(H_x,H_x1);
  H_z=_mm256_sub_pd(H_z1,H_z);
  H_z=_mm256_add_pd(H_x,H_z);
  H_z=_mm256_mul_pd(H_z,mu_dt_simd);
  E_y=_mm256_add_pd(H_z,E_y);
  _mm256_store_pd(get_ptr(En,loc),E_y);
#endif
  return;
}
inline void updateEz(double* Ez, double* En, field H, point loc){
#ifndef __AVX__
  int i;
  for(i=0;i<4;i++){
    *(get_ptr_xyz(En,loc.x+i,loc.y,loc.z))=
      (1-(sigma*dt/episilon))*get_value_xyz(Ez,loc.x+i,loc.y,loc.z)+
        dt*episilon*((get_value_xyz(H.y,loc.x+i,loc.y,loc.z)-
                     get_value_xyz(H.y,loc.x+i-1,loc.y,loc.z))+
                    (get_value_xyz(H.x,loc.x+i,loc.y-1,loc.z)-
                     get_value_xyz(H.x,loc.x+i,loc.y,loc.z)));
  }
#endif
#ifdef __AVX__
  register m256d H_y=_mm256_load_pd(get_ptr_xyz(H.x,loc.x,loc.y,loc.z));
  register m256d H_y1=_mm256_load_pd(get_ptr_xyz(H.x-1,loc.x,loc.y,loc.z));
  register m256d H_x=_mm256_load_pd(get_ptr_xyz(H.z,loc.x,loc.y,loc.z));
  register m256d H_x1=_mm256_load_pd(get_ptr_xyz(H.z,loc.x,loc.y-1,loc.z));
  register m256d E_z=_mm256_load_pd(get_ptr_xyz(Ez,loc.x,loc.y,loc.z));
  H_y=_mm256_sub_pd(H_y,H_y1);
  H_x=_mm256_sub_pd(H_x1,H_x);
  H_x=_mm256_add_pd(H_y,H_x);
  H_x=_mm256_mul_pd(H_x,mu_dt_simd);
  E_z=_mm256_add_pd(H_x,E_z);
  _mm256_store_pd(get_ptr(En,loc),E_z);
#endif
  return;
}
int main(int argc,char** argv){
  int i,j,k;
  double t=0,t_max=30;
  int wtemp = num_writes;
  int write_interval= t_max /num_writes;
  int write_time=write_interval;
  //create field is a macro to define and initalize a double array
  create_field(H_nx);create_field(H_ny);create_field(H_nz);
  create_field(H_n1x);create_field(H_n1y);create_field(H_n1z);
  create_field(E_nx);create_field(E_ny);create_field(E_nz);
  create_field(E_n1x);create_field(E_n1y);create_field(E_n1z);
  field H_n={H_nx,H_ny,H_nz};
  field H_n1={H_n1x,H_n1y,H_n1z};//H at time n and time n+1
  field E_n={E_nx,E_ny,E_nz};
  field E_n1={E_n1x,E_n1y,E_n1z};//E at time n and time n+1
  /*lets assume a central radiating source at(0,0,0) so points
   *(0/1/-1,0/1/-1,0/1/-1) are effected.
   *flow will be, update from source, i.e. unit cube around origin is updated 
   *based on source+current value, then iterate to find H @ time 
   *n+1(really n+0.5), then find E @ time n+1, output data in some way
   *check to see if time is up, if so return, else loop again.b/c we have 
   *two seperate fields for current time and next time paralization should
   *be pretty easy, in fact we could probably just use threads*/
  //lets say our initial conditon is E_z(i,j,k) for all -1=<i,j,k<=1
  //is E_z = 1000*sin(M_PI*pow(t,2))
  while(t<t_max){
    if(t==write_time){
      write_time+=write_interval;
      dump_data(t);
    }
  for(k=-1;k<=1;k++){
    for(j=-1;j<=1;j++){
      for(i=-1;i<=1;i++){
        if(k==0 && j==0 && i==0){
          continue;
        } else {
          *(get_ptr_xyz(E_n.z,i,j,k))=1000*sin(M_PI*pow(t,2));
        }
      }
    }
  }
for(k=z_min+1;k<z_max;k++){
  for(j=y_min+1;j<y_max;j++){
    for(k=x_min+1;k<x_max;k+=4){
      updateHx(H_n.x,H_n1.x,E_n,(point){i,j,k});
      updateHy(H_n.y,H_n1.y,E_n,(point){i,j,k});
      updateHz(H_n.z,H_n1.z,E_n,(point){i,j,k});
    }
  }
 }
 for(k=z_min+1;k<z_max;k++){
  for(j=y_min+1;j<y_max;j++){
    for(k=x_min+1;k<x_max;k+=4){
      updateEx(E_n.x,E_n1.x,H_n,(point){i,j,k});
      updateEy(E_n.y,E_n1.y,H_n,(point){i,j,k});
      updateEz(E_n.z,E_n1.z,H_n,(point){i,j,k});
    }
  }
 }
  }
  return 0;
}



