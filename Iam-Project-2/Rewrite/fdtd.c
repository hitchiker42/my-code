#include "fdtd.h"
//these six functions re all the calculus
const double sigma;
const double episilon;
const double mu;
const double dt;
const double dx;
const double mu_dt=mu*dt;
const m256d mu_dt_simd = _mm256_set1_pd(mu_dt);
inline void updateHx(double* Hx,double* Hn,field E,point loc){
#ifndef __AVX__
  int i
    for(i=0;i<4;i++)
      return get_value(Hx,loc) +
        dt*mu*((get_value_xyz(E.y,loc.x,loc.y,loc.z+1)-
               get_value_xyz(E.y,loc.x,loc,y,loc.z)) +
              (get_value_xyz(E.z,loc.x,loc.y,loc.z)-
               get_vaule_xyz(E.z,loc.x,loc.y+1,loc.z)));
#endif
#ifdef __AVX__
      //or
      m256d E_y=_mm256_set_pd(get_value_xyz(E.y,loc.x+0,loc.y,loc.z),
                                get_value_xyz(E.y,loc.x+1,loc.y,loc.z),
                                get_value_xyz(E.y,loc.x+2,loc.y,loc.z),
                                get_value_xyz(E.y,loc.x+3,loc.y,loc.z));
      m256d E_y1=_mm256_set_pd(get_value_xyz(E.y,loc.x+0,loc.y,loc.z+1),
                                get_value_xyz(E.y,loc.x+1,loc.y,loc.z+1),
                                get_value_xyz(E.y,loc.x+2,loc.y,loc.z+1),
                                get_value_xyz(E.y,loc.x+3,loc.y,loc.z+1));
      m256d E_z=_mm256_set_pd(get_value_xyz(E.y,loc.x+0,loc.y,loc.z),
                                get_value_xyz(E.y,loc.x+1,loc.y,loc.z),
                                get_value_xyz(E.y,loc.x+2,loc.y,loc.z),
                                get_value_xyz(E.y,loc.x+3,loc.y,loc.z));
      m256d E_z1=_mm256_set_pd(get_value_xyz(E.y,loc.x+0,loc.y+1,loc.z),
                                get_value_xyz(E.y,loc.x+1,loc.y+1,loc.z),
                                get_value_xyz(E.y,loc.x+2,loc.y+1,loc.z),
                                get_value_xyz(E.y,loc.x+3,loc.y+1,loc.z));
      m256d H_x=_mm256_set_pd(get_value_xyz(Hx,loc.x+0,loc.y,loc.z),
                                get_value_xyz(Hx,loc.x+1,loc.y,loc.z),
                                get_value_xyz(Hx,loc.x+2,loc.y,loc.z),
                                get_value_xyz(Hx,loc.x+3,loc.y,loc.z));
      E_y=_mm256_sub_pd(E_y1,E_y);
      E_z=_mm256_sub_pd(E_z,E_z1);
      E_z=_mm256_add_pd(E_y,E_z);
      E_z=_mm256_mul_pd(E_z,mu_dt_simd);
      H_x=_mm256_add_pd(H_x,E_z);
      //either
      return H_x;
      //or
      _mm256_store_pd(get_ptr(H_n1,loc),H_x);

#endif
}
inline double updateHy(double *Hy,field E,point loc){
       return get_value(Hy,loc) +
        dt*mu*((get_value_xyz(E.z,loc.x+1,loc.y,loc.z)-
               get_value_xyz(E.z,loc.x,loc,y,loc.z)) +
              (get_value_xyz(E.x,loc.x,loc.y,loc.z)-
               get_vaule_xyz(E.x,loc.x,loc.y,loc.z+1)));
}
inline double updateHz(double* Hz,field E,point loc){
      return get_value(Hz,loc) +
        dt*mu*((get_value_xyz(E.x,loc.x,loc.y+1,loc.z)-
               get_value_xyz(E.x,loc.x,loc,y,loc.z)) +
              (get_value_xyz(E.y,loc.x,loc.y,loc.z)-
               get_value_xyz(E.y,loc.x+1,loc.y,loc.z)));
}
inline double updateEx(double* Ex,field H,point loc){
      return (1-(sigma*dt/episilon))*get_value(Ex,loc)+
        dt*episilon((get_value_xyz(H.z,loc.x,loc.y,loc.z)-
                     get_value_xyz(H.z,loc,x,loc.y-1,loc.z))+
                    (get_value_xyz(H.y,loc.x,loc.y,loc.z-1)-
                     get_value_xyz(H.y,loc.x,loc.y,loc.z)));
}
inline double updateEy(double* Ey,field H,point loc){
      return (1-(sigma*dt/episilon))*get_value(Ey,loc)+
        dt*episilon((get_value_xyz(H.x,loc.x,loc.y,loc.z)-
                     get_value_xyz(H.x,loc,x,loc.y,loc.z-1))+
                    (get_value_xyz(H.z,loc.x-1,loc.y,loc.z)-
                     get_value_xyz(H.z,loc.x,loc.y,loc.z)));
}
inline double updateEz(double* Ez,field H,point loc){
      return (1-(sigma*dt/episilon))*get_value(Ez,loc)+
        dt*episilon((get_value_xyz(H.y,loc.x,loc.y,loc.z)-
                     get_value_xyz(H.y,loc,x-1,loc.y,loc.z))+
                    (get_value_xyz(H.z,loc.x,loc.y-1,loc.z)-
                     get_value_xyz(H.z,loc.x,loc.y,loc.z)));
}
