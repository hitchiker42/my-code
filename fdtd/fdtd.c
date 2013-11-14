#include "fdtd_consts.h"
#include "fdtd.h"
#include "fdtd_io.h"
pthread_barrier_t field_barrier;
pthread_cond_t field_cond=PTHREAD_COND_INITIALIZER;
pthread_mutex_t field_mutex=PTHREAD_MUTEX_INITIALIZER;//I suppose I don't use this
pthread_cond_t main_cond=PTHREAD_COND_INITIALIZER;
pthread_mutex_t main_mutex=PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t write_mutex=PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t write_cond=PTHREAD_COND_INITIALIZER;
int continue_threads=1;
double t;
double t_max;
int x_update_flag[1];
int y_update_flag[1];
int z_update_flag[1];
int continue_writing=1;
field H_n;
field H_n1;
field E_n;
field E_n1;
void* thread_main(void *field_info);
struct timespec wait_time={.tv_sec=0,.tv_nsec=1000};
//constants
static inline void updateHx(double* Hx,double* Hn,field E,point loc){
#ifndef __AVX__
  int i;
  for(i=0;i<4;i++){
    *(get_ptr_xyz(Hn,loc.x+i,loc.y,loc.z))=
      get_value_xyz(Hx,loc.x+i,loc.y,loc.z) +
      dt*mu*((get_value_xyz(E.y,loc.x+i,loc.y,loc.z+1)-
              get_value_xyz(E.y,loc.x+i,loc.y,loc.z)) +
             (get_value_xyz(E.z,loc.x+i,loc.y,loc.z)-
              get_value_xyz(E.z,loc.x+i,loc.y+1,loc.z)));
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
static inline void updateHy(double *Hy,double* Hn,field E,point loc){
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
  register m256d E_z1=_mm256_loadu_pd(get_ptr_xyz(E.z,loc.x+1,loc.y,loc.z));
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
static inline void updateHz(double* Hz,double* Hn,field E,point loc){
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
  register m256d E_y1=_mm256_loadu_pd(get_ptr_xyz(E.y,loc.x+1,loc.y,loc.z));
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
static inline void updateEx(double* Ex,double* En,field H,point loc){
#ifndef __AVX__
  int i;
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
static inline void updateEy(double* Ey,double* En,field H,point loc){
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
  register m256d H_z1=_mm256_loadu_pd(get_ptr_xyz(H.z,loc.x-1,loc.y,loc.z));
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
static inline void updateEz(double* Ez, double* En, field H, point loc){
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
  register m256d H_y1=_mm256_loadu_pd(get_ptr_xyz(H.x-1,loc.x,loc.y,loc.z));
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
struct thread_data {
  void (*update_H)(double*,double*,field,point);
  void (*update_E)(double*,double*,field,point);
  double* my_Hn;
  double* my_Hn_1;
  double* my_En;
  double* my_En_1;
  int* done_updating;
};
void* write_main(void* x __attribute__((unused))){
  while(1){
    pthread_mutex_lock(&write_mutex);
    pthread_cond_wait(&write_cond,&write_mutex);
    if(continue_writing){
      print_as_slices
        (t,(field){.x=H_write,.y=H_write+grid_size,.z=H_write+(2*grid_size)},
         (field){.x=E_write,.y=E_write+grid_size,.z=E_write+(2*grid_size)});
      fprintf(stderr,"finished writing\n"); 
      pthread_mutex_unlock(&write_mutex);
      continue;
    } else {
      return 0;
    }
  }
}
int main(int argc,char** argv){
  //I tried to allocate all this stuff statically but gcc wouldn't let me
  x_update_flag[0]=0;
  y_update_flag[0]=0;
  z_update_flag[0]=0;
  H_n_mem=xmalloc(grid_size3*sizeof(double));
  H_n1_mem=xmalloc(grid_size3*sizeof(double));
  H_write=xmalloc(grid_size3*sizeof(double));
  E_n_mem=xmalloc(grid_size3*sizeof(double));
  E_n1_mem=xmalloc(grid_size3*sizeof(double));
  E_write=xmalloc(grid_size3*sizeof(double));
  H_nx=H_n_mem;
  H_n1x=H_n1_mem;
  E_nx=E_n_mem;
  E_n1x=E_n1_mem;
  H_ny=H_n_mem+grid_size;
  H_n1y=H_n1_mem+grid_size;
  E_ny=E_n_mem+grid_size;
  E_n1y=E_n1_mem+grid_size;
  H_nz=H_n_mem+(grid_size*2);
  H_n1z=H_n1_mem+(grid_size*2);
  E_nz=E_n_mem+(grid_size*2);
  E_n1z=E_n1_mem+(grid_size*2);
struct thread_data x_field_data={.update_H=updateHx,.update_E=updateEx,
                                 .my_Hn=H_nx,.my_Hn_1=H_n1x,
                                 .my_En=E_nx,.my_En_1=E_n1x,
                                 .done_updating=x_update_flag};
struct thread_data y_field_data={.update_H=updateHy,.update_E=updateEy,
                                 .my_Hn=H_ny,.my_Hn_1=H_n1y,
                                 .my_En=E_ny,.my_En_1=E_n1y,
                                 .done_updating=y_update_flag};
struct thread_data z_field_data={.update_H=updateHz,.update_E=updateEz,
                                 .my_Hn=H_nz,.my_Hn_1=H_n1z,
                                 .my_En=E_nz,.my_En_1=E_n1z,
                                 .done_updating=z_update_flag};
  pthread_barrier_init(&field_barrier,NULL,3);
  pthread_t field_threads[3];
  pthread_t write_thread[1];
  t=0;
  t_max=1;
  double write_interval=t_max/num_writes;
  double write_time=write_interval;
  H_n=(field){H_nx,H_ny,H_nz};
  H_n1=(field){H_n1x,H_n1y,H_n1z};//H at time n and time n+1
  E_n=(field){E_nx,E_ny,E_nz};
  E_n1=(field){E_n1x,E_n1y,E_n1z};//E at time n and time n+1
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
  init_dir("fdtd_data");
  pthread_create(field_threads,NULL,thread_main,&x_field_data);
  pthread_create(field_threads+1,NULL,thread_main,&y_field_data);
  pthread_create(field_threads+2,NULL,thread_main,&z_field_data);
  pthread_create(write_thread,NULL,write_main,NULL);
  while(1){
    //        fprintf(stderr,"time=%f\n",t);
    pthread_mutex_lock(&main_mutex);
    while(!x_update_flag[0]||!y_update_flag[0]||!z_update_flag[0]){
      //wait for field threads to do their thing
      pthread_cond_wait(&main_cond,&main_mutex);
    }
    //inc time and see if we're done    
    t+=dt;
    if(t>=t_max){
      continue_threads=0;
      continue_writing=0;
      pthread_cond_broadcast(&field_cond);
      pthread_cond_broadcast(&write_cond);
      pthread_mutex_unlock(&main_mutex);
      goto END;
    } else {
      //see if we need to write data
      if(t>=write_time){
        //do write stuff
        fprintf(stderr,"time = %lf\n",t);
        fprintf(stderr,"printing \n");
        write_time+=write_interval;
        pthread_mutex_lock(&write_mutex);
        memcpy((void*)H_write,(void*)H_n_mem,grid_size*3*sizeof(double));
        memcpy((void*)E_write,(void*)E_n_mem,grid_size*3*sizeof(double));
        pthread_cond_broadcast(&write_cond);
        pthread_mutex_unlock(&write_mutex);        
      }
      //wakeup the field threads
      pthread_cond_broadcast(&field_cond);
      pthread_mutex_unlock(&main_mutex);
    }
  }
 END:
  fprintf(stderr,"waiting to join threads\n");
  pthread_join(field_threads[0],NULL);
  pthread_join(field_threads[1],NULL);
  pthread_join(field_threads[2],NULL);  
  fprintf(stderr,"data calculations finished, writing data\n");
  if(t >= t_max){
    print_as_slices(t,H_n,E_n);
  }
  fprintf(stderr,"waiting to join threads\n");
  pthread_join(write_thread[0],NULL);
  return 0;
}


void* thread_main(void *field_info){
  int i,j,k,l,m,n;  
  struct thread_data *data=(struct thread_data*)field_info;
  double* my_Hn=data->my_Hn;
  double* my_Hn_1=data->my_Hn_1;
  double* my_En=data->my_En;
  double* my_En_1=data->my_En_1;
  int* done_updating=data->done_updating;
  fprintf(stderr,"starting field thread number %lu\n",pthread_self());
  //update fields effected by the source function
  while(1){
    for(k=-1;k<=1;k++){
      for(j=-1;j<=1;j++){
        for(i=-1;i<=1;i++){
          if(k==0 && j==0 && i==0){
            continue;
          } else {
            *(get_ptr_xyz(my_Hn,i,j,k))+=10000*sin(M_PI*pow(t,2));          
            *(get_ptr_xyz(my_En,i,j,k))+=10000*cos(M_PI*pow(t,2));
          }
        }
      }
    }
    int I_should_update_H = pthread_barrier_wait(&field_barrier);
    for(k=z_min;k<z_max;k++){
      for(j=y_min;j<y_max;j++){
        //setting i to x_min, rather than x_min+1 for alignment reasons
        //hopefully this won't fail horribly
        for(i=x_min;i<x_max;i+=4){
          data->update_H(my_Hn,my_Hn_1,E_n,(point){i,j,k});
        }
      }
    }
    if(I_should_update_H){
      H_n=H_n1;//update H
    }
    int I_should_update_E=pthread_barrier_wait(&field_barrier);
    for(k=z_min+1;k<z_max;k++){
      for(j=y_min+1;j<y_max;j++){
        for(i=x_min;i<x_max;i+=4){
          data->update_E(my_En,my_En_1,H_n,(point){i,j,k});
        }
      }
    }
    if(I_should_update_E){
      E_n=E_n1;//update E
    }
    //attempt to set boundries to 0
    for(k=z_min;k<=z_max;k+=(2*z_max)){
      for(j=y_min;j<=y_max;j+=(2*y_max)){
        for(i=x_min;i<=x_max;i+=(1*x_max)){
          set_boundires_coord(my_Hn);
          set_boundires_coord(my_En);
        }
      }
    }
    pthread_mutex_lock(&main_mutex);
    done_updating[0]=1;
    pthread_cond_broadcast(&main_cond);
    pthread_cond_wait(&field_cond,&main_mutex);
    if(continue_threads){
      done_updating[0]=0;
      pthread_mutex_unlock(&main_mutex);
    } else {
      pthread_mutex_unlock(&main_mutex);
      HERE();
      goto THREAD_END;
    }    
  }
 THREAD_END:
  HERE();
  fprintf(stderr,"ending field thread number %lu\n",pthread_self());
  return 0;
}
#if 0
  init_dir("fdtd_data");
  while(t<t_max){
    //        fprintf(stderr,"time=%f\n",t);
    if(t==write_time){
      fprintf(stderr,"printing \n");
      write_time+=write_interval;
      print_as_slices(t,H_n,E_n);
    }
    for(k=-1;k<=1;k++){
      for(j=-1;j<=1;j++){
        for(i=-1;i<=1;i++){
          if(k==0 && j==0 && i==0){
            continue;
          } else {
            *(get_ptr_xyz(H_n.x,i,j,k))+=10000*sin(M_PI*pow(t,2));
            *(get_ptr_xyz(H_n.y,i,j,k))+=10000*sin(M_PI*pow(t,2));
            *(get_ptr_xyz(H_n.z,i,j,k))+=10000*sin(M_PI*pow(t,2));
            *(get_ptr_xyz(E_n.x,i,j,k))+=10000*cos(M_PI*pow(t,2));
            *(get_ptr_xyz(E_n.y,i,j,k))+=10000*cos(M_PI*pow(t,2));
            *(get_ptr_xyz(E_n.z,i,j,k))+=10000*cos(M_PI*pow(t,2));
          }
        }
      }
    }
    for(k=z_min;k<z_max;k++){
      for(j=y_min;j<y_max;j++){
        //setting i to x_min, rather than x_min+1 for alignment reasons
        //hopefully this won't fail horribly
        for(i=x_min;i<x_max;i+=4){
          updateHx(H_n.x,H_n1.x,E_n,(point){i,j,k});
          updateHy(H_n.y,H_n1.y,E_n,(point){i,j,k});
          updateHz(H_n.z,H_n1.z,E_n,(point){i,j,k});
          /*          updateHx(H_n.x,H_n1.x,E_n,(point){l,m,n});
          updateHy(H_n.y,H_n1.y,E_n,(point){l,m,n});
          updateHz(H_n.z,H_n1.z,E_n,(point){l,m,n});*/
        }
      }
    }
    H_n=H_n1;//update H
    for(k=z_min+1;k<z_max;k++){
      for(j=y_min+1;j<y_max;j++){
        for(i=x_min;i<x_max;i+=4){
          updateEx(E_n.x,E_n1.x,H_n,(point){i,j,k});
          updateEy(E_n.y,E_n1.y,H_n,(point){i,j,k});
          updateEz(E_n.z,E_n1.z,H_n,(point){i,j,k});
        }
      }
    }
    E_n=E_n1;//update E
    t+=dt;
    //attempt to set boundries to 0
    for(k=z_min;k<=z_max;k+=(2*z_max)){
      for(j=y_min;j<=y_max;j+=(2*y_max)){
        for(i=x_min;i<=x_max;i+=(1*x_max)){
          set_boundires(H,x);set_boundires(H,y);set_boundires(H,z);
          set_boundires(E,x);set_boundires(E,y);set_boundires(E,z);
        }
      }
    }
  }
#endif 
