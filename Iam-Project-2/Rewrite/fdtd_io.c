#include "fdtd.h"
#include "fdtd_io.h"
#include "fdtd_consts.h"
#include <sys/stat.h>
#include <dirent.h>
#define _GNU_SOURCE
#define unless(bool_expr) if(!bool_expr)
static int yes(const struct dirent *unused){return 1;}
int cnt = 0;//incrementing counter to generate new filenames
int init_dir(const char* dir_name){
  if (!mkdir(dir_name,0755)){
    return chdir(dir_name);
//return
  } else {
    printf("Default data location %s exists, overwrite?\ny/n:",dir_name);
    int tol = 3;//tolerance for nonsense input
    while(1){
      if(tol <=0){puts("Assuming no.\n");}
      if ('y' == getc(stdin)){
        struct stat is_dir;
        stat(dir_name,&is_dir);    
        if(S_ISREG(is_dir.st_mode)){
          unlink(dir_name);
          mkdir(dir_name,0755);
          return chdir(dir_name);          
//return
        } else if (S_ISDIR(is_dir.st_mode)){
          printf("%s is a directory, delete recursively?\ny/n:",dir_name);
          char* command;
          asprintf(&command,"rm -rf %s",dir_name);
          system(command);
          return chdir(dir_name);
          tol=3;
          while(1){
            if ('y' == getc(stdin)){break;}
            else if ('n' == getc(stdin)||!tol){goto NEW_DIR;}
            else(tol--);
          }
          struct dirent **eps;
          int n;
          n = scandir(dir_name,&eps,yes,alphasort);
          if (n >= 0){
            int i;
            for (i=0;i<n;i++){
              if (eps[i]->d_type == DT_REG){
                if(unlink(eps[i]->d_name)){
                  my_error("Error: failed to delete file %s"
                           " in dir %s, exiting\n",eps[i]->d_name,dir_name);
                }
              } else {
                my_error("Error: file %s is not a regular file, exiting\n",
                         eps[i]->d_name);
              }
            }
            free(eps);
            return chdir(dir_name);
//return
          } else {
            my_error("Error: error reading directory %s\n",dir_name);
          }
        } else {
          printf("%s is not a regular file or directory, refusing to delete\n",
                 dir_name);
          goto NEW_DIR;
        }
      } else if ('n' == getc(stdin) || tol <= 0){
        /*label used to prevent excessive looping in containing while
         *loop, if it bothers you, replace all calls of goto with tol=0
         *then break to outermost while loop*/
      NEW_DIR:
        puts("Creating new directory");
        char* new_dir_name;
        asprintf(&new_dir_name,"%s_%d",dir_name,++cnt);
        init_dir(new_dir_name);
        int retval = chdir(new_dir_name);
        free(new_dir_name);
        return retval;;
//return
      } else{tol--;}
    }
    //shouldn't get here
    my_error("Error control flow shouldn't reach here\n");
  }
}
//Bit of a workout for the preprocessor here
#define print_header(file)                                              \
fprintf(file,"%.16f %.16f  %.16f %.16f %.16f %d %d %d\n",                 \
        dx, dt, mu,episilon, sigma, x_max, y_max, z_max)
#define print_data(array,file)                  \
  fwrite(array,sizeof(double),grid_size,file)
#define mkfile(field)                                                   \
  char* field##_name;                                                   \
  asprintf(&field##_name,"fdtd_raw_%s_t%d.dat",#field,time);            \
  FILE* field##_data=fopen(field##_name,"w");                            \
  free(field##_name)
#define print_stuff(he,HE,xyz)                    \
  print_header(he##xyz##_data);                   \
  print_data(HE##_n.xyz,he##xyz##_data)        
void dump_data(int time,field H_n,field E_n){
  mkfile(hx); mkfile(hy); mkfile(hz); mkfile(ex); mkfile(ey); mkfile(ez);
  print_stuff(h,H,x); print_stuff(h,H,y); print_stuff(h,H,z);
  print_stuff(e,E,x); print_stuff(e,E,y); print_stuff(e,E,z);
  return;
}

void print_as_slices(int time,double* data,char* filename){
  /* z=0 (y=0 x1..xn,...y=n x1..xn), z=n(y=0 x1..xn,y=n x1..xn)
     gnuplot wants
     #cols y0 ... yn
     x0    z0,0 .. z0,n 
     ..
     xn    zn,0 .. zn,n */
  //meh, do this. print z = number, print grid of x,z points, blank line
  FILE* file=fopen(filename,"w");
  int i,j,k;
  for(k=z_min;k<z_max;k++){    
    fprintf(file,"z=%d",k);
    for(j=y_min;j<y_max;j++){
      fprintf(file,"\n");
      for(i=x_min;i<x_max;i++){
        fprintf(file,"%10.10f ",get_value_xyz(data,i,j,k));
      }
    }
    fprintf(file,"\n");
  }
}
