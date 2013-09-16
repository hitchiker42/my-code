#ifndef __FDTD_IO_H__
#define __FDTD_IO_H__
void print_as_slices(int time,double* data,char* filename);
int init_dir(const char* dir_name);
void dump_data(int time,field H_n,field E_n);
#endif
