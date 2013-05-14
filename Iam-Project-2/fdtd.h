//includes
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>
//defines
#define  PI (4.0*atan(1.0))
#define  TAU (2.0*PI)
//function templates
void updatePsi(void);
void initialize(void);
void update(int choice);
void exchangeH(void);
void nrerror(char error_text[]);
//define global vars (really could be declared and defined in main, but eh)
MPI_Comm MPI_SubComm,MPI_CartSubComm;
extern int nodes,rank;
extern int nodes_x,nodes_y;
typedef struct {
  int size;
  double* data;
  } MpiBuf;
extern MpiBuf *SendBuf_Hy_x,*SendBuf_Hx_y,*RecvBuf_Hy_x,*RecvBuf_Hx_y;
