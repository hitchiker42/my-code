/* Author: Tucker DiNapoli
 * This code is licensed under the GNU General Public License
 *
 * Much of the code is taken from the emwave2d program, which is distributed
 * under the GNU Public License(see COPYING file)
 * Many of the parallization ideas were adapted from the angora package, another
 * GPL licesened program.*/
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "fdtd.h"
#define NR_END 1 //Whats this
#define FREE_ARG char* //And whats this 
double *f1d(int nl, int nh);
double **f2d(int nrl, int nrh, int ncl, int nch);
void free_f1d(double *v, int nl, int nh);
void free_f2d(double **m, int nrl, int nrh, int ncl, int nch);
MPI_Comm MPI_SubComm,MPI_CartSubComm;
int rank,nodes;
//global variables
//TODO:REPLACE DOUBLE*S WITH DOUBLES
//GIVE VARIABLES MORE DESCRIPTIVE NAMES
int choice, width, thickness, wavetype, px, py;
double vscale, vmax, refindex;
long int isave;
char saveFilename[40];

//What is this stuff indices?
int i,j,n,n2, imed,NX,NY,NT,NMED,NPML,NCB;
int ia, ib, ja, jb, ic, jc;
int isrc,jsrc;
int tmpcolor;

//physical constants?
double rn,M, cabc, SIGMA_MAX, SPEED_OF_LIGHT;
double lambda, delt, dels, l, ra, rb, r,s,btemp, etemp, temp;
//Stuff te do with media
double *eps_r, *mu_r, *sigma, *cea, *ceb, *ch, *mr,*mi;
//Boundary Conditons?
double *pxa, *pya, *qxa, *qya, *rxa, *rya;
double *pxb, *pyb, *qxb, *qyb, *rxb, *ryb;

//This code as is allows multiple media,
//I might leave things this way, or just simplify things
//and use only one possible medium

double ** media;
//Grids Need to change to h instead of d
double **ex, **ey;
double **dx, **dy;
double **hz, **bz;

double *incident_hz,*incident_ex, *incident_ey; /* incident fields */

FILE  * outdata, * indata;
char inputFilename[40], refIndexFilename[40];
int main(int argc,char *argv[]){
  MPI_Init(&argc,&argv);
  atexit((void (*)())MPI_Finalize);
  //NOTE: NEED TO KNOW WHICH VARIABLES ARE GLOBAL AND WHICH ARE PROCESS LOCAL
  //initialize grid, and parallel stuff
  //total processes(size) and pid of each process
  int nodes,rank; //declared in header
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &nodes);
  //define subcommunicator
  //use the common value 0 instead of GridIndex to include all nodes in the same subcommunicator
  //Why not just use MPI_Comm_create
  MPI_Comm_split(MPI_COMM_WORLD,0,rank,&MPI_SubComm);
  //set geometry close to a square
  int nodes_y=nodes/2;
  int nodes_x=(nodes-nodes_y);
  //I suppose its better to use variables than contsants in function args
  int ndims = 2; //came from 3-D code
  int dims[2] = {nodes_x,nodes_y};
  int periods[2] = {0,0};	//non-periodic
  int reorder = 1;	//permit reorder
  MPI_Cart_create(MPI_SubComm, ndims, dims, periods, reorder, &MPI_CartSubComm);
  //these need to be local
  //each node needs to know its neighbors to transfer H info
  int rank_above,rank_behind,rank_left,rank_right;
  MPI_Cart_shift(MPI_CartSubComm, 0, next, &rank_left, &rank_right);
  MPI_Cart_shift(MPI_CartSubComm, 1, next, &rank_below, &rank_above);
  //Now get cooridnates
  int cart_coords[3];
  MPI_Cart_coords(MPI_CartSubComm,rank,2,cart_coords);
  rank_x = cart_coords[0];
  rank_y = cart_coords[1];
  //HERE:we need to initalize the actual grids and such
  //for (n=0;n<tmax;n+=dt){
  //updatePsi()
  //if n%something=0{
  //write some data to a file
  //free memory & such
  //Do any movement of external files and such
  //exit(and implicly call MPI_Finalize)
  //}}
  return 0;
}
void update(int choice){
  // Play(timestep)
  if(choice == 1){
    updatePsi();
  }

  // Restart(fairly obvious)
  if(choice == 2){
    //Must be a better way that this
    for(i=0;i<=NX;i++){
      for(j=0;j<=NY;j++){
        hz[i][j] = 0;
        ex[i][j] = 0;
        ey[i][j] = 0;
        bz[i][j] = 0;
        dx[i][j] = 0;
        dy[i][j] = 0;
      }}

    for(j=0;j<=NY;j++){
      incident_ex[j] = 0;
      incident_ey[j] = 0;
      incident_hz[j] = 0;
    }

    for(i=0;i<=NMED;i++){
      eps_r[i] = mr[i]*mr[i]-mi[i]*mi[i];
      sigma[i] = 4*PI*mi[i]*mr[i]/eps_r[i];
      mu_r[i] = 1.0;
    }

    for(i=0;i<=NMED;i++){
      temp = 0.5*delt*sigma[i];
      cea[i] = (1.0-temp)/(1.0+temp);
      ceb[i] = (delt/dels)/(eps_r[i]*(1.0+temp));
      ch[i] = delt/(mu_r[i]*dels);
    }


    choice = 0;
    n = 0;
  }

  // Save(save data, need to change)
  if(choice == 3){
    isave++;
    sprintf(saveFilename,"Save-Hz-%ld.dat",isave);
    // SAVING and PRINTING
    if((outdata = fopen(saveFilename,"w"))==NULL){
p      fprintf(stderr," Cannot Open File \n");
      exit(3);
    }
    for(i=ia;i<=ib;i++){
      for(j=ja;j<=jb;j++){
        fprintf(outdata,"%f ",hz[i][j]);
      }
      fprintf(outdata,"\n");
    }
  }
}
void updatePsi(void)
{

  /* ITERATIONS */
  for(n2=0;n2<10;n2++){
    n++;
    rn = n;

    /* UPDATE INCIDENT FIELD */
    /* plane wave */
    if(wavetype==1){
      incident_hz[0] = sin(TAU*rn*delt/vscale);
      temp = incident_hz[NY-1];
      for(j=1;j<=NY-1;j++){
        incident_hz[j]= incident_hz[j] + ch[0]*(incident_ex[j+1] - incident_ex[j]);
      } /* j*/
      /* absorbing boundary - one way */
      incident_hz[NY]=temp + cabc*(incident_hz[NY-1]-incident_hz[NY]);
    }
    /* UPDATE MAGNETIC FIELDS */
    for(i=0;i<=NX-1;i++){
      for(j=0;j<=NY-1;j++){
        //imed = 0;
        imed = media[i][j];
        if(imed != -1){
          btemp = bz[i][j];
          bz[i][j]= pxb[i]*rxb[i]*bz[i][j]
            + rxb[i]*ch[imed]*(ex[i][j+1] - ex[i][j]- ey[i+1][j] + ey[i][j]);
          hz[i][j]= ryb[j]*(pyb[j]*hz[i][j] + (bz[i][j] - btemp));
        }
        else{
          hz[i][j] = 0;
        }

      }} /* ij */

    if(wavetype == 2){
      //point source
      hz[py+ic-200][px+jc-200] = hz[py+ic-200][px+jc-200] + 50*sin(TAU*rn*delt/vscale);
    }


    /* UPDATE - CONNECTING BOUNDARY */
    if(wavetype == 1){
      /* plane x = npml +1 */

      i = NPML + NCB-1;
      for(j=NPML+NCB;j<=NY-NPML-NCB-1;j++){
        hz[i][j] = hz[i][j] + ch[0]*incident_ey[j];
      }

      /* plane x = NX - NPML +1  */
      i = NX-NPML-NCB;
      for(j=NPML+NCB;j<=NY-NPML-NCB-1;j++){
        hz[i][j] = hz[i][j] - ch[0]*incident_ey[j];
      }

      /* plane y = npml +1 */

      j = NPML + NCB-1;
      for(i=NPML+NCB;i<=NX-NPML-NCB-1;i++){
        hz[i][j] = hz[i][j] - ch[0]*incident_ex[j];
      }

      /* plane y = NY - npml - 1 */

      j = NY - NPML - NCB;
      for(i=NPML+NCB;i<=NX-NPML-NCB-1;i++){
        hz[i][j] = hz[i][j] + ch[0]*incident_ex[j];
      }


      /* UPDATE INCIDENT FIELD */
      for(j=1;j<=NY;j++){
        incident_ex[j]= cea[0]*incident_ex[j]+ ceb[0]*(incident_hz[j] - incident_hz[j-1]);
      } /* ijk */
    }
    /* ELECTRIC FIELDS */
    for(i=0;i<=NX;i++){
      for(j=0;j<=NY;j++){
        imed = media[i][j];
        if(imed == -1) imed = 0;
        if(j!=0 && j!=NY){
          etemp = dx[i][j];
          dx[i][j]= pya[j]*rya[j]*cea[imed]*dx[i][j]
            + rya[j]*ceb[imed]*(hz[i][j] - hz[i][j-1]);
          ex[i][j]= ex[i][j] + (qxb[i]*dx[i][j] - pxb[i]*etemp);
        }
        else {
          /* PEC BOUNDDARY */
          ex[i][j] = 0.0;
        }
        if(i!=0 && i!=NX){
          etemp = dy[i][j];
          dy[i][j]= pxa[i]*rxa[i]*cea[imed]*dy[i][j]
            + rxa[i]*ceb[imed]*(hz[i-1][j]- hz[i][j]);
          ey[i][j]= ey[i][j] + (qyb[j]*dy[i][j]- pyb[j]*etemp);
        }
        else {
          /* PEC BOUNDDARY */
          ey[i][j] = 0.0;
        }
      }} /* ij */

    if(wavetype == 1){
      /* UPDATE - CONNECTING BOUNDARY */

      /* plane x = npml + NCB */

      i = NPML + NCB;
      for(j=NPML+NCB;j<=NY-NPML-NCB-1;j++){
        ey[i][j] = ey[i][j] + ceb[0]*incident_hz[j];
      }

      /* plane x = NX - NPML + NCB  */
      i = NX-NPML-NCB;
      for(j=NPML+NCB;j<=NY-NPML-NCB-1;j++){
        ey[i][j] = ey[i][j] - ceb[0]*incident_hz[j];
      }

      /* plane y = npml +1 */

      j = NPML + NCB;
      for(i=NPML+NCB;i<NX-NPML-NCB-1;i++){
        ex[i][j] = ex[i][j] - ceb[0]*incident_hz[j-1];
      }

      /* plane y = NY - npml - 1 */

      j = NY - NPML - NCB;
      for(i=NPML+NCB;i<NX-NPML-NCB-1;i++){
        ex[i][j] = ex[i][j] + ceb[0]*incident_hz[j];
      }
    }
  }/* n iterations */

}

void initialize(void)
{
  FILE *infile;
  float temp1, temp2;
  char stemp[80];

  /* INPUT PARAMETERS */
  dels = 0.03;//grid spacing(ie how fine is the grid)
  delt = dels/2.0;//Time step, based on grid spacing
  cabc = ((delt-dels)/(delt+dels));
  NMED = 10;  /* number of media excluding the host medium */
  NPML = 11;//Number of perfectly matched layers?
  NCB = 2;
  NX = 404 + 2*NPML;
  NY = 404 + 2*NPML;
  M = 3.0;
  SIGMA_MAX = 50.0;

  ia = NPML+NCB+1;
  ib = NX - NPML - NCB;
  ja = NPML+NCB+1;
  jb = NY - NPML - NCB;

  ic = NX/2;
  jc = NY/2;

  /* ALLOCATE MEMORY and INITIALIZE TO ZERO */

  /* MEDIA */
  media = f2d(0,NX,0,NY);

  cea = f1d(0,NMED);  ceb = f1d(0,NMED);
  ch = f1d(0,NMED); sigma = f1d(0,NMED);
  epsr = f1d(0,NMED); mur = f1d(0,NMED);
  mr = f1d(0,NMED); mi = f1d(0,NMED);

  /* UPML */
  pxa = f1d(0,NX); pya = f1d(0,NY);
  qxa = f1d(0,NX); qya = f1d(0,NY);
  rxa = f1d(0,NX); rya = f1d(0,NY);
  pxb = f1d(0,NX); pyb = f1d(0,NY);
  qxb = f1d(0,NX); qyb = f1d(0,NY);
  rxb = f1d(0,NX); ryb = f1d(0,NY);

  /* FIELDS */
  ex = f2d(0,NX,0,NY); ey = f2d(0,NX,0,NY);
  hz = f2d(0,NX,0,NY); dx = f2d(0,NX,0,NY);
  dy = f2d(0,NX,0,NY); bz = f2d(0,NX,0,NY);

  eix = f1d(0,NY); hiz = f1d(0,NY); eiy = f1d(0,NY);

  /* REF INDEX */
  mr[0] = 1.0; mi[0] = 0.0;
  // Read from File refindex.dat
  infile = fopen(refIndexFilename,"r");
  fscanf(infile,"%s  %f  %f ", stemp, &temp1, &temp2);
  mr[1] = temp1; mi[1] = temp2;
  fscanf(infile,"%s  %f  %f ", stemp, &temp1, &temp2);
  mr[2] = temp1; mi[2] = temp2;
  fscanf(infile,"%s  %f  %f ", stemp, &temp1, &temp2);
  mr[3] = temp1; mi[3] = temp2;
  fclose(infile);

  for(i=0;i<=NMED;i++){
    epsr[i] = mr[i]*mr[i]-mi[i]*mi[i];
    sigma[i] = 4*PI*mi[i]*mr[i]/epsr[i];
    mur[i] = 1.0;
  }

  for(i=0;i<=NMED;i++){
    temp = 0.5*delt*sigma[i];
    cea[i] = (1.0-temp)/(1.0+temp);
    ceb[i] = (delt/dels)/(epsr[i]*(1.0+temp));
    ch[i] = delt/(mur[i]*dels);
  }

  /* UPML MEDIA */

  l = NPML;

  /* INTERIOR - I initialize all to 1.0 */
  for(i=0;i<=NX;i++){
    pxa[i] = qxa[i] = rxa[i] = 1.0E0;
    pxb[i] = qxb[i] = rxb[i] = 1.0E0;
  }

  /* PML - change only for the PML region */
  for(i=1;i<=NPML;i++){
    /* half */
    rb = (l-(float)i)/l;
    ra = rb + 1.0/l;
    s = SIGMA_MAX*l*(pow(ra,M+1.0)-pow(rb,M+1.0))/(M+1.0);
    temp = 0.5*delt*s;
    pxb[i-1] = pxb[NX-i] = 1.0 - temp;
    qxb[i-1] = qxb[NX-i] = 1.0 + temp;
    rxb[i-1] = rxb[NX-i] = 1.0/qxb[i-1];

    /* interger */
    if(i!=NPML){
      rb = (l - (float)i-0.5)/l;
      ra = rb + 1.0/l;
      s = SIGMA_MAX*NPML*(pow(ra,M+1.0)-pow(rb,M+1.0))/(M+1.0);
    }
    else{
      ra = 0.5/l;
      s = SIGMA_MAX*l*(pow(ra,M+1.0))/(M+1.0);
    }

    temp = 0.5*delt*s;
    pxa[i] = pxa[NX-i] = 1.0 - temp;
    qxa[i] = qxa[NX-i] = 1.0 + temp;
    rxa[i] = rxa[NX-i] = 1.0/qxa[i];

  }

  /* interior */
  for(j=0;j<=NY;j++){
    pya[j] = qya[j] = rya[j] = 1.0E0;
    pyb[j] = qyb[j] = ryb[j] = 1.0E0;
  }

  for(j=1;j<=NPML;j++){
    /* half */
    l = NPML;
    rb = (l - (float)j)/l;
    ra = rb+ 1.0/l;
    s = SIGMA_MAX*l*(pow(ra,M+1.0)-pow(rb,M+1.0))/(M+1.0);
    temp = 0.5*delt*s;
    pyb[NY-j] = pyb[j-1] = 1.0 - temp;
    qyb[NY-j] = qyb[j-1] = 1.0 + temp;
    ryb[NY-j] = ryb[j-1] = 1.0/qyb[j-1];

    /* interger */
    if(j!=NPML){
      rb = (l - (float)j-0.5)/l;
      ra = rb + 1.0/l;
      s = SIGMA_MAX*l*(pow(ra,M+1.0)-pow(rb,M+1.0))/(M+1.0);
    }
    else{
      ra = 0.5/l;
      s = SIGMA_MAX*l*(pow(ra,M+1.0))/(M+1.0);
    }
    temp = 0.5*delt*s;
    pya[NY-j] = pya[j] = 1.0 - temp;
    qya[NY-j] = qya[j] = 1.0 + temp;
    rya[NY-j] = rya[j] = 1.0/qya[j];

  }

  /* MEDIA INDEX or OPEN MEDIA FILE */


  for(i=0;i<=NX;i++){
    for(j=0;j<=NY;j++){
      media[i][j]=0;
    }}

  n2 =0;
  for(i=ia;i<ia+400;i++){
    for(j=ja;j<ja+400;j++){
      media[i][j]= bmpdata[n2];
      n2++;
    }}

  n = 0;
}
void exchangeH(void){
  //Example, Obviously SendBuf&RecvBuf need to be Process local
  //Also we need to test for the edge values and deal with them
  //Also need to do a bit more research to decide where to swap stuff
  int Hyxtag
  SendBuf_Hy_x = hy(someindex);
  MPI_Sendrecv(SendBuf_Hy_x->data,SendBuf_Hy_x->size,MPI_DOUBLE,rank_above,Hyxtag,
               RecvBuf_Hy_x->data,RecvBuf_Hy_x->size,MPI_DOUBLE,rank_below,Hyxtag,MPI_CartSubComm,MPI_STATUS_IGNORE)
  int Hxytag
  SendBuf_Hx_y = hy(someindex);
  MPI_Sendrecv(SendBuf_Hx_y->data,SendBuf_Hx_y->size,MPI_DOUBLE,rank_above,Hyxtag,
               RecvBuf_Hx_y->data,RecvBuf_Hx_y->size,MPI_DOUBLE,rank_below,Hyxtag,MPI_CartSubComm,MPI_STATUS_IGNORE);
}

double *f1d(int nl, int nh)
/* allocate a double vector with subscript range v[nl..nh] */
{
  int i;
  double *v;

  v=(double *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(double*)));
  if (!v) nrerror("allocation failure in f1d()");

  /* initialize to zero */
  for(i=nl;i<=nh;i++) v[i] = 0.0;

  /* return the vector */
  return v-nl+NR_END;
}


double **f2d(int nrl, int nrh, int ncl, int nch)
/* allocate a double matrix with subscript range m[nrl..nrh][ncl..nch] */
{
  int i, j, nrow=nrh-nrl+1,ncol=nch-ncl+1;
  double **m;

  /* allocate pointers to rows */
  m=(double **) malloc((size_t)((nrow+NR_END)*sizeof(double**)));
  if (!m) nrerror("allocation failure 1 in f2d()");
  m += NR_END;
  m -= nrl;

  /* allocate rows and set pointers to them */
  m[nrl]=(double *) malloc((size_t)((nrow*ncol+NR_END)*sizeof(double*)));
  if (!m[nrl]) nrerror("allocation failure 2 in f2d()");
  m[nrl] += NR_END;
  m[nrl] -= ncl;

  for(i=nrl+1;i<=nrh;i++) m[i]=m[i-1]+ncol;

  /* initialize to zero */
  for(i=nrl;i<=nrh;i++){
    for(j=ncl;j<=nch;j++){
      m[i][j] = 0.0;
    }
  }

  /* return pointer to array of pointers to rows */
  return m;
}

void free_f2d(double **m, int nrl, int nrh, int ncl, int nch)
/* free a double matrix allocated by matrix() */
{
  free((FREE_ARG) (m[nrl]+ncl-NR_END));
  free((FREE_ARG) (m+nrl-NR_END));
}

void free_f1d(double *v, int nl, int nh)
/* free a double vector allocated with vector() */
{
  free((FREE_ARG) (v+nl-NR_END));
}

void nrerror(char error_text[])
/* Numerical Recipes standard error handler */
{
  fprintf(stderr,"FDTD run-time error...\n");
  fprintf(stderr,"%s\n",error_text);
  fprintf(stderr,"...now exiting to system...\n");
  exit(1);
}

//Parallization
/*NFFFT_td.UpdateFarField(n);
  NFFFT_pd.UpdateFarField(n);
  OpticalImages.UpdateFarField(n);
  
  updateE(n);		//Update E-field in the main grid
  Absorb.UpdateE(); //Update the E-field at the boundaries
  TFSF.CorrectE(n);	//Apply TF/SF corrections to the E-field
  
  PointSources.ApplySources(n);
  
  updateH(n);		//Update H-field in the main grid
  Absorb.UpdateH(); //Update the H-field at the boundaries
  TFSF.CorrectH(n);	//Apply TF/SF corrections to the H-field
  
  exchangeH();	//Exchange necessary H-field information before next E-field update*/
/*Basically what is done in angora is to create a number of mpi processes equal to the
  number of nodes then use mpi cart create to create a topology for said processes. Then
  weird boundry condition stuff, as usual (perfecly matched layer stuff). The only communtication
  that is done is exchanging componants of the H field with negboring nodes. The updates to H are
  done using blocknig MPI_Sendrecv operations, which prevents deadlocks, and causes a barrier.
  Thus the above code updates the entire grid one timestep, and the main program is mostly just
  for n=0;n<n_man;n+=dt do above done, with relevant output recorded when needed*/
/* Parallel initaliztion
   MPI_Comm_rank(MPI_COMM_WORLD, &rank_global);//get total rank
   MPI_Comm_size(MPI_COMM_WORLD, &nodes_global);//get total size
   MPI_Comm_split(MPI_COMM_WORLD,0,rank,&MPI_SubComm);//split into rank processes(
   //or something close enough to that
   MPI_Comm_rank(MPI_SubComm, &rank);//get new rank?
   MPI_Comm_size(MPI_SubComm, &nodes);//gen new size?
   (*still need to initialize send buffers*)
   //now geometry
   //initialize geometry
    MPI_Cart_create(MPI_SubComm, ndims, dims, periods, reorder, &MPI_CartSubComm);
    //Store the ranks of the adjacent nodes (store MPI_PROC_NULL if the node is at the boundary of the global grid)
    MPI_Cart_shift(MPI_CartSubComm, 0, next, &rank_behind, &rank_front);
    MPI_Cart_shift(MPI_CartSubComm, 1, next, &rank_left, &rank_right);
    MPI_Cart_shift(MPI_CartSubComm, 2, next, &rank_below, &rank_above);
    int cart_coords[3];
    //determine ranks of x y and z
    MPI_Cart_coords(MPI_CartSubComm,rank,3,cart_coords);
    rank_x = cart_coords[0];
    rank_y = cart_coords[1];
    rank_z = cart_coords[2];
    //Now find out what part of the grid represents boundry conditions
    //and do some maniplations to account for that
    //send buffers, but we only need like 1/3 of these
    //Also regular grids should be initialized here
    //initialize MPI send and receive buffer arrays:
    //x direction (send-receive Hy and Hz)
    SendBuf_Hy_x.resize(Range(jleft,jright+1),Range(klower,kupper));
    SendBuf_Hz_x.resize(Range(jleft,jright),Range(klower,kupper+1));
    RecvBuf_Hy_x.resize(Range(jleft,jright+1),Range(klower,kupper));
    RecvBuf_Hz_x.resize(Range(jleft,jright),Range(klower,kupper+1));
    //y direction (send-receive Hx and Hz)
    SendBuf_Hx_y.resize(Range(iback,ifront+1),Range(klower,kupper));
    SendBuf_Hz_y.resize(Range(iback,ifront),Range(klower,kupper+1));
    RecvBuf_Hx_y.resize(Range(iback,ifront+1),Range(klower,kupper));
    RecvBuf_Hz_y.resize(Range(iback,ifront),Range(klower,kupper+1));
    //z direction (send-receive Hx and Hy)
    SendBuf_Hx_z.resize(Range(iback,ifront+1),Range(jleft,jright));
    SendBuf_Hy_z.resize(Range(iback,ifront),Range(jleft,jright+1));
    RecvBuf_Hx_z.resize(Range(iback,ifront+1),Range(jleft,jright));
    RecvBuf_Hy_z.resize(Range(iback,ifront),Range(jleft,jright+1));
    (*Still need to Initalize Geometry*)
 */
