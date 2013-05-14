#include "fdtd.h";
//initalize grids etc.
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
               RecvBuf_Hx_y->data,RecvBuf_Hx_y->size,MPI_DOUBLE,rank_below,Hyxtag,MPI_CartSubComm,MPI_STATUS_IGNORE)

              
