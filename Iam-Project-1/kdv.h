/******************************************************************************
 * Kdv partial differential equation solver                                   *
 * Author: Tucker DiNapoli                                                    *
 * Date Created: 29/3/12                                                      *
 * Last Modified: 29/3/12                                                     *
 * A program for soving the Korteweg-de-Vires partial differential equation   *
 * A one dimensional equation used (among other things) to model shallow water*
 * waves, where u is the hight of the water above the sea floor               *
 * The equation has the form:                                                 *
 * \partial_tu+\partial_x^3u-6u\partial_xu=0                                  *
 * The solver uses finite-differences in space to discretize the problem      *
 * Periodic boundary conditions are used such that the fields satisify:       *
 * u(x+Ln) = u(x,y) for all integers n; this implies the grid has a domain of *
 * [0:L] with spacial steps \Delta x=L/mx, where mx is the number of points   *
 * the periodic boundary conditions imply that for at a boundary n            *
 * where normally n-2,n-1,n,n+1,n+2 be used to calculate n we use             *
 * n-2,n-1,n,n-1,n-2....I think....                                           *
 * the code will use either a hand written rk4 time step or one of several    *
 * adaptive algorithms from the gnu scientific library(i.e, QGAS,QAG..etc)    *
 * Initial conditions are taken from Zabusky & Kruskal '65 and will be        *
 * u_0 = cos(\pix) & u = cos(\pix-ut)                                         *
 * this results in a perodic solution with a recurrence time of Tr=30.4/pi    *
 * thus the default time to run is Tr.                                        *
 *****************************************************************************/
#ifndef KDV_H
#define KDV_H
#include "Misc.h"
void
 help();
double*
seq(double init,double final,double step);
double 
inital_u(double y);
void
update(double* u,double* x,double t_n,double h_x,double h_t,int len);
#endif
/*x values are constant, so we need to find u values at each time t. We find u
 *inital via the inital conditions(Duh) and then find u at time t+1 by 2 steps
 *first we find numerical values for the x deritives of u via finite differences
 *then we step u in time via rk4 or some other method
 *we can use this data in several ways, if we're really clever we can use
 *our gnu plot interface to animate the equation in real time, which probably
 *won't happen.
 *otherwise we can write data to a file to be plotted later.
 *to do this make 2 u arrays, one to use for reading to a file and one to 
 *do the time update on.
 *at time t_n the u arrays are identical, the writing thread starts copying the
 *data to a file while all other threads update the other copy of u to u at time
 *t_n+1. when both threads are done their tasks the updated data gets passed to
 *the writing thread and the cycle repeats*/
