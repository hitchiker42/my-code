/******************************************************************************
 * Kdv partial differential equation solver                                   *
 * Author: Tucker DiNapoli                                                    *
 * Date Created: 29/3/12                                                      *
 * Last Modified: 29/3/12                                                     *
 * A program for soving the Korteweg-de-Vires partial differential equation   *
 * A one dimensional equation used (among other things) to model shallow water*
 * waves, where u is the hight of the water above the sea floor               *
 * The equation has the form:                                                 *
 * \partial_tu+\partial_t^3u-6u\partial_xu=0                                  *
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
