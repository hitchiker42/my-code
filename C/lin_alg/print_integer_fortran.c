/* print_integer_fortran.F -- translated by f2c (version 20100827).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;

/* Subroutine */ int print_integer_fortran__(integer *i__)
{
    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 6, 0, 0, 0 };


    *i__ = *i__;
    s_wsle(&io___1);
    do_lio(&c__9, &c__1, "i=", (ftnlen)2);
    do_lio(&c__3, &c__1, (char *)&(*i__), (ftnlen)sizeof(integer));
    e_wsle();
    return 0;
} /* print_integer_fortran__ */

