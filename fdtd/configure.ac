#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT([Finite_Difference_Time_Domain], [0.01])
AM_INIT_AUTOMAKE([-Wall foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# Checks for programs.
#AC_PROG_CC [gcc]

AC_PROG_CC_C99
if test -z "$CC" || test "$CC" = "no"; then
  AC_MSG_ERROR([No suitable C99 compiler found])
fi
AM_PROG_CC_C_O

#Need GCC extensions, so force std=gcc99

# add -Wall if possible
#AX_CHECK_COMPILER_FLAGS([-Wall], [CFLAGS="$CFLAGS -Wall -g"])
#CFLAGS="$CFLAGS -std=gnu99"
# find flags for using OpenMP
AC_OPENMP
CFLAGS="$CFLAGS $OPENMP_CFLAGS"

# Check for debugging
#AC_ARG_ENABLE(debug,
#  AS_HELP_STRING([--disable-debug], [Turn off debugging]),
#  [case "${enableval}"
#     yes) debug=true ;;
#     no)  debug=false ;;
#     *) AC_MSG_ERROR(bad value ${enableval} for --enable-debug) ;;
#   esac],
#  [debug=true])

#AS_IF([test "$debug" = "false"], [CPPFLAGS="${CPPFLAGS} -DNDEBUG"])

#latex support
AC_DEFUN([AC_PROG_PDFLATEX],[
AC_CHECK_PROGS(pdflatex,[pdflatex],no)
export pdflatex;
if test $pdflatex = "no" ;
then
	AC_MSG_ERROR([Unable to find a PDFLaTeX application]);
fi
AC_SUBST(pdflatex)
])
AC_PROG_PDFLATEX

# for library support
AC_PROG_RANLIB
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])

AX_EXT
AC_CHECK_LIB([m],[cos])
#AC_CHECK_LIB([gslcblas],[cblas_dgemm])
#AC_CHECK_LIB([gsl],[gsl_blas_dgemm])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT

echo ""
echo "                       Configure Summary"
echo "                       ================="
echo ""
echo "Compiling Options:"
echo "------------------"
echo "                     C Compiler: $CC"
echo "                       CPPFLAGS: $CPPFLAGS"
echo "                         CFLAGS: $CFLAGS"
echo "                        LDFLAGS: $LDFLAGS"
echo "                           LIBS: $LIBS"
