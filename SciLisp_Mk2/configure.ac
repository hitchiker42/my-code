#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ([2.69])
LT_PREREQ([2.4.2])
AC_INIT([SciLisp], [0.01], [SciLisp@gmail.com], [],
                   [https://github.com/hitchiker42/My-Code/SciLisp_Mk2])
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([subdir-objects foreign -Wall -Wno-gnu])
AM_PROG_AR
AM_PROG_AS
PKG_CHECK_MODULES([CHECK], [check >= 0.9.6])
LT_INIT
# Checks for programs.
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CC_C99
AC_PROG_INSTALL
AC_PROG_LN_S

AC_CHECK_LIB([dl], [dlsym])
AC_CHECK_LIB([pthread], [pthread_create])
AC_CHECK_LIB([m], [cos])
AC_CHECK_LIB([gmp], [__gmpz_init])
AC_CHECK_LIB([mpfr], [mpfr_init])

AC_ARG_WITH([readline],
  [AS_HELP_STRING([--with-readline],
    [Use readline for command line editing (default=check)])],
  [],
  [with_readline=check])
AS_IF([test "x$with_readline" != xno],
  [AC_CHECK_LIB([readline], [readline],
     [AC_DEFINE([HAVE_READLINE], [1],
                [Define if readline is enabled])
                [LIBS="$LIBS -lreadline"]],
     [if test "x$with_readline" != xcheck; then
        AC_MSG_FAILURE(
          [--with-readline was given, but libreadline not found])
     fi])])

AC_CONFIG_FILES([Makefile
                 src/Makefile
                 tests/Makefile
                 doc/Makefile])
AC_OUTPUT
