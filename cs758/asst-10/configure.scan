#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([assignment-10], [0.1])
AC_CONFIG_AUX_DIR([build_aux])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE([subdir-objects foreign -Wall -Wno-gnu])
AM_PROG_AR
AM_PROG_AS

# Checks for programs.
AC_PROG_CC
AC_PROG_CC_C99

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h limits.h stdlib.h string.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([gettimeofday pathconf strtol])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
