#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([spanning-tree],[0.1])
AC_CONFIG_AUX_DIR([build_aux])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE([subdir-objects foreign -Wall -Wno-gnu])
AM_PROG_AR
AM_PROG_AS
# Checks for programs.
AC_PROG_CC
AC_PROG_CC_C99

# Checks for libraries.
# FIXME: Replace `main' with a function in `-lm':
AC_CHECK_LIB([m], [cos])

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h string.h sys/time.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STRTOD
AC_CHECK_FUNCS([pow sqrt strtol])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
