AC_PREREQ([2.69]) # autoconf minimum version
LT_PREREQ([2.4.5]) # libtool minium version
AC_INIT([util_c], [0.01]) # pkg information
AC_CONFIG_HEADERS([src/config.h]) # where to put config.h
AC_CONFIG_AUX_DIR([build-aux]) # where to put files needed by autotools
AC_CONFIG_MACRO_DIR([m4]) # where to search for extra macros
AM_INIT_AUTOMAKE([subdir-objects foreign -Wall -Wno-gnu]) # init automake

# AM_PROG_AR and AM_PROG_AS need to be called before LT_INIT
AM_PROG_AR # look for archiver
AM_PROG_AS # look for assembler
LT_INIT # init libtool
# less verbose output (e.g. CC program.o\nAS asm.o\nCCLD program)
# normal compilation output can be obtained by adding V=1 to make invocation
AM_SILENT_RULES([yes])
# Checks for programs.

AC_PROG_AWK # look for awk
AC_PROG_CC # look for C compiler
AC_PROG_CC_C99 # insure C compiler supports C99
AC_PROG_INSTALL # look for install (probably copy it to config_aux_dir)
AC_PROG_LN_S # look for program to make symbolic links

# Check for gcc, in order to set some compilier specific flags
# Specifically this limits the number of errors gcc will output
AS_CASE([$CC], [gcc], [cflags_extra='-fmax-errors=20'],[cflags_extra=''])
# create makefile variable from shell variable
AC_SUBST([CFLAGS_EXTRA],[$cflags_extra])

# required libraries
# 3rd argument is action if found, by default it appends the required
# linker flags to LIBS and defines HAVE_LIBRARY

# AC_CHECK_LIB([dl], [dlsym])
AC_CHECK_LIB([pthread], [pthread_create])
AC_CHECK_LIB([m], [cos])


# debugging code. Defaults to yes.
# Defines one of the macros DEBUG or NDEBUG in config.h.
AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],
    [enable debugging options, currently the default])],
  [enable_debug=$enableval],
  [enable_debug=yes])
AS_IF([test "x$enable_debug" == xyes],
  [AC_DEFINE([DEBUG],[],[Enable debugging])],
  [AC_DEFINE([NDEBUG],[],[Disable debugging])])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT # Generate output
