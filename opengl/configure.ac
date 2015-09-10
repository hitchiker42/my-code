AC_PREREQ([2.69])
LT_PREREQ([2.4.5])
AC_INIT([opengl_tests], [0.01])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([subdir-objects foreign -Wall -Wno-gnu])

dnl AM_PROG_AR and AM_PROG_AS need to be called before LT_INIT
AM_PROG_AR
AM_PROG_AS
LT_INIT
dnl less verbose output (e.g. CC program.o\nAS asm.o\nCCLD program)
dnl normal compilation output can be obtained by adding V=1 to make invocation
AM_SILENT_RULES([yes])
# Checks for programs.

AC_PROG_AWK
AC_PROG_CC
AX_PROG_CC_C11
AX_PROG_CC_GNU11
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_LN_S

#Check for gcc, in order to set some compilier specific flags
AS_CASE([$CC], [gcc], [cflags_extra='-fmax-errors=20'],[cflags_extra=''])
AC_SUBST([CFLAGS_EXTRA],[$cflags_extra])

#required libraries

AC_CHECK_LIB([dl], [dlsym])
AC_CHECK_LIB([pthread], [pthread_create])
AC_CHECK_LIB([m], [cos])
AC_CHECK_LIB([GLEW], [glewInit])
AC_CHECK_LIB([glfw], [glfwInit])
AX_CHECK_GL
AX_CHECK_GLU
AX_CHECK_GLUT
AX_CHECK_GLX

AS_IF([test "x$no_gl" == xyes],
        [AC_MSG_ERROR([Error, libGL not found])])
AS_IF([test "x$no_glu" != xyes],
        [GL_CFLAGS="$GL_CFLAGS$GLU_CFLAGS" ]
        [GL_LIBS="$GL_LIBS$GLU_LIBS"],
        [AC_MSG_ERROR([Error, libGLU not found])])
AS_IF([test "x$no_glut" != xyes],
        [GL_CFLAGS="$GL_CFLAGS$GLUT_CFLAGS" ]
        [GL_LIBS="$GL_LIBS$GLUT_LIBS"],
        [AC_MSG_ERROR([Error, libGLUT not found])])
AS_IF([test "x$no_glx" != xyes],
        [GL_CFLAGS="$GL_CFLAGS$GLX_CFLAGS" ]
        [GL_LIBS="$GL_LIBS$GLX_LIBS"],
        [AC_MSG_WARN([Warning, libGLX not found])])

AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],
    [enable debugging options, currently the default])],
  [enable_debug=$enableval],
  [enable_debug=yes])
AS_IF([test "x$enable_debug" == xyes],
  [AC_DEFINE([DEBUG],[],[Enable debugging])],
  [AC_DEFINE([NDEBUG],[],[Disable debugging])])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
