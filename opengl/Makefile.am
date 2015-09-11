bin_PROGRAMS = basic_test cube
basic_test_SOURCES = basic_test.c gl_util.c C_util.c
cube_SOURCES = basic_loop.c gl_util.c C_util.c cube.c
AM_CFLAGS = @GL_CFLAGS@ -Wall -Wno-unused -g -O2 -D_GNU_SOURCE -fPIC
AM_LDFLAGS = @GL_LIBS@ @GL_LDFLAGS@ @LIBS@
