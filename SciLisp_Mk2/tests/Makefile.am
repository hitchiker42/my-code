TESTS = scilisp_check scilisp_test.sh
check_PROGRAMS = scilisp_check
scilisp_check_SOURCES = scilisp_check.c scilisp_check.h
scilisp_check_CFLAGS = @CHECK_CFLAGS@ -Wall
scilisp_check_LDADD = $(top_builddir)/src/libscilisp.la @CHECK_LIBS@
scilisp_test.sh: scilisp_test.sh.in
	@echo "#!/bin/bash" > scilisp_test.sh;\
	echo "scilisp = $(srcdir)/src/Scilisp" >> scilisp_test.sh;\
	cat scilisp_test.sh.in >> scilisp_test.sh;	\
	chmod +x scilisp_test.sh
