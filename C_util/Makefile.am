AM_CPPFLAGS = -O2 -g $(CFLAGS_EXTRA) -std=gnu11 -Wall -Wno-unused

SOURCE_FILES = C_util.c svector.c sorting.c heap.c utf8.c hash.c \
		rbtree.c btree.c string_buf.c util_string.c
HEADER_FILES = C_util.h debug.h svector.h hash.h string_buf.h util_string.h

lib_LTLIBRARIES = libcutil.la
libcutil_la_SOURCES = $(SOURCE_FILES)
libcutil_la_CPPFLAGS = $(AM_CPPFLAGS) -D__LIBCUTIL__
include_HEADERS = $(HEADER_FILES)

## Move this stuff into a makefile.am in test and include it here
check_PROGRAMS = test/test_sort test/test_hash test/test_rbtree test/test_btree
# There has got to be a better way to do this
test_test_sort_LDADD = libcutil.la
test_test_sort_CPPFLAGS = $(AM_CPPFLAGS) -Og -g3
test_test_hash_LDADD = libcutil.la
test_test_hash_CPPFLAGS = $(AM_CPPFLAGS) -Og -g3
test_test_rbtree_LDADD = libcutil.la
test_test_rbtree_CPPFLAGS = $(AM_CPPFLAGS) -Og -g3
test_test_btree_LDADD = libcutil.la
test_test_btree_CPPFLAGS = $(AM_CPPFLAGS) -Og -g3 
TESTS: tests/run_test.pl
