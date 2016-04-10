AM_CPPFLAGS = -O2 -g $(CFLAGS_EXTRA) -std=gnu11 -Wall -Wno-unused

SOURCE_FILES = C_util.c svector.c sorting.c heap.c utf8.c hash.c \
		rbtree.c btree.c string_buf.c util_string.c fib_heap.c
HEADER_FILES = C_util.h debug.h svector.h hash.h string_buf.h util_string.h

lib_LTLIBRARIES = libcutil.la
libcutil_la_SOURCES = $(SOURCE_FILES)
libcutil_la_CPPFLAGS = $(AM_CPPFLAGS) -D__LIBCUTIL__
include_HEADERS = $(HEADER_FILES)

include test/Makefile.am

TESTS: test/run_test.pl
