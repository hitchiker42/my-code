AM_CPPFLAGS = -O2 -g $(CFLAGS_EXTRA) -std=gnu11 -Wall -Wno-unused
lib_LTLIBRARIES = libcutil.la
libcutil_la_SOURCES = C_util.c svector.c sorting.c heap.c utf8.c hash.c rbtree.c

check_PROGRAMS = test/test_sort test/test_hash test/test_rbtree
test_test_sort_LDADD = libcutil.la
test_test_hash_LDADD = libcutil.la
test_test_rbtree_LDADD = libcutil.la
TESTS: tests/run_test.pl
