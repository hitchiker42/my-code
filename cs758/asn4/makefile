CC=gcc
CFLAGS=-ansi -pedantic -Wall -Werror -std=c99

all: disk-sched disk-sched_debug disk-sched-rbtree

disk-sched: bst.c list.c disk_loc.c disk_sched.c rbtree.c
	$(CC) $(CFLAGS) -DNDEBUG -O2 $^ -o disk-sched


disk-sched_debug: bst.c list.c disk_loc.c disk_sched.c rbtree.c
	$(CC) $(CFLAGS) -g $^ -o disk-sched_debug

disk-sched-rbtree: bst.c list.c disk_loc.c disk_sched.c rbtree.c
	$(CC) $(CFLAGS) -g -O2 $^ -o $@

clean:
	rm -f disk-sched disk-sched_debug disk-sched-rbtree
	rm -f *.o
