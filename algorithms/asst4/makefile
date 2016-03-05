CC=gcc
CFLAGS=-std=c11 -Wall

all: disk-sched disk-sched_debug


disk-sched: bst.c list.c disk_loc.c disk_sched.c rbtree.c
	$(CC) $(CFLAGS) -DNDEBUG -O2 $^ -o $@


disk-sched_debug: bst.c list.c disk_loc.c disk_sched.c rbtree.c
	$(CC) $(CFLAGS) -g $^ -o $@


clean:
	rm -f disk-sched disk-sched_debug
	rm -f *.o

