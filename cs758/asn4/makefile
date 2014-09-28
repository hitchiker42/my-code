CC=gcc
CFLAGS=-ansi -pedantic -Wall -Werror

all: disk-sched disk-sched_debug


disk-sched: bst.c list.c disk_loc.c disk_sched.c
	$(CC) $(CFLAGS) -DNDEBUG -O2 $^ -o disk-sched


disk-sched_debug: bst.c list.c disk_loc.c disk_sched.c
	$(CC) $(CFLAGS) -g $^ -o disk-sched_debug


clean:
	rm -f disk-sched disk-sched_debug
	rm -f *.o

