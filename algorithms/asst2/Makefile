CC=gcc
CFLAGS=-std=c11 -pedantic -Wall -Werror -ffloat-store -O2

all: add add_debug


add: add.c heap.c
	$(CC) $(CFLAGS) -DNDEBUG $^ -o $@


add_debug: add.c heap.c
	$(CC) $(CFLAGS) -g3 -Og $^ -o $@


clean:
	rm -f add add_debug
	rm -f *.o

