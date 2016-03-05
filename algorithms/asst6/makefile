CC=gcc
CFLAGS=-std=c11 -Wall -Werror

SRC=	align.c align_io.c dfs_solver.c

all: align align_debug

align: $(SRC)
	$(CC) $(CFLAGS) -DNDEBUG -O2 $^ -o $@

align_debug: $(SRC)
	$(CC) $(CFLAGS) -Og -g3 $^ -o $@

clean:
	rm -f align align_debug
	rm -f *.o

