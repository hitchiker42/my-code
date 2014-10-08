CC=gcc
CFLAGS= -pedantic -Wall  -std=c99

SRC=	align.c align_io.c dfs_solver.c

all: align align_debug

align: $(SRC)
	$(CC) $(CFLAGS) -DNDEBUG -O2 $^ -o align

align_debug: $(SRC)
	$(CC) $(CFLAGS) -g $^ -o align_debug -std=gnu99

clean:
	rm -f align align_debug
	rm -f *.o

