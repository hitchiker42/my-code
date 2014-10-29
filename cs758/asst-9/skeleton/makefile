CC=gcc
CFLAGS=-lm -ansi -pedantic -Wall -Werror

SRC=	spanning_tree.c \
	graph.c

all: tree tree_debug

tree: $(SRC)
	$(CC) $(CFLAGS) -DNDEBUG -O3 $^ -o tree


tree_debug: $(SRC)
	$(CC) $(CFLAGS) -g $^ -o tree_debug

clean:
	rm -f tree tree_debug
	rm -f *.o

