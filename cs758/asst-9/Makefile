CC=gcc
CFLAGS=-lm -std=c99 -pedantic -Wall -Werror

SRC=	spanning_tree.c graph.c union_

all: tree tree_debug

tree: $(SRC)
	$(CC) $(CFLAGS) -DNDEBUG -g -O3 $^ -o tree


tree_debug: $(SRC)
	$(CC) $(CFLAGS) -std=gnu99 -DDEBUG -g $^ -o tree_debug

clean:
	rm -f tree tree_debug
	rm -f *.o

