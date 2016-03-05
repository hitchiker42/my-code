CC=gcc
CFLAGS=-ansi -pedantic -Wall -Werror

SRC=	trie.c		\
	depgraph.c	\
	nodelist.c	\
	depfile.c	\
	debug.c		\
	depends.c

all: depends depends_debug

depends: $(SRC)
	$(CC) $(CFLAGS) -DNDEBUG -O2 $^ -o depends


depends_debug: $(SRC)
	$(CC) $(CFLAGS) -g $^ -o depends_debug

clean:
	rm -f depends depends_debug
	rm -f *.o

