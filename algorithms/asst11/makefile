CC=gcc
CFLAGS=-lm -std=c99 -Wall -pedantic

SRC= flow_net.c

all: default

default: $(SRC) main.c
	$(CC) -DNDEBUG -O3 $^ -o flow $(CFLAGS)
	$(CC) -g $^ -o flow_debug $(CFLAGS)

clean:
	rm -f flow
	rm -f *.o

