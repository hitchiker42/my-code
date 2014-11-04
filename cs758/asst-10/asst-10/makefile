CC=gcc
CFLAGS=-ansi -pedantic -Wall -Werror

SRC=	roadmap.c	\
	system.c	\
	map_route.c

all: map-route map-route_debug

map-route: $(SRC)
	$(CC) $(CFLAGS) -DNDEBUG -O2 $^ -o map-route


map-route_debug: $(SRC)
	$(CC) $(CFLAGS) -g $^ -o map-route_debug

clean:
	rm -f map-route map-route_debug
	rm -f *.o

