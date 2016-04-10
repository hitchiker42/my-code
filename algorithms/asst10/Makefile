CC=gcc
CFLAGS=-std=c11 -Wall

SRC=	roadmap.c system.c map_route.c heap.c roadmap_old.c

all: map-route map-route_debug

map-route: $(SRC)
	$(CC) $(CFLAGS) -DNDEBUG -O2 $^ -o map-route


map-route_debug: $(SRC)
	$(CC) $(CFLAGS) -fsanitize=address -std=gnu11 -g3 -DDEBUG $^ -o map-route_debug

clean:
	rm -f map-route map-route_debug
	rm -f *.o

