bin_PROGRAMS = map-route map-route-debug
general_sources = roadmap.c system.c map_route.c heap.c util.c
general_cflags = -Wall -Wno-unused -pedantic -g
map_route_SOURCES = $(general_sources)
map_route_debug_SOURCES = $(general_sources)
map_route_CFLAGS = $(general_cflags) -O2 -DNDEBUG -std=c99
map_route_debug_CFLAGS = $(general_cflags) -DDEBUG -Og -std=gnu99
