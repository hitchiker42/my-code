bin_PROGRAMS = flow flow_debug
generic_sources = flow_net.c main.c util.c
AM_CFLAGS = -g -std=c99 -D_GNU_SOURCE -Wall -pedantic -Wno-unused
flow_SOURCES = $(generic_sources)
flow_debug_SOURCES = $(generic_sources)
flow_CFLAGS = $(AM_CFLAGS) -O3 -DNDEBUG
flow_debug_CFLAGS = $(AM_CFLAGS) -Og -DDEBUG
