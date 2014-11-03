bin_PROGRAMS = tree tree_debug tree_prof
tree_SOURCES = graph.c heap.c spanning_tree.c
tree_debug_SOURCES = graph.c heap.c spanning_tree.c
tree_prof_SOURCES = graph.c heap.c spanning_tree.c
tree_debug_CFLAGS = -g -Og -std=c99 -Wall -Wno-unused -DDEBUG
tree_CFLAGS = -O3 -std=c99 -mtune=native -Wall -Wno-unused -DNDEBUG  -pedantic  -ffast-math
tree_prof_CFLAGS =  -g -O3 -fopenmp -std=c99 -mtune=native -Wall -Wno-unused -DNDEBUG -ffast-math -pg
