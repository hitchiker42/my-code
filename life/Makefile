all: life life_ncurses
util.o: util.c util.h
	gcc -std=gnu99 -o $@ util.c -O2 -g -c
life: life.c life.h util.o
	gcc -std=gnu99 -o $@ $^ -O2 -g -DLIFE_MAIN
life_ncurses: life.c life.h life_ncurses.c util.o
	gcc -std=gnu99 -o $@ $^ -O2 -g -lcurses
sdl_test: sdl_test.c sdl_util.h
	gcc -std=gnu99 -o $@ $^ -O2 -g -lSDL2
life_sdl: life_sdl.c sdl_util.h life.c life.h util.o
	gcc -std=gnu99 -o $@ $^ -O2 -g -lSDL2
