all: life life_ncurses life_sdl
util.o: util.c util.h
	gcc -std=gnu99 -o $@ util.c -O2 -g -c
life: life.c life.h util.o
	gcc -std=gnu99 -o $@ $^ -O2 -g -DLIFE_MAIN
life_ncurses: life.c life.h life_ncurses.c util.o
	gcc -std=gnu99 -o $@ $^ -O2 -g -lcurses
sdl_test: sdl_test.c sdl_util.h
	gcc -std=gnu99 -o $@ $^ -O2 -g -lSDL2
LIFE_SDL_SOURCES := life_sdl.c life_sdl_eventloop.c life_sdl_work.c\
life_sdl.h sdl_util.h sdl_util.c life.c life.h util.o
life_sdl: $(LIFE_SDL_SOURCES)
	gcc -std=gnu99 -o $@ $^ -O2 -g -lSDL2 -pthread
clean:
	rm -f life life_ncurses sdl_test life_sdl util.o
