all: life life_ncurses
life: life.c life.h
	gcc -std=gnu99 -o life $^ -O -g -DLIFE_MAIN
life_ncurses: life.c life.h life_ncurses.c
	gcc -std=gnu99 -o life_ncurses $^ -O -g -lcurses
