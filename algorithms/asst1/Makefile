CFLAGS = -pedantic -Wall -Werror -std=c11 -O2
.PHONY: all clean
all: sort-ints_debug sort-ints
sort-ints: sort-ints.c sorting.c
	$(CC) $(CFLAGS) $^ -o $@
sort-ints_debug: sort-ints.c sorting.c
	$(CC) $(CFLAGS) $^ -o $@
clean:
	rm -f sort-ints sort-ints_debug
