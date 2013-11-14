CFLAGS:= $(CFLAGS) -lm -pthread -O2 -g -std=gnu99 -D_GNU_SOURCE #-mavx
CC:=gcc
.PHONY: clean all asm
all: gen_consts fdtd
fdtd: fdtd.o fdtd_consts.o fdtd_io.o
	$(CC) $(CFLAGS) $^ -o fdtd
fdtd.o: fdtd.c fdtd.h xmalloc.h fdtd_consts.h
fdtd_io.o: fdtd_io.c fdtd_io.h fdtd_consts.h xmalloc.h
gen_consts: gen_consts.c
	$(CC) $(CFLAGS) $^ -o gen_consts
fdtd_consts.h: gen_consts gen_consts.c
	$(shell ./gen_consts)
clean:
	rm -f fdtd gen_consts *.o *.s
asm: 
	$(CC) $(CFLAGS) -fverbose-asm -S *.c
