XCFLAGS:= -lm -O2 -g -std=gnu99 -D_GNU_SOURCE #-mavx
CC:=gcc
.PHONY: clean all asm
all: gen_consts fdtd
fdtd: gen_consts fdtd.c fdtd.h xmalloc.h fdtd_consts.h fdtd_io.c fdtd_io.h
	$(shell ./gen_consts)\
	$(CC) $(XCFLAGS) fdtd.h fdtd.c fdtd_io.c fdtd_consts.h -o fdtd
gen_consts: fdtd_consts.c
	$(CC) $(XCFLAGS) $^ -o gen_consts
clean:
	rm -f fdtd gen_consts *.o *.s
asm: 
	$(CC) $(XCFLAGS) -fverbose-asm -S *.c
