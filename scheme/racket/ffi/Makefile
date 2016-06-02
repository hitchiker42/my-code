all = racket_util.so
racket_util.so: racket_util.c
	gcc -std=gnu11 -g -O2 --shared -fPIC $(HOME)/usr/lib/racket/mzdyn3m.o \
	-I$(HOME)/usr/include $^ -o $@
