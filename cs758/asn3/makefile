CC=gcc
CFLAGS= -std=c99 -g

babbler: markov.c eprintf.c eprintf.h htable.h htable.c linked.h linked.c storer.h storer.c stringpool.h stringpool.c
	$(CC) $(CFLAGS) -o babbler storer.c htable.c linked.c markov.c stringpool.c eprintf.c 

clean:
	rm -f babbler
