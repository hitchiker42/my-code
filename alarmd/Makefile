all: alarmd alarmd_client
CC:=gcc
CFLAGS:=-g -O2 -pthread -lm -std=gnu99 -D_GNU_SOURCE -flto
alarmd: alarmd.o alarm_queue.o
	$(CC) $(CFLAGS)  $^ -o alarmd
alarmd_client: alarmd_client.o 
	$(CC) $(CFLAGS)  $^ -o alarmd_client
alarmd.o: alarmd.c alarmd.h
alarm_queue.o: alarm_queue.c alarmd.h
alarmd_client.o: alarmd_client.c alarmd.h
