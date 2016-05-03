CC ?= gcc
ECHO_CFLAGS = -O2 -g -lpthread $(CFLAGS) 
echo: echo.c server.c client.c
	$(CC) $(ECHO_CFLAGS) $^ -o $@
