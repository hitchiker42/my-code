CC=gcc
CFLAGS= -std=c11 -Wall -Werror -Wno-unused -O2 -g

all: parser parser_debug

PARSER_SOURCES = parser.c cyk.c rdp.c
parser: $(PARSER_SOURCES)
	$(CC) $(CFLAGS) -O2 -o $@ $^
parser_debug: $(PARSER_SOURCES)
	$(CC) $(CFLAGS) -Og -g3 -o $@ $^
clean:
	rm -f parser parser_debug

