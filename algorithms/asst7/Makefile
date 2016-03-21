CC=gcc
CFLAGS= -std=c11 -Wall -Wno-unused -O2 -g -D_GNU_SOURCE -Iutil

all: parser parser_debug

PARSER_SOURCES = parser.c rdp.c my_parser.c util/hash.c util/svector.c\
	 util/string_buf.c util/util_string.c util/C_util.c
parser: $(PARSER_SOURCES)
	$(CC) $(CFLAGS) -O2 -o $@ $^
parser_debug: $(PARSER_SOURCES)
	$(CC) $(CFLAGS) -DDEBUG -Og -g3 -o $@ $^
clean:
	rm -f parser parser_debug

