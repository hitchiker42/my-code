bin_PROGRAMS = parser parser_debug my_parser
AM_CFLAGS = -std=c99 -pedantic -Wall -Wno-unused
parser_SOURCES = parser.c cyk.c rdp.c
parser_debug_SOURCES = parser.c cyk.c rdp.c
parser_debug_CFLAGS = $(AM_CFLAGS) -g -Og -DDEBUG
parser_CFLAGS = $(AM_CFLAGS) -DNDEBUG
my_parser_SOURCES = my_parser.c lexer.c rdp.c cyk.c
my_parser_CFLAGS = $(AM_CFLAGS) -DMY_PARSER -Og
