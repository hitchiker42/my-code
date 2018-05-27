DEBUGFLAGS= -DDEBUG -fsanitize=address
CPPFLAGS=-I$(PWD)/../C++/util -Wall -Wextra \
-Wno-unused-function -Wmissing-braces
CXXFLAGS=-std=gnu++17 -g -Og $(DEBUGFLAGS)  -fno-rtti -fno-exceptions
CXXFLAGS_NDEBUG=-std=gnu++17 -O2 -DNDEBUG -fno-rtti -fno-exceptions
CFLAGS=-std=gnu11 -g -Og $(DEBUGFLAGS)

LDFLAGS=-lssl -lcrypto -lsqlite3 -lpcre -lz
.PHONY: clean

all: vndb_cpp tag_trait_parser

vndb.o : vndb.cpp vndb.h json.hpp sql.h
connection.o : connection.cpp vndb.h json.hpp
sqlite.o : sqlite.cpp vndb.h json.hpp sql.h
sqlite_ext.o: sqlite_ext.c
tag_trait_parser.o: tag_trait_parser.cpp vndb.h json.hpp sql.h
# compile c++ to asm
%.s:%.cpp
	$(CXX) $(CXXFLAGS_NDEBUG) $(CPPFLAGS) -S $< -o $@
# compile c++ to asm, then demangle names.
# Just a quick note, -o- prints to stdout, at least when outputing asm
%.S:%.cpp
	$(CXX) $(CXXFLAGS_NDEBUG) $(CPPFLAGS) -S $< -o- | c++filt > $@

vndb_cpp: vndb.o connection.o sqlite.o # sqlite_ext.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^

tag_trait_parser: connection.o sqlite.o tag_trait_parser.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^

clean:
	rm -f vndb_cpp *.o
