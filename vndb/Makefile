DEBUGFLAGS= -DDEBUG # -fsanitize=address # Asan is great, but it breaks under gdb
CPPFLAGS=-I$(PWD)/../C++/util -Wall -Wextra \
-Wno-unused-function -Wmissing-braces
CXXFLAGS=-std=gnu++17 -g -Og $(DEBUGFLAGS)  -fno-rtti -fno-exceptions
CXXFLAGS_NDEBUG=-std=gnu++17 -O2 -DNDEBUG -fno-rtti -fno-exceptions
CFLAGS=-std=gnu11 -g -Og -lm $(DEBUGFLAGS)

LDFLAGS=-lssl -lcrypto -lpcre -lz -ljpeg -ldl #-lsqlite3 

SOURCES=$(wildcard *.cpp) $(wildcard *.h) $(wildcard *.c) json.hpp

PROGRAM_FILES=vndb_cpp vndb_cpp_old tag_trait_parser

.PHONY: all clean tags

all: vndb_cpp

# C++ files
main.o: main.cpp vndb.h json.hpp sql.h sqlite_wrappers.h connection.h image.h
vndb.o : vndb.cpp vndb.h json.hpp sql.h sqlite_wrappers.h connection.h
connection.o : connection.cpp vndb.h json.hpp connection.h sqlite_wrappers.h
sqlite_wrappers.o : sqlite_wrappers.cpp vndb.h json.hpp sql.h sqlite_wrappers.h
tag_trait_parser.o: tag_trait_parser.cpp vndb.h json.hpp sql.h sqlite_wrappers.h
# C files
sqlite_ext.o: sqlite_ext.c
image.o: image.c image.h
# the .o file doesn't actually depend on the .h file, but it
# makes more sense to put the dependency here rather than vndb_cpp.
progress_bar.o: progress_bar.c progress_bar.h
# compile c++ to asm
%.s:%.cpp
	$(CXX) $(CXXFLAGS_NDEBUG) $(CPPFLAGS) -S $< -o $@
# compile c++ to asm, then demangle names.
# Just a quick note, -o- prints to stdout, at least when outputing asm
%.S:%.cpp
	$(CXX) $(CXXFLAGS_NDEBUG) $(CPPFLAGS) -S $< -o- | c++filt > $@

# Compile the sqlite library, We complile it outselves since we use a fairly recent
# sql extension, but this allows us to improve performance quite a bit since
# we can disable things we don't need (like multithreading).
sqlite3.o: sqlite3.c
	$(CC) -c -O2 -o $@ $^ -Wall -DSQLITE_ENABLE_JSON1 \
	-DSQLITE_THREADSAFE=0 -DSQLITE_DEFAULT_MEMSTATUS=0 \
	-DSQLITE_LIKE_DOESNT_MATCH_BLOBS -DSQLITE_OMIT_SHARED_CACHE=1 \
	-DSQLITE_OMIT_DEPRECATED=1 -DSQLITE_OMIT_PROGRESS_CALLBACK=1 \
	-DSQLITE_DEFAULT_MEMSTATUS=0 -DSQLITE_USE_ALLOCA=1 \
	-DSQLITE_ENABLE_COLUMN_METADATA=1 -DSQLITE_ENABLE_STMTVTAB=1
vndb_cpp: vndb.o connection.o sqlite_wrappers.o progress_bar.o main.o image.o tag_trait_parser.o sqlite3.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^
vndb_cpp_old: vndb.o connection.o sqlite_wrappers.o progress_bar.o # sqlite_ext.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^ -lsqlite3

tag_trait_parser: connection.o sqlite_wrappers.o tag_trait_parser.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^ -lsqlite3

clean:
	rm -f *.o $(PROGRAM_FILES)
tags: TAGS
TAGS: $(SOURCES)
	ctags -eR *
