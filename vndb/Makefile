DEBUGFLAGS= -DDEBUG -fsanitize=address
CPPFLAGS=-I$(PWD)/../C++/util -Wall -Wextra \
-Wno-unused-function -Wmissing-braces
CXXFLAGS=-std=gnu++17 -g -Og $(DEBUGFLAGS)  -fno-rtti

LDFLAGS=-lssl -lcrypto -lsqlite3
.PHONY: clean

all: vndb_cpp

vndb.o : vndb.cpp vndb.h json.hpp
connection.o : connection.cpp vndb.h json.hpp

vndb_cpp: vndb.o connection.o #sqlite.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^


clean:
	rm -f vndb_cpp *.o
