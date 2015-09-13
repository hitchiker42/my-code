#
# Makefile for C++ OpenGL applications suitable for CS770/870
#   rdb
#
#   07/29/14   Added trick to get MAIN name from directory
#   08/06/14   Added CS770 variable to reference required packages that are 
#              installed in default system locations
#   09/09/15   Added -lpthread to GL_LIBS on non-Darwin systems; fixes a
#              problem with some linux systems without breaking ours!
#-------------------------------------------------------------------
# If the program executable name is NOT the name of the directory containing
# the code, just modify the MAIN variable to specify the name of your executable
# You may also add entries to MYCPPFLAGS.
#
#---------------------------------------------------------------------

#--------- You can edit MAIN on next if you want.
MAIN = $(notdir ${PWD} )   # change if dir name is not executable name
#MAIN = basicGLSL

# Note there can be no spaces inside the "ifeq" parenthetical expression
ifeq ($(USER),cs770)
    MYCS770 = $(HOME)
else    
#------------ You can edit MYCS770 below
    MYCS770 = $(HOME)/cs770
                           # change to your local installation directory
                           # This directory should have subdirectories:
                           #   lib - where package load libraries are kept
                           #   include - where package include files are
                           # These are NOT locations of your program
endif

# if CS770 is NOT defined in environment define it with MYCS770
CS770 ?= $(MYCS770)

# set "special" flags here; "g" generates debug info
CPPFLAGS = -g
# -------------------- set system-dependent variables ----------------
OS := $(shell uname)

ifeq ($(OS),Darwin)
    CC = clang
    CCC = clang++
    LINKER = clang++
    GL_LIBS     = -framework OpenGL -framework GLUT -lGLEW
    CPPFLAGS    += -Wno-overloaded-virtual -Wno-deprecated-declarations

else
    CC          = gcc 
    CCC         = g++
    LINKER      = g++

    X_INC       = -I/usr/X11/include
    XLIBS       = -L/usr/X11/lib 

        # -lpthread addresses a problem with some linux systems and/or
        #   configurations that causes a segfault before main is executed.
        #   It occurs with a combination of glut and std::string in the same
        #   program???
    GL_LIBS     = -L/usr/lib64 -lglut -lpthread -lGLU -lGL -lGLEW
endif

# -------------- Common specifications ---------------------------------

MAKEFILE    = Makefile

LIBS        =  $(XLIBS) $(GL_LIBS) -L$(CS770)/lib # -lglui -ljpeg 

# -------------- Compiler and Linker flags ---------------
CPPFLAGS    += -Wall -std=c++11 $(X_INC) -I$(CS770)/include
#
# If any linker flags are needed, they should be prefixed by "-Wl," 
# This insures that cpp or cc will not confuse these flags with ones
# intended for it.
# 
LDFLAGS     =  

#---------- Application info ------------------------------------------

SRCS = $(wildcard *cpp)

# for every .cpp input, need to produce a .o
OBJS = $(SRCS:.cpp=.o) 

#------------------- dependencies/actions ------------------------
# dependency: need1 need2 ...  
#         action(s)
#
default: all

build: compile $(MAIN)

compile: $(OBJS) 

clean:	clear all

all: run

%.o : %.cpp %.h
	$(CCC) -c $(CPPFLAGS) $*.cpp

%.o : %.cpp
	$(CCC) -c $(CPPFLAGS) $*.cpp

$(MAIN): $(OBJS) $(COBJS)
	$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(MAIN)

run: build
	./$(MAIN)

clear:
	rm -f *.o $(MAIN)
