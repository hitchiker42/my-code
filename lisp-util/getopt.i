%module getopt
%{
  #define __GNU_LIBRARY__
  #include <getopt.h>
%}
#define __GNU_LIBRARY__
%include <getopt.h>

