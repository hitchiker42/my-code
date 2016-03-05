/**
 * \file align_io.h
 *
 *
 *
 * \author jtd7
 * \date 03-10-2011
 */

#ifndef ALIGN_IO_H
#define ALIGN_IO_H
#include <getopt.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


/*
 * Loads two token arrays from two different files.  The caller is
 * responsable for freeing the arrays.
 *
 * Return 0 on success and 1 on error.
 */
int load_token_arrays(char *f0, char *f1,
		      char **ary0, unsigned int *n0,
		      char **ary1, unsigned int *n1);

void output_alignment(char *str1, char  *str2);

#endif
