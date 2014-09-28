/**
 * \file disk_loc.h
 *
 *
 *
 * \author eaburns
 * \date 10-08-2010
 */

#if !defined(_DISK_LOC_H_)
#define _DISK_LOC_H_

#include <stdio.h>

/* The direction of the head. */
enum direction { UP, DOWN };


/* A location on the disk. */
struct disk_location {
	unsigned int track;
	unsigned int sector;
};

/*
 * Compare two disk locations.  Returns negative if a < b, 0 if a == b
 * and positive if a > b.
 */
int compare_locations(struct disk_location *a, struct disk_location *b);

/*
 * Outputs the location to the given file.
 */
void output_location(FILE *outfile, struct disk_location *loc);

/*
 * Reads the next location from the given input file into the location
 * pointed to by the'loc' argument.
 *
 * Returns 0 on success, 1 on failure or EOF if the end of file was
 * reached (in which case 'loc' is left untouched).
 */
int read_location (FILE *infile, struct disk_location *loc);

#endif	/* !_DISK_LOC_H_ */
