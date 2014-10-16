/**
 * \file disk_loc.c
 *
 * Disk locations
 *
 * \author eaburns
 * \date 10-08-2010
 */

#include <stdio.h>

#include "disk_loc.h"

int compare_locations(struct disk_location *a, struct disk_location *b)
{
	if (a->track > b->track)
		return 1;
	else if (a->track < b->track)
		return -1;
	else
		return a->sector - b->sector;
}


void output_location(FILE *outfile, struct disk_location *loc)
{
	fprintf(outfile, "%d %d", loc->track, loc->sector);
}


int read_location (FILE *infile, struct disk_location *loc)
{
	int ret;
	unsigned int track, sector;

	ret = fscanf(infile, "%u %u\n", &track, &sector);
	if (ret == EOF)
		return EOF;
	if (ret != 2) {
		fprintf(stderr, "Bad input values when reading location\n");
		return 1;
	}

	loc->track = track;
	loc->sector = sector;

	return 0;
}
