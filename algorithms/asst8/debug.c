/**
 * \file utils.c
 *
 * Some utility functions.
 *
 * \author eaburns
 * \date 25-08-2010
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int current_debug_level = 1;


/* Debug printing. */
void dprintf(int level, const char *format, ...)
{
#if !defined(NDEBUG)
    va_list ap;
    va_start(ap, format);
    if (level <= current_debug_level)
	vfprintf(stderr, format, ap);
    va_end(ap);
#endif				/* !NDEBUG */
    return;
}
