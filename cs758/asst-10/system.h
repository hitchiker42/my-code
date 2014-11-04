/**
 * \file system.h
 *
 * Some systems level functions.
 *
 * \author eaburns
 * \date 18-08-2010
 */

#if !defined(_SYSTEM_H_)
#define _SYSTEM_H_

#include <stdio.h>

/* Gets the time of day in seconds. */
double get_current_seconds(void);

/* Get the peak memory usage for the current process. */
int peak_memory_usage(void);

#endif				/*!_SYSTEM_H_ */
