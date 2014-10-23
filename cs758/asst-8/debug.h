/**
 * \file debug.h
 *
 * Debug printing.
 *
 * \author eaburns
 * \date 25-08-2010
 */

#if !defined(_DEBUG_H_)
#define _DEBUG_H_

/* debug levels based on the amount of output. */
#define DEBUG_SPAM 3
#define DEBUG_LOTS 2
#define DEBUG_MINOR 1

extern int current_debug_level;

/* Debug printing. */
void debug_printf(int level, const char *format, ...);


#endif				/* !_DEBUG_H_ */
