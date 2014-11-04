/**
 * \file system.c
 *
 *
 *
 * \author eaburns
 * \date 18-08-2010
 */

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/time.h>
#include <sys/types.h>

#if !defined(LINE_MAX)
#if !defined(_POSIX2_LINE_MAX)
#define LINE_MAX 4096		/* should be large enough. */
#else
#define LINE_MAX _POSIX2_LINE_MAX
#endif				/* !_POSIX2_LINE_MAX */
#endif				/* !LINE_MAX */


double get_current_seconds(void)
{
    double sec, usec;
    struct timeval tv;

    if (gettimeofday(&tv, NULL) < 0) {
	perror("gettimeofday failed");
	exit(EXIT_FAILURE);
    }

    sec = tv.tv_sec;
    usec = tv.tv_usec;

    return sec + (usec / 1000000);
}

/* Eat characters until a space is found. */
static void eat_till_space(FILE * infile)
{
    while (!isspace(getc(infile)));
}


/* Eats whitespace and returns the first non-whitespace character (or
 * EOF). */
static int eat_space(FILE * infile)
{
    int c;

    do {
	c = getc(infile);
    } while (isspace(c));

    return c;
}


/* Adds 'c' to the 'i'th index of 'w' (performs bounds checking on the
 * array). */
static void add_to_word(FILE * infile, int c, unsigned int i,
			char w[], unsigned int n)
{
    if (i >= n) {
	fprintf(stderr, "Word is too long: truncating");
	eat_till_space(infile);
	w[n - 1] = '\0';
    } else {
	w[i] = c;
    }
}


/*
 * Reads the next token from the given input file.
 *
 * Return 0 on success or EOF if the end of file was reached (in which
 * case 'w' is left in an unknown state.
 */
static int next_token(FILE * infile, char w[], unsigned int n)
{
    int c;
    unsigned int i = 0;

    c = eat_space(infile);
    while (c != EOF) {
	if (isspace(c)) {
	    add_to_word(infile, '\0', i, w, n);
	    break;
	} else {
	    add_to_word(infile, c, i, w, n);
	    i += 1;
	}
	c = getc(infile);
    }

    if (c == EOF)
	return EOF;

    assert(w[i] == '\0');
    return 0;
}


/*
 * Reads tokens from the given file until the key is reached or EOF.
 *
 * Return 0 on success and EOF on end of file.
 */
static int read_till_key(FILE * f, const char *key)
{
    char word[LINE_MAX + 1];
    int ret;

    do {
	ret = next_token(f, word, LINE_MAX + 1);
	if (ret == EOF)
	    return EOF;
    } while (strcmp(word, key) != 0);

    return 0;
}

/* Get the peak memory usage (in MB) for the current process, or
 * negative on error. */
int peak_memory_usage(void)
{
    char word[LINE_MAX + 1];
    char *status_file;
    FILE *f;
    long pathmax = pathconf("/proc", _PC_PATH_MAX);
    pid_t pid = getpid();
    int ret;

    status_file = malloc((pathmax + 1) * sizeof(*status_file));
    if (!status_file) {
	perror("malloc failed");
	return -1;
    }
    sprintf(status_file, "/proc/%d/status", pid);

#if !defined(NDEBUG)
    printf("Opening status file: [%s]\n", status_file);
#endif				/* !NDEBUG */

    f = fopen(status_file, "r");
    if (!f) {
	perror("unable to open status file");
	free(status_file);
	return -1;
    }
    free(status_file);


    ret = read_till_key(f, "VmPeak:");
    if (ret || ret == EOF) {
	fclose(f);
	return -1;
    }

    ret = next_token(f, word, LINE_MAX + 1);
    if (ret == EOF) {
	fprintf(stderr, "Unexpected end of file\n");
	fclose(f);
	return -1;
    }

    fclose(f);

    return strtol(word, NULL, 10) / 1024;
}
