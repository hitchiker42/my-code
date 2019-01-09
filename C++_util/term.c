#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>

static const char ansi_csi[2] = {'\e', '['}; //0x1b, 0x5b
//Macro version to allow compile time string concatenation
#define ANSI_CSI "\e["
#define ANSI_CSI_SEQ(str) ANSI_CSI " " str
volatile static sig_atomic_t caught_signal;
//Static variables for the term fd and old term attributes
//it makes more sense to just use global variables here since
//the terminal state itself is already global.
static struct termios saved_term_attrs;
static int term_fd = STDIN_FILENO;

static void handle_signal(int signo){
  caught_signal = signo;
}
void reset_term_attrs(){
  if(tcsetttr(term_fd, saved_term_attrs) < 0){
    perror("tcgetattr");
  }
}
int init_term_attrs(){
  if(term_fd < 0){
    return -1;
  }
  int err = 0;
  struct termios tios;
  if((err = tcgetattr(term_fd, &saved_term_attrs)) < 0){
    perror("tcgetattr");
    return err;
  }  
  atexit(reset_term_attrs);

  //Mostly taken from linenoise, since I'm not sure what all the flags do.
  /* Start with the current terminal settings */
  tios = saved_term_attrs;
  /* input modes: no break, no CR to NL, no parity check, no strip char,
   * no start/stop output control. */
  tios.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  /* output modes - disable post processing */
  tios.c_oflag &= ~(OPOST);
  /* control modes - set 8 bit chars */
  tios.c_cflag |= (CS8);
  //ensure 8 bit input bytes and that we get newlines properly
  tios.c_iflag &= ~(ISTRIP | IGNCR | INLCR | BRKINT | IGNBRK);
  /* local modes - echoing off, canonical off, no extended functions,
   * no signal chars (^Z,^C,^\) */
  tios.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  //Conditions for returning from read call. 
  //VMIN is the minimum number of characters
  //VTIME is the maximum amount of time, or ∞ if 0 
  //This sets read to block until at least 1 byte is available
  tios.c_cc[VMIN] = 1; 
  tios.c_cc[VTIME] = 0;
  //Flush input and output buffers before changing mode.
  if((err = tcsetattr(term_fd, TCSAFLUSH, &tios)) < 0){
    perror("tcsetattr");
    return err;
  }
  return 0;
}

int get_term_width(int tty){
  //try to get the value using the COLUMNS env variable first,
  //since its much simpler & more portable than making an ioctl call. 
  int termwidth = 0;
  char *cols = getenv("COLUMNS");
  if(cols){
    return strtol(cols, NULL, 10);
  } else {
    struct winsize ws;
    if(ioctl(tty, TIOCGWINSZ, &ws) < 0){
      return 80; // if ioctl fails just guess
    } else {
      return ws.ws_col;
    }
  }
}
/*
  CSI = Control Sequence Introduction = ESC [
  n,m = positive integers, optional
  Rows and columns use 1 based indexing.
  Movement:
  CSI n [A-D] | Relative movement, default n = 1
    A = up, B = down, C = forward, D = back
  CSI n [EF] | Line movement, moves to column 1, default n = 1
    E = next line, F = previous line
  CSI n;m H | absolute movement, default n,m = 1
  CSI n J | erase display, default n = 0
    for n = 0 clear from cursor to end of display
    for n = 1 clean from curson to start of display
    for n = 2,3 clear whole display, and scrollback buffer for 3
  CSI n K | erase line, default n = 0
    same meanings for n as last command, but line based, and
    only 0,1,2 are valid.
  CSI n [ST] | scroll, default n = 1
    S = up, T = down
  CSI [su] | save/restore cursor position (s = save, u = restore)
  CSI 6n | literal n
    causes current cursor position to be written to output in the format
    CSI n; mR
  CSI n m | literal m, n is not optional, alias SGR
    Introduces a graphics command (select graphic rendition)
    Only the most useful are shown below
  SGR 0 | reset graphics to default
  SGR 30-37 | set forground color
  SGR 90-97 | set bright forground color
  SGR 40-47 | set background color
  SGR 100-107 | set bright background color
    Sets color to one of 8 predefined colors they are (in order):
    black, red, green, yellow, blue, magenta, cyan, white
  SGR 38 2;r;g;b | set forground color to given rgb triplet
  SGR 48 2;r;g;b | set background color to given rgb triplet
*/
//Converts n into a string, with the least significant digit at ptr, and
//further digits at ptr-1,ptr-2,...
static inline char* my_itoa(char *ptr, int n){
  while(n > 0){
    int rem = n % 10;
    *ptr-- = rem + 0x30;
    n /= 10;
  }
  return ptr;
}
static inline char* fmt_one_arg_cmd(char *buf, size_t sz,
                                    int n, char cmd){
  //write sequence backward so we can eaisly do the conversion
  //from number to string ourselves
  char *ptr = buf + sz-1;//pointer to last char in buf
  *ptr-- = '\0';
  *ptr-- = cmd;
  ptr = my_itoa(ptr, n);
  *ptr-- = ANSI_CSI[1];
  *ptr-- = ANSI_CSI[0];
  return ptr;
}
static inline char* fmt_two_arg_cmd(char *buf, size_t sz,
                                    int n, int m, char cmd){
  //write sequence backward so we can eaisly do the conversion
  //from number to string ourselves
  char *ptr = buf + sz-1;//sz-1 is null terminator
  *ptr-- = '\0';
  *ptr-- = cmd;
  ptr = my_itoa(ptr, m);
  *ptr-- = ';';
  ptr = my_itoa(ptr, n);
  *ptr-- = ANSI_CSI[1];
  *ptr-- = ANSI_CSI[0];
  return ptr;
}
/* Use the ESC [6n escape sequence to query the horizontal cursor position
 * and return it. On error -1 is returned, on success the position of the
 * cursor. */
static int get_cursor_position(int ifd, int ofd) {
  static const char*
    char buf[32];
    int cols, rows;
    unsigned int i = 0;

    /* Report cursor location */
    if (write(ofd, "\x1b[6n", 4) != 4) return -1;

    /* Read the response: ESC [ rows ; cols R */
    while (i < sizeof(buf)-1) {
        if (read(ifd,buf+i,1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
    }
    buf[i] = '\0';

    /* Parse it. */
    if (buf[0] != ESC || buf[1] != '[') return -1;
    if (sscanf(buf+2,"%d;%d",&rows,&cols) != 2) return -1;
    return cols;
}

/* Try to get the number of columns in the current terminal, or assume 80
 * if it fails. */
static int getColumns(int ifd, int ofd) {
    struct winsize ws;

    if (ioctl(1, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        /* ioctl() failed. Try to query the terminal itself. */
        int start, cols;

        /* Get the initial position so we can restore it later. */
        start = getCursorPosition(ifd,ofd);
        if (start == -1) goto failed;

        /* Go to right margin and get position. */
        if (write(ofd,"\x1b[999C",6) != 6) goto failed;
        cols = getCursorPosition(ifd,ofd);
        if (cols == -1) goto failed;

        /* Restore position. */
        if (cols > start) {
            char seq[32];
            snprintf(seq,32,"\x1b[%dD",cols-start);
            if (write(ofd,seq,strlen(seq)) == -1) {
                /* Can't recover... */
            }
        }
        return cols;
    } else {
        return ws.ws_col;
    }

failed:
    return 80;
}

/* Clear the screen. Used to handle ctrl+l */
void linenoiseClearScreen(void) {
    if (write(STDOUT_FILENO,"\x1b[H\x1b[2J",7) <= 0) {
        /* nothing to do, just to avoid warning. */
    }
}
