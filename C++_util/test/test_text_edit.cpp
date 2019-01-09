#include "../util.h"
//#include "../text_edit.h"
#include <curses.h>
static int term_rows;
static int term_cols;
//symbolic names for the argument to curs_set.
enum cursor_state {
  CURSOR_INVISIBLE = 0,
  CURSOR_VISIBLE = 1,
  CURSOR_VERY_VISIBLE = 2
};
void init_ncurses(bool enable_keypad = true){
  initscr();//start curses mode
  cbreak();//disable line buffering, pass ^C,^Z,etc to terminal
  //raw();//disable line buffering, don't pass ctrl chars to terminal
  noecho();//don't echo keyboard input;
  getmaxyx(stdscr, term_rows, term_cols);//get term dimensions
  //These take a window argument but ignore it
  //Input options
  meta(stdscr, true); //force 8 bit characters
  keypad(stdscr, enable_keypad); //get symbolic values for escape sequences
  //output options
  idlok(stdscr, true); //enable line insert/delete, since we're doing text editing
  scrollok(stdscr, true); //enable screen scrolling
}
void test_ncurses(WINDOW *win){
  static constexpr size_t bufsz = 256;
  int ch;
  int inbuf[bufsz];
  char fmtbuf[bufsz];
  char strbuf[bufsz];
  while((ch = wgetch(win)) != ERR){
    int *bufptr = inbuf;
    wtimeout(win, 0);//set non-blocking read mode.
    *bufptr++ = ch;
    //Read any pending characters/bytes
    while((ch = wgetch(win) != ERR)){
      *bufptr++ = ch;
    }
    {
      int *tmp = inbuf;
      int nbytes = snprintf(fmtbuf, bufsz, "Values read as integers: %d", *tmp++);
      while(tmp < bufptr){
        nbytes += snprintf(fmtbuf+nbytes, bufsz-nbytes, ", %d", *tmp++);
      }
    }
    {
      int *tmp = inbuf;
      char *strptr = strcpy(strbuf, "Values read as a string: ");

      while(tmp < bufptr){
        *strptr++ = ((*tmp++) & 0xff);
      }
      *strptr++ = '\0';
    }
    werase(win);
    mvwaddstr(win, 0, 0, fmtbuf);
    mvwaddstr(win, 1, 0, strbuf);
    wrefresh(win);
    wtimeout(win, -1);//back to blocking read
  }
}
int main(){
  init_ncurses(false);
  test_ncurses(stdscr);
}
