static constexpr char ANSI_CSI[2] = {'\e', '['}; //0x1b, 0x5b
int init_term_fd(int fd, struct termios *old_tios){
  if(fd < 0){
    return -1;
  }
  struct termios tios;
  if(tcgetattr(fd, old_tios) < 0){
    perror("tcgetattr");
    goto error;
  }  
  memcpy(tios, old_tios, sizeof(struct termios));
 error:
  close(fd);
  return -1;
}
int init_controlling_term(){
  return init_term_fd(open("/dev/tty", O_RDWR));
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
