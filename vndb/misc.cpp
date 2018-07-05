#include "vndb.h"
//if prefix is a prefix of one and only one of the strings in 'strs' return the
//index of that string, if prefix is not a prefix of any of strs return -1,
//and if prefix is a prefix of more than one of strs return -2.
int is_unique_prefix(std::string_view prefix, const std::string_view *strs, int nstrs){
  int idx = -1;
  for(int i = 0; i < nstrs; i++){
    if(is_prefix_of(prefix, strs[i])){
      if(idx != -1){
        vndb_log->log_debug("'%s' is a prefix of '%s' and '%s'.\n",
                            prefix.data(), strs[idx].data(), strs[i].data());
        return -2;
      } else {
        idx = i;
      }
    }
  }
  return idx;
}
#if (defined __unix__)
void set_close_on_exec_all(){
  DIR* dirp = opendir("/proc/self/fd");
  struct dirent *d;
  while((d = readdir(dirp)) != nullptr){
    int fd = strtol(d->d_name, NULL, 0);
    set_close_on_exec(fd);
  }
  closedir(dirp);
  return;
}
#else
void set_close_on_exec_all(){};
#endif
#if 0
//Simplified version of tail from gnu coreutils.
bool my_tail(FILE* f_ptr, uintmax_t n_lines,
             off_t start_pos, off_t end_pos){
  static size_t bufsz = 512;
  char buffer[bufsz];
  size_t bytes_read;
  off_t pos = end_pos;
  FILE_wrapper f(f_ptr);
  if (n_lines == 0)
    return true;

  /* Set 'bytes_read' to the size of the last, probably partial, buffer;
     0 < 'bytes_read' <= 'BUFSIZ'.  */
  bytes_read = (pos - start_pos) % BUFSIZ;
  if (bytes_read == 0)
    bytes_read = BUFSIZ;
  /* Make 'pos' a multiple of 'BUFSIZ' (0 if the file is short), so that all
     reads will be on block boundaries, which might increase efficiency.  */
  pos -= bytes_read;
  xlseek (fd, pos, SEEK_SET, pretty_filename);
  bytes_read = safe_read (fd, buffer, bytes_read);
  if (bytes_read == SAFE_READ_ERROR)
    {
      error (0, errno, _("error reading %s"), quoteaf (pretty_filename));
      return false;
    }
  *read_pos = pos + bytes_read;

  /* Count the incomplete line on files that don't end with a newline.  */
  if (bytes_read && buffer[bytes_read - 1] != line_end)
    --n_lines;

  do
    {
      /* Scan backward, counting the newlines in this bufferfull.  */

      size_t n = bytes_read;
      while (n)
        {
          char const *nl;
          nl = memrchr (buffer, line_end, n);
          if (nl == NULL)
            break;
          n = nl - buffer;
          if (n_lines-- == 0)
            {
              /* If this newline isn't the last character in the buffer,
                 output the part that is after it.  */
              if (n != bytes_read - 1)
                xwrite_stdout (nl + 1, bytes_read - (n + 1));
              *read_pos += dump_remainder (false, pretty_filename, fd,
                                           end_pos - (pos + bytes_read));
              return true;
            }
        }

      /* Not enough newlines in that bufferfull.  */
      if (pos == start_pos)
        {
          /* Not enough lines in the file; print everything from
             start_pos to the end.  */
          xlseek (fd, start_pos, SEEK_SET, pretty_filename);
          *read_pos = start_pos + dump_remainder (false, pretty_filename, fd,
                                                  end_pos);
          return true;
        }
      pos -= BUFSIZ;
      xlseek (fd, pos, SEEK_SET, pretty_filename);

      bytes_read = safe_read (fd, buffer, BUFSIZ);
      if (bytes_read == SAFE_READ_ERROR)
        {
          error (0, errno, _("error reading %s"), quoteaf (pretty_filename));
          return false;
        }

      *read_pos = pos + bytes_read;
    }
  while (bytes_read > 0);

  return true;
}
#endif
