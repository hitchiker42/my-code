//if prefix is a prefix of one and only one of the strings in 'strs' return the
//index of that string, if prefix is not a prefix of any of strs return -1,
//and if prefix is a prefix of more than one of strs return -2.
int is_unique_prefix(std::string_view prefix, std::string_view *strs, int nstrs){
  int idx = -1;
  for(int i = 0; i < nstrs; i++){
    if(is_prefix_of(prefix, strs[i])){
      if(idx != -1){
        vndb_log->log_debug("'%s' is a prefix of '%s' and '%s'.\n",
                            prefix.data(), strs[idx].data(), strfs[i].data());
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
