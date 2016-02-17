/*
  I'm not sure if mmap_resize needs to call munmap. munmap can 
  unmap multiple mappings at once, so if the current mapping and
  the new mapping overlap the old one will get unmapped when we
  unmap the new one. 

  However I need to have the address of the new mapping to calculate this,
  and if they don't overlap compeletly I'd need to unmap the old memory,
  but I'm not sure if this would also invalidate the memory I just mapped.

  So for now I'm just being careful and unmapping the old memory before
  mapping the new memory.
*/
#define ROUND_TO_PAGE_SIZE(sz)                  \
  ((sz + (PAGE_SIZE-1)) & ~(PAGE_SIZE-1))
/*
  Calls ftruncate to set the size of the underlying file of 's' to
  new_sz and updates the memory mapping to reflect this new size.
*/
int mmap_resize(mmap_stream *s, size_t new_sz){
  size_t actual_size = ROUND_TO_PAGE_SIZE(new_sz);
  off_t offset = s->bufptr - s->buf;
  if(ftruncate(s->fd, actual_size) < 0){
    perror("ftruncate");
    return -1;
  }
  munmap(s->buf, s->size);
  void *tmp = mmap(NULL, actual_size, s->mode, MAP_SHARED, s->fd, 0);
  if(tmp == MAP_FAILED){
    perror("mmap");
    return -1;
  }
  s->buf = tmp;
  s->bufptr = s->buf + offset;
}
