#include "text_edit.h"
namespace util {
//Some typedefs for static functions.
using pt_Descriptor = piece_table::Descriptor;
using pt_Marker = piece_table::Marker;
using pt_Edit = piece_table::Edit;
using pt_Span = piece_table::Span;
using pt_Offset = piece_table::Offset;
using pt_offset_t = piece_table::offset_t;
using pt_signed_offset_t = piece_table::signed_offset_t;
//I currently use forward_char for only forward movement and backward_char
//for only backward movement (calling the other function if given a
//negitive argument). It may be a better idea to just have them both
//call a seperate function which does all the movement.
bool piece_table::forward_char(Marker *mk, signed_offset_t N) {
  if(N < 0){
    return this->backward_char(mk, -N);
  }
  //No need to finish an edit in this case (I think).
  if(this->at_end_of_text(mk)){
    return false;
  }
  this->maybe_finish_edit();
  //Move mk to the right descriptor
  while(N >= mk->span_end_offset().as_chars){
    N -= mk->span_end_offset().as_chars;
    if(!this->marker_to_next_desc(mk)){
      return false; //At end of text;
    }
  }
  CharT *pt = this->get_marker_text(mk);
  CharT *end = this->get_descriptor_end(mk->desc);
  while(N--){ //N could be 0 after prevous loop
    int count = utf8_char_size(*pt);
    //The previous loop should ensure this.
    assert((pt + count) < end);
    mk->update(count);
  }
  return true;
}
bool piece_table::backward_char(Marker* mk, signed_offset_t N) {
  if(N < 0){
    return this->forward_char(mk, -N);
  }
  //No need to finish an edit in this case (I think).
  if(this->at_start_of_text(mk)){
    return false;
  }
  this->maybe_finish_edit();
  //We only use >, not >= to avoid leaving mk at the end of a descriptor.
  while(N > mk->span_offset.as_chars){
    N -= mk->span_offset.as_chars;
    if(!this->marker_to_prev_desc(mk)){
      return false; //at start of text
    }
  }
  const CharT *pt = this->get_marker_text(mk);
  do {
    pt = utf8_prev_char_start(pt);
    mk->update(-utf8_char_size(*pt));
  } while(--N);
  return true;
}
bool piece_table::goto_char(Marker *mk, offset_t N){
  offset_t char_length = this->text_length.as_chars;
  if(N > char_length){
    return false;
  }
  //Determine if N is closer to the start, point or the end.
  signed_offset_t pt_char = this->point->text_offset.as_chars;
  signed_offset_t dist_to_pt = N - pt_char;
  offset_t dist_to_edge = std::min(N, char_length - N);
  if(abs(dist_to_pt) < dist_to_edge){
    *mk = point;
    this->forward_char(mk, dist_to_pt);
  } else if(N < (char_length - N)){
    this->marker_to_start_of_text(mk);
    this->forward_char(mk, N);
  } else {
    this->marker_to_end_of_text(mk);
    this->backward_char(mk, N);
  }
  return true;
}
//Scan for the given byte in a utf8 string and return the offset of the byte
//in both characters and bytes. If the byte is not found the return value
//byte offset will be equal to sz (i.e it doesn't return a special sentinal
//value if the byte isn't found).
static pt_Offset find_byte_offset_fwd_utf8(const uint8_t *str, size_t sz,
                                           uint8_t byte){
  pt_Offset ret = {0,0};
  int idx = 0;
  while(idx < sz){
    uint8_t ch = str[idx];
    if(ch == byte){
      return ret;
    } else {
      int cnt = utf8_char_size(ch);
      ret.update(cnt);
      idx += cnt;
    }
  }
  return ret;
}
//Count the number of occurances of the given byte in the given string,
//decrementing 'cnt' for each occurance. If cnt reaches 0 return the 
//offset of the last byte, otherwise return the end offset.
static pt_Offset count_bytes_fwd_utf8(const uint8_t *str, size_t sz,
                                      uint8_t byte, int& cnt){
  pt_Offset ret = {0,0};
  int idx = 0;
  while(cnt && idx < sz){
    uint8_t ch = str[idx];
    if(ch == byte){
      --cnt;
    }
    int nbytes = utf8_char_size(ch);
    ret.update(nbytes);
    idx += nbytes;
  }
  return ret;
}
//Wrappers to avoid casting.
static uint8_t* u8memchr(const uint8_t *str, uint8_t byte, size_t sz){
  return static_cast<uint8_t*>(memchr(str, byte, sz));
}
static uint8_t* u8memrchr(const uint8_t *str, uint8_t byte, size_t sz){
  return static_cast<uint8_t*>(memrchr(str, byte, sz));
}
//Returns the offset of the first occurance of 'byte' in 'str' or 'sz'
//if byte doesn't occur (it's just memchr with a different return value).
static pt_offset_t find_byte_offset_fwd_ascii(const uint8_t *str, uint8_t byte,
                                              size_t sz){
  uint8_t *off = u8memchr(str, byte, sz);
  return (off ? off - str : sz);
}
static pt_offset_t count_bytes_fwd_ascii(const uint8_t *str, uint8_t byte,
                                         size_t sz, int &cnt){
  size_t off = 0;
  while(cnt){
    uint8_t* tmp = u8memchr(str + off, byte, sz - off);
    if(!tmp){ return sz; }
    off += (tmp - (str + off));
    cnt--;
  }
  return off;
}
//The way I've done these find_offset_bkwd functions is weird and kinda
//confunsing, but it makes the actual functions scaning descriptors
//eaiser, so I'm not sure if I should change them.
//They also probably don't work corrently due to indexing errors.

//The actual string is given by the range [(str-sz), str), so str should
//point just past the last valid character (like an end iterator).
//Returns an offset such that (str - (off.as_bytes + 1)) points to byte, this
//is so that returning 'off.as_bytes == sz' will indicate that the 
//byte wasn't found.
static pt_Offset find_byte_offset_bkwd_utf8(const uint8_t *str, size_t sz,
                                            uint8_t byte){
  pt_Offset ret = {0,0};
  int idx = 0;
  while(idx < sz){
    int cnt = utf8_prev_char_size(str - idx);
    //this is also str[-(idx+cnt)], but that just looks weird.
    uint8_t ch = *(str-(idx + cnt));
    if(ch == byte){
      return ret;
    } else {
      ret.update(nbytes);
      idx += cnt;
    }
  }
  return ret;
}
//Count the number of occurances of the given byte in the given string,
//which is really [(str - sz), str)
//decrementing 'cnt' for each occurance. If cnt reaches 0 return the 
//offset of the last byte, otherwise return the end offset.
static pt_Offset count_bytes_bkwd_utf8(const uint8_t *str, size_t sz,
                                       uint8_t byte, int& cnt){
  pt_Offset ret = {0,0};
  int idx = 0;
  while(cnt && (idx < sz)){
    int nbytes = utf8_prev_char_size(str - idx);
    uint8_t ch = *(str-(idx + cnt));
    if(ch == byte){
      cnt--;
    }
    ret.update(nbytes);
    idx += nbytes;
  }
  return ret;
}
static pt_offset_t find_byte_offset_bkwd_ascii(const uint8_t *str, uint8_t byte,
                                               size_t sz){
  const uint8_t *start = str - sz;
  uint8_t *off = u8memrchr(start, byte, sz);
  return (off ? (sz-1) - (off - start) : sz);
}
static pt_offset_t count_bytes_bkwd_ascii(const uint8_t *str, uint8_t byte,
                                          size_t sz){
  size_t len = sz;
  const uint8_t *start = str - sz;
  while(cnt){
    uint8_t* tmp = u8memrchr(start, byte, len);
    if(!tmp){ return sz; }
    len = (tmp - start);
    cnt--;
  }
  return (sz-1) - len;
}
//Moves the given marker 'mk' to the first occurance of 'byte', searching
//forward from its current position. If 'byte' isn't found 'mk' is moved
//to the end of the text and false is returned, otherwise returns true.
static bool find_byte_forward_acc(piece_table *ptab,
                                  pt_Marker *mk, uint8_t byte){
  CharT *txt = get_text_at_marker(mk);
  if(mk->desc->unibyte_only()){
    pt_offset_t off = find_byte_offset_fwd_ascii(txt, byte, mk->desc->size());
    mk->update(off,off);
  } else {
    pt_Offset off = find_byte_offset_fwd_utf8(txt, byte, mk->desc->size());
    mk->update(off);
  }
  if(!mk->at_end_of_desc()){
    return true;
  } else if(ptab->marker_to_next_desc_unsafe(mk)){
    return find_byte_forward_acc(ptab, mk, byte);
  } else {
    return false;
  }
}
//Same as above, but searchs backward and leaves 'mk' at the start
//of text on failure.
static bool find_byte_backward_acc(piece_table *ptab,
                                   pt_Marker *mk, uint8_t byte){
  CharT *txt = get_text_at_marker(mk);
  if(mk->desc->unibyte_only()){
    pt_offset_t off = find_byte_offset_bkwd_ascii(txt, byte, mk->desc->size());
    mk->update(-off,-off);
  } else {
    pt_Offset off = find_byte_offset_fwd_utf8(txt, byte, mk->desc->size());
    mk->update(-off);
  }
  if(!mk->at_start_of_desc()){
    mk->update(-1,-1);
    return true;
  } else if(ptab->marker_to_prev_desc_unsafe(mk)){
    return find_byte_backward_acc(ptab, mk, byte);
  } else {
    return false;
  }
}
//Same as find_byte_forward_acc, but searches for 'cnt' occurances of byte,
//'cnt' is decremented each time 'byte' is found.
static bool count_bytes_forward_acc(piece_table *ptab, pt_Marker *mk,
                                    uint8_t byte, int &cnt){
  CharT *txt = get_text_at_marker(mk);
  if(mk->desc->unibyte_only()){
    pt_offset_t off = count_bytes_fwd_ascii(txt, byte, mk->desc->size());
    mk->update(off,off);
  } else {
    pt_Offset off = count_bytes_fwd_utf8(txt, byte, mk->desc->size());
    mk->update(off);
  }
  if(!mk->at_end_of_desc()){
    return true;
  } else if(ptab->marker_to_next_desc_unsafe(mk)){
    return find_byte_forward_acc(ptab, mk, byte);
  } else {
    return false;
  }
}
//same as above but search backward
static bool count_bytes_backward_acc(piece_table *ptab,
                                     pt_Marker *mk, uint8_t byte){
  CharT *txt = get_text_at_marker(mk);
  if(mk->desc->unibyte_only()){
    pt_offset_t off = count_bytes_bkwd_ascii(txt, byte, mk->desc->size());
    mk->update(-off,-off);
  } else {
    pt_Offset off = count_bytes_fwd_utf8(txt, byte, mk->desc->size());
    mk->update(-off);
  }
  if(!mk->at_start_of_desc()){
    mk->update(-1,-1);
    return true;
  } else if(ptab->marker_to_prev_desc_unsafe(mk)){
    return find_byte_backward_acc(ptab, mk, byte);
  } else {
    return false;
  }
}

//Creates a new empty descriptor immediately before the descriptor 'mk' is
//pointing to which points to the end of the buffer used for insertion.
//Assumes mk is pointing to the start of a descriptor.
static bool split_for_insertion(piece_table *ptab, piece_table::Marker* mk){
  ptab->split_at_marker(mk);
  assert(mk->desc->piece.length.zerop());
  mk->desc->piece.start = ptab->current - ptab->buf;
  mk->desc->piece.buffer = 0;
  return true;
}
bool piece_table::split_for_edit(edit_type type, Marker *mk){
  //Pointing to the end of text is a special case.
  if(this->at_end_of_text(mk)){
    return !(type == edit_type::forward_deletion);
  }
  //If at the start of a descriptor we can avoid one split.
  if(mk->at_start_of_desc()){
    if(type == edit_type::forward_deletion){
      return true;
    } else if(type == edit_type::backward_deletion){
      if(this->at_start_of_text(mk)){
        return false;
      } else {
        mk->desc = mk->desc->prev();
        mk->desc->span_offset = mk->desc->len();
        return true;
      }
    } else {
      return split_for_insertion(this, mk);
    }
  }
  this->split_at_marker(mk);
  if(edit == edit_type::backward_deletion){
    return true;
  } else {
    //Move from end of desc to begining of next, doesn't change the
    //logical location in the text.
    mk->desc = mk->desc->next();
    mk->span_offset.reset();
    if(edit == edit_type::forward_deletion){
      return true;
    } else {
      return split_for_insertion(this, mk);
    }
  }
}
/*
  How I'm doing edits (at the moment).
  Begin edit: Allocate an edit struct and copy point.
  For insertions: we just wait until we're done and mark the length.
  For deletions: We may need multiple descriptors, the 'desc' field
    in the 'location' Marker of the Edit may be a list of multiple descriptors
    if an entire descriptor was deleted.
  The descriptors in the edit structure are copies, but still point to the original
  prev/next descriptors, so to undo an edit we just need to link the old descriptors
  back into the list.
*/
void piece_table::begin_edit(edit_type type){
  if(this->redo_history){
    //This is the `normal` way to do undo/redo, any edit clears the redo
    //history, I'd like to support tree style undo eventually, but
    //this is the eaisest option, so I'm using it for now
    this->free_memory_block_list(this->redo_history);
    this->redo_history = nullptr;
  }
  this->current_edit = this->alloc_edit();
  this->current_edit->type = type;
  this->current_edit->location = this->duplicate_point();
}
void piece_table::do_insert(const CharT *bytes, offset_t nbytes,
                            offset_t nchars){
  this->maybe_resize(nbytes);
  if(!this->is_continuous_edit(edit_type::insertion)){
    this->maybe_finish_edit();
    this->split_for_edit(edit_type::insertion);
    this->begin_edit(edit_type::insertion);
  }
  this->current = (CharT*)mempcpy(this->current, bytes, nbytes);
  this->text_length.update(nbytes, nchars);
  this->point.update(nbytes, nchars);
  this->point.desc->piece.update(nbytes, nchars);
  this->update_current_edit_length(nbytes, nchars);
}
void piece_table::insert_char(const CharT *utf8_char){
  int count = utf8_char_size(*utf8_char);
  this->do_insert(utf8_char, count, 1);
}
void piece_table::insert_string(std::string_view sv, signed_offset_t nchars){
  if(nchars == -1){
    nchars = utf8_strlen(sv);
  }
  this->do_insert((CharT*)sv.data(), (offset_t)sv.size(), (offset_t)nchars);
}
static void delete_backwards_simple(piece_table *p, int N){
  const piece_table::CharT *pt = p->get_point_text();
  const piece_table::CharT *start = pt;
  for(int i = 0; i < N; i++){
    pt = utf8_prev_char_start(pt);
  }
  int count = start - pt;
  p->point.update(-count, -N);
  p->point.desc->piece.update(-count, -N);
  p->text_length.update(-count, -N);
  p->update_current_edit_length(count, N);
  return;
}
static void delete_forwards_simple(piece_table *p, int N){
  const piece_table::CharT *pt = p->get_point_text();
  const piece_table::CharT *start = pt;
  for(int i = 0; i < N; i++){
    pt = utf8_next_char_start(pt);
  }
  int count = start - pt;
  //deleting forward, point doesn't move
  p->point.desc->piece.update(-count, -N);
  //We need to move the start of the span forwards
  p->point.desc->piece.start += count;
  p->text_length.update(-count, -N);
  p->update_current_edit_length(count, N);
  return;
}
bool piece_table::delete_descriptor_for_edit(){
  Edit*& edit = this->current_edit(); //to save typing
  Descriptor* desc = this->point->desc; //Descriptor we're deleting
  edit->length += desc->len();
  this->text_length -= desc->len();
  assert(edit_type_is_deletion(edit->type));
  bool is_forward_deletion = (edit->type == edit_type::forward_deletion);
  //Move point out of the descriptor we're removing.
  if(is_forward_deletion){
    if(!this->point_to_next_desc()){ return false; }
  } else {
    if(!this->point_to_prev_desc()){ return false; }
  }
  //Set edit->location equal to point, reusing desc for the descriptor.
  this->unlink_descriptor(desc); //remove desc from table
  Descriptor *tmp = edit->location->desc;
  //Copy point, reusing the memory from the descriptor we just removed
  edit->location = this->duplicate_point(desc);
  if(is_forward_deletion){
    edit->location.desc->prev() = tmp;
  } else {
    edit->location.desc->next() = tmp;
  }
  return true;
}
bool piece_table::delete_char(signed_offset_t N){
  if(N < 0){
    return this->delete_char_backwards(-N);
  }
  if(!this->is_continuous_edit(edit_type::forward_deletion)){
    this->maybe_finish_edit();
    if(!this->split_for_edit(edit_type::forward_deletion)){
      return false;
    }
    this->begin_edit(edit_type::forward_deletion);
  }
  while(N >= this->point.span_end_offset().as_chars){
    N -= this->point.span_end_offset().as_chars;
    if(!this->point_to_next_desc()){
      return false; //At end of text;
    }
    this->delete_descriptor(this->point.desc->prev());
  }
  if(N){
    delete_forwards_simple(this, N);
  }
  return true;
}
bool piece_table::delete_char_backwards(signed_offset_t N){
  if(N < 0){
    return this->delete_char(-N);
  }
  if(!this->is_continuous_edit(edit_type::backward_deletion)){
    this->maybe_finish_edit();
    if(!this->split_for_edit(edit_type::backward_deletion)){
      return false;
    }
    this->begin_edit(edit_type::backward_deletion);
  }
  //Unlike in backward_char we can leave point at the end of a descriptor.
  while(N >= this->point.span_offset.as_chars){
    N -= this->point.span_offset.as_chars;
    if(!this->point_to_prev_desc()){
      return false;
    }
    //Will likely need to modify this to support undo.
    this->delete_descriptor(this->point.desc->next());
  }
  if(N){
    delete_backwards_simple(this, N);
  }
  return true;
}
//The complexity of this comes from creating an Edit structure for supporting
//undo, since I'm putting that off for now this doesn't really do much yet.
void piece_table::finish_edit(){
  Edit*& edit = this->current_edit();
  //If this is a deletion fixup the marker in edit so it points just
  //before/after the last character deleted.
  if(edit->type == edit_type::forward_deletion){
    //For forward deletion place the marker after the last character deleted.
    Offset diff = edit->location.desc->len() - this->point.desc->len();
    edit->location.span_offset += diff;
    edit->location.text_offset += diff;
  } else if(edit->type == backwards_deletion) {
    //For backward deletion we move the location so it points to the last
    //character deleted.
    edit->location.span_offset = this->point.span_offset;
    edit->location.text_offset = this->point.text_offset;
  }
  //Normalize point so its not pointing at the end of a descriptor unless we're
  //at the end of the text. This happens for insertion and backward deletion.
  if(this->point.at_end_of_desc()){
    this->point_to_next_desc();
  }
  edit->next() = this->undo_history;
  this->undo_history = edit;
  this->current_edit = nullptr;
}
/*
  TODO: Try and abstract some of the complicated pointer juggling in the undo/redo
  functions into some helper functions.
*/
//For these 3 functions I'm relying on the compiler to inline them and remove
//most of the branches, if it doesn't I'll need to make more specialized functions.
static pt_Descriptor* splice_desc_prev(piece_table *ptab, pt_Descriptor *desc){
  pt_Descriptor *ret;
  if(desc->prev() == nullptr){//first descriptor
    ret = ptab->table;
    ptab->table = desc;
  } else {
    ret = desc->prev()->next();
    desc->prev->next() = desc;
  }
  return ret;
}
static pt_Descriptor* splice_desc_next(piece_table *ptab, pt_Descriptor *desc){
  pt_Descriptor *ret;
  if(desc->next() == nullptr){//last descriptor
    ret = ptab->table_end;
    ptab->table_end = desc;
  } else {
    ret = desc->next()->prev();
    desc->next->prev() = desc;
  }
  return ret;
}
static pt_Descriptor* splice_desc(piece_table *ptab, pt_Descriptor *desc){
  pt_Descriptor *ret;
  if(desc->next() == nullptr){//last descriptor
    ret = ptab->table_end;
    ptab->table_end = desc;
    desc->prev()->next() = dest;
  } else if(desc->prev() == nullptr){
    ret = ptab->table;
    ptab->table = desc;
    desc->next()->prev() = desc;
  } else {
    ret = desc->prev()->next();
    desc->prev()->next() = desc;
    desc->next()->prev() = desc;
  }
  return ret;
}
pt_Descriptor* undo_insert(piece_table *ptab, pt_Edit *edit){
  pt_Marker *mk = &edit->location;
  pt_Descriptor *ret;
  pt_Descriptor *desc = mk->desc;
  if(desc->next() == nullptr){//insertion was at the end of the text.
    ret = splice_desc(ptab, desc);
    ptab->point_to_end_of_text();
  } else { //insertion not at end of text, desc is empty
    assert(desc->size() == 0);
    ret = desc->next()->prev();
    if(desc->prev() == nullptr){
      ptab->table = desc->next();
    } else {
      desc->prev()->next() = desc->next();
    }
    desc->next()->prev() = desc->prev();
    ptab->set_point(desc->next(), {0,0}, mk->text_offset);
    ptab->free_descriptor(desc);
    mk->desc = nullptr;
  }
  ptab->text_length -= edit->length;
  return ret;
}
pt_Descriptor* redo_insert(piece_table *ptab, pt_Edit *edit){
  pt_Marker *mk = &edit->location;
  pt_Descriptor *ret;
  pt_Descriptor *desc = mk->desc;
  if(desc->next() == nullptr){    //insertion was at the end of the text.
    //Here we actually have a descriptor to replace
    ret = splice_desc(ptab, desc);
    ptab->set_point(desc, ret->length(), ptab->text_length);
  } else { //insertion not at end of text, desc is empty
    //We need to allocate a new descriptor to indicate the location.
    if(desc->prev() == nullptr){
      ptab->table = desc->next();
    } else {
      desc->prev()->next() = desc;
    }
    desc->next()->prev() = desc;
    //Allocate a new empty descriptor at the same location as desc.
    ret = ptab->copy_descriptor(desc);
    ret->piece.length = {0,0};
    ptab->set_point(desc, {0,0}, mk->text_offset);
  }
  ptab->text_length += edit->length;
  return ret;
}
pt_Descriptor* undo_backward_delete(piece_table *ptab, pt_Edit *edit){
  pt_Marker *mk = &edit->location;
  pt_Descriptor *desc = mk->desc;
  pt_Descriptor *ret = splice_desc_prev(ptab, desc);
  Descriptor *next = ret->next();
  while(desc->next() != next){
    desc = desc->next();
  }
  //Sets point at the end of the undeleted characters
  if(next){
    next->prev() = desc;
    ptab->set_point(next, {0,0}, mk->text_offset);
  } else {
    ptab->table_end = desc;
    ptab->point_to_end_of_text();
  }
  ptab->text_length += edit->length;
  return ret;
}
pt_Descriptor* redo_backward_delete(piece_table *ptab, pt_Edit *edit){
  pt_Marker *mk = &edit->location;
  pt_Descriptor *desc = mk->desc;
  Descriptor *ret = splice_desc_prev(ptab, desc);
  //Change length here so point_to_end_of_text in the else branch works.
  ptab->text_length -= edit->length;
  //Potentially skip over some descriptors since we know the descriptor
  //we need to link to.
  Descriptor *next = desc->next();
  if(next){
    next->prev() = desc;
    ptab->set_point(next, {0,0}, mk->text_offset - edit->length);
  } else {
    ptab->table_end = desc;
    ptab->point_to_end_of_text();
  }
  return ret;
}
pt_Descriptor* undo_forward_delete(piece_table *ptab, pt_Edit *edit){
  pt_Marker *mk = &edit->location;
  pt_Descriptor *desc = mk->desc;
  Descriptor *ret = splice_desc_next(ptab, desc);
  Descriptor *prev = ret->prev();
  while(desc->prev() != prev){
    desc = desc->prev();
  }
  //Sets point at the end of the undeleted characters
  if(prev){
    prev->next() = desc;
    ptab->set_point(prev, {0,0}, mk->text_offset);
  } else {
    ptab->table_end = desc;
    ptab->point_to_end_of_text();
  }
  ptab->text_length += edit->length;
  return ret;
}
pt_Descriptor* redo_forward_delete(piece_table *ptab, pt_Edit *edit){
  pt_Marker *mk = &edit->location;
  pt_Descriptor *desc = mk->desc;
  Descriptor *ret = splice_desc_next(ptab, desc);
  //Change length here so point_to_end_of_text in the else branch works.
  ptab->text_length -= edit->length;

  Descriptor *prev = desc->prev();
  if(prev){
    prev->next() = desc;
    ptab->set_point(next, {0,0}, mk->text_offset - edit->length);
  } else {
    ptab->table_end = desc;
    ptab->point_to_end_of_text();
  }
  return ret;
}
Edit* piece_table::apply_edit(Edit *edit){
  switch(edit->type){
    case edit_type::insertion: {
      Descriptor *redo = undo_insert(this, edit);
      edit->type = insertion_undo;
      edit->location.desc = redo;
      return edit;
    }
    case edit_type::insertion_undo: {
      //This is an empty descriptor (or the last descriptor)
      Descriptor *undo = redo_insert(this, edit);
      edit->type = insertion;
      edit->location.desc = undo;
      return edit;
    }
    case edit_type::backward_deletion: {
      Descriptor *redo = undo_backward_delete(this, edit);
      edit->type = backward_deletion_undo;
      edit->location.desc = redo;
      return edit;
    }
    case edit_type::backward_deletion_undo: {
      Descriptor *undo = redo_backward_delete(this, edit);
      edit->type = backward_deletion;
      edit->location.desc = undo;
      return edit;
    }
    case edit_type::forward_deletion: {
      Descriptor *redo = undo_forward_delete(this, edit);
      edit->type = forward_deletion_undo;
      edit->location.desc = redo;
      return edit;
    }
    case edit_type::forward_deletion_undo: {
      Descriptor *undo = redo_forward_delete(this, edit);
      edit->type = forward_deletion;
      edit->location.desc = undo;
      return edit;
    }
  }
}
static void to_string_unsafe(piece_table *ptab, void *mem){
  auto desc = ptab->table;
  while(desc){
    auto txt = ptab->get_descriptor_text(desc);
    auto sz = desc->size();
    mem = mempcpy(mem, txt, sz);
    desc = desc->next();
  }
  *((char*)mem) = '\0';
}
size_t piece_table::to_string(CharT **ptr){
  *ptr = (CharT*)malloc(this->size() + 1);
  to_string_unsafe(this, *ptr);
  return this->size();
}
std::string piece_table::to_string(){
  std::string str(this->size() + 1, '\0');
  to_string_unsafe(this, str.data());
  return str;
}
} //Namespace util
