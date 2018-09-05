#include "text_edit.h"
namespace util {
//I currently use forward_char for only forward movement and backward_char
//for only backward movement (calling the other function if given a
//negitive argument). It may be a better idea to just have them both
//call a seperate function which does all the movement.
bool piece_table::forward_char(signed_offset N, Point &point){
  if(N < 0){
    return this->backward_char(-N, point);
  }
  //No need to finish an edit in this case (I think).
  if(this->at_end_of_text(point)){
    return false;
  }
  this->maybe_finish_edit();
  //Move point to the right descriptor
  while(N >= point.span_end_offset().as_chars){
    N -= point.span_end_offset().as_chars;
    if(!this->marker_to_next_desc(point)){
      return false; //At end of text;
    }
  }
  char *pt = this->get_marker_text(point);
  char *end = this->get_descriptor_end(point.desc);
  while(N--){ //N could be 0 after prevous loop
    int count = utf8_char_size(*pt);
    //The previous loop should ensure this.
    assert((pt + count) < end);
    point.update(count);
  }
  return true;
}
bool piece_table::backward_char(signed_offset N, Point &point){
  if(N < 0){
    return this->forward_char(-N, point);
  }
  //No need to finish an edit in this case (I think).
  if(this->at_start_of_text(point)){
    return false;
  }
  this->maybe_finish_edit();
  //We only use >, not >= to avoid leaving point at the end of a descriptor.
  while(N > point.span_offset.as_chars){
    N -= point.span_offset.as_chars;
    if(!this->marker_to_prev_desc(point)){
      return false; //at start of text
    }
  }
  char *pt = this->get_marker_text(point);
  do {
    pt = utf8_prev_char_start(pt);
    point.update(-get_utf8_char_size(*pt));
  } while(--N);
  return true;
}
void piece_table::split_for_deletion(Marker *pt){
  if(this->at_end_of_text(pt) || this->at_start_of_text(pt)){
    return;
  }
  if(pt->span_offset.zerop()){
    pt->desc = pt->desc->next();
    pt->span_offset = pt->desc.piece.length;
    return;
  }
  //It shouldn't be possible for pt to already point past the end
  //of a descriptor, since thats generally not allowed.

  this->split_at_marker(pt);
  return;
}
void piece_table::split_for_insertion(Marker *pt){
  if(this->at_end_of_text(pt)){
    return;
  }
  //if not pointing to the start of a descriptor split in two and
  //leave 'pt' at the start of the right of the split.
  if(!pt->span_offset.zerop()){
    this->split_at_marker(pt);
    pt->desc = pt->desc->next();
    pt->span_offset.reset();
  }
  //Split descriptor in two at the begining, leaving 'pt' pointing to an
  //empty descriptor
  this->split_at_marker(pt);
  assert(pt->desc->piece.length == {0,0});
  pt->desc->piece.start = this->current - this->buf;
  pt->desc->piece.buffer = 0;
  return;
}
void piece_table::do_insert(const CharT *bytes, offset_t nbytes,
                            offset_t nchars){
  this->maybe_resize(nbytes);
  if(!this->is_continuous_edit(edit_type::insert)){
    this->maybe_finish_edit();
    this->split_for_insertion();
    this->current_edit_type = edit_type::insert;
  }
  this->current = mempcpy(this->current, bytes, nbytes);
  this->text_length.update(nbytes, nchars);
  this->point.update(nbytes, nchars);
  this->point.desc->piece.update(nbytes, nchars);
  this->current_edit_length.update(nbytes, nchars);
}
void piece_table::insert_char(const CharT *utf8_char){
  int count = utf8_char_size(*utf8_char);
  this->do_insert(utf8_char, count, 1);
}
void piece_table::insert_string(std::string_view sv, signed_offset nchars){
  if(nchars == -1){
    nchars = utf8_strlen(sv);
  }
  this->do_insert(sv.data(), (offset_t)sv.size(), (offset_t)nchars);
}
static void delete_backwards_simple(piece_table *p, int N){
  piece_table::CharT *pt = p->get_point_text();
  piece_table::CharT *start = pt;
  for(signed_offset i = 0; i < N; i++){
    pt = utf8_prev_char_start(pt);
  }
  int count = start - pt;
  p->point.update(-count, -N);
  p->point.desc->piece.update(-count, -N);
  p->text_length.update(-count, -N);
  p->current_edit_length.update(count, N);
  return;  
}
static void delete_forwards_simple(piece_table *p, int N){
  piece_table::CharT *pt = p->get_point_text();
  piece_table::CharT *start = pt;
  for(signed_offset i = 0; i < N; i++){
    pt = utf8_next_char(pt);
  }
  int count = start - pt;
  //deleting forward, point doesn't move
  p->point.desc->piece.update(-count, -N);
  //We need to move the start of the span forwards
  p->point.desc->piece.start += count;
  p->text_length.update(-count, -N);
  p->current_edit_length.update(count, N);
  return;  
}
void piece_table::delete_char(signed_offset N){
  if(N < 0){
    return this->delete_char_backwards(-N);
  }
  if(!this->is_continuous_edit(edit_type::forward_deletion)){
    this->maybe_finish_edit();
    this->split_for_deletion();
    this->current_edit_type = edit_type::forward_deletion;
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
void piece_table::delete_char_backwards(signed_offset N){
  if(N < 0){
    return this->delete_char(-N);
  }
  if(!this->is_continuous_edit(edit_type::backward_deletion)){
    this->maybe_finish_edit();
    this->split_for_deletion();
    this->current_edit_type = edit_type::backward_deletion;
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
  edit_type type = this->current_edit_type_unsafe();
  offset count = abs(this->currently_editing);
  //Edit edit;
  //edit.type = type;
  if(type == edit_type::insertion){
    //Nothing for now
  } else if(type == edit_type::forward_deletion){  
    //Nothing for now
  } else {
    //Point is currently at the end of a dsecriptor, move it to the
    //start of the next so it is ponting to a valid character.
    this->point_to_next_desc();
  }
  this->current_edit_length.reset();
  this->current_edit_type = edit_type::none;
}
