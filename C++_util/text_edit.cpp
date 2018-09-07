#include "text_edit.h"
namespace util {
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
void piece_table::split_for_deletion(Marker *pt){
  if(this->at_end_of_text(pt) || this->at_start_of_text(pt)){
    return;
  }
  if(pt->span_offset.zerop()){
    pt->desc = pt->desc->next();
    pt->span_offset = pt->desc->len();
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
  assert(pt->desc->piece.length.zerop());
  pt->desc->piece.start = this->current - this->buf;
  pt->desc->piece.buffer = 0;
  return;
}
void piece_table::begin_edit(edit_type type){
  this->current_edit_type() = type;
}  
//   if(type == edit_type::insertion){
//     }
// }
void piece_table::do_insert(const CharT *bytes, offset_t nbytes,
                            offset_t nchars){
  this->maybe_resize(nbytes);
  if(!this->is_continuous_edit(edit_type::insertion)){
    this->maybe_finish_edit();
    this->split_for_insertion();
    this->begin_edit(edit_type::insertion);
  }
  this->current = (CharT*)mempcpy(this->current, bytes, nbytes);
  this->text_length.update(nbytes, nchars);
  this->point.update(nbytes, nchars);
  this->point.desc->piece.update(nbytes, nchars);
  this->current_edit_length().update(nbytes, nchars);
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
  p->current_edit_length().update(count, N);
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
  p->current_edit_length().update(count, N);
  return;
}
bool piece_table::delete_char(signed_offset_t N){
  if(N < 0){
    return this->delete_char_backwards(-N);
  }
  if(!this->is_continuous_edit(edit_type::forward_deletion)){
    this->maybe_finish_edit();
    this->split_for_deletion();
    this->current_edit_type() = edit_type::forward_deletion;
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
    this->split_for_deletion();
    this->current_edit_type() = edit_type::backward_deletion;
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
  edit_type type = this->current_edit_type();
  Offset count = this->current_edit_length();
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
  this->current_edit_length().reset();
  this->current_edit_type() = edit_type::none;
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
