#ifndef __TEXT_EDIT_H__
#define __TEXT_EDIT_H__
#include "util.h"
#include "unicode.h"
//An implementation of the piece-table data structure for
//use as the backbone of a text editor
/*
  I'm starting with a fairly simple implementation, one underlying
  buffer for both inital and appended text, storing spans as a simple
  linked list and not trying to do any complicated optimization.
  Currently all offsets are just byte positions, however the text
  is assumed to be utf8 encoded, so there is an opportunity for
  optimization here by saving character positions in addition to byte positions.
*/
namespace util {
struct piece_table {
  //typedefs
  struct Span;
  struct Descriptor;
  struct Point;
  struct Edit;
  struct Offset;
  using offset_t = uint32_t;
  using signed_offset_t = int64_t;
  using CharT = unsigned char;
  using codepoint_t = int32_t;
  using utf8_char_t = uint8_t[4];
  //Holds an offset into utf8 encoded text as both a byte offset and
  //a character offset.
  struct Offset {
    offset_t as_bytes = 0;
    offset_t as_chars = 0;
    void update(int nbytes, int nchars = 1){
      as_bytes += nbytes;
      as_chars += nchars;
    };
    void reset(){ as_bytes = as_chars = 0; }
    bool unibyte_only() const { return (as_bytes == as_chars); }
    //For comparisons I check agianst both values, but this is just for
    //consistancy, both values should represent the same logical offset.
    bool zerop() const {
      return (as_bytes == 0 && as_chars == 0);
    }
    //There really shouldn't be many if
    friend bool operator==(const Offset& lhs, const Offset& rhs){
      return lhs.as_bytes == rhs.as_bytes && lhs.as_chars == rhs.as_chars;
    }
    friend bool operator<(const Offset& lhs, const Offset& rhs){
      return lhs.as_bytes < rhs.as_bytes && lhs.as_chars < rhs.as_chars;
    }
    friend Offset& operator+=(Offset& lhs, const Offset& rhs){
      lhs.as_bytes += rhs.as_bytes;
      lhs.as_chars += rhs.as_chars;
      return lhs;
    }
    friend Offset& operator-=(Offset& lhs, const Offset& rhs){
      lhs.as_bytes -= rhs.as_bytes;
      lhs.as_chars -= rhs.as_chars;
      return lhs;
    }
    friend Offset operator+(const Offset& lhs, const Offset& rhs){
      Offset ret = lhs;
      ret += rhs;
      return ret;
    }
    friend Offset operator-(const Offset& lhs, const Offset& rhs){
      Offset ret = lhs;
      ret -= rhs;
      return ret;
    }
  };
  /// Substructures.
  // Holds information about a contigous sequence of text, the underlying
  // buffer/file, its offset and its length. The length is store as both
  // bytes and characters to make indexing by character faster.
  // Using pointers would cause all existing spans to be invalidated
  // whenever the buffer needed to be resized.
  struct Span {
    //which buffer/file this span points to, currently unused. For future
    //compatibility the buffer we are currently writing to will always be
    //buffer 0.
    int32_t buffer = 0;
    offset_t start = 0; //byte offset in the underlying buffer.
    Offset length = {0,0}; //length, in both bytes and characters
    void update(int nbytes, int nchars = 1){
      length.update(nbytes, nchars);
    }
    void update(Offset off){
      length += off;
    }
    Offset len() const { return length; }
    size_t size() const { return length.as_bytes; }
    bool unibyte_only() const { return length.unibyte_only(); }
  };
  //Abstract type that holds information about a span and
  //its location (i.e a linked list node, an array index, a node in a tree),
  //Currently it's a linked list node, but this may change.
  struct Descriptor {
    Span piece;
    Descriptor *prev_ptr = nullptr;
    Descriptor *next_ptr = nullptr;
    Descriptor* next() const { return next_ptr; }
    Descriptor* prev() const { return prev_ptr; }
    Descriptor*& next() { return next_ptr; }
    Descriptor*& prev() { return prev_ptr; }
    Offset len() const { return piece.len(); }
    size_t size() const { return piece.size(); }
    void update(int nbytes, int nchars = 1){
      piece.update(nbytes, nchars);
    }
    void update(Offset off){
      piece.update(off);
    }
  };
  //Holds information about the a specific text location, i.e which span it is
  //in, the offset within that span, and the location in the text overall.
  //It generally points to the start of a character, but can point at to
  //the end of a descriptor if it is at the end of the text or during an edit.
  struct Marker {
    Descriptor *desc;
    Offset span_offset = {0,0};
    Offset text_offset = {0,0};
    //Just updates the offsets by the given amounts, doesn't check to
    //make sure they are still valid.
    void update(int nbytes, int nchars = 1){
      span_offset.update(nbytes, nchars);
      text_offset.update(nbytes, nchars);
    };
    void update(Offset off){
      span_offset += off;
      text_offset += off;
    }
    void reset(){ span_offset.reset(); text_offset.reset(); }
    Offset span_end_offset() const {
      Offset ret = desc->len();
      ret -= span_offset;
      return ret;
    }
    //For the sake of symmetry
    Offset span_start_offset() const {
      return span_offset;
    }
    bool at_end_of_desc() const {
      return span_offset == desc->len();
    }
    bool at_start_of_desc() const {
      return span_offset.zerop();
    }
    friend bool operator==(const Marker& lhs, const Marker& rhs){
      return lhs.text_offset == rhs.text_offset;
    }
    friend bool operator<(const Marker& lhs, const Marker& rhs){
      return lhs.text_offset < rhs.text_offset;
    }
  };
  enum class edit_type : int8_t {
    none = 0,
    backward_deletion = 1,
    insertion = 2,
    forward_deletion = 3,
    //Could remove these in favor of just having an undo flag
    //I should probably do that.
    backward_deletion_undo = 4,
    insertion_undo = 5,
    forward_deletion_undo = 6
  };
  static constexpr edit_type_is_deletion(edit_type type){
    return (type == edit_type::backward_deletion ||
            type == edit_type::forward_deletion); // ||
    //This is a kind of deletion, but I don't think it should count.
    //type == edit_type::insertion_undo);
  }
  //structure representing a modification to the text, used to store undo
  //information. Multiple modifications at the same location are combined
  //into a single edit.
  struct Edit {
    //The descriptors used by Edits are seperate from the descriptors used
    //for the main text, they are just used to identify the text of the edit,
    //we still need to manually find the actual location using the text_offset.
    Marker location;
    Offset length;
    //Should never be set to edit_type::none
    edit_type type;
    //Pointer to the next edit (the edit before this one chronologically).
    Edit *next_ptr;
    Edit* next() const{ return next_ptr; }
    Edit*& next() { return next_ptr; }
  };
  //Fields
  //The memory for the actual text, new text is inserted at *current.
  CharT *buf = nullptr;
  CharT *end = nullptr;
  CharT *current = nullptr;
  Offset text_length = {0,0}; //Length of logical text.
  Marker point = {}; //Current position in logical text.
  Marker mark = {};
  Edit *current_edit;
  //Number of characters/bytes added/removed during the current edit.
  Offset _current_edit_length = {0,0};
  edit_type _current_edit_type = edit_type::none;
  //Functions to get current edit length and type, Since I'm not 100% sure
  //on how I'm going to ultimately implement these I've abstracted them into
  //functions returning a reference.
  Offset current_edit_length() const {
    return _current_edit_length;
    //return (current_edit ? current_edit->length : 0);
  }
  Offset& current_edit_length() {
    return _current_edit_length;
    //return (current_edit ? current_edit->length : 0);
  }
  edit_type current_edit_type() const {
    return _current_edit_type;
    //return (current_edit ? current_edit->type : edit_type::none);
  }
  edit_type& current_edit_type() {
    return _current_edit_type;
    //return (current_edit ? current_edit->type : edit_type::none);
  }
  //Should probably have a field:
  //Man I wish I had designated inintializers.
  //Edit current_edit = {{0}, {0,0}, edit_type::none};
  bool mark_active = false;
  Descriptor *table = nullptr;
  Descriptor *table_end = nullptr;
  mutable Descriptor *descriptor_freelist = nullptr;
  //I'd like to implement a history tree, but for now I'm just using
  //a simple undo/redo system. Except I'm not totally sure how
  //I want to implement redo, so its just undo for now.
  Edit *undo_history = nullptr;
  Edit *redo_history = nullptr;
  static constexpr size_t page_size = 4096;
  static constexpr size_t round_to_page_size(size_t sz){
    if(!(sz & (page_size - 1))){
      return (sz ? page_size : sz);
    } else {
      return (sz & ~(page_size -1)) + page_size;
    }
  }
  piece_table() = default;
  Offset len() const {
    return this->text_length;
  }
  size_t size() const {
    return this->text_length.as_bytes;
  }
  CharT* get_buf() const {
    return buf;
  }
  //Easy way of checking if we have only ascii characters.
  bool unibyte_only() const {
    return this->text_length.unibyte_only();
  }
  bool is_mark_active(){
    return mark_active;
  }
  void deactivate_mark(){
    mark_active = false;
  }
  bool currently_editing() const {
    return current_edit_type() != edit_type::none;
  }
  bool is_continuous_edit(edit_type type) const {
    return current_edit_type() == type;
  }
  CharT* get_span_text(const Span& sp) const {
    return buf + sp.start;
  }
  CharT* get_span_end(const Span& sp) const {
    return buf + sp.start + sp.length.as_bytes;
  }
  std::string_view get_span_sv(const Span& sp) const {
    return std::string_view((char*)get_span_text(sp), sp.size());
  }
  CharT* get_span_text_at_byte(const Span& sp, offset_t n) const {
    return get_span_text(sp) + n;
  }
  CharT* get_span_text_at_char(const Span& sp, offset_t n) const {
    CharT *str = get_span_text(sp);
    while(n--){
      str += utf8_char_size(*str);
    };
    return str;
  }
  CharT* get_descriptor_text(const Descriptor* desc) const {
    return get_span_text(desc->piece);
  }
  CharT* get_descriptor_end(const Descriptor* desc) const {
    return get_span_end(desc->piece);
  }
  //Should I create functions to get the a pointer to text
  //at the end of a marker's descriptor?
  CharT* get_marker_text(const Marker* mk) const {
    return get_descriptor_text(mk->desc) + mk->span_offset.as_bytes;
  }
  CharT* get_point_text() const {
    return get_marker_text(&this->point);
  }
  codepoint_t get_character_at_marker(const Marker* mk) const {
    return utf8_decode_char(get_marker_text(mk));
  }
  codepoint_t get_character_at_point() const {
    return utf8_decode_char(get_point_text());
  }
  bool at_end_of_text(const Marker* mk) const {
    return mk->text_offset == this->text_length;
  }
  bool at_end_of_text() const {
    return this->point.text_offset == this->text_length;
  }
  bool at_start_of_text(const Marker* mk) const {
    return mk->text_offset.zerop();
  }
  bool at_start_of_text() const {
    return this->point.text_offset.zerop();
  }
  Marker create_marker(Descriptor *desc, Offset span_offset, Offset text_offset) const {
    Marker ret;
    ret.desc = desc;
    ret.text_offset = text_offset;
    ret.span_offset = span_offset;
    return ret;
  }
  void set_point(Descriptor *desc, Offset span_offset, Offset text_offset){
    point = create_marker(desc, span_offset, text_offset);
  }
  void set_point(Marker mk){
    point = mk;
  }
  //functions to create markers for the start/end iterators.
  void marker_to_start_of_text(Marker *mk) const {
    mk->reset();
    mk->desc = table;
  }
  void marker_to_end_of_text(Marker *mk) const {
    mk->desc = table_end;
    mk->text_offset = text_length;
    mk->span_offset = mk->desc->len();
  }
  Marker create_marker_at_start_of_text() const {
    return create_marker(table, {0,0}, {0,0});
  }
  Marker create_marker_at_end_of_text() const {
    return create_marker(table_end,  table_end->len(), text_length);
  }
  void point_to_start_of_text() {
    point = create_marker_at_start_of_text();
  }
  void point_to_end_of_text() {
    point = create_marker_at_end_of_text();
  }
  Edit* alloc_edit() const {
    return (Edit*)calloc(sizeof(Edit), 1);
  }
  void free_edit(Edit *edit) const {
    free(edit);
  }
/*
  Edit* create_edit_at_point(edit_type type){
    Edit* ret = alloc_edit();
    ret->location = duplicate_point();
    ret->type = type;
    return ret;
  }
*/
  //Create a deep copy of a marker, i.e duplicates the underlying descriptor rather
  //than just copying a pointer to it.
  Marker duplicate_marker(const Marker *mk) const {
    return duplicate_marker(mk, alloc_descriptor());
  }
  //Same as above but uses 'mem' as the memory for the copy of the descriptor.
  Marker duplicate_marker(const Marker *mk, Descriptor *mem) const {
    memcpy(mem, mk->desc, sizeof(Descriptor));
    Marker ret = *mk;
    ret.desc = mem;
    return ret;
  }
  //Same as above but allocates a new descriptor.

  Marker duplicate_point() const {
    return duplicate_marker(point);
  }
  Marker duplicate_point(Descriptor *new_desc) const {
    return duplicate_marker(point, new_desc);
  }
  Descriptor* alloc_descriptor() const {
    if(descriptor_freelist){
      Descriptor *ret = descriptor_freelist;
      descriptor_freelist = descriptor_freelist->next();
      return ret;
    } else {
      return (Descriptor*)calloc(sizeof(Descriptor), 1);
    }
  }
  Descriptor* copy_descriptor(Descriptor* desc) const {
    Descriptor *new_desc = alloc_descriptor();
    memcpy(new_desc, desc, sizeof(Descriptor));
    return new_desc;
  }
  void free_descriptor(Descriptor *desc) const {
    memset(desc, '\0', sizeof(Descriptor));
    desc->next() = descriptor_freelist;
    descriptor_freelist = desc;
    return;
  }
  //Split a descriptor into two at the given offset. The Given descriptor
  //will point to the first (left) half of the split
  void split_descriptor(Descriptor *desc, Offset where){
    Descriptor *left = desc;
    Descriptor *right = copy_descriptor(left);
    left->next() = right;
    right->prev() = left;
    right->piece.start = where.as_bytes;
    right->piece.length = left->piece.length - where;
    left->piece.length = where;
  }
  //Removes the given descriptor from the list of descriptors,
  //but otherwise leaves it intact.
  Descriptor* unlink_descriptor(Descriptor *desc){
    if(desc->prev() == nullptr){
      this->table = desc->next();
    }
    desc->prev()->next() = desc->next();
    if(desc->next() == nullptr){
      this->table_end = desc->prev();
    }
    desc->next()->prev() = desc->prev();
    return desc;
  }
  void delete_descriptor(Descriptor *desc){
    unlink_descriptor(desc);
    free_descriptor(desc);
  }
  bool piece_table::delete_descriptor_for_edit();
  //Splits the descriptor 'pt' refers to at the location 'pt' refers to.
  //Doesn't change 'pt' so it is left pointing past the end of the left
  //descriptor.
  void split_at_marker(const Marker& pt){
    split_descriptor(pt.desc, pt.span_offset);
  }
  void split_at_marker(const Marker* pt){
    split_descriptor(pt->desc, pt->span_offset);
  }
  //I may write alternate versions of marker_to_next/prev_desc for
  //the case where we know the marker is at the end/begining of
  //the current descriptor, for optimization.
  bool marker_to_prev_desc(Marker *pt) const {
    if(pt->desc->prev() == nullptr){
      pt->reset();
      return false;
    }
    pt->desc = pt->desc->prev();
    pt->text_offset -= pt->span_offset;
    pt->span_offset = pt->desc->len();
    return true;
  }
  bool marker_to_next_desc(Marker *pt) const {
    if(pt->desc->next() == nullptr){
      pt->span_offset = pt->desc->len();
      pt->text_offset = this->text_length;
      return false;
    }
    //We do the arithmetic here in two steps to avoid possible temporaries
    pt->text_offset -= pt->span_offset;
    pt->text_offset += pt->desc->len();
    pt->desc = pt->desc->next();
    pt->span_offset.reset();
    return true;
  }
  bool point_to_prev_desc(){
    return marker_to_prev_desc(&point);
  }
  bool point_to_next_desc(){
    return marker_to_next_desc(&point);
  }
  /*
    Update descriptors to prepare for an edit of the given type at
    the location indicated by 'mk'. Returns a boolean indicating if the edit
    can be performed at the given location. For insertions always returns true,
    for deletions returns false if at the start/end of text for backward/forward
    deletions respectively.
    'mk' is left pointing to the end of a descriptor for backward deletions and
    insertions (though the descriptor may be empty for insertions) and at
    the start of a descriptor for forward deletions.

    Writing seperate functions for each type would likely be faster, but unless
    I need the speedup this is cleaner.
  */
  bool split_for_edit(edit_type type, Marker *mk);
  bool split_for_edit(edit_type type){
    return split_for_edit(type, &this->point);
  }
  //Enlarges the underlying buffer (or allocates it in the first place)
  void resize(){
    size_t offset = current - buf;
    size_t sz = end - buf;
    size_t new_sz = std::max(sz*2, page_size);
    buf = (CharT*)realloc(buf, new_sz);
    current = buf + offset;
    end = buf + new_sz;
  }
  //Ensures the underlying buffer can hold 'count' more bytes, by
  //resizing it if necessary.
  void maybe_resize(int count){
    if(!buf || ((current + count) < end)){
      resize();
    }
  }
  //Functions for moving a marker forward/backward one character,
  //possibly premature optimization, but useful for iterators.
  bool marker_to_next_char(Marker* mk) const {
    CharT *txt = get_marker_text(mk);
    int count = utf8_char_size(*txt);
    if((txt + count) >= get_descriptor_end(mk->desc)){
      return marker_to_next_desc(mk);
    } else {
      mk->update(count);
      return true;
    }
  }
  bool point_to_next_char(){
    return marker_to_next_char(&point);
  }
  bool marker_to_prev_char(Marker *mk) const {
    CharT *txt = get_marker_text(mk);
    if(txt <= get_descriptor_text(mk->desc)){
      return marker_to_prev_desc(mk);
    } else {
      mk->update(txt - utf8_prev_char_start(txt));
      return true;
    }
  }
  bool point_to_prev_char(){
    return marker_to_prev_char(&point);
  }
  bool next_char(){
    if(at_end_of_text()){ return false; }
    maybe_finish_edit();
    point_to_next_char();
  }
  bool prev_char(){
    if(at_start_of_text()){ return false; }
    maybe_finish_edit();
    point_to_prev_char();
  }
  //Move point forward N characters, (backward if N is negative),
  //Returns false if the beginning / end of the text was reached
  //before N characters, otherwise returns true.
  //(Normally I avoid non const reference arguments, but the versions
  // which take an explicit marker argument are for internal use only.)
  bool forward_char(Marker* mk, signed_offset_t N = 1);
  bool forward_char(signed_offset_t N = 1){
    return forward_char(&this->point, N);
  }
  bool backward_char(Marker* mk, signed_offset_t N = 1);
  bool backward_char(signed_offset_t N = 1){
    return backward_char(&this->point, N);
  }
  //Move the given marker 'mk' to the Nth charcater, if
  //there are fewer than N charcters return false and 'mk' is unchanged.
  //May be more efficent than forward/backward char if N is close to point.
  bool goto_char(Marker *mk, offset_t N);
  //Move point to the Nth character, just calls forward/backward char so it
  //may not be the fastest way to get to that character.
  bool point_to_char(offset_t N){
    signed_offset_t pt_char = this->point->text_offset.as_chars;
    signed_offset_t diff = (signed_offset_t)N - pt_char;
    forward_char(diff);
  }

  //Core insert function, makes sure there is enough space and possibly splits
  //descriptors then inserts the given bytes and performs necessary updates.
  void do_insert(const CharT *bytes, offset_t nbytes, offset_t nchars);
  //Insert the character represented by the given codepoint / utf8 character
  //(depending on the overload) into the text at the current point. Dosen't
  //check that its input is valid unicode.
  void insert_char(const CharT *utf8_char);
  void insert_char(const char *utf8_char){
    return insert_char((CharT*)utf8_char);
  }
  void insert_char(int32_t codepoint){
    CharT buf[utf8_max_char_size];
    utf8_encode_char(codepoint, buf);
    return insert_char(buf);
  }
  //Insert a sequence of characters at point
  void insert_string(std::string_view sv, signed_offset_t nchars = -1);
  void insert_string(std::u32string_view sv);
  //TODO: Formatted text insertion.
  //Delete the following / previous N characters (depending on if delete
  //or delete backwards is called), the direction is reversed if the
  //sign is flipped. Returns false if there are fewer than N characters
  //to delete in the given direction (but will still delete
  //as many characters as possible).
  bool delete_char(signed_offset_t N = 1);
  bool delete_char_backwards(signed_offset_t N = 1);


  //Start making an edit of the given type, allocates a new Edit structure
  //and saves some necessary information. Should be called after any
  //descriptors that need to be split have been.
  void begin_edit(edit_type type);
  //Finish the current edit by modifying any descriptors that need
  //modification, setting currently_editing to 0 and updating the
  //undo list with the edit.
  void finish_edit();
  void maybe_finish_edit(){
    if(currently_editing()){
      finish_edit();
    }
  }
  //Applies the given edit 'edit' (either redoing or undoing something),
  //modifies 'edit' so that when applied it will revert the edit we just applied
  //and returns a pointer to the modified edit structure.
  //Put simply apply_edit(apply_edit(edit)) will leave the text unchanged.
  Edit* apply_and_update_edit(Edit *edit);
  //Same as above but doesn't modify the given edit, instead returns a newly
  //allocaed edit structure.
  Edit* apply_and_preserve_edit(const Edit *edit){
    Edit *new_edit = alloc_edit();
    memcpy(new_edit, edit, sizeof(Edit));
    return apply_edit(new_edit);
  }
  //Undo the last edit & move it to the redo list
  //Returns false if there are no edits to undo.
  bool undo_last_edit(){
    if(undo_history == nullptr){ return false; }
    Edit *edit = undo_history;
    undo_history = edit->next();


  int undo_edit(int count){
    for(int i = 0; i < count; i++){
      if(undo_history == nullptr){
        return i;
      } else {
        undo_edit();
      }
    }
    return count;
  }
  void redo_edit(int count = 1);
  //Copies the logical text into a newly allocated null terminated string
  //and returns its length.
  size_t to_string(CharT **ptr);
  size_t to_string(char **ptr){
    return to_string((CharT**)ptr);
  }
  //Copies the logical text into a std::string.
  std::string to_string();

  //Synchronizes the logical and actual text, This destroys the edit
  //history and copies the logical text into a new buffer.
  void flush();
  //An iterator over the characters (codepoints) of the text.
  //Consists of a pointer to the table (so we can access the actual text),
  //a marker (the location) and a cached version of the current character.
  struct codepoint_iterator {
    using pointer =  const codepoint_t*;
    using reference = const codepoint_t&;
    using value_type = codepoint_t;
    using size_type = offset_t;
    using difference_type = ptrdiff_t;
    using iterator_category = std::bidirectional_iterator_tag;

    const piece_table *ptab = nullptr;
    Marker mk = {};
    mutable codepoint_t c = 0;

    codepoint_iterator() {}
    codepoint_iterator(const piece_table* ptab)
      : ptab{ptab}, mk{ptab->create_marker_at_start_of_text()} {}
    codepoint_iterator(const piece_table* ptab, Descriptor *desc,
                       Offset span_offset, Offset text_offset)
      : ptab{ptab}, mk{desc, span_offset, text_offset} {}
    codepoint_iterator(const piece_table* ptab, Marker m)
      : ptab{ptab}, mk{m} {}
    int32_t get_char() const {
      if(c == 0){
        c = ptab->get_character_at_marker(mk);
      }
      return c;
    }
    Marker to_marker(){
      return mk;
    }
    void advance(){
      ptab->marker_to_next_char(&mk);
    }
    void reverse(){
      ptab->marker_to_next_char(&mk);
      //Its fine to compute the value here since decrementing the begin iterator
      //is already undefined behavior.
      c = ptab->get_character_at_marker(mk);
    }

    char32_t operator*() const {
      return get_char();
    }
    codepoint_iterator& operator++(){//pre-increment
      advance();
      return *this;
    }
    codepoint_iterator operator++(int){//post-increment
      get_char();//insure char is computed before copying to avoid doing it 2x
      auto ret = *this;
      advance();
      return ret;
    }
    codepoint_iterator& operator--(){//pre-decrement
      reverse();
      return *this;
    }
    codepoint_iterator operator--(int){//post-decrement
      //Don't call get_char, this could be an end iterator.
      auto ret = *this;
      reverse();
      return ret;
    }
    //Equality needs to be defined so that it ignores the cached character.
    bool operator==(const codepoint_iterator& rhs) const {
      return mk == rhs.mk;
    }
    bool operator!=(const codepoint_iterator& rhs) const {
      return !(mk == rhs.mk);
    }
  };
};
}
#endif /* __TEXT_EDIT_H__ */

/* Local Variables: */
/* mode: c++ */
/* End: */
#if 0
template<bool utf8>
struct iterator {
  using value_type = std::conditonal_t<use_utf8, codepoint_t, utf8_char>;
  using pointer =  const value_type*;
  using reference = const value_type&;
  using size_type = offset_t;
  using difference_type = ptrdiff_t;
  using iterator_category = std::bidirectional_iterator_tag;

  const piece_table *ptab = nullptr;
  Marker mk = {};
  mutable codepoint_t c = 0;

  iterator() {}
  iterator(const piece_table* ptab)
    : ptab{ptab}, mk{ptab->create_marker_at_start_of_text()} {}
  iterator(const piece_table* ptab, Descriptor *desc,
                     Offset span_offset, Offset text_offset)
    : ptab{ptab}, mk{desc, span_offset, text_offset} {}
  iterator(const piece_table* ptab, Marker m)
    : ptab{ptab}, mk{m} {}
  int32_t get_codepoint() const {
    if(c == 0){
      c = ptab->get_character_at_marker(mk);
    }
    return c;
  }
  int32_t get_codepoint() const {
    if(c == 0){
      c = ptab->get_character_at_marker(mk);
    }
    return c;
  }
  Marker to_marker(){
    return mk;
  }
  void advance(){
    ptab->marker_to_next_char(&mk);
  }
  void reverse(){
    ptab->marker_to_next_char(&mk);
    //Its fine to compute the value here since decrementing the begin iterator
    //is already undefined behavior.
    c = ptab->get_character_at_marker(mk);
  }

  char32_t operator*() const {
    return get_char();
  }
  codepoint_iterator& operator++(){//pre-increment
    advance();
    return *this;
  }
  codepoint_iterator operator++(int){//post-increment
    get_char();//insure char is computed before copying to avoid doing it 2x
    auto ret = *this;
    advance();
    return ret;
  }
  codepoint_iterator& operator--(){//pre-decrement
    reverse();
    return *this;
  }
  codepoint_iterator operator--(int){//post-decrement
    //Don't call get_char, this could be an end iterator.
    auto ret = *this;
    reverse();
    return ret;
  }
  //Equality needs to be defined so that it ignores the cached character.
  bool operator==(const codepoint_iterator& rhs) const {
    return mk == rhs.mk;
  }
  bool operator!=(const codepoint_iterator& rhs) const {
    return !(mk == rhs.mk);
  }
};
#endif
