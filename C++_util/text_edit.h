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
namespace text_edit {
//Holds an offset into utf8 encoded text as both a byte offset and
//a character offset.
struct Offset { //8 bytes
  uint32_t as_bytes = 0;
  uint32_t as_chars = 0;
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
  size_t size() const { return as_bytes; }
  //unary minus, keep in mind offsets are technically unsigned.
  friend Offset& operator-(Offset &off){
    off.as_bytes = -off.as_bytes;
    off.as_chars = -off.as_chars;
    return off;
  }
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
static constexpr Offset EOF_Offset = {-1U,-1U};
struct piece_table {
  //typedefs
  struct Span;
  struct Descriptor;
  struct Marker;
  struct Edit;
  using offset_t = uint32_t;
  using signed_offset_t = int64_t;
  using CharT = unsigned char;
  using codepoint_t = int32_t;
  using utf8_char_t = uint8_t[4];

  /// Substructures.
  // Holds information about a contigous sequence of text, the underlying
  // buffer/file, its offset and its length. The length is store as both
  // bytes and characters to make indexing by character faster.
  // Using pointers would cause all existing spans to be invalidated
  // whenever the buffer needed to be resized.
  struct Span {//16 bytes (only really needs 12 bytes + 1 bit)
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
  struct Descriptor {//32 bytes (16 bytes + 2 pointers)
    Descriptor *next_ptr = nullptr;
    Descriptor *prev_ptr = nullptr;
    Span piece;

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
    bool unibyte_only() const { return piece.unibyte_only(); }
  };
  //Holds information about the a specific text location, i.e which span it is
  //in, the offset within that span, and the location in the text overall.
  //It generally points to the start of a character, but can point at to
  //the end of a descriptor if it is at the end of the text or during an edit.
  //If pointing at the end of a descriptor not at the end of text or during
  //an edit the marker is considered invalid.
  struct Marker { //24 bytes (16 bytes + 1 pointer
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
    offset_t span_end_byte_offset() const {
      return desc->size() - span_offset.size();
    }
    //For the sake of symmetry
    Offset span_start_offset() const {
      return span_offset;
    }
    offset_t span_start_byte_offset() const {
      return span_offset.size();
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
  static constexpr bool edit_type_is_deletion(edit_type type){
    return (type == edit_type::backward_deletion ||
            type == edit_type::forward_deletion); // ||
    //This is a kind of deletion, but I don't think it should count.
    //type == edit_type::insertion_undo);
  }
  //structure representing a modification to the text, used to store undo
  //information. Multiple modifications at the same location are considered
  //as a single edit.
  struct Edit {//48 bytes (41 bytes, presumably packed to 48)
    //Pointer to the next edit (the edit before this one chronologically).
    //TODO: Maybe store the edit type in the low order bits.
    Edit *next_ptr;
    //The descriptors used by Edits are not shared with the descriptors
    //used for text, but they point to the text descriptors so it's easy
    //to link them back in.
    Marker location;
    Offset length;
    //Should never be set to edit_type::none
    edit_type type;

    Edit* next() const{ return next_ptr; }
    Edit*& next() { return next_ptr; }
  };
  //I need to allocate/free a lot of Edits and Descriptors so I'm using a freelist
  //To help speed that up. I currently using a single freelist, this means that
  ///each Descriptor will be larger than necessary, but I'm hoping/thinking that
  //the speedup/simplicity of having a single freelist will be worth it.
  static constexpr size_t freelist_block_size = std::max(sizeof(Edit), sizeof(Descriptor));
  struct Freelist_Block {
    Freelist_Block *next;
    char mem[freelist_block_size - sizeof(Freelist_Block*)];
  };
  static void zero_freelist_block(Freelist_Block *blk){
    memset(blk->mem, '\0', freelist_block_size - sizeof(Freelist_Block*));
  }
  //Fields
  //TODO: add fields for original text / file
  //The memory for the actual text, new text is inserted at *current.
  CharT *buf = nullptr;
  CharT *end = nullptr;
  CharT *current = nullptr;
  Offset text_length = {0,0}; //Length of logical text.
  Marker point = {}; //Current position in logical text.
  Marker mark = {};
  Edit *current_edit = nullptr;
  Descriptor *table = nullptr;
  Descriptor *table_end = nullptr;
  //I'd like to implement a history tree, but for now I'm just using
  //a simple undo/redo system.
  Edit *undo_history = nullptr;
  Edit *redo_history = nullptr;
  //This is mutable since it could be removed and replaced with
  //regular memory allocation without any visable changes.
  mutable Freelist_Block *freelist = nullptr;
  //TODO: incorporate this into a more general flags field
  bool mark_active = false;
  static constexpr size_t page_size = 4096;
  static constexpr size_t round_to_page_size(size_t sz){
    if(!(sz & (page_size - 1))){
      return (sz ? page_size : sz);
    } else {
      return (sz & ~(page_size -1)) + page_size;
    }
  }
  piece_table() = default;
  //piece_table(const char *filename);
  //piece_table(FILE*);
  //piece_table(char*,size_t); //maybe
  Offset len() const {
    return this->text_length;
  }
  size_t size() const {
    return this->text_length.as_bytes;
  }
  CharT* get_buf() const {
    return buf;
  }
  Offset current_edit_length() const {
    return (current_edit ? current_edit->length : 0);
  }
  void update_current_edit_length(offset_t nbytes, offset_t nchars){
    current_edit->length.update(nbytes, nchars);
  }
  edit_type current_edit_type() const {
    return (current_edit ? current_edit->type : edit_type::none);
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
  //These aren't as useful as you would think, but it's still worth having them.
  std::string_view get_descriptor_sv(const Descriptor *desc){
    return std::string_view(get_descriptor_text(desc), desc->size());
  }
  //Should I create functions to get the a pointer to text
  //at the end of a marker's descriptor?
  CharT* get_marker_text(const Marker* mk) const {
    return get_descriptor_text(mk->desc) + mk->span_offset.as_bytes;
  }
  //I like this name better, but I use the previous one a bunch
  //so I'll need to change a bunch of code before I can totally remove it.
  CharT* get_text_at_marker(const Marker* mk) const {
    return get_marker_text(mk);
  }
  CharT* get_point_text() const {
    return get_marker_text(&this->point);
  }
  CharT* get_text_at_point() const {
    return get_text_at_marker(&this->point);
  }
  codepoint_t get_codepoint_at_marker(const Marker* mk) const {
    return utf8_decode_char(get_marker_text(mk));
  }
  codepoint_t get_codepoint_at_point() const {
    return utf8_decode_char(get_point_text());
  }
  utf8_char get_utf8_character_at_marker(const Marker* mk) const {
    return utf8_char(get_marker_text(mk));
  }
  utf8_char get_utf8_character_at_point() const {
    return utf8_char(get_point_text());
  }
  uint8_t get_byte_at_marker(const Marker* mk){
    return *get_marker_text();
  }
  uint8_t get_byte_at_point(){
    return *get_point_text();
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
  Marker create_marker_at_start_of_text() const {
    return create_marker(table, {0,0}, {0,0});
  }
  Marker create_marker_at_end_of_text() const {
    return create_marker(table_end,  table_end->len(), text_length);
  }
  //functions to create markers for the start/end iterators.
  void marker_to_start_of_text(Marker *mk) const {
    *mk = create_marker_at_start_of_text();
  }
  void marker_to_end_of_text(Marker *mk) const {
    *mk = create_marker_at_end_of_text();
  }
  void point_to_start_of_text() {
    point = create_marker_at_start_of_text();
  }
  void point_to_end_of_text() {
    point = create_marker_at_end_of_text();
  }
  void* get_memory_block() const {
    if(freelist){
      void *ret = freelist;
      freelist = freelist->next;
      return ret;
    } else {
      return calloc(sizeof(Freelist_Block), 1);
    }
  }
  void free_memory_block(void* mem) const {
    Freelist_Block *blk = static_cast<Freelist_Block*>(mem);
    zero_freelist_block(blk);
    blk->next = freelist;
    freelist = blk;
    return;
  }
  void free_memory_block_list(void* mem) const {
    Freelist_Block *head = static_cast<Freelist_Block*>(mem);
    Freelist_Block *tail = head;
    while(tail->next){
      zero_freelist_block(tail); //Doesn't touch 'next' field.
      tail = tail->next;
    }
    tail->next = freelist;
    freelist = head;
    return;
  }
  Edit* alloc_edit() const {
    return static_cast<Edit*>(get_memory_block());
    //    return (Edit*)calloc(sizeof(Edit), 1);
  }
  void free_edit(Edit *edit) const {
    free(edit);
  }
  Descriptor* alloc_descriptor() const {
    return static_cast<Descriptor*>(get_memory_block());
    /*if(descriptor_freelist){
      Descriptor *ret = descriptor_freelist;
      descriptor_freelist = descriptor_freelist->next();
      return ret;
    } else {
      return (Descriptor*)calloc(sizeof(Descriptor), 1);
    }*/
  }
  void free_descriptor(Descriptor *desc) const {
    free_memory_block(desc);
    /*
      memset(desc, '\0', sizeof(Descriptor));
      desc->next() = descriptor_freelist;
      descriptor_freelist = desc;
      return;
    */
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
  Descriptor* copy_descriptor(Descriptor* desc) const {
    Descriptor *new_desc = alloc_descriptor();
    memcpy(new_desc, desc, sizeof(Descriptor));
    return new_desc;
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
  bool delete_descriptor_for_edit();
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
  //`unsafe` versions of marker_to_prev/next_desc which assume
  //the marker is located at the start/end of a descriptor,
  //but don't check this. They are faster since they don't need to update
  //the text_offset.
  bool marker_to_prev_desc_unsafe(Marker *mk) const {
    if(mk->desc->prev() == nullptr){
      return false;
    } else {
      mk->desc = mk->desc->prev();
      mk->span_offset = mk->desc->len();
      return true;
    }
  }
  bool marker_to_next_desc_unsafe(Marker *mk) const {
    if(mk->desc->next() == nullptr){
      return false;
    } else {
      mk->desc = mk->desc->next();
      mk->span_offset.reset();
      return true;
    }
  }
  bool point_to_prev_desc(){
    return marker_to_prev_desc(&point);
  }
  bool point_to_next_desc(){
    return marker_to_next_desc(&point);
  }
  bool point_to_prev_desc_unsafe(){
    return marker_to_prev_desc_unsafe(&point);
  }
  bool point_to_next_desc_unsafe(){
    return marker_to_next_desc_unsafe(&point);
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
    if(at_end_of_text(mk)){ return false; }
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
  bool find_byte_forward(Marker *mk, uint8_t byte);

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
  Edit* apply_edit(Edit *edit);
  //Same as above but doesn't modify the given edit, instead returns a newly
  //allocaed edit structure.
  Edit* apply_and_preserve_edit(const Edit *edit){
    Edit *new_edit = alloc_edit();
    memcpy(new_edit, edit, sizeof(Edit));
    return apply_edit(new_edit);
  }
  //functions to undo or redo edits, since the only difference between
  //undo and redo are the names of the variables.

  //Undo/redo the last edit & move it to the redo/undo list
  //Returns false if there are no edits to undo/redo.
  bool apply_edit_and_update_history(Edit *&from, Edit *&to){
    if(from == nullptr){ return false; }
    Edit *edit = from;
    from = edit->next();
    edit = apply_edit(edit);
    edit->next() = to;
    to = edit;
    return true;
  }
  //Undo/redo count edits, returns number of edits actually undone/redone.
  int apply_edits_and_update_history(int count, Edit *&from, Edit *&to){
    for(int i = 0; i < count; i++){
      if(from == nullptr){
        return i;
      } else {
        apply_edit_and_update_history(count, from, to);
      }
    }
    return count;
  }

  bool undo_edit(){
    return apply_edit_and_update_history(undo_history, redo_history);
  }
  bool redo_edit(){
    return apply_edit_and_update_history(redo_history, undo_history);
  }
  int undo_edit(int count){
    return apply_edits_and_update_history(count, undo_history, redo_history);
  }
  int redo_edit(int count){
    return apply_edits_and_update_history(count, redo_history, undo_history);
  }
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
  template<bool use_utf8>
    struct iterator {
    using value_type = std::conditional_t<use_utf8, codepoint_t, utf8_char>;
    using pointer =  const value_type*;
    using reference = const value_type&;
    using size_type = offset_t;
    using difference_type = ptrdiff_t;
    using iterator_category = std::bidirectional_iterator_tag;

    const piece_table *ptab = nullptr;
    Marker mk = {};
    //Using {0} will correctly initalize both possible types.
    mutable value_type c = {0};

    iterator() {}
    iterator(const piece_table* ptab)
      : ptab{ptab}, mk{ptab->create_marker_at_start_of_text()} {}
    iterator(const piece_table* ptab, Descriptor *desc,
             Offset span_offset, Offset text_offset)
      : ptab{ptab}, mk{desc, span_offset, text_offset} {}
    iterator(const piece_table* ptab, Marker m)
      : ptab{ptab}, mk{m} {}

    bool is_zero(value_type x){
      if constexpr(use_utf8){
        return *x.data();
      } else {
        return x;
      }
    }
    value_type get_value() const {
      if(is_zero(c)){
        utf8_char ch = ptab->get_utf8_character_at_marker(mk);
        if constexpr(use_utf8){
          c = ch;
        } else {
          c = ch.codepoint();
        }
      }
      return c;
    }
    int32_t get_codepoint() const {
      get_value();
      if constexpr(use_utf8){
        return c.codepoint();
      } else {
        return c;
      }
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
      get_value();
    }
    iterator<true> to_utf8_iterator(){
      if constexpr(use_utf8){
        return *this;
      } else {
        return *(::new(this) iterator<true>(ptab, mk));
      }
    }
    iterator<false> to_codepoint_iterator(){
      if constexpr(use_utf8){
        return *(::new(this) iterator<false>(ptab, mk));
      } else {
        return *this;
      }
    }
    value_type operator*() const {
      return get_value();
    }
    iterator& operator++(){//pre-increment
      advance();
      return *this;
    }
    iterator operator++(int){//post-increment
      get_value();//insure char is computed before copying to avoid doing it 2x
      auto ret = *this;
      advance();
      return ret;
    }
    iterator& operator--(){//pre-decrement
      reverse();
      return *this;
    }
    iterator operator--(int){//post-decrement
      //Don't call get_value, this could be an end iterator.
      auto ret = *this;
      reverse();
      return ret;
    }
    //Equality needs to be defined so that it ignores the cached character.
    bool operator==(const iterator& rhs) const {
      return mk == rhs.mk;
    }
    bool operator!=(const iterator& rhs) const {
      return !(mk == rhs.mk);
    }
    //Comparison is independent of the value type, I may change this.
    template<bool x>
    bool operator==(const iterator<x>& rhs) const {
      return mk == rhs.mk;
    }
    template<bool x>
    bool operator!=(const iterator<x>& rhs) const {
      return mk != rhs.mk;
    }
  };
  using utf8_iterator = iterator<true>;
  using codepoint_iterator = iterator<false>;
#if 0
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
#endif
};
struct gap_buffer {
  struct Marker;
  struct Edit;
  using offset_t = uint32_t;
  using signed_offset_t = int64_t;
  using CharT = unsigned char;
  using codepoint_t = int32_t;
  using utf8_char_t = uint8_t[4];

  CharT *buf;
  Offset gap_position;
  Offset gap_end;
  constexpr offset_t gap_size(){
    return gap_end.size() - gap_position.size();
  }
  Offset buf_size;

  
}
} //namespace text_edit
using text_edit::piece_table;
using text_edit::gap_buffer;
} //namespace util  
#endif /* __TEXT_EDIT_H__ */

/* Local Variables: */
/* mode: c++ */
/* End: */
