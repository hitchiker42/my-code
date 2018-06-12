#ifndef __BITSET_H__
#define __BITSET_H__
namespace util {
/*
  Fixed size but dynamically allocated bitset. It shouldn't be too hard
  to extend this to a true dynamic bitset if needed.

  Most of the code is based on a combination of boost::dynamic_bitset and
  the stdlibc++ bitset code.
*/
//template <class Allocator = std::allocator<uintptr_t>>
struct bitset {
  typedef uintptr_t word_type;
  typedef size_t size_type;
  static constexpr word_type offset_mask = (sizeof(word_type) == 8 ? 0x3f : 0x1f);
  static constexpr word_type bits_per_word = sizeof(word_type) * CHAR_BIT;
  word_type *mem;
  size_type nbits;
  //Not very useful
  bitset() : mem{nullptr}, nbits{0} {};
  bitset(size_t nbits)
    : mem{(word_type*)calloc(get_nwords(nbits), sizeof(word_type))},
      nbits{nbits} {};
  bitset(const bitset &other)
    : mem{(word_type*)malloc(get_nbytes(other.nbits))}, nbits{other.nbits} {
    memcpy(mem, other.mem, get_nbytes(nbits));
  }
  bitset(const bitset &&other)
    : mem{other.mem}, nbits{other.nbits} {
    other.mem = nullptr;
    other.nbits = 0;
  }
  // A proxy to simulate lvalues of bit type.
  struct reference {
    word_type *loc;
    word_type mask;
    reference(bitset &bs, size_type pos)
      : loc(bs.get_word_ptr(pos)), mask(bs.get_bit_mask(pos)) {}
    reference(bitset *bs, size_type pos)
      : loc(bs->get_word_ptr(pos)), mask(bs->get_bit_mask(pos)) {}
    reference(const reference &other) = default;
    // Explicitly declare address of operator as undefined since it doesn't
    // do what you would expect.
    void operator&();
    operator bool() const { return ((*loc) & mask) != 0; }
    bool operator~() const { return ((*loc) & mask) == 0; }
    reference& flip() { do_flip(); return *this; }
    reference& operator=(bool x){ do_assign(x);   return *this; } // for b[i] = x
    reference& operator=(const reference& rhs) { do_assign(rhs); return *this; } // for b[i] = b[j]
    reference& operator|=(bool x) { if  (x) do_set();   return *this; }
    reference& operator&=(bool x) { if (!x) do_reset(); return *this; }
    reference& operator^=(bool x) { if  (x) do_flip();  return *this; }
    reference& operator-=(bool x) { if  (x) do_reset(); return *this; }
    void do_set() { (*loc) |= mask; }
    void do_reset() { (*loc) &= ~mask; }
    void do_flip() { (*loc) ^= mask; }
    void do_assign(bool x) { x ? do_set() : do_reset(); }
  };
  typedef bool const_reference;
  //helper functions to convert indexes into word / bit positions.
  static constexpr size_type get_word_idx(size_type idx) noexcept {
    return (idx / bits_per_word);
  }
  static constexpr size_type get_bit_pos(size_type idx) noexcept {
    return (idx & offset_mask);
  }
  static constexpr word_type get_bit_mask(size_type idx) noexcept {
    return word_type(1) << get_bit_pos(idx);
  }
  //Returns the number of bytes needed to represent nbits, will always
  //be a multiple of sizeof(word_type).
  static constexpr size_type get_nbytes(size_type nbits) noexcept {
    return get_nwords(nbits) * sizeof(word_type);
  }
  static constexpr size_type get_nwords(size_type nbits) noexcept {
    return ((nbits / bits_per_word) + (nbits & offset_mask ? 1 : 0));
  }
  //returns the number of bits in the last word that are unused
  static constexpr size_type get_extrabits(size_type nbits) noexcept {
    return ((get_nwords(nbits) - bits_per_word) - nbits);
  }
  //Returns a word that will mask off the unused bits in the high word
  static constexpr word_type get_word_mask(size_type nbits){
    return (~word_type(0) >> ((get_nwords(nbits) * bits_per_word) - nbits);

  word_type& get_word_ref(size_type idx) noexcept {
    return mem[get_word_idx(idx)];
  }
  word_type* get_word_ptr(size_type idx) noexcept {
    return mem + get_word_idx(idx);
  }
  //Returns the last word, this is useful since we often need to treat the
  //last word specially since not all the bits in it are necessarly meaningful
  word_type& get_high_word() noexcept {
    return mem[get_nwords(nbits)-1];
  }
  const_reference get(size_type idx) const {
    return get_word_ref(idx) & get_bit_mask(idx);
  }
  reference get(size_type idx) {
    return reference(this, idx);
  }
  //all of the do_XX  functions are borrowed from the stdlibc++ bitset implementation.
  void do_and(const bitset& x) noexcept {
    for (size_type i = 0; i < get_nwords(nbits); i++){
      mem[i] &= x.mem[i];
    }
  }

  void do_or(const bitset& x) noexcept {
    for (size_type i = 0; i < get_nwords(nbits); i++) {
      mem[i] |= x.mem[i];
    }
  }

  void do_xor(const bitset& x) noexcept {
    for (size_type i = 0; i < get_nwords(nbits); i++) {
      mem[i] ^= x.mem[i];
    }
  }
  void do_left_shift(size_type shift) noexcept;
  void do_right_shift(size_type shift) noexcept;

  void do_flip() noexcept {
    for (size_type i = 0; i < get_nwords(nbits); i++){
      mem[i] = ~mem[i];
    }
  }

  void do_set() noexcept {
    memset(mem, 0xff, get_nbytes(nbits));
  }
  void do_reset() noexcept {
    memset(w, 0, get_nbytes(nbits));
  }

  bool is_equal(const bitset& x) const noexcept {
    for (size_type i = 0; i < get_nwords(nbits); ++i) {
      if (mem[i] != x.mem[i]) {
        return false;
      }
    }
    return true;
  }
  bool are_all() const noexcept {
    for (size_type i = 0; i < get_nwords(nbits) - 1; i++) {
      if (mem[i] != ~word_type(0)) {
        return false;
      }
    }
    return (get_high_word() ==
            (~word_type(0) >> ((get_nwords(nbits) * bits_per_word) - _Nb)));
  }

  bool is_any() const noexcept {
    for (size_type i = 0; i < get_nwords(nbits); i++){
      if (mem[i] != static_cast<_word_type>(0)){
        return true;
      }
    }
    return false;
  }
  size_type do_count() const noexcept {
    size_type result = 0;
    for (size_type i = 0; i < get_nwords(nbits); i++) {
      //popcount is defined in util.h
      result += popcount(mem[i]);
    }
    return result;
  }
  // find first "on" bit
  size_type
  do_find_first(size_type) const noexcept;

  // find the next "on" bit that follows "prev"
  size_type
  do_find_next(size_type, size_type) const noexcept;
  bitset& operator&=(const bitset& __rhs) noexcept {
    this->do_and(__rhs);
    return *this;
  }
  bitset& operator|=(const bitset& __rhs) noexcept {
    this->do_or(__rhs);
    return *this;
  }
  bitset& operator^=(const bitset& __rhs) noexcept {
    this->do_xor(__rhs);
    return *this;
  }
      //@}

      //@{
      /**
       *  Operations on bitsets.
       *  @param  __position  The number of places to shift.
       *
       *  These should be self-explanatory.
       */
      bitset&
      operator<<=(size_t __position) noexcept
      {
	if (__builtin_expect(__position < _Nb, 1))
	  {
	    this->do_left_shift(__position);
	    this->do_sanitize();
	  }
	else
	  this->do_reset();
	return *this;
      }

      bitset&
      operator>>=(size_t __position) noexcept
      {
	if (__builtin_expect(__position < _Nb, 1))
	  {
	    this->do_right_shift(__position);
	    this->do_sanitize();
	  }
	else
	  this->do_reset();
	return *this;
      }
      //@}

      //@{
      /**
       *  These versions of single-bit set, reset, flip, and test are
       *  extensions from the SGI version.  They do no range checking.
       *  @ingroup SGIextensions
       */
      bitset&
      _Unchecked_set(size_t __pos) noexcept
      {
	this->getword(__pos) |= _Base::_S_maskbit(__pos);
	return *this;
      }

      bitset&
      _Unchecked_set(size_t __pos, int __val) noexcept
      {
	if (__val)
	  this->getword(__pos) |= _Base::_S_maskbit(__pos);
	else
	  this->getword(__pos) &= ~_Base::_S_maskbit(__pos);
	return *this;
      }

      bitset&
      _Unchecked_reset(size_t __pos) noexcept
      {
	this->getword(__pos) &= ~_Base::_S_maskbit(__pos);
	return *this;
      }

      bitset&
      _Unchecked_flip(size_t __pos) noexcept
      {
	this->getword(__pos) ^= _Base::_S_maskbit(__pos);
	return *this;
      }

      _GLIBCXX_CONSTEXPR bool
      _Unchecked_test(size_t __pos) const noexcept
      { return ((this->getword(__pos) & _Base::_S_maskbit(__pos))
		!= static_cast<_WordT>(0)); }
      //@}

      // Set, reset, and flip.
      /**
       *  @brief Sets every bit to true.
       */
      bitset&
      set() noexcept
      {
	this->do_set();
	this->do_sanitize();
	return *this;
      }

      /**
       *  @brief Sets a given bit to a particular value.
       *  @param  __position  The index of the bit.
       *  @param  __val  Either true or false, defaults to true.
       *  @throw  std::out_of_range  If @a pos is bigger the size of the %set.
       */
      bitset&
      set(size_t __position, bool __val = true)
      {
	this->check(__position, __N("bitset::set"));
	return _Unchecked_set(__position, __val);
      }

      /**
       *  @brief Sets every bit to false.
       */
      bitset&
      reset() noexcept
      {
	this->do_reset();
	return *this;
      }

      /**
       *  @brief Sets a given bit to false.
       *  @param  __position  The index of the bit.
       *  @throw  std::out_of_range  If @a pos is bigger the size of the %set.
       *
       *  Same as writing @c set(pos,false).
       */
      bitset&
      reset(size_t __position)
      {
	this->check(__position, __N("bitset::reset"));
	return _Unchecked_reset(__position);
      }

      /**
       *  @brief Toggles every bit to its opposite value.
       */
      bitset&
      flip() noexcept
      {
	this->do_flip();
	this->do_sanitize();
	return *this;
      }

      /**
       *  @brief Toggles a given bit to its opposite value.
       *  @param  __position  The index of the bit.
       *  @throw  std::out_of_range  If @a pos is bigger the size of the %set.
       */
      bitset&
      flip(size_t __position)
      {
	this->check(__position, __N("bitset::flip"));
	return _Unchecked_flip(__position);
      }

      /// See the no-argument flip().
      bitset
      operator~() const noexcept
      { return bitset(*this).flip(); }

      //@{
      /**
       *  @brief  Array-indexing support.
       *  @param  __position  Index into the %bitset.
       *  @return A bool for a <em>const %bitset.  For non-const
       *           bitsets, an instance of the reference proxy class.
       *  @note  These operators do no range checking and throw no exceptions,
       *         as required by DR 11 to the standard.
       *
       *  _GLIBCXX_RESOLVE_LIB_DEFECTS Note that this implementation already
       *  resolves DR 11 (items 1 and 2), but does not do the range-checking
       *  required by that DR's resolution.  -pme
       *  The DR has since been changed:  range-checking is a precondition
       *  (users' responsibility), and these functions must not throw.  -pme
       */
      reference
      operator[](size_t __position)
      { return reference(*this, __position); }

      _GLIBCXX_CONSTEXPR bool
      operator[](size_t __position) const
      { return _Unchecked_test(__position); }
      //@}

      /**
       *  @brief Returns a numerical interpretation of the %bitset.
       *  @return  The integral equivalent of the bits.
       *  @throw  std::overflow_error  If there are too many bits to be
       *                               represented in an @c unsigned @c long.
       */
      unsigned long
      to_ulong() const
      { return this->do_to_ulong(); }

#if __cplusplus >= 201103L
      unsigned long long
      to_ullong() const
      { return this->do_to_ullong(); }
#endif

      /**
       *  @brief Returns a character interpretation of the %bitset.
       *  @return  The string equivalent of the bits.
       *
       *  Note the ordering of the bits:  decreasing character positions
       *  correspond to increasing bit positions (see the main class notes for
       *  an example).
       */
      template<class _CharT, class _Traits, class _Alloc>
	std::basic_string<_CharT, _Traits, _Alloc>
	to_string() const
	{
	  std::basic_string<_CharT, _Traits, _Alloc> __result;
	  copy_to_string(__result, _CharT('0'), _CharT('1'));
	  return __result;
	}

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 396. what are characters zero and one.
      template<class _CharT, class _Traits, class _Alloc>
	std::basic_string<_CharT, _Traits, _Alloc>
	to_string(_CharT __zero, _CharT __one = _CharT('1')) const
	{
	  std::basic_string<_CharT, _Traits, _Alloc> __result;
	  copy_to_string(__result, __zero, __one);
	  return __result;
	}

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 434. bitset::to_string() hard to use.
      template<class _CharT, class _Traits>
	std::basic_string<_CharT, _Traits, std::allocator<_CharT> >
	to_string() const
	{ return to_string<_CharT, _Traits, std::allocator<_CharT> >(); }

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 853. to_string needs updating with zero and one.
      template<class _CharT, class _Traits>
	std::basic_string<_CharT, _Traits, std::allocator<_CharT> >
	to_string(_CharT __zero, _CharT __one = _CharT('1')) const
	{ return to_string<_CharT, _Traits,
	                   std::allocator<_CharT> >(__zero, __one); }

      template<class _CharT>
	std::basic_string<_CharT, std::char_traits<_CharT>,
	                  std::allocator<_CharT> >
	to_string() const
	{
	  return to_string<_CharT, std::char_traits<_CharT>,
	                   std::allocator<_CharT> >();
	}

      template<class _CharT>
	std::basic_string<_CharT, std::char_traits<_CharT>,
	                  std::allocator<_CharT> >
	to_string(_CharT __zero, _CharT __one = _CharT('1')) const
	{
	  return to_string<_CharT, std::char_traits<_CharT>,
	                   std::allocator<_CharT> >(__zero, __one);
	}

      std::basic_string<char, std::char_traits<char>, std::allocator<char> >
      to_string() const
      {
	return to_string<char, std::char_traits<char>,
	                 std::allocator<char> >();
      }

      std::basic_string<char, std::char_traits<char>, std::allocator<char> >
      to_string(char __zero, char __one = '1') const
      {
	return to_string<char, std::char_traits<char>,
	                 std::allocator<char> >(__zero, __one);
      }

      // Helper functions for string operations.
      template<class _CharT, class _Traits>
        void
        copy_from_ptr(const _CharT*, size_t, size_t, size_t,
			 _CharT, _CharT);

      template<class _CharT, class _Traits, class _Alloc>
	void
	copy_from_string(const std::basic_string<_CharT,
			    _Traits, _Alloc>& __s, size_t __pos, size_t __n,
			    _CharT __zero, _CharT __one)
	{ copy_from_ptr<_CharT, _Traits>(__s.data(), __s.size(), __pos, __n,
					    __zero, __one); }

      template<class _CharT, class _Traits, class _Alloc>
	void
        copy_to_string(std::basic_string<_CharT, _Traits, _Alloc>&,
			  _CharT, _CharT) const;

      // NB: Backward compat.
      template<class _CharT, class _Traits, class _Alloc>
	void
	copy_from_string(const std::basic_string<_CharT,
			    _Traits, _Alloc>& __s, size_t __pos, size_t __n)
	{ copy_from_string(__s, __pos, __n, _CharT('0'), _CharT('1')); }

      template<class _CharT, class _Traits, class _Alloc>
	void
        copy_to_string(std::basic_string<_CharT, _Traits,_Alloc>& __s) const
	{ copy_to_string(__s, _CharT('0'), _CharT('1')); }

      /// Returns the number of bits which are set.
      size_t
      count() const noexcept
      { return this->do_count(); }

      /// Returns the total number of bits.
      _GLIBCXX_CONSTEXPR size_t
      size() const noexcept
      { return _Nb; }

      //@{
      /// These comparisons for equality/inequality are, well, @e bitwise.
      bool
      operator==(const bitset& __rhs) const noexcept
      { return this->is_equal(__rhs); }

      bool
      operator!=(const bitset& __rhs) const noexcept
      { return !this->is_equal(__rhs); }
      //@}

      /**
       *  @brief Tests the value of a bit.
       *  @param  __position  The index of a bit.
       *  @return  The value at @a pos.
       *  @throw  std::out_of_range  If @a pos is bigger the size of the %set.
       */
      bool
      test(size_t __position) const
      {
	this->check(__position, __N("bitset::test"));
	return _Unchecked_test(__position);
      }

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // DR 693. std::bitset::all() missing.
      /**
       *  @brief Tests whether all the bits are on.
       *  @return  True if all the bits are set.
       */
      bool
      all() const noexcept
      { return this->template are_all<_Nb>(); }

      /**
       *  @brief Tests whether any of the bits are on.
       *  @return  True if at least one bit is set.
       */
      bool
      any() const noexcept
      { return this->is_any(); }

      /**
       *  @brief Tests whether any of the bits are on.
       *  @return  True if none of the bits are set.
       */
      bool
      none() const noexcept
      { return !this->is_any(); }

      //@{
      /// Self-explanatory.
      bitset
      operator<<(size_t __position) const noexcept
      { return bitset(*this) <<= __position; }

      bitset
      operator>>(size_t __position) const noexcept
      { return bitset(*this) >>= __position; }
      //@}

      /**
       *  @brief  Finds the index of the first "on" bit.
       *  @return  The index of the first bit set, or size() if not found.
       *  @ingroup SGIextensions
       *  @sa  _Find_next
       */
      size_t
      _Find_first() const noexcept
      { return this->do_find_first(_Nb); }

      /**
       *  @brief  Finds the index of the next "on" bit after prev.
       *  @return  The index of the next bit set, or size() if not found.
       *  @param  __prev  Where to start searching.
       *  @ingroup SGIextensions
       *  @sa  _Find_first
       */
      size_t
      _Find_next(size_t __prev) const noexcept
      { return this->do_find_next(__prev, _Nb); }
    };

  // Definitions of non-inline member functions.
  template<size_t _Nb>
    template<class _CharT, class _Traits>
      void
      bitset::
      copy_from_ptr(const _CharT* __s, size_t __len,
		       size_t __pos, size_t __n, _CharT __zero, _CharT __one)
      {
	reset();
	const size_t __nbits = std::min(_Nb, std::min(__n, size_t(__len - __pos)));
	for (size_t __i = __nbits; __i > 0; --__i)
	  {
	    const _CharT __c = __s[__pos + __nbits - __i];
	    if (_Traits::eq(__c, __zero))
	      ;
	    else if (_Traits::eq(__c, __one))
	      _Unchecked_set(__i - 1);
	    else
	      __throw_invalid_argument(__N("bitset::copy_from_ptr"));
	  }
      }

  template<size_t _Nb>
    template<class _CharT, class _Traits, class _Alloc>
      void
      bitset::
      copy_to_string(std::basic_string<_CharT, _Traits, _Alloc>& __s,
			_CharT __zero, _CharT __one) const
      {
	__s.assign(_Nb, __zero);
	for (size_t __i = _Nb; __i > 0; --__i)
	  if (_Unchecked_test(__i - 1))
	    _Traits::assign(__s[_Nb - __i], __one);
      }

  // 23.3.5.3 bitset operations:
  //@{
  /**
   *  @brief  Global bitwise operations on bitsets.
   *  @param  __x  A bitset.
   *  @param  __y  A bitset of the same size as @a __x.
   *  @return  A new bitset.
   *
   *  These should be self-explanatory.
  */
  template<size_t _Nb>
    inline bitset
    operator&(const bitset& __x, const bitset& __y) noexcept
    {
      bitset __result(__x);
      __result &= __y;
      return __result;
    }

  template<size_t _Nb>
    inline bitset
    operator|(const bitset& __x, const bitset& __y) noexcept
    {
      bitset __result(__x);
      __result |= __y;
      return __result;
    }

  template <size_t _Nb>
    inline bitset
    operator^(const bitset& __x, const bitset& __y) noexcept
    {
      bitset __result(__x);
      __result ^= __y;
      return __result;
    }

};

// Definitions of non-inline functions from _Base_bitset.
template<size_type get_nwords(nbits)>
void
bitset::do_left_shift(size_type shift) noexcept
{
  if (builtin_expect(shift != 0, 1))
    {
      const size_type wshift = shift / _GLIBCXX_BITSET_BITS_PER_WORD;
      const size_type offset = shift % _GLIBCXX_BITSET_BITS_PER_WORD;

      if (offset == 0)
        for (size_type n = get_nwords(nbits) - 1; n >= wshift; --n)
          mem[n] = mem[n - wshift];
      else
        {
          const size_type sub_offset = (_GLIBCXX_BITSET_BITS_PER_WORD
                                        - offset);
          for (size_type n = get_nwords(nbits) - 1; n > wshift; --n)
            mem[n] = ((mem[n - wshift] << offset)
                      | (mem[n - wshift - 1] >> sub_offset));
          mem[wshift] = mem[0] << offset;
        }

      std::fill(w + 0, w + wshift, static_cast<_word_type>(0));
    }
}

template<size_type get_nwords(nbits)>
void
bitset::do_right_shift(size_type shift) noexcept
{
  if (builtin_expect(shift != 0, 1))
    {
      const size_type wshift = shift / _GLIBCXX_BITSET_BITS_PER_WORD;
      const size_type offset = shift % _GLIBCXX_BITSET_BITS_PER_WORD;
      const size_type limit = get_nwords(nbits) - wshift - 1;

      if (offset == 0)
        for (size_type n = 0; n <= limit; ++n)
          mem[n] = mem[n + wshift];
      else
        {
          const size_type sub_offset = (_GLIBCXX_BITSET_BITS_PER_WORD
                                        - offset);
          for (size_type n = 0; n < limit; ++n)
            mem[n] = ((mem[n + wshift] >> offset)
                      | (mem[n + wshift + 1] << sub_offset));
          mem[limit] = mem[get_nwords(nbits)-1] >> offset;
        }

      std::fill(w + limit + 1, w + get_nwords(nbits), static_cast<_word_type>(0));
    }
}

template<size_type get_nwords(nbits)>
unsigned long
bitset::do_to_ulong() const
{
  for (size_type i = 1; i < get_nwords(nbits); ++i)
    if (mem[i])
      throw_overflow_error(N("_Base_bitset::do_to_ulong"));
  return mem[0];
}

#if cplusplus >= 201103L
template<size_type get_nwords(nbits)>
unsigned long long
bitset::do_to_ullong() const
{
  const bool dw = sizeof(unsigned long long) > sizeof(unsigned long);
  for (size_type i = 1 + dw; i < get_nwords(nbits); ++i)
    if (mem[i])
      throw_overflow_error(N("_Base_bitset::do_to_ullong"));

  if (dw)
    return mem[0] + (static_cast<unsigned long long>(mem[1])
                     << _GLIBCXX_BITSET_BITS_PER_WORD);
  return mem[0];
}
#endif

template<size_type get_nwords(nbits)>
size_type
bitset::
do_find_first(size_type not_found) const noexcept
{
  for (size_type i = 0; i < get_nwords(nbits); i++)
    {
      _word_type thisword = mem[i];
      if (thisword != static_cast<_word_type>(0))
        return (i * _GLIBCXX_BITSET_BITS_PER_WORD
                + builtin_ctzl(thisword));
    }
  // not found, so return an indication of failure.
  return not_found;
}

template<size_type get_nwords(nbits)>
size_type
bitset::
do_find_next(size_type prev, size_type not_found) const noexcept
{
  // make bound inclusive
  ++prev;

  // check out of bounds
  if (prev >= get_nwords(nbits) * _GLIBCXX_BITSET_BITS_PER_WORD)
    return not_found;

  // search first word
  size_type i = _S_whichword(prev);
  _word_type thisword = mem[i];

  // mask off bits below bound
  thisword &= (~static_cast<_word_type>(0)) << _S_whichbit(prev);

  if (thisword != static_cast<_word_type>(0))
    return (i * _GLIBCXX_BITSET_BITS_PER_WORD
            + builtin_ctzl(thisword));

  // check subsequent words
  i++;
  for (; i < get_nwords(nbits); i++)
    {
      thisword = mem[i];
      if (thisword != static_cast<_word_type>(0))
        return (i * _GLIBCXX_BITSET_BITS_PER_WORD
                + builtin_ctzl(thisword));
    }
  // not found, so return an indication of failure.
  return not_found;
} // end do_find_next

};
} // namespace util
#endif /* __BITSET_H__ */
