template<size_t nregs, size_t mem_size>
struct VM {
  using word_t = int;
  struct instruction {
    opcode op;
    word_t arg1;
    word_t arg2;
    word_t arg3;
  };
  std::array<word_t, nregs> registers;
  union {
    std::array<uint8_t, mem_size> mem;
    std::array<word_t, mem_size / sizeof(word_t)> mem_words;
  };
}
  
  
