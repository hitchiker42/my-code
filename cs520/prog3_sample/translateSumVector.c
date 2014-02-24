//
// This is an Intel 64 implementation of the vm520 sumVector.obj.
//
// The 32-bit vm520 operands are "super-sized" into Intel quadwords.
//
// The vm520 registers r0-r5 are mapped to Intel registers r8-r11, rcx
// and rdx, respectively.
//
// The vm520 program is assumed to start with a JMP instruction to jump
// over the data section. The vm520 words in the data section are converted
// to Intel quadwords with the same values. The vm520 instructions, including
// the initial JMP, are converted to equivalent Intel instructions.
//
// The vm520 program is assumed to end with a HALT instruction, which is
// translated to an Intel RET instruction.
//
// vm520 memory references are translated to offset(%rdi), because the
// base address of the buffer containing the translated instructions
// will be passed to the code when it is executed via a function call.
//
// The ldaddr and ldind instructions need special care. The translation
// of ldaddr needs to load the vm520 address. Then this address will
// be translated when it is used by a ldind instruction. So ldind needs
// to first add the ldind offset to the ldind register. Then it shifts
// that result 3 bits to the left, to multiply it by 8, which translate
// the vm520 word address to an Intel byte address, where each vm520
// word is implemented by an Intel quadword. That result is then added
// to %rdi, which contains the base address of where the vm520 memory
// is located in Intel memory. Finally a movq (%rdx), %rdx is done to
// load the value from the translated address.

// translate vm520 object code to Intel object 64 code
//   this example just hardcodes the Intel instructions that are equivalent
//     to the instructions in the vm520 program sumVector.obj
//
void translateBinary(char *vm520ObjFile,
                     unsigned char *buffer,
                     unsigned int length)
{
  int i = 0;

  // offset 0x00
  buffer[i++] = 0xE9;  // jmp skipData --> jmp +59
  buffer[i++] = 0x3B;  // i.e. skip over 7 quadwords plus 3 padding bytes
  buffer[i++] = 0x00;  // so 59 bytes == 0x3B
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;

  buffer[i++] = 0x00;  // insert 0 bytes to align the following data words
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;

  // offset 0x08
  buffer[i++] = 0x00;  // sum: word 0 --> .quad 0
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;

  // offset 0x10
  buffer[i++] = 0x05; // len: word 5 --> .quad 5
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
 
  // offset 0x18 
  buffer[i++] = 0x01; // vector: word 1 --> .quad 1
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
 
  // offset 0x20 
  buffer[i++] = 0x02; // word 2 --> .quad 2
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  
  // offset 0x28 
  buffer[i++] = 0x03; // word 3 --> .quad 3
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  
  // offset 0x30 
  buffer[i++] = 0x04; // word 4 --> .quad 4
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  
  // offset 0x38 
  buffer[i++] = 0x05; // word 5 --> .quad 5
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  
  // offset 0x40 
  buffer[i++] = 0x49; // skipData: ldimm r0, 0 --> movq $0, %r8
  buffer[i++] = 0xC7;
  buffer[i++] = 0xC0;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;

  // offset 0x47
  buffer[i++] = 0x4C; // load r1, len --> movq 16(%rdi), %r9
  buffer[i++] = 0x8B;
  buffer[i++] = 0x8F;
  buffer[i++] = 0x10;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;

  // offset 0x4E
  buffer[i++] = 0x49; // ldaddr r2, vector --> movq $3, %r10
  buffer[i++] = 0xC7; // needs to load a vm520 address
  buffer[i++] = 0xC2; // it will be translated to an Intel address
  buffer[i++] = 0x03; //   when used by a ldind instruction
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;

  // offset 0x55
  buffer[i++] = 0x49; // ldimm r3, 0 --> movq $0, %r11
  buffer[i++] = 0xC7;
  buffer[i++] = 0xC3;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;

  // offset 0x5C
  buffer[i++] = 0x48; // ldimm r4, 1 --> movq $1, %rcx
  buffer[i++] = 0xC7;
  buffer[i++] = 0xC1;
  buffer[i++] = 0x01;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;

  // offset 0x63
  buffer[i++] = 0x4D; // top: beq r0, r1, done --> cmpq %r8, %r9 and ...
  buffer[i++] = 0x39;
  buffer[i++] = 0xC1;

  // offset 0x66
  buffer[i++] = 0x0F; //                           je +34
  buffer[i++] = 0x84;
  buffer[i++] = 0x22;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;

                      // ldind needs to translate vm520 addr to Intel addr
  // offset 0x6C
  buffer[i++] = 0x4C; // ldind r5, 0(r2) --> movq %r10, %rdx and ...
  buffer[i++] = 0x89;
  buffer[i++] = 0xD2;

  // offset 0x6F
  buffer[i++] = 0x48; //                     addq $0, %rdx and ...
  buffer[i++] = 0x81;
  buffer[i++] = 0xC2;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;

  // offset 0x76
  buffer[i++] = 0x48; //                     salq $3, %rdx and ...
  buffer[i++] = 0xC1;
  buffer[i++] = 0xE2;
  buffer[i++] = 0x03;

  // offset 0x7A
  buffer[i++] = 0x48; //                     addq %rdi, %rdx and ...
  buffer[i++] = 0x01;
  buffer[i++] = 0xFA;

  // offset 0x7D
  buffer[i++] = 0x48; //                     movq (%rdx), %rdx
  buffer[i++] = 0x8B;
  buffer[i++] = 0x12;

  // offset 0x80
  buffer[i++] = 0x49; // addi r3, r5 --> addq %rdx, %r11
  buffer[i++] = 0x01;
  buffer[i++] = 0xD3;

  // offset 0x83
  buffer[i++] = 0x49; // addi r2, r4 --> addq %rcx, %r10
  buffer[i++] = 0x01;
  buffer[i++] = 0xCA;

  // offset 0x86
  buffer[i++] = 0x49; // addi r0, r4 --> addq %rcx, %r8
  buffer[i++] = 0x01;
  buffer[i++] = 0xC8;

  // offset 0x89
  buffer[i++] = 0xE9; // jmp top --> jmp -26
  buffer[i++] = 0xD5;
  buffer[i++] = 0xFF;
  buffer[i++] = 0xFF;
  buffer[i++] = 0xFF;

  // offset 0x8E
  buffer[i++] = 0x4C; // done: store r3, sum -->  movq %r11, 8(%rdi)
  buffer[i++] = 0x89;
  buffer[i++] = 0x9F;
  buffer[i++] = 0x08;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;
  buffer[i++] = 0x00;

  // offset 0x95
  buffer[i++] = 0xC3; // halt --> ret
}

