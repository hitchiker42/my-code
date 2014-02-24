//translate vm520 instructions into x86_64 instructions
/*
  VM_HALT -> hlt
  //not sure how to do load/store
  add/sub -> addl/subl
  mul/div ->  //assuing args are in %edi and %esi
  movl %esi,%eax
  //if div xorl %edx,%edx
  mull %edi or divl %edi
  retl
  fadd/fsub/fdiv/fmul -> //args in %edi and %esi
  movd %edi,%xmm0
  movd %esi,%xmm1
  add/sub/div/mul ps %xmm0,%xmm1
  movd %xmm1,%eax
  retl
  blt/bgt/beq ->
  cmpl %edi,%esi
  jl/jg/je %edx
  ret
  
  cmpxchg -> lock cmpxchgl

 */
