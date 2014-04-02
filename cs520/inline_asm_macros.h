//fn name is <type> <name>
//args arg <type> arg
//it should be easy to duplicate this and make a version that takes 
//additional output constraint
#define inline_safe_asm6(rtype, fn_name, type1, arg1, type2, arg2,        \
                        type3, arg3, type4, arg4, type5, arg5, type6, arg6, \
                        asm_code,input_constraints)                     \
  rtype fn_name(type1 arg1##_,type2 arg2##_,type3 arg3##_,               \
               type4 arg4##_,type5 arg5##_, type6 arg6##_){             \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register type1 arg1 __asm__ ("%rdi")=arg1##_;                       \
    register type2 arg2 __asm__ ("%rsi")=arg2##_;                       \
    register type3 arg3 __asm__ ("%rdx")=arg3##_;                       \
    register type4 arg4 __asm__ ("%rcx")=arg4##_;                       \
    register type5 arg5 __asm__ ("%r8") =arg5##_;                       \
    register type6 arg6 __asm__ ("%r9") =arg6##_;                       \
    register rtype retval __asm__ ("%rax");                                   \
    __asm__ volatile(asm_code                                           \
                     : "=r" (retval)                                    \
                     : input_constraints, "r"(arg1), "r"(arg2),         \
                       "r"(arg3), "r"(arg4), "r"(arg5), "r"(arg6));     \
    return retval;                                                      \
  }

#define inline_safe_asm5(rtype, fn_name, type1, arg1, type2, arg2,        \
                         type3, arg3, type4, arg4, type5, arg5,         \
                        asm_code,input_constraints)                     \
  rtype fn_name(type1 arg1##_,type2 arg2##_,type3 arg3##_,               \
               type4 arg4##_,type5 arg5##_){                            \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register type1 arg1 __asm__ ("%rdi")=arg1##_;                       \
    register type2 arg2 __asm__ ("%rsi")=arg2##_;                       \
    register type3 arg3 __asm__ ("%rdx")=arg3##_;                       \
    register type4 arg4 __asm__ ("%rcx")=arg4##_;                       \
    register type5 arg5 __asm__ ("%r8") =arg5##_;                       \
    register rtype retval __asm__ ("%rax");                                   \
    __asm__ volatile(asm_code                                           \
                     : "=r" (retval)                                    \
                     : input_constraints, "r"(arg1), "r"(arg2),         \
                       "r"(arg3), "r"(arg4), "r"(arg5));                \
    return retval;                                                      \
  }
#define inline_safe_asm4(rtype, fn_name, type1, arg1, type2, arg2,       \
                         type3, arg3, type4, arg4,                      \
                         asm_code,input_constraints)                    \
  rtype fn_name(type1 arg1##_,type2 arg2##_,type3 arg3##_,               \
               type4 arg4##_,type5 arg5##_){                            \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register type1 arg1 __asm__ ("%rdi")=arg1##_;                       \
    register type2 arg2 __asm__ ("%rsi")=arg2##_;                       \
    register type3 arg3 __asm__ ("%rdx")=arg3##_;                       \
    register type4 arg4 __asm__ ("%rcx")=arg4##_;                       \
    register rtype retval __asm__ ("%rax");                                   \
    __asm__ volatile(asm_code                                           \
                     : "=r" (retval)                                    \
                     : input_constraints, "r"(arg1), "r"(arg2),         \
                       "r"(arg3), "r"(arg4));                           \
    return retval;                                                      \
  }
#define inline_safe_asm3(rtype, fn_name, type1, arg1, type2, arg2,       \
                         type3, arg3,                                   \
                         asm_code,input_constraints)                    \
  rtype fn_name(type1 arg1##_,type2 arg2##_,type3 arg3##_){              \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register type1 arg1 __asm__ ("%rdi")=arg1##_;                       \
    register type2 arg2 __asm__ ("%rsi")=arg2##_;                       \
    register type3 arg3 __asm__ ("%rdx")=arg3##_;                       \
    register rtype retval __asm__ ("%rax");                                   \
    __asm__ volatile(asm_code                                           \
                     : "=r" (retval)                                    \
                     : input_constraints, "r"(arg1), "r"(arg2),         \
                       "r"(arg3));                                      \
    return retval;                                                      \
  }
#define inline_safe_asm2(rtype, fn_name, type1, arg1, type2, arg2,       \
                         asm_code,input_constraints)                    \
  rtype fn_name(type1 arg1##_,type2 arg2##_){                            \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register type1 arg1 __asm__ ("%rdi")=arg1##_;                       \
    register type2 arg2 __asm__ ("%rsi")=arg2##_;                       \
    register rtype retval __asm__ ("%rax");                                   \
    __asm__ volatile(asm_code                                           \
                     : "=r" (retval)                                    \
                     : input_constraints, "r"(arg1), "r"(arg2));        \
    return retval;                                                      \
  }
#define inline_safe_asm1(rtype, fn_name, type1, arg1,                    \
                         asm_code,input_constraints)                    \
  rtype fn_name(type1 arg1##_){                                          \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register type1 arg1 __asm__ ("%rdi")=arg1##_;                       \
    register rtype retval __asm__ ("%rax");                                   \
    __asm__ volatile(asm_code                                           \
                     : "=r" (retval)                                    \
                     : input_constraints, "r"(arg1));                   \
    return retval;                                                      \
  }
#define inline_safe_asm0(rtype, fn_name,asm_code,input_constraints)      \
  rtype fn_name(){                                                       \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register rtype retval __asm__ ("%rax");                                   \
    __asm__ volatile(asm_code                                           \
                     : "=r" (retval)                                    \
                     : input_constraints);                              \
    return retval;                                                      \
  }


//syscall macros
#define inline_safe_syscall6(rtype, fn_name, type1, arg1, type2, arg2,   \
                             type3, arg3, type4, arg4, type5, arg5,\
                             type6, arg6, syscall_no)                   \
  rtype fn_name(type1 arg1##_,type2 arg2##_,type3 arg3##_,               \
               type4 arg4##_,type5 arg5##_, type6 arg6##_){             \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register type1 arg1 __asm__ ("%rdi")=arg1##_;                       \
    register type2 arg2 __asm__ ("%rsi")=arg2##_;                       \
    register type3 arg3 __asm__ ("%rdx")=arg3##_;                       \
    register type4 arg4 __asm__ ("%rcx")=arg4##_;                       \
    register type5 arg5 __asm__ ("%r8") =arg5##_;                       \
    register type6 arg6 __asm__ ("%r9") =arg6##_;                       \
    register rtype retval __asm__ ("%rax")=syscall_no;                        \
    __asm__ volatile("syscall"                                          \
                      : "+r" (retval)                                   \
                      : "r"(arg1), "r"(arg2),                           \
                      "r"(arg3), "r"(arg4), "r"(arg5), "r"(arg6));      \
    if((uint64_t)retval>=(uint64_t)-4095){                              \
      errno=-retval;                                                    \
      return -1;                                                        \
    }                                                                   \
    return retval;                                                      \
  }

#define inline_safe_syscall5(rtype, fn_name, type1, arg1, type2, arg2,   \
                             type3, arg3, type4, arg4, type5, arg5,     \
                             syscall_no)                                \
  rtype fn_name(type1 arg1##_,type2 arg2##_,type3 arg3##_,               \
               type4 arg4##_,type5 arg5##_){                            \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register type1 arg1 __asm__ ("%rdi")=arg1##_;                       \
    register type2 arg2 __asm__ ("%rsi")=arg2##_;                       \
    register type3 arg3 __asm__ ("%rdx")=arg3##_;                       \
    register type4 arg4 __asm__ ("%rcx")=arg4##_;                       \
    register type5 arg5 __asm__ ("%r8") =arg5##_;                       \
    register rtype retval __asm__ ("%rax")=syscall_no;                        \
    __asm__ volatile("syscall"                                          \
                      : "+r" (retval)                                   \
                      : "r"(arg1), "r"(arg2),                           \
                      "r"(arg3), "r"(arg4), "r"(arg5));                 \
    if((uint64_t)retval>=(uint64_t)-4095){                              \
      errno=-retval;                                                    \
      return -1;                                                        \
    }                                                                   \
    return retval;                                                      \
  }
#define inline_safe_syscall4(rtype, fn_name, type1, arg1, type2, arg2,   \
                             type3, arg3, type4, arg4, syscall_no)       \
  rtype fn_name(type1 arg1##_,type2 arg2##_,type3 arg3##_,               \
               type4 arg4##_){                                          \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register type1 arg1 __asm__ ("%rdi")=arg1##_;                       \
    register type2 arg2 __asm__ ("%rsi")=arg2##_;                       \
    register type3 arg3 __asm__ ("%rdx")=arg3##_;                       \
    register type4 arg4 __asm__ ("%rcx")=arg4##_;                       \
    register rtype retval __asm__ ("%rax")=syscall_no;                        \
    __asm__ volatile("syscall"                                          \
                      : "+r" (retval)                                   \
                      : "r"(arg1), "r"(arg2),                           \
                      "r"(arg3), "r"(arg4));                            \
    if((uint64_t)retval>=(uint64_t)-4095){                              \
      errno=-retval;                                                    \
      return -1;                                                        \
    }                                                                   \
    return retval;                                                      \
  }
#define inline_safe_syscall3(rtype, fn_name, type1, arg1, type2, arg2,   \
                             type3, arg3, syscall_no)                   \
  rtype fn_name(type1 arg1##_,type2 arg2##_,type3 arg3##_){              \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register type1 arg1 __asm__ ("%rdi")=arg1##_;                       \
    register type2 arg2 __asm__ ("%rsi")=arg2##_;                       \
    register type3 arg3 __asm__ ("%rdx")=arg3##_;                       \
    register rtype retval __asm__ ("%rax")=syscall_no;                        \
    __asm__ volatile("syscall"                                          \
                      : "+r" (retval)                                   \
                      : "r"(arg1), "r"(arg2),                           \
                        "r"(arg3));                                     \
    if((uint64_t)retval>=(uint64_t)-4095){                              \
      errno=-retval;                                                    \
      return -1;                                                        \
    }                                                                   \
    return retval;                                                      \
  }
#define inline_safe_syscall2(rtype, fn_name, type1, arg1, type2, arg2,   \
                             syscall_no)                                \
  rtype fn_name(type1 arg1##_,type2 arg2##_){                            \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register type1 arg1 __asm__ ("%rdi")=arg1##_;                       \
    register type2 arg2 __asm__ ("%rsi")=arg2##_;                       \
    register rtype retval __asm__ ("%rax")=syscall_no;                        \
    __asm__ volatile("syscall"                                          \
                     : "+r" (retval)                                    \
                     : "r"(arg1), "r"(arg2));                           \
    if((uint64_t)retval>=(uint64_t)-4095){                              \
      errno=-retval;                                                    \
      return -1;                                                        \
    }                                                                   \
    return retval;                                                      \
  }
#define inline_safe_syscall1(rtype, fn_name, type1, arg1, syscall_no)    \
  rtype fn_name(type1 arg1##_){                                          \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register type1 arg1 __asm__ ("%rdi")=arg1##_;                       \
    register rtype retval __asm__ ("%rax")=syscall_no;                        \
    __asm__ volatile("syscall"                                          \
                     : "+r" (retval)                                    \
                     : "r"(arg1));                                      \
    if((uint64_t)retval>=(uint64_t)-4095){                              \
      errno=-retval;                                                    \
      return -1;                                                        \
    }                                                                   \
    return retval;                                                      \
  }
#define inline_safe_syscall0(rtype, fn_name, syscall_no)                 \
  rtype fn_name(){                                                       \
    PRINT_MSG("Calling "#fn_name"\n");                                  \
    register rtype retval __asm__ ("%rax")=syscall_no;                        \
    __asm__ volatile("syscall"                                          \
                     : "+r" (retval));                                  \
    if((uint64_t)retval>=(uint64_t)-4095){                              \
      errno=-retval;                                                    \
      return -1;                                                        \
    }                                                                   \
    return retval;                                                      \
  }
