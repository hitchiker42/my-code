AC_DEFUN([AX_PROG_CC_C11],
    [AX_CHECK_COMPILE_FLAG([-std=c11],
        [CFLAGS+=" -std=c11"],
        [echo "C compiler can not compile C11 code"
         exit -1])])
AC_DEFUN([AX_PROG_CC_GNU11],
    [AX_CHECK_COMPILE_FLAG([-std=gnu11],
        [CFLAGS+=" -std=gnu11"],
        [echo "C compiler does not support C11 with gnu extensions"
         exit -1])])
