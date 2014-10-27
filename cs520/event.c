#include <stdint.h>
/* Cloning flags.  */
#define CSIGNAL       0x000000ff /* Signal mask to be sent at exit.  */
#define CLONE_VM      0x00000100 /* Set if VM shared between processes.  */
#define CLONE_FS      0x00000200 /* Set if fs info shared between processes.  */
#define CLONE_FILES   0x00000400 /* Set if open files shared between processes.  */
#define CLONE_SIGHAND 0x00000800 /* Set if signal handlers shared.  */
#define CLONE_PTRACE  0x00002000 /* Set if tracing continues on the child.  */
#define CLONE_VFORK   0x00004000 /* Set if the parent wants the child to
				     wake it up on mm_release.  */
#define CLONE_PARENT  0x00008000 /* Set if we want to have the same
				     parent as the cloner.  */
#define CLONE_THREAD  0x00010000 /* Set to add to same thread group.  */
#define CLONE_NEWNS   0x00020000 /* Set to create new namespace.  */
#define CLONE_SYSVSEM 0x00040000 /* Set to shared SVID SEM_UNDO semantics.  */
#define CLONE_SETTLS  0x00080000 /* Set TLS info.  */
#define CLONE_PARENT_SETTID 0x00100000 /* Store TID in userlevel buffer
					   before MM copy.  */
#define CLONE_CHILD_CLEARTID 0x00200000 /* Register exit futex and memory
					    location to clear.  */
#define CLONE_DETACHED 0x00400000 /* Create clone detached.  */
#define CLONE_UNTRACED 0x00800000 /* Set if the tracing process can't
				      force CLONE_PTRACE on this clone.  */
#define CLONE_CHILD_SETTID 0x01000000 /* Store TID in userlevel buffer in
					  the child.  */
#define CLONE_NEWUTS	0x04000000	/* New utsname group.  */
#define CLONE_NEWIPC	0x08000000	/* New ipcs.  */
#define CLONE_NEWUSER	0x10000000	/* New user namespace.  */
#define CLONE_NEWPID	0x20000000	/* New pid namespace.  */
#define CLONE_NEWNET	0x40000000	/* New network namespace.  */
#define CLONE_IO	0x80000000	/* Clone I/O context.  */
//clone syscall no = 56
__asm__(".globl clone\n"
        ".type clone,@function\n"
        ".p2align 3\n"
        "clone:\n"
        ".cfi_strtproc\n"
        "movq $56,%rax\n"
        "syscall\n"
        "retq\n"
        ".cfi_endproc\n"
        ".size clone,.-clone")
extern long clone(uint64_t flags, void* child_stack,...
                  /*void *ptid,void *ctid, struct pt_regs *regs*/);

/*Idea:
  One parent thread who's job it is to wait on children and put their
  exit values into some struct.

  One Main thread, child of the above parent, this thread runs
  the event loop, it reacts to events and creates a new thread 
  for each event.

  N event threads, created by the main thread (but childern of 
  the parent thread). These do the actual work, each thread
  has an assoicated struct which contains the thread id, exit value, 
  flag to indicate if still running, and a void* user data field.

  the parent thread and the other threads don't share signal handlers,
  the main thread and the event threads do
*/
#define CLONE_MAIN_FLAGS CLONE_VM|CLONE_FS|CLONE_FILES|\
  CLONE_THREAD|CLONE_PARENT_SETTID
#define CLONE_EVENT_FLAGS CLONE_MAIN_FLAGS|CLONE_SIGHAND 
