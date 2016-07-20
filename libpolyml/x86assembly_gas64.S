/*
   Title:  Assembly code routines for the poly system.
   Author:    David Matthews
   Copyright (c) David C. J. Matthews 2000-2016
 
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License version 2.1 as published by the Free Software Foundation.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
   
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/*
   This is the 64-bit gas version of the assembly code file.
   There are separate versions of 32/64 and MAMS (Intel syntax) and
   and GCC (gas syntax).
*/

/*
 Registers used :-

  %rax: First argument to function.  Result of function call.
  %rbx: Second argument to function.
  %rcx: General register
  %rdx: Closure pointer in call.
  %rbp: Points to memory used for extra registers
  %rsi: General register.
  %rdi: General register.
  %rsp: Stack pointer.
  %r8:   Third argument to function
  %r9:   Fourth argument to function
  %r10:  Fifth argument to function
  %r11:  General register
  %r12:  General register
  %r13:  General register
  %r14:  General register
  %r15:  Memory allocation pointer
*/


#include "config.h"
#ifdef SYMBOLS_REQUIRE_UNDERSCORE
#define EXTNAME(x)  _##x
#else
#define EXTNAME(x)  x
#endif

/* Macro to begin the hand-coded functions */
#ifdef MACOSX
#define GLOBAL .globl
#else
#define GLOBAL .global
#endif

#define INLINE_ROUTINE(id) \
GLOBAL EXTNAME(id); \
EXTNAME(id):

/* The "stack frame" pointed to by rbp acts as the "memory registers". */
#define Fr_RaiseOverflow        -120
#define Fr_HeapOverflow         -104
#define Fr_StackOverflow        -96
#define Fr_StackOverflowEx      -88
#define Fr_HandlerRegister      -80
#define Fr_LocalMbottom         -72
#define Fr_StackLimit           -64

/* The Windows calling convention reserves space for arguments in the
   call.  In Unix we instead save the argument in an unused save location. */
#ifdef _WIN32
#define Fr_Argument             16
#else
#define Fr_Argument             -48
#endif

/* Extra entries on the C stack */
#define Fr_SavedMLSP            -128

#define Fr_Size                 144         /* Must be multiple of 16 to get alignment correct */

/* This is the argument vector passed in to X86AsmSwitchToPoly
   It is used to initialise the frame.  A few values are updated
   when ML returns. */
#define Arg_LocalMpointer       0x0
#define Arg_HandlerRegister     0x8
#define Arg_LocalMbottom        0x10
#define Arg_StackLimit          0x18
#define Arg_ExceptionPacket     0x20  /* Address of packet to raise */
#define Arg_RequestCode         0x28  /* Byte: Io function to call. */
#define Arg_ReturnReason        0x2a  /* Byte: Reason for returning from ML. */
#define Arg_UnusedRestore       0x2b  /* Byte: Full/partial restore */
#define Arg_SaveCStack          0x30  /* Current stack base */
#define Arg_ThreadId            0x38  /* My thread id */
#define Arg_StackPtr            0x40  /* Stack Pointer */
#define Arg_ProgramCtr          0x48
#define Arg_SaveRAX             0x50
#define Arg_SaveRBX             0x58
#define Arg_SaveRCX             0x60
#define Arg_SaveRDX             0x68
#define Arg_SaveRSI             0x70
#define Arg_SaveRDI             0x78
#define Arg_SaveR8              0x80
#define Arg_SaveR9              0x88
#define Arg_SaveR10             0x90
#define Arg_SaveR11             0x98
#define Arg_SaveR12             0xa0
#define Arg_SaveR13             0xa8
#define Arg_SaveR14             0xb0
#define Arg_SaveXMM0            0xb8
#define Arg_SaveXMM1            0xc0
#define Arg_SaveXMM2            0xc8
#define Arg_SaveXMM3            0xd0
#define Arg_SaveXMM4            0xd8
#define Arg_SaveXMM5            0xe0
#define Arg_SaveXMM6            0xe8

/* IO function numbers.  These are functions that are called
   to handle special cases in this code */
#include "sys.h"

#define RETURN_HEAP_OVERFLOW        1
#define RETURN_STACK_OVERFLOW       2
#define RETURN_STACK_OVERFLOWEX     3
#define RETURN_CALLBACK_RETURN      6
#define RETURN_CALLBACK_EXCEPTION   7
#define RETURN_RAISE_OVERFLOW       8
#define RETURN_KILL_SELF            9

# Mark the stack as non-executable when supported
#ifdef HAVE_GNU_STACK
.section .note.GNU-stack, "", @progbits
#endif

#
# CODE STARTS HERE
#
    .text

#define CALL_EXTRA(index) \
        pushq %rcx; \
        movq  Fr_Argument(%rbp),%rcx; \
        movb  $index,Arg_ReturnReason(%rcx); \
        popq  %rcx; \
        jmp   SaveFullState;


/* Load the registers from the ML stack and jump to the code. */
INLINE_ROUTINE(X86AsmSwitchToPoly)
    pushq   %rbp                            # Standard entry sequence
#    movq    %rsp,%rbp
    movq    %rsp,Arg_SaveCStack(%rdi)
    pushq   %rbx
    pushq   %r12
    pushq   %r13
    pushq   %r14
    pushq   %r15
#ifdef _WIN32
    pushq   %rdi                            # Callee save in Windows
    pushq   %rsi
    subq    $(Fr_Size-56),%rsp              # Argument is already in %rcx
#else
    subq    $(Fr_Size-40),%rsp
    movq    %rdi,%rcx                       # Argument is in %rdi
#endif
    movq    %rdi,%rbp

    movq    %rcx,Fr_Argument(%rbp)
    movq    Arg_StackLimit(%rcx),%rax
    movq    %rax,Fr_StackLimit(%rbp)
    movq    Arg_LocalMbottom(%rcx),%rax
    movq    %rax,Fr_LocalMbottom(%rbp)
    movq    Arg_HandlerRegister(%rcx),%rax
    movq    %rax,Fr_HandlerRegister(%rbp)
    lea     callExStackEx(%rip),%rax
    movq    %rax,Fr_StackOverflowEx(%rbp)
    lea     callExStack(%rip),%rax
    movq    %rax,Fr_StackOverflow(%rbp)
    lea     callExHeap(%rip),%rax
    movq    %rax,Fr_HeapOverflow(%rbp)
    lea     callExRaiseOverflow(%rip),%rax
    movq    %rax,Fr_RaiseOverflow(%rbp)
    movq    Arg_LocalMpointer(%rcx),%r15
    movq    Arg_StackPtr(%rcx),%rsp               # Set the new stack ptr
    movq    Arg_ExceptionPacket(%rcx),%rax
    cmpq    $1,%rax                             # Did we raise an exception?
    jnz     raisexLocal

    movsd   Arg_SaveXMM0(%rcx),%xmm0
    movsd   Arg_SaveXMM1(%rcx),%xmm1
    movsd   Arg_SaveXMM2(%rcx),%xmm2
    movsd   Arg_SaveXMM3(%rcx),%xmm3
    movsd   Arg_SaveXMM4(%rcx),%xmm4
    movsd   Arg_SaveXMM5(%rcx),%xmm5
    movsd   Arg_SaveXMM6(%rcx),%xmm6
    movq    Arg_SaveRAX(%rcx),%rax              # Load the registers
    movq    Arg_SaveRBX(%rcx),%rbx
    movq    Arg_SaveRDX(%rcx),%rdx
    movq    Arg_SaveRSI(%rcx),%rsi
    movq    Arg_SaveRDI(%rcx),%rdi
    movq    Arg_SaveR8(%rcx),%r8
    movq    Arg_SaveR9(%rcx),%r9
    movq    Arg_SaveR10(%rcx),%r10
    movq    Arg_SaveR11(%rcx),%r11
    movq    Arg_SaveR12(%rcx),%r12
    movq    Arg_SaveR13(%rcx),%r13
    movq    Arg_SaveR14(%rcx),%r14
    cld                                     # Clear this just in case
    push    Arg_ProgramCtr(%rcx)
    movq    Arg_SaveRCX(%rcx),%rcx
    ret                                     # Jump to code address

/* This is exactly the same as raisex but seems to be needed to work round a PIC problem. */
raisexLocal:
    movq    Fr_HandlerRegister(%rbp),%rcx    # Get next handler into %rcx
    jmp     *(%rcx)

/* Code to save the state and switch to C
   This saves the full register state. */
SaveFullState:
    pushq   %rax                                # Save rax
    movq    Fr_Argument(%rbp),%rax
    movq    %rbx,Arg_SaveRBX(%rax)
    movq    %rcx,Arg_SaveRCX(%rax)
    movq    %rdx,Arg_SaveRDX(%rax)
    movq    %rsi,Arg_SaveRSI(%rax)
    movq    %rdi,Arg_SaveRDI(%rax)
    movsd   %xmm0,Arg_SaveXMM0(%rax)
    movsd   %xmm1,Arg_SaveXMM1(%rax)
    movsd   %xmm2,Arg_SaveXMM2(%rax)
    movsd   %xmm3,Arg_SaveXMM3(%rax)
    movsd   %xmm4,Arg_SaveXMM4(%rax)
    movsd   %xmm5,Arg_SaveXMM5(%rax)
    movsd   %xmm6,Arg_SaveXMM6(%rax)
    movq    %r8,Arg_SaveR8(%rax)
    movq    %r9,Arg_SaveR9(%rax)
    movq    %r10,Arg_SaveR10(%rax)
    movq    %r11,Arg_SaveR11(%rax)
    movq    %r12,Arg_SaveR12(%rax)
    movq    %r13,Arg_SaveR13(%rax)
    movq    %r14,Arg_SaveR14(%rax)
    popq    %rbx                                # Get old rax value
    movq    %rbx,Arg_SaveRAX(%rax)
    movq    %rsp,Arg_StackPtr(%rax)
    movq    Fr_Argument(%rbp),%rax
    movq    %r15,Arg_LocalMpointer(%rax)        # Save back heap pointer
    movq    Fr_HandlerRegister(%rbp),%rbx       # and handler pointer
    movq    %rbx,Arg_HandlerRegister(%rax)
    movq    Arg_SaveCStack(%rax),%rsp           # Restore C stack pointer
#ifdef _WIN32
    subq    $56,%rsp
    popq    %rsi
    popq    %rdi
#else
    subq    $40,%rsp
#endif
    popq    %r15                                # Restore callee-save registers
    popq    %r14
    popq    %r13
    popq    %r12
    popq    %rbx
    popq    %rbp
    ret

callExHeap:
    CALL_EXTRA(RETURN_HEAP_OVERFLOW)
callExStack:
    CALL_EXTRA(RETURN_STACK_OVERFLOW)
callExStackEx:
    CALL_EXTRA(RETURN_STACK_OVERFLOWEX)
callExRaiseOverflow:
    CALL_EXTRA(RETURN_RAISE_OVERFLOW)


/* Used when entering new code.  The argument and closure are on the stack
   in case there is a GC before we enter the code. */
INLINE_ROUTINE(X86AsmPopArgAndClosure)
    popq    %rdx
    popq    %rax
    jmp     *(%rdx)

# This is used if the RTS sets up an exception.  It's probably no longer relevant.
INLINE_ROUTINE(X86AsmRaiseException)
    movq    Fr_HandlerRegister(%rbp),%rcx    # Get next handler into %rcx
    jmp     *(%rcx)
# Additional assembly code routines

# RTS call to kill the current thread. 
INLINE_ROUTINE(X86AsmKillSelf)
    CALL_EXTRA(RETURN_KILL_SELF)

INLINE_ROUTINE(X86AsmCallbackReturn)
    CALL_EXTRA(RETURN_CALLBACK_RETURN)

INLINE_ROUTINE(X86AsmCallbackException)
    CALL_EXTRA(RETURN_CALLBACK_EXCEPTION)

# This implements atomic addition in the same way as atomic_increment
INLINE_ROUTINE(X86AsmAtomicIncrement)
#ifdef _WIN32
    movq    %rcx,%rax       # On Windows the argument is passed in %rcx
#else
    movq    %rdi,%rax   # On X86_64 the argument is passed in %rdi
#endif
# Use %rcx and %rax because they are volatile (unlike %rbx on X86/64/Unix)
    movq    $2,%rcx
    lock xaddq %rcx,(%rax)
    addq    $2,%rcx
    movq    %rcx,%rax
    ret

