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
   This is the 32-bit Unix version of the assembly code file.
   There are separate versions of 32/64 and Windows (Intel syntax)
   and Unix (gas syntax).
*/

/*
 Registers used :-

  %%eax: First argument to function.  Result of function call.
  %%ebx: Second argument to function.
  %%ecx: General register
  %%edx: Closure pointer in call.
  %%ebp: Points to memory used for extra registers
  %%esi: General register.
  %%edi: General register.
  %%esp: Stack pointer.
*/


#include "config.h"
#ifdef SYMBOLS_REQUIRE_UNDERSCORE
#define EXTNAME(x)  _##x
#else
#define EXTNAME(x)  x
#endif

#
# Macro to begin the hand-coded functions
#

#ifdef MACOSX
#define GLOBAL .globl
#else
#define GLOBAL .global
#endif

#define INLINE_ROUTINE(id) \
GLOBAL EXTNAME(id); \
EXTNAME(id):

#define TAG         1
#define TAGSHIFT    1
#define TAGMULT     (1 << TAGSHIFT)

#define TAGGED(i) ((i << TAGSHIFT) | TAG)
#define MAKETAGGED(from,to)     leal    TAG(,from,2),to

#define POLYWORDSIZE    4

#define NIL         TAGGED(0)
#define UNIT        TAGGED(0)
#define ZERO        TAGGED(0)
#define FALSE       TAGGED(0)
#define TRUE        TAGGED(1)
#define MINUS1      TAGGED(0-1)

#define B_bytes     0x01
#define B_typeBits  0x03
#define B_mutable   0x40

#define Max_Length  0x00ffffff


/* The "stack frame" pointed to by ebp acts as the "memory registers". */
#define Fr_RaiseOverflow            -48
#define Fr_HeapOverflow             -40
#define Fr_StackOverflow            -36
#define Fr_StackOverflowEx          -32
#define Fr_LocalMpointer            -28
#define Fr_HandlerRegister          -24
#define Fr_LocalMbottom             -20
#define Fr_StackLimit               -16
#define Fr_SavedEsi                 -12
#define Fr_SavedEdi                 -8
#define Fr_SavedEbx                 -4
#define Fr_SavedEBP                 0
#define Fr_Argument                 8
/* Extra entries on the C stack */
#define Fr_SavedMLSP                -52

#define Fr_Size                     56

/* This is the argument vector passed in to X86AsmSwitchToPoly
   It is used to initialise the frame.  A few values are updated
   when ML returns. */
#define Arg_LocalMpointer       0x0
#define Arg_HandlerRegister     0x4
#define Arg_LocalMbottom        0x8
#define Arg_StackLimit          0xc
#define Arg_ExceptionPacket     0x10  /* Address of packet to raise */
#define Arg_RequestCode         0x14 /* Byte: Io function to call. */
#define Arg_ReturnReason        0x16  /* Byte: Reason for returning from ML. */
#define Arg_FullRestore         0x17  /* Byte: Full/partial restore */
#define Arg_PolyStack           0x18  /* Current stack base */
#define Arg_ThreadId            0x1c  /* My thread id */
#define Arg_StackPtr            0x20  /* Stack Pointer */
#define Arg_ProgramCtr          0x24
#define Arg_SaveRAX             0x28
#define Arg_SaveRBX             0x2c
#define Arg_SaveRCX             0x30
#define Arg_SaveRDX             0x34
#define Arg_SaveRSI             0x38
#define Arg_SaveRDI             0x3c
#define Arg_SaveFP              0x40

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
        pushl %ecx; \
        movl  Fr_Argument(%ebp),%ecx; \
        movb  $index,Arg_ReturnReason(%ecx); \
        popl  %ecx; \
        jmp   SaveFullState;

/* Load the registers from the ML stack and jump to the code.
  This is used to start ML code.
  The argument is the address of the MemRegisters struct and goes into %rbp.
  This is the general code for switching control to ML.  There are a number of cases to consider:
  1.  Initial entry to root function or a new thread.  Needs to load EDX at least.
  2.  Normal return from an RTS call.  Could just do a simple return.
  3.  Exception raised in RTS call.
  4.  Callback from C to an ML function.  In effect this is a coroutine. Similar to 1.
  5.  Return from "trap" i.e. Heap/Stack overflow.  Stack-overflow can result in an exception
      either because the stack can't be grown or because Interrupt has been raised. */
INLINE_ROUTINE(X86AsmSwitchToPoly)
    pushl   %ebp                            # Standard entry sequence
    movl    %esp,%ebp
    pushl   %ebx
    pushl   %edi
    pushl   %esi                            # Push callee-save registers
    subl    $(Fr_Size-12),%esp              # Allocate frame
    movl    Fr_Argument(%ebp),%ecx          # Address of argument vector
    movl    Arg_StackLimit(%ecx),%eax
    movl    %eax,Fr_StackLimit(%ebp)
    movl    Arg_LocalMbottom(%ecx),%eax
    movl    %eax,Fr_LocalMbottom(%ebp)
    movl    Arg_HandlerRegister(%ecx),%eax
    movl    %eax,Fr_HandlerRegister(%ebp)
    movl    Arg_LocalMpointer(%ecx),%eax
    movl    %eax,Fr_LocalMpointer(%ebp)
    movl    $callExStackEx,%eax
    movl    %eax,Fr_StackOverflowEx(%ebp)
    movl    $callExStack,%eax
    movl    %eax,Fr_StackOverflow(%ebp)
    movl    $callExHeap,%eax
    movl    %eax,Fr_HeapOverflow(%ebp)
    movl    $callExRaiseOverflow,%eax
    movl    %eax,Fr_RaiseOverflow(%ebp)
    movl    Arg_StackPtr(%ecx),%esp               # Set the new stack ptr
    movl    Arg_ExceptionPacket(%ecx),%eax
    cmpl    $1,%eax                             # Did we raise an exception?
    jnz     raisexlocal
    testb   $1,Arg_FullRestore(%ecx)           # Should we restore or clear the regs?
    jnz     sw2polyfull
/* We're returning from an RTS call. */
    movl    Arg_SaveRAX(%ecx),%eax              # The return value
    cld                                     # Clear this just in case
    jmp     *Arg_ProgramCtr(%ecx)            # Jump to code address

sw2polyfull:
    FRSTOR  Arg_SaveFP(%ecx)
    movl    Arg_SaveRAX(%ecx),%eax              # Load the registers
    movl    Arg_SaveRBX(%ecx),%ebx              # Load the registers
    movl    Arg_SaveRDX(%ecx),%edx
    movl    Arg_SaveRSI(%ecx),%esi
    movl    Arg_SaveRDI(%ecx),%edi
    cld                                     # Clear this just in case
    push    Arg_ProgramCtr(%ecx)
    movl    Arg_SaveRCX(%ecx),%ecx
    ret                                     # Jump to code address


/* Code to save the state and switch to C
   This saves the full register state. */
SaveFullState:
    pushl   %eax                            # Save eax
    movl    Fr_Argument(%ebp),%eax
    movl    %ebx,Arg_SaveRBX(%eax)
    movl    %ecx,Arg_SaveRCX(%eax)
    movl    %edx,Arg_SaveRDX(%eax)
    movl    %esi,Arg_SaveRSI(%eax)
    movl    %edi,Arg_SaveRDI(%eax)
    fnsave  Arg_SaveFP(%eax)                # Save FP state.  Also resets the state so...
    fldcw   Arg_SaveFP(%eax)                # ...load because we need the same rounding mode in the RTS
    popl    %ebx                            # Get old eax value
    movl    %ebx,Arg_SaveRAX(%eax)
    movl    %esp,Arg_StackPtr(%eax)               # Save ML stack pointer
    movl    Fr_Argument(%ebp),%eax
    movl    Fr_LocalMpointer(%ebp),%ebx     # Copy back heap pointer
    movl    %ebx,Arg_LocalMpointer(%eax)
    movl    Fr_HandlerRegister(%ebp),%ebx   # and handler pointer
    movl    %ebx,Arg_HandlerRegister(%eax)
    movl    %ebp,%esp                       # Restore C stack pointer
    subl    $12,%esp                        # Sp is just before the registers
    popl    %esi
    popl    %edi
    popl    %ebx
    popl    %ebp
    ret

callExHeap:
callheapOverflowLocal:
    CALL_EXTRA(RETURN_HEAP_OVERFLOW)
callExStack:
    CALL_EXTRA(RETURN_STACK_OVERFLOW)
callExStackEx:
    CALL_EXTRA(RETURN_STACK_OVERFLOWEX)
callExRaiseOverflow:
    CALL_EXTRA(RETURN_RAISE_OVERFLOW)

# As X86AsmSaveFullState but only save what is necessary for an RTS call.
INLINE_ROUTINE(X86AsmSaveStateAndReturn)
SaveStateAndReturnLocal: # This is necessary so that the jmps use a PC-relative address
    jmp SaveFullState
    
#if (0)
    pushl   %eax                # Save eax
    movl    PolyStack(%ebp),%eax
    movl    %ebx,EBX_OFF(%eax)
    movl    %edx,EDX_OFF(%eax)
    fstcw   FPREGS_OFF(%eax)
    fninit                     # Reset the FP state.
    fldcw   FPREGS_OFF(%eax)   # But reload the rounding mode
    popl    %ebx                # Get old eax value
    movl    %ebx,EAX_OFF(%eax)
    movl    %esp,SP_OFF(%eax)
    movb    $1,InRTS(%ebp)             # inRTS:=0 (stack now kosher)
    movl    SavedSp(%ebp),%esp
    popal
    ret
#endif

/* Used when entering new code.  The argument and closure are on the stack
   in case there is a GC before we enter the code. */
INLINE_ROUTINE(X86AsmPopArgAndClosure)
    popl    %edx
    popl    %eax
    jmp     *(%edx)

INLINE_ROUTINE(X86AsmRaiseException)
raisexlocal:
    movl    Fr_HandlerRegister(%ebp),%ecx    # Get next handler into %rcx
    jmp     *(%ecx)

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
#ifndef HOSTARCHITECTURE_X86_64
    movl    4(%esp),%eax
#else
    movl    %edi,%eax   # On X86_64 the argument is passed in %edi
#endif
# Use %ecx and %eax because they are volatile (unlike %ebx on X86/64/Unix)
    movl    $2,%ecx
    lock; xaddl %ecx,(%eax)
    addl    $2,%ecx
    movl    %ecx,%eax
    ret

