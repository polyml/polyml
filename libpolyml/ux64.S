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

  %Reax: First argument to function.  Result of function call.
  %Rebx: Second argument to function.
  %Recx: General register
  %Redx: Closure pointer in call.
  %Rebp: Points to memory used for extra registers
  %Resi: General register.
  %Redi: General register.
  %Resp: Stack pointer.
  %R8:   Third argument to function
  %R9:   Fourth argument to function
  %R10:  Fifth argument to function
  %R11:  General register
  %R12:  General register
  %R13:  General register
  %R14:  General register
  %R15:  Memory allocation pointer
*/


#include "config.h"
#if defined __MINGW64__
#  if defined POLY_LINKAGE_PREFIX
#    define EXTNAME(x) POLY_LINKAGE_PREFIX ## x
#  else
#    define EXTNAME(x) x
#  endif    
/* External names in older versions of FreeBSD have a leading underscore. */
#elif ! defined(__ELF__)
#define EXTNAME(x)  _##x
#else
#define EXTNAME(x)  x
#endif

#ifndef HOSTARCHITECTURE_X86_64
#define Reax        %eax
#define Rebx        %ebx
#define Recx        %ecx
#define Redx        %edx
#define Resi        %esi
#define Redi        %edi
#define Resp        %esp
#define Rebp        %ebp
#else
#define Reax        %rax
#define Rebx        %rbx
#define Recx        %rcx
#define Redx        %rdx
#define Resi        %rsi
#define Redi        %rdi
#define Resp        %rsp
#define Rebp        %rbp
#endif

#ifdef HOSTARCHITECTURE_X86_64
#define R8          %r8
#define R9          %r9
#define R10         %r10
#define R11         %r11
#define R12         %r12
#define R13         %r13
#define R14         %r14
#define R15         %r15
#endif

#ifdef HOSTARCHITECTURE_X86_64
#define MOVL         movq
#define MOVB         movb
#define ADDL         addq
#define SUBL         subq
#define XORL         xorq
#define ORL          orq
#define ANDL         andq
#define CMPL         cmpq
#define CMPB         cmpb
#define LEAL         leaq
#define SHRL         shrq
#define SARL         sarq
#define SHLL         shlq
#define TESTL        testq
#define IMULL        imulq
#define MULL         mulq
#define DIVL         divl
#define NEGL         negq
#define PUSHL        pushq
#define POPL         popq
#define POPFL        popfq
#define PUSHFL       pushfq
#define PUSHAL       pushaq
#define POPAL        popaq
#define LOCKXADDL    lock xaddq

# Return instructions for n arguments.  All these are in registers
#define RET0         ret
#define RET1         ret
#define RET2         ret
#define RET3         ret
#define RET4         ret
#define RET5         ret

#else
#define MOVL         movl
#define MOVB         movb
#define ADDL         addl
#define SUBL         subl
#define XORL         xorl
#define ORL          orl
#define ANDL         andl
#define CMPL         cmpl
#define CMPB         cmpb
#define LEAL         leal
#define SHRL         shrl
#define SARL         sarl
#define SHLL         shll
#define TESTL        testl
#define IMULL        imull
#define MULL         mull
#define DIVL         divl
#define NEGL         negl
#define PUSHL        pushl
#define POPL         popl
#define POPFL        popfl
#define PUSHFL       pushfl
#define PUSHAL       pushal
#define POPAL        popal
# Older versions of GCC require a semicolon here.
#define LOCKXADDL    lock; xaddl

# Return instructions for n arguments.
#define RET0         ret
#define RET1         ret
#define RET2         ret
#define RET3         ret $4
#define RET4         ret $8
#define RET5         ret $12

#endif

# Register mask entries - must match coding used in I386CODECONS.ML
#define     M_Reax  0x000001
#define     M_Recx  0x000002
#define     M_Redx  0x000004
#define     M_Rebx  0x000008
#define     M_Resi  0x000010
#define     M_Redi  0x000020

#ifdef HOSTARCHITECTURE_X86_64
#define     M_R8         64
#define     M_R9        128
#define     M_R10       256
#define     M_R11       512
#define     M_R12      1024
#define     M_R13      2048
#define     M_R14      4096
#endif
            # Floating point registers.
#define     M_FP0   0x002000
#define     M_FP1   0x004000
#define     M_FP2   0x008000
#define     M_FP3   0x010000
#define     M_FP4   0x020000
#define     M_FP5   0x040000
#define     M_FP6   0x080000
#define     M_FP7   0x100000

#define     Mask_all 0x1FFFFF

#define     RegMask(name,mask) \
.set        Mask_##name,    mask

#define     OR  |


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

.set    TAG,        1
.set    TAGSHIFT,   1
.set    TAGMULT,    (1 << TAGSHIFT)

#define TAGGED(i) ((i << TAGSHIFT) | TAG)
#define MAKETAGGED(from,to)     LEAL    TAG(,from,2),to


#ifdef HOSTARCHITECTURE_X86_64
.set    POLYWORDSIZE,8
#else
.set    POLYWORDSIZE,4
#endif

.set    NIL,        TAGGED(0)
.set    UNIT,       TAGGED(0)
.set    ZERO,       TAGGED(0)
.set    FALSE,      TAGGED(0)
.set    TRUE,       TAGGED(1)
.set    MINUS1,     TAGGED(0-1)
.set    B_bytes,    0x01
.set    B_mutable,  0x40
.set    B_mutablebytes, 0x41
#ifndef HOSTARCHITECTURE_X86_64
.set    Max_Length,0x00ffffff
#else
.set    Max_Length,0x00ffffffffffffff
#endif


/* The "memory registers" are pointed to by Rebp within the ML code
  The first few offsets are built into the compiled code.
  All the offsets are built into x86_dep.c . */
.set    LocalMpointer,0
#ifndef HOSTARCHITECTURE_X86_64
.set    HandlerRegister,4
.set    LocalMbottom,8
.set    ExceptionPacket,16
.set    RequestCode,20
.set    InRTS,21
.set    ReturnReason,22
.set    FullRestore,23
.set    PolyStack,24
.set    SavedSp,28
.set    ThreadId,60
.set    RealTemp,64
.set    RaiseOverflow,68
.set    Mr_ESP,72
#else
.set    HandlerRegister,8
.set    LocalMbottom,16
.set    ExceptionPacket,32
.set    RequestCode,40
.set    InRTS,41
.set    ReturnReason,42
.set    FullRestore,43
.set    PolyStack,48
.set    SavedSp,56
.set    HeapOverflow,64
.set    StackOverflow,72
.set    StackOverflowEx,80
.set    RaiseExEntry,88
.set    ThreadId,120
.set    RealTemp,128
.set    RaiseOverflow,136
.set    Mr_ESP,144
#endif

/* IO function numbers.  These are functions that are called
   to handle special cases in this code */
#include "sys.h"

#define RETURN_HEAP_OVERFLOW        1
#define RETURN_STACK_OVERFLOW       2
#define RETURN_STACK_OVERFLOWEX     3
#define RETURN_CALLBACK_RETURN      6
#define RETURN_CALLBACK_EXCEPTION   7
#define RETURN_RAISE_OVERFLOW       8

/*
  Stack format from objects.h is:
   typedef struct
   {                byte offset of start
     word  p_space ;            0 -- Now unused - remove
     byte *p_pc ;               4
     word *p_sp ;               8 -- Now unused - remove
     word *p_hr ;              12
     word  p_nreg ;            16  -- Now unused - remove
     word  p_reg(1) ;          20
   } StackObject ;
*/
 
/* Starting offsets */

#.set    SPACE_OFF,  0
#ifndef HOSTARCHITECTURE_X86_64
.set    PC_OFF,     4
.set    SP_OFF,     8
.set    EAX_OFF,    20
.set    EBX_OFF,    24
.set    ECX_OFF,    28
.set    EDX_OFF,    32
.set    ESI_OFF,    36
.set    EDI_OFF,    40
.set    FLAGS_OFF,  48
.set    FPREGS_OFF, 52
#else
.set    PC_OFF,     8
.set    SP_OFF,     16
# 32 is the count of the number of checked registers
.set    EAX_OFF,    40
.set    EBX_OFF,    48
.set    ECX_OFF,    56
.set    EDX_OFF,    64
.set    ESI_OFF,    72
.set    EDI_OFF,    80
.set    R8_OFF,     88
.set    R9_OFF,     96
.set    R10_OFF,    104
.set    R11_OFF,    112
.set    R12_OFF,    120
.set    R13_OFF,    128
.set    R14_OFF,    136
# 144 is the count of the number of unchecked registers
.set    FLAGS_OFF,  152
.set    FPREGS_OFF, 160
#endif

# Mark the stack as non-executable when compiling for Linux
#ifdef __linux__
#ifdef __ELF__
.section .note.GNU-stack, "", @progbits
#endif
#endif

#
# CODE STARTS HERE
#
    .text

/* Define standard call macro. CALL_IO ioCallNo  where ioCallNo is the io function to call.
   We need to include M_Redx in the register sets.  MD_set_for_retry may modify it
   if the function was called directly and not via the closure register. */

#define CALL_IO(index) \
        MOVB  $index,RequestCode(Rebp); \
        jmp   SaveStateAndReturnLocal;

#define CALL_EXTRA(index) \
        MOVB  $index,ReturnReason(Rebp); \
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
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    4(Resp),Recx                    # Argument - address of MemRegisters - goes into Rebp
    PUSHAL                                  # Save all the registers just to be safe
    MOVL    Resp,SavedSp(Recx)              # savedSp:=%Resp - Save the system stack pointer.

    MOVL    Recx,Rebp                       # Put address of MemRegisters where it belongs
#else
# The argument to the function is passed in Redi
    MOVL    Redi,Recx
    PUSHL   Rebp                             # Save callee--save registers
    PUSHL   Rebx
    PUSHL   R12
    PUSHL   R13
    PUSHL   R14
    PUSHL   R15
    MOVL    Resp,SavedSp(Recx)              # savedSp:=%Resp - Save the system stack pointer.
    MOVL    Recx,Rebp                       # Put address of MemRegisters where it belongs
#endif
    MOVL    PolyStack(Rebp),Reax
#ifdef HOSTARCHITECTURE_X86_64
    MOVL    LocalMpointer(Rebp),R15         # Set the heap pointer register
#endif
    MOVL    SP_OFF(Reax),Resp               # Set the new stack ptr
    PUSHL   PC_OFF(Reax)                    # Push the code address
    testb   $ 1,FullRestore(Rebp)       # Should we restore or clear the regs?
    jnz     sw2polyfull
/* We're returning from an RTS call.  We need to clear the registers we're
   not restoring so that they are valid if we GC.  We restore EDX and the
   argument regs because this may have been CallCode */
    MOVL    EBX_OFF(Reax),Rebx
    MOVL    $ ZERO,Recx
    MOVL    EDX_OFF(Reax),Redx
    MOVL    $ZERO,Resi
    MOVL    $ZERO,Redi
#ifdef HOSTARCHITECTURE_X86_64
    MOVL    R8_OFF(Reax),R8
    MOVL    R9_OFF(Reax),R9
    MOVL    R10_OFF(Reax),R10
    MOVL    $ZERO,R11
    MOVL    $ZERO,R12
    MOVL    $ZERO,R13
    MOVL    $ZERO,R14
#endif
    MOVL    EAX_OFF(Reax),Reax
    cld                                     # Clear this just in case
    MOVB    $0,InRTS(Rebp)             # inRTS:=0 (stack now kosher)
    ret                                     # Jump to code address

sw2polyfull:
    PUSHL   FLAGS_OFF(Reax)                 # Push the flags
    FRSTOR  FPREGS_OFF(Reax)
    MOVL    EBX_OFF(Reax),Rebx              # Load the registers
    MOVL    ECX_OFF(Reax),Recx
    MOVL    EDX_OFF(Reax),Redx
    MOVL    ESI_OFF(Reax),Resi
    MOVL    EDI_OFF(Reax),Redi
#ifdef HOSTARCHITECTURE_X86_64
    MOVL    R8_OFF(Reax),R8
    MOVL    R9_OFF(Reax),R9
    MOVL    R10_OFF(Reax),R10
    MOVL    R11_OFF(Reax),R11
    MOVL    R12_OFF(Reax),R12
    MOVL    R13_OFF(Reax),R13
    MOVL    R14_OFF(Reax),R14
#endif
    cld                                     # Clear this just in case
    MOVL    EAX_OFF(Reax),Reax
    POPFL                                   # reset flags
    MOVB    $0,InRTS(Rebp)             # inRTS:=0 (stack now kosher)
    ret                                     # Jump to code address


/* Code to save the state and switch to C
   This saves the full register state. */
SaveFullState:
    PUSHFL                      # Save flags
    PUSHL   Reax                # Save eax
    MOVL    PolyStack(Rebp),Reax
    MOVL    Rebx,EBX_OFF(Reax)
    MOVL    Recx,ECX_OFF(Reax)
    MOVL    Redx,EDX_OFF(Reax)
    MOVL    Resi,ESI_OFF(Reax)
    MOVL    Redi,EDI_OFF(Reax)
    FNSAVE  FPREGS_OFF(Reax)          # Save FP state.  Also resets the state so...
    FLDCW   FPREGS_OFF(Reax)          # ...load because we need the same rounding mode in the RTS
#ifdef HOSTARCHITECTURE_X86_64
    MOVL    R8,R8_OFF(Reax)
    MOVL    R9,R9_OFF(Reax)
    MOVL    R10,R10_OFF(Reax)
    MOVL    R11,R11_OFF(Reax)
    MOVL    R12,R12_OFF(Reax)
    MOVL    R13,R13_OFF(Reax)
    MOVL    R14,R14_OFF(Reax)
    MOVL    R15,LocalMpointer(Rebp)  # Save the heap pointer
#endif
    POPL    Rebx                # Get old eax value
    MOVL    Rebx,EAX_OFF(Reax)
    POPL    Rebx
    MOVL    Rebx,FLAGS_OFF(Reax)
    MOVL    Resp,SP_OFF(Reax)
    MOVB    $1,InRTS(Rebp)             # inRTS:=0 (stack now kosher)
    MOVL    SavedSp(Rebp),Resp
#ifndef HOSTARCHITECTURE_X86_64
    POPAL
#else
    POPL    R15                            # Restore callee-save registers
    POPL    R14
    POPL    R13
    POPL    R12
    POPL    Rebx
    POPL    Rebp
#endif
    ret

# As X86AsmSaveFullState but only save what is necessary for an RTS call.
	INLINE_ROUTINE(X86AsmSaveStateAndReturn)
SaveStateAndReturnLocal: # This is necessary so that the jmps use a PC-relative address

    PUSHL   Reax                # Save eax
    MOVL    PolyStack(Rebp),Reax
    MOVL    Rebx,EBX_OFF(Reax)
    MOVL    Redx,EDX_OFF(Reax)
    FSTCW   FPREGS_OFF(Reax)
    FNINIT                     # Reset the FP state.
    FLDCW   FPREGS_OFF(Reax)   # But reload the rounding mode
#ifdef HOSTARCHITECTURE_X86_64
    MOVL    R8,R8_OFF(Reax)
    MOVL    R9,R9_OFF(Reax)
    MOVL    R10,R10_OFF(Reax)
    MOVL    R15,LocalMpointer(Rebp)  # Save the heap pointer
#endif
    POPL    Rebx                # Get old eax value
    MOVL    Rebx,EAX_OFF(Reax)
    MOVL    Resp,SP_OFF(Reax)
    MOVB    $1,InRTS(Rebp)             # inRTS:=0 (stack now kosher)
    MOVL    SavedSp(Rebp),Resp
#ifndef HOSTARCHITECTURE_X86_64
    POPAL
#else
    POPL    R15                            # Restore callee-save registers
    POPL    R14
    POPL    R13
    POPL    R12
    POPL    Rebx
    POPL    Rebp
#endif
    ret

/* CallcodeTupled.  This is currently only used to "fold" RTS functions that are applied
   to constant arguments.  Oddly, compared with other RTS functions, it takes a single
   argument that is a pair containing the function closure and an argument vector. */
	INLINE_ROUTINE(callcodeTupled)
    MOVL    (Reax),Redx                         # closure
    MOVL    POLYWORDSIZE(Reax),Resi            # address of arg vector
    CMPL    $NIL,Resi                      # If calling a function without args this could be nil
    je      cct2
    MOVL    $Max_Length,Recx               # Mask for length removing any flags
    ANDL    (-POLYWORDSIZE)(Resi),Recx          # Load and mask length
    jz      cct2
    MOVL    (Resi),Reax                         # First argument
    ADDL    $POLYWORDSIZE,Resi
    SUBL    $1,Recx
    jz      cct2
    MOVL    (Resi),Rebx                         # Second argument
    ADDL    $POLYWORDSIZE,Resi
    SUBL    $1,Recx
    jz      cct2
#ifdef HOSTARCHITECTURE_X86_64
    MOVL    (Resi),R8                           # Third argument
    ADDL    $POLYWORDSIZE,Resi
    SUBL    $1,Recx
    jz      cct2
    MOVL    (Resi),R9                           # Fourth argument
    ADDL    $POLYWORDSIZE,Resi
    SUBL    $1,Recx
    jz      cct2
    MOVL    (Resi),R10                          # Fifth argument
    ADDL    $POLYWORDSIZE,Resi
    SUBL    $1,Recx
    jz      cct2
#endif
    POPL    Redi                                # Get the return address
cct1:                                           # Push the remaining args to the stack
    PUSHL   (Resi)
    ADDL    $POLYWORDSIZE,Resi
    LOOP    cct1
    PUSHL   Redi                                # Push the return address
cct2:                                           # Finished - enter function
    MOVL    $UNIT,Resi                     # Clobber, for the moment
    MOVL    $UNIT,Recx
    jmp     *(Redx)



	RegMask(callcodeTupled,Mask_all)             # All because we're calling an unknown function
/*
A number of functions implemented in Assembly for efficiency reasons
*/

	INLINE_ROUTINE(int_to_word)
 # Extract the low order bits from a word.
    TESTL   $TAG,Reax
    jz      get_first_long_word_a1
    ret                 # Return the argument
	RegMask(int_to_word,(M_Reax))

 # This is now used in conjunction with isShort in Word.fromInt.
	INLINE_ROUTINE(get_first_long_word_a)
get_first_long_word_a1:
    testb   $16,(-1)(Reax)     # 16 is the "negative" bit
    MOVL    (Reax),Reax     # Extract the word which is already little-endian
    jz      gfw1
    NEGL    Reax            # We can ignore overflow
gfw1:
	MAKETAGGED(Reax,Reax)
    ret
	RegMask(get_first_long_word,(M_Reax))



	INLINE_ROUTINE(move_bytes)
/* Move a segment of memory from one location to another.
   Must deal with the case of overlapping segments correctly.
   (source, sourc_offset, destination, dest_offset, length) */

 # Assume that the offsets and length are all short integers.
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    12(Resp),Redi               # Destination address
    MOVL    8(Resp),Recx                # Destination offset, untagged
#else
    MOVL    R8,Redi               # Destination address
    MOVL    R9,Recx                # Destination offset, untagged
#endif
    SHRL    $TAGSHIFT,Recx
    ADDL    Recx,Redi
    MOVL    Reax,Resi                   # Source address
    SHRL    $TAGSHIFT,Rebx
    ADDL    Rebx,Resi
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    4(Resp),Recx                # Get the length to move
#else
    MOVL    R10,Recx                # Get the length to move
#endif
    SHRL    $TAGSHIFT,Recx
    cld                             # Default to increment Redi,Resi
    CMPL    Redi,Resi                   # Check for potential overlap
 # If dest > src then use decrementing moves else
 # use incrementing moves.
    ja      mvb1
    std                             # Decrement Redi,Resi
    LEAL    (-1)(Resi,Recx),Resi
    LEAL    (-1)(Redi,Recx),Redi
mvb1:
    rep; movsb                      # Copy the bytes
    MOVL    $UNIT,Reax              # The function returns unit
    MOVL    Reax,Rebx               # Clobber bad value in %rbx
    MOVL    Reax,Recx               # and %Recx
    MOVL    Reax,Redi
    MOVL    Reax,Resi
 # Visual Studio 5 C++ seems to assume that the direction flag
 # is cleared.  I think that`s a bug but we have to go along with it.
    cld
#ifndef HOSTARCHITECTURE_X86_64
    ret     $12
#else
    ret
#endif

	RegMask(move_bytes,Mask_all)


	INLINE_ROUTINE(move_words)
/* Move a segment of memory from one location to another.
   Must deal with the case of overlapping segments correctly.
   (source, source_offset, destination, dest_offset, length)
   Assume that the offsets and length are all short integers. */
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    12(Resp),Redi               # Destination address
    MOVL    8(Resp),Recx                # Destination offset
    LEAL    (-2)(Redi,Recx,2),Redi      # Destination address plus offset
    LEAL    (-2)(Reax,Rebx,2),Resi      # Source address plus offset
    MOVL    4(Resp),Recx                # Get the length to move (words)
#else
    MOVL    R8,Redi               # Destination address
    MOVL    R9,Recx                # Destination offset
    LEAL    (-4)(Redi,Recx,4),Redi      # Destination address plus offset
    LEAL    (-4)(Reax,Rebx,4),Resi      # Source address plus offset
    MOVL    R10,Recx                # Get the length to move (words)
#endif
    SHRL    $TAGSHIFT,Recx
    cld                             # Default to increment Redi,Resi
    CMPL    Redi,Resi                   # Check for potential overlap
 # If dest > src then use decrementing moves else
 # use incrementing moves.
    ja      mvw1
    std                             # Decrement Redi,Resi

    LEAL    (-POLYWORDSIZE)(Resi,Recx,POLYWORDSIZE),Resi
    LEAL    (-POLYWORDSIZE)(Redi,Recx,POLYWORDSIZE),Redi

mvw1:
#ifndef HOSTARCHITECTURE_X86_64
    rep; movsl                           # Copy the words
#else
    rep; movsq                           # Copy the words
#endif
    MOVL    $UNIT,Reax             # The function returns unit
    MOVL    Reax,Recx               # Clobber bad values
    MOVL    Reax,Redi
    MOVL    Reax,Resi
 # Visual Studio 5 C++ seems to assume that the direction flag
 # is cleared.  I think that`s a bug but we have to go along with it.
    cld
#ifndef HOSTARCHITECTURE_X86_64
    ret     $12
#else
    ret
#endif

	RegMask(move_words,Mask_all)
#

RetFalse:
    MOVL    $FALSE,Reax
    ret

RetTrue:
    MOVL    $TRUE,Reax
    ret

	INLINE_ROUTINE(not_bool)
    XORL    $(TRUE-TAG),Reax   # Change the value but leave the tag
    ret
	RegMask(not_bool,(M_Reax))

 # or, and, xor shift etc. assume the values are tagged integers
	INLINE_ROUTINE(or_word)
    ORL     Rebx,Reax
    ret
	RegMask(or_word,(M_Reax))

	INLINE_ROUTINE(and_word)
    ANDL    Rebx,Reax
    ret
	RegMask(and_word,(M_Reax))

	INLINE_ROUTINE(xor_word)
    XORL    Rebx,Reax
    ORL     $TAG,Reax  # restore the tag
    ret
	RegMask(xor_word,(M_Reax))

	INLINE_ROUTINE(shift_left_word)
 # Assume that both args are tagged integers
 # Word.<<(a,b) is defined to return 0 if b > Word.wordSize
#ifndef HOSTARCHITECTURE_X86_64
    CMPL    $TAGGED(31),Rebx
#else
    CMPL    $TAGGED(63),Rebx
#endif
    jb      slw1
    MOVL    $ZERO,Reax
    ret
slw1:
    MOVL    Rebx,Recx
    SHRL    $TAGSHIFT,Recx # remove tag
    SUBL    $TAG,Reax
    SHLL    %cl,Reax
    ORL     $TAG,Reax  # restore the tag
    MOVL    Reax,Recx       # clobber %Recx
    ret
	RegMask(shift_left_word,(M_Reax OR M_Recx))

	INLINE_ROUTINE(shift_right_word)
 # Word.>>(a,b) is defined to return 0 if b > Word.wordSize
#ifndef HOSTARCHITECTURE_X86_64
    CMPL    $TAGGED(31),Rebx
#else
    CMPL    $TAGGED(63),Rebx
#endif
    jb      srw1
    MOVL    $ZERO,Reax
    ret
srw1:
    MOVL    Rebx,Recx
    SHRL    $TAGSHIFT,Recx # remove tag
    SHRL    %cl,Reax
    ORL     $TAG,Reax  # restore the tag
    MOVL    Reax,Recx       # clobber %Recx
    ret
	RegMask(shift_right_word,(M_Reax OR M_Recx))

	INLINE_ROUTINE(shift_right_arith_word)
 # Word.~>>(a,b) is defined to return 0 or ~1 if b > Word.wordSize
 # The easiest way to do that is to set the shift to 31.
#ifndef HOSTARCHITECTURE_X86_64
    CMPL    $TAGGED(31),Rebx
#else
    CMPL    $TAGGED(63),Rebx
#endif
    jb      sra1
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    $TAGGED(31),Rebx
#else
    MOVL    $TAGGED(63),Rebx
#endif
sra1:
    MOVL    Rebx,Recx
    SHRL    $TAGSHIFT,Recx # remove tag
    SARL    %cl,Reax
    ORL     $TAG,Reax  # restore the tag
    MOVL    Reax,Recx       # clobber %Recx
    ret
	RegMask(shift_right_arith_word,(M_Reax OR M_Recx))

# Clears the "mutable" bit on a segment
	INLINE_ROUTINE(locksega)
    andb    $(0xff-B_mutable),-1(Reax)
    MOVL     $TAGGED(0),Reax   # Return Unit,
    ret
	RegMask(lockseg,M_Reax)

	INLINE_ROUTINE(get_length_a)
    MOVL    (-POLYWORDSIZE)(Reax),Reax
    SHLL    $8,Reax            # Clear top byte
    SHRL    $(8-TAGSHIFT),Reax  # Make it a tagged integer
    ORL $TAG,Reax
    ret
	RegMask(get_length,(M_Reax))


	INLINE_ROUTINE(is_shorta)
# Returns true if the argument is tagged
    ANDL    $TAG,Reax
    jz      RetFalse
    jmp     RetTrue
	RegMask(is_short,(M_Reax))

	INLINE_ROUTINE(string_length)
    TESTL   $TAG,Reax  # Single char strings are represented by the
    jnz     RetOne      # character.
    MOVL    (Reax),Reax # Get length field
	MAKETAGGED(Reax,Reax)
    ret
RetOne: MOVL    $TAGGED(1),Reax
    ret
	RegMask(string_length,(M_Reax))

 # Store the length of a string in the first word.
	INLINE_ROUTINE(set_string_length_a)
    SHRL    $TAGSHIFT,Rebx # Untag the length
    MOVL    Rebx,(Reax)
    MOVL    $UNIT,Reax     # Return unit
    MOVL    Reax,Rebx           # Clobber untagged value
    ret
	RegMask(set_string_length,(M_Reax OR M_Rebx))

# raisex (formerly raisexn) is used by compiled code.
	INLINE_ROUTINE(raisex)
    MOVL    HandlerRegister(Rebp),Recx    # Get next handler into %rcx
    jmp     *(Recx)

	INLINE_ROUTINE(load_byte)
    MOVL    Rebx,Redi
    SHRL    $TAGSHIFT,Redi
#ifndef HOSTARCHITECTURE_X86_64
    movzbl  (Reax,Redi,1),Redi
#else
    movzbq  (Reax,Redi,1),Redi
#endif
	MAKETAGGED(Redi,Reax)
    MOVL    Reax,Redi       # Clobber bad value in %Redi
    ret
	RegMask(load_byte,(M_Reax OR M_Redi))

	INLINE_ROUTINE(load_word)
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    (-2)(Reax,Rebx,2),Reax
#else
    MOVL    (-4)(Reax,Rebx,4),Reax
#endif
    MOVL    Reax,Rebx
    ret
	RegMask(load_word,(M_Reax))

	INLINE_ROUTINE(assign_byte)
# We can assume that the data value will not overflow 30 bits (it is only 1 byte!)
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    4(Resp),Recx
#else
    MOVL    R8,Recx
#endif
    SHRL    $TAGSHIFT,Recx       # Remove tags from data value

# We can assume that the index will not overflow 30 bits i.e. it is a tagged short
    SHRL    $TAGSHIFT,Rebx     # Remove tags from offset
    MOVB    %cl,(Reax,Rebx)

    MOVL    $UNIT,Reax             # The function returns unit
    MOVL    Reax,Rebx                   # Clobber bad value in %Rebx
    MOVL    Reax,Recx                   # and %Recx
#ifndef HOSTARCHITECTURE_X86_64
    ret     $4
#else
    ret
#endif
	RegMask(assign_byte,(M_Reax OR M_Rebx OR M_Recx))


	INLINE_ROUTINE(assign_word)
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    4(Resp),Recx
    MOVL    Recx,(-2)(Reax,Rebx,2)
#else
    MOVL    R8,(-4)(Reax,Rebx,4)      # The offset is tagged already
#endif
    MOVL    $UNIT,Reax           # The function returns unit
#ifndef HOSTARCHITECTURE_X86_64
    ret     $4
	RegMask(assign_word,(M_Reax OR M_Recx))
#else
    ret
	RegMask(assign_word,(M_Reax))
#endif

/* Allocate a piece of memory that does not need to be initialised.
   We can't actually risk leaving word objects uninitialised so for the
   moment we always initialise. */
	INLINE_ROUTINE(alloc_uninit)
#ifdef HOSTARCHITECTURE_X86_64
    MOVL    $ZERO,R8
#else
    POP     Recx         # Get the return address
    PUSHL   $ZERO   # Push the initial value - zero
    PUSHL   Recx         # Restore the return address
#endif
# Drop through into alloc_store

#ifndef HOSTARCHITECTURE_X86_64
	RegMask(alloc_uninit,Mask_all) # All, because we may call RTS
#else
	RegMask(alloc_uninit,Mask_all) # All, because we may call RTS
#endif

/* alloc(size, flags, initial).  Allocates a segment of a given size and
  initialises it.
 
  This is primarily used for arrays and for strings.  Refs are
  allocated using inline code. */
	INLINE_ROUTINE(alloc_store)
allsts:
 # First check that the length is acceptable
    TESTL   $TAG,Reax
    jz      alloc_in_rts            # Get the RTS to raise an exception
    MOVL    Reax,Redi
    SHRL    $TAGSHIFT,Redi     # Remove tag
#ifndef HOSTARCHITECTURE_X86_64
    CMPL    $Max_Length,Redi   # Length field must fit in 24 bits
#else
    MOVL    $Max_Length,Redx   # Length field must fit in 56 bits
    CMPL    Redx,Redi
#endif
    ja      alloc_in_rts            # Get the RTS to raise an exception
#ifndef HOSTARCHITECTURE_X86_64
    INCL    Redi                    # Add 1 word
    SHLL    $2,Redi            # Get length in bytes
    MOVL    LocalMpointer(Rebp),Redx
#else
    ADDL    $1,Redi            # Add 1 word
    SHLL    $3,Redi            # Get length in bytes
    MOVL    R15,Redx
#endif
    SUBL    Redi,Redx               # Allocate the space
    MOVL    Reax,Redi               # Clobber bad value in Redi
    CMPL    LocalMbottom(Rebp),Redx            # Check for free space
    jb      alloc_in_rts
/* Normally the above test is sufficient but if LocalMpointer is near the bottom of
   memory and the store requested is very large the value in Redx can be negative
   which is greater, unsigned, than LocalMbottom.  We have to check it is less
   than, unsigned, the allocation pointer. */
#ifndef HOSTARCHITECTURE_X86_64
    CMPL    LocalMpointer(Rebp),Redx
    jnb     alloc_in_rts
    MOVL    Redx,LocalMpointer(Rebp)             # Put back in the heap ptr
#else
    CMPL    R15,Redx
    jnb     alloc_in_rts
    MOVL    Redx,R15                 # Put back in the heap ptr
#endif
    SHRL    $TAGSHIFT,Reax
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    Reax,(-4)(Redx)         # Put in length
#else
    MOVL    Reax,(-8)(Redx)         # Put in length
#endif
    SHRL    $TAGSHIFT,Rebx     # remove tag from flag
    ORL     $B_mutable,Rebx    # set mutable bit
    MOVB    %bl,(-1)(Redx)         # and put it in.
 # Initialise the store.
    MOVL    Reax,Recx               # Get back the no. of words.
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    4(Resp),Reax            # Get initial value.
#else
    MOVL    R8,Reax                 # Get initial value.
#endif
    CMPL    $B_mutablebytes,Rebx
    jne     allst2

 # If this is a byte seg
    SHRL    $TAGSHIFT,Reax # untag the initialiser
#ifndef HOSTARCHITECTURE_X86_64
    SHLL    $2,Recx        # Convert to bytes
#else
    SHLL    $3,Recx        # Convert to bytes
#endif
    MOVL    Redx,Redi
    rep; stosb
    jmp     allst3

 # If this is a word segment
allst2:
    MOVL    Redx,Redi
#ifndef HOSTARCHITECTURE_X86_64
    rep; stosl
#else
    rep; stosq
#endif

allst3:
    MOVL    Redx,Reax

    MOVL    Reax,Recx       # Clobber these
    MOVL    Reax,Redx
    MOVL    Reax,Rebx
    MOVL    Reax,Redi
#ifndef HOSTARCHITECTURE_X86_64
    ret     $4
#else
    ret
#endif
	RegMask(alloc_store,Mask_all) # All, because we may use RTS call

# This is used if we have reached the store limit and need to garbage-collect.
alloc_in_rts:
    MOVL    Reax,Redx       # Clobber these first
    MOVL    Reax,Redi
	CALL_IO(POLY_SYS_alloc_store)

	INLINE_ROUTINE(touch_final)
# This is really a pseudo-op
    MOVL    $UNIT,Reax
    ret
	RegMask(touch_final,(M_Reax))

	INLINE_ROUTINE(add_long)
    MOVL    Reax,Redi
    ANDL    Rebx,Redi
    ANDL    $TAG,Redi
    jz      add_really_long
    LEAL    (-TAG)(Reax),Redi
    ADDL    Rebx,Redi
    jo      add_really_long
    MOVL    Redi,Reax
    ret
add_really_long:
    MOVL    Reax,Redi
	CALL_IO(POLY_SYS_aplus)
	RegMask(aplus,(M_Reax OR M_Redi OR Mask_all))

	INLINE_ROUTINE(sub_long)
    MOVL    Reax,Redi
    ANDL    Rebx,Redi
    ANDL    $TAG,Redi
    jz      sub_really_long
    MOVL    Reax,Redi
    SUBL    Rebx,Redi
    jo      sub_really_long
    LEAL    TAG(Redi),Reax      # Put back the tag
    MOVL    Reax,Redi
    ret
sub_really_long:
    MOVL    Reax,Redi
	CALL_IO(POLY_SYS_aminus)
	RegMask(aminus,(M_Reax OR M_Redi OR Mask_all))

	INLINE_ROUTINE(mult_long)
    MOVL    Reax,Redi
    ANDL    Rebx,Redi
    ANDL    $TAG,Redi
    jz      mul_really_long
    MOVL    Rebx,Redi
    SARL    $TAGSHIFT,Redi # Shift multiplicand
    MOVL    Reax,Resi
    SUBL    $TAG,Resi          # Just subtract off the tag off multiplier
    IMULL   Redi,Resi
    jo      mul_really_long
    ADDL    $TAG,Resi
    MOVL    Resi,Reax
    MOVL    Reax,Redi
    ret
mul_really_long:
    MOVL    Reax,Resi       # Clobber this
    MOVL    Reax,Redi
	CALL_IO(POLY_SYS_amul)
	RegMask(amul,(M_Reax OR M_Redi OR M_Resi OR Mask_all))

	INLINE_ROUTINE(div_long)
    MOVL    Reax,Redi
    ANDL    Rebx,Redi
    ANDL    $TAG,Redi          # %Redi now contains $0 or $1 (both legal!)
    jz      div_really_long
    CMPL    $TAGGED(0),Rebx    # Check that it's non-zero
    jz      div_really_long         # We don't want a trap.
 # The only case of overflow is dividing the smallest negative number by -1
    CMPL    $TAGGED((-1)),Rebx
    jz      div_really_long
    SARL    $TAGSHIFT,Reax
    MOVL    Rebx,Redi
    SARL    $TAGSHIFT,Redi
#ifndef HOSTARCHITECTURE_X86_64
    cdq
#else
    cqo
#endif
    idiv    Redi
	MAKETAGGED(Reax,Reax)
    MOVL    Reax,Redx
    MOVL    Reax,Redi
    ret
div_really_long:
    MOVL    Reax,Redi
	CALL_IO(POLY_SYS_adiv)
	RegMask(adiv,(M_Reax OR M_Redi OR M_Redx OR Mask_all))

	INLINE_ROUTINE(rem_long)
    MOVL    Reax,Redi
    ANDL    Rebx,Redi
    ANDL    $TAG,Redi      # %Redi now contains $0 or $1 (both legal!
    jz      rem_really_long
    CMPL    $TAGGED(0),Rebx    # Check that it's non-zero
    jz      rem_really_long         # We don't want a trap.
 # The only case of overflow is dividing the smallest negative number by -1
    CMPL    $TAGGED((-1)),Rebx
    jz      rem_really_long
    SARL    $TAGSHIFT,Reax
    MOVL    Rebx,Redi
    SARL    $TAGSHIFT,Redi
#ifndef HOSTARCHITECTURE_X86_64
    cdq
#else
    cqo
#endif
    idiv    Redi
	MAKETAGGED(Redx,Reax)
    MOVL    Reax,Redx
    MOVL    Reax,Redi
    ret
rem_really_long:
    MOVL    Reax,Redi
	CALL_IO(POLY_SYS_amod)
	RegMask(amod,(M_Reax OR M_Redi OR M_Redx OR Mask_all))

/* Combined quotient and remainder.  We have to use the long form
   if the arguments are long or there's an overflow.  The first two
   arguments are the values to be divided.  The third argument is the
   address where the results should be placed. */
	INLINE_ROUTINE(quotrem_long)
    MOVL    Reax,Redi
    ANDL    Rebx,Redi
    ANDL    $TAG,Redi
    jz      quotrem_really_long
    CMPL    $TAGGED(0),Rebx
    jz      quotrem_really_long
 # The only case of overflow is dividing the smallest negative number by -1
    CMPL    $TAGGED((-1)),Rebx
    jz      quotrem_really_long

 # Get the address for the result.
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    4(Resp),Recx
#else
    MOVL    R8,Recx
#endif
# Do the division
    SARL    $TAGSHIFT,Reax
    MOVL    Rebx,Redi
    SARL    $TAGSHIFT,Redi
#ifndef HOSTARCHITECTURE_X86_64
    cdq
#else
    cqo
#endif
    idiv    Redi
	MAKETAGGED(Reax,Reax)
	MAKETAGGED(Redx,Redx)
    MOVL    Reax,Redi
    MOVL    Reax,(Recx)
    MOVL    Redx,POLYWORDSIZE(Recx)
    MOVL    Recx,Reax
#ifndef HOSTARCHITECTURE_X86_64
    ret     $4
#else
    ret
#endif

mem_for_remquot1:  # Not enough store: clobber bad value in ecx.
    MOVL   $1,Recx

quotrem_really_long:
    MOVL    Reax,Redi
	CALL_IO(POLY_SYS_quotrem)
	RegMask(quotrem,(M_Reax OR M_Redi OR M_Redx OR Mask_all))

# TODO: Isn't this byte equality?
	INLINE_ROUTINE(equal_long)
    CMPL    Reax,Rebx
    je      RetTrue
    MOVL    Reax,Recx   # If either is short
    ORL     Rebx,Reax   # the result is false
    ANDL    $TAG,Reax
    jnz     RetFalse
    MOVL    Recx,Reax
	CALL_IO(POLY_SYS_equala)
	RegMask(equala,(M_Reax OR M_Recx OR Mask_all))


	INLINE_ROUTINE(or_long)
#ifdef NOTATTHEMOMENT
    MOVL    Reax,Redi
    ANDL    Rebx,Redi
    ANDL    $TAG,Redi
    jz      or_really_long
    ORL     Rebx,Reax
    MOVL    Reax,Redi
    ret
or_really_long:
#endif
	CALL_IO(POLY_SYS_ora)
	RegMask(ora,(M_Reax OR M_Redi OR Mask_all))

	INLINE_ROUTINE(xor_long)
#ifdef NOTATTHEMOMENT
    MOVL    Reax,Redi
    ANDL    Rebx,Redi
    ANDL    $TAG,Redi
    jz      xor_really_long
    XORL    Rebx,Reax
    ORL     $TAG,Reax  # restore the tag
    MOVL    Reax,Redi
    ret
xor_really_long:
#endif
	CALL_IO(POLY_SYS_xora)
	RegMask(xora,(M_Reax OR M_Redi OR Mask_all))

	INLINE_ROUTINE(and_long)
#ifdef NOTATTHEMOMENT
    MOVL    Reax,Redi
    ANDL    Rebx,Redi
    ANDL    $TAG,Redi
    jz      and_really_long
    ANDL    Rebx,Reax
    MOVL    Reax,Redi
    ret
and_really_long:
#endif
	CALL_IO(POLY_SYS_anda)
	RegMask(anda,(M_Reax OR M_Redi OR Mask_all))

	INLINE_ROUTINE(neg_long)
    TESTL   $TAG,Reax
    jz      neg_really_long
    MOVL    $(TAGGED(0)+TAG),Redi
    SUBL    Reax,Redi
    jo      neg_really_long
    MOVL    Redi,Reax
    ret
neg_really_long:
    MOVL    Reax,Redi
	CALL_IO(POLY_SYS_aneg)
	RegMask(aneg,(M_Reax OR M_Redi OR Mask_all))

	INLINE_ROUTINE(int_geq)
    TESTL   $TAG,Reax # Is first arg short?
    jz      igeq2
    TESTL   $TAG,Rebx # Is second arg short?
    jz      igeq1
    CMPL    Rebx,Reax
    jge     RetTrue
    jmp     RetFalse
igeq1:
 # First arg is short, second isn't
    testb   $16,(-1)(Rebx)     # 16 is the "negative" bit
    jnz     RetTrue     # Negative - always less
    jmp     RetFalse

igeq2:
 # First arg is long
    TESTL   $TAG,Rebx # Is second arg short?
    jz      igeq3
 # First arg is long, second is short
    testb   $16,(-1)(Reax)     # 16 is the "negative" bit
    jz      RetTrue    # Positive - always greater
    jmp     RetFalse

igeq3:
 # Both long
	CALL_IO(POLY_SYS_int_geq)
	RegMask(int_geq,(M_Reax OR Mask_all))


	INLINE_ROUTINE(int_leq)
    TESTL   $TAG,Reax # Is first arg short?
    jz      ileq2
    TESTL   $TAG,Rebx # Is second arg short?
    jz      ileq1
    CMPL    Rebx,Reax
    jle     RetTrue
    jmp     RetFalse
ileq1:
 # First arg is short, second isn't
    testb   $16,(-1)(Rebx)     # 16 is the "negative" bit
    jz      RetTrue     # Negative - always less
    jmp     RetFalse

ileq2:
 # First arg is long
    TESTL   $TAG,Rebx # Is second arg short?
    jz      ileq3
 # First arg is long, second is short
    testb   $16,(-1)(Reax)     # 16 is the "negative" bit
    jnz     RetTrue    # Positive - always greater
    jmp     RetFalse

ileq3:
	CALL_IO(POLY_SYS_int_leq)
	RegMask(int_leq,(M_Reax OR M_Recx OR Mask_all))


	INLINE_ROUTINE(int_gtr)
    TESTL   $TAG,Reax # Is first arg short?
    jz      igtr2
    TESTL   $TAG,Rebx # Is second arg short?
    jz      igtr1
    CMPL    Rebx,Reax
    jg      RetTrue
    jmp     RetFalse
igtr1:
 # First arg is short, second isn't
    testb   $16,(-1)(Rebx)     # 16 is the "negative" bit
    jnz     RetTrue     # Negative - always less
    jmp     RetFalse

igtr2:
 # First arg is long
    TESTL   $TAG,Rebx # Is second arg short?
    jz      igtr3
 # First arg is long, second is short
    testb   $16,(-1)(Reax)     # 16 is the "negative" bit
    jz      RetTrue    # Positive - always greater
    jmp     RetFalse

igtr3:
	CALL_IO(POLY_SYS_int_gtr)
	RegMask(int_gtr,(M_Reax OR M_Recx OR Mask_all))


	INLINE_ROUTINE(int_lss)
    TESTL   $TAG,Reax # Is first arg short?
    jz      ilss2
    TESTL   $TAG,Rebx # Is second arg short?
    jz      ilss1
    CMPL    Rebx,Reax
    jl      RetTrue
    jmp     RetFalse
ilss1:
 # First arg is short, second isn't
    testb   $16,(-1)(Rebx)     # 16 is the "negative" bit
    jz      RetTrue     # Negative - always less
    jmp     RetFalse

ilss2:
 # First arg is long
    TESTL   $TAG,Rebx # Is second arg short?
    jz      ilss3
 # First arg is long, second is short
    testb   $16,(-1)(Reax)     # 16 is the "negative" bit
    jnz     RetTrue    # Positive - always greater
    jmp     RetFalse

ilss3:
	CALL_IO(POLY_SYS_int_lss)
	RegMask(int_lss,(M_Reax OR M_Recx OR Mask_all))

	INLINE_ROUTINE(offset_address)
 # This is needed in the code generator, but is a very risky thing to do.
    SHRL    $TAGSHIFT,Rebx     # Untag
    ADDL    Rebx,Reax       # and add in
    MOVL    Reax,Rebx
    ret
	RegMask(offset_address,(M_Reax OR M_Rebx))

# General test routine.  Returns with the condition codes set
# appropriately.

teststr:
    TESTL   $TAG,Reax     # Is arg1 short
    jz      tststr1
    TESTL   $TAG,Rebx     # Yes: is arg2 also short?
    jz      tststr0a
    # Both are short - just compare the characters
    CMPL    Rebx,Reax
    ret

tststr0a:
    MOVL    $1,Redi        # Is arg2 the null string ?
    CMPL    (Rebx),Redi
    jg      tststr4            # Return with "gtr" set if it is
    SHRL    $TAGSHIFT,Reax
    CMPB    POLYWORDSIZE(Rebx),%al
    jne     tststr4            # If they're not equal that's the result
    CMPL    $256,Reax     # But if they're equal set "less" because A is less than B
    jmp     tststr4

tststr1: # arg2 is not short.  Is arg1 ?
    TESTL   $TAG,Rebx
    jz      tststr2
    MOVL    (Reax),Redi        # Is arg1 the null string
    CMPL    $1,Redi
    jl      tststr4            # Return with "less" set if it is
    SHRL    $TAGSHIFT,Rebx
    MOVB    POLYWORDSIZE(Reax),%cl
    CMPB    %bl,%cl
    jne     tststr4            # If they're not equal that's the result
    CMPL    $0,Redi      # But if they're equal set "greater" because A is greater than B
    jmp     tststr4

tststr2:
    MOVL    (Reax),Redi     # Get length.
    MOVL    (Rebx),Recx     # 
    CMPL    Recx,Redi       # Find shorter length
    jge     tststr3
    MOVL    Redi,Recx
tststr3:
    LEAL    POLYWORDSIZE(Reax),Resi    # Load ptrs for cmpsb
    LEAL    POLYWORDSIZE(Rebx),Redi
    cld                 # Make sure we increment
    CMPL    Reax,Reax       # Set the Zero bit
    repe; cmpsb           # Compare while equal and %rcx > 0
    jnz     tststr4
 # Strings are equal as far as the shorter of the two.  Have to compare
 # the lengths.
    MOVL    (Reax),Redi
    CMPL    (Rebx),Redi
tststr4:
    MOVL    $1,Reax      # Clobber these
    MOVL    Reax,Rebx       
    MOVL    Reax,Recx       
    MOVL    Reax,Resi
    MOVL    Reax,Redi
    ret

 # These functions compare strings for lexical ordering.  This version, at
 # any rate, assumes that they are UNSIGNED bytes.

	INLINE_ROUTINE(str_compare)
    call    teststr
    ja      RetTrue         # Return TAGGED(1) if it's greater
    je      RetFalse        # Return TAGGED(0) if it's equal
    MOVL    $MINUS1,Reax   # Return TAGGED(-1) if it's less.
    ret
	RegMask(str_compare,(M_Reax OR M_Recx OR M_Redi OR M_Resi))


	INLINE_ROUTINE(teststrgeq)
    call    teststr
    jnb     RetTrue
    jmp     RetFalse
	RegMask(teststrgeq,(M_Reax OR M_Recx OR M_Redi OR M_Resi))

	INLINE_ROUTINE(teststrleq)
    call    teststr
    jna     RetTrue
    jmp     RetFalse
	RegMask(teststrleq,(M_Reax OR M_Recx OR M_Redi OR M_Resi))

	INLINE_ROUTINE(teststrlss)
    call    teststr
    jb      RetTrue
    jmp     RetFalse
	RegMask(teststrlss,(M_Reax OR M_Recx OR M_Redi OR M_Resi))

	INLINE_ROUTINE(teststrgtr)
    call    teststr
    ja      RetTrue
    jmp     RetFalse
	RegMask(teststrgtr,(M_Reax OR M_Recx OR M_Redi OR M_Resi))


	INLINE_ROUTINE(bytevec_eq)
/* Compare arrays of bytes.  The arguments are the same as move_bytes.
   (source, sourc_offset, destination, dest_offset, length) */

/* Assume that the offsets and length are all short integers. */
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    12(Resp),Redi               # Destination address
    MOVL    8(Resp),Recx                # Destination offset, untagged
#else
    MOVL    R8,Redi                     # Destination address
    MOVL    R9,Recx                     # Destination offset, untagged
#endif
    SHRL    $TAGSHIFT,Recx
    ADDL    Recx,Redi
    MOVL    Reax,Resi                   # Source address
    SHRL    $TAGSHIFT,Rebx
    ADDL    Rebx,Resi
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    4(Resp),Recx                # Get the length to move
#else
    MOVL    R10,Recx                    # Get the length to move
#endif
    SHRL    $TAGSHIFT,Recx
    cld                     # Make sure we increment
    CMPL    Reax,Reax       # Set the Zero bit
    repe; cmpsb
    MOVL    Reax,Resi       # Make these valid
    MOVL    Reax,Recx
    MOVL    Reax,Redi
    jz      bvTrue
    MOVL    $FALSE,Reax
    jmp     bvRet
bvTrue:
    MOVL    $TRUE,Reax
bvRet:
#ifndef HOSTARCHITECTURE_X86_64
    ret     $12
#else
    ret
#endif
	RegMask(bytevec_eq,(M_Reax OR M_Recx OR M_Redi OR M_Resi))


	INLINE_ROUTINE(is_big_endian)
    jmp     RetFalse    # I386/486 is little-endian
	RegMask(is_big_endian,(M_Reax))

	INLINE_ROUTINE(bytes_per_word)
    MOVL    $TAGGED(POLYWORDSIZE),Reax  # 4/8 bytes per word
    ret
	RegMask(bytes_per_word,(M_Reax))

 # Word functions.  These are all unsigned and do not raise Overflow
 
	INLINE_ROUTINE(mul_word)
    SHRL    $TAGSHIFT,Rebx # Untag the multiplier
    SUBL    $TAG,Reax      # Remove the tag from the multiplicand
    MULL    Rebx                # unsigned multiplication
    ADDL    $TAG,Reax      # Add back the tag, but don`t shift
    MOVL    Reax,Redx           # clobber this which has the high-end result
    MOVL    Reax,Rebx           # and the other bad result.
    ret
	RegMask(mul_word,(M_Reax OR M_Rebx OR M_Redx))

	INLINE_ROUTINE(plus_word)
    LEAL    (-TAG)(Reax,Rebx),Reax  # Add the values and subtract a tag
    ret
	RegMask(plus_word,(M_Reax))

	INLINE_ROUTINE(minus_word)
    SUBL    Rebx,Reax
    ADDL    $TAG,Reax          # Put back the tag
    ret
	RegMask(minus_word,(M_Reax))

	INLINE_ROUTINE(div_word)
    SHRL    $TAGSHIFT,Rebx     # Check for division by zero is done in ML
    SHRL    $TAGSHIFT,Reax
    MOVL    $0,Redx
    div     Rebx
	MAKETAGGED(Reax,Reax)
    MOVL    Reax,Redx
    MOVL    Reax,Rebx
    ret
	RegMask(div_word,(M_Reax OR M_Rebx OR M_Redx))

	INLINE_ROUTINE(mod_word)
    SHRL    $TAGSHIFT,Rebx # Check for division by zero is done in ML
    SHRL    $TAGSHIFT,Reax
    MOVL    $0,Redx
    div     Rebx
	MAKETAGGED(Redx,Reax)
    MOVL    Reax,Redx
    MOVL    Reax,Rebx
    ret
	RegMask(mod_word,(M_Reax OR M_Rebx OR M_Redx))

	INLINE_ROUTINE(word_eq)
    CMPL    Rebx,Reax
    jz      RetTrue         # True if they are equal.
    jmp     RetFalse
	RegMask(word_eq,(M_Reax))

	INLINE_ROUTINE(word_geq)
    CMPL    Rebx,Reax
    jnb     RetTrue
    jmp     RetFalse
	RegMask(word_geq,(M_Reax))

	INLINE_ROUTINE(word_leq)
    CMPL    Rebx,Reax
    jna     RetTrue
    jmp     RetFalse
	RegMask(word_leq,(M_Reax))

	INLINE_ROUTINE(word_gtr)
    CMPL    Rebx,Reax
    ja      RetTrue
    jmp     RetFalse
	RegMask(word_gtr,(M_Reax))

 	INLINE_ROUTINE(word_lss)
    CMPL    Rebx,Reax
    jb      RetTrue
    jmp     RetFalse
	RegMask(word_lss,(M_Reax))

	INLINE_ROUTINE(fixed_geq)
    CMPL    Rebx,Reax
    jge     RetTrue
    jmp     RetFalse
	RegMask(fixed_geq,(M_Reax))

	INLINE_ROUTINE(fixed_leq)
    CMPL    Rebx,Reax
    jle     RetTrue
    jmp     RetFalse
	RegMask(fixed_leq,(M_Reax))

	INLINE_ROUTINE(fixed_gtr)
    CMPL    Rebx,Reax
    jg      RetTrue
    jmp     RetFalse
	RegMask(fixed_gtr,(M_Reax))

	INLINE_ROUTINE(fixed_lss)
    CMPL    Rebx,Reax
    jl      RetTrue
    jmp     RetFalse
	RegMask(fixed_lss,(M_Reax))

	INLINE_ROUTINE(fixed_add)
	LEAL    (-TAG)(Reax),Reax
	ADDL	Rebx,Reax
    jo      raiseOverflowEx
	ret
	RegMask(fixed_add,(M_Reax))

	INLINE_ROUTINE(fixed_sub)
	SUBL	Rebx,Reax
	jo      raiseOverflowEx
	ADDL    $TAG,Reax      # Put back the tag
	ret
	RegMask(fixed_sub,(M_Reax))

	INLINE_ROUTINE(fixed_mul)
    SARL    $TAGSHIFT,Rebx # Untag the multiplier
    SUBL    $TAG,Reax      # Remove the tag from the multiplicand
    IMULL   Rebx,Reax           # signed multiplication
	jo      raiseOverflowEx
    ADDL    $TAG,Reax      # Add back the tag, but don`t shift
    MOVL    Reax,Redx           # clobber this which has the high-end result
    MOVL    Reax,Rebx           # and the other bad result.
    ret
	RegMask(fixed_mul,(M_Reax OR M_Rebx OR M_Redx))

	INLINE_ROUTINE(fixed_quot)
# Checking for overflow and zero is done in ML
    SARL    $TAGSHIFT,Rebx
    SARL    $TAGSHIFT,Reax
#ifndef HOSTARCHITECTURE_X86_64
    cdq
#else
    cqo
#endif
    idiv    Rebx
	MAKETAGGED(Reax,Reax)
    MOVL    Reax,Redx
    MOVL    Reax,Rebx
    ret
	RegMask(fixed_quot,(M_Reax OR M_Rebx OR M_Redx))

	INLINE_ROUTINE(fixed_rem)
    SARL    $TAGSHIFT,Rebx
    SARL    $TAGSHIFT,Reax
#ifndef HOSTARCHITECTURE_X86_64
    cdq
#else
    cqo
#endif
    idiv     Rebx
	MAKETAGGED(Redx,Reax)
    MOVL    Reax,Redx
    MOVL    Reax,Rebx
    ret
	RegMask(fixed_rem,(M_Reax OR M_Rebx OR M_Redx))

# TODO: This needs to be fixed.
	INLINE_ROUTINE(fixed_div)
# Checking for overflow and zero is done in ML
    SARL    $TAGSHIFT,Rebx
    SARL    $TAGSHIFT,Reax
#ifndef HOSTARCHITECTURE_X86_64
    cdq
#else
    cqo
#endif
    idiv    Rebx
	CMPL	$0,Redx
	jz		fixed_div1		# If the remainder if non-zero ...
	XORL	Redx,Rebx		# and has a different sign from the divisor ...
	jns		fixed_div1
	SUBL	$1,Reax	# subtract one to round to -infinity rather than zero.
fixed_div1:
	MAKETAGGED(Reax,Reax)
    MOVL    Reax,Redx
    MOVL    Reax,Rebx
    ret
	RegMask(fixed_div,(M_Reax OR M_Rebx OR M_Redx))

	INLINE_ROUTINE(fixed_mod)
    SARL    $TAGSHIFT,Rebx
    SARL    $TAGSHIFT,Reax
#ifndef HOSTARCHITECTURE_X86_64
    cdq
#else
    cqo
#endif
    idiv     Rebx
# Result is in Redx.  We have to change the result so that it has the sign as the divisor.
	CMPL	$0,Redx
	jz      fixed_mod1	# Result is zero - no change
	XORL    Redx,Rebx
	jns		fixed_mod1	# Skip if they had the same signs
	XORL	Redx,Rebx	# Restore the original divisor
	ADDL	Rebx,Redx	# And add it in
fixed_mod1:
	MAKETAGGED(Redx,Reax)
    MOVL    Reax,Redx
    MOVL    Reax,Rebx
    ret
	RegMask(fixed_mod,(M_Reax OR M_Rebx OR M_Redx))

raiseOverflowEx:
# Build the exception packet.
# Allocate four word item.
# Set first word to TAGGED(5)
# Set second word to the string "Overflow" - That's going to need allocation.
# Set third and fourth words to TAGGED(0)
# Raise the exception.
    jmp     *RaiseOverflow(Rebp)

# Atomically increment the value at the address of the arg and return the
# updated value.  Since the xadd instruction returns the original value
# we have to increment it.
	INLINE_ROUTINE(atomic_increment)
atomic_incr:                    # Internal name in case "atomic_increment" is munged.
    MOVL    $2,Rebx
    LOCKXADDL Rebx,(Reax)
    ADDL    $2,Rebx
    MOVL    Rebx,Reax
    ret

	RegMask(atomic_incr,(M_Reax OR M_Rebx))

# Atomically decrement the value at the address of the arg and return the
# updated value.  Since the xadd instruction returns the original value
# we have to decrement it.
	INLINE_ROUTINE(atomic_decrement)
atomic_decr:
    MOVL    $-2,Rebx
    LOCKXADDL Rebx,(Reax)
    MOVL    Rebx,Reax
    SUBL    $2,Reax
    ret

	RegMask(atomic_decr,(M_Reax OR M_Rebx))

/* Reset a mutex to (tagged) one.  Because the increment and decrements
   are atomic this doesn't have to do anything special. */
	INLINE_ROUTINE(atomic_reset)
    MOVL    $3,(Reax)
    MOVL    $UNIT,Reax  # The function returns unit
    ret

	RegMask(atomic_reset,M_Reax)

# Return the thread id object for the current thread
	INLINE_ROUTINE(thread_self)
    MOVL    ThreadId(Rebp),Reax
    ret
	RegMask(thread_self,(M_Reax))



/* Memory for LargeWord.word values.  This is the same as mem_for_real on
  64-bits but only a single word on 32-bits.
  ********************************
  Some of this code is temporary.  The final version should compute the result and
  simply jump here to box it.  That requires the heap-overflow code to save the
  registers across the trap but not to examine them for pointers.  Temporarily we
  don't do that but instead clear all the registers across a trap. */
mem_for_largeword:
#ifndef HOSTARCHITECTURE_X86_64
        MOVL    LocalMpointer(Rebp),Recx
        SUBL    $8,Recx        # Length word (4 bytes) + 4 bytes
#ifdef TEST_ALLOC
# Test case - this will always force a call into RTS.
        CMPL    LocalMpointer(Rebp),Recx
#else
        CMPL    LocalMbottom(Rebp),Recx
#endif
        jnb      mem_for_largeword1
/* ********************************
  Temporarily: push these registers to the stack
  and pop them afterwards.  This isn't the final version of this code
  but is useful as a test. */
		PUSHL	 Reax
		PUSHL    Rebx
        call	 X86AsmCallExtraRETURN_HEAP_OVERFLOW
		POPL     Rebx
		POPL     Reax
mem_for_largeword1:
        MOVL    Recx,LocalMpointer(Rebp) # Updated allocation pointer
        MOVL    $0x01000001,(-4)(Recx)     # Length word
        ret
#endif
# Else if it is 64-bits just drop through

# FLOATING POINT

mem_for_real:
# Allocate memory for the result.
#ifndef HOSTARCHITECTURE_X86_64
        MOVL    LocalMpointer(Rebp),Recx
        SUBL    $12,Recx        # Length word (4 bytes) + 8 bytes
#else
        MOVL    R15,Recx
        SUBL    $16,Recx        # Length word (8 bytes) + 8 bytes
#endif
#ifdef TEST_ALLOC
# Test case - this will always force a call into RTS.
        CMPL    LocalMpointer(Rebp),Recx
#else
        CMPL    LocalMbottom(Rebp),Recx
#endif
        jnb      mem_for_real1
/* ********************************
  Temporarily: push these registers to the stack
  and pop them afterwards.  This isn't the final version of this code
  but is useful as a test. */
		PUSHL	 Reax
		PUSHL    Rebx
        call	 X86AsmCallExtraRETURN_HEAP_OVERFLOW
		POPL     Rebx
		POPL     Reax
mem_for_real1:
#ifndef HOSTARCHITECTURE_X86_64
        MOVL    Recx,LocalMpointer(Rebp) # Updated allocation pointer
        MOVL    $0x01000002,(-4)(Recx)     # Two words plus tag
#else
        MOVL    Recx,R15                        # Updated allocation pointer
        MOVL    $1,(-8)(Recx)      # One word
        MOVB    $B_bytes,(-1)(Recx)    # Set the byte flag.
#endif
        ret


	INLINE_ROUTINE(real_add)
        call    mem_for_real
/* Do the operation and put the result in the allocated space. */
    FLDL    (Reax)
    FADDL   (Rebx)
    FSTPL   (Recx)
    MOVL    Recx,Reax
    ret

# The mask includes FP7 rather than FP0 because this pushes a value which
# overwrites the bottom of the stack.
	RegMask(real_add,(M_Reax OR M_Recx OR M_Redx OR M_FP7))



	INLINE_ROUTINE(real_sub)
        call    mem_for_real
# Do the operation and put the result in the allocated
# space.
    FLDL    (Reax)
    FSUBL   (Rebx)
    FSTPL   (Recx)
    MOVL    Recx,Reax
    ret

	RegMask(real_sub,(M_Reax OR M_Recx OR M_Redx OR M_FP7))


	INLINE_ROUTINE(real_mul)
        call    mem_for_real
# Do the operation and put the result in the allocated
# space.
    FLDL    (Reax)
    FMULL   (Rebx)
    FSTPL   (Recx)
    MOVL    Recx,Reax
    ret

	RegMask(real_mul,(M_Reax OR M_Recx OR M_Redx OR M_FP7))


	INLINE_ROUTINE(real_div)
        call    mem_for_real
# Do the operation and put the result in the allocated
# space.
    FLDL    (Reax)
    FDIVL   (Rebx)
    FSTPL   (Recx)
    MOVL    Recx,Reax
    ret

	RegMask(real_div,(M_Reax OR M_Recx OR M_Redx OR M_FP7))


# For all values except NaN it's possible to do this by a test such as
# "if x < 0.0 then ~ x else x" but the test always fails for NaNs

	INLINE_ROUTINE(real_abs)
    MOVL	Reax,Rebx                # Put a valid value in Rebx
    call    mem_for_real
# Do the operation and put the result in the allocated
# space.
# N.B. Real.~ X is not the same as 0.0 - X.  Real.~ 0.0 is ~0.0;
    FLDL    (Reax)
    FABS
    FSTPL   (Recx)
    MOVL    Recx,Reax
    ret

	RegMask(real_abs,(M_Reax OR M_Rebx OR M_Recx OR M_Redx OR M_FP7)          # Temporarily include Rebx)


	INLINE_ROUTINE(real_neg)
    MOVL	Reax,Rebx                # Put a valid value in Rebx
    call    mem_for_real
# Do the operation and put the result in the allocated
# space.
# N.B. Real.~ X is not the same as 0.0 - X.  Real.~ 0.0 is ~0.0;
    FLDL    (Reax)
    FCHS
    FSTPL   (Recx)
    MOVL    Recx,Reax
    ret

	RegMask(real_neg,(M_Reax OR M_Rebx OR M_Recx OR M_Redx OR M_FP7)         # Temporarily include Rebx)



	INLINE_ROUTINE(real_eq)
    FLDL    (Reax)
    FCOMPL  (Rebx)
    FNSTSW  %ax
# Not all 64-bit processors support SAHF.
# The result is true if the zero flag is set and parity flag clear.  
    ANDL    $17408,Reax        # 0x4400
    CMPL    $16384,Reax        # 0x4000
    je      RetTrue
    jmp     RetFalse
	RegMask(real_eq,(M_Reax OR M_FP7))


	INLINE_ROUTINE(real_neq)
    FLDL    (Reax)
    FCOMPL  (Rebx)
    FNSTSW  %ax
    ANDL    $17408,Reax        # 0x4400
    CMPL    $16384,Reax        # 0x4000
    jne     RetTrue
    jmp     RetFalse

	RegMask(real_neq,(M_Reax OR M_FP7))


	INLINE_ROUTINE(real_lss)
# Compare Rebx > Reax
    FLDL    (Rebx)
    FCOMPL  (Reax)
    FNSTSW  %ax

# True if the carry flag (C0), zero flag (C3) and parity (C2) are all clear
    ANDL    $17664,Reax # 0x4500
    je      RetTrue
    jmp     RetFalse

	RegMask(real_lss,(M_Reax OR M_FP7))


	INLINE_ROUTINE(real_gtr)
    FLDL    (Reax)
    FCOMPL  (Rebx)
    FNSTSW  %ax

# True if the carry flag (C0), zero flag (C3) and parity (C2) are all clear
    ANDL    $17664,Reax    # 0x4500
    je      RetTrue
    jmp     RetFalse

	RegMask(real_gtr,(M_Reax OR M_FP7))


	INLINE_ROUTINE(real_leq)
# Compare Rebx > Reax
    FLDL    (Rebx)
    FCOMPL  (Reax)
    FNSTSW  %ax
# True if the carry flag (C0) and parity (C2) are both clear
    ANDL    $1280,Reax # 0x500
    je      RetTrue
    jmp     RetFalse

	RegMask(real_leq,(M_Reax OR M_FP7))


	INLINE_ROUTINE(real_geq)
    FLDL    (Reax)
    FCOMPL  (Rebx)
    FNSTSW  %ax
# True if the carry flag (C0) and parity (C2) are both clear
    ANDL    $1280,Reax # 0x500
    je      RetTrue
    jmp     RetFalse

	RegMask(real_geq,(M_Reax OR M_FP7))

	INLINE_ROUTINE(real_from_int)
    TESTL   $TAG,Reax   # Is it long ?
    jz      real_float_1
    MOVL	Reax,Rebx                # Put a valid value in Rebx
    call    mem_for_real
    SARL    $TAGSHIFT,Reax # Untag the value
    MOVL    Reax,RealTemp(Rebp) # Save it in a temporary (N.B. It's now untagged)
#ifdef HOSTARCHITECTURE_X86_64
    FILDQ   RealTemp(Rebp)
#else
    FILDL   RealTemp(Rebp)
#endif
    FSTPL   (Recx)
    MOVL    Recx,Reax
    ret

real_float_1:
    	CALL_IO(POLY_SYS_int_to_real)

	RegMask(real_from_int,(M_Reax OR M_Recx OR M_Redx OR M_FP7 OR Mask_all))

	INLINE_ROUTINE(fixed_to_real)
    MOVL	Reax,Rebx                # Put a valid value in Rebx
    call    mem_for_real
    SARL    $TAGSHIFT,Reax # Untag the value
    MOVL    Reax,RealTemp(Rebp) # Save it in a temporary (N.B. It's now untagged)
#ifdef HOSTARCHITECTURE_X86_64
    FILDQ   RealTemp(Rebp)
#else
    FILDL   RealTemp(Rebp)
#endif
    FSTPL   (Recx)
    MOVL    Recx,Reax
    ret

	RegMask(fixed_to_real,(M_Reax OR M_Rebx OR M_Recx OR M_Redx OR M_FP7)    # Temporarily include Rebx)

	INLINE_ROUTINE(set_exception_trace)
/* The argument is the closure to call.  The return address is already on the stack.
   We need to push the addresses of some code.  To make it position-independent we use
   CALLs in a non-standard way. */
	MOVL	Reax,Redx					# Target closure
	PUSHL	HandlerRegister(Rebp)		# Previous handler
	CALL    setexct1                    # Jump to setexct1 pushing the next addr to the stack
    /* This is the code that is called if we get an exception.
       The exception packet is the first argument. */
    MOVB    $POLY_SYS_give_ex_trace_fn,RequestCode(Rebp)
    jmp     X86AsmSaveStateAndReturn
    #
setexct1:
    MOVL    Resp,HandlerRegister(Rebp)  # Set up the handler
    CALL    setexct2                    # Jump to setexct2 pushing the next addr to the stack
    # This is the code that is called if we return without raising an exception
    ADDL    $POLYWORDSIZE,Resp     # Remove handler
    POPL    HandlerRegister(Rebp)
    RET
setexct2:
    MOVL    $UNIT,Reax             # The function takes a unit arg.
    jmp     *(Redx)

# Additional assembly code routines

# RTS call to kill the current thread. 
	INLINE_ROUTINE(X86AsmKillSelf)
        MOVB    $POLY_SYS_kill_self,RequestCode(Rebp)
        jmp     X86AsmSaveStateAndReturn

	INLINE_ROUTINE(X86AsmCallbackReturn)
        MOVB    $RETURN_CALLBACK_RETURN,ReturnReason(Rebp)
        jmp     X86AsmSaveStateAndReturn

	INLINE_ROUTINE(X86AsmCallbackException)
        MOVB    $RETURN_CALLBACK_EXCEPTION,ReturnReason(Rebp)
        jmp     X86AsmSaveStateAndReturn

# This implements atomic addition in the same way as atomic_increment
	INLINE_ROUTINE(X86AsmAtomicIncrement)
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    4(Resp),Reax
#else
    MOVL    Redi,Reax   # On X86_64 the argument is passed in Redi
#endif
# Use Recx and Reax because they are volatile (unlike Rebx on X86/64/Unix)
    MOVL    $2,Recx
    LOCKXADDL Recx,(Reax)
    ADDL    $2,Recx
    MOVL    Recx,Reax
    ret

# LargeWord.word operations.  These are 32 or 64-bit values in a single-word byte
# memory cell.
	INLINE_ROUTINE(eq_longword)
    MOVL    (Reax),Reax
    CMPL    (Rebx),Reax
    jz      RetTrue         # True if they are equal.
    jmp     RetFalse
	RegMask(eq_longword,(M_Reax))

	INLINE_ROUTINE(geq_longword)

    MOVL    (Reax),Reax
    CMPL    (Rebx),Reax
    jnb     RetTrue
    jmp     RetFalse
	RegMask(geq_longword,(M_Reax))

	INLINE_ROUTINE(leq_longword)
    MOVL    (Reax),Reax
    CMPL    (Rebx),Reax
    jna     RetTrue
    jmp     RetFalse
	RegMask(leq_longword,(M_Reax))

	INLINE_ROUTINE(gt_longword)
    MOVL    (Reax),Reax
    CMPL    (Rebx),Reax
    ja      RetTrue
    jmp     RetFalse
	RegMask(gt_longword,(M_Reax))

	INLINE_ROUTINE(lt_longword)
    MOVL    (Reax),Reax
    CMPL    (Rebx),Reax
    jb      RetTrue
    jmp     RetFalse
	RegMask(lt_longword,(M_Reax))

	INLINE_ROUTINE(longword_to_tagged)
# Load the value and tag it, discarding the top bit
    MOVL    (Reax),Reax
    	MAKETAGGED(Reax,Reax)
    ret
	RegMask(longword_to_tagged,(M_Reax))

	INLINE_ROUTINE(signed_to_longword)
# Shift the value to remove the tag and store it.
	MOVL	Reax,Rebx					# mem_for_largeword may push rebx
    call    mem_for_largeword
    SARL    $TAGSHIFT,Reax         # Arithmetic shift, preserve sign
    MOVL    Reax,(Recx)
    MOVL    Recx,Reax
    ret
	RegMask(signed_to_longword,(M_Reax OR M_Rebx OR M_Recx)   # Temporarily include Rebx)

	INLINE_ROUTINE(unsigned_to_longword)
# Shift the value to remove the tag and store it.
	MOVL	Reax,Rebx					# mem_for_largeword may push rebx
    call    mem_for_largeword
    SHRL    $TAGSHIFT,Reax         # Logical shift, zero top bit
    MOVL    Reax,(Recx)
    MOVL    Recx,Reax
    ret
	RegMask(unsigned_to_longword,(M_Reax OR M_Rebx OR M_Recx)   # Temporarily include Rebx)

	INLINE_ROUTINE(plus_longword)
    call    mem_for_largeword
    MOVL    (Reax),Reax
    ADDL    (Rebx),Reax
    MOVL    Reax,(Recx)
    MOVL    Recx,Reax
    ret
	RegMask(plus_longword,(M_Reax OR M_Recx))

	INLINE_ROUTINE(minus_longword)
    call    mem_for_largeword
    MOVL    (Reax),Reax
    SUBL    (Rebx),Reax
    MOVL    Reax,(Recx)
    MOVL    Recx,Reax
    ret
	RegMask(minus_longword,(M_Reax OR M_Recx))

	INLINE_ROUTINE(mul_longword)
    call    mem_for_largeword
    MOVL    (Reax),Reax
    MULL    (Rebx)
    MOVL    Reax,(Recx)
    MOVL    Recx,Reax
    MOVL    Reax,Redx           # clobber this which has the high-end result
    ret
	RegMask(mul_longword,(M_Reax OR M_Recx OR M_Redx))

	INLINE_ROUTINE(div_longword)
# Check for division by zero is done in ML
    call    mem_for_largeword
    MOVL    (Reax),Reax
    MOVL    $0,Redx
    DIVL    (Rebx)
    MOVL    Reax,(Recx)         # Store the quotient
    MOVL    Recx,Reax
    MOVL    Reax,Redx           # clobber this which has the remainder
    ret

	RegMask(div_longword,(M_Reax OR M_Recx OR M_Redx))

	INLINE_ROUTINE(mod_longword)
# Check for division by zero is done in ML
    call    mem_for_largeword
    MOVL    (Reax),Reax
    MOVL    $0,Redx
    DIVL    (Rebx)
    MOVL    Redx,(Recx)         # Store the remainder
    MOVL    Recx,Reax
    MOVL    Reax,Redx           # clobber this which has the remainder
    ret

	RegMask(mod_longword,(M_Reax OR M_Recx OR M_Redx))

	INLINE_ROUTINE(andb_longword)
    call    mem_for_largeword
    MOVL    (Reax),Reax
    ANDL    (Rebx),Reax
    MOVL    Reax,(Recx)
    MOVL    Recx,Reax
    ret

	RegMask(andb_longword,(M_Reax OR M_Recx))

	INLINE_ROUTINE(orb_longword)
    call    mem_for_largeword
    MOVL    (Reax),Reax
    ORL     (Rebx),Reax
    MOVL    Reax,(Recx)
    MOVL    Recx,Reax
    ret

	RegMask(orb_longword,(M_Reax OR M_Recx))

	INLINE_ROUTINE(xorb_longword)
    call    mem_for_largeword
    MOVL    (Reax),Reax
    XORL    (Rebx),Reax
    MOVL    Reax,(Recx)
    MOVL    Recx,Reax
    ret

	RegMask(xorb_longword,(M_Reax OR M_Recx))

	INLINE_ROUTINE(shift_left_longword)
    call    mem_for_largeword
    MOVL    Recx,Redx           # We need Recx for the shift
 # The shift value is always a Word.word value i.e. tagged
 # LargeWord.<<(a,b) is defined to return 0 if b > LargeWord.wordSize
#ifndef HOSTARCHITECTURE_X86_64
    CMPL    $TAGGED(32),Rebx
#else
    CMPL    $TAGGED(64),Rebx
#endif
    jb      sllw1
    MOVL    $0,Reax
    jmp     sllw2
sllw1:
    MOVL    Rebx,Recx
    SHRL    $TAGSHIFT,Recx # remove tag
    MOVL    (Reax),Reax
    SHLL    %cl,Reax
sllw2:
    MOVL    Reax,(Redx)
    MOVL    Redx,Reax
    MOVL    Reax,Recx           # Clobber Recx
    ret
	RegMask(shift_left_longword,(M_Reax OR M_Recx OR M_Redx))

	INLINE_ROUTINE(shift_right_longword)
    call    mem_for_largeword
    MOVL    Recx,Redx           # We need Recx for the shift
/* The shift value is always a Word.word value i.e. tagged
   LargeWord.>>(a,b) is defined to return 0 if b > LargeWord.wordSize */
#ifndef HOSTARCHITECTURE_X86_64
    CMPL    $TAGGED(32),Rebx
#else
    CMPL    $TAGGED(64),Rebx
#endif
    jb      srlw1
    MOVL    $0,Reax
    jmp     srlw2
srlw1:
    MOVL    Rebx,Recx
    SHRL    $TAGSHIFT,Recx # remove tag
    MOVL    (Reax),Reax
    SHRL    %cl,Reax
srlw2:
    MOVL    Reax,(Redx)
    MOVL    Redx,Reax
    MOVL    Reax,Recx           # Clobber Recx
    ret
	RegMask(shift_right_longword,(M_Reax OR M_Recx OR M_Redx))

	INLINE_ROUTINE(shift_right_arith_longword)
    call    mem_for_largeword
    MOVL    Recx,Redx           # We need Recx for the shift
/* The shift value is always a Word.word value i.e. tagged
   LargeWord.~>>(a,b) is defined to return 0 or ~1 if b > LargeWord.wordSize */
#ifndef HOSTARCHITECTURE_X86_64
    CMPL    $TAGGED(32),Rebx
#else
    CMPL    $TAGGED(64),Rebx
#endif
    jb      sralw1
    # Setting the shift to 31/63 propagates the sign bit
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    $TAGGED(31),Rebx
#else
    MOVL    $TAGGED(63),Rebx
#endif
sralw1:
    MOVL    Rebx,Recx
    SHRL    $TAGSHIFT,Recx # remove tag
    MOVL    (Reax),Reax
    SARL    %cl,Reax
    MOVL    Reax,(Redx)
    MOVL    Redx,Reax
    MOVL    Reax,Recx           # Clobber Recx
    ret
	RegMask(shift_right_arith_longword,(M_Reax OR M_Rebx OR M_Recx OR M_Redx))

# C-memory operations.
	INLINE_ROUTINE(cmem_load_asm_8)
    MOVL    (Reax),Reax             # The address is boxed.
    SARL    $TAGSHIFT,Rebx     # The offset is a signed tagged value
    ADDL    Rebx,Reax               # Add it in
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    4(Resp),Rebx            # Get the index.
#else
    MOVL    R8,Rebx                 # Get the index.
#endif
    SARL    $TAGSHIFT,Rebx     # That's also tagged
#ifndef HOSTARCHITECTURE_X86_64
    movzbl  (Reax,Rebx,1),Reax
#else
    movzbq  (Reax,Rebx,1),Reax
#endif
	MAKETAGGED(Reax,Reax)
    MOVL    Reax,Rebx       # Clobber bad value in %Rebx
    RET3
	RegMask(cmem_load_8,(M_Reax OR M_Rebx))

	INLINE_ROUTINE(cmem_load_asm_16)
    MOVL    (Reax),Reax             # The address is boxed.
    SARL    $TAGSHIFT,Rebx     # The offset is a signed tagged value
    ADDL    Rebx,Reax               # Add it in
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    4(Resp),Rebx            # Get the index.
#else
    MOVL    R8,Rebx                 # Get the index.
#endif
    # The index is tagged but since we want to multiply by two we don't need anything here.
#ifndef HOSTARCHITECTURE_X86_64
    movzwl  -1(Reax,Rebx,1),Reax
#else
    movzwq  -1(Reax,Rebx,1),Reax
#endif
	MAKETAGGED(Reax,Reax)
    RET3
	RegMask(cmem_load_16,(M_Reax OR M_Rebx))

	INLINE_ROUTINE(cmem_load_asm_32)
#ifdef HOSTARCHITECTURE_X86_64
# 64-bit mode - the result is tagged
    MOVL    (Reax),Reax             # The address is boxed.
    SARL    $TAGSHIFT,Rebx     # The offset is a signed tagged value
    ADDL    Rebx,Reax               # Add it in
    movl    -2(Reax,R8,2),%eax
	MAKETAGGED(Reax,Reax)
    MOVL    Reax,Rebx       # Clobber bad value in %Rebx
    RET3

	RegMask(cmem_load_32,(M_Reax OR M_Rebx))

#else
# 32-bit mode - the result is boxed
    call    mem_for_largeword
    MOVL    (Reax),Reax             # The address is boxed.
    SARL    $TAGSHIFT,Rebx     # The offset is a signed tagged value
    ADDL    Rebx,Reax               # Add it in
    MOVL    4(Resp),Rebx            # Get the index.
    MOVL    (-2)(Reax,Rebx,2),Reax
    MOVL    Reax,(Recx)             # Save in the new memory
    MOVL    Recx,Reax               # Copy the result address
    RET3

	RegMask(cmem_load_32,(M_Reax OR M_Rebx OR M_Recx))
#endif

	INLINE_ROUTINE(cmem_load_asm_64 # The result is boxed in 64-bit mode. Not implemented in 32-bit mode)
#ifdef HOSTARCHITECTURE_X86_64
    PUSHL	R8						# Save R8 which isn't saved in mem_for_largeword
    call    mem_for_largeword
	POPL	R8
    MOVL    (Reax),Reax             # The address is boxed.
    SARL    $TAGSHIFT,Rebx     # The offset is a signed tagged value
    ADDL    Rebx,Reax               # Add it in
    MOVL    (-4)(Reax,R8,4),Reax
    MOVL    Reax,(Recx)             # Save in the new memory
    MOVL    Recx,Reax               # Copy the result address
    MOVL    Reax,Rebx               # Clobber bad value
    RET3

	RegMask(cmem_load_64,(M_Reax OR M_Rebx OR M_Recx))
#endif

	INLINE_ROUTINE(cmem_load_asm_float)
#ifdef HOSTARCHITECTURE_X86_64
    PUSHL	R8						# Save R8 which isn't saved in mem_for_largeword
#endif
    call    mem_for_real
#ifdef HOSTARCHITECTURE_X86_64
    POPL	R8						# Save R8 which isn't saved in mem_for_largeword
#endif
    MOVL    (Reax),Reax             # The address is boxed.
    SARL    $TAGSHIFT,Rebx     # The offset is a signed tagged value
    ADDL    Rebx,Reax               # Add it in
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    4(Resp),Rebx            # Get the index.
#else
    MOVL    R8,Rebx                 # Get the index.
#endif
    FLDS    -2(Reax,Rebx,2)
    FSTPL   (Recx)
    MOVL    Recx,Reax
    RET3

	RegMask(cmem_load_float,(M_Reax OR M_Rebx OR M_Recx OR M_FP7))

	INLINE_ROUTINE(cmem_load_asm_double)
#ifdef HOSTARCHITECTURE_X86_64
    PUSHL	R8						# Save R8 which isn't saved in mem_for_largeword
#endif
    call    mem_for_real
#ifdef HOSTARCHITECTURE_X86_64
    POPL	R8						# Save R8 which isn't saved in mem_for_largeword
#endif
    MOVL    (Reax),Reax             # The address is boxed.
    SARL    $TAGSHIFT,Rebx     # The offset is a signed tagged value
    ADDL    Rebx,Reax               # Add it in
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    4(Resp),Rebx            # Get the index.
#else
    MOVL    R8,Rebx                 # Get the index.
#endif
    FLDL    -4(Reax,Rebx,4)
    FSTPL   (Recx)
    MOVL    Recx,Reax
    RET3

	RegMask(cmem_load_double,(M_Reax OR M_Rebx OR M_Recx OR M_FP7))
   
	INLINE_ROUTINE(cmem_store_asm_8)
    MOVL    (Reax),Reax             # The address is boxed.
    SARL    $TAGSHIFT,Rebx     # The offset is a signed tagged value
    ADDL    Rebx,Reax               # Add it in
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    8(Resp),Rebx            # Get the index.
    MOVL    4(Resp),Recx            # Get the value to store
#else
    MOVL    R8,Rebx                 # Get the index.
    MOVL    R9,Recx
#endif
    SARL    $TAGSHIFT,Rebx     # That's also tagged
    SARL    $TAGSHIFT,Recx
    MOVB    %cl,(Reax,Rebx)
    MOVL    $UNIT,Reax             # The function returns unit
    MOVL    Reax,Rebx                   # Clobber bad value in %Rebx
    MOVL    Reax,Recx                   # and %Recx
    RET4
	RegMask(cmem_store_8,(M_Reax OR M_Rebx OR M_Recx))

	INLINE_ROUTINE(cmem_store_asm_16)
    MOVL    (Reax),Reax             # The address is boxed.
    SARL    $TAGSHIFT,Rebx     # The offset is a signed tagged value
    ADDL    Rebx,Reax               # Add it in
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    8(Resp),Rebx            # Get the index.
    MOVL    4(Resp),Recx            # Get the value to store
#else
    MOVL    R8,Rebx                 # Get the index.
    MOVL    R9,Recx
#endif
    SARL    $TAGSHIFT,Recx     # Untag the value to store
    movw    %cx,-1(Reax,Rebx,1)
    MOVL    $UNIT,Reax             # The function returns unit
    MOVL    Reax,Recx                   # Bad value in %Recx
    RET4
	RegMask(cmem_store_16,(M_Reax OR M_Rebx OR M_Recx))

	INLINE_ROUTINE(cmem_store_asm_32)
    MOVL    (Reax),Reax             # The address is boxed.
    SARL    $TAGSHIFT,Rebx     # The offset is a signed tagged value
    ADDL    Rebx,Reax               # Add it in
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    8(Resp),Rebx            # Get the index.
    MOVL    4(Resp),Recx            # Get the value to store
    MOVL    (Recx),Recx
#else
    MOVL    R8,Rebx                 # Get the index.
    MOVL    R9,Recx
    SARL    $TAGSHIFT,Recx     # Untag the value to store
#endif
    movl    %ecx,-2(Reax,Rebx,2)
    MOVL    $UNIT,Reax             # The function returns unit
    MOVL    Reax,Recx                   # Bad value in %Recx
    RET4
	RegMask(cmem_store_32,(M_Reax OR M_Rebx OR M_Recx))

	INLINE_ROUTINE(cmem_store_asm_64 # The value is boxed in 64-bit mode. Not implemented in 32-bit mode)
#ifdef HOSTARCHITECTURE_X86_64
    MOVL    (Reax),Reax             # The address is boxed.
    SARL    $TAGSHIFT,Rebx     # The offset is a signed tagged value
    ADDL    Rebx,Reax               # Add it in
    MOVL    (R9),Rebx               # Value to store
    MOVL    Rebx,(-4)(Reax,R8,4)    # Store it
    MOVL    $UNIT,Reax         # The function returns unit
    MOVL    Reax,Rebx               # Bad value in %Rebx
    RET4

	RegMask(cmem_store_64,(M_Reax OR M_Rebx OR M_Recx))
#endif

	INLINE_ROUTINE(cmem_store_asm_float)
    MOVL    (Reax),Reax             # The address is boxed.
    SARL    $TAGSHIFT,Rebx     # The offset is a signed tagged value
    ADDL    Rebx,Reax               # Add it in
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    8(Resp),Rebx            # Get the index.
    MOVL    4(Resp),Recx            # Get the address of the real
#else
    MOVL    R8,Rebx                 # Get the index.
    MOVL    R9,Recx
#endif
    FLDL    (Recx)
    FSTPS    -2(Reax,Rebx,2)
    MOVL    $UNIT,Reax         # The function returns unit
    MOVL    Reax,Rebx               # Bad value in %Rebx
    RET4
	RegMask(cmem_store_float,(M_Reax OR M_Rebx OR M_Recx OR M_FP7))

	INLINE_ROUTINE(cmem_store_asm_double)
    MOVL    (Reax),Reax             # The address is boxed.
    SARL    $TAGSHIFT,Rebx     # The offset is a signed tagged value
    ADDL    Rebx,Reax               # Add it in
#ifndef HOSTARCHITECTURE_X86_64
    MOVL    8(Resp),Rebx            # Get the index.
    MOVL    4(Resp),Recx            # Get the address of the real
#else
    MOVL    R8,Rebx                 # Get the index.
    MOVL    R9,Recx
#endif
    FLDL    (Recx)
    FSTPL    -4(Reax,Rebx,4)
    MOVL    $UNIT,Reax         # The function returns unit
    MOVL    Reax,Rebx               # Bad value in %Rebx
    RET4
	RegMask(cmem_store_double,(M_Reax OR M_Rebx OR M_Recx OR M_FP7))

#define CREATE_IO_CALL(index) \
    INLINE_ROUTINE(Call##index##) \
    CALL_IO(index)

#define CREATE_EXTRA_CALL(index) \
    INLINE_ROUTINE(X86AsmCallExtra##index##) \
    CALL_EXTRA(index)

	CREATE_IO_CALL(POLY_SYS_exit)
# 	CREATE_IO_CALL(POLY_SYS_chdir)
	CREATE_IO_CALL(POLY_SYS_get_flags)
	CREATE_IO_CALL(POLY_SYS_profiler)
	CREATE_IO_CALL(POLY_SYS_Real_str)
	CREATE_IO_CALL(POLY_SYS_Real_Dispatch)
	CREATE_IO_CALL(POLY_SYS_conv_real)
	CREATE_IO_CALL(POLY_SYS_real_to_int)
	CREATE_IO_CALL(POLY_SYS_sqrt_real)
	CREATE_IO_CALL(POLY_SYS_sin_real)
	CREATE_IO_CALL(POLY_SYS_signal_handler)
	CREATE_IO_CALL(POLY_SYS_os_specific)
	CREATE_IO_CALL(POLY_SYS_network)
	CREATE_IO_CALL(POLY_SYS_io_dispatch)
	CREATE_IO_CALL(POLY_SYS_poly_specific)
	CREATE_IO_CALL(POLY_SYS_set_code_constant)
	CREATE_IO_CALL(POLY_SYS_code_flags)
	CREATE_IO_CALL(POLY_SYS_shrink_stack)
	CREATE_IO_CALL(POLY_SYS_process_env)
	CREATE_IO_CALL(POLY_SYS_foreign_dispatch)
	CREATE_IO_CALL(POLY_SYS_ffi)
	CREATE_IO_CALL(POLY_SYS_stack_trace)
	CREATE_IO_CALL(POLY_SYS_full_gc)
	CREATE_IO_CALL(POLY_SYS_XWindows)
	CREATE_IO_CALL(POLY_SYS_timing_dispatch)
	CREATE_IO_CALL(POLY_SYS_kill_self)
	CREATE_IO_CALL(POLY_SYS_thread_dispatch)
	CREATE_IO_CALL(POLY_SYS_io_operation)
	CREATE_IO_CALL(POLY_SYS_ln_real)
	CREATE_IO_CALL(POLY_SYS_exp_real)
	CREATE_IO_CALL(POLY_SYS_arctan_real)
	CREATE_IO_CALL(POLY_SYS_cos_real)

	CREATE_EXTRA_CALL(RETURN_HEAP_OVERFLOW)
	CREATE_EXTRA_CALL(RETURN_STACK_OVERFLOW)
	CREATE_EXTRA_CALL(RETURN_STACK_OVERFLOWEX)
	CREATE_EXTRA_CALL(RETURN_RAISE_OVERFLOW)

# Direct calls to the RTS

/* chdir.  May raise an exception which means it may allocate space for the exception packet. */
/* The caller is supposed to preserve RBP */
	INLINE_ROUTINE(CallPOLY_SYS_chdir)
        MOVL    $UNIT,ExceptionPacket(Rebp)    # Clear the exception
        MOVL    Resp,Mr_ESP(Rebp)                   # Preserve the ML stack pointer
        MOVL    SavedSp(Rebp),Resp                  # Switch to the C stack
#ifndef HOSTARCHITECTURE_X86_64
# X86 calling conventions.  We keep a 16-byte stack alignment because GCC likes it.
# No need to save/restore the heap pointer since it's in memregs.
# N.B.  We don't actually need to remove any arguments from the C stack because
# we always set the C stack pointer to the value when we were first called.
        PUSHL   $0                             # Alignment - Needed??
        PUSHL   Reax                                # Argument
        PUSHL   Rebp                                # Memregs address
        call    X86ChDir
#else
        MOVL    R15,LocalMpointer(Rebp)             # Save the heap pointer
        MOVL    Rebp,Redi
        PUSHL   $0                             # Alignment - Needed??
        MOVL    Reax,Resi
        call    X86ChDir
        MOVL    LocalMpointer(Rebp),R15             # Restore the heap pointer register
#endif
        MOVL    Mr_ESP(Rebp),Resp                   # Restore ML stack ptr
        CMPL    $UNIT,ExceptionPacket(Rebp)
        jne     cpsdraisex
        CMPL    $UNIT,ExceptionPacket(Rebp)
        jne     cpsdraisex
        ret

cpsdraisex:
        MOVL    ExceptionPacket(Rebp),Reax
        jmp     raisex


/* Register mask vector. - extern int registerMaskVector();
   Each entry in this vector is a set of the registers modified
   by the function.  It is an untagged bitmap with the registers
   encoded in the same way as the */
        GLOBAL EXTNAME(registerMaskVector)
EXTNAME(registerMaskVector):
    .long  Mask_all                # 0 is unused
    .long  Mask_all                 # 1
    .long  Mask_all                 # 2
    .long  Mask_all                 # 3 is unused
    .long  Mask_all                 # 4 is unused
    .long  Mask_all                 # 5 is unused
    .long  Mask_all                 # 6
    .long  Mask_all                 # 7 is unused
    .long  Mask_all                 # 8 is unused
    .long  Mask_all                 # 9
    .long  Mask_all                 # 10 is unused
    .long  Mask_alloc_store         # 11
    .long  Mask_alloc_uninit        # 12
    .long  Mask_all                 # return = 13
    .long  Mask_all                 # raisex = 14
    .long  Mask_get_length          # 15
    .long  Mask_all                 # 16 is unused
    .long  Mask_all                 # 17
    .long  Mask_all                 # 18 is no longer used
    .long  Mask_all                 # 19 is no longer used
    .long  Mask_all                 # 20 is no longer used
    .long  Mask_all                 # 21 is unused
    .long  Mask_all                 # 22 is unused
    .long  Mask_str_compare         # 23
    .long  Mask_all                 # 24 is unused
    .long  Mask_all                 # 25 is unused
    .long  Mask_teststrgtr          # 26
    .long  Mask_teststrlss          # 27
    .long  Mask_teststrgeq          # 28
    .long  Mask_teststrleq          # 29
    .long  Mask_all                 # 30
    .long  Mask_all                 # 31 is no longer used
    .long  Mask_all                 # exception_trace_fn 32 - Calls unknown function
    .long  Mask_all                 # 33 is no longer used
    .long  Mask_all                 # 34 is no longer used
    .long  Mask_all                 # 35 is no longer used
    .long  Mask_all                 # 36 is no longer used
    .long  Mask_all                 # 37 is unused
    .long  Mask_all                 # 38 is unused
    .long  Mask_all                 # 39 is unused
    .long  Mask_all                 # 40
    .long  Mask_all                 # 41 is unused
    .long  Mask_all                 # 42
    .long  Mask_all                 # 43
    .long  Mask_all                 # 44 is no longer used
    .long  Mask_all                 # 45 is no longer used
    .long  Mask_all                 # 46
    .long  Mask_lockseg             # 47
    .long  Mask_all                 # nullorzero = 48
    .long  Mask_all                 # 49 is no longer used
    .long  Mask_all                 # 50 is no longer used
    .long  Mask_all                 # 51
    .long  Mask_all                 # 52
    .long  Mask_eq_longword         # 53
    .long  Mask_all				 # 54 is no longer used
    .long  Mask_geq_longword        # 55
    .long  Mask_leq_longword        # 56
    .long  Mask_gt_longword         # 57
    .long  Mask_lt_longword         # 58
    .long  Mask_all                 # 59 is unused
    .long  Mask_all                 # 60 is unused
    .long  Mask_all                 # 61
    .long  Mask_all                 # 62
    .long  Mask_all                 # 63 is unused
    .long  Mask_all                 # 64 is unused
    .long  Mask_all                 # 65 is unused
    .long  Mask_all                 # 66 is unused
    .long  Mask_all                 # 67 is unused
    .long  Mask_all                 # 68 is unused
    .long  Mask_atomic_reset        # 69
    .long  Mask_atomic_incr         # 70
    .long  Mask_atomic_decr         # 71
    .long  Mask_thread_self         # 72
    .long  Mask_all                 # 73
    .long  Mask_plus_longword       # 74
    .long  Mask_minus_longword      # 75
    .long  Mask_mul_longword        # 76
    .long  Mask_div_longword        # 77
    .long  Mask_mod_longword        # 78
    .long  Mask_andb_longword       # 79
    .long  Mask_orb_longword        # 80
    .long  Mask_xorb_longword       # 81
    .long  Mask_all                 # 82 is unused
    .long  Mask_all                 # 83 is now unused
    .long  Mask_all                 # 84
    .long  Mask_shift_left_longword # 85
    .long  Mask_shift_right_longword # 86
    .long  Mask_shift_right_arith_longword # 87
    .long  Mask_all                 # 88
    .long  Mask_longword_to_tagged  # 89
    .long  Mask_signed_to_longword  # 90
    .long  Mask_unsigned_to_longword # 91
    .long  Mask_all                 # 92
    .long  Mask_all                 # 93
    .long  Mask_all                 # 94
    .long  Mask_all                 # 95 is unused
    .long  Mask_all                 # 96 is unused
    .long  Mask_all                 # 97 is unused
    .long  Mask_all                 # 98
    .long  Mask_all                 # 99
    .long  Mask_all                 # 100
    .long  Mask_all                 # 101 is unused
    .long  Mask_all                 # 102 is unused
    .long  Mask_all                 # 103
    .long  Mask_quotrem             # 104
    .long  Mask_is_short            # 105
    .long  Mask_aplus               # 106
    .long  Mask_aminus              # 107
    .long  Mask_amul                # 108
    .long  Mask_adiv                # 109
    .long  Mask_amod                # 110
    .long  Mask_aneg                # 111
    .long  Mask_xora                # 112
    .long  Mask_equala              # 113
    .long  Mask_ora                 # 114
    .long  Mask_anda                # 115
    .long  Mask_all                 # 116 is unused
    .long  Mask_all                 # 117
    .long  Mask_real_geq            # 118
    .long  Mask_real_leq            # 119
    .long  Mask_real_gtr            # 120
    .long  Mask_real_lss            # 121
    .long  Mask_real_eq             # 122
    .long  Mask_real_neq            # 123
    .long  Mask_all                 # 124
    .long  Mask_real_add            # 125
    .long  Mask_real_sub            # 126
    .long  Mask_real_mul            # 127
    .long  Mask_real_div            # 128
    .long  Mask_real_abs            # 129
    .long  Mask_real_neg            # 130
    .long  Mask_all                 # 131 is unused
    .long  Mask_all                 # 132
    .long  Mask_all                 # 133
    .long  Mask_all                 # 134
    .long  Mask_real_from_int       # 135
    .long  Mask_all                 # 136
    .long  Mask_all                 # 137
    .long  Mask_all                 # 138
    .long  Mask_all                 # 139
    .long  Mask_all                 # 140
    .long  Mask_all                 # 141
    .long  Mask_fixed_to_real       # 142
    .long  Mask_all                 # 143 is unused
    .long  Mask_all                 # 144 is unused
    .long  Mask_all                 # 145 is unused
    .long  Mask_all                 # 146 is unused
    .long  Mask_all                 # 147 is unused
    .long  Mask_all                 # stdin = 148
    .long  Mask_all                 # stdout= 149
    .long  Mask_all                 # 150
    .long  Mask_set_string_length   # 151
    .long  Mask_get_first_long_word # 152
    .long  Mask_all                 # poly_specific = 153
    .long  Mask_bytevec_eq          # 154
    .long  Mask_all                 # 155 is unused
    .long  Mask_all                 # 156 is unused
    .long  Mask_all                 # 157 is unused
    .long  Mask_all                 # 158 is unused
    .long  Mask_all                 # 159 is unused
    .long  Mask_cmem_load_8         # 160
    .long  Mask_cmem_load_16        # 161
    .long  Mask_cmem_load_32        # 162
#ifdef HOSTARCHITECTURE_X86_64
    .long  Mask_cmem_load_64        # 163
#else
    .long  Mask_all                 # 169
#endif
    .long  Mask_cmem_load_float     # 164
    .long  Mask_cmem_load_double    # 165
    .long  Mask_cmem_store_8        # 166
    .long  Mask_cmem_store_16       # 167
    .long  Mask_cmem_store_32       # 168
#ifdef HOSTARCHITECTURE_X86_64
    .long  Mask_cmem_store_64       # 169
#else
    .long  Mask_all                 # 169
#endif
    .long  Mask_cmem_store_float    # 170
    .long  Mask_cmem_store_double   # 171
    .long  Mask_all                 # 172 is unused
    .long  Mask_all                 # 173 is unused
    .long  Mask_all                 # 174 is unused
    .long  Mask_all                 # 175 is unused
    .long  Mask_all                 # 176 is unused
    .long  Mask_all                 # 177 is unused
    .long  Mask_all                 # 178 is unused
    .long  Mask_all                 # 179 is unused
    .long  Mask_fixed_add           # 180
    .long  Mask_fixed_sub           # 181
    .long  Mask_fixed_mul           # 182
    .long  Mask_fixed_quot          # 183
    .long  Mask_fixed_rem           # 184
    .long  Mask_fixed_div           # 185
    .long  Mask_fixed_mod           # 186
    .long  Mask_all                 # 187 is unused
    .long  Mask_all                 # 188 is unused
    .long  Mask_all                 # 189
    .long  Mask_all                 # 190
    .long  Mask_all                 # 191 is no longer used
    .long  Mask_all                 # 192 is unused
    .long  Mask_move_words          # 193
    .long  Mask_all                 # 194
    .long  Mask_move_words          # 195
    .long  Mask_shift_right_arith_word  # 196
    .long  Mask_int_to_word         # 197
    .long  Mask_move_bytes          # 198
    .long  Mask_move_bytes          # 199
    .long  Mask_all                 # 200
    .long  Mask_all                 # 201
    .long  Mask_all                 # stderr = 202
    .long  Mask_all                 # 203 now unused
    .long  Mask_callcodeTupled      # 204 - callcode
    .long  Mask_all                 # 205
    .long  Mask_all                 # 206
    .long  Mask_all                 # 207 is unused
    .long  Mask_all                 # 208 now unused
    .long  Mask_all                 # 209
    .long  Mask_all                 # 210 is unused
    .long  Mask_all                 # 211 is unused
    .long  Mask_all                 # 212 is unused
    .long  Mask_is_big_endian       # 213
    .long  Mask_bytes_per_word      # 214
    .long  Mask_offset_address      # 215
    .long  Mask_shift_right_word    # 216
    .long  Mask_all				 # 217 - no longer used
    .long  Mask_not_bool            # 218
    .long  Mask_fixed_geq           # 219
    .long  Mask_fixed_leq           # 220
    .long  Mask_fixed_gtr           # 221
    .long  Mask_fixed_lss           # 222
    .long  Mask_string_length       # 223
    .long  Mask_all                 # 224 is unused
    .long  Mask_all                 # 225 is unused
    .long  Mask_all                 # 226 is unused
    .long  Mask_all                 # 227 is unused
    .long  Mask_touch_final         # 228
    .long  Mask_all                 # 229 - no longer used
    .long  Mask_all                 # 230 - no longer used
    .long  Mask_int_geq             # 231
    .long  Mask_int_leq             # 232
    .long  Mask_int_gtr             # 233
    .long  Mask_int_lss             # 234
    .long  Mask_load_byte           # load_byte_immut = 235
    .long  Mask_load_word           # load_word_immut = 236
    .long  Mask_all                 # 237 is unused
    .long  Mask_mul_word            # 238
    .long  Mask_plus_word           # 239
    .long  Mask_minus_word          # 240
    .long  Mask_div_word            # 241
    .long  Mask_or_word             # 242
    .long  Mask_and_word            # 243
    .long  Mask_xor_word            # 244
    .long  Mask_shift_left_word     # 245
    .long  Mask_mod_word            # 246
    .long  Mask_word_geq            # 247
    .long  Mask_word_leq            # 248
    .long  Mask_word_gtr            # 249
    .long  Mask_word_lss            # 250
    .long  Mask_word_eq             # 251
    .long  Mask_load_byte           # 252
    .long  Mask_load_word           # 253
    .long  Mask_assign_byte         # 254
    .long  Mask_assign_word         # 255

