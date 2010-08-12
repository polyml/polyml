!
!  Title:   Assembly code routines for the poly system.
!  Author:    David Matthews
!  Copyright (c) Cambridge University Technical Services Limited 2000-7
!
!   This library is free software; you can redistribute it and/or
!   modify it under the terms of the GNU Lesser General Public
!   License as published by the Free Software Foundation; either
!   version 2.1 of the License, or (at your option) any later version.
!   
!   This library is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   Lesser General Public License for more details.
!   
!   You should have received a copy of the GNU Lesser General Public
!   License along with this library; if not, write to the Free Software
!   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
!
! Call machine code from the interpreter.
! 
/*
 Linkage conventions:
We ignore the register window and use only the %o, %l and %g register sets.
%i0, the original parameter to the C code, points to a memRegisters structure.
%i1 is currently unused
%i2 and %i3 are unsaved scratch registers.  The difference is that if %i2 is the
target of a sethi/add combination the value is assumed to be an address and can be
updated by the GC whereas if it is %i3 it is not.
%i4 and %i5 are saved unchecked registers.
%i6 and %i7 are never used (%i7 is the return address from ML to C).
%o0 is used for the first argument to a function, and for the result.
%o1-%o3 are used for the next 3 args, any others being passed on the stack.
%o4 is a general work register (previously the code address/constant ptr)
%o5 is the closure pointer or static link pointer.
%o6 is never used (the system assumes it points to an area where regs can be stored).
%o7 is the return address i.e. the address of the jmpl instruction.
Return is done by jmp %o7+6. 2 is always added to %o7 on compiled function entry
so that it is distinguishable from other addresses which point at the
start of objects.

%g3 points to the top exception handler,
%g4 is the stack pointer,
%g5 is the heap limit - previously stack  limit register,
%g6 is the heap pointer,
%g7 is unused - used by pthread library

%g1 and %g2 are available as work regs, as are the %l registers.

%i4 and %i5 are untagged scratch registers which don't need to contain
tagged values. They should NEVER contain pointers if there is any possibility
of a garbage collection, since the garbage collector doesn't change the
values in these registers.

Other register MUST contain properly tagged values if there any a possibility
of a garbage collection.
*/

/* Include sys.h for the POLY_SYS entries. */
#include "sys.h"


/* Assume we're compiling for ELF and don't need an underscore for globals */
#define gbl(id)  id
#define gbla(id) id##a
#define gblc(id) id##c

#define globldec(id) \
    .globl  gbl(id); \
    gbl(id):

/* Standard start-up for inline calls (those that don't call C) */
#define INLINE_ROUTINE(id) \
    globldec(id) \
    or  %o7,2,%o7; /* Required by ML compiler */  \
    mov UNIT,%o4   /* This can be removed once we use a temporary reg here. */

/* relative to a StackObject structure */
#define P_SPACE(base)   [base+0]
#define P_PC(base)      [base+4]
#define P_SP(base)      [base+8]
#define P_HR(base)      [base+12]
#define P_NREG(base)    [base+16]
#define P_REG(i,base)   [base+(20+4*i)]

/* Relative to the MemRegisters structure.  This is pointed at by %i0. */
#define MR_INRTS          [%i0+0]
#define MR_REQUEST        [%i0+4]
#define MR_REASON         [%i0+8]
#define MR_HEAPPOINTER    [%i0+12]
#define MR_HEAPSPACET     [%i0+16]
#define MR_POLYSTACK      [%i0+20]
#define MR_STACKLIMIT     [%i0+24]
#define MR_STACKTOP       [%i0+28]
#define MR_IOENTRY        [%i0+36]
#define MR_THREADID       [%i0+40]

#define loadg(x, r) sethi %hi(x),r; ld [r+%lo(x)],r
#define storeg(r, x, w) sethi %hi(x),w; st r,[w+%lo(x)]

/* The shift up/down mnemonics are different for the AIX versions */
#define shiftup(word,places) ((word)<<(places))
#define shiftdown(word,places) ((word)>>(places))

/* The bottom two bits are used as tags */
#define TAGSHIFT 2
#define TAGBITS (0x3)
#define TAGGED(n) (shiftup(n,TAGSHIFT)+1)
#define TRUE  TAGGED(1)
#define FALSE TAGGED(0)
#define UNIT TAGGED(0)

/* Used to convert byte-counts to word-counts. It's just a coincidence
   that this is the same as TAGSHIFT on this machine.
   SPF 18/12/95
*/
#define WORDSHIFT 2

/* The most significant TYPESHIFT of a length-word are type bits.
   The remaining 32-TYPESHIFT bits constitute an unsigned integer
   which is the number of words (not counting the length-word itself)
   contained in the object.
   SPF 18/12/95
*/
#define TYPESHIFT 8

/* Register mask entries - must match coding used in codeCons.ML */
#define     M_O0        0x000001
#define     M_O1        0x000002
#define     M_O2        0x000004
#define     M_O3        0x000008
#define     M_O4        0x000010
#define     M_O5        0x000020
#define     M_G1        0x000040    /* O6 is not used */
#define     M_O7        0x000080
#define     M_L0        0x000100
#define     M_L1        0x000200
#define     M_L2        0x000400
#define     M_L3        0x000800
#define     M_L4        0x001000
#define     M_L5        0x002000
#define     M_L6        0x004000
#define     M_L7        0x008000
#define     M_G2        0x010000

/* TODO: The present mask entries are just a first go through.  Most of them are
   set to Mask_all when they certainly don't need to be.  DCJM 11/12/00. */

#define     RegMask(name,mask) Mask_##name=mask

/* Default mask for unused entries and also for the special cases
   where we don't know what the effect of calling the function
   will be. */
    RegMask(all,0x1ffff)


/* SparcAsmSaveStateAndReturn.  Saves the ML state into poly_stack and returns to C.
   This is the return half of the code in SparcAsmSwitchToPoly.
*/
    .global gbl(SparcAsmSaveStateAndReturn)
gbl(SparcAsmSaveStateAndReturn):
    nop
    nop
    nop
    ld      MR_POLYSTACK,%i3
    st      %g6,MR_HEAPPOINTER          ! Save the heap address
    st      %g1,P_REG(0,%i3)             ! general registers
    st      %g2,P_REG(1,%i3)
    st      %o0,P_REG(2,%i3)
    st      %o1,P_REG(3,%i3)
    st      %o2,P_REG(4,%i3)
    st      %o3,P_REG(5,%i3)
    st      %o4,P_REG(6,%i3)
    st      %o5,P_REG(7,%i3)
    st      %o7,P_REG(9,%i3)
    st      %l0,P_REG(10,%i3)
    st      %l1,P_REG(11,%i3)
    st      %l2,P_REG(12,%i3)
    st      %l3,P_REG(13,%i3)
    st      %l4,P_REG(14,%i3)
    st      %l5,P_REG(15,%i3)
    st      %l6,P_REG(16,%i3)
    st      %l7,P_REG(17,%i3)
    st      %i4,[%i3+96]                ! Unchecked reg
    st      %i5,[%i3+100]               ! Unchecked
    st      %g3,[%i3+12]                ! hr
    st      %g4,[%i3+8]                 ! sp
    mov     1,%g1
    st      %g1,MR_INRTS                ! Set the InRunTime flag
    ld      [%sp+92],%i7

! Return to C.  This code is the counterpart to the "save" at the start
! of SparcAsmSwitchToPoly.
    ret
    restore ! DELAY SLOT

/***************************************************************************/
/* SparcAsmSwitchToPoly                                                    */
/***************************************************************************/

/* Entry point for C */
    .global gbl(SparcAsmSwitchToPoly)
gbl(SparcAsmSwitchToPoly):
/*  The ML code is treated as a single C procedure.  It is called with "memRegisters" as
    its argument and returns whenever it needs RTS support.  It can also be forced to return
    when it receives a signal by having the signal handler set the PC values to
    SparcAsmSaveStateAndReturn.  The C stack pointer (%o6) is never modified by the ML code. */
    save    %sp,-120,%sp                ! Create a new frame (%sp == %o6).  This has two local words.
! Save these for the moment.
    st      %i7,[%sp+92]                ! Save the return address in the frame. First local word.
! %i0 contains the address of memRegs.  Save that in the stack
    st      %i0,[%sp+188]               ! This is the conventional location for the first parameter

    ld      MR_POLYSTACK,%i3
    ld      P_SP(%i3),%g4         ! sp
    ld      P_HR(%i3),%g3         ! hr
    /* Now rsp, rhr and rsl have been loaded we can clear inRTS */
    st      %g0,MR_INRTS

    ld      MR_HEAPPOINTER,%g6    ! Local pointer
    ld      MR_HEAPSPACET,%g5     ! Set to space available - maxint

    ld      P_REG(0,%i3),%g1
    ld      P_REG(1,%i3),%g2
    ld      P_REG(2,%i3),%o0
    ld      P_REG(3,%i3),%o1
    ld      P_REG(4,%i3),%o2
    ld      P_REG(5,%i3),%o3
    ld      P_REG(6,%i3),%o4
    ld      P_REG(7,%i3),%o5
! /* There's a slot reserved for %o6, but we mustn't touch that register. */    
    ld      P_REG(9,%i3),%o7
    ld      P_REG(10,%i3),%l0
    ld      P_REG(11,%i3),%l1
    ld      P_REG(12,%i3),%l2
    ld      P_REG(13,%i3),%l3
    ld      P_REG(14,%i3),%l4
    ld      P_REG(15,%i3),%l5
    ld      P_REG(16,%i3),%l6
    ld      P_REG(17,%i3),%l7
! /* There's a slot reserved for the untagged register count. */    
    ld      P_REG(19,%i3),%i4
    ld      P_REG(20,%i3),%i5
    ld      P_REG(21,%i3),%i2   ! condition code (only for emulation)
    ld      P_PC(%i3),%i3       ! pc
 
! The condition code is not preserved across most traps and is only used
! when emulating an arbitrary precision comparison instruction.
    jmp     %i3         !
    cmp     %i2,0       ! DELAY SLOT Set the condition code


/***************************************************************************/
/* Standard C call macro                                                   */
/***************************************************************************/

/* Set the request code and return to C, saving the registers on the way.
   The first word must be or %o7,2,%o7.  The function call sequence may
   skip this word if it is jumping to the function (and so an earlier
   call will have or-ed it already) rather than calling. */
#define CALL_IO(name, ioCall) \
    .global gbla(name); \
gbla(name): \
    or      %o7,2,%o7; \
    ld      MR_IOENTRY,%i4; \
    mov     ioCall,%i3; \
    jmp     %i4; \
    st      %i3,MR_REQUEST; /* Delay slot */ \
    RegMask(name, Mask_all)

/***************************************************************************/
/* Functions implemented in C                                              */
/***************************************************************************/

    CALL_IO(finish, POLY_SYS_exit)
    CALL_IO(change_dir, POLY_SYS_chdir)
    CALL_IO(profiler, POLY_SYS_profiler)
    CALL_IO(Real_str,POLY_SYS_Real_str)
    CALL_IO(Real_geq,POLY_SYS_Real_geq)
    CALL_IO(Real_leq,POLY_SYS_Real_leq)
    CALL_IO(Real_gtr,POLY_SYS_Real_gtr)
    CALL_IO(Real_lss,POLY_SYS_Real_lss)
    CALL_IO(Real_eq,POLY_SYS_Real_eq)
    CALL_IO(Real_neq,POLY_SYS_Real_neq)
    CALL_IO(Real_dispatch,POLY_SYS_Real_Dispatch)
    CALL_IO(Real_add, POLY_SYS_Add_real)
    CALL_IO(Real_sub, POLY_SYS_Sub_real)
    CALL_IO(Real_mul, POLY_SYS_Mul_real)
    CALL_IO(Real_div, POLY_SYS_Div_real)
    CALL_IO(Real_neg, POLY_SYS_Neg_real)
    CALL_IO(Real_int, POLY_SYS_real_to_int)
    CALL_IO(Real_float, POLY_SYS_int_to_real)
    CALL_IO(Real_sqrt, POLY_SYS_sqrt_real)
    CALL_IO(Real_sin, POLY_SYS_sin_real)
    CALL_IO(Real_cos, POLY_SYS_cos_real)
    CALL_IO(Real_arctan, POLY_SYS_arctan_real)
    CALL_IO(Real_exp, POLY_SYS_exp_real)
    CALL_IO(Real_ln, POLY_SYS_ln_real)
    CALL_IO(Real_repr, POLY_SYS_Repr_real)
    CALL_IO(Real_conv, POLY_SYS_conv_real)
    CALL_IO(thread_dispatch, POLY_SYS_thread_dispatch)
    CALL_IO(kill_self, POLY_SYS_kill_self)
    CALL_IO(objsize_, POLY_SYS_objsize)                     ! MJC 27/04/88
    CALL_IO(showsize_, POLY_SYS_showsize)                    ! MJC 09/03/89
    CALL_IO(timing_dispatch_,POLY_SYS_timing_dispatch)          ! DCJM 10/4/00

    CALL_IO(XWindows_, POLY_SYS_XWindows)            ! MJC 27/09/90

    CALL_IO(full_gc_, POLY_SYS_full_gc)                       ! MJC 18/03/91 
    CALL_IO(stack_trace_, POLY_SYS_stack_trace)                   ! MJC 18/03/91
    
    CALL_IO(foreign_dispatch_, POLY_SYS_foreign_dispatch)        ! NIC 22/04/94
    CALL_IO(IO_dispatch_, POLY_SYS_io_dispatch)         ! DCJM 8/5/00
    CALL_IO(Net_dispatch_, POLY_SYS_network)            ! DCJM 22/5/00
    CALL_IO(OS_spec_dispatch_, POLY_SYS_os_specific)        ! DCJM 22/5/00
    CALL_IO(Sig_dispatch_, POLY_SYS_signal_handler)            ! DCJM 18/7/00

    CALL_IO(shrink_stack_, POLY_SYS_shrink_stack)          ! SPF  1/12/96
    CALL_IO(process_env_dispatch_,POLY_SYS_process_env)         ! DCJM 25/4/2000
    CALL_IO(get_flags_, POLY_SYS_get_flags)
    CALL_IO(set_flags_, POLY_SYS_code_flags)         ! SPF 12/02/97
    
    CALL_IO(set_code_constant, POLY_SYS_set_code_constant)      ! DCJM 11/1/2001

    CALL_IO(poly_dispatch_, POLY_SYS_poly_specific)
    CALL_IO(callcode_tupled, POLY_SYS_callcode_tupled)
    CALL_IO(io_operation, POLY_SYS_io_operation)
    CALL_IO(exception_trace, POLY_SYS_exception_trace)
    CALL_IO(bytevec_eq_, POLY_SYS_bytevec_eq)
    CALL_IO(alloc_uninit_, POLY_SYS_alloc_uninit)

! alloc(size, flags, initial).  Allocates a segment of a given size and
! initialises it.
globldec(alloc_store)
    CALL_IO(alloc_store_long_, POLY_SYS_alloc_store)
    RegMask(alloc_store,Mask_all)

INLINE_ROUTINE(int_to_word)
/* Extract the low order 32 bits from an integer.  If it is a tagged integer
   there's nothing to do, if it is a long integer we have to extract the
   low order word from the vector. */
! This first part is no longer used.  Word.fromInt is now implemented using
! is_short and get_first_word_long.
    andcc   %o0,1,%g0
    be  itw0        /* If it's long we have to get the first word. */
    nop
    jmp %o7+6       /* If it's short we just return it. */
    nop
itw0:
    CALL_IO(get_first_long_word_, POLY_SYS_get_first_long_word)
    RegMask(get_first_long_word,Mask_all)


INLINE_ROUTINE(not_bool)
    jmp %o7+6
    xor %o0,4,%o0   ! use of delay slot (SPF 13/10/94)
    RegMask(not_bool,M_O0|M_O7|M_O4)

INLINE_ROUTINE(or_word)
    jmp %o7+6
    or  %o0,%o1,%o0
    RegMask(or_word,M_O0|M_O7|M_O4)

INLINE_ROUTINE(and_word)
    jmp %o7+6
    and %o0,%o1,%o0
    RegMask(and_word,M_O0|M_O7|M_O4)

INLINE_ROUTINE(xor_word)
    xor %o0,%o1,%i4 ! This will zero the tag field (tags were equal)
    jmp %o7+6
    or  %i4,1,%o0   ! restore the tag bit
    RegMask(xor_word,M_O0|M_O7|M_O4)

! Assume that both args are tagged integers
! Word.<<(a,b) is defined to return 0 if b > Word.wordSize
INLINE_ROUTINE(shift_left_word)
    subcc   %o1,TAGGED(32-TAGSHIFT),%g0
    bgu,a   slw1
    mov 0,%i4           ! Only if branch taken i.e. %o1 > 30
    sub %o0,1,%i4       ! untag value to shift (but offset by 2)
    srl %o1,TAGSHIFT,%i5    ! amount to shift
    sll %i4,%i5,%i4
slw1:   jmp %o7+6
    or  %i4,1,%o0       ! restore the tag bit
    RegMask(shift_left_word,M_O0|M_O7|M_O4)

! Assume that both args are tagged integers
! Word.>>(a,b) is defined to return 0 if b > Word.wordSize
INLINE_ROUTINE(shift_right_word)
    subcc   %o1,TAGGED(32-TAGSHIFT),%g0
    bgu,a   srw1
    mov 0,%i4           ! Only if branch taken i.e. %o1 > 30
    srl %o1,TAGSHIFT,%i5    ! amount to shift 
    srl %o0,%i5,%i4
    andn    %i4,TAGBITS,%i4     ! remove stray bits from tag positions
srw1:   jmp %o7+6
    or  %i4,1,%o0       ! restore the tag bit
    RegMask(shift_right_word,M_O0|M_O7|M_O4)

! Assume that both args are tagged integers
! Word.~>>(a,b) is defined to return 0 or ~1 if b > Word.wordSize
INLINE_ROUTINE(shift_right_arith_word)
    subcc   %o1,TAGGED(32-TAGSHIFT),%g0
    bgu,a   saw1
    mov TAGGED(32-TAGSHIFT),%o1 ! Set the shift to 30. This will give us either 0 or ~1.
saw1:   srl %o1,TAGSHIFT,%i5    ! amount to shift 
    sra %o0,%i5,%i4
    andn    %i4,TAGBITS,%i4     ! remove stray bits from tag positions
    jmp %o7+6
    or  %i4,1,%o0       ! restore the tag bit
    RegMask(shift_right_arith_word,M_O0|M_O7|M_O4)

! This is needed in the code generator, but is a very risky thing to do.
INLINE_ROUTINE(offset_address)
    srl %o1,TAGSHIFT,%i5    ! untag offset
    jmp %o7+6
    add %o0,%i5,%o0
    RegMask(offset_address,M_O0|M_O7|M_O4)

! INLINE_ROUTINE(name) could be inlined to use delay slot. SPF 17/7/96
#define TEST(name, br_cond) \
    INLINE_ROUTINE(name); \
    cmp %o0,%o1; \
    br_cond 1f; \
    nop; \
    jmp %o7+6;  \
    mov FALSE,%o0; \
1:  jmp %o7+6;  \
    mov TRUE,%o0; \
    RegMask(name,M_O0|M_O7|M_O4)

    TEST(int_eq,be)
    TEST(int_neq,bne)

INLINE_ROUTINE(locksega)
! Clears the "mutable" bit on a segment
    ldub    [%o0-4],%i4
    andn    %i4,0x40,%i4        ! Reset mutable bit
    stb %i4,[%o0-4]     ! (DELAY SLOT)
    jmp %o7+6
    mov UNIT,%o0
    RegMask(lockseg,Mask_all)


INLINE_ROUTINE(get_length_a)
    ld  [%o0-4],%i4
    sll %i4,TYPESHIFT,%i4   ! Clear top byte
    srl %i4,(TYPESHIFT-TAGSHIFT),%i4
    jmp %o7+6
    add %i4,1,%o0       ! return it as a tagged integer
    RegMask(get_length,M_O0|M_O7|M_O4)

!       String comparison operations.

INLINE_ROUTINE(teststrneq)
    ba  ts1
    mov FALSE,%o2   ! return false (tagged 0) if strings are equal

/* String equality code rewritten (SPF 14/10/94)
   this code wouldn't work for Dave's run-time system */
INLINE_ROUTINE(teststreq)
    mov TRUE,%o2    ! return true (tagged 1) if strings are equal

ts1:
    cmp %o0,%o1     ! are the two values identical?
    be  ts_equal
    andcc   %o0,TAGBITS,%g0
    bnz ts_unequal  ! return if arg1 is a single char (i.e. tag is non-zero)
    andcc   %o1,TAGBITS,%g0 
    bnz ts_unequal  ! return if arg2 is a single char
    nop 

! We have two "long" strings
    ld  [%o0-4],%i4 ! get the length words
    ld  [%o1-4],%i5
    cmp %i4,%i5
    bne ts_unequal  ! return if length words differ
    
    sll     %i4,TYPESHIFT,%i4   ! remove type bits
    srl     %i4,(TYPESHIFT-WORDSHIFT),%i4   ! convert to byte offset

! The main loop: %i4 has the word offset of the last word we checked
ts5:
    subcc   %i4,4,%i4
    bl  ts_equal    ! /* return if we've checked all the words */
!   nop
! /* Don't need a "nop" here, since reloading the length word would be safe */
    
    ld  [%o0+%i4],%i5
    ld  [%o1+%i4],%l2
    cmp %i5,%l2
    be  ts5
    mov UNIT,%l2    ! zap l2
    
! /* We've found a difference, so return false */
ts_unequal:
    jmp %o7+6       ! invert "string_equal" boolean
    xor %o2,4,%o0   ! 1 xor 4 = 5; 5 xor 4 = 1
    
ts_equal:
    jmp %o7+6       ! return "string_equal" boolean
    mov %o2,%o0
    RegMask(teststreq,Mask_all)
    RegMask(teststrneq,Mask_all)


INLINE_ROUTINE(teststrgeq)
    mov     %o7,%i2         ! Save real return addr
    call    test_string     !
    nop
    mov     %i2,%o7         ! Restore return address and clobber bad address
    bge    teststrgeq1      ! Skip if it's greater or equal
    mov     TRUE,%o0        ! Delay slot
    mov     FALSE,%o0
teststrgeq1:
    jmp     %o7+6
    nop

INLINE_ROUTINE(teststrleq)
    mov     %o7,%i2         ! Save real return addr
    call    test_string     !
    nop
    mov     %i2,%o7         ! Restore return address and clobber bad address
    ble,a    teststrleq1    ! Skip if it's less or equal
    mov     TRUE,%o0        ! Delay slot
    mov     FALSE,%o0
teststrleq1:
    jmp     %o7+6
    nop

INLINE_ROUTINE(teststrlss)
    mov     %o7,%i2         ! Save real return addr
    call    test_string     !
    nop
    mov     %i2,%o7         ! Restore return address and clobber bad address
    bl      teststrlss1     ! Skip if it's less.
    mov     TRUE,%o0        ! Delay slot
    mov     FALSE,%o0
teststrlss1:
    jmp     %o7+6
    nop

INLINE_ROUTINE(teststrgtr)
    mov     %o7,%i2         ! Save real return addr
    call    test_string     !
    nop
    mov     %i2,%o7         ! Restore return address and clobber bad address
    bg      teststrgtr1     ! Skip if it's greater
    mov     TRUE,%o0        ! Delay slot
    mov     FALSE,%o0
teststrgtr1:
    jmp     %o7+6
    nop

INLINE_ROUTINE(str_comparea)
    mov     %o7,%i2         ! Save real return addr
    call    test_string     !
    nop
    mov     %i2,%o7         ! Restore return address and clobber bad address
        /* Set the result to -1 if less, 0 if equal and 1 if greater. */
    beq     str_comp1
    mov     TAGGED(0),%o0   ! Delay slot
    bg      str_comp1
    mov     TAGGED(1),%o0   ! Delay slot
    mov     TAGGED(-1),%o0
str_comp1:
    jmp     %o7+6
    nop

    RegMask(teststrgeq,Mask_all)
    RegMask(teststrleq,Mask_all)
    RegMask(teststrgtr,Mask_all)
    RegMask(teststrlss,Mask_all)
    RegMask(str_compare,Mask_all)


/***************************************************************************/
/* test_string - basic string comparison utility function                  */
/***************************************************************************/

/* Compare two strings; returns with condition codes set appropriately. */
/* N.B.  Doesn't adjust %o7.  Even with 2 added this will be an invalid address
   so the caller must overwrite %o7. */
test_string:
    andcc   %o0,TAGBITS,%g0   ! Single char?
    be,a    test_string2      ! Skip if already an address.  Annul the next instr if it's short.
    ld      [%o0],%i4         ! Use the delay slot to get the length

    andcc   %o1,TAGBITS,%g0   ! test arg2

    /* arg1 is a single character - is arg2? */
    be,a    test_string1      ! Must annul the next instr if this is a single char.
    ld      [%o1],%i5         ! Use the delay slot to get the length

    /* Both are single characters - just compare them */
    jmp     %o7+8
    cmp     %o0,%o1           ! In the delay slot

test_string1:
    /* arg1 is a single character, but arg2 isn't. */
    /* Is arg2 a null string? - return "GT" if 1 > length(arg2). */ 
    mov     1,%i4
    cmp     %i4,%i5
    bg    test_string6
    nop

    /* Compare arg1 with the first byte of arg2. 
       If the bytes differ, that's the result we want. */
    ldub    [%o1+4],%i5
    srl     %o0,TAGSHIFT,%i4
    cmp     %i4,%i5
    bne     test_string6
    mov     0,%i4           ! Delay slot. Set %i4 to zero ready for test
    /* But if they're equal set "less" because A is less than B */
    jmp     %o7+8
    cmp     %i4,1           ! In the delay slot. 

test_string2:
    andcc   %o1,TAGBITS,%g0   ! test arg2
    /* arg1 is not a single character - is arg2? */
    be,a    test_string3      ! Annul the next instr if it's short 
    ld      [%o1],%i5         ! Use the delay slot to get the length

    /* arg1 is not a single character, but arg2 is.
       Is arg1 a null string? - return "LT" if length(arg1) < 1. */
    cmp      %i4,1
    bl       test_string6
    nop
    /* Compare first byte of arg1 with arg2. 
       If the bytes differ, that's the result we want. */
    ldub     [%o0+4],%i4
    srl      %o1,TAGSHIFT,%i5
    cmp     %i4,%i5
    bne     test_string6
    mov     1,%i4           ! Delay slot. Set %i4 to 1 ready for test
    /* But if they're equal set "greater" because A is greater than B */
    jmp     %o7+8
    cmp     %i4,0           ! In the delay slot.

test_string3:
    /* Both are addresses rather than single characters.  We've loaded the
       length of arg1 into i4 and of arg2 in i5 in the delay slots. */
! A is greater than B if, at the first position at which A and B differ,
! A[i] > B[i] or if the end of B is found before they differ.
! Set %i4 to the shorter length and %i5 to a -ve value if B is shorter
    subcc   %i5,%i4,%i5       ! Set i5 to excess of B over A
    bneg,a  test_string3a     ! i.e. execute next instr if negative
    addcc   %i4,%i5,%i4       ! Subtract (negative) excess if A is longer

test_string3a:
    cmp     %i4,0             ! Check the length first time through.  It's possible the above code will do that.
test_string4:
! Start of main loop.  The length is checked either above or in the dealy slot below.
    bne     test_string5
    nop
    b       test_string5a     ! Results depend on the lengths
    cmp     %i4,%i5           ! In the delay slot, compare i4 which is zero with the length excess

test_string5:
    ldub    [%o0+4],%i3
    ldub    [%o1+4],%g1
    add     %o0,1,%o0
    add     %o1,1,%o1
    cmp     %i3,%g1
    be,a    test_string4      ! Repeat if equal.  If it isn't annul the next instr.
    subcc   %i4,1,%i4         ! Subtract 1 and set the condition code
! If we find a difference we return with the result in the condition codes.

test_string5a:
    mov     TAGGED(0),%o0     ! Clobber these which will point into the string
    mov     TAGGED(0),%o1
	mov     TAGGED(0),%g1     ! and this which is an untagged character

test_string6:
    jmp     %o7+8
    nop


/* raisex is used by compiled code. N.B. note that raisex is NOT the same as raise_ex */
INLINE_ROUTINE(raisex)
    ld   MR_STACKTOP,%i3    ! i3 = end_of_stack
    ld  [%o0],%i4           ! i4 = exception id
    ld  [%g3],%i5           ! i5 = handler id
    mov %g3,%o1             ! o1 = handler ptr

! Loop to find the handler for this exception. Handlers consist of one or more
! pairs of identifier and code address, followed by the address of the next
! handler.
rsx1:   cmp %i5,TAGGED(0)   ! Is the handler identifier 0 or TAGGED(0)?
    bleu    rsx7        ! If so we have a default handler.
!   nop         ! Unnecessary NOP deleted
    
    ! non-default handler
    cmp %i4,%i5     ! Does it match the exception id?
    beq rsx7        ! Skip if we found a match.
    nop
    
    ! This handler does not match - try the next one.
    !  This can be either a genuine handler pair, or a
    !  pointer up the stack.
    ld  [%o1+8],%i5 ! Get the next handler id
    add %o1,8,%o1   ! Increment the handler pointer
    
    ! The very last handler points at itself, so we use "blu", not "bleu" here
    cmp %i5,%o1
    blu rsx1        ! Not a stack pointer (too small)
!   nop         ! Unnecessary NOP deleted
    
    cmp %i5,%i3
    bgeu    rsx1        ! Not a stack pointer (too large)
    nop
    
    ! /* It's a stack pointer - get the next batch of handlers */
    mov %i5,%o1
    b   rsx1
    ld  [%i5],%i5
    
    
rsx7:   ! We have found the right handler - %o1 points to the data
    ld  [%o1+4],%o7 ! Get the handler entry point

rsx6:   
    ld  [%o1+8],%i5 ! Get the next handler id
    add %o1,8,%o1
    
    ! The very last handler points at itself, so we use "blu", not "bleu" here
    cmp %i5,%o1
    blu rsx6        ! Not a stack pointer (too small)
!   nop         ! Unnecessary NOP deleted
    
    cmp %i5,%i3
    bgeu    rsx6        ! Not a stack pointer (too large)
!   nop         ! Unnecessary NOP deleted
    
    /* o1 now points at the pointer to the next group of handlers
       i.e. the old (saved) value of the handler register
       and %i5 contains the pointer itself */
    
    /* Is this handler a real one, or was it set by exception_trace? */
    cmp %o7,TAGGED(0)
    bleu    rsx8
    nop

! Ordinary exception
    add %o1,4,%g4   ! Pop stack back past saved rhr
    mov %i5,%g3     ! Reload rhr from saved value
    jmp %o7-2       ! Now enter the handler
    mov UNIT,%o1    ! Zap bad value in %o1

rsx8:
    /*We've found a handler set by exception_trace.
    ! Push %o7 onto the stack. It should contain a "return"
    ! address inside the function that raised the exception.
    ! (That's because raising an exception is actually a CALL
    ! to the RTS.) Pushing it onto the stack allows ex_tracec
    ! to identify the function that raised the exception. We
    ! then put a dummy value into %o7 (so it's not mistaken for a normal return address) and
    ! finally call ex_tracec, which doesn't return but actually
    ! unwinds the stack to the next handler and the re-raises the
    ! exception.
    ! SPF 9/4/97 */
    mov %o0,%l1
    st  %o7,[%g4-4]
    mov %o1,%o0     ! stack-mark is arg1
    sub %g4,4,%g4
    mov %l1,%o1     ! exception packet is arg2
    mov TAGGED(1),%o7   ! make return address look like a tagged int.
    CALL_IO(ex_trace, POLY_SYS_give_ex_trace)

! Arbitrary precision arithmetic. These only call the procedures in arb.c
! if the values are in the long format.

INLINE_ROUTINE(neg_long)
    sub %o0,1,%i4   ! Remove tag
    tsubcctv    %g0,%i4,%i4
    jmp %o7+6
    add %i4,1,%o0
    RegMask(aneg,M_O0|M_O7|M_O4)


INLINE_ROUTINE(add_long)
    sub %o0,1,%i4   ! Remove tags
    sub %o1,1,%i5
    taddcctv    %i4,%i5,%i4 ! Check tags and overflow
    jmp %o7+6
    add %i4,1,%o0   ! Restore tag
    RegMask(aplus,M_O0|M_O7|M_O4)

INLINE_ROUTINE(sub_long)
    sub %o0,1,%i4   ! Remove tags
    sub %o1,1,%i5
    tsubcctv    %i4,%i5,%i4 ! Check tags and overflow
    jmp %o7+6
    add %i4,1,%o0   ! Restore tag
    RegMask(aminus,M_O0|M_O7|M_O4)

INLINE_ROUTINE(mult_long)
    and %o0,1,%i5   ! test for any long arguments
    andcc   %o1,%i5,%g0
    bz  mul_really_long
    mov %o7,%i5     ! (DELAY SLOT) Save link reg
    call    mul_signed
    nop
    bnz mul_really_long ! Skip if overflow
    mov %i5,%o7     ! (DELAY SLOT) Restore link
    jmp %o7+6
    mov %o2,%o0     ! Get result.

mul_really_long:
    CALL_IO(mult_long, POLY_SYS_amul)
    RegMask(amul,Mask_all)

    CALL_IO(div_long, POLY_SYS_adiv)
    RegMask(adiv,Mask_all)

    CALL_IO(rem_long, POLY_SYS_amod)
    RegMask(amod,Mask_all)

    CALL_IO(quotrem_long, POLY_SYS_quotrem)
    RegMask(quotrem,Mask_all)

! INLINE_ROUTINE(name) not optimised due to possible trap. SPF 17/7/96
#define ARBTEST(name, br_cond) \
    INLINE_ROUTINE(name); \
    sub %o0,1,%i4; \
    sub %o1,1,%i5; \
    tsubcctv    %i4,%i5,%g0; \
    br_cond 1f; \
    nop; \
    jmp %o7+6;  \
    mov FALSE,%o0;  \
1:  jmp %o7+6;  \
    mov TRUE,%o0


    ARBTEST(equal_long, be)
    ARBTEST(int_geq, bge)
    ARBTEST(int_leq, ble)
    ARBTEST(int_gtr, bg)
    ARBTEST(int_lss, bl)
    RegMask(equala,M_O0|M_O4|M_O7)
    RegMask(int_geq,M_O0|M_O4|M_O7)
    RegMask(int_leq,M_O0|M_O4|M_O7)
    RegMask(int_gtr,M_O0|M_O4|M_O7)
    RegMask(int_lss,M_O0|M_O4|M_O7)

INLINE_ROUTINE(or_long)
    and %o0,1,%i5   ! test for any long arguments
    andcc   %o1,%i5,%g0
    bz  or_really_long
    nop
    jmp %o7+6
    or  %o0,%o1,%o0
or_really_long:
    CALL_IO(or_long, POLY_SYS_ora)
    RegMask(ora,Mask_all)

INLINE_ROUTINE(and_long)
    and %o0,1,%i5   ! test for any long arguments
    andcc   %o1,%i5,%g0
    bz  and_really_long
    nop
    jmp %o7+6
    and %o0,%o1,%o0
and_really_long:
    CALL_IO(and_long, POLY_SYS_anda)
    RegMask(anda,Mask_all)

INLINE_ROUTINE(xor_long)
    and %o0,1,%i5   ! test for any long arguments
    andcc   %o1,%i5,%g0
    bz  xor_really_long
    nop
    xor %o0,%o1,%i4 ! This will zero the tag field (tags were equal)
    jmp %o7+6
    or  %i4,1,%o0   ! restore the tag bit
xor_really_long:
    CALL_IO(xor_long, POLY_SYS_xora)
    RegMask(xora,Mask_all)

INLINE_ROUTINE(is_shorta)
    and %o0,1,%i4   ! %i4 = 1 for short, 0 for others
    sll %i4,TAGSHIFT,%i4
    jmp %o7+6
    add %i4,1,%o0   ! Tag and return it
    RegMask(is_short,M_O0|M_O4|M_O7)
    
/* These are the same as int_eq/neq.  These were previously distinct
   because pointer equality required special code in the old persistent
   store system.  That is no longer relevant. */
    TEST(word_eq, be)
    TEST(word_neq, bne)
    
INLINE_ROUTINE(load_byte)
/* We can assume index will not overflow 30 bits
   Shouldn't we check for Range though? */
    sra %o1,TAGSHIFT,%i5    ! was srl
    ldub    [%o0+%i5],%i4
    sll %i4,TAGSHIFT,%i4    ! tag it
    jmp %o7+6
    add %i4,1,%o0
    RegMask(load_byte,M_O0|M_O4|M_O7)

INLINE_ROUTINE(load_word)
/* We can assume index will not overflow 30 bits
   Shouldn't we check for Range though? */
    sub %o1,1,%i5   ! Remove tag bit - the result is a word offset
    jmp %o7+6
    ld  [%o0+%i5],%o0   ! /* Load in the delay slot wouldn't work on Dave's system */ 
    RegMask(load_word,M_O0|M_O4|M_O7)


INLINE_ROUTINE(assign_byte)
/* We can assume index will not overflow 30 bits
   Shouldn't we check for Range though? */
    srl %o1,TAGSHIFT,%i4     ! Remove tag on arg2
    
    srl %o2,TAGSHIFT,%i5     ! Remove tag on arg3

    stb %i5,[%o0+%i4]
    jmp %o7+6
    mov UNIT,%o0         ! result of operation is unit

    RegMask(assign_byte,Mask_all)

INLINE_ROUTINE(assign_word)
/* We can assume index will not overflow 30 bits
   Shouldn't we check for Range though? */
! The following only works if TAGSHIFT = WORDSHIFT
    sub %o1,1,%i4        ! Remove tag bit on arg2 (but keep shift)
    st  %o2,[%o0+%i4]
    jmp %o7+6
    mov UNIT,%o0         ! result of operation is unit
    RegMask(assign_word,Mask_all)

INLINE_ROUTINE(string_length)
    andcc   %o0,TAGBITS,%g0 ! Single char strings are represented by the character
    bz,a    sl1
    ld  [%o0],%i4   ! Get length field

! a single character    
    jmp %o7+6       ! Return TAGGED(1)
    mov TAGGED(1),%o0

! not a single character string
sl1:    
    sll %i4,TAGSHIFT,%i4    ! Return tagged length
    jmp %o7+6
    add %i4,1,%o0
    RegMask(string_length,M_O0|M_O4|M_O7)

! Store the length of a string in the first word.
INLINE_ROUTINE(set_string_length_a)
    srl %o1,TAGSHIFT,%i4    ! Untag the length
    st  %i4,[%o0]
    jmp %o7+6           ! Return UNIT
    mov UNIT,%o0

    RegMask (set_string_length,M_O0|M_O4|M_O7)

   
INLINE_ROUTINE(is_big_endian)
    jmp %o7+6
    mov TRUE,%o0    !(DELAY SLOT)   SPARC is big-endian
    RegMask(is_big_endian,M_O0|M_O4|M_O7)

INLINE_ROUTINE(bytes_per_word)
    jmp %o7+6
    mov TAGGED(4),%o0   !(DELAY SLOT)   4 bytes per word
    RegMask(bytes_per_word,M_O0|M_O4|M_O7)


! 30-bit signed multiply routine. Takes two tagged 31 bit integers in %o0 and
! %o1 and returns a tagged result in %o2, clearing the Z condition code if
! there has been an overflow.
! It does not modify %o0 or %o1 so that it can be used for arbitrary
! precision multiplies which overflow.
! This code is almost a direct copy of the signed multiply routine in
! the Sparc Architecture Manual.
mul_signed:
    sub %o1,1,%o1   ! Just remove tag bit
    sra %o0,TAGSHIFT,%o4    ! Shift other arg
    mov %o4,%y      ! multiplier to y reg
! We have to allow 3 instruction after the mov %o4,%y before the first
! mulscc so we might as well use that time to see if we can do it quicker.
    andncc  %o4,0xfff,%g0
    be  mls3
    andcc   %g0,%g0,%o4 ! (DELAY SLOT) zero pp and clear N and V
    mulscc  %o4,%o1,%o4 ! first iteration
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4 ! 32nd iteration
    mulscc  %o4,%o1,%o4 ! last iteration only shifts
/* Correction for negative multiplier.  Since we don't actually use the
   high order part the only reason for doing this is to get the overflow
   check right. */
    tst %o0
    rd  %y,%o2
    bge mls1
    nop         ! Delay slot
    sub %o4,%o1,%o4
mls1:
    add %o1,1,%o1   ! Restore o1 to original value.
! Now check for overflow, setting Z if no overflow
    addcc   %o2,1,%o2   ! Set tag bit in result and test.
    bge mls2
    cmp %o4,0       ! (DELAY SLOT) no overflow if = 0
    cmp %o4,-1      ! no overflow if = -1
mls2:
    mov 1,%o4       ! clear o4 (which will be invalid).
    jmp %o7+8
    clr %o7
!
! Quicker version for small multipliers.
mls3:
    mulscc  %o4,%o1,%o4 ! First iteration of 13
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4
    mulscc  %o4,%o1,%o4 ! 12th iteration
    mulscc  %o4,%o1,%o4 ! last iteration only shifts
!
    rd  %y,%o5
    sll %o4,12,%o2
    srl %o5,20,%o5
    orcc    %o5,%o2,%o2
    add %o2,1,%o2   ! Set tag bit in result.
    mov UNIT,%o5    ! Clear o5 (invalid value).
    add %o1,1,%o1   ! Restore o1 to original value.
! Now check for overflow, setting Z if no overflow
    sra %o4,20,%o4
    bge mls4
    cmp %o4,0
    cmp %o4,-1
mls4:
    mov UNIT,%o4    ! clear o4 (which will be invalid).
    jmp %o7+8
    clr %o7

/*
! Synchronise the i-cache with the d-cache following a garbage collection
! Necessary on some machines, but not on others - it depends whether
! or not the hardware performs automatic synchronisation. I don't (yet)
! know how to find out what sort of machine we're running on, so
! (conservatively) assume we always have to flush the cache.
! SPF 18/12/95

! The address to start the flush is on %o0, the length in bytes is in %o1
! This routine corrupts %o0, %o1 and %o2. It doesn't matter if we flush
! too much, so we make the main loop execute 4 flush instructions to
! reduce the loop overhead. If extra "flush" instruction caused a problem,
! we would have to rewrite the loop initialisation code to be more careful.
! For multiprocessor v9 implementations, we should probably have some
! "membar" instructions in this routine (at the start). However, since v8
! processors don't support this instruction, and since Poly/ML is designed
! to run in a single thread (on a single processor), I'm not going to do
! this. SPF 18/12/95
*/
globldec(SparcAsmFlushInstructionCache)
    and %o0,0x7,%o2
    andn    %o0,0x7,%o0 ! align on double word
    add %o1,%o2,%o1 ! adjust byte-count to compensate

fic1:
    subcc   %o1,32,%o1  ! decrement byte-count
    flush   %o0
    flush   %o0+8
    flush   %o0+16
    flush   %o0+24
    bgt fic1        ! loop if byte-count is still positive
    add %o0,32,%o0  ! (DELAY SLOT)

    jmp %o7+8       ! return to caller
    nop

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

INLINE_ROUTINE(move_bytes)
! Move a segment of memory from one location to another.
! Must deal with the case of overlapping segments correctly.
! (source (o0), source_offset (o1), destination (o2), dest_offset (o3), length ([%g4]))
    srl %o1,TAGSHIFT,%i4     ! Remove tag on source offset
    srl %o3,TAGSHIFT,%i5     ! Remove tag on arg3

    ld  [%g4],%i3        ! length
    add %i4,%o0,%i4      ! source address
    srl %i3,TAGSHIFT,%i3     ! untagged length
    cmp %i3,0
    be  mvb3
    add %i5,%o2,%i5      ! Use delay slot to calculate destination address
    cmp %i5,%i4          ! carry not set if dest >= source
! If dest >= src then use decrementing moves else
! use incrementing moves.
    bcs mvb2
    nop
    sub %i4,1,%i4
    sub %i5,1,%i5
    ldub    [%i4+%i3],%o0        ! get the first value before the loop
mvb1:   stb %o0,[%i5+%i3]
    subcc   %i3,1,%i3
    bne,a   mvb1
    ldub    [%i4+%i3],%o0        ! use delay slot to load next value
    ba  mvb3
    nop

mvb2:   ldub    [%i4],%o0
    add %i4,1,%i4        ! incr source pointer
    stb %o0,[%i5]
    subcc   %i3,1,%i3
    bne mvb2
    add %i5,1,%i5        ! use delay slot to increment dest ptr.

mvb3:   add %g4,4,%g4        ! pop the extra arg from the stack
    jmp %o7+6
        mov UNIT,%o0         ! result of operation is unit

    RegMask(move_bytes,Mask_all) ! Should really be the union of the local and non-local cases

INLINE_ROUTINE(move_words)
    CALL_IO(move_words_long_, POLY_SYS_move_words)
    RegMask(move_words,Mask_all)

! Word functions.  These are all unsigned and do not raise Overflow

INLINE_ROUTINE(mul_word)
/* mul_signed works equally for signed or unsigned arithmetic if we're not
   interested in overflow. */
    mov %o7,%i5     ! (DELAY SLOT) Save link reg
    call    mul_signed
    nop
    mov %i5,%o7     ! (DELAY SLOT) Restore link
    jmp %o7+6
    mov %o2,%o0     ! Get result.
    RegMask(mul_word,Mask_all)

INLINE_ROUTINE(plus_word)
    sub     %o1,1,%i5           ! Remove a tag
    jmp     %o7+6
    add     %o0,%i5,%o0         ! Add the values
    RegMask(plus_word,M_O0|M_O4|M_O7)

INLINE_ROUTINE(minus_word)
    sub     %o1,1,%i5           ! Remove a tag
    jmp     %o7+6
    sub     %o0,%i5,%o0         ! Subtract the untagged value.
    RegMask(minus_word,M_O0|M_O4|M_O7)

! Unsigned operations for word_div and word_rem
    CALL_IO(div_word, POLY_SYS_div_word)
    CALL_IO(mod_word, POLY_SYS_mod_word)

! Unsigned tests on words.
    TEST(word_geq,bcc)

    TEST(word_leq,bleu)

    TEST(word_gtr,bgu)

    TEST(word_lss,bcs)

INLINE_ROUTINE(atomic_incr)
    membar #StoreLoad | #LoadLoad ! Pre-atomic barrier
ati1:
    ld     [%o0],%i4            ! First word of mutex
    add    %i4,shiftup(1,TAGSHIFT),%i5 ! Add one
    cas    [%o0],%i4,%i5        ! Try to update it
    cmp    %i4,%i5              ! Did it work?
    bne,pn %icc,ati1
    nop                         ! (DELAY SLOT)
    add    %i4,shiftup(1,TAGSHIFT),%o0 ! Put the updated value in %o0
    ba,pt  %xcc,ati2
    membar #StoreLoad | #StoreStore
ati2:
    jmp    %o7+6
    nop
    RegMask (atomic_incr,M_O0|M_O4|M_O7)

INLINE_ROUTINE(atomic_decr)
    membar #StoreLoad | #LoadLoad ! Pre-atomic barrier
atd1:
    ld     [%o0],%i4            ! First word of mutex
    sub    %i4,shiftup(1,TAGSHIFT),%i5 ! Subtract one
    cas    [%o0],%i4,%i5        ! Try to update it
    cmp    %i4,%i5              ! Did it work?
    bne,pn %icc,atd1
    nop                         ! (DELAY SLOT)
    sub    %i4,shiftup(1,TAGSHIFT),%o0 ! Put the updated value in %o0
    ba,pt  %xcc,atd2
    membar #StoreLoad | #StoreStore
atd2:
    jmp    %o7+6
    nop
    RegMask (atomic_decr,M_O0|M_O4|M_O7)

INLINE_ROUTINE(thread_self)
    jmp    %o7+6
    ld     MR_THREADID,%o0     ! (DELAY SLOT) Return thread id
    RegMask (thread_self,M_O0|M_O4|M_O7)

/* Register mask vector. - extern int registerMaskVector[];
   Each entry in this vector is a set of the registers modified
   by the function.  It is an untagged bitmap with the registers
   encoded in the same way as in the code generator.
   Unused entries are set to Mask_all for safety in case a new
   entry is added to the iovector without also adding an entry
   here. */
globldec(registerMaskVector)
    .long   Mask_all                /* 0 is unused */
    .long   Mask_finish             /* 1 */
    .long   Mask_all                /* 2 - now unused */
    .long   Mask_all                /* 3 is unused */
    .long   Mask_all                /* 4 is unused */
    .long   Mask_all                /* 5 is unused */
    .long   Mask_all                /* 6 - now unused */
    .long   Mask_all                /* 7 is unused */
    .long   Mask_all                /* 8 is unused */
    .long   Mask_change_dir         /* 9 */
    .long   Mask_all                /* 10 is unused */
    .long   Mask_alloc_store         /* 11 */
    .long   Mask_all                 /* 12 is unused */
    .long   Mask_all                 /* return = 13 */
    .long   Mask_all                 /* raisex = 14 */
    .long   Mask_get_length          /* 15 */
    .long   Mask_all                /* 16 is unused */
    .long   Mask_get_flags_          /* 17 */
    .long   Mask_all            /* 18 - now unused */
    .long   Mask_all             /* 19 - now unused */
    .long   Mask_all            /* 20 - now unused */
    .long   Mask_all                /* 21 is unused */
    .long   Mask_all                /* 22 is unused */
    .long   Mask_str_compare         /* 23 */
    .long   Mask_teststreq           /* 24 */
    .long   Mask_teststrneq          /* 25 */
    .long   Mask_teststrgtr          /* 26 */
    .long   Mask_teststrlss          /* 27 */
    .long   Mask_teststrgeq          /* 28 */
    .long   Mask_teststrleq          /* 29 */
    .long   Mask_exception_trace     /* 30 */
    .long   Mask_all         /* 31 */
    .long   Mask_all          /* 32 - now unused */
    .long   Mask_all          /* 33 - now unused */
    .long   Mask_all         /* 34 - now unused */
    .long   Mask_all         /* 35 - now unused */
    .long   Mask_all           /* 36 - now unused */
    .long   Mask_all                /* 37 is unused */
    .long   Mask_all                /* 38 is unused */
    .long   Mask_all                /* 39 is unused */
    .long   Mask_all                /* 40 */
    .long   Mask_all                /* 41 is unused */
    .long   Mask_all              /* 42 */
    .long   Mask_all              /* 43 */
    .long   Mask_all          /* 44 - now unused */
    .long   Mask_all         /* 45 - now unused */
    .long   Mask_all                 /* 46 - now unused */
    .long   Mask_lockseg             /* 47 */
    .long   Mask_all                 /* nullorzero = 48 */
    .long   Mask_all             /* 49 - now unused */
    .long   Mask_all           /* 50 - now unused */
    .long   Mask_Net_dispatch_       /* 51 */
    .long   Mask_OS_spec_dispatch_   /* 52 */
    .long   Mask_all                /* 53 - now unused */
    .long   Mask_all                /* 54 - now unused */
    .long   Mask_all                /* version_number = 55 */
    .long   Mask_all                /* 56 is unused */
    .long   Mask_all                /* 57 is unused */
    .long   Mask_all                /* 58 is unused */
    .long   Mask_all                /* 59 is unused */
    .long   Mask_all                /* 60 is unused */
    .long   Mask_IO_dispatch_        /* 61 */
    .long   Mask_Sig_dispatch_       /* 62 */
    .long   Mask_all                /* 63 is unused */
    .long   Mask_all                /* 64 is unused */
    .long   Mask_all                /* 65 is unused */
    .long   Mask_all                /* 66 is unused */
    .long   Mask_all                /* 67 is unused */
    .long   Mask_all                /* 68 is unused */
    .long   Mask_all                /* 69 is unused */
    .long   Mask_atomic_incr        /* 70 */
    .long   Mask_atomic_decr        /* 71 */
    .long   Mask_thread_self        /* 72 */
    .long   Mask_thread_dispatch    /* 73 */
    .long   Mask_all                /* 74 is unused */
    .long   Mask_all                /* 75 is unused */
    .long   Mask_all                /* 76 is unused */
    .long   Mask_all                /* 77 is unused */
    .long   Mask_all                /* 78 is unused */
    .long   Mask_all                /* 79 is unused */
    .long   Mask_all                /* Mask_version_number_1 = 80 */
    .long   Mask_all          /* 81 - now unused */
    .long   Mask_all                 /* 82 - now unused */
    .long   Mask_all                 /* 83 - now unused */
    .long   Mask_kill_self           /* 84 */
    .long   Mask_all                 /* 85 - now unused */
    .long   Mask_all                 /* 86 - now unused */
    .long   Mask_all                 /* 87 - now unused */
    .long   Mask_profiler            /* 88 */
    .long   Mask_all                /* 89 is unused */
    .long   Mask_all                /* 90 is unused */
    .long   Mask_all                /* 91 is unused */
    .long   Mask_full_gc_            /* 92 */
    .long   Mask_stack_trace_        /* 93 */
    .long   Mask_timing_dispatch_    /* 94 */
    .long   Mask_all                /* 95 is unused */
    .long   Mask_all                /* 96 is unused */
    .long   Mask_all                /* 97 is unused */
    .long   Mask_all                /* 98 is unused */
    .long   Mask_objsize_            /* 99 */
    .long   Mask_showsize_           /* 100 */
    .long   Mask_all                /* 101 is unused */
    .long   Mask_all                /* 102 is unused */
    .long   Mask_all                 /* 103 - now unused */
    .long   Mask_quotrem             /* 104 */
    .long   Mask_is_short            /* 105 */
    .long   Mask_aplus               /* 106 */
    .long   Mask_aminus              /* 107 */
    .long   Mask_amul                /* 108 */
    .long   Mask_adiv                /* 109 */
    .long   Mask_amod                /* 110 */
    .long   Mask_aneg                /* 111 */
    .long   Mask_xora                /* 112 */
    .long   Mask_equala              /* 113 */
    .long   Mask_ora                 /* 114 */
    .long   Mask_anda                /* 115 */
    .long   Mask_all                 /* version_number_3 = 116 */
    .long   Mask_Real_str            /* 117 */
    .long   Mask_Real_geq            /* 118 */
    .long   Mask_Real_leq            /* 119 */
    .long   Mask_Real_gtr            /* 120 */
    .long   Mask_Real_lss            /* 121 */
    .long   Mask_Real_eq             /* 122 */
    .long   Mask_Real_neq            /* 123 */
    .long   Mask_Real_dispatch       /* 124 */
    .long   Mask_Real_add            /* 125 */
    .long   Mask_Real_sub            /* 126 */
    .long   Mask_Real_mul            /* 127 */
    .long   Mask_Real_div            /* 128 */
    .long   Mask_all                 /* 129 is unused */
    .long   Mask_Real_neg            /* 130 */
    .long   Mask_all                 /* 131 is unused */
    .long   Mask_Real_repr           /* 132 */
    .long   Mask_Real_conv           /* 133 */
    .long   Mask_Real_int            /* 134 */
    .long   Mask_Real_float          /* 135 */
    .long   Mask_Real_sqrt           /* 136 */
    .long   Mask_Real_sin            /* 137 */
    .long   Mask_Real_cos            /* 138 */
    .long   Mask_Real_arctan         /* 139 */
    .long   Mask_Real_exp            /* 140 */
    .long   Mask_Real_ln             /* 141 */
    .long   Mask_all           /* 142 - now unused */
    .long   Mask_all                 /* 143 is unused */
    .long   Mask_all                 /* 144 is unused */
    .long   Mask_all                 /* 145 is unused */
    .long   Mask_all                 /* 146 is unused */
    .long   Mask_all                 /* 147 is unused */
    .long   Mask_all                 /* stdin = 148 */
    .long   Mask_all                 /* stdout= 149 */
    .long   Mask_process_env_dispatch_   /* 150 */
    .long   Mask_set_string_length       /* 151 */
    .long   Mask_get_first_long_word     /* 152 */
    .long   Mask_all                 /* 153 is unused */
    .long   Mask_all                 /* 154 is unused */
    .long   Mask_all                 /* 155 is unused */
    .long   Mask_all                 /* 156 is unused */
    .long   Mask_all                 /* 157 is unused */
    .long   Mask_all                 /* 158 is unused */
    .long   Mask_all                 /* 159 is unused */
    .long   Mask_all                 /* 160 is unused */
    .long   Mask_all                 /* 161 is unused */
    .long   Mask_all                 /* 162 is unused */
    .long   Mask_all                 /* 163 is unused */
    .long   Mask_all                 /* 164 is unused */
    .long   Mask_all                 /* 165 is unused */
    .long   Mask_all                 /* 166 is unused */
    .long   Mask_all                 /* 167 is unused */
    .long   Mask_all                 /* 168 is unused */
    .long   Mask_all                 /* 169 is unused */
    .long   Mask_all                 /* 170 is unused */
    .long   Mask_all                 /* 171 is unused */
    .long   Mask_all                 /* 172 is unused */
    .long   Mask_all                 /* 173 is unused */
    .long   Mask_all                 /* 174 is unused */
    .long   Mask_all                 /* 175 is unused */
    .long   Mask_all                 /* 176 is unused */
    .long   Mask_all                 /* 177 is unused */
    .long   Mask_all                 /* 178 is unused */
    .long   Mask_all                 /* 179 is unused */
    .long   Mask_all                 /* 180 is unused */
    .long   Mask_all                 /* 181 is unused */
    .long   Mask_all                 /* 182 is unused */
    .long   Mask_all                 /* 183 is unused */
    .long   Mask_all                 /* 184 is unused */
    .long   Mask_all                 /* 185 is unused */
    .long   Mask_all                 /* 186 is unused */
    .long   Mask_all                 /* 187 is unused */
    .long   Mask_all                 /* 188 is unused */
    .long   Mask_io_operation        /* 189 */
    .long   Mask_all                 /* 190 is unused */
    .long   Mask_all           /* 191 - now unused */
    .long   Mask_all                 /* 192 is unused */
    .long   Mask_all                 /* 193 is unused */
    .long   Mask_set_code_constant   /* 194 */
    .long   Mask_move_words          /* 195 */
    .long   Mask_shift_right_arith_word  /* 196 */
    .long   Mask_all                 /* 197 */
    .long   Mask_move_bytes          /* 198 */
    .long   Mask_all                 /* 199 now unused */
    .long   Mask_set_flags_          /* 200 */
    .long   Mask_shrink_stack_       /* 201 */
    .long   Mask_all                 /* stderr = 202 */
    .long   Mask_all                 /* 203 now unused */
    .long   Mask_callcode_tupled     /* 204 */
    .long   Mask_foreign_dispatch_   /* 205 */
    .long   Mask_all                 /* 206 now unused */
    .long   Mask_all                 /* 207 is unused */
    .long   Mask_all                 /* 208 now unused */
    .long   Mask_XWindows_           /* 209 */
    .long   Mask_all                 /* 210 is unused */
    .long   Mask_all                 /* 211 is unused */
    .long   Mask_all                 /* 212 is unused */
    .long   Mask_is_big_endian       /* 213 */
    .long   Mask_bytes_per_word      /* 214 */
    .long   Mask_offset_address      /* 215 */
    .long   Mask_shift_right_word    /* 216 */
    .long   Mask_word_neq            /* 217 */
    .long   Mask_not_bool            /* 218 */
    .long   Mask_all                 /* 219 is unused */
    .long   Mask_all                 /* 220 is unused */
    .long   Mask_all                 /* 221 is unused */
    .long   Mask_all                 /* 222 is unused */
    .long   Mask_string_length       /* 223 */
    .long   Mask_all                 /* 224 is unused */
    .long   Mask_all                 /* 225 is unused */
    .long   Mask_all                 /* 226 is unused */
    .long   Mask_all                 /* 227 is unused */
    .long   Mask_all                 /* 228 is unused */
    .long   Mask_int_eq              /* 229 */
    .long   Mask_int_neq             /* 230 */
    .long   Mask_int_geq             /* 231 */
    .long   Mask_int_leq             /* 232 */
    .long   Mask_int_gtr             /* 233 */
    .long   Mask_int_lss             /* 234 */
    .long   Mask_all                 /* 235 - now unused */
    .long   Mask_all                 /* 236 is unused */
    .long   Mask_all                 /* 237 is unused */
    .long   Mask_mul_word            /* 238 */
    .long   Mask_plus_word           /* 239 */
    .long   Mask_minus_word          /* 240 */
    .long   Mask_div_word            /* 241 */
    .long   Mask_or_word             /* 242 */
    .long   Mask_and_word            /* 243 */
    .long   Mask_xor_word            /* 244 */
    .long   Mask_shift_left_word     /* 245 */
    .long   Mask_mod_word            /* 246 */
    .long   Mask_word_geq            /* 247 */
    .long   Mask_word_leq            /* 248 */
    .long   Mask_word_gtr            /* 249 */
    .long   Mask_word_lss            /* 250 */
    .long   Mask_word_eq             /* 251 */
    .long   Mask_load_byte           /* 252 */
    .long   Mask_load_word           /* 253 */
    .long   Mask_assign_byte         /* 254 */
    .long   Mask_assign_word         /* 255 */

