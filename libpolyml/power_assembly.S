/*
    Title:  Assembly code for Power architecture.

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/*
 Linkage conventions:

r0 scratch register (unsaved?)
r1 - don't touch - dedicated C register (stack - like SPARC %o6) 
r2 - don't touch - dedicated C register (TOC)
r3      used for the first argument to a function, and for the result.
r4-r6  used for the next 3 args, any others being passed on the stack.
(We may later decide to use r7-r10 for parameters too).
 
r24        is the closure pointer or static link pointer (like SPARC %o5)
rr (rr)   is used as the compiler-visible link register (like SPARC %o7)
r26        RTS scratch register
r27 (rsp)  is the ML stack pointer,
r28        is no longer used (previously the stack limit register)
r29 (rhp)  is the heap pointer,
r30 (rhl)  is the heap limit,
r31 (rhr)  points to the top exception handler.

r7-r10 and r14-r22 (14 registers) are available as general work registers,
as are r3-r6 and r24-rr, when they are not fulfilling their specialised
duties. That's a total of 20 general-purpose registers (as opposed to
17 on the SPARC).

r11, r12 are used as code-generator visible untagged registers.
r26 is used as a compiler-invisible RTS scratch register for
handling traps.

Note: the RS/6000 follows a callee-saves convention for r13-r31
inclusive, so we'll have to be careful to save these registers
in the when we first enter ML. We can remove this later if it
appears to be unnecessary.

An RTS function is entered with the return address in LR.
Since this isn't a saved register, it must be copied (or'ed with 2)
into rr if the function executes a trap (which may or may not copy
rr back into LR) or explicitly calls C.

Choosing 2 us to simplify the return code, since we don't have to
subtract RETURNOFFSET before moving the result to LR (since the least
2 significant bits of the return address are ignored).

Returning from a function then looks like:

    mtlr    rr
    blr

on the assumption that the return-address is still in rr.
However, we needn't (shouldn't) be quite a naive as this, since
we can get better super-scalar performance by interleaving these
instructions with the last few "real" instructions of the function.

All non-scratch registers MUST contain properly tagged values if
there any a possibility of a garbage collection. 
*/

/* AIX code now removed DCJM June 2006. */

/***************************************************************************/
/* Useful macro definitions                                                */
/***************************************************************************/
/* Include sys.h to pick up the POLY_SYS entries we need here. */
#include "sys.h"

#ifdef MACOSX
#define gbl(id) _##id
#else
#define gbl(id)  id
#endif

#define gbla(id) id##a

#define gblc(id) gbl(id##c)


#define RETURNOFFSET (2)

#define shiftup(word,places) ((word)<<(places))
#define shiftdown(word,places) ((word)>>(places))

#define TAGSHIFT 1
#define TAGBITS (0x1)

#define TAGGED(n) (shiftup(n,TAGSHIFT)+1)
#define TRUE  TAGGED(1)
#define FALSE TAGGED(0)
#define UNIT TAGGED(0)

/* generate standard prelude */

#define globldec(id) .globl gbl(id) ; gbl(id) :

/* Standard start-up for inline calls (those that don't call C) */
#define INLINE_ROUTINE(id) \
    globldec(id) 

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

/* Bits in the flags byte in the length word of an object. */
#define B_bytes         1
#define B_code          2
#define B_mutable       64


/***************************************************************************/
/* General Assembler Initialisation                                        */
/***************************************************************************/

#ifdef MACOSX
    /* untagged registers */
#define rtemp1  r11 /* used by compiler */
#define rtemp2  r12 /* used by compiler */
#define rtemp3  r26 /* scratch */

    /* special purpose */
#define rr  r25 /* regReturn  */
    
    /* dedicated ML registers */
#define rsp r27
#define rhp r29
#define rhl r30
#define rhr r31
#else

    /* untagged registers */
    .set r0,0   /* used by compiler, but scratch in a trap handler */
    .set r1,1
    .set rtemp1,11  /* used by compiler */
    .set rtemp2,12  /* used by compiler */
    .set rtemp3,26  /* scratch */
    
    /* parameter registers */
    .set r3,3   /* arg 1 and result */
    .set r4,4   /* arg 2 */
    .set r5,5   /* arg 3 */
    .set r6,6   /* arg 4 */

    /* general registers */
    .set r7,7
    .set r8,8
    .set r9,9
    .set r10,10

    .set r13,13
    .set r14,14
    .set r15,15
    .set r16,16
    .set r17,17
    .set r18,18
    .set r19,19
    .set r20,20
    .set r21,21
    .set r22,22

    /* special purpose */
    .set r23,23 /* regCode    */
    .set r24,24 /* regClosure */
    .set rr,25  /* regReturn  */
    
    /* dedicated ML registers */
    .set rsp,27
    .set rhp,29
    .set r28,28
    .set rhl,30
    .set rhr,31
#endif

;# Register mask entries - must match coding used in codeCons.ML
#define     M_R3        0x000001
#define     M_R4        0x000002
#define     M_R5        0x000004
#define     M_R6        0x000008
#define     M_R7        0x000010
#define     M_R8        0x000020
#define     M_R9        0x000040
#define     M_R10       0x000080
#define     M_R14       0x000100
#define     M_R15       0x000200
#define     M_R16       0x000400
#define     M_R17       0x000800
#define     M_R18       0x001000
#define     M_R19       0x002000
#define     M_R20       0x004000
#define     M_R21       0x008000
#define     M_R22       0x010000
#define     M_R23       0x020000
#define     M_R24       0x040000
#define     M_R25       0x080000
#define     M_RR        M_R25

#define     RegMask(name,mask) .set Mask_##name,mask

/* Default mask for unused entries and also for the special cases
   where we don't know what the effect of calling the function
   will be. */
    RegMask(all,0x0fffff)

/***************************************************************************/
/* Standard function prelude stuff                                         */
/***************************************************************************/
    .set    argarea,    32
    .set    linkarea,   24
    .set    locstkarea, 0
    .set    nfprs,      0           /* FP registers saved - none. */
    .set    ngprs,      (32-13)     /* GP Registers save - r13 to r32 */
    .set    szdsa,      8*nfprs+4*ngprs+linkarea+argarea+locstkarea

/***************************************************************************/
/* Branch-prediction mnemonics                                             */
/***************************************************************************/

/* 
   The AIX 3.2 assembler doesn't support branch-prediction mnemonics.
   Worse than that, it silently generates bad branches, because it
   parses:
   
    beq-    neg_long1
     
   as:

    beq - neg_long1
   
   With the result that the code just wanders off in random directions.
   This took me a (very frustrating) day to find.
   
   SPF 31/8/95
*/
   
   
   

#define beqM beq-
#define bsoM bso-
#define bgtP bgt+
#define bltP blt+
#define beqP beq+
#define bneP bne+
#define bltlrP bltlr+

/***************************************************************************/
/* Code fragments used by CALL_IOn macros                                  */
/***************************************************************************/


/***************************************************************************

   This code is used to return from the RTS to ML under 2 different
   circumstances:
    
      (1) Following execution of a "trap" instruction (heap or
          stack overflow, or arbitrary-precision emulation required).
          
      (2) Following a call to the RTS from ML.
      
   Note that SIGALRM and SIGVTALRM interrupts don't use this mechanism -
   the normal return-from-interrupt mechanism is used to restore the
   execution context as it was before the interrupt (apart from a
   possible change to the stack limit register).
   
   Since the RTS may process-switch rather than immediately execute an
   input operation (amongst others), it is necessary to save the state
   in such a way that the operation can be restarted. Since it is
   also necessary that the saved state has *no* pointers into the RTS
   (since another process might commit the state), we save the state
   immediately before the RTS call. This ensures that when the process
   is restarted, it will re-execute the RTS call and all will be well.
   
   (OLD COMMENT:
   We also take the opportunity, when performing an RTS call, to zap
   all the registers not actually used in the call. This enables
   us to reduce the amount of random garbage that the machine holds
   onto in rarely used registers.)
   This has been removed now that we attempt to avoid pushing registers
   that are not modified in a call.  I don't know whether it was important
   anyway. DCJM 29/11/00
   
   A trap saves more state than an RTS call, since we want to be able
   to treat it as a normal instruction. We can't zap any registers
   (except that r0 is regarded as being volatile across traps) and
   we should alter only registers "documented" as being changed
   by the trap.
   
   The actual saving of the trap state is currently performed in C
   (it makes the C a little easier to follow), rather than using
   MD_trap_handler here. (This is part of my drive to reduce the
   amount of assembly code in the system.) The state is
   restored using MD_switch_to_poly_X, just as for RTS calls
   that need to call C.
   
   Exception: MD_trap_handler passes LR as a parameter to MD_trap_handler1,
   since this register is needed to when the native code calls
   interpreted code (it contains the return address).
   
   The RTS indicates that it wants to retry the RTS call by calling
   MD_set_for_retry in the C. All this does is to set
   "poly_stack->p_pc" to TAGGED(0). This is treated as a special
   value by "MD_switch_to_poly_X", which then re-executes the call,
   rather than simply returning.
    
   Note: I've managed to simplify the code compared with the SPARC
   version. In particular, I've inlined "return_from_io" and
   removed "RTD0" completely. Since the latter was an RTS address
   getting saved in "poly_stack->p_pc", I'm not at all sure that the
   SPARC version was actually commit-safe. I must investigate this
   sometime.
   
   SPF 7/8/95.

    
***************************************************************************/


/***************************************************************************/
/* set_registers_for_retry                                                 */
/***************************************************************************/
/*
Sets up the sp, pc and hr values, saves the parameter registers and
zaps the tagged registers (except for r24 and rr, which are used for
retrying the function call).
*/

/*
Offset 0    p_space
       4    p_pc
       8    p_sp
      12    p_hr
      16    p_nregs (22)
      20    r3-r10  (8 registers)
      52    r14-rr (12 registers)
     100    link register
     104    number of untagged registers (3)
     108   rtemp1
     112    rtemp2
     116   CR
*/

/*
Offsets in MemRegisters
     0     inRTS
     4     requestCode
     8     returnReason
    12     heapPointer
    16     heapBase
    20     polyStack
    24     stackLimit
    56     stackTop
    60     threadId
*/

/***************************************************************************/
/* PPCAsmSwitchToPoly                              */
/***************************************************************************/

/* Entry point for C */
globldec(PPCAsmSwitchToPoly)

/* Standard prelude for a C function to support calls from ML to C */
    mflr    r0
    mfcr    rtemp2
    stmw    r13,-8*nfprs-4*ngprs(r1) /* DCJM.  Formerly stm. */
#ifdef MACOSX
    stw     r0,8(r1)                 /* Return address */
    stw     rtemp2,4(r1)             /* Condition codes. */
#else
/* Linux saves the return address in the second word. */
    stw     r0,4(r1)
#endif
    stwu    r1,-szdsa(r1)
    
    mr      r13,r3                   /* Move the MemRegisters value into r13 */

    lwz     rtemp3,20(r13)              /* rt3 =  poly_stack */
    
    lwz     r28,24(r13)                 /* Load the old stack limit reg for backwards compat. */
    
    /* Load rsp and rhr now in case of profile trap */
    lwz     rsp,8(rtemp3)
    lwz     rhr,12(rtemp3)

    /* Now rsp, rhr and rsl have been loaded we can clear inRTS */
    li      r0,0
    stw     r0,0(r13)             /* clear "in_run_time_system" */

    lwz     rhp,12(r13)           /* Load rhp from heapPointer */
    lwz     rhl,16(r13)           /* Set rhl to the number of free bytes */

        /* Save the ML stack pointer and handler registers */
    stw rsp,8(rtemp3)
    stw rhr,12(rtemp3)
   
    /* reload the parameter registers */
    lwz r3,20(rtemp3)
    lwz r4,24(rtemp3)
    lwz r5,28(rtemp3)
    lwz r6,32(rtemp3)
    lwz r7,36(rtemp3)
    lwz r8,40(rtemp3)
    lwz r9,44(rtemp3)
    lwz r10,48(rtemp3)
    
    /* reload the general registers */
    lwz r14,52(rtemp3)
    lwz r15,56(rtemp3)
    lwz r16,60(rtemp3)
    lwz r17,64(rtemp3)
    lwz r18,68(rtemp3)
    lwz r19,72(rtemp3)
    lwz r20,76(rtemp3)
    lwz r21,80(rtemp3)
    lwz r22,84(rtemp3)
    lwz r23,88(rtemp3)
    lwz r24,92(rtemp3)
    lwz rr,96(rtemp3)

    /* reload the scratch registers */
    lwz rtemp1,108(rtemp3)
    lwz rtemp2,112(rtemp3)
    
    lwz     r0,100(rtemp3)      /* Link register. */
    mtlr    r0

    li      r0,0            /* Clear overflow bit. */
    mtxer   r0              /* mcrxr is being phased out. */

    lwz     r0,116(rtemp3)      /* condition codes */

    lwz     rtemp3,4(rtemp3)    /* pc */

    mtctr   rtemp3          /* set up return address */
    mtcr    r0              /* restore condition codes */
    bctr                    /* complete return */


globldec(PPCSaveStateAndReturn)
PPCSaveStateAndReturn1:
/* Code to save the state and switch to C.  This is used both for
   IO Calls and also for traps so it needs to save everything.
   DOESN'T put anything into poly_stack->p_pc.  This may be set by the trap
   handler if we have come by way of a trap or by the CALL_IO functions.   */
    lwz     rtemp3,20(r13)        /* rt3 =  poly_stack */
    stw     rsp,8(rtemp3)       /* poly_stack->p_sp */
    stw     rhr,12(rtemp3)      /* poly_stack->p_hr */

    /* save the parameter registers */
    stw     r3,20(rtemp3)       /* save r3 */
    stw     r4,24(rtemp3)       /* save r4 */
    stw     r5,28(rtemp3)       /* save r5 */
    stw     r6,32(rtemp3)       /* save r6 */

    /* and the general registers */
    stw     r7,36(rtemp3)       /* save r7 */
    stw     r8,40(rtemp3)       /* save r8 */
    stw     r9,44(rtemp3)       /* save r9 */
    stw     r10,48(rtemp3)      /* savep r10 */
    
    stw     r14,52(rtemp3)      /* save r14 */
    stw     r15,56(rtemp3)      /* save r15 */
    stw     r16,60(rtemp3)      /* save r16 */
    stw     r17,64(rtemp3)      /* save r17 */
    stw     r18,68(rtemp3)      /* save r18 */
    stw     r19,72(rtemp3)      /* save r19 */
    stw     r20,76(rtemp3)      /* save r20 */
    stw     r21,80(rtemp3)      /* save r21 */
    stw     r22,84(rtemp3)      /* save r22 */
    stw     r23,88(rtemp3)      /* save r23 */
 
    /* include the untagged registers */
    stw     rtemp1,108(rtemp3)
    stw     rtemp2,112(rtemp3)
   
    /* and the closure pointer (used for linkage) */
    stw     r24,92(rtemp3)      /* save r24 (closure pointer) */
    stw     rr,96(rtemp3)      /* save rr (genuine return address) */
    mflr    rtemp2
    stw     rtemp2,100(rtemp3)  /* save link reg. If we've come by way of CALL_IO this
                                   will have to be moved into the p_pc field and this value
                                   must be overwritten. */

    li      rtemp1,1
    stw     rtemp1,0(r13)       /* Initialise in_run_time_system */

    stw     rhp,12(r13)                   /* heapPointer = rhp */

    /* Reload the registers and return to C. */
    lwz     r1,0(r1)
#ifdef MACOSX
    lwz     r0,8(r1)
    lwz     rtemp1,4(r1)
    mtcr    rtemp1
#else
    lwz     r0,4(r1)
#endif
    mtlr    r0
    lmw     r13,-8*nfprs-4*ngprs(r1)
    blr




/***************************************************************************/
/* Standard C call macros                                                  */
/***************************************************************************/

/*
Define standard call macros. They are of the form
CALL_IOn(name, res), where n is the number of arguments.
The result mode is either IND if the result is by reference and NOIND if it
is not. The reason arguments or results may be passed by reference is that
the garbage-collector may more objects on the heap but will only update
values on the Poly stack. REF arguments are copied to the save_vec and the
address of the entry on it is returned.
*/

/***************************************************************************/
/* CALL_IO                                                                */
/***************************************************************************/
#define CALL_IO(name, ioCall) \
globldec(gbla(name)) \
    li      rtemp2,ioCall; \
    stw     rtemp2,4(r13); \
    b       PPCSaveStateAndReturn1; \
    RegMask(name,Mask_all)

#define  CALL_IO_LOCAL(name, ioCall) \
        /*EXTERN(gblc(name));*/ \
    li      rtemp2,ioCall; \
    stw     rtemp2,4(r13); \
    b       PPCSaveStateAndReturn1; \
    RegMask(name,Mask_all)

    
    CALL_IO(kill_self, POLY_SYS_kill_self)

/* alloc_uninit.  Allocate without necessarily initialising.  Just use
   alloc_store to do this. */
INLINE_ROUTINE(alloc_uninit)
    li      r5,TAGGED(0)
	/* And drop through to alloc_store. */
    RegMask(alloc_uninit,(M_R3|M_RR|M_R4|M_R23))

/* alloc(size, flags, initial). 
   Allocates a segment of a given size and initialises it.
   This is primarily used for arrays and for strings.  Refs are
   allocated using inline code */
INLINE_ROUTINE(alloc_store)
  /* First check that the length is acceptable */
    andi.   r0,r3,1
    beq     alloc_in_rts            /* Get the RTS to raise the exception. */
    srawi.  rtemp1,r3,TAGSHIFT      /* Remove tag */
    bne     allst0                  /* (test for 0) Make zero sized objects 1 */
    li      rtemp1,1                /* because they mess up the g.c. */
    li      r3,TAGGED(1)
allst0:
    rlwinm. r0,rtemp1,0,0,TYPESHIFT /* Length field must fit in 24 bits */
    addi    rtemp2,rtemp1,1         /* Add 1 word for length word. */
    bne     alloc_in_rts
    slwi    rtemp2,rtemp2,2         /* Get length in bytes */
    sub     rhp,rhp,rtemp2          /* Allocate the space */
    cmplw   rhp,rhl
    blt     alloc_in_rts0
    ori     r4,r4,TAGGED(B_mutable) /* Set the mutable bit in the flags. */
    rlwinm  rtemp3,r4,32-TYPESHIFT-TAGSHIFT,0,7 /* Get flags byte (untagged). */
    or      rtemp3,rtemp1,rtemp3    /* Combine flags and length word. */
    stw     rtemp3,0(rhp)           /* Store length word. */
    addi    r3,rhp,4                /* Point to first "real" word of seg. */

 /* Initialise the store. */
    cmplwi  r4,TAGGED(B_mutable|B_bytes)    /* Byte segment? */
    mr      rtemp2,r5               /* Initialiser word. */
    bne     allst1

 /* If this is a byte seg.  Set the initialiser word to 4 bytes of
    the untagged initialisation byte. */
    srawi   rtemp2,r5,TAGSHIFT
    slwi    rtemp3,rtemp2,8
    or      rtemp2,rtemp2,rtemp3
    slwi    rtemp3,rtemp3,8
    or      rtemp2,rtemp2,rtemp3
    slwi    rtemp3,rtemp3,8
    or      rtemp2,rtemp2,rtemp3

allst1:
    mtctr   rtemp1
    subi    rtemp1,r3,4             /* Start of object minus 4 bytes. */

allst2:
    stwu    rtemp2,4(rtemp1)        /* Set the word. */
    bdnz    allst2

    blr

alloc_in_rts0:
/* We don't have enough space - call the RTS to do the allocation.  This is preferable
   to treating it as though we'd run out of store in compiled code because that would
   leave us with a return address into this code. */
    add   rhp,rhp,rtemp2            /* Add back the space before we call */
alloc_in_rts:
    CALL_IO_LOCAL(alloc_store, POLY_SYS_alloc_store)

    RegMask(alloc_store,(M_R3|M_RR|M_R4|M_R23))

/***************************************************************************/
/* Functions implemented in assembly code                                  */
/***************************************************************************/
INLINE_ROUTINE(not_bool)
    xori    r3,r3,shiftup(1,TAGSHIFT)
    blr
    RegMask(not_bool,(M_R3|M_R23))

globldec(or_bool)
INLINE_ROUTINE(or_word)
    or  r3,r3,r4
    blr
    RegMask(or_bool,(M_R3|M_R23))
    RegMask(or_word,(M_R3|M_R23))

globldec(and_bool)
INLINE_ROUTINE(and_word)
    and r3,r3,r4
    blr
    RegMask(and_bool,(M_R3|M_R23))
    RegMask(and_word,(M_R3|M_R23))

INLINE_ROUTINE(xor_word)
    xor rtemp1,r3,r4    /* tag bits will be equal */
    ori r3,rtemp1,1 /* restore tag bit */
    blr
    RegMask(xor_word,(M_R3|M_R23))

INLINE_ROUTINE(shift_left_word)
/* Assume that both args are tagged integers */
    srawi   rtemp2,r4,TAGSHIFT  /* untag amount to shift */
    subi    rtemp1,r3,1     /* untag shiftee (offset by TAGSHIFT) */
    cmplwi  rtemp2,32-TAGSHIFT  /* shift too large? */
    slw rtemp1,rtemp1,rtemp2
    ori r3,rtemp1,1     /* restore tagbit */
    bltlrP              /* return if shift amount is OK */
    li  r3,TAGGED(0)
    blr
    RegMask(shift_left_word,(M_R3|M_R23))
     
INLINE_ROUTINE(shift_right_word)
/* Assume that both args are tagged integers */
    srawi   rtemp2,r4,TAGSHIFT  /* untag amount to shift */
    srw rtemp1,r3,rtemp2
    cmplwi  rtemp2,(32-TAGSHIFT)    /* shift too large? */
    rlwinm  rtemp1,rtemp1,0,0,(31-TAGSHIFT) /* remove stray bits from tag */
    ori r3,rtemp1,1     /* restore tagbit */
    bltlrP              /* return if shift amount is OK */
    li  r3,TAGGED(0)
    blr
    RegMask(shift_right_word,(M_R3|M_R23))

INLINE_ROUTINE(shift_right_arith_word)
/* Assume that both args are tagged integers */
/* Shift right by the appropriate number of bits, preserving the sign.
   If the shift is too large return either 0 or -1. */
    srawi   rtemp2,r4,TAGSHIFT  /* untag amount to shift */
    cmplwi  rtemp2,(32-TAGSHIFT)    /* shift too large? */
    bltP    sraw1
    li  rtemp2,31       /* We just want the sign bit. */
sraw1:
    sraw    rtemp1,r3,rtemp2
    rlwinm  rtemp1,rtemp1,0,0,(31-TAGSHIFT) /* remove stray bits from tag */
    ori r3,rtemp1,1     /* restore tagbit */
    blr
    RegMask(shift_right_arith_word,(M_R3|M_R23))
     
/***************************************************************************/
/* Arithmetic tests on short integers.                                     */
/***************************************************************************/

#define TEST(name, cond) \
INLINE_ROUTINE(name) \
    cmpw    r3,r4; /* These are UNsigned comparisons */ \
    li  r3,TRUE; \
    b##cond##lr; /* Return TRUE if condition holds */ \
    li  r3,FALSE;  \
    blr; \
    RegMask(name,(M_R3|M_R23))

    TEST(int_eq,eq)    /* Is this right? */
    TEST(int_neq,ne)   /* Is this right? */

/* These are the same as int_eq/neq.  These were previously distinct
   because pointer equality required special code in the old persistent
   store system.  That is no longer relevant. */
    TEST(word_eq,  eq)
    TEST(word_neq, ne)

/***************************************************************************/
/* Miscellaneous functions                                                */
/***************************************************************************/

/* This is needed in the code generator, but is a very risky thing to do. */
INLINE_ROUTINE(offset_address)
    srawi   rtemp2,r4,TAGSHIFT /* untag offset */
    add r3,r3,rtemp2
    blr
    RegMask(offset_address,(M_R3|M_R23))

/* Clears the "mutable" bit on a segment */
/* Should this return unit, or its original parameter? */
INLINE_ROUTINE(locksega)
    lbz     rtemp1,-4(r3)
    andi.   rtemp2,rtemp1,(255-B_mutable) /* Reset MUTABLE bit */
    stb     rtemp2,-4(r3)
    blr
    RegMask(lockseg,(M_R3|M_R23))


INLINE_ROUTINE(get_length_a)
    lwz rtemp1,-4(r3)       /* get length word */
        /* get 24 bits of length, and shift up by TAGSHIFT */
    rlwinm  rtemp1,rtemp1,TAGSHIFT,(TYPESHIFT-TAGSHIFT),(31-TAGSHIFT)
    ori r3,rtemp1,1     /* return result as a tagged integer */
    blr
    RegMask(get_length,(M_R3|M_R23))

/***************************************************************************/
/* test_string - basic string comparison utility function                  */
/***************************************************************************/

/* Compare two strings; returns with condition codes set appropriately. */
/* Corrupts r3,  r4, r0, rtemp1, rtemp2, rtemp3. */
test_string:
    /* Is arg1 a single character? */
    andi.   r0,r3,1
    beq test_string2
    
    /* arg1 is a single character - is arg2? */
    andi.   r0,r4,1
    beq test_string1
    
    /* Both are single characters - just compare them */
    cmpw    r3,r4
    blr
    
test_string1:
    /* arg1 is a single character, but arg2 isn't. */
    /* Is arg2 a null string? - return "GT" if 1 > length(arg2). */ 
    lwz rtemp2,0(r4)
    li  rtemp1,1
    cmpw    rtemp1,rtemp2
    bgtlr
    
    /* Compare arg1 with the first byte of arg2. 
       If the bytes differ, that's the result we want. */
    lbz rtemp2,4(r4)
    srwi    rtemp1,r3,TAGSHIFT
    cmpw    rtemp1,rtemp2
    bnelr
    
    /* If the bytes are equal, arg1 < arg2, so set CR accordingly. */
    li  rtemp1,-1
    cmpwi   rtemp1,0
    blr
    
test_string2:
    /* arg1 is not a single character - is arg2? */
    andi.   r0,r4,1
    beq test_string3
    
    /* arg1 is not a single character, but arg2 is.
       Is arg1 a null string? - return "LT" if length(arg1) < 1. */
    lwz rtemp1,0(r3)
    cmpwi   rtemp1,1
    bltlr

    /* Compare first byte of arg1 with arg2. 
       If the bytes differ, that's the result we want. */
    lbz rtemp1,4(r3)
    srwi    rtemp2,r4,TAGSHIFT
    cmpw    rtemp1,rtemp2
    bnelr
    
    /* If the bytes are equal, arg1 > arg2, so set CR accordingly. */
    li  rtemp1,1
    cmpwi   rtemp1,0
    blr


/* A is greater than B if, at the first position at which A and B differ,
   A[i] > B[i] or if the end of B is found before they differ.
   Set rtemp1 to the shorter length and rtemp3 to length(A) - length(B)
*/
test_string3:
    /* Neither string is a single character */
    lwz rtemp1,0(r3)
    lwz rtemp2,0(r4)
    
    subfc.  rtemp3,rtemp2,rtemp1 /* rt3 = length(A) - length(B) */
    ble test_string4         /* done if length(A) <= length(B) */
    subfc   rtemp1,rtemp3,rtemp1 /* otherwise shorten rtemp1 */
    
test_string4:   
    /* round-up byte count to word-count */
    addi    rtemp1,rtemp1,3
    srwi    rtemp1,rtemp1,2

    /* have to treat length 0 as special case (it's a "repeat" loop) */
    cmpwi   rtemp1,0
    beq test_string6
    mtctr   rtemp1

    /* Since we have a big-endian machine, we can do the
       comparison a whole word at a time. This assumes
       that strings are zero-padded (is this true?). */
test_string5:   
    lwzu    rtemp1,4(r3)
    lwzu    rtemp2,4(r4)
    cmplw   rtemp1,rtemp2   /* UNSIGNED comparison needed */
/* DCJM: Formerly bdneq. */
    bdnzt   eq,test_string5 /* loop while counter > 0 and words are equal */
test_string6:
    /* Here we have 2 possibilities for the loop exit:
        (1) rtemp1 <> rtemp2
        (2) rtemp1 = rtemp2, and we've examined all of the common prefix
    */

    li  r4,TAGGED(0)        /* zap r4 */
    li  r3,TAGGED(0)        /* zap r3 */

    /* case (1) - rtemp1 <> rtemp2 
       Just return - the condition code is already set appropriately. */
    bnelr
    
    /* case (2) - the common prefix is equal. in this case,
       the result depends on which string is the longer.
       The result of the comparison is, in fact, the same
       as the comparison between the lengths, which is the same
       as comparing (length(A) - length(B)) to 0.  */
    cmpwi   rtemp3,0 
    blr
    
/***************************************************************************/
/* String comparison functions                                            */
/***************************************************************************/

#define STRINGTEST(name, cond) \
INLINE_ROUTINE(name) \
    mflr    rr; \
    ori rr,rr,RETURNOFFSET; \
    bl  test_string; \
    cror    31,31,31; /* needed? */\
    mtlr    rr; \
    li  r3,TRUE; \
    b##cond##lr; \
    li  r3,FALSE; \
    blr; \
    RegMask(name,(M_R3|M_R4|M_R23|M_RR))


STRINGTEST(teststrgeq,ge)
STRINGTEST(teststrleq,le)
STRINGTEST(teststrlss,lt)
STRINGTEST(teststrgtr,gt)
STRINGTEST(teststrneq,ne)
STRINGTEST(teststreq,eq)

INLINE_ROUTINE(str_compare)
    mflr    rr
    ori     rr,rr,RETURNOFFSET
    bl      test_string
    cror    31,31,31 /* needed? */
    mtlr    rr
    li      r3,TAGGED(1)
    bgtlr
    li      r3,TAGGED(0)
    beqlr
    li      r3,TAGGED(-1)
    blr
    RegMask(str_compare, (M_R3|M_R4|M_R23|M_RR))


/***************************************************************************/
/* Exception handling                                                      */
/***************************************************************************/


/* Loop to find the handler for this exception. Handlers consist of one or more
   pairs of identifier and code address, followed by the address of the next
   handler.
*/
INLINE_ROUTINE(raisex)
    lwz     rtemp3,56(r13)      /* rt3 = end_of_stack */
    lwz     r0,0(r3)            /* r0  = exception id  */
    mr      rtemp1,rhr          /* rt1 = handler ptr   */
    lwz     rtemp2,0(rhr)       /* rt2 = handler id    */

rsx1:
    cmplwi  rtemp2,TAGGED(0)    /* Is it zero (or TAGGED(0))? */
    ble     rsx7                /* If so, we have a default handler */
    /* non-default handler */
    cmpw    rtemp2,r0           /* Does it match the exception id? */
    beq     rsx7
    /* This handler doesn't match - try the next one.
       This can be either a genuine handler pair, or a
       pointer up the stack. */
    lwzu    rtemp2,8(rtemp1)    /* rt2 = next handler id */
    cmplw   rtemp2,rtemp1
    blt rsx1            /* Not a stack pointer (too small) */
    cmplw   rtemp2,rtemp3
    bge rsx1            /* Not a stack pointer (too big)  */
    
    /* It's a stack pointer - get the next batch of handlers */
    mr  rtemp1,rtemp2       /* rt1 = new handler ptr */
    lwz rtemp2,0(rtemp2)    /* rt2 = next handler id */
    b   rsx1
    
rsx7:
    /* We've found a handler that matches; rtemp1 points at the id */
    lwz rr,4(rtemp1)        /* Get the handler entry point */

rsx6:
    /* Remove the other handlers in this group. */
    lwzu    rtemp2,8(rtemp1)    /* Get next handler id */
    cmplw   rtemp2,rtemp1
    blt rsx6            /* Not a stack pointer (too small) */
    cmplw   rtemp2,rtemp3
    bge rsx6            /* Not a stack pointer (too big)  */
    /* rtemp1 now points at the pointer to the next group of handlers
       i.e. the old (saved) value of the handler register
       and rtemp2 contains the pointer itself */

    /* Is this handler a real one, or was it set by exception_trace? */
    cmplwi  rr,TAGGED(0)
    bgtP    rsx9
    
    /* We've found a handler set by exception_trace.
        Push the return address onto the stack, so that
        it will be printed by ex_tracec, load a dummy value
        into rr, then call ex_tracec (which doesn't return).
    */
    mflr    rr      /* get return address */
    mr  r4,r3       /* exception packet is arg2 */
    ori rr,rr,2     /* tag return address */
    mr  r3,rtemp1   /* stack-mark is arg1 */
    stwu    rr,-4(rsp)  /* save return address */
    li  rr,TAGGED(1)    /* a special marker */
    CALL_IO_LOCAL(ex_trace, POLY_SYS_give_ex_trace)
rsx9:   
    /* Ordinary exception handler */
    mtlr    rr      /* "Return" to handler */
    mr  rhr,rtemp2  /* Reload rhr from saved value */
    addi    rsp,rtemp1,4    /* Pop stack back past saved rhr */
    blr


/***************************************************************************/
/* Arbitrary-precision arithmetic                                          */
/***************************************************************************/

/* Problem: what happens if one of these instructions traps,
  and the emulation code causes a garbage-collection? We'll end
  up with the PC pointing into something that's not a code
  segment. We avoid this by using explicit tests and calls
  to the emulation code. */

INLINE_ROUTINE(neg_long)
    andi.   r0,r3,1
    li  rtemp1,TAGGED(0)
    beqM    neg_long1   /* emulate if argument is long */
    
    li      r0,0
    mtxer   r0       /* reset XER overflow state */

    subfco. rtemp2,r3,rtemp1
    bsoM    neg_long1   /* emulate if result overflows */
        
    addi    r3,rtemp2,1 /* restore tag bit */
    blr
neg_long1:
    CALL_IO_LOCAL(neg_long, POLY_SYS_aneg)

    RegMask (aneg,(M_R3|M_R23|Mask_neg_long))
    
/***************************************************************************/
INLINE_ROUTINE(add_long)
    and r0,r3,r4
    andi.   r0,r0,1
    beqM    add_long1   /* emulate if either argument is long */

    li      r0,0
    mtxer   r0       /* reset XER overflow state */
 
    addo.   rtemp2,r3,r4
    bsoM    add_long1   /* emulate if result overflows */
    subi    r3,rtemp2,1 /* restore tag bit */
    blr
add_long1:
    CALL_IO_LOCAL(add_long, POLY_SYS_aplus)

    RegMask (aplus,(M_R3|M_R23|Mask_add_long))
    
/***************************************************************************/
INLINE_ROUTINE(sub_long)
    and r0,r3,r4
    andi.   r0,r0,1
    beqM    sub_long1   /* emulate if either argument is long */

    li      r0,0
    mtxer   r0       /* reset XER overflow state */

    subfco. rtemp2,r4,r3
    bsoM    sub_long1   /* emulate if result overflows */
    addi    r3,rtemp2,1 /* restore tag bit */
    blr
sub_long1:
    CALL_IO_LOCAL(sub_long, POLY_SYS_aminus)

    RegMask (aminus,(M_R3|M_R23|Mask_sub_long))

/***************************************************************************/
INLINE_ROUTINE(mult_long)
    and r0,r3,r4
    mr  rtemp1,r3        /* save r3 */
    andi.   r0,r0,1
    mr  rtemp2,r4        /* save r4 */
    beq mult_really_long
  
    srawi   r3,r3,TAGSHIFT  /* Untag one argument. */
    subi    r4,r4,TAGGED(0) /* Remove tag, but don't shift */
    /* This assumes that the machine supports the multiply instr. */
    mullwo. r3,r3,r4
    bsoM    mult_really_long2
    li  r4,TAGGED(0)
    ori r3,r3,1

    blr

mult_really_long2:
    /* restore r3 and r4, then call the C routine */
    mr  r3,rtemp1
    mr  r4,rtemp2

mult_really_long:
    /* call the (slow) C multiplication routine */
    CALL_IO_LOCAL(mult_long, POLY_SYS_amul)

    RegMask (amul,(M_R3|M_R4|M_R23|Mask_mult_long))
    
/***************************************************************************/
INLINE_ROUTINE(div_long)
    /* check for long arguments */
    and     r0,r3,r4
    andi.   r0,r0,1
    beqM    div_really_long

    /* Check for division by zero */
    cmpwi   r4,TAGGED(0)
    beqM    div_by_zero
    
    /* check for division of MININT by -1 */
    cmpwi   r4,TAGGED(-1)
    bneP    div_long1
    rlwinm  r0,r3,1,0,31
    /* If we rotate MININT 1 place left, this is what we get. */
    cmpwi   r0,(shiftup(1,1) | 1) /* tag bit plus rotated sign bit */
    beq div_really_long

div_long1:
         
    /* untag the values into r3 and r4 */
    srawi   r3,r3,TAGSHIFT
    srawi   r4,r4,TAGSHIFT

    divw    r3,r3,r4
        /* r3, r4 now contain the 32 bit quotient and remainder respectively */
    slwi    r3,r3,TAGSHIFT  /* result is the quotient */
    li  r4,TAGGED(0)    /* zap r4 */
    ori r3,r3,1     /* tag result */

    blr

div_really_long:
    CALL_IO_LOCAL(div_long, POLY_SYS_adiv)
    
    RegMask (adiv,(M_R3|M_R4|M_R23|Mask_div_long))

div_by_zero:
    lwz     r0,48(r13)    /* Jump to the Raise_div entry point */
    mtctr   r0
    bctr

    
/***************************************************************************/
INLINE_ROUTINE(rem_long)
    /* check for long arguments */
    and r0,r3,r4
    andi.   r0,r0,1
    
rem_really_long:
    CALL_IO_LOCAL(rem_long, POLY_SYS_amod)

    RegMask (amod,(M_R3|M_R4|M_R23|Mask_rem_long))

/***************************************************************************/
/* Arithmetic tests on arbitrary-precision integers.                       */
/***************************************************************************/

/* new version - doesn't need a trap (which can be expensive) */
/*
   For an equality test, we can use short test if either argument is short.
   For other tests, we need both arguments to be short, which explains
   why the "combine" parameter is "or" for equality tests and "and" for
   other tests. SPF 1/11/95.
*/ 
#define ARBTEST(name, name2, maskname, cond, combine, ioCall) \
INLINE_ROUTINE(name) \
    combine rtemp1,r3,r4; \
    andi.   rtemp1,rtemp1,1; \
    beqM    name##_really_long; /* at least one argument is long */ \
    cmpw    r3,r4; \
    li  r3,TRUE; \
    b##cond##lr; /* Return TRUE if condition holds */ \
    li  r3,FALSE; \
    blr;         /* Return FALSE otherwise */ \
name##_really_long: \
    CALL_IO_LOCAL(name2, ioCall) ; \
    RegMask(maskname,(M_R3|M_R23|Mask_##name2))


    ARBTEST(equal_long, equal_long, equala,  eq, or, POLY_SYS_equala)
    ARBTEST(int_geq,    ge_long,    int_geq, ge, and, POLY_SYS_int_geq)
    ARBTEST(int_leq,    le_long,    int_leq, le, and, POLY_SYS_int_leq)
    ARBTEST(int_gtr,    gt_long,    int_gtr, gt, and, POLY_SYS_int_gtr)
    ARBTEST(int_lss,    ls_long,    int_lss, lt, and, POLY_SYS_int_lss)

INLINE_ROUTINE(or_long)
    and r0,r3,r4
    andi.   r0,r0,1
    beqM    or_really_long  /* emulate if either argument is long */

    or  r3,r3,r4
    blr

or_really_long:
    CALL_IO(or_long, POLY_SYS_ora)

    RegMask (ora,(M_R3|M_R23|Mask_or_long))

INLINE_ROUTINE(and_long)
    and r0,r3,r4
    andi.   r0,r0,1
    beqM    and_really_long /* emulate if either argument is long */

    and r3,r3,r4
    blr

and_really_long:
    CALL_IO(and_long, POLY_SYS_anda)

    RegMask (anda,(M_R3|M_R23|Mask_and_long))


INLINE_ROUTINE(xor_long)
    and r0,r3,r4
    andi.   r0,r0,1
    beqM    xor_really_long /* emulate if either argument is long */

    xor rtemp1,r3,r4    /* tag bits will be equal */
    ori r3,rtemp1,1 /* restore tag bit */
    blr

xor_really_long:
    CALL_IO(xor_long, POLY_SYS_xora)

    RegMask (xora,(M_R3|M_R23|Mask_xor_long))

/***************************************************************************/
/* Loads and Stores                                                        */
/***************************************************************************/

INLINE_ROUTINE(load_byte)
    srawi   rtemp1,r4,TAGSHIFT  /* untag to byte offset */
    lbzx    rtemp1,r3,rtemp1    /* fetch byte */
    slwi    rtemp1,rtemp1,TAGSHIFT
    addi    r3,rtemp1,1 /* return result as a tagged integer */
    blr
    RegMask (load_byte,(M_R3|M_R23))

INLINE_ROUTINE(load_word)
    rlwinm  rtemp1,r4,2-TAGSHIFT,0,29 /* untag to word offset, masking off shifted tag */
    lwzx    r3,r3,rtemp1    /* fetch (tagged) word */
    blr
    RegMask (load_word,(M_R3|M_R23))

INLINE_ROUTINE(assign_byte)
    srawi   rtemp1,r4,TAGSHIFT      /* rt1 = untagged byte-offset */
    
    srawi   rtemp2,r5,TAGSHIFT      /* rt2 = untagged byte */
    
    stbx    rtemp2,r3,rtemp1
    li  r3,UNIT             /* result is always "()" */
    blr
    
    RegMask (assign_byte,(M_R3|M_R23))

INLINE_ROUTINE(assign_word)
    rlwinm  rtemp1,r4,2-TAGSHIFT,0,29   /* rt1 = untagged word-offset */
    
    stwx    r5,r3,rtemp1
    li  r3,UNIT             /* result is always "()" */
    blr

    RegMask (assign_word,(M_R3|M_R23))

/***************************************************************************/
/* Miscellaneous                                                           */
/***************************************************************************/

INLINE_ROUTINE(is_shorta)
        /* Move tag bit into LS digit position */
    rlwinm  rtemp1,r3,TAGSHIFT,(31-TAGSHIFT),(31-TAGSHIFT)
    ori r3,rtemp1,1 /* return result as a tagged integer */
    blr
    RegMask (is_short,(M_R3|M_R23))

/* Single character strings are represented as shorts */
INLINE_ROUTINE(string_length)
    andi.   rtemp1,r3,1 /* Is it a short? (Set CR0) */
    beq sl1

/* a single character */
    li  r3,TAGGED(1)
    blr

/* not a single character */
sl1:    
    lwz rtemp1,0(r3)        /* Get string length (in bytes) */
    slwi    rtemp1,rtemp1,TAGSHIFT  /* Return tagged length */
    addi    r3,rtemp1,1
    blr

    RegMask (string_length,(M_R3|M_R23))

/* Store the length of a string in the first word. */
INLINE_ROUTINE(set_string_length_a)
    srwi    rtemp1,r4,TAGSHIFT  /* Untag the length */
    stw rtemp1,0(r3)
    li  r3,UNIT             /* Return unit */
    blr
    RegMask (set_string_length,(M_R3|M_R23))


INLINE_ROUTINE(is_big_endian)
    li  r3,TRUE
    blr
    RegMask (is_big_endian,(M_R3|M_R23))

INLINE_ROUTINE(bytes_per_word)
    li  r3,TAGGED(4)
    blr
    RegMask (bytes_per_word,(M_R3|M_R23))

INLINE_ROUTINE(move_bytes)
    srawi   rtemp1,r4,TAGSHIFT      /* rt1 = untagged source offset */
    srawi   rtemp2,r6,TAGSHIFT      /* rt2 = untagged dest offset */
    lwz     rtemp3,0(rsp)           /* rt3 = number of bytes to move. */
    add     rtemp1,r3,rtemp1        /* Source address. */
    add     rtemp2,r5,rtemp2        /* Destination address. */
    srawi.  rtemp3,rtemp3,TAGSHIFT
    mtctr   rtemp3
    beq     MB2
    cmplw   rtemp1,rtemp2           /* If the source < destination use decrementing move. */
    blt     MB4
    addi    rtemp1,rtemp1,-1        /* else use incrementing move. */
    addi    rtemp2,rtemp2,-1
MB1:
    lbzu    rtemp3,1(rtemp1)
    stbu    rtemp3,1(rtemp2)
    bdnz    MB1
    
MB2:    li  r3,TAGGED(0)
    addi    rsp,rsp,4           /* Pop last argument. */
    blr

MB4:
    add     rtemp1,rtemp3,rtemp1
    add     rtemp2,rtemp3,rtemp2
MB5:
    lbzu    rtemp3,-1(rtemp1)
    stbu    rtemp3,-1(rtemp2)
    bdnz    MB5

    li      r3,TAGGED(0)
    addi    rsp,rsp,4           /* Pop last argument. */
    blr
    RegMask (move_bytes,(M_R3|M_R23))


INLINE_ROUTINE(move_words)
    rlwinm  rtemp1,r4,2-TAGSHIFT,0,29 /* rt1 = untag to word offset, masking off shifted tag */
    rlwinm  rtemp2,r6,2-TAGSHIFT,0,29 /* rt1 = untag to word offset, masking off shifted tag */
    lwz     rtemp3,0(rsp)           /* rt3 = number of words to move. */
    add     rtemp1,r3,rtemp1        /* Source address. */
    add     rtemp2,r5,rtemp2        /* Destination address. */
    srawi.  rtemp3,rtemp3,TAGSHIFT
    mtctr   rtemp3
    beq     MW2
    cmplw   rtemp1,rtemp2           /* If the source < destination use decrementing move. */
    blt     MW4
    addi    rtemp1,rtemp1,-4        /* else use incrementing move. */
    addi    rtemp2,rtemp2,-4
MW1:
    lwzu    rtemp3,4(rtemp1)
    stwu    rtemp3,4(rtemp2)
    bdnz    MW1
    
MW2:    li  r3,TAGGED(0)
    addi    rsp,rsp,4           /* Pop last argument. */
    blr

MW4:
    slwi    rtemp3,rtemp3,2
    add     rtemp1,rtemp3,rtemp1
    add     rtemp2,rtemp3,rtemp2
MW5:
    lwzu    rtemp3,-4(rtemp1)
    stwu    rtemp3,-4(rtemp2)
    bdnz    MW5

    li      r3,TAGGED(0)
    addi    rsp,rsp,4           /* Pop last argument. */
    blr
    RegMask (move_words,(M_R3|M_R23))

/* Word functions.  These are all unsigned and do not raise Overflow */

INLINE_ROUTINE(mul_word)
    srawi   r3,r3,TAGSHIFT      /* Untag one argument. */
    subi    rtemp1,r4,1         /* Remove tag but don't shift. */
    mullw   r3,r3,rtemp1
    addi    r3,r3,1             /* Add back the tag. */
    blr
    RegMask (mul_word,M_R3|M_R23)

INLINE_ROUTINE(plus_word)
    subi    rtemp2,r3,1         /* Remove a tag */
    add     r3,rtemp2,r4        /* Add the values */
    blr
    RegMask (plus_word,(M_R3|M_R23))

INLINE_ROUTINE(minus_word)
    sub     rtemp2,r3,r4        /* Do the subtraction. */
    addi    r3,rtemp2,1         /* restore tag bit */
    blr
    RegMask (minus_word,(M_R3|M_R23))

INLINE_ROUTINE(div_word)
    cmpwi   r4,TAGGED(0)
    beqM    div_by_zero
    subi    r3,r3,1             /* Subtract tag from args. */
    subi    rtemp1,r4,1
    divwu   r3,r3,rtemp1
    slwi    r3,r3,TAGSHIFT      /* Tag the result. */
    addi    r3,r3,1
    blr
    RegMask (div_word,M_R3|M_R23)

INLINE_ROUTINE(mod_word)
    cmpwi   r4,TAGGED(0)
    beqM    div_by_zero
    subi    rtemp1,r3,TAGSHIFT  /* Untag arguments. */
    subi    rtemp2,r4,TAGSHIFT
    /* We don't get the remainder directly so we have to do this. */
    divwu   rtemp1,rtemp1,rtemp2
    mullw   rtemp1,rtemp1,rtemp2
    sub r3,r3,rtemp1
    blr
    RegMask (mod_word,M_R3|M_R23)

/* Unsigned tests on words. */
    TEST(word_geq,ge)
    TEST(word_leq,le)
    TEST(word_gtr,gt)
    TEST(word_lss,lt)

INLINE_ROUTINE(int_to_word)
    andi.   r0,r3,1
    bnelr           /* Return it if it's short. */
    /* Else drop through. */

	CALL_IO_LOCAL(get_first_long_word_a, POLY_SYS_get_first_long_word)    
    
INLINE_ROUTINE(atomic_incr)
ati1:
    lwarx  r4,0,r3        /* Load value at 0(r3) with reservation. */
    addi   r4,r4,2        /* 2 is TAGGED(1)-TAG */
    stwcx. r4,0,r3        /* Store the updated value unless someone else did. */
    bne-   ati1
    mr     r3,r4
    blr
    RegMask(atomic_incr,M_R3|M_R4|M_R23)
    
INLINE_ROUTINE(atomic_decr)
atd1:
    lwarx  r4,0,r3
    subi   r4,r4,2        /* 2 is TAGGED(1)-TAG */
    stwcx. r4,0,r3
    bne-   atd1
    mr     r3,r4
    blr
    RegMask(atomic_decr,M_R3|M_R4|M_R23)

INLINE_ROUTINE(thread_self)
    lwz    r3,60(r13)  /* Load the the thread id. */
    blr
    RegMask(thread_self,M_R3|M_R23)

globldec(MD_flush_instruction_cache)
/*
This function is needed because the instruction cache on the
PowerPC does not see changes in the data cache.  When code
segments are written it is necessary to flush the data from
the data cache and also invalidate the instruction cache just
in case the location we have written to happened to previously
contain code as was in the instruction cache.
*/
#define CACHE_LINE_SIZE 32
/* TODO: We only need to flush the appropriate cache lines. 
   i.e. We don't need to call dcbf for each byte. */
    mtctr   r4
mfic1:  dcbf    0,r3 /* Flush data - i.e. make sure memory is up to date. */
    sync            /* Make sure they really have been flushed BEFORE we call icbi. */
    icbi    0,r3    /* Flush instructions - make sure we reload. */
    addi    r3,r3,1
    bdnz    mfic1
    sync; isync     /* This MAY help. */
    blr


/* Register mask vector. - extern int registerMaskVector[];
   Each entry in this vector is a set of the registers modified
   by the function.  It is an untagged bitmap with the registers
   encoded in the same way as in the code generator.
   Unused entries are set to Mask_all for safety in case a new
   entry is added to the iovector without also adding an entry
   here. 
   Entries that call into the RTS also use Mask_all now.  That's
   not essential - in many cases registers are actually preserved -
   but certain RTS calls (e.g. callcode and exception_trace) have
   the effect of modifying any register.
*/
#define dd  .long
globldec(registerMaskVector)
    dd  Mask_all                /* 0 is unused */
    dd  Mask_all             /* 1 */
    dd  Mask_all       /* 2 */
    dd  Mask_all                /* 3 is unused */
    dd  Mask_all                /* 4 is unused */
    dd  Mask_all                /* 5 is unused */
    dd  Mask_all          /* 6 */
    dd  Mask_all                /* 7 is unused */
    dd  Mask_all                /* 8 is unused */
    dd  Mask_all         /* 9 */
    dd  Mask_all                /* 10 is unused */
    dd  Mask_alloc_store         /* 11 */
    dd  Mask_alloc_uninit        /* 12 */
    dd  Mask_all                 /* return = 13 */
    dd  Mask_all                 /* raisex = 14 */
    dd  Mask_get_length          /* 15 */
    dd  Mask_all                /* 16 is unused */
    dd  Mask_all         /* 17 */
    dd  Mask_all                /* 18 - now unused */
    dd  Mask_all                /* 19 - now unused */
    dd  Mask_all                /* 20 - now unused */
    dd  Mask_all                /* 21 is unused */
    dd  Mask_all                /* 22 is unused */
    dd  Mask_all                /* 23 is unused */
    dd  Mask_teststreq           /* 24 */
    dd  Mask_teststrneq          /* 25 */
    dd  Mask_teststrgtr          /* 26 */
    dd  Mask_teststrlss          /* 27 */
    dd  Mask_teststrgeq          /* 28 */
    dd  Mask_teststrleq          /* 29 */
    dd  Mask_all     /* 30 */
    dd  Mask_all                 /* 31 - now unused */
    dd  Mask_all                 /* 32 - now unused */
    dd  Mask_all                 /* 33 - now unused */
    dd  Mask_all                 /* 34 - now unused */
    dd  Mask_all                 /* 35 - now unused */
    dd  Mask_all                 /* 36 */
    dd  Mask_all                /* 37 is unused */
    dd  Mask_all                /* 38 is unused */
    dd  Mask_all                /* 39 is unused */
    dd  Mask_all              /* 40 */
    dd  Mask_all                /* 41 is unused */
    dd  Mask_all        /* 42 */
    dd  Mask_all         /* 43 */
    dd  Mask_all                 /* 44 - now unused */
    dd  Mask_all                 /* 45 - now unused */
    dd  Mask_all             /* 46 */
    dd  Mask_lockseg             /* 47 */
    dd  Mask_all                 /* nullorzero = 48 */
    dd  Mask_all                 /* 49 - now unused */
    dd  Mask_all                 /* 50 - now unused */
    dd  Mask_all       /* 51 */
    dd  Mask_all   /* 52 */
    dd  Mask_all                /* 53 is unused */
    dd  Mask_all                /* 54 is unused */
    dd  Mask_all                /* version_number = 55 */
    dd  Mask_all                /* 56 is unused */
    dd  Mask_all                /* 57 is unused */
    dd  Mask_all                /* 58 is unused */
    dd  Mask_all                /* 59 is unused */
    dd  Mask_all                /* 60 is unused */
    dd  Mask_all                /* 61 */
    dd  Mask_all                /* 62 */
    dd  Mask_all                /* 63 is unused */
    dd  Mask_all                /* 64 is unused */
    dd  Mask_all                /* 65 is unused */
    dd  Mask_all                /* 66 is unused */
    dd  Mask_all                /* 67 is unused */
    dd  Mask_all                /* 68 is unused */
    dd  Mask_all                /* 69 is unused */
    dd  Mask_atomic_incr        /* 70 */
    dd  Mask_atomic_decr        /* 71 */
    dd  Mask_thread_self        /* 72 */
    dd  Mask_all                /* 73 */
    dd  Mask_all                /* 74 is unused */
    dd  Mask_all                /* 75 is unused */
    dd  Mask_all                /* 76 is unused */
    dd  Mask_all                /* 77 is unused */
    dd  Mask_all                /* 78 is unused */
    dd  Mask_all                /* 79 is unused */
    dd  Mask_all                /* Mask_version_number_1 = 80 */
    dd  Mask_all                /* 81 - now unused */
    dd  Mask_all                /* 82 */
    dd  Mask_all                /* 83 */
    dd  Mask_all                /* 84 */
    dd  Mask_all                /* 85 */
    dd  Mask_all                /* 86 */
    dd  Mask_all                /* 87 */
    dd  Mask_all                /* 88 */
    dd  Mask_all                /* 89 is unused */
    dd  Mask_all                /* 90 is unused */
    dd  Mask_all                /* 91 is unused */
    dd  Mask_all            /* 92 */
    dd  Mask_all        /* 93 */
    dd  Mask_all    /* 94 */
    dd  Mask_all                /* 95 is unused */
    dd  Mask_all                /* 96 is unused */
    dd  Mask_all                /* 97 is unused */
    dd  Mask_all                /* 98 is now unused */
    dd  Mask_all            /* 99 */
    dd  Mask_all           /* 100 */
    dd  Mask_all                /* 101 is unused */
    dd  Mask_all                /* 102 is unused */
    dd  Mask_all /* 103 */
    dd  Mask_all                /* 104 is unused */
    dd  Mask_is_short            /* 105 */
    dd  Mask_aplus               /* 106 */
    dd  Mask_aminus              /* 107 */
    dd  Mask_amul                /* 108 */
    dd  Mask_adiv                /* 109 */
    dd  Mask_amod                /* 110 */
    dd  Mask_aneg                /* 111 */
    dd  Mask_xora                /* 112 */
    dd  Mask_equala              /* 113 */
    dd  Mask_ora                 /* 114 */
    dd  Mask_anda                /* 115 */
    dd  Mask_all                 /* version_number_3 = 116 */
    dd  Mask_all            /* 117 */
    dd  Mask_all            /* 118 */
    dd  Mask_all            /* 119 */
    dd  Mask_all            /* 120 */
    dd  Mask_all            /* 121 */
    dd  Mask_all             /* 122 */
    dd  Mask_all            /* 123 */
    dd  Mask_all       /* 124 */
    dd  Mask_all            /* 125 */
    dd  Mask_all            /* 126 */
    dd  Mask_all            /* 127 */
    dd  Mask_all            /* 128 */
    dd  Mask_all                 /* 129 is unused */
    dd  Mask_all            /* 130 */
    dd  Mask_all                 /* 131 is unused */
    dd  Mask_all           /* 132 */
    dd  Mask_all           /* 133 */
    dd  Mask_all            /* 134 */
    dd  Mask_all          /* 135 */
    dd  Mask_all           /* 136 */
    dd  Mask_all            /* 137 */
    dd  Mask_all            /* 138 */
    dd  Mask_all         /* 139 */
    dd  Mask_all            /* 140 */
    dd  Mask_all             /* 141 */
    dd  Mask_all                 /* 142 - now unused */
    dd  Mask_all                 /* 143 is unused */
    dd  Mask_all                 /* 144 is unused */
    dd  Mask_all                 /* 145 is unused */
    dd  Mask_all                 /* 146 is unused */
    dd  Mask_all                 /* 147 is unused */
    dd  Mask_all                 /* stdin = 148 */
    dd  Mask_all                 /* stdout= 149 */
    dd  Mask_all   /* 150 */
    dd  Mask_set_string_length   /* 151 */
    dd  Mask_all                 /* 152 */
    dd  Mask_all                 /* 153 is unused */
    dd  Mask_all                 /* 154 is unused */
    dd  Mask_all                 /* 155 is unused */
    dd  Mask_all                 /* 156 is unused */
    dd  Mask_all                 /* 157 is unused */
    dd  Mask_all                 /* 158 is unused */
    dd  Mask_all                 /* 159 is unused */
    dd  Mask_all                 /* 160 is unused */
    dd  Mask_all                 /* 161 is unused */
    dd  Mask_all                 /* 162 is unused */
    dd  Mask_all                 /* 163 is unused */
    dd  Mask_all                 /* 164 is unused */
    dd  Mask_all                 /* 165 is unused */
    dd  Mask_all                 /* 166 is unused */
    dd  Mask_all                 /* 167 is unused */
    dd  Mask_all                 /* 168 is unused */
    dd  Mask_all                 /* 169 is unused */
    dd  Mask_all                 /* 170 is unused */
    dd  Mask_all                 /* 171 is unused */
    dd  Mask_all                 /* 172 is unused */
    dd  Mask_all                 /* 173 is unused */
    dd  Mask_all                 /* 174 is unused */
    dd  Mask_all                 /* 175 is unused */
    dd  Mask_all                 /* 176 is unused */
    dd  Mask_all                 /* 177 is unused */
    dd  Mask_all                 /* 178 is unused */
    dd  Mask_all                 /* 179 is unused */
    dd  Mask_all                 /* 180 is unused */
    dd  Mask_all                 /* 181 is unused */
    dd  Mask_all                 /* 182 is unused */
    dd  Mask_all                 /* 183 is unused */
    dd  Mask_all                 /* 184 is unused */
    dd  Mask_all                 /* 185 is unused */
    dd  Mask_all                 /* 186 is unused */
    dd  Mask_all                 /* 187 is unused */
    dd  Mask_all                 /* 188 is unused */
    dd  Mask_all       /* 189 */
    dd  Mask_all                 /* 190 is unused */
    dd  Mask_all                 /* 191 - now unused */
    dd  Mask_all                 /* 192 is unused */
    dd  Mask_all                 /* 193 is unused */
    dd  Mask_all   /* 194 */
    dd  Mask_move_words          /* 195 */
    dd  Mask_shift_right_arith_word  /* 196 */
    dd  Mask_all                 /* 197 */
    dd  Mask_move_bytes          /* 198 */
    dd  Mask_all                 /* 199 now unused */
    dd  Mask_all          /* 200 */
    dd  Mask_all       /* 201 */
    dd  Mask_all                 /* stderr = 202 */
    dd  Mask_all                 /* 203 now unused */
    dd  Mask_all     /* 204 */
    dd  Mask_all   /* 205 */
    dd  Mask_all  /* 206 */
    dd  Mask_all                 /* 207 is unused */
    dd  Mask_all                 /* 208 now unused */
    dd  Mask_all           /* 209 */
    dd  Mask_all                 /* 210 is unused */
    dd  Mask_all                 /* 211 is unused */
    dd  Mask_all                 /* 212 is unused */
    dd  Mask_is_big_endian       /* 213 */
    dd  Mask_bytes_per_word      /* 214 */
    dd  Mask_offset_address      /* 215 */
    dd  Mask_shift_right_word    /* 216 */
    dd  Mask_word_neq            /* 217 */
    dd  Mask_not_bool            /* 218 */
    dd  Mask_all                 /* 219 is unused */
    dd  Mask_all                 /* 220 is unused */
    dd  Mask_all                 /* 221 is unused */
    dd  Mask_all                 /* 222 is unused */
    dd  Mask_string_length       /* 223 */
    dd  Mask_all                 /* 224 is unused */
    dd  Mask_all                 /* 225 is unused */
    dd  Mask_all                 /* 226 is unused */
    dd  Mask_all                 /* 227 is unused */
    dd  Mask_all                 /* 228 is unused */
    dd  Mask_int_eq              /* 229 */
    dd  Mask_int_neq             /* 230 */
    dd  Mask_int_geq             /* 231 */
    dd  Mask_int_leq             /* 232 */
    dd  Mask_int_gtr             /* 233 */
    dd  Mask_int_lss             /* 234 */
    dd  Mask_all          /* 235 */
    dd  Mask_all                 /* 236 is unused */
    dd  Mask_all                 /* 237 is unused */
    dd  Mask_mul_word            /* 238 */
    dd  Mask_plus_word           /* 239 */
    dd  Mask_minus_word          /* 240 */
    dd  Mask_div_word            /* 241 */
    dd  Mask_or_word             /* 242 */
    dd  Mask_and_word            /* 243 */
    dd  Mask_xor_word            /* 244 */
    dd  Mask_shift_left_word     /* 245 */
    dd  Mask_mod_word            /* 246 */
    dd  Mask_word_geq            /* 247 */
    dd  Mask_word_leq            /* 248 */
    dd  Mask_word_gtr            /* 249 */
    dd  Mask_word_lss            /* 250 */
    dd  Mask_word_eq             /* 251 */
    dd  Mask_load_byte           /* 252 */
    dd  Mask_load_word           /* 253 */
    dd  Mask_assign_byte         /* 254 */
    dd  Mask_assign_word         /* 255 */

