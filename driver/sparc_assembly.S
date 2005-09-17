!
!  Title: 	Assembly code routines for the poly system.
!  Author:    David Matthews
!  Copyright (c) Cambridge University Technical Services Limited 2000
!
!	This library is free software; you can redistribute it and/or
!	modify it under the terms of the GNU Lesser General Public
!	License as published by the Free Software Foundation; either
!	version 2.1 of the License, or (at your option) any later version.
!	
!	This library is distributed in the hope that it will be useful,
!	but WITHOUT ANY WARRANTY; without even the implied warranty of
!	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!	Lesser General Public License for more details.
!	
!	You should have received a copy of the GNU Lesser General Public
!	License along with this library; if not, write to the Free Software
!	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
!
! Call machine code from the interpreter.
! 
/*
 Linkage conventions:
We ignore the register window and use only the %o, %l and %g register sets.
The %i set is not used so that returning to C is relatively easy, apart from %i4
and i5 which are used as unchecked work registers, and %i3 which is used in assembly code.
%o0 is used for the first argument to a function, and for the result.
%o1-%o3 are used for the next 3 args, any others being passed on the stack.
%o4 is the address of the code being called
%o5 is the closure pointer or static link pointer.
%o6 is never used (the system assumes it points to an area where regs can be stored).
%o7 is the return address i.e. the address of the jmpl instruction.
Return is done by jmp %o7+6. 2 is always added to %o7 on compiled function entry
so that it is distinguishable from other addresses which point at the
start of objects.

%g3 points to the top exception handler,
%g4 is the stack pointer,
%g5 is the stack limit,
%g6 is the heap pointer,
%g7 is the heap limit.

%g1 and %g2 are available as work regs, as are the %l registers.

%i4 and %i5 are untagged scratch registers which don't need to conatin
tagged values. They should NEVER contain pointers if there is any possibility
of a garbage collection, since the garbage collector doesn't change the
values in these registers.

Other register MUST contain properly tagged values if there any a possibility
of a garbage collection. This includes the case where we execute "tvs 16"
or "tnz 16" to (possibly) raise "Range". Dave's version of the run-time
system often left an untagged result in o0 whenexecuting these instructions,
which is just asking for trouble.
*/

/* 
   I've just found a very subtle bug in this RTS code. If a routine such
   as assign_word is called via the general function call interface
   (rather than as a known RTS function), it will be entered with
   regCode (%o4 on the SPARC) pointing at the RTS machine code in
   this file. If we don't explicitly zap regCode, then it will still
   point at the RTS code when we exit the routine. If we then do
   an ML process switch and the new process attempts to commit to
   the database, the copygc code will detect the illegal (because unportable
   between RTS versions) pointer and produces a core dump. I *think*
   this is the cause of the bug James found. 
   
   This problem doesn't occur with Dave's version of the compiler because
   it always explictly zaps regCode at the start of each code segment,
   before a process switch could possibly occur. My version of the
   compiler tries to optimise away this overhead, which only works if we
   change the RTS to ensure that regCode always contains a legal value.
   
   I'm about to do this! We may or may not need changes to the i386
   and PowerPC versions too.
   
   SPF 17/7/96
*/ 
   

/*
! Note: for Dave's run-time system, we mustn't put a load or store into a
! delay slot unless we can guarantee that the address is non-persistent
! i.e. that we have already "touched" that ML object. (SPF)

! This no longer applies, so restrictions like this can be removed.
! DCJM 17/4/00 
*/

/* unfortunately, we can't include sys.h, because that contains real C */
#define POLY_SYS_return	13
#define EXC_divide      7
#define EXC_substring   11

#ifdef SOLARIS2
#ifdef __STDC__
#define gbl(id)  id
#define gbla(id) id##a
#define gblc(id) id##c
#else
#define gbl(id)  id
#define gbla(id) id/*join*/a
#define gblc(id) id/*join*/c
#endif
#endif

#ifdef BSD
#ifdef __STDC__
#define gbl(id)  _##id
#define gbla(id) _##id##a
#define gblc(id) _##id##c
#else
#define gbl(id)  _/*join*/id
#define gbla(id) _/*join*/id/*join*/a
#define gblc(id) _/*join*/id/*join*/c
#endif
#endif

#define globldec(id) \
	.globl	gbl(id); \
	gbl(id):

/* Standard start-up for inline calls (those that don't call C) */
#define INLINE_ROUTINE(id) \
	globldec(id) \
	or	%o7,2,%o7; /* Required by ML compiler */  \
	mov	UNIT,%o4   /* Also required by ML compiler SPF 17/7/96 */

/* For routines that use the CALL_IO macros, the call to
   set_registers_for_retry sets up the register save area such
   that %o4 gets zapped when we reload the registers after
   returning from C. SPF 17/7/96
*/

#define localMbottom	gbl(A)+4
#define localMpointer	gbl(A)+8
#define localMtop	gbl(A)+12

/* relative to a StackObject structure */
#define P_SPACE(base) 	[base+0]
#define P_PC(base) 	[base+4]
#define P_SP(base) 	[base+8]
#define P_HR(base)	[base+12]
#define P_NREG(base)	[base+16]
#define P_REG(i,base)	[base+(20+4*i)]

#define loadg(x, r)	sethi %hi(x),r; ld [r+%lo(x)],r
#define storeg(r, x, w)	sethi %hi(x),w; st r,[w+%lo(x)]

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
#define		M_O0		0x000001
#define		M_O1		0x000002
#define		M_O2		0x000004
#define		M_O3		0x000008
#define		M_O4		0x000010
#define		M_O5		0x000020
#define		M_G1		0x000040	/* O6 is not used */
#define		M_O7		0x000080
#define		M_L0		0x000100
#define		M_L1		0x000200
#define		M_L2		0x000400
#define		M_L3		0x000800
#define		M_L4		0x001000
#define		M_L5		0x002000
#define		M_L6		0x004000
#define		M_L7		0x008000
#define		M_G2		0x010000

/* TODO: The present mask entries are just a first go through.  Most of them are
   set to Mask_all when they certainly don't need to be.  DCJM 11/12/00. */

#define		RegMask(name,mask) Mask_##name=mask

/* Default mask for unused entries and also for the special cases
   where we don't know what the effect of calling the function
   will be. */
	RegMask(all,0x1ffff)


#ifdef PORTING
/* call_MC_code - Call a piece of machine code from the interpreter. */
globldec(call_MC_code)
! Arguments already in %o0 .. %03;
! code address, closure and return address are saved on the ML stack 
	loadg(gbl(poly_stack), %i5)	! i5 = poly_stack
	ld	P_SPACE(%i5),%i4	! i4 = poly_stack->p_space
	sll	%i4,WORDSHIFT,%i4	! convert words to bytes
	add	%i4,%i5,%g5		! g5 = poly_stack+poly_stack->p_space
	ld	P_HR(%i5),%g3		! hr
	ld	P_SP(%i5),%g4		! sp

/* Passing the following values on the ML stack is less work than trying
   to figure out the native C calling convention for arguments on
   the stack. It's a hack, but it's only temporary code anyway.
   SPF 23/1/97
*/
	ld	[%g4+0],%o4		! %o4 = code
	ld	[%g4+4],%o5		! %o5 = closure
	ld	[%g4+8],%o7		! %o7  = (tagged) return address */
	add	%g4,12,%g4		! pop values from stack.
	st	%g4,P_SP(%i5)		! store modified poly_stack->p_sp (necessary?)

	loadg(localMpointer,%g6)
	loadg(localMbottom,%g7)		! Lower limit of locals
	sub	%g6,%g7,%g7		! Set to space available
	sethi	%hi(0x80000000),%g1
	or	%g1,%g7,%g7		! Subtract maxint to cause overflow
					! when the space runs out
	add	%g6,4,%g6		! Local pointer


	mov	UNIT,%g1		! o0 - 05 already initialised
	mov	UNIT,%g2		! Other registers = TAGGED(0)
	mov	UNIT,%l0
	mov	UNIT,%l1
	mov	UNIT,%l2
	mov	UNIT,%l3
	mov	UNIT,%l4
	mov	UNIT,%l5
	mov	UNIT,%l6
! Ignore rest of set-up for the moment
	storeg(%g0,gbl(in_run_time_system),%g1) ! Clear in_run_time_system
	jmp	%o4			! enter code.
	mov	UNIT,%l7
#endif

/* MD_trap_handler - Called as a result of a garbage collection trap. 
   Saves the state and then calls the run-time
   system.  The original pc has already been saved.
*/
	.global	gbl(MD_trap_handler)
gbl(MD_trap_handler):
	nop
	nop
	nop
	loadg(gbl(poly_stack), %i3)
	st	%g1,[%i3+20]		! general registers
	st	%g2,[%i3+24]
	st	%o0,[%i3+28]
	st	%o1,[%i3+32]
	st	%o2,[%i3+36]
	st	%o3,[%i3+40]
	st	%o4,[%i3+44]
	st	%o5,[%i3+48]
	st	%o7,[%i3+56]
	st	%l0,[%i3+60]
	st	%l1,[%i3+64]
	st	%l2,[%i3+68]
	st	%l3,[%i3+72]
	st	%l4,[%i3+76]
	st	%l5,[%i3+80]
	st	%l6,[%i3+84]
	st	%l7,[%i3+88]
	st	%i4,[%i3+96]		! Unchecked
	st	%i5,[%i3+100]		! Unchecked
	st	%g3,[%i3+12]		! hr
	st	%g4,[%i3+8]		! sp
	sub	%g6,4,%g6
	storeg(%g6,localMpointer,%i5)		! Local pointer
	call	gbl(MD_trap_handler1)
	nop

/***************************************************************************/
/* MD_switch_to_poly_X and MD_switch_to_poly1                              */
/***************************************************************************/

/* Entry point for C */
	.global	gbl(MD_switch_to_poly_X)
gbl(MD_switch_to_poly_X):

/* We need to set up a C stack frame to allow ML to call C. Actually, we
   can avoid this overhead by reusing our caller's stack frame. This
   only works because:
   
      (1) We are called by indirectly by MD_switch_to_poly, not directly from
          enter_poly_code. (We would have a problem if we were called directly
          from enter_poly_code because the latter function requires that its
          stack frame in still in place when it is re-entered via longjmp.)
          
      (2) None of the C functions that we call require more than 4 parameters.
          If they require more parameters than this, we'll need a stack frame
          with extra space to accommodate them, and the MD_switch_to_poly
          stack frame won't be large enough.
          
  So we just "do nothing", very carefully.
  SPF 23/1/97
*/

/* Entry point from CALL_IOn macros (via return_from_io) */
MD_switch_to_poly1:
	/* Our real return address is actually top of the ML stack (rather weird!),
	   unless we're returning from a trap. This is all pretty horrible, but
	   hard to change without doing a full bootstrap, since "commit" will
	   save this state too.
	   SPF 30/1/97
         */
	loadg(gbl(poly_stack), %i3)
	ld	P_SPACE(%i3),%o2
	sll	%o2,WORDSHIFT,%o2	! convert words to bytes
	add	%o2,%i3,%g5		! Set up stack limit reg
	ld	P_SP(%i3),%g4		! sp
	ld	P_HR(%i3),%g3		! hr
	storeg(%g0,gbl(in_run_time_system),%g1) ! Clear in_run_time_system

/* If "interrupted" has been set we point the stack limit register at the
   end of the stack.  This will cause a stack overflow trap soon and return us
   to the run-time system to process the interrupt. */
	loadg(gbl(interrupted),%o2)
	cmp	%o2,%g0
	be	MDstp1
!	nop				! Unnecessary NOP deleted

/*
! N.B. NOT the following:
!	loadg(gbl(end_of_stack),%g5)	! g5 = end_of_stack
! because it wouldn't maintain the invariant that %g5 always contains
! a legal value (pointing to the top or bottom of the stack) whenever
! in_run_time_system isn't set.

! This version is OK, because it updates %g5 atomically.
*/
	loadg(gbl(end_of_stack),%o2)	! o2 = end_of_stack
	mov	%o2,%g5
	
MDstp1:
	loadg(localMpointer,%g6)
	loadg(localMbottom,%g7)		! Lower limit of locals
	sub	%g6,%g7,%g7		! Set to space available
	sethi	%hi(0x80000000),%g1
	or	%g1,%g7,%g7		! Subtract maxint to cause overflow
					! when the space runs out
	add	%g6,4,%g6		! Local pointer

/* If we are profiling store allocation we set g7 to -maxint so that a trap
   will be generated. */
	loadg(gbl(store_profiling),%o2)
	cmp	%o2,%g0
	bne,a	MDstp2
	mov	%g1,%g7
MDstp2:

	ld	P_REG(0,%i3),%g1
	ld	P_REG(1,%i3),%g2
	ld	P_REG(2,%i3),%o0
	ld	P_REG(3,%i3),%o1
	ld	P_REG(4,%i3),%o2
	ld	P_REG(5,%i3),%o3
	ld	P_REG(6,%i3),%o4
	ld	P_REG(7,%i3),%o5
! /* There's a slot reserved for %o6, but we mustn't touch that register. */	
	ld	P_REG(9,%i3),%o7
	ld	P_REG(10,%i3),%l0
	ld	P_REG(11,%i3),%l1
	ld	P_REG(12,%i3),%l2
	ld	P_REG(13,%i3),%l3
	ld	P_REG(14,%i3),%l4
	ld	P_REG(15,%i3),%l5
	ld	P_REG(16,%i3),%l6
	ld	P_REG(17,%i3),%l7
! /* There's a slot reserved for the untagged register count. */	
	ld	P_REG(19,%i3),%i4
	ld	P_REG(20,%i3),%i5
	ld	P_REG(21,%i3),%i2	! condition code (only for emulation)
	ld	P_PC(%i3),%i3		! pc
	
	cmp	%i3,TAGGED(0)
	bgu	MDstp3			! May be zero indicating a retry
	nop
	
/* Retry RTS call - tail-call saved closure */	
	ld	[%o5],%o4		! get code address.
	ld	[%g4],%o7		! restore (tagged) return address
	jmp	%o4+4
	add	%g4,4,%g4		! pop return address from stack
	
! The condition code is not preserved across most traps and is only used
! when emulating an arbitrary precision comparison instruction.
/* Normal return from RTS call or trap. */	
MDstp3:	jmp	%i3			! /* Goes to RTD0, unless we're returning from a trap*/
	cmp	%i2,1			! Set the condition code

#ifdef BSD
! Solaris2 version can do this in C
globldec(MD_increment_profile_count)
! It appears that g4 is stored at a certain offset in the previous frame.
	ld	[%fp+0xf0],%o1
	save	%sp,-(16*4),%sp
	mov	%i0,%o0
	mov	%i1,%o1
	call	gbl(MD_increment_profile_count1)
	restore	%o0,0,%o0
	jmp	%o7+8
	clr	%o7

globldec(MD_interrupt_code)
! g5 is at a certain offset in the previous frame - we need its address.
	add	%fp,0xf4,%o0
	save	%sp,-(16*4),%sp
	mov	%i0,%o0
	call	gbl(MD_interrupt_code1)
	restore	%o0,0,%o0
	jmp	%o7+8
	clr	%o7
#endif


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

We have to reverse the order of the parameters because the C functions
have their parameters backwards. That's presumably something to do
with the first (VAX?) implementation of Poly/ML.
SPF 8/4/1998
*/

#define IND ld [%o0],%o0
#define NOIND nop

/***************************************************************************/
/* CALL_IO0                                                                */
/***************************************************************************/
#define  CALL_IO0(name, res) \
	.global gbla(name); \
gbla(name): \
	or	%o7,2,%o7; \
	mov	UNIT,%o0; \
	mov	UNIT,%o1; \
	mov	UNIT,%o2; \
	mov	UNIT,%o3; \
	st	%o7,[%g4-4]; \
	set	RTD0,%l0; \
	call	gbl(set_registers_for_retry); \
	sub	%g4,4,%g4; \
	set	gbl(save_vec),%l1; \
	storeg(%l1,gbl(save_vec_addr),%i5); \
	call	gblc(name); \
	nop; \
	ba	return_from_io; \
	res; \
	RegMask(name, Mask_all)

/***************************************************************************/
/* CALL_IO1                                                                */
/***************************************************************************/

#define  CALL_IO1(name, res) \
	.global gbla(name); \
gbla(name): \
	or	%o7,2,%o7; \
	mov	UNIT,%o1; \
	mov	UNIT,%o2; \
	mov	UNIT,%o3; \
	st	%o7,[%g4-4]; \
	set	RTD0,%l0; \
	call	gbl(set_registers_for_retry); \
	sub	%g4,4,%g4; \
	set	gbl(save_vec),%l1; \
	add	%l1,4,%l1; \
	st	%o0,[%l1-4]; \
	storeg(%l1,gbl(save_vec_addr),%i5); \
	call	gblc(name); \
	add	%l1,-4,%o0; \
	ba	return_from_io; \
	res; \
	RegMask(name, Mask_all)


/***************************************************************************/
/* CALL_IO2                                                                */
/***************************************************************************/

#define  CALL_IO2(name, res) \
	.globl gbla(name); \
gbla(name): \
	or	%o7,2,%o7; \
	mov	UNIT,%o2; \
	mov	UNIT,%o3; \
	st	%o7,[%g4-4]; \
	set	RTD0,%l0; \
	call	gbl(set_registers_for_retry); \
	sub	%g4,4,%g4; \
	set	gbl(save_vec),%l1; \
	add	%l1,8,%l1; \
	st	%o0,[%l1-8]; \
	add	%l1,-4,%o0; \
	st	%o1,[%l1-4]; \
	storeg(%l1,gbl(save_vec_addr),%i5); \
	call	gblc(name); \
	add	%l1,-8,%o1; \
	ba	return_from_io; \
	res; \
	RegMask(name, Mask_all)


/* CALL_IO2U is a hack needed by exception_trace
   don't try to adjust adjust %o7 */
#define  CALL_IO2U(name, res) \
	mov	UNIT,%o2; \
	mov	UNIT,%o3; \
	st	%o7,[%g4-4]; \
	set	RTD0,%l0; \
	call	gbl(set_registers_for_retry); \
	sub	%g4,4,%g4; \
	set	gbl(save_vec),%l1; \
	add	%l1,8,%l1; \
	st	%o0,[%l1-8]; \
	add	%l1,-4,%o0; \
	st	%o1,[%l1-4]; \
	storeg(%l1,gbl(save_vec_addr),%i5); \
	call	gblc(name); \
	add	%l1,-8,%o1; \
	ba	return_from_io; \
	res; \
	RegMask(name, Mask_all)


/***************************************************************************/
/* CALL_IO3                                                                */
/***************************************************************************/
#define  CALL_IO3(name, res) \
	.globl gbla(name); \
gbla(name): \
	or	%o7,2,%o7; \
	mov	UNIT,%o3; \
	st	%o7,[%g4-4]; \
	set	RTD0,%l0; \
	call	gbl(set_registers_for_retry); \
	sub	%g4,4,%g4; \
	set	gbl(save_vec),%l1; \
	add	%l1,12,%l1; \
	st	%o0,[%l1-12]; \
	add	%l1,-4,%o0; \
	st	%o1,[%l1-8]; \
	add	%l1,-8,%o1; \
	st	%o2,[%l1-4]; \
	storeg(%l1,gbl(save_vec_addr),%i5); \
	call	gblc(name); \
	add	%l1,-12,%o2; \
	ba	return_from_io; \
	res; \
	RegMask(name, Mask_all)

/***************************************************************************/
/* CALL_IO4                                                                */
/***************************************************************************/
#define  CALL_IO4(name, res) \
	.globl gbla(name); \
gbla(name): \
	or	%o7,2,%o7; \
	st	%o7,[%g4-4]; \
	set	RTD0,%l0; \
	call	gbl(set_registers_for_retry); \
	sub	%g4,4,%g4; \
	set	gbl(save_vec),%l1; \
	add	%l1,16,%l1; \
	st	%o0,[%l1-16]; \
	add	%l1,-4,%o0; \
	st	%o1,[%l1-12]; \
	add	%l1,-8,%o1; \
	st	%o2,[%l1-8]; \
	add	%l1,-12,%o2; \
	st	%o3,[%l1-4]; \
	storeg(%l1,gbl(save_vec_addr),%i5); \
	call	gblc(name); \
	add	%l1,-16,%o3; \
	ba	return_from_io; \
	res; \
	RegMask(name, Mask_all)


/***************************************************************************/
/* CALL_IO5                                                                */
/***************************************************************************/
#define  CALL_IO5(name, res) \
	.globl gbla(name); \
gbla(name): \
	or	%o7,2,%o7; \
	st	%o7,[%g4-4]; \
	set	RTD1,%l0; \
	call	gbl(set_registers_for_retry); \
	sub	%g4,4,%g4; \
	set	gbl(save_vec),%l1; \
	add	%l1,20,%l1; \
	st	%o0,[%l1-20]; \
	add	%l1,-4,%o0; \
	st	%o1,[%l1-16]; \
	add	%l1,-8,%o1; \
	st	%o2,[%l1-12]; \
	add	%l1,-12,%o2; \
	st	%o3,[%l1-8]; \
	ld	[%g4+4],%o3; \
	st	%o3,[%l1-4]; \
	add	%l1,-16,%o3; \
	storeg(%l1,gbl(save_vec_addr),%i5); \
	call	gblc(name); \
	add	%l1,-20,%o4; \
	ba	return_from_io; \
	res; \
	RegMask(name, Mask_all)



/***************************************************************************/
/* set_registers_for_retry                                                 */
/***************************************************************************/

globldec(set_registers_for_retry)	! /* global so ``prof'' counts it separately.*/
/*
Sets up the sp, pc and hr values. It is not necessary to save the other
registers because they are not required to be restored after a run-time 
system call so they are cleared. This helps to ensure that values are
not kept in little-used registers long after they have ceased to be of use.
Enters with %l0 pointing to a return address which will remove the
arguments from the stack and return to the caller.
*/
	loadg(gbl(poly_stack),%l1)
	st	%l0,P_PC(%l1)		! pc for normal return
	mov	1,%l0
	st	%g4,P_SP(%l1)		! sp
	storeg(%l0,gbl(in_run_time_system),%i5)
	st	%g3,P_HR(%l1)		! handler register
	clr	P_REG(0,%l1)		! g1
	clr	P_REG(1,%l1)		! g2
	st	%o0,P_REG(2,%l1)	! o0 (Arg 1)
	st	%o1,P_REG(3,%l1)	! o1 (Arg 2)
	st	%o2,P_REG(4,%l1)	! o2 (Arg 3)
	st	%o3,P_REG(5,%l1)	! o3 (Arg 4)
	clr	P_REG(6,%l1)		! o4
	st	%o5,P_REG(7,%l1)	! o5 (Closure ptr)
! /* We don't need to zap the %o6 slot	*/
	clr	P_REG(9,%l1)		! o7
	clr	P_REG(10,%l1)		! l0
	clr	P_REG(11,%l1)		! l1
	clr	P_REG(12,%l1)		! l2
	clr	P_REG(13,%l1)		! l3
	clr	P_REG(14,%l1)		! l4
	clr	P_REG(15,%l1)		! l5
	clr	P_REG(16,%l1)		! l6
	clr	P_REG(17,%l1)		! l7
	sub	%g6,4,%g6
	storeg(%g6,localMpointer,%i5)	! Local pointer
	jmp	%o7+8
	clr	%o7			! Clear o7 "just in case"

return_from_io:
! Reload the registers saved above. Note - any of the registers may
! have been modified by garbage collection so we must load all the registers
! to ensure that we do not have registers pointing at invalid addresses.
	loadg(gbl(poly_stack),%l0)	! Get old stack base
	st	%o0,P_REG(2,%l0)	! Set the result in the %o0 slot
	ba	MD_switch_to_poly1
	nop

RTD0:	ld	[%g4],%o7
	jmp	%o7+6
	add	%g4,4,%g4		! Now uses delay slot (SPF 13/10/94)

RTD1:	ld	[%g4],%o7
	jmp	%o7+6
	add	%g4,8,%g4		! Now uses delay slot (SPF 13/10/94)

/***************************************************************************/
/* Functions implemented in C                                              */
/***************************************************************************/

	CALL_IO1(finish, NOIND)
	CALL_IO1(install_root, NOIND)
	CALL_IO1(change_dir, NOIND)
	CALL_IO3(substring, IND)
	CALL_IO1(profiler, NOIND)
	CALL_IO0(commit, NOIND)
	CALL_IO3(createf, NOIND)
	CALL_IO3(Real_str,NOIND)
	CALL_IO2(Real_geq,NOIND)
	CALL_IO2(Real_leq,NOIND)
	CALL_IO2(Real_gtr,NOIND)
	CALL_IO2(Real_lss,NOIND)
	CALL_IO2(Real_eq,NOIND)
	CALL_IO2(Real_neq,NOIND)
	CALL_IO2(Real_dispatch,NOIND)
	CALL_IO2(Real_add, NOIND)
	CALL_IO2(Real_sub, NOIND)
	CALL_IO2(Real_mul, NOIND)
	CALL_IO2(Real_div, NOIND)
	CALL_IO1(Real_neg, NOIND)
	CALL_IO1(Real_int, NOIND)
	CALL_IO1(Real_float, NOIND)
	CALL_IO1(Real_sqrt, NOIND)
	CALL_IO1(Real_sin, NOIND)
	CALL_IO1(Real_cos, NOIND)
	CALL_IO1(Real_arctan, NOIND)
	CALL_IO1(Real_exp, NOIND)
	CALL_IO1(Real_ln, NOIND)
	CALL_IO1(Real_repr, NOIND)
	CALL_IO1(Real_conv, NOIND)
	CALL_IO2(fork_process, IND)
	CALL_IO2(choice_process, NOIND)
	CALL_IO1(int_process, NOIND)
	CALL_IO0(kill_self, NOIND)
	CALL_IO2(send_on_channel, NOIND)
	CALL_IO1(receive_on_channel, NOIND)

	CALL_IO1(objsize_, IND)                  	! MJC 27/04/88
	CALL_IO1(showsize_, IND)                   	! MJC 09/03/89
	CALL_IO2(timing_dispatch_,IND)			! DCJM 10/4/00
	CALL_IO0(get_dbasetime_,IND)                    ! MJC 15/09/89
	CALL_IO0(interrupt_console_processes_,NOIND)	! MJC 01/08/90

	CALL_IO1(install_subshells_, NOIND)		! MJC 12/09/90

	CALL_IO1(XWindows_, IND)			! MJC 27/09/90

	CALL_IO0(full_gc_, NOIND)                       ! MJC 18/03/91 
	CALL_IO0(stack_trace_, NOIND)                   ! MJC 18/03/91
    
	CALL_IO2(foreign_dispatch_, IND)  		! NIC 22/04/94
	CALL_IO3(IO_dispatch_, IND)		  	! DCJM 8/5/00
	CALL_IO2(Net_dispatch_, IND)		  	! DCJM 22/5/00
	CALL_IO2(OS_spec_dispatch_, IND)		! DCJM 22/5/00
	CALL_IO2(Sig_dispatch_, IND)			! DCJM 18/7/00

	CALL_IO1(shrink_stack_, NOIND)  		! SPF  1/12/96
	CALL_IO2(process_env_dispatch_,IND)			! DCJM 25/4/2000
	CALL_IO1(get_flags_, NOIND)
	CALL_IO2(set_flags_, NOIND)			! SPF 12/02/97
	
	CALL_IO4(set_code_constant, NOIND)		! DCJM 11/1/2001
    
! alloc(size, flags, initial).  Allocates a segment of a given size and
! initialises it.
globldec(alloc_store)
	CALL_IO3(alloc_store_long_, NOIND)
	RegMask(alloc_store,Mask_alloc_store_long_)

INLINE_ROUTINE(int_to_word)
/* Extract the low order 32 bits from an integer.  If it is a tagged integer
   there's nothing to do, if it is a long integer we have to extract the
   low order word from the vector. */
! This first part is no longer used.  Word.fromInt is now implemented using
! is_short and get_first_word_long.
	andcc	%o0,1,%g0
	be	itw0		/* If it's long we have to get the first word. */
	nop
	jmp	%o7+6		/* If it's short we just return it. */
	nop

INLINE_ROUTINE(get_first_long_word_a)
! We have to get the word from the vector of bytes, but it is
! in little-endian order.
itw0:	ldub	[%o0],%i5
	sll	%i5,TAGSHIFT,%o1	! Put tag shifted value into o1
	ldub	[%o0+1],%i5
	sll	%i5,(TAGSHIFT+8),%i5
	or	%i5,%o1,%o1
	ldub	[%o0+2],%i5
	sll	%i5,(TAGSHIFT+16),%i5
	or	%i5,%o1,%o1
	ldub	[%o0+3],%i5
	sll	%i5,(TAGSHIFT+24),%i5
	or	%i5,%o1,%o1
	ldub	[%o0-4],%i5		! See if the "negative" bit is set
	andcc	%i5,0x10,%g0
	bne,a	itw1
	sub	%g0,%o1,%o1		! Delay slot - annulled if not negative
itw1:
	jmp	%o7+6
	or	%o1,1,%o0		! Put result into o0 ensuring tag is set.
	RegMask(get_first_long_word,Mask_all)
	RegMask(int_to_word,Mask_all)


INLINE_ROUTINE(not_bool)
	jmp	%o7+6
	xor	%o0,4,%o0	! use of delay slot (SPF 13/10/94)
	RegMask(not_bool,M_O0|M_O7|M_O4)

INLINE_ROUTINE(or_word)
	jmp	%o7+6
	or	%o0,%o1,%o0
	RegMask(or_word,M_O0|M_O7|M_O4)

INLINE_ROUTINE(and_word)
	jmp	%o7+6
	and	%o0,%o1,%o0
	RegMask(and_word,M_O0|M_O7|M_O4)

INLINE_ROUTINE(xor_word)
	xor	%o0,%o1,%i4	! This will zero the tag field (tags were equal)
	jmp	%o7+6
	or	%i4,1,%o0	! restore the tag bit
	RegMask(xor_word,M_O0|M_O7|M_O4)

! Assume that both args are tagged integers
! Word.<<(a,b) is defined to return 0 if b > Word.wordSize
INLINE_ROUTINE(shift_left_word)
	subcc	%o1,TAGGED(32-TAGSHIFT),%g0
	bgu,a	slw1
	mov	0,%i4			! Only if branch taken i.e. %o1 > 30
	sub	%o0,1,%i4		! untag value to shift (but offset by 2)
	srl	%o1,TAGSHIFT,%i5	! amount to shift
	sll	%i4,%i5,%i4
slw1:	jmp	%o7+6
	or	%i4,1,%o0		! restore the tag bit
	RegMask(shift_left_word,M_O0|M_O7|M_O4)

! Assume that both args are tagged integers
! Word.>>(a,b) is defined to return 0 if b > Word.wordSize
INLINE_ROUTINE(shift_right_word)
	subcc	%o1,TAGGED(32-TAGSHIFT),%g0
	bgu,a	srw1
	mov	0,%i4			! Only if branch taken i.e. %o1 > 30
	srl	%o1,TAGSHIFT,%i5	! amount to shift 
	srl	%o0,%i5,%i4
	andn    %i4,TAGBITS,%i4		! remove stray bits from tag positions
srw1:	jmp	%o7+6
	or	%i4,1,%o0		! restore the tag bit
	RegMask(shift_right_word,M_O0|M_O7|M_O4)

! Assume that both args are tagged integers
! Word.~>>(a,b) is defined to return 0 or ~1 if b > Word.wordSize
INLINE_ROUTINE(shift_right_arith_word)
	subcc	%o1,TAGGED(32-TAGSHIFT),%g0
	bgu,a	saw1
	mov	TAGGED(32-TAGSHIFT),%o1	! Set the shift to 30. This will give us either 0 or ~1.
saw1:	srl	%o1,TAGSHIFT,%i5	! amount to shift 
	sra	%o0,%i5,%i4
	andn    %i4,TAGBITS,%i4		! remove stray bits from tag positions
	jmp	%o7+6
	or	%i4,1,%o0		! restore the tag bit
	RegMask(shift_right_arith_word,M_O0|M_O7|M_O4)

! This is needed in the code generator, but is a very risky thing to do.
INLINE_ROUTINE(offset_address)
	srl	%o1,TAGSHIFT,%i5	! untag offset
	jmp	%o7+6
	add	%o0,%i5,%o0
	RegMask(offset_address,M_O0|M_O7|M_O4)

! INLINE_ROUTINE(name) could be inlined to use delay slot. SPF 17/7/96
#define TEST(name, br_cond) \
	INLINE_ROUTINE(name); \
	cmp	%o0,%o1; \
	br_cond	1f; \
	nop; \
	jmp	%o7+6;  \
	mov	FALSE,%o0; \
1:	jmp	%o7+6;  \
	mov	TRUE,%o0; \
	RegMask(name,M_O0|M_O7|M_O4)

	TEST(int_eq,be)
	TEST(int_neq,bne)

INLINE_ROUTINE(get_dbentrya)
! Get the database specific entry.
	loadg(gbl(processes),%o0)	! Points to the root vector.
	jmp	%o7+6
	ld	[%o0+8],%o0		! /* Load in delay slot - Dave's sytem can't do this */
	RegMask(get_dbentry,M_O0|M_O7|M_O4)

INLINE_ROUTINE(io_operation)
! Returns the address of an entry in io_vector
! should check argument is in range 0..255?
	set	gbl(interface_map),%i5
	sra	%o0,TAGSHIFT,%i4	! Remove tag (was srl)
	sll	%i4,WORDSHIFT,%i4	! convert to word offset
	jmp	%o7+6
	ld	[%i5+%i4],%o0	! /* Load in delay slot - Dave's sytem can't do this */
	RegMask(io_operation,M_O0|M_O7|M_O4)

INLINE_ROUTINE(locksega)
! Clears the "mutable" bit on a segment
	ldub	[%o0-4],%i4
	andcc	%i4,0x02,%g0		! Test code bit
	andn	%i4,0x40,%i4		! Reset mutable bit
	beq	lksg1			! Fall-through for a code-segment
	stb	%i4,[%o0-4]		! (DELAY SLOT)
	
! Get the length in bytes, then flush the instruction cache 	
	ld	[%o0-4],%i4
	sll	%i4,TYPESHIFT,%i4	! Clear top byte of length word
	mov	%o7,%i5			! Save link register
	call	gbl(MD_flush_instruction_cache)
	srl	%i4,(TYPESHIFT-WORDSHIFT),%o1	! Convert to byte-count (DELAY SLOT)
	mov	%i5,%o7			! Restore link register
	
! Zap %o1 and %o2 (corrupted by MD_flush_instruction_cache)
	mov	UNIT,%o1
	mov	UNIT,%o2

! Return UNIT in %o0		
lksg1:
	jmp	%o7+6
	mov	UNIT,%o0
	RegMask(lockseg,Mask_all)


INLINE_ROUTINE(get_length_a)
	ld	[%o0-4],%i4
	sll	%i4,TYPESHIFT,%i4	! Clear top byte
	srl	%i4,(TYPESHIFT-TAGSHIFT),%i4
	jmp	%o7+6
	add	%i4,1,%o0		! return it as a tagged integer
	RegMask(get_length,M_O0|M_O7|M_O4)


/* Added SPF 7/7/94
! Calls a procedure with a given argument vector. The argument vector must
! be copied onto the stack. If the argument vector is nil then there are no
! arguments. One difference between "callcode_tupled" and "callcode" is that
! "callcode_tupled" expects a single parameter which should be an ML pair of
! the function closure and the argument tuple, whereas "callcode" expects
! the closure and the argument tuple to be already in registers. The
! other difference is that "callcode" extracts the arguments from the
! tuple *backwards*, for reasons that I don't understand. */

INLINE_ROUTINE(callcode_tupled)
	ld	[%o0+4],%l1	! %l1 = arg-vec address

	cmp	%l1,1		! Is argument "()" (tagged zero)
	be	clcdt9		! Skip if it is
	ld	[%o0],%o5	! %o5 = closure address

	ld	[%l1-4],%i4	! get length word of arg-vec; "touches" arg-vec
	sll     %i4,TYPESHIFT,%i4	! remove flags
	srl	%i4,(TYPESHIFT-WORDSHIFT),%i4	! %i4 = 4 * number of arguments (non-zero)

        subcc	%i4,4,%i4	! last one?
	be	clcdt9
	ld	[%l1],%o0	! First arg; ld is safe since arg-vec is "touched"
        
        subcc	%i4,4,%i4
	be	clcdt9
	ld	[%l1+4],%o1	! Second arg

        subcc	%i4,4,%i4
	be	clcdt9
	ld	[%l1+8],%o2	! Third arg

        subcc	%i4,4,%i4
	be	clcdt9
	ld	[%l1+12],%o3	! Fourth arg

! this must be a "safe" state
! %o0-%o3 will contain the first 4 (properly tagged) arguments
! %o4 will be unchanged
! %o5 will point to the closure for the function
! %l1 will point to the argument vector
! %i4 will contain an untagged number (4 * remaining argument count)

! stack limit check (added SPF 13/10/94)
	sub	%g4,%i4,%i5
	cmp	%i5,%g5
	tlu	16		! trap to get bigger stack (if needed)

	mov	16,%i5	
clcdt8:	
	ld	[%l1+%i5],%l3	! %i5 has offset for next argument
	sub	%g4,4,%g4
	st	%l3,[%g4]
        subcc	%i4,4,%i4	! last one?
	bne	clcdt8
	add	%i5,4,%i5

! When we get here:
! %o0-%o3 will contain the properly tagged arguments or be unchanged
! %o4 will be unchanged
! %o5 will point to the closure for the function
! %l1 will point to the argument vector or be unchanged
! %l3 will contain a properly tagged value or be unchanged
! %i4 and %i5 will contain "random" untagged numbers
clcdt9:	
	ld	[%o5],%o4
	jmp	%o4		! and jump to the procedure.
	nop
	RegMask(callcode_tupled,Mask_all)



!		String comparison operations.

INLINE_ROUTINE(teststrneq)
	ba	ts1
	mov	FALSE,%o2	! return false (tagged 0) if strings are equal

/* String equality code rewritten (SPF 14/10/94)
   this code wouldn't work for Dave's run-time system */
INLINE_ROUTINE(teststreq)
	mov	TRUE,%o2	! return true (tagged 1) if strings are equal

ts1:
	cmp	%o0,%o1		! are the two values identical?
	be	ts_equal
	andcc	%o0,TAGBITS,%g0
	bnz	ts_unequal	! return if arg1 is a single char (i.e. tag is non-zero)
	andcc	%o1,TAGBITS,%g0 
	bnz	ts_unequal	! return if arg2 is a single char
	nop 

! We have two "long" strings
	ld	[%o0-4],%i4	! get the length words
	ld	[%o1-4],%i5
	cmp	%i4,%i5
	bne	ts_unequal	! return if length words differ
	
	sll     %i4,TYPESHIFT,%i4	! remove type bits
	srl     %i4,(TYPESHIFT-WORDSHIFT),%i4	! convert to byte offset

! The main loop: %i4 has the word offset of the last word we checked
ts5:
	subcc	%i4,4,%i4
	bl	ts_equal	! /* return if we've checked all the words */
!	nop
! /* Don't need a "nop" here, since reloading the length word would be safe */
	
	ld	[%o0+%i4],%i5
	ld	[%o1+%i4],%l2
	cmp	%i5,%l2
	be	ts5
	mov	UNIT,%l2	! zap l2
	
! /* We've found a difference, so return false */
ts_unequal:
	jmp	%o7+6		! invert "string_equal" boolean
	xor	%o2,4,%o0	! 1 xor 4 = 5; 5 xor 4 = 1
	
ts_equal:
	jmp	%o7+6		! return "string_equal" boolean
	mov	%o2,%o0
	RegMask(teststreq,Mask_all)
	RegMask(teststrneq,Mask_all)


INLINE_ROUTINE(teststrgeq)
	mov	%o0,%o2		! a >= b is the same as ~ (b > a)
	mov	%o1,%o0		! swap arguments
	mov	%o2,%o1
	ba	teststrgt
	mov	4,%o4		! Invert the test

INLINE_ROUTINE(teststrleq)
	ba	teststrgt	! a <= b is the same as ~ (a > b)
	mov	4,%o4		! Invert the test

INLINE_ROUTINE(teststrlss)
	mov	%o0,%o2		! a < b is the same as b > a
	mov	%o1,%o0
	mov	%o2,%o1
	ba	teststrgt
	clr	%o4		! /* Don't invert test */

INLINE_ROUTINE(teststrgtr)
	clr	%o4		! /* Don't invert test */
teststrgt:
	andcc	%o0,TAGBITS,%g0	! Single char?
	be	strgt1		! Skip if already an address
	mov	%o0,%o2		! (DELAY SLOT)
	set	strbuff1,%o2
	srl	%o0,TAGSHIFT,%i5
	stb	%i5,[%o2+4]
strgt1:	ld	[%o2],%g0	! Force into store. ( May trap )

	andcc	%o1,TAGBITS,%g0	! Single char?
	be	strgt2		! Skip if already an address
	mov	%o1,%o3		! (DELAY SLOT)
	set	strbuff2,%o3
	srl	%o1,TAGSHIFT,%i5
	stb	%i5,[%o3+4]
strgt2:	ld	[%o3],%o1 	! Load length field (may trap).
	ld	[%o2],%o0	! Can now get length which may be > 64k
! A is greater than B if, at the first position at which A and B differ,
! A[i] > B[i] or if the end of B is found before they differ.
! Set %o0 to the shorter length and %o1 to a -ve value if B is shorter
!
	subcc	%o1,%o0,%o1	! Set o1 to excess of B over A
	bneg,a	strgt3		! i.e. execute next instr if negative
	add	%o0,%o1,%o0	! Subtract (negative) excess if A is longer

! Start of main loop
strgt3:	cmp	%o0,0		! If we have reached the end we skip
	bne	strgt3a
	cmp	%o1,0
	bneg,a	test_s_r	! True if B shorter
	mov	TRUE,%o0
	ba	test_s_r	! False if not
	mov	FALSE,%o0	

strgt3a:
	ldub	[%o2+4],%i5
	ldub	[%o3+4],%g1
	add	%o2,1,%o2
	add	%o3,1,%o3
	cmp	%i5,%g1
	be	strgt3		! Repeat if equal
	sub	%o0,1,%o0
	bg	test_s_r	! Return true if greater
	mov	TRUE,%o0	! (DELAY SLOT)
	mov	FALSE,%o0	! otherwise false

test_s_r:	! Return the result from testing the strings.
	mov	UNIT,%o1	! Clear these invalid addresses.
	mov	UNIT,%o2
	mov	UNIT,%o3
	mov	UNIT,%g1
	xor	%o0,%o4,%o0	! Invert result if necessary
	jmp	%o7+6
	mov	UNIT,%o4	! 0 or 4 are invalid integers, so zap %o4
	RegMask(teststrgeq,Mask_all)
	RegMask(teststrleq,Mask_all)
	RegMask(teststrgtr,Mask_all)
	RegMask(teststrlss,Mask_all)


	.data
strbuff1:
	.long	1
	.long	0
strbuff2:
	.long	1
	.long	0
	.text

INLINE_ROUTINE(string_sub)
	andcc	%o0,TAGBITS,%g0	! Single char?
	bnz	ssub1
	sra	%o1,TAGSHIFT,%i4	! remove tag from index
	
! arg1 is not a single character
	cmp	%i4,0
	ble,a	gbla(raise_ex)	! raise exception Substring if index <= 0
	mov	TAGGED(EXC_substring),%o0
	
	ld	[%o0],%i5	! string length
	cmp	%i4,%i5		! Check index against length
	
	bg,a	gbla(raise_ex)	! raise exception Substring if index > length
	mov	TAGGED(EXC_substring),%o0
	
	add	%i4,3,%i4
	ldub	[%o0+%i4],%i4	! Get character
	sll	%i4,TAGSHIFT,%i4	! Tag it
	jmp	%o7+6
	add	%i4,1,%o0

! arg1 is a single character
ssub1:
	cmp	%i4,1
	bne,a	gbla(raise_ex)	! raise exception Substring if index <> 1
	mov	TAGGED(EXC_substring),%o0

	jmp	%o7+6		! return character unchanged
	nop
	RegMask(string_sub,Mask_all)

	CALL_IO1(raise_ex, NOIND)

/* raisex is used by compiled code. N.B. note that raisex it NOT the same as raise_ex
   I've just rewritten this code to remove a lot of junk that was originally associated
   with supporting Dave Matthews's persistent store system. SPF 9/4/97 */
INLINE_ROUTINE(raisex)
	loadg(gbl(end_of_stack),%i3)	! i3 = end_of_stack
	ld	[%o0],%i4		! i4 = exception id
 	ld	[%g3],%i5		! i5 = handler id
 	mov	%g3,%o1			! o1 = handler ptr

! Loop to find the handler for this exception. Handlers consist of one or more
! pairs of identifier and code address, followed by the address of the next
! handler.
rsx1:	cmp	%i5,TAGGED(0)	! Is the handler identifier 0 or TAGGED(0)?
	bleu	rsx7		! If so we have a default handler.
!	nop			! Unnecessary NOP deleted
	
	! non-default handler
	cmp	%i4,%i5		! Does it match the exception id?
	beq	rsx7		! Skip if we found a match.
	nop
	
	! This handler does not match - try the next one.
	!  This can be either a genuine handler pair, or a
	!  pointer up the stack.
	ld	[%o1+8],%i5	! Get the next handler id
	add	%o1,8,%o1	! Increment the handler pointer
	
	! The very last handler points at itself, so we use "blu", not "bleu" here
	cmp	%i5,%o1
	blu	rsx1		! Not a stack pointer (too small)
!	nop			! Unnecessary NOP deleted
	
	cmp	%i5,%i3
	bgeu	rsx1		! Not a stack pointer (too large)
	nop
	
	! /* It's a stack pointer - get the next batch of handlers */
	mov	%i5,%o1
	b	rsx1
	ld	[%i5],%i5
	
	
rsx7:	! We have found the right handler - %o1 points to the data
	ld	[%o1+4],%o7	! Get the handler entry point

rsx6:	
	ld	[%o1+8],%i5	! Get the next handler id
	add	%o1,8,%o1
	
	! The very last handler points at itself, so we use "blu", not "bleu" here
	cmp	%i5,%o1
	blu	rsx6		! Not a stack pointer (too small)
!	nop			! Unnecessary NOP deleted
	
	cmp	%i5,%i3
	bgeu	rsx6		! Not a stack pointer (too large)
!	nop			! Unnecessary NOP deleted
	
	/* o1 now points at the pointer to the next group of handlers
	   i.e. the old (saved) value of the handler register
	   and %i5 contains the pointer itself */
	
	/* Is this handler a real one, or was it set by exception_trace? */
	cmp	%o7,TAGGED(0)
	bleu	rsx8
	nop

! Ordinary exception
	add	%o1,4,%g4	! Pop stack back past saved rhr
	mov	%i5,%g3		! Reload rhr from saved value
	jmp	%o7-2		! Now enter the handler
	mov	UNIT,%o1	! Zap bad value in %o1

rsx8:
	/*We've found a handler set by exception_trace.
	! Push %o7 onto the stack. It should contain a "return"
	! address inside the function that raised the exception.
	! (That's because raising an exception is actually a CALL
	! to the RTS.) Pushing it onto the stack allows ex_tracec
	! to identify the function that raised the exception. We
	! then put a dummy value into %o7 (there must be some good
	! reason for this, but I can't currently remember it) and
	! finally call ex_tracec, which doesn't return but actually
	! unwinds the stack to the next handler and the re-raises the
	! exception.
	! SPF 9/4/97 */
	mov	%o0,%l1
	st	%o7,[%g4-4]
	mov	%o1,%o0		! stack-mark is arg1
	sub	%g4,4,%g4
	mov	%l1,%o1		! exception packet is arg2
	mov	TAGGED(1),%o7	! make return address look like a tagged int.
	CALL_IO2U(ex_trace, NOIND)

INLINE_ROUTINE(exception_tracea)
! Calls a procedure with no arguments and, if it returns normally, returns
! its result. If the procedure raises an exception it prints a trace of the
! stack from the place where the exception was first raised.
	mov	%o0,%o5		! Get argument (the procedure)
	ld	[%o0],%o4	! Get code address
	mov	TAGGED(0),%o0
	st	%o7,[%g4-4]	! Push (tagged) return address
	st	%g3,[%g4-8]	! Set up handler - save old handler
	st	%o0,[%g4-12]	! push dummy handler address.
	st	%o0,[%g4-16]	! and dummy exception id.
	sub	%g4,16,%g3	! hr now points here.
! The values on the stack must not point directly into this assembly
! code segment otherwise there would be problems if we wrote out the stack.
! Instead we indirect through the interface map.
	loadg(gbl(interface_map)+(POLY_SYS_return*4),%o7)	! Return to return_code
	jmp	%o4		! Enter the procedure.
	sub	%g4,16,%g4	! Adjust stack pointer

globldec(return_code) ! /* It's NOT a function entry point SPF 17/7/96 */
! If it all works we return here. Now remove the handler.
	ld	[%g4+12],%o7
	ld	[%g4+8],%g3
	jmp	%o7+6
	add	%g4,16,%g4	! (DELAY SLOT)
	RegMask(exception_trace,Mask_all)

! Arbitrary precision arithmetic. These only call the procedures in arb.c
! if the values are in the long format.

INLINE_ROUTINE(neg_long)
	sub	%o0,1,%i4	! Remove tag
	tsubcctv	%g0,%i4,%i4
	jmp	%o7+6
	add	%i4,1,%o0
	RegMask(aneg,M_O0|M_O7|M_O4)


INLINE_ROUTINE(add_long)
	sub	%o0,1,%i4	! Remove tags
	sub	%o1,1,%i5
	taddcctv	%i4,%i5,%i4	! Check tags and overflow
	jmp	%o7+6
	add	%i4,1,%o0	! Restore tag
	RegMask(aplus,M_O0|M_O7|M_O4)

INLINE_ROUTINE(sub_long)
	sub	%o0,1,%i4	! Remove tags
	sub	%o1,1,%i5
	tsubcctv	%i4,%i5,%i4	! Check tags and overflow
	jmp	%o7+6
	add	%i4,1,%o0	! Restore tag
	RegMask(aminus,M_O0|M_O7|M_O4)

INLINE_ROUTINE(mult_long)
	and	%o0,1,%i5	! test for any long arguments
	andcc	%o1,%i5,%g0
	bz	mul_really_long
	mov	%o7,%i5		! (DELAY SLOT) Save link reg
	call	mul_signed
	nop
	bnz	mul_really_long	! Skip if overflow
	mov	%i5,%o7		! (DELAY SLOT) Restore link
	jmp	%o7+6
	mov	%o2,%o0		! Get result.

mul_really_long:
	CALL_IO2(mult_long, IND)
	RegMask(amul,Mask_all)

INLINE_ROUTINE(div_long)
	and	%o0,1,%i5	! test for any long arguments
	andcc	%o1,%i5,%g0
	bz	div_really_long
	nop
! The only case of overflow is dividing the smallest negative number by -1
! If there is no overflow use the ordinary divide routine.
	cmp	%o1,-3		! (DELAY SLOT) tagged(-1)
	bne	div_signed
	nop
	cmp	%o0,4		! (DELAY SLOT)
	bvc	div_signed      ! subtracting 4 from tagged(MININT) causes overflow
	nop

div_really_long:
	CALL_IO2(div_long, IND)
	RegMask(adiv,Mask_all)

INLINE_ROUTINE(rem_long)
	and	%o0,1,%i5	! test for any long arguments
	andcc	%o1,%i5,%g0
	bz	rem_really_long
	nop
! The only case of overflow is moding the smallest negative number by -1
! If there is no overflow use the ordinary mod routine.
	cmp	%o1,-3		! (DELAY SLOT)
	bne	rem_signed
	nop
	cmp	%o0,4		! (DELAY SLOT)
	bvc	rem_signed	! subtracting 4 from tagged(MININT) causes overflow
	nop

rem_really_long:
	CALL_IO2(rem_long, IND)
	RegMask(amod,Mask_all)

! INLINE_ROUTINE(name) not optimised due to possible trap. SPF 17/7/96
#define ARBTEST(name, br_cond) \
	INLINE_ROUTINE(name); \
	sub	%o0,1,%i4; \
	sub	%o1,1,%i5; \
	tsubcctv	%i4,%i5,%g0; \
	br_cond	1f; \
	nop; \
	jmp	%o7+6;  \
	mov	FALSE,%o0;  \
1:	jmp	%o7+6;  \
	mov	TRUE,%o0


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
	and	%o0,1,%i5	! test for any long arguments
	andcc	%o1,%i5,%g0
	bz	or_really_long
	nop
	jmp	%o7+6
	or	%o0,%o1,%o0
or_really_long:
	CALL_IO2(or_long, IND)
	RegMask(ora,Mask_all)

INLINE_ROUTINE(and_long)
	and	%o0,1,%i5	! test for any long arguments
	andcc	%o1,%i5,%g0
	bz	and_really_long
	nop
	jmp	%o7+6
	and	%o0,%o1,%o0
and_really_long:
	CALL_IO2(and_long, IND)
	RegMask(anda,Mask_all)

INLINE_ROUTINE(xor_long)
	and	%o0,1,%i5	! test for any long arguments
	andcc	%o1,%i5,%g0
	bz	xor_really_long
	nop
	xor	%o0,%o1,%i4	! This will zero the tag field (tags were equal)
	jmp	%o7+6
	or	%i4,1,%o0	! restore the tag bit
xor_really_long:
	CALL_IO2(xor_long, IND)
	RegMask(xora,Mask_all)

INLINE_ROUTINE(is_shorta)
	and	%o0,1,%i4	! %i4 = 1 for short, 0 for others
	sll	%i4,TAGSHIFT,%i4
	jmp	%o7+6
	add	%i4,1,%o0	! Tag and return it
	RegMask(is_short,M_O0|M_O4|M_O7)
	
/* These are the same as int_eq/neq.  These were previously distinct
   because pointer equality required special code in the old persistent
   store system.  That is no longer relevant. */
	TEST(word_eq, be)
	TEST(word_neq, bne)
	
INLINE_ROUTINE(load_byte)
/* We can assume index will not overflow 30 bits
   Shouldn't we check for Range though? */
	sra	%o1,TAGSHIFT,%i5	! was srl
	ldub	[%o0+%i5],%i4
	sll	%i4,TAGSHIFT,%i4	! tag it
	jmp	%o7+6
	add	%i4,1,%o0
	RegMask(load_byte,M_O0|M_O4|M_O7)

INLINE_ROUTINE(load_word)
/* We can assume index will not overflow 30 bits
   Shouldn't we check for Range though? */
	sub	%o1,1,%i5	! Remove tag bit - the result is a word offset
	jmp	%o7+6
	ld	[%o0+%i5],%o0	! /* Load in the delay slot wouldn't work on Dave's system */ 
	RegMask(load_word,M_O0|M_O4|M_O7)


INLINE_ROUTINE(assign_byte)
/* We can assume index will not overflow 30 bits
   Shouldn't we check for Range though? */
	loadg(localMbottom, %i5)     ! tests for non-local-mutable address
	cmp	%o0,%i5              ! carry set if o0 < localMbottom
	bcs	assign_byte_long
	srl	%o1,TAGSHIFT,%i4     ! Use delay slot to remove tag on arg2
	
	loadg(localMtop, %i5)
	cmp	%o0,%i5              ! carry not set if o0 >= localMtop
	bcc	assign_byte_long
	srl	%o2,TAGSHIFT,%i5     ! Use delay slot to remove tag on arg3

	stb	%i5,[%o0+%i4]
	jmp	%o7+6
        mov	UNIT,%o0	     ! result of operation is unit

assign_byte_long:
	CALL_IO3(assign_byte_long_, NOIND)
	RegMask(assign_byte,Mask_all)

INLINE_ROUTINE(assign_word)
/* We can assume index will not overflow 30 bits
   Shouldn't we check for Range though? */
	loadg(localMbottom, %i5)     ! tests for non-local-mutable address
	cmp	%o0,%i5              ! carry set if o0 < localMbottom
	bcs	assign_word_long
! The following only works if TAGSHIFT = WORDSHIFT
	sub	%o1,1,%i4	     ! Remove tag bit on arg2 (but keep shift)
	
	loadg(localMtop, %i5)
	cmp	%o0,%i5              ! carry not set if o0 >= localMtop
	bcc	assign_word_long
	nop			     ! retain tag on arg3 (it is a word)
	
	st	%o2,[%o0+%i4]
	jmp	%o7+6
        mov	UNIT,%o0	     ! result of operation is unit

assign_word_long:
	CALL_IO3(assign_word_long_, NOIND)
	RegMask(assign_word,Mask_all)

INLINE_ROUTINE(string_length)
	andcc	%o0,TAGBITS,%g0	! Single char strings are represented by the character
	bz,a	sl1
	ld	[%o0],%i4	! Get length field

! a single character	
	jmp	%o7+6		! Return TAGGED(1)
	mov	TAGGED(1),%o0

! not a single character string
sl1:	
	sll	%i4,TAGSHIFT,%i4	! Return tagged length
	jmp	%o7+6
	add	%i4,1,%o0
	RegMask(string_length,M_O0|M_O4|M_O7)

! Store the length of a string in the first word.
INLINE_ROUTINE(set_string_length_a)
	srl	%o1,TAGSHIFT,%i4	! Untag the length
	st	%i4,[%o0]
	jmp	%o7+6			! Return UNIT
	mov	UNIT,%o0

	RegMask (set_string_length,M_O0|M_O4|M_O7)

	
	CALL_IO2(strconcat, IND)
	CALL_IO1(set_dbentry_, NOIND)
	CALL_IO0(BadOpCode_, NOIND)

INLINE_ROUTINE(is_big_endian)
	jmp	%o7+6
	mov	TRUE,%o0	!(DELAY SLOT)	SPARC is big-endian
	RegMask(is_big_endian,M_O0|M_O4|M_O7)

INLINE_ROUTINE(bytes_per_word)
	jmp	%o7+6
	mov	TAGGED(4),%o0	!(DELAY SLOT)	4 bytes per word
	RegMask(bytes_per_word,M_O0|M_O4|M_O7)


! 30-bit signed multiply routine. Takes two tagged 31 bit integers in %o0 and
! %o1 and returns a tagged result in %o2, clearing the Z condition code if
! there has been an overflow.
! It does not modify %o0 or %o1 so that it can be used for arbitrary
! precision multiplies which overflow.
! This code is almost a direct copy of the signed multiply routine in
! the Sparc Architecture Manual.
mul_signed:
	sub	%o1,1,%o1	! Just remove tag bit
	sra	%o0,TAGSHIFT,%o4	! Shift other arg
	mov	%o4,%y		! multiplier to y reg
! We have to allow 3 instruction after the mov %o4,%y before the first
! mulscc so we might as well use that time to see if we can do it quicker.
	andncc	%o4,0xfff,%g0
	be	mls3
	andcc	%g0,%g0,%o4	! (DELAY SLOT) zero pp and clear N and V
	mulscc	%o4,%o1,%o4	! first iteration
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4	! 32nd iteration
	mulscc	%o4,%o1,%o4	! last iteration only shifts
/* Correction for negative multiplier.  Since we don't actually use the
   high order part the only reason for doing this is to get the overflow
   check right. */
	tst	%o0
	rd	%y,%o2
	bge	mls1
	nop			! Delay slot
	sub	%o4,%o1,%o4
mls1:
	add	%o1,1,%o1	! Restore o1 to original value.
! Now check for overflow, setting Z if no overflow
	addcc	%o2,1,%o2	! Set tag bit in result and test.
	bge	mls2
	cmp	%o4,0		! (DELAY SLOT) no overflow if = 0
	cmp	%o4,-1		! no overflow if = -1
mls2:
	mov	1,%o4		! clear o4 (which will be invalid).
	jmp	%o7+8
	clr	%o7
!
! Quicker version for small multipliers.
mls3:
	mulscc	%o4,%o1,%o4	! First iteration of 13
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4
	mulscc	%o4,%o1,%o4	! 12th iteration
	mulscc	%o4,%o1,%o4	! last iteration only shifts
!
	rd	%y,%o5
	sll	%o4,12,%o2
	srl	%o5,20,%o5
	orcc	%o5,%o2,%o2
	add	%o2,1,%o2	! Set tag bit in result.
	mov	UNIT,%o5	! Clear o5 (invalid value).
	add	%o1,1,%o1	! Restore o1 to original value.
! Now check for overflow, setting Z if no overflow
	sra	%o4,20,%o4
	bge	mls4
	cmp	%o4,0
	cmp	%o4,-1
mls4:
	mov	UNIT,%o4	! clear o4 (which will be invalid).
	jmp	%o7+8
	clr	%o7

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
#if defined(SOLARIS2)
globldec(MD_flush_instruction_cache)
	and	%o0,0x7,%o2
	andn	%o0,0x7,%o0	! align on double word
	add	%o1,%o2,%o1	! adjust byte-count to compensate

fic1:
	subcc	%o1,32,%o1	! decrement byte-count
	flush	%o0
	flush	%o0+8
	flush	%o0+16
	flush	%o0+24
	bgt	fic1		! loop if byte-count is still positive
	add	%o0,32,%o0	! (DELAY SLOT)

	jmp	%o7+8		! return to caller
	nop
#endif
		
#if defined(BSD)
/* Unfortunately, the SunOS assembler doesn't support the "flush" instruction,
   so let's hope no-one is running SunOS 4.1 on a HyperSPARC. */
globldec(MD_flush_instruction_cache)
	jmp	%o7+8		! return to caller
	nop
#endif


div_signed:			! Also used by div_long
	cmp	%o1,TAGGED(0)
	be,a	gbla(raise_ex)	! raise Div exception
	mov	TAGGED(EXC_divide),%o0
	
! .div uses g1, g2 and g3 internally so we have to save g3 and clear g1 and g2
	mov	%g3,%o2
	
! Use new register window to call .div
	save	%sp,-(16*4),%sp		! why adjust stack?
	sra	%i0,TAGSHIFT,%o0	! %io == old register %o0
	call	.div
	sra	%i1,TAGSHIFT,%o1	! %i1 == old register %o1
	restore	%o0,0,%i4		! copy from current %o0 to old %i4
	
	mov	%o2,%g3
	mov	UNIT,%g1		! These may be invalid
	mov	UNIT,%g2
	
	sll	%i4,TAGSHIFT,%i4
	jmp	%o7+6
	add	%i4,1,%o0		! Restore tag bits

rem_signed:
	cmp	%o1,TAGGED(0)
	be,a    gbla(raise_ex)		! raise Div exception
	mov	TAGGED(EXC_divide),%o0

! .rem uses g1, g2 and g3 internally so we have to save g3 and clear g1 and g2
	mov	%g3,%o2
	
! Use new register window to call .rem
	save	%sp,-(16*4),%sp		! why adjust stack?
	sra	%i0,TAGSHIFT,%o0
	call	.rem
	sra	%i1,TAGSHIFT,%o1	! (DELAY SLOT)
	restore	%o0,0,%i4		! untagged result into old %i4
	
	mov	%o2,%g3
	mov	UNIT,%g1		! These may be invalid
	mov	UNIT,%g2
	sll	%i4,TAGSHIFT,%i4
	jmp	%o7+6
	add	%i4,1,%o0		! Restore tag bits

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

INLINE_ROUTINE(move_bytes)
! Move a segment of memory from one location to another.
! Must deal with the case of overlapping segments correctly.
! (source (o0), source_offset (o1), destination (o2), dest_offset (o3), length ([%g4]))
	loadg(localMbottom, %i5)     ! tests for non-local-mutable address
	cmp	%o2,%i5              ! carry set if o2 < localMbottom
	bcs	move_bytes_long
	srl	%o1,TAGSHIFT,%i4     ! Use delay slot to remove tag on source offset
	
	loadg(localMtop, %i5)
	cmp	%o2,%i5              ! carry not set if o2 >= localMtop
	bcc	move_bytes_long
	srl	%o3,TAGSHIFT,%i5     ! Use delay slot to remove tag on arg3

	ld	[%g4],%i3	     ! length
	add	%i4,%o0,%i4	     ! source address
	srl	%i3,TAGSHIFT,%i3     ! untagged length
	cmp	0,%i3
	be	mvb3
	add	%i5,%o2,%i5	     ! Use delay slot to calculate destination address
	cmp	%i5,%i4		     ! carry not set if dest >= source
! If dest >= src then use decrementing moves else
! use incrementing moves.
	bcs	mvb2
	nop
	sub	%i4,1,%i4
	sub	%i5,1,%i5
	ldub	[%i4+%i3],%o0	     ! get the first value before the loop
mvb1:	stb	%o0,[%i5+%i3]
	subcc	%i3,1,%i3
	bne,a	mvb1
	ldub	[%i4+%i3],%o0	     ! use delay slot to load next value
	ba	mvb3
	nop

mvb2:	ldub	[%i4],%o0
	add	%i4,1,%i4	     ! incr source pointer
	stb	%o0,[%i5]
	subcc	%i3,1,%i3
	bne	mvb2
	add	%i5,1,%i5	     ! use delay slot to increment dest ptr.

mvb3:	add	%g4,4,%g4	     ! pop the extra arg from the stack
	jmp	%o7+6
        mov	UNIT,%o0	     ! result of operation is unit

move_bytes_long:
	CALL_IO5(move_bytes_long_, NOIND)
	RegMask(move_bytes,Mask_all) ! Should really be the union of the local and non-local cases


INLINE_ROUTINE(move_words)
	CALL_IO5(move_words_long_, NOIND)
	RegMask(move_words,Mask_move_words_long_)

! Word functions.  These are all unsigned and do not raise Overflow

INLINE_ROUTINE(mul_word)
/* mul_signed works equally for signed or unsigned arithmetic if we're not
   interested in overflow. */
	mov	%o7,%i5		! (DELAY SLOT) Save link reg
	call	mul_signed
	nop
	mov	%i5,%o7		! (DELAY SLOT) Restore link
	jmp	%o7+6
	mov	%o2,%o0		! Get result.
	RegMask(mul_word,Mask_all)

INLINE_ROUTINE(plus_word)
	sub		%o1,1,%i5			! Remove a tag
	jmp		%o7+6
	add		%o0,%i5,%o0			! Add the values
	RegMask(plus_word,M_O0|M_O4|M_O7)

INLINE_ROUTINE(minus_word)
	sub		%o1,1,%i5			! Remove a tag
	jmp		%o7+6
	sub		%o0,%i5,%o0			! Subtract the untagged value.
	RegMask(minus_word,M_O0|M_O4|M_O7)

! Unsigned operations for word_div and word_rem
INLINE_ROUTINE(div_word)
	cmp	%o1,TAGGED(0)
	be,a	gbla(raise_ex)	! raise Div exception
	mov	TAGGED(EXC_divide),%o0
	
! .udiv uses g1, g2 and g3 internally so we have to save g3 and clear g1 and g2
	mov	%g3,%o2
	
! Use new register window to call .div
	save	%sp,-(16*4),%sp		! why adjust stack?
	srl	%i0,TAGSHIFT,%o0	! %io == old register %o0 (N.B. logical shift)
	call	.udiv
	srl	%i1,TAGSHIFT,%o1	! %i1 == old register %o1
	restore	%o0,0,%i4		! copy from current %o0 to old %i4
	
	mov	%o2,%g3
	mov	UNIT,%g1		! These may be invalid
	mov	UNIT,%g2
	
	sll	%i4,TAGSHIFT,%i4
	jmp	%o7+6
	add	%i4,1,%o0		! Restore tag bits
	RegMask(div_word,Mask_all)

INLINE_ROUTINE(mod_word)
	cmp	%o1,TAGGED(0)
	be,a    gbla(raise_ex)		! raise Div exception
	mov	TAGGED(EXC_divide),%o0

! .urem uses g1, g2 and g3 internally so we have to save g3 and clear g1 and g2
	mov	%g3,%o2
	
! Use new register window to call .urem
	save	%sp,-(16*4),%sp		! why adjust stack?
	srl	%i0,TAGSHIFT,%o0	! (N.B. logical shift)
	call	.urem
	srl	%i1,TAGSHIFT,%o1	! (DELAY SLOT)
	restore	%o0,0,%i4		! untagged result into old %i4
	
	mov	%o2,%g3
	mov	UNIT,%g1		! These may be invalid
	mov	UNIT,%g2
	sll	%i4,TAGSHIFT,%i4
	jmp	%o7+6
	add	%i4,1,%o0		! Restore tag bits
	RegMask(mod_word,Mask_all)


! Unsigned tests on words.
	TEST(word_geq,bcc)

	TEST(word_leq,bleu)

	TEST(word_gtr,bgu)

	TEST(word_lss,bcs)


/* Register mask vector. - extern int registerMaskVector[];
   Each entry in this vector is a set of the registers modified
   by the function.  It is an untagged bitmap with the registers
   encoded in the same way as in the code generator.
   Unused entries are set to Mask_all for safety in case a new
   entry is added to the iovector without also adding an entry
   here. */
globldec(registerMaskVector)
	.long	Mask_all				/* 0 is unused */
	.long	Mask_finish				/* 1 */
	.long	Mask_install_root       /* 2 */
	.long	Mask_all				/* 3 is unused */
	.long	Mask_all				/* 4 is unused */
	.long	Mask_all				/* 5 is unused */
	.long	Mask_strconcat          /* 6 */
	.long	Mask_all				/* 7 is unused */
	.long	Mask_all				/* 8 is unused */
	.long	Mask_change_dir         /* 9 */
	.long	Mask_all				/* 10 is unused */
	.long	Mask_alloc_store         /* 11 */
	.long	Mask_substring           /* 12 */
	.long	Mask_all				 /* return = 13 */
	.long	Mask_all				 /* raisex = 14 */
	.long	Mask_get_length          /* 15 */
	.long	Mask_all				/* 16 is unused */
	.long	Mask_get_flags_          /* 17 */
	.long	Mask_all            /* 18 - now unused */
	.long	Mask_all             /* 19 - now unused */
	.long	Mask_all            /* 20 - now unused */
	.long	Mask_all				/* 21 is unused */
	.long	Mask_all				/* 22 is unused */
	.long	Mask_all				/* 23 is unused */
	.long	Mask_teststreq           /* 24 */
	.long	Mask_teststrneq          /* 25 */
	.long	Mask_teststrgtr          /* 26 */
	.long	Mask_teststrlss          /* 27 */
	.long	Mask_teststrgeq          /* 28 */
	.long	Mask_teststrleq          /* 29 */
	.long	Mask_exception_trace     /* 30 */
	.long	Mask_all         /* 31 */
	.long	Mask_all          /* 32 - now unused */
	.long	Mask_all          /* 33 - now unused */
	.long	Mask_all         /* 34 - now unused */
	.long	Mask_all         /* 35 - now unused */
	.long	Mask_all           /* 36 - now unused */
	.long	Mask_all				/* 37 is unused */
	.long	Mask_all				/* 38 is unused */
	.long	Mask_all				/* 39 is unused */
	.long	Mask_commit              /* 40 */
	.long	Mask_all				/* 41 is unused */
	.long	Mask_set_dbentry_        /* 42 */
	.long	Mask_get_dbentry         /* 43 */
	.long	Mask_all          /* 44 - now unused */
	.long	Mask_all         /* 45 - now unused */
	.long	Mask_createf             /* 46 */
	.long	Mask_lockseg             /* 47 */
	.long	Mask_all				 /* nullorzero = 48 */
	.long	Mask_all             /* 49 - now unused */
	.long	Mask_all           /* 50 - now unused */
	.long	Mask_Net_dispatch_		 /* 51 */
	.long	Mask_OS_spec_dispatch_	 /* 52 */
	.long	Mask_all				/* 53 is unused */
	.long	Mask_all				/* 54 is unused */
	.long	Mask_all				/* version_number = 55 */
	.long	Mask_all				/* 56 is unused */
	.long	Mask_all				/* 57 is unused */
	.long	Mask_all				/* 58 is unused */
	.long	Mask_all				/* 59 is unused */
	.long	Mask_all				/* 60 is unused */
	.long	Mask_IO_dispatch_		 /* 61 */
	.long	Mask_Sig_dispatch_		 /* 62 */
	.long	Mask_all				/* 63 is unused */
	.long	Mask_all				/* 64 is unused */
	.long	Mask_all				/* 65 is unused */
	.long	Mask_all				/* 66 is unused */
	.long	Mask_all				/* 67 is unused */
	.long	Mask_all				/* 68 is unused */
	.long	Mask_all				/* 69 is unused */
	.long	Mask_all				/* 70 is unused */
	.long	Mask_all				/* 71 is unused */
	.long	Mask_all				/* 72 is unused */
	.long	Mask_all				/* 73 is unused */
	.long	Mask_all				/* 74 is unused */
	.long	Mask_all				/* 75 is unused */
	.long	Mask_all				/* 76 is unused */
	.long	Mask_all				/* 77 is unused */
	.long	Mask_all				/* 78 is unused */
	.long	Mask_all				/* 79 is unused */
	.long	Mask_all				/* Mask_version_number_1 = 80 */
	.long	Mask_all          /* 81 - now unused */
	.long	Mask_fork_process        /* 82 */
	.long	Mask_choice_process      /* 83 */
	.long	Mask_kill_self           /* 84 */
	.long	Mask_int_process         /* 85 */
	.long	Mask_send_on_channel     /* 86 */
	.long	Mask_receive_on_channel  /* 87 */
	.long	Mask_profiler            /* 88 */
	.long	Mask_all				/* 89 is unused */
	.long	Mask_all				/* 90 is unused */
	.long	Mask_all				/* 91 is unused */
	.long	Mask_full_gc_            /* 92 */
	.long	Mask_stack_trace_        /* 93 */
	.long	Mask_timing_dispatch_	 /* 94 */
	.long	Mask_all				/* 95 is unused */
	.long	Mask_all				/* 96 is unused */
	.long	Mask_all				/* 97 is unused */
	.long	Mask_get_dbasetime_      /* 98 */
	.long	Mask_objsize_            /* 99 */
	.long	Mask_showsize_           /* 100 */
	.long	Mask_all				/* 101 is unused */
	.long	Mask_all				/* 102 is unused */
	.long	Mask_interrupt_console_processes_ /* 103 */
	.long	Mask_all				/* 104 is unused */
	.long	Mask_is_short            /* 105 */
	.long	Mask_aplus               /* 106 */
	.long	Mask_aminus              /* 107 */
	.long	Mask_amul                /* 108 */
	.long	Mask_adiv                /* 109 */
	.long	Mask_amod                /* 110 */
	.long	Mask_aneg                /* 111 */
	.long	Mask_xora				 /* 112 */
	.long	Mask_equala              /* 113 */
	.long	Mask_ora				 /* 114 */
	.long	Mask_anda				 /* 115 */
	.long	Mask_all				 /* version_number_3 = 116 */
	.long	Mask_Real_str			 /* 117 */
	.long	Mask_Real_geq            /* 118 */
	.long	Mask_Real_leq            /* 119 */
	.long	Mask_Real_gtr            /* 120 */
	.long	Mask_Real_lss            /* 121 */
	.long	Mask_Real_eq             /* 122 */
	.long	Mask_Real_neq            /* 123 */
	.long	Mask_Real_dispatch		 /* 124 */
	.long	Mask_Real_add            /* 125 */
	.long	Mask_Real_sub            /* 126 */
	.long	Mask_Real_mul            /* 127 */
	.long	Mask_Real_div            /* 128 */
	.long	Mask_all				 /* 129 is unused */
	.long	Mask_Real_neg            /* 130 */
	.long	Mask_all				 /* 131 is unused */
	.long	Mask_Real_repr           /* 132 */
	.long	Mask_Real_conv           /* 133 */
	.long	Mask_Real_int            /* 134 */
	.long	Mask_Real_float          /* 135 */
	.long	Mask_Real_sqrt           /* 136 */
	.long	Mask_Real_sin            /* 137 */
	.long	Mask_Real_cos            /* 138 */
	.long	Mask_Real_arctan         /* 139 */
	.long	Mask_Real_exp            /* 140 */
	.long	Mask_Real_ln             /* 141 */
	.long	Mask_all           /* 142 - now unused */
	.long	Mask_all				 /* 143 is unused */
	.long	Mask_all				 /* 144 is unused */
	.long	Mask_all				 /* 145 is unused */
	.long	Mask_all				 /* 146 is unused */
	.long	Mask_all				 /* 147 is unused */
	.long	Mask_all				 /* stdin = 148 */
	.long	Mask_all				 /* stdout= 149 */
	.long	Mask_process_env_dispatch_	 /* 150 */
	.long	Mask_set_string_length		 /* 151 */
	.long	Mask_get_first_long_word	 /* 152 */
	.long	Mask_all				 /* 153 is unused */
	.long	Mask_all				 /* 154 is unused */
	.long	Mask_all				 /* 155 is unused */
	.long	Mask_all				 /* 156 is unused */
	.long	Mask_all				 /* 157 is unused */
	.long	Mask_all				 /* 158 is unused */
	.long	Mask_all				 /* 159 is unused */
	.long	Mask_all				 /* 160 is unused */
	.long	Mask_all				 /* 161 is unused */
	.long	Mask_all				 /* 162 is unused */
	.long	Mask_all				 /* 163 is unused */
	.long	Mask_all				 /* 164 is unused */
	.long	Mask_all				 /* 165 is unused */
	.long	Mask_all				 /* 166 is unused */
	.long	Mask_all				 /* 167 is unused */
	.long	Mask_all				 /* 168 is unused */
	.long	Mask_all				 /* 169 is unused */
	.long	Mask_all				 /* 170 is unused */
	.long	Mask_all				 /* 171 is unused */
	.long	Mask_all				 /* 172 is unused */
	.long	Mask_all				 /* 173 is unused */
	.long	Mask_all				 /* 174 is unused */
	.long	Mask_all				 /* 175 is unused */
	.long	Mask_all				 /* 176 is unused */
	.long	Mask_all				 /* 177 is unused */
	.long	Mask_all				 /* 178 is unused */
	.long	Mask_all				 /* 179 is unused */
	.long	Mask_all				 /* 180 is unused */
	.long	Mask_all				 /* 181 is unused */
	.long	Mask_all				 /* 182 is unused */
	.long	Mask_all				 /* 183 is unused */
	.long	Mask_all				 /* 184 is unused */
	.long	Mask_all				 /* 185 is unused */
	.long	Mask_all				 /* 186 is unused */
	.long	Mask_all				 /* 187 is unused */
	.long	Mask_all				 /* 188 is unused */
	.long	Mask_io_operation        /* 189 */
	.long	Mask_all				 /* 190 is unused */
	.long	Mask_all           /* 191 - now unused */
	.long	Mask_all				 /* 192 is unused */
	.long	Mask_all				 /* 193 is unused */
	.long	Mask_set_code_constant	 /* 194 */
	.long	Mask_move_words			 /* 195 */
	.long	Mask_shift_right_arith_word	 /* 196 */
	.long	Mask_int_to_word		 /* 197 */
	.long	Mask_move_bytes			 /* 198 */
 	.long	Mask_all				 /* 199 now unused */
	.long	Mask_set_flags_          /* 200 */
	.long	Mask_shrink_stack_       /* 201 */
	.long	Mask_all				 /* stderr = 202 */
 	.long	Mask_all				 /* 203 now unused */
	.long	Mask_callcode_tupled     /* 204 */
	.long	Mask_foreign_dispatch_   /* 205 */
	.long	Mask_install_subshells_  /* 206 */
	.long	Mask_all				 /* 207 is unused */
	.long	Mask_all				 /* 208 now unused */
	.long	Mask_XWindows_           /* 209 */
	.long	Mask_all				 /* 210 is unused */
	.long	Mask_all				 /* 211 is unused */
	.long	Mask_all				 /* 212 is unused */
	.long	Mask_is_big_endian       /* 213 */
	.long	Mask_bytes_per_word      /* 214 */
	.long	Mask_offset_address      /* 215 */
	.long	Mask_shift_right_word    /* 216 */
	.long	Mask_word_neq            /* 217 */
	.long	Mask_not_bool            /* 218 */
	.long	Mask_all				 /* 219 is unused */
	.long	Mask_all				 /* 220 is unused */
	.long	Mask_all				 /* 221 is unused */
	.long	Mask_all				 /* 222 is unused */
	.long	Mask_string_length       /* 223 */
	.long	Mask_all				 /* 224 is unused */
	.long	Mask_all				 /* 225 is unused */
	.long	Mask_all				 /* 226 is unused */
	.long	Mask_all				 /* 227 is unused */
	.long	Mask_all				 /* 228 is unused */
	.long	Mask_int_eq              /* 229 */
	.long	Mask_int_neq             /* 230 */
	.long	Mask_int_geq             /* 231 */
	.long	Mask_int_leq             /* 232 */
	.long	Mask_int_gtr             /* 233 */
	.long	Mask_int_lss             /* 234 */
	.long	Mask_string_sub          /* 235 */
	.long	Mask_all				 /* 236 is unused */
	.long	Mask_all				 /* 237 is unused */
	.long	Mask_mul_word            /* 238 */
	.long	Mask_plus_word           /* 239 */
	.long	Mask_minus_word          /* 240 */
	.long	Mask_div_word            /* 241 */
	.long	Mask_or_word             /* 242 */
	.long	Mask_and_word            /* 243 */
	.long	Mask_xor_word            /* 244 */
	.long	Mask_shift_left_word     /* 245 */
	.long	Mask_mod_word            /* 246 */
	.long	Mask_word_geq            /* 247 */
	.long	Mask_word_leq            /* 248 */
	.long	Mask_word_gtr            /* 249 */
	.long	Mask_word_lss            /* 250 */
	.long	Mask_word_eq             /* 251 */
	.long	Mask_load_byte           /* 252 */
	.long	Mask_load_word           /* 253 */
	.long	Mask_assign_byte         /* 254 */
	.long	Mask_assign_word         /* 255 */

