/*
    Title: 	Assembly code for Power architecture.

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
 
r23        is the address of the code being called (like SPARC %o4)
r24        is the closure pointer or static link pointer (like SPARC %o5)
rr (rr)   is used as the compiler-visible link register (like SPARC %o7)
r26        RTS scratch register
r27 (rsp)  is the ML stack pointer,
r28 (rsl)  is the stack limit,
r29 (rhp)  is the heap pointer,
r30 (rhl)  is the heap limit,
r31 (rhr)  points to the top exception handler.

r7-r10 and r12-r22 (15 registers) are available as general work registers,
as are r3-r6 and r23-rr, when they are not fulfilling their specialised
duties. That's a total of 21 general-purpose registers (as opposed to
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

	mtlr	rr
	blr

on the assumption that the return-address is still in rr.
However, we needn't (shouldn't) be quite a naive as this, since
we can get better super-scalar performance by interleaving these
instructions with the last few "real" instructions of the function.

All non-scratch registers MUST contain properly tagged values if
there any a possibility of a garbage collection. 
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
   
   We certainly need to change the PowerPC version so that it zaps regCode.
   SPF 18/7/96
*/

/*
This code was originally written for the AIX assembler.  I have ported it
to run on Linux and Mac OS X (beta).  I have tried to retain the old AIX
code but I haven't checked it to see if it will still assemble on AIX.
DCJM 12/11/00.
*/

/***************************************************************************/
/* Useful macro definitions                                                */
/***************************************************************************/

/* unfortunately, we can't include sys.h, because that contains real C */
#define POLY_SYS_return	13
#define EXC_size       4
#define EXC_divide      7
#define EXC_subscript   11

#ifdef MACOSX
#define gbl(id)	_##id
#else
#define gbl(id)  id
#endif

#define gbla(id) id##a

#ifndef AIX
#define gblc(id) gbl(id##c)
#else
#define gblc(id) .##id##c[pr]
#endif

#ifdef MACOSX
#define EXTERN(x)	.globl x
#else
#define EXTERN(x)	.extern x
#endif

#define RETURNOFFSET (2)

#ifdef AIX
/* Assembler uses relational operators for shifts! */
#define shiftup(word,places) ((word)<(places))
#define shiftdown(word,places) ((word)>(places))
#else
#define shiftup(word,places) ((word)<<(places))
#define shiftdown(word,places) ((word)>>(places))
#endif

#ifdef USE_SPARC_TAGGING
/* The original version was ported from the Sparc and used the same 2 bit shift.
   I've now changed it to use a single bit shift so there are now 31 bits of
   precision plus a tag bit.  DCJM 13/11/00. */ 
#define TAGSHIFT 2
#define TAGBITS (0x3)
#else
#define TAGSHIFT 1
#define TAGBITS (0x1)
#endif
#define TAGGED(n) (shiftup(n,TAGSHIFT)+1)
#define TRUE  TAGGED(1)
#define FALSE TAGGED(0)
#define UNIT TAGGED(0)

/* generate standard prelude */

#define globldec(id) .globl gbl(id) ; gbl(id) :

/* Standard start-up for inline calls (those that don't call C) */
#define INLINE_ROUTINE(id) \
	globldec(id) \
	li	r23,UNIT; /* Required by ML compiler SPF 18/7/96 */

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

/* Guaranteed minimum usable stack (in bytes) when we call an ML function.
   This must be a conservative (big) estimate.
*/
#define MINSTACK 100

/* Bits in the */
#define	B_bytes			1
#define	B_code			2
#define	B_mutable		64
#define B_first_is_ptr	4

/***************************************************************************/
/* Datastructure offsets                                                   */
/***************************************************************************/

/* relative to the structure A */
#define localMbottom	4
#define localMpointer	8
#define localMtop	12

/* relative to a StackObject structure */
/* not yet done */


/***************************************************************************/
/* General Assembler Initialisation                                        */
/***************************************************************************/

	/* make sure we use only portable code */
#ifdef AIX
	.machine "com"
#endif

#ifdef MACOSX
	/* untagged registers */
#define rtemp1	r11	/* used by compiler */
#define	rtemp2	r12	/* used by compiler */
#define	rtemp3	r26	/* scratch */

	/* AIX predefined registers */
#define rtoc	r2	/* Point at TOC section */

	/* special purpose */
#define	rr	r25	/* regReturn  */
	
	/* dedicated ML registers */
#define	rsp	r27
#define	rsl	r28
#define	rhp	r29
#define	rhl	r30
#define	rhr	r31
#else

	/* untagged registers */
	.set r0,0	/* used by compiler, but scratch in a trap handler */
	.set rtemp1,11	/* used by compiler */
	.set rtemp2,12	/* used by compiler */
	.set rtemp3,26	/* scratch */

	/* AIX predefined registers */
	.set r1,1	/* C stack */
	.set rtoc,2	/* Point at TOC section */
	
	/* parameter registers */
	.set r3,3	/* arg 1 and result */
	.set r4,4	/* arg 2 */
	.set r5,5	/* arg 3 */
	.set r6,6	/* arg 4 */

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
	.set r23,23	/* regCode    */
	.set r24,24	/* regClosure */
	.set rr,25	/* regReturn  */
	
	/* dedicated ML registers */
	.set rsp,27
	.set rsl,28
	.set rhp,29
	.set rhl,30
	.set rhr,31
#endif

;# Register mask entries - must match coding used in codeCons.ML
#define		M_R3		0x000001
#define		M_R4		0x000002
#define		M_R5		0x000004
#define		M_R6		0x000008
#define		M_R7		0x000010
#define		M_R8		0x000020
#define		M_R9		0x000040
#define		M_R10		0x000080
#define		M_R13		0x000100
#define		M_R14		0x000200
#define		M_R15		0x000400
#define		M_R16		0x000800
#define		M_R17		0x001000
#define		M_R18		0x002000
#define		M_R19		0x004000
#define		M_R20		0x008000
#define		M_R21		0x010000
#define		M_R22		0x020000
#define		M_R23		0x040000
#define		M_R24		0x080000
#define		M_R25		0x100000
#define		M_RR		M_R25

#define		RegMask(name,mask) .set Mask_##name,mask

/* Default mask for unused entries and also for the special cases
   where we don't know what the effect of calling the function
   will be. */
	RegMask(all,0x1fffff)

/***************************************************************************/
/* Standard function prelude stuff                                         */
/***************************************************************************/
	.set	argarea,	32
	.set	linkarea,	24
	.set	locstkarea,	0
	.set	nfprs,		0
	.set	ngprs,		(32-13)
	.set	szdsa,		8*nfprs+4*ngprs+linkarea+argarea+locstkarea

#ifdef AIX
/* Declarations for AIX millicode routines */
	.extern .__divss
	.extern .__mull
#endif

/***************************************************************************/
/* Branch-prediction mnemonics                                             */
/***************************************************************************/

/* 
   The AIX 3.2 assembler doesn't support branch-prediction mnemonics.
   Worse than that, it silently generates bad branches, because it
   parses:
   
	beq-	neg_long1
	 
   as:

	beq	- neg_long1
   
   With the result that the code just wanders off in random directions.
   This took me a (very frustrating) day to find.
   
   SPF 31/8/95
*/
   
   
   

#ifdef AIX
/* AIX 3.2 version */
#define beqM beq
#define bsoM bso
#define bgtP bgt
#define bltP blt
#define beqP beq
#define bneP bne
#define bnslrP bnslr
#define bltlrP bltlr

#else
/* When we get a decent assembler, we'll use these definitions instead */
#define beqM beq-
#define bsoM bso-
#define bgtP bgt+
#define bltP blt+
#define beqP beq+
#define bneP bne+
#define bltlrP bltlr+
#endif

/***************************************************************************/
/* Declarations for C data and functions                                   */
/***************************************************************************/

#ifndef AIX
	EXTERN(gbl(MD_trap_handler1))
	/* Needed to flush caches after we generate instructions */
#else	
	.extern gbl(.MD_trap_handler1[pr])
	.extern gbl(.MD_trap_handler_address[pr])
	/* Needed to flush caches after we generate instructions */
	.extern gbl(._sync_cache_range)
#endif

#ifdef MACOSX
#define TOCENTRY(name)	.globl	gbl(name)
#define	LOADADDR(reg, name)	addis	reg,0,ha16(name); addi	reg,reg,lo16(name)
#elif (! defined(AIX))
#define TOCENTRY(name)	.globl	gbl(name)
#define	LOADADDR(reg, name)	addis	reg,0,name@ha; addi	reg,reg,name@l
#else

#define TOCENTRY(name) .extern name[rw]; T.##name: .tc name[tc], name[rw];
/* DCJM: I haven't actually tested this on AIX */
#define	LOADADDR(reg, name)	lwz	reg,T.name(rtoc)

	.toc

T.MD_trap_handler: .tc MD_trap_handler[tc], MD_trap_handler
#endif

	TOCENTRY(processes)
	TOCENTRY(interface_map)
	TOCENTRY(in_run_time_system)
	TOCENTRY(interrupted)
	TOCENTRY(store_profiling)
	
/* save_vec is a C array; save_vec_addr is a pointer into that array */
	TOCENTRY(save_vec)
	TOCENTRY(save_vec_addr)
	
	TOCENTRY(poly_stack)
	TOCENTRY(end_of_stack)
	TOCENTRY(A) /* MJC's disgusting naming convention */


#ifdef AIX
	TOCENTRY(msg1)
	TOCENTRY(msg2)
	TOCENTRY(msg3)

	.csect	msg1[rw]
	.byte "mult_really_long: %8x, %8x"
	.byte  10,0
	.csect	msg2[rw]
	.byte "mult_really_long2: %8x, %8x"
	.byte  10,0
	.csect	msg3[rw]
	.byte "mult_long: %8x, %8x"
	.byte  10,0

#endif

#define SHOUT(msg) \
\
	mr	r13,r3;\
	mr	r14,r4;\
	mr	r15,r5;\
	mr	r16,r6;\
	mr	r17,r7;\
	mr	r18,r8;\
	mr	r19,r9;\
	mr	r20,r10;\
	mflr	r21;\
	mfcr	r22;\
;\
	/* set in_run_time_system, to protect C code from interrupts */;\
	LOADADDR(r10,gbl(in_run_time_system));\
	li	r9,1;\
	stw	r9,0(r10);\
;\
	.extern .printf;\
	mr	r5,r4;\
	mr	r4,r3;\
	LOADADDR(r3,msg);\
	bl	.printf;\
	cror	31,31,31;\
;\
	/* clear in_run_time_system */;\
	LOADADDR(r10,gbl(in_run_time_system));\
	li	r9,0;\
	stw	r9,0(r10);\
;\
	mr	r3,r13;\
	mr	r4,r14;\
	mr	r5,r15;\
	mr	r6,r16;\
	mr	r7,r17;\
	mr	r8,r18;\
	mr	r9,r19;\
	mr	r10,r20;\
	mtlr	r21;\
	mtcr	r22;

#ifdef AIX
	.csect
#endif


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
/* MD_trap_handler                                                         */
/***************************************************************************/

/* Abbreviated version - state has already been saved by C routines  */

/* 
   MD_trap_handler - Called as a result of a trap. 
   Most of the state has already been saved - here we just save the
   LR so that it can be used by the interpretted version. 
*/
globldec(MD_trap_handler)
/* This call never returns */
	mflr	r3		/* added 29/9/95 SPF */
	ori	r3,r3,2         /* convert to proper code-pointer */
#ifdef AIX
	bl	gbl(.MD_trap_handler1[pr])
#else
	bl	gbl(MD_trap_handler1)
#endif
	cror	31,31,31


/* Return the code address of MD_trap_handler */
#ifndef AIX
	globldec(MD_trap_handler_address)
#else
globldec(.MD_trap_handler_address)
#endif
	LOADADDR(r3, gbl(MD_trap_handler))	/* r3 = &MD_trap_handler */
	blr

/***************************************************************************/
/* set_registers_for_retry                                                 */
/***************************************************************************/
/*
Sets up the sp, pc and hr values, saves the parameter registers and
zaps the tagged registers (except for r24 and rr, which are used for
retrying the function call).
*/

/*
Offset 0	p_space
       4	p_pc
       8	p_sp
      12	p_hr
      16	p_nregs (22)
      20	r3-r10  (8 registers)
      52	r13-rr (13 registers)
     104	link register
     108	number of untagged registers (3)
     112	rtemp1
     116	rtemp2
     120	CR
*/

globldec(set_registers_for_retry)
	LOADADDR(rtemp3,gbl(poly_stack))	/* rt3 = &poly_stack */
	lwz	rtemp3,0(rtemp3)		/* rt3 =  poly_stack */
	stw	rr,4(rtemp3)		/* poly_stack->p_pc for normal return */
	stw	rsp,8(rtemp3)		/* poly_stack->p_sp */
	stw	rhr,12(rtemp3)		/* poly_stack->p_hr */

#ifdef TEMPORARY
	/* Initialise tagged register count (since we may be
	   overwriting an old, SPARC-style stack segment)
	*/
	li	r0,22
	stw	r0,16(rtemp3)
#endif

	li r23,UNIT				/* I don't know if this is really needed or not.
							   r23 contains the address of the code on a
							   normal call and the INLINE_ROUTINE macro clears
							   it for calls that don't involve calling C and
							   the assembly code sections for i386 and Sparc
							   both clear the equivalent register here.
							   DCJM 29/11/00. */	
	/* save the parameter registers */
	stw	r3,20(rtemp3)		/* save r3 */
	stw	r4,24(rtemp3)		/* save r4 */
	stw	r5,28(rtemp3)		/* save r5 */
	stw	r6,32(rtemp3)		/* save r6 */

	/* and the general registers */
	stw	r7,36(rtemp3)		/* save r7 */
	stw	r8,40(rtemp3)		/* save r8 */
	stw	r9,44(rtemp3)		/* save r9 */
	stw	r10,48(rtemp3)		/* savep r10 */
	
	stw	r13,52(rtemp3)		/* save r13 */
	stw	r14,56(rtemp3)		/* save r14 */
	stw	r15,60(rtemp3)		/* save r15 */
	stw	r16,64(rtemp3)		/* save r16 */
	stw	r17,68(rtemp3)		/* save r17 */
	stw	r18,72(rtemp3)		/* save r18 */
	stw	r19,76(rtemp3)		/* save r19 */
	stw	r20,80(rtemp3)		/* save r20 */
	stw	r21,84(rtemp3)		/* save r21 */
	stw	r22,88(rtemp3)		/* save r22 */
	stw	r23,92(rtemp3)		/* save r23 */
	
	/* and the closure pointer (used for linkage) */
	stw	r24,96(rtemp3)		/* save r24 (closure pointer) */
	stw	rr,100(rtemp3)		/* save rr (genuine return address) */
	stw	rr,104(rtemp3)		/* and also set this as the link reg. */
  		
#ifdef TEMPORARY
	/* Initialise untagged register count (since we may be
	   overwriting an old, SPARC-style stack)
	*/
	li	r0,3
	stw	r0,108(rtemp3)
#endif

	/* ignore the untagged registers */

	/* Initialise in_run_time_system */
	LOADADDR(rtemp2,gbl(in_run_time_system)) /* rt2 = &in_run_time_system */
 	li	rtemp1,1
	stw	rtemp1,0(rtemp2)
	
	/* Update A.M.pointer from rhp */
	LOADADDR(rtemp2,gbl(A))		/* rt2 = &A */
	stw	rhp,localMpointer(rtemp2)	/* A.M.pointer = rhp */

        /* Return to CALL_IOn macro */
	blr

/***************************************************************************/
/* MD_switch_to_poly_X and MD_switch_to_poly1                              */
/***************************************************************************/

/* Entry point for C */
#ifndef AIX
globldec(MD_switch_to_poly_X)
#else
globldec(.MD_switch_to_poly_X)
#endif

/* This initialisation is needed to support calls from ML to C */
	mflr	r0
	mfcr	rtemp2
	stmw	r13,-8*ngprs-4*ngprs(r1) /* DCJM.  Formerly stm. */
	stw	r0,8(r1)
	stw	rtemp2,4(r1)
	stwu	r1,-szdsa(r1)
	
/* Entry point from CALL_IOn macros */
MD_switch_to_poly1:

	LOADADDR(rtemp3,gbl(poly_stack))	/* rt3 = &poly_stack */
	lwz	rtemp3,0(rtemp3)		/* rt3 =  poly_stack */
	
	/* set up stack limit register */
	lwz	rtemp1,0(rtemp3)	/* get size of save area (in words) */
	slwi	rtemp1,rtemp1,WORDSHIFT		/* convert to bytes */
	add	rsl,rtemp3,rtemp1
	
	/* Load rsp and rhr now in case of profile trap */
	lwz	rsp,8(rtemp3)
	lwz	rhr,12(rtemp3)
	
	/* clear "in_run_time_system" */
	LOADADDR(rtemp2,gbl(in_run_time_system)) /* rt2 = &in_run_time_system */
 	li	r0,0
	stw	r0,0(rtemp2)
	
	/* Is "interrupted" set? */
	LOADADDR(rtemp2,gbl(interrupted))	/* rt2 = &interrupted */
	lwz	rtemp1,0(rtemp2)		/* rt1 =  interrupted */
	cmpwi	rtemp1,0
	beqP	MDstp1
	
/* If "interrupted" has been set we point the stack limit register at the
   end of the stack.  This will cause a stack overflow trap soon and return us
   to the run-time system to process the interrupt.
*/
	lwz	rtemp1,-4(rtemp3)		/* stack length word */
	rlwinm	rtemp1,rtemp1,WORDSHIFT,(TYPESHIFT-WORDSHIFT),(31-WORDSHIFT)
	add	rsl,rtemp3,rtemp1
	
MDstp1:
	/* Load rhp from A.M.pointer */
	LOADADDR(rtemp2,gbl(A))		/* rt2 = &A */
	lwz	rhp,localMpointer(rtemp2)	/* A.M.pointer = rhp */
	
	/* Set rhl to the number of free bytes */
	lwz	rtemp1,localMbottom(rtemp2)	/* rt1 = A.M.bottom */
	subfc	rhl,rtemp1,rhp			/* rhl = rhp - rt1 */
	
	/* Is "store_profiling" set? */
	LOADADDR(rtemp2,gbl(store_profiling))	/* rt2 = &store_profiling */
	lwz	rtemp1,0(rtemp2)		/* rt1 =  store_profiling */
	cmpwi	rtemp1,0
	beqP	MDstp2

	/* If we are profiling store allocations, set rhl to 0
	   ("there's no space left") so that a trap will be generated. */
	li	rhl,0
MDstp2:
        /* Save the ML stack pointer and handler registers */
	stw	rsp,8(rtemp3)
	stw	rhr,12(rtemp3)
   
	/* reload the parameter registers */
	lwz	r3,20(rtemp3)
	lwz	r4,24(rtemp3)
	lwz	r5,28(rtemp3)
	lwz	r6,32(rtemp3)
	lwz	r7,36(rtemp3)
	lwz	r8,40(rtemp3)
	lwz	r9,44(rtemp3)
	lwz	r10,48(rtemp3)
	
	/* reload the general registers */
	lwz	r13,52(rtemp3)
	lwz	r14,56(rtemp3)
	lwz	r15,60(rtemp3)
	lwz	r16,64(rtemp3)
	lwz	r17,68(rtemp3)
	lwz	r18,72(rtemp3)
	lwz	r19,76(rtemp3)
	lwz	r20,80(rtemp3)
	lwz	r21,84(rtemp3)
	lwz	r22,88(rtemp3)
	
	lwz	r23,92(rtemp3)
	lwz	r24,96(rtemp3)
	lwz	rr,100(rtemp3)

	/* reload the scratch registers */
	lwz	rtemp1,112(rtemp3)
	lwz	rtemp2,116(rtemp3)
	
	/*
	   If we're returning from a trap, we must ensure that if LR
	   cached rr before the trap, it still does so afterwards.
	   We ensure that this happens by simply copying rr into LR,
	   and returning via CTR. Note that this means that the
	   value in LR will be tagged, even if it wasn't tagged
	   before the trap. Our code mustn't make any assumptions
	   about whether LR is tagged or not.
	*/
	/* We now explicitly save the link register so we need to
	   restore it.  The reason for the change is that we no
	   want to be able to select a return register rather than
	   using a fixed one. DCJM 1/12/00. */
#ifdef TEMPORARY
	stw	rr,104(rtemp3)		/* This won't have been correct. */
#endif
	lwz	r0,104(rtemp3)
	mtlr	r0

	lwz	r0,120(rtemp3)		/* condition codes */

	lwz	rtemp3,4(rtemp3)	/* pc */
	cmplwi	rtemp3,TAGGED(0)
	bgtP	MDstp3			/* (TAGGED) 0 indicates a retry */
	
	/* Retry. Tail-call the saved RTS closure. */
	lwz	r23,0(r24)	/* get code address from closure */
	mtctr	r23		/* code address for tail-call */
	mcrxr	0		/* Clear overflow bit.  DCJM 14/11/00. */
	mtcr	r0		/* restore condition codes */
	bctr			/* complete tail-call */
	
MDstp3:
	/* 
	   Normal return (we MUST reset the XER overflow state here,
	   because the compiled code for arbitrary precision arithmetic
	   relies on the SO flag being reset on return from a trap).
	   
	   If we're returning from a trap, we mustn't disturb LR,
	   so we return using CTR instead.
	 */
	/* I don't understand that comment.  The original code did NOT
	   include the next instruction and instead called mcrxr 0 before
	   every instruction which needed to check for overflow.  I've
	   removed that extra instruction and inserted the mcrxr 0 here.
	   DCJM 14/11/00. */
	mcrxr	0		/* Clear overflow bit. */
	mtctr	rtemp3		/* set up return address */
	mtcr	r0		/* restore condition codes */
	bctr			/* complete return */
		

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

#define IND lwz	r3,0(r3)
#define NOIND /* do nothing */

/***************************************************************************/
/* CALL_IO0                                                                */
/***************************************************************************/
#define CALL_IO0(name, res) \
globldec(gbla(name)) \
	mflr	rr; \
	ori	rr,rr,RETURNOFFSET; \
	bl	gbl(set_registers_for_retry); \
	cror	31,31,31; /* needed? */\
/* Initialise save_vec_addr */ \
	LOADADDR(rtemp2,gbl(save_vec));       /* rt2 = &(save_vec[0]) */ \
	LOADADDR(rtemp1,gbl(save_vec_addr));  /* rt1 = &save_vec_addr */ \
	stw	rtemp2,0(rtemp1); \
/* Call the C version of the function */ \
	bl	gblc(name); \
	cror	31,31,31; \
/* Save result, dereferencing if necessary */ \
	LOADADDR(rtemp3,gbl(poly_stack));  /* rt3 = &poly_stack */ \
	lwz	rtemp3,0(rtemp3);           /* rt3 =  poly_stack */ \
	res; \
	stw	r3,20(rtemp3); \
/* Restart execution of ML */ \
	b	MD_switch_to_poly1; \
	RegMask(name,M_R3|M_RR|M_R23)

/***************************************************************************/
/* CALL_IO1                                                                */
/***************************************************************************/

#define  CALL_IO1_LOCAL(name, res) \
        /*EXTERN(gblc(name));*/ \
	mflr	rr; \
	ori	rr,rr,RETURNOFFSET; \
	bl	gbl(set_registers_for_retry); \
	cror	31,31,31; /* needed? */\
/* Initialise save_vec_addr */ \
	LOADADDR(rtemp2,gbl(save_vec));       /* rt2 = &(save_vec[0]) */ \
	LOADADDR(rtemp1,gbl(save_vec_addr));  /* rt1 = &save_vec_addr */ \
/* Save parameter(s), reversing their order. */ \
	addi	rtemp2,rtemp2,4; \
	stw	r3,-4(rtemp2); \
	addi	r3,rtemp2,-4; \
	stw	rtemp2,0(rtemp1); \
/* Call the C version of the function */ \
	bl	gblc(name); \
	cror	31,31,31; \
/* Save result, dereferencing if necessary */ \
	LOADADDR(rtemp3,gbl(poly_stack));  /* rt3 = &poly_stack */ \
	lwz	rtemp3,0(rtemp3);           /* rt3 =  poly_stack */ \
	res; \
	stw	r3,20(rtemp3); \
/* Restart execution of ML */ \
	b	MD_switch_to_poly1; \
	RegMask(name,M_R3|M_RR|M_R23)

/* We have to copy the body, rather than simply passing the
    parameters to CALL_IO1_LOCAL because otherwise "res" gets expanded
    prematurely, which means that the intended macro-call doesn't
    get expanded properly. SPF 9/8/95
*/
#define CALL_IO1(name, res) \
globldec(gbla(name)) \
	mflr	rr; \
	ori	rr,rr,RETURNOFFSET; \
	bl	gbl(set_registers_for_retry); \
	cror	31,31,31; /* needed? */\
/* Initialise save_vec_addr */ \
	LOADADDR(rtemp2,gbl(save_vec));       /* rt2 = &(save_vec[0]) */ \
	LOADADDR(rtemp1,gbl(save_vec_addr));  /* rt1 = &save_vec_addr */ \
/* Save parameter(s), reversing their order. */ \
	addi	rtemp2,rtemp2,4; \
	stw	r3,-4(rtemp2); \
	addi	r3,rtemp2,-4; \
	stw	rtemp2,0(rtemp1); \
/* Call the C version of the function */ \
	bl	gblc(name); \
	cror	31,31,31; \
/* Save result, dereferencing if necessary */ \
	LOADADDR(rtemp3,gbl(poly_stack));  /* rt3 = &poly_stack */ \
	lwz	rtemp3,0(rtemp3);           /* rt3 =  poly_stack */ \
	res; \
	stw	r3,20(rtemp3); \
/* Restart execution of ML */ \
	b	MD_switch_to_poly1; \
	RegMask(name,M_R3|M_RR|M_R23)

/***************************************************************************/
/* CALL_IO2                                                                */
/***************************************************************************/
#define  CALL_IO2_LOCAL(name, res) \
	mflr	rr; \
	ori	rr,rr,RETURNOFFSET; \
	bl	gbl(set_registers_for_retry); \
	cror	31,31,31; /* needed? */\
/* Initialise save_vec_addr */ \
	LOADADDR(rtemp2,gbl(save_vec));       /* rt2 = &(save_vec[0]) */ \
	LOADADDR(rtemp1,gbl(save_vec_addr));  /* rt1 = &save_vec_addr */ \
/* Save parameter(s), reversing their order. */ \
	addi	rtemp2,rtemp2,8; \
	stw	r3,-8(rtemp2); \
	addi	r3,rtemp2,-4; \
	stw	r4,-4(rtemp2); \
	addi	r4,rtemp2,-8; \
	stw	rtemp2,0(rtemp1); \
/* Call the C version of the function */ \
	bl	gblc(name); \
	cror	31,31,31; \
/* Save result, dereferencing if necessary */ \
	LOADADDR(rtemp3,gbl(poly_stack));  /* rt3 = &poly_stack */ \
	lwz	rtemp3,0(rtemp3);           /* rt3 =  poly_stack */ \
	res; \
	stw	r3,20(rtemp3); \
/* Restart execution of ML */ \
	b	MD_switch_to_poly1; \
	RegMask(name,M_R3|M_RR|M_R23)

/* We have to copy the body, rather than simply passing the
    parameters to CALL_IO2_LOCAL because otherwise "res" gets expanded
    prematurely, which means that the intended macro-call doesn't
    get expanded properly. SPF 9/8/95
*/
#define CALL_IO2(name, res) \
globldec(gbla(name)) \
	mflr	rr; \
	ori	rr,rr,RETURNOFFSET; \
	bl	gbl(set_registers_for_retry); \
	cror	31,31,31; /* needed? */\
/* Initialise save_vec_addr */ \
	LOADADDR(rtemp2,gbl(save_vec));       /* rt2 = &(save_vec[0]) */ \
	LOADADDR(rtemp1,gbl(save_vec_addr));  /* rt1 = &save_vec_addr */ \
/* Save parameter(s), reversing their order. */ \
	addi	rtemp2,rtemp2,8; \
	stw	r3,-8(rtemp2); \
	addi	r3,rtemp2,-4; \
	stw	r4,-4(rtemp2); \
	addi	r4,rtemp2,-8; \
	stw	rtemp2,0(rtemp1); \
/* Call the C version of the function */ \
	bl	gblc(name); \
	cror	31,31,31; \
/* Save result, dereferencing if necessary */ \
	LOADADDR(rtemp3,gbl(poly_stack));  /* rt3 = &poly_stack */ \
	lwz	rtemp3,0(rtemp3);           /* rt3 =  poly_stack */ \
	res; \
	stw	r3,20(rtemp3); \
/* Restart execution of ML */ \
	b	MD_switch_to_poly1; \
	RegMask(name,M_R3|M_RR|M_R23)
	
/***************************************************************************/
/* CALL_IO2U_LOCAL                                                         */
/***************************************************************************/
/* A hacked version of CALL_IO2_LOCAL, which doesn't adjust rr */

#define  CALL_IO2U_LOCAL(name, res) \
	bl	gbl(set_registers_for_retry); \
	cror	31,31,31; /* needed? */\
/* Initialise save_vec_addr */ \
	LOADADDR(rtemp2,gbl(save_vec));       /* rt2 = &(save_vec[0]) */ \
	LOADADDR(rtemp1,gbl(save_vec_addr));  /* rt1 = &save_vec_addr */ \
/* Save parameter(s), reversing their order. */ \
	addi	rtemp2,rtemp2,8; \
	stw	r3,-8(rtemp2); \
	addi	r3,rtemp2,-4; \
	stw	r4,-4(rtemp2); \
	addi	r4,rtemp2,-8; \
	stw	rtemp2,0(rtemp1); \
/* Call the C version of the function */ \
	bl	gblc(name); \
	cror	31,31,31; \
/* Save result, dereferencing if necessary */ \
	LOADADDR(rtemp3,gbl(poly_stack));  /* rt3 = &poly_stack */ \
	lwz	rtemp3,0(rtemp3);           /* rt3 =  poly_stack */ \
	res; \
	stw	r3,20(rtemp3); \
/* Restart execution of ML */ \
	b	MD_switch_to_poly1; \
	RegMask(name,M_R3|M_R23)

/***************************************************************************/
/* CALL_IO3                                                                */
/***************************************************************************/
#define  CALL_IO3_LOCAL(name, res) \
	mflr	rr; \
	ori	rr,rr,RETURNOFFSET; \
	bl	gbl(set_registers_for_retry); \
	cror	31,31,31; /* needed? */\
/* Initialise save_vec_addr */ \
	LOADADDR(rtemp2,gbl(save_vec));       /* rt2 = &(save_vec[0]) */ \
	LOADADDR(rtemp1,gbl(save_vec_addr));  /* rt1 = &save_vec_addr */ \
/* Save parameter(s), reversing their order. */ \
	addi	rtemp2,rtemp2,12; \
	stw	r3,-12(rtemp2); \
	addi	r3,rtemp2,-4; \
	stw	r4,-8(rtemp2); \
	addi	r4,rtemp2,-8; \
	stw	r5,-4(rtemp2); \
	addi	r5,rtemp2,-12; \
	stw	rtemp2,0(rtemp1); \
/* Call the C version of the function */ \
	bl	gblc(name); \
	cror	31,31,31; \
/* Save result, dereferencing if necessary */ \
	LOADADDR(rtemp3,gbl(poly_stack));  /* rt3 = &poly_stack */ \
	lwz	rtemp3,0(rtemp3);           /* rt3 =  poly_stack */ \
	res; \
	stw	r3,20(rtemp3); \
/* Restart execution of ML */ \
	b	MD_switch_to_poly1; \
	RegMask(name,M_R3|M_RR|M_R23)

/* We have to copy the body, rather than simply passing the
    parameters to CALL_IO3_LOCAL because otherwise "res" gets expanded
    prematurely, which means that the intended macro-call doesn't
    get expanded properly. SPF 9/8/95
*/
#define CALL_IO3(name, res) \
globldec(gbla(name)) \
	mflr	rr; \
	ori	rr,rr,RETURNOFFSET; \
	bl	gbl(set_registers_for_retry); \
	cror	31,31,31; /* needed? */\
/* Initialise save_vec_addr */ \
	LOADADDR(rtemp2,gbl(save_vec));       /* rt2 = &(save_vec[0]) */ \
	LOADADDR(rtemp1,gbl(save_vec_addr));  /* rt1 = &save_vec_addr */ \
/* Save parameter(s), reversing their order. */ \
	addi	rtemp2,rtemp2,12; \
	stw	r3,-12(rtemp2); \
	addi	r3,rtemp2,-4; \
	stw	r4,-8(rtemp2); \
	addi	r4,rtemp2,-8; \
	stw	r5,-4(rtemp2); \
	addi	r5,rtemp2,-12; \
	stw	rtemp2,0(rtemp1); \
/* Call the C version of the function */ \
	bl	gblc(name); \
	cror	31,31,31; \
/* Save result, dereferencing if necessary */ \
	LOADADDR(rtemp3,gbl(poly_stack));  /* rt3 = &poly_stack */ \
	lwz	rtemp3,0(rtemp3);           /* rt3 =  poly_stack */ \
	res; \
	stw	r3,20(rtemp3); \
/* Restart execution of ML */ \
	b	MD_switch_to_poly1; \
	RegMask(name,M_R3|M_RR|M_R23)

#define CALL_IO4(name, res) \
globldec(gbla(name)) \
	mflr	rr; \
	ori	rr,rr,RETURNOFFSET; \
	bl	gbl(set_registers_for_retry); \
	cror	31,31,31; /* needed? */\
/* Initialise save_vec_addr */ \
	LOADADDR(rtemp2,gbl(save_vec));       /* rt2 = &(save_vec[0]) */ \
	LOADADDR(rtemp1,gbl(save_vec_addr));  /* rt1 = &save_vec_addr */ \
/* Save parameter(s), reversing their order. */ \
	addi	rtemp2,rtemp2,16; \
	stw	r3,-16(rtemp2); \
	addi	r3,rtemp2,-4; \
	stw	r4,-12(rtemp2); \
	addi	r4,rtemp2,-8; \
	stw	r5,-8(rtemp2); \
	addi	r5,rtemp2,-12; \
	stw	r6,-4(rtemp2); \
	addi	r6,rtemp2,-16; \
	stw	rtemp2,0(rtemp1); \
/* Call the C version of the function */ \
	bl	gblc(name); \
	cror	31,31,31; \
/* Save result, dereferencing if necessary */ \
	LOADADDR(rtemp3,gbl(poly_stack));  /* rt3 = &poly_stack */ \
	lwz	rtemp3,0(rtemp3);           /* rt3 =  poly_stack */ \
	res; \
	stw	r3,20(rtemp3); \
/* Restart execution of ML */ \
	b	MD_switch_to_poly1; \
	RegMask(name,M_R3|M_RR|M_R23)

#define CALL_IO5(name, res) \
globldec(gbla(name)) \
	mflr	rr; \
	ori	rr,rr,RETURNOFFSET; \
	lwz		r7,0(rsp); /* Pop fifth arg from the stack. */ \
	addi	rsp,rsp,4; \
	bl	gbl(set_registers_for_retry); \
	cror	31,31,31; /* needed? */\
/* Initialise save_vec_addr */ \
	LOADADDR(rtemp2,gbl(save_vec));       /* rt2 = &(save_vec[0]) */ \
	LOADADDR(rtemp1,gbl(save_vec_addr));  /* rt1 = &save_vec_addr */ \
/* Save parameter(s), reversing their order. */ \
	addi	rtemp2,rtemp2,20; \
	stw	r3,-20(rtemp2); \
	addi	r3,rtemp2,-4; \
	stw	r4,-16(rtemp2); \
	addi	r4,rtemp2,-8; \
	stw	r5,-12(rtemp2); \
	addi	r5,rtemp2,-12; \
	stw 	r6,-8(rtemp2); \
	addi	r6,rtemp2,-16; \
	stw 	r7,-4(rtemp2); \
	addi	r7,rtemp2,-20; \
	stw	rtemp2,0(rtemp1); \
/* Call the C version of the function */ \
	bl	gblc(name); \
	cror	31,31,31; \
/* Save result, dereferencing if necessary */ \
	LOADADDR(rtemp3,gbl(poly_stack));  /* rt3 = &poly_stack */ \
	lwz	rtemp3,0(rtemp3);           /* rt3 =  poly_stack */ \
	res; \
	stw	r3,20(rtemp3); \
/* Restart execution of ML */ \
	b	MD_switch_to_poly1; \
	RegMask(name,M_R3|M_RR|M_R23|M_R7)


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

	CALL_IO1(objsize_,  IND)                   	/* MJC 27/04/88 */
	CALL_IO1(showsize_, IND)                   	/* MJC 09/03/89 */
	CALL_IO2(timing_dispatch_,IND)			/* DCJM 10/4/00 */
	CALL_IO0(get_dbasetime_,IND)                    /* MJC 15/09/89 */
	CALL_IO0(interrupt_console_processes_, NOIND)	/* MJC 01/08/90 */

	CALL_IO1(install_subshells_, NOIND)		/* MJC 12/09/90 */

	CALL_IO1(XWindows_, IND)			/* MJC 27/09/90 */

    CALL_IO0(full_gc_, NOIND)                       /* MJC 18/03/91 */ 
    CALL_IO0(stack_trace_, NOIND)                   /* MJC 18/03/91 */
    
    CALL_IO2(foreign_dispatch_, IND)  		/* NIC 22/04/94 */
	CALL_IO3(IO_dispatch_, IND)		  	/* DCJM 8/5/00 */
	CALL_IO2(Net_dispatch_, IND)		  	/* DCJM 22/5/00 */
	CALL_IO2(OS_spec_dispatch_, IND)		/* DCJM 22/5/00 */
	CALL_IO2(Sig_dispatch_, IND)		/* DCJM 18/7/00 */

    CALL_IO1(shrink_stack_, NOIND)  		/* SPF  1/12/96 */
	CALL_IO2(process_env_dispatch_,IND)			/* DCJM 25/4/2000 */
	CALL_IO1(get_flags_, NOIND)
	CALL_IO2(set_flags_, NOIND)			/* SPF 12/02/97 */
	CALL_IO4(set_code_constant, NOIND)		/* DCJM 11/1/2001 */

/* alloc(size, flags, initial). 
   Allocates a segment of a given size and initialises it.
   This is primarily used for arrays and for strings.  Refs are
   allocated using inline code */
INLINE_ROUTINE(alloc_store)
	mflr	rr		/* Save in case of a trap. */
 /* First check that the length is acceptable */
	andi.	r0,r3,1
	ori	rr,rr,RETURNOFFSET
	beq		raise_size
	srawi.	rtemp1,r3,TAGSHIFT		/* Remove tag */
	bne		allst0					/* (test for 0) Make zero sized objects 1 */
	li		rtemp1,1				/* because they mess up the g.c. */
	li		r3,TAGGED(1)
allst0:
	rlwinm.	r0,rtemp1,0,0,TYPESHIFT	/* Length field must fit in 24 bits */
	addi	rtemp2,rtemp1,1			/* Add 1 word for length word. */
	bne		raise_size
	slwi	rtemp2,rtemp2,2			/* Get length in bytes */
	twlt	rhl,rtemp2				/* Check that we have enough store. */
	sub		rhl,rhl,rtemp2			/* Allocate the space */
	sub		rhp,rhp,rtemp2
	ori		r4,r4,TAGGED(B_mutable)	/* Set the mutable bit in the flags. */
	rlwinm	rtemp3,r4,32-TYPESHIFT-TAGSHIFT,0,7	/* Get flags byte (untagged). */
	or		rtemp3,rtemp1,rtemp3	/* Combine flags and length word. */
	stw		rtemp3,0(rhp)			/* Store length word. */
	addi	r3,rhp,4				/* Point to first "real" word of seg. */

 /* Initialise the store. */
	cmplwi	r4,TAGGED(B_mutable|B_bytes)	/* Byte segment? */
	mr		rtemp2,r5				/* Initialiser word. */
	bne		allst1

 /* If this is a byte seg.  Set the initialiser word to 4 bytes of
    the untagged initialisation byte. */
	srawi	rtemp2,r5,TAGSHIFT
	slwi	rtemp3,rtemp2,8
	or	rtemp2,rtemp2,rtemp3
	slwi	rtemp3,rtemp3,8
	or	rtemp2,rtemp2,rtemp3
	slwi	rtemp3,rtemp3,8
	or	rtemp2,rtemp2,rtemp3

allst1:
	mtctr	rtemp1
	subi	rtemp1,r3,4				/* Start of object minus 4 bytes. */

allst2:
	stwu	rtemp2,4(rtemp1)		/* Set the word. */
	bdnz	allst2

	mtlr	rr
	/* Is this a combined closure/code segment? */
	andi.	r0,r4,shiftup(B_first_is_ptr,TAGSHIFT)
	beqlr	/* No? we're done. */

	addi	rtemp2,r3,4				/* set first word to point to second. */
	stw		rtemp2,0(r3)
	blr

raise_size:
	li	r3,TAGGED(EXC_size)
	b	raise_exa
	RegMask(alloc_store,(M_R3|M_RR|M_R4|M_R23))

/***************************************************************************/
/* Functions implemented in assembly code                                  */
/***************************************************************************/
INLINE_ROUTINE(not_bool)
	xori	r3,r3,shiftup(1,TAGSHIFT)
	blr
	RegMask(not_bool,(M_R3|M_R23))

globldec(or_bool)
INLINE_ROUTINE(or_word)
	or	r3,r3,r4
	blr
	RegMask(or_bool,(M_R3|M_R23))
	RegMask(or_word,(M_R3|M_R23))

globldec(and_bool)
INLINE_ROUTINE(and_word)
	and	r3,r3,r4
	blr
	RegMask(and_bool,(M_R3|M_R23))
	RegMask(and_word,(M_R3|M_R23))

INLINE_ROUTINE(xor_word)
	xor	rtemp1,r3,r4	/* tag bits will be equal */
	ori	r3,rtemp1,1	/* restore tag bit */
	blr
	RegMask(xor_word,(M_R3|M_R23))

INLINE_ROUTINE(shift_left_word)
/* Assume that both args are tagged integers */
	srawi	rtemp2,r4,TAGSHIFT	/* untag amount to shift */
	subi	rtemp1,r3,1		/* untag shiftee (offset by TAGSHIFT) */
	cmplwi	rtemp2,32-TAGSHIFT	/* shift too large? */
	slw	rtemp1,rtemp1,rtemp2
	ori	r3,rtemp1,1		/* restore tagbit */
	bltlrP				/* return if shift amount is OK */
	li	r3,TAGGED(0)
	blr
	RegMask(shift_left_word,(M_R3|M_R23))
	 
INLINE_ROUTINE(shift_right_word)
/* Assume that both args are tagged integers */
	srawi	rtemp2,r4,TAGSHIFT	/* untag amount to shift */
	srw	rtemp1,r3,rtemp2
	cmplwi	rtemp2,(32-TAGSHIFT)	/* shift too large? */
	rlwinm	rtemp1,rtemp1,0,0,(31-TAGSHIFT) /* remove stray bits from tag */
	ori	r3,rtemp1,1		/* restore tagbit */
	bltlrP				/* return if shift amount is OK */
	li	r3,TAGGED(0)
	blr
	RegMask(shift_right_word,(M_R3|M_R23))

INLINE_ROUTINE(shift_right_arith_word)
/* Assume that both args are tagged integers */
/* Shift right by the appropriate number of bits, preserving the sign.
   If the shift is too large return either 0 or -1. */
	srawi	rtemp2,r4,TAGSHIFT	/* untag amount to shift */
	cmplwi	rtemp2,(32-TAGSHIFT)	/* shift too large? */
	bltP	sraw1
	li	rtemp2,31		/* We just want the sign bit. */
sraw1:
	sraw	rtemp1,r3,rtemp2
	rlwinm	rtemp1,rtemp1,0,0,(31-TAGSHIFT) /* remove stray bits from tag */
	ori	r3,rtemp1,1		/* restore tagbit */
	blr
	RegMask(shift_right_arith_word,(M_R3|M_R23))
	 
/***************************************************************************/
/* Arithmetic tests on short integers.                                     */
/***************************************************************************/

#define TEST(name, cond) \
INLINE_ROUTINE(name) \
	cmpw	r3,r4; /* These are UNsigned comparisons */ \
	li	r3,TRUE; \
	b##cond##lr; /* Return TRUE if condition holds */ \
	li	r3,FALSE;  \
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
	srawi	rtemp2,r4,TAGSHIFT /* untag offset */
	add	r3,r3,rtemp2
	blr
	RegMask(offset_address,(M_R3|M_R23))

/* Get the database specific entry. */
INLINE_ROUTINE(get_dbentrya)
	LOADADDR(r3,gbl(processes))
	lwz	r3,0(r3)		/* Points to the root vector. */
	lwz	r3,8(r3)		/* Get entry */
	blr
	RegMask(get_dbentry,(M_R3|M_R23))

/* Returns the address of an entry in io_vector */
/* Should check that the argument is in range 0..255? */
/* N.B. We are replacing the following code, which untags r3,
        then adjusts it to contain a word-offset:
	srawi	r3,r3,TAGSHIFT
	slwi	r3,r3,WORDSHIFT
	
   With the less obvious code:
	rlwinm	r3,r3,(WORDSHIFT-TAGSHIFT),0,(31-WORDSHIFT)
   
   This only works properly if TAGSHIFT <= WORDSHIFT and r3 is non-negative.
*/
INLINE_ROUTINE(io_operation)
/* offset 0 entry */
	LOADADDR(rtemp1, gbl(interface_map))	/* rt1 = &(interface_map[0]) */
	rlwinm	r3,r3,(WORDSHIFT-TAGSHIFT),0,(31-WORDSHIFT)
	lwzx	r3,rtemp1,r3			/* r3 = interface_map[r3] */
	blr
	RegMask(io_operation,(M_R3|M_R23))

/* Clears the "mutable" bit on a segment */
/* Should this return unit, or its original parameter? */
INLINE_ROUTINE(locksega)
	lbz 	rtemp1,-4(r3)
	andi.	rtemp2,rtemp1,(255-B_mutable) /* Reset MUTABLE bit */
	stb     rtemp2,-4(r3)
	blr
	RegMask(lockseg,(M_R3|M_R23))


INLINE_ROUTINE(get_length_a)
	lwz	rtemp1,-4(r3)		/* get length word */
        /* get 24 bits of length, and shift up by TAGSHIFT */
	rlwinm	rtemp1,rtemp1,TAGSHIFT,(TYPESHIFT-TAGSHIFT),(31-TAGSHIFT)
	ori	r3,rtemp1,1		/* return result as a tagged integer */
	blr
	RegMask(get_length,(M_R3|M_R23))

/***************************************************************************/
/* test_string - basic string comparison utility function                  */
/***************************************************************************/

/* Compare two strings; returns with condition codes set appropriately. */
/* Corrupts r3,  r4, r0, rtemp1, rtemp2, rtemp3. */
test_string:
	/* Is arg1 a single character? */
	andi.	r0,r3,1
	beq	test_string2
	
	/* arg1 is a single character - is arg2? */
	andi.	r0,r4,1
	beq	test_string1
	
	/* Both are single characters - just compare them */
	cmpw	r3,r4
	blr
	
test_string1:
	/* arg1 is a single character, but arg2 isn't. */
	/* Is arg2 a null string? - return "GT" if 1 > length(arg2). */ 
	lwz	rtemp2,0(r4)
	li	rtemp1,1
	cmpw	rtemp1,rtemp2
	bgtlr
	
	/* Compare arg1 with the first byte of arg2. 
	   If the bytes differ, that's the result we want. */
	lbz	rtemp2,4(r4)
	srwi	rtemp1,r3,TAGSHIFT
	cmpw	rtemp1,rtemp2
	bnelr
	
	/* If the bytes are equal, arg1 < arg2, so set CR accordingly. */
	li	rtemp1,-1
	cmpwi	rtemp1,0
	blr
	
test_string2:
	/* arg1 is not a single character - is arg2? */
	andi.	r0,r4,1
	beq	test_string3
	
	/* arg1 is not a single character, but arg2 is.
	   Is arg1 a null string? - return "LT" if length(arg1) < 1. */
	lwz	rtemp1,0(r3)
	cmpwi	rtemp1,1
	bltlr

	/* Compare first byte of arg1 with arg2. 
	   If the bytes differ, that's the result we want. */
	lbz	rtemp1,4(r3)
	srwi	rtemp2,r4,TAGSHIFT
	cmpw	rtemp1,rtemp2
	bnelr
	
	/* If the bytes are equal, arg1 > arg2, so set CR accordingly. */
	li	rtemp1,1
	cmpwi	rtemp1,0
	blr


/* A is greater than B if, at the first position at which A and B differ,
   A[i] > B[i] or if the end of B is found before they differ.
   Set rtemp1 to the shorter length and rtemp3 to length(A) - length(B)
*/
test_string3:
	/* Neither string is a single character */
	lwz	rtemp1,0(r3)
	lwz	rtemp2,0(r4)
	
	subfc.	rtemp3,rtemp2,rtemp1 /* rt3 = length(A) - length(B) */
	ble	test_string4         /* done if length(A) <= length(B) */
	subfc	rtemp1,rtemp3,rtemp1 /* otherwise shorten rtemp1 */
	
test_string4:	
	/* round-up byte count to word-count */
	addi	rtemp1,rtemp1,3
	srwi	rtemp1,rtemp1,2

	/* have to treat length 0 as special case (it's a "repeat" loop) */
	cmpwi	rtemp1,0
	beq	test_string6
	mtctr	rtemp1

	/* Since we have a big-endian machine, we can do the
	   comparison a whole word at a time. This assumes
	   that strings are zero-padded (is this true?). */
test_string5:	
	lwzu	rtemp1,4(r3)
	lwzu	rtemp2,4(r4)
	cmplw	rtemp1,rtemp2	/* UNSIGNED comparison needed */
/* DCJM: Formerly bdneq. */
 	bdnzt	eq,test_string5	/* loop while counter > 0 and words are equal */
test_string6:
	/* Here we have 2 possibilities for the loop exit:
	    (1) rtemp1 <> rtemp2
	    (2) rtemp1 = rtemp2, and we've examined all of the common prefix
	*/

	li	r4,TAGGED(0)	    /* zap r4 */
	li	r3,TAGGED(0)	    /* zap r3 */

	/* case (1) - rtemp1 <> rtemp2 
	   Just return - the condition code is already set appropriately. */
	bnelr
	
	/* case (2) - the common prefix is equal. in this case,
	   the result depends on which string is the longer.
	   The result of the comparison is, in fact, the same
	   as the comparison between the lengths, which is the same
	   as comparing (length(A) - length(B)) to 0.  */
	cmpwi	rtemp3,0 
	blr
	
/***************************************************************************/
/* String comparison functions                                            */
/***************************************************************************/

#define STRINGTEST(name, cond) \
INLINE_ROUTINE(name) \
	mflr	rr; \
	ori	rr,rr,RETURNOFFSET; \
	bl	test_string; \
	cror	31,31,31; /* needed? */\
	mtlr	rr; \
	li	r3,TRUE; \
	b##cond##lr; \
	li	r3,FALSE; \
	blr; \
	RegMask(name,(M_R3|M_R4|M_R23|M_RR))


STRINGTEST(teststrgeq,ge)
STRINGTEST(teststrleq,le)
STRINGTEST(teststrlss,lt)
STRINGTEST(teststrgtr,gt)
STRINGTEST(teststrneq,ne)
STRINGTEST(teststreq,eq)

INLINE_ROUTINE(str_compare)
    mflr	rr
    ori		rr,rr,RETURNOFFSET
    bl		test_string
    cror	31,31,31 /* needed? */
    mtlr	rr
    li		r3,TAGGED(1)
    bgtlr
    li		r3,TAGGED(0)
    beqlr
    li		r3,TAGGED(-1)
    blr
    RegMask(str_compare, (M_R3|M_R4|M_R23|M_RR))

/***************************************************************************/
/* String equality functions                                               */
/***************************************************************************/

#if 0
/* Replaced by the more general code above */

INLINE_ROUTINE(teststrneq)
        /* "equal" means "return FALSE" */
	li	r0,FALSE
	b	teststreq0	

/* The same as the above, except for the return values! */
INLINE_ROUTINE(teststreq)
        /* "equal" means "return TRUE" */
	li	r0,TRUE
	b

teststreq0:
	/* are the strings identical? */
	cmpw	r3,r4
	beq	teststreq_equal
	
	/* return if either is a single char */
	or	r0,r3,r4
	andi.	r0,r0,1
	bne	teststreq_unequal
	
	/* We have two "long" strings - return if length words differ */
	lwz	rtemp1,-4(r3)
	lwz	rtemp2,-4(r4)
	cmpw	rtemp1,rtemp2
	bne	teststreq_unequal
	
	/* remove flags from length word; convert to byte offset */
	rlwinm	rtemp1,rtemp1,WORDSHIFT,(TYPESHIFT-WORDSHIFT),(31-WORDSHIFT)

teststreq1:
	/* The main loop.
	   rtemp1 has the byte offset of the last word we checked */
	addic.	rtemp1,rtemp1,-4
	blt	teststreq_equal	/* return if we've checked all the words */
	
	lwzx	rtemp2,r3,rtemp1
	lwzx	rtemp3,r4,rtemp1
	cmp 	rtemp2,rtemp3
	beq	teststreq1
	
teststreq_unequal:
	/* We've found a difference, so invert the result */
	xori	r3,r0,upshift(1,TAGSHIFT)
	blr
	
teststreq_equal:
	/* The strings are equal, so don't invert the result */
	mr	r3,r0
	blr
	
#endif /* specialised string equality code */

/***************************************************************************/
/* String operations - string_sub                                          */
/*    Index 1 means the first character, but that's actually stored at     */
/*    position 4 (because of the string-length word), so we have to adjust */
/*    the offset.   SPF 30/8/95                                            */
/***************************************************************************/
INLINE_ROUTINE(string_sub)
	andi.	r0,r3,1			/* single char? */
	srawi	rtemp1,r4,TAGSHIFT	/* untag index */
	bne	string_sub_single_char
	
	/* arg1 is not a single character */
	cmpwi	rtemp1,0
	lwz	rtemp2,0(r3)		/* get string-length */
	ble	raise_substring		/* exception if index <= 0 */
	
	cmpw	rtemp1,rtemp2		/* check index against string-length */
	addi	rtemp1,rtemp1,(4-1)	/* adjust the offset */
	bgt	raise_substring		/* exception if length < index */
	
	lbzx	rtemp1,r3,rtemp1	/* fetch the byte */
	slwi	rtemp1,rtemp1,TAGSHIFT
	ori	r3,rtemp1,1		/* tag it */
	blr				/* and return */

string_sub_single_char:
	/* arg1 is a single character - index should be 1.
	   If it is, we just return the character unchanged. */
	cmpwi	rtemp1,1
	beqlr
	/* Otherwise, fall through to raise exception */

raise_substring:
	li	r3,TAGGED(EXC_subscript)
	b	raise_exa

	RegMask(string_sub,(M_R3|M_R23))


/***************************************************************************/
/* Function application - callcode_tupled                                  */
/***************************************************************************/

/* 
   Calls a procedure with a given argument vector. The argument vector must
   be copied onto the stack. If the argument vector is nil then there are no
   arguments. One difference between "callcode_tupled" and "callcode" is that
   "callcode_tupled" expects a single parameter which should be an ML pair of
   the function closure and the argument tuple, whereas "callcode" expects
   the closure and the argument tuple to be already in registers. The
   other difference is that "callcode" extracts the arguments from the
   tuple *backwards*, for reasons that I don't understand.

   This routine contains a stack-limit check - but what happens if this
   causes a GC? We have to be careful because RTS traps corrupt CTR
   (it's used to return to the interrupted code) and possibly also
   (I'm not sure yet) corrupt LR. We must also be sure that none
   of the untagged registers (rt1, rt2, rt3) contain valuable
   pointers when we take the trap because they wouldn't get updated
   by the garbage-collector. That's why we use r23 (rather than rtemp3,
   say) to hold the arg-vec address in the code below.
*/
INLINE_ROUTINE(callcode_tupled)
	lwz	r23,4(r3)		/* r23 = arg-vec address */
	mflr	rr
	lwz	r24,0(r3)		/* r24 = closure address */
	cmpwi	r23,UNIT		/* Is the argument "()"? */
	ori	rr,rr,RETURNOFFSET
	beq	clcdt9			/* Skip if it is */
	
	lwz	rtemp2,-4(r23)		/* rt2 = length word of arg-vec */
	rlwinm	rtemp2,rtemp2,0,TYPESHIFT,31	/* remove flag byte to get      */
	mtctr	rtemp2			/* argument count (will be > 0) */
	
	/* Load first 4 parameters into registers */
	lwz	r3,0(r23)
	bdz	clcdt9			/* branch if this was the last */
	lwz	r4,4(r23)
	bdz	clcdt9
	lwz	r5,8(r23)
	bdz	clcdt9
	lwz	r6,12(r23)
	bdz	clcdt9
	
	/* set rt2 to be the number (non-zero) of "stack" arguments */
	subi	rtemp2,rtemp2,4

	/* Make a conservative estimation of our stack usage. */
	subi	rtemp1,rsp,MINSTACK
	slwi	rtemp3,rtemp2,WORDSHIFT	/* rt3 = bytes for arguments  */
	subfc	rtemp1,rtemp3,rtemp1	/* rt1 = required stack floor */
	
	/* 
	   The (potential) trap itself - this must be a "safe" state.
	   r3-r6 will contain the first 4 (properly tagged) arguments
	   r23 will point to the argument vector
	   r24 will point to the closure for the function
	   rr  will contain our return address
	   Other "tagged" registers will be unaltered 
	   rt1 will contain the required stack floor
	   rt2 will contain the total number of :stack" parameters
	*/
	twllt	rtemp1,rsl

	/* ctr may be invalidated by the trap, so reload it */
	mtctr	rtemp2

	/* point rt1 at the last argument loaded */
	addi	rtemp1,r23,12
	
	
	/*
	   The standard ML convention is that arguments are pushed
	   onto the stack in the order that they're evaluated
	   (apart from the first 4 arguments, which are passed in
	   registers). That means that we have to push arguments
	   onto the stack in the same order that we "pop" them
	   from the tuple.
	*/
clcdt8:
	lwzu	rtemp3,4(rtemp1)	/* get next parameter */
	stwu	rtemp3,-4(rsp)		/* push it onto stack */
	bdnz	clcdt8			/* loop if it wasn't the last */
	/* 
	   When we get here:
	   r3-r6 will contain the first 4 (properly tagged) arguments
	   r23 will point to the argument vector (or be UNIT)
	   r24 will point to the closure for the function
	   rr  will contain our return address
	   The remaining arguments have been pushed on the stack 
	   Other "tagged" registers will be unaltered
	*/
clcdt9:	
	/* We've set up the arguments - tail-call the function */
	lwz	r23,0(r24)		/* get code address */
	mtctr	r23
	mtlr	rr
	bctr

	RegMask(callcode_tupled,Mask_all) /* Calls an unknown function. */

/***************************************************************************/
/* Exception handling                                                      */
/***************************************************************************/


/* Loop to find the handler for this exception. Handlers consist of one or more
   pairs of identifier and code address, followed by the address of the next
   handler.
*/
INLINE_ROUTINE(raisex)
	LOADADDR(rtemp3,gbl(end_of_stack))	/* rt3 = &end_of_stack */
	lwz	r0,0(r3)			/* r0  = exception id  */
	mr	rtemp1,rhr			/* rt1 = handler ptr   */
	lwz	rtemp2,0(rhr)			/* rt2 = handler id    */
	lwz	rtemp3,0(rtemp3)		/* rt3 = end_of_stack  */

rsx1:
	cmplwi	rtemp2,TAGGED(0)	/* Is it zero (or TAGGED(0))? */
	ble	rsx7			/* If so, we have a default handler */
	/* non-default handler */
	cmpw	rtemp2,r0		/* Does it match the exception id? */
	beq	rsx7
	/* This handler doesn't match - try the next one.
	   This can be either a genuine handler pair, or a
	   pointer up the stack. */
	lwzu	rtemp2,8(rtemp1)	/* rt2 = next handler id */
	cmplw	rtemp2,rtemp1
	blt	rsx1			/* Not a stack pointer (too small) */
	cmplw	rtemp2,rtemp3
	bge	rsx1			/* Not a stack pointer (too big)  */
	
	/* It's a stack pointer - get the next batch of handlers */
	mr	rtemp1,rtemp2		/* rt1 = new handler ptr */
	lwz	rtemp2,0(rtemp2)	/* rt2 = next handler id */
	b	rsx1
	
rsx7:
	/* We've found a handler that matches; rtemp1 points at the id */
	lwz	rr,4(rtemp1)		/* Get the handler entry point */

rsx6:
	/* Remove the other handlers in this group. */
	lwzu	rtemp2,8(rtemp1)	/* Get next handler id */
	cmplw	rtemp2,rtemp1
	blt	rsx6			/* Not a stack pointer (too small) */
	cmplw	rtemp2,rtemp3
	bge	rsx6			/* Not a stack pointer (too big)  */
	/* rtemp1 now points at the pointer to the next group of handlers
	   i.e. the old (saved) value of the handler register
	   and rtemp2 contains the pointer itself */

	/* Is this handler a real one, or was it set by exception_trace? */
	cmplwi	rr,TAGGED(0)
	bgtP	rsx9
	
	/* We've found a handler set by exception_trace.
	    Push the return address onto the stack, so that
	    it will be printed by ex_tracec, load a dummy value
	    into rr, then call ex_tracec (which doesn't return).
	*/
	mflr	rr		/* get return address */
	mr	r4,r3		/* exception packet is arg2 */
	ori	rr,rr,2		/* tag return address */
	mr	r3,rtemp1	/* stack-mark is arg1 */
	stwu	rr,-4(rsp)	/* save return address */
	li	rr,TAGGED(1)    /* a special marker */
	CALL_IO2U_LOCAL(ex_trace, NOIND)
rsx9:	
	/* Ordinary exception handler */
	mtlr	rr		/* "Return" to handler */
	mr	rhr,rtemp2	/* Reload rhr from saved value */
	addi	rsp,rtemp1,4	/* Pop stack back past saved rhr */
	blr

	/* Used to raise exceptions in the assembly code below.
	   Note: raise_exc is NOT referenced by interface_map;
	   compiled code goes via raisex (above) instead.
	*/
raise_exa:
	CALL_IO1_LOCAL(raise_ex, NOIND)

/***************************************************************************/
/* Exception tracing                                                       */
/***************************************************************************/
/* Calls a procedure with no arguments and, if it returns normally, returns
   its result. If the procedure raises an exception it prints a trace of the
   stack from the place where the exception was first raised.

   The values on the stack must not point directly into this assembly
   code segment otherwise there would be problems if we wrote out the stack.
   Instead we indirect through the interface map.
*/
INLINE_ROUTINE(exception_tracea)
	mflr	rr
	li	r0,TAGGED(0)    /* changed (was 0) SPF 28/9/95 */
	ori	rr,rr,RETURNOFFSET
	
	stw	rr,-4(rsp)	/* Push return address */
	stw	rhr,-8(rsp)	/* Push old handler    */
	stw	r0,-12(rsp)	/* Push dummy handler address */
	stwu	r0,-16(rsp)	/* Push dummy exception id.   */
	
	mr	r24,r3		/* Get argument (the procedure) */
	mr	rhr,rsp		/* rhr now points here */
	lwz	r23,0(r24)	/* Get code address */

	/* tail-call the procedure, returning to return_code */
	LOADADDR(rtemp3,gbl(interface_map)) /* rt3 = &interface_map */
	mtctr	r23
	lwz	rr,(POLY_SYS_return*4)(rtemp3)
	ori	rr,rr,RETURNOFFSET /* make it into a proper return address */
	li	r3,UNIT
	mtlr	rr		/* Give the procedure a unit argument */
	bctr

	RegMask(exception_trace,Mask_all) /* Calls an unknown function. */

/* If the traced procedure didn't raise an exception, we end up here. */
/* This is NOT a function entry point, so we don't need INLINE_ROUTINE */
globldec(return_code)
	lwz	rr,12(rsp)	/* Restore old return address */
	lwz	rhr,8(rsp)	/* Restore old handler */
	mtlr	rr
	addi	rsp,rsp,16	/* Pop stack */
	blr			/* Return to exception_trace's caller */

/* Question: if the handler is executed, what removes the
             return address from the stack?
   Answer: after the dummy handler has been processed, generating
           a stack trace, the system processes the real handler,
           which chops the stack further, removing (at least)
           the return address.
*/

/***************************************************************************/
/* Arbitrary-precision arithmetic                                          */
/***************************************************************************/

/* Problem: what happens if one of these instructions traps,
  and the emulation code causes a garbage-collection? We'll end
  up with the PC pointing into something that's not a code
  segment. We avoid this by using explicit tests and calls
  to the emulation code. */

INLINE_ROUTINE(neg_long)
	andi.	r0,r3,1
	li	rtemp1,TAGGED(0)
	beqM	neg_long1	/* emulate if argument is long */
	
	mcrxr	0		/* reset XER overflow state */
	subfco.	rtemp2,r3,rtemp1
	bsoM	neg_long1	/* emulate if result overflows */
		
	addi	r3,rtemp2,1	/* restore tag bit */
	blr
neg_long1:
	CALL_IO1_LOCAL(neg_long, IND)

	RegMask (aneg,(M_R3|M_R23|Mask_neg_long))
	
/***************************************************************************/
INLINE_ROUTINE(add_long)
	and	r0,r3,r4
	andi.	r0,r0,1
	beqM	add_long1	/* emulate if either argument is long */

	mcrxr	0		/* reset XER overflow state */
	addo.	rtemp2,r3,r4
	bsoM	add_long1	/* emulate if result overflows */
	subi	r3,rtemp2,1	/* restore tag bit */
	blr
add_long1:
	CALL_IO2_LOCAL(add_long, IND)

	RegMask (aplus,(M_R3|M_R23|Mask_add_long))
	
/***************************************************************************/
INLINE_ROUTINE(sub_long)
	and	r0,r3,r4
	andi.	r0,r0,1
	beqM	sub_long1	/* emulate if either argument is long */

	mcrxr	0		/* reset XER overflow state */
	subfco.	rtemp2,r4,r3
	bsoM	sub_long1	/* emulate if result overflows */
	addi	r3,rtemp2,1	/* restore tag bit */
	blr
sub_long1:
	CALL_IO2_LOCAL(sub_long, IND)

	RegMask (aminus,(M_R3|M_R23|Mask_sub_long))

/***************************************************************************/
INLINE_ROUTINE(mult_long)
	and	r0,r3,r4
	mr	rtemp1,r3        /* save r3 */
	andi.	r0,r0,1
	mr	rtemp2,r4        /* save r4 */
	beq	mult_really_long

	/* Set in_run_time_system, to protect the following code
	   from interrupts (otherwise the SIGALRM handler could
	   attempt change rsl, which could greatly upset the
	   .__mull milicode routine). SPF 13/10/95  */
	LOADADDR(rtemp3,gbl(in_run_time_system))
	li	r0,1
	stw	r0,0(rtemp3)
		
#ifndef AIX
	srawi	r3,r3,TAGSHIFT	/* Untag one argument. */
	subi	r4,r4,TAGGED(0) /* Remove tag, but don't shift */
	/* This assumes that the machine supports the multiply instr. */
	mullwo.	r3,r3,r4
	bsoM	mult_really_long2
#else
	/* save LR, untag, multiple, restore LR */
	srawi	r3,r3,TAGSHIFT
	srawi	r4,r4,TAGSHIFT
	mflr	rtemp3
	bla	.__mull /* corrupts r0 and LR */
	mtlr	rtemp3
	/* r3 contains high-order 32 bits, r4 contains low-order 32 bits */

	/* 
	   The top 34 bits should be the same as the sign bit, otherwise
	   we've got overflow. We check that r3 is either 0 or -1, and 
	   that sign-extending the top 3 bits of r4 is the same as r3.
	*/ 
        srawi	rtemp3,r4,31-TAGSHIFT
	cmpwi	7,r3,0
	cmpwi	6,r3,-1
        cmpw	0,r3,rtemp3
	cror	4*5+2,4*6+2,4*7+2  /* combine EQ bits of CR6 and CR7 */
	bne	0,mult_really_long2
	bne	5,mult_really_long2
        /* we've got a short result, so tag it, zap r4 and return */
	rlwinm	r3,r4,TAGSHIFT,0,31-TAGSHIFT
#endif
	li	r4,TAGGED(0)
	ori	r3,r3,1

	/* clear in_run_time_system */
	LOADADDR(rtemp3,gbl(in_run_time_system))
	li	r0,0
	stw	r0,0(rtemp3)

	blr

mult_really_long2:
	/* restore r3 and r4, then call the C routine */
	mr	r3,rtemp1
	mr	r4,rtemp2

mult_really_long:
	/* call the (slow) C multiplication routine */
	CALL_IO2_LOCAL(mult_long, IND)

	RegMask (amul,(M_R3|M_R4|M_R23|Mask_mult_long))
	
/***************************************************************************/
INLINE_ROUTINE(div_long)
	/* check for long arguments */
	and	r0,r3,r4
	andi.	r0,r0,1
	beqM	div_really_long

	/* Check for division by zero */
	cmpwi	r4,TAGGED(0)
	beqM 	div_by_zero
	
	/* check for division of MININT by -1 */
	cmpwi	r4,TAGGED(-1)
	bneP    div_long1
	rlwinm	r0,r3,1,0,31
	/* If we rotate MININT 1 place left, this is what we get. */
	cmpwi	r0,(shiftup(1,1) | 1) /* tag bit plus rotated sign bit */
	beq	div_really_long

div_long1:
	/* Set in_run_time_system, to protect the following code
	   from interrupts (otherwise the SIGALRM handler could
	   attempt change rsl, which could greatly upset the
	   .__divss milicode routine). SPF 13/10/95  */
	LOADADDR(rtemp3,gbl(in_run_time_system))
	li	r0,1
	stw	r0,0(rtemp3)
		
	/* untag the values into r3 and r4 */
	srawi	r3,r3,TAGSHIFT
	srawi	r4,r4,TAGSHIFT

#ifndef AIX
	divw	r3,r3,r4
#else
	mflr	rtemp3
	bla	.__divss      /* corrupts r0 and LR */
	mtlr	rtemp3

#endif
        /* r3, r4 now contain the 32 bit quotient and remainder respectively */
	slwi	r3,r3,TAGSHIFT  /* result is the quotient */
	li	r4,TAGGED(0)	/* zap r4 */
	ori	r3,r3,1		/* tag result */

	/* clear in_run_time_system */
	LOADADDR(rtemp3,gbl(in_run_time_system))
	li	r0,0
	stw	r0,0(rtemp3)

	blr

div_really_long:
	CALL_IO2_LOCAL(div_long, IND)

div_by_zero:
	/* Division by 0 */
	li	r4,TAGGED(0)	/* zap r4 */
	li	r3,TAGGED(EXC_divide)	
	b	raise_exa

	RegMask (adiv,(M_R3|M_R4|M_R23|Mask_div_long))

	
/***************************************************************************/
INLINE_ROUTINE(rem_long)
	/* check for long arguments */
	and	r0,r3,r4
	andi.	r0,r0,1
#ifdef AIX
	beq	rem_really_long

	/* Check for division by zero */
	cmpwi	r4,TAGGED(0)
	beq 	div_by_zero
	
	/* check for division of MININT by -1 */
	cmpwi	r4,TAGGED(-1)
	bneP    rem_long1
	rlwinm	r0,r3,1,0,31
	/* If we rotate MININT 1 place left, this is what we get. */
	cmpwi	r0,(shiftup(1,1) | 1) /* tag bit plus rotated sign bit */
	beq	rem_really_long

rem_long1:
	/* Set in_run_time_system, to protect the following code
	   from interrupts (otherwise the SIGALRM handler could
	   attempt change rsl, which could greatly upset the
	   .__divss milicode routine). SPF 13/10/95  */
	LOADADDR(rtemp3,gbl(in_run_time_system))
	li	r0,1
	stw	r0,0(rtemp3)
		
	/* untag the values into r3 and r4 */
	srawi	r3,r3,TAGSHIFT
	srawi	r4,r4,TAGSHIFT
	
	mflr	rtemp3
	bla	.__divss      /* corrupts r0 and LR */
	mtlr	rtemp3

        /* r3, r4 now contain the 32 bit quotient and remainder respectively */
	slwi	r3,r4,TAGSHIFT  /* result is the remainder */
	li	r4,TAGGED(0)	/* zap r4 */
	ori	r3,r3,1		/* tag result */

	/* clear in_run_time_system */
	LOADADDR(rtemp3,gbl(in_run_time_system))
	li	r0,0
	stw	r0,0(rtemp3)

	blr
#endif
	
rem_really_long:
	CALL_IO2_LOCAL(rem_long, IND)

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
#define ARBTEST(name, name2, maskname, cond, combine) \
INLINE_ROUTINE(name) \
	combine	rtemp1,r3,r4; \
	andi.	rtemp1,rtemp1,1; \
	beqM	name##_really_long; /* at least one argument is long */ \
	cmpw	r3,r4; \
	li	r3,TRUE; \
	b##cond##lr; /* Return TRUE if condition holds */ \
	li	r3,FALSE; \
	blr;         /* Return FALSE otherwise */ \
name##_really_long: \
	CALL_IO2_LOCAL(name2, NOIND) ; \
	RegMask(maskname,(M_R3|M_R23|Mask_##name2))


	ARBTEST(equal_long, equal_long, equala,  eq, or)
	ARBTEST(int_geq,    ge_long,    int_geq, ge, and)
	ARBTEST(int_leq,    le_long,    int_leq, le, and)
	ARBTEST(int_gtr,    gt_long,    int_gtr, gt, and)
	ARBTEST(int_lss,    ls_long,    int_lss, lt, and)

INLINE_ROUTINE(or_long)
	and	r0,r3,r4
	andi.	r0,r0,1
	beqM	or_really_long	/* emulate if either argument is long */

	or	r3,r3,r4
	blr

or_really_long:
	CALL_IO2(or_long, IND)

	RegMask (ora,(M_R3|M_R23|Mask_or_long))

INLINE_ROUTINE(and_long)
	and	r0,r3,r4
	andi.	r0,r0,1
	beqM	and_really_long	/* emulate if either argument is long */

	and	r3,r3,r4
	blr

and_really_long:
	CALL_IO2(and_long, IND)

	RegMask (anda,(M_R3|M_R23|Mask_and_long))


INLINE_ROUTINE(xor_long)
	and	r0,r3,r4
	andi.	r0,r0,1
	beqM	xor_really_long	/* emulate if either argument is long */

	xor	rtemp1,r3,r4	/* tag bits will be equal */
	ori	r3,rtemp1,1	/* restore tag bit */
	blr

xor_really_long:
	CALL_IO2(xor_long, IND)

	RegMask (xora,(M_R3|M_R23|Mask_xor_long))

/***************************************************************************/
/* Loads and Stores                                                        */
/***************************************************************************/

INLINE_ROUTINE(load_byte)
	srawi	rtemp1,r4,TAGSHIFT	/* untag to byte offset */
	lbzx	rtemp1,r3,rtemp1	/* fetch byte */
	slwi	rtemp1,rtemp1,TAGSHIFT
	addi	r3,rtemp1,1	/* return result as a tagged integer */
	blr
	RegMask (load_byte,(M_R3|M_R23))

INLINE_ROUTINE(load_word)
	rlwinm	rtemp1,r4,2-TAGSHIFT,0,29 /* untag to word offset, masking off shifted tag */
	lwzx	r3,r3,rtemp1	/* fetch (tagged) word */
	blr
	RegMask (load_word,(M_R3|M_R23))

/* We can do the assignment in-line if r3 points into the local
   mutable area; otherwise we have to call C (which may have
   to set the modified bit for a database page, and so on).
*/
/*
   It is no longer strictly necessary to check for addresses in 
   the database area since this area is write-protected and assignments
   to it will trap.  N.B. If the explicit checks are removed it is
   essential to save the return address before the assignment since
   emulating the trap will corrupt the link register. DCJM 16/11/00.
*/
INLINE_ROUTINE(assign_byte)
	LOADADDR(rtemp3,gbl(A))		/* rt3 = &A */
	lwz	rtemp2,localMbottom(rtemp3)	/* rt2 = A.M.bottom */
	lwz	rtemp3,localMtop(rtemp3)	/* rt3 = A.M.top */
	
	cmplw	r3,rtemp2 
	srawi	rtemp1,r4,TAGSHIFT		/* rt1 = untagged byte-offset */
	blt	assign_byte_long		/* below mutable area */
	
	cmplw	r3,rtemp3 
	srawi	rtemp2,r5,TAGSHIFT		/* rt2 = untagged byte */
	bge	assign_byte_long		/* above mutable area */
	
	/* r3 points into the local mutable area */
	stbx	rtemp2,r3,rtemp1
	li	r3,UNIT				/* result is always "()" */
	blr
	
assign_byte_long:
	CALL_IO3_LOCAL(assign_byte_long_, NOIND)

	RegMask (assign_byte,(M_R3|M_R23|Mask_assign_byte_long_))

/* We can do the assignment in-line if r3 points into the local
   mutable area; otherwise we have to call C (which may have
   to set the modified bit for a database page, and so on).
*/
/*
   It is no longer strictly necessary to check for addresses in 
   the database area since this area is write-protected and assignments
   to it will trap.  N.B. If the explicit checks are removed it is
   essential to save the return address before the assignment since
   emulating the trap will corrupt the link register. DCJM 16/11/00.
*/
INLINE_ROUTINE(assign_word)
	LOADADDR(rtemp3,gbl(A))		/* rt3 = &A */
	lwz	rtemp2,localMbottom(rtemp3)	/* rt2 = A.M.bottom */
	lwz	rtemp3,localMtop(rtemp3)	/* rt3 = A.M.top */
	
	cmplw	r3,rtemp2 
	blt	assign_word_long		/* below mutable area */
	
	cmplw	r3,rtemp3 
	rlwinm	rtemp1,r4,2-TAGSHIFT,0,29	/* rt1 = untagged word-offset */
	bge	assign_word_long		/* above mutable area */
	
	/* r3 points into the local mutable area */
	stwx	r5,r3,rtemp1
	li	r3,UNIT				/* result is always "()" */
	blr

assign_word_long:
	CALL_IO3_LOCAL(assign_word_long_, NOIND)

	RegMask (assign_word,(M_R3|M_R23|Mask_assign_word_long_))

/***************************************************************************/
/* Miscellaneous                                                           */
/***************************************************************************/

INLINE_ROUTINE(is_shorta)
        /* Move tag bit into LS digit position */
	rlwinm	rtemp1,r3,TAGSHIFT,(31-TAGSHIFT),(31-TAGSHIFT)
	ori	r3,rtemp1,1	/* return result as a tagged integer */
	blr
	RegMask (is_short,(M_R3|M_R23))

/* Single character strings are represented as shorts */
INLINE_ROUTINE(string_length)
	andi.	rtemp1,r3,1	/* Is it a short? (Set CR0) */
	beq	sl1

/* a single character */
	li	r3,TAGGED(1)
	blr

/* not a single character */
sl1:	
	lwz	rtemp1,0(r3)		/* Get string length (in bytes) */
	slwi	rtemp1,rtemp1,TAGSHIFT	/* Return tagged length */
	addi	r3,rtemp1,1
	blr

	RegMask (string_length,(M_R3|M_R23))

/* Store the length of a string in the first word. */
INLINE_ROUTINE(set_string_length_a)
	srwi	rtemp1,r4,TAGSHIFT	/* Untag the length */
	stw	rtemp1,0(r3)
	li	r3,UNIT				/* Return unit */
	blr
	RegMask (set_string_length,(M_R3|M_R23))


	CALL_IO2(strconcat, IND)
	CALL_IO1(set_dbentry_, NOIND)
	CALL_IO0(BadOpCode_, NOIND)

INLINE_ROUTINE(is_big_endian)
	li	r3,TRUE
	blr
	RegMask (is_big_endian,(M_R3|M_R23))

INLINE_ROUTINE(bytes_per_word)
	li	r3,TAGGED(4)
	blr
	RegMask (bytes_per_word,(M_R3|M_R23))

INLINE_ROUTINE(move_bytes)
	LOADADDR(rtemp3,gbl(A))		/* rt3 = &A */
	lwz	rtemp2,localMbottom(rtemp3)	/* rt2 = A.M.bottom */
	lwz	rtemp3,localMtop(rtemp3)	/* rt3 = A.M.top */
	
	cmplw	r5,rtemp2 
	blt	MB3		/* below mutable area */
	
	cmplw	r5,rtemp3 
	bge	MB3		/* above mutable area */

	srawi	rtemp1,r4,TAGSHIFT		/* rt1 = untagged source offset */
	srawi	rtemp2,r6,TAGSHIFT		/* rt2 = untagged dest offset */
	lwz 	rtemp3,0(rsp)			/* rt3 = number of bytes to move. */
	add 	rtemp1,r3,rtemp1		/* Source address. */
	add 	rtemp2,r5,rtemp2		/* Destination address. */
	srawi.	rtemp3,rtemp3,TAGSHIFT
	mtctr	rtemp3
	beq 	MB2
	cmplw	rtemp1,rtemp2			/* If the source < destination use decrementing move. */
	blt 	MB4
	addi	rtemp1,rtemp1,-1		/* else use incrementing move. */
	addi	rtemp2,rtemp2,-1
MB1:
	lbzu	rtemp3,1(rtemp1)
	stbu	rtemp3,1(rtemp2)
	bdnz	MB1
	
MB2:	li	r3,TAGGED(0)
	addi	rsp,rsp,4			/* Pop last argument. */
	blr

MB4:
	add 	rtemp1,rtemp3,rtemp1
	add 	rtemp2,rtemp3,rtemp2
MB5:
	lbzu	rtemp3,-1(rtemp1)
	stbu	rtemp3,-1(rtemp2)
	bdnz	MB5

	li  	r3,TAGGED(0)
	addi	rsp,rsp,4			/* Pop last argument. */
	blr


MB3:
	CALL_IO5(move_bytes_long_,NOIND)
	RegMask (move_bytes,(M_R3|M_R23|Mask_move_bytes_long_))


INLINE_ROUTINE(move_words)
/* TODO: Write this in assembly code as well. */
	CALL_IO5(move_words_long_,NOIND)
	RegMask (move_words,(M_R3|M_R23|Mask_move_words_long_))

/* Word functions.  These are all unsigned and do not raise Overflow */

INLINE_ROUTINE(mul_word)
	srawi	r3,r3,TAGSHIFT		/* Untag one argument. */
	subi	rtemp1,r4,1			/* Remove tag but don't shift. */
	mullw	r3,r3,rtemp1
	addi	r3,r3,1				/* Add back the tag. */
	blr
	RegMask (mul_word,M_R3|M_R23)

INLINE_ROUTINE(plus_word)
	subi	rtemp2,r3,1			/* Remove a tag */
	add		r3,rtemp2,r4		/* Add the values */
	blr
	RegMask (plus_word,(M_R3|M_R23))

INLINE_ROUTINE(minus_word)
	sub		rtemp2,r3,r4		/* Do the subtraction. */
	addi	r3,rtemp2,1			/* restore tag bit */
	blr
	RegMask (minus_word,(M_R3|M_R23))

INLINE_ROUTINE(div_word)
	cmpwi	r4,TAGGED(0)
	beqM 	div_by_zero
	subi	r3,r3,1				/* Subtract tag from args. */
	subi	rtemp1,r4,1
	divwu	r3,r3,rtemp1
	slwi	r3,r3,TAGSHIFT		/* Tag the result. */
	addi	r3,r3,1
	blr
	RegMask (div_word,M_R3|M_R23)

INLINE_ROUTINE(mod_word)
	cmpwi	r4,TAGGED(0)
	beqM 	div_by_zero
	subi	rtemp1,r3,TAGSHIFT	/* Untag arguments. */
	subi	rtemp2,r4,TAGSHIFT
	/* We don't get the remainder directly so we have to do this. */
	divwu	rtemp1,rtemp1,rtemp2
	mullw	rtemp1,rtemp1,rtemp2
	sub	r3,r3,rtemp1
	blr
	RegMask (mod_word,M_R3|M_R23)

/* Unsigned tests on words. */
	TEST(word_geq,ge)
	TEST(word_leq,le)
	TEST(word_gtr,gt)
	TEST(word_lss,lt)

INLINE_ROUTINE(int_to_word)
	andi.	r0,r3,1
	bnelr			/* Return it if it's short. */
	/* Else drop through. */

/* This is now used in conjunction with isShort in Word.fromint */
INLINE_ROUTINE(get_first_long_word_a)
/* If it's long we can take the first word of the long
   precision representation.  It is in little-endian form
   and the sign bit is in the header. */
	lbz	rtemp3,-4(r3)	/* Flag byte */
	andi.	r0,rtemp3,16	/* 16 is negative bit. */
	li	rtemp3,0 
	lwbrx	r3,r3,rtemp3
	beq	i2w1
	subf	r3,r3,rtemp3	/* Negate. */
i2w1:	
	slwi	r3,r3,TAGSHIFT	/* Tag the result. */
	addi	r3,r3,1
	blr
	RegMask (int_to_word,M_R3|M_R23)
	RegMask (get_first_long_word,M_R3|M_R23)


#ifndef AIX
globldec(MD_flush_instruction_cache)
/*
This function is needed because the instruction cache on the
PowerPC does not see changes in the data cache.  When code
segments are written it is necessary to flush the data from
the data cache and also invalidate the instruction cache just
in case the location we have written to happened to previously
contain code as was in the instruction cache.
*/
#define CACHE_LINE_SIZE	32
/* TODO: We only need to flush the appropriate cache lines. 
   i.e. We don't need to call dcbf for each byte. */
	mtctr	r4
mfic1:	dcbf	0,r3 /* Flush data - i.e. make sure memory is up to date. */
	icbi	0,r3	/* Flush instructions - make sure we reload. */
	addi	r3,r3,1
	bdnz	mfic1
	sync; isync		/* This MAY help. */
	blr
#endif


/* Register mask vector. - extern int registerMaskVector[];
   Each entry in this vector is a set of the registers modified
   by the function.  It is an untagged bitmap with the registers
   encoded in the same way as in the code generator.
   Unused entries are set to Mask_all for safety in case a new
   entry is added to the iovector without also adding an entry
   here. */
#define dd	.long
globldec(registerMaskVector)
	dd	Mask_all				/* 0 is unused */
	dd	Mask_finish				/* 1 */
	dd	Mask_install_root       /* 2 */
	dd	Mask_all				/* 3 is unused */
	dd	Mask_all				/* 4 is unused */
	dd	Mask_all				/* 5 is unused */
	dd	Mask_strconcat          /* 6 */
	dd	Mask_all				/* 7 is unused */
	dd	Mask_all				/* 8 is unused */
	dd	Mask_change_dir         /* 9 */
	dd	Mask_all				/* 10 is unused */
	dd	Mask_alloc_store         /* 11 */
	dd	Mask_substring           /* 12 */
	dd	Mask_all				 /* return = 13 */
	dd	Mask_all				 /* raisex = 14 */
	dd	Mask_get_length          /* 15 */
	dd	Mask_all				/* 16 is unused */
	dd	Mask_get_flags_         /* 17 */
	dd	Mask_all                /* 18 - now unused */
	dd	Mask_all                /* 19 - now unused */
	dd	Mask_all                /* 20 - now unused */
	dd	Mask_all				/* 21 is unused */
	dd	Mask_all				/* 22 is unused */
	dd	Mask_all				/* 23 is unused */
	dd	Mask_teststreq           /* 24 */
	dd	Mask_teststrneq          /* 25 */
	dd	Mask_teststrgtr          /* 26 */
	dd	Mask_teststrlss          /* 27 */
	dd	Mask_teststrgeq          /* 28 */
	dd	Mask_teststrleq          /* 29 */
	dd	Mask_exception_trace     /* 30 */
	dd	Mask_all                 /* 31 - now unused */
	dd	Mask_all                 /* 32 - now unused */
	dd	Mask_all                 /* 33 - now unused */
	dd	Mask_all                 /* 34 - now unused */
	dd	Mask_all                 /* 35 - now unused */
	dd	Mask_all                 /* 36 */
	dd	Mask_all				/* 37 is unused */
	dd	Mask_all				/* 38 is unused */
	dd	Mask_all				/* 39 is unused */
	dd	Mask_commit              /* 40 */
	dd	Mask_all				/* 41 is unused */
	dd	Mask_set_dbentry_        /* 42 */
	dd	Mask_get_dbentry         /* 43 */
	dd	Mask_all                 /* 44 - now unused */
	dd	Mask_all                 /* 45 - now unused */
	dd	Mask_createf             /* 46 */
	dd	Mask_lockseg             /* 47 */
	dd	Mask_all				 /* nullorzero = 48 */
	dd	Mask_all                 /* 49 - now unused */
	dd	Mask_all                 /* 50 - now unused */
	dd	Mask_Net_dispatch_		 /* 51 */
	dd	Mask_OS_spec_dispatch_	 /* 52 */
	dd	Mask_all				/* 53 is unused */
	dd	Mask_all				/* 54 is unused */
	dd	Mask_all				/* version_number = 55 */
	dd	Mask_all				/* 56 is unused */
	dd	Mask_all				/* 57 is unused */
	dd	Mask_all				/* 58 is unused */
	dd	Mask_all				/* 59 is unused */
	dd	Mask_all				/* 60 is unused */
	dd	Mask_IO_dispatch_		 /* 61 */
	dd	Mask_Sig_dispatch_		 /* 62 */
	dd	Mask_all				/* 63 is unused */
	dd	Mask_all				/* 64 is unused */
	dd	Mask_all				/* 65 is unused */
	dd	Mask_all				/* 66 is unused */
	dd	Mask_all				/* 67 is unused */
	dd	Mask_all				/* 68 is unused */
	dd	Mask_all				/* 69 is unused */
	dd	Mask_all				/* 70 is unused */
	dd	Mask_all				/* 71 is unused */
	dd	Mask_all				/* 72 is unused */
	dd	Mask_all				/* 73 is unused */
	dd	Mask_all				/* 74 is unused */
	dd	Mask_all				/* 75 is unused */
	dd	Mask_all				/* 76 is unused */
	dd	Mask_all				/* 77 is unused */
	dd	Mask_all				/* 78 is unused */
	dd	Mask_all				/* 79 is unused */
	dd	Mask_all				/* Mask_version_number_1 = 80 */
	dd	Mask_all                 /* 81 - now unused */
	dd	Mask_fork_process        /* 82 */
	dd	Mask_choice_process      /* 83 */
	dd	Mask_kill_self           /* 84 */
	dd	Mask_int_process         /* 85 */
	dd	Mask_send_on_channel     /* 86 */
	dd	Mask_receive_on_channel  /* 87 */
	dd	Mask_profiler            /* 88 */
	dd	Mask_all				/* 89 is unused */
	dd	Mask_all				/* 90 is unused */
	dd	Mask_all				/* 91 is unused */
	dd	Mask_full_gc_            /* 92 */
	dd	Mask_stack_trace_        /* 93 */
	dd	Mask_timing_dispatch_	 /* 94 */
	dd	Mask_all				/* 95 is unused */
	dd	Mask_all				/* 96 is unused */
	dd	Mask_all				/* 97 is unused */
	dd	Mask_get_dbasetime_      /* 98 */
	dd	Mask_objsize_            /* 99 */
	dd	Mask_showsize_           /* 100 */
	dd	Mask_all				/* 101 is unused */
	dd	Mask_all				/* 102 is unused */
	dd	Mask_interrupt_console_processes_ /* 103 */
	dd	Mask_all				/* 104 is unused */
	dd	Mask_is_short            /* 105 */
	dd	Mask_aplus               /* 106 */
	dd	Mask_aminus              /* 107 */
	dd	Mask_amul                /* 108 */
	dd	Mask_adiv                /* 109 */
	dd	Mask_amod                /* 110 */
	dd	Mask_aneg                /* 111 */
	dd	Mask_xora				 /* 112 */
	dd	Mask_equala              /* 113 */
	dd	Mask_ora				 /* 114 */
	dd	Mask_anda				 /* 115 */
	dd	Mask_all				 /* version_number_3 = 116 */
	dd	Mask_Real_str			 /* 117 */
	dd	Mask_Real_geq            /* 118 */
	dd	Mask_Real_leq            /* 119 */
	dd	Mask_Real_gtr            /* 120 */
	dd	Mask_Real_lss            /* 121 */
	dd	Mask_Real_eq             /* 122 */
	dd	Mask_Real_neq            /* 123 */
	dd	Mask_Real_dispatch		 /* 124 */
	dd	Mask_Real_add            /* 125 */
	dd	Mask_Real_sub            /* 126 */
	dd	Mask_Real_mul            /* 127 */
	dd	Mask_Real_div            /* 128 */
	dd	Mask_all				 /* 129 is unused */
	dd	Mask_Real_neg            /* 130 */
	dd	Mask_all				 /* 131 is unused */
	dd	Mask_Real_repr           /* 132 */
	dd	Mask_Real_conv           /* 133 */
	dd	Mask_Real_int            /* 134 */
	dd	Mask_Real_float          /* 135 */
	dd	Mask_Real_sqrt           /* 136 */
	dd	Mask_Real_sin            /* 137 */
	dd	Mask_Real_cos            /* 138 */
	dd	Mask_Real_arctan         /* 139 */
	dd	Mask_Real_exp            /* 140 */
	dd	Mask_Real_ln             /* 141 */
	dd	Mask_all                 /* 142 - now unused */
	dd	Mask_all				 /* 143 is unused */
	dd	Mask_all				 /* 144 is unused */
	dd	Mask_all				 /* 145 is unused */
	dd	Mask_all				 /* 146 is unused */
	dd	Mask_all				 /* 147 is unused */
	dd	Mask_all				 /* stdin = 148 */
	dd	Mask_all				 /* stdout= 149 */
	dd	Mask_process_env_dispatch_	 /* 150 */
	dd	Mask_set_string_length	 /* 151 */
	dd	Mask_get_first_long_word /* 152 */
	dd	Mask_all				 /* 153 is unused */
	dd	Mask_all				 /* 154 is unused */
	dd	Mask_all				 /* 155 is unused */
	dd	Mask_all				 /* 156 is unused */
	dd	Mask_all				 /* 157 is unused */
	dd	Mask_all				 /* 158 is unused */
	dd	Mask_all				 /* 159 is unused */
	dd	Mask_all				 /* 160 is unused */
	dd	Mask_all				 /* 161 is unused */
	dd	Mask_all				 /* 162 is unused */
	dd	Mask_all				 /* 163 is unused */
	dd	Mask_all				 /* 164 is unused */
	dd	Mask_all				 /* 165 is unused */
	dd	Mask_all				 /* 166 is unused */
	dd	Mask_all				 /* 167 is unused */
	dd	Mask_all				 /* 168 is unused */
	dd	Mask_all				 /* 169 is unused */
	dd	Mask_all				 /* 170 is unused */
	dd	Mask_all				 /* 171 is unused */
	dd	Mask_all				 /* 172 is unused */
	dd	Mask_all				 /* 173 is unused */
	dd	Mask_all				 /* 174 is unused */
	dd	Mask_all				 /* 175 is unused */
	dd	Mask_all				 /* 176 is unused */
	dd	Mask_all				 /* 177 is unused */
	dd	Mask_all				 /* 178 is unused */
	dd	Mask_all				 /* 179 is unused */
	dd	Mask_all				 /* 180 is unused */
	dd	Mask_all				 /* 181 is unused */
	dd	Mask_all				 /* 182 is unused */
	dd	Mask_all				 /* 183 is unused */
	dd	Mask_all				 /* 184 is unused */
	dd	Mask_all				 /* 185 is unused */
	dd	Mask_all				 /* 186 is unused */
	dd	Mask_all				 /* 187 is unused */
	dd	Mask_all				 /* 188 is unused */
	dd	Mask_io_operation        /* 189 */
	dd	Mask_all				 /* 190 is unused */
	dd	Mask_all                 /* 191 - now unused */
	dd	Mask_all				 /* 192 is unused */
	dd	Mask_all				 /* 193 is unused */
	dd	Mask_set_code_constant	 /* 194 */
	dd	Mask_move_words			 /* 195 */
	dd	Mask_shift_right_arith_word	 /* 196 */
	dd	Mask_int_to_word		 /* 197 */
	dd	Mask_move_bytes			 /* 198 */
 	dd	Mask_all				 /* 199 now unused */
	dd	Mask_set_flags_          /* 200 */
	dd	Mask_shrink_stack_       /* 201 */
	dd	Mask_all				 /* stderr = 202 */
 	dd	Mask_all				 /* 203 now unused */
	dd	Mask_callcode_tupled     /* 204 */
	dd	Mask_foreign_dispatch_   /* 205 */
	dd	Mask_install_subshells_  /* 206 */
	dd	Mask_all				 /* 207 is unused */
	dd	Mask_all				 /* 208 now unused */
	dd	Mask_XWindows_           /* 209 */
	dd	Mask_all				 /* 210 is unused */
	dd	Mask_all				 /* 211 is unused */
	dd	Mask_all				 /* 212 is unused */
	dd	Mask_is_big_endian       /* 213 */
	dd	Mask_bytes_per_word      /* 214 */
	dd	Mask_offset_address      /* 215 */
	dd	Mask_shift_right_word    /* 216 */
	dd	Mask_word_neq            /* 217 */
	dd	Mask_not_bool            /* 218 */
	dd	Mask_all				 /* 219 is unused */
	dd	Mask_all				 /* 220 is unused */
	dd	Mask_all				 /* 221 is unused */
	dd	Mask_all				 /* 222 is unused */
	dd	Mask_string_length       /* 223 */
	dd	Mask_all				 /* 224 is unused */
	dd	Mask_all				 /* 225 is unused */
	dd	Mask_all				 /* 226 is unused */
	dd	Mask_all				 /* 227 is unused */
	dd	Mask_all				 /* 228 is unused */
	dd	Mask_int_eq              /* 229 */
	dd	Mask_int_neq             /* 230 */
	dd	Mask_int_geq             /* 231 */
	dd	Mask_int_leq             /* 232 */
	dd	Mask_int_gtr             /* 233 */
	dd	Mask_int_lss             /* 234 */
	dd	Mask_string_sub          /* 235 */
	dd	Mask_all				 /* 236 is unused */
	dd	Mask_all				 /* 237 is unused */
	dd	Mask_mul_word            /* 238 */
	dd	Mask_plus_word           /* 239 */
	dd	Mask_minus_word          /* 240 */
	dd	Mask_div_word            /* 241 */
	dd	Mask_or_word             /* 242 */
	dd	Mask_and_word            /* 243 */
	dd	Mask_xor_word            /* 244 */
	dd	Mask_shift_left_word     /* 245 */
	dd	Mask_mod_word            /* 246 */
	dd	Mask_word_geq            /* 247 */
	dd	Mask_word_leq            /* 248 */
	dd	Mask_word_gtr            /* 249 */
	dd	Mask_word_lss            /* 250 */
	dd	Mask_word_eq             /* 251 */
	dd	Mask_load_byte           /* 252 */
	dd	Mask_load_word           /* 253 */
	dd	Mask_assign_byte         /* 254 */
	dd	Mask_assign_word         /* 255 */

