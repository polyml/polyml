/*
    Title: 	Machine dependent code for Power architecture.

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


#include <sys/time.h>
#include <sys/file.h>
#if defined(MACOSX)
#if defined(MACOSXPRE102)
/* Before OS X 10.2 signal handlers were given sigcontext structures. */
#define NIP(scp)	scp->sc_ir
#define CCR(scp)	scp->sc_psw
#define GPR0(scp)	(((word*)(scp)->sc_regs)[2])
#define GPR1(scp)	(((word*)(scp)->sc_regs)[1+2])
#define GPR2(scp)	(((word*)(scp)->sc_regs)[2+2])
#define GPR3(scp)	(((word*)(scp)->sc_regs)[3+2])
#define GPR4(scp)	(((word*)(scp)->sc_regs)[4+2])
#define GPR5(scp)	(((word*)(scp)->sc_regs)[5+2])
#define GPR6(scp)	(((word*)(scp)->sc_regs)[6+2])
#define GPR7(scp)	(((word*)(scp)->sc_regs)[7+2])
#define GPR8(scp)	(((word*)(scp)->sc_regs)[8+2])
#define GPR9(scp)	(((word*)(scp)->sc_regs)[9+2])
#define GPR10(scp)	(((word*)(scp)->sc_regs)[10+2])
#define GPR11(scp)	(((word*)(scp)->sc_regs)[11+2])
#define GPR12(scp)	(((word*)(scp)->sc_regs)[12+2])
#define GPR13(scp)	(((word*)(scp)->sc_regs)[13+2])
#define GPR14(scp)	(((word*)(scp)->sc_regs)[14+2])
#define GPR15(scp)	(((word*)(scp)->sc_regs)[15+2])
#define GPR16(scp)	(((word*)(scp)->sc_regs)[16+2])
#define GPR17(scp)	(((word*)(scp)->sc_regs)[17+2])
#define GPR18(scp)	(((word*)(scp)->sc_regs)[18+2])
#define GPR19(scp)	(((word*)(scp)->sc_regs)[19+2])
#define GPR20(scp)	(((word*)(scp)->sc_regs)[20+2])
#define GPR21(scp)	(((word*)(scp)->sc_regs)[21+2])
#define GPR22(scp)	(((word*)(scp)->sc_regs)[22+2])
#define GPR23(scp)	(((word*)(scp)->sc_regs)[23+2])
#define GPR24(scp)	(((word*)(scp)->sc_regs)[24+2])
#define GPR25(scp)	(((word*)(scp)->sc_regs)[25+2])
#define GPR26(scp)	(((word*)(scp)->sc_regs)[26+2])
#define GPR27(scp)	(((word*)(scp)->sc_regs)[27+2])
#define GPR28(scp)	(((word*)(scp)->sc_regs)[28+2])
#define GPR29(scp)	(((word*)(scp)->sc_regs)[29+2])
#define GPR30(scp)	(((word*)(scp)->sc_regs)[30+2])
#define GPR31(scp)	(((word*)(scp)->sc_regs)[31+2])
#define LINK(scp)	(((word*)(scp)->sc_regs)[36])
#else
/* Mac OS X 10.2 and, hopefully, later. */
#define NIP(scp)	scp->uc_mcontext->ss.srr0
#define CCR(scp)	scp->uc_mcontext->ss.cr
#define GPR0(scp)	scp->uc_mcontext->ss.r0
#define GPR1(scp)	scp->uc_mcontext->ss.r1
#define GPR2(scp)	scp->uc_mcontext->ss.r2
#define GPR3(scp)	scp->uc_mcontext->ss.r3
#define GPR4(scp)	scp->uc_mcontext->ss.r4
#define GPR5(scp)	scp->uc_mcontext->ss.r5
#define GPR6(scp)	scp->uc_mcontext->ss.r6
#define GPR7(scp)	scp->uc_mcontext->ss.r7
#define GPR8(scp)	scp->uc_mcontext->ss.r8
#define GPR9(scp)	scp->uc_mcontext->ss.r9
#define GPR10(scp)	scp->uc_mcontext->ss.r10
#define GPR11(scp)	scp->uc_mcontext->ss.r11
#define GPR12(scp)	scp->uc_mcontext->ss.r12
#define GPR13(scp)	scp->uc_mcontext->ss.r13
#define GPR14(scp)	scp->uc_mcontext->ss.r14
#define GPR15(scp)	scp->uc_mcontext->ss.r15
#define GPR16(scp)	scp->uc_mcontext->ss.r16
#define GPR17(scp)	scp->uc_mcontext->ss.r17
#define GPR18(scp)	scp->uc_mcontext->ss.r18
#define GPR19(scp)	scp->uc_mcontext->ss.r19
#define GPR20(scp)	scp->uc_mcontext->ss.r20
#define GPR21(scp)	scp->uc_mcontext->ss.r21
#define GPR22(scp)	scp->uc_mcontext->ss.r22
#define GPR23(scp)	scp->uc_mcontext->ss.r23
#define GPR24(scp)	scp->uc_mcontext->ss.r24
#define GPR25(scp)	scp->uc_mcontext->ss.r25
#define GPR26(scp)	scp->uc_mcontext->ss.r26
#define GPR27(scp)	scp->uc_mcontext->ss.r27
#define GPR28(scp)	scp->uc_mcontext->ss.r28
#define GPR29(scp)	scp->uc_mcontext->ss.r29
#define GPR30(scp)	scp->uc_mcontext->ss.r30
#define GPR31(scp)	scp->uc_mcontext->ss.r31
#define LINK(scp)	scp->uc_mcontext->ss.lr
#endif
#else /* Linux */
#define NIP(scp)	(scp)->regs->nip
#define	CCR(scp)	(scp)->regs->ccr
#define	GPR0(scp)	(scp)->regs->gpr[PT_R0]
#define	GPR1(scp)	(scp)->regs->gpr[PT_R1]
#define	GPR2(scp)	(scp)->regs->gpr[PT_R2]
#define	GPR3(scp)	(scp)->regs->gpr[PT_R3]
#define	GPR4(scp)	(scp)->regs->gpr[PT_R4]
#define	GPR5(scp)	(scp)->regs->gpr[PT_R5]
#define	GPR6(scp)	(scp)->regs->gpr[PT_R6]
#define	GPR7(scp)	(scp)->regs->gpr[PT_R7]
#define	GPR8(scp)	(scp)->regs->gpr[PT_R8]
#define	GPR9(scp)	(scp)->regs->gpr[PT_R9]
#define	GPR10(scp)	(scp)->regs->gpr[PT_R10]
#define	GPR11(scp)	(scp)->regs->gpr[PT_R11]
#define	GPR12(scp)	(scp)->regs->gpr[PT_R12]
#define	GPR13(scp)	(scp)->regs->gpr[PT_R13]
#define	GPR14(scp)	(scp)->regs->gpr[PT_R14]
#define	GPR15(scp)	(scp)->regs->gpr[PT_R15]
#define	GPR16(scp)	(scp)->regs->gpr[PT_R16]
#define	GPR17(scp)	(scp)->regs->gpr[PT_R17]
#define	GPR18(scp)	(scp)->regs->gpr[PT_R18]
#define	GPR19(scp)	(scp)->regs->gpr[PT_R19]
#define	GPR20(scp)	(scp)->regs->gpr[PT_R20]
#define	GPR21(scp)	(scp)->regs->gpr[PT_R21]
#define	GPR22(scp)	(scp)->regs->gpr[PT_R22]
#define	GPR23(scp)	(scp)->regs->gpr[PT_R23]
#define	GPR24(scp)	(scp)->regs->gpr[PT_R24]
#define	GPR25(scp)	(scp)->regs->gpr[PT_R25]
#define	GPR26(scp)	(scp)->regs->gpr[PT_R26]
#define	GPR27(scp)	(scp)->regs->gpr[PT_R27]
#define	GPR28(scp)	(scp)->regs->gpr[PT_R28]
#define	GPR29(scp)	(scp)->regs->gpr[PT_R29]
#define	GPR30(scp)	(scp)->regs->gpr[PT_R30]
#define	GPR31(scp)	(scp)->regs->gpr[PT_R31]
#define LINK(scp)	(scp)->regs->gpr[PT_LNK]
#endif

#include <stdlib.h>
#include <signal.h>
#include <assert.h>

#include <sys/errno.h>
#include "globals.h"
#include "memory.h"
#include "mm.h"
#include "gc.h"
#include "machine_assembly.h"
#include "run_time.h"
#include "mpoly.h"
#include "arb.h"
#include "int_opcodes.h"
#include "reals.h"
#include "addresses.h"
#include "diagnostics.h"
#include "processes.h"
#include "sighandler.h"
#include "machine_dep.h"
#include "proper_io.h"
#include "profiling.h"

#ifndef SA_ONSTACK
/* Older versions of Linux did not define SA_ONSTACK. */
#define SA_ONSTACK 0
#endif
/* May need some of these for older versions of Mac OS X. */
#ifndef SA_RESTART
#define SA_RESTART	0
#endif
#ifndef SA_SIGINFO
#define SA_SIGINFO	0
#endif


#define VERSION_NUMBER POLY_version_number

/* forward declarations */
static void add_return_code(void);
static void add_raisex(void);
static void add_kill_self(void);

extern int MD_trap_handler_address(void);
extern void MD_trap_handler(void);
/*extern void MD_trap_handler1(int);*/

#define add_function_to_io_area(x,y) add_word_to_io_area(x,(word)(y))

/* This module provides various routines used by the rest of the system to do
   machine dependent operations. Their names all begin with MD. There are
   also a few such procedures written in assembly code in power_assembly.s */

/* This is set when poly_stack->p_pc and poly_stack->p_sp are set */
int in_run_time_system = 1; /* Start off in run-time system. */

/******************************************************************************/
/*                                                                            */
/*      Stack Frame                                                           */
/*                                                                            */
/******************************************************************************/

/*

What do we store in the ML stack object?

  p_space
  p_pc
  p_sp (r30)
  p_hr (r31)
  p_nregs = 27
  21 tagged registers (r3-r10, r13-r25)
  the literal 3
  3 untagged registers (r11, r12, CR)
  the real ML stack
  
Register Usage
--------------

r0 scratch register (unsaved?)
r1 - don't touch - dedicated C register (stack - like SPARC %o6) 
r2 - don't touch - dedicated C register (TOC)
r3      used for the first argument to a function, and for the result.
r4-r6  used for the next 3 args, any others being passed on the stack.
(We may later decide to use r7-r10 for parameters too).
 
r23        is the address of the code being called (like SPARC %o4)
r24        is the closure pointer or static link pointer (like SPARC %o5)
r25 (rr)   is used as the compiler-visible link register (like SPARC %o7)
r26        is an RTS scratch register (unsaved)
r27 (rsp)  is the ML stack pointer,
r28 (rsl)  is the stack limit,
r29 (rhp)  is the heap pointer,
r30 (rhl)  is the heap limit,
r31 (rhr)  points to the top exception handler.

r7-r10 and r12-r22 (15 registers) are available as general work registers,
as are r3-r6 and r23-r25, when they are not fulfilling their specialised
duties. That's a total of 21 general-purpose registers (as opposed to
17 on the SPARC).

r11, r12 are used as code-generator visible untagged registers.
r26 is used as a compiler-invisible RTS scratch register for
handling traps.

LR is a semi-volatile register used to cache the return address.
Executing a trap can copy rr into LR, even if it wasn't
there before. Furthermore, it can change the tagged status of
LR - it may be tagged after the trap, even if it was untagged
before. What we do guarantee is that if LR cached rr before
the trap, then it caches it afterwards (modulo possible tag bit
discrepancies). Executing any function call (including an RTS
call) invalidates both rr and LR.

CTR is a volatile register - we use it to to return from traps
and normal RTS calls, and it is also used to implement tail-calls.

*/
#define ML_HR 31
#define ML_HL 30
#define ML_HP 29
#define ML_SL 28
#define ML_SP 27
#define GPSP(scp)	GPR27(scp)
#define GPHR(scp)	GPR31(scp)
#define GPHP(scp)	GPR29(scp)
#define GPSL(scp)	GPR28(scp)

/* The first instruction executed after native code returns
   is the saved return address -2 */ 
#define RETURNOFFSET (-2)

/* The first instruction executed in a handler
   is the stored address -2 */
#define HANDLEROFFSET (-2)

#ifndef NOISY
#define NOISY 0
#endif
#define NOTIFYGC 0

/******************************************************************************/
/*                                                                            */
/*      dumpregs                                                              */
/*                                                                            */
/******************************************************************************/
void dumpregs(char *where)
{
  int i;
  proper_printf("%s: poly_stack = 0x%08x\n", where, (int)poly_stack);
  proper_printf("poly_stack->p_pc = 0x%08x\n", (int)poly_stack->p_pc);
  proper_printf("poly_stack->p_sp = 0x%08x\n", (int)poly_stack->p_sp);
  proper_printf("poly_stack->p_hr = 0x%08x\n", (int)poly_stack->p_hr);
  proper_printf("in_run_time_system = %i\n", in_run_time_system);
  for (i=0; i < poly_stack->p_nreg; i++)
  {
    proper_printf("poly_stack->p_reg[%i] = 0x%08x\n",i,poly_stack->p_reg[i]);
  }
  proper_fflush(stdout);
  for (i= -2; i < 8; i++)
  {
    proper_printf("poly_stack->p_sp[%i] = 0x%08x\n",i,poly_stack->p_sp[i]);
  }
  proper_fflush(stdout);
  
  give_stack_trace((word *)poly_stack + OBJECT_LENGTH(poly_stack));
  proper_fflush(stdout);
}
    

/* we are using a modified form of the "mips" version here, because that's
   set up for the SPARC(!) format tagged integers, whereas the sparc version
   is set up for the 68000 integers. I guess it depends on what *previous*
   architecture was used to do the port. SPF 21/6/95 */

/******************************************************************************/
/*                                                                            */
/*      MD_init_stack_frame - called by run-time system                       */
/*                                                                            */
/******************************************************************************/
void MD_init_stack_frame(StackObject *stack, Handle proc, Handle arg)
/* Initialise stack frame. */
{
    word stack_size,i;

    stack_size = OBJ_OBJECT_LENGTH(((word*)stack)[-1]);
    stack->p_space = OVERFLOW_STACK_SIZE;
    stack->p_pc = (byte*)TAGGED(0); /* As if we had called MD_set_for_retry. */
    stack->p_sp  = (word*)stack + stack_size-4; /* sp */
    stack->p_nreg = 22; /* r3-r8, r13-r25, link */
    /* Reset all registers since this may be an old stack frame */
    for (i = 0; i < 22; i++)
    {
      stack->p_reg[i] = TAGGED(0);
    }
    stack->p_reg[19] = (word)DEREFWORDHANDLE(proc); /* Set r24 to the closure address. */
    stack->p_reg[21] = (word)interface_map[POLY_SYS_kill_self]; /* Set link reg to code to kill process. */
    stack->p_reg[22] = 3; /* 3(?) unchecked registers. */
    /* Since they're unchecked, they don't need to be initialised,
       but set them to 0 anyway. */
    stack->p_reg[23] = 0;
    stack->p_reg[24] = 0;
    stack->p_reg[25] = 0;
    /* If this function takes an argument store it in the argument register. */
    if (arg != 0) stack->p_reg[0] = (word)DEREFWORDHANDLE(arg);
    /* No previous handler so point it at itself. */
    ((word**)stack)[stack_size-1] = &((word*)stack)[stack_size-1];
    /* Default handler. */
    ((word*)stack)[stack_size-2] = (word)interface_map[POLY_SYS_kill_self] - HANDLEROFFSET;
    /* Set up exception handler */
    ((word*)stack)[stack_size-3] = TAGGED(0); /* Default handler. */
    stack->p_hr = (word*)stack+(stack_size-3);
    /* Return address. */
    ((word*)stack)[stack_size-4] = (word)interface_map[POLY_SYS_kill_self] - RETURNOFFSET;
    return;
}

/******************************************************************************/
/*                                                                            */
/*      MD_increment_profile_count1 - called by run-time system               */
/*                                                                            */
/******************************************************************************/
void MD_increment_profile_count1(byte *pc, word *sp)
/* Called from MD_increment_profile_count which gets the sp value. */
{
  if (in_run_time_system)
  {
    /* poly_stack points to poly code that called the runtime system */
    add_count(poly_stack->p_pc, poly_stack->p_sp, 1);
  }
  else /* in poly code or assembly code */
  {
    if ((word *)poly_stack <= sp && sp < end_of_stack)
    {
      add_count(pc, sp, 1);
    }
    else
    {
      Crash("Bad sp in MD_increment_profile_count, pc = %p, ps = %p",
             pc, sp);
    }
  }
  return;
}

/******************************************************************************/
/*                                                                            */
/*      MD_increment_profile_count_using_context - called by run_time.c       */
/*                                                                            */
/******************************************************************************/
void MD_increment_profile_count_using_context(SIGNALCONTEXT *scp)
{
  /* Poly/ML uses register r27 as the stack pointer. (C uses r1.)  */
  MD_increment_profile_count1( (byte *)(NIP(scp)), (word *)(GPSP(scp)));
  return;
}

/******************************************************************************/
/*                                                                            */
/*      MD_interrupt_code1 - called by run-time system                        */
/*                                                                            */
/******************************************************************************/
void MD_interrupt_code1(word **sl)
/* Called with sl as the address of the saved stack limit register. */
{
/*
   Note: to force an interrupt, we set the stack limit register to
   point at the "end of the stack". This is interpretted as
   meaning "the last word of the stack" on the SPARC, but
   "the first word after the stack" on the RS/6000.
   SPF 12/10/95
*/

  if (! in_run_time_system) /* only if in Poly code */
  {
    word *limit  = *sl;
    word *top    = end_of_stack; /* "end_of_stack - 1" on the SPARC */
    word *bottom = (word*)poly_stack + poly_stack->p_space;
  
    if (limit == top)
    {
       /* already set for interrupt */
       return;
    }
    else if (limit == bottom)
    {
       *sl = top;
    }
    else
    {
       Crash("Unable to interrupt code: bottom = %p, limit = %p, top = %p",
              bottom, limit, top);
    }
  }
  return;
}

/******************************************************************************/
/*                                                                            */
/*      MD_interrupt_code_using_context - called by run_time.c                */
/*                                                                            */
/******************************************************************************/
void MD_interrupt_code_using_context(SIGNALCONTEXT *scp)
{
  /* Poly/ML uses register r28 as the stack limit register. */
  MD_interrupt_code1((word **)&(GPSL(scp)));
  return;
}

/******************************************************************************/
/*                                                                            */
/*      MD_switch_to_poly - called by run-time system                         */
/*                                                                            */
/******************************************************************************/
void MD_switch_to_poly(void)
/* (Re)-enter the Poly code from C. */
{
   MD_switch_to_poly_X();
}

/******************************************************************************/
/*                                                                            */
/*      MD_set_for_retry - called by run-time system                          */
/*                                                                            */
/******************************************************************************/
void MD_set_for_retry(int ioCall) /* Nothing to do. */
{
  poly_stack->p_pc = (byte*)TAGGED(0); /* This value is treated specially. */
  return;
}

/******************************************************************************/
/*                                                                            */
/*      MD_save_ML_state - utility function                                   */
/*                                                                            */
/******************************************************************************/
void MD_save_ML_state(SIGNALCONTEXT *scp)
{
  poly_stack->p_pc = (byte*)NIP(scp);
  poly_stack->p_sp = (word*)GPSP(scp);
  poly_stack->p_hr = (word*)GPHR(scp);
  
  /* Parameter registers */
  poly_stack->p_reg[0]  = GPR3(scp);
  poly_stack->p_reg[1]  = GPR4(scp);
  poly_stack->p_reg[2]  = GPR5(scp);
  poly_stack->p_reg[3]  = GPR6(scp);
  poly_stack->p_reg[4]  = GPR7(scp);
  poly_stack->p_reg[5]  = GPR8(scp);
  poly_stack->p_reg[6]  = GPR9(scp);
  poly_stack->p_reg[7]  = GPR10(scp);
  
  /* General ML registers */
  poly_stack->p_reg[8]  = GPR13(scp);
  poly_stack->p_reg[9]  = GPR14(scp);
  poly_stack->p_reg[10] = GPR15(scp);
  poly_stack->p_reg[11] = GPR16(scp);
  poly_stack->p_reg[12] = GPR17(scp);
  poly_stack->p_reg[13] = GPR18(scp);
  poly_stack->p_reg[14] = GPR19(scp);
  poly_stack->p_reg[15] = GPR20(scp);
  poly_stack->p_reg[16] = GPR21(scp);
  poly_stack->p_reg[17] = GPR22(scp);
  poly_stack->p_reg[18] = GPR23(scp);
  poly_stack->p_reg[19] = GPR24(scp);
  poly_stack->p_reg[20] = GPR25(scp);
  poly_stack->p_reg[21] = LINK(scp) | 2; /* We have to mark this as a code ptr. */

  /* Temporarily - set the number of registers since we're
     changing the number of registers in the stack. */
  poly_stack->p_nreg = 22;
  poly_stack->p_reg[22] = 3;

  /* GPR26 is an unsaved RTS scratch register */
  /* GPR27 and GPR28 are the heap and stack limit, which are
     recomputed from C variables when we restart the ML
     process. */

  /* Update heap pointer */
  A.M.pointer = (word*)GPHP(scp);

  /* Untagged ML registers */
  poly_stack->p_reg[23] = GPR11(scp);
  poly_stack->p_reg[24] = GPR12(scp);
  
  /* condition code register */
  poly_stack->p_reg[25] = CCR(scp);
}

/******************************************************************************/
/*                                                                            */
/*      catchFPE - utility function                                           */
/*                                                                            */
/******************************************************************************/
#ifdef LINUX
static void catchFPE(int sig, struct sigcontext context)
#else
static void catchFPE(int sig, int code, SIGNALCONTEXT *scp)
#endif
{
#ifdef LINUX
	struct sigcontext *scp = &context;
#endif
  /* It is possible for a floating point error to occur */
  /* legally in the run-time system.                    */
  assert(sig == SIGFPE);
  assert(scp != NULL);
  
  /* unblock SIGFPE interrupt, since we don't "return" from handler */
  { /* use standard SYSV calls */
    sigset_t mask;
    assert(sigemptyset(&mask) == 0);
    assert(sigaddset(&mask,SIGFPE) == 0);
    assert(sigprocmask(SIG_UNBLOCK,&mask,NULL) == 0);
  }
  
#if 0
  /* Not yet supported */
  raise_exception0(decode_fpcodes(info->si_code));
#else
  raise_exception0(EXC_overflow);
#endif

  /* NOT REACHED */
  return;
}


/******************************************************************************/
/*                                                                            */
/*      catchILL - utility function                                           */
/*                                                                            */
/******************************************************************************/
#ifdef LINUX
static void catchILL(int sig,  struct sigcontext context)
#else
static void catchILL(int sig, int code, SIGNALCONTEXT *scp)
#endif
{
#ifdef LINUX
  struct sigcontext *scp = &context;
#endif
  assert(sig == SIGILL);
  assert(scp != NULL);

#if 0  
  psiginfo(info,"catchILL");
#endif

  /* Shouldn't get stack overflow in run-time system */
  if (in_run_time_system)
  {
    { /* use standard SYSV calls */
      sigset_t mask;
      assert(sigemptyset(&mask) == 0);
      assert(sigaddset(&mask,SIGILL) == 0);
      assert(sigprocmask(SIG_UNBLOCK,&mask,NULL) == 0);
    }

#if 0
    psiginfo(info,"catchILL (illegal instruction in run-time system)");
#endif
    
    if (A.garbage_collecting)
      proper_printf("\nStack overflow in the garbage collector.\n");
    else
      proper_printf("\nStack overflow in the runtime system.\n");

    proper_printf("You may need to increase your stack limit and try again.\n");
    proper_fflush(stdout);

#if 0
  proper_printf("catchILL: in_run_time_system=0x%08x\n", in_run_time_system);
  proper_printf("catchILL: trapping pc=0x%08x\n", (int)NIP(scp));
  proper_printf("catchILL: proper_printf=0x%08x, MD_trap_handler=0x%08x, MD_trap_handler1=0x%08x\n",
          (int)proper_printf,(int)MD_trap_handler,(int)MD_trap_handler1);
     {
       int i;
       proper_printf("scp=0x%08x\n", (int)scp);
       for (i = 0; i < 32; i ++)
       {
         proper_printf("scp->GPR[%i]=0x%08x\n",i, (int)(scp->GPR[i]));
       }
       proper_fflush(stdout);
     }

     dumpregs("catchILL");
#endif

    exit(1);
    /*NOTREACHED*/
  }
  
  /* NOW, we are in the run-time system */
  in_run_time_system = 1;
  
#if NOISY
  proper_printf("catchILL: trapping pc=%08x\n", (int)NIP(scp));
#endif
  
  /* Save the ML state in poly_stack */
  MD_save_ML_state(scp);

  /* Instead, we "return" to the MD_trap_handler assembly language routine,
     which calls MD_trap_handler1, allowing the TOC address to be set
     up properly. SPF 14/8/95 */
  NIP(scp) = MD_trap_handler_address();

}


/******************************************************************************/
/*                                                                            */
/*      catchTRAP - utility function                                          */
/*                                                                            */
/******************************************************************************/
#ifdef LINUX
static void catchTRAP(int sig, struct sigcontext context)
#else
static void catchTRAP(int sig, int code, SIGNALCONTEXT *scp)
#endif
{
#ifdef LINUX
	struct sigcontext *scp = &context;
#endif
  assert(sig == SIGTRAP);
  assert(scp != NULL);

  /* shouldn't get SIGTRAP from run-time system,         */
  /* so reinstall default handler and return for retry, */
  /* which should lead to core dump.                    */
  if (in_run_time_system)
    {
      signal(SIGTRAP,SIG_DFL);
      return;
    }

  /* NOW, we are in the run-time system */
  in_run_time_system = 1;

#if NOISY
  proper_printf("catchTRAP: trapping pc=%08x\n", (int)NIP(scp));
#endif
  
  /* Save the ML state in poly_stack */
  MD_save_ML_state(scp);
  
  /* Instead, we "return" to the MD_trap_handler assembly language routine,
     which calls MD_trap_handler1, allowing the TOC address to be set
     up properly. SPF 14/8/95 */
  NIP(scp) = MD_trap_handler_address();
}

/******************************************************************************/
/*                                                                            */
/*      catchSEGV - utility function                                          */
/*                                                                            */
/******************************************************************************/
#ifdef LINUX
static void catchSEGV(int sig, struct sigcontext context)
#else
static void catchSEGV(int sig, int code, SIGNALCONTEXT *scp)
#endif
{
#ifdef LINUX
	struct sigcontext *scp = &context;
#endif
#ifdef MACOSX
  assert(sig == SIGSEGV || sig == SIGBUS);
#else
  assert(sig == SIGSEGV);
#endif
  assert(scp != NULL);
  /* shouldn't get SIGSEGV or SIGBUS from run-time system, */
  /* so reinstall default handler and return for retry,    */
  /* which should lead to core dump.                       */
  if (in_run_time_system)
    {
      signal(SIGSEGV,SIG_DFL);
      signal(SIGBUS,SIG_DFL);
      return;
    }

  /* NOW, we are in the run-time system */
  in_run_time_system = 1;
#if NOISY
  proper_printf("catchSEGV: trapping pc=%08x\n", (int)NIP(scp));
#endif
  
  /* Save the ML state in poly_stack */
  MD_save_ML_state(scp);
  
  NIP(scp) = MD_trap_handler_address();
}


/******************************************************************************/
/*                                                                            */
/*      get_reg - utility function                                            */
/*                                                                            */
/******************************************************************************/
static word *get_reg(int rno)
/* Returns a pointer to the register given by the 5 bit value rno. */
{
  /* Registers r0, r1, r2, r26, ML_SL, ML_HL and ML_HP should not be
     needed by the emulation code. */

  if (3 <= rno && rno <= 10)
  { /* Parameter registers */
    return &poly_stack->p_reg[rno-3];
  }
  
  else if (11 <= rno && rno <= 12)
  {
    /* Untagged ML registers */
    return &poly_stack->p_reg[rno+12];
  }
  
  else if (13 <= rno && rno <= 25)
  { /* General registers */
    return &poly_stack->p_reg[rno-5];
  }
  
  else if (rno == ML_SP)
  {
    return (word*)&poly_stack->p_sp;
  }

  else if (rno == ML_HR)
  {
    return (word*)&poly_stack->p_hr;
  }
  else
  {
    Crash("Unknown register %d at 0x%8x\n", rno, (int)(poly_stack->p_pc));
  }
}


/******************************************************************************/
/*                                                                            */
/*      set_CR0 - utility function                                            */
/*                                                                            */
/******************************************************************************/
static void set_CR0(int CR0)
{
  /* insert CR0 into condition register */
  poly_stack->p_reg[25] =
      (poly_stack->p_reg[25] & 0x0fffffff) | (CR0 << 28);
}

/******************************************************************************/
/*                                                                            */
/*      emulate_trap - utility function                                       */
/*                                                                            */
/******************************************************************************/
static void emulate_trap(word instr)
/* Emulate an "addo.", "subfco.", "cmpw" or "cmpwi" instruction */
{
    
    int opcode   = (instr >> 26) & 0x3f;  /*  6 bits */
    int RT       = (instr >> 21) & 0x1f;  /*  5 bits */
    int RA       = (instr >> 16) & 0x1f;  /*  5 bits */

    /* added 19/12/95 SPF */
    if (emulate_profiling)
    {
      add_count(poly_stack->p_pc, poly_stack->p_sp, 1);
    }

#define OPCODE2_addodot   ((1 << 10) | (266 << 1) | 1)   
#define OPCODE2_subfcodot ((1 << 10) | (8   << 1) | 1)   
#define OPCODE2_cmp       ((0 << 10) | (0   << 1) | 0)
#define OPCODE2_srawi	              ((824 << 1) | 0)
     
    /* We emulate comparisons only if they use CR field 0 */
    if (opcode == 11 && RT == 0)
    {
      /* cmpwi  RA,SI */
      Handle arg1, arg2;
      int UI = instr & 0xffff; /* 16 bits */
      
      /* signed comparisons sign-extend the immediate. */
      int exp2_15 = 1 << 15;
      int exp2_16 = 1 << 16;
      int SI = (UI < exp2_15) ? UI : UI - exp2_16;
      
      init_save_vec();
      arg1 = push_to_save_vec(*get_reg(RA));
      arg2 = push_to_save_vec(SI);
    
      {
        /* arguments to comp_longc are backwards */
        int res = comp_longc(arg2,arg1);
      
        switch (res)
        {
          case TAGGED(1): /* arg1 > arg2 */
            set_CR0(0x4); /* not LT; GT; not EQ; not SO */
            break;
        
          case TAGGED(0): /* arg1 = arg2 */
            set_CR0(0x2); /* not LT; not GT; EQ; not SO */
            break;
        
          case TAGGED(-1): /* arg1 < arg2 */
            set_CR0(0x8); /* LT; not GT; not EQ; not SO */
            break;
        
          default:
            Crash("Bad return value 0x%08x from comp_longc\n", res);
            break;
        }
      }
    
    } /* opcode == 11 */
    
    else if (opcode == 31)
    { 
      int RB       = (instr >> 11) & 0x1f;  /*  5 bits */
      int opcode2  = instr & 0x7ff;         /* 11 bits */
       
      init_save_vec();
      
      if (opcode2 == OPCODE2_addodot)
      {
        int a1 = *get_reg(RA);
        int a2 = *get_reg(RB);
	Handle arg1, arg2, res;
	/* We may have trapped either because one of the arguments
	   was long or because of overflow.  In the latter case
	   we may well have modified the destination register already.
	   That's fine unless RT == RA. */
	if (RB != 12) Crash("RB != regtemp2");
	a2 += 1; /* We've removed the tag from this. */
	/* printf("Emulating addition rt=%d, ra=%d, rb=%d\n", RT, RA, RB); */
	if (IS_INT(a1) && IS_INT(a2))
	{
		/* Must have been overflow.  May have to undo the
		   addition. */
		if (RT == RA) a1 = a1 - (a2-1);
	}
	arg1 = push_to_save_vec(a1);
	arg2 = push_to_save_vec(a2);
	res = add_longc(arg2, arg1);
      
        /* store result in RT */
        *(get_reg(RT)) = ((word)*res);
        
        /* we don't emulate the condition codes properly,
           but we MUST ensure that we reset the SO flag,
           because the compiled code tests this flag.
           If we left it set, the compiled code would loop
           back to re-emulate the instruction, giving us an
           endless succession of traps. SPF 11/8/95
        */
        set_CR0(0x0); /* not LT; not GT; not EQ; not SO */
      }
      
      else if (opcode2 == OPCODE2_subfcodot)
      {
	/* We may have trapped either because one of the arguments
	   was long or because of overflow.  In the latter case
	   we may well have modified the destination register already.
	   That's fine unless RT == RA or RT == RB. */
        int a1 = *get_reg(RA);
        int a2 = *get_reg(RB);
	Handle arg1, arg2, res;
	/* printf("Emulating subtraction rt=%d, ra=%d, rb=%d\n", RT, RA, RB); */
	if (RA == 12) a1 += 1;
	else if (RB == 12) a2 -= 1;
	else crash("RA != regtemp2 and RB != regtemp2");
	if (IS_INT(a1) && IS_INT(a2))
	{
		/* Must have been overflow.  May have to undo the
		   subtraction. */
		if (RT == RB) a2 = a2 + (a1-1);
		else if (RT == RB) a1 = (a2+1) - a1;
	}

	arg1 = push_to_save_vec(a1);
	arg2 = push_to_save_vec(a2);
        /* subf is *reversed* subtraction, but sub_longc
          (currently) expects reversed arguments, so the
          following call ought to be correct. */
        res = sub_longc(arg1, arg2);
      
        /* store result in RT */
        *(get_reg(RT)) = ((word)*res);

        /* we don't emulate the condition codes properly,
           but we MUST ensure that we reset the SO flag.
        */
        set_CR0(0x0); /* not LT; not GT; not EQ; not SO */
      }
      
      /* We emulate comparisons only if they use CR field 0 */
      else if (opcode2 == OPCODE2_cmp && RT == 0)
      {
        /* arguments to comp_longc are backwards */
        int a1 = *get_reg(RA);
        int a2 = *get_reg(RB);
	Handle arg1 = push_to_save_vec(a1);
	Handle arg2 = push_to_save_vec(a2);
        int res = comp_longc(arg2,arg1);
      
        switch (res)
        {
          case TAGGED(1): /* arg1 > arg2 */
            set_CR0(0x4); /* not LT; GT; not EQ; not SO */
            break;
        
          case TAGGED(0): /* arg1 = arg2 */
            set_CR0(0x2); /* not LT; not GT; EQ; not SO */
            break;
        
          case TAGGED(-1): /* arg1 < arg2 */
            set_CR0(0x8); /* LT; not GT; not EQ; not SO */
            break;
        
          default:
            Crash("Bad return value 0x%08x from comp_longc\n", res);
            break;
        }
      }
      else if (opcode2 == OPCODE2_srawi)
      {
	/* Start of multiply sequence.  poly_stack->p_pc now
	   points at the multiply. */
	int a1 = *get_reg(RT); /* srawi has regs reversed. */
	int a2 = *get_reg(12); /* The second arg is in regtemp2. */
	Handle arg1, arg2, res;
	word mullwInstr = ((word*)poly_stack->p_pc)[0];
	poly_stack->p_pc += 4;
	assert(RA == 11); /* destination of srawi. */
	/* Should be mullwo. rt,r12,r11 */
	assert((mullwInstr & 0xfc1fffff) == 0x7C0C5DD7);
	RT = (mullwInstr >> 21) & 0x1f;
	arg1 = push_to_save_vec(a1);
	arg2 = push_to_save_vec(a2+1); /* This has been untagged. */
	res = mult_longc(arg2, arg1);
	*(get_reg(RT)) = ((word)*res) - 1; /* Next instr will tag it. */
        set_CR0(0x0); /* Clear overflow bit. */
      }
      
      else
      {
        Crash("Can't emulate instruction: 0x%08x\n",instr);
      }

    } /* opcode == 31 */
    
    else /* opcode != 11 && opcode != 31 */
    {
      Crash("Can't emulate instruction: 0x%08x\n",instr);
    } 

}

/******************************************************************************/
/*                                                                            */
/*      check_heap                                                            */
/*                                                                            */
/******************************************************************************/
static void check_heap(int words_to_allocate)
{
   /* We don't have to adjust A.M.pointer because the AIX version
      doesn't adjust rhp until *after* it knows the allocation is safe.
      Contrariwise, we *do* have adjust A.M.bottom when we compare it with
      A.M.pointer. */
  if (store_profiling)
  {
    add_count(poly_stack->p_pc, poly_stack->p_sp, words_to_allocate);
  }
  else if (A.M.pointer >= A.M.bottom + words_to_allocate) 
  {
    Crash ("Spurious heap-limit trap");
  }
  
  /* Do we have a genuine out-of-heap problem, or did we only
     interrupt to profile the heap? */
  if (A.M.pointer < A.M.bottom + words_to_allocate)
  {
#if NOTIFYGC
    proper_printf("MD_trap_handler1: calling GC\n");
#endif
    QuickGC(words_to_allocate); /* Garbage-collect. */
  }
#if NOTIFYGC
  else proper_printf("MD_trap_handler1: not calling GC\n");
#endif
}

/******************************************************************************/
/*                                                                            */
/*      MD_trap_handler1 - called from MD_trap_handler                        */
/*                                                                            */
/******************************************************************************/
void MD_trap_handler1(int return_address)
{
 
#if NOISY
  proper_printf("In MD_trap_handler1\n");
#endif
    /* POWER pc is always word aligned */
    assert(ALIGNED(poly_stack->p_pc)); /* SPF */
    
    in_run_time_system = 1;
    
    {
       /* instruction that trapped. */
       word instr   = ((word*)poly_stack->p_pc)[0];
       int opcode   = (instr >> 26) & 0x3f;  /*  6 bits */
       int TO       = (instr >> 21) & 0x1f;  /*  5 bits */
       int RA       = (instr >> 16) & 0x1f;  /*  5 bits */
       int RB       = (instr >> 11) & 0x1f;  /*  5 bits */
       int opcode2  = instr & 0x7ff;         /* 11 bits */

#define OPCODE2_tw ((4 << 1) | 0)
#define OPCODE2_stwx ((151 << 1) | 0)
#define OPCODE2_stbx ((215 << 1) | 0)
    
       if (opcode == 3) 
       { 
          /* twi ?,?,? */
          if (RA == ML_HL)
          {
            /* twi ?,rhl,<allocation> ("small" heap check) */
            int bytes = instr & 0xffff; /* 16 bits */
            
            /* Skip over the trap instruction. */
            poly_stack->p_pc += 4;
   
            if ((1 << 15) <= bytes)
            {
               Crash("MD_trap_handler1: bad trap instruction 0x%08x (heap allocation < 0)\n",
                      instr);
            }
            check_heap(bytes / sizeof(word));        
          }
          else if (opcode2 == 1)
          {
	    /* Divide by zero check. */
            poly_stack->p_pc += 4;
            raise_exception0(EXC_divide);
          }
          else if (opcode2 == 0)
          {
            /* arbitrary precision emulation trap */
             word instr = ((word*)poly_stack->p_pc)[1];
             
             /* skip over the trap instruction and the emulated instruction */
             poly_stack->p_pc += 8;
    
             /* and do the emulation */
             emulate_trap(instr);
          }
	  else Crash("MD_trap_handler1: bad trap instruction at 0x%08x)\n", poly_stack->p_pc);

       } /* twi */
       
       else if (opcode == 31 && opcode2 == OPCODE2_tw)
       { 
         /* tw ?,?,? */
         if (RA == ML_HL)
         {
           /* tw ?,rhl,? ("large" heap check) */
            int bytes = *(get_reg(RB));
   
            if (bytes <= 0)
            {
               Crash("MD_trap_handler1: bad trap instruction 0x%08x (heap allocation < 0)\n",
                      instr);
            }
   
            /* Skip over the trap instruction. */
            poly_stack->p_pc += 4;
            check_heap(bytes / sizeof(word));
         }
         else if (RB == ML_SL)
         { /* tw ?,?,rsl (stack check) */
           /* Skip over the trap instruction. */
           poly_stack->p_pc += 4;
  
           check_current_stack_size((word*)*(get_reg(RA)));
        
           /* Now handle any interrupts. This may switch processes. */
           execute_pending_interrupts();
         }
         else
         {
           /* arbitrary precision emulation trap */
           /* the next instruction should be "addo.", "subfco.", cmp or cmpi */
           word instr = ((word*)poly_stack->p_pc)[1];
             
           /* skip over the trap instruction and the emulated instruction */
           poly_stack->p_pc += 8;
    
           /* and do the emulation */
           emulate_trap(instr);
         }
       } /* tw */
          
	   else if (opcode == 36) /* stw */
	   {
			/* Assignment to database mutable. */
		    int offset = instr & 0xffff;
			word value = *(get_reg(TO));
			word baseAddr = *(get_reg(RA));
			if (offset >= 32768) offset = offset - 65536;
			AssignMappedDatabaseWord(H, (word*)baseAddr, offset/sizeof(word), value);

			poly_stack->p_pc += 4; /* Skip over instruction. */
	   }

	   else if (opcode == 38) /* stb */
	   {
			/* Assignment to database mutable. */
		    int offset = instr & 0xffff;
			word value = *(get_reg(TO));
			word baseAddr = *(get_reg(RA));
			if (offset >= 32768) offset = offset - 65536;
			AssignMappedDatabaseByte(H, (byte*)baseAddr, offset, (byte)value);

			poly_stack->p_pc += 4; /* Skip over instruction. */
	   }

	   else if (opcode == 31 && opcode2 == OPCODE2_stwx)
	   {
			/* Assignment to database mutable. */
			word value = *(get_reg(TO));
			byte *baseAddr = (byte*)*(get_reg(RA));
			AssignMappedDatabaseWord(H, (word*)baseAddr, *(get_reg(RB))/sizeof(word), value);

			poly_stack->p_pc += 4; /* Skip over instruction. */
	   }

	   else if (opcode == 31 && opcode2 == OPCODE2_stbx)
	   {
			/* Assignment to database mutable. */
			word value = *(get_reg(TO));
			byte *baseAddr = (byte*)*(get_reg(RA));
			AssignMappedDatabaseByte(H, baseAddr, *(get_reg(RB)), (byte)value);

			poly_stack->p_pc += 4; /* Skip over instruction. */
	   }

        else 
       {
          Crash("MD_trap_handler1: 0x%08x is not a trap instruction; pc = %p\n",
                 instr, poly_stack->p_pc);
       }
    } /* scope of instr */

    LONGJMP(re_enter_poly,111); /* Return to poly process. */
}

/******************************************************************************/
/*                                                                            */
/*      MD_init_interface_vector - called from run-time system                */
/*                                                                            */
/******************************************************************************/
void MD_init_interface_vector(void)
{
    int i;
  
    for (i = 0; i < POLY_SYS_vecsize; i++)
      {
        add_function_to_io_area (i, BadOpCode_a);
      }

    add_function_to_io_area(POLY_SYS_exit, finisha);
    add_function_to_io_area(POLY_SYS_install_root, install_roota);
    add_function_to_io_area(POLY_SYS_strconcat, strconcata);
    add_function_to_io_area(POLY_SYS_alloc_store, alloc_store);
    add_function_to_io_area(POLY_SYS_chdir, change_dira);
    add_function_to_io_area(POLY_SYS_substring, substringa);
    add_function_to_io_area(POLY_SYS_get_length, get_length_a);
    add_function_to_io_area(POLY_SYS_get_flags, get_flags_a);
    add_function_to_io_area(POLY_SYS_str_compare, str_compare);
    add_function_to_io_area(POLY_SYS_teststreq, teststreq);
    add_function_to_io_area(POLY_SYS_teststrneq, teststrneq);
    add_function_to_io_area(POLY_SYS_teststrgtr, teststrgtr);
    add_function_to_io_area(POLY_SYS_teststrlss, teststrlss);
    add_function_to_io_area(POLY_SYS_teststrgeq, teststrgeq);
    add_function_to_io_area(POLY_SYS_teststrleq, teststrleq);
    add_function_to_io_area(POLY_SYS_exception_trace, exception_tracea);
    add_function_to_io_area(POLY_SYS_commit, commita);
    add_function_to_io_area(POLY_SYS_set_dbentry, set_dbentry_a);
    add_function_to_io_area(POLY_SYS_get_dbentry, get_dbentrya);
    add_function_to_io_area(POLY_SYS_createf, createfa);
    add_function_to_io_area(POLY_SYS_lockseg, locksega);
    add_function_to_io_area(POLY_SYS_profiler, profilera);
    add_function_to_io_area(POLY_SYS_is_short, is_shorta);
    add_function_to_io_area(POLY_SYS_aplus, add_long);
    add_function_to_io_area(POLY_SYS_aminus, sub_long);
    add_function_to_io_area(POLY_SYS_amul, mult_long);
    add_function_to_io_area(POLY_SYS_adiv, div_long);
    add_function_to_io_area(POLY_SYS_amod, rem_long);
    add_function_to_io_area(POLY_SYS_aneg, neg_long);
    add_function_to_io_area(POLY_SYS_equala, equal_long);
    add_function_to_io_area(POLY_SYS_ora, or_long);
    add_function_to_io_area(POLY_SYS_anda, and_long);
    add_function_to_io_area(POLY_SYS_xora, xor_long);
    add_function_to_io_area(POLY_SYS_Real_str, Real_stra);
    add_function_to_io_area(POLY_SYS_Real_geq, Real_geqa);
    add_function_to_io_area(POLY_SYS_Real_leq, Real_leqa);
    add_function_to_io_area(POLY_SYS_Real_gtr, Real_gtra);
    add_function_to_io_area(POLY_SYS_Real_lss, Real_lssa);
    add_function_to_io_area(POLY_SYS_Real_eq, Real_eqa);
    add_function_to_io_area(POLY_SYS_Real_neq, Real_neqa);
    add_function_to_io_area(POLY_SYS_Real_Dispatch, Real_dispatcha);
    add_function_to_io_area(POLY_SYS_Add_real, Real_adda);
    add_function_to_io_area(POLY_SYS_Sub_real, Real_suba);
    add_function_to_io_area(POLY_SYS_Mul_real, Real_mula);
    add_function_to_io_area(POLY_SYS_Div_real, Real_diva);
    add_function_to_io_area(POLY_SYS_Neg_real, Real_nega);
    add_function_to_io_area(POLY_SYS_Repr_real, Real_repra);
    add_function_to_io_area(POLY_SYS_conv_real, Real_conva);
    add_function_to_io_area(POLY_SYS_real_to_int, Real_inta);
    add_function_to_io_area(POLY_SYS_int_to_real, Real_floata);
    add_function_to_io_area(POLY_SYS_sqrt_real, Real_sqrta);
    add_function_to_io_area(POLY_SYS_sin_real, Real_sina);
    add_function_to_io_area(POLY_SYS_cos_real, Real_cosa);
    add_function_to_io_area(POLY_SYS_arctan_real, Real_arctana);
    add_function_to_io_area(POLY_SYS_exp_real, Real_expa);
    add_function_to_io_area(POLY_SYS_ln_real, Real_lna);
    add_function_to_io_area(POLY_SYS_io_operation, io_operation);
    add_function_to_io_area(POLY_SYS_fork_process, fork_processa);
    add_function_to_io_area(POLY_SYS_choice_process, choice_processa);
    add_function_to_io_area(POLY_SYS_int_process, int_processa);
    add_function_to_io_area(POLY_SYS_send_on_channel, send_on_channela);
    add_function_to_io_area(POLY_SYS_receive_on_channel, receive_on_channela);

    add_function_to_io_area(POLY_SYS_offset_address, offset_address);
    add_function_to_io_area(POLY_SYS_shift_right_word, shift_right_word);
    add_function_to_io_area(POLY_SYS_word_neq, word_neq);
    add_function_to_io_area(POLY_SYS_not_bool, not_bool);
    add_function_to_io_area(POLY_SYS_string_length, string_length);
    add_function_to_io_area(POLY_SYS_int_eq, int_eq);
    add_function_to_io_area(POLY_SYS_int_neq, int_neq);
    add_function_to_io_area(POLY_SYS_int_geq, int_geq);
    add_function_to_io_area(POLY_SYS_int_leq, int_leq);
    add_function_to_io_area(POLY_SYS_int_gtr, int_gtr);
    add_function_to_io_area(POLY_SYS_int_lss, int_lss);
    add_function_to_io_area(POLY_SYS_string_sub, string_sub);
    
    add_function_to_io_area(POLY_SYS_or_word, or_word);
    add_function_to_io_area(POLY_SYS_and_word, and_word);
    add_function_to_io_area(POLY_SYS_xor_word, xor_word);
    add_function_to_io_area(POLY_SYS_shift_left_word, shift_left_word);
    add_function_to_io_area(POLY_SYS_word_eq, word_eq);
    add_function_to_io_area(POLY_SYS_load_byte, load_byte);
    add_function_to_io_area(POLY_SYS_load_word, load_word);
    add_function_to_io_area(POLY_SYS_is_big_endian, is_big_endian);
    add_function_to_io_area(POLY_SYS_bytes_per_word, bytes_per_word);
    
    add_function_to_io_area(POLY_SYS_assign_byte, assign_byte);
    add_function_to_io_area(POLY_SYS_assign_word, assign_word);

    add_function_to_io_area(POLY_SYS_objsize, objsize_a ); /* MJC 27/04/88 */
    add_function_to_io_area(POLY_SYS_showsize, showsize_a); /* MJC 09/03/89 */


    add_word_to_io_area(POLY_SYS_version_number,TAGGED(VERSION_NUMBER));  /* MJC 05/07/90 */
    add_function_to_io_area(POLY_SYS_timing_dispatch, timing_dispatch_a); /* DCJM 10/4/00 */
    add_function_to_io_area(POLY_SYS_getdbasetime, get_dbasetime_a);  /* MJC 15/09/89 */
    
    add_function_to_io_area (POLY_SYS_interrupt_console_processes,
                         interrupt_console_processes_a); /* MJC 01/08/90 */
    
    add_function_to_io_area(POLY_SYS_install_subshells, install_subshells_a); /* MJC 12/09/90 */

    add_function_to_io_area(POLY_SYS_XWindows, XWindows_a); /* MJC 27/09/90 */

    add_function_to_io_area(POLY_SYS_full_gc, full_gc_a);     /* MJC 18/03/91 */
    add_function_to_io_area(POLY_SYS_stack_trace,stack_trace_a); /* MJC 18/03/91 */

    add_function_to_io_area(POLY_SYS_foreign_dispatch, foreign_dispatch_a); /* NIC 22/04/94 */
    add_function_to_io_area(POLY_SYS_callcode_tupled,  callcode_tupled);    /* SPF 07/07/94 */
    add_function_to_io_area(POLY_SYS_process_env,      process_env_dispatch_a); /* DCJM 25/4/00 */
    add_function_to_io_area(POLY_SYS_set_string_length, &set_string_length_a); /* DCJM 28/2/01 */
    add_function_to_io_area(POLY_SYS_get_first_long_word, &get_first_long_word_a); /* DCJM 28/2/01 */
    add_function_to_io_area(POLY_SYS_shrink_stack,     shrink_stack_a);     /* SPF  9/12/96 */
    add_function_to_io_area(POLY_SYS_set_flags,        set_flags_a);        /* SPF 12/02/97 */
    
    add_function_to_io_area(POLY_SYS_shift_right_arith_word, shift_right_arith_word); /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_int_to_word,      int_to_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_set_code_constant,set_code_constanta); /* DCJM 2/1/01 */
    add_function_to_io_area(POLY_SYS_move_bytes,       move_bytes);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_move_words,       move_words);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_mul_word,         mul_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_plus_word,        plus_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_minus_word,       minus_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_div_word,         div_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_mod_word,         mod_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_word_geq,		   word_geq);
    add_function_to_io_area(POLY_SYS_word_leq,		   word_leq);
    add_function_to_io_area(POLY_SYS_word_gtr,		   word_gtr);
    add_function_to_io_area(POLY_SYS_word_lss,		   word_lss);

    add_function_to_io_area(POLY_SYS_io_dispatch, &IO_dispatch_a); /* DCJM 8/5/00 */
    add_function_to_io_area(POLY_SYS_network, &Net_dispatch_a); /* DCJM 8/5/00 */
    add_function_to_io_area(POLY_SYS_os_specific, &OS_spec_dispatch_a); /* DCJM 8/5/00 */
    add_function_to_io_area(POLY_SYS_signal_handler, &Sig_dispatch_a); /* DCJM 18/7/00 */

    add_return_code();
    add_raisex();
    add_kill_self();
    {
      struct sigaction catchvec;
  
      /* disable SIGFPE while in SIGFPE handler               */
      /* we could possibly use sigset here instead, but we    */
      /* need to be sure that the signal is handled on-stack  */
  
      sigemptyset(&catchvec.sa_mask);
      sigaddset(&catchvec.sa_mask,SIGFPE);
      catchvec.sa_flags = SA_ONSTACK;
      catchvec.sa_handler = (signal_handler_type)catchFPE;
      assert(sigaction(SIGFPE, &catchvec, 0) == 0);
      /* Tell the signal handler not to mess this up. */
      markSignalInuse(SIGFPE);

      /* disable all "user" interrupts while we are in the interupt handler.
         we should be able to call init_asyncmask in run_time.c to do
         this; unfortunately, that's declared as static. - must fix that
         some time. SPF 5/7/95 */
      sigemptyset(&catchvec.sa_mask);
      sigaddset(&catchvec.sa_mask,SIGINT);
      sigaddset(&catchvec.sa_mask,SIGVTALRM);
      sigaddset(&catchvec.sa_mask,SIGALRM);
      sigaddset(&catchvec.sa_mask,SIGWINCH);
      sigaddset(&catchvec.sa_mask,SIGILL);
      sigaddset(&catchvec.sa_mask,SIGFPE);

#ifdef LINUX
      catchvec.sa_flags = SA_ONSTACK | SA_RESTART;
#else
      catchvec.sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
#endif
#ifdef MACOSXPRE102
      catchvec.sa_flags |= SV_SAVE_REGS;
#endif
      catchvec.sa_handler = (signal_handler_type)catchTRAP;
      assert(sigaction(SIGTRAP, &catchvec, 0) == 0);
      markSignalInuse(SIGTRAP);

#ifdef LINUX
      catchvec.sa_flags = SA_ONSTACK | SA_RESTART;
#else
      catchvec.sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
#endif
#ifdef MACOSXPRE102
      catchvec.sa_flags |= SV_SAVE_REGS;
#endif
      catchvec.sa_handler = (signal_handler_type)catchILL;
      assert(sigaction(SIGILL, &catchvec, 0) == 0);
      markSignalInuse(SIGILL);

      /* Handle writes to references in the database area. */
      sigemptyset(&catchvec.sa_mask);
#ifdef LINUX
      catchvec.sa_flags = SA_ONSTACK | SA_RESTART;
#else
      catchvec.sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
#endif
#ifdef MACOSXPRE102
      catchvec.sa_flags |= SV_SAVE_REGS;
#endif
      catchvec.sa_handler = (signal_handler_type)catchSEGV;
      assert(sigaction(SIGSEGV, &catchvec, 0) == 0);
      markSignalInuse(SIGSEGV);

#ifdef MACOSX
      /* Mac OS X follows FreeBSD in generating a SIGBUS rather than a SIGSEGV when we
         write to a database mutable which is write-protected. */
      assert(sigaction(SIGBUS, &catchvec, 0) == 0);
      markSignalInuse(SIGBUS);
#endif
    }
    return;
}

/******************************************************************************/
/*                                                                            */
/*      add_indirection - utility function                                    */
/*                                                                            */
/******************************************************************************/
static void add_indirection(int rts_code, word *addr, int name_word)
{
  word *base = interface_map[rts_code];

  /* lis r0,hi(addr) i.e addis r0,r0,hi(addr) */
  int instr0 =
    (15 << 26) | (0 << 21) | (0 << 16) | (((int)addr >> 16) & 0xffff);
    
  /* ori r0,r0,low(addr) */
  int instr1 =
    (24 << 26) | (0 << 21) | (0 << 16) | ((int)addr & 0xffff);
    
  /* mtctr r0 i.e. mtspr 9,r0 */
  int instr2 =
    (31 << 26) | (0 << 21) | (9 << 16) | (0 << 11) | (467 << 1) | 0;
  
  /* bctr ie bcctr 20,0 */
  int instr3 =
    (19 << 26) | (20 << 21) | (0 << 16) | (0 << 11) | (528 << 1) | 0;
  
  /* Check we've got word-aligned code addresses */
  assert(ALIGNED(base)); 
  assert(ALIGNED(addr)); 
  
  /*
     We can't use "b addr" because the offset is likely
     to be too large. Instead, we generate:
       lis	r0,hi(addr)
       ori	r0,r0,low(addr)
       mtctr	r0
       bctr
  */
  base[0] = instr0;       /* lis r0,hi(addr)     */
  base[1] = instr1;       /* ori r0,r0,low(addr) */
  base[2] = instr2;       /* mtctr r0            */
  base[3] = instr3;       /* bctr                */
  base[4] = 0;            /* marker word         */
  base[5] = 20;           /* bytes to the start  */
  base[6] = 0;            /* profile count       */
  base[7] = name_word;    /* name                */
  
  /* Need to sync instruction cache - "just in case" */
  MD_flush_instruction_cache((char *)base, 8 * sizeof(word));
}
/******************************************************************************/
/*                                                                            */
/*      add_return_code - utility function                                    */
/*                                                                            */
/******************************************************************************/
static void add_return_code(void)
{
  add_indirection(POLY_SYS_return,(word *)return_code,TAGGED('R'));
}
  
/******************************************************************************/
/*                                                                            */
/*      add_raisex - utility function                                         */
/*                                                                            */
/******************************************************************************/
/* We don't need to save the return address on the stack, because
   this is now done in the assembly code for raisex. */
static void add_raisex(void)
{
  add_indirection(POLY_SYS_raisex,(word *)raisex,TAGGED('X'));
}

/******************************************************************************/
/*                                                                            */
/*      add_kill_self - utility function                                      */
/*                                                                            */
/******************************************************************************/
static void add_kill_self(void)
{
  add_indirection(POLY_SYS_kill_self,(word *)kill_selfa,TAGGED('K'));
}

/******************************************************************************/
/*                                                                            */
/*      MD_set_exception - called by run-time system                          */
/*                                                                            */
/******************************************************************************/
void MD_set_exception(stack, exc)
/* Set up the stack of a process to raise an exception. */
StackObject *stack;
poly_exn *exc;
{
   /* N.B. we MUST have "interface_map[POLY_SYS_raisex]", NOT
      "raisex" here, since we mustn't store the address of
      a C function in the Poly/ML heap (unless the code for
      the function is actually stored in the I/O area).
      SPF 9/8/95
    */  

    stack->p_pc     = (byte *)interface_map[POLY_SYS_raisex];
#if 0
    /* The following doesn't work if we're setting the exception on
       a stack other than the current poly_stack. SPF 31/10/95 */
    *(get_reg(GPR3)) = (word)exc; /* r3 to exception data */
#else
    /* This isn't as symbolic, but it works better! SPF 31/10/95 */
    stack->p_reg[0]  = (word)exc; /* r3 to exception data */
#endif
    return;
}

/******************************************************************************/
/*                                                                            */
/*      MD_is_exception - called by run-time system                           */
/*                                                                            */
/******************************************************************************/
int MD_is_exception(StackObject *stack)  /* Return true if MD_set_exception was called */
{
  /* SPF 29/11/93 - when GC debugging is active, this functions */
  /*                may be called before stack is initialised   */
  if (stack == 0) return 0;

  return stack->p_pc == (byte *)interface_map[POLY_SYS_raisex];
}

/******************************************************************************/
/*                                                                            */
/*      MD_reset_signals - called by run_time.c                               */
/*                                                                            */
/******************************************************************************/
void MD_reset_signals(void)
{
  /* restore default signal handling in child process after a "fork". */
  signal(SIGFPE,  SIG_DFL);
#ifdef SIGEMT
  signal(SIGEMT,  SIG_DFL);
#endif
  signal(SIGTRAP, SIG_DFL);
  signal(SIGILL,  SIG_DFL);

  /* "just in case" */
  signal(SIGBUS,  SIG_DFL);
  signal(SIGSEGV, SIG_DFL);
}


void MD_update_code_addresses(word **addr, word **old, int L, void (*op)(word **))
{
    word *end = (word*)addr + OBJ_OBJECT_LENGTH(L);
    word *pt = (word*)addr;
    assert(OBJ_IS_CODE_OBJECT(L));
    /* Find the end of the code (before the constants). */
    end -= end[-1] + 4;
    assert(*end == 0); /* This should be the marker word. */

    while (pt != end)
    {
    	word instr = *pt++;
    	if ((instr & 0xfc1f0000) == 0x3c000000) /* addis rd,r0. */
    	{
                int isSigned = (*pt & 0xfc000000) == 0x38000000; /* Add instruction. */
			int reg = (instr >> 21) & 31;
    		word *valu, *oldvalu;
			unsigned hi, lo;
    		/* Ignore the unchecked registers. */
    		if (reg == 11 || reg == 12) continue;
    		/* Next must be an ADD or OR instruction. */
    		assert((*pt & 0xfc000000) == 0x38000000 || (*pt & 0xfc000000) == 0x60000000);
    		/* Put together the two halves. */
			hi = instr & 0xffff;
			lo = *pt & 0xffff;
			if (lo >= 32768 && isSigned) hi--; /* Correct for sign extension. */
    		valu = (word*)((hi << 16) + lo);
    		oldvalu = valu;
    		/* Process this address. */
    		ForConstant(&valu, op);
			/* Put the corrected value back. */
			if (valu != oldvalu)
			{
				hi = (word)valu >> 16;
				lo = (word)valu & 0xffff;
				if ((lo & 0x8000) && isSigned) hi++; /* Adjust the for sign extension */
	    		pt[-1] = (instr & 0xffff0000) | hi;
	    		*pt = (*pt & 0xffff0000) | lo;
			}
			pt++;
    	}
    }
}

/* Store a constant at a specific address in the code.  This is used by
   the code generator.  It needs to be built into the RTS because we
   have to split the value in order to store it into two instructions.
   Separately the two values might well be invalid addresses. */
void set_code_constantc(Handle data, Handle constant, Handle offseth, Handle base)
{
    /* The offset is a byte count. */
    unsigned offset = get_C_ulong(DEREFWORDHANDLE(offseth));
    word *pointer   = &(DEREFWORDHANDLE(base)[offset/sizeof(word)]);
	unsigned c = (unsigned)DEREFWORDHANDLE(constant);
	unsigned hi = c >> 16, lo = c & 0xffff;
	if (lo & 0x8000) hi++; /* Adjust the for sign extension */
    assert((offset & 3) == 0);
	/* The first word must be addis rd,r0,0. */
	assert((pointer[0] & 0xfc1fffff) == 0x3c000000);
	/* and the next must be addi rd,rd,1. */
	assert((pointer[1] & 0xfc00ffff) == 0x38000001);
	pointer[0] |= hi;
	pointer[1] = (pointer[1] & 0xffff0000) | lo;
}

