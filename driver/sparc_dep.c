/*
    Title: 	Sparc dependent code.
    Author: 	Dave Matthews, Cambridge University Computer Laboratory
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


/*  8/10/93 SPF Added "void" and "return" to make lint happier */

#include <sys/time.h>
#include <sys/file.h>
#include <stdlib.h>
#include <signal.h>
#include <assert.h>

#ifdef SOLARIS2
/* we want to use psignal and psiginfo */
#include <siginfo.h>
#include <sys/errno.h>
#include <ucontext.h>
#endif

#include "globals.h"
#include "memory.h"
#include "mm.h"
#include "gc.h"
#include "machine_assembly.h"
#include "run_time.h"
#include "mpoly.h"
#include "arb.h"
/* added 27/11/95 SPF */
#include "diagnostics.h"
#include "objects.h"
#include "processes.h"
#include "sys.h"
#include "profiling.h"

#if defined(PORTING)
#include "int_opcodes.h"
#include "reals.h"
#include "addresses.h"
#endif

#include "proper_io.h"
#include "sighandler.h"
#include "machine_dep.h"

#if 0
#define DEBUG_INTRUCTION_TRACE 1;
#endif

/* forward declarations */
static void add_return_code(void);
static void add_raisex(void);
static void add_kill_self(void);

#if defined(PORTING)
#if defined(DEBUG_INTRUCTION_TRACE)
static void dump_states(void);
#endif /* DEBUG_INTRUCTION_TRACE */
#endif /* PORTING */

/* Note: we've reserved a slot for %o6, but we're not allowed to change it
  since it's the C stack pointer, so we really only have 17 usable
  tagged registers. SPF 30/1/97.
*/

/* Location of these registers in poly_stack->p_reg[] */ 
#define OFFSET_REGRESULT    2 /* regResult == argReg0 on SPARC */ 
#define OFFSET_ARGREG0      2
#define OFFSET_ARGREG1      3
#define OFFSET_ARGREG2      4
#define OFFSET_ARGREG3      5
#define OFFSET_REGCODE      6
#define OFFSET_REGCLOSURE   7
#define OFFSET_REGRETURN    9
/* slots 0 and 1 are used for %g1 and %g2 */


/* Note: for correct operation, we must have OFFSET_REGRESULT !=1 
   and OFFSET_REGRESULT != 5, as these numbers are hard-coded into
   the interpreter (and must be architecture-independent.
   SPF 30/1/97
*/

/* The first instruction executed after the SPARC returns is the return 
   address + 6. However, for compatibility with the POWER architecture
   that's the target of the port, we want the return addresses to be
   saved with an offset of 2, not -6. I've hacked the interpretted
   compiler to provide 2 return points, which should be enough to
   handle the difference. SPF 3/8/95 */ 
#define SAVERETURNOFFSET (-2)

/* The first instruction executed after native code returns
   is the saved return address + 6 */ 
#define RETURNOFFSET 6
#define HANDLEROFFSET -2

#define add_function_to_io_area(x,y) add_word_to_io_area(x,(word)(y))

/* This module provides various routines used by the rest of the system to do
   machine dependent operations. Their names all begin with MD. There are
   also a few such procedures written in assembly code in sparc_assembly.s */

/* This is set when poly_stack->p_pc and poly_stack->p_sp are set */
int in_run_time_system = 1; /* Start off in run-time system. */

#define VERSION_NUMBER POLY_version_number

#if defined(PORTING)
/******************************************************************************/
/*                                                                            */
/*      Extra stuff for interpreted version                                   */
/*                                                                            */
/******************************************************************************/
static int interrupt_requested  = 0;
static int profile_count_wanted = 0;

#define True      TAGGED(1)
#define False     TAGGED(0)
#define nil_value TAGGED(0)
#define addresses_equalc(x,y) (((x) == (y)) ? True : False)

#define storetrace QuickGC
#define newptr     A.M.pointer
#define locallimit A.M.bottom

extern word *save_vec[];
extern word **save_vec_addr;

static void interpreter(void);

#define NOISY 0
#define VERYNOISY 0
#define NOTIFYGC 0

#define SAVE(x) push_to_save_vec((word)(x))

/* There's a limit to the number of arguments we can pass between
   interpretted and compiled code. This limit is set by the format of
   the enter_int instruction. The limit is currently 16 (0..15 arguments), but
   we can probably increase this to 31 (0..30 arguments) providing we change
   the corresponding change to the compiler. (We can't use 32 rather than 31,
   becuause we need to reserve space in out 6-bit enter_int parameter range
   for the enter_int_handler form as well as both the enter_int_call and
   enter_int_return forms.)
   SPF 9/1/97
*/
#define MAXINTARGS 31


/******************************************************************************/
/*                                                                            */
/*      dumpregs - called by run-time system                                  */
/*                                                                            */
/******************************************************************************/
void dumpregs(char *where)
{
  int i;
  proper_printf("%s: poly_stack = 0x%08x\n", where, (int)poly_stack);
  proper_printf("poly_stack->p_pc = 0x%08x\n", (int)poly_stack->p_pc);
  proper_printf("poly_stack->p_sp = 0x%08x\n", (int)poly_stack->p_sp);
  proper_printf("poly_stack->p_hr = 0x%08x\n", (int)poly_stack->p_hr);
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
  
#if defined(DEBUG_INTRUCTION_TRACE)
  proper_printf("gc_count = %x\n", A.full_gcs + A.partial_gcs);
  proper_fflush(stdout);
  dump_states();
#endif
  
  give_stack_trace((word *)poly_stack + OBJECT_LENGTH(poly_stack));
  proper_fflush(stdout);
}
    

#endif /* PORTING */

/******************************************************************************/
/*                                                                            */
/*      MD_init_stack_frame - called by run-time system                       */
/*                                                                            */
/******************************************************************************/
void MD_init_stack_frame(StackObject *stack, Handle proc, Handle arg)
/* Initialise stack frame. */
{
   /* This code is pretty tricky. 
        (1) We pretend that the function we want to call is actually an RTS entry
            that has been interrupted, so that it gets called via the RTS call
            retry mechanism.
            
        (2) We have to ensure that if the function returns or is interrupted,
            it gets into the kill_self code.
            
        (3) This is the last exception handler on the stack, so we make it
            point at itself.
            
        (4) The SPARC currently saves the return address on the stack (which
            means it has to return via the logically unnecessary RTD0 thunk)
            while the POWER and HPPA versions store it in regReturn because that's
            just a lot more natural. One day I'll correct the SPARC version but
            that requires writing a conversion program to change the saved database
            state - a bit messy!
            
      SPF 31/1/97
   */
        
    int i;
    int stack_size = OBJ_OBJECT_LENGTH(((word*)stack)[-1]);
    stack->p_space = OVERFLOW_STACK_SIZE;
    stack->p_pc    = (byte*)TAGGED(0); /* As if we had called MD_set_for_retry. */
    stack->p_nreg  = CHECKED_REGS;
    
    /* Reset all registers since this may be an old stack frame */
    for (i = 0; i < CHECKED_REGS; i++)
    {
      stack->p_reg[i] = TAGGED(0);
    }
    stack->p_reg[OFFSET_REGCLOSURE] = (word)DEREFWORDHANDLE(proc); /* Set regClosureto the closure address. */
    stack->p_reg[CHECKED_REGS]      = UNCHECKED_REGS;
    /* If this function takes an argument store it in the argument register. */
    if (arg != 0) stack->p_reg[OFFSET_ARGREG0] = (word)DEREFWORDHANDLE(arg);
    
    /* Since they're unchecked, they don't need to be initialised,
       but set them to 0 anyway. */
    for (i = 0; i < UNCHECKED_REGS; i++)
    {
      stack->p_reg[CHECKED_REGS+1+i] = TAGGED(0);
    }
    
    /* Set up exception handler group - there's no previous handler so point
       "previous handler" pointer at itself.*/
    ((word**)stack)[stack_size-1] = &((word*)stack)[stack_size-1];
    ((word*)stack)[stack_size-2]  = (word)interface_map[POLY_SYS_kill_self] - HANDLEROFFSET;
    ((word*)stack)[stack_size-3]  = TAGGED(0); /* Default handler. */
    
    stack->p_sp = (word*)stack + stack_size - 3;
    stack->p_hr = stack->p_sp;
    
#ifdef SPARC    
    /* Return address. The SPARC puts it on the stack.*/
    stack->p_sp --;
    *(stack->p_sp) = (word)interface_map[POLY_SYS_kill_self] - SAVERETURNOFFSET;
#else
    /* POWER and HPPA put it in regReturn */
    stack->p_reg[OFFSET_REGRETURN]   = (word)interface_map[POLY_SYS_kill_self] - SAVERETURNOFFSET;   
#endif

   /* We have to pass a unit argument, but we've already done this
      by initialising argReg0 to TAGGED(0). SPF 31/1/97
   */
}

/******************************************************************************/
/*                                                                            */
/*      MD_increment_profile_count1 - called by run-time system               */
/*                                                                            */
/******************************************************************************/
void MD_increment_profile_count1(byte *pc, word *sp)
/* Called from MD_increment_profile_count which gets the sp value. */
{
/* Porting systems don't support profiling */
#if !defined(PORTING)
  if (in_run_time_system)
  {
    /* poly_stack points to poly code that called the runtime system */
    add_count(poly_stack->p_pc,poly_stack->p_sp,1);
  }
  else /* in poly code or assembly code */
  {
    if (sp >= (word *)poly_stack && sp < end_of_stack)
      add_count(pc,sp,1);
    else
      Crash("Bad sp in MD_increment_profile_count");
  }
#endif
  return;
}

#ifdef SOLARIS2
/* BSD version uses MD_increment_profile_count in sparc_assembly.s instead */
 
/******************************************************************************/
/*                                                                            */
/*      MD_increment_profile_count_using_context - called by run_time.c       */
/*                                                                            */
/******************************************************************************/
void MD_increment_profile_count_using_context(ucontext_t *context)
{
  /* It appears that g4 is stored at a certain offset in the previous frame. */
  /* NB Poly/ML uses register g4 as the stack pointer, C appears to use o6.  */
  MD_increment_profile_count1((byte *)context->uc_mcontext.gregs[REG_PC],
                              (word *)context->uc_mcontext.gregs[REG_G4]);
  return;
}
#endif

/******************************************************************************/
/*                                                                            */
/*      MD_interrupt_code1 - called by run-time system                        */
/*                                                                            */
/******************************************************************************/
void MD_interrupt_code1(word **sl)
/* Called from MD_interrupt_code in the assembly code with sl as the address
   of the saved stack limit register. */
{
/* Porting systems don't support interrupting compiled code. */
#if !defined(PORTING)
  if (! in_run_time_system) /* only if in Poly code */
  {
    word *limit  = *sl;
    word *top    = end_of_stack;
    word *bottom = (word*)poly_stack+poly_stack->p_space;
  
    if (limit == top) return;
  
    if (limit == bottom)
      *sl = top;
    else
      Crash("Unable to interrupt code: top = %p, bottom = %p, limit = %p",top,bottom,limit);
  }
#endif
  return;
}

#ifdef SOLARIS2
/* BSD version uses MD_interrupt_code in sparc_assembly.s instead */
 
/******************************************************************************/
/*                                                                            */
/*      MD_interrupt_code_using_context - called by run_time.c                */
/*                                                                            */
/******************************************************************************/
void MD_interrupt_code_using_context(ucontext_t *context)
{
  /* g5 is at a certain offset in the previous frame - we need its address. */
  MD_interrupt_code1((word **)&(context->uc_mcontext.gregs[REG_G5]));
  return;
}
#endif

word *saved_sp; /* Set by MD_trap_recovery in the run-time system. */
byte *saved_pc;

#if defined(PORTING)
static int must_interpret = 0;
#endif

/******************************************************************************/
/*                                                                            */
/*      MD_switch_to_poly - called by run-time system                         */
/*                                                                            */
/******************************************************************************/
void MD_switch_to_poly(void)
/* (Re)-enter the Poly code from C. */
{
#if defined(PORTING)
   if (must_interpret)
   {
     must_interpret = 0;
     interpreter();
   }
   else
#endif   
   {
     MD_switch_to_poly_X();
   }
}

/******************************************************************************/
/*                                                                            */
/*      MD_set_for_retry - called by run-time system                          */
/*                                                                            */
/******************************************************************************/
void MD_set_for_retry(int ioCall)
{
  poly_stack->p_pc = (byte*)TAGGED(0); /* This value is treated specially. */
  return;
}

/******************************************************************************/
/*                                                                            */
/*      decode_fpcodes - utility function                                     */
/*                                                                            */
/******************************************************************************/
static int decode_fpcodes(int code)
/* Returns the exception name for the different codes. */
{
  switch(code) {
#ifdef SOLARIS2
      case FPE_INTOVF:
	  return EXC_size;
      case FPE_INTDIV: /* integer divide by zero */
      case FPE_FLTDIV: /* floating divide by zero */
	  return EXC_divide;
      case FPE_FLTUND: /* Underflow */
	  return EXC_underflow;
      case FPE_FLTOVF: /* Overflow */
	  return EXC_overflow;
#endif
      default: /* Unknown: FPE_FLTUND, FPE_FLTRES, FPE_FLTINV, FPE_FLTSUB */
	 crash("Unknown interrupt code %d\n", (int)code);
      /*NOTREACHED*/
    }
}



#ifdef SOLARIS2

/******************************************************************************/
/*                                                                            */
/*      catchSEGV - utility function                                          */
/*                                                                            */
/******************************************************************************/
static void catchSEGV(int sig, siginfo_t  *info, ucontext_t *context)
{
  assert(sig == SIGSEGV);
  assert(info != NULL);
  assert(context != NULL);
  
  /* The code should indicate that we have incorrect permission, not
     that the address is invalid.
     This will lead to a retry with normal consequences (core dump) */
  if (info->si_code != SEGV_ACCERR || in_run_time_system)
  {
     signal(SIGSEGV,SIG_DFL);
     psiginfo(info,"catchSEGV (unrecogised instruction)");
     return;
   }

  /* NOW, we are in the run-time system */
  in_run_time_system = 1;
  
/* This piece of code is extremely messy. It has to get the state when the
   interrupt occured by unwinding the stack. It can then save the registers
   and call ``translate''. */
#define SAVED_PC  (context->uc_mcontext.gregs[REG_PC])
#define SAVED_NPC (context->uc_mcontext.gregs[REG_nPC])
#if NOISY
  proper_printf("catchSEGV: trapping pc=%08x, npc=%08x, MD_trap_handler=%08x\n",
          (int)SAVED_PC, (int)SAVED_NPC, (int)MD_trap_handler);
#endif
  poly_stack->p_pc = (byte*)SAVED_PC; /* Save trapping pc. */
  SAVED_PC         = (int)&MD_trap_handler; /* Restart in trap_handler. */
  SAVED_NPC        = SAVED_PC + 4;
#undef SAVED_PC
#undef SAVED_NPC

#if 0
     {
       int i;
       
       proper_printf("context->uc_link=0x%08x\n", (int)context->uc_link);
       proper_printf("context->uc_stack=0x%08x\n", (int)context->uc_stack);
       for (i = 0; i < NGREG; i ++)
       {
         proper_printf("context->uc_mcontext.gregs[%i]=0x%08x\n",i,
                (int)context->uc_mcontext.gregs[i]);
       }

         proper_printf("context->uc_mcontext.gwins=0x%08x\n",
                (int)context->uc_mcontext.gwins);

       proper_fflush(stdout);
     }
#endif
  return; /* "returns" to MD_trap_handler */
}

#if defined(PORTING) && 0
/******************************************************************************/
/*                                                                            */
/*      catchBUS - utility function                                           */
/*                                                                            */
/******************************************************************************/
static void catchBUS(int sig, siginfo_t  *info, ucontext_t *context)
{
  assert(sig == SIGBUS);
  assert(info != NULL);
  assert(context != NULL);
  
    if (sig             == SIGBUS     &&
        info->si_code   == BUS_OBJERR &&
        info->si_errno  == EINTR     )
    {
    
#if 1
      proper_printf("Ignoring bus error during page fault  (SunOS bug)\n");
      proper_fflush(stdout);
#endif
      return;
    }

  {
     signal(SIGBUS,SIG_DFL);

  psiginfo(info,"catchBUS");
  proper_printf("info=%8x, info->si_code=%i, info->si_addr=%8x\n",
          (int)info, (int)info->si_code, (int)info->si_addr);

#define SAVED_PC  (context->uc_mcontext.gregs[REG_PC])
#define SAVED_NPC (context->uc_mcontext.gregs[REG_nPC])
  proper_printf("catchBUS: trapping pc=%08x, npc=%08x, MD_trap_handler=%08x\n",
          (int)SAVED_PC, (int)SAVED_NPC, (int)MD_trap_handler);
  proper_fflush(stdout);
#undef SAVED_PC
#undef SAVED_NPC
     {
       int i;
       proper_printf("context=0x%08x\n", (int)context);
       proper_printf("context->uc_link=0x%08x\n", (int)(context->uc_link));
       proper_printf("context->uc_stack=0x%08x\n", (int)(context->uc_stack));
       for (i = 0; i < NGREG; i ++)
       {
         proper_printf("context->uc_mcontext.gregs[%i]=0x%08x\n",i,
                (int)(context->uc_mcontext.gregs[i]));
       }

         proper_printf("context->uc_mcontext.gwins=0x%08x\n",
                (int)(context->uc_mcontext.gwins));

       proper_fflush(stdout);
     }

     dumpregs("catchBUS");
     return;
   }
  
}
#endif

/******************************************************************************/
/*                                                                            */
/*      catchFPE - utility function                                           */
/*                                                                            */
/******************************************************************************/
/* Floating point fault handler.                                              */
static void catchFPE(int sig, siginfo_t *info, ucontext_t *context)
{
  /* It is possible for a floating point error to occur */
  /* legally in the run-time system.                    */
  assert(sig == SIGFPE);
  assert(info != NULL);
  
  /* unblock SIGFPE interrupt, since we don't "return" from handler */
  { /* use standard SYSV calls */
    sigset_t mask;
    assert(sigemptyset(&mask) == 0);
    assert(sigaddset(&mask,SIGFPE) == 0);
    assert(sigprocmask(SIG_UNBLOCK,&mask,NULL) == 0);
  }
  raise_exception0(decode_fpcodes(info->si_code));
    
  /* NOT REACHED */
  return;
}


/******************************************************************************/
/*                                                                            */
/*      catchILL - utility function                                           */
/*                                                                            */
/******************************************************************************/
static void catchILL(int sig, siginfo_t *info, ucontext_t *context)
{
  assert(sig == SIGILL);
  assert(info != NULL);
  assert(context != NULL);

#if defined(PORTING) && 0  
  psiginfo(info,"catchILL");
  proper_printf("info=%08x, info->si_code=%i, info->si_addr=%08x, context=%08x,context->uc_stack=0x%08x\n",
          (int)info, (int)info->si_code, (int)info->si_addr, (int)context,context->uc_stack);

  if (context->uc_link)
  {
     proper_printf("context->uc_link=0x%08x\n", (int)context->uc_link);
  }
#endif

  
  /* Not stack overflow? Reinstall default handler and return.     */
  /* This will lead to a retry with normal consequences (core dump) */
#if defined(PORTING)
  if (info->si_code != ILL_ILLTRP && info->si_code != ILL_ILLOPC)
#else
 if (info->si_code != ILL_ILLTRP)
#endif
  {
     signal(SIGILL,SIG_DFL);
     psiginfo(info,"catchILL (unrecogised instruction)");
     return;
   }
  
  /* Shouldn't get stack overflow in run-time system */
  if (in_run_time_system)
  {
    { /* use standard SYSV calls */
      sigset_t mask;
      assert(sigemptyset(&mask) == 0);
      assert(sigaddset(&mask,SIGILL) == 0);
      assert(sigprocmask(SIG_UNBLOCK,&mask,NULL) == 0);
    }
    
    if (A.garbage_collecting)
      proper_printf("\nStack overflow in the garbage collector.\n");
    else
      proper_printf("\nStack overflow in the runtime system.\n");

    proper_printf("You may need to increase your stack limit and try again.\n");
    proper_fflush(stdout);
    exit(1);
    /*NOTREACHED*/
  }
  
  /* NOW, we are in the run-time system */
  in_run_time_system = 1;
  
/* This piece of code is extremely messy. It has to get the state when the
   interrupt occured by unwinding the stack. It can then save the registers
   and call ``translate''. */
#define SAVED_PC  (context->uc_mcontext.gregs[REG_PC])
#define SAVED_NPC (context->uc_mcontext.gregs[REG_nPC])
#if NOISY
  proper_printf("catchILL: trapping pc=%08x, npc=%08x, MD_trap_handler=%08x\n",
          (int)SAVED_PC, (int)SAVED_NPC, (int)MD_trap_handler);
#endif
  poly_stack->p_pc = (byte*)SAVED_PC; /* Save trapping pc. */
  SAVED_PC         = (int)&MD_trap_handler; /* Restart in trap_handler. */
  SAVED_NPC        = SAVED_PC + 4;
#undef SAVED_PC
#undef SAVED_NPC

#if 0
     {
       int i;
       
       proper_printf("context->uc_link=0x%08x\n", (int)context->uc_link);
       proper_printf("context->uc_stack=0x%08x\n", (int)context->uc_stack);
       for (i = 0; i < NGREG; i ++)
       {
         proper_printf("context->uc_mcontext.gregs[%i]=0x%08x\n",i,
                (int)context->uc_mcontext.gregs[i]);
       }

         proper_printf("context->uc_mcontext.gwins=0x%08x\n",
                (int)context->uc_mcontext.gwins);

       proper_fflush(stdout);
     }
#endif
  return; /* "returns" to MD_trap_handler */

}

/******************************************************************************/
/*                                                                            */
/*      catchEMT - utility function                                           */
/*                                                                            */
/******************************************************************************/
static void catchEMT(int sig, siginfo_t *info, ucontext_t *context)
{
  assert(sig == SIGEMT);
  assert(context != NULL);

  /* shouldn't get SIGEMT from run-time system,         */
  /* so reinstall default handler and return for retry, */
  /* which should lead to core dump.                    */
  if (in_run_time_system)
    {
      signal(SIGEMT,SIG_DFL);
      return;
    }
  
  /* NOW, we are in the run-time system */
  in_run_time_system = 1;

/* This piece of code is extremely messy. It has to get the state when the
   interrupt occured by unwinding the stack. It can then save the registers
   and call ``translate''. */
#define SAVED_PC  (context->uc_mcontext.gregs[REG_PC])
#define SAVED_NPC (context->uc_mcontext.gregs[REG_nPC])
#if NOISY
  proper_printf("catchEMT: trapping pc=%08x, npc=%08x, MD_trap_handler=%08x\n",
         (int)SAVED_PC, (int)SAVED_NPC, (int)MD_trap_handler);
#endif
  poly_stack->p_pc = (byte*)SAVED_PC; /* Save trapping pc. */
  SAVED_PC         = (int)&MD_trap_handler; /* Restart in trap_handler. */
  SAVED_NPC        = SAVED_PC + 4;
#undef SAVED_PC
#undef SAVED_NPC

  return; /* "returns" to MD_trap_handler */
}

/* end of Solaris2 signal handling */
#endif

static int zero = 0;

/******************************************************************************/
/*                                                                            */
/*      get_reg - utility function                                            */
/*                                                                            */
/******************************************************************************/
static word *get_reg(int rno)
/* Returns a pointer to the register given by the 5 bit value rno. */
{
   if (8 <= rno && rno <= 23) /* %o0 - %l7 */
   {
      return &poly_stack->p_reg[rno-6];
   }

  else switch (rno) 
  {
     case 0:  /* %g0 */ return &zero;
     case 1:  /* %g1 */ return &poly_stack->p_reg[0];
     case 2:  /* %g2 */ return &poly_stack->p_reg[1];
     /* These last two are used as unchecked work registers. */
     case 28: /* %i4 */ return &poly_stack->p_reg[CHECKED_REGS+1];
     case 29: /* %i5 */ return &poly_stack->p_reg[CHECKED_REGS+2];
     case 3:  /* %g3 (hr) */ return (word*)&poly_stack->p_hr;
     case 4:  /* %g4 (sp) */ return (word*)&poly_stack->p_sp;
     default: crash("Unknown register %d at %x\n", rno, (int)(poly_stack->p_pc));
     /*NOTREACHED*/
  }
}

#if defined(PORTING)
/******************************************************************************/
/*                                                                            */
/*      enter_int - utility function                                          */
/*                                                                            */
/******************************************************************************/
static void enter_int(word instr)
/* Emulate an enter_int instruction. */
{
   /* Entering the interpreter on a call, return
      or exception handler.
      
      Calling a function involves copying the arguments
      from registers onto the stack.
      
      Returning to the interpreter involves copying the
      return value in %ret0 onto the stack. 

      The interpreter needs to know how many arguments 
      to pass to a native-code function when it calls it
      (so it can do the right stack adjustments for the call).
      Rather than encode this information in the call, it
      is encoded in the "return" form of the enter_int
      instruction, which always immediately follows the
      original call. This is a bit of a hack, but shouldn't
      cause any major problems.
      
      Note: the compiler must put a "return" form of the
      enter_int instruction after *every* non-tail call (not just
      closure calls) because the called function may itself
      tail-call a native SPARC closure.
      
      Entering an interrupt handler used to require to copy
      the exception value from regReturn to poly_stack->p_reg[0].
      This requirement has now been removed, since the
      interpreter now uses poly_stack->p_reg[OFFSET_REGRETURN]
      for the exception packet. (It shouldn't be a problem
      that this is a different location on different architectures,
      since we can't commit in a state where the exception packet
      hasn't been handled.) We still need to cleanly enter
      the interpreter though, and we generate an "enter_int 63"
      to handle this.
		 
      SPF 30/1/97
   */
      
      /* Push arguments in registers - how many? 
	 We look at the enter_int instruction to find out. SPF */
      int nargs = ((instr >> 16) & 0x3f) - 1; /* 6 bits */

      if (nargs == 2 * MAXINTARGS) /* enter interrupt handler */
      {
#if NOISY
	proper_printf("Interpreter: handling exception at %x, sp= %08x\n",
	       (int)poly_stack->p_pc, (int)poly_stack->p_sp);
#endif
      }
      else if (nargs < MAXINTARGS) /* returning from function call with n args */
      {
      /* The argument count is actually used to help interpret the
	 preceding call instruction, rather than the return itself. */
      /* push the contents of regResult onto the stack */
      *(--(poly_stack->p_sp)) = poly_stack->p_reg[OFFSET_REGRESULT];
#if NOISY
      proper_printf("Interpreter: return to code at %p after %i argument call, sp=%p, return value=%08x\n",
	     poly_stack->p_pc, nargs, poly_stack->p_sp, *(poly_stack->p_sp));
#endif
      }
    else if (nargs < 2 * MAXINTARGS) /* calling an interpretted function */
    {
      word *sp = poly_stack->p_sp;
      
      nargs = nargs - MAXINTARGS;

      if (nargs > 4)
      {
	  /* Must shift the args down to make room for the regs. */
	  int i;
	  sp -= 4;
	  for (i = 4; i < nargs; i++)
	  {
	     sp[i-4] = sp[i];
	  }
	  sp[nargs-1] = poly_stack->p_reg[OFFSET_ARGREG0];
	  sp[nargs-2] = poly_stack->p_reg[OFFSET_ARGREG1];
	  sp[nargs-3] = poly_stack->p_reg[OFFSET_ARGREG2];
	  sp[nargs-4] = poly_stack->p_reg[OFFSET_ARGREG3];
      }
      else
      {
	  if (nargs >= 1) *(--sp) = poly_stack->p_reg[OFFSET_ARGREG0];
	  if (nargs >= 2) *(--sp) = poly_stack->p_reg[OFFSET_ARGREG1];
	  if (nargs >= 3) *(--sp) = poly_stack->p_reg[OFFSET_ARGREG2];
	  if (nargs >= 4) *(--sp) = poly_stack->p_reg[OFFSET_ARGREG3];
      }
      
#ifdef SPARC
      /* Ensure the return-address is off-word aligned. N.B. don't add 2 here,
	 because the SPARC code may have already adjusted %o7 (if this is
	 a tail-call in the SPARC code). If we wanted to be fancy,
	 we could avoid this uncertainty by generating a different enterInt
	 instruction at the tail-call point for interpretted code, but the
	 following is much simpler. On the HPPA, for example, we don't
	 need to do anything special because it's the caller's (not the callee's)
	 responsibility to adjust regReturn. I can't remember what we have
	 to do on the POWER - this code is slightly diofferent on that
	 machine anyway.
	 SPF 30/1/97
      */
      poly_stack->p_reg[OFFSET_REGRETURN] |= 2; /* %o7 */
#endif

#if NOISY
      proper_printf("Interpreter: call to code at %p with %i arguments, initial sp=%p\n",
	      poly_stack->p_pc, nargs, poly_stack->p_sp);
#endif

      /* Push the return address so we can return to it. */
      *(--sp) = poly_stack->p_reg[OFFSET_REGRETURN];

      /* Push the closure pointer. Needed because the interpretted
	 code may access non-locals via its closure. SPF 29/6/95 */
      *(--sp) = poly_stack->p_reg[OFFSET_REGCLOSURE];
#if 0
      /* Zap regCode, because it may not be a proper value,
	 if we've done a SkipInd call to an "optimised" closure. */
      poly_stack->p_reg[OFFSET_REGCODE] = TAGGED(0);
#endif  
      poly_stack->p_sp = sp;
    }
    else 
      crash("Interpreter: too many arguments (%d) in function call %08x\n",
	     nargs, instr);
    
    /* set this special value as the "last instruction executed" */
    poly_stack->p_reg[1] = TAGGED(256);

#if NOISY
    proper_printf("setting flag to call interpreter\n");
    proper_fflush(stdout);
#endif
   /* We can't call interpreter directly, because this will lead to
      an eventual C stack overflow. Instead, we set a flag and do
      a LONGJMP to re_enter_poly. This then calls MD_switch_to_poly,
      which calls the interpreter (providing that the flag is set). It
      took me a *long* time to work out what was going wrong here,
      so please don't change it. SPF 5/7/95 */ 
   must_interpret = 1;
}
#endif /* PORTING */


/******************************************************************************/
/*                                                                            */
/*      emulate_trap - utility function                                       */
/*                                                                            */
/******************************************************************************/
static void emulate_trap(word instr)
/* Emulate a taddcctv or tsubcctv instruction. */
/* One or both of the arguments may be i4 or i5.  These registers are saved
   by the trap handler but are not preserved by the garbage collector.  */
{
    Handle arg1;
    Handle arg2;
    int rd;
    
    init_save_vec();
    
    rd = (instr >> 25) & 31; /* Destination register. */
    
    assert(ALIGNED(get_reg((instr >> 14) & 31))); /* SPF */
    
    arg1 = push_to_save_vec(*(get_reg((instr >> 14) & 31)) + 1);
    
    if (instr & 0x2000)
      {
	/* Immediate - the argument is the value in the instruction with
	   the bottom tag bit set. */
	if (instr & 0x1000) arg2 = push_to_save_vec(-(instr & 0xfff) + 1);
	else arg2 = push_to_save_vec((instr & 0xfff) + 1);
      }
    else 
      {
        assert(ALIGNED(get_reg(instr & 31))); /* SPF */
        arg2 = push_to_save_vec(*(get_reg(instr & 31)) + 1);
      }
    
    if (rd == 0) /* g0 */
      {
        int r;
	/* If we are putting the result into g0 it must be because we are
	   interested in the condition code.  We do a comparison rather than
	   a subtraction. */

	r = comp_longc(arg2, arg1);
	poly_stack->p_reg[CHECKED_REGS+3] = r; /* Put into condition code field. */
      }
    else if ((instr & 0x2000) == 0)
     {
 	word *res, *i4v, *i5v;

    /* We have to be careful here.  Multiplication is done by repeated addition
       using values in i4 and i5.  They are assumed to have had 1 subtracted from
       them, but if we have a garbage collection while doing the long-precision
       operation they may be left pointing in the wrong place.  We save the
       values +1 on the save vec so they will be updated if they are addresses,
       and put them back just before putting in the result. */
        i4v = (word*)push_to_save_vec(poly_stack->p_reg[CHECKED_REGS+1] + 1);
        i5v = (word*)push_to_save_vec(poly_stack->p_reg[CHECKED_REGS+2] + 1);
        
	if (instr & 0x80000) 
	  {
	    res = (word*)sub_longc(arg2, arg1);
	  }
	else
	  {
	    res = (word*)add_longc(arg2, arg1);
	  }
	
	/* Put back the values into i4 and i5, and then put in the result (into 
	   either i4 or i5). */
        poly_stack->p_reg[CHECKED_REGS+1] = *i4v-1;
        poly_stack->p_reg[CHECKED_REGS+2] = *i5v-1;
        assert(ALIGNED(get_reg(rd))); /* SPF */
	*(get_reg(rd)) = *res-1;
    }
    else { /* Adding or subtracting a constant - can't be doing a multiplication
              and we must not save i4 or i5 because only one of them is being
              used in this operation and the other may contain anything. */
	word *res;

	if (instr & 0x80000)
	  {
	    res = (word*)sub_longc(arg2, arg1);
	  }
	else
	  {
	    res = (word*)add_longc(arg2, arg1);
	  }
	/* Subtract 1 from the result (we will add it back in the next instr),
           and put it in the destination register. */
        assert(ALIGNED(get_reg(rd))); /* SPF */
	*(get_reg(rd)) = *res-1;
    }
    return;
}


/******************************************************************************/
/*                                                                            */
/*      MD_trap_handler1 - called from sparc_assembly.s                       */
/*                                                                            */
/******************************************************************************/
void MD_trap_handler1(void)
/* Called from MD_trap_handler after registers have been saved in poly_stack. */
{
    word instr; /* instruction that trapped. */
    
    in_run_time_system = 1;

    /* SPARC pc is always word aligned(?) */
    assert(ALIGNED(poly_stack->p_pc)); /* SPF */
    instr = *(word*)poly_stack->p_pc;
    
    /* Trap instructions can be as a result of stack or heap overflow,
       or may be caused by arithmetic overflows when using tagged numbers.
       Various different traps are in use.
       tlu 16 occurs as a result of stack overflow.
       tvs 16 and tnz 16 are a result of tagged word overflow.
       tsubcctv %g7,?,%g7 occurs as a result of storage allocation.
       taddcctv and tsubcctv occur as a result of arithmetic overflow. */

    /* Skip over the trap instruction. */
    poly_stack->p_pc += 4;

    if (instr == 0x8bd02010)  /* tlu 16 is stack overflow */
    {
      /* Check the size of the stack and expand if necessary. */
      /* We need to examine the previous instruction */
      /* in order to work out the required amount of space. */

      /* SPARC pc is always word aligned(?) */
      assert(ALIGNED(poly_stack->p_pc)); /* SPF */
      instr = *(word*)(poly_stack->p_pc-8);  /* get previous instruction */

      if (instr == 0x80a10005) /* cmp %g4, %g5 is normal stack check */
      {
        check_current_stack_size(poly_stack->p_sp); /* may allocate */
      }
      else if (instr == 0x80a74005) /* cmp %i5, %g5 is large stack check */
      {
        check_current_stack_size((word *)poly_stack->p_reg[CHECKED_REGS+2]);
      }
      else Crash ("Bad stack check sequence"); /* may allocate */

      /* Now handle any interrupts. This may switch processes. */
      execute_pending_interrupts();
    }
    else if ((instr & 0xfffff000) == 0x8f19e000 /* tsubcctv %g7,len,%g7 */ ||
              (instr & 0xffffe01f) == 0x8f19c01d /* tsubcctv %g7,%i5,%g7 */)
    {
      int len = ((instr & 0xfffff000) == 0x8f19e000) ? (instr & 0xfff) : poly_stack->p_reg[CHECKED_REGS+2];
      
      len >>= 2;
      
   /* proper_printf ("Trap:0x%08x = tsubcctv %%g7,%d,%%g7\n",instr,len); */
      
      if (store_profiling)
        add_count(poly_stack->p_pc, poly_stack->p_sp, len);
      else
        if (A.M.pointer >= A.M.bottom) Crash ("Spurious %%g7 trap");
      
      if (A.M.pointer < A.M.bottom)
      {
        /* Put back A.M.pointer (garbage collector expects A.M.pointer >= A.M.bottom) */
      
        A.M.pointer += len;
      
        if (A.M.pointer < A.M.bottom) Crash ("Bad length in %%g7 trap");
#if NOTIFYGC
        proper_printf("MD_trap_handler1: calling GC\n");
#endif
        QuickGC(len); /* Garbage-collect. */

        A.M.pointer -= len; /* Can now allocate it. */
      }

      /* QuickGC will check that there is space for the object so
         we can allocate it just by decrementing A.M.pointer. g6 will be
         loaded with A.M.pointer+1 in switch_to_poly and so will
         point to this new object. g7 is reloaded with the amount
         of free space. The result will be to continue as
         though the allocation had happened without a garbage-collect.
      */
    }
    else if ((instr & 0xc1f00000) == 0x81100000 /* tsubcctv or taddcctv */)
    {
      if (emulate_profiling)
        {
          add_count(poly_stack->p_pc, poly_stack->p_sp, 1);
        }
      
      emulate_trap(instr);
    }
    else if (instr == 0x8fd02010 || instr == 0x93d02010 /* tvs 16 or tnz 16 */)
    {
      raise_exception0(EXC_size);
    }    
#if defined(PORTING)
    /* For a SPARC ILLTRAP (aka UNIMP) instruction, we require
           (instr & 0xc1c00000) == 0  (=2/?5/=3/?22 bits must be 0)
       For a HPPA BREAK instruction, we require
           (instr & 0xfc001fd0) == 0  (=6/?13/=8/?5 bits must be 0)
       Combining these, we require
           (instr & 0xfdc01fd0) == 0  (=6/?1/=3/?9/=8/?5)
       We actually insist on
           (instr & 0xffc0ffff) == 0  (=10/?6/=16)
       and use the remaining 6 bits to encode enter_int's argument
    */
    else if ((instr & 0xffc0ffff) == 0)
    {
      /* Unskip trap instruction, because we want to interpret it. */
      poly_stack->p_pc -= 4;
      enter_int(instr);
    }
#endif /*PORTING */

  else if ((instr & 0xc1f80000) == 0xc0200000) /* stw. */
  {
	/* Assignment to database mutable. */
	int offset;
	word value = *(get_reg((instr >> 25) & 31));
	word baseAddr = *(get_reg((instr >> 14) & 31));
	if ((instr & 0x2000) != 0)
	{
	    /* Offset is a constant. */
	    offset = instr & 0x1fff;
	    if (offset >= 4096) offset = offset - 8192;
	}
	else offset = *(get_reg(instr & 31)); /* Index reg. */
	AssignMappedDatabaseWord(H, (word*)baseAddr, offset/sizeof(word), value);
  }

  else if ((instr & 0xc1f80000) == 0xc0280000) /* stb. */
  {
	int offset;
	word value = *(get_reg((instr >> 25) & 31));
	word baseAddr = *(get_reg((instr >> 14) & 31));
	if ((instr & 0x2000) != 0)
	{
	    /* Offset is a constant. */
	    offset = instr & 0x1fff;
	    if (offset >= 4096) offset = offset - 8192;
	}
	else offset = *(get_reg(instr & 31));
	AssignMappedDatabaseByte(H, (byte*)baseAddr, offset, value);
  }

  else Crash("Bad trap pc=%p, instr=%08x",poly_stack->p_pc-4,instr);

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
        add_function_to_io_area (i,& BadOpCode_a);
      }

    add_function_to_io_area(POLY_SYS_exit, &finisha);
    add_function_to_io_area(POLY_SYS_install_root, &install_roota);
    add_function_to_io_area(POLY_SYS_strconcat, &strconcata);
    add_function_to_io_area(POLY_SYS_alloc_store, &alloc_store);
    add_function_to_io_area(POLY_SYS_chdir, &change_dira);
    add_function_to_io_area(POLY_SYS_substring, &substringa);
    add_function_to_io_area(POLY_SYS_get_length, &get_length_a);
    add_function_to_io_area(POLY_SYS_get_flags, &get_flags_a);
    add_function_to_io_area(POLY_SYS_teststreq, &teststreq);
    add_function_to_io_area(POLY_SYS_teststrneq, &teststrneq);
    add_function_to_io_area(POLY_SYS_teststrgtr, &teststrgtr);
    add_function_to_io_area(POLY_SYS_teststrlss, &teststrlss);
    add_function_to_io_area(POLY_SYS_teststrgeq, &teststrgeq);
    add_function_to_io_area(POLY_SYS_teststrleq, &teststrleq);
    add_function_to_io_area(POLY_SYS_exception_trace, &exception_tracea);
    add_function_to_io_area(POLY_SYS_commit, &commita);
    add_function_to_io_area(POLY_SYS_set_dbentry, &set_dbentry_a);
    add_function_to_io_area(POLY_SYS_get_dbentry, &get_dbentrya);
    add_function_to_io_area(POLY_SYS_createf, &createfa);
    add_function_to_io_area(POLY_SYS_lockseg, &locksega);
    add_function_to_io_area(POLY_SYS_profiler, &profilera);
    add_function_to_io_area(POLY_SYS_is_short, &is_shorta);
    add_function_to_io_area(POLY_SYS_aplus, &add_long);
    add_function_to_io_area(POLY_SYS_aminus, &sub_long);
    add_function_to_io_area(POLY_SYS_amul, &mult_long);
    add_function_to_io_area(POLY_SYS_adiv, &div_long);
    add_function_to_io_area(POLY_SYS_amod, &rem_long);
    add_function_to_io_area(POLY_SYS_aneg, &neg_long);
    add_function_to_io_area(POLY_SYS_equala, &equal_long);
    add_function_to_io_area(POLY_SYS_ora, &or_long);
    add_function_to_io_area(POLY_SYS_anda, &and_long);
    add_function_to_io_area(POLY_SYS_xora, &xor_long);
    add_function_to_io_area(POLY_SYS_Real_str, &Real_stra);
    add_function_to_io_area(POLY_SYS_Real_geq, &Real_geqa);
    add_function_to_io_area(POLY_SYS_Real_leq, &Real_leqa);
    add_function_to_io_area(POLY_SYS_Real_gtr, &Real_gtra);
    add_function_to_io_area(POLY_SYS_Real_lss, &Real_lssa);
    add_function_to_io_area(POLY_SYS_Real_eq, &Real_eqa);
    add_function_to_io_area(POLY_SYS_Real_neq, &Real_neqa);
    add_function_to_io_area(POLY_SYS_Real_Dispatch, &Real_dispatcha);
    add_function_to_io_area(POLY_SYS_Add_real, &Real_adda);
    add_function_to_io_area(POLY_SYS_Sub_real, &Real_suba);
    add_function_to_io_area(POLY_SYS_Mul_real, &Real_mula);
    add_function_to_io_area(POLY_SYS_Div_real, &Real_diva);
    add_function_to_io_area(POLY_SYS_Neg_real, &Real_nega);
    add_function_to_io_area(POLY_SYS_Repr_real, &Real_repra);
    add_function_to_io_area(POLY_SYS_conv_real, &Real_conva);
    add_function_to_io_area(POLY_SYS_real_to_int, &Real_inta);
    add_function_to_io_area(POLY_SYS_int_to_real, &Real_floata);
    add_function_to_io_area(POLY_SYS_sqrt_real, &Real_sqrta);
    add_function_to_io_area(POLY_SYS_sin_real, &Real_sina);
    add_function_to_io_area(POLY_SYS_cos_real, &Real_cosa);
    add_function_to_io_area(POLY_SYS_arctan_real, &Real_arctana);
    add_function_to_io_area(POLY_SYS_exp_real, &Real_expa);
    add_function_to_io_area(POLY_SYS_ln_real, &Real_lna);
    add_function_to_io_area(POLY_SYS_io_operation, &io_operation);
    add_function_to_io_area(POLY_SYS_fork_process, &fork_processa);
    add_function_to_io_area(POLY_SYS_choice_process, &choice_processa);
    add_function_to_io_area(POLY_SYS_int_process, &int_processa);
    add_function_to_io_area(POLY_SYS_send_on_channel, &send_on_channela);
    add_function_to_io_area(POLY_SYS_receive_on_channel, &receive_on_channela);

    add_function_to_io_area(POLY_SYS_offset_address, &offset_address);
    add_function_to_io_area(POLY_SYS_shift_right_word, &shift_right_word);
    add_function_to_io_area(POLY_SYS_word_neq, &word_neq);
    add_function_to_io_area(POLY_SYS_not_bool, &not_bool);
    add_function_to_io_area(POLY_SYS_string_length, &string_length);
    add_function_to_io_area(POLY_SYS_int_eq, &int_eq);
    add_function_to_io_area(POLY_SYS_int_neq, &int_neq);
    add_function_to_io_area(POLY_SYS_int_geq, &int_geq);
    add_function_to_io_area(POLY_SYS_int_leq, &int_leq);
    add_function_to_io_area(POLY_SYS_int_gtr, &int_gtr);
    add_function_to_io_area(POLY_SYS_int_lss, &int_lss);
    add_function_to_io_area(POLY_SYS_string_sub, &string_sub);
    
    add_function_to_io_area(POLY_SYS_or_word, &or_word);
    add_function_to_io_area(POLY_SYS_and_word, &and_word);
    add_function_to_io_area(POLY_SYS_xor_word, &xor_word);
    add_function_to_io_area(POLY_SYS_shift_left_word, &shift_left_word);
    add_function_to_io_area(POLY_SYS_word_eq, &word_eq);
    add_function_to_io_area(POLY_SYS_load_byte, &load_byte);
    add_function_to_io_area(POLY_SYS_load_word, &load_word);
    add_function_to_io_area(POLY_SYS_is_big_endian, &is_big_endian);
    add_function_to_io_area(POLY_SYS_bytes_per_word, &bytes_per_word);
    
    add_function_to_io_area(POLY_SYS_assign_byte, &assign_byte);
    add_function_to_io_area(POLY_SYS_assign_word, &assign_word);

    add_function_to_io_area(POLY_SYS_objsize, & objsize_a ); /* MJC 27/04/88 */
    add_function_to_io_area(POLY_SYS_showsize,& showsize_a); /* MJC 09/03/89 */

	/* POLY_SYS_version_number has now been replaced by a call to process_env.
	   DCJM 26/1/01. */
    add_word_to_io_area(POLY_SYS_version_number,TAGGED(VERSION_NUMBER));  /* MJC 05/07/90 */
    add_function_to_io_area(POLY_SYS_timing_dispatch, & timing_dispatch_a); /* DCJM 10/4/00 */
    add_function_to_io_area(POLY_SYS_getdbasetime,   & get_dbasetime_a);  /* MJC 15/09/89 */
    
    add_function_to_io_area (POLY_SYS_interrupt_console_processes,
                        & interrupt_console_processes_a); /* MJC 01/08/90 */
    
    add_function_to_io_area(POLY_SYS_install_subshells, & install_subshells_a); /* MJC 12/09/90 */

    add_function_to_io_area(POLY_SYS_XWindows,& XWindows_a); /* MJC 27/09/90 */

    add_function_to_io_area(POLY_SYS_full_gc,     & full_gc_a);     /* MJC 18/03/91 */
    add_function_to_io_area(POLY_SYS_stack_trace, & stack_trace_a); /* MJC 18/03/91 */

    add_function_to_io_area(POLY_SYS_foreign_dispatch, &foreign_dispatch_a); /* NIC 22/04/94 */
    add_function_to_io_area(POLY_SYS_callcode_tupled,  &callcode_tupled);    /* SPF 07/07/94 */
    add_function_to_io_area(POLY_SYS_process_env,      &process_env_dispatch_a); /* DCJM 25/4/00 */
    add_function_to_io_area(POLY_SYS_set_string_length, &set_string_length_a); /* DCJM 28/2/01 */
    add_function_to_io_area(POLY_SYS_get_first_long_word, &get_first_long_word_a); /* DCJM 28/2/01 */
    add_function_to_io_area(POLY_SYS_shrink_stack,     &shrink_stack_a);     /* SPF  9/12/96 */
    add_function_to_io_area(POLY_SYS_set_flags,        &set_flags_a);        /* SPF 12/02/97 */
    
    add_function_to_io_area(POLY_SYS_shift_right_arith_word, &shift_right_arith_word); /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_int_to_word,      &int_to_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_set_code_constant,&set_code_constanta); /* DCJM 2/1/01 */
    add_function_to_io_area(POLY_SYS_move_bytes,       &move_bytes);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_move_words,       &move_words);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_mul_word,         &mul_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_plus_word,        &plus_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_minus_word,       &minus_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_div_word,         &div_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_mod_word,         &mod_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_word_geq,		   &word_geq);
    add_function_to_io_area(POLY_SYS_word_leq,		   &word_leq);
    add_function_to_io_area(POLY_SYS_word_gtr,		   &word_gtr);
    add_function_to_io_area(POLY_SYS_word_lss,		   &word_lss);

    add_function_to_io_area(POLY_SYS_io_dispatch, &IO_dispatch_a); /* DCJM 8/5/00 */
    add_function_to_io_area(POLY_SYS_network, &Net_dispatch_a); /* DCJM 8/5/00 */
    add_function_to_io_area(POLY_SYS_os_specific, &OS_spec_dispatch_a); /* DCJM 8/5/00 */
    add_function_to_io_area(POLY_SYS_signal_handler, &Sig_dispatch_a); /* DCJM 18/7/00 */

    add_return_code();
    add_raisex();
    add_kill_self();
    {

#ifdef SOLARIS2
      /* Solaris 2 */
      struct sigaction catchvec;
  
      /* disable SIGFPE while in SIGFPE handler               */
      /* we could possibly use sigset here instead, but we    */
      /* need to be sure that the signal is handled on-stack  */
  
      sigemptyset(&catchvec.sa_mask);
      sigaddset(&catchvec.sa_mask,SIGFPE);
      catchvec.sa_flags = SA_ONSTACK | SA_SIGINFO;
      catchvec.sa_handler = catchFPE;
      assert(sigaction(SIGFPE, &catchvec, 0) == 0);
	  markSignalInuse(SIGFPE);
      
      /* disable all interrupts while we are in the interupt handler  */
      sigfillset(&catchvec.sa_mask);
      catchvec.sa_flags = SA_ONSTACK | SA_SIGINFO | SA_RESTART;
      catchvec.sa_handler = catchEMT;
      assert(sigaction(SIGEMT, &catchvec, 0) == 0);
	  markSignalInuse(SIGEMT);

      sigfillset(&catchvec.sa_mask);
      catchvec.sa_flags = SA_ONSTACK | SA_SIGINFO | SA_RESTART;
      catchvec.sa_handler = catchILL;
      assert(sigaction(SIGILL, &catchvec, 0) == 0);
	  markSignalInuse(SIGILL);

      /* SIGSEGV can be raised as a result of assigning into the database area. */
      sigemptyset(&catchvec.sa_mask);
      catchvec.sa_flags = SA_ONSTACK | SA_SIGINFO;
      catchvec.sa_handler = catchSEGV;
      assert(sigaction(SIGSEGV, &catchvec, 0) == 0);
	  markSignalInuse(SIGSEGV);

#if defined(PORTING) && 0
      /* temporary */
      sigemptyset(&catchvec.sa_mask);
      catchvec.sa_flags = SA_ONSTACK | SA_SIGINFO;
      catchvec.sa_handler = catchBUS;
      assert(sigaction(SIGBUS, &catchvec, 0) == 0);
	  markSignalInuse(SIGBUS);
#endif
#endif
    }
    return;
}

/******************************************************************************/
/*                                                                            */
/*      add_return_code - utility function                                    */
/*                                                                            */
/******************************************************************************/
static void add_return_code(void)
{
  word *base   = (word*)interface_map[POLY_SYS_return];
  word *addr   = (word *)&return_code;
  word  offset = (addr-base-2) & 0x3fffffff;
  
  base[0] = 0x01000000;          /* nop  */
  base[1] = 0x01000000;          /* nop  */
  base[2] = 0x40000000 | offset; /* call */
  base[3] = 0x01000000;          /* nop  */
  base[4] = 0;                   /* marker */
  base[5] = 20;                  /* bytes to the start */
  base[6] = 0;                   /* profile count */
#if 1
  base[7] = TAGGED('R');         /* name */
#else
  base[7] = TAGGED(0);           /* anonymous - SPF 30/1/95 */
#endif
  
  return;
}

/******************************************************************************/
/*                                                                            */
/*      add_raisex - utility function                                         */
/*                                                                            */
/******************************************************************************/
/* the problem with this version occurs if the exception is raised
   from the RTS - in this case, o7 may contain 0. If we then tag
   this value to 2, exception_trace will get very confused.
   This is "solved" by the hack in MD_set_exception.
   SPF 31/10/95.
*/
static void add_raisex(void)
{
  word *base   = (word*)interface_map[POLY_SYS_raisex];
  word *addr   = (word *)&raisex;
  word  offset = (addr-base-1) & 0x3fffffff;
  
  base[0] = 0x9e13e002;          /* or %o7,2,%o7  */
  base[1] = 0xde213ffc;          /* st %o7,[%g4-4] */
  base[2] = 0x40000000 | offset; /* call */
  base[3] = 0x88013ffc;          /* add %g4,-4,%g4 (in delay slot) */
  base[4] = 0;                   /* marker */
  base[5] = 24;                  /* bytes to the start */
  base[6] = 0;                   /* profile count */
#if 1
  base[7] = TAGGED('X');         /* name */
#else
  base[7] = TAGGED(0);           /* anonymous - SPF 30/1/95 */
#endif
  
  return;
}

/******************************************************************************/
/*                                                                            */
/*      add_kill_self - utility function                                      */
/*                                                                            */
/******************************************************************************/
static void add_kill_self(void)
{
  word *base   = (word*)interface_map[POLY_SYS_kill_self];
  word *addr   = (word *)&kill_selfa;
  word  offset = (addr-base-2) & 0x3fffffff;
  
  /* The NOPs are needed, because this code kills a process under
     two circumstances:
       (1) The top-level function raises an unhandled exception.
       (2) The top-level function returns.
     In both cases, we store "(word)base+2" as the address, but
     in case (1) we'll start the handler at ((word)base+2) + HANDLEROFFSET)
     but in case (2) we'll return to ((word)base+2) + RETURNOFFSET), which
     causes us to skip the first 2 instructions - that's why they're NOPs.
     SPF 30/1/97
     
     We now zap the return address from our call to kill_selfa. This
     is turning up in copygc and blowing an assertion. This shouldn't
     happen since the stack for this process should become unreachable,
     but something clearly isn't working properly. Until I find it,
     zapping this address is the best work-around I can find.
     SPF 2/12/1997 
  */
      
  
  base[0] = 0x01000000;          /* nop  */
  base[1] = 0x01000000;          /* nop  */
  base[2] = 0x40000000 | offset; /* call */
  base[3] = 0x9e100000;          /* mov %g0,%o7 (NOT nop) */
  base[4] = 0;                   /* marker */
  base[5] = 20;                  /* bytes to the start */
  base[6] = 0;                   /* profile count */
#if 1
  base[7] = TAGGED('K');         /* name */
#else
  base[7] = TAGGED(0);           /* anonymous - SPF 30/1/95 */
#endif
  
  return;
}

/******************************************************************************/
/*                                                                            */
/*      MD_set_exception - called by run-time system                          */
/*                                                                            */
/******************************************************************************/
void MD_set_exception(StackObject *stack, poly_exn *exc)
/* Set up the stack of a process to raise an exception. */
{
   /* N.B. we MUST have "interface_map[POLY_SYS_raisex]", NOT
      "raisex" here, since we mustn't store the address of
      a C function in the Poly/ML heap (unless the code for
      the function is actually stored in the I/O area).
      SPF 9/8/95
      
      Note: the exception packet is passed in regResult, NOT
      argReg0. This makes a difference on some architectures.
      SPF 30/1/97
    */  

    stack->p_pc     = (byte *)interface_map[POLY_SYS_raisex];
    stack->p_reg[OFFSET_REGRESULT] = (word)exc;

#ifdef SPARC
  /* Copy the program counter into %o7. This is needed if we
     are raising an exception from the RTS (as opposed to
     calling raise_exc from compiled code), as otherwise %o7
     could contain 0, which, when tagged as a code pointer, confuses
     exception_trace. I'me not really happy with this, but
     it seems to work. Better ideas welcome. SPF 31/10/95.
     
     I think this problem is due to the fact that the SPARC
     assembly code saves the return addresses on the stack rather than
     in stack->p_reg[OFFSET_REGRETURN], which it zaps. I may
     have to come back to this annoyance and fix it properly.
     SPD 30/1/97
  */ 
    stack->p_reg[OFFSET_REGRETURN] = (word)stack->p_pc;
#endif
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

#if defined(PORTING)
/******************************************************************************/
/*                                                                            */
/*      string_test - utility function                                        */
/*                                                                            */
/******************************************************************************/

static int string_test
(
  word    *arg_x,
  word    *arg_y,
  word    *sp,
  byte    *pc,
  int      li
)
/* Returns -1, 0, +1 if the first string is less, equal to or greater than the
   second. These are addresses of the strings because calling
   fix_persistent_address could result in a garbage-collection which could
   move the other string. */
{
  static STRING s_test_x;
  static STRING s_test_y;
  static pstring s_x_addr = &s_test_x;
  static pstring s_y_addr = &s_test_y;
  
  pstring *x = (pstring *)arg_x;
  pstring *y = (pstring *)arg_y;

    int i;
    /* Deal with single characters. */
    if (IS_INT(*x))
    {
       s_test_x.length = 1;
       s_test_x.chars[0] = (char)UNTAGGED((word)*x);
       x = &s_x_addr;
    }
        
    if (IS_INT(*y))
    {
       s_test_y.length = 1;
       s_test_y.chars[0] = (char)UNTAGGED((word)*y);
       y = &s_y_addr;
    }
        
    /* Now do the comparison. */
    for(i = 0; i < (*x)->length && i < (*y)->length; i++)
    {
        if ((*x)->chars[i] != (*y)->chars[i])
        {
            return (*x)->chars[i] < (*y)->chars[i] ? -1 : 1;
        }
    }
            
    /* They must be equal or one must be a leading substring of the other. */
    if (i < (*x)->length) return 1; /* y must be the substring. */
    else if (i < (*y)->length) return -1; /* x must be the substring */
    else return 0; /* They must be equal. */
}


#if defined(DEBUG_INTRUCTION_TRACE)

/******************************************************************************/
/*                                                                            */
/*      savestate                                                             */
/*                                                                            */
/******************************************************************************/

#define LOGSIZE 1000

typedef struct
{
  int gc_count;
  StackObject *stack_addr;
  int size;
  word space;
  byte *pc;
  word *sp;
  word *hr;
  word stack[10];
  word li;
} save_struct;

static save_struct save[LOGSIZE];
static save_index = 0;

static void save_state(int li)
{
  int i;
  
  save[save_index].gc_count  = A.full_gcs + A.partial_gcs;
  save[save_index].stack_addr = poly_stack;
  save[save_index].size  = OBJ_OBJECT_LENGTH(((word *)poly_stack)[-1]);
  save[save_index].space = poly_stack->p_space;
  save[save_index].pc    = poly_stack->p_pc;
  save[save_index].sp    = poly_stack->p_sp;
  save[save_index].hr    = poly_stack->p_hr;
  save[save_index].li    = li;
  
  for (i = -2; i < 8; i ++)
  {
     save[save_index].stack[i+2] = poly_stack->p_sp[i];
  }
  
  save_index = (save_index + 1) % LOGSIZE;
}

static void dump_state(int save_index)
{
  int i;

  proper_printf("gc_count = %i\n",save[save_index].gc_count);
  proper_printf("poly_stack = %p\n",save[save_index].stack_addr);
  proper_printf("objsize(poly_stack) = %x\n",save[save_index].size);
  proper_printf("poly_stack->p_space = %x\n",save[save_index].space);
  proper_printf("poly_stack->p_pc = %p\n",save[save_index].pc);
  proper_printf("poly_stack->p_sp = %p\n",save[save_index].sp);
  proper_printf("poly_stack->p_hr = %p\n",save[save_index].hr);
  proper_printf("li = %x\n",save[save_index].li);
  
  for (i = -2; i < 8; i ++)
  {
    proper_printf("poly_stack->p_sp[%i] = %x\n",i,save[save_index].stack[i+2]);
  }
  proper_fflush(stdout);
}

static void dump_states(void)
{
  int i;

  for (i = 0; i < LOGSIZE; i++)
  {
    proper_printf("\n\n**** state[-%i] ****\n",LOGSIZE-i);
    dump_state((save_index + i) % LOGSIZE);
  }

}
#endif

/******************************************************************************/
/*                                                                            */
/*      interpreter - the basic instruction interpreter                       */
/*                                                                            */
/******************************************************************************/
static void interpreter(void)
{
    register word *sp; /* Stack pointer. */
    register byte *pc; /* Program counter. */
    register word *t,u; /* Temporary registers */
    register word *sl; /* Stack limit register. */
    register int  li;  /* Last instruction. */
    register byte *savepc; /* PC value to save on calls */

#define arg1    pc[0] + pc[1]*256
#define arg2    pc[2] + pc[3]*256
#define arg3    pc[4] + pc[5]*256

    RESTART: /* Load or reload the registers and run the code. */
    sp = poly_stack->p_sp; /* Reload these. */
    pc = poly_stack->p_pc;
    li = UNTAGGED(poly_stack->p_reg[1]);
    sl = (word*)poly_stack + OVERFLOW_STACK_SIZE;
    
    if (li != 256) goto RETRY; /* Re-execute instruction if necessary. */

    for(;;){ /* Each instruction */

    li = *pc++; /* Get next instruction. */

    RETRY:
#if 0 || defined(DEBUG_INTRUCTION_TRACE)
    poly_stack->p_sp = sp;
    poly_stack->p_pc = pc;
    poly_stack->p_reg[5] = TAGGED(li);
#if defined(DEBUG_INTRUCTION_TRACE)
    save_state(li);
#endif
#endif

    /* Check for stack overflow and interrupts. These can be done less
       frequently than every instruction. */
    if (sp < sl)
    {
       poly_stack->p_sp = sp;
       poly_stack->p_pc = pc;
       poly_stack->p_reg[1] = TAGGED(li);
       check_current_stack_size(sp);
       goto RESTART;
    }

    if (interrupt_requested)
    {
       interrupt_requested = 0;
       poly_stack->p_sp = sp;
       poly_stack->p_pc = pc;
       poly_stack->p_reg[1] = TAGGED(li);
       execute_pending_interrupts();
       goto RESTART;
    }

    if (profile_count_wanted)
    {
      add_count(pc, sp, profile_count_wanted);
      profile_count_wanted = 0;
    }

#if VERYNOISY
    proper_printf("pc=%p, sp=%p, ins=%x, sp[0]=%x, sp[1]=%x, sp[2]=%x\n",
       pc, sp, li, sp[0], sp[1], sp[2]);
    proper_fflush(stdout);
#if 0
    dumpregs("new instruction");
#endif
#endif
    switch(li) {

        case INSTR_enter_int: pc += 3; /* Skip the rest of the instruction. */ break;

        case INSTR_jump_false:
            u = *sp++; /* Pop argument */
            if (u == True) { pc += 1; break; }
            /* else - false - take the jump */

        case INSTR_jump: pc += *pc + 1; break;

        case INSTR_push_handler: /* Save the old handler value. */
            *(--sp) = (word)poly_stack->p_hr; /* Push old handler */
            break;

        case INSTR_set_handler: /* Set up a handler */
            *(--sp) = (word)(pc + *pc + 1) - HANDLEROFFSET; /* Address of handler */
            poly_stack->p_hr = sp-1; /*Point to identifier about to be pushed*/
            pc += 1;
            break;

        case INSTR_del_handler: /* Delete handler retaining the result. */
            u = *sp++;
            sp = poly_stack->p_hr;
            while((t = (word*)*sp) < sp || t > end_of_stack) sp++;
            poly_stack->p_hr = t;
            *sp = u;
            pc += *pc + 1; /* Skip the handler */
            break;

        case INSTR_jump_i_false:
            if (*sp++ == True) { pc += 1; break; }
            /* else - false - take the jump */

        case INSTR_jump_i: /* Indirect jump */
            pc += *pc + 1;
            /* This may jump backwards. */
            u = arg1; if (u > 32767) u -= 65536; pc += u + 2; break;

        case INSTR_set_handler_i: /* Set up a handler */
            u = (word)(pc + *pc + 1);
            *(--sp) = /* Address of handler */
                (word)((byte*)u + ((byte*)u)[0] + ((byte*)u)[1]*256 + 2) - HANDLEROFFSET;
            poly_stack->p_hr = sp-1;
            pc += 1;
            break;

        case INSTR_del_handler_i: /* Delete handler retaining the result. */
            u = *sp++;
            sp = poly_stack->p_hr;
            while((t = (word*)*sp) < sp || t > end_of_stack) sp++;
            poly_stack->p_hr = t;
            *sp = u;
            pc += *pc + 1; /* Skip the handler */
            pc += arg1 + 2;
            break;

        case INSTR_case:
            u = UNTAGGED((word)*sp++); /* Get the value */
            if (u > arg1 || u < 0) pc += (arg1+2)*2; /* Out of range */
            else {
                pc += 2;
                pc += /* Index */pc[u*2]+pc[u*2 + 1]*256; }
            break;

        case INSTR_call_sl: /* Static link call */
            /* Get static link value. */
            t = sp+arg2;
            for(u = 1; u <= arg3; u++) t = (word*)t[-1];
            u = (word)(pc+arg1+2); /* Get entry point. */
            /* Push return address to point after instruction. */
            pc += 6;
            assert(ALIGNED(pc));
            assert(pc[0] == INSTR_enter_int && pc[1] <= MAXINTARGS);
            assert(pc[8] == INSTR_enter_int && pc[9] == pc[1]);
            *(--sp) = (word)(pc - SAVERETURNOFFSET);
            *(--sp) = (word)t; /* Push static link */
            pc = *(byte**)u;
            break;

/* new code copied from interpret.c */
        case INSTR_tail_3_b:
           u = 3;
           t = sp + u;
           sp = t + *(pc++); /* SPF */
           goto TAIL_CALL;

        case INSTR_tail_3_2:
           u = 3;
           t = sp + u;
           sp = t + 2;
           goto TAIL_CALL;

        case INSTR_tail_3_3:
           u = 3;
           t = sp + u;
           sp = t + 3;
           goto TAIL_CALL;

        case INSTR_tail_4_b:
           u = 4;
           t = sp + u;
           sp = t + *(pc++); /* SPF */
           goto TAIL_CALL;

        case INSTR_tail_b_b:
           u = *pc++; /* SPF */
           t = sp + u;
           sp = t + *(pc++); /* SPF */
           goto TAIL_CALL;

        case INSTR_tail:
           /* Tail recursive call. */
           /* Move items up the stack. */
           /* There may be an overlap if the function we are calling
              has more args than this one. */
           u = arg1;
           t = sp + u;
           sp = t + arg2;
           pc += 4; /* SPF 7/7/95 */
           
           TAIL_CALL: /* For general case. */
           if (u < 2) crash("Invalid argument\n");
           for (; u > 0; u--) *(--sp) = *(--t);
           
           /* At this point, stack contains:
              (top) return_address; closure; args ... (bottom)
           */
           assert(ALIGNED(pc));
           assert(pc[0] == INSTR_enter_int && pc[1] <= MAXINTARGS);
           assert(pc[8] == INSTR_enter_int && pc[9] == pc[1]);
           savepc = (byte*)*sp++; /* Pop the original return address. */
           assert(ALIGNED(savepc + RETURNOFFSET));
           li = INSTR_call_closure; /* If we have to re-execute. */
           goto CALL_CLOSURE;  /* SPF */
           
/* end of new code */

        case INSTR_call_closure: /* Closure call - may be machine code. */
            assert(ALIGNED(pc));
            assert(pc[0] == INSTR_enter_int && pc[1] <= MAXINTARGS);
            assert(pc[8] == INSTR_enter_int && pc[9] == pc[1]);
            savepc = pc - SAVERETURNOFFSET; /* SPF */

        CALL_CLOSURE:  /* Jumped to from INSTR_tail */
           /* At this point, stack contains:
              (top) closure; args ... (bottom)
              and savepc is where we want to return to after the call
           */
            t = (word*)*sp; /* Closure */
            u = *t;         /* Code address. */
            assert(ALIGNED(u));
            
            if ((unsigned)IO_BOTTOM <= (unsigned)t && (unsigned)t < (unsigned)IO_TOP) 
            {
#if NOISY
              {
                 int callno = ((unsigned)t - (unsigned)IO_BOTTOM) / (8 * sizeof(word));
                 proper_printf("RTS call 0x%02x (%i)\n", callno, callno);
                 proper_fflush(stdout);
              }
#endif
              /* it's a run-time call - can we execute it in-line? */
              sp++; /* remove the closure from the stack */
              switch(((unsigned)t - (unsigned)IO_BOTTOM) / (8 * sizeof(word)))
              {
                case POLY_SYS_strconcat:
                  /* ensure we don't run out of save-vec entries */
                  init_save_vec();
                  
                  /* save state in case of GC */
                  *(--sp) = (int)savepc;
                  poly_stack->p_sp = sp;
                  poly_stack->p_pc = pc;
                  
                  /* N.B. strconcatc mustn't process-switch */
                  t = (int *)strconcatc(SAVE(sp[1]),SAVE(sp[2]));
                  
                  /* restore state after possible GC */
                  pc = poly_stack->p_pc;
                  sp = poly_stack->p_sp;
                  savepc = (byte *)*sp;
                  sp += 2;
                  *sp = *t;
                  break;

                case POLY_SYS_substring:
                  init_save_vec();
                  
                  /* save state in case of GC */
                  *(--sp) = (int)savepc;
                  poly_stack->p_sp = sp;
                  poly_stack->p_pc = pc;
                  
                  /* N.B. strconcatc mustn't process-switch */
                  t = (int *)substringc(SAVE(sp[1]),SAVE(sp[2]),SAVE(sp[3]));
                  
                  /* restore state after possible GC */
                  pc = poly_stack->p_pc;
                  sp = poly_stack->p_sp;
                  savepc = (byte *)*sp;
                  sp += 3;
                  *sp = *t;
                  break;

                case POLY_SYS_amul:
                  /* ensure we don't run out of save-vec entries */
                  init_save_vec();
                  
                  /* save state in case of GC */
                  *(--sp) = (int)savepc;
                  poly_stack->p_sp = sp;
                  poly_stack->p_pc = pc;
                  
                  /* N.B. mult_longc mustn't process-switch */
                  t = (int *)mult_longc(SAVE(sp[1]),SAVE(sp[2]));
                  
                  /* restore state after possible GC */
                  pc = poly_stack->p_pc;
                  sp = poly_stack->p_sp;
                  savepc = (byte *)*sp;
                  sp += 2;
                  *sp = *t;
                  break;

                case POLY_SYS_adiv:
                  /* ensure we don't run out of save-vec entries */
                  init_save_vec();
                  
                  /* save state in case of GC */
                  *(--sp) = (int)savepc;
                  poly_stack->p_sp = sp;
                  poly_stack->p_pc = pc;
                  
                  /* N.B. mult_longc mustn't process-switch */
                  t = (int *)div_longc(SAVE(sp[1]),SAVE(sp[2]));
                  
                  /* restore state after possible GC */
                  pc = poly_stack->p_pc;
                  sp = poly_stack->p_sp;
                  savepc = (byte *)*sp;
                  sp += 2;
                  *sp = *t;
                  break;

                case POLY_SYS_amod:
                  /* ensure we don't run out of save-vec entries */
                  init_save_vec();
                  
                  /* save state in case of GC */
                  *(--sp) = (int)savepc;
                  poly_stack->p_sp = sp;
                  poly_stack->p_pc = pc;
                  
                  /* N.B. mult_longc mustn't process-switch */
                  t = (int *)rem_longc(SAVE(sp[1]),SAVE(sp[2]));
                  
                  /* restore state after possible GC */
                  pc = poly_stack->p_pc;
                  sp = poly_stack->p_sp;
                  savepc = (byte *)*sp;
                  sp += 2;
                  *sp = *t;
                  break;

                case POLY_SYS_int_eq:
                  u = *sp++;
                  *sp = (u == *sp)?True:False;
                  break;

                case POLY_SYS_int_neq:
                  u = *sp++;
                  *sp = (u != *sp)?True:False;
                  break;

                case POLY_SYS_word_eq: 
                  u = *sp++;
                  *sp = addresses_equalc(u, *sp);
                  break;

                case POLY_SYS_word_neq:
                  u = *sp++;
                  *sp = (addresses_equalc(u, *sp) == True) ? False : True;
                  break;

                case POLY_SYS_or_word:
                  u = *sp++;
                  *sp = TAGGED(UNTAGGED(*sp) | UNTAGGED(u));
                  break; 

                case POLY_SYS_and_word: 
                  u = *sp++;
                  *sp = TAGGED(UNTAGGED(*sp) & UNTAGGED(u));
                  break; 

                case POLY_SYS_not_bool:
                  *sp = (*sp == True) ? False : True;
                  break; 


                case POLY_SYS_string_length: 
                  /* Length is first word of string unless it is a
                    single character. */
                 if (IS_INT(*sp))
                 {
                   *sp = TAGGED(1);
                 }
                 else
                 {
                   *sp = TAGGED(*(word*)(*sp));
                 }
                break; 

                case POLY_SYS_string_sub:
                { /* don't use u "register" because then we
                     can't restart with "goto USE_RTS_CALL" */
                  int u = UNTAGGED(sp[0]);
                  if (IS_INT(sp[1])) /* Single char. */
                  {
#if 0
                    if (u != 1) raise_exception0(EXC_substring);
#endif
                    if (u != 1) goto USE_RTS_CALL;
                    sp++; /* drop subscript, and return character as string */

                  }
                  else if (u > 0 && u <= ((pstring)sp[1])->length)
                  {
                     sp++; /* drop subscript */
                     *sp = TAGGED(((pstring)*sp)->chars[u-1]);
                  }
                  else
                  {
#if 0
                     raise_exception0(EXC_substring);
#endif
                     goto USE_RTS_CALL; /* to raise the exception */
                  }
                }
                  break; 


                case POLY_SYS_xor_word: 
                   u = *sp++;
                   *sp = TAGGED(UNTAGGED(*sp) ^ UNTAGGED(u));
                   break; 

                case POLY_SYS_shift_left_word: 
                  u = *sp++;
                  *sp = TAGGED(UNTAGGED(*sp) << UNTAGGED(u));
                  break; 

                case POLY_SYS_shift_right_word:
                  u = *sp++;
                  *sp = TAGGED(UNTAGGED(*sp) >> UNTAGGED(u));
                  break; 

                case POLY_SYS_load_byte:
                  u = UNTAGGED(*sp++);
                  *sp = TAGGED((*(byte**)sp)[u]);
                  break; 

                case POLY_SYS_load_word: 
                  u = UNTAGGED(*sp++);
                  *sp = (*(word**)sp)[u];
                  break; 

                case POLY_SYS_assign_byte: 
                  if (IS_LOCAL_MUTABLE(sp[2]))
                  {
                    ((byte*)sp[2])[UNTAGGED(sp[1])] = (byte)UNTAGGED(sp[0]);
                    sp += 2;
                    *sp = TAGGED(0);
                  }
                  else goto USE_RTS_CALL;
                  break;

                case POLY_SYS_assign_word: 
                  if (IS_LOCAL_MUTABLE(sp[2]))
                  {
                    ((word*)sp[2])[UNTAGGED(sp[1])] = (word)sp[0];
                    sp += 2;
                    *sp = TAGGED(0);
                  }
                  else goto USE_RTS_CALL;
                  break;

                case POLY_SYS_teststreq:
                  sp[1] = ((string_test(sp+1, sp, sp, pc, li) == 0) ? True : False);
                  sp++;
                  break;

                case POLY_SYS_teststrneq:
                  sp[1] = ((string_test(sp+1, sp, sp, pc, li) != 0) ? True : False);
                  sp++;
                  break;

                case POLY_SYS_teststrgtr:
                  sp[1] = ((string_test(sp+1, sp, sp, pc, li) > 0) ? True : False);
                  sp++;
                  break;

                case POLY_SYS_teststrlss:
                  sp[1] = ((string_test(sp+1, sp, sp, pc, li) < 0) ? True : False);
                  sp++;
                  break;

                case POLY_SYS_teststrgeq:
                  sp[1] = ((string_test(sp+1, sp, sp, pc, li) >= 0) ? True : False);
                  sp++;
                  break;

                case POLY_SYS_teststrleq:
                  sp[1] = ((string_test(sp+1, sp, sp, pc, li) <= 0) ? True : False);
                  sp++;
                  break;

#if 0
/* Use proper call, because we also have to flush the cache */
                case POLY_SYS_lockseg:
                  /* Clear the mutable bit. */
                  assert(OBJ_IS_MUTABLE_OBJECT(((word*)*sp)[-1]));
                  ((word*)*sp)[-1] &= ~OBJ_MUTABLE_BIT;
                  *sp = UNTAGGED(0);
                  break;
#endif

                case POLY_SYS_get_length: 
                  /* Return the length word. */
                  *sp = TAGGED(OBJ_OBJECT_LENGTH(((word*)*sp)[-1]));
                  break;

                case POLY_SYS_get_flags:
                  /* Return the flags. */
                  *sp = get_flags_c((word*)*sp);
                  break;

                case POLY_SYS_exit:
                  finish(0);

                case POLY_SYS_is_short:
                  *sp = IS_INT(*sp) ? True : False;
                  break;

                case POLY_SYS_io_operation:
                     /* The instruction following the call should be
                        an enter_int to tell us how many arguments
                        we're passing. */
                    assert(pc[0] == INSTR_enter_int && pc[1] <= MAXINTARGS);
                    assert(pc[8] == INSTR_enter_int && pc[9] == pc[1]);
                    assert(pc[1] == 2 /* ML */ || pc[1] == 3 /* Poly */);
                    
                    if (pc[1] == 3)
                    {
                      /* discard the second Poly argument (the Poly type) */
                      sp++; 
                    }
                    
                    *sp = (word)interface_map[UNTAGGED(*sp)];
                    break;

                case POLY_SYS_alloc_store:
                  {
                    /* don't use u "register", because then we can't
                       restart with "goto USE_RTS_CALL" */
                    int u = UNTAGGED(sp[2]);
                    newptr -= u+1;
                    if (newptr < locallimit)
                    { 
                      newptr += u+1;
#if NOTIFYGC
                      proper_printf("POLY_SYS_alloc_store: using native call (for GC)\n");
                      proper_fflush(stdout);
#endif
                      goto USE_RTS_CALL;
                    }
                    *newptr = u | (UNTAGGED(sp[1]) << 24) | OBJ_MUTABLE_BIT;
                    t = ((word *)newptr) + 1;
                    if (UNTAGGED(sp[1]) & (OBJ_BYTE_BIT >> 24))
                    {
                       sp[0] = UNTAGGED(sp[0]);
                    }
                    
                    /* New GC - Now must always initialise the store */
                    for(; --u >= 0;)
                    {
                       t[u] = sp[0];
                    }
                    /* If F_first_ptr is set the first word must point to the
                       second. */
                    if (UNTAGGED(sp[1]) & (OBJ_FIRST_BIT >> 24))
                    {
                       *t = (word)(t + 1);
                    }
                    sp += 2;
                    *sp = (word)t;
                  }
                  break;

#define SMALL(t) (-MAXTAGGED-1 <= (t) && (t) <= MAXTAGGED)
               case POLY_SYS_aplus:
                 {
                   int a2 = sp[0];
                   int a1 = sp[1];
                   
                   if (IS_INT(a1) && IS_INT(a2))
                   {
                     int res = UNTAGGED(a1) + UNTAGGED(a2);
                     
                     if (SMALL(res))
                     {
                       sp++;
                       *sp = TAGGED(res);
                     }
                     else goto USE_RTS_CALL;
                   }
                   else goto USE_RTS_CALL;
                 }
                 break;
                     
               case POLY_SYS_aminus:
                 {
                   int a2 = sp[0];
                   int a1 = sp[1];
                   
                   if (IS_INT(a1) && IS_INT(a2))
                   {
                     int res = UNTAGGED(a1) - UNTAGGED(a2);
                     
                     if (SMALL(res))
                     {
                       sp++;
                       *sp = TAGGED(res);
                     }
                     else goto USE_RTS_CALL;
                   }
                   else goto USE_RTS_CALL;
                 }
                 break;
                     
               case POLY_SYS_aneg:
                 {
                   int a1 = sp[0];
                   
                   if (IS_INT(a1))
                   {
                     int res =  - UNTAGGED(a1);
                     
                     if (SMALL(res))
                     {
                       *sp = TAGGED(res);
                     }
                     else goto USE_RTS_CALL;
                   }
                   else goto USE_RTS_CALL;
                 }
                 break;
                     
               case POLY_SYS_equala:
                 {
                   int a2 = sp[0];
                   int a1 = sp[1];
                   
                   if (IS_INT(a1))
                   {
                     if (a1 == a2)
                     {
                       sp++;
                       *sp = True;
                     }
                     else
                     {
                       sp++;
                       *sp = False;
                     }
                   }
                   else if (IS_INT(a2))
                   {
                     sp++;
                     *sp = False;
                   }
                   else /* both long */
                     goto USE_RTS_CALL;
                 }
                 break;
                 
               case POLY_SYS_int_geq:
                 {
                   int a2 = sp[0];
                   int a1 = sp[1];
                   
                   if (IS_INT(a1) && IS_INT(a2))
                   {
                     if (UNTAGGED(a1) >= UNTAGGED(a2))
                     {
                       sp++;
                       *sp = True;
                     }
                     else
                     {
                       sp++;
                       *sp = False;
                     }
                   }
                   else /* at least one long */
                     goto USE_RTS_CALL;
                 }
                 break;

               case POLY_SYS_int_leq:
                 {
                   int a2 = sp[0];
                   int a1 = sp[1];
                   
                   if (IS_INT(a1) && IS_INT(a2))
                   {
                     if (UNTAGGED(a1) <= UNTAGGED(a2))
                     {
                       sp++;
                       *sp = True;
                     }
                     else
                     {
                       sp++;
                       *sp = False;
                     }
                   }
                   else /* at least one long */
                     goto USE_RTS_CALL;
                 }
                 break;

               case POLY_SYS_int_gtr:
                 {
                   int a2 = sp[0];
                   int a1 = sp[1];
                   
                   if (IS_INT(a1) && IS_INT(a2))
                   {
                     if (UNTAGGED(a1) > UNTAGGED(a2))
                     {
                       sp++;
                       *sp = True;
                     }
                     else
                     {
                       sp++;
                       *sp = False;
                     }
                   }
                   else /* at least one long */
                     goto USE_RTS_CALL;
                 }
                 break;
                 
               case POLY_SYS_int_lss:
                 {
                   int a2 = sp[0];
                   int a1 = sp[1];
                   
                   if (IS_INT(a1) && IS_INT(a2))
                   {
                     if (UNTAGGED(a1) < UNTAGGED(a2))
                     {
                       sp++;
                       *sp = True;
                     }
                     else
                     {
                       sp++;
                       *sp = False;
                     }
                   }
                   else /* at least one long */
                     goto USE_RTS_CALL;
                 }
                 break;

#undef SMALL

                default:
                USE_RTS_CALL:
                  /* put the closure back on the stack,
                     and call it the hard way. We mustn't corrupt u if
                     we want to come here. */
                  *(--sp) = (int)t;
#if 0
                 proper_printf("Unoptimised RTS call %i\n", ((unsigned)t - (unsigned)IO_BOTTOM) / (8 * sizeof(word)));
#endif
                  goto CALL_NORMAL_CLOSURE;
              }
              /* we've executed the call - where do we resume execution? */
              /* At this point, stack contains:
                  (top) return-value ... (bottom)
              */

              /* Now return using savepc (to handle tail-calls) */
              pc = savepc + RETURNOFFSET;
              if (!ALIGNED(pc))
              {
                 dumpregs("after run_time_call");
                 proper_printf("pc=%08x, sp=%08x\n", (int)pc, (int)sp);
                 proper_fflush(stdout);
              }
              assert(ALIGNED(pc));
              
              /* we may now be executing machine code, so check ... */
              if (pc[0] == INSTR_enter_int && pc[1] < 64)
              {
                /* still interpretting - just return to main loop */
              }
              else
              {
                 /* pop return value off stack and save it in o0 */
                 poly_stack->p_reg[OFFSET_REGRESULT] = *(sp++);
                 poly_stack->p_reg[OFFSET_REGRETURN] = (int)savepc;
                 poly_stack->p_sp = sp;
                 poly_stack->p_pc = pc;
#if NOISY
                 proper_printf("Returning from RTS to address %p with %x; sp=%p\n",
                         pc, poly_stack->p_reg[OFFSET_REGRESULT], poly_stack->p_sp);
                 proper_fflush(stdout);
#endif
                 LONGJMP(re_enter_poly,44);
              }
            } /* End of system calls. */
            
            else
            {
        CALL_NORMAL_CLOSURE:
            /* normal closure call - t has closure, u has code address,
                  pc has address of real call, savepc has address to
                  save on the stack. */
              
               if (((byte*)u)[0] == INSTR_enter_int && ((byte *)u)[1] < 64)
                 /* interpretted code segment? */
                {
                   sp--;
                   sp[0] = sp[1];        /* Move closure up. */
                   sp[1] = (word)savepc; /* Save return address. */

               /* At this point, stack contains:
                  (top) closure; return-address; args ... (bottom)
                   and savepc is where we want to return to after the call */
                   pc = (byte*)u;    /* Get entry point. */
                }
                else
                 { /* It's machine code. */
                    word a0, a1, a2, a3;
                    /* Find the number of args - This call will be followed by
                       an enter_int instruction followed by a byte containing
                       the number of args. */

                     /* The instruction following the call should be
                        an enter_int to tell us how many arguments
                        we're passing. */
                    assert(pc[0] == INSTR_enter_int && pc[1] <= MAXINTARGS);
                    assert(pc[8] == INSTR_enter_int && pc[9] == pc[1]);
                    u = pc[1] - 1; /* Number of args. */

                    sp += 1; /* Remove closure. */
           /* At this point, stack contains:
              (top) args ... (bottom),
              u is the number of arguments to pass
              t is the closure to call
              and savepc is where we want to return to after the call */
                  /* The first 4 args are passed in registers, however the
                     first 4 are the furthest up the stack. */
                  if (u >= 1) a0 = sp[u-1]; else a0 = TAGGED(0);
                  if (u >= 2) a1 = sp[u-2]; else a1 = TAGGED(0);
                  if (u >= 3) a2 = sp[u-3]; else a2 = TAGGED(0);
                  if (u >= 4) a3 = sp[u-4]; else a3 = TAGGED(0);

#if NOISY
                  proper_printf("Calling %x with %d(%x,%x,%x,%x) from %p with sp=%p,savepc=%p,stack=%p\n", *t, u,
                     a0, a1, a2, a3, pc, sp, savepc,poly_stack);
                  proper_fflush(stdout);
#endif
                  while (u > 4) 
                  { 
                    sp[u-1] = sp[u-5]; u--;
                  }
                  /* proper_printf("sp=%x, sp[0]=%x, sp[1]=%x, sp[2]=%x\n",
                       sp, sp[0], sp[1], sp[2]); */
                  sp += u;
                  
                  if (!ALIGNED(pc))
                  {
                     dumpregs("calling machine code");
                     proper_printf("pc=%08x, savepc=%08x, sp=%08x\n", 
                             (int)pc, (int)savepc, (int)sp);
                  }
                  assert(ALIGNED(pc));
                  assert(ALIGNED(savepc + RETURNOFFSET));
/* passing these values on the ML stack is less work than trying
   to figure out the native C calling convention for arguments on
   the stack. It's a hack, but it's only temporary code anyway.
   SPF 23/1/97
*/
                  *(--sp) = (word)savepc;
                  *(--sp) = (word)t;
                  *(--sp) = (word)*t;
                  poly_stack->p_sp = sp;
                  poly_stack->p_pc = pc;
                  call_MC_code(a0, a1, a2, a3);
                  /* Does not return */
                }
              }
            break;

        case INSTR_return_w:
            u = arg1;  pc += 2; /* Get no. of args to remove. */

            RETURN: /* Common code for return. */
              /* At this point, stack contains:
                 (top) return-value, closure,  return-address, arg1, arg2, ... (bottom)
              */

#if NOISY
            proper_printf("RETURN; sp = %p, pc = %p\n", sp, pc);
            proper_fflush(stdout);
#endif
            t = (word*)*sp++; /* Result */
            sp++; /* Remove the link/closure */
            pc = (byte*)*sp++; /* Return address */
            sp += u; /* Add on number of args. */
            
            if ((int)pc == TAGGED(0))  /* should be 0? */
            {
               kill_selfc(); /* Exit */
            }
             
            /* We must return to the saved pc + RETURNOFFSET. */
            else
            {
              pc += RETURNOFFSET;
              if (!ALIGNED(pc))
              {
              
                dumpregs("executing return");
                proper_printf("pc=%08x, sp=%08x\n", (int)pc, (int)sp);
              }
              assert(ALIGNED(pc)); /* must be aligned on the SPARC version */
              
#if NOISY
              proper_printf("RETURN2; sp = %p, pc = %p\n", sp, pc);
              proper_fflush(stdout);
#endif
              if (pc[0] == INSTR_enter_int && pc[1] < 64)
              {
                /* Return to some more interpreted code. */
                *(--sp) = (word)t; /* Result */
              }
              
              else
              {
                 /* Returning to machine code. */
                 poly_stack->p_sp = sp;
                 poly_stack->p_pc = pc; /* SPF */
                 poly_stack->p_reg[OFFSET_REGRESULT] = (word)t;
#if NOISY
                 proper_printf("Returning to address %p with %p\n", pc, t);
                 proper_fflush(stdout);
#endif
                 LONGJMP(re_enter_poly,43);
              }
            }
            break;

        case INSTR_return_b: u = *(pc++); goto RETURN;
        case INSTR_return_0: u = 0; goto RETURN;
        case INSTR_return_1: u = 1; goto RETURN;
        case INSTR_return_2: u = 2; goto RETURN;
        case INSTR_return_3: u = 3; goto RETURN;

        case INSTR_pad: /* No-op */ break;

        case INSTR_project_w: /* Project from a union. */
            t = (word*)*sp; /* Get the union value */
            if (t[1] == TAGGED(arg1)) { /* Does the tag match? */
                *sp = t[0]; pc += 2; break; }
            else raise_exception0(EXC_project);

        case INSTR_project_b: /* Project from a union. */
            t = (word*)*sp; /* Get the union value */
            if (t[1] == TAGGED(*pc))
             {
                /* Does the tag match? */
                *sp = t[0]; pc += 1; break;
            }
            else raise_exception0(EXC_project);

        case INSTR_raise_ex: /* modified SPF */
            /* RAISE: */
            init_save_vec();
            poly_stack->p_reg[OFFSET_REGRESULT] = *sp; /* Get exception data */
            u = *(word*)*sp; /* Get exception identifier. */
            poly_stack->p_sp = sp; /* Save this in case of trace. */
            t = poly_stack->p_hr;  /* First handler */
            /* Handlers consist of one or more pairs of identifier and
               code address, followed by the address of the next handler. */
            while (*t != TAGGED(0) && addresses_equalc(*t, u) == False)
            {
                /* Loop until we find an ELSE handler or one that matches */
                t += 2; /* Go on to next. */
                /* If it points into stack it must be a pointer to the next
                   handler. */
                if ((word*)*t > t && (word*)*t < end_of_stack) t = (word*)*t;
            }
            t++; /* Skip over the identifier to point at the code address. */
            if (*t == TAGGED(0))
            { /* Trace this exception. */ 
                *sp = (word)pc; /* So that this proc. will be included. */
                poly_stack->p_hr = (word*)t[1]; /* Next handler. */
                t++; /* Next handler. */
                ex_tracec(SAVE(poly_stack->p_reg[OFFSET_REGRESULT]), t);
            }
            /* get the address of the handler */
            poly_stack->p_pc = (byte*)*t + HANDLEROFFSET;
            assert(ALIGNED(poly_stack->p_pc));
            
            /* Now remove this handler. */
            sp = t;
            while((t = (word*)*sp) < sp || t > end_of_stack) sp++;
            poly_stack->p_hr = t; /* Restore old handler */
            sp++; /* Remove that entry. */
            poly_stack->p_sp = sp;
            
            /* Restart in case this is machine code. */
            LONGJMP(re_enter_poly,1);

        case INSTR_get_store_w:
            u = arg1; t = (word*)2;

            GET_STORE: /* Common code for allocation. */
            newptr -= u+1;
            if (newptr < locallimit)
            {
#if NOTIFYGC
                proper_printf("GET_STORE(%i): calling GC\n",u);
                proper_fflush(stdout);
#endif
                poly_stack->p_sp = sp;
                poly_stack->p_pc = pc;
                poly_stack->p_reg[1] = TAGGED(li);
                newptr += u+1;
                storetrace(u);
#if NOTIFYGC
                proper_printf("GET_STORE(%i): called GC\n",u);
                proper_fflush(stdout);
#endif
                goto RESTART;
             }
            pc += (word)t;
            *newptr = (u | OBJ_MUTABLE_BIT); /* Allocation must be mutable! */
            t = newptr+1;
            for(; --u >= 0; ) t[u] = 0; /* Must initialise store! */
            *(--sp) = (word)t;
            break;

        case INSTR_get_store_2: u = 2; t = 0; goto GET_STORE;
        case INSTR_get_store_3: u = 3; t = 0; goto GET_STORE;
        case INSTR_get_store_4: u = 4; t = 0; goto GET_STORE;
        case INSTR_get_store_b: u = *pc; t = (word*)1; goto GET_STORE;


        case INSTR_tuple_w:
            u = arg1; t = (word*)2;

            TUPLE: /* Common code for tupling. */
            newptr -= u+1;
            if (newptr < locallimit)
            {
#if NOTIFYGC
                proper_printf("TUPLE(%i): calling GC\n",u);
                proper_fflush(stdout);
#endif
                poly_stack->p_sp = sp;
                poly_stack->p_pc = pc;
                poly_stack->p_reg[1] = TAGGED(li);
                newptr += u+1;
                storetrace(u);
#if NOTIFYGC
                proper_printf("TUPLE(%i): called GC\n",u);
                proper_fflush(stdout);
#endif
                goto RESTART;
             }
            pc += (word)t;
            *newptr = u;
            t = newptr+1;
            for(; --u >= 0; ) t[u] = *sp++;
            *(--sp) = (word)t;
            break;

        case INSTR_tuple_2: u = 2; t = 0; goto TUPLE;
        case INSTR_tuple_3: u = 3; t = 0; goto TUPLE;
        case INSTR_tuple_4: u = 4; t = 0; goto TUPLE;
        case INSTR_tuple_b: u = *pc; t = (word*)1; goto TUPLE;


        case INSTR_non_local:
            t = sp+arg1;
            for(u = 1; u <= arg2; u++) t = (word*)t[-1];
            u = arg3; /* Can be negative. */
            if (u > 32767) u -= 65536;
            *(--sp) = t[u];
            pc += 6;
            break;

        case INSTR_local_w:
            u = sp[arg1]; *(--sp) = u; pc += 2; break;

        case INSTR_indirect_w:
            *sp = ((word*)*sp)[arg1]; pc += 2; break;

        case INSTR_move_to_vec_w:
            u = *sp++; ((word*)*sp)[arg1] = u; pc += 2; break;

        case INSTR_set_stack_val_w: u = *sp++; sp[arg1-1] = u; pc += 2; break;

        case INSTR_reset_w: sp += arg1; pc += 2; break;

        case INSTR_reset_r_w: u = *sp; sp += arg1; *sp = u; pc += 2; break;

        case INSTR_const_addr: *(--sp) = *(word*)(pc + arg1 + 2); pc += 2; break;

        case INSTR_const_int_w: *(--sp) = TAGGED(arg1); pc += 2; break;

        case INSTR_io_vec_entry:
            *(--sp) = (word)interface_map[*pc]; pc += 1; break;

        case INSTR_const_nil: *(--sp) = nil_value; break;

        case INSTR_jump_back: pc -= *pc + 1; break;

        case INSTR_lock: ((word*)*sp)[-1] &= ~OBJ_MUTABLE_BIT; break;

        /* The exception value is passed in regResult, which is
           at different locations on deifferent machines.
           SPF 30/1/97
        */
        case INSTR_ldexc: *(--sp) = poly_stack->p_reg[OFFSET_REGRESULT]; break;

        case INSTR_local_b: u = sp[*pc]; *(--sp) = u; pc += 1; break;

        case INSTR_indirect_b:
            *sp = ((word*)*sp)[*pc]; pc += 1; break;

        case INSTR_move_to_vec_b:
            u = *sp++; ((word*)*sp)[*pc] = u; pc += 1; break;

        case INSTR_set_stack_val_b:
            u = *sp++; sp[*pc-1] = u; pc += 1; break;

        case INSTR_reset_b: sp += *pc; pc += 1; break;

        case INSTR_reset_r_b:
            u = *sp; sp += *pc; *sp = u; pc += 1; break;

        case INSTR_const_int_b: *(--sp) = TAGGED(*pc); pc += 1; break;

        case INSTR_local_0: u = sp[0]; *(--sp) = u; break;
        case INSTR_local_1: u = sp[1]; *(--sp) = u; break;
        case INSTR_local_2: u = sp[2]; *(--sp) = u; break;
        case INSTR_local_3: u = sp[3]; *(--sp) = u; break;
        case INSTR_local_4: u = sp[4]; *(--sp) = u; break;
        case INSTR_local_5: u = sp[5]; *(--sp) = u; break;
        case INSTR_local_6: u = sp[6]; *(--sp) = u; break;
        case INSTR_local_7: u = sp[7]; *(--sp) = u; break;
        case INSTR_local_8: u = sp[8]; *(--sp) = u; break;
        case INSTR_local_9: u = sp[9]; *(--sp) = u; break;
        case INSTR_local_10: u = sp[10]; *(--sp) = u; break;
        case INSTR_local_11: u = sp[11]; *(--sp) = u; break;

        case INSTR_indirect_0:
            *sp = ((word*)*sp)[0]; break;

        case INSTR_indirect_1:
            *sp = ((word*)*sp)[1]; break;

        case INSTR_indirect_2:
            *sp = ((word*)*sp)[2]; break;

        case INSTR_indirect_3:
            *sp = ((word*)*sp)[3]; break;

        case INSTR_indirect_4:
            *sp = ((word*)*sp)[4]; break;

        case INSTR_indirect_5:
            *sp = ((word*)*sp)[5]; break;

        case INSTR_const_0: *(--sp) = TAGGED(0); break;
        case INSTR_const_1: *(--sp) = TAGGED(1); break;
        case INSTR_const_2: *(--sp) = TAGGED(2); break;
        case INSTR_const_3: *(--sp) = TAGGED(3); break;
        case INSTR_const_4: *(--sp) = TAGGED(4); break;
        case INSTR_const_10: *(--sp) = TAGGED(10); break;

        case INSTR_move_to_vec_0: u = *sp++; ((word*)*sp)[0] = u; break;
        case INSTR_move_to_vec_1: u = *sp++; ((word*)*sp)[1] = u; break;
        case INSTR_move_to_vec_2: u = *sp++; ((word*)*sp)[2] = u; break;
        case INSTR_move_to_vec_3: u = *sp++; ((word*)*sp)[3] = u; break;
        case INSTR_move_to_vec_4: u = *sp++; ((word*)*sp)[4] = u; break;
        case INSTR_move_to_vec_5: u = *sp++; ((word*)*sp)[5] = u; break;
        case INSTR_move_to_vec_6: u = *sp++; ((word*)*sp)[6] = u; break;
        case INSTR_move_to_vec_7: u = *sp++; ((word*)*sp)[7] = u; break;

        case INSTR_reset_r_1: u = *sp; sp += 1; *sp = u; break;
        case INSTR_reset_r_2: u = *sp; sp += 2; *sp = u; break;
        case INSTR_reset_r_3: u = *sp; sp += 3; *sp = u; break;

        case INSTR_reset_1: sp += 1; break;
        case INSTR_reset_2: sp += 2; break;

        case INSTR_non_local_l_1:
            u = *pc;
            t = ((word**)sp[u >> 4])[(u & 0xf) - 6];
            *(--sp) = (word)t; pc += 1; break;

        case INSTR_non_local_l_2:
            u = *pc;
            t = (word*)sp[u >> 4] -1;
            *(--sp) = (*(word**)t)[(u & 0xf) - 6];
            pc += 1; break;

        case INSTR_non_local_l_3:
            u = *pc;
            t = (word*)sp[u >> 4] -1;
            t = (word*)*t - 1;
            *(--sp) = (*(word**)t)[(u & 0xf) - 6];
            pc += 1; break;

        case INSTR_call_sl_c: /* Static link call */
            /* Get static link value. */
            u = pc[2];
            t = sp + (u >> 4) + 2;
            for(u = u & 0xf; u > 0; u--) t = (word*)t[-1];
            u = (word)(pc+arg1+2); /* Get entry point. */
            /* Push return address to point after instruction. */
            pc += 3;
            assert(ALIGNED(pc));
            assert(pc[0] == INSTR_enter_int && pc[1] <= MAXINTARGS);
            assert(pc[8] == INSTR_enter_int && pc[9] == pc[1]);
            *(--sp) = (word)(pc - SAVERETURNOFFSET);
            *(--sp) = (word)t; /* Push static link */
            pc = *(byte**)u;
            break;

        case INSTR_io_vec_5: *(--sp) = (word)interface_map[5]; break;
        case INSTR_io_vec_6: *(--sp) = (word)interface_map[6]; break;
        case INSTR_io_vec_225: *(--sp) = (word)interface_map[225]; break;
        case INSTR_io_vec_226: *(--sp) = (word)interface_map[226]; break;
        case INSTR_io_vec_229: *(--sp) = (word)interface_map[229]; break;
        case INSTR_io_vec_233: *(--sp) = (word)interface_map[233]; break;
        case INSTR_io_vec_236: *(--sp) = (word)interface_map[236]; break;
        case INSTR_io_vec_251: *(--sp) = (word)interface_map[251]; break;
        case INSTR_io_vec_253: *(--sp) = (word)interface_map[253]; break;
        case INSTR_io_vec_255: *(--sp) = (word)interface_map[255]; break;

        case INSTR_integer_equal:
            u = *sp++; *sp = (u == *sp)?True:False; break;

        case INSTR_integer_leq:
            u = UNTAGGED(*sp++);
            *sp = (UNTAGGED(*sp) <= u)?True:False; break; 

        case INSTR_integer_greater:
            u = UNTAGGED(*sp++);
            *sp = (UNTAGGED(*sp) > u)?True:False; break; 

        case INSTR_boolean_or:
            u = *sp++; if (u == True) *sp = True; break; 

        case INSTR_word_equal:
           u = *sp++;
           if (u == *sp) *sp = True;
           else *sp = addresses_equalc(u, *sp);
           break;

        case INSTR_assign_word:
           t = (word*)sp[0];
           u = UNTAGGED(sp[1]);
           if (IS_LOCAL_MUTABLE(sp[2]))
           {
              ((word**)sp[2])[u] = t;
           }
           else
           {
            /* need to go through proper RTS call SPF 29/6/95 */
             AssignMappedDatabaseWord(H,(word *)sp[2] + u,(word)t);
           }
           sp += 2;
           *sp = TAGGED(0);
           break;

       default: crash("Unknown instruction %x\n", li);

    } /* switch */
    } /* for */
#undef arg1
#undef arg2
#undef arg3
} /* interpreter */

#endif /* PORTING */

/******************************************************************************/
/*                                                                            */
/*      MD_reset_signals - called by run_time.c                               */
/*                                                                            */
/******************************************************************************/
void MD_reset_signals(void)
{
  /* restore default signal handling in child process after a "fork". */
  signal(SIGFPE,  SIG_DFL);
  signal(SIGEMT,  SIG_DFL);
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
    	if ((instr & 0xc1c00000) == 0x01000000) /* sethi instr. */
    	{
    	    int reg = (instr >> 25) & 31;
    	    word *valu, *oldvalu;
    	    /* Ignore the i registers because those are unchecked and g0
    	       because that's a nop. */
    	    if (reg >= 24 || reg == 0) continue;
    	    /* Next must be an ADD instruction. */
	    assert((*pt & 0xc1f83000) == 0x80002000);
	    /* Put together the two halves. */
	    valu = (word*)((instr << 10) | (*pt & 0x3ff));
	    oldvalu = valu;
	    /* Process this address. */
	    ForConstant(&valu, op);
	    /* Put the corrected value back. */
	    if (valu != oldvalu)
	    {
	    	pt[-1] = (instr & 0xffc00000) | ((unsigned)valu >> 10);
	    	*pt = (*pt & 0xfffff000) | ((int)valu & 0x3ff);
	    }
	    pt++;
    	}
    	else if ((instr & 0xc0000000) == 0x40000000) /* Call instr. */
    	{
    	    int disp = instr & 0x3fffffff;
    	    int newDisp;
    	    word *valu = (pt-1) + disp;
    	    if (valu >= (word*)addr && valu < end) continue; /* It's local to this code seg. */
	    /* The old value of the displacement was relative to the old address.  */
	    valu = valu - (word*)addr + (word*)old;
	    /* Process this address. */
	    ForConstant(&valu, op);
	    /* Put the corrected value back. */
	    newDisp = valu - (pt-1);
	    if (newDisp != disp)
	    {
	        pt[-1] = (newDisp & 0x3fffffff) | 0x40000000;
	    }
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
    assert((offset & 3) == 0);
    if (pointer[0] == 0x40000000) /* Call instruction. */
    {
    	word *c = (word*)DEREFWORDHANDLE(constant);
    	int disp = c - pointer; /* Signed displacement in words. */
    	pointer[0] = (disp & 0x3fffffff) | 0x40000000;
    }
    else
    {
	unsigned c = (unsigned)DEREFWORDHANDLE(constant);
	unsigned hi = c >> 10, lo = c & 0x3ff;
	/* The first word must be SETHI. */
	assert((pointer[0] & 0xc1ffffff) == 0x01000000);
	/* and the next must be ADD.  The immediate value must currently be
	   tagged(0). */
	assert((pointer[1] & 0xc1f83fff) == 0x80002001);
	pointer[0] |= hi;
	pointer[1] = (pointer[1] & 0xfffff000) | lo;
    }
}
