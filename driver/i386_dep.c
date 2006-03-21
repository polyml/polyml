/*
    Title: 	Machine dependent code for i386 under Windows and Unix

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

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>	  
#include <assert.h>
#include <string.h>

#ifdef WINDOWS_PC
#include <windows.h>
#include <excpt.h>
#endif /* WINDOWS_PC */

#include "globals.h"
#include "objects.h"
#include "memory.h"
#include "mm.h"
#include "gc.h"
#include "machine_assembly.h"
#include "run_time.h"
#include "mpoly.h"
#include "arb.h"
#include "diagnostics.h"
#include "processes.h"
#include "sys.h"
#include "profiling.h"
#include "sighandler.h"
#include "machine_dep.h"

#ifndef SA_ONSTACK
/* Older versions of Linux did not define SA_ONSTACK. */
#define SA_ONSTACK 0
#endif

int in_run_time_system = 1; /* Start off in run-time system. */

#define add_function_to_io_area(x,y) add_word_to_io_area(x,(word)(y))

#define VERSION_NUMBER	POLY_version_number

/**********************************************************************
 *
 * Register usage:
 *
 * %eax: First argument to function.  Result of function call.
 * %ebx: Second argument to function.
 * %ecx: Code pointer in call.
 * %edx: Closure pointer in call.
 * %ebp: Points to memory used for extra registers
 * %esi: General register.
 * %edi: General register.
 * %esp: Stack pointer.
 *
 * SEE ALSO i386.asm 
 *
 **********************************************************************/

#define CHECKED_REGS	6
/* The unchecked reg field is used for the condition codes. */
#define UNCHECKED_REGS	1

extern StackObject *poly_stack;
extern word *end_of_stack;

static int isAccess; /* Reason for last fault. */

void add_jumps(int,word *);

/**********************************************************************
 *
 * Register fields in poly_stack. 
 *
 **********************************************************************/
#define EAX	poly_stack->p_reg[0]
#define EBX     poly_stack->p_reg[1]
#define ECX     poly_stack->p_reg[2]
#define EDX	poly_stack->p_reg[3]
#define ESI	poly_stack->p_reg[4]
#define EDI	poly_stack->p_reg[5]

#define EFLAGS	poly_stack->p_reg[CHECKED_REGS+1]

#define IC 	    poly_stack->p_pc
#define SP      poly_stack->p_sp
#define HR      poly_stack->hr	


/*********************************************************************
 *                                                                        
 * MD_init_stack_frame - called by run-time system                     
 *                                                                          
 *********************************************************************/
void MD_init_stack_frame(StackObject *stack, Handle proc, Handle arg)
/* Initialise stack frame. */
{
    word stack_size,i;

    stack_size     = OBJ_OBJECT_LENGTH(((word*)stack)[-1]);
    stack->p_space = OVERFLOW_STACK_SIZE;
    stack->p_pc    = (byte*)TAGGED(0);
    stack->p_sp    = (word*)stack + stack_size-4; 
    stack->p_nreg  = CHECKED_REGS;

    for (i = 0; i < CHECKED_REGS; i++) stack->p_reg[i] = TAGGED(0);

    stack->p_reg[CHECKED_REGS] = UNCHECKED_REGS; /* 1 unchecked register */
	stack->p_reg[CHECKED_REGS+1] = 0;
	stack->p_reg[3] = (word)DEREFWORDHANDLE(proc); /* edx - closure pointer */
    /* If this function takes an argument store it in the argument register. */
    if (arg != 0) stack->p_reg[0] = (word)DEREFWORDHANDLE(arg);

    /* No previous handler so point it at itself. */
    ((word**)stack)[stack_size-1] = &((word*)stack)[stack_size-1];
    /* Default handler. */
    ((word*)stack)[stack_size-2] = (word)interface_map[POLY_SYS_kill_self] + 2;
    /* Set up exception handler */
    ((word*)stack)[stack_size-3] = TAGGED(0); /* Default handler. */
    stack->p_hr = (word*)stack+stack_size-3;
    /* Return address. */
    ((word*)stack)[stack_size-4] = (word)interface_map[POLY_SYS_kill_self] + 2;
    return;
}

void MD_OK(int n, int m)
{
   printf("Assembly code: place %x, value %x\n",m,n);
   fflush(stdout);
   return;
}

#ifndef WINDOWS_PC
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
    add_count(poly_stack->p_pc,poly_stack->p_sp,1);
  }
  else /* in poly code or assembly code */
  {
    if (sp >= (word *)poly_stack && sp < end_of_stack)
      add_count(pc,sp,1);
    else
      Crash("Bad sp in MD_increment_profile_count, sp=%p, poly_stack=%p, end_of_stack=%p", sp, poly_stack, end_of_stack);
  }
  return;
}

/******************************************************************************/
/*                                                                            */
/*      MD_increment_profile_count_using_context - called by run_time.c       */
/*                                                                            */
/******************************************************************************/
void MD_increment_profile_count_using_context(struct sigcontext *context)
{
#ifdef LINUX
  MD_increment_profile_count1((byte *)context->eip,
                              (word *)context->esp_at_signal);
#else
  MD_increment_profile_count1((byte *)context->sc_pc,
                              (word *)context->sc_sp);
#endif
  return;
}

#else /* WINDOWS_PC */

/**************************************************************************
 *                                                                        
 * signal_filter - called by run_time.c, executes on ML stack        
 *                                                                        
 **************************************************************************/ 

static int signal_filter (int exn_code, struct _EXCEPTION_POINTERS *exn_info)
{
	/* DCJM: This previously tried to save exn_info->ContextRecord into
	   a global variable.  That caused a crash in some cases because not all
	   the ContextRecord was readable.
	 */

#ifdef DEBUG
	printf("In exception handler, code = %x, pc = %8x\n",exn_code,(exn_info->ContextRecord)-> Eip);
	fflush(stdout);
#endif

	switch (exn_code)
	  {
		/* raised by ML stack check. */
		case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
		/* raised by ML divide instruction */
		case EXCEPTION_INT_DIVIDE_BY_ZERO:
		/* raised by ML heap check and ordinary code */
		case EXCEPTION_INT_OVERFLOW:
		/* raised by assignment to database area. */
	  	case EXCEPTION_ACCESS_VIOLATION:
		  {
			/* save faulting program counter, then resume in MD_trap_handler */
			/* MD_trap_handler will save the other registers then switch back
			   to the C stack and resume in MD_trap_handler1 (below). We could do
			   save the registers here instead but we already have the necessary
			   assembly code written, and some of the "registers" are, in fact,
			   pseudo-registers which are local to the assembly code.
			   It might be profitable to save "exn_code" here so that
			   MD_trap_handler1	can use its value, but that doesn't seem to
			   be absolutely necessary.
			*/
		    CONTEXT *context = exn_info -> ContextRecord;
			if (exn_code == EXCEPTION_ACCESS_VIOLATION)
			{
				/* Check it really is a fault on writing to
				   the database mutable area and not something else. */
				if (exn_info->ExceptionRecord->ExceptionInformation[0] != 1)
					/* Must be writing. */
					return EXCEPTION_CONTINUE_SEARCH;
				/* ExceptionInformation[1] is the faulting address so
				   we could verify that it's in the mutable area. */
			}
			IC  = (byte *)context -> Eip;
			(byte *)context -> Eip = (byte *)&MD_trap_handler;
			isAccess = exn_code == EXCEPTION_ACCESS_VIOLATION ? 1 : 0;

			return EXCEPTION_CONTINUE_EXECUTION; 				
		 }
		  		
		/* Debugging exceptions - shouldn't occur in ML,
		   so take standard system action */
		case EXCEPTION_BREAKPOINT:
		case EXCEPTION_SINGLE_STEP:
 
		/* Non-fatal floating point exceptions - shouldn't occur in ML,
		   so take standard system action */
	    case EXCEPTION_FLT_DENORMAL_OPERAND:	    
	    case EXCEPTION_FLT_DIVIDE_BY_ZERO:
		case EXCEPTION_FLT_INEXACT_RESULT:
		case EXCEPTION_FLT_OVERFLOW:
		case EXCEPTION_FLT_UNDERFLOW:
 
		/* fatal errors - take default system action */
		case EXCEPTION_DATATYPE_MISALIGNMENT:
		case EXCEPTION_FLT_INVALID_OPERATION:
		case EXCEPTION_FLT_STACK_CHECK:		
		case EXCEPTION_PRIV_INSTRUCTION:
		case EXCEPTION_NONCONTINUABLE_EXCEPTION:
		  return EXCEPTION_CONTINUE_SEARCH;

		default:
		 {
		   printf("Unrecognised exception code %x\n",exn_code);
		   fflush(stdout);
		   return EXCEPTION_CONTINUE_SEARCH;
		 }	    
	  } 
}
#endif /* WINDOWS_PC */

/******************************************************************************/
/*                                                                            */
/*      MD_interrupt_code_using_context - called by run_time.c                */
/*                                                                            */
/******************************************************************************/

#ifndef WINDOWS_PC
void MD_interrupt_code_using_context(struct sigcontext *context)
#else /* WINDOWS_PC */
void MD_interrupt_code(void)
#endif /* WINDOWS_PC */
{
  extern word *stack_limit;
  if(in_run_time_system) 
     return;
  /* set the stack limit pointer to the top of the stack to cause
     a trap when we next check for stack overflow */
  else
     stack_limit = end_of_stack-1; 
}

/****************************************************************
 *                                                                   
 *   MD_switch_to_poly - called by run-time system             
 *                                                         
 ****************************************************************/

void MD_switch_to_poly(void)
/* (Re)-enter the Poly code from C. Just call the relevant assembly code. */
{
#if defined(WINDOWS_PC) /* PC version */
  __try
  {
	MD_switch_to_poly_X();
  }
  __except (signal_filter(_exception_code(),_exception_info()))
    {
      Crash("Unhandled exception %x", _exception_code());
    }
#else /* UNIX */
  /* Run the process(es) - will never return */
    MD_switch_to_poly_X();
#endif
}

/**************************************************************
 *                                                        
 * MD_set_for_retry - called by run-time system          
 *                                                         
 **************************************************************/

void MD_set_for_retry(int ioCall)
{
	/* We now have to set the closure entry for the RTS call to work.
	   DCJM 4/1/01. */
	EDX = (word)interface_map[ioCall];
	IC = (byte*)TAGGED(0); /* This value is treated specially. */
	return;
}

#ifdef WINDOWS_PC
/****************************************************
 *                                               
 * decode_fpcodes - utility function           
 *                                              
 ****************************************************/
static int decode_fpcodes(int code)
/* Returns the exception name for the different codes. */
{
  switch(code) 
    {
	  case EXCEPTION_INT_OVERFLOW :
	                        return EXC_size;

      case EXCEPTION_INT_DIVIDE_BY_ZERO : 
      case EXCEPTION_FLT_DIVIDE_BY_ZERO :
                            return EXC_divide;

      case EXCEPTION_FLT_UNDERFLOW : 
                            return EXC_underflow;

      case EXCEPTION_FLT_OVERFLOW :
	                        return EXC_overflow;

      default : 
             Crash("Unknown interrupt code %d\n", (int)code);
			 return 0;
    }
}

#endif /* WINDOWS_PC */

#ifdef LINUX
/*****************************************************************************
                                                                            
      catchFPE - floating point handler for genuine FPE errors within RTS   
                                                                            
 *****************************************************************************/
static void catchFPE(int sig, struct sigcontext context)
/* Only used in Linux.  FreeBSD passes FPE signals to catchSEGV */
{

#if NEVER
    printf("In catchFPE, sig = %i\n",sig); fflush(stdout);
#endif
 
    assert (sig == SIGFPE);

	/* It used to be possible for divide-by-zero to occur in the
	   RTS when handling real numbers.  It probably can't happen
	   now that we have IEEE arithmetic. */

    if (in_run_time_system)
    {
      raise_exception0(EXC_divide);
	  /*  raise_exception0(decode_fpcodes(code)); */
    }
	/* Switch to trap handler to raise the exception. */
	if (context.trapno != 0)
	{
		signal(SIGFPE,SIG_DFL);
		fprintf(stderr,"catchFPE; &context = %p, in_run_time_system=%i, context.trapno=%ld\n",
             &context, in_run_time_system, context.trapno);
		raise(SIGFPE);
	}
#ifdef EXTRA_DEBUG
  {
     signal(SIGFPE,SIG_DFL);
     fprintf(stderr,"catchFPE; &context = %p, in_run_time_system=%i, context.trapno=%ld\n",
             &context, in_run_time_system, context.trapno);
     signal(SIGFPE,catchFPE);
  }
#endif

  /* NOW, we are in the run-time system */
  in_run_time_system = 1;
  isAccess = 0;
  poly_stack->p_pc = (byte*)context.eip; /* Save trapping pc. */
  context.eip      = (int)&MD_trap_handler; /* Restart in trap_handler. */
  return; /* "returns" to MD_trap_handler */
 }
#endif

#ifndef WINDOWS_PC
/******************************************************************************/
/*                                                                            */
/*      catchSEGV - utility function                                          */
/*                                                                            */
/******************************************************************************/
#ifdef FREEBSD
static void catchSEGV(int sig, int code, struct sigcontext *scp)
#else
static void catchSEGV(int sig, struct sigcontext context)
#endif
{
  assert(sig == SIGSEGV || sig == SIGFPE || sig == SIGBUS);

  /* The i386 code generator needs run-time support to handle
     long arithmetic, stack overflow and heap overflow.

     Long arithmetic emulation is entered by an "int $4" instruction.
     On Linux, this causes SIGSEGV to be raised, with context.trapno=4.
     In FreeBSD it raises SIGFPE.

     Stack overflow is detected using the "boundl" instruction. If
     there's an overflow, this raises SIGSEGV with context.trapno=5.

     Heap overflow is also detected using "boundl", so also raises
     SIGSEGV with context.trapno=5.

     This is also used to handle traps resulting from assigning to
     references in the "mutable" area of a database.  These assignments
     have to be handled specially so the area is made read-only.  That
     results in a SIGBUS in FreeBSD and a SIGSEGV with context.trapno=14
     in Linux.  OpenBSD generates a SIGSEGV instead of a SIGBUS.
  */
  
#ifdef FREEBSD
  if (in_run_time_system)
  {
	if (sig == SIGFPE) {
	      raise_exception0(EXC_divide); /* temporary!!! */
	  	/*  raise_exception0(decode_fpcodes(code)); */
	}
	else
	{
	    signal(SIGSEGV,SIG_DFL);
	    fprintf(stderr,"catchSEGV; scp = %p, in_run_time_system=%i\n",
            scp, in_run_time_system);
	    raise(SIGSEGV);
	}
  }
#else
  if (in_run_time_system || 
      (context.trapno != 4 && context.trapno != 5 && context.trapno != 14))
  {
     signal(SIGSEGV,SIG_DFL);
     fprintf(stderr,"catchSEGV; &context = %p, in_run_time_system=%i, context.trapno=%ld\n",
             &context, in_run_time_system, context.trapno);
     raise(SIGSEGV);
  }
#endif
#ifdef EXTRA_DEBUG
  else
  {
     signal(SIGSEGV,SIG_DFL);
     fprintf(stderr,"catchSEGV; &context = %p, in_run_time_system=%i, context.trapno=%ld\n",
             &context, in_run_time_system, context.trapno);
     signal(SIGSEGV,catchSEGV);
  }
#endif

  /* NOW, we are in the run-time system */
  in_run_time_system = 1;


/* This piece of code is extremely messy. It has to get the state when the
   interrupt occured by unwinding the stack. It can then save the registers
   and call ``translate''. */
#ifdef FREEBSD
  isAccess = sig == SIGBUS || sig == SIGSEGV;
  poly_stack->p_pc = (byte*)scp->sc_pc; /* Save trapping pc. */
  scp->sc_pc      = (int)&MD_trap_handler; /* Restart in trap_handler. */
#else
  isAccess = context.trapno == 14;
  poly_stack->p_pc = (byte*)context.eip; /* Save trapping pc. */
  context.eip      = (int)&MD_trap_handler; /* Restart in trap_handler. */
#endif

  return; /* "returns" to MD_trap_handler */

}
#endif /* not WINDOWS_PC */

/*******************************************
                                       
        get_reg - utility function         
                                        
 *******************************************/
static word *get_reg(int n)
/* Returns a pointer to the register given by n. */
{
  switch (n) 
    {
      case 0: return &EAX;
      case 1: return &ECX;
      case 2: return &EDX;
      case 3: return &EBX;
      case 4: return (word*)&SP;
      case 6: return &ESI;
      case 7: return &EDI;
      default: 
        Crash("Unknown register %d at %x\n", n, (int)(IC));
    }
}

/******************************************************************************/
/*                                                                            */
/*      do_compare - do a "long" comparison, setting the flags register       */
/*                                                                            */
/******************************************************************************/
static void do_compare(word v1, word v2)
{
    word r;
    Handle val1, val2;
    /* Must push these to the save vec.  A persistent store trap
       might cause a garbage collection and move the stack. */
    val1 = push_to_save_vec(v1);
    val2 = push_to_save_vec(v2);
    r = comp_longc(val2, val1);
    /* Clear the flags. */
    EFLAGS &= -256;
    if (r == TAGGED(0)) EFLAGS |= 0x40;
    else if (r == TAGGED(-1)) EFLAGS |= 0x80;
}

/******************************************************************************/
/*                                                                            */
/*      do_op - do a "long" operation, setting the destination register       */
/*                                                                            */
/******************************************************************************/
static void do_op(int dest, word v1, word v2, Handle (*op)(Handle, Handle))
{
    Handle val1, val2, result;
    /* Must push these to the save vec.  A persistent store trap
       or a garbage collection might move the stack. */
    val1 = push_to_save_vec(v1);
    val2 = push_to_save_vec(v2);
    /* Clobber the destination which may have overflowed. */
    *(get_reg(dest)) = TAGGED(0);
    result = op (val2, val1);	  /* N.B parameters are intentionally reversed */
    /* N.B. the stack may have moved so we must recompute get_reg(dest). */
    *(get_reg(dest)) = (word)DEREFHANDLE(result);
}

/***************************************************************
                                                            
          emulate_instrs - do a "long" instruction               
                                                             
		  ARE THESE OK ?????

 ***************************************************************/
 
void emulate_instrs(void)
{
    int src1 = -1, src2 = -1, dest = -1;
    int untagged = 0;
    init_save_vec(); /* Reset it. */
    while(1) {
		switch (IC[0]) {
		case 0x03: /* add. */
			if ((IC[1] & 0xc0) != 0xc0)
				Crash("Expected register");
			if (dest != ((IC[1] >> 3) & 7))
				Crash("Expected same destination register.");
			src2 = IC[1] & 7;
			do_op(dest, *(get_reg(src1)), *(get_reg(src2)), add_longc);
			IC += 2;
			LONGJMP(re_enter_poly,999);

		case 0x2b: /* Subtraction. */
			if ((IC[1] & 0xc0) != 0xc0)
				Crash("Expected register");
			if (dest != ((IC[1] >> 3) & 7))
				Crash("Expected same destination register.");
			src2 = IC[1] & 7;
			do_op(dest, *(get_reg(src1)), *(get_reg(src2)), sub_longc);
				/* Subtract one from the result, because the next
			   instruction will put on a tag. */
			*(get_reg(dest)) -= 1;
			IC += 2;
			LONGJMP(re_enter_poly,999);

		case 0x3b: /* Compare. */
			if ((IC[1] & 0xc0) != 0xc0)
				Crash("Expected register");
			src1 = (IC[1] >> 3) & 7;
			src2 = IC[1] & 7;
			do_compare(*(get_reg(src1)), *(get_reg(src2)));
			IC += 2;
			LONGJMP(re_enter_poly,999);

		case 0x8d: /* leal - Used to remove a tag before an add and multiply. */
			if (IC[2] != 0xff) Crash("Tag?");
			if (src1 == -1) src1 = IC[1] & 7; else src2 = IC[1] & 7;
			dest = (IC[1] >> 3) & 7;
			untagged = 1;
			IC += 3;
			break;

		case 0x89: /* movl: move source into dest. */
			if ((IC[1] & 0xc0) != 0xc0)
				 Crash("Can't move into store.");
			dest = IC[1] & 7;
			if (src1 == -1) src1 = (IC[1] >> 3) & 7; else src2 = (IC[1] >> 3) & 7;
			untagged = 0;
			IC += 2;
				/* Next should be add-immediate. */
			break;

		case 0x83: { /* One byte immediate: Add, sub or compare. */
			int cval = IC[2];
			if (cval >= 128) cval -= 256;
			
			switch (IC[1] & (7 << 3))
			{
			  case (0 << 3): /* add */
					  {
				if (dest != (IC[1] & 7))
					Crash("Expected same destination register.");
				/* immediate value is shifted, but hasn't had 1 added;
				   do this now before calling add_longc */
				do_op(dest, *(get_reg(src1)), cval+1, add_longc);
				break;
			  }
			  case (5 << 3): /* sub */
					  {
				if (dest != (IC[1] & 7))
					Crash("Expected same destination register.");
				/* immediate value is shifted, but hasn't had 1 added;
				   do this now before calling sub_longc */
				do_op(dest, *(get_reg(src1)), cval+1, sub_longc);
				break;
			  }
			  
			  case (7 << 3): /* cmp */
					  {
				if ((IC[1] & 0xc0) != 0xc0)
					Crash("Can't test with store.");
				src1 = IC[1] & 7;
				/* immediate value is already tagged */
				do_compare(*(get_reg(src1)), cval);
				break;
			  }
		  			
			 default: Crash("Unknown instruction after overflow trap");
			}
			
			IC += 3;
			LONGJMP(re_enter_poly,999);
			}

		case 0x81: { /* 4 byte immediate: Add, sub or compare. */
			int cval = IC[5];
			if (cval >= 128) cval -= 256;
			cval = cval*256 + IC[4];
			cval = cval*256 + IC[3];
			cval = cval*256 + IC[2];
			if ((IC[1] & 0xc0) != 0xc0)
				Crash("Expected register");

			switch (IC[1] & (7 << 3))
			{
			  case (0 << 3): /* add */
					  {
				if (dest != (IC[1] & 7))
					Crash("Expected same destination register.");
				/* immediate value is shifted, but hasn't had 1 added;
				   do this now before calling add_longc */
				do_op(dest, *(get_reg(src1)), cval+1, add_longc);
				break;
			  }
			  case (5 << 3): /* sub */
					  {
				if (dest != (IC[1] & 7))
					Crash("Expected same destination register.");
				/* immediate value is shifted, but hasn't had 1 added;
				   do this now before calling sub_longc */
				do_op(dest, *(get_reg(src1)), cval+1, sub_longc);
				break;
			  }
			  
			  case (7 << 3): /* cmp */
					  {
				src1 = IC[1] & 7;
				/* immediate value is already tagged */
				do_compare(*(get_reg(src1)), cval);
				break;
			  }
		  			
			 default: Crash("Unknown instruction after overflow trap");
			}

			IC += 6;
			LONGJMP(re_enter_poly,999);
			}

		case 0xeb: /* jmp - used in branch forwarding. */
			/* While forwarded jumps are always positive we may use backward
			   branches in the future. */
			if (IC[1] >= 128) IC += 256 - IC[1] + 2;
			else IC += IC[1] + 2;
			break;

		case 0x50: /* push eax - used before a multiply. */
			*(--SP) = EAX;
			IC += 1;
			break;

		case 0x52: /* push edx - used before a multiply. */
			*(--SP) = EDX;
			IC += 1;
			break;

		case 0xd1: /* Group1A - must be sar edx before a multiply. */
			if (IC[1] != 0xfa)
				Crash("Unknown instruction after overflow trap");
			IC += 2;
			/* If we haven't moved anything into edx then edx must be
			   one of the arguments. */
			if (src2 == -1) src2 = 2; /* edx. */
			break;

		case 0xf7: /* Multiply instruction. */
			if (IC[1] != 0xea)
				Crash("Unknown instruction after overflow trap");
			do_op(0 /* eax */, *(get_reg(src1)), *(get_reg(src2)), mult_longc);
			EAX--; /* Subtract one because the next instruction will tag it. */
			IC += 2;
			LONGJMP(re_enter_poly,999);

		default:
			Crash("Unknown instruction after overflow trap");
		}
    }
}

/*********************************************************
                                                       
        MD_trap_handler1 - called from i386.asm         
                                                   
 *********************************************************/

void MD_trap_handler1(void)
/* Called from MD_trap_handler after registers have been saved in poly_stack. 
   We should now be executing on the C stack, not one of the ML one. */
{
#ifdef DEBUG
   printf("In MD_trap_handler1\n");
   printf("IC = %x\n",IC);
   fflush(stdout);
#endif

   if (isAccess)
   { /* Segmentation fault - should be an attempt to assign to the
	    database area. */
	   int opcode = IC[0];
	   /* Include handling these traps in the emulation count. */
	   if (emulate_profiling) add_count(IC, SP, 1);

	   if ((opcode & 0xfe) == 0x88 || (opcode & 0xfe) == 0xc6)
	   {
			/* Move to memory. */
			/* Really need to distinguish different reasons for fault. */
			/* Value to be assigned. */
			word value;
			byte *baseAddr;
			word indexVal = 0;
			word c = 0;
			int  bytes = 2;
			if ((IC[1] & 7) == 4)
			{	/* Index register present. */
				baseAddr = *(byte**)(get_reg(IC[2] & 7));
				indexVal = *(get_reg((IC[2] >> 3) & 7));
				bytes++;
				switch ((IC[2] >> 6) & 3)
				{
				case 3: indexVal *= 2; /* x8 */
				case 2: indexVal *= 2; /* x4 */
				case 1: indexVal *= 2; /* x2 */
				}
			}
			else baseAddr = *(byte**)(get_reg(IC[1] & 7));
			switch ((IC[1] >> 6) & 3)
			{
			case 1: /* One byte offset. */
				c = IC[bytes++];
				if (c >= 128) c -= 256;
				break;
			case 2: /* 32 bit offset. */
				c = IC[bytes+3];
				if (c >= 128) c -= 256;
				c = c*256+IC[bytes+2];
				c = c*256+IC[bytes+1];
				c = c*256+IC[bytes];
				bytes += 4;
			}
			if (opcode == 0xc6)
			{
				/* Immediate operand. */
				value = IC[bytes++];
				if (value >= 128) value -= 256;
			}
			else if (opcode == 0xc7)
			{
				/* Immediate operand. */
				value = IC[bytes+3];
				if (value >= 128) value -= 256;
				value = value*256+IC[bytes+2];
				value = value*256+IC[bytes+1];
				value = value*256+IC[bytes];
				bytes += 4;
			}
			else /* The value to be assigned is in the register. */
			{
				/* N.B.  For byte operands this is only correct
				   for eax, ebx, ecx and edx but that's all we use. */
				if (opcode == 0x88) { assert(((IC[1] >> 3) & 7) < 4); }
				value = *(get_reg((IC[1] >> 3) & 7));
			}
			if ((opcode & 1) == 1) /* Word operation. */
				AssignMappedDatabaseWord(H, (word*)baseAddr, (indexVal + c)/sizeof(word), value);
			else AssignMappedDatabaseByte(H, baseAddr, indexVal + c, (byte)value);
			IC += bytes;
			LONGJMP(re_enter_poly,999); /* get back inside C exception handler */
		}
		/* some other instruction. */
		Crash("Can't process access violation fault at %x\n", IC);
   }
   if (IC[0] == 0x62 /* bound */) 
    {
  	  /* Either a heap overflow or a stack overflow. */
	  int regn = (IC[1] >> 3) & 7;
	  /* In either case the register contents may be invalid, so we
	     extract the value and clobber the register, putting back an
	     acceptable value later if necessary. */
	  word **reg = (word**)get_reg(regn);
	  word *reg_val = *reg;

#ifdef DEBUG
	  printf("In IC[0] == 0x62\n");
          fflush(stdout);
	  {
	   int i;
	   for(i=-4 ; i<13 ; i++)
	      printf("Now IC[%i] = %02x\n",i,IC[i]);
	   fflush(stdout);
	  }
#endif

	  if (regn != 4 /* %esp */) *reg = (word*)TAGGED(0);
	  if (IC[2] == 16)
	  { /* Checking stack. 
	       Must be stack overflow or trap. */ 

#ifdef DEBUG
	    printf("Stack overflow\n");
#endif

	    check_current_stack_size(reg_val);

        /* don't re-execute the bound instruction, just to check we've fixed 
		   everything up properly, because this may cause problems if
		   we're profiling SPF 16/11/94 */
	    /* Skip the "bound" instruction BEFORE any change of process. */
	    IC += 3;
	    /* Now handle any interrupts. This may switch processes. */
	    execute_pending_interrupts();
	  }
	  else
	  { /* Must be heap overflow. */
	    /* The space we need is the difference between this register
	       and the current value of newptr. */
	    unsigned len = (A.M.pointer - reg_val) + 1;
	    /* length in words, including length word */
		/* (We have to add 1 because the assembly code interface
		    is off-by-one with respect to the C interface i.e. A.M.pointer
			will point at the length word of the last word we allocated, whereas
			reg_val will point at the first data word of the next object;
			&(reg_val[-1]) is the address of the corresponding length word.
			SPF 16/11/94 */

#ifdef DEBUG
	    printf("Not stack overflow\n");
		printf("profiling = %i\n",store_profiling);
        printf("len = %x\n",len);
        fflush(stdout);
#endif

		assert (len <= (1<<24)); /* Max object size including length/flag word is 2^24 words.  */
	    
	    if (store_profiling)
	       add_count(IC, SP, len);
	     /* This counts the length words; I don't think the SPARC version does. */

        /* On the PC A.M.pointer is only adjusted AFTER the "bound" instruction */
		/* the SPARC adjusts things first and asks questions afterwards */

        else if (A.M.pointer >= A.M.bottom + len)
           Crash ("Spurious heap overflow trap");

#ifdef DEBUG
		printf("A.M.pointer=%x,A.M.bottom=%x,len=%x\n",A.M.pointer,A.M.bottom,len);
        fflush(stdout);
#endif

        if (A.M.pointer < A.M.bottom + len)
        /* a genuine storage request, not just (or as well as) a profiling trap */
        {

#ifdef DEBUG
	  printf("A.M.pointer < A.M.bottom + len\n");
          fflush(stdout);
#endif

          if (A.M.pointer < A.M.bottom) Crash ("Bad length in heap overflow trap");

          QuickGC(len); /* Garbage-collect. */

         /* the stack may have moved, so we have to recalculate reg */
	     reg = (word**)get_reg(regn);
		 A.M.pointer -= len;     /* allocate the space */		 
		 *reg = A.M.pointer + 1; /* remember: it's off-by-one */
        }
		else
		{
			/* Put back the overwritten register value. */
			/* DCJM: This is certainly needed in Windows when running
			   the store profiling.  Is it also needed in Linux?  */
			*reg = reg_val;
#ifdef DEBUG
		  printf("A.M.pointer >= A.M.bottom + len\n");
          fflush(stdout);
#endif
		 }
		/* don't re-execute "bound" instruction just to make sure we've fixed
		   everything up properly, because this will cause problems if
		   we're profiling. */
	    IC += 3; /* Skip the "bound" instruction. */
	  }

#ifdef DEBUG
	  printf("new IC = %x\n",IC);
	  printf("Re-entering ML\n");
          fflush(stdout);
#endif

#ifdef WNTDEBUG
	  printf("Here CTRL+C +  f fails to raise interrupts to the console processes.\n");
#endif
	  LONGJMP(re_enter_poly,999); /* get back inside C exception handler */

    }
    else if (IC[-1] == 4 && IC[-2] == 0xcd)
	{
   	  /* Integer overflow trap.
   	     Arithmetic operation has overflowed or detected long values. */
	  if (emulate_profiling)
	      add_count(IC, SP, 1);
	  emulate_instrs();
    }
	else if (IC[0] == 0xf7 && (IC[1] & 0x30) == 0x30)
	{
		/* Divide instructions. Must be divide-by-zero. */
		raise_exception0(EXC_divide);
	}

    /* If all else fails. */
    Crash("Can't process signal at %x\n", IC);
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
    add_function_to_io_area(POLY_SYS_str_compare, str_compare);
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
    
    /* for compatibility with DCJM's system */
/*    add_function_to_io_area(POLY_SYS_load_immut_byte, &load_byte);
      add_function_to_io_area(POLY_SYS_load_immut_word, &load_word); */
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
    add_function_to_io_area(POLY_SYS_foreign_result, &foreign_result_a); /* DCJM 7/6/01 */

    /* For some reason, the following functions appear to require fewer 
       levels of indirection than those above. This worries me greatly,
	   since I don't understand why this should be so. SPF 14/11/94 */
	/* The reason is that all the above are real functions and so we need
	   a (dummy) closure for each.  In contrast the entries below aren't functions.
	   They are code which link to the run-time system. DCJM 29/6/99 */
    /* This is OK since we have 8 words per entry in the IO area,
       and these only need 4 (?) SPF 1/11/94 */
    add_jumps(POLY_SYS_return, (void *)return_code);
    add_jumps(POLY_SYS_raisex, (void *)raisex);
    add_jumps(POLY_SYS_kill_self, (void *)kill_selfa);

#ifndef WINDOWS_PC
    {
      struct sigaction catchvec;
	memset(&catchvec, 0, sizeof(catchvec));
  
      /* disable SIGFPE while in SIGFPE handler               */
      /* we could possibly use sigset here instead, but we    */
      /* need to be sure that the signal is handled on-stack  */
  
      sigemptyset(&catchvec.sa_mask);
      sigaddset(&catchvec.sa_mask,SIGFPE);
      catchvec.sa_flags = SA_ONSTACK;
#ifdef LINUX
      catchvec.sa_handler = (signal_handler_type)catchFPE;
#else
      catchvec.sa_handler = (signal_handler_type)catchSEGV;
#endif
      assert(sigaction(SIGFPE, &catchvec, 0) == 0);
	  /* Tell the signal handler not to mess this up. */
	  markSignalInuse(SIGFPE);

      sigfillset(&catchvec.sa_mask);
      catchvec.sa_flags = SA_ONSTACK; /* SA_RESTART; */
      catchvec.sa_handler = (signal_handler_type)catchSEGV;
      assert(sigaction(SIGSEGV, &catchvec, 0) == 0);
	  /* Tell the signal handler not to mess this up. */
	markSignalInuse(SIGSEGV);
#ifdef FREEBSD
	assert(sigaction(SIGBUS, &catchvec, 0) == 0);
	markSignalInuse(SIGBUS);
#endif
    }
#endif /* not WINDOWS_PC */
    return;
}

#define JUMPWORDS 7
static word jump_code[JUMPWORDS+2] = {
    0xe9909090,      /* nop, nop, nop, jmp.*/
    0x00000000,	     /* offset in here */
    0,	             /* marker */
    (3)*sizeof(word),/* bytes to the start */
    0,               /* profile count */
    TAGGED(0),       /* name string - this code segment is anonymous */
    1                /* number of constants */
};


/* THIS IS AGAIN DEFINED IN MMAP.C - MAKE IT GLOBAL !!! */
#define IO_SPACING 8

void add_jumps(int sys, word *addr)
{
    word *base;
    int i;

	assert(JUMPWORDS <= IO_SPACING);

    base = (word*)interface_map[sys];
    /* Copy the code over. */
    for (i=0; i < JUMPWORDS; i++) 
              base[i] = jump_code[i];
    /* Put in the address. */
    base[1] = (word)addr-(word)base-8;
#ifdef EXTRA_DEBUG
    printf("entry no %d, at address %x is a jumpentry to %x\n",
	       sys, interface_map[sys], addr);
#endif
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
    */  

    stack->p_pc     = (byte *) interface_map[POLY_SYS_raisex];
    stack->p_reg[0] = (word)exc; /* put exception data into eax */
    return;
}

/******************************************************************************/
/*                                                                            */
/*      MD_is_exception - called by run-time system                           */
/*                                                                            */
/******************************************************************************/
int MD_is_exception(StackObject *stack) /* Return true if MD_set_exception was called */
{
  /* SPF 29/11/93 - when GC debugging is active, this functions */
  /*                may be called before stack is initialised   */
  if (stack == 0) return 0;

  return stack->p_pc == (byte *) interface_map[POLY_SYS_raisex];
}

/********************************************************/
/*                                                      */
/*      MD_reset_signals - called by run_time.c         */
/*                                                      */
/********************************************************/
void MD_reset_signals(void)
{
  /* restore default signal handling. */
  /* SIGILL, SIGEMT are not used in PC version */
  signal(SIGFPE,  SIG_DFL);
}

/********************************************************/
/*                                                      */
/*      MD_update_code_addresses - called by g.c.       */
/*                                                      */
/********************************************************/

static void processAddr(byte *pt, void (*op)(word **))
{
	word *valu, *oldvalu;
	/* This isn't word aligned so it's best to copy it. */
	valu = (word*)(pt[0] | (pt[1] << 8) | (pt[2] << 16) | (pt[3] << 24));
	if (IS_INT(valu)) return; /* Ignore tagged integers. */
	oldvalu = valu;
	/* The old code generator generated reverse subtraction
	   of words using a move immediate which loaded a register
	   with a the tagged value plus one.  In practice the only
	   reverse subtraction of a constant is 0-x so for backwards
	   compatibility we need to treat 2 specially. */
	if ((word)valu != 2) ForConstant(&valu, op);
	if (valu != oldvalu)
	{
		unsigned v = (unsigned)valu;
		*pt++ = (v & 255); v >>= 8;
		*pt++ = (v & 255); v >>= 8;
		*pt++ = (v & 255); v >>= 8;
		*pt++ = (v & 255); v >>= 8;
	}
}

static void skipea(byte **pt, void (*op)(word **))
{
	unsigned int modrm = *((*pt)++);
	unsigned int md = modrm >> 6;
	unsigned int rm = modrm & 7;

	if (md == 3) { } /* Register. */
	else if (rm == 4)
	{
		/* s-i-b present. */
		unsigned int sib = *((*pt)++);

		if (md == 0)
		{
			if ((sib & 7) == 5) 
			{
				/* An immediate address. */
				processAddr(*pt, op);
				(*pt) += 4;
			}
		}
        else if (md == 1) (*pt)++;
        else if (md == 2) (*pt) += 4;
	}
	else if (md == 0 && rm == 5)
	{
		/* Absolute address. */
		processAddr(*pt, op);
		*pt += 4;
	}
	else
	{
		if (md == 1) *pt += 1;
		else if (md == 2) *pt += 4;
	}
}

/* Added to deal with constants within the
   code rather than in the constant area.  The constant
   area is still needed for the function name.
   DCJM 2/1/2001 
*/
void MD_update_code_addresses(word **addr, word **old, int L, void (*op)(word **))
{
	word *end = (word*)addr + OBJ_OBJECT_LENGTH(L);
	byte *pt = (byte*)addr;
	assert(OBJ_IS_CODE_OBJECT(L));
	/* Find the end of the code (before the constants). */
	end -= end[-1] + 4;
	assert(*end == 0); /* This should be the marker word. */

	while (pt != (byte*)end)
	{
		switch (*pt)
		{
		case 0x50: case 0x51: case 0x52: case 0x53:
		case 0x54: case 0x55: case 0x56: case 0x57: /* Push */
		case 0x58: case 0x59: case 0x5a: case 0x5b:
		case 0x5c: case 0x5d: case 0x5e: case 0x5f: /* Pop */
		case 0x90: /* nop */ case 0xc3: /* ret */
		case 0xf9: /* stc */ case 0xce: /* into */
			pt += 1; break;

		case 0x70: case 0x72: case 0x73: case 0x74: case 0x75: case 0x76:
		case 0x77: case 0x7c: case 0x7d: case 0x7e: case 0x7f: case 0xeb:
			/* short jumps. */
		case 0xcd: /* INT */
		case 0xa8: /* TEST_ACC8 */
		case 0x6a: /* PUSH_8 */
			pt += 2; break;

		case 0xc2: /* RET_16 */
			pt += 3; break;

		case 0x03: case 0x0b: case 0x13: case 0x1b:
		case 0x23: case 0x2b: case 0x33: case 0x3b: /* Add r,ea etc. */
		case 0x88: /* MOVB_R_A */ case 0x89: /* MOVL_R_A */
		case 0x8b: /* MOVL_A_R */
		case 0x62: /* BOUNDL */
		case 0xff: /* Group5 */
		case 0xd1: /* Group2_1_A */
		case 0x8f: /* POP_A */
			pt++; skipea(&pt, op); break;

		case 0xf6: /* Group3_a */
			{
				int isTest = 0;
				pt++;
				/* The test instruction has an immediate operand. */
				if ((*pt & 0x38) == 0) isTest = 1;
				skipea(&pt, op);
				if (isTest) pt++;
				break;
			}

		case 0xf7: /* Group3_A */
			{
				int isTest = 0;
				pt++;
				/* The test instruction has an immediate operand. */
				if ((*pt & 0x38) == 0) isTest = 1;
				skipea(&pt, op);
				if (isTest) pt += 4;
				break;
			}
         
		case 0xc1: /* Group2_8_A */
		case 0xc6: /* MOVB_8_A */
		case 0x83: /* Group1_8_A */
			pt++; skipea(&pt, op); pt++; break;

		case 0x81: /* Group1_32_A */
			{
				pt ++;
				skipea(&pt, op);
				/* Ignore the 32 bit constant here.  It may be
				   untagged and it shouldn't be an address. */
				pt += 4;
				break;
			}

		case 0x8d: /* leal. */
			{
				pt++;
				if ((*pt & 0x3f) == 0x9)
				{
					/* leal ecx,n(ecx).  If this is setting ecx
					   to be the constant pointer then we stop
					   processing here.  This isn't just an optimisation
					   for old code segments, it's needed because old
					   code segments used a jump table for indexed jumps
					   (case expressions) and these don't contain valid
					   i386 instructions. */
					int md = (*pt) & 0xc0;
					if (md == 0x40 || md == 0x80)
					{
						if ((pt[1] & 3) == 2) return;
					}
				}
				skipea(&pt, op);
				break;
			}

		case 0xe8: case 0xe9:
			/* Long jump and call.  These are used within a function and
			   also to call constant (known) functions. */
			{
				int disp, newDisp;
				byte *absAddr, *oldAbsAddr;
				pt++;
				/* Get the displacement. This is signed. */
				disp = pt[0] | (pt[1] << 8) | (pt[2] << 16) | (pt[3] << 24);
				pt += 4;
				absAddr = pt + disp;
				/* Is this local to the segment? */
				if (absAddr >= (byte*)addr && absAddr < (byte*)end) break;

				/* The old value of the displacement was relative to the old
				   address.  */
				absAddr = absAddr - (byte*)addr + (byte*)old;
				oldAbsAddr = absAddr;
				ForConstant((word**)&absAddr, op);
				/* We have to correct the displacement for the new location. */
				newDisp = absAddr - pt;
				if (newDisp != disp)
				{
					unsigned v = (unsigned)newDisp;
					pt[-4] = v & 255; v >>= 8;
					pt[-3] = v & 255; v >>= 8;
					pt[-2] = v & 255; v >>= 8;
					pt[-1] = v & 255; v >>= 8;
				}
				break;
			}

		case 0xc7:/* MOVL_32_A */
			{
				pt++;
				if ((*pt & 0xc0) == 0x40 && (pt[1] & 0x80) != 0)
				{
					/* We may use a move instruction to set the length
					   word on a new segment.  I think this will always be a move
					   to [reg-4] so we should handle that case specially.  Normally
					   all offsets will be positive.  This should be the only negative. */
					/* Negative byte offset - check it's minus one. */
					assert(pt[1] == 0xfc);
					pt += 6; /* Skip the modrm byte, the offset and the constant. */
				}
				else
				{
					skipea(&pt, op);
					processAddr(pt, op);
					pt += 4;
				}
				break;
			}

		case 0xb8: case 0xb9: case 0xba: case 0xbb:
		case 0xbc: case 0xbd: case 0xbe: case 0xbf: /* MOVL_32_R */
		case 0x68: /* PUSH_32 */
			pt ++;
			processAddr(pt, op);
			pt += 4;
			break;
	  
		case 0x0f: /* ESCAPE */
			{
				pt++;
				switch (*pt)
				{
				case 0xb6: /* movzl */
					pt++; skipea(&pt, op); break;

				case 0x80: case 0x81: case 0x82: case 0x83:
				case 0x84: case 0x85: case 0x86: case 0x87:
				case 0x88: case 0x89: case 0x8a: case 0x8b:
				case 0x8c: case 0x8d: case 0x8e: case 0x8f:
					/* Conditional branches with 32-bit displaacement. */
					pt += 5; break;

				default: Crash("Unknown opcode %d at %p\n", *pt, pt);
				}
				break;
			}
			
		default: Crash("Unknown opcode %d at %p\n", *pt, pt);
		}
	}
}
