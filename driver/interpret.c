/*
    Title: 	An interpreter for a compact instruction set.
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
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include "globals.h"
#include "int_opcodes.h"
#include "machine_dep.h"
#include "sys.h"
#include "profiling.h"
#include "mm.h"
#include "arb.h"
#include "processes.h"
#include "run_time.h"
#include "mpoly.h"
#include "gc.h"
#include "basicio.h"
#include "timing.h"
#include "arb.h"
#include "reals.h"
#include "objsize.h"
#include "xwindows.h"
#include "foreign.h"
#include "process_env.h"
#include "network.h"
#include "basicio.h"
#include "sighandler.h"
#include "os_specific.h"

#define VERSION_NUMBER	POLY_version_number

#define	arg1	(pc[0] + pc[1]*256)
#define	arg2	(pc[2] + pc[3]*256)
#define	arg3	(pc[4] + pc[5]*256)
#define	arg4	(pc[6] + pc[7]*256)

#define True	TAGGED(1)
#define False	TAGGED(0)
#define storetrace QuickGC
#define newptr     A.M.pointer
#define locallimit A.M.bottom

int registerMaskVector[1]; /* This is referred to in process_env.c but never actually used in the interpreted version. */

extern StackObject *poly_stack;
//extern word *end_of_stack;
extern word    *save_vec[];
extern word   **save_vec_addr;

int in_run_time_system = 1; /* Always in the RTS. */


static void run_time_call(word u, word *sp, byte *pc, int li);
static int string_test(pstring *x, pstring *y, word *sp, byte *pc, int li);

void MD_init_stack_frame(StackObject *stack, Handle proc, Handle arg)
/* Initialise stack frame. */
{
    word stack_size;
	word *closure = DEREFWORDHANDLE(proc);
    stack_size = OBJ_OBJECT_LENGTH(((word*)stack)[-1]);
    stack->p_space = OVERFLOW_STACK_SIZE;
    stack->p_pc = *(byte**)(closure);
    stack->p_sp  = (word*)stack + stack_size-3; /* sp */
    stack->p_nreg = CHECKED_REGS; /* exception arg and last instruction. */
	stack->p_reg[CHECKED_REGS] = UNCHECKED_REGS;
    stack->p_reg[0] = TAGGED(0); /* Used for exception argument. */
    stack->p_reg[1] = TAGGED(256); /* No instruction. */
	stack->p_sp  = (word*)stack + stack_size;

    /* Set up exception handler */
    /* No previous handler so point it at itself. */
	stack->p_sp--; *(stack->p_sp) = (word)stack->p_sp;
	*(--stack->p_sp) = TAGGED(0); /* Default return address. */
	*(--stack->p_sp) = TAGGED(0); /* Default handler. */
	stack->p_hr = stack->p_sp;

    /* If this function takes an argument store it on the stack. */
	if (arg != 0) *(--stack->p_sp) = (word)DEREFWORDHANDLE(arg);

	*(--stack->p_sp) = TAGGED(0); /* Return address. */
    *(--stack->p_sp) = (word)closure; /* Closure address */
}

int interrupt_requested = 0;
int profile_count_wanted = 0;

#if defined(SOLARIS2) || defined(HPUX64)
#include <ucontext.h>
void MD_increment_profile_count_using_context(ucontext_t *context)
{
}

void MD_interrupt_code_using_context(ucontext_t *context)
{
   interrupt_requested = 1;
}

#elif ! defined(WINDOWS_PC)
void MD_increment_profile_count_using_context(struct sigcontext *context)
{
}

void MD_interrupt_code_using_context(struct sigcontext *context)
{
   interrupt_requested = 1;
}

#else
void MD_interrupt_code(void)
/* Stop the Poly code at a suitable place. */
/* We may get an asynchronous interrupt at any time. */
{
   interrupt_requested = 1;
}
#endif

void MD_set_exception(StackObject *stack, poly_exn *exc)
/* Set up the stack of a process to raise an exception. */
{
   stack->p_reg[1] = TAGGED(INSTR_raise_ex);
   *(--stack->p_sp) = (word)exc; /* push exception data */
}

int MD_is_exception(StackObject *stack) /* Return true if MD_set_exception was called */
{
  /* SPF 29/11/93 - when GC debugging is active, this functions */
  /*                may be called before stack is initialised   */
  if (stack == 0) return 0;

  return stack->p_reg[1] == TAGGED(INSTR_raise_ex);
}

void MD_switch_to_poly(void)
/* (Re)-enter the Poly code from C. */
{
    register word *sp; /* Stack pointer. */
    register byte *pc; /* Program counter. */
    register word *t,u; /* Temporary registers */
    register word *sl; /* Stack limit register. */
    register int  li;  /* Last instruction. */

    RESTART: /* Load or reload the registers and run the code. */
    sp = poly_stack->p_sp; /* Reload these. */
    pc = poly_stack->p_pc;
    li = UNTAGGED(poly_stack->p_reg[1]);
    sl = (word*)poly_stack+OVERFLOW_STACK_SIZE;
    if (li != 256) goto RETRY; /* Re-execute instruction if necessary. */

    for(;;){ /* Each instruction */

//		char buff[100];
		li = *pc++; /* Get next instruction. */

		RETRY:
//		sprintf(buff, "PC=%p, i=%x\n", pc, li);
//		OutputDebugString(buff);
		/* Check for stack overflow and interrupts. These can be done less
		   frequently than every instruction. */
		if (sp < sl) {
			poly_stack->p_sp = sp;
			poly_stack->p_pc = pc;
			poly_stack->p_reg[1] = TAGGED(li);
			check_current_stack_size(sp);
			goto RESTART;
		}

		if (interrupt_requested) {
			interrupt_requested = 0;
			poly_stack->p_sp = sp;
			poly_stack->p_pc = pc;
			poly_stack->p_reg[1] = TAGGED(li);
			execute_pending_interrupts();
			goto RESTART;
		}

		if (profile_count_wanted) {
			add_count(pc, sp, profile_count_wanted);
			profile_count_wanted = 0;
		}

		switch(li) {

		case INSTR_enter_int: pc++; /* Skip the argument. */ break;

		case INSTR_jump_false:
			u = *sp++; /* Pop argument */
			if (u == True) { pc += 1; break; }
			/* else - false - take the jump */

		case INSTR_jump: pc += *pc + 1; break;

		case INSTR_push_handler: /* Save the old handler value. */
			*(--sp) = (word)poly_stack->p_hr; /* Push old handler */
			break;

		case INSTR_set_handler: /* Set up a handler */
			*(--sp) = (word)(pc + *pc + 1); /* Address of handler */
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
			(word)((byte*)u + ((byte*)u)[0] + ((byte*)u)[1]*256 + 2);
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
			*(--sp) = (word)(pc+6); /* Push return address to point after
						   instruction. */
			*(--sp) = (word)t; /* Push static link */
			pc = *(byte**)u;
			break;

		case INSTR_call_sl_X:
			/* Get static link value. */
			t = sp+arg3;
			for(u = 1; u <= arg4; u++) t = (word*)t[-1];
			u = (word)(pc+arg2+(arg1+4)*sizeof(word)+4); /* Get entry point. */
			*(--sp) = (word)(pc+8); /* Push return address to point after
						   instruction. */
			*(--sp) = (word)t; /* Push static link */
			pc = *(byte**)u;
			break;

		case INSTR_tail_3_b:
		   u = 3;
		   t = sp + u;
		   sp = t + *pc;
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
		   sp = t + *pc;
		   goto TAIL_CALL;

		case INSTR_tail_b_b:
		   u = *pc;
		   t = sp + u;
		   sp = t + pc[1];
		   goto TAIL_CALL;

		case INSTR_tail:
		   /* Tail recursive call. */
		   /* Move items up the stack. */
		   /* There may be an overlap if the function we are calling
			  has more args than this one. */
		   u = arg1;
		   t = sp + u;
		   sp = t + arg2;
		   TAIL_CALL: /* For general case. */
		   if (u < 2) crash("Invalid argument\n");
		   for (; u > 0; u--) *(--sp) = *(--t);
		   pc = (byte*)*sp++; /* Pop the original return address. */
		   li = INSTR_call_closure; /* If we have to re-execute. */
		   /* And drop through. */

		case INSTR_call_closure: /* Closure call - may be machine code. */
		CALL_CLOSURE:  /* Jumped to from POLY_SYS_callcode */
			t = (word*)*sp; /* Closure */
			u = *t;   /* Get code address. (1st word of closure) */

			if (IS_INT(u)) { /* Closure address is io vector */
				sp++; /* Remove closure. */
				u = UNTAGGED(u);
//				sprintf(buff, "System call %d\n", u);
//				OutputDebugString(buff);
				switch(u) {

				case POLY_SYS_callcode_tupled:
					{
						word *v;
						t = (word*)(*sp++);
						v = ((word**)t)[1]; /* Arguments. */
						if (v != nil_value) /* No args. */
						{
							u = OBJ_OBJECT_LENGTH(v[-1]); /* No. of args. */
							for (; u > 0; u--) *(--sp) = *(v++);
						}
						*(--sp) = t[0]; /* Push closure. */
						goto CALL_CLOSURE;
					}

#if(0)
				case POLY_SYS_callcode:
					/* Move the arguments from the argument vector onto the
					   stack. */
					if ((word*)*sp == nil_value) sp++; /* No args. */
					else {
						t = (word*)*sp++; /* Argument pointer */
						u = OBJ_OBJECT_LENGTH(t[-1]); /* No. of args. */
						sp -= u; /* Space for args. */
						*sp = sp[u]; /* Move the closure down */
						for(; u > 0; u--) sp[u] = t[u-1];
					}
					/* The closure is on the stack. */
					goto CALL_CLOSURE;
#endif

				case POLY_SYS_int_eq: u = *sp++; *sp = (u == *sp)?True:False; break;

				case POLY_SYS_int_neq: u = *sp++; *sp = (u != *sp)?True:False; break;

				case POLY_SYS_word_eq: 
				   u = *sp++;
				   *sp = u == *sp ? True : False;
				   break;

				case POLY_SYS_word_neq:
				   u = *sp++;
				   *sp = u == *sp ? False : True;
				   break;

				case POLY_SYS_word_geq:
					u = *sp++; *sp = ((unsigned)*sp >= (unsigned)u)?True:False; break;

				case POLY_SYS_word_leq:
					u = *sp++; *sp = ((unsigned)*sp <= (unsigned)u)?True:False; break;

				case POLY_SYS_word_gtr:
					u = *sp++; *sp = ((unsigned)*sp > (unsigned)u)?True:False; break;

				case POLY_SYS_word_lss:
					u = *sp++; *sp = ((unsigned)*sp < (unsigned)u)?True:False; break;

				case POLY_SYS_or_word:
					u = *sp++; *sp = TAGGED(UNTAGGED(*sp) | UNTAGGED(u)); break; 

				case POLY_SYS_and_word: 
					u = *sp++; *sp = TAGGED(UNTAGGED(*sp) & UNTAGGED(u)); break; 

				case POLY_SYS_not_bool:  *sp = (*sp == True) ? False : True; break; 

				case POLY_SYS_string_length: 
					/* Length is first word of string unless it is a
					   single character. */
					if (IS_INT(*sp)) *sp = TAGGED(1);
					else *sp = TAGGED(*(word*)(*sp));
					break; 

				case POLY_SYS_set_string_length: 
					/* Store the length word of a string. */
					t = (word*)UNTAGGED(*sp++);
					((word**)*sp)[0] = t;
					*sp = TAGGED(0);
					break; 

				case POLY_SYS_string_sub: 
					u = UNTAGGED(*sp++);
					if (IS_INT(*sp)) /* Single char. */ {
						if (u != 1) raise_exception0(EXC_subscript);
						/* else character is the string */}
					else if (u > 0 && u <= ((pstring)*sp)->length)
						*sp = TAGGED(((pstring)*sp)->chars[u-1]);
					else raise_exception0(EXC_subscript);
					break; 

				case POLY_SYS_mul_word:
					u = *sp++; *sp = TAGGED(UNTAGGED(*sp) * UNTAGGED(u)); break;

				case POLY_SYS_plus_word: 
					u = *sp++; *sp = TAGGED(UNTAGGED(*sp) + UNTAGGED(u)); break;

				case POLY_SYS_minus_word: 
					u = *sp++; *sp = TAGGED(UNTAGGED(*sp) - UNTAGGED(u)); break; 

				case POLY_SYS_div_word:
					if ((u = UNTAGGED(*sp++)) == 0) raise_exception0(EXC_divide);
					*sp = TAGGED((unsigned)UNTAGGED(*sp) / (unsigned)u); break; 

				case POLY_SYS_mod_word: 
					if ((u = UNTAGGED(*sp++)) == 0) raise_exception0(EXC_divide);
					*sp = TAGGED((unsigned)UNTAGGED(*sp) % (unsigned)u); break; 

				case POLY_SYS_xor_word: 
					u = *sp++; *sp = TAGGED(UNTAGGED(*sp) ^ UNTAGGED(u)); break; 

				case POLY_SYS_shift_left_word: 
					u = *sp++;
					if (UNTAGGED(u) > sizeof(word)*8) *sp = TAGGED(0);
					else *sp = TAGGED(UNTAGGED(*sp) << UNTAGGED(u));
					break; 

				case POLY_SYS_shift_right_word:
					u = *sp++;
					if (UNTAGGED(u) > sizeof(word)*8) *sp = TAGGED(0);
					else *sp = TAGGED(((unsigned)UNTAGGED(*sp)) >> UNTAGGED(u));
					break; 

				case POLY_SYS_shift_right_arith_word:
					u = *sp++;
					if (UNTAGGED(u) > sizeof(word)*8)
					{
						if (UNTAGGED(*sp) < 0) *sp = TAGGED(-1);
						else *sp = TAGGED(0);
					}
					else *sp = TAGGED(UNTAGGED(*sp) >> UNTAGGED(u)); break;

				case POLY_SYS_load_byte:
					u = UNTAGGED(*sp++);
					*sp = TAGGED((*(byte**)sp)[u]); break; 

				case POLY_SYS_load_word: 
					u = UNTAGGED(*sp++);
					*sp = (*(word**)sp)[u]; break; 

				case POLY_SYS_assign_byte: 
					if (IS_LOCAL_MUTABLE(sp[2]))
					{
						t = (word*)*sp++; u = UNTAGGED(*sp++);
						((byte*)*sp)[u] = (byte)UNTAGGED(t);
						*sp = TAGGED(0);
						break; 
					}
					else goto USE_RTS_CALL;

				case POLY_SYS_assign_word: 
					if (IS_LOCAL_MUTABLE(sp[2]))
					{
						t = (word*)*sp++; u = UNTAGGED(*sp++);
						((word**)*sp)[u] = t;
						*sp = TAGGED(0); break;
					}
					else goto USE_RTS_CALL;

                case POLY_SYS_str_compare:
                    sp[1] = TAGGED(string_test((pstring*)(sp+1), (pstring*)sp, sp, pc, li));
                    sp++; break;

				case POLY_SYS_teststreq:
					sp[1] = ((string_test((pstring*)(sp+1), (pstring*)sp, sp, pc, li) == 0) ? True : False);
					sp++; break;

				case POLY_SYS_teststrneq:
					sp[1] = ((string_test((pstring*)(sp+1), (pstring*)sp, sp, pc, li) != 0) ? True : False);
					sp++; break;

				case POLY_SYS_teststrgtr:
					sp[1] = ((string_test((pstring*)(sp+1), (pstring*)sp, sp, pc, li) > 0) ? True : False);
					sp++; break;

				case POLY_SYS_teststrlss:
					sp[1] = ((string_test((pstring*)(sp+1), (pstring*)sp, sp, pc, li) < 0) ? True : False);
					sp++; break;

				case POLY_SYS_teststrgeq:
					sp[1] = ((string_test((pstring*)(sp+1), (pstring*)sp, sp, pc, li) >= 0) ? True : False);
					sp++; break;

				case POLY_SYS_teststrleq:
					sp[1] = ((string_test((pstring*)(sp+1), (pstring*)sp, sp, pc, li) <= 0) ? True : False);
					sp++; break;

				case POLY_SYS_lockseg:
					/* Clear the mutable bit. */
					((word*)*sp)[-1] &= ~OBJ_MUTABLE_BIT;
					*sp = TAGGED(0);
					break;

#if (0)
				case POLY_SYS_network:
					/* This is a bit of a mess.  This entry was previously
					   used for lock_or_copy and when porting old databases
					   that call still arises.  Check the argument, and if
					   it's mutable, assume that this is a lock_or_copy call. */
					if (!IS_INT(*sp) && OBJ_IS_MUTABLE_OBJECT(((word*)*sp)[-1]))
					{
						((word*)*sp)[-1] &= ~OBJ_MUTABLE_BIT;
						/* Leave the address of the object on the stack. */
						break;
					}
					else goto USE_RTS_CALL;
#endif

				case POLY_SYS_get_length: 
					/* Return the length word. */
					*sp = TAGGED(OBJ_OBJECT_LENGTH(((word*)*sp)[-1])); break;

				case POLY_SYS_get_flags:
					/* Return the flags. */
					*sp = TAGGED((((word*)*sp)[-1] >> 24) & 0xff); break;

				case POLY_SYS_get_dbentry:
					/* This is a bit of a fudge and relies on the relative
					   offsets of db_specific_list and processes. */
					*(--sp) = ((word*)processes)[2]; break;

				case POLY_SYS_exit:
					finish(*sp);

				case POLY_SYS_is_short:
					*sp = IS_INT(*sp) ? TAGGED(1) : TAGGED(0); break;

				case POLY_SYS_io_operation:
					/* The io_operation call has changed between the old Poly
					   version and the new ML version.  In Poly it took two
					   parameters, the first always being an empty type.
					   In ML it takes just one parameter. */
					if (*sp == TAGGED(0)) sp++;
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
							/* Insufficient store.  Use the
							   RTS call which will garbage collect. */
							newptr += u+1;
							goto USE_RTS_CALL;
						}
						*newptr = UNTAGGED(sp[2]) | (UNTAGGED(sp[1]) << 24) | OBJ_MUTABLE_BIT;
						t = newptr+1;
						if (UNTAGGED(sp[1]) & (OBJ_BYTE_BIT >> 24)) sp[0] = UNTAGGED(sp[0]);
						for(; --u >= 0;) t[u] = sp[0];
						sp += 2; *sp = (word)t;
						break;
					}

				case POLY_SYS_exception_trace:
					u = *sp; /* Procedure to call. */
					*(--sp) = (word)pc; /* Push a return address. */
					*(--sp) = (word)poly_stack->p_hr; /* Push old handler */
					*(--sp) = TAGGED(0); /* Marks exception trace. */
					*(--sp) = TAGGED(0); /* Catch everything. */
					poly_stack->p_hr = sp; /* Handler is here. */
					pc = (byte*)TAGGED(1); /* Special return address. */
					*(--sp) = TAGGED(0); /* Unit argument to the function. */
					*(--sp) = u; /* Push the procedure. */
					goto CALL_CLOSURE;

				case POLY_SYS_is_big_endian: {
					union { word wrd; char chrs[sizeof(word)]; } endian;
					endian.wrd = 1;
					*(--sp) = endian.chrs[0] == 0 ? TAGGED(1) : TAGGED(0);
					break;
				}

				case POLY_SYS_bytes_per_word:
					*(--sp) = TAGGED(sizeof(word)); break;

				default:
                USE_RTS_CALL:
					run_time_call(u, sp, pc, li); goto RESTART;
				}
			} /* End of system calls. */
			else {
				sp--;
				*sp = sp[1];      /* Move closure up. */
				sp[1] = (word)pc; /* Save return address. */
				pc = (byte*)u;    /* Get entry point. */
			}
			break;

		case INSTR_return_w:
			u = arg1; /* Get no. of args to remove. */

			RETURN: /* Common code for return. */
			t = (word*)*sp++; /* Result */
			sp++; /* Remove the link/closure */
			pc = (byte*)*sp++; /* Return address */
			sp += u; /* Add on number of args. */
			if (pc == (byte*)TAGGED(0)) kill_selfc(); /* Exit */
			else if (pc == (byte*)TAGGED(1)) {
				/* Return from a call to exception_trace when an exception
				   has not been raised. */
			sp += 1;
			poly_stack->p_hr = (word*)sp[1];
			*sp = (word)t;
			u = 1;
			goto RETURN; }
			*(--sp) = (word)t; /* Result */
			break;

		case INSTR_return_b: u = *pc; goto RETURN;
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
			if (t[1] == TAGGED(*pc)) {
			/* Does the tag match? */
				*sp = t[0]; pc += 1; break; }
			else raise_exception0(EXC_project);

		case INSTR_raise_ex:
			poly_stack->p_reg[0] = *sp; /* Get exception data */
			u = *(word*)*sp; /* Get exception identifier. */
			poly_stack->p_sp = sp; /* Save this in case of trace. */
			t = poly_stack->p_hr;  /* First handler */
			/* Handlers consist of one or more pairs of identifier and
			   code address, followed by the address of the next handler. */
			while (*t != TAGGED(0) && *t != u) {
				/* Loop until we find an ELSE handler or one that matches */
				t += 2; /* Go on to next. */
				/* If it points into stack it must be a pointer to the next
				   handler. */
				if ((word*)*t > t && (word*)*t < end_of_stack) t = (word*)*t;
			}
			t++; /* Skip over the identifier to point at the code address. */
			if (*t == TAGGED(0)) { /* Trace this exception. */ 
				word *nextHandler;
				*sp = (word)pc; /* So that this proc. will be included. */
				t++; /* Next handler. */
				nextHandler = t;
				ex_tracec((Handle)&poly_stack->p_reg[0], (Handle)&nextHandler);
			}
			poly_stack->p_pc = (byte*)*t;
			/* Now remove this handler. */
			sp = t;
			while((t = (word*)*sp) < sp || t > end_of_stack) sp++;
			poly_stack->p_hr = t; /* Restore old handler */
			sp++; /* Remove that entry. */
			poly_stack->p_sp = sp;
			poly_stack->p_reg[1] = TAGGED(256); /* Get the next instruction. */
			goto RESTART; /* Restart in case pc is persistent. */

		case INSTR_get_store_w:
			u = arg1; t = (word*)2;

			GET_STORE: /* Common code for allocation. */
			newptr -= u+1;
			if (newptr < locallimit) {
				poly_stack->p_sp = sp;
				poly_stack->p_pc = pc;
				poly_stack->p_reg[1] = TAGGED(li);
				newptr += u+1;
				storetrace(u);
				goto RESTART; }
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
			if (newptr < locallimit) {
				poly_stack->p_sp = sp;
				poly_stack->p_pc = pc;
				poly_stack->p_reg[1] = TAGGED(li);
				newptr += u+1;
				storetrace(u);
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

		case INSTR_const_addr:
			*(--sp) = *(word*)(pc + arg1 + 2); pc += 2; break;

		case INSTR_const_addr_Xb:
			*(--sp) =
				*(word*)(pc + (pc[0]+4)*sizeof(word) + pc[1] + pc[2]*256 + 3);
			pc += 3;
			break;

		case INSTR_const_addr_Xw:
			*(--sp) = *(word*)(pc + (arg1+4)*sizeof(word)+arg2 + 4); pc += 4; break;

		case INSTR_const_int_w: *(--sp) = TAGGED(arg1); pc += 2; break;

		case INSTR_io_vec_entry:
			*(--sp) = (word)interface_map[*pc];
			if (*pc != POLY_SYS_nullorzero && *pc != POLY_SYS_stdin &&
					(*sp == 0 || **(word**)sp == 0)) {
				crash("Undefined interface map entry: %d\n", *pc);
			}
			pc += 1;
			break;

		case INSTR_const_nil: *(--sp) = (word)nil_value; break;

		case INSTR_jump_back: pc -= *pc + 1; break;

		case INSTR_lock: ((word*)*sp)[-1] &= ~OBJ_MUTABLE_BIT; break;

		case INSTR_ldexc: *(--sp) = poly_stack->p_reg[0]; break;

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


		/* move_to_vec only occurs to newly allocated store so there is
		   no problem with persistent store faults. */
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
			*(--sp) = (word)(pc+3); /* Push return address to point after
						   instruction. */
			*(--sp) = (word)t; /* Push static link */
			pc = *(byte**)u;
			break;

		case INSTR_call_sl_cX:
			/* Get static link value. */
			u = pc[3];
			t = sp + (u >> 4) + 2;
			for(u = u & 0xf; u > 0; u--) t = (word*)t[-1];
			u = (word)(pc+(pc[0]+4)*sizeof(word) + pc[1] + pc[2]*256+3); /* Get entry point. */
			*(--sp) = (word)(pc+4); /* Push return address to point after
						   instruction. */
			*(--sp) = (word)t; /* Push static link */
			pc = *(byte**)u;
			break;

#if (0)
		case INSTR_io_vec_5: *(--sp) = (word)interface_map[POLY_SYS_prints]; break;
#endif
		case INSTR_io_vec_6: *(--sp) = (word)interface_map[POLY_SYS_strconcat]; break;
#if (0)
		case INSTR_io_vec_226: *(--sp) = (word)interface_map[POLY_SYS_minus_int]; break;
#endif
		case INSTR_io_vec_229: *(--sp) = (word)interface_map[POLY_SYS_int_eq]; break;
		case INSTR_io_vec_233: *(--sp) = (word)interface_map[POLY_SYS_int_gtr]; break;
		case INSTR_io_vec_236: *(--sp) = (word)interface_map[POLY_SYS_or_word]; break;
		case INSTR_io_vec_251: *(--sp) = (word)interface_map[POLY_SYS_word_eq]; break;
		case INSTR_io_vec_253: *(--sp) = (word)interface_map[POLY_SYS_load_word]; break;
		case INSTR_io_vec_255: *(--sp) = (word)interface_map[POLY_SYS_assign_word]; break;

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
		   else *sp = u == *sp ? True : False;
		   break;

		case INSTR_assign_word:
			t = (word*)*sp++; u = UNTAGGED(*sp++);
			((word**)*sp)[u] = t;
			*sp = TAGGED(0); break;

        case INSTR_container: /* Create a container. */
            /* This is supposed to be on the stack but that causes problems in gencde
               so we create a mutable segment on the heap. */
 			u = arg1;
			newptr -= u+1;
			if (newptr < locallimit) {
				poly_stack->p_sp = sp;
				poly_stack->p_pc = pc;
				poly_stack->p_reg[1] = TAGGED(li);
				newptr += u+1;
				storetrace(u);
				goto RESTART;
			}
			*newptr = OBJ_MUTABLE_BIT | u;
			t = newptr+1;
			for(; --u >= 0; ) t[u] = TAGGED(0);
            *(--sp) = (word)t; /* Push the address of the container. */
            pc += 2;
            break;

        case INSTR_set_container: /* Copy a tuple into a container. */
            t =(word*)*sp++; /* Pop the source tuple address. */
            for (u = arg1; --u >= 0; ) ((word*)(*sp))[u] = t[u]; /* Copy the items. */
            sp++;
            pc += 2;
            break;

        case INSTR_tuple_container: /* Create a tuple from a container. */
			u = arg1;
			newptr -= u+1;
			if (newptr < locallimit) {
				poly_stack->p_sp = sp;
				poly_stack->p_pc = pc;
				poly_stack->p_reg[1] = TAGGED(li);
				newptr += u+1;
				storetrace(u);
				goto RESTART;
			}
			*newptr = u;
			t = newptr+1;
			for(; --u >= 0; ) t[u] = ((word*)(*sp))[u];
			*sp = (word)t;
			pc += 2;
			break;

		default: crash("Unknown instruction %x\n", li);

		} /* switch */
    } /* for */
} /* MD_switch_to_poly */

void MD_set_for_retry(int ioCall) /* Nothing to do. */
{
}

static void run_time_call(word u, word *sp, byte *pc, int li)
{
    Handle indRes;
	word **svp;
	word noIndRes;
	/* Save the state so that the instruction can be retried if necessary. */
    poly_stack->p_pc = pc; /* Pc value after instruction. */
    poly_stack->p_reg[1] = TAGGED(li); /* Previous instruction. */
    poly_stack->p_sp = sp-1; /* Include the closure address. */
    svp = save_vec;

    switch(u) {
	case POLY_SYS_install_root:
		/* Call the function first and get the result. */
		*svp++ = *(word**)sp;
	    save_vec_addr = svp;
		/* install_rootc does not return. */
		install_rootc((Handle)(svp-1));
		break;

#if (0)
	case POLY_SYS_printb:
		/* Call the function first and get the result. */
		*svp++ = *(word**)sp;
	    save_vec_addr = svp;
		noIndRes = OLD_printbc((Handle)(svp-1));
		/* Now save it to the stack, which could move as a result of the
		   function call. */
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_prints:
		*svp++ = *(word**)sp;
	    save_vec_addr = svp;
		noIndRes = OLD_printsc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;
#endif

	case POLY_SYS_strconcat:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = strconcatc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

#if (0)
	case POLY_SYS_convch:
		*svp++ = *(word**)sp;
	    save_vec_addr = svp;
		noIndRes = (word)OLD_convchc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;
#endif

	case POLY_SYS_chdir:
		*svp++ = *(word**)sp;
	    save_vec_addr = svp;
		noIndRes = change_dirc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_substring:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		*svp++ = ((word**)sp)[2];
		save_vec_addr = svp;
		indRes = substringc((Handle)(svp-3), (Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 3;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_profiler:
		*svp++ = *(word**)sp;
	    save_vec_addr = svp;
		noIndRes = (word)profilerc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_commit:
	    save_vec_addr = svp;
		noIndRes = (word)commitc();
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_createf:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		*svp++ = ((word**)sp)[2];
		save_vec_addr = svp;
		noIndRes = createfc((Handle)(svp-3), (Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 3;
		*(poly_stack->p_sp) = noIndRes;

	case POLY_SYS_aplus:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = add_longc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_aminus:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = sub_longc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_amul:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = mult_longc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_adiv:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = div_longc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_amod:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = rem_longc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_aneg:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		indRes = neg_longc((Handle)(svp-1));
		*(++poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_equala:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = equal_longc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_int_geq:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = ge_longc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_int_leq:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = le_longc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_int_lss:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = ls_longc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_int_gtr:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = gt_longc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_Add_real:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = (word)Real_addc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_Sub_real:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = (word)Real_subc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_Mul_real:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = (word)Real_mulc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_Div_real:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = (word)Real_divc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_Neg_real:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = (word)Real_negc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_Repr_real:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = (word)Real_reprc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_conv_real:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = (word)Real_convc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_real_to_int:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = Real_intc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_int_to_real:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = (word)Real_floatc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_sqrt_real:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = (word)Real_sqrtc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_sin_real:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = (word)Real_sinc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_cos_real:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = (word)Real_cosc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_arctan_real:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = (word)Real_arctanc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_exp_real:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = (word)Real_expc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_ln_real:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = (word)Real_lnc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_fork_process:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = fork_processc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_choice_process:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = choice_processc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_int_process:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = int_processc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_send_on_channel:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = send_on_channelc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_receive_on_channel:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = receive_on_channelc((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_objsize:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		indRes = objsize_c((word *)(svp-1));
		*(++poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_showsize:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		indRes = showsize_c((word *)(svp-1));
		*(++poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_assign_byte:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		*svp++ = ((word**)sp)[2];
		save_vec_addr = svp;
		noIndRes = assign_byte_long_c((Handle)(svp-3), (Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 3;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_assign_word:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		*svp++ = ((word**)sp)[2];
		save_vec_addr = svp;
		noIndRes = assign_word_long_c((Handle)(svp-3), (Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 3;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_set_dbentry:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = set_dbentry_c((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_timing_dispatch:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = timing_dispatch_c((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_getdbasetime:
		save_vec_addr = svp;
		indRes = get_dbasetime_c();
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_interrupt_console_processes:
		save_vec_addr = svp;
		/* interrupt_console_processes_c does not return. */
		interrupt_console_processes_c();
		break;

	case POLY_SYS_install_subshells:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = install_subshells_c((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_XWindows:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		indRes = XWindows_c((Handle)(svp-1));
		*(++poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_full_gc:
		save_vec_addr = svp;
		noIndRes = full_gc_c();
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_stack_trace:
		save_vec_addr = svp;
		noIndRes = stack_trace_c();
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_foreign_dispatch:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = foreign_dispatch_c((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_process_env:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = process_env_dispatch_c((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_shrink_stack:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = shrink_stack_c((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_set_flags:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = set_flags_c((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_move_bytes:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		*svp++ = ((word**)sp)[2];
		*svp++ = ((word**)sp)[3];
		*svp++ = ((word**)sp)[4];
		save_vec_addr = svp;
		noIndRes = move_bytes_long_c((Handle)(svp-5), (Handle)(svp-4), (Handle)(svp-3),
									 (Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 5;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_move_words:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		*svp++ = ((word**)sp)[2];
		*svp++ = ((word**)sp)[3];
		*svp++ = ((word**)sp)[4];
		save_vec_addr = svp;
		noIndRes = move_words_long_c((Handle)(svp-5), (Handle)(svp-4), (Handle)(svp-3),
									 (Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 5;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_io_dispatch:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		*svp++ = ((word**)sp)[2];
		save_vec_addr = svp;
		indRes = IO_dispatch_c((Handle)(svp-3), (Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 3;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_network:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = Net_dispatch_c((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_os_specific:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = OS_spec_dispatch_c((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_signal_handler:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = Sig_dispatch_c((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_int_to_word:
	case POLY_SYS_get_first_long_word:
		*svp++ = *(word**)sp;
		save_vec_addr = svp;
		noIndRes = int_to_word_c((Handle)(svp-1));
		*(++poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_xora:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = xor_longc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_ora:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = or_longc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_anda:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		indRes = and_longc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = (word)DEREFWORDHANDLE(indRes);
		break;

	case POLY_SYS_Real_str:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		*svp++ = ((word**)sp)[2];
		save_vec_addr = svp;
		noIndRes = (word)Real_strc((Handle)(svp-3), (Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 3;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_Real_geq:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = Real_geqc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_Real_leq:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = Real_leqc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_Real_gtr:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = Real_gtrc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_Real_lss:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = Real_lssc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_Real_eq:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = Real_eqc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_Real_neq:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = Real_neqc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_Real_Dispatch:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		save_vec_addr = svp;
		noIndRes = (word)Real_dispatchc((Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 2;
		*(poly_stack->p_sp) = noIndRes;
		break;

	case POLY_SYS_alloc_store:
		*svp++ = *(word**)sp;
		*svp++ = ((word**)sp)[1];
		*svp++ = ((word**)sp)[2];
		save_vec_addr = svp;
		noIndRes = (word)alloc_store_long_c((Handle)(svp-3),
						(Handle)(svp-2), (Handle)(svp-1));
		poly_stack->p_sp += 3;
		*(poly_stack->p_sp) = noIndRes;
		break;

	default: crash("Unknown function %d\n", u);

    }
    /* Continue with the next instruction. */
    poly_stack->p_reg[1] = TAGGED(256); /* Take next instruction. */
} /* run_time_call */


static STRING s_test_x, s_test_y;
static pstring s_x_addr = &s_test_x, s_y_addr = &s_test_y;

static int string_test(pstring *x, pstring *y, word *sp, byte *pc, int li)
/* Returns -1, 0, +1 if the first string is less, equal to or greater than the
   second. These are addresses of the strings because calling
   fix_persistent_address could result in a garbage-collection which could
   move the other string. */
{
    int i;
    /* Deal with single characters. */
    if (IS_INT(*x)) {
		s_test_x.length = 1; s_test_x.chars[0] = (char)UNTAGGED((word)*x);
		x = &s_x_addr;}
    if (IS_INT(*y)) {
		s_test_y.length = 1; s_test_y.chars[0] = (char)UNTAGGED((word)*y);
		y = &s_y_addr;}
    /* Now do the comparison. */
    for(i = 0; i < (*x)->length && i < (*y)->length; i++)
        if ((*x)->chars[i] != (*y)->chars[i])
	    return (*x)->chars[i] < (*y)->chars[i] ? -1 : 1;
    /* They must be equal or one must be a leading substring of the other. */
    if (i < (*x)->length) return 1; /* y must be the substring. */
    else if (i < (*y)->length) return -1; /* x must be the substring */
    else return 0; /* They must be equal. */
}

void MD_init_interface_vector(void)
{
    add_word_to_io_area(POLY_SYS_exit, TAGGED(POLY_SYS_exit));
    add_word_to_io_area(POLY_SYS_install_root, TAGGED(POLY_SYS_install_root));
    add_word_to_io_area(POLY_SYS_strconcat, TAGGED(POLY_SYS_strconcat));
    add_word_to_io_area(POLY_SYS_alloc_store, TAGGED(POLY_SYS_alloc_store));
    add_word_to_io_area(POLY_SYS_chdir, TAGGED(POLY_SYS_chdir));
    add_word_to_io_area(POLY_SYS_substring, TAGGED(POLY_SYS_substring));
    add_word_to_io_area(POLY_SYS_get_length, TAGGED(POLY_SYS_get_length));
    add_word_to_io_area(POLY_SYS_get_flags, TAGGED(POLY_SYS_get_flags));
    add_word_to_io_area(POLY_SYS_str_compare, TAGGED(POLY_SYS_str_compare));
    add_word_to_io_area(POLY_SYS_teststreq, TAGGED(POLY_SYS_teststreq));
    add_word_to_io_area(POLY_SYS_teststrneq, TAGGED(POLY_SYS_teststrneq));
    add_word_to_io_area(POLY_SYS_teststrgtr, TAGGED(POLY_SYS_teststrgtr));
    add_word_to_io_area(POLY_SYS_teststrlss, TAGGED(POLY_SYS_teststrlss));
    add_word_to_io_area(POLY_SYS_teststrgeq, TAGGED(POLY_SYS_teststrgeq));
    add_word_to_io_area(POLY_SYS_teststrleq, TAGGED(POLY_SYS_teststrleq));
    add_word_to_io_area(POLY_SYS_exception_trace, TAGGED(POLY_SYS_exception_trace));
    add_word_to_io_area(POLY_SYS_commit, TAGGED(POLY_SYS_commit));
    add_word_to_io_area(POLY_SYS_set_dbentry, TAGGED(POLY_SYS_set_dbentry));
    add_word_to_io_area(POLY_SYS_get_dbentry, TAGGED(POLY_SYS_get_dbentry));
    add_word_to_io_area(POLY_SYS_createf, TAGGED(POLY_SYS_createf));
    add_word_to_io_area(POLY_SYS_lockseg, TAGGED(POLY_SYS_lockseg));
    add_word_to_io_area(POLY_SYS_profiler, TAGGED(POLY_SYS_profiler));
    add_word_to_io_area(POLY_SYS_is_short, TAGGED(POLY_SYS_is_short));
//    add_word_to_io_area(POLY_SYS_raiseexception, TAGGED(POLY_SYS_raiseexception));
//    add_word_to_io_area(POLY_SYS_printw, TAGGED(POLY_SYS_printw));
//    add_word_to_io_area(POLY_SYS_reprw, TAGGED(POLY_SYS_reprw));
    add_word_to_io_area(POLY_SYS_aplus, TAGGED(POLY_SYS_aplus));
    add_word_to_io_area(POLY_SYS_aminus, TAGGED(POLY_SYS_aminus));
    add_word_to_io_area(POLY_SYS_amul, TAGGED(POLY_SYS_amul));
    add_word_to_io_area(POLY_SYS_adiv, TAGGED(POLY_SYS_adiv));
    add_word_to_io_area(POLY_SYS_amod, TAGGED(POLY_SYS_amod));
    add_word_to_io_area(POLY_SYS_aneg, TAGGED(POLY_SYS_aneg));

    add_word_to_io_area(POLY_SYS_xora, TAGGED(POLY_SYS_xora));
    add_word_to_io_area(POLY_SYS_ora, TAGGED(POLY_SYS_ora));
    add_word_to_io_area(POLY_SYS_anda, TAGGED(POLY_SYS_anda));
    add_word_to_io_area(POLY_SYS_Real_str, TAGGED(POLY_SYS_Real_str));
    add_word_to_io_area(POLY_SYS_Real_geq, TAGGED(POLY_SYS_Real_geq));
    add_word_to_io_area(POLY_SYS_Real_leq, TAGGED(POLY_SYS_Real_leq));
    add_word_to_io_area(POLY_SYS_Real_gtr, TAGGED(POLY_SYS_Real_gtr));
    add_word_to_io_area(POLY_SYS_Real_lss, TAGGED(POLY_SYS_Real_lss));
    add_word_to_io_area(POLY_SYS_Real_eq, TAGGED(POLY_SYS_Real_eq));
    add_word_to_io_area(POLY_SYS_Real_neq, TAGGED(POLY_SYS_Real_neq));
    add_word_to_io_area(POLY_SYS_Real_Dispatch, TAGGED(POLY_SYS_Real_Dispatch));

//    add_word_to_io_area(POLY_SYS_testa, TAGGED(POLY_SYS_testa));
    add_word_to_io_area(POLY_SYS_equala, TAGGED(POLY_SYS_equala));
    add_word_to_io_area(POLY_SYS_Add_real, TAGGED(POLY_SYS_Add_real));
    add_word_to_io_area(POLY_SYS_Sub_real, TAGGED(POLY_SYS_Sub_real));
    add_word_to_io_area(POLY_SYS_Mul_real, TAGGED(POLY_SYS_Mul_real));
    add_word_to_io_area(POLY_SYS_Div_real, TAGGED(POLY_SYS_Div_real));
//    add_word_to_io_area(POLY_SYS_Comp_real, TAGGED(POLY_SYS_Comp_real));
    add_word_to_io_area(POLY_SYS_Neg_real, TAGGED(POLY_SYS_Neg_real));
//    add_word_to_io_area(POLY_SYS_Print_real, TAGGED(POLY_SYS_Print_real));
    add_word_to_io_area(POLY_SYS_Repr_real, TAGGED(POLY_SYS_Repr_real));
    add_word_to_io_area(POLY_SYS_conv_real, TAGGED(POLY_SYS_conv_real));
    add_word_to_io_area(POLY_SYS_real_to_int, TAGGED(POLY_SYS_real_to_int));
    add_word_to_io_area(POLY_SYS_int_to_real, TAGGED(POLY_SYS_int_to_real));
    add_word_to_io_area(POLY_SYS_sqrt_real, TAGGED(POLY_SYS_sqrt_real));
    add_word_to_io_area(POLY_SYS_sin_real, TAGGED(POLY_SYS_sin_real));
    add_word_to_io_area(POLY_SYS_cos_real, TAGGED(POLY_SYS_cos_real));
    add_word_to_io_area(POLY_SYS_arctan_real, TAGGED(POLY_SYS_arctan_real));
    add_word_to_io_area(POLY_SYS_exp_real, TAGGED(POLY_SYS_exp_real));
    add_word_to_io_area(POLY_SYS_ln_real, TAGGED(POLY_SYS_ln_real));
    add_word_to_io_area(POLY_SYS_io_operation, TAGGED(POLY_SYS_io_operation));
    add_word_to_io_area(POLY_SYS_fork_process, TAGGED(POLY_SYS_fork_process));
    add_word_to_io_area(POLY_SYS_choice_process, TAGGED(POLY_SYS_choice_process));
    add_word_to_io_area(POLY_SYS_int_process, TAGGED(POLY_SYS_int_process));
    add_word_to_io_area(POLY_SYS_send_on_channel, TAGGED(POLY_SYS_send_on_channel));
    add_word_to_io_area(POLY_SYS_receive_on_channel, TAGGED(POLY_SYS_receive_on_channel));
    add_word_to_io_area(POLY_SYS_is_big_endian, TAGGED(POLY_SYS_is_big_endian));
    add_word_to_io_area(POLY_SYS_bytes_per_word, TAGGED(POLY_SYS_bytes_per_word));
    add_word_to_io_area(POLY_SYS_offset_address, TAGGED(POLY_SYS_offset_address));
    add_word_to_io_area(POLY_SYS_objsize, TAGGED(POLY_SYS_objsize));
    add_word_to_io_area(POLY_SYS_showsize, TAGGED(POLY_SYS_showsize));
    add_word_to_io_area(POLY_SYS_shift_right_word, TAGGED(POLY_SYS_shift_right_word));
    add_word_to_io_area(POLY_SYS_word_neq, TAGGED(POLY_SYS_word_neq));
    add_word_to_io_area(POLY_SYS_not_bool, TAGGED(POLY_SYS_not_bool));
    add_word_to_io_area(POLY_SYS_string_length, TAGGED(POLY_SYS_string_length));
    add_word_to_io_area(POLY_SYS_int_eq, TAGGED(POLY_SYS_int_eq));
    add_word_to_io_area(POLY_SYS_int_neq, TAGGED(POLY_SYS_int_neq));
    add_word_to_io_area(POLY_SYS_int_geq, TAGGED(POLY_SYS_int_geq));
    add_word_to_io_area(POLY_SYS_int_leq, TAGGED(POLY_SYS_int_leq));
    add_word_to_io_area(POLY_SYS_int_gtr, TAGGED(POLY_SYS_int_gtr));
    add_word_to_io_area(POLY_SYS_int_lss, TAGGED(POLY_SYS_int_lss));
    add_word_to_io_area(POLY_SYS_string_sub, TAGGED(POLY_SYS_string_sub));
    add_word_to_io_area(POLY_SYS_mul_word, TAGGED(POLY_SYS_mul_word));
    add_word_to_io_area(POLY_SYS_plus_word, TAGGED(POLY_SYS_plus_word));
    add_word_to_io_area(POLY_SYS_minus_word, TAGGED(POLY_SYS_minus_word));
    add_word_to_io_area(POLY_SYS_div_word, TAGGED(POLY_SYS_div_word));
    add_word_to_io_area(POLY_SYS_or_word, TAGGED(POLY_SYS_or_word));
    add_word_to_io_area(POLY_SYS_and_word, TAGGED(POLY_SYS_and_word));
    add_word_to_io_area(POLY_SYS_xor_word, TAGGED(POLY_SYS_xor_word));
    add_word_to_io_area(POLY_SYS_shift_left_word, TAGGED(POLY_SYS_shift_left_word));
    add_word_to_io_area(POLY_SYS_mod_word, TAGGED(POLY_SYS_mod_word));
    add_word_to_io_area(POLY_SYS_word_geq, TAGGED(POLY_SYS_word_geq));
    add_word_to_io_area(POLY_SYS_word_leq, TAGGED(POLY_SYS_word_leq));
    add_word_to_io_area(POLY_SYS_word_gtr, TAGGED(POLY_SYS_word_gtr));
    add_word_to_io_area(POLY_SYS_word_lss, TAGGED(POLY_SYS_word_lss));
    add_word_to_io_area(POLY_SYS_word_eq, TAGGED(POLY_SYS_word_eq));
    add_word_to_io_area(POLY_SYS_load_byte, TAGGED(POLY_SYS_load_byte));
    add_word_to_io_area(POLY_SYS_load_word, TAGGED(POLY_SYS_load_word));
    add_word_to_io_area(POLY_SYS_assign_byte, TAGGED(POLY_SYS_assign_byte));
    add_word_to_io_area(POLY_SYS_assign_word, TAGGED(POLY_SYS_assign_word));

    add_word_to_io_area(POLY_SYS_version_number,TAGGED(VERSION_NUMBER));
    add_word_to_io_area(POLY_SYS_timing_dispatch, TAGGED(POLY_SYS_timing_dispatch));
    add_word_to_io_area(POLY_SYS_getdbasetime,   TAGGED(POLY_SYS_timing_dispatch));
    add_word_to_io_area (POLY_SYS_interrupt_console_processes,
                        TAGGED(POLY_SYS_timing_dispatch));
    add_word_to_io_area(POLY_SYS_install_subshells, TAGGED(POLY_SYS_install_subshells));
    add_word_to_io_area(POLY_SYS_XWindows, TAGGED(POLY_SYS_XWindows));
    add_word_to_io_area(POLY_SYS_full_gc,     TAGGED(POLY_SYS_full_gc));
    add_word_to_io_area(POLY_SYS_stack_trace, TAGGED(POLY_SYS_stack_trace));
    add_word_to_io_area(POLY_SYS_foreign_dispatch, TAGGED(POLY_SYS_foreign_dispatch));
    add_word_to_io_area(POLY_SYS_callcode_tupled,  TAGGED(POLY_SYS_callcode_tupled));
    add_word_to_io_area(POLY_SYS_process_env,      TAGGED(POLY_SYS_process_env));
    add_word_to_io_area(POLY_SYS_set_string_length, TAGGED(POLY_SYS_set_string_length));
    add_word_to_io_area(POLY_SYS_get_first_long_word, TAGGED(POLY_SYS_get_first_long_word));
    add_word_to_io_area(POLY_SYS_shrink_stack,     TAGGED(POLY_SYS_shrink_stack));
    add_word_to_io_area(POLY_SYS_set_flags,        TAGGED(POLY_SYS_set_flags));
    add_word_to_io_area(POLY_SYS_shift_right_arith_word, TAGGED(POLY_SYS_shift_right_arith_word));
    add_word_to_io_area(POLY_SYS_int_to_word,      TAGGED(POLY_SYS_int_to_word));
    add_word_to_io_area(POLY_SYS_set_code_constant,TAGGED(POLY_SYS_set_code_constant));
    add_word_to_io_area(POLY_SYS_move_bytes,       TAGGED(POLY_SYS_move_bytes));
    add_word_to_io_area(POLY_SYS_move_words,       TAGGED(POLY_SYS_move_words));

    add_word_to_io_area(POLY_SYS_io_dispatch, TAGGED(POLY_SYS_io_dispatch));
    add_word_to_io_area(POLY_SYS_network, TAGGED(POLY_SYS_network));
    add_word_to_io_area(POLY_SYS_os_specific, TAGGED(POLY_SYS_os_specific));
    add_word_to_io_area(POLY_SYS_signal_handler, TAGGED(POLY_SYS_signal_handler));

#if (0)
    add_word_to_io_area(236, TAGGED(POLY_SYS_or_word)); /* or byte */
    add_word_to_io_area(237, TAGGED(POLY_SYS_and_word)); /* and byte */
    add_word_to_io_area(211, TAGGED(POLY_SYS_load_byte)); /* load immutable byte */
    add_word_to_io_area(212, TAGGED(POLY_SYS_load_word)); /* load immutable word */
    add_word_to_io_area(22, TAGGED(POLY_SYS_get_length)); /* immut_vec_length */	
    add_word_to_io_area(POLY_SYS_OLD_printb, TAGGED(POLY_SYS_printb));
    add_word_to_io_area(POLY_SYS_OLD_prints, TAGGED(POLY_SYS_prints));
    add_word_to_io_area(POLY_SYS_OLD_convch, TAGGED(POLY_SYS_convch));
    add_word_to_io_area(POLY_SYS_OLD_callcode, TAGGED(POLY_SYS_callcode));
    add_word_to_io_area(POLY_SYS_OLD_lconvint, TAGGED(POLY_SYS_lconvint));
    add_word_to_io_area(POLY_SYS_OLD_repri, TAGGED(POLY_SYS_repri));
    add_word_to_io_area(POLY_SYS_OLD_reprb, TAGGED(POLY_SYS_reprb));
    add_word_to_io_area(21, TAGGED(POLY_SYS_lconvint)); /* wconvint */
    add_word_to_io_area(POLY_SYS_OLD_minus_int, TAGGED(POLY_SYS_minus_int));
    add_word_to_io_area(POLY_SYS_OLD_succ_char, TAGGED(POLY_SYS_succ_char));
    add_word_to_io_area(POLY_SYS_OLD_pred_char, TAGGED(POLY_SYS_pred_char));
#endif
}

/* Word functions. These functions assume that their arguments are tagged
   integers and treat them as unsigned values. */
/* These were previously in run_time.c but now that there are hand-coded
   versions in all the assembly code files we only need them here and they
   could really be included directly in the functions above. */
word mul_word_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	return TAGGED(wx*wy);
}

word plus_word_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	return TAGGED(wx+wy);
}

word minus_word_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	return TAGGED(wx-wy);
}

word div_word_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	if (wy == 0) raise_exception0(EXC_divide);
	return TAGGED(wx/wy);
}

word mod_word_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	if (wy == 0) raise_exception0(EXC_divide);
	return TAGGED(wx%wy);
}

word word_eq_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	return wx==wy ? TAGGED(1) : TAGGED(0);
}

word word_neq_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	return wx!=wy ? TAGGED(1) : TAGGED(0);
}

word word_geq_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	return wx>=wy ? TAGGED(1) : TAGGED(0);
}

word word_leq_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	return wx<=wy ? TAGGED(1) : TAGGED(0);
}

word word_gtr_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	return wx>wy ? TAGGED(1) : TAGGED(0);
}

word word_lss_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	return wx<wy ? TAGGED(1) : TAGGED(0);
}

word and_word_c(Handle y, Handle x)
{
	/* Normally it isn't necessary to remove the tags and put them
	   back on again.  We leave this code as it is just in case some
	   architecture does it differently. */
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	return TAGGED(wx & wy);
}

word or_word_c(Handle y, Handle x)
{
	/* Normally it isn't necessary to remove the tags and put them
	   back on again.  We leave this code as it is just in case some
	   architecture does it differently. */
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	return TAGGED(wx | wy);
}

word xor_word_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	return TAGGED(wx ^ wy);
}


word not_bool_c(Handle x)
{
	return *(int*)x == TAGGED(0) ? TAGGED(1) : TAGGED(0);
}

word shift_left_word_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	/* It is defined to return 0 if the shift is greater than the
	   number of bits in the word.  The shift instructions on many
	   architectures don't get that right. */
	if (wy > sizeof(word)*8) return TAGGED(0);
	return TAGGED(wx<<wy);
}

word shift_right_word_c(Handle y, Handle x)
{
	unsigned int wx = UNTAGGED_UNSIGNED(*(unsigned int*)x);
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	/* It is defined to return 0 if the shift is greater than the
	   number of bits in the word.  The shift instructions on many
	   architectures don't get that right. */
	if (wy > sizeof(word)*8) return TAGGED(0);
	return TAGGED(wx>>wy);
}

word shift_right_arith_word_c(Handle y, Handle x)
{
	word wx = UNTAGGED_UNSIGNED(*(word*)x); /* Treat as a signed quantity. */
	unsigned int wy = UNTAGGED_UNSIGNED(*(unsigned int*)y);
	/* It is defined to return 0 or ~1 if the shift is greater than the
	   number of bits in the word.  The shift instructions on many
	   architectures don't get that right. */
	if (wy > sizeof(word)*8) return wx < 0 ? TAGGED(-1) : TAGGED(0);
	return TAGGED(wx>>wy);
}

void MD_update_code_addresses(word **addr, word **old, int L, void (*op)(word **))
{
}
