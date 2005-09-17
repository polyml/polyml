/*
    Title: 	machine_dep.h - exports signature for machine_dep.c 

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

/* created 27/10/93 SPF */

#ifndef _MACHINE_DEP_H
#define _MACHINE_DEP_H

/* set by assembly code */
extern int in_run_time_system;
extern word *saved_sp;
extern byte *saved_pc;

#if defined(SOLARIS2) || defined(HPUX64)
#include <ucontext.h>
#define SIGNALCONTEXT ucontext_t
#elif defined(MACOSX) && ! defined(MACOSXPRE102)
#include <signal.h>
#include <ucontext.h>
#define SIGNALCONTEXT ucontext_t
#elif defined(WINDOWS_PC)
#else
#include <signal.h>
#define SIGNALCONTEXT struct sigcontext
#endif

#if defined(WINDOWS_PC)
extern void MD_interrupt_code(void);
#else
void MD_increment_profile_count_using_context(SIGNALCONTEXT *context);
void MD_interrupt_code_using_context(SIGNALCONTEXT *context);
#if defined(POWER2)
void MD_save_ML_state(SIGNALCONTEXT *scp);
#endif /* defined(SOLARIS2) */
#endif

extern void MD_init_stack_frame(StackObject *stack, Handle proc, Handle arg);
extern void MD_switch_to_poly(void);
extern void MD_set_for_retry(int ioCall);
extern void MD_init_interface_vector(void);
extern void MD_set_exception(StackObject *stack,poly_exn *exc);
extern int  MD_is_exception(StackObject *stack);
extern void MD_reset_signals(void);
/* MD_update_code_addresses - update addresses within a code segment.
   Added DCJM 1/1/2001. */
extern void MD_update_code_addresses(word **addr, word **oldAddr, int L, void (*op)(word **));

#if defined(POWER2)
/* MD_trap_handler passes the return address to MD_trap_handler1 */
extern void MD_trap_handler1(int);
#else /* !defined(POWER2) */
/* MD_trap_handler saves the return address in a register in poly_stack */
extern void MD_trap_handler1(void);
#endif /* !defined(POWER2) */

extern int registerMaskVector[];

#endif /* _MACHINE_DEP_H */
