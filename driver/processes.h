/*
    Title:      Lightweight process library
    Author:     Dave Matthews, Cambridge University Computer Laboratory

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

#ifndef _PROCESSES_H_
#define _PROCESSES_H_

#ifndef __GNUC__
#ifndef __attribute__
#define __attribute__(attribute) /* attribute */
#endif
#endif

#if	(defined(_MSC_EXTENSIONS) && (_MSC_VER >= 1200))
#define NORETURN	__declspec(noreturn)
#else
#define NORETURN
#endif


extern word int_processc(ProcessHandle proc);
#ifndef WINDOWS_PC
extern void process_may_block(int fd, int ioCall);
#endif
extern void NORETURN block_and_restart(int fd, int interruptable, int ioCall)
	__attribute__((noreturn));
extern void block_and_restart_if_necessary(int fd, int ioCall);

extern word switch_subshells_c(void);
extern void select_next_process(void);

/* files, channels and processes */
NORETURN extern void kill_selfc(void) __attribute__((noreturn));
NORETURN extern void select_a_new_process(void) __attribute__((noreturn));

extern word send_on_channelc(Handle val, ChanHandle chan);
extern word receive_on_channelc(ChanHandle chan);

extern Handle make_new_root_proc(Handle proc);
extern ProcessHandle fork_processc(Handle console_handle, Handle proc);
extern ProcessHandle fork_function(Handle proc, Handle arg);

extern word choice_processc(Handle proc1, Handle proc2);
extern void check_current_stack_size(word *lower_limit);
extern word int_processc(ProcessHandle proc);
extern void interrupt_console_processes(void);
extern void interrupt_signal_processes(void);
NORETURN extern void install_rootc(Handle proc) __attribute__((noreturn));
NORETURN void interrupt_console_processes_c(void) __attribute__((noreturn));
extern void set_process_list(root_struct * root);
extern void kill_process(process_base *to_kill);
extern void remove_process(process_base *to_kill);
extern void add_process(ProcessHandle p_base, int state);

extern word install_subshells_c (Handle root_function);
extern word shrink_stack_c (Handle reserved_space);

extern void init_process_system(void);
extern void re_init_process_system(void);
extern void uninit_process_system(void);

extern ProcessHandle processes;
extern StackObject *poly_stack;
extern word        *end_of_stack;

#endif
