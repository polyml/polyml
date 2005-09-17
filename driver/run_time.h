/*
    Title: 	run_time.h

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

#ifndef _RUNTIME_H_DEFINED
#define _RUNTIME_H_DEFINED 1


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

/* for root_struct */
#include "mm.h"

/* For JMP_BUF etc */
#include "globals.h"

#if 0
/* for use by assembly code only */
extern word    *save_vec[];
extern word   **save_vec_addr;
#endif

/**********************************************************************
 *
 * How do we handle longjmp?
 *
 **********************************************************************/

/* for jmp_buf */
#include <setjmp.h>

/* We want to choose a setjmp/longjmp pair that does NOT
   save and restore the system signal mask, since this would be
   a large run-time overhead for Poly/ML. For SunOS/Solaris
   and AIX, setjmp/longjmp is the "naked" pair, but WNT requires
   _setjmp/longjmp and HP-UX requires _setjmp/_longjmp. 
*/
#if defined(WINDOWS_PC)
#define JMP_BUF jmp_buf
#define SETJMP(buffer) _setjmp(buffer)
#define LONGJMP(buffer,value) longjmp(buffer,value)

#elif defined(SOLARIS2) || defined(BSD)
/* These are usually the "naked" versions */
#define JMP_BUF jmp_buf
#define SETJMP(buffer) setjmp(buffer)
#define LONGJMP(buffer,value) longjmp(buffer,value)

#else /* non-naked versions (safe, but expensive) */
#define JMP_BUF sigjmp_buf
#define SETJMP(buffer) sigsetjmp(buffer,1)
#define LONGJMP(buffer,value) siglongjmp(buffer,value)

#endif
/* shared data objects */
extern JMP_BUF      re_enter_poly;
extern int          store_profiling;
extern int          emulate_profiling;


/* storage allocation functions */
extern int isValidHandle(Handle h);
extern void init_save_vec(void);
extern Handle mark_save_vec(void);
extern void reset_save_vec(Handle old_value);

extern Handle push_to_save_vec(word valu);
extern word *alloc(int lengthword);
extern Handle alloc_and_save(int size);

/* for profiling */
extern int store_profiling;
extern int emulate_profiling;


/* called from assembly language */
word set_dbentry_c(Handle value_handle);

/* pstring functions */
extern pstring Buffer_to_Poly(const char *buffer,int length);
extern pstring C_string_to_Poly(const char *buffer);
extern word Poly_string_to_C(pstring ps,char *buff,int bufflen);
char *Poly_string_to_C_alloc(pstring ps);

Handle convert_string_list(int count, char **strings);
extern char **stringListToVector(Handle list);
extern void freeStringVector(char **vec);

extern Handle strconcatc(Handle x, Handle y);
extern Handle substringc(Handle L,Handle N, Handle s);

/* exceptions and interrupts */
NORETURN extern void raise_exception(word id, Handle arg) __attribute__((noreturn));
NORETURN extern void raise_exception0(word id) __attribute__((noreturn));
NORETURN extern void raise_exception_string(word id,char *str) __attribute__((noreturn));
NORETURN extern void raise_syscall(char *errmsg, int err) __attribute__((noreturn));
Handle create_syscall_exception(char *errmsg, int err);

extern void register_interrupt_proc(InterruptFunc int_proc);
extern void execute_pending_interrupts(void);

/* This should be PRINTFLIKE1 but that doesn't seem to work */
NORETURN extern void crash() __attribute__((noreturn));

extern void ex_tracec(Handle exc_data, Handle handler_handle);

extern word full_gc_c(void);
extern word stack_trace_c(void);

extern word get_flags_c(Handle addr_handle);
extern word set_flags_c(Handle value_handle, Handle addr_handle);

extern void raise_exc(Handle id_handle);
extern word BadOpCode_c(void);

extern void re_init_run_time_system(void);
extern void init_run_time_system(void);
extern void enter_poly_code(void);

extern void print_string(pstring);
extern void give_stack_trace(word *finish);

extern void uninit_run_time_system(void);

#ifdef WINDOWS_PC

extern HANDLE hMainThread; /* Handle to main thread. */
void RequestFinish(int n);

#endif

#define RE_ENTER_POLY(x) LONGJMP(re_enter_poly,(x))

Handle make_exn(word id, Handle arg);

extern int interrupted;
extern int trace_allowed;

#endif /* _RUNTIME_H_DEFINED */
