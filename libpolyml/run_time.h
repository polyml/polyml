/*
    Title:  run_time.h

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

#include "globals.h" // PolyWord, PolyObject etc
#include "noreturn.h"

class SaveVecEntry;
typedef SaveVecEntry *Handle;

// Exceptions thrown by C++ code.  Indicates that the caller should not return normally.
// They can be thrown in one of two different situations:
// 1.  The IO function needs to raise an ML exception
// 2.  The IO function needs to retry the call.  There may have been a process switch.
enum EXCEPTION_REASON {
    EXC_EXCEPTION,
    EXC_RETRY
};

class IOException {
public:
    IOException(enum EXCEPTION_REASON reason):
      m_reason(reason)
      { }
    enum EXCEPTION_REASON m_reason;
};

#ifdef _WIN32_WCE
#define THROW_EXCEPTION     longjmp(exception_jump, 2)
#define THROW_RETRY         longjmp(exception_jump, 2)
#else
#define THROW_EXCEPTION     throw IOException(EXC_EXCEPTION)
#define THROW_RETRY         throw IOException(EXC_RETRY)
#endif

/* storage allocation functions */

extern PolyObject *alloc(POLYUNSIGNED words, unsigned flags = 0);
extern Handle alloc_and_save(POLYUNSIGNED words, unsigned flags = 0);

extern Handle ex_tracec(Handle exc_data, Handle handler_handle);

extern int interrupted;

/* exceptions and interrupts */
NORETURNFN(extern void raise_exception(int id, Handle arg));
NORETURNFN(extern void raise_exception0(int id));
NORETURNFN(extern void raise_exception_string(int id, char *str));
NORETURNFN(extern void raise_syscall(char *errmsg, int err));
Handle create_syscall_exception(char *errmsg, int err);

typedef void (*InterruptFunc)(int signum);
extern void register_interrupt_proc(InterruptFunc int_proc);
extern void execute_pending_interrupts(void);

extern void re_init_run_time_system(void);
extern void init_run_time_system(void);
extern Handle EnterPolyCode(void);

extern void give_stack_trace(PolyWord *stackPtr, PolyWord *finish);

extern void uninit_run_time_system(void);

Handle make_exn(int id, Handle arg);

extern bool trace_allowed;

extern void add_word_to_io_area(unsigned sysop, PolyWord val);

Handle CodeSegmentFlags(Handle flags_handle, Handle addr_handle);

#endif /* _RUNTIME_H_DEFINED */
