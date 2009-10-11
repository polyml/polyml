/*
    Title:  run_time.h

    Copyright (c) 2000-9
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
class TaskData;

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

// This exception is used in the exporter and sharedata code.  It is
// converted into an ML exception at the outer level.
class MemoryException {
public:
    MemoryException() {}
};

// A request to kill the thread raises this exception. 
class KillException {
public:
    KillException() {}
};

/* storage allocation functions */
extern PolyObject *alloc(TaskData *taskData, POLYUNSIGNED words, unsigned flags = 0);
extern Handle alloc_and_save(TaskData *taskData, POLYUNSIGNED words, unsigned flags = 0);

extern Handle ex_tracec(TaskData *taskData, Handle exc_data, Handle handler_handle);

/* exceptions and interrupts */
NORETURNFN(extern void raise_exception(TaskData *taskData, int id, Handle arg));
NORETURNFN(extern void raise_exception0(TaskData *taskData, int id));
NORETURNFN(extern void raise_exception_string(TaskData *taskData, int id, const char *str));
NORETURNFN(extern void raise_fail(TaskData *taskData, const char *errmsg));

// Raise OS.SysCall(OS.errorMsg err, SOME err)
NORETURNFN(extern void raiseSyscallError(TaskData *taskData, int err));
// Raise OS.SysCall(msg, NONE)
NORETURNFN(extern void raiseSyscallMessage(TaskData *taskData, const char *errmsg));
// This was the previous version.  The errmsg argument is ignored unless err is zero.
inline void raise_syscall(TaskData *taskData, const char *errmsg, int err)
{
    if (err == 0) raiseSyscallMessage(taskData, errmsg);
    else raiseSyscallError(taskData, err);
}

extern void re_init_run_time_system(void);
extern void init_run_time_system(void);
extern Handle EnterPolyCode(TaskData *taskData);

extern void give_stack_trace(TaskData *taskData, PolyWord *stackPtr, PolyWord *finish);

extern void uninit_run_time_system(void);

Handle make_exn(TaskData *taskData, int id, Handle arg);

extern bool trace_allowed;

extern void add_word_to_io_area(unsigned sysop, PolyWord val);

extern Handle CodeSegmentFlags(TaskData *taskData, Handle flags_handle, Handle addr_handle);

// Check to see that there is space in the stack.  May GC and may raise a C++ exception.
extern void CheckAndGrowStack(TaskData *mdTaskData, PolyWord *lower_limit);

extern Handle errorMsg(TaskData *taskData, int err);

#endif /* _RUNTIME_H_DEFINED */
