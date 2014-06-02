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
// 2.  The IO function needs to retry the call.

class IOException {
public:
    IOException() { }
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

extern Handle exceptionToTraceException(TaskData *taskData, Handle exnHandle);

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
NORETURNFN(extern void raise_syscall(TaskData *taskData, const char *errmsg, int err));

extern void give_stack_trace(TaskData *taskData, PolyWord *stackPtr, PolyWord *finish);

Handle make_exn(TaskData *taskData, int id, Handle arg);

extern void add_word_to_io_area(unsigned sysop, PolyWord val);

extern Handle CodeSegmentFlags(TaskData *taskData, Handle flags_handle, Handle addr_handle);

// Check to see that there is space in the stack.  May GC and may raise a C++ exception.
extern void CheckAndGrowStack(TaskData *mdTaskData, POLYUNSIGNED minSize);

extern Handle errorMsg(TaskData *taskData, int err);

extern void IncrementRTSCallCount(unsigned ioFunction);

// These are C-language versions of small functions referenced from the IO vector.
// A lot of these are implemented directly by the X86 code generator.  On the X86 they
// may also be implemented directly in assembly code.  In a few cases the assembly code
// version falls back to the C version, for example if there is insufficient space in the
// allocation area.
extern Handle assign_byte_long_c(TaskData *taskData, Handle value_handle, Handle byte_no, Handle vector);
extern Handle assign_word_long_c(TaskData *taskData, Handle value_handle, Handle word_no, Handle vector);
extern Handle move_bytes_long_c(TaskData *taskData, Handle len, Handle dest_offset_handle, Handle dest_handle,
                       Handle src_offset_handle, Handle src_handle);
extern Handle move_words_long_c(TaskData *taskData, Handle len, Handle dest_offset_handle, Handle dest_handle,
                       Handle src_offset_handle, Handle src_handle);
extern Handle testBytesEqual(TaskData *taskData, Handle len, Handle yOffset, Handle y,
                             Handle xOffset, Handle x);
extern Handle vec_length_c(TaskData *taskData, Handle vector);
extern Handle load_byte_long_c(TaskData *taskData, Handle byte_no /* offset in BYTES */, Handle addr);
extern Handle load_word_long_c(TaskData *taskData, Handle word_no /* offset in WORDS */, Handle addr);
extern Handle alloc_store_long_c(TaskData *taskData, Handle initial, Handle flags_handle, Handle size );
extern Handle alloc_uninit_c(TaskData *taskData, Handle flags_handle, Handle size );
extern Handle mul_word_c(TaskData *taskData, Handle y, Handle x);
extern Handle plus_word_c(TaskData *taskData, Handle y, Handle x);
extern Handle minus_word_c(TaskData *taskData, Handle y, Handle x);
extern Handle div_word_c(TaskData *taskData, Handle y, Handle x);
extern Handle mod_word_c(TaskData *taskData, Handle y, Handle x);
extern Handle word_eq_c(TaskData *taskData, Handle y, Handle x);
extern Handle word_neq_c(TaskData *taskData, Handle y, Handle x);
extern Handle word_geq_c(TaskData *taskData, Handle y, Handle x);
extern Handle word_leq_c(TaskData *taskData, Handle y, Handle x);
extern Handle word_gtr_c(TaskData *taskData, Handle y, Handle x);
extern Handle word_lss_c(TaskData *taskData, Handle y, Handle x);
extern Handle and_word_c(TaskData *taskData, Handle y, Handle x);
extern Handle or_word_c(TaskData *taskData, Handle y, Handle x);
extern Handle xor_word_c(TaskData *taskData, Handle y, Handle x);
extern Handle not_bool_c(TaskData *taskData, Handle x);
extern Handle shift_left_word_c(TaskData *taskData, Handle y, Handle x);
extern Handle shift_right_word_c(TaskData *taskData, Handle y, Handle x);
extern Handle shift_right_arith_word_c(TaskData *taskData, Handle y, Handle x);
extern Handle eqLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle neqLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle geqLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle leqLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle gtLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle ltLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle makeLongWord(TaskData *taskData, POLYUNSIGNED w);
extern Handle plusLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle minusLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle mulLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle divLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle modLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle andbLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle orbLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle xorbLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle shiftLeftLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle shiftRightLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle shiftRightArithLongWord(TaskData *taskData, Handle y, Handle x);
extern Handle longWordToTagged(TaskData *taskData, Handle x);
extern Handle signedToLongWord(TaskData *taskData, Handle x);
extern Handle unsignedToLongWord(TaskData *taskData, Handle x);
extern Handle get_flags_c(TaskData *taskData, Handle addr_handle);
extern Handle io_operation_c(TaskData *taskData, Handle entry);
extern Handle full_gc_c(TaskData *taskData);
extern Handle stack_trace_c(TaskData *taskData);
extern Handle shrink_stack_c(TaskData *taskData, Handle reserved_space);

#endif /* _RUNTIME_H_DEFINED */
