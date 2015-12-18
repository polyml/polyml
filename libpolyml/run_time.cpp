/*
    Title:      Run-time system.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further work copyright David C. J. Matthews 2009, 2012, 2015

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

/************************************************************************
 *
 * Include system headers 
 *
 ************************************************************************/

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h> 
#endif

#ifdef HAVE_PROCESS_H 
#include <process.h> 
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_IO_H
#include <io.h>
#endif

#ifdef HAVE_EXCPT_H
#include <excpt.h>
#endif

#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif

#ifdef HAVE_SYS_SYSTEMINFO_H
#include <sys/systeminfo.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/************************************************************************
 *
 * Include runtime headers
 *
 ************************************************************************/

#include "globals.h"
#include "gc.h"
#include "mpoly.h"
#include "arb.h"
#include "machine_dep.h"
#include "diagnostics.h"
#include "processes.h"
#include "profiling.h"
#include "run_time.h"
#include "sys.h"
#include "process_env.h"
#include "sighandler.h"
#include "scanaddrs.h"
#include "check_objects.h"
#include "polystring.h"
#include "save_vec.h"
#include "rts_module.h"
#include "memmgr.h"

#define SAVE(x) taskData->saveVec.push(x)
#define SIZEOF(x) (sizeof(x)/sizeof(PolyWord))

// used heavily by MD_init_interface_vector in machine_dep.c
void add_word_to_io_area (unsigned sysop, PolyWord val)
{
    ASSERT (sysop > 0 && sysop < 256);
    PolyWord *objAddr = IoEntry(sysop);
    objAddr[0] = val;
}

/******************************************************************************/
/*                                                                            */
/*      STORAGE ALLOCATION                                                    */
/*                                                                            */
/******************************************************************************/

// This is the storage allocator for allocating heap objects in the RTS.
PolyObject *alloc(TaskData *taskData, POLYUNSIGNED data_words, unsigned flags)
/* Allocate a number of words. */
{
    POLYUNSIGNED words = data_words + 1;
    
    if (profileMode == kProfileStoreAllocation)
    {
        add_count(taskData, taskData->pc(), taskData->sp(), words);
    }

    PolyWord *foundSpace = processes->FindAllocationSpace(taskData, words, false);
    if (foundSpace == 0)
    {
        // Failed - the thread is set to raise an exception.
        throw IOException();
    }

    PolyObject *pObj = (PolyObject*)(foundSpace + 1);
    pObj->SetLengthWord(data_words, flags);
    
    // Must initialise object here, because GC doesn't clean store.
    // N.B.  This sets the store to zero NOT TAGGED(0).
    // This is particularly important for byte segments (e.g. strings) since the
    // ML code may leave bytes at the end uninitialised.  Structure equality
    // checks all the bytes so for it to work properly we need to be sure that
    // they always have the same value.
    for (POLYUNSIGNED i = 0; i < data_words; i++) pObj->Set(i, PolyWord::FromUnsigned(0));
    return pObj;
}

/******************************************************************************/
/*                                                                            */
/*      alloc_and_save - called by run-time system                            */
/*                                                                            */
/******************************************************************************/
Handle alloc_and_save(TaskData *taskData, POLYUNSIGNED size, unsigned flags)
/* Allocate and save the result on the vector. */
{
    return SAVE(alloc(taskData, size, flags));
}

/******************************************************************************/
/*                                                                            */
/*      full_gc_c - called by assembly code                                */
/*                                                                            */
/******************************************************************************/
/* CALL_IO0(full_gc_, NOIND) */
Handle full_gc_c(TaskData *taskData)
{
    FullGC(taskData);
    return SAVE(TAGGED(0));
}


/******************************************************************************/
/*                                                                            */
/*      Error Messages                                                        */
/*                                                                            */
/******************************************************************************/


// Return the handle to a string error message.  This will return
// something like "Unknown error" from strerror if it doesn't match
// anything.
Handle errorMsg(TaskData *taskData, int err)
{
#ifdef _WIN32
    /* In the Windows version we may have both errno values
       and also GetLastError values.  We convert the latter into
       negative values before returning them. */
    if (err < 0)
    {
        LPTSTR lpMsg = NULL;
        TCHAR *p;
        if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
                FORMAT_MESSAGE_ALLOCATE_BUFFER |
                FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL, (DWORD)(-err), 0, (LPTSTR)&lpMsg, 1, NULL) > 0)
        {
            /* The message is returned with CRLF at the end.  Remove them. */
            for (p = lpMsg; *p != '\0' && *p != '\n' && *p != '\r'; p++);
            *p = '\0';
            Handle res = SAVE(C_string_to_Poly(taskData, lpMsg));
            LocalFree(lpMsg);
            return res;
        }
    }
#endif
    // Unix and unknown Windows errors.
    return SAVE(C_string_to_Poly(taskData, strerror(err)));
}


/******************************************************************************/
/*                                                                            */
/*      EXCEPTIONS                                                            */
/*                                                                            */
/******************************************************************************/

Handle make_exn(TaskData *taskData, int id, Handle arg)
{
    const char *exName;
    switch (id) {
    case EXC_interrupt: exName = "Interrupt"; break;
    case EXC_syserr: exName = "SysErr"; break;
    case EXC_size: exName = "Size"; break;
    case EXC_overflow: exName = "Overflow"; break;
    case EXC_underflow: exName = "Underflow"; break;
    case EXC_divide: exName = "Div"; break;
    case EXC_conversion: exName = "Conversion"; break;
    case EXC_XWindows: exName = "XWindows"; break;
    case EXC_subscript: exName = "Subscript"; break;
    case EXC_foreign: exName = "Foreign"; break;
    case EXC_Fail: exName = "Fail"; break;
    case EXC_thread: exName = "Thread"; break;
    case EXC_extrace: exName = "ExTrace"; break;
    default: ASSERT(0); exName = "Unknown"; // Shouldn't happen.
    }
   

    Handle pushed_name = SAVE(C_string_to_Poly(taskData, exName));
    
    Handle exnHandle = alloc_and_save(taskData, SIZEOF(poly_exn));
    
    DEREFEXNHANDLE(exnHandle)->ex_id   = TAGGED(id);
    DEREFEXNHANDLE(exnHandle)->ex_name = DEREFWORD(pushed_name);
    DEREFEXNHANDLE(exnHandle)->arg     = DEREFWORDHANDLE(arg);
    DEREFEXNHANDLE(exnHandle)->ex_location = TAGGED(0);

    return exnHandle;
}

/******************************************************************************/
/*                                                                            */
/*      raise_exception - called by run-time system                           */
/*                                                                            */
/******************************************************************************/
void raise_exception(TaskData *taskData, int id, Handle arg)
/* Raise an exception with no arguments. */
{
    Handle exn = make_exn(taskData, id, arg);
    /* N.B.  We must create the packet first BEFORE dereferencing the
       process handle just in case a GC while creating the packet
       moves the process and/or the stack. */
    taskData->SetException(DEREFEXNHANDLE(exn));
    throw IOException(); /* Return to Poly code immediately. */
    /*NOTREACHED*/
}


/******************************************************************************/
/*                                                                            */
/*      raise_exception0 - called by run-time system                          */
/*                                                                            */
/******************************************************************************/
void raise_exception0(TaskData *taskData, int id)
/* Raise an exception with no arguments. */
{
    raise_exception(taskData, id, SAVE(TAGGED(0)));
    /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      raise_exception_string - called by run-time system                    */
/*                                                                            */
/******************************************************************************/
void raise_exception_string(TaskData *taskData, int id, const char *str)
/* Raise an exception with a C string as the argument. */
{
    raise_exception(taskData, id, SAVE(C_string_to_Poly(taskData, str)));
    /*NOTREACHED*/
}

// Raise a SysErr exception with a given error code.
// The string part must match the result of OS.errorMsg
void raiseSyscallError(TaskData *taskData, int err)
{
    Handle errornum = Make_arbitrary_precision(taskData, err);
    Handle pushed_option = alloc_and_save(taskData, 1);
    DEREFHANDLE(pushed_option)->Set(0, DEREFWORDHANDLE(errornum)); /* SOME err */
    Handle pushed_name = errorMsg(taskData, err); // Generate the string.
    Handle pair = alloc_and_save(taskData, 2);
    DEREFHANDLE(pair)->Set(0, DEREFWORDHANDLE(pushed_name));
    DEREFHANDLE(pair)->Set(1, DEREFWORDHANDLE(pushed_option));

    raise_exception(taskData, EXC_syserr, pair);
}

// Raise a SysErr exception which does not correspond to an error code.
void raiseSyscallMessage(TaskData *taskData, const char *errmsg)
{
    Handle pushed_option = SAVE(NONE_VALUE); /* NONE */
    Handle pushed_name = SAVE(C_string_to_Poly(taskData, errmsg));
    Handle pair = alloc_and_save(taskData, 2);
    DEREFHANDLE(pair)->Set(0, DEREFWORDHANDLE(pushed_name));
    DEREFHANDLE(pair)->Set(1, DEREFWORDHANDLE(pushed_option));

    raise_exception(taskData, EXC_syserr, pair);
}

// This was the previous version.  The errmsg argument is ignored unless err is zero.
// Calls to it should really be replaced with calls to either raiseSyscallMessage
// or raiseSyscallError but it's been left because there may be cases where errno
// actually contains zero.
void raise_syscall(TaskData *taskData, const char *errmsg, int err)
{
    if (err == 0) raiseSyscallMessage(taskData, errmsg);
    else raiseSyscallError(taskData, err);
}

// Raises a Fail exception.
void raise_fail(TaskData *taskData, const char *errmsg)
{
    raise_exception_string(taskData, EXC_Fail, errmsg);
}

/* "Polymorphic" function to generate a list. */
Handle makeList(TaskData *taskData, int count, char *p, int size, void *arg,
                       Handle (mkEntry)(TaskData *, void*, char*))
{
    Handle saved = taskData->saveVec.mark();
    Handle list = SAVE(ListNull);
    /* Start from the end of the list. */
    p += count*size;
    while (count > 0)
    {
        Handle value, next;
        p -= size; /* Back up to the last entry. */
        value = mkEntry(taskData, arg, p);
        next  = alloc_and_save(taskData, SIZEOF(ML_Cons_Cell));

        DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(value); 
        DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);

        taskData->saveVec.reset(saved);
        list = SAVE(DEREFHANDLE(next));
        count--;
    }
    return list;
}

// Build a list of the function names on the stack.
Handle buildStackList(TaskData *taskData, PolyWord *startOfTrace, PolyWord *endOfTrace)
{
    Handle saved = taskData->saveVec.mark();
    Handle list = SAVE(ListNull);
    PolyWord *endStack = taskData->stack->top - 1;
    if (endOfTrace > endStack) endOfTrace = endStack;

    for (PolyWord *sp = endOfTrace; sp >= startOfTrace; sp--)
    {
        PolyWord pc = *sp;
        if (pc.IsCodePtr() && sp != taskData->hr())
        {
            // A code pointer can be a return address or an exception
            // handler but if we're producing an exception trace the
            // only exception handler will be the one for exception
            // trace itself.
            PolyObject *ptr = ObjCodePtrToPtr(pc.AsCodePtr());
            PolyWord *consts = ptr->ConstPtrForCode();

            // The name may be zero if it is anonymous.
            // We have to be careful that a GC might move the code or the name.
            // Stack areas are no longer in the ML heap so we don't need to worry
            // about the stack pointer.
            Handle functionName =
                consts[0] == TAGGED(0) ? SAVE(C_string_to_Poly(taskData, "<anon>")) : SAVE(consts[0]);
            Handle next  = alloc_and_save(taskData, sizeof(ML_Cons_Cell) / sizeof(PolyWord));

            DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(functionName); 
            DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);

            taskData->saveVec.reset(saved);
            list = SAVE(DEREFHANDLE(next));
        }
    }

    return list;
}

void give_stack_trace(TaskData *taskData, PolyWord *sp, PolyWord *finish)
{
    Handle listHandle = buildStackList(taskData, sp, finish);
    PolyWord list = listHandle->Word();

    while (! (list.IsTagged()))
    {
        ML_Cons_Cell *p = (ML_Cons_Cell*)list.AsObjPtr();
        print_string(p->h);
        putc('\n',stdout);
        list = p->t;
    }
    fflush(stdout);
}


/******************************************************************************/
/*                                                                            */
/*      stack_trace_c - called from assembly code                             */
/*                                                                            */
/******************************************************************************/
/* CALL_IO0(stack_trace_, NOIND) */
Handle stack_trace_c(TaskData *taskData)
{
    give_stack_trace (taskData, taskData->sp(), taskData->stack->top);
    return SAVE(TAGGED(0));
}

// Current exception trace.  This creates a special exception packet and
// raises it so that the ML code can print the trace.
Handle exceptionToTraceException(TaskData *taskData, Handle exnHandle)
{
    // p_hr points at a pair of values.  The first word will be the
    // entry point to the handler i.e. to this code, the second word is
    // the stack address of the handler to restore.
    PolyWord *handler = taskData->hr()+1;
    Handle listHandle = buildStackList(taskData, taskData->sp(), handler);
    // Construct a pair of the trace list and the exception packet
    Handle pair = alloc_and_save(taskData, 2);
    pair->WordP()->Set(0, listHandle->Word());
    pair->WordP()->Set(1, exnHandle->Word());
    // Set up the next handler so we don't come back here when we raise
    // the exception again. */
    taskData->set_hr((PolyWord*)(handler->AsStackAddr()));
    // Raise this as an exception.  The handler will be able to
    // print the trace and reraise the exception.
    raise_exception(taskData, EXC_extrace, pair);
}

// Return the address of the iovec entry for a given index.
Handle io_operation_c(TaskData *taskData, Handle entry)
{
    unsigned entryNo = get_C_unsigned(taskData, DEREFWORD(entry));
    if (entryNo >= POLY_SYS_vecsize)
        raise_exception0(taskData, EXC_subscript);
    return SAVE((PolyObject*)IoEntry(entryNo));
}

/******************************************************************************/
/*                                                                            */
/*      get_flags_c - called from machine_assembly.s                          */
/*                                                                            */
/******************************************************************************/
/* CALL_IO1(get_flags_,REF,NOIND) */
Handle get_flags_c(TaskData *taskData, Handle addr_handle)
{
    PolyObject *pt = DEREFWORDHANDLE(addr_handle);
    PolyWord *addr = (PolyWord*)pt;

    /* This is for backwards compatibility only.  Previously this
       was used to test for an IO address.  Instead an entry has
       been added to process_env to test for an IO address. */
    if (gMem.IsIOPointer(addr))
    {
        return SAVE(TAGGED(256));
    }
    else
    {
        const POLYUNSIGNED old_word  = pt->LengthWord();
        const POLYUNSIGNED old_flags =
            ((old_word & OBJ_PRIVATE_USER_FLAGS_MASK) >> OBJ_PRIVATE_FLAGS_SHIFT);
        return SAVE(TAGGED(old_flags));
    }
}

// This is called twice when constructing a piece of code.  The first
// time is to convert a mutable byte segment into a mutable code segment and
// the second call is to freeze the mutable code segment.  The reason for the
// two calls is that we first have to make sure we have a validly formatted code
// segment with the "number of constants" value set before we can make it a code
// segment and actually store the constants in it.
Handle CodeSegmentFlags(TaskData *taskData, Handle flags_handle, Handle addr_handle)
{
    PolyObject *pt = DEREFWORDHANDLE(addr_handle);
    unsigned short newFlags = get_C_ushort(taskData, DEREFWORD(flags_handle));

    if (newFlags >= 256)
        raise_exception_string(taskData, EXC_Fail, "FreezeCodeSegment flags must be less than 256");

    if (! pt->IsMutable())
        raise_exception_string(taskData, EXC_Fail, "FreezeCodeSegment must be applied to a mutable segment");

    const POLYUNSIGNED objLength = pt->Length();
    pt->SetLengthWord(objLength, (byte)newFlags);

    // Flush the cache on architectures that need it.
    if (pt->IsCodeObject() && ! pt->IsMutable())
        machineDependent->FlushInstructionCache(pt, objLength * sizeof(PolyWord));
    
    return SAVE(TAGGED(0));
}

/* CALL_IO3(assign_byte_long_, REF, REF, REF, NOIND) */
Handle assign_byte_long_c(TaskData *taskData, Handle value_handle, Handle byte_no, Handle vector)
{
    PolyWord value = DEREFHANDLE(value_handle);
    POLYUNSIGNED  offset  = getPolyUnsigned(taskData, DEREFWORDHANDLE(byte_no));  /* SPF 31/10/93 */
    byte *pointer = DEREFBYTEHANDLE(vector);    
    byte v = (byte)UNTAGGED(value);
    pointer[offset] = v;
    return taskData->saveVec.push(TAGGED(0));
}

/* CALL_IO3(assign_word_long_, REF, REF, REF, NOIND) */
Handle assign_word_long_c(TaskData *taskData, Handle value_handle, Handle word_no, Handle vector)
{
    PolyWord value      = DEREFHANDLE(value_handle);
    POLYUNSIGNED offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(word_no)); /* SPF 31/10/93 */
    PolyObject *pointer   = DEREFWORDHANDLE(vector);
    pointer->Set(offset, value);
    return taskData->saveVec.push(TAGGED(0));
}

/* CALL_IO5(move_bytes_long_, REF, REF, REF, REF, REF, NOIND) */
/* Move a segment of bytes, typically a string.  */
Handle move_bytes_long_c(TaskData *taskData, Handle len, Handle dest_offset_handle, Handle dest_handle,
                       Handle src_offset_handle, Handle src_handle)
{
    POLYUNSIGNED src_offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(src_offset_handle));
    byte *source = DEREFBYTEHANDLE(src_handle) + src_offset;
    POLYUNSIGNED dest_offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(dest_offset_handle));
    byte *destination = DEREFBYTEHANDLE(dest_handle);
    byte *dest = destination + dest_offset;
    POLYUNSIGNED bytes = getPolyUnsigned(taskData, DEREFWORDHANDLE(len));
    PolyObject *obj = DEREFHANDLE(dest_handle);
    ASSERT(obj->IsByteObject());

    memmove(dest, source, bytes);  /* must work for overlapping segments. */
    return taskData->saveVec.push(TAGGED(0));
}

/* CALL_IO5(move_words_long_, REF, REF, REF, REF, REF, NOIND) */
/* Move a segment of words.   Similar to move_bytes_long_ except that
   it is used for PolyWord segments. */
Handle move_words_long_c(TaskData *taskData, Handle len, Handle dest_offset_handle, Handle dest_handle,
                       Handle src_offset_handle, Handle src_handle)
{
    POLYUNSIGNED src_offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(src_offset_handle));
    PolyObject *sourceObj = DEREFWORDHANDLE(src_handle);
    PolyWord *source = sourceObj->Offset(src_offset);

    POLYUNSIGNED dest_offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(dest_offset_handle));

    PolyObject *destObject = DEREFWORDHANDLE(dest_handle);
    PolyWord *dest = destObject->Offset(dest_offset);

    POLYUNSIGNED words = getPolyUnsigned(taskData, DEREFWORDHANDLE(len));

    ASSERT(! destObject->IsByteObject());

    memmove(dest, source, words*sizeof(PolyWord));  /* must work for overlapping segments. */
    return taskData->saveVec.push(TAGGED(0));
}

Handle testBytesEqual(TaskData *taskData, Handle len, Handle yOffset, Handle y,
                             Handle xOffset, Handle x)
{
    POLYUNSIGNED x_offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(xOffset));
    byte *xAddr = DEREFBYTEHANDLE(x) + x_offset;

    POLYUNSIGNED y_offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(yOffset));
    byte *yAddr = DEREFBYTEHANDLE(y) + y_offset;

    POLYUNSIGNED bytes = getPolyUnsigned(taskData, DEREFWORDHANDLE(len));

    int res = memcmp(xAddr, yAddr, bytes);
    if (res == 0) return taskData->saveVec.push(TAGGED(1));
    else return taskData->saveVec.push(TAGGED(0));
}

Handle vec_length_c(TaskData *taskData, Handle vector)    /* Length of a vector */
{
    POLYUNSIGNED length = vector->WordP()->Length();
    return taskData->saveVec.push(TAGGED(length));
}

Handle load_byte_long_c(TaskData *taskData, Handle byte_no /* offset in BYTES */, Handle addr)
{
    POLYUNSIGNED offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(byte_no));
    return taskData->saveVec.push(TAGGED(DEREFBYTEHANDLE(addr)[offset]));
}

Handle load_word_long_c(TaskData *taskData, Handle word_no /* offset in WORDS */, Handle addr)
{
    POLYUNSIGNED offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(word_no));
    return taskData->saveVec.push(addr->Word().AsObjPtr()->Get(offset));
}

// In most cases the assembly coded version of this will handle the
// allocation.  The function can be called by the assembly code
// when it finds it has run out.  Using it avoids us having a
// return address into the assembly code.
Handle alloc_store_long_c(TaskData *taskData, Handle initial, Handle flags_handle, Handle size )
{
    unsigned flags = get_C_unsigned(taskData, DEREFWORD(flags_handle));
    POLYUNSIGNED usize = getPolyUnsigned(taskData, DEREFWORD(size));
    
    if (usize == 0 || usize >= MAX_OBJECT_SIZE)
        raise_exception0(taskData, EXC_size);
    
    PolyObject *vector = alloc(taskData, usize, flags| F_MUTABLE_BIT);
    
    PolyWord value = DEREFWORD(initial);
    
    if (vector->IsByteObject()) {
        // Byte segments are supposed to be initialised only with zero
        if (value != TAGGED(0))
            raise_exception_string(taskData, EXC_Fail, "non-zero byte segment");
    }
    else if (value != PolyWord::FromUnsigned(0))  {
        for (POLYUNSIGNED i = 0; i < usize; i++)
            vector->Set(i, value);
    }
    
    return taskData->saveVec.push(vector);
}

// Alloc_uninit returns the address of some memory that will be filled in later
// so need not be initialised unlike alloc_store where the initial value may be
// significant.  For word objects we can't risk leaving them uninitialised in case
// we GC before we've finished filling them.  There's no harm in initialising byte
// objects.
Handle alloc_uninit_c(TaskData *taskData, Handle flags_handle, Handle size )
{
    Handle initial = SAVE(TAGGED(0));
    return alloc_store_long_c(taskData, initial, flags_handle, size);
}

/* Word functions. These functions assume that their arguments are tagged
   integers and treat them as unsigned values.
   These functions will almost always be implemented directly in the code
   generator with back-up versions in the machine-dependent assembly code
   section.  They are included here for completeness. */
Handle mul_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(TAGGED(wx*wy));
}

Handle plus_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(TAGGED(wx+wy));
}

Handle minus_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(TAGGED(wx-wy));
}

Handle div_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    if (wy == 0) raise_exception0(taskData, EXC_divide);
    return taskData->saveVec.push(TAGGED(wx/wy));
}

Handle mod_word_c(TaskData *taskData, Handle y, Handle x)
{
    // In most cases it doesn't matter whether we use UNTAGGED or UNTAGGED_UNSIGNED
    // but in mod we will get the wrong answer if we use UNTAGGED here.
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    if (wy == 0) raise_exception0(taskData, EXC_divide);
    return taskData->saveVec.push(TAGGED(wx%wy));
}

Handle word_eq_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(wx==wy ? TAGGED(1) : TAGGED(0));
}

Handle word_neq_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(wx!=wy ? TAGGED(1) : TAGGED(0));
}

Handle word_geq_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(wx>=wy ? TAGGED(1) : TAGGED(0));
}

Handle word_leq_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(wx<=wy ? TAGGED(1) : TAGGED(0));
}

Handle word_gtr_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(wx>wy ? TAGGED(1) : TAGGED(0));
}

Handle word_lss_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(wx<wy ? TAGGED(1) : TAGGED(0));
}

Handle and_word_c(TaskData *taskData, Handle y, Handle x)
{
    /* Normally it isn't necessary to remove the tags and put them
       back on again.  We leave this code as it is just in case some
       architecture does it differently. */
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(TAGGED(wx & wy));
}

Handle or_word_c(TaskData *taskData, Handle y, Handle x)
{
    /* Normally it isn't necessary to remove the tags and put them
       back on again.  We leave this code as it is just in case some
       architecture does it differently. */
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(TAGGED(wx | wy));
}

Handle xor_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(TAGGED(wx ^ wy));
}


Handle not_bool_c(TaskData *taskData, Handle x)
{
    return taskData->saveVec.push(DEREFWORD(x) == TAGGED(0) ? TAGGED(1) : TAGGED(0));
}

Handle shift_left_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    /* It is defined to return 0 if the shift is greater than the
       number of bits in the PolyWord.  The shift instructions on many
       architectures don't get that right. */
    if (wy > sizeof(PolyWord)*8)
        return taskData->saveVec.push(TAGGED(0));
    return taskData->saveVec.push(TAGGED(wx<<wy));
}

Handle shift_right_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    /* It is defined to return 0 if the shift is greater than the
       number of bits in the word.  The shift instructions on many
       architectures don't get that right. */
    if (wy > sizeof(PolyWord)*8)
        return taskData->saveVec.push(TAGGED(0));
    return taskData->saveVec.push(TAGGED(wx>>wy));
}

Handle shift_right_arith_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYSIGNED wx = UNTAGGED(DEREFWORD(x)); /* Treat as a signed quantity. */
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    // This function in ML is defined to return 0 or ~1 if the shift is greater
    // than the number of bits in the word.
    // C does not actually define how signed values are shifted although most
    // (all?) compilers seem to use arithmetic shifts here.
    if (wy > sizeof(PolyWord)*8)
        return taskData->saveVec.push(wx < 0 ? TAGGED(-1) : TAGGED(0));
    return taskData->saveVec.push(TAGGED(wx >> wy));
}

// Large-word operations.  A large word is a 32/64-bit value in a byte segment.
// These will normally be code-generated and in the assembly code.
Handle eqLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    return taskData->saveVec.push(wx==wy ? TAGGED(1) : TAGGED(0));
}

Handle neqLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    return taskData->saveVec.push(wx!=wy ? TAGGED(1) : TAGGED(0));
}

Handle geqLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    return taskData->saveVec.push(wx>=wy ? TAGGED(1) : TAGGED(0));
}

Handle leqLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    return taskData->saveVec.push(wx<=wy ? TAGGED(1) : TAGGED(0));
}

Handle gtLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    return taskData->saveVec.push(wx>wy ? TAGGED(1) : TAGGED(0));
}

Handle ltLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    return taskData->saveVec.push(wx<wy ? TAGGED(1) : TAGGED(0));
}

Handle makeLongWord(TaskData *taskData, POLYUNSIGNED w)
{
    Handle result = alloc_and_save(taskData, 1, F_BYTE_OBJ);
    result->WordP()->Set(0, PolyWord::FromUnsigned(w));
    return result;
}

Handle plusLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    return makeLongWord(taskData, wx + wy);
}

Handle minusLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    return makeLongWord(taskData, wx - wy);
}

Handle mulLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    return makeLongWord(taskData, wx * wy);
}

Handle divLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    if (wy == 0) raise_exception0(taskData, EXC_divide);
    return makeLongWord(taskData, wx / wy);
}

Handle modLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    if (wy == 0) raise_exception0(taskData, EXC_divide);
    return makeLongWord(taskData, wx % wy);
}

Handle andbLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    return makeLongWord(taskData, wx & wy);
}

Handle orbLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    return makeLongWord(taskData, wx | wy);
}

Handle xorbLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = y->WordP()->Get(0).AsUnsigned();
    return makeLongWord(taskData, wx ^ wy);
}

Handle shiftLeftLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    // The amount to shift is always a Word.word value.
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    /* It is defined to return 0 if the shift is greater than the
       number of bits in the PolyWord.  The shift instructions on many
       architectures don't get that right. */
    if (wy > sizeof(PolyWord)*8)
        return makeLongWord(taskData, 0);
    return makeLongWord(taskData, wx << wy);
}

Handle shiftRightLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    /* It is defined to return 0 if the shift is greater than the
       number of bits in the word.  The shift instructions on many
       architectures don't get that right. */
    if (wy > sizeof(PolyWord)*8)
        return makeLongWord(taskData, 0);
    return makeLongWord(taskData, wx >> wy);
}

Handle shiftRightArithLongWord(TaskData *taskData, Handle y, Handle x)
{
    POLYSIGNED wx = x->WordP()->Get(0).AsSigned();
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    if (wy > sizeof(PolyWord)*8)
        return makeLongWord(taskData, wx < 0 ? -1 : 0);
    // Strictly speaking, C does not define that this uses an arithmetic shift
    // so we really ought to set the high-order bits explicitly.
    return makeLongWord(taskData, (POLYUNSIGNED)(wx >> wy));
}

// Extract the first word and return it as a tagged value.  This loses the top-bit
Handle longWordToTagged(TaskData *taskData, Handle x)
{
    POLYUNSIGNED wx = x->WordP()->Get(0).AsUnsigned();
    return taskData->saveVec.push(TAGGED(wx));
}

// Shift the tagged value to remove the tag and put it into the first word.
// The original sign bit is copied in the shift.
Handle signedToLongWord(TaskData *taskData, Handle x)
{
    POLYSIGNED wx = x->Word().UnTagged();
    return makeLongWord(taskData, (POLYUNSIGNED)wx);
}

// As with the above except the value is treated as an unsigned
// value and the top bit is zero.
Handle unsignedToLongWord(TaskData *taskData, Handle x)
{
    POLYUNSIGNED wx = x->Word().UnTaggedUnsigned();
    return makeLongWord(taskData, wx);
}

void CheckAndGrowStack(TaskData *taskData, POLYUNSIGNED minSize)
/* Expands the current stack if it has grown. We cannot shrink a stack segment
   when it grows smaller because the frame is checked only at the beginning of
   a function to ensure that there is enough space for the maximum that can
   be allocated. */
{
    /* Get current size of new stack segment. */
    POLYUNSIGNED old_len = taskData->stack->spaceSize();

    if (old_len >= minSize) return; /* Ok with present size. */

    // If it is too small double its size.
    POLYUNSIGNED new_len; /* New size */
    for (new_len = old_len; new_len < minSize; new_len *= 2);
    POLYUNSIGNED limitSize = getPolyUnsigned(taskData, taskData->threadObject->mlStackSize);

    // Do not grow the stack if its size is already too big.
    if ((limitSize != 0 && old_len >= limitSize) || ! gMem.GrowOrShrinkStack(taskData, new_len))
    {
        /* Cannot expand the stack any further. */
        fprintf(stderr, "Warning - Unable to increase stack - interrupting thread\n");
        if (debugOptions & DEBUG_THREADS)
            Log("THREAD: Unable to grow stack for thread %p from %lu to %lu\n", taskData, old_len, new_len);
        // We really should do this only if the thread is handling interrupts
        // asynchronously.  On the other hand what else do we do?
        Handle exn = make_exn(taskData, EXC_interrupt, SAVE(TAGGED(0)));
        taskData->SetException(DEREFEXNHANDLE(exn));
    }
    else
    {
        if (debugOptions & DEBUG_THREADS)
            Log("THREAD: Growing stack for thread %p from %lu to %lu\n", taskData, old_len, new_len);
    }
}

// This is used after executing each top-level command to minimise the
// heap size.  It's fairly dubious and there ought to be a better way to do this.
Handle shrink_stack_c(TaskData *taskData, Handle reserved_space)
/* Shrinks the current stack. */
{
    unsigned reserved = get_C_unsigned(taskData, DEREFWORDHANDLE(reserved_space));
 
    /* The minimum size must include the reserved space for the registers. */
    POLYUNSIGNED min_size = taskData->currentStackSpace() + reserved;

    POLYUNSIGNED new_len; /* New size */
    for (new_len = machineDependent->InitialStackSize(); new_len < min_size; new_len *= 2);

    if (taskData->stack->spaceSize() <= new_len) return SAVE(TAGGED(0)); /* OK with present size. */

    // Try to change the stack size but ignore any error since the current size will do.
    gMem.GrowOrShrinkStack(taskData, new_len);

    return SAVE(TAGGED(0));
}

static unsigned long rtsCallCounts[POLY_SYS_vecsize];

void IncrementRTSCallCount(unsigned ioFunction)
{
    if ((debugOptions & DEBUG_RTSCALLS) && ioFunction < POLY_SYS_vecsize)
        rtsCallCounts[ioFunction]++;
}

// This RTS module is defined purely to allow us to print the statistics.
class RTS: public RtsModule
{
public:
    virtual void Stop(void);
};

static RTS rtsModule;

static const char * const rtsName[POLY_SYS_vecsize] =
{
    "RTS Call   0",
    "SYS_exit",
    "RTS Call   2",
    "RTS Call   3",
    "RTS Call   4",
    "RTS Call   5",
    "RTS Call   6",
    "RTS Call   7",
    "RTS Call   8",
    "SYS_chdir",
    "RTS Call  10",
    "SYS_alloc_store",
    "SYS_alloc_unit",
    "RTS Call  13",
    "SYS_raisex",
    "SYS_get_length",
    "RTS Call  16",
    "SYS_get_flags",
    "RTS Call  18",
    "RTS Call  19",
    "RTS Call  20",
    "RTS Call  21",
    "RTS Call  22",
    "SYS_str_compare",
    "RTS Call  24",
    "RTS Call  25",
    "SYS_teststrgtr",
    "SYS_teststrlss",
    "SYS_teststrgeq",
    "SYS_teststrleq",
    "RTS Call  30",
    "RTS Call  31",
    "SYS_exception_trace_fn",
    "SYS_give_ex_trace_fn",
    "RTS Call  34",
    "RTS Call  35",
    "RTS Call  36",
    "RTS Call  37",
    "RTS Call  38",
    "RTS Call  39",
    "RTS Call  40",
    "RTS Call  41",
    "RTS Call  42",
    "RTS Call  43",
    "RTS Call  44",
    "RTS Call  45",
    "RTS Call  46",
    "SYS_lockseg",
    "SYS_emptystring", // Not actually a call
    "SYS_nullvector",  // Not actually a call
    "RTS Call  50",
    "SYS_network",
    "SYS_os_specific",
    "SYS_eq_longword",
    "SYS_neq_longword",
    "SYS_geq_longword",
    "SYS_leq_longword",
    "SYS_gt_longword",
    "SYS_lt_longword",
    "RTS Call  59",
    "RTS Call  60",
    "SYS_io_dispatch",
    "SYS_signal_handler",
    "RTS Call  63",
    "RTS Call  64",
    "RTS Call  65",
    "RTS Call  66",
    "RTS Call  67",
    "RTS Call  68",
    "SYS_atomic_reset",
    "SYS_atomic_incr",
    "SYS_atomic_decr",
    "SYS_thread_self",
    "SYS_thread_dispatch",
    "SYS_plus_longword",
    "SYS_minus_longword",
    "SYS_mul_longword",
    "SYS_div_longword",
    "SYS_mod_longword",
    "SYS_andb_longword",
    "SYS_orb_longword",
    "SYS_xorb_longword",
    "RTS Call  82",
    "RTS Call  83",
    "SYS_kill_self",
    "SYS_shift_left_longword",
    "SYS_shift_right_longword",
    "SYS_shift_right_arith_longword",
    "SYS_profiler",
    "SYS_longword_to_tagged",
    "SYS_signed_to_longword",
    "SYS_unsigned_to_longword",
    "SYS_full_gc",
    "SYS_stack_trace",
    "SYS_timing_dispatch",
    "RTS Call  95",
    "RTS Call  96",
    "RTS Call  97",
    "RTS Call  98",
    "SYS_objsize",
    "SYS_showsize",
    "RTS Call 101",
    "RTS Call 102",
    "RTS Call 103",
    "SYS_quotrem",
    "SYS_is_short",
    "SYS_aplus",
    "SYS_aminus",
    "SYS_amul",
    "SYS_adiv",
    "SYS_amod",
    "SYS_aneg",
    "SYS_xora",
    "SYS_equala",
    "SYS_ora",
    "SYS_anda",
    "RTS Call 116",
    "SYS_Real_str",
    "SYS_Real_geq",
    "SYS_Real_leq",
    "SYS_Real_gtr",
    "SYS_Real_lss",
    "SYS_Real_eq",
    "SYS_Real_neq",
    "SYS_Real_Dispatch",
    "SYS_Add_real",
    "SYS_Sub_real",
    "SYS_Mul_real",
    "SYS_Div_real",
    "SYS_Abs_real",
    "SYS_Neg_real",
    "RTS Call 131",
    "SYS_Repr_real",
    "SYS_conv_real",
    "SYS_real_to_int",
    "SYS_int_to_real",
    "SYS_sqrt_real",
    "SYS_sin_real",
    "SYS_cos_real",
    "SYS_arctan_real",
    "SYS_exp_real",
    "SYS_ln_real",
    "RTS Call 142",
    "RTS Call 143",
    "RTS Call 144",
    "RTS Call 145",
    "RTS Call 146",
    "RTS Call 147",
    "SYS_stdin",    // Not actually a call
    "SYS_stdout",   // Not actually a call
    "SYS_process_env",
    "SYS_set_string_length",
    "SYS_get_first_long_word",
    "SYS_poly_specific",
    "SYS_bytevec_eq",
    "RTS Call 155",
    "RTS Call 156",
    "RTS Call 157",
    "RTS Call 158",
    "RTS Call 159",
    "SYS_cmem_load_8",
    "SYS_cmem_load_16",
    "SYS_cmem_load_32",
    "SYS_cmem_load_64",
    "SYS_cmem_load_float",
    "SYS_cmem_load_double",
    "SYS_cmem_store_8",
    "SYS_cmem_store_16",
    "SYS_cmem_store_32",
    "SYS_cmem_store_64",
    "SYS_cmem_store_float",
    "SYS_cmem_store_double",
    "RTS Call 172",
    "RTS Call 173",
    "RTS Call 174",
    "RTS Call 175",
    "RTS Call 176",
    "RTS Call 177",
    "RTS Call 178",
    "RTS Call 179",
    "RTS Call 170",
    "RTS Call 181",
    "RTS Call 182",
    "RTS Call 183",
    "RTS Call 184",
    "RTS Call 185",
    "RTS Call 186",
    "RTS Call 187",
    "RTS Call 188",
    "SYS_io_operation",
    "SYS_ffi",
    "RTS Call 191",
    "RTS Call 192",
    "RTS Call 193",
    "SYS_set_code_constant",
    "SYS_move_words",
    "SYS_shift_right_arith_word",
    "SYS_int_to_word",
    "SYS_move_bytes",
    "SYS_move_bytes_overlap",
    "SYS_code_flags",
    "SYS_shrink_stack",
    "SYS_stderr",
    "RTS Call 203",
    "SYS_callcode_tupled",
    "SYS_foreign_dispatch",
    "SYS_foreign_null",
    "RTS Call 207",
    "RTS Call 208",
    "SYS_XWindows",
    "RTS Call 210",
    "RTS Call 211",
    "RTS Call 212",
    "SYS_is_big_endian",
    "SYS_bytes_per_word",
    "SYS_offset_address",
    "SYS_shift_right_word",
    "SYS_word_neq",
    "SYS_not_bool",
    "RTS Call 219",
    "RTS Call 220",
    "RTS Call 221",
    "RTS Call 222",
    "SYS_string_length",
    "RTS Call 224",
    "RTS Call 225",
    "RTS Call 226",
    "RTS Call 227",
    "SYS_touch_final",
    "SYS_int_eq",
    "SYS_int_neq",
    "SYS_int_geq",
    "SYS_int_leq",
    "SYS_int_gtr",
    "SYS_int_lss",
    "SYS_load_byte_immut",
    "SYS_load_word_immut",
    "RTS Call 237",
    "SYS_mul_word",
    "SYS_plus_word",
    "SYS_minus_word",
    "SYS_div_word",
    "SYS_or_word",
    "SYS_and_word",
    "SYS_xor_word",
    "SYS_shift_left_word",
    "SYS_mod_word",
    "SYS_word_geq",
    "SYS_word_leq",
    "SYS_word_gtr",
    "SYS_word_lss",
    "SYS_word_eq",
    "SYS_load_byte",
    "SYS_load_word",
    "SYS_assign_byte",
    "SYS_assign_word"
};

void RTS::Stop()
{
    if (debugOptions & DEBUG_RTSCALLS)
    {
        for (unsigned i = 0; i < POLY_SYS_vecsize; i++)
        {
            if (rtsCallCounts[i] != 0)
                Log("RTS: %s called %lu times\n", rtsName[i], rtsCallCounts[i]);
        }
    }
}
