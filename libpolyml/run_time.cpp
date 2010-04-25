/*
    Title:      Run-time system.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000, 2009
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(WIN32)
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
#include "xwindows.h"
#include "mpoly.h"
#include "arb.h"
#include "machine_dep.h"
#include "objsize.h"
#include "foreign.h"
#include "diagnostics.h"
#include "processes.h"
#include "profiling.h"
#include "basicio.h"
#include "run_time.h"
#include "sys.h"
#include "process_env.h"
#include "timing.h"
#include "network.h"
#include "os_specific.h"
#include "sighandler.h"
#include "reals.h"
#include "scanaddrs.h"
#include "check_objects.h"
#include "polystring.h"
#include "poly_specific.h"
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
        StackObject *stack = taskData->stack;
        add_count(taskData, stack->p_pc, stack->p_sp, words);
    }

    PolyWord *foundSpace = processes->FindAllocationSpace(taskData, words, false);
    if (foundSpace == 0)
    {
        // Failed - the thread is set to raise an exception.
        throw IOException(EXC_EXCEPTION);
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
#ifdef WINDOWS_PC
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
    machineDependent->SetException(taskData, DEREFEXNHANDLE(exn));
    throw IOException(EXC_EXCEPTION); /* Return to Poly code immediately. */
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

// Raises a Fail exception.
void raise_fail(TaskData *taskData, const char *errmsg)
{
    raise_exception_string(taskData, EXC_Fail, errmsg);
}


/******************************************************************************/
/*                                                                            */
/*      TRACE FUNCTIONS                                                       */
/*                                                                            */
/******************************************************************************/

bool trace_allowed = false; // Allows ^C to abort a trace.

/******************************************************************************/
/*                                                                            */
/*      give_stack_trace - utility function - doesn't allocate                */
/*                                                                            */
/******************************************************************************/
void give_stack_trace(TaskData *taskData, PolyWord *sp, PolyWord *finish)
{
    /* Now search for the return addresses on the stack.
       The values we find on the stack which are not PolyWord aligned may be
       either return addresses or the addresses of handlers. */

    trace_allowed = true; /* May be switch off by catchINT. */
    
    // The exception handler is used to suppress the addresses of
    // handlers which would otherwise look like return addresses.
    // Since we don't pass that in we may find it is actually out of
    // date if we are producing a trace as a result of pressing ^C.
    StackObject *stack = taskData->stack;
    PolyWord *exceptions = stack->p_hr;
    PolyWord *endStack = stack->Offset(stack->Length());
    
#ifdef DEBUG    
    printf("starting trace: sp = %p, finish = %p, end_of_stack = %p\n",
        sp, finish, endStack);
    fflush(stdout);
#endif
    
    if (finish > endStack) finish = endStack;
    
    for(; trace_allowed && sp < finish-1; sp++)
    {
        PolyWord pc = *sp;
        
        /* If this is an exception handler do not treat it as return
           address, just get the next handler */
        if (sp == exceptions)
        {
            /* Skip over the handlers until we find the next pointer up the
                stack. */
            while (sp < finish) {
                exceptions = (*sp).AsStackAddr();
                if (exceptions >= sp && exceptions <= endStack)
                    break;
                sp++;
            }
        }
        else if (pc.IsCodePtr())
        {
           /* A code pointer will be either a return address or a pointer
              to the constant section. (Or an exception handler?) */
            // We used to have a check that this was not a constant area
            // pointer but we don't have those any more.
            
            /* Initialise ptr to points at the end-of-code marker */
            // This used to use the OBJ_CODEPTR_TO_CONSTS_PTR macro which
            // simply looked for the end-of-code marker.
            PolyObject *ptr = ObjCodePtrToPtr(pc.AsCodePtr());
            PolyWord *consts = ptr->ConstPtrForCode();
            PolyWord p_name = consts[0]; /* Get procedure name */
            
            /* The name may be zero if it is anonymous */
            if (p_name == TAGGED(0)) fputs("<anon>\n",stdout);
            else {
                print_string(p_name);
                putc('\n',stdout);
            }
        }
    }
    fflush(stdout);
}


/******************************************************************************/
/*                                                                            */
/*      stack_trace_c - called from assembly code                             */
/*                                                                            */
/******************************************************************************/
/* CALL_IO0(stack_trace_, NOIND) */
static Handle stack_trace_c(TaskData *taskData)
{
    StackObject *stack = taskData->stack;
    PolyWord *endStack = stack->Offset(stack->Length());
    give_stack_trace (taskData, stack->p_sp, endStack);
    return SAVE(TAGGED(0));
}

/******************************************************************************/
/*                                                                            */
/*      ex_tracec - called from assembly code                                 */
/*                                                                            */
/******************************************************************************/
/* CALL_IO2(ex_trace, REF, REF, NOIND) */
Handle ex_tracec(TaskData *taskData, Handle exnHandle, Handle handler_handle)
{
    PolyWord *handler = DEREFWORD(handler_handle).AsStackAddr();
    
    fputs("\nException trace for exception - ", stdout);
    print_string((DEREFEXNHANDLE(exnHandle))->ex_name);
    // For backwards compatibility check the packet length first
    if (DEREFHANDLE(exnHandle)->Length() == SIZEOF(poly_exn)) {
        if (DEREFEXNHANDLE(exnHandle)->ex_location.IsDataPtr()) {
            PolyObject *location = DEREFEXNHANDLE(exnHandle)->ex_location.AsObjPtr();
            PolyWord fileName = location->Get(0);
            POLYSIGNED lineNo = get_C_long(taskData, location->Get(1));
            if (fileName.IsTagged() || ((PolyStringObject *)fileName.AsObjPtr())->length != 0) {
                printf(" raised in ");
                print_string(fileName);
                if (lineNo != 0) printf(" line %ld", lineNo);
            }
            else if (lineNo != 0) printf(" raised at line %ld", lineNo);
            fputs("\n", stdout);
        }
    }
    putc('\n',stdout);
    
    /* Trace down as far as the dummy handler on the stack. */
    StackObject *stack = taskData->stack;
    give_stack_trace(taskData, stack->p_sp, handler);
    fputs("End of trace\n\n",stdout);
    fflush(stdout);
    
    /* Set up the next handler so we don't come back here when we raise
       the exception again. */
    taskData->stack->p_hr = (PolyWord*)(handler->AsStackAddr());
    
    /* Set the exception data back again. */
    machineDependent->SetException(taskData, (poly_exn *)DEREFHANDLE(exnHandle));
    
    throw IOException(EXC_EXCEPTION); /* Reraise the exception. */
    /*NOTREACHED*/
}

/* end of interrupt handling */

// Return the address of the iovec entry for a given index.
Handle io_operation_c(TaskData *taskData, Handle entry)
{
    POLYUNSIGNED entryNo = get_C_ulong(taskData, DEREFWORD(entry));
    if (entryNo >= POLY_SYS_vecsize)
        raise_exception0(taskData, EXC_subscript);
    return SAVE((PolyObject*)IoEntry(entryNo));
}


/******************************************************************************/
/*                                                                            */
/*      INITIALISATION                                                         */
/*                                                                            */
/******************************************************************************/

/******************************************************************************/
/*                                                                            */
/*      re_init_run_time_system - called from mpoly.c                         */
/*                                                                            */
/******************************************************************************/
void re_init_run_time_system(void)
{
    ReinitModules();
}

/******************************************************************************/
/*                                                                            */
/*      init_run_time_system - called from mpoly.c                            */
/*                                                                            */
/******************************************************************************/
void init_run_time_system(void)
{
    InitModules(); // Initialise other modules.
}

/******************************************************************************/
/*                                                                            */
/*      uninit_run_time_system - called from ???                            */
/*                                                                            */
/******************************************************************************/

/* Release all resources.
   This is really only needed with Windows when running as a DLL within
   the address space of another process. */
void uninit_run_time_system(void)
{
    UninitModules();

#if (0)
    uninit_xwindow_system(); /* Probably needs to be done. */
#endif
}


/******************************************************************************/
/*                                                                            */
/*      get_flags_c - called from machine_assembly.s                          */
/*                                                                            */
/******************************************************************************/
/* CALL_IO1(get_flags_,REF,NOIND) */
static Handle get_flags_c(TaskData *taskData, Handle addr_handle)
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

/******************************************************************************/
/*                                                                            */
/*      BadOpCode_c - called from machine_assembly.s                          */
/*                                                                            */
/******************************************************************************/
Handle BadOpCode_c(TaskData *taskData)
{
    raise_exception_string(taskData, EXC_Fail, "Bad RunTime OpCode");
    return SAVE(TAGGED(1));
}

/* CALL_IO3(assign_byte_long_, REF, REF, REF, NOIND) */
static Handle assign_byte_long_c(TaskData *taskData, Handle value_handle, Handle byte_no, Handle vector)
{
    PolyWord value = DEREFHANDLE(value_handle);
    POLYUNSIGNED  offset  = get_C_ulong(taskData, DEREFWORDHANDLE(byte_no));  /* SPF 31/10/93 */
    byte *pointer = DEREFBYTEHANDLE(vector);    
    byte v = (byte)UNTAGGED(value);
    pointer[offset] = v;
    return taskData->saveVec.push(TAGGED(0));
}

/* CALL_IO3(assign_word_long_, REF, REF, REF, NOIND) */
static Handle assign_word_long_c(TaskData *taskData, Handle value_handle, Handle word_no, Handle vector)
{
    PolyWord value      = DEREFHANDLE(value_handle);
    POLYUNSIGNED offset = get_C_ulong(taskData, DEREFWORDHANDLE(word_no)); /* SPF 31/10/93 */
    PolyObject *pointer   = DEREFWORDHANDLE(vector);
    pointer->Set(offset, value);
    return taskData->saveVec.push(TAGGED(0));
}

/* CALL_IO5(move_bytes_long_, REF, REF, REF, REF, REF, NOIND) */
/* Move a segment of bytes, typically a string.  */
static Handle move_bytes_long_c(TaskData *taskData, Handle len, Handle dest_offset_handle, Handle dest_handle,
                       Handle src_offset_handle, Handle src_handle)
{
    unsigned src_offset = get_C_ulong(taskData, DEREFWORDHANDLE(src_offset_handle));
    byte *source = DEREFBYTEHANDLE(src_handle) + src_offset;
    unsigned dest_offset = get_C_ulong(taskData, DEREFWORDHANDLE(dest_offset_handle));
    byte *destination = DEREFBYTEHANDLE(dest_handle);
    byte *dest = destination + dest_offset;
    unsigned bytes = get_C_ulong(taskData, DEREFWORDHANDLE(len));
    PolyObject *obj = DEREFHANDLE(dest_handle);
    ASSERT(obj->IsByteObject());

    memmove(dest, source, bytes);  /* must work for overlapping segments. */
    return taskData->saveVec.push(TAGGED(0));
}

/* CALL_IO5(move_words_long_, REF, REF, REF, REF, REF, NOIND) */
/* Move a segment of words.   Similar to move_bytes_long_ except that
   it is used for PolyWord segments. */
static Handle move_words_long_c(TaskData *taskData, Handle len, Handle dest_offset_handle, Handle dest_handle,
                       Handle src_offset_handle, Handle src_handle)
{
    POLYUNSIGNED src_offset = get_C_ulong(taskData, DEREFWORDHANDLE(src_offset_handle));
    PolyObject *sourceObj = DEREFWORDHANDLE(src_handle);
    PolyWord *source = sourceObj->Offset(src_offset);

    POLYUNSIGNED dest_offset = get_C_ulong(taskData, DEREFWORDHANDLE(dest_offset_handle));

    PolyObject *destObject = DEREFWORDHANDLE(dest_handle);
    PolyWord *dest = destObject->Offset(dest_offset);

    POLYUNSIGNED words = get_C_ulong(taskData, DEREFWORDHANDLE(len));

    ASSERT(! destObject->IsByteObject());

    memmove(dest, source, words*sizeof(PolyWord));  /* must work for overlapping segments. */
    return taskData->saveVec.push(TAGGED(0));
}

static Handle testBytesEqual(TaskData *taskData, Handle len, Handle yOffset, Handle y,
                             Handle xOffset, Handle x)
{
    POLYUNSIGNED x_offset = get_C_ulong(taskData, DEREFWORDHANDLE(xOffset));
    byte *xAddr = DEREFBYTEHANDLE(x) + x_offset;

    POLYUNSIGNED y_offset = get_C_ulong(taskData, DEREFWORDHANDLE(yOffset));
    byte *yAddr = DEREFBYTEHANDLE(y) + y_offset;

    unsigned bytes = get_C_ulong(taskData, DEREFWORDHANDLE(len));

    int res = memcmp(xAddr, yAddr, bytes);
    if (res == 0) return taskData->saveVec.push(TAGGED(1));
    else return taskData->saveVec.push(TAGGED(0));
}

static Handle vec_length_c(TaskData *taskData, Handle vector)    /* Length of a vector */
{
    POLYUNSIGNED length = vector->WordP()->Length();
    return Make_arbitrary_precision(taskData, length);
}

static Handle load_byte_long_c(TaskData *taskData, Handle byte_no /* offset in BYTES */, Handle addr)
{
    POLYUNSIGNED offset = get_C_ulong(taskData, DEREFWORDHANDLE(byte_no));
    return taskData->saveVec.push(TAGGED(DEREFBYTEHANDLE(addr)[offset]));
}

static Handle load_word_long_c(TaskData *taskData, Handle word_no /* offset in WORDS */, Handle addr)
{
    POLYUNSIGNED offset = get_C_ulong(taskData, DEREFWORDHANDLE(word_no));
    return taskData->saveVec.push(addr->Word().AsObjPtr()->Get(offset));
}

// In most cases the assembly coded version of this will handle the
// allocation.  The function can be called by the assembly code
// when it finds it has run out.  Using it avoids us having a
// return address into the assembly code.
static Handle alloc_store_long_c(TaskData *taskData, Handle initial, Handle flags_handle, Handle size )
{
    POLYUNSIGNED flags = get_C_ulong(taskData, DEREFWORD(flags_handle));
    POLYUNSIGNED usize = get_C_ulong(taskData, DEREFWORD(size));
    
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
static Handle alloc_uninit_c(TaskData *taskData, Handle flags_handle, Handle size )
{
    Handle initial = SAVE(TAGGED(0));
    return alloc_store_long_c(taskData, initial, flags_handle, size);
}

/* Word functions. These functions assume that their arguments are tagged
   integers and treat them as unsigned values.
   These functions will almost always be implemented directly in the code
   generator with back-up versions in the machine-dependent assembly code
   section.  They are included here for completeness. */
static Handle mul_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(TAGGED(wx*wy));
}

static Handle plus_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(TAGGED(wx+wy));
}

static Handle minus_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(TAGGED(wx-wy));
}

static Handle div_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    if (wy == 0) raise_exception0(taskData, EXC_divide);
    return taskData->saveVec.push(TAGGED(wx/wy));
}

static Handle mod_word_c(TaskData *taskData, Handle y, Handle x)
{
    // In most cases it doesn't matter whether we use UNTAGGED or UNTAGGED_UNSIGNED
    // but in mod we will get the wrong answer if we use UNTAGGED here.
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    if (wy == 0) raise_exception0(taskData, EXC_divide);
    return taskData->saveVec.push(TAGGED(wx%wy));
}

static Handle word_eq_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(wx==wy ? TAGGED(1) : TAGGED(0));
}

static Handle word_neq_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(wx!=wy ? TAGGED(1) : TAGGED(0));
}

static Handle word_geq_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(wx>=wy ? TAGGED(1) : TAGGED(0));
}

static Handle word_leq_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(wx<=wy ? TAGGED(1) : TAGGED(0));
}

static Handle word_gtr_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(wx>wy ? TAGGED(1) : TAGGED(0));
}

static Handle word_lss_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(wx<wy ? TAGGED(1) : TAGGED(0));
}

static Handle and_word_c(TaskData *taskData, Handle y, Handle x)
{
    /* Normally it isn't necessary to remove the tags and put them
       back on again.  We leave this code as it is just in case some
       architecture does it differently. */
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(TAGGED(wx & wy));
}

static Handle or_word_c(TaskData *taskData, Handle y, Handle x)
{
    /* Normally it isn't necessary to remove the tags and put them
       back on again.  We leave this code as it is just in case some
       architecture does it differently. */
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(TAGGED(wx | wy));
}

static Handle xor_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return taskData->saveVec.push(TAGGED(wx ^ wy));
}


static Handle not_bool_c(TaskData *taskData, Handle x)
{
    return taskData->saveVec.push(DEREFWORD(x) == TAGGED(0) ? TAGGED(1) : TAGGED(0));
}

static Handle shift_left_word_c(TaskData *taskData, Handle y, Handle x)
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

static Handle shift_right_word_c(TaskData *taskData, Handle y, Handle x)
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

static Handle shift_right_arith_word_c(TaskData *taskData, Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x)); /* Treat as a signed quantity. */
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    /* It is defined to return 0 or ~1 if the shift is greater than the
       number of bits in the word.  The shift instructions on many
       architectures don't get that right. */
    if (wy > sizeof(PolyWord)*8)
        return taskData->saveVec.push(wx < 0 ? TAGGED(-1) : TAGGED(0));
    return taskData->saveVec.push(TAGGED(wx>>wy));
}

static Handle set_code_constant(TaskData *taskData, Handle data, Handle constant, Handle offseth, Handle base)
{
    machineDependent->SetCodeConstant(taskData, data, constant, offseth, base);
    return taskData->saveVec.push(TAGGED(0));
}

void CheckAndGrowStack(TaskData *taskData, PolyWord *lower_limit)
/* Expands the current stack if it has grown. We cannot shrink a stack segment
   when it grows smaller because the frame is checked only at the beginning of
   a procedure to ensure that there is enough space for the maximum that can
   be allocated. */
{
    /* Get current size of new stack segment. */
    POLYUNSIGNED old_len = OBJECT_LENGTH(taskData->stack);
 
    /* The minimum size must include the reserved space for the registers. */
    POLYUNSIGNED min_size = ((PolyWord*)taskData->stack) + old_len - lower_limit + taskData->stack->p_space;
    
    if (old_len >= min_size) return; /* Ok with present size. */

    // If it is too small double its size.
    // BUT, the maximum size is 2^24-1 words (on 32 bit) or 2^56-1 on 64 bit.

    if (old_len == MAX_OBJECT_SIZE)
    {
        /* Cannot expand the stack any further. */
        fprintf(stderr, "Warning - Stack limit reached - interrupting process\n");
        // We really should do this only if the thread is handling interrupts
        // asynchronously.  On the other hand what else do we do?
        Handle exn = make_exn(taskData, EXC_interrupt, SAVE(TAGGED(0)));
        machineDependent->SetException(taskData, DEREFEXNHANDLE(exn));
        return;
    }

    POLYUNSIGNED new_len; /* New size */
    for (new_len = old_len; new_len < min_size; new_len *= 2);
    if (new_len > MAX_OBJECT_SIZE) new_len = MAX_OBJECT_SIZE;

    /* Must make a new frame and copy the data over. */
    StackObject *new_stack = // N.B.  May throw a C++ exception.
        (StackObject *)alloc(taskData, new_len, F_MUTABLE_BIT|F_STACK_OBJ);
    CopyStackFrame(taskData->stack, new_stack);
    taskData->stack = new_stack;
}

// This is used after executing each top-level command to minimise the
// heap size.  It's fairly dubious and there ought to be a better way to do this.
static Handle shrink_stack_c(TaskData *taskData, Handle reserved_space)
/* Shrinks the current stack. */
{
    int reserved = get_C_long(taskData, DEREFWORDHANDLE(reserved_space));

    int old_len; /* Current size of stack segment. */
    int new_len; /* New size */
    int min_size;
    StackObject *new_stack;

    if (reserved < 0)
    {
       raise_exception0(taskData, EXC_size);
    }

    /* Get current size of new stack segment. */
    old_len = OBJECT_LENGTH(taskData->stack);
 
    /* The minimum size must include the reserved space for the registers. */
    min_size = (((PolyWord*)taskData->stack) + old_len - (PolyWord*)taskData->stack->p_sp) + taskData->stack->p_space + reserved;
    
    for (new_len = machineDependent->InitialStackSize(); new_len < min_size; new_len *= 2);

    if (old_len <= new_len) return SAVE(TAGGED(0)); /* OK with present size. */

    /* Must make a new frame and copy the data over. */
    new_stack = (StackObject *)alloc(taskData, new_len, F_MUTABLE_BIT|F_STACK_OBJ);
    CopyStackFrame(taskData->stack, new_stack);
    taskData->stack = new_stack;    
    return SAVE(TAGGED(0));
}

Handle EnterPolyCode(TaskData *taskData)
/* Called from "main" to enter the code. */
{
    Handle hOriginal = taskData->saveVec.mark(); // Set this up for the IO calls.
    while (1)
    {
        taskData->saveVec.reset(hOriginal); // Remove old RTS arguments and results.
        // Run the ML code and return with the function to call.
        int ioFunction = machineDependent->SwitchToPoly(taskData);
	    try {
            switch (ioFunction)
            {
            case -1:
                // We've been interrupted.  This usually involves simulating a
                // stack overflow so we could come here because of a genuine
                // stack overflow.
                // Previously this code was executed on every RTS call but there
                // were problems on Mac OS X at least with contention on schedLock.
                taskData->pendingInterrupt = false; // Clear this before we handle these
                // Process any asynchronous events i.e. interrupts or kill
                processes->ProcessAsynchRequests(taskData);
                // Release and re-acquire use of the ML memory to allow another thread
                // to GC.
                processes->ThreadReleaseMLMemory(taskData);
                processes->ThreadUseMLMemory(taskData);
                break;

            case -2: // A callback has returned.
                return machineDependent->CallBackResult(taskData);

            case POLY_SYS_exit:
                machineDependent->CallIO1(taskData, &finishc);
                break;

            case POLY_SYS_alloc_store:
                machineDependent->CallIO3(taskData, &alloc_store_long_c);
                break;

            case POLY_SYS_alloc_uninit:
                machineDependent->CallIO2(taskData, &alloc_uninit_c);
                break;

            case POLY_SYS_chdir:
                machineDependent->CallIO1(taskData, &change_dirc);
                break;

            case POLY_SYS_get_length:
                machineDependent->CallIO1(taskData, &vec_length_c);
                break;

            case POLY_SYS_get_flags:
                machineDependent->CallIO1(taskData, &get_flags_c);
                break;

            case POLY_SYS_str_compare:
                machineDependent->CallIO2(taskData, compareStrings);
                break;

            case POLY_SYS_teststreq:
                machineDependent->CallIO2(taskData, &testStringEqual);
                break;

            case POLY_SYS_teststrneq:
                machineDependent->CallIO2(taskData, &testStringNotEqual);
                break;

            case POLY_SYS_teststrgtr:
                machineDependent->CallIO2(taskData, &testStringGreater);
                break;

            case POLY_SYS_teststrlss:
                machineDependent->CallIO2(taskData, &testStringLess);
                break;

            case POLY_SYS_teststrgeq:
                machineDependent->CallIO2(taskData, &testStringGreaterOrEqual);
                break;

            case POLY_SYS_teststrleq:
                machineDependent->CallIO2(taskData, &testStringLessOrEqual);
                break;

            case POLY_SYS_exception_trace: // Special case.
                machineDependent->SetExceptionTrace(taskData);
                break;

    //        case POLY_SYS_lockseg: machineDependent->CallIO1(taskData, &locksegc); break;

            case POLY_SYS_profiler:
                machineDependent->CallIO1(taskData, &profilerc);
                break;

            case POLY_SYS_quotrem:
                machineDependent->CallIO3(taskData, &quot_rem_c);
                break;

    //        case POLY_SYS_is_short: machineDependent->CallIO1(taskData, &is_shortc); break;

            case POLY_SYS_aplus:
                machineDependent->CallIO2(taskData, &add_longc);
                break;

            case POLY_SYS_aminus:
                machineDependent->CallIO2(taskData, &sub_longc);
                break;

            case POLY_SYS_amul:
                machineDependent->CallIO2(taskData, &mult_longc);
                break;

            case POLY_SYS_adiv:
                machineDependent->CallIO2(taskData, &div_longc);
                break;

            case POLY_SYS_amod:
                machineDependent->CallIO2(taskData, &rem_longc);
                break;

            case POLY_SYS_aneg:
                machineDependent->CallIO1(taskData, &neg_longc);
                break;

            case POLY_SYS_equala:
                machineDependent->CallIO2(taskData, &equal_longc);
                break;

            case POLY_SYS_ora:
                machineDependent->CallIO2(taskData, &or_longc);
                break;

            case POLY_SYS_anda:
                machineDependent->CallIO2(taskData, &and_longc);
                break;

            case POLY_SYS_xora:
                machineDependent->CallIO2(taskData, &xor_longc);
                break;

            case POLY_SYS_Real_str:
                machineDependent->CallIO3(taskData, &Real_strc);
                break;

            case POLY_SYS_Real_geq:
                machineDependent->CallIO2(taskData, &Real_geqc);
                break;

            case POLY_SYS_Real_leq:
                machineDependent->CallIO2(taskData, &Real_leqc);
                break;

            case POLY_SYS_Real_gtr:
                machineDependent->CallIO2(taskData, &Real_gtrc);
                break;

            case POLY_SYS_Real_lss:
                machineDependent->CallIO2(taskData, &Real_lssc);
                break;

            case POLY_SYS_Real_eq:
                machineDependent->CallIO2(taskData, &Real_eqc);
                break;

            case POLY_SYS_Real_neq:
                machineDependent->CallIO2(taskData, &Real_neqc);
                break;

            case POLY_SYS_Real_Dispatch:
                machineDependent->CallIO2(taskData, &Real_dispatchc);
                break;

            case POLY_SYS_Add_real:
                machineDependent->CallIO2(taskData, &Real_addc);
                break;

            case POLY_SYS_Sub_real:
                machineDependent->CallIO2(taskData, &Real_subc);
                break;

            case POLY_SYS_Mul_real:
                machineDependent->CallIO2(taskData, &Real_mulc);
                break;

            case POLY_SYS_Div_real:
                machineDependent->CallIO2(taskData, &Real_divc);
                break;

            case POLY_SYS_Neg_real:
                machineDependent->CallIO1(taskData, &Real_negc);
                break;

            case POLY_SYS_Repr_real:
                machineDependent->CallIO1(taskData, &Real_reprc);
                break;

            case POLY_SYS_conv_real:
                machineDependent->CallIO1(taskData, &Real_convc);
                break;

            case POLY_SYS_real_to_int:
                machineDependent->CallIO1(taskData, &Real_intc);
                break;

            case POLY_SYS_int_to_real:
                machineDependent->CallIO1(taskData, &Real_floatc);
                break;

            case POLY_SYS_sqrt_real:
                machineDependent->CallIO1(taskData, &Real_sqrtc);
                break;

            case POLY_SYS_sin_real:
                machineDependent->CallIO1(taskData, &Real_sinc);
                break;

            case POLY_SYS_cos_real:
                machineDependent->CallIO1(taskData, &Real_cosc);
                break;

            case POLY_SYS_arctan_real:
                machineDependent->CallIO1(taskData, &Real_arctanc);
                break;

            case POLY_SYS_exp_real:
                machineDependent->CallIO1(taskData, &Real_expc);
                break;

            case POLY_SYS_ln_real:
                machineDependent->CallIO1(taskData, &Real_lnc);
                break;

            case POLY_SYS_io_operation:
                machineDependent->CallIO1(taskData, &io_operation_c);
                break;

            case POLY_SYS_atomic_incr:
                machineDependent->CallIO1(taskData, &AtomicIncrement);
                break;

            case POLY_SYS_atomic_decr:
                machineDependent->CallIO1(taskData, &AtomicDecrement);
                break;

            case POLY_SYS_thread_self:
                machineDependent->CallIO0(taskData, &ThreadSelf);
                break;

            case POLY_SYS_thread_dispatch:
                machineDependent->CallIO2(taskData, &ThreadDispatch);
                break;

//            case POLY_SYS_offset_address: machineDependent->CallIO2(taskData, &offset_addressc); break;

            case POLY_SYS_shift_right_word:
                machineDependent->CallIO2(taskData, &shift_right_word_c);
                break;
    
            case POLY_SYS_word_neq:
                machineDependent->CallIO2(taskData, &word_neq_c);
                break;
    
            case POLY_SYS_not_bool:
                machineDependent->CallIO1(taskData, &not_bool_c);
                break;

            case POLY_SYS_string_length:
                machineDependent->CallIO1(taskData, &string_length_c);
                break;

            case POLY_SYS_int_eq:
                machineDependent->CallIO2(taskData, &equal_longc);
                break;

            case POLY_SYS_int_neq:
                machineDependent->CallIO2(taskData, &not_equal_longc);
                break;

            case POLY_SYS_int_geq:
                machineDependent->CallIO2(taskData, &ge_longc);
                break;

            case POLY_SYS_int_leq:
                machineDependent->CallIO2(taskData, &le_longc);
                break;

            case POLY_SYS_int_gtr:
                machineDependent->CallIO2(taskData, &gt_longc);
                break;

            case POLY_SYS_int_lss:
                machineDependent->CallIO2(taskData, &ls_longc);
                break;

            case POLY_SYS_or_word:
                machineDependent->CallIO2(taskData, &or_word_c);
                break;

            case POLY_SYS_and_word:
                machineDependent->CallIO2(taskData, &and_word_c);
                break;

            case POLY_SYS_xor_word:
                machineDependent->CallIO2(taskData, &xor_word_c);
                break;

            case POLY_SYS_shift_left_word:
                machineDependent->CallIO2(taskData, &shift_left_word_c);
                break;

            case POLY_SYS_word_eq:
                machineDependent->CallIO2(taskData, &word_eq_c);
                break;

            case POLY_SYS_load_byte:
                machineDependent->CallIO2(taskData, &load_byte_long_c);
                break;

            case POLY_SYS_load_word:
                machineDependent->CallIO2(taskData, &load_word_long_c);
                break;

    //        case POLY_SYS_is_big_endian: machineDependent->CallIO0(taskData, &is_big_endianc); break;
    //        case POLY_SYS_bytes_per_word: machineDependent->CallIO0(taskData, &bytes_per_wordc); break;

            case POLY_SYS_assign_byte:
                machineDependent->CallIO3(taskData, &assign_byte_long_c);
                break;

            case POLY_SYS_assign_word:
                machineDependent->CallIO3(taskData, &assign_word_long_c);
                break;

            // ObjSize and ShowSize are now in the poly_specific functions and
            // probably should be removed from here.
            case POLY_SYS_objsize:
                machineDependent->CallIO1(taskData, &ObjSize);
                break;

            case POLY_SYS_showsize:
                machineDependent->CallIO1(taskData, &ShowSize);
                break;

            case POLY_SYS_timing_dispatch:
                machineDependent->CallIO2(taskData, &timing_dispatch_c);
                break;

            case POLY_SYS_XWindows:
                machineDependent->CallIO1(taskData, &XWindows_c);
                break;

            case POLY_SYS_full_gc:
                machineDependent->CallIO0(taskData, &full_gc_c);
                break;

            case POLY_SYS_stack_trace:
                machineDependent->CallIO0(taskData, & stack_trace_c);
                break;

            case POLY_SYS_foreign_dispatch:
                machineDependent->CallIO2(taskData, &foreign_dispatch_c);
                break;

            case POLY_SYS_callcode_tupled:
                machineDependent->CallCodeTupled(taskData);
                break;

            case POLY_SYS_process_env: machineDependent->CallIO2(taskData, &process_env_dispatch_c); break;

    //        case POLY_SYS_set_string_length: machineDependent->CallIO2(taskData, &set_string_length_c); break;

            case POLY_SYS_shrink_stack:
                machineDependent->CallIO1(taskData, &shrink_stack_c);
                break;

            case POLY_SYS_code_flags:
                machineDependent->CallIO2(taskData, &CodeSegmentFlags);
                break;

            case POLY_SYS_shift_right_arith_word:
                machineDependent->CallIO2(taskData, &shift_right_arith_word_c);
                break;

            case POLY_SYS_get_first_long_word:
            case POLY_SYS_int_to_word:
                // POLY_SYS_int_to_word has generally been replaced by POLY_SYS_get_first_long_word.
                // The reason is that POLY_SYS_int_to_word may be applied to either a long or
                // a short argument whereas POLY_SYS_get_first_long_word must be applied to a
                // long argument and can be implemented very easily in the code-generator, at
                // least on a little-endian machine.
                machineDependent->CallIO1(taskData, &int_to_word_c);
                break;

            case POLY_SYS_poly_specific:
                machineDependent->CallIO2(taskData, &poly_dispatch_c);
                break;

            case POLY_SYS_bytevec_eq:
                machineDependent->CallIO5(taskData, &testBytesEqual);
                break;

            case POLY_SYS_set_code_constant:
                machineDependent->CallIO4(taskData, &set_code_constant);
                break;

            case POLY_SYS_move_bytes:
                machineDependent->CallIO5(taskData, &move_bytes_long_c);
                break;

            case POLY_SYS_move_words:
                machineDependent->CallIO5(taskData, &move_words_long_c);
                break;

            case POLY_SYS_mul_word:
                machineDependent->CallIO2(taskData, &mul_word_c);
                break;

            case POLY_SYS_plus_word:
                machineDependent->CallIO2(taskData, &plus_word_c);
                break;

            case POLY_SYS_minus_word:
                machineDependent->CallIO2(taskData, &minus_word_c);
                break;

            case POLY_SYS_div_word:
                machineDependent->CallIO2(taskData, &div_word_c);
                break;

            case POLY_SYS_mod_word:
                machineDependent->CallIO2(taskData, &mod_word_c);
                break;

            case POLY_SYS_word_geq:
                machineDependent->CallIO2(taskData, &word_geq_c);
                break;

            case POLY_SYS_word_leq:
                machineDependent->CallIO2(taskData, &word_leq_c);
                break;

            case POLY_SYS_word_gtr:
                machineDependent->CallIO2(taskData, &word_gtr_c);
                break;

            case POLY_SYS_word_lss:
                machineDependent->CallIO2(taskData, &word_lss_c);
                break;

            case POLY_SYS_io_dispatch:
                machineDependent->CallIO3(taskData, &IO_dispatch_c);
                break;

            case POLY_SYS_network:
                machineDependent->CallIO2(taskData, &Net_dispatch_c);
                break;

            case POLY_SYS_os_specific:
                machineDependent->CallIO2(taskData, &OS_spec_dispatch_c);
                break;

            case POLY_SYS_signal_handler:
                machineDependent->CallIO2(taskData, &Sig_dispatch_c);
                break;

            case POLY_SYS_kill_self:
                machineDependent->CallIO0(taskData, exitThread);
                break;

            // This is called from assembly code and doesn't actually have an entry in the
            // io vector.
            case POLY_SYS_give_ex_trace:
                machineDependent->CallIO2(taskData, ex_tracec);
                break;

            default:
                Crash("Unknown io operation %d\n", ioFunction);
            }
		}
		catch (IOException) {
        }

    }
}
