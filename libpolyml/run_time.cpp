/*
    Title:      Run-time system.
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

/* Contains most of the routines in the interface_map vector. Others are
   in their own modules e.g. arb.c, reals.c and persistence.c */

#ifdef _WIN32_WCE
#include "winceconfig.h"
#include "wincelib.h"
#else
#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
#endif
#endif


#ifndef __GNUC__
#ifndef __attribute__
#define __attribute__(attribute) /* attribute */
#endif
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

#define SAVE(x) gSaveVec->push(x)
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

/* Storage Allocator for the run-time system. 
   The addresses of objects allocated are saved in 
   this vector in case they are garbage-collected. */

PolyObject *alloc(POLYUNSIGNED data_words, unsigned flags)
/* Allocate a number of words. */
{
    POLYUNSIGNED words = data_words + 1;
    
    if (store_profiling)
        add_count(poly_stack->p_pc, poly_stack->p_sp, words);

    LocalMemSpace *allocSpace = 0;

    // See if we have space in some local mutable area.
    if (! (userOptions.debug & DEBUG_FORCEGC))
        allocSpace = gMem.GetAllocSpace(words);

    if (allocSpace == 0) // Space not available (or force GC)
    {
        if (! QuickGC (words))
        {
            fprintf(stderr,"Run out of store - interrupting console processes\n");
            processes->interrupt_console_processes();
            THROW_RETRY;
        }
        // Try again.  There should be space.
        allocSpace = gMem.GetAllocSpace(words);
    }
    ASSERT(allocSpace != 0);
    // There should be space now.
    allocSpace->pointer -= words;
    
    PolyWord *pt = allocSpace->pointer + 1;
    PolyObject *pObj = (PolyObject*)pt;
    pObj->SetLengthWord(data_words, flags);
    
    /* Must initialise object here, because GC no longer cleans store. */
    // N.B.  This sets the store to zero NOT TAGGED(0).
    // We do this even for byte segments because we rely on unused bytes having a
    // defined value when we do structure equality.
    for (POLYUNSIGNED i = 0; i < data_words; i++) pObj->Set(i, PolyWord::FromUnsigned(0));
    return pObj;
}

/******************************************************************************/
/*                                                                            */
/*      alloc_and_save - called by run-time system                            */
/*                                                                            */
/******************************************************************************/
Handle alloc_and_save(POLYUNSIGNED size, unsigned flags)
/* Allocate and save the result on the vector. */
{
    return SAVE(alloc(size, flags));
}

/******************************************************************************/
/*                                                                            */
/*      full_gc_c - called by assembly code                                */
/*                                                                            */
/******************************************************************************/
/* CALL_IO0(full_gc_, NOIND) */
Handle full_gc_c(void)
{
    FullGC();
    return SAVE(TAGGED(0));
}

/******************************************************************************/
/*                                                                            */
/*      EXCEPTIONS                                                            */
/*                                                                            */
/******************************************************************************/

/******************************************************************************/
/*                                                                            */
/*      make_exn - utility function - allocates in Poly heap                  */
/*                                                                            */
/******************************************************************************/
Handle make_exn(int id, Handle arg)
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
    default: ASSERT(0); exName = "Unknown"; // Shouldn't happen.
    }
   

    Handle pushed_name = SAVE(C_string_to_Poly(exName));
    
    Handle exnHandle = alloc_and_save(SIZEOF(poly_exn));
    
    DEREFEXNHANDLE(exnHandle)->ex_id   = TAGGED(id);
    DEREFEXNHANDLE(exnHandle)->ex_name = DEREFWORD(pushed_name);
    DEREFEXNHANDLE(exnHandle)->arg     = DEREFWORDHANDLE(arg);
    
    return exnHandle;
}

/******************************************************************************/
/*                                                                            */
/*      raise_exception - called by run-time system                           */
/*                                                                            */
/******************************************************************************/
void raise_exception(int id, Handle arg)
/* Raise an exception with no arguments. */
{
    Handle exn = make_exn(id,arg);
    /* N.B.  We must create the packet first BEFORE dereferencing the
       process handle just in case a GC while creating the packet
       moves the process and/or the stack. */
    machineDependent->SetException(processes->CurrentProcess()->stack, DEREFEXNHANDLE(exn));
    THROW_EXCEPTION; /* Return to Poly code immediately. */
    /*NOTREACHED*/
}


/******************************************************************************/
/*                                                                            */
/*      raise_exception0 - called by run-time system                          */
/*                                                                            */
/******************************************************************************/
void raise_exception0(int id)
/* Raise an exception with no arguments. */
{
  raise_exception(id, SAVE(TAGGED(0)));
  /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      raise_exception_string - called by run-time system                    */
/*                                                                            */
/******************************************************************************/
void raise_exception_string(int id, char *str)
/* Raise an exception with a C string as the argument. */
{
  raise_exception(id, SAVE(C_string_to_Poly(str)));
  /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      create_syscall_exception - called by run-time system                  */
/*                                                                            */
/******************************************************************************/
/* Create a syscall exception packet.  Used both directly in raise_syscall and
   also in interrupt_signal_processes. */
Handle create_syscall_exception(char *errmsg, int err)
{
    /* exception SysErr of (string * syserror Option.option) */
    /* If the argument is zero we don't have a suitable error number
       so map this to NONE.  Other values are mapped to SOME(n) */
    /* Create a pair of the error message and the error number. */
    Handle pushed_option, pushed_name, pair;
    if (err == 0) pushed_option = SAVE(NONE_VALUE); /* NONE */
    else
    {   /* SOME err */
        Handle errornum = Make_arbitrary_precision(err);
        pushed_option = alloc_and_save(1);
        DEREFHANDLE(pushed_option)->Set(0, DEREFWORDHANDLE(errornum));
    }
    pushed_name = SAVE(C_string_to_Poly(errmsg));
    pair = alloc_and_save(2);
    DEREFHANDLE(pair)->Set(0, DEREFWORDHANDLE(pushed_name));
    DEREFHANDLE(pair)->Set(1, DEREFWORDHANDLE(pushed_option));
    return pair;
}


/******************************************************************************/
/*                                                                            */
/*      raise_syscall - called by run-time system                             */
/*                                                                            */
/******************************************************************************/
/* Raises a Syserr exception. */
void raise_syscall(char *errmsg, int err)
{
    raise_exception(EXC_syserr, create_syscall_exception(errmsg, err));
}

/******************************************************************************/
/*                                                                            */
/*      execute_pending_interrupts - utility function - shouldn't allocate   */
/*                                                                            */
/******************************************************************************/
void execute_pending_interrupts(void)
/* Called when we are in a safe state to execute this. */
{
    while (interrupted)
    {
        /* We may get interrupts while processing others. */ 
        int sig = interrupted;
        interrupted = 0;
        InterruptModules(sig);
    }
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
void give_stack_trace(PolyWord *sp, PolyWord *finish)
{
    /* Now search for the return addresses on the stack.
       The values we find on the stack which are not PolyWord aligned may be
       either return addresses or the addresses of handlers. */

    trace_allowed = true; /* May be switch off by catchINT. */
    
    // The exception handler is used to suppress the addresses of
    // handlers which would otherwise look like return addresses.
    // Since we don't pass that in we may find it is actually out of
    // date if we are producing a trace as a result of pressing ^C.
    PolyWord *exceptions = poly_stack->p_hr;
    
#ifdef DEBUG    
    printf("starting trace: sp = %p, finish = %p, end_of_stack = %p\n",
        sp, finish, end_of_stack);
    fflush(stdout);
#endif
    
    if (finish > end_of_stack) finish = end_of_stack;
    
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
                if (exceptions >= sp && exceptions <= end_of_stack)
                    break;
                sp++;
            }
        }
        else if (pc.IsCodePtr())
        {
           /* A code pointer will be either a return address or a pointer
              to the constant section. (Or an exception handler?) */
            PolyWord *ptr;
            // We used to have a check that this was not a constant area
            // pointer but we don't have those any more.
            
            /* Initialise ptr to points at the end-of-code marker */
            OBJ_CODEPTR_TO_CONSTS_PTR(pc, ptr);
            
            PolyWord p_name = ptr[3]; /* Get procedure name */
            
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
Handle stack_trace_c(void)
{
    give_stack_trace (poly_stack->p_sp, end_of_stack);
    return SAVE(TAGGED(0));
}

/******************************************************************************/
/*                                                                            */
/*      ex_tracec - called from assembly code                                 */
/*                                                                            */
/******************************************************************************/
/* CALL_IO2(ex_trace, REF, REF, NOIND) */
Handle ex_tracec(Handle exnHandle, Handle handler_handle)
{
    PolyWord *handler = DEREFWORD(handler_handle).AsStackAddr();
    
    fputs("\nException trace for exception - ", stdout);
    print_string(((poly_exn *)DEREFHANDLE(exnHandle))->ex_name);
    putc('\n',stdout);
    
    /* Trace down as far as the dummy handler on the stack. */
    give_stack_trace(poly_stack->p_sp, handler);
    fputs("End of trace\n\n",stdout);
    fflush(stdout);
    
    /* Set up the next handler so we don't come back here when we raise
       the exception again. */
    processes->CurrentProcess()->stack->p_hr = (PolyWord*)(handler->AsStackAddr());
    
    /* Set the exception data back again. */
    machineDependent->SetException(processes->CurrentProcess()->stack,(poly_exn *)DEREFHANDLE(exnHandle));
    
    THROW_EXCEPTION; /* Reraise the exception. */
    /*NOTREACHED*/
	return 0;
}

/******************************************************************************/
/*                                                                            */
/*     catchINT - utility function: handles SIGINT signals; doesn't allocate  */
/*                                                                            */
/*  PC handleINT (CTRL+C handling function) - utility function (doesn't allocate) */
/*                                                                            */
/* This function is executed on a separate thread created by the OS when      */
/* CTRL+C is pressed. The main process is suspended and resumed at the end.   */
/* If the main process is not suspended then both the main process and the    */
/* thread (created by WNT) try to read from the same input(console) and this  */
/* causes problems.                                                           */
/*                                                                            */
/******************************************************************************/
/*
    It now simply sets an event and returns.  Some time later handleINT
    is called to actually deal with input.
*/
#if defined(WINDOWS_PC)
static int exitCode = 0;
static int exitRequest = 0;

static DWORD WINAPI CrowBarFn(LPVOID lpParameter)
/* This thread pauses for 10 seconds and then exits the process anyway.
   It is used to give the process time to close down gracefully if
   possible but to make sure it doesn't hang around if something is too
   badly screwed up. */
{
    Sleep(10000);
#ifdef _WIN32_WCE
	exit(exitCode);
#else
    ExitProcess(exitCode);
#endif
    return 0;
}

/* Request termination.  Called from the Window thread. */
void RequestFinish(int n)
{
    HANDLE hCrowBar;
    DWORD  dwId;
    /* We request an interrupt which should occur next time
       we do a stack check. */
    exitCode = n;
    exitRequest = 1;
    machineDependent->InterruptCode();
    hCrowBar = CreateThread(NULL, 0, CrowBarFn, 0, 0, &dwId);
	if (! hCrowBar)
	{
#ifdef _WIN32_WCE
		exit(n);
#else
		ExitProcess(n);
#endif
	}
    CloseHandle(hCrowBar);
}
#endif

/* end of interrupt handling */

// Return the address of the iovec entry for a given index.
Handle io_operation_c(Handle entry)
{
    POLYUNSIGNED entryNo = get_C_ulong(DEREFWORD(entry));
    if (entryNo >= POLY_SYS_vecsize)
        raise_exception0(EXC_subscript);
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

int interrupted = 0;

/******************************************************************************/
/*                                                                            */
/*      init_run_time_system - called from mpoly.c                            */
/*                                                                            */
/******************************************************************************/
void init_run_time_system(void)
{
    interrupted = 0;
    gSaveVec->init(); // initialise interface save vector
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
Handle get_flags_c(Handle addr_handle)
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
Handle CodeSegmentFlags(Handle flags_handle, Handle addr_handle)
{
    PolyObject *pt = DEREFWORDHANDLE(addr_handle);
    unsigned short newFlags = get_C_ushort(DEREFWORD(flags_handle));

    if (newFlags >= 256)
        raise_exception_string(EXC_Fail, "FreezeCodeSegment flags must be less than 256");

    if (! pt->IsMutable())
        raise_exception_string(EXC_Fail, "FreezeCodeSegment must be applied to a mutable segment");

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
Handle BadOpCode_c(void)
{
    raise_exception_string(EXC_Fail, "Bad RunTime OpCode");
    return SAVE(TAGGED(1));
}


// This is (was?) used to raise an exception from the assembly code section.
// It simplified the process by allowing the assembly code stub to pass an integer
// representing the exception to be raised and this created the exception packet.
Handle raise_exc(Handle id_handle)
/* Called from the assembly-code segment. */
{
   PolyWord id = DEREFWORD(id_handle);
   raise_exception0(UNTAGGED(id));
   return SAVE(TAGGED(0)); // Doesn't actually return
}

/* CALL_IO3(assign_byte_long_, REF, REF, REF, NOIND) */
Handle assign_byte_long_c(Handle value_handle, Handle byte_no, Handle vector)
{
    PolyWord value = DEREFHANDLE(value_handle);
    POLYUNSIGNED  offset  = get_C_ulong(DEREFWORDHANDLE(byte_no));  /* SPF 31/10/93 */
    byte *pointer = DEREFBYTEHANDLE(vector);    
    byte v = (byte)UNTAGGED(value);
    pointer[offset] = v;
    return gSaveVec->push(TAGGED(0));
}

/* CALL_IO3(assign_word_long_, REF, REF, REF, NOIND) */
Handle assign_word_long_c(Handle value_handle, Handle word_no, Handle vector)
{
    PolyWord value      = DEREFHANDLE(value_handle);
    POLYUNSIGNED offset = get_C_ulong(DEREFWORDHANDLE(word_no)); /* SPF 31/10/93 */
    PolyObject *pointer   = DEREFWORDHANDLE(vector);
    pointer->Set(offset, value);
    return gSaveVec->push(TAGGED(0));
}

/* CALL_IO5(move_bytes_long_, REF, REF, REF, REF, REF, NOIND) */
/* Move a segment of bytes, typically a string.  */
Handle move_bytes_long_c(Handle len, Handle dest_offset_handle, Handle dest_handle,
                       Handle src_offset_handle, Handle src_handle)
{
    unsigned src_offset = get_C_ulong(DEREFWORDHANDLE(src_offset_handle));
    byte *source = DEREFBYTEHANDLE(src_handle) + src_offset;
    unsigned dest_offset = get_C_ulong(DEREFWORDHANDLE(dest_offset_handle));
    byte *destination = DEREFBYTEHANDLE(dest_handle);
    byte *dest = destination + dest_offset;
    unsigned bytes = get_C_ulong(DEREFWORDHANDLE(len));
    PolyObject *obj = DEREFHANDLE(dest_handle);
    ASSERT(obj->IsByteObject());

    memmove(dest, source, bytes);  /* must work for overlapping segments. */
    return gSaveVec->push(TAGGED(0));
}

/* CALL_IO5(move_words_long_, REF, REF, REF, REF, REF, NOIND) */
/* Move a segment of words.   Similar to move_bytes_long_ except that
   it is used for PolyWord segments. */
Handle move_words_long_c(Handle len, Handle dest_offset_handle, Handle dest_handle,
                       Handle src_offset_handle, Handle src_handle)
{
    POLYUNSIGNED src_offset = get_C_ulong(DEREFWORDHANDLE(src_offset_handle));
    PolyObject *sourceObj = DEREFWORDHANDLE(src_handle);
    PolyWord *source = sourceObj->Offset(src_offset);

    POLYUNSIGNED dest_offset = get_C_ulong(DEREFWORDHANDLE(dest_offset_handle));

    PolyObject *destObject = DEREFWORDHANDLE(dest_handle);
    PolyWord *dest = destObject->Offset(dest_offset);

    POLYUNSIGNED words = get_C_ulong(DEREFWORDHANDLE(len));

    ASSERT(! destObject->IsByteObject());

    memmove(dest, source, words*sizeof(PolyWord));  /* must work for overlapping segments. */
    return gSaveVec->push(TAGGED(0));
}

static Handle vec_length_c(Handle vector)    /* Length of a vector */
{
    POLYUNSIGNED length = vector->WordP()->Length();
    return Make_arbitrary_precision (length);
}

static Handle load_byte_long_c(Handle byte_no /* offset in BYTES */, Handle addr)
{
    POLYUNSIGNED offset = get_C_ulong(DEREFWORDHANDLE(byte_no));
    return gSaveVec->push(TAGGED(DEREFBYTEHANDLE(addr)[offset]));
}

static Handle load_word_long_c(Handle word_no /* offset in WORDS */, Handle addr)
{
    POLYUNSIGNED offset = get_C_ulong(DEREFWORDHANDLE(word_no));
    return gSaveVec->push(addr->Word().AsObjPtr()->Get(offset));
}

// In most cases the assembly coded version of this will handle the
// allocation.  The function can be called by the assembly code
// when it finds it has run out.  Using it avoids us having a
// return address into the assembly code.
static Handle alloc_store_long_c(Handle initial, Handle flags_handle, Handle size )
{
    POLYUNSIGNED flags = get_C_ulong(DEREFWORD(flags_handle));
    POLYUNSIGNED usize = get_C_ulong(DEREFWORD(size));
    
    if (usize == 0) usize = 1;
    if (usize >= MAX_OBJECT_SIZE) raise_exception0(EXC_size);
    
    PolyObject *vector = alloc(usize, flags| F_MUTABLE_BIT);
    
    PolyWord value = DEREFWORD(initial);
    
    if (vector->IsByteObject()) {
        // Byte segments are supposed to be initialised only with zero
        if (value != TAGGED(0))
            raise_exception_string(EXC_Fail, "non-zero byte segment");
    }
    else if (value != PolyWord::FromUnsigned(0))  {
        for (POLYUNSIGNED i = 0; i < usize; i++)
            vector->Set(i, value);
    }
    
    return gSaveVec->push(vector);
}

/* Word functions. These functions assume that their arguments are tagged
   integers and treat them as unsigned values.
   These functions will almost always be implemented directly in the code
   generator with back-up versions in the machine-dependent assembly code
   section.  They are included here for completeness. */
static Handle mul_word_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return gSaveVec->push(TAGGED(wx*wy));
}

static Handle plus_word_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return gSaveVec->push(TAGGED(wx+wy));
}

static Handle minus_word_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return gSaveVec->push(TAGGED(wx-wy));
}

static Handle div_word_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    if (wy == 0) raise_exception0(EXC_divide);
    return gSaveVec->push(TAGGED(wx/wy));
}

static Handle mod_word_c(Handle y, Handle x)
{
    // In most cases it doesn't matter whether we use UNTAGGED or UNTAGGED_UNSIGNED
    // but in mod we will get the wrong answer if we use UNTAGGED here.
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    if (wy == 0) raise_exception0(EXC_divide);
    return gSaveVec->push(TAGGED(wx%wy));
}

static Handle word_eq_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return gSaveVec->push(wx==wy ? TAGGED(1) : TAGGED(0));
}

static Handle word_neq_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return gSaveVec->push(wx!=wy ? TAGGED(1) : TAGGED(0));
}

static Handle word_geq_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return gSaveVec->push(wx>=wy ? TAGGED(1) : TAGGED(0));
}

static Handle word_leq_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return gSaveVec->push(wx<=wy ? TAGGED(1) : TAGGED(0));
}

static Handle word_gtr_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return gSaveVec->push(wx>wy ? TAGGED(1) : TAGGED(0));
}

static Handle word_lss_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return gSaveVec->push(wx<wy ? TAGGED(1) : TAGGED(0));
}

static Handle and_word_c(Handle y, Handle x)
{
    /* Normally it isn't necessary to remove the tags and put them
       back on again.  We leave this code as it is just in case some
       architecture does it differently. */
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return gSaveVec->push(TAGGED(wx & wy));
}

static Handle or_word_c(Handle y, Handle x)
{
    /* Normally it isn't necessary to remove the tags and put them
       back on again.  We leave this code as it is just in case some
       architecture does it differently. */
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return gSaveVec->push(TAGGED(wx | wy));
}

static Handle xor_word_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    return gSaveVec->push(TAGGED(wx ^ wy));
}


static Handle not_bool_c(Handle x)
{
    return gSaveVec->push(DEREFWORD(x) == TAGGED(0) ? TAGGED(1) : TAGGED(0));
}

static Handle shift_left_word_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    /* It is defined to return 0 if the shift is greater than the
       number of bits in the PolyWord.  The shift instructions on many
       architectures don't get that right. */
    if (wy > sizeof(PolyWord)*8)
        return gSaveVec->push(TAGGED(0));
    return gSaveVec->push(TAGGED(wx<<wy));
}

static Handle shift_right_word_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x));
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    /* It is defined to return 0 if the shift is greater than the
       number of bits in the word.  The shift instructions on many
       architectures don't get that right. */
    if (wy > sizeof(PolyWord)*8)
        return gSaveVec->push(TAGGED(0));
    return gSaveVec->push(TAGGED(wx>>wy));
}

static Handle shift_right_arith_word_c(Handle y, Handle x)
{
    POLYUNSIGNED wx = UNTAGGED_UNSIGNED(DEREFWORD(x)); /* Treat as a signed quantity. */
    POLYUNSIGNED wy = UNTAGGED_UNSIGNED(DEREFWORD(y));
    /* It is defined to return 0 or ~1 if the shift is greater than the
       number of bits in the word.  The shift instructions on many
       architectures don't get that right. */
    if (wy > sizeof(PolyWord)*8)
        return gSaveVec->push(wx < 0 ? TAGGED(-1) : TAGGED(0));
    return gSaveVec->push(TAGGED(wx>>wy));
}

static Handle set_code_constant(Handle data, Handle constant, Handle offseth, Handle base)
{
    machineDependent->SetCodeConstant(data, constant, offseth, base);
    return gSaveVec->push(TAGGED(0));
}

Handle EnterPolyCode(void)
/* Called from "main" to enter the code. */
{
    /* Select the first process. */
    processes->select_next_process(); 

    while (1)
    {
        // Run the ML code and return with the function to call.
        int ioFunction = machineDependent->SwitchToPoly();
        gSaveVec->init(); // Set this up for the IO calls.
#ifdef _WIN32_WCE
		jmp_buf tbuf;
		memcpy(&tbuf, &exception_jump, sizeof(jmp_buf));
		if (setjmp(exception_jump) == 0) {
#else
	    try {
#endif
            switch (ioFunction)
            {
            case -1:
                execute_pending_interrupts();
                break;

            case -2: // A callback has returned.
                return machineDependent->CallBackResult();

            case POLY_SYS_exit:
                machineDependent->CallIO1(&finishc);
                break;

            case POLY_SYS_install_root:
                machineDependent->CallIO1(&install_rootc);
                break;

            case POLY_SYS_strconcat:
                machineDependent->CallIO2(&strconcatc);
                break;

            case POLY_SYS_alloc_store:
                machineDependent->CallIO3(&alloc_store_long_c);
                break;

            case POLY_SYS_chdir:
                machineDependent->CallIO1(&change_dirc);
                break;

            case POLY_SYS_get_length:
                machineDependent->CallIO1(&vec_length_c);
                break;

            case POLY_SYS_get_flags:
                machineDependent->CallIO1(&get_flags_c);
                break;

            case POLY_SYS_str_compare:
                machineDependent->CallIO2(compareStrings);
                break;

            case POLY_SYS_teststreq:
                machineDependent->CallIO2(&testStringEqual);
                break;

            case POLY_SYS_teststrneq:
                machineDependent->CallIO2(&testStringNotEqual);
                break;

            case POLY_SYS_teststrgtr:
                machineDependent->CallIO2(&testStringGreater);
                break;

            case POLY_SYS_teststrlss:
                machineDependent->CallIO2(&testStringLess);
                break;

            case POLY_SYS_teststrgeq:
                machineDependent->CallIO2(&testStringGreaterOrEqual);
                break;

            case POLY_SYS_teststrleq:
                machineDependent->CallIO2(&testStringLessOrEqual);
                break;

            case POLY_SYS_exception_trace: // Special case.
                machineDependent->SetExceptionTrace();
                break;

    //        case POLY_SYS_lockseg: machineDependent->CallIO1(&locksegc); break;

            case POLY_SYS_profiler:
                machineDependent->CallIO1(&profilerc);
                break;

    //        case POLY_SYS_is_short: machineDependent->CallIO1(&is_shortc); break;

            case POLY_SYS_aplus:
                machineDependent->CallIO2(&add_longc);
                break;

            case POLY_SYS_aminus:
                machineDependent->CallIO2(&sub_longc);
                break;

            case POLY_SYS_amul:
                machineDependent->CallIO2(&mult_longc);
                break;

            case POLY_SYS_adiv:
                machineDependent->CallIO2(&div_longc);
                break;

            case POLY_SYS_amod:
                machineDependent->CallIO2(&rem_longc);
                break;

            case POLY_SYS_aneg:
                machineDependent->CallIO1(&neg_longc);
                break;

            case POLY_SYS_equala:
                machineDependent->CallIO2(&equal_longc);
                break;

            case POLY_SYS_ora:
                machineDependent->CallIO2(&or_longc);
                break;

            case POLY_SYS_anda:
                machineDependent->CallIO2(&and_longc);
                break;

            case POLY_SYS_xora:
                machineDependent->CallIO2(&xor_longc);
                break;

            case POLY_SYS_Real_str:
                machineDependent->CallIO3(&Real_strc);
                break;

            case POLY_SYS_Real_geq:
                machineDependent->CallIO2(&Real_geqc);
                break;

            case POLY_SYS_Real_leq:
                machineDependent->CallIO2(&Real_leqc);
                break;

            case POLY_SYS_Real_gtr:
                machineDependent->CallIO2(&Real_gtrc);
                break;

            case POLY_SYS_Real_lss:
                machineDependent->CallIO2(&Real_lssc);
                break;

            case POLY_SYS_Real_eq:
                machineDependent->CallIO2(&Real_eqc);
                break;

            case POLY_SYS_Real_neq:
                machineDependent->CallIO2(&Real_neqc);
                break;

            case POLY_SYS_Real_Dispatch:
                machineDependent->CallIO2(&Real_dispatchc);
                break;

            case POLY_SYS_Add_real:
                machineDependent->CallIO2(&Real_addc);
                break;

            case POLY_SYS_Sub_real:
                machineDependent->CallIO2(&Real_subc);
                break;

            case POLY_SYS_Mul_real:
                machineDependent->CallIO2(&Real_mulc);
                break;

            case POLY_SYS_Div_real:
                machineDependent->CallIO2(&Real_divc);
                break;

            case POLY_SYS_Neg_real:
                machineDependent->CallIO1(&Real_negc);
                break;

            case POLY_SYS_Repr_real:
                machineDependent->CallIO1(&Real_reprc);
                break;

            case POLY_SYS_conv_real:
                machineDependent->CallIO1(&Real_convc);
                break;

            case POLY_SYS_real_to_int:
                machineDependent->CallIO1(&Real_intc);
                break;

            case POLY_SYS_int_to_real:
                machineDependent->CallIO1(&Real_floatc);
                break;

            case POLY_SYS_sqrt_real:
                machineDependent->CallIO1(&Real_sqrtc);
                break;

            case POLY_SYS_sin_real:
                machineDependent->CallIO1(&Real_sinc);
                break;

            case POLY_SYS_cos_real:
                machineDependent->CallIO1(&Real_cosc);
                break;

            case POLY_SYS_arctan_real:
                machineDependent->CallIO1(&Real_arctanc);
                break;

            case POLY_SYS_exp_real:
                machineDependent->CallIO1(&Real_expc);
                break;

            case POLY_SYS_ln_real:
                machineDependent->CallIO1(&Real_lnc);
                break;

            case POLY_SYS_io_operation:
                machineDependent->CallIO1(&io_operation_c);
                break;

            case POLY_SYS_fork_process:
                machineDependent->CallIO2(&fork_processc);
                break;

            case POLY_SYS_choice_process:
                machineDependent->CallIO2(&choice_processc);
                break;

            case POLY_SYS_int_process:
                machineDependent->CallIO1(&int_processc);
                break;

            case POLY_SYS_send_on_channel:
                machineDependent->CallIO2(&send_on_channelc);
                break;

            case POLY_SYS_receive_on_channel:
                machineDependent->CallIO1(&receive_on_channelc);
                break;

//            case POLY_SYS_offset_address: machineDependent->CallIO2(&offset_addressc); break;

            case POLY_SYS_shift_right_word:
                machineDependent->CallIO2(&shift_right_word_c);
                break;
    
            case POLY_SYS_word_neq:
                machineDependent->CallIO2(&word_neq_c);
                break;
    
            case POLY_SYS_not_bool:
                machineDependent->CallIO1(&not_bool_c);
                break;

            case POLY_SYS_string_length:
                machineDependent->CallIO1(&string_length_c);
                break;

            case POLY_SYS_int_eq:
                machineDependent->CallIO2(&equal_longc);
                break;

            case POLY_SYS_int_neq:
                machineDependent->CallIO2(&not_equal_longc);
                break;

            case POLY_SYS_int_geq:
                machineDependent->CallIO2(&ge_longc);
                break;

            case POLY_SYS_int_leq:
                machineDependent->CallIO2(&le_longc);
                break;

            case POLY_SYS_int_gtr:
                machineDependent->CallIO2(&gt_longc);
                break;

            case POLY_SYS_int_lss:
                machineDependent->CallIO2(&ls_longc);
                break;

            case POLY_SYS_string_sub:
                machineDependent->CallIO2(&string_subc);
                break;

            case POLY_SYS_or_word:
                machineDependent->CallIO2(&or_word_c);
                break;

            case POLY_SYS_and_word:
                machineDependent->CallIO2(&and_word_c);
                break;

            case POLY_SYS_xor_word:
                machineDependent->CallIO2(&xor_word_c);
                break;

            case POLY_SYS_shift_left_word:
                machineDependent->CallIO2(&shift_left_word_c);
                break;

            case POLY_SYS_word_eq:
                machineDependent->CallIO2(&word_eq_c);
                break;

            case POLY_SYS_load_byte:
                machineDependent->CallIO2(&load_byte_long_c);
                break;

            case POLY_SYS_load_word:
                machineDependent->CallIO2(&load_word_long_c);
                break;

    //        case POLY_SYS_is_big_endian: machineDependent->CallIO0(&is_big_endianc); break;
    //        case POLY_SYS_bytes_per_word: machineDependent->CallIO0(&bytes_per_wordc); break;

            case POLY_SYS_assign_byte:
                machineDependent->CallIO3(&assign_byte_long_c);
                break;

            case POLY_SYS_assign_word:
                machineDependent->CallIO3(&assign_word_long_c);
                break;

            // ObjSize and ShowSize are now in the poly_specific functions and
            // probably should be removed from here.
            case POLY_SYS_objsize:
                machineDependent->CallIO1(&ObjSize);
                break;

            case POLY_SYS_showsize:
                machineDependent->CallIO1(&ShowSize);
                break;

            case POLY_SYS_timing_dispatch:
                machineDependent->CallIO2(&timing_dispatch_c);
                break;

            case POLY_SYS_interrupt_console_processes:
                machineDependent->CallIO0(&interrupt_console_processes_c);
                break; 

            case POLY_SYS_install_subshells:
                machineDependent->CallIO1(&install_subshells_c);
                break;

            case POLY_SYS_XWindows:
                machineDependent->CallIO1(&XWindows_c);
                break;

            case POLY_SYS_full_gc:
                machineDependent->CallIO0(&full_gc_c);
                break;

            case POLY_SYS_stack_trace:
                machineDependent->CallIO0(& stack_trace_c);
                break;

            case POLY_SYS_foreign_dispatch:
                machineDependent->CallIO2(&foreign_dispatch_c);
                break;

            case POLY_SYS_callcode_tupled:
                machineDependent->CallCodeTupled();
                break;

            case POLY_SYS_process_env: machineDependent->CallIO2(&process_env_dispatch_c); break;

    //        case POLY_SYS_set_string_length: machineDependent->CallIO2(&set_string_length_c); break;

            case POLY_SYS_shrink_stack:
                machineDependent->CallIO1(&shrink_stack_c);
                break;

            case POLY_SYS_code_flags:
                machineDependent->CallIO2(&CodeSegmentFlags);
                break;

            case POLY_SYS_shift_right_arith_word:
                machineDependent->CallIO2(&shift_right_arith_word_c);
                break;

            case POLY_SYS_get_first_long_word:
            case POLY_SYS_int_to_word:
                // POLY_SYS_int_to_word has generally been replaced by POLY_SYS_get_first_long_word.
                // The reason is that POLY_SYS_int_to_word may be applied to either a long or
                // a short argument whereas POLY_SYS_get_first_long_word must be applied to a
                // long argument and can be implemented very easily in the code-generator, at
                // least on a little-endian machine.
                machineDependent->CallIO1(&int_to_word_c);
                break;

            case POLY_SYS_poly_specific:
                machineDependent->CallIO2(&poly_dispatch_c);
                break;

            case POLY_SYS_set_code_constant:
                machineDependent->CallIO4(&set_code_constant);
                break;

            case POLY_SYS_move_bytes:
                machineDependent->CallIO5(&move_bytes_long_c);
                break;

            case POLY_SYS_move_words:
                machineDependent->CallIO5(&move_words_long_c);
                break;

            case POLY_SYS_mul_word:
                machineDependent->CallIO2(&mul_word_c);
                break;

            case POLY_SYS_plus_word:
                machineDependent->CallIO2(&plus_word_c);
                break;

            case POLY_SYS_minus_word:
                machineDependent->CallIO2(&minus_word_c);
                break;

            case POLY_SYS_div_word:
                machineDependent->CallIO2(&div_word_c);
                break;

            case POLY_SYS_mod_word:
                machineDependent->CallIO2(&mod_word_c);
                break;

            case POLY_SYS_word_geq:
                machineDependent->CallIO2(&word_geq_c);
                break;

            case POLY_SYS_word_leq:
                machineDependent->CallIO2(&word_leq_c);
                break;

            case POLY_SYS_word_gtr:
                machineDependent->CallIO2(&word_gtr_c);
                break;

            case POLY_SYS_word_lss:
                machineDependent->CallIO2(&word_lss_c);
                break;

            case POLY_SYS_io_dispatch:
                machineDependent->CallIO3(&IO_dispatch_c);
                break;

            case POLY_SYS_network:
                machineDependent->CallIO2(&Net_dispatch_c);
                break;

            case POLY_SYS_os_specific:
                machineDependent->CallIO2(&OS_spec_dispatch_c);
                break;

            case POLY_SYS_signal_handler:
                machineDependent->CallIO2(&Sig_dispatch_c);
                break;

            case POLY_SYS_kill_self:
                machineDependent->CallIO0(kill_selfc);
                break;

            // This is called from assembly code and doesn't actually have an entry in the
            // io vector.
            case POLY_SYS_give_ex_trace:
                machineDependent->CallIO2(ex_tracec);
                break;

            default:
                Crash("Unknown io operation %d\n", ioFunction);
            }
#ifdef _WIN32_WCE
			memcpy(&exception_jump, &tbuf, sizeof(jmp_buf));
		}
		else
		{
			memcpy(&exception_jump, &tbuf, sizeof(jmp_buf));
#else
		}
		catch (IOException) {
#endif
        }

    }
}

class RunTime: public RtsModule
{
public:
    void GarbageCollect(ScanAddress *process);
    virtual void Interrupt(int sig);
};

// Declare this.  It will be automatically added to the table.
static RunTime runtimeModule;

void RunTime::GarbageCollect(ScanAddress *process)
{
    gSaveVec->gcScan(process);
}

void RunTime::Interrupt(int /*signum*/)
/* Called from execute_pending_interrupts some time after a signal such as
   SIGALRM, SIGINT etc. 
   If the signal was SIGALRM it selects the next process (a more
   subtle scheme would try to find the process which wanted to do IO). */
{
#ifdef WINDOWS_PC
    if (exitRequest) finish(exitCode);
#endif
    /* There is never any harm in switching processes. */
    if (processes->CurrentProcess() != NO_PROCESS)
        processes->select_next_process();
}
