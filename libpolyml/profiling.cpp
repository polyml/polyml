/*
    Title:      Profiling
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

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
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


/************************************************************************
 *
 * Include runtime headers
 *
 ************************************************************************/

#include "globals.h"
#include "sighandler.h"
#include "arb.h"
#include "machine_dep.h"
#include "foreign.h"
#include "diagnostics.h"
#include "processes.h"
#include "polystring.h"
#include "profiling.h"
#include "save_vec.h"
#include "rts_module.h"
#include "gc.h" // For gc_phase
#include "memmgr.h"
#include "scanaddrs.h"

/************************************************************************
 *
 * Static variables used by Windows version
 *
 ************************************************************************/

#if defined(WINDOWS_PC)
#include "Console.h"

static HANDLE hStopEvent; /* Signalled to stop all threads. */
HANDLE profilingHd;
#endif

/******************************************************************************/
/*                                                                            */
/*      PROFILING SHARED DATA                                                 */
/*                                                                            */
/******************************************************************************/
static POLYUNSIGNED gc_count1 = 0;
static POLYUNSIGNED gc_count2 = 0;
static POLYUNSIGNED gc_count3 = 0;
static POLYUNSIGNED run_time_count = 0;
static POLYUNSIGNED total_count = 0;
static POLYUNSIGNED foreign_code_count = 0;
static POLYUNSIGNED unknown_count  = 0;
bool store_profiling   = 0; /* also updated by sparc_dep.c */
bool emulate_profiling = false; /* also updated by sparc_dep.c */

/* Profiling settings. */
#define PROF_OFF                0
#define PROF_TIMING             1
#define PROF_ALLOCATION         2
#define PROF_EMULATION          3
/* If Accumulated is added to one of the above profiling is only turned off
   by the next call with PROF_ACCUMULATED+PROF_OFF.  This allows us to profile
   the compiler. */
#define PROF_ACCUMULATED    4

/******************************************************************************/
/*                                                                            */
/*      PROFILING FUNCTIONS                                                   */
/*                                                                            */
/******************************************************************************/
typedef struct
{
    POLYUNSIGNED count;
    PolyWord functionName;
} PROFENTRY, *PPROFENTRY;

/* Initial vector size. */
#define INITSIZE 40

/* This structure is used to retain count information.
   Whenever counting is turned off we go through the store
   checking for non-zero counts and build a table of the count
   with the name of the function.  That is then sorted and printed. */
static struct
{
  POLYUNSIGNED   total;
  PPROFENTRY pTab;
  int size;
  int used;
} P;


/******************************************************************************/
/*                                                                            */
/*      add_count - utility function                                          */
/*                                                                            */
/******************************************************************************/
void add_count(POLYCODEPTR fpc, PolyWord *sp, int incr)
/* Adds incr to the profile count for the function */
/* pointed at by pc or by one of its callers.      */
{

    /* The first PC may be valid even if it's not a code pointer */
    bool is_code = true;
    PolyWord pc = PolyWord::FromCodePtr(fpc);
    PolyWord *endStack = poly_stack->Offset(poly_stack->Length());
    
#define INRANGE(val,start,end) ((start) <= (val) && (val) < (end))
    
    total_count += incr;
    
    if (machineDependent->InRunTimeSystem())
    {
        if (in_foreign_code)
           /* We can only be in foreign code if we are
              already in the runtime system */
            foreign_code_count += incr;
        else
            run_time_count += incr;
    }
    
    /* Now try to discover which Poly/ML function we're in. Note:
       if we're in the RTS, the pc value that we've been given may not
       be valid since the machine-dependent code for the SPARC
       and other platforms uses TAGGED(0) as a special marker.
       The correct fix is to change the machine-dependant
       code so that add_count only sees legal values, but for now I'm
       just going to implement the following work-around.
       SPF 2/8/96
    
       We also have to worry about those grubby bits of assembly code
       in the RTS that haven't set in_run_time_system (yet?) but which
       are definitely NOT well-formed ML code segments. On the PC,
       the pc could even have the right alignment to look like a
       code pointer. So what we do is:
     
       (1) Check whether the pc value looks like a code pointer.
       (2) Check whether it points into the heap/database
      
       If so, we have a genuine code segment; if not, we have some
       sort of bad value (TAGGED(0) or RTS code address).
       SPF 2/8/96
       
       We don't want to lose the function that actually called into the
       RTS, so I've added the new is_code flag to try to handle this.
       SPF 17/3/97
    */
    
    /* First try the pc value we have been given - if that fails search down
    the stack to see if there is a return address we can use. */
    for (;;)
    {
        /* Get the address of the code segment from the code pointer */
        if (pc.IsCodePtr() || is_code)
        {   
            /* The first PC gets special treatment, since it may not
               be correctly tagged as a code-pointer.  Problem - what
               happens if the first PC is an RTS address that "just happens"
               to look like a properly-tagged code-pointer? (This can happen
               on the i386 but not on the RISC machines.) Then OBJ_CODEPTR_TO_PTR
               can fail horribly! So we have to check that pc has a sensible value
               BEFORE we call this macro.
               SPF 17/11/1998 */
            is_code = false;
            
            /* N.B. we must NOT attempt to increment the profile counts for
               those pieces of code that live in the I/O area, because
               they don't have them! Similarly, we must NOT test their
               flags - they don't have these either. */
            MemSpace *space = gMem.SpaceForAddress(pc.AsAddress());
            if (space != 0)
            {
                PolyObject *ptr = ObjCodePtrToPtr(pc.AsCodePtr());
                ASSERT(ptr->IsCodeObject());
                PolyWord *consts = ptr->ConstPtrForCode();
                if (consts[0] != TAGGED(0)) /* Anonymous segment - try again */
                {
                    ((POLYUNSIGNED*)consts)[-1] += incr;
                    return;
                }
            }
            /* else just fall through and try next candidate address */
        } /* OBJ_IS_CODEPTR(pc) */
        
        /* Find next candidate address */
        if (sp < endStack)
            pc = *sp++;
        else /* Reached bottom of stack without finding valid code pointer */
        {
            unknown_count += incr;
            return;
        }
    } /* loop "forever" */
    /*NOTREACHED*/
#undef INRANGE
}


/* newProfileEntry - return the address of an entry in the profile table,
   extending it if necessary. */
static PPROFENTRY newProfileEntry(void)
{
    if (P.used == P.size)
    {
        if (P.size == 0) P.size = INITSIZE;
        else P.size += P.size/2;
        P.pTab = (PPROFENTRY)realloc(P.pTab, P.size*sizeof(PROFENTRY));
    }
    return &P.pTab[P.used++];
}

/* qsortTab - sort the profiling results by count.  It could use the
   name as a secondary sort but that's probably unnecessary.  I'm not
   sure that Quicksort is the best way of sorting the table since
   the values tend to be very unevenly spread, with a few large
   values and large numbers of very small values.  */
static void qsortTab(int nStart, int nEnd)
{
    int startPos, endPos;
    if (nEnd - nStart <= 1) return;
    if (nEnd - nStart == 2)
    {
        if (P.pTab[nStart].count > P.pTab[nStart+1].count)
        {
            /* Swap entries. */
            PROFENTRY saved = P.pTab[nStart];
            P.pTab[nStart] = P.pTab[nStart+1];
            P.pTab[nStart+1] = saved;
        }
        return;
    }
    /* Three or more entries. */
    startPos = nStart;
    endPos = nEnd-1;
    PROFENTRY median = P.pTab[startPos]; /* Use the first entry as the median. */
    while (1)
    {
        while (endPos > startPos && P.pTab[endPos].count > median.count) endPos--;
        if (endPos == startPos) break;
        P.pTab[startPos++] = P.pTab[endPos];
        if (endPos == startPos) break;
        while (endPos > startPos && P.pTab[startPos].count <= median.count) startPos++;
        if (endPos == startPos) break;
        P.pTab[endPos--] = P.pTab[startPos];
        if (endPos == startPos) break;
    }
    P.pTab[startPos] = median;
    qsortTab(nStart, startPos);
    qsortTab(startPos+1, nEnd);
}

/* writeProfileResults - print out the saved results. */
static void writeProfileResults(void)
{
    if (P.used != 0)
    {
        int i;
        /* Sort the table.  */
        qsortTab(0, P.used);
        /* Print it out. */
        for (i = 0; i < P.used; i++)
        {
            PPROFENTRY pEnt = &P.pTab[i];
            fprintf(stdout, "%10lu ", pEnt->count);
            print_string(pEnt->functionName);
            putc ('\n', stdout);
        }
    }
    free(P.pTab);
    P.pTab = 0;
    P.size = 0;
    P.used = 0;
}

// We don't use ScanAddress here because we're only interested in the
// objects themselves not the addresses in them.
static void PrintProfileCounts(PolyWord *bottom, PolyWord *top)
{
    PolyWord *ptr = bottom;
    
    while (ptr < top)
    {
        ptr++; // Skip the length word
        PolyObject *obj = (PolyObject*)ptr;
        if (obj->ContainsForwardingPtr())
        {
            // It's a forwarding pointer - get the length from the new location
            while (obj->ContainsForwardingPtr())
                obj = obj->GetForwardingPtr();
            ASSERT(obj->ContainsNormalLengthWord());
            ptr += obj->Length();
        }
        else
        {
            ASSERT(obj->ContainsNormalLengthWord());
            
            if (obj->IsCodeObject())
            {
                PolyWord *firstConstant = obj->ConstPtrForCode();
                PolyWord name = firstConstant[0];
                // The word before is an untagged count
                POLYUNSIGNED    count = firstConstant[-1].AsUnsigned();
                
                if (count != 0)
                {
                    if (name != TAGGED(0))
                    {
                        PPROFENTRY pEnt = newProfileEntry();
                        pEnt->count = count;
                        pEnt->functionName = name;
                    }
                    
                    firstConstant[-1] = PolyWord::FromUnsigned(0);
                    P.total += count;
                } /* count != 0 */
            } /* code object */
            ptr += obj->Length();
        } /* else */
    } /* while */
}

/* This type is coercible to PolyString *. We can use static "pstrings"
   since we are not going to garbage collect while we are printing out
   the profiling information. */
static struct {
    POLYUNSIGNED length; char chars[40];
} psMarkPhase, psCopyPhase, psUpdatePhase, psGCTotal, psUnknown;

/******************************************************************************/
/*                                                                            */
/*      printprofile - utility function - doesn't allocate                    */
/*                                                                            */
/******************************************************************************/
static void printprofile(void)
/* Print profiling information and reset profile counts.    */
/* Profile counts are also reset by commit so that objects  */
/* written to the persistent store always have zero counts. */
{
    PPROFENTRY pEnt;
    /* flush old user output before printing profile SPF 30/9/94 */
    fflush (stdout); 
    
    P.total = 0;
    P.used = 0;

    unsigned j;
    for (j = 0; j < gMem.npSpaces; j++)
    {
        MemSpace *space = gMem.pSpaces[j];
        // Permanent areas are filled with objects from the bottom.
        PrintProfileCounts(space->bottom, space->top); // Bottom to top
    }
    for (j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[j];
        // Local areas only have objects from the allocation pointer to the top.
        PrintProfileCounts(space->pointer, space->top);
    }
    
    if (gc_count1 || gc_count2 || gc_count3)
    {
        int gc_count = gc_count1 + gc_count2 + gc_count3;
        P.total     += gc_count;
        total_count += gc_count;
        
        pEnt = newProfileEntry();
        strcpy(psMarkPhase.chars, "GARBAGE COLLECTION (mark phase)");
        psMarkPhase.length = strlen(psMarkPhase.chars);
        pEnt->count = gc_count1;
        pEnt->functionName = PolyWord::FromUnsigned((POLYUNSIGNED)&psMarkPhase);
        
        pEnt = newProfileEntry();
        strcpy(psCopyPhase.chars, "GARBAGE COLLECTION (copy phase)");
        psCopyPhase.length = strlen(psCopyPhase.chars);
        pEnt->count = gc_count2;
        pEnt->functionName = PolyWord::FromUnsigned((POLYUNSIGNED)&psCopyPhase);
        
        pEnt = newProfileEntry();
        strcpy(psUpdatePhase.chars, "GARBAGE COLLECTION (update phase)");
        psUpdatePhase.length = strlen(psUpdatePhase.chars);
        pEnt->count = gc_count3;
        pEnt->functionName = PolyWord::FromUnsigned((POLYUNSIGNED)&psUpdatePhase);
        
        pEnt = newProfileEntry();
        strcpy(psGCTotal.chars, "GARBAGE COLLECTION (total)");
        psGCTotal.length = strlen(psGCTotal.chars);
        pEnt->count = gc_count;
        pEnt->functionName = PolyWord::FromUnsigned((POLYUNSIGNED)&psGCTotal);
        
        gc_count1 = 0;
        gc_count2 = 0;
        gc_count3 = 0;
    }
    
    if (unknown_count)
    {
        P.total += unknown_count;
        pEnt = newProfileEntry();
        strcpy(psUnknown.chars, "UNKNOWN");
        psUnknown.length = strlen(psUnknown.chars);
        pEnt->count = unknown_count;
        pEnt->functionName = PolyWord::FromUnsigned((POLYUNSIGNED)&psUnknown);
        unknown_count = 0;
    }
    
    writeProfileResults();
    
    if (total_count)
    {
        
        printf("\nTotal: %lu (RTS: %lu); Counted: %lu; Uncounted: %lu",
            total_count, run_time_count, P.total, total_count - P.total);
        
        total_count = 0;
        run_time_count = 0;
        
        /* Only print out the foreign_code_count if it is greater than zero */
        if (foreign_code_count)
        {
            printf(" (Foreign code %lu)", foreign_code_count);
            foreign_code_count = 0;
        }
        putc('\n', stdout);
    }
    
    /* flush profile before printing next user output SPF 30/9/94 */
    fflush (stdout); 
}

static void handleProfileTrap(SIGNALCONTEXT *context)
{
    /* If we are in the garbage-collector add the count to "gc_count"
        otherwise try to find out where we are. */
    switch (gc_phase)
    {
    case 0 : 
        {
            PolyWord *sp;
            POLYCODEPTR pc;
            if (machineDependent->GetPCandSPFromContext(context, sp, pc))
                add_count(pc, sp, 1);
        }
        break;
        
    case 1 :
        gc_count1++;
        break;
        
    case 2 :
        gc_count2++;
        break;
        
    case 3 :
        gc_count3++;
        break;
        
    default :
        unknown_count++;
        break;
    }
}

/******************************************************************************/
/*                                                                            */
/*      catchVTALRM - handler for alarm-clock signal                          */
/*                                                                            */
/******************************************************************************/
#if !defined(WINDOWS_PC)
static void catchVTALRM(SIG_HANDLER_ARGS(sig, context))
{
    ASSERT(sig == SIGVTALRM);
    handleProfileTrap((SIGNALCONTEXT*)context);

} /* catchVTALRM for UNIX  */

#else /* PC */
/* This function is running on a separate OS thread.
   Every 20msec of its own virtual time it updates the count.
   Unfortunately on Windows there is no way to set a timer in 
   the virtual time of PolyML (not real time, CPU time used by PolyML).
   It would be possible to approximate this in Windows NT by looking
   at the CPU time used in the last real time slice and adjusting the
   count appropriately.  That won't work in Windows 95 which doesn't
   have a CPU time counter.
*/
DWORD WINAPI ProfilingTimer(LPVOID parm)
{               
    // Wait for 20ms or until the stop event is signalled.
    while (WaitForSingleObject(hStopEvent, 20) == WAIT_TIMEOUT)        
    {
        CONTEXT context;
        SuspendThread(hMainThread);
        context.ContextFlags = CONTEXT_CONTROL; /* Get Eip and Esp */
        if (GetThreadContext(hMainThread, &context))
            handleProfileTrap(&context);
        else
            total_count++;
        ResumeThread(hMainThread);
    }
    return 0;
}
#endif


/******************************************************************************/
/*                                                                            */
/*      stop_profiling - utility function - doesn't allocate                  */
/*                                                                            */
/******************************************************************************/
#if defined(WINDOWS_PC) /* PC version */
void stop_profiling(int print)
{
    if (hStopEvent) SetEvent(hStopEvent);
    // Wait for the thread to stop
    if (profilingHd) WaitForSingleObject(profilingHd, 10000);
    CloseHandle(profilingHd);
    profilingHd = NULL;

    store_profiling   = 0;
    emulate_profiling = false;
    if (print) printprofile();
}

#else /* UNIX version */
void stop_profiling(int print)
{
    static struct itimerval stoptime = {{0, 0}, {0, 0}};

    /* Stop the timer */
    setitimer(ITIMER_VIRTUAL, & stoptime, NULL);

    /* Switch off profiling. */
    store_profiling   = 0;
    emulate_profiling = false;

    if (print) printprofile();
}
#endif


/******************************************************************************/
/*                                                                            */
/*      start_timer - utility function - doesn't allocate                     */
/*                                                                            */
/******************************************************************************/
#if defined(WINDOWS_PC) /* PC version */
static void start_timer(void)
{
    DWORD threadId;
    if (profilingHd)
        return;
    ResetEvent(hStopEvent);
    profilingHd = CreateThread(NULL, 0, ProfilingTimer, NULL, 0, &threadId);
    if (profilingHd == NULL)
        fputs("Creating ProfilingTimer thread failed.\n", stdout); 
    /* Give this a higher than normal priority so it pre-empts the main
       thread.  Without this it will tend only to be run when the main
       thread blocks for some reason. */
    SetThreadPriority(profilingHd, THREAD_PRIORITY_ABOVE_NORMAL);
}
#else /* UNIX version */
static void start_timer(void)
{
    setSignalHandler(SIGVTALRM, catchVTALRM);
    /* set virtual timer to go off every 
       20000 microseconds = 20 msec ~ 50 Hz */
    static struct itimerval starttime = {{0,20000}, {0,20000}};
    starttime.it_interval.tv_sec = starttime.it_value.tv_sec = 0;
    starttime.it_interval.tv_usec = starttime.it_value.tv_usec = 20000;
    setitimer(ITIMER_VIRTUAL,&starttime,NULL);
}
#endif

/******************************************************************************/
/*                                                                            */
/*      profilerc - called from assembly.s                                    */
/*                                                                            */
/******************************************************************************/

/* CALL_IO1(profiler, NOIND, NOIND) */
Handle profilerc(Handle mode_handle)    /* Generate profile information */
/* Profiler - generates statistical profiles of the code.
   The parameter is an integer which determines the value to be profiled.
   When profiler is called it always resets the profiling and prints out any
   values which have been accumulated.
   If the parameter is 0 this is all it does, 
   if the parameter is 1 then it produces time profiling,
   if the parameter is 2 it produces store profiling.
   3 - arbitrary precision emulation traps. */
{
  static unsigned profile_mode = 0;
  POLYUNSIGNED mode = get_C_ulong(DEREFWORDHANDLE(mode_handle));
  
  /* No change in mode = no-op */
  if (mode == profile_mode) return gSaveVec->push(TAGGED(0));
  
  /* profiling 0 turns off profiling and prints results,
     but is ignored for profile_modes > 4. To get the
     accumulated results of these, set profiling to 4. */
  if (mode == 0 && profile_mode > PROF_ACCUMULATED) return gSaveVec->push(TAGGED(0));
  
  switch (mode & ~PROF_ACCUMULATED)
  {
    case PROF_OFF:
      /* Turn off old profiling mechanism and print out accumulated results */
      stop_profiling(1);
      break;
  
    case PROF_TIMING:
      start_timer();
      break;
    
    case PROF_ALLOCATION:
      store_profiling = 1;
      break;
    
    case PROF_EMULATION:
      emulate_profiling = true;
      break;
    
    default: /* do nothing */
      break;
  }
  
  profile_mode = mode;
  
  return gSaveVec->push(TAGGED(0));
} /* profilerc */


class Profiling: public RtsModule
{
public:
    virtual void Init(void);
    virtual void Uninit(void);
};

// Declare this.  It will be automatically added to the table.
static Profiling profileModule;

void Profiling::Init(void)
{
    // Reset profiling counts.
    store_profiling = 0;
    emulate_profiling = false;
    gc_count1 = 0;
    gc_count2 = 0;
    gc_count3 = 0;
    run_time_count = 0;
    total_count = 0;
    foreign_code_count = 0;
    unknown_count  = 0;

#if defined(WINDOWS_PC) /* PC version */
    // Create stop event for time profiling.
    hStopEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
#endif
    return;
}

/* Release all resources.
   This is really only needed with Windows when running as a DLL within
   the address space of another process. */
void Profiling::Uninit(void)
{
#if defined(WINDOWS_PC)
    /* Stop the timer and profiling threads. */
    if (hStopEvent) SetEvent(hStopEvent);
    if (profilingHd)
    {
        WaitForSingleObject(profilingHd, 10000);
        CloseHandle(profilingHd);
        profilingHd = NULL;
    }
    if (hStopEvent) CloseHandle(hStopEvent);
    hStopEvent = NULL;
#else
    // Make sure the timer is not running
    struct itimerval stoptime;
    memset(&stoptime, 0, sizeof(stoptime));
    setitimer(ITIMER_VIRTUAL, &stoptime, NULL);
#endif
}
