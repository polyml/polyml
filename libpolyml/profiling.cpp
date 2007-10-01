/*
    Title:      Profiling
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000-7
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

#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
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
#include "locking.h"

/******************************************************************************/
/*                                                                            */
/*      PROFILING SHARED DATA                                                 */
/*                                                                            */
/******************************************************************************/
static POLYUNSIGNED gc_count1 = 0;
static POLYUNSIGNED gc_count2 = 0;
static POLYUNSIGNED gc_count3 = 0;
static POLYUNSIGNED total_count = 0;
static POLYUNSIGNED unknown_count  = 0;

ProfileMode profileMode;

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
void add_count(TaskData *taskData, POLYCODEPTR fpc, PolyWord *sp, int incr)
/* Adds incr to the profile count for the function */
/* pointed at by pc or by one of its callers.      */
{

    /* The first PC may be valid even if it's not a code pointer */
    bool is_code = true;
    PolyWord pc = PolyWord::FromCodePtr(fpc);
    StackObject *stack = taskData->stack;
    PolyWord *endStack = stack->Offset(stack->Length());
    
    total_count += incr;
    
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
                        // This should be a valid string which may be a single character.
                        ASSERT(name.IsTagged() || name.AsObjPtr()->IsByteObject());
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
        
        printf("\nTotal: %lu; Counted: %lu; Uncounted: %lu",
            total_count, P.total, total_count - P.total);
        
        total_count = 0;
        putc('\n', stdout);
    }
    
    /* flush profile before printing next user output SPF 30/9/94 */
    fflush (stdout); 
}

void handleProfileTrap(TaskData *taskData, SIGNALCONTEXT *context)
{
    /* If we are in the garbage-collector add the count to "gc_count"
        otherwise try to find out where we are. */
    switch (gc_phase)
    {
    case 0 : 
        {
            if (taskData)
            {
                PolyWord *sp;
                POLYCODEPTR pc;
                if (machineDependent->GetPCandSPFromContext(taskData, context, sp, pc))
                    add_count(taskData, pc, sp, 1);
            }
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

class ProfileRequest: public MainThreadRequest
{
public:
    ProfileRequest(unsigned prof): mode(prof) {}

    virtual void Perform();
    unsigned mode;
};

// To speed up testing whether profiling is off already we
// maintain a variable that contains the global state.
static unsigned profile_mode = 0;
static PLock profLock;

// Called from ML to control profiling.
Handle profilerc(TaskData *taskData, Handle mode_handle)
/* Profiler - generates statistical profiles of the code.
   The parameter is an integer which determines the value to be profiled.
   When profiler is called it always resets the profiling and prints out any
   values which have been accumulated.
   If the parameter is 0 this is all it does, 
   if the parameter is 1 then it produces time profiling,
   if the parameter is 2 it produces store profiling.
   3 - arbitrary precision emulation traps. */
{
    unsigned mode = get_C_ulong(taskData, DEREFWORDHANDLE(mode_handle));
    {
        PLocker locker(&profLock);
        if (mode == profile_mode) // No change in mode = no-op
            return taskData->saveVec.push(TAGGED(0));
    
        /* profiling 0 turns off profiling and prints results,
           but is ignored for profile_modes > 4. To get the
           accumulated results of these, set profiling to 4. */
        if (mode == 0 && profile_mode > PROF_ACCUMULATED)
            return taskData->saveVec.push(TAGGED(0));
        profile_mode = mode;
    }
    // All these actions are performed by the root thread.  Only profile
    // printing needs to be performed with all the threads stopped but it's
    // simpler to serialise all requests.
    ProfileRequest request(mode);
    processes->MakeRootRequest(taskData, &request);
    return taskData->saveVec.push(TAGGED(0));
}

// This is called from the root thread when all the ML threads have been paused.
void ProfileRequest::Perform()
{
    switch (mode & ~PROF_ACCUMULATED)
    {
    case kProfileOff:
        // Turn off old profiling mechanism and print out accumulated results 
        profileMode = kProfileOff;
        processes->StopProfiling();
        printprofile();
        break;
        
    case kProfileTime:
        profileMode = kProfileTime;
        processes->StartProfiling();
        break;
        
    case kProfileStoreAllocation:
        profileMode = kProfileStoreAllocation;
        break;
        
    case kProfileEmulation:
        profileMode = kProfileEmulation;
        break;
        
    default: /* do nothing */
        break;
    }
    
} /* profilerc */


class Profiling: public RtsModule
{
public:
    virtual void Init(void);
};

// Declare this.  It will be automatically added to the table.
static Profiling profileModule;

void Profiling::Init(void)
{
    // Reset profiling counts.
    profileMode = kProfileOff;
    gc_count1 = 0;
    gc_count2 = 0;
    gc_count3 = 0;
    total_count = 0;
    unknown_count  = 0;
}
