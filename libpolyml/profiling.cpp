/*
    Title:      Profiling
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited
    Further development copyright (c) David C.J. Matthews 2011

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
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
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
#include "memmgr.h"
#include "scanaddrs.h"
#include "locking.h"

/******************************************************************************/
/*                                                                            */
/*      PROFILING SHARED DATA                                                 */
/*                                                                            */
/******************************************************************************/
static POLYUNSIGNED mainThreadCounts[MTP_MAXENTRY];
static const char* mainThreadText[MTP_MAXENTRY] =
{
    "UNKNOWN",
    "GARBAGE COLLECTION (sharing phase)",
    "GARBAGE COLLECTION (mark phase)",
    "GARBAGE COLLECTION (copy phase)",
    "GARBAGE COLLECTION (update phase)",
    "GARBAGE COLLECTION (minor collection)",
    "Common data sharing",
    "Exporting",
    "Saving state",
    "Loading saved state",
    "Profiling",
    "Setting signal handler",
    "Cygwin spawn"
};

// Entries for store profiling
enum _extraStore {
    EST_CODE = 0,
    EST_STRING,
    EST_BYTE,
    EST_WORD,
    EST_MUTABLE,
    EST_MUTABLEBYTE,
    EST_MAX_ENTRY
};

static POLYUNSIGNED extraStoreCounts[EST_MAX_ENTRY];
static const char * extraStoreText[EST_MAX_ENTRY] =
{
    "Function code",
    "Strings",
    "Byte data (long precision ints etc)",
    "Unidentified word data",
    "Unidentified mutable data",
    "Mutable byte data (profiling counts)"
};

static POLYUNSIGNED total_count = 0;

/* This type is coercible to PolyString *. We can use static "pstrings"
   since we are not going to garbage collect while we are printing out
   the profiling information. */
static struct {
    POLYUNSIGNED length; char chars[40];
} psStrings[MTP_MAXENTRY+EST_MAX_ENTRY], psGCTotal;

ProfileMode profileMode;

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

// Lock to serialise updates of counts. Only used during update.
// Not required when we print the counts since there's only one thread
// running then.
static PLock countLock;

// Get the profile object associated with a piece of code.  Returns null if
// there isn't one, in particular if this is in the old format.
static PolyObject *getProfileObjectForCode(PolyObject *code)
{
    ASSERT(code->IsCodeObject());
    PolyWord *consts;
    POLYUNSIGNED constCount;
    code->GetConstSegmentForCode(consts, constCount);
    if (constCount < 3 || ! consts[2].IsDataPtr()) return 0;
    PolyObject *profObject = consts[2].AsObjPtr();
    if (profObject->IsMutable() && profObject->IsByteObject() && profObject->Length() == 1)
        return profObject;
    else return 0;
}

// Adds incr to the profile count for the function pointed at by
// pc or by one of its callers.
// This is called from a signal handler in the case of time profiling.
void add_count(TaskData *taskData, POLYCODEPTR fpc, PolyWord *sp, POLYUNSIGNED incr)
{

    /* The first PC may be valid even if it's not a code pointer */
    bool is_code = true;
    PolyWord pc = PolyWord::FromCodePtr(fpc);
    PolyWord *endStack = taskData->stack->top;
    
    /* First try the pc value we have been given - if that fails search down
       the stack to see if there is a return address we can use. */
    for (;;)
    {
        /* Get the address of the code segment from the code pointer */
        if (pc.IsCodePtr() || is_code)
        {   
            is_code = false;
            
            // Check that the pc value is within the heap.  It could be
            // in the assembly code.
            MemSpace *space = gMem.SpaceForAddress(pc.AsAddress());
            if (space != 0)
            {
                PolyObject *profObject = getProfileObjectForCode(ObjCodePtrToPtr(pc.AsCodePtr()));
                PLocker locker(&countLock);
                if (profObject)
                    profObject->Set(0, PolyWord::FromUnsigned(profObject->Get(0).AsUnsigned() + incr));
                total_count += incr;
                return;
            }
            /* else just fall through and try next candidate address */
        } /* OBJ_IS_CODEPTR(pc) */
        
        /* Find next candidate address */
        if (sp < endStack)
            pc = *sp++;
        else /* Reached bottom of stack without finding valid code pointer */
        {
            PLocker locker(&countLock);
            mainThreadCounts[MTP_USER_CODE] += incr;
            total_count += incr;
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
            fprintf(stdout, "%10" POLYUFMT " ", pEnt->count);
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
                PolyObject *profCount = getProfileObjectForCode(obj);
                if (profCount)
                {
                    POLYUNSIGNED count = profCount->Get(0).AsUnsigned();
                
                    if (count != 0)
                    {
                        if (name != TAGGED(0))
                        {
                            PPROFENTRY pEnt = newProfileEntry();
                            pEnt->count = count;
                            pEnt->functionName = name;
                        }
                    
                        profCount->Set(0, PolyWord::FromUnsigned(0));
                        P.total += count;
                    }
                }
            } /* code object */
            ptr += obj->Length();
        } /* else */
    } /* while */
}

void printprofile(void)
/* Print profiling information and reset profile counts.    */
/* Profile counts are also reset by commit so that objects  */
/* written to the persistent store always have zero counts. */
{
    PPROFENTRY pEnt;
    /* flush old user output before printing profile SPF 30/9/94 */
    fflush (stdout); 
    
    P.total = 0;
    P.used = 0;

    if (total_count != 0)
    {
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
            PrintProfileCounts(space->bottom, space->lowerAllocPtr);
            PrintProfileCounts(space->upperAllocPtr, space->top);
        }
    } // else if we haven't actually had an interrupt avoid expensive scan of memory.

    {
        POLYUNSIGNED gc_count =
            mainThreadCounts[MTP_GCPHASESHARING]+
            mainThreadCounts[MTP_GCPHASEMARK]+
            mainThreadCounts[MTP_GCPHASECOMPACT] +
            mainThreadCounts[MTP_GCPHASEUPDATE] +
            mainThreadCounts[MTP_GCQUICK];
        if (gc_count)
        {
            pEnt = newProfileEntry();
            strcpy(psGCTotal.chars, "GARBAGE COLLECTION (total)");
            psGCTotal.length = strlen(psGCTotal.chars);
            pEnt->count = gc_count;
            pEnt->functionName = PolyWord::FromUnsigned((POLYUNSIGNED)&psGCTotal);
        }
    }

    for (unsigned k = 0; k < MTP_MAXENTRY; k++)
    {
        if (mainThreadCounts[k])
        {
            P.total     += mainThreadCounts[k];
            total_count += mainThreadCounts[k];
            pEnt = newProfileEntry();
            strcpy(psStrings[k].chars, mainThreadText[k]);
            psStrings[k].length = strlen(psStrings[k].chars);
            pEnt->count = mainThreadCounts[k];
            pEnt->functionName = PolyWord::FromUnsigned((POLYUNSIGNED)&psStrings[k]);
            mainThreadCounts[k] = 0;
        }
    }

    for (unsigned l = 0; l < EST_MAX_ENTRY; l++)
    {
        if (extraStoreCounts[l])
        {
            P.total     += extraStoreCounts[l];
            total_count += extraStoreCounts[l];
            pEnt = newProfileEntry();
            strcpy(psStrings[MTP_MAXENTRY+l].chars, extraStoreText[l]);
            psStrings[MTP_MAXENTRY+l].length = strlen(psStrings[MTP_MAXENTRY+l].chars);
            pEnt->count = extraStoreCounts[l];
            pEnt->functionName = PolyWord::FromUnsigned((POLYUNSIGNED)&psStrings[MTP_MAXENTRY+l]);
            extraStoreCounts[l] = 0;
        }
    }

    writeProfileResults();
    
    if (total_count)
    {
        
        printf("\nTotal: %" POLYUFMT "; Counted: %" POLYUFMT "; Uncounted: %" POLYUFMT,
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
    if (mainThreadPhase == MTP_USER_CODE)
    {
        if (taskData)
        {
            PolyWord *sp;
            POLYCODEPTR pc;
            if (machineDependent->GetPCandSPFromContext(taskData, context, sp, pc))
                add_count(taskData, pc, sp, 1);
            else mainThreadCounts[MTP_USER_CODE]++;
        }
        else mainThreadCounts[MTP_USER_CODE]++;
        // On Mac OS X all virtual timer interrupts seem to be directed to the root thread
        // so all the counts will be "unknown".
    }
    else mainThreadCounts[mainThreadPhase]++;
}

// Called from the GC when allocation profiling is on.
void AddObjectProfile(PolyObject *obj)
{
    ASSERT(obj->ContainsNormalLengthWord());
    POLYUNSIGNED length = obj->Length();

    if (obj->IsWordObject() && OBJ_HAS_PROFILE(obj->LengthWord()))
    {
        // It has a profile pointer.  The last word should point to the
        // closure or code of the allocating function.  Add the size of this to the count.
        ASSERT(length != 0);
        PolyWord profWord = obj->Get(length-1);
        ASSERT(profWord.IsDataPtr());
        PolyObject *profObject = profWord.AsObjPtr();
        ASSERT(profObject->IsMutable() && profObject->IsByteObject() && profObject->Length() == 1);
        profObject->Set(0, PolyWord::FromUnsigned(profObject->Get(0).AsUnsigned() + length + 1));
        total_count += length+1;
    }
    // If it doesn't have a profile pointer add it to the appropriate count.
    else if (obj->IsMutable())
    {
        if (obj->IsByteObject())
            extraStoreCounts[EST_MUTABLEBYTE] += length+1;
        else extraStoreCounts[EST_MUTABLE] += length+1;
    }
    else if (obj->IsCodeObject())
        extraStoreCounts[EST_CODE] += length+1;
    else if (obj->IsByteObject())
    {
        // Try to separate strings from other byte data.  This is only
        // approximate.
        if (OBJ_IS_NEGATIVE(obj->LengthWord()))
            extraStoreCounts[EST_BYTE] += length+1;
        else
        {
            PolyStringObject *possString = (PolyStringObject*)obj;
            POLYUNSIGNED bytes = length * sizeof(PolyWord);
            // If the length of the string as given in the first word is sufficient
            // to fit in the exact number of words then it's probably a string.
            if (length >= 2 &&
                possString->length <= bytes - sizeof(POLYUNSIGNED) &&
                possString->length > bytes - 2 * sizeof(POLYUNSIGNED))
                    extraStoreCounts[EST_STRING] += length+1;
            else
            {
                extraStoreCounts[EST_BYTE] += length+1;
            }
        }
    }
    else extraStoreCounts[EST_WORD] += length+1;
}


class ProfileRequest: public MainThreadRequest
{
public:
    ProfileRequest(unsigned prof): MainThreadRequest(MTP_PROFILING), mode(prof) {}

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
    unsigned mode = get_C_unsigned(taskData, DEREFWORDHANDLE(mode_handle));
    {
        PLocker locker(&profLock);
        if (mode == profile_mode) // No change in mode = no-op
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
    switch (mode)
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

    case kProfileLiveData:
        profileMode = kProfileLiveData;
        break;
 
    case kProfileLiveMutables:
        profileMode = kProfileLiveMutables;
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
    for (unsigned k = 0; k < MTP_MAXENTRY; k++) mainThreadCounts[k] = 0;
    total_count = 0;
}
