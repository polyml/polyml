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
/* This file was originally part of run_time.c */

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

#if defined(WINDOWS_PC)

#include <assert.h>
#include <windows.h>  
#else
/* UNIX version */

#include <time.h>
#include <sys/param.h>
#include <sys/time.h>
#include <signal.h>

#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

#include <stdlib.h>
#include <string.h>

#include <limits.h>
#include <assert.h>
#endif


/************************************************************************
 *
 * Include runtime headers
 *
 ************************************************************************/

#include "proper_io.h"

#include "globals.h"
#include "memory.h"
#include "arb.h"
#include "machine_dep.h"
#include "foreign.h"
#include "diagnostics.h"
#include "processes.h"
#include "run_time.h"
#include "profiling.h"
#include "sighandler.h"

/************************************************************************
 *
 * Static variables used by Windows version
 *
 ************************************************************************/

#if defined(WINDOWS_PC)
static HANDLE hStopEvent; /* Signalled to stop all threads. */
HANDLE profilingHd;
#endif

/******************************************************************************/
/*                                                                            */
/*      PROFILING SHARED DATA                                                 */
/*                                                                            */
/******************************************************************************/
static int gc_count1 = 0;
static int gc_count2 = 0;
static int gc_count3 = 0;
static int run_time_count = 0;
static int total_count = 0;
static int foreign_code_count = 0;
static int unknown_count  = 0;
int store_profiling   = 0; /* also updated by sparc_dep.c */
int emulate_profiling = 0; /* also updated by sparc_dep.c */

/* Profiling settings. */
#define PROF_OFF                0
#define	PROF_TIMING             1
#define	PROF_ALLOCATION         2
#define	PROF_EMULATION          3
/* If Accumulated is added to one of the above profiling is only turned off
   by the next call with PROF_ACCUMULATED+PROF_OFF.  This allows us to profile
   the compiler. */
#define PROF_ACCUMULATED	4

/******************************************************************************/
/*                                                                            */
/*      PROFILING FUNCTIONS                                                   */
/*                                                                            */
/******************************************************************************/
typedef struct
{
	int count;
	pstring functionName;
} PROFENTRY, *PPROFENTRY;

/* Initial vector size. */
#define INITSIZE 40

/* This structure is used to retain count information.
   Whenever counting is turned off we go through the store
   checking for non-zero counts and build a table of the count
   with the name of the function.  That is then sorted and printed. */
static struct
{
  int   total;
  PPROFENTRY pTab;
  int size;
  int used;
} P;


/******************************************************************************/
/*                                                                            */
/*      add_count - utility function                                          */
/*                                                                            */
/******************************************************************************/
void add_count(byte *pc, word *sp, int incr)
/* Adds incr to the profile count for the function */
/* pointed at by pc or by one of its callers.      */
{

  /* The first PC may be valid even if it's not a code pointer */
  int is_code = 1;

#define INRANGE(val,start,end) ((unsigned)(start) <= (unsigned)(val) && (unsigned)(val) < (unsigned)(end))
#define IS_MLPOINTER(p) \
    (INRANGE((p),A.I.pointer,A.I.top) || \
     INRANGE((p),A.M.pointer,A.M.top) || \
     IsDataPointer(H, (word *)(p)))

  total_count += incr;

  if (in_run_time_system)
  {
    if (in_foreign_code)
    {
      /* We can only be in foreign code if we are
         already in the runtime system */
      foreign_code_count += incr;
    }
    else
    {
      run_time_count += incr;
    }
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
    if (OBJ_IS_CODEPTR(pc) || is_code)
    {   
      /* The first PC gets special treatment, since it may not
         be correctly tagged as a code-pointer.  Problem - what
         happens if the first PC is an RTS address that "just happens"
         to look like a properly-tagged code-pointer? (This can happen
         on the i386 but not on the RISC machines.) Then OBJ_CODEPTR_TO_PTR
         can fail horribly! So we have to check that pc has a sensible value
         BEFORE we call this macro.
         SPF 17/11/1998
      */
      is_code = 0;

      /* N.B. we must NOT attempt to increment the profile counts for
         those pieces of code that live in the I/O area, because
         they don't have them! Similarly, we must NOT test their
         flags - they don't have these either.
       */
  
      if (INRANGE(pc,A.I.pointer,A.I.top) 
       || INRANGE(pc,A.M.pointer,A.M.top))
      {
        word *ptr;
        OBJ_CODEPTR_TO_PTR(pc, ptr);
        {
           word *consts;
        
           /* Check that we have a genuine code segement */
           word L = ptr[-1];
           if (! OBJ_IS_CODE_OBJECT(L))
           {
              crash ("bad code object: pc = %08x, ptr = %08x, flags = %08x\n",
                      pc, ptr, L);
           }
        
           OBJ_CODEPTR_TO_CONSTS_PTR(pc, consts);
           if (consts[3] != TAGGED(0)) /* Anonymous segment - try again */
           {
             consts[2] += incr;
             return;
           }
	}
      }
      else if (IsDataPointer (H,(word *)pc)) /* Hacky! */
      {
        word *ptr;
        OBJ_CODEPTR_TO_PTR(pc, ptr);
        {
           word *consts;
        
           /* Check that we have a genuine code segement */
           word L = ptr[-1];
           if (! OBJ_IS_CODE_OBJECT(L))
           {
              crash ("bad code object: pc = %08x, ptr = %08x, flags = %08x\n",
                      pc, ptr, L);
           }
        
           OBJ_CODEPTR_TO_CONSTS_PTR(pc, consts);
           if (consts[3] != TAGGED(0)) /* Anonymous segment - try again */
           {
             IncrementProfileCount (H,&(consts[2]),incr,&unknown_count);
             return;
           }
        }
      }
      /* else just fall through and try next candidate address */
    } /* OBJ_IS_CODEPTR(pc) */

    /* Find next candidate address */
    if (sp < end_of_stack)
    {
       pc = (byte *) *sp++;
    }
    else /* Reached bottom of stack without finding valid code pointer */
    {
      unknown_count += incr;
      return;
    }
  } /* loop "forever" */
  /*NOTREACHED*/
#undef INRANGE
#undef IS_MLPOINTER
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
	PROFENTRY saved;
	if (nEnd - nStart <= 1) return;
	if (nEnd - nStart == 2)
	{
		if (P.pTab[nStart].count > P.pTab[nStart+1].count)
		{
			/* Swap entries. */
			saved = P.pTab[nStart];
			P.pTab[nStart] = P.pTab[nStart+1];
			P.pTab[nStart+1] = saved;
		}
		return;
	}
	/* Three or more entries. */
	startPos = nStart;
	endPos = nEnd-1;
	saved = P.pTab[startPos]; /* Use the first entry as the median. */
	while (1)
	{
		while (endPos > startPos && P.pTab[endPos].count > saved.count) endPos--;
		if (endPos == startPos) break;
		P.pTab[startPos++] = P.pTab[endPos];
		if (endPos == startPos) break;
		while (endPos > startPos && P.pTab[startPos].count <= saved.count) startPos++;
		if (endPos == startPos) break;
		P.pTab[endPos--] = P.pTab[startPos];
		if (endPos == startPos) break;
	}
	P.pTab[startPos] = saved;
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
            proper_fprintf(stdout, "%10d ", pEnt->count);
            print_string(pEnt->functionName);
            proper_putc ('\n', stdout);
		}
	}
	free(P.pTab);
	P.pTab = 0;
	P.size = 0;
	P.used = 0;
}


/******************************************************************************/
/*                                                                            */
/*      PrintLocalProfileCounts - utility function - doesn't allocate         */
/*                                                                            */
/******************************************************************************/
static void PrintLocalProfileCounts(word *bottom, word *top)
{
  word *ptr = bottom;
  
  while (ptr < top)
  {
    word length = *ptr++;
    
    if (OBJ_IS_POINTER(length))
    {
      word *addr = OBJ_GET_POINTER(length);
      ptr += OBJECT_LENGTH(addr);
    }
    else
    {
      assert(OBJ_IS_LENGTH(length));
      
      ptr += OBJ_OBJECT_LENGTH(length);
      
      if (OBJ_IS_CODE_OBJECT(length))
      {
        word   *addr  = ptr - ptr[-1] - 1;
        word    count = addr[-1];
        pstring name  = (pstring) addr[0];
        
        if (count != 0)
        {
          if (name != (pstring)TAGGED(0))
          {
			  PPROFENTRY pEnt = newProfileEntry();
			  pEnt->count = count;
			  pEnt->functionName = name;
          }
          
          addr[-1] = 0;
          P.total += count;
        } /* count != 0 */
      } /* code object */
    } /* else */
  } /* while */
  return;
}

/******************************************************************************/
/*                                                                            */
/*      PrintOldProfileCount - utility function - doesn't allocate            */
/*                                                                            */
/******************************************************************************/
static void PrintOldProfileCount(word *addr)
{
  word   *count = addr;
  word    n     = *count;
  pstring name  = (pstring) addr[1];
   
  addr -= 2;
  
  if (*addr != 0)
  {
    crash ("marker missing\n");/* check marker word */
  }
  
  if ((unsigned)name == TAGGED(0))
  {
    crash ("name missing\n"); /* check name word */
  }
  
  addr -= addr[1]/sizeof(word); /* go to beginning of object */
  
  if (! OBJ_IS_CODE_OBJECT(*addr)) crash ("bad object type\n");
  
  if (n)
  {
	  PPROFENTRY pEnt = newProfileEntry();
	  pEnt->count = n;
	  pEnt->functionName = name;
	  P.total += n;
	  *count = 0;
  }
  return;
}

/* This type is coercible to pstring. We can use static "pstrings"
   since we are not going to garbage collect while we are printing out
   the profiling information. */
struct {
	word length; char chars[40];
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
  proper_fflush (stdout); 

  P.total = 0;
  P.used = 0;

  /* PrintLocalProfileCounts makes nasty assumptions about the
     heap - in particular, that the area between pointer and top
     will be "clean" (contain nothing but objects, tombstones
     and zero words). Luckily, this invariant is established
     by each GC (essentially by UpdateObjectsInArea, for the
     benefit of OpMutableBlock) and allocating new objects
     doesn't break it. Phew! That PrintLocalProfileCounts also
     requires this invariant suggests that (at least some of)
     this code should be moved to gc.c.
     SPF 23/10/96
  */
  
  PrintLocalProfileCounts (A.I.pointer,A.I.top);
  PrintLocalProfileCounts (A.M.pointer,A.M.top);

  OpOldProfileCounts (H,PrintOldProfileCount);

  if (gc_count1 || gc_count2 || gc_count3)
  {
		int gc_count = gc_count1 + gc_count2 + gc_count3;
		P.total     += gc_count;
		total_count += gc_count;

		pEnt = newProfileEntry();
		strcpy(psMarkPhase.chars, "GARBAGE COLLECTION (mark phase)");
		psMarkPhase.length = strlen(psMarkPhase.chars);
		pEnt->count = gc_count1;
		pEnt->functionName = (pstring)&psMarkPhase;

		pEnt = newProfileEntry();
		strcpy(psCopyPhase.chars, "GARBAGE COLLECTION (copy phase)");
		psCopyPhase.length = strlen(psCopyPhase.chars);
		pEnt->count = gc_count2;
		pEnt->functionName = (pstring)&psCopyPhase;

		pEnt = newProfileEntry();
		strcpy(psUpdatePhase.chars, "GARBAGE COLLECTION (update phase)");
		psUpdatePhase.length = strlen(psUpdatePhase.chars);
		pEnt->count = gc_count3;
		pEnt->functionName = (pstring)&psUpdatePhase;

		pEnt = newProfileEntry();
		strcpy(psGCTotal.chars, "GARBAGE COLLECTION (total)");
		psGCTotal.length = strlen(psGCTotal.chars);
		pEnt->count = gc_count;
		pEnt->functionName = (pstring)&psGCTotal;

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
		pEnt->functionName = (pstring)&psUnknown;
		unknown_count = 0;
  }

  writeProfileResults();

  if (total_count)
  {

    proper_printf("\nTotal: %d (RTS: %d); Counted: %d; Uncounted: %d",
                   total_count, run_time_count, P.total, total_count - P.total);
                   
    total_count = 0;
    run_time_count = 0;

    /* Only print out the foreign_code_count if it is greater than zero */
    if (foreign_code_count)
    {
      proper_printf(" (Foreign code %d)", foreign_code_count);
      foreign_code_count = 0;
    }
    proper_putc('\n', stdout);
  }
  
  /* flush profile before printing next user output SPF 30/9/94 */
  proper_fflush (stdout); 
}

/******************************************************************************/
/*                                                                            */
/*      catchVTALRM - handler for alarm-clock signal                          */
/*                                                                            */
/******************************************************************************/
#if !defined(WINDOWS_PC)

#if defined(LINUX)
static void catchVTALRM(int sig, struct sigcontext context)
#else
static void catchVTALRM(int sig, int code, SIGNALCONTEXT *scp)
#endif
{
    /* If we are in the garbage-collector add the count to "gc_count"
       otherwise try to find out where we are. */
       
    assert(sig == SIGVTALRM); /* SPF */

#if !defined(LINUX)
    assert(scp != NULL);
#endif

    switch (A.garbage_collecting)
    {
       case 0 : 
#if defined(LINUX)
         MD_increment_profile_count_using_context(&context);
#else
         MD_increment_profile_count_using_context(scp);
#endif
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
	CONTEXT context;
	// Wait for 20ms or until the stop event is signalled.
	while (WaitForSingleObject(hStopEvent, 20) == WAIT_TIMEOUT)        
	{
		switch (A.garbage_collecting)
		{
		case 0 :
			context.ContextFlags = CONTEXT_CONTROL; /* Get Eip and Esp */
			/* We have to suspend the thread before trying to get the
			   thread context. */
			SuspendThread(hMainThread);
			if (in_run_time_system)
				add_count(poly_stack->p_pc,poly_stack->p_sp,1);
			else if (GetThreadContext(hMainThread, &context))
			{
				/* Check that the stack value, at least, is valid. 
				   Perhaps this should be done in add_count as an
				   extra check. */
				word *sp = (word *)context.Esp;
				if (sp > (word*)poly_stack && sp < end_of_stack)
					add_count((byte *)context.Eip, sp, 1);
				else total_count++;
			}
			else total_count++;
			ResumeThread(hMainThread);
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
	return 0;
}/* handleVTALRM PC */
#endif


/******************************************************************************/
/*                                                                            */
/*      stop_profiling - utility function - doesn't allocate                  */
/*                                                                            */
/******************************************************************************/
#if defined(WINDOWS_PC) /* PC version */
void stop_profiling(int print)
{
	DWORD dwSuspendCount = SuspendThread(profilingHd);
	/* SuspendCount increments the suspend count for the thread and
	   returns the previous suspend count.  stop_profiling is called
	   to stop any profiling activity, not just time profiling, so
	   we need to make sure the suspend count is exactly one. */
	if (dwSuspendCount != 0xFFFFFFFF /* Error */ && dwSuspendCount != 0)
		ResumeThread(profilingHd); /* Just decrement the count. */
	store_profiling   = 0;
	emulate_profiling = 0;
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
  emulate_profiling = 0;
  
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
	ResumeThread(profilingHd);
}
#else /* UNIX version */
static void start_timer(void)
{
	setSignalHandler(SIGVTALRM, (signal_handler_type)catchVTALRM);

  {
    /* set virtual timer to go off every 
       20000 microseconds = 20 msec ~ 50 Hz */
    static struct itimerval starttime = {{0,20000}, {0,20000}};
    starttime.it_interval.tv_sec = starttime.it_value.tv_sec = 0;
    starttime.it_interval.tv_usec = starttime.it_value.tv_usec = 20000;
    setitimer(ITIMER_VIRTUAL,&starttime,NULL);
  }
}
#endif

/******************************************************************************/
/*                                                                            */
/*      profilerc - called from assembly.s                                    */
/*                                                                            */
/******************************************************************************/

/* CALL_IO1(profiler, NOIND, NOIND) */
word profilerc(Handle mode_handle)    /* Generate profile information */
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
  unsigned mode = get_C_ulong(DEREFWORDHANDLE(mode_handle));
  
  /* No change in mode = no-op */
  if (mode == profile_mode) return TAGGED(0);
  
  /* profiling 0 turns off profiling and prints results,
     but is ignored for profile_modes > 4. To get the
     accumulated results of these, set profiling to 4. */
  if (mode == 0 && profile_mode > PROF_ACCUMULATED) return TAGGED(0);
  
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
      emulate_profiling = 1;
      break;
    
    default: /* do nothing */
      break;
  }
  
  profile_mode = mode;
  
  return TAGGED(0);
} /* profilerc */

/******************************************************************************/
/*                                                                            */
/*      init_profiling_system - called from init_run_time_system                            */
/*                                                                            */
/******************************************************************************/
void init_profiling_system(void)
{
#if defined(WINDOWS_PC)
	DWORD	dwId;
#endif
	/* Reset profiling counts. */
	store_profiling = 0;
	emulate_profiling = 0;
	gc_count1 = 0;
	gc_count2 = 0;
	gc_count3 = 0;
	run_time_count = 0;
	total_count = 0;
	foreign_code_count = 0;
	unknown_count  = 0;

#if defined(WINDOWS_PC) /* PC version */
	/* Create thread for time profiling */
	hStopEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
	profilingHd = CreateThread(NULL,0,ProfilingTimer, NULL,
							CREATE_SUSPENDED,&dwId);
	if (profilingHd == NULL)
	{
		proper_fputs("Creating ProfilingTimer thread failed.\n", stdout); 
	}
	/* Give this a higher than normal priority so it pre-empts the main
	   thread.  Without this it will tend only to be run when the main
	   thread blocks for some reason. */
	SetThreadPriority(profilingHd, THREAD_PRIORITY_ABOVE_NORMAL);
#endif
    return;
}

/******************************************************************************/
/*                                                                            */
/*      uninit_profiling_system - called from uninit_run_time_system                            */
/*                                                                            */
/******************************************************************************/

/* Release all resources.
   This is really only needed with Windows when running as a DLL within
   the address space of another process. */
void uninit_profiling_system(void)
{
#if defined(WINDOWS_PC)
	/* Stop the timer and profiling threads. */
	if (hStopEvent) SetEvent(hStopEvent);
	if (profilingHd)
	{
		ResumeThread(profilingHd); /* This thread is probably suspended. */
		/* Give the thread time to finish. */
		WaitForSingleObject(profilingHd, 10000);
		CloseHandle(profilingHd);
		profilingHd = NULL;
	}
	if (hStopEvent) CloseHandle(hStopEvent);
	hStopEvent = NULL;
#endif
}
