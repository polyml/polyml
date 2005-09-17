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

/* Note: the arguments for Poly functions are in reverse order to those for C.
   This is because the arguments in Poly are evaluated strictly from left to
   right. */


/*********************************************************************************
  
  DOCUMENTATION (simon) :


                 
        Event                                           State                   SPARC signal    PC signal
        -----                                           -----                   ------------    ---------
(*)     short integer overflow          ML-safe                 sigEMT                  sigFPE (?)
(*)     stack overflow                          ML-safe                 sigILL (?)              sigFPE (?)
(*)     heap limit reached                      ML-safe                 sigEMT                  sigFPE (?)
(#)     float overflow                          RTS-safe(?)             sigFPE                  sigFPE
(+)     Time-slice expired (1Hz)        unsafe                  sigALRM                 not yet implemented
             every sec of real time 
                         I/O interrupts thread - channel i/o is considered i/o
(+)     CPU profiling     (50Hz)        unsafe                  sigVTALRM               not implemented
                         every 20msec of CPU time used by PolyML

Here's how we handle these signals on the SPARC:

        (*) Save the ML state, enter MD_trap_handler1, emulate instruction or grow
            stack or garbage-collect as needed (instruction emulation or growing
            the stack may also cause a garbage-collection). Re-enter ML (not
            necessarily the same process) by a longjmp out of the trap handler.

        (#) Raise an exception i.e. longjmp out of the C exception handler and
            re-enter the current ML process via the ML exception handling mechanism.
                Creating the exception packet may cause a garbage collection, I think,
                so we have to be careful.
            
        (+) Handle the condition within the exception handler, then do a normal
            return from the exception handler to restart execution of the ML (or
            run-time code) at exactly the place where it was interrupted. Note
            that we may have changed it's state however - the time-slice expired
            interrupt changes the stack limit register so that the next time
            the ML process checks its stack we get a "stack exhausted" signal
            that allows us to do a process-swap with a SAFE ML state. The control-C
            interrupt handler does something very similar if the user wants
            to interrupt the running ML function or swap to the alternate process set.
                
Here's how we're going to handle them on the PC:

        (*) In the exception filter (the __except part of the __try block surrounding
            the call to enter ML), save the ML state (extracted from the CONTEXT
            information) and return EXCEPTION_EXECUTE_HANDLER
                to get us into the main handler block. In the main handler block,
                switch back to the C stack, emulate the instruction, grow the ML stack,
                garbage collect as needed etc. Re-enter ML (not necessarily the same process)
                by a longjmp out of the handler block (?).

                Second attempt: In the exception filter, save the ML state, adjust the
                PC in the context to point into the assembly code and return
                EXCEPTION_CONTINUE_EXECUTION to get into the assembly code. In the
                assembly code, switch back to the C stack, then enter the main C
                interrupt handling routines.

        (#) Since this event only occurs in the RTS, we have already saved the
            ML state for this process. Re-enter ML from the UNIX-style exception
            handler by a longjmp to raise the ML exception. 

        (*) In the exception filter, return EXCEPTION_CONTINUE_SEARCH. This
            should mean that we eventually get to execute the UNIX-style exception
                handler for this exception and then return to execute the next instruction.
                If we're in the RTS rather than the ML, we won't execute the exception
                filter, but we will execute the UNIX-style handler. Note that this
                handler does NOT have access to the CONTEXT information - hope we don't
                need it!

Some more subtleties:

        The UNIX signal handlers are executed on a separate signal stack (that's
        what the call to sigaltstack is for). Switching between stacks is handled
        transparently by the operating system when the signal occurs and when the
        handler terminates (memo to self: check that it switches back correctly
        if we longjmp out of the handler).

        I don't think WNT does this - signal handlers are just executed using the
        current value of the stack pointer. While we are executing ML, the stack
        pointer will be pointing into the stack for the current ML process rather
        than at the standard C stack. We therefore have to ensure that each ML
        process has enough spare stack space to execute both our exception
        handlers and the standard WNT system overhead (it pushes a CONTEXT onto
        the stack when it receives a signal). We do this in the assembly code, by
        telling the ML that the stack-bottom is higher than it really is, so
        reserving some extra space on the stack for interrupts. We also have to
        be very careful that we switch back to the C system stack before we
        do anything that might cause a garbage collection, since the ML stack
        isn't going to be big enough for that.

***************************************************************************************/

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
/* PC version */

#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

#include <time.h>
#include <windows.h>  
#include <process.h>  
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>
#include <excpt.h>
#include <float.h>

#else
/* UNIX version */

#include <sys/times.h>
#include <signal.h>

#if defined(SOLARIS2)
#include <sys/systeminfo.h>
#include <ucontext.h>
#endif

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
#include "gc.h"
#include "xwindows.h"
#include "mpoly.h"
#include "arb.h"
#include "machine_dep.h"
#include "machine_assembly.h"
#include "copygc.h"
#include "objsize.h"
#include "mm.h"
#include "foreign.h"
#include "diagnostics.h"
#include "cwd.h"
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

/*********************************************************************
 *
 * forward declarations 
 *
 *********************************************************************/

NORETURN void crash() __attribute__((noreturn));
void give_stack_trace(word *finish);
void init_run_time_system(void);

#define SAVE(x) push_to_save_vec((word)(x))
#define ALLOC(n) alloc_and_save(n)
#define SIZEOF(x) (sizeof(x)/sizeof(word))

#define EMPTYSTRING interface_map[POLY_SYS_nullorzero]

#define NEWSTRINGHANDLE(len) (ALLOC((len + sizeof(word)-1)/sizeof(word) + OBJ_BYTE_BIT + 1))

JMP_BUF re_enter_poly;

/******************************************************************************/
/*                                                                            */
/*      STORAGE ALLOCATION                                                    */
/*                                                                            */
/******************************************************************************/

/* Storage Allocator for the run-time system. 
   The addresses of objects allocated are saved in 
   this vector in case they are garbage-collected. */


/******************************************************************************/
/*                                                                            */
/*      alloc - called by run-time system                                     */
/*                                                                            */
/******************************************************************************/
word *alloc(word lengthword)
/* Allocate a number of words. */
{
  int data_words = OBJ_OBJECT_LENGTH(lengthword);
  int words = data_words + 1;
  int space = A.M.pointer - A.M.bottom;

  if (store_profiling)
  {
    add_count(poly_stack->p_pc, poly_stack->p_sp, words);
  }

  if ((words > space) || (A.debug & DEBUG_FORCEGC))
  {
    QuickGC (words);
  }

  A.M.pointer -= words;
  
  {
    int i;
    word *pt = A.M.pointer + 1;
    pt[-1] = lengthword;
    
    /* Must initialise object here, because GC no longer cleans store. */
    for (i = 0; i < data_words; i++)
    {
      pt[i] = 0;
    }
    return pt;
  }
}

/******************************************************************************/
/*                                                                            */
/*      HANDLES                                                               */
/*                                                                            */
/******************************************************************************/

#define SVEC_SIZE 1000

/* still used by the assembly code, unfortunately */
word save_vec[SVEC_SIZE];

/* Reset before each run-time system call */
Handle save_vec_addr = (Handle)(&save_vec[0]);

const Handle save_vec_min = (Handle)(&save_vec[0]);
const Handle save_vec_max = (Handle)(&save_vec[SVEC_SIZE]);

/******************************************************************************/
/*                                                                            */
/*      isValidHandle - called by run-time system                             */
/*                                                                            */
/******************************************************************************/
int isValidHandle(Handle h)
{
  return(save_vec_min <= h && h < save_vec_addr);
}

/******************************************************************************/
/*                                                                            */
/*      init_save_vec - called by run-time system                             */
/*                                                                            */
/******************************************************************************/
void init_save_vec(void)
{
  save_vec_addr = save_vec_min;
}

/******************************************************************************/
/*                                                                            */
/*      mark_save_vec - called by run-time system                             */
/*                                                                            */
/******************************************************************************/
Handle mark_save_vec(void)
{
  return save_vec_addr;
}

/******************************************************************************/
/*                                                                            */
/*      reset_save_vec - called by run-time system                            */
/*                                                                            */
/******************************************************************************/
void reset_save_vec(Handle old_value)
{
  assert(isValidHandle(old_value));

  save_vec_addr = old_value;
}


/******************************************************************************/
/*                                                                            */
/*      push_to_save_vec - called by run-time system                          */
/*                                                                            */
/******************************************************************************/
Handle push_to_save_vec(word valu) /* Push a word onto the save vec. */
{
  if (save_vec_addr >= save_vec_max)
  {
     crash("Save_vec overflow\n");
  }
  
  *((word *)save_vec_addr) = valu; /* should use macro here ??? */

  return save_vec_addr++;
}


/******************************************************************************/
/*                                                                            */
/*      alloc_and_save - called by run-time system                            */
/*                                                                            */
/******************************************************************************/
Handle alloc_and_save(int size)
/* Allocate and save the result on the vector. */
{
  return SAVE(alloc(size));
}


/******************************************************************************/
/*                                                                            */
/*      GARBAGE COLLECTOR - see also xwindows.c                               */
/*                                                                            */
/******************************************************************************/

/******************************************************************************/
/*                                                                            */
/*      run_time_gc - utility function, called indirectly                     */
/*                                                                            */
/******************************************************************************/
static void run_time_gc(GCOpFunc op)
/* Ensures that all the objects are retained and their addresses updated. */
{
    Handle sv;
    
    /* Entries in the save vector. */
    for(sv = save_vec_min; sv < save_vec_addr; sv++)
      {
        (*op)((word **)sv, 0);
      }

    return;
}

/******************************************************************************/
/*                                                                            */
/*      full_gc_c - called by sparc_assembly.s                                */
/*                                                                            */
/******************************************************************************/
/* CALL_IO0(full_gc_, NOIND) */
word full_gc_c(void)
{
  FullGC();
  return TAGGED(0);
}

/******************************************************************************/
/*                                                                            */
/*      CONVERSION FUNCTIONS (1)                                              */
/*                                                                            */
/******************************************************************************/

/******************************************************************************/
/*                                                                            */
/*      Buffer_to_Poly - called by run-time system                            */
/*                                                                            */
/******************************************************************************/
pstring Buffer_to_Poly(const char *buffer, int length) 
/* Returns a string as a Poly string. */
{
    pstring result;

    /* Return the null string if it's empty. */
    if (length == 0) return (pstring)EMPTYSTRING;
    
    /* Return the character itself if the length is 1 */
    if (length == 1) return (pstring)(TAGGED(((unsigned char *)buffer)[0]));
    
    /* Get the number of words required, plus 1 for length word,
       plus flag bit. */
    result = (pstring)(alloc(WORDS(length) + 1 + OBJ_BYTE_BIT));
    
    /* Set length of string, then copy the characters. */
    result->length = length;
    memcpy(result->chars,buffer,length);
	/* We are relying on alloc zeroing any unused bytes in the
	   allocated store.  That's essential for structure equality to
	   work properly since it compares ALL bytes in a byte segment.
	   (c.f. test*/
    return(result);
} /* Buffer_to_Poly */


/******************************************************************************/
/*                                                                            */
/*      C_string_to_Poly - called by run-time system                          */
/*                                                                            */
/******************************************************************************/
pstring C_string_to_Poly(const char *buffer)
/* Returns a C string as a Poly string. */
{
    if (buffer == NULL) return (pstring)EMPTYSTRING;
    
    return Buffer_to_Poly(buffer, strlen(buffer));
} /* C_string_to_Poly */

/******************************************************************************/
/*                                                                            */
/*      Poly_string_to_C - called by run-time system                          */
/*                                                                            */
/******************************************************************************/
word Poly_string_to_C(pstring ps, char *buff, int bufflen)
/* Copies the characters from the string into the destination buffer.
   Returns original length of string. */
{
    int chars;

    if (IS_INT(ps))
    {
        buff[0] = (char)(UNTAGGED(ps));
        buff[1] = '\0';
        return(1);
    }
    chars = ps->length >= bufflen ? bufflen-1 : ps->length;
    if (chars != 0) strncpy(buff, ps->chars, chars);
    buff[chars] = '\0';
    return(chars);
} /* Poly_string_to_C */


/******************************************************************************/
/*                                                                            */
/*      Poly_string_to_C_alloc - called by run-time system                          */
/*                                                                            */
/******************************************************************************/
char *Poly_string_to_C_alloc(pstring ps)
/* Similar to Poly_string_to_C except that the string is allocated using
   malloc and must be freed by the caller. */
{
    int		chars;
	char	*res;

    if (IS_INT(ps))
    {
		res = malloc(2);
		res[0] = (char)(UNTAGGED(ps));
        res[1] = '\0';
    }
	else
	{
		chars = ps->length;
		res = malloc(chars+1);
		if (chars != 0) strncpy(res, ps->chars, chars);
		res[chars] = '\0';
	}
    return res;
} /* Poly_string_to_C_alloc */

/* convert_string_list return a list of strings. */
Handle convert_string_list(int count, char **strings)
{
   Handle list  = SAVE(nil_value);
   Handle saved = mark_save_vec();
 
   int   i;
   
   /* It's simplest to process the strings in reverse order */
   for (i = count - 1; 0 <= i; i--)
   {
     /* 
	 The order of these declarations is important, becaue we don't
	 want to have make to make the cons cell mutable. This is only
	 safe if we promise to fully initialise it before the next
	 ML heap allocation. SPF 29/11/96
     */
     Handle value = SAVE(C_string_to_Poly(strings[i]));
     Handle next  = ALLOC(SIZEOF(ML_Cons_Cell));
     
     DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(value); 
     DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);
     
     DEREFHANDLE(list) = DEREFHANDLE(next);
 
     /* reset save vector to stop it overflowing */    
     reset_save_vec(saved);
   }
   
   return list;
}

/* Convert a string list to a vector of C strings. */
char **stringListToVector(Handle list)
{
	int len = 0;
	ML_Cons_Cell *p;
	char **vec;
	/* Find the length of the list */
	for (p = DEREFLISTHANDLE(list); p != (ML_Cons_Cell*)nil_value; p = p->t) len++;
	/* Allocate vector. */
	vec = (char**)calloc(len+1, sizeof(char*));
	/* Copy the strings and put them into the vector. */
	len = 0;
	for (p = DEREFLISTHANDLE(list); p != (ML_Cons_Cell*)nil_value; p = p->t)
		vec[len++] = Poly_string_to_C_alloc((pstring)(p->h));
	return vec;
}

/* Free the memory used by the vector. */
void freeStringVector(char **vec)
{
	char **p;
	if (vec == 0) return;
	for (p = vec; *p != 0; p++) free(*p);
	free(vec);
}

/******************************************************************************/
/*                                                                            */
/*      EXCEPTIONS                                                            */
/*                                                                            */
/******************************************************************************/

static char *exception_names[] =
{
  "Interrupt",
  "SysErr",       /* System call failed. */
  "Commit",
  "Size",         /* general size problem */
  "Overflow",     /* raised when real arithmetic overflows  */
  "Underflow",    /* raised when real arithmetic underflows */
  "Div",
  "Domain",       /* can't convert value from one type to another */
  "Io",			  /* Being phased out. */
  "XWindows",
  "Subscript",
  "Projection",    /* No longer used */
  "StackOverflow", /* No longer used */
  "Prod",		   /* No longer used */
  "Quot",		   /* No longer used */
  "Sum",		   /* No longer used */
  "Diff",		   /* No longer used */
  "Neg",		   /* No longer used */
  "Sqrt",		   /* No longer used */
  "undefined",     /* used only by Poly compiler(?) */
  "Ln",		       /* No longer used */
  "Exp",		   /* No longer used */
  "Foreign"
};

#define MAXENAME (sizeof(exception_names)/sizeof(exception_names[0]))

/******************************************************************************/
/*                                                                            */
/*      make_exn - utility function - allocates in Poly heap                  */
/*                                                                            */
/******************************************************************************/
Handle make_exn(word id, Handle arg)
{
  if (! (1 <= id && id <= MAXENAME))
    {
      crash ("Interrupt id (%d) out of range\n",id);
    }
   
  {
    Handle pushed_name = SAVE(C_string_to_Poly(exception_names[id-1]));
    
    Handle exnHandle = ALLOC(SIZEOF(poly_exn));
    
    DEREFEXNHANDLE(exnHandle)->ex_id   = TAGGED(id);
    DEREFEXNHANDLE(exnHandle)->ex_name = (pstring) DEREFHANDLE(pushed_name);
    DEREFEXNHANDLE(exnHandle)->arg     = DEREFWORDHANDLE(arg);
    
    return exnHandle;
  }
}

/******************************************************************************/
/*                                                                            */
/*      set_exception_state - utility function - allocates in Poly heap       */
/*                                                                            */
/******************************************************************************/
static void set_exception_state(ProcessHandle proc, word id, Handle arg)
/* Raise an exception from the run-time system. */
{
	Handle exn = make_exn(id,arg);
	/* N.B.  We must create the packet first BEFORE dereferencing the
	   process handle just in case a GC while creating the packet
	   moves the process and/or the stack. */
	MD_set_exception(DEREFPROCHANDLE(proc)->stack, DEREFEXNHANDLE(exn));
}

/******************************************************************************/
/*                                                                            */
/*      raise_exception - called by run-time system                           */
/*                                                                            */
/******************************************************************************/
void raise_exception(word id, Handle arg)
/* Raise an exception with no arguments. */
{
    set_exception_state(processes, id, arg);
    RE_ENTER_POLY(222); /* Return to Poly code immediately. */
    /*NOTREACHED*/
}


/******************************************************************************/
/*                                                                            */
/*      raise_exception0 - called by run-time system                          */
/*                                                                            */
/******************************************************************************/
void raise_exception0(word id)
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
void raise_exception_string(word id, char *str)
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
	{	/* SOME err */
		Handle errornum = Make_arbitrary_precision(err);
		pushed_option = ALLOC(1);
		DEREFHANDLE(pushed_option)[0] = DEREFWORDHANDLE(errornum);
	}
	pushed_name = SAVE(C_string_to_Poly(errmsg));
	pair = ALLOC(2);
	DEREFHANDLE(pair)[0] = DEREFWORDHANDLE(pushed_name);
	DEREFHANDLE(pair)[1] = DEREFWORDHANDLE(pushed_option);
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
/*      strconcatc - called by sparc_assembly.s                               */
/*                                                                            */
/******************************************************************************/
/* CALL_IO2(strconcat, REF, REF, IND) */
Handle strconcatc(Handle y, Handle x)
/* Concatenate two strings - used only by other routines in the run-time
   system and in the interpreter. There is an alternative version of this
   in the assembly-code segments for the Sun and the Vax. */
/* Note: arguments are in the reverse order from Poly */
{
    Handle result;
    word len, xlen, ylen;
    char *from_ptr, *to_ptr;
    
    if (IS_INT(DEREFSTRINGHANDLE(x)))
      xlen = 1;
    else 
      xlen = DEREFSTRINGHANDLE(x)->length;
    
    /* Don't concatenate with null strings */
    if (xlen == 0) return y;
    
    if (IS_INT(DEREFSTRINGHANDLE(y)))
      ylen = 1;
    else
      ylen = DEREFSTRINGHANDLE(y)->length;
      
    if (ylen == 0) return x;
    
    len = xlen + ylen;
    
    /* Get store for combined string. Include rounding up to next word and
       room for the length word and add in the flag. */
    result = NEWSTRINGHANDLE(len);
    
    DEREFSTRINGHANDLE(result)->length = len;
    
    /* Copy first string */
    to_ptr = DEREFSTRINGHANDLE(result)->chars;
    if (xlen == 1)
       {
         *to_ptr++ = (char)UNTAGGED(DEREFSTRINGHANDLE(x));
       }
    else
      {
        from_ptr = DEREFSTRINGHANDLE(x)->chars;
        while (xlen-- > 0) (*to_ptr++ = *from_ptr++);
      }
        
        
    /* Add on second */
    if (ylen == 1)
      {
        *to_ptr = (char)UNTAGGED(DEREFSTRINGHANDLE(y));
      }
    else
      {
        from_ptr = DEREFSTRINGHANDLE(y)->chars;
        while (ylen-- > 0) (*to_ptr++ = *from_ptr++);
      }
      
    return(result);
} /* strconcat */

/******************************************************************************/
/*                                                                            */
/*      substringc - called by sparc_assembly.s                               */
/*                                                                            */
/******************************************************************************/
/* CALL_IO3(substring, REF, REF, REF, IND) */
Handle substringc(Handle lHandle, Handle nHandle, Handle sHandle)
/* Returns the substring of s from position n of length l. Raises
   "subscripterror" if l is negative, or n or n+l are not in the string. */
/* Note: arguments in reverse order from Poly. */
{
    Handle resultHandle;
    char *from_ptr, *to_ptr;
    word slength;

    unsigned l = get_C_ulong(DEREFWORDHANDLE(lHandle)); /* SPF 31/10/93 */
    unsigned n = get_C_ulong(DEREFWORDHANDLE(nHandle)); /* SPF 31/10/93 */

#define s ((pstring)DEREFWORDHANDLE(sHandle))
    if (IS_INT(s))
       slength = 1;
    else
      slength = s->length;
    
    /* Length must be +ve, offset >= 1 and end of new string in s */
    /* if (l < 0 || n <= 0 || (l+n-1) > slength) ... but l, n are UNsigned */
    if (n == 0 || (l+n-1) > (unsigned)slength)
       {
        raise_exception0(EXC_subscript);
        /* NOTREACHED */
       }
       
    /* If the length required is the same as the length of the string then
       we can return the original string. (n must be 1 otherwise we would have
       raised an exception). */
    if (l == (unsigned)slength) return sHandle;
    
    /* If the result is the null string return the entry in the I/O vector. */
    if (l == 0) return (Handle)(&EMPTYSTRING);

    /* The source string must be at least 2 chars and the result is a valid
       substring. Is just one character required? */
    if (l == 1) return SAVE(TAGGED((unsigned char)(s->chars[n-1])));

    /* Get store for new string. */
    resultHandle = NEWSTRINGHANDLE(l);

#define result ((pstring)DEREFWORDHANDLE(resultHandle))
    result->length = l;
    from_ptr = s->chars+n-1;
    to_ptr   = result->chars;
    while (l-- > 0) (*to_ptr++ = *from_ptr++);
#undef result
#undef s

    return resultHandle;
} /* substringc */



/******************************************************************************/
/*                                                                            */
/*      INTERRUPT HANDLERS                                                    */
/*                                                                            */
/******************************************************************************/

#define INT_VEC_SIZE 5
/* Each of these is given control when an interrupt has occured. */
static InterruptFunc int_procs[INT_VEC_SIZE] = {0, 0, 0, 0, 0};

int interrupted = 0;

/******************************************************************************/
/*                                                                            */
/*      register_interrupt_proc - utility function - doesn't allocate         */
/*                                                                            */
/******************************************************************************/
void register_interrupt_proc(InterruptFunc int_proc)
/* Add an interrupt procedure to the set. */
{
    int i;
    for(i = 0; i < INT_VEC_SIZE && int_procs[i] != 0; i++){};
    if (i == INT_VEC_SIZE) crash("Interrupt table overflow.\n");
    int_procs[i] = int_proc;
    return;
}

/******************************************************************************/
/*                                                                            */
/*      execute_pending_interrupts - utility function - shouldn't allocates   */
/*                                                                            */
/******************************************************************************/
void execute_pending_interrupts(void)
/* Called when we are in a safe state to execute this. */
{
    int i,sig;
    while (interrupted)
      {
       /* We may get interrupts while processing others. FISHY??? */ 
       sig = interrupted; interrupted = 0;
       for(i = 0; i < INT_VEC_SIZE; i++)
         {
           if (int_procs[i])
            { 
              int_procs[i](sig);
            }
         }
       }
    return;
}

/******************************************************************************/
/*                                                                            */
/*      PRINT FUNCTIONS                                                       */
/*                                                                            */
/******************************************************************************/

/******************************************************************************/
/*                                                                            */
/*      print_string - utility function (also used by xwindows and profiling) */
/*                                                                            */
/******************************************************************************/
void print_string(pstring s)
{
  if (IS_INT(s))
  { 
     proper_putc((char)UNTAGGED(s), stdout);
  }
  else
  {
     proper_fwrite_chars(s->chars, s->length, stdout);
  }
}


/******************************************************************************/
/*                                                                            */
/*      TRACE FUNCTIONS                                                       */
/*                                                                            */
/******************************************************************************/

int trace_allowed = 0; /* Allows ^C to stop a trace. */

/******************************************************************************/
/*                                                                            */
/*      give_stack_trace - utility function - doesn't allocate                */
/*                                                                            */
/******************************************************************************/
void give_stack_trace(word *finish)
{
    word *sp;
    word *exceptions;
    /* Now search for the return addresses on the stack.
       The values we find on the stack which are not word aligned may be
       either return addresses or the addresses of handlers.
    */
    trace_allowed = 1; /* May be switch off by catchINT. */
    
    sp = poly_stack->p_sp;
    exceptions = poly_stack->p_hr;
    
#ifdef DEBUG    
    proper_printf("starting trace: sp = %p, finish = %p, end_of_stack = %p\n",
                    sp, finish, end_of_stack);
    proper_fflush(stdout);
#endif

    if (finish > end_of_stack) finish = end_of_stack;
    
    for(; trace_allowed && sp < finish-1; sp++)
      {
        int pc = *sp;
        
        /* If this is an exception handler do not treat it as return
           address, just get the next handler */
        if (sp == exceptions)
          {
            /* Skip over the handlers until we find the next pointer up the
               stack. */
            while (sp < finish &&
                   !((word*)*sp >= sp && (word*)*sp <= end_of_stack)) sp++;
                   
            exceptions = (word*)*sp;
          }
        else if (OBJ_IS_CODEPTR(pc))
         {
            /* A code pointer will be either a return address or a pointer
               to the constant section. (Or an exception handler?)
            */
            word *ptr, p_name;

            /* The constant pointer is also a code pointer but
                we don't want to see it in the stack trace. Actually,
                a real code pointer could conceivably look like
                the constant pointer. This doesn't happen on
                the SPARC (there wouldn't be room for any code after
                the function call) but it could on other architectures
                (the PC?). SPF 26/7/96
             */
            if (OBJ_CODEPTR_POINTS_AT_CONSTANTS_SECTION(pc)) continue;

           /* Initialise ptr to points at the end-of-code marker */
           OBJ_CODEPTR_TO_CONSTS_PTR(pc, ptr);
            
            p_name = ptr[3]; /* Get procedure name */
            
            /* The name may be zero if it is anonymous */
            if (p_name == TAGGED(0))
            {
               proper_fputs("<anon>\n",stdout);
            }
            else
            {
               print_string((pstring)p_name);
               proper_putc('\n',stdout);
            }
        }
    }
   proper_fflush(stdout);
   return;
} /* give_stack_trace */


/******************************************************************************/
/*                                                                            */
/*      stack_trace_c - called from sparc_assembly.s                          */
/*                                                                            */
/******************************************************************************/
/* CALL_IO0(stack_trace_, NOIND) */
word stack_trace_c(void)
{
  give_stack_trace (end_of_stack);
  return TAGGED(0);
}

/******************************************************************************/
/*                                                                            */
/*      ex_tracec - called from sparc_assembly.s                              */
/*                                                                            */
/******************************************************************************/
/* CALL_IO2(ex_trace, REF, REF, NOIND) */
void ex_tracec(Handle exnHandle, Handle handler_handle)
{
   word *handler = DEREFWORDHANDLE(handler_handle);
 
   proper_fputs("\nException trace for exception - ", stdout);
   print_string(((poly_exn *)DEREFHANDLE(exnHandle))->ex_name);
   proper_putc('\n',stdout);
   
   /* Trace down as far as the dummy handler on the stack. */
   give_stack_trace(handler);
   proper_fputs("End of trace\n\n",stdout);
   proper_fflush(stdout);
   
   /* Set up the next handler so we don't come back here when we raise
      the exception again. */
   DEREFPROCHANDLE(processes)->stack->p_hr = (word *)(*handler);
   
   /* Set the exception data back again. */
   MD_set_exception(DEREFPROCHANDLE(processes)->stack,(poly_exn *)DEREFHANDLE(exnHandle));
   
   RE_ENTER_POLY(555); /* Reraise the exception. */
   /*NOTREACHED*/
}


/******************************************************************************/
/*                                                                            */
/*      crash - utility function - doesn't allocate                           */
/*                                                                            */
/******************************************************************************/
/*PRINTFLIKE1*/  /* Prints out a message and crashes. */
void crash(str, a, b, c, d)
char *str;
int a, b,c, d;
{ /* Give reason for crash */
  proper_fflush(stdout);
  proper_printf(str, a, b, c, d);
  proper_fflush(stdout);
  
  /* Try to give trace */
  give_stack_trace(end_of_stack);
  
/* abort closes open streams */
#if NEVER
  /* Close and flush all streams */
   _cleanup();
#endif
   
#if defined(WINDOWS_PC) /* porting */
   proper_fputs("run-time.c - crash ... \n",stdout);
   proper_fflush(stdout);
#else    
  {
    sigset_t set;
    sigemptyset(&set);
    sigprocmask(SIG_SETMASK,&set,NULL);
  }
#endif    
  
  /* Crash */    
  abort();
  /*NOTREACHED*/
} /* crash */


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
	ExitProcess(exitCode);
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
	MD_interrupt_code();
	hCrowBar = CreateThread(NULL, 0, CrowBarFn, 0, 0, &dwId);
	if (! hCrowBar) ExitProcess(n);
	CloseHandle(hCrowBar);
}
#endif

/* end of interrupt handling */

/******************************************************************************/
/*                                                                            */
/*      run_time_interrupts - utility function - allocates???                 */
/*                                                                            */
/******************************************************************************/
static void run_time_interrupts(int signum /* not used */)
/* Called from execute_pending_interrupts some time after a signal such as
   SIGALRM, SIGINT etc. 
   If the signal was SIGALRM it selects the next process (a more
   subtle scheme would try to find the process which wanted to do IO). */
{
#ifdef WINDOWS_PC
	if (exitRequest) finish(exitCode);
#endif
	/* There is never any harm in switching processes. */
	if (DEREFPROCHANDLE(processes) != NO_PROCESS)
		select_next_process();
}


/******************************************************************************/
/*                                                                            */
/*      set_dbentry_c - called from sparc_assembly.s                          */
/*                                                                            */
/******************************************************************************/
/* CALL_IO1(set_dbentry_,VAL,NOIND) */
word set_dbentry_c (Handle value_handle)
{
  word value = (word)DEREFHANDLE(value_handle);
  runtime_assign_word (((word *)processes)+2,value);
  return (TAGGED(0));
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
	re_init_io_system();
	re_init_process_system();
    re_init_xwindow_system(); /* added 18/11/93 SPF */
	re_init_proc_system();
	re_init_timing_system();
	re_init_net_system();
	re_init_os_specific();
	re_init_sig_handler();
	re_init_reals();
    return;
}

/******************************************************************************/
/*                                                                            */
/*      init_run_time_system - called from mpoly.c                            */
/*                                                                            */
/******************************************************************************/
void init_run_time_system(void)
{
    int i;
	/* Reset globals.  Only really needed in Windows DLL (?).
	   Some of these may only need resetting if we had a crash. */
    for(i = 0; i < INT_VEC_SIZE; i++) int_procs[i] = 0;

	interrupted = 0;

    /* ensure that all signals are handled using exception_stack */ 
#if !defined(WINDOWS_PC)
    /* Old comment:
         Stack for interrupts. Ensures that poly processes using sp to point
         to the top of the stack will not get C stack frames on them.
         On the Sun this has to be used to get rts and rtd right.
       
       New comment:
         The above comment only applies to architectures (i386, m68000, vax)
         where the Poly/ML stack is implemented using the standard stack pointer.
         All the current RISC implementations (SPARC, HPPA, POWER) use a different
         register for the Poly/ML stack pointer, and could safely handle signals
         on the standard C stack. However, we still choose to use the alternate
         signal stack mechanism so that we can handle stack-overflow errors
         and the like which can occur, especially while we are PORTING.
         SPF 15/11/1997
    */


/* DON'T install alternate stack on HP-UX - it may cause problems. */
/* Earlier versions of Linux did not have sigaltstack.  It MUST be used
   in later versions for sigactions installed with SA_ONSTACK otherwise
   a SIGSEGV is sent when the signal would be delivered. */
#if (defined(SA_ONSTACK) && !defined(HPUX))
#define EXCEPTION_STACK_SIZE (4*MINSIGSTKSZ)
      /* For SYSV, we don't care whether the stack grows up or down -
         we just give the OS the start address and size of the save
         area. It should figure the rest out itself.
      */
    {
      static char exception_stack[EXCEPTION_STACK_SIZE];
#if (defined(FREEBSD) || defined(MACOSX))
	struct sigaltstack ex_stack;
#else
	stack_t ex_stack;
#endif
      ex_stack.ss_sp    = exception_stack;
      ex_stack.ss_size  = EXCEPTION_STACK_SIZE;
      ex_stack.ss_flags = 0; /* not SS_DISABLE */
      {
         int sigaltstack_result = sigaltstack(&ex_stack, NULL);
         assert(sigaltstack_result == 0);
      }
   }
#endif

#endif /* not the PC */


    
    /* Register procedures to garbage-collect the procedure and run-time
       data-structures. */
    RegisterGCProc(run_time_gc);

    /* Register procedures to garbage-collect volatiles */
    RegisterGCProc(volatile_gc);
    
    
    /* Register interrupt procedure to select the next process. */
    register_interrupt_proc(run_time_interrupts);

	init_process_system();
	init_profiling_system();

    /* initialise working directory (SPF 21/11/95) */
    init_working_directory();

    /* initialise interface save vector */
    init_save_vec();
	init_io_system();
	init_proc_system();
	init_timing_system();
	init_net_system();
	init_os_specific();
	init_sig_handler();
	init_reals();
    init_xwindow_system(); /* Initialise X Window system. */
    init_objsize_system(); /* MJC 27/04/88 */
    
    return;
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
#if (0)
	uninit_xwindow_system(); /* Probably needs to be done. */
#endif

	uninit_profiling_system();
	uninit_io_system();
	uninit_proc_system();
	uninit_timing_system();
	uninit_net_system();
	uninit_os_specific();
	uninit_sig_handler();
	uninit_process_system();
	uninit_reals();
}


/******************************************************************************/
/*                                                                            */
/*      get_flags_c - called from machine_assembly.s                          */
/*                                                                            */
/******************************************************************************/
/* CALL_IO1(get_flags_,REF,NOIND) */
word get_flags_c(Handle addr_handle)
{
   word *pt = DEREFWORDHANDLE(addr_handle);

  if ((pt >= A.M.bottom && pt <= A.M.top) ||
      (pt >= A.I.bottom && pt <= A.I.top) ||
      IsDataPointer(H,pt))
  {
    const word old_word  = pt[-1];
    const word old_flags = OBJ_GET_OBJECT_USER_FLAGS(old_word);
    return TAGGED(old_flags);
  }
  /* This is for backwards compatibility only.  Previously this
     was used to test for an IO address.  Instead an entry has
     been added to process_env to test for an IO address. */
  else if (IsIOPointer(H,pt))
  {
	return TAGGED(256);
  }
  else Crash ("Bad call to get_flags(%p)",pt);
  /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      set_flags_c - called from machine_assembly.s                          */
/*                                                                            */
/******************************************************************************/
/* CALL_IO2(set_flags_,VAL,VAL,NOIND) */
word set_flags_c(Handle value_handle, Handle addr_handle)
{
  word *pt = DEREFWORDHANDLE(addr_handle);
  word x   = (word)DEREFHANDLE(value_handle);

  if (pt >= A.M.bottom && pt <= A.M.top)
  {
    const word old_word = pt[-1];
    const word new_flags = UNTAGGED(x);
    const word new_word  = OBJ_SET_OBJECT_USER_FLAGS(old_word,new_flags);
    
    if (!OBJ_IS_AN_INTEGER(x))
    {
      Crash ("Bad call to set_flags(%p,%x)",pt,x);
    }

    if (!OBJ_IS_MUTABLE_OBJECT(old_word))
    {
      Crash ("Bad call to set_flags(%p,%x)",pt,x);
    }
    
    assert(OBJ_OBJECT_LENGTH(new_word) == OBJ_OBJECT_LENGTH(old_word));
    assert(OBJ_GET_OBJECT_USER_FLAGS(new_word) == new_flags);
  
    pt[-1] = new_word;
    
    /* If we've just created an immutable code segment, we may be about
       to execute it, so we must flush the cache. SPF 13/2/97
    */
    if (OBJ_IS_CODE_OBJECT(new_word) && !OBJ_IS_MUTABLE_OBJECT(new_word))
    {
      MD_flush_instruction_cache(pt, OBJ_OBJECT_LENGTH(new_word) * sizeof(word));
    }
  
    return TAGGED(0);
  }
  else if (IsDataPointer(H,pt))
  {
    const word old_word = pt[-1];
    const word new_flags = UNTAGGED(x);
    const word new_word  = OBJ_SET_OBJECT_USER_FLAGS(old_word,new_flags);
    
    if (!OBJ_IS_AN_INTEGER(x))
    {
      Crash ("Bad call to set_flags(%p,%x)",pt,x);
    }

    if (!OBJ_IS_MUTABLE_OBJECT(old_word))
    {
      Crash ("Bad call to set_flags(%p,%x)",pt,x);
    }
    
    assert(OBJ_OBJECT_LENGTH(new_word) == OBJ_OBJECT_LENGTH(old_word));
    assert(OBJ_GET_OBJECT_USER_FLAGS(new_word) == new_flags);
  
    AssignMappedDatabaseWord(H, pt, -1, new_word);
  
    /* If we've just created an immutable code segment, we may be about
       to execute it, so we must flush the cache. SPF 13/2/97
    */
    if (OBJ_IS_CODE_OBJECT(new_word) && !OBJ_IS_MUTABLE_OBJECT(new_word))
    {
      MD_flush_instruction_cache(pt, OBJ_OBJECT_LENGTH(new_word) * sizeof(word));
    }
  
    return TAGGED(0);
  }
  else Crash ("Bad call to set_flags(%p,%x)",pt,x);
  /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      BadOpCode_c - called from machine_assembly.s                          */
/*                                                                            */
/******************************************************************************/
word BadOpCode_c(void)
{
  crash ("Bad RunTime OpCode\n");
  return 3;
}

/******************************************************************************/
/*                                                                            */
/*      raise_exc - called from machine_assembly.s                            */
/*                                                                            */
/******************************************************************************/
/* CALL_IO1(raise_ex, REF, NOIND) */
void raise_exc(Handle id_handle)
/* Called from the assembly-code segment. */
{
   word id = (word)DEREFHANDLE(id_handle);
   raise_exception0(UNTAGGED(id));
}

/******************************************************************************/
/*                                                                            */
/*      enter_poly_code - called from mpoly.c                                 */
/*                                                                            */
/******************************************************************************/
void enter_poly_code(void)
/* Called from "main" to enter the code. */
{
  /* Select the first process. */
  select_next_process(); 
  
  /* Set the jump for exceptions. */
  SETJMP(re_enter_poly);
  /* This is where we come after an exception or retry */
  /* Run the process(es) - will never return */
  MD_switch_to_poly();
  Crash("MD_switch_to_poly unexpectedly returned");
}
