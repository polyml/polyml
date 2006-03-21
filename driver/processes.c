/*
    Title:      Lightweight process library
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

/* This was originally in run_time.c  */

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
#include <sys/types.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>
#include <errno.h>

#include <stdlib.h>
#include <string.h>
#include <signal.h>
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
#include "mpoly.h"
#include "arb.h"
#include "machine_dep.h"
#include "diagnostics.h"

/* added 21/11/95 SPF */
#include "cwd.h"

#include "objects.h"
#include "processes.h"
#include "run_time.h"
#include "sys.h"
#include "sighandler.h"


#define SAVE(x) push_to_save_vec((word)(x))
#define ALLOC(n) alloc_and_save(n)
#define SIZEOF(x) (sizeof(x)/sizeof(word))

/*******************************************************************
 *
 * The initial size of a stack frame. It can expand from this 
 * and need not be a power of 2. However it must be larger than 
 * 40 + the size of the base area in each stack frame, which is 
 * machine dependent. The figure 40 comes from 2*min_stack_check 
 * in the Poly code-generator. 
 *
 *******************************************************************/

#define INIT_STACK_SIZE (128 + EXTRA_STACK)

#define ASSIGN(p,v) runtime_assign_word((word *)(&(p)),(word)(v))


#define EMPTYSTRING interface_map[POLY_SYS_nullorzero]

#define ISZEROHANDLE(handle) ((int)DEREFHANDLE(handle) == TAGGED(0))

#define MUTABLE(x) (OBJ_MUTABLE_BIT | (x))

#define NEWSTACK(len) ((StackObject *)alloc(MUTABLE((len) + OBJ_STACK_BIT)))
#define NEWPROCESSHANDLE()   (ALLOC(MUTABLE(sizeof(process_base)/sizeof(word))))
#define NEWSYNCHROHANDLE()   (ALLOC(MUTABLE(sizeof(synchroniser)/sizeof(word))))

/* processes is used in machine_assembly.s */
       ProcessHandle  processes       = NULL;       /* Process chain. */
static int            no_of_processes = 0;          /* Number of processes. */
static int            no_waiting      = 0;          /* Number of those processes waiting for IO. */
static ProcessHandle console_chain   = NULL;       /* Chain of console processes. */

StackObject *poly_stack   = 0;    /* Stack of current process. */
word        *end_of_stack = 0;  /* Address of the end of poly_stack. */

static poly_exn *interrupt_exn = 0;

/* Forward declarations. */

static void fix_process_addresses(ProcessHandle proc);

#ifdef WINDOWS_PC
static HANDLE hStopEvent; /* Signalled to stop all threads. */
static HANDLE hInterruptTimer; /* Interrupt timer thread. */
#endif

#ifndef WINDOWS_PC
/* The file descriptors currently active are held in the bits in this word.
   Apart from processes waiting for IO streams the window system also needs to
   be aware of window activity.
   We don't use this for Windows because the task of determining which
   devices have become available is just too complicated.  We rely on timing
   out in that case.
*/
static fd_set file_desc;
#endif

/* The following are only declared forward so we can attach attributes */
NORETURN void select_a_new_process(void) __attribute__((noreturn));

static void SET_PROCESS_STATUS(struct proc_base *p, int s)
{
  if (GET_PROCESS_STATUS(p) == s) return;
  
  ASSIGN(p->status,(p->status & ~PROCESS_STATUS) | s);
}

/******************************************************************************/
/*                                                                            */
/*      CHANNELS                                                              */
/*                                                                            */
/******************************************************************************/

static ProcessHandle synchronise(process_base **wchain,
								 process_base **pchain, int ioCall);


/******************************************************************************/
/*                                                                            */
/*      send_on_channelc - called from sparc_assembly.s                       */
/*                                                                            */
/******************************************************************************/
/* CALL_IO2(send_on_channel, REF, REF, NOIND) */
word send_on_channelc(Handle val, ChanHandle chan)
/* Send a value on a channel. */
{
    ProcessHandle receiver;
    
    /* Save the value and channel in case we block. */
    ASSIGN(DEREFPROCHANDLE(processes)->block_data,DEREFHANDLE(val));
    ASSIGN(DEREFPROCHANDLE(processes)->block_channel,DEREFCHANHANDLE(chan));
    
    /* See if we can match up a receiver - if we cannot we do not return. */
    receiver = synchronise(&(DEREFCHANHANDLE(chan)->receivers),
                           &(DEREFCHANHANDLE(chan)->senders),
						   POLY_SYS_send_on_channel);
    
    /* Transfer the value. */
    ASSIGN(DEREFPROCHANDLE(receiver)->block_data,DEREFHANDLE(val));
    
    ASSIGN(DEREFPROCHANDLE(processes)->block_data,TAGGED(0));
    ASSIGN(DEREFPROCHANDLE(processes)->block_channel,NO_CHANNEL);
    return TAGGED(0);
}

/******************************************************************************/
/*                                                                            */
/*      receive_on_channelc - called from sparc_assembly.s                    */
/*                                                                            */
/******************************************************************************/
/* CALL_IO1(receive_on_channel, REF, NOIND) */
word receive_on_channelc(ChanHandle chan)
{
    ProcessHandle sender;
    
    /* Save the channel in case we block. */
    ASSIGN(DEREFPROCHANDLE(processes)->block_channel,DEREFCHANHANDLE(chan));
    
    /* See if we can match up a sender - if we cannot we do not return. */
    sender = synchronise(&(DEREFCHANHANDLE(chan)->senders),
                         &(DEREFCHANHANDLE(chan)->receivers),
						 POLY_SYS_receive_on_channel);
    return DEREFPROCHANDLE(sender)->block_data; /* Get the value. */
}

/******************************************************************************/
/*                                                                            */
/*      fork_proc - utility function - allocates in Poly heap                 */
/*                                                                            */
/******************************************************************************/
static ProcessHandle fork_proc(Handle proc, SynchroHandle synchro, int console, Handle arg)
/* Fork a new process.  "arg" is either nil or a function argument. */
{
    /* Load the closure and make the pc local. */
    ProcessHandle p_base = NEWPROCESSHANDLE();
       
    DEREFPROCHANDLE(p_base)->stack = NEWSTACK(INIT_STACK_SIZE);

    /* Get the frame ready to run. */
    MD_init_stack_frame(DEREFPROCHANDLE(p_base)->stack, proc, arg);
    
    DEREFPROCHANDLE(p_base)->status        = PROCESS_RUNABLE; /* Ready to run. */
    DEREFPROCHANDLE(p_base)->block_data    = TAGGED(0);
    DEREFPROCHANDLE(p_base)->block_channel = NO_CHANNEL;
    
    if (console)
      {
        DEREFPROCHANDLE(p_base)->status       |= PROCESS_IS_CONSOLE;
        DEREFPROCHANDLE(p_base)->console_link  = DEREFPROCHANDLE(console_chain);
        ASSIGN(DEREFPROCHANDLE(console_chain),DEREFPROCHANDLE(p_base));
      }
    else (DEREFPROCHANDLE(p_base))->console_link = NO_PROCESS;
    
    DEREFPROCHANDLE(p_base)->synchro = (synchro == 0 ? NO_SYNCH : DEREFSYNCHROHANDLE(synchro));
    add_process(p_base, PROCESS_RUNABLE); /* allocates */
    return p_base; 
}

/******************************************************************************/
/*                                                                            */
/*      make_new_root_proc - utility function - allocates in Poly heap        */
/*                                                                            */
/******************************************************************************/
ProcessHandle make_new_root_proc(Handle proc)
/* Create a root process for a new database. */
{
    /* Must allocate in the local area so it becomes part of the new database */
    ProcessHandle p_base = NEWPROCESSHANDLE();
      
    DEREFPROCHANDLE(p_base)->stack = NEWSTACK(INIT_STACK_SIZE);

    /* Get the frame ready to run. */
    MD_init_stack_frame(DEREFPROCHANDLE(p_base)->stack, proc, 0);

    DEREFPROCHANDLE(p_base)->status        = PROCESS_RUNABLE; /* Ready to run. */
    DEREFPROCHANDLE(p_base)->block_data    = TAGGED(0);
    DEREFPROCHANDLE(p_base)->block_channel = NO_CHANNEL;
    DEREFPROCHANDLE(p_base)->status       |= PROCESS_IS_CONSOLE;
    DEREFPROCHANDLE(p_base)->console_link  = NO_PROCESS;
    DEREFPROCHANDLE(p_base)->synchro       = NO_SYNCH;
    DEREFPROCHANDLE(p_base)->f_chain       = DEREFPROCHANDLE(p_base);
    DEREFPROCHANDLE(p_base)->b_chain       = DEREFPROCHANDLE(p_base);
    
    return p_base; 
}

/******************************************************************************/
/*                                                                            */
/*      PROCESSES                                                             */
/*                                                                            */
/******************************************************************************/

/******************************************************************************/
/*                                                                            */
/*      select_next_process - utility function - doesn't allocate             */
/*                                                                            */
/******************************************************************************/
void select_next_process(void)
/* Set up the next process in the chain to run when this returns. */
{
    int status;
    ASSIGN(DEREFPROCHANDLE(processes),
           DEREFPROCHANDLE(processes)->f_chain);
    status = GET_PROCESS_STATUS(DEREFPROCHANDLE(processes));

    if (status == PROCESS_IO_BLOCK || status == (PROCESS_IO_BLOCK | PROCESS_INTERRUPTABLE))
      {
        /* Try running it. If IO is not possible it will block immediately
           by calling process_may_block. (Unless a console interrupt has
           happened). */
#ifndef WINDOWS_PC
		int fd = UNTAGGED(DEREFPROCHANDLE(processes)->block_data);
		if (fd >= 0) FD_CLR(fd, &file_desc);
#endif
        no_waiting--;
        SET_PROCESS_STATUS(DEREFPROCHANDLE(processes),PROCESS_RUNABLE);
     }
    poly_stack   = DEREFPROCHANDLE(processes)->stack;
    end_of_stack = (word*)poly_stack + OBJECT_LENGTH(poly_stack);
    return;
}

/******************************************************************************/
/*                                                                            */
/*      add_process - utility function - allocates                            */
/*                                                                            */
/******************************************************************************/
void add_process(ProcessHandle p_base, int state)
/* Add a process to the runnable set. */
{
  SET_PROCESS_STATUS(DEREFPROCHANDLE(p_base),state); /* Either runable or unblocked. */
  
  fix_process_addresses(p_base); /* allocates */
  
  if (DEREFPROCHANDLE(processes) == NO_PROCESS)
  { /* First process. */
    /* Put THIS(p_base) process in.
       The processes are on a doubly linked list. 
  
           BEFORE : -> PROCESSES  ---> NO_PROCESS 
                    -- #########   

                                                          (1)             (2)
            AFTER : -> PROCESSES  --->  THIS  --\ 
                                -- #########   /--  ####  <-/  
                                                           \--->(3)               
     */
      ASSIGN(DEREFPROCHANDLE(processes),DEREFPROCHANDLE(p_base));
      ASSIGN(DEREFPROCHANDLE(p_base)->f_chain,DEREFPROCHANDLE(p_base));
      ASSIGN(DEREFPROCHANDLE(p_base)->b_chain,DEREFPROCHANDLE(p_base));
  }
  else
  { 
    /* Put THIS(p_base) process in after the current one.
       The processes are on a doubly linked list. 
  
           BEFORE : -> PROCESSES  --->  CURRENT -- 
                    -- #########  <---  ####### <-

                                                          (1)             (2)
            AFTER : -> PROCESSES  --->  THIS  --->  CURRENT -- 
                                -- #########  <---  ####  <---  ####### <-
                                                           (3)             (4)
     */
      ASSIGN(DEREFPROCHANDLE(p_base)->b_chain,DEREFPROCHANDLE(processes));
      ASSIGN(DEREFPROCHANDLE(p_base)->f_chain,DEREFPROCHANDLE(processes)->f_chain);
      ASSIGN(DEREFPROCHANDLE(processes)->f_chain,DEREFPROCHANDLE(p_base));
      ASSIGN(DEREFPROCHANDLE(p_base)->f_chain->b_chain,DEREFPROCHANDLE(p_base));
  }
  
  no_of_processes++;
  return;
}

/******************************************************************************/
/*                                                                            */
/*      remove_process - utility function - doesn't allocate                  */
/*                                                                            */
/******************************************************************************/
void remove_process(process_base *to_kill)
/* Remove a process from the runable set. May be because it has been killed
   or because it is waiting for an event. */
{
    /* Select a different process if this is the active one. */
    if (to_kill == DEREFPROCHANDLE(processes))
      {
        ASSIGN(DEREFPROCHANDLE(processes),to_kill->b_chain);
      }
    
    /* Select this process if it is waiting for an event.
                         -> ->                       ---->
           BEFORE : O  O  O     AFTER : O  ?  O
                         <-     <-                           <----
         */
    /* May already be waiting for an event. */
    if (to_kill->f_chain != NO_PROCESS && to_kill->b_chain != NO_PROCESS)
      {
        /* Else - remove it from the chain. */
        ASSIGN(to_kill->f_chain->b_chain,to_kill->b_chain);
        ASSIGN(to_kill->b_chain->f_chain,to_kill->f_chain);
        no_of_processes--;

        if (no_of_processes == 0)
        {
           ASSIGN(DEREFPROCHANDLE(processes),NO_PROCESS);
        }
      }
    return;
}

/******************************************************************************/
/*                                                                            */
/*      fork_processc - called from sparc_assembly.s - allocates              */
/*                                                                            */
/******************************************************************************/
/* CALL_IO2(fork_process, REF, REF, IND) */
ProcessHandle fork_processc(Handle console_handle, Handle proc) /* Handle to (process_base *) */
/* Fork a new process. Returns a reference to the process base. */
{
    int console = get_C_long(DEREFWORDHANDLE(console_handle));

    SynchroHandle synchro = SAVE(DEREFPROCHANDLE(processes)->synchro);
    
    if (DEREFSYNCHROHANDLE(synchro) != NO_SYNCH &&
        DEREFSYNCHROHANDLE(synchro)->synch_type != synch_par)
    
       {
        SynchroHandle new_sync = NEWSYNCHROHANDLE();
            
        DEREFSYNCHROHANDLE(new_sync)->synch_type = synch_par;
        DEREFSYNCHROHANDLE(new_sync)->synch_base = DEREFSYNCHROHANDLE(synchro);
        
        ASSIGN(DEREFPROCHANDLE(processes)->synchro,DEREFSYNCHROHANDLE(new_sync));
        synchro = new_sync;
      }
        
    return fork_proc(proc, synchro, console, 0);
}

/******************************************************************************/
/*                                                                            */
/*      choice_processc - called from sparc_assembly.s                        */
/*                                                                            */
/******************************************************************************/
/* CALL_IO2(choice_process, REF, REF, NOIND) */
word choice_processc(Handle proc1, Handle proc2)
/* Fork two new processes, but only allow them to run until the first
   successful read or write. The parent is not affected. */
{
  SynchroHandle synchro = SAVE(DEREFPROCHANDLE(processes)->synchro);
  SynchroHandle new_sync;
  
  /* If this is already a choice we have to run the new processes in parallel,
     with the parent. */
     
  if (DEREFSYNCHROHANDLE(synchro) != NO_SYNCH &&
      DEREFSYNCHROHANDLE(synchro)->synch_type != synch_par)
    {
      new_sync = NEWSYNCHROHANDLE();
      DEREFSYNCHROHANDLE(new_sync)->synch_type = synch_par;
      DEREFSYNCHROHANDLE(new_sync)->synch_base = DEREFSYNCHROHANDLE(synchro);
      
      ASSIGN(DEREFPROCHANDLE(processes)->synchro,DEREFSYNCHROHANDLE(new_sync));
      synchro = new_sync;
    }
    
  /* Now the synchroniser for the new processes. */
  new_sync = NEWSYNCHROHANDLE();
  
  DEREFSYNCHROHANDLE(new_sync)->synch_type = synch_choice;
  DEREFSYNCHROHANDLE(new_sync)->synch_base = DEREFSYNCHROHANDLE(synchro);
  fork_proc(proc1, new_sync, 0, 0);
  fork_proc(proc2, new_sync, 0, 0);

  return TAGGED(0);
}

/******************************************************************************/
/*                                                                            */
/*      check_current_stack_size - utility function- allocates in Poly heap   */
/*                                                                            */
/******************************************************************************/
void check_current_stack_size(word *lower_limit)
/* Expands the current stack if it has grown. We cannot shrink a stack segment
   when it grows smaller because the frame is checked only at the beginning of
   a procedure to ensure that there is enough space for the maximum that can
   be allocated. */
{
    int old_len; /* Current size of stack segment. */
    int new_len; /* New size */
    int min_size; /* This is the minimum space required on the stack. */
    StackObject *new_stack;

    /* Get current size of new stack segment. */
    old_len = OBJECT_LENGTH(poly_stack);
 
    /* The minimum size must include the reserved space for the registers. */
    min_size = end_of_stack - lower_limit + poly_stack->p_space;
    
    if (old_len >= min_size) return; /* Ok with present size. */

    /* If it is too small double its size. */
    /* BUT, the maximum size is 2^24 - 1 words. */

#define MAXIMUM_STACK	OBJ_PRIVATE_LENGTH_MASK

    if (old_len == MAXIMUM_STACK)
    {
	/* Cannot expand the stack any further. */
	proper_fprintf(stderr, "Warning - Stack limit reached - interrupting process\n");
	MD_set_exception(poly_stack,interrupt_exn);
	return;
    }

    for (new_len = old_len; new_len < min_size; new_len *= 2);
    if (new_len > MAXIMUM_STACK) new_len = MAXIMUM_STACK;

#if (0)
    /* Suppress this message.  It seems to cause problems. */
    if (new_len >= (1 << 16))
    {
		/* DCJM 23/3/04. This is now sent to stderr rather than stdout.
		  (suggestion from David Aspinall) */
      proper_fprintf(stderr,"Warning - Increasing stack from %d to %d bytes\n",
                               old_len * sizeof(word), new_len * sizeof(word));
	  proper_fflush(stderr);
    }
#endif

    /* Must make a new frame and copy the data over. */
    new_stack = NEWSTACK(new_len);
    CopyStackFrame(poly_stack,new_stack);
    /* Now change the frame into a byte segment. The reason for this is
       that if the frame was read from persistent store it will be written
       back when the database is committed even though it has been superceded
       by a new frame. This will result in all the values accessible from the
       old stack being written back even though they may not be required any
       longer. Changing it into a byte segment will cause the garbage collector
       and the persistent store system to ignore the contents. The stack
       will be written back but not the values referred to from it.
       NB - must remain a mutable segment. If it is the first item in a
       persistent storage segment the mutable bit is used to see if the
       segment should be removed from the mutable map after committing. */ 
    ((word*)poly_stack)[-1] = (((word*)poly_stack)[-1] & ~OBJ_STACK_BIT) | OBJ_BYTE_BIT;
    poly_stack = new_stack;
    end_of_stack = (word*)new_stack + new_len;
    
    ASSIGN((DEREFPROCHANDLE(processes))->stack,poly_stack);
    return;
}

/******************************************************************************/
/*                                                                            */
/*      shrink_stack - called from sparc_assembly.s - allocates               */
/*                                                                            */
/******************************************************************************/
word shrink_stack_c(Handle reserved_space)
/* Shrinks the current stack. */
{
    int reserved = get_C_long(DEREFWORDHANDLE(reserved_space));

    int old_len; /* Current size of stack segment. */
    int new_len; /* New size */
    int min_size;
    StackObject *new_stack;

    if (reserved < 0)
    {
       raise_exception0(EXC_size);
    }

    /* Get current size of new stack segment. */
    old_len = OBJECT_LENGTH(poly_stack);
 
    /* The minimum size must include the reserved space for the registers. */
    min_size = (end_of_stack - poly_stack->p_sp) + poly_stack->p_space + reserved;
    
    for (new_len = INIT_STACK_SIZE; new_len < min_size; new_len *= 2);

    if (old_len <= new_len) return TAGGED(0); /* OK with present size. */

    /* Must make a new frame and copy the data over. */
    new_stack = NEWSTACK(new_len);
    CopyStackFrame(poly_stack,new_stack);
    /* Now change the frame into a byte segment. The reason for this is
       that if the frame was read from persistent store it will be written
       back when the database is committed even though it has been superceded
       by a new frame. This will result in all the values accessible from the
       old stack being written back even though they may not be required any
       longer. Changing it into a byte segment will cause the garbage collector
       and the persistent store system to ignore the contents. The stack
       will be written back but not the values referred to from it.
       NB - must remain a mutable segment. If it is the first item in a
       persistent storage segment the mutable bit is used to see if the
       segment should be removed from the mutable map after committing. */ 
    ((word*)poly_stack)[-1] = (((word*)poly_stack)[-1] & ~OBJ_STACK_BIT) | OBJ_BYTE_BIT;
    poly_stack = new_stack;
    end_of_stack = (word*)new_stack + new_len;
    
    ASSIGN((DEREFPROCHANDLE(processes))->stack,poly_stack);
    return TAGGED(0);
}

/******************************************************************************/
/*                                                                            */
/*      int_processc - called from sparc_assembly.s - allocates               */
/*                                                                            */
/******************************************************************************/
/* CALL_IO1(int_process, REF, NOIND) */
word int_processc(ProcessHandle proc)
/* Raise a console interrupt in a specified process. */
{

  if (GET_PROCESS_STATUS(DEREFPROCHANDLE(proc)) == PROCESS_BLOCKED) /* blocked on channel */
  {
    process_base **p; /* NOT a handle */
      
    /* Remove this process from this senders and receivers for that channel */
    
    for (p = &(DEREFPROCHANDLE(proc)->block_channel->senders);
         *p != NO_PROCESS;
         p = &((*p)->b_chain))
    {
      if (*p == DEREFPROCHANDLE(proc))
      {
        ASSIGN(*p,(*p)->b_chain);
        break;
      }
    }
      
    for (p = & DEREFPROCHANDLE(proc)->block_channel->receivers;
         *p != NO_PROCESS;
         p = & (*p)->b_chain)
    {
      if (*p == DEREFPROCHANDLE(proc))
      {
        ASSIGN(*p,(*p)->b_chain);
        break;
      }
    }
      
    add_process(proc, PROCESS_RUNABLE); /* allocates */
  }
    
  MD_set_exception(DEREFPROCHANDLE(proc)->stack,interrupt_exn);

  return TAGGED(0);
}

/******************************************************************************/
/*                                                                            */
/*      interrupt_console_processes_c - called from sparc_assembly.s          */
/*                                                                            */
/******************************************************************************/
/* CALL_IO0(interrupt_console_processes_,NOIND) */
/* int interrupt_console_processes_c() */
void interrupt_console_processes_c(void)
{
  interrupt_console_processes();
  RE_ENTER_POLY(777);
  /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      install_rootc - called from sparc_assembly.s                          */
/*                                                                            */
/******************************************************************************/
/* CALL_IO1(install_root, REF, NOIND) */
void install_rootc(Handle proc)
/* Installs a new procedure as the root. This involves killing all the processes
   and starting a new process with this root. */
{
    ASSIGN(DEREFPROCHANDLE(processes),NO_PROCESS);
    ASSIGN(DEREFPROCHANDLE(console_chain),NO_PROCESS);
    no_waiting = 0;
    no_of_processes = 0;
    fork_proc(proc, (SynchroHandle)0, 1, 0);
    select_a_new_process();
    /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      kill_process - utility function - doesn't allocate                    */
/*                                                                            */
/******************************************************************************/
void kill_process(process_base *to_kill)
/* Kill a process. Used when a process dies or when it tries to do a transfer
   after a different choice has already been taken. There is no mechanism
   for one process to kill another. */
{
    /* Remove process to_kill from the process chain. */
    remove_process(to_kill);
    
    if (to_kill->status & PROCESS_IS_CONSOLE)
      {
        /* Remove it from the console chain. */
        process_base **consoles /* NOT a handle */
           = (process_base **)console_chain;
        
        while(*consoles != NO_PROCESS && *consoles != to_kill)
          {
            consoles = &(*consoles)->console_link;
          }
          
        if (*consoles != NO_PROCESS) 
         {
           ASSIGN(*consoles,(*consoles)->console_link);
         }
     }
    return;
}

/******************************************************************************/
/*                                                                            */
/*      interrupt_console_processes - utility function - allocates            */
/*                                                                            */
/******************************************************************************/
void interrupt_console_processes(void)
{
  /* changed 8/11/93 SPF - now uses handles for safety */
  ProcessHandle p;
  
  for (p = SAVE(DEREFPROCHANDLE(console_chain));
       DEREFPROCHANDLE(p) != NO_PROCESS;
       DEREFPROCHANDLE(p) = DEREFPROCHANDLE(p)->console_link)
    {
      int_processc(p); /* allocates! */
    }
}

/******************************************************************************/
/*                                                                            */
/*      interrupt_signal_processes - utility function - allocates             */
/*                                                                            */
/******************************************************************************/
/* When a signal is received any process which is waiting for a Posix system
   call should instead be interrupted as though the call had failed with
   EINTR.  At the moment this is limited to a few special cases. */
void interrupt_signal_processes(void)
{
	ProcessHandle p;
	Handle exc =
		make_exn(EXC_syserr, create_syscall_exception("Call interrupted by signal", EINTR));

	assert(no_of_processes > 0);

	p = SAVE(DEREFPROCHANDLE(processes));
	do 
	{
		if ((GET_PROCESS_STATUS(DEREFPROCHANDLE(p)) & PROCESS_INTERRUPTABLE) ==
			PROCESS_INTERRUPTABLE)
		{
			SET_PROCESS_STATUS(DEREFPROCHANDLE(p), PROCESS_RUNABLE);
			MD_set_exception(DEREFPROCHANDLE(p)->stack,
				DEREFEXNHANDLE(exc));
		}
		DEREFPROCHANDLE(p) = DEREFPROCHANDLE(p)->f_chain;
	} while (DEREFPROCHANDLE(p) != DEREFPROCHANDLE(processes));
}



/******************************************************************************/
/*                                                                            */
/*      select_a_new_process - utility function - allocates                   */
/*                                                                            */
/******************************************************************************/
void select_a_new_process(void)
/* The current process has blocked. Find something else to do. */
{
  /* Deal with any pending interrupts. */
  execute_pending_interrupts();
  
  /* Should we block the Unix process? */
  if (no_of_processes == 0)
  { /* No processes able to run. */
    if (DEREFPROCHANDLE(console_chain) == NO_PROCESS)
    {
      finish(0);
    }
    
    proper_fputs("Processes have deadlocked. Interrupting console processes.\n",stdout);
    interrupt_console_processes();
  }

	if (no_waiting == no_of_processes)
	{
		/* All our processes are waiting for input or timers. We need
	       to block the OS process.  Ideally we would wait until one of
		   the events was ready and then return.  Once we return
		   select_next_process will schedule the next process in the
	       chain.  If that is not ready for IO now or its timer has
	       not yet expired we'll come back here, possibly block again,
	       and then schedule another process.  We can do that approximately
	       in Unix when we are reading by passing in the file descriptors.
		   We use "select" to check all the file descriptors for processes
	       which have blocked on reading.  select_next_process removes the
	       file descriptor from the set before retrying a process.  If the
		   process we start isn't the one that's ready select will return
		   immediately when we come here and we'll ripple through the processes
		   until we come to the right one.  It's only approximate because
		   we ignore blocking on writing, reading high-priority data from
	       sockets, blocking in "poll", blocking on a timer or the possibility
		   that there might be two processes both trying to read from the
		   same stream.  We deal with all of these by having a timer and
	       retrying whenever the timer goes off.  */
#ifdef WINDOWS_PC
		/* It's too complicated in Windows to try and wait for input.
		   We simply wait for half a second or until some input arrives.
		   If the stop event is signalled we exit. */

		{
			/* We seem to need to reset the queue before calling
			   MsgWaitForMultipleObjects otherwise it frequently returns
			   immediately, often saying there is a message with a message ID
			   of 0x118 which doesn't correspond to any listed message.
			   While calling PeekMessage afterwards might be better this doesn't
			   seem to work properly.  We need to use MsgWaitForMultipleObjects
			   here so that we get a reasonable response with the Windows GUI. */
			MSG msg;
			PeekMessage(&msg, 0, 0, 0, PM_NOREMOVE);

			switch (MsgWaitForMultipleObjects(1, &hStopEvent, FALSE, 500, QS_ALLINPUT))
			{
			case WAIT_OBJECT_0: /* stopEvent has been signalled. */ ExitThread(0);
			case WAIT_OBJECT_0 + 1: /* new input is available. */
				break;
			}
		}
#else
		fd_set read_fds, write_fds, except_fds;
		struct timeval toWait = { 0, 500000 }; /* Half a second. */

		/* copy file_desc because select modifies its arguments */
		read_fds = file_desc;
		FD_ZERO(&write_fds);
		FD_ZERO(&except_fds);
		select(FD_SETSIZE, &read_fds, &write_fds, &except_fds, &toWait);
		/* select may also return if our timer goes off or if we've had ctrl-C. */
#endif
  }

  /* Do another process. */
  select_next_process();
  RE_ENTER_POLY(666);
  /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      kill_selfc - called from sparc_assembly.s                             */
/*                                                                            */
/******************************************************************************/
/* CALL_IO0(kill_self, NOIND) */
void kill_selfc(void)
/* A call to this is put on the stack of a new process so when the procedure
   returns the process goes away. */  
{
    kill_process(DEREFPROCHANDLE(processes));
    select_a_new_process();
   /*NOTREACHED*/
}


#ifndef WINDOWS_PC
void process_may_block(int fd, int ioCall)
/* A process is about to read from a file descriptor which may cause it to
   block. It returns only if the process will not block. Otherwise it waits
   for a signal such as SIGALRM. */
{
      static struct timeval poll = {0,0};
      /* 
        AIX doesn't document support for NULL pointers in the select call,
         so we have to initialise empty fd_sets instead. SPF 30/10/95
      */
      fd_set read_fds, write_fds, except_fds;
      int selRes;
      FD_ZERO(&write_fds);
      FD_ZERO(&except_fds);
  
      FD_ZERO(&read_fds);
      FD_SET((int)fd,&read_fds);

      /* If there is something there we can return. */
      selRes = select(FD_SETSIZE,&read_fds,&write_fds,&except_fds,&poll);
	  if (selRes > 0) return; /* Something waiting. */
	  else if (selRes < 0 && errno != EINTR) crash("select failed %d\n", errno);
	  /* Have to block unless this is the only process. */
	  block_and_restart_if_necessary(fd, ioCall);
}

#endif

void block_and_restart(int fd, int interruptable, int ioCall)
/* The process is waiting for IO or for a timer.  Suspend it and
   then restart it later.  fd may be negative if the file descriptor
   value is not relevant.
   If this is interruptable (currently only used for Posix functions)
   the process will be set to raise an exception if a signal is handled. */
{
	int status = PROCESS_IO_BLOCK;
	if (interruptable) status |= PROCESS_INTERRUPTABLE;

    SET_PROCESS_STATUS(DEREFPROCHANDLE(processes), status);
    ASSIGN((DEREFPROCHANDLE(processes))->block_data,TAGGED((int)fd));
    
    /* Suspend this process for the moment. */
    MD_set_for_retry(ioCall);
    no_waiting++;
#ifndef WINDOWS_PC
	/* Add the file descriptor to the set.  fd may be < 0 if the reason
	   we blocked was other than for reading. */
    if (fd >= 0) FD_SET(fd,&file_desc);
#endif
    select_a_new_process();
	/* NOTREACHED */
}

void block_and_restart_if_necessary(int fd, int ioCall)
/* Similar to block_and_restart except that this can return if there
   is only one process.  It can be called before a system call, e.g.
   "read", so that we can block if there is nothing else to do. */
{
    /*
       Original comment:
         If there is just one process and no file descriptors being
         looked after by routines called from "execute_pending_interrupts"
         it does not matter if we block. We can simply return. Doing this
         allows the system to work if the process that is sending us the data
         is using select to see if we are actually reading.
	  SPF added a comment to this concerning AHL's licence manager. Essentially
	  it is safe to block only if there is no other activity which is time
	  critical.
	  In the Windows version we can't block because we handle control-C
	  as a synchronous interrupt.  There may be similar constraints in
	  Unix so it's simpler never to block.
	  DCJM May 2000.
	*/
	/* if (no_of_processes == 1) return; */

	block_and_restart(fd, 0, ioCall);
	/* NOTREACHED */
}

/******************************************************************************/
/*                                                                            */
/*      process_can_do_transfer - utility function - doesn't allocate         */
/*                                                                            */
/******************************************************************************/
static int process_can_do_transfer(process_base *proc)
/* Can a process do a read or a write? */
{
    synchroniser *synchr;
    for (synchr = proc->synchro; synchr != NO_SYNCH; synchr = synchr->synch_base)
      {
        if (synchr->synch_type == synch_taken) return 0; /* Must kill it. */
      }
    return 1; /* Ok. */
}

/******************************************************************************/
/*                                                                            */
/*      accept_this_choice - utility function - doesn't allocate              */
/*                                                                            */
/******************************************************************************/
static void accept_this_choice(process_base *proc)
/* Decide to accept this choice. Set all "choices" to "taken", and set
   all the links to zero so that "pars" in this choice can continue. */
{
   synchroniser *synchro, *next_sync;
   
   for (synchro = proc->synchro; synchro != NO_SYNCH; synchro = next_sync)
     {
        if (synchro->synch_type == synch_choice)
          {
            ASSIGN(synchro->synch_type,synch_taken);
          }
        next_sync = synchro->synch_base;
        ASSIGN(synchro->synch_base,NO_SYNCH);
     }
   /* This process can continue without further problems. */
   ASSIGN(proc->synchro,NO_SYNCH);
   return;
}

/******************************************************************************/
/*                                                                            */
/*      separate_choices - utility function - doesn't allocate                */
/*                                                                            */
/******************************************************************************/
static int separate_choices(process_base *x, process_base *y)
/* If we have two process wanting to exchange information we cannot do it
   if they are alternative choices. */ 
{
    synchroniser *sx, *sy;
    
    for(sx = x->synchro; sx != NO_SYNCH; sx = sx->synch_base)
        for(sy = y->synchro; sy != NO_SYNCH; sy = sy->synch_base)
          {
            if (sx == sy) /* Found the common element. */
                return (sx->synch_type == synch_par); /* Ok if parallel. */
          }
    return 1; /* Nothing in common - ok */
} 


/******************************************************************************/
/*                                                                            */
/*      synchronise - utility function - allocates                            */
/*                                                                            */
/******************************************************************************/
static ProcessHandle synchronise(process_base **wchain, process_base **pchain, int ioCall)
/* Tries to find a process to match with this one, and adds the current process
   to the waiting list if it can't find one. Normally if there is a process on
   the "wchain" it will be the one that will be chosen, however it may be that
   this process depends on a choice which has been taken by another process, in
   which case the process is removed. Alternatively it may be that the current
   process is an alternative choice to it so must not be allowed to match
   i.e. we may have    choice(send X..., receive X ...). */
/* wchain is the chain to search for a match. */
/* pchain is the chain to add to if there isn't a match. */

/* N.B. wchain and pchain are NOT handles, they are merely the addresses      */
/* of a couple of (process_base *) pointers, which may themselves be embedded */
/* inside a Poly/ML heap object. The extra level of indirection is because    */
/* we are doing imperative list processing.                                   */
{
    process_base *this_process;

    /* If there is no process waiting to receive the data we must block until
       someone wants it. This means that some time later will be restarted
       and re-enter this procedure. We need to distinguish the cases. */
    if (GET_PROCESS_STATUS(DEREFPROCHANDLE(processes)) == PROCESS_UNBLOCKED)
      {
        SET_PROCESS_STATUS(DEREFPROCHANDLE(processes),PROCESS_RUNABLE);
        return processes;
      }

    /* Can we do send or receive? - start a new process if it cannot */
    if (!process_can_do_transfer(DEREFPROCHANDLE(processes)))
      {
        kill_process(DEREFPROCHANDLE(processes)); /* Choice has been taken already. */
        select_a_new_process();
        /*NOTREACHED*/
      }

    /* Find a receiver/sender on the wait chain which is not in common with the
       current process. */
    while(*wchain != NO_PROCESS /* Until the end of the list or we return */)
      {
        if (!(process_can_do_transfer(*wchain)))
          {
            /* Remove it if an alternative has already been chosen. */
            process_base *next = (*wchain)->b_chain; /* Save because kill unchains it. */
            kill_process(*wchain);
            ASSIGN(*wchain,next);
          }
        else if (!(separate_choices(DEREFPROCHANDLE(processes), *wchain)))
          {
            /* These are alternative choices - cannot match. */
            wchain = &((*wchain)->b_chain);
          }
        else
          {
            ProcessHandle part = SAVE(*wchain);
            
            /* Remove it from the chain. */
            ASSIGN(*wchain,(*wchain)->b_chain);
            ASSIGN(DEREFPROCHANDLE(part)->b_chain,NO_PROCESS);
            ASSIGN(DEREFPROCHANDLE(part)->f_chain,NO_PROCESS);
            
            /* This choice is taken for both sides. */
            accept_this_choice(DEREFPROCHANDLE(processes));
            accept_this_choice(DEREFPROCHANDLE(part));
            
            /* Unblock and put back onto chain. */
            add_process(part,PROCESS_UNBLOCKED); /* allocates */
            return part; /* Exit with the process we have found. */
          }
    }

    /* No-one waiting. Schedule something else.  - does not return. */
    this_process = DEREFPROCHANDLE(processes);
    SET_PROCESS_STATUS(this_process, PROCESS_BLOCKED);
    MD_set_for_retry(ioCall);
    remove_process(this_process);
    
    /* Add to the end of the chain of senders/receivers. While we are chaining
       down we remove any that can no longer run. */
    while(*pchain != NO_PROCESS)
      {
        if (process_can_do_transfer(*pchain))
          {
            pchain = &((*pchain)->b_chain);
          }
        else
          { /* Get the next process and kill this one. */

            process_base *next = (*pchain)->b_chain;
            kill_process(*pchain);
            ASSIGN(*pchain,next);
          }
     }
     
    ASSIGN(*pchain,this_process);
    ASSIGN(this_process->f_chain,NO_PROCESS);
    ASSIGN(this_process->b_chain,NO_PROCESS);
    select_a_new_process();
    /* NOTREACHED */
}

/******************************************************************************/
/*                                                                            */
/*      set_process_list - called from mpoly.c - allocates                    */
/*                                                                            */
/******************************************************************************/
void set_process_list(root_struct *root)
/* Sets the initial process list when the system is entered. */
{
   /* now use handles and an explicit dereference, because              */
   /* fix_process_addresses may cause a garbage collection. 9/11/93 SPF */
    ProcessHandle procHandle = SAVE(root->process_list);

    processes = (Handle)&(root->process_list); /* SAFE??? */
    console_chain = (Handle)&(root->console_chain); /* SAFE??? */
    no_waiting = 0;
    no_of_processes = 0;
#ifndef WINDOWS_PC
    FD_ZERO(&file_desc);
#endif
    
    /* Count the processes and fix up the addresses. */
    do {
        fix_process_addresses(procHandle); /* allocates */
        no_of_processes++;
        
        /* Make them all runnable. If they are blocked waiting for streams
           other than stdin or stdout they will get an exception as soon as
           they try to read. */
        SET_PROCESS_STATUS(DEREFPROCHANDLE(procHandle), PROCESS_RUNABLE);
        DEREFPROCHANDLE(procHandle) = DEREFPROCHANDLE(procHandle)->f_chain;
    } while (DEREFPROCHANDLE(procHandle) != DEREFPROCHANDLE(processes));
    
    /* Now the console list. This is needed because we cannot risk bringing
       the processes into local memory in "run_time_interrupts". */
    
    for(DEREFPROCHANDLE(procHandle) = DEREFPROCHANDLE(console_chain); 
        DEREFPROCHANDLE(procHandle) != NO_PROCESS;
        DEREFPROCHANDLE(procHandle) = DEREFPROCHANDLE(procHandle)->console_link)
    {
      fix_process_addresses(procHandle);  /* allocates */
    }
    
    return;
}

/******************************************************************************/
/*                                                                            */
/*      fix_process_addresses - utility function - allocates                  */
/*                                                                            */
/******************************************************************************/
/* Now uses a Handle for safety - SPF 9/11/93 */
static void fix_process_addresses(ProcessHandle proc)
{
    int pbaseLen = OBJECT_LENGTH(DEREFWORDHANDLE(proc));
    /* DCJM 8/4/04.  The process base has now grown and we may have to convert
       process bases which were previously in the database.
	   fix_process_addresses is called for the initial processes and for
	   the processes on the console chain and for other processes as they are
	   added to the run queue. */
    if (pbaseLen < sizeof(process_base)/sizeof(word)) {
        /* Copy the process base into new memory. */
        int i;
        ProcessHandle pNew = NEWPROCESSHANDLE();
		process_base **p;
        for (i = 0; i < sizeof(process_base)/sizeof(word); i++) {
		    DEREFWORDHANDLE(pNew)[i] = TAGGED(0);
        }
        memcpy(DEREFHANDLE(pNew), DEREFHANDLE(proc), pbaseLen*sizeof(word));

		/* Update pointers in the runnable process list. */
		p = &DEREFPROCHANDLE(processes);
		do {
			if (*p == DEREFPROCHANDLE(proc))
				ASSIGN(*p, DEREFPROCHANDLE(pNew));
			/* Check the forward and backward pointers and the console chain. */
		    if ((*p)->f_chain == DEREFPROCHANDLE(proc))
		        ASSIGN((*p)->f_chain, DEREFPROCHANDLE(pNew));
		    if ((*p)->b_chain == DEREFPROCHANDLE(proc))
		        ASSIGN((*p)->b_chain, DEREFPROCHANDLE(pNew));
            p = &((*p)->f_chain);
		} while (*p != DEREFPROCHANDLE(processes));

		/* Update pointers in the console chain. */
		for (p = &DEREFPROCHANDLE(console_chain); *p != NO_PROCESS; p = &((*p)->console_link))
		{
			if (*p == DEREFPROCHANDLE(proc))
		       ASSIGN(*p, DEREFPROCHANDLE(pNew));
		}

		/* Update pointers if this is blocked on a channel.  That could happen for processes
		   on the console chain.  */
		if (DEREFPROCHANDLE(pNew)->block_channel != NO_CHANNEL) {
			for (p = &(DEREFPROCHANDLE(pNew)->block_channel->senders); *p != NO_PROCESS; p = &((*p)->b_chain))
			{
				if (*p == DEREFPROCHANDLE(proc))
					ASSIGN(*p, DEREFPROCHANDLE(pNew));
			}

			for (p = &(DEREFPROCHANDLE(pNew)->block_channel->receivers); *p != NO_PROCESS; p = &((*p)->b_chain))
			{
				if (*p == DEREFPROCHANDLE(proc))
					ASSIGN(*p, DEREFPROCHANDLE(pNew));
			}
		}
		*proc = *pNew; /* Set the handle to the updated address. */
    }
	{
        /* copy_mapped_stack may allocate storage */
        StackObject *newstack = copy_mapped_stack(DEREFPROCHANDLE(proc)->stack);
  
        ASSIGN(DEREFPROCHANDLE(proc)->stack,newstack);
	}
}

/******************************************************************************/
/*                                                                            */
/*      install_subshells_c - called from sparc_assembly.s                    */
/*                                                                            */
/******************************************************************************/
/* CALL_IO1(install_subshells_, REF, NOIND) */
word install_subshells_c(Handle root_function)
{
  
  root_struct *root = Root(H);
  
  if ((word)DEREFHANDLE(root_function) == TAGGED(0))
  {
    /* kill existing alternate subshell processes */
    ASSIGN(root->alt_process_list, 0);
    ASSIGN(root->alt_console_chain,0);
  }
  else
  {
    /* Create new process and install it as alternate subshell */
    ProcessHandle root_process = make_new_root_proc(root_function);
    
    /* Store away these two pointers so that they can */
    /* be used to create a subshell at a later time.  */
    
    if (OBJECT_LENGTH(root) != 255)
    {
      proper_fputs("Cannot install subshells\n",stdout);
      return TAGGED(0);
    }
    
    ASSIGN(root->alt_process_list, DEREFPROCHANDLE(root_process));
    ASSIGN(root->alt_console_chain,DEREFPROCHANDLE(root_process));
  }
  
  return TAGGED(0);
}

/******************************************************************************/
/*                                                                            */
/*      switch_subshells_c                                                    */
/*                                                                            */
/******************************************************************************/
/* CALL_IO0(switch_subshells_,NOIND) */
word switch_subshells_c(void)
{
  root_struct *root = Root(H);
  
  if ((word)root->alt_process_list == 0)        /* right test? */
  {
    proper_fputs("Subshells have not been installed\n",stdout);
    return TAGGED(0);
  }
  
#define Swap(a,b) {process_base *t = a; ASSIGN(a,b); ASSIGN(b,t);}
  Swap(root->process_list, root->alt_process_list);
  Swap(root->console_chain,root->alt_console_chain);
#undef Swap
  
  set_process_list(root);
  select_a_new_process();
  /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      fork_function                                                         */
/*                                                                            */
/******************************************************************************/
/* fork_function.  Creates a new process from within the RTS. */
ProcessHandle fork_function(Handle proc, Handle arg)
{
	return fork_proc(proc, (SynchroHandle)0, 0, arg);
}

/******************************************************************************/
/*                                                                            */
/*      process_gc - utility function, called indirectly                     */
/*                                                                            */
/******************************************************************************/
static void process_gc(GCOpFunc op)
/* Ensures that all the objects are retained and their addresses updated. */
{
    
    /* Processes */
    /* If we get a garbage-collection while loading the initial data we may
       find these are still zero. */
#ifdef NEVER
    if (processes != NULL)     (*op)(processes, 0);
    if (console_chain != NULL) (*op)(console_chain, 0);
#endif

    if (poly_stack != 0)
      {
        (*op)((word **)&poly_stack, 0);
        end_of_stack = (word*)poly_stack + OBJECT_LENGTH(poly_stack);
      }

    /* The interrupt exn */
    
    (*op)((word **)&interrupt_exn,0);

    return;
}


/******************************************************************************/
/*                                                                            */
/*      catchALRM - utility function - doesn't allocate                       */
/*        called when time-slice expired (1Hz) - signal sigALRM               */
/*                                                                            */
/******************************************************************************/

#if defined(WINDOWS_PC)
DWORD WINAPI ProcessTimeOut(LPVOID parm)
{
	// Go round this loop every timeslice until the stop event is signalled.
	while (WaitForSingleObject(hStopEvent, A.timeslice) == WAIT_TIMEOUT)
	{
		interrupted=-1; /* Temporary to get run_time_interrupts called */
		MD_interrupt_code();
	}
	return 0;
} /* handleALRM on PC */

#else /* UNIX */

#if defined(LINUX)
static void catchALRM(int sig, struct sigcontext context)
#elif defined(SOLARIS2) || defined(HPUX64)
static void catchALRM(int sig, siginfo_t *info, ucontext_t *scp)
#else
static void catchALRM(int sig, int code, struct sigcontext *scp)
#endif
/* Called to request a call to the entries in 
   int_procs some time later. */
{
  assert(sig == SIGALRM); /* SPF */
  interrupted = sig;
#if defined(LINUX)
  MD_interrupt_code_using_context(&context);
#else
  MD_interrupt_code_using_context(scp);
#endif
  return;
}

#endif /* UNIX */

/******************************************************************************/
/*                                                                            */
/*      start_interrupt_timer - utility function - doesn't allocate           */
/*                                                                            */
/******************************************************************************/
static void start_interrupt_timer(void)
/* Start the interrupt timer.                                */
/* It is used to ensure that processes get a fair share of the machine. */
{
#if defined(WINDOWS_PC) /* PC version */
	DWORD dwId;
	hInterruptTimer = CreateThread(NULL, 0, ProcessTimeOut, NULL, 0, &dwId);
	if (hInterruptTimer == NULL)
	{
		proper_fputs("Creating Interrupt Timer thread failed.\n", stdout); 
	}
  
#else /* UNIX version */
    /* The interrupt frequency can now be set by the user */
    int seconds      = A.timeslice / 1000;
    int microseconds = (A.timeslice % 1000) * 1000;

    struct itimerval per_sec;
    per_sec.it_interval.tv_sec = seconds;
    per_sec.it_interval.tv_usec = microseconds;
    per_sec.it_value.tv_sec = seconds;
    per_sec.it_value.tv_usec = microseconds;
    
    setitimer(ITIMER_REAL, &per_sec, NULL);
#endif

    return;
}


void init_process_system(void)
{
	poly_stack = 0;
	end_of_stack = 0;
    RegisterGCProc(process_gc);
#ifdef WINDOWS_PC

	/* Create event to stop timeslice interrupts. */
	hStopEvent = CreateEvent(NULL, TRUE, FALSE, NULL);

#else /* Unix */
    /* Set up a handler for SIGALRM. SIGALRM
       is sent periodically when there are several processes running to ensure
       that each process gets a fair share of the machine. */
	markSignalInuse(SIGALRM);
	setSignalHandler(SIGALRM, (signal_handler_type)catchALRM);
#endif
    
    start_interrupt_timer();
}

void re_init_process_system(void)
{
	interrupt_exn = DEREFEXNHANDLE(make_exn(EXC_interrupt, SAVE(TAGGED(0))));
}

void uninit_process_system(void)
{
#ifdef WINDOWS_PC
	/* Stop the timer thread. */
	if (hStopEvent) SetEvent(hStopEvent);
	if (hInterruptTimer)
	{
		WaitForSingleObject(hInterruptTimer, 10000);
		CloseHandle(hInterruptTimer);
		hInterruptTimer = NULL;
	}

	if (hStopEvent) CloseHandle(hStopEvent);
	hStopEvent = NULL;
#endif
}

