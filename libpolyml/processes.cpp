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

#ifdef HAVE_ERRNO_H
#include <errno.h>
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
#define ASSERT(x)
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

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h>
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
#include "run_time.h"
#include "sys.h"
#include "sighandler.h"
#include "scanaddrs.h"
#include "save_vec.h"
#include "rts_module.h"
#include "noreturn.h"
#include "memmgr.h"


#define SAVE(x) gSaveVec->push(x)
#define SIZEOF(x) (sizeof(x)/sizeof(PolyWord))

#define DEREFCHANHANDLE(_x)      ((ProcessChannel*)(_x)->WordP())
#define DEREFSYNCHROHANDLE(_x)   ((synchroniser*)(_x)->WordP())

/*******************************************************************
 *
 * The initial size of a stack frame. It can expand from this 
 * and need not be a power of 2. However it must be larger than 
 * 40 + the size of the base area in each stack frame, which is 
 * machine dependent. The figure 40 comes from 2*min_stack_check 
 * in the Poly code-generator. 
 *
 *******************************************************************/

#define EMPTYSTRING (PolyObject*)IoEntry(POLY_SYS_emptystring)

#define ISZEROHANDLE(handle) ((int)DEREFHANDLE(handle) == TAGGED(0))

#define MUTABLE(x) (OBJ_MUTABLE_BIT | (x))

#define NEWSTACK(len) ((StackObject *)alloc(len, F_MUTABLE_BIT|F_STACK_BIT))
#define NEWSTACKHANDLE(len) (alloc_and_save(len, F_MUTABLE_BIT|F_STACK_BIT))
#define NEWPROCESSHANDLE()   (alloc_and_save(sizeof(ProcessBase)/sizeof(PolyWord), F_MUTABLE_BIT))
#define NEWSYNCHROHANDLE()   (alloc_and_save(sizeof(synchroniser)/sizeof(PolyWord), F_MUTABLE_BIT))


class Processes: public ProcessExternal, public RtsModule
{
public:
    Processes(): process_list(0), console_chain(0), alt_process_list(0), alt_console_chain(0),
        no_of_processes(0), no_waiting(0), timerRunning(false), interrupt_exn(0) {}
    virtual void Init(void);
    virtual void Uninit(void);
    virtual void Reinit(void);
    void GarbageCollect(ScanAddress *process);
private:
    void GCProcessList(ScanAddress *process, ProcessBase * &plist);
public:
    ProcessBase *CurrentProcess(void) { return process_list; }
    void SetCurrentProcess(ProcessBase *p) { process_list = p; }
    unsigned RunQueueSize(void) { return no_of_processes+no_waiting; }

    // Set up the next process in the chain to run when this returns..
    void select_next_process(void);

    // The current process has blocked. Find something else to do.
    void select_a_new_process(void);

    void StartStopInterruptTimer(void);
    ProcessHandle fork_proc(Handle proc, SynchroHandle synchro, bool isConsole, Handle arg);
    ProcessHandle make_new_root_proc(Handle proc);
    void add_process(ProcessHandle p_base, unsigned state);
    void remove_process(ProcessBase *to_kill);
    void kill_process(ProcessBase *to_kill);
    void interrupt_console_processes(void);
    void interrupt_signal_processes(void);
    void set_process_list(PolyObject *rootFunction);
    void block_and_restart(int fd, int interruptable, int ioCall);
    void block_and_restart_if_necessary(int fd, int ioCall);
    ProcessHandle synchronise(ProcessBase **wchain, ProcessBase **pchain, int ioCall);
    void SwitchSubShells(void);

    void killAll(void);
private:

    bool process_can_do_transfer(ProcessBase *proc);
    void accept_this_choice(ProcessBase *proc);
    bool separate_choices(ProcessBase *x, ProcessBase *y);

    ProcessBase *process_list;
    ProcessBase *console_chain;
    ProcessBase *alt_process_list;
    ProcessBase *alt_console_chain;

    unsigned  no_of_processes;     // Number of processes.
    unsigned  no_waiting;          // Number of those processes waiting for IO.
    bool      timerRunning;

#ifdef HAVE_WINDOWS_H
public:
    HANDLE hStopEvent; /* Signalled to stop all threads. */
private:
    HANDLE hInterruptTimer; /* Interrupt timer thread. */
#endif
#ifndef WINDOWS_PC
    /* The file descriptors currently active are held in the bits in this word.
       Apart from processes waiting for IO streams the window system also needs to
       be aware of window activity.
       We don't use this for Windows because the task of determining which
       devices have become available is just too complicated.  We rely on timing
       out in that case.
    */
    fd_set file_desc;
#endif

public:
    poly_exn *interrupt_exn;

    friend Handle install_subshells_c(Handle root_function);
    friend Handle switch_subshells_c(void);
};

// Global process data.
Processes static processesModule;
ProcessExternal *processes = &processesModule;

// These need to be globals since they're widely used.
StackObject *poly_stack   = 0;    /* Stack of current process. */
PolyWord    *end_of_stack = 0;  /* Address of the end of poly_stack. */

inline POLYUNSIGNED GET_PROCESS_STATUS(ProcessBase *p) { return UNTAGGED(p->status) & PROCESS_STATUS; }

static void SET_PROCESS_STATUS(ProcessBase *p, unsigned s)
{
    if (GET_PROCESS_STATUS(p) == s) return;
    POLYUNSIGNED oldStatus = GET_PROCESS_STATUS(p);
    
    p->status = TAGGED((oldStatus & ~PROCESS_STATUS) | s);
}


/******************************************************************************/
/*                                                                            */
/*      send_on_channelc - called from sparc_assembly.s                       */
/*                                                                            */
/******************************************************************************/
/* CALL_IO2(send_on_channel, REF, REF, NOIND) */
Handle send_on_channelc(Handle val, ChanHandle chan)
/* Send a value on a channel. */
{
    ProcessHandle receiver;
    
    /* Save the value and channel in case we block. */
    processesModule.CurrentProcess()->block_data = DEREFWORD(val);
    processesModule.CurrentProcess()->block_channel = DEREFCHANHANDLE(chan);
    
    /* See if we can match up a receiver - if we cannot we do not return. */
    receiver = processesModule.synchronise(&(DEREFCHANHANDLE(chan)->receivers),
                           &(DEREFCHANHANDLE(chan)->senders),
                           POLY_SYS_send_on_channel);
    
    /* Transfer the value. */
    DEREFPROCHANDLE(receiver)->block_data = DEREFWORD(val);
    
    processesModule.CurrentProcess()->block_data = TAGGED(0);
    processesModule.CurrentProcess()->block_channel = NO_CHANNEL;
    return SAVE(TAGGED(0));
}

/******************************************************************************/
/*                                                                            */
/*      receive_on_channelc - called from sparc_assembly.s                    */
/*                                                                            */
/******************************************************************************/
/* CALL_IO1(receive_on_channel, REF, NOIND) */
Handle receive_on_channelc(ChanHandle chan)
{
    ProcessHandle sender;
    
    /* Save the channel in case we block. */
    processesModule.CurrentProcess()->block_channel = DEREFCHANHANDLE(chan);
    
    /* See if we can match up a sender - if we cannot we do not return. */
    sender = processesModule.synchronise(&(DEREFCHANHANDLE(chan)->senders),
                         &(DEREFCHANHANDLE(chan)->receivers),
                         POLY_SYS_receive_on_channel);
    return SAVE(DEREFPROCHANDLE(sender)->block_data); /* Get the value. */
}

/******************************************************************************/
/*                                                                            */
/*      fork_proc - utility function - allocates in Poly heap                 */
/*                                                                            */
/******************************************************************************/
ProcessHandle Processes::fork_proc(Handle proc, SynchroHandle synchro, bool isConsole, Handle arg)
/* Fork a new process.  "arg" is either nil or a function argument. */
{
    /* Load the closure and make the pc local. */
    ProcessHandle p_base = NEWPROCESSHANDLE();
    Handle stack = NEWSTACKHANDLE(machineDependent->InitialStackSize());
    machineDependent->InitStackFrame(stack, proc, arg);
    DEREFPROCHANDLE(p_base)->stack = (StackObject*)DEREFWORDHANDLE(stack);
    
    POLYUNSIGNED status = PROCESS_RUNABLE; //  Ready to run. */

    DEREFPROCHANDLE(p_base)->block_data    = TAGGED(0);
    DEREFPROCHANDLE(p_base)->block_channel = NO_CHANNEL;
    
    if (isConsole)
    {
        status |= PROCESS_IS_CONSOLE;
        DEREFPROCHANDLE(p_base)->console_link  = console_chain;
        console_chain = DEREFPROCHANDLE(p_base);
    }
    else (DEREFPROCHANDLE(p_base))->console_link = NO_PROCESS;

    DEREFPROCHANDLE(p_base)->status = TAGGED(status);
    
    DEREFPROCHANDLE(p_base)->synchro = (synchro == 0 ? NO_SYNCH : DEREFSYNCHROHANDLE(synchro));
    add_process(p_base, PROCESS_RUNABLE); /* allocates */
    return p_base; 
}

/******************************************************************************/
/*                                                                            */
/*      make_new_root_proc - utility function - allocates in Poly heap        */
/*                                                                            */
/******************************************************************************/
ProcessHandle Processes::make_new_root_proc(Handle proc)
// Create an initial root process.
{
    ProcessHandle p_base = NEWPROCESSHANDLE();
    Handle stack = NEWSTACKHANDLE(machineDependent->InitialStackSize());
    machineDependent->InitStackFrame(stack, proc, 0);
      
    DEREFPROCHANDLE(p_base)->stack = (StackObject*)DEREFWORDHANDLE(stack);
    DEREFPROCHANDLE(p_base)->status        = TAGGED(PROCESS_RUNABLE|PROCESS_IS_CONSOLE); /* Ready to run. */
    DEREFPROCHANDLE(p_base)->block_data    = TAGGED(0);
    DEREFPROCHANDLE(p_base)->block_channel = NO_CHANNEL;
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

// Set up the next process in the chain to run when this returns.
void Processes::select_next_process(void)
{
    process_list = process_list->f_chain;
    int status = GET_PROCESS_STATUS(process_list);

    if (status == PROCESS_IO_BLOCK || status == (PROCESS_IO_BLOCK | PROCESS_INTERRUPTABLE))
    {
        /* Try running it. If IO is not possible it will block immediately
           by calling process_may_block. (Unless a console interrupt has
           happened). */
#ifndef WINDOWS_PC
        int fd = UNTAGGED(process_list->block_data);
        if (fd >= 0) FD_CLR(fd, &file_desc);
#endif
        no_waiting--;
        SET_PROCESS_STATUS(process_list,PROCESS_RUNABLE);
        StartStopInterruptTimer();
    }
    poly_stack   = process_list->stack;
    end_of_stack = (PolyWord*)poly_stack + OBJECT_LENGTH(poly_stack);
}

/******************************************************************************/
/*                                                                            */
/*      add_process - utility function - allocates                            */
/*                                                                            */
/******************************************************************************/
void Processes::add_process(ProcessHandle p_base, unsigned state)
/* Add a process to the runnable set. */
{
    SET_PROCESS_STATUS(DEREFPROCHANDLE(p_base),state); /* Either runable or unblocked. */
    
    if (process_list == NO_PROCESS)
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
        process_list = DEREFPROCHANDLE(p_base);
        DEREFPROCHANDLE(p_base)->f_chain = DEREFPROCHANDLE(p_base);
        DEREFPROCHANDLE(p_base)->b_chain = DEREFPROCHANDLE(p_base);
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
        DEREFPROCHANDLE(p_base)->b_chain = process_list;
        DEREFPROCHANDLE(p_base)->f_chain = process_list->f_chain;
        process_list->f_chain = DEREFPROCHANDLE(p_base);
        DEREFPROCHANDLE(p_base)->f_chain->b_chain = DEREFPROCHANDLE(p_base);
    }
    
    no_of_processes++;
    StartStopInterruptTimer();
}

/******************************************************************************/
/*                                                                            */
/*      remove_process - utility function - doesn't allocate                  */
/*                                                                            */
/******************************************************************************/
void Processes::remove_process(ProcessBase *to_kill)
/* Remove a process from the runable set. May be because it has been killed
   or because it is waiting for an event. */
{
    /* Select a different process if this is the active one. */
    if (to_kill == process_list)
        process_list = to_kill->b_chain;
    
    /* Select this process if it is waiting for an event.
    -> ->                       ---->
    BEFORE : O  O  O     AFTER : O  ?  O
    <-     <-                           <----
    */
    /* May already be waiting for an event. */
    if (to_kill->f_chain != NO_PROCESS && to_kill->b_chain != NO_PROCESS)
    {
        /* Else - remove it from the chain. */
        to_kill->f_chain->b_chain = to_kill->b_chain;
        to_kill->b_chain->f_chain = to_kill->f_chain;
        no_of_processes--;
        
        if (no_of_processes == 0)
            process_list = NO_PROCESS;
        StartStopInterruptTimer();
    }
}

/******************************************************************************/
/*                                                                            */
/*      fork_processc - called from sparc_assembly.s - allocates              */
/*                                                                            */
/******************************************************************************/
/* CALL_IO2(fork_process, REF, REF, IND) */
ProcessHandle fork_processc(Handle console_handle, Handle proc) /* Handle to (ProcessBase *) */
/* Fork a new process. Returns a reference to the process base. */
{
    bool console = get_C_long(DEREFWORDHANDLE(console_handle)) != 0;
    
    SynchroHandle synchro = SAVE(processesModule.CurrentProcess()->synchro);
    
    if (DEREFSYNCHROHANDLE(synchro) != NO_SYNCH &&
        DEREFSYNCHROHANDLE(synchro)->synch_type != synch_par)
        
    {
        SynchroHandle new_sync = NEWSYNCHROHANDLE();
        
        DEREFSYNCHROHANDLE(new_sync)->synch_type = synch_par;
        DEREFSYNCHROHANDLE(new_sync)->synch_base = DEREFSYNCHROHANDLE(synchro);
        
        processesModule.CurrentProcess()->synchro = DEREFSYNCHROHANDLE(new_sync);
        synchro = new_sync;
    }
    
    return processesModule.fork_proc(proc, synchro, console, 0);
}

/******************************************************************************/
/*                                                                            */
/*      choice_processc - called from sparc_assembly.s                        */
/*                                                                            */
/******************************************************************************/
/* CALL_IO2(choice_process, REF, REF, NOIND) */
Handle choice_processc(Handle proc1, Handle proc2)
/* Fork two new processes, but only allow them to run until the first
   successful read or write. The parent is not affected. */
{
    SynchroHandle synchro = SAVE(processesModule.CurrentProcess()->synchro);
    SynchroHandle new_sync;
    
    /* If this is already a choice we have to run the new processes in parallel,
    with the parent. */
    
    if (DEREFSYNCHROHANDLE(synchro) != NO_SYNCH &&
        DEREFSYNCHROHANDLE(synchro)->synch_type != synch_par)
    {
        new_sync = NEWSYNCHROHANDLE();
        DEREFSYNCHROHANDLE(new_sync)->synch_type = synch_par;
        DEREFSYNCHROHANDLE(new_sync)->synch_base = DEREFSYNCHROHANDLE(synchro);
        
        processesModule.CurrentProcess()->synchro = DEREFSYNCHROHANDLE(new_sync);
        synchro = new_sync;
    }
    
    /* Now the synchroniser for the new processes. */
    new_sync = NEWSYNCHROHANDLE();
    
    DEREFSYNCHROHANDLE(new_sync)->synch_type = synch_choice;
    DEREFSYNCHROHANDLE(new_sync)->synch_base = DEREFSYNCHROHANDLE(synchro);
    processesModule.fork_proc(proc1, new_sync, false, 0);
    processesModule.fork_proc(proc2, new_sync, false, 0);
    
    return SAVE(TAGGED(0));
}

/******************************************************************************/
/*                                                                            */
/*      check_current_stack_size - utility function- allocates in Poly heap   */
/*                                                                            */
/******************************************************************************/
void check_current_stack_size(PolyWord *lower_limit)
/* Expands the current stack if it has grown. We cannot shrink a stack segment
   when it grows smaller because the frame is checked only at the beginning of
   a procedure to ensure that there is enough space for the maximum that can
   be allocated. */
{
    POLYUNSIGNED old_len; /* Current size of stack segment. */
    POLYUNSIGNED new_len; /* New size */
    POLYUNSIGNED min_size; /* This is the minimum space required on the stack. */
    StackObject *new_stack;

    /* Get current size of new stack segment. */
    old_len = OBJECT_LENGTH(poly_stack);
 
    /* The minimum size must include the reserved space for the registers. */
    min_size = end_of_stack - lower_limit + poly_stack->p_space;
    
    if (old_len >= min_size) return; /* Ok with present size. */

    /* If it is too small double its size. */
    /* BUT, the maximum size is 2^24 - 1 words. */

    if (old_len == MAX_OBJECT_SIZE)
    {
        /* Cannot expand the stack any further. */
        fprintf(stderr, "Warning - Stack limit reached - interrupting process\n");
        machineDependent->SetException(poly_stack, processesModule.interrupt_exn);
        return;
    }

    for (new_len = old_len; new_len < min_size; new_len *= 2);
    if (new_len > MAX_OBJECT_SIZE) new_len = MAX_OBJECT_SIZE;

    /* Must make a new frame and copy the data over. */
    new_stack = NEWSTACK(new_len); // N.B.  May throw a C++ exception.
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
    poly_stack->SetLengthWord(OBJECT_LENGTH(poly_stack), F_BYTE_BIT|F_MUTABLE_BIT);
    poly_stack = new_stack;
    end_of_stack = (PolyWord*)new_stack + new_len;
    
    processesModule.CurrentProcess()->stack = poly_stack;
}

/******************************************************************************/
/*                                                                            */
/*      shrink_stack - called from sparc_assembly.s - allocates               */
/*                                                                            */
/******************************************************************************/
Handle shrink_stack_c(Handle reserved_space)
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
    min_size = (end_of_stack - (PolyWord*)poly_stack->p_sp) + poly_stack->p_space + reserved;
    
    for (new_len = machineDependent->InitialStackSize(); new_len < min_size; new_len *= 2);

    if (old_len <= new_len) return SAVE(TAGGED(0)); /* OK with present size. */

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
    // The above probably doesn't apply any longer.  We don't export stacks.
    poly_stack->SetLengthWord(OBJECT_LENGTH(poly_stack), F_BYTE_BIT|F_MUTABLE_BIT);
    poly_stack = new_stack;
    end_of_stack = (PolyWord*)new_stack + new_len;
    
    processesModule.CurrentProcess()->stack = poly_stack;
    return SAVE(TAGGED(0));
}

// Raise a console interrupt in a specified process.  Also used in
// interrupt_console_processes_c to raise interrupts in all console processes.
Handle int_processc(ProcessHandle proc)
{
    if (GET_PROCESS_STATUS(DEREFPROCHANDLE(proc)) == PROCESS_BLOCKED) /* blocked on channel */
    {
        ProcessBase **p; /* NOT a handle */
        
        /* Remove this process from this senders and receivers for that channel */
        
        for (p = &(DEREFPROCHANDLE(proc)->block_channel->senders);
             *p != NO_PROCESS;
             p = &((*p)->b_chain))
        {
            if (*p == DEREFPROCHANDLE(proc))
            {
                *p = (*p)->b_chain;
                break;
            }
        }
        
        for (p = & DEREFPROCHANDLE(proc)->block_channel->receivers;
             *p != NO_PROCESS;
             p = & (*p)->b_chain)
        {
            if (*p == DEREFPROCHANDLE(proc))
            {
                *p = (*p)->b_chain;
                break;
            }
        }
        
        processesModule.add_process(proc, PROCESS_RUNABLE); /* allocates */
    }
    
    machineDependent->SetException(DEREFPROCHANDLE(proc)->stack, processesModule.interrupt_exn);
    
    return SAVE(TAGGED(0));
}

/* CALL_IO0(interrupt_console_processes_,NOIND) */
/* int interrupt_console_processes_c() */
Handle interrupt_console_processes_c(void)
{
    processesModule.interrupt_console_processes();
    THROW_RETRY; // Actually we've raised an exception
	/*NOTREACHED*/
#ifdef _WIN32_WCE
	return 0;
#endif
}

void Processes::killAll(void)
{
    process_list = NO_PROCESS;
    console_chain = NO_PROCESS;
    no_waiting = 0;
    no_of_processes = 0;
    StartStopInterruptTimer();
}

/* CALL_IO1(install_root, REF, NOIND) */
Handle install_rootc(Handle proc)
/* Installs a new procedure as the root. This involves killing all the processes
   and starting a new process with this root. */
{
    processesModule.killAll();
    processesModule.fork_proc(proc, (SynchroHandle)0, true, 0);
    processesModule.select_a_new_process();
    THROW_RETRY; // Because the current process has been killed
    /*NOTREACHED*/
#ifdef _WIN32_WCE
	return 0;
#endif
}

/******************************************************************************/
/*                                                                            */
/*      kill_process - utility function - doesn't allocate                    */
/*                                                                            */
/******************************************************************************/
void Processes::kill_process(ProcessBase *to_kill)
/* Kill a process. Used when a process dies or when it tries to do a transfer
   after a different choice has already been taken. There is no mechanism
   for one process to kill another. */
{
    /* Remove process to_kill from the process chain. */
    remove_process(to_kill);
    
    if (UNTAGGED(to_kill->status) & PROCESS_IS_CONSOLE)
    {
        /* Remove it from the console chain. */
        ProcessBase **consoles /* NOT a handle */ = &console_chain;
        
        while(*consoles != NO_PROCESS && *consoles != to_kill)
        {
            consoles = &(*consoles)->console_link;
        }
        
        if (*consoles != NO_PROCESS)
            *consoles = (*consoles)->console_link;
    }
}

/******************************************************************************/
/*                                                                            */
/*      interrupt_console_processes - utility function - allocates            */
/*                                                                            */
/******************************************************************************/
void Processes::interrupt_console_processes(void)
{
    /* changed 8/11/93 SPF - now uses handles for safety */
    Handle saved = gSaveVec->mark();
    ProcessHandle p = SAVE(console_chain);
    while (DEREFPROCHANDLE(p) != NO_PROCESS) {
        int_processc(p); /* allocates! */
        gSaveVec->reset(saved);
        p = SAVE(DEREFPROCHANDLE(p)->console_link);
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
void Processes::interrupt_signal_processes(void)
{
    Handle exc =
        make_exn(EXC_syserr, create_syscall_exception("Call interrupted by signal", EINTR));
    ASSERT(no_of_processes > 0);

    Handle saved = gSaveVec->mark();
    ProcessHandle p = SAVE(process_list);
    do 
    {
        if ((GET_PROCESS_STATUS(DEREFPROCHANDLE(p)) & PROCESS_INTERRUPTABLE) ==
            PROCESS_INTERRUPTABLE)
        {
            SET_PROCESS_STATUS(DEREFPROCHANDLE(p), PROCESS_RUNABLE);
            machineDependent->SetException(DEREFPROCHANDLE(p)->stack,
                DEREFEXNHANDLE(exc));
            no_waiting--;
        }
        gSaveVec->reset(saved);
        p = SAVE(DEREFPROCHANDLE(p)->f_chain);
    } while (DEREFPROCHANDLE(p) != process_list);
    StartStopInterruptTimer();
}

// The current process has blocked. Find something else to do.
void Processes::select_a_new_process(void)
{
    /* Deal with any pending interrupts. */
    execute_pending_interrupts();

    /* Should we block the Unix process? */
    if (no_of_processes == 0)
    { /* No processes able to run. */
        if (console_chain == NO_PROCESS) finish(0);

        fputs("Processes have deadlocked. Interrupting console processes.\n",stdout);
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
            // N.B.  It seems that calling PeekMessage may result in a callback
            // to a window proc directly without a call to DispatchMessage.  That
            // could result in a recursive call here if we have installed an ML
            // window proc.
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
}

/******************************************************************************/
/*                                                                            */
/*      kill_selfc - called from sparc_assembly.s                             */
/*                                                                            */
/******************************************************************************/
/* CALL_IO0(kill_self, NOIND) */
Handle kill_selfc(void)
/* A call to this is put on the stack of a new process so when the procedure
   returns the process goes away. */  
{
    processesModule.kill_process(processesModule.CurrentProcess());
    processesModule.select_a_new_process();
    THROW_RETRY; // Current process has been killed so can't return.
   /*NOTREACHED*/
#ifdef _WIN32_WCE
	return 0;
#endif
}


#ifndef WINDOWS_PC
void process_may_block(int fd, int ioCall)
/* A process is about to read from a file descriptor which may cause it to
   block. It returns only if the process will not block. Otherwise it waits
   for a signal such as SIGALRM. */
{
#ifdef __CYGWIN__
      static struct timeval poll = {0,1};
#else
      static struct timeval poll = {0,0};
#endif
      fd_set read_fds;
      int selRes;
  
      FD_ZERO(&read_fds);
      FD_SET((int)fd,&read_fds);

      /* If there is something there we can return. */
      selRes = select(FD_SETSIZE, &read_fds, NULL, NULL, &poll);
      if (selRes > 0) return; /* Something waiting. */
      else if (selRes < 0 && errno != EINTR) Crash("select failed %d\n", errno);
      /* Have to block unless this is the only process. */
      processesModule.block_and_restart_if_necessary(fd, ioCall);
}

#endif

void Processes::block_and_restart(int fd, int interruptable, int ioCall)
/* The process is waiting for IO or for a timer.  Suspend it and
   then restart it later.  fd may be negative if the file descriptor
   value is not relevant.
   If this is interruptable (currently only used for Posix functions)
   the process will be set to raise an exception if a signal is handled. */
{
    int status = PROCESS_IO_BLOCK;
    if (interruptable) status |= PROCESS_INTERRUPTABLE;

    SET_PROCESS_STATUS(process_list, status);
    process_list->block_data = TAGGED((int)fd);
    
    /* Suspend this process for the moment. */
    machineDependent->SetForRetry(ioCall);
    no_waiting++;
    StartStopInterruptTimer();
#ifndef WINDOWS_PC
    /* Add the file descriptor to the set.  fd may be < 0 if the reason
       we blocked was other than for reading. */
    if (fd >= 0) FD_SET(fd,&file_desc);
#endif
    select_a_new_process();
    THROW_RETRY;
    /* NOTREACHED */
}

void Processes::block_and_restart_if_necessary(int fd, int ioCall)
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
bool Processes::process_can_do_transfer(ProcessBase *proc)
/* Can a process do a read or a write? */
{
    synchroniser *synchr;
    for (synchr = proc->synchro; synchr != NO_SYNCH; synchr = synchr->synch_base)
    {
        if (synchr->synch_type == synch_taken) return false; /* Must kill it. */
    }
    return true; /* Ok. */
}

/******************************************************************************/
/*                                                                            */
/*      accept_this_choice - utility function - doesn't allocate              */
/*                                                                            */
/******************************************************************************/
void Processes::accept_this_choice(ProcessBase *proc)
/* Decide to accept this choice. Set all "choices" to "taken", and set
   all the links to zero so that "pars" in this choice can continue. */
{
    synchroniser *synchro, *next_sync;
    
    for (synchro = proc->synchro; synchro != NO_SYNCH; synchro = next_sync)
    {
        if (synchro->synch_type == synch_choice)
            synchro->synch_type = synch_taken;
        next_sync = synchro->synch_base;
        synchro->synch_base = NO_SYNCH;
    }
    /* This process can continue without further problems. */
    proc->synchro = NO_SYNCH;
}

/******************************************************************************/
/*                                                                            */
/*      separate_choices - utility function - doesn't allocate                */
/*                                                                            */
/******************************************************************************/
bool Processes::separate_choices(ProcessBase *x, ProcessBase *y)
/* If we have two process wanting to exchange information we cannot do it
   if they are alternative choices. */ 
{
    synchroniser *sx, *sy;
    
    for(sx = x->synchro; sx != NO_SYNCH; sx = sx->synch_base)
    {
        for(sy = y->synchro; sy != NO_SYNCH; sy = sy->synch_base)
        {
            if (sx == sy) /* Found the common element. */
                return (sx->synch_type == synch_par); /* Ok if parallel. */
        }
    }
    return true; /* Nothing in common - ok */
} 


/******************************************************************************/
/*                                                                            */
/*      synchronise - utility function - allocates                            */
/*                                                                            */
/******************************************************************************/
ProcessHandle Processes::synchronise(ProcessBase **wchain, ProcessBase **pchain, int ioCall)
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
/* of a couple of (ProcessBase *) pointers, which may themselves be embedded */
/* inside a Poly/ML heap object. The extra level of indirection is because    */
/* we are doing imperative list processing.                                   */
{
    ProcessBase *this_process;
    
    /* If there is no process waiting to receive the data we must block until
    someone wants it. This means that some time later will be restarted
    and re-enter this procedure. We need to distinguish the cases. */
    if (GET_PROCESS_STATUS(process_list) == PROCESS_UNBLOCKED)
    {
        SET_PROCESS_STATUS(process_list,PROCESS_RUNABLE);
        return SAVE(process_list);
    }
    
    /* Can we do send or receive? - start a new process if it cannot */
    if (!process_can_do_transfer(process_list))
    {
        kill_process(process_list); /* Choice has been taken already. */
        select_a_new_process();
        THROW_RETRY; // Current process has been killed
        /*NOTREACHED*/
    }
    
    /* Find a receiver/sender on the wait chain which is not in common with the
    current process. */
    while (*wchain != NO_PROCESS /* Until the end of the list or we return */)
    {
        if (!(process_can_do_transfer(*wchain)))
        {
            /* Remove it if an alternative has already been chosen. */
            ProcessBase *next = (*wchain)->b_chain; /* Save because kill unchains it. */
            kill_process(*wchain);
            *wchain = next;
        }
        else if (!(separate_choices(process_list, *wchain)))
        {
            /* These are alternative choices - cannot match. */
            wchain = &((*wchain)->b_chain);
        }
        else
        {
            ProcessHandle part = SAVE(*wchain);
            
            /* Remove it from the chain. */
            *wchain = (*wchain)->b_chain;
            DEREFPROCHANDLE(part)->b_chain = NO_PROCESS;
            DEREFPROCHANDLE(part)->f_chain = NO_PROCESS;
            
            /* This choice is taken for both sides. */
            accept_this_choice(process_list);
            accept_this_choice(DEREFPROCHANDLE(part));
            
            /* Unblock and put back onto chain. */
            add_process(part,PROCESS_UNBLOCKED); /* allocates */
            return part; /* Exit with the process we have found. */
        }
    }
    
    /* No-one waiting. Schedule something else.  - does not return. */
    this_process = process_list;
    SET_PROCESS_STATUS(this_process, PROCESS_BLOCKED);
    machineDependent->SetForRetry(ioCall);
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
            
            ProcessBase *next = (*pchain)->b_chain;
            kill_process(*pchain);
            *pchain = next;
        }
    }
    
    *pchain = this_process;
    this_process->f_chain = NO_PROCESS;
    this_process->b_chain = NO_PROCESS;
    select_a_new_process();
    THROW_RETRY; // Current process has been suspended
    /* NOTREACHED */
	return 0;
}

/******************************************************************************/
/*                                                                            */
/*      set_process_list - called from mpoly.c - allocates                    */
/*                                                                            */
/******************************************************************************/
// Sets up the initial process from the root function.

void Processes::set_process_list(PolyObject *rootFunction)
/* Sets the initial process list when the system is entered. */
{
    no_waiting = 0;
    no_of_processes = 0;
    process_list = NO_PROCESS;
    console_chain = NO_PROCESS;
    no_waiting = 0;
    no_of_processes = 0;
    fork_proc(gSaveVec->push(rootFunction), (SynchroHandle)0, true, 0);

#ifndef WINDOWS_PC
    FD_ZERO(&file_desc);
#endif
}

/* we must copy database stacks to local store since updates to them occur inline */

StackObject *copy_mapped_stack (StackObject *old)
{
    StackObject *newp;
    PolyWord        *addr = (PolyWord *)(old);
    
    ASSERT(old->IsStackObject());
    
    /* SPF 15/11/94 - make sure we reserve enough space
       for the C exception data */
    if (! gMem.IsLocalMutable(addr)/* || (old->p_space < OVERFLOW_STACK_SIZE)*/)
    {
        /* SPF 3/7/95 We MUST NOT shrink the stack (MJC's code tried to do this)
           because the suspended process may just have done a stack check that
           asked for a MUCH bigger stack than it is apparently using. */
/*        POLYUNSIGNED res_space = 
            (old->p_space >= OVERFLOW_STACK_SIZE) ? old->p_space : OVERFLOW_STACK_SIZE;*/
        POLYUNSIGNED res_space = old->p_space;
        POLYUNSIGNED extra_space = res_space - old->p_space;
        POLYUNSIGNED oldL = addr[-1].AsUnsigned();
        POLYUNSIGNED minimum = extra_space + OBJ_OBJECT_LENGTH(oldL);
        POLYUNSIGNED len;
        
        /* make a power of two (why? SPF) */
        for (len = 1; len < minimum; len *= 2)
        {
            /* do nothing */
        }
        
        /* allocate the newp stack, then copy the contents */
        newp = (StackObject *) alloc(len, F_MUTABLE_BIT | F_STACK_BIT);
        CopyStackFrame (old,newp);
        
        /* SPF 15/11/94 - make sure we reserve enough space for the C exception data */
        ASSERT(newp->p_space <= res_space);
        newp->p_space = res_space;
#ifdef EXTRA_DEBUG
        fprintf(stderr, "copied stack frame from %p to %p, length %x, reserved %x\n",
            old, newp, len, newp->p_space);
#endif    
        return newp;
    }
#ifdef EXTRA_DEBUG
    else fprintf(stderr, "Not bothering to copy local mutable stack from %p\n",old);
#endif
    
    return old;
}


/******************************************************************************/
/*                                                                            */
/*      install_subshells_c - called from sparc_assembly.s                    */
/*                                                                            */
/******************************************************************************/
/* CALL_IO1(install_subshells_, REF, NOIND) */
Handle install_subshells_c(Handle root_function)
{
    if ((PolyWord)DEREFHANDLE(root_function) == TAGGED(0))
    {
        /* kill existing alternate subshell processes */
        processesModule.alt_process_list = 0;
        processesModule.alt_console_chain = 0;
    }
    else
    {
        /* Create new process and install it as alternate subshell */
        ProcessHandle root_process = processesModule.make_new_root_proc(root_function);
        processesModule.alt_process_list = DEREFPROCHANDLE(root_process);
        processesModule.alt_console_chain = DEREFPROCHANDLE(root_process);
    }
    
    return SAVE(TAGGED(0));
}

void Processes::SwitchSubShells(void)
{
    // Swap the process list
    ProcessBase *t;
    t = process_list;
    process_list = alt_process_list;
    alt_process_list = t;
    // Swap the console chain.
    t = console_chain;
    console_chain = alt_console_chain;
    alt_console_chain = t;

    no_waiting = 0;
    no_of_processes = 0;

    ProcessBase *proc = process_list;
    // Count the processes in the new process list.
    do {
        no_of_processes++;
        // Make them all runnable.
        SET_PROCESS_STATUS(proc, PROCESS_RUNABLE);
        proc = proc->f_chain;
    } while (proc != process_list);

    StartStopInterruptTimer();

    select_a_new_process();
    THROW_RETRY; // Current process has been suspended
    /*NOTREACHED*/
}

/* CALL_IO0(switch_subshells_,NOIND) */
Handle switch_subshells_c(void)
{
    if (processesModule.alt_process_list == (PolyObject*)0)        /* right test? */
    {
        fputs("Subshells have not been installed\n",stdout);
        return SAVE(TAGGED(0));
    }
    processesModule.SwitchSubShells();
    /*NOTREACHED*/
	return 0;
}

/******************************************************************************/
/*                                                                            */
/*      fork_function                                                         */
/*                                                                            */
/******************************************************************************/
/* fork_function.  Creates a new process from within the RTS. */
ProcessHandle fork_function(Handle proc, Handle arg)
{
    return processesModule.fork_proc(proc, (SynchroHandle)0, false, arg);
}

/******************************************************************************/
/*                                                                            */
/*      catchALRM - utility function - doesn't allocate                       */
/*        called when time-slice expired (1Hz) - signal sigALRM               */
/*                                                                            */
/******************************************************************************/

#if defined(HAVE_WINDOWS_H)
/* This thread interrupts the ML code periodically.  It is used in Windows
   in place of SIGALRM.  Cygwin seems to have problems with SIGALRM so we
   use this in Cygwin as well. */
DWORD WINAPI ProcessTimeOut(LPVOID parm)
{
    // Go round this loop every timeslice until the stop event is signalled.
    while (WaitForSingleObject(processesModule.hStopEvent, userOptions.timeslice) == WAIT_TIMEOUT)
    {
        interrupted=-1; /* Temporary to get run_time_interrupts called */
        machineDependent->InterruptCode();
    }
    return 0;
} /* handleALRM on PC */

#else /* UNIX */

static void catchALRM(SIG_HANDLER_ARGS(sig, context))
// Called to request a call to the entries in int_procs some time later.
{
    SIGNALCONTEXT *scp = (SIGNALCONTEXT *)context;
    ASSERT(sig == SIGALRM);
    interrupted = sig;
    machineDependent->InterruptCodeUsingContext(scp);
}

#endif /* UNIX */

void Processes::StartStopInterruptTimer(void)
/* Start or stop the interrupt timer.                                */
/* It is used to ensure that processes get a fair share of the machine. */
{
#if ! defined(HAVE_WINDOWS_H) /* UNIX version */
    /* We currently run this all the time.  It is needed because
       addSigCount in sighandler doesn't actually call InterruptCode
       (there's a similar situation with ^C followed by "f") and so
       an interrupt handler won't be called. */
    if (1 || no_of_processes > no_waiting+1)
    { // We have more than one process able to run
        if (! timerRunning && userOptions.timeslice != 0)
        {
            /* The interrupt frequency can now be set by the user */
            int seconds      = userOptions.timeslice / 1000;
            int microseconds = (userOptions.timeslice % 1000) * 1000;

            struct itimerval per_sec;
            per_sec.it_interval.tv_sec = seconds;
            per_sec.it_interval.tv_usec = microseconds;
            per_sec.it_value.tv_sec = seconds;
            per_sec.it_value.tv_usec = microseconds;
    
            setitimer(ITIMER_REAL, &per_sec, NULL);
            timerRunning = true;
        }
    }
    else if (timerRunning)
    { // Stop the timer.
        struct itimerval cancelTimer;
        memset(&cancelTimer, 0, sizeof(cancelTimer));
        setitimer(ITIMER_REAL, &cancelTimer, 0);
        timerRunning = false;
    }
#endif
}

void Processes::Init(void)
{
    poly_stack = 0;
    end_of_stack = 0;
#ifdef HAVE_WINDOWS_H
    /* Create event to stop timeslice interrupts. */
    hStopEvent = CreateEvent(NULL, TRUE, FALSE, NULL);

    DWORD dwId;
    hInterruptTimer = CreateThread(NULL, 0, ProcessTimeOut, NULL, 0, &dwId);
    if (hInterruptTimer == NULL)
    {
        fputs("Creating Interrupt Timer thread failed.\n", stdout); 
    }

#else /* Unix */
    /* Set up a handler for SIGALRM. SIGALRM
       is sent periodically when there are several processes running to ensure
       that each process gets a fair share of the machine. */
    markSignalInuse(SIGALRM);
    setSignalHandler(SIGALRM, catchALRM);
#endif
}

void Processes::Reinit(void)
{
    interrupt_exn = DEREFEXNHANDLE(make_exn(EXC_interrupt, SAVE(TAGGED(0))));
}

void Processes::Uninit(void)
{
#ifdef HAVE_WINDOWS_H
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
#else
    // Stop the timer.  We mustn't get any interrupts from now on.
    struct itimerval cancelTimer;
    memset(&cancelTimer, 0, sizeof(cancelTimer));
    setitimer(ITIMER_REAL, &cancelTimer, 0);
#endif
}

void Processes::GCProcessList(ScanAddress *process, ProcessBase *&plist)
{
    if (plist != 0)
    {
        PolyObject *p = plist;
        process->ScanRuntimeAddress(&p, ScanAddress::STRENGTH_STRONG);
        plist = (ProcessBase*)p;
    }
}

void Processes::GarbageCollect(ScanAddress *process)
/* Ensures that all the objects are retained and their addresses updated. */
{
    /* Processes */
    GCProcessList(process, process_list);
    GCProcessList(process, console_chain);
    GCProcessList(process, alt_process_list);
    GCProcessList(process, alt_console_chain);

    if (poly_stack != 0)
    {
        PolyObject *p = poly_stack;
        process->ScanRuntimeAddress(&p, ScanAddress::STRENGTH_STRONG);
        poly_stack = (StackObject*)p;
        end_of_stack = (PolyWord*)poly_stack + OBJECT_LENGTH(poly_stack);
    }
    
    /* The interrupt exn */
    if (interrupt_exn != 0) {
        PolyObject *p = interrupt_exn;
        process->ScanRuntimeAddress(&p, ScanAddress::STRENGTH_STRONG);
        interrupt_exn = (PolyException*)p;
    }
}
