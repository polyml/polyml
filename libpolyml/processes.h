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

#ifndef _PROCESSES_H_
#define _PROCESSES_H_

#include "globals.h"
#include "rts_module.h"

#include "noreturn.h"

#ifndef WINDOWS_PC
extern void process_may_block(int fd, int ioCall);
#endif

class SaveVecEntry;
typedef SaveVecEntry *Handle;
/***************************************************************************
 * 
 * Types & Definitions for PROCESSES
 *
 ***************************************************************************/

/******************************************
 * Synchroniser - used in ``choice'' forks. 
 ******************************************/
typedef enum {synch_choice, synch_par, synch_taken } SynchType;

class synchroniser: public PolyObject {
public:
    SynchType synch_type;
    synchroniser *synch_base; 
};

class ProcessChannel;

/******************************************
 * Process base. 
 ******************************************/
class ProcessBase: public PolyObject {
public:
    /* Stack for process. */
    StackObject *stack;    

    /* Processes are created by runtime system calls.
       Runnable processes are put into a doubly-linked list. 
        Links to front and back process in chain. */
    ProcessBase *f_chain; 
    ProcessBase *b_chain; 

    /* Status of process. */
    PolyWord status;    

    /* Used while blocked for various values, depending on status. 
       If status=PROCESS_BLOCKED or PROCESS_UNBLOCKED it can contain 
       the value being transferred. If status=PROCESS_IO_BLOCK it 
       contains the file-descriptor being waited for. */
    PolyWord block_data;  

    /* channel this process is blocked on */
    ProcessChannel *block_channel ;

    /* Chain of console processes. */
    ProcessBase *console_link; 

    /* Synchronisation semaphore. */
    synchroniser *synchro; 

    /* Added  DCJM 8/4/04.  These are used when returning from a foreign function. */
    PolyWord lastCallResult; /* Result returned. */
    PolyWord lastErrNo; /* Value of errno after return. */
    #ifdef WINDOWS_PC
    PolyWord lastErrcode; /* Value of GetLastError() after return. */
    #endif
    ProcessBase *callbackCaller; /* The process running when a callback was called. */
};

typedef ProcessBase process_base;

/*******************************************
 * Channel.
 *******************************************/
class ProcessChannel: public PolyObject {
public:
    ProcessBase *senders;
    ProcessBase *receivers;
};
typedef ProcessChannel channel;

/******************************************
 * CONSTANTS & MACROS for processes.
 ******************************************/
#define PROCESS_RUNABLE         ((unsigned)0) /* Not waiting for anything. */
#define PROCESS_IO_BLOCK        ((unsigned)1) /* Waiting for I/O */
#define PROCESS_BLOCKED         ((unsigned)2) /* Waiting for a channel. */
#define PROCESS_UNBLOCKED       ((unsigned)3) /* Was waiting */
/* PROCESS_INTERRUPTABLE.  Added DCJM 23/7/00.  Indicates that the
   process is waiting for a system call such as Posix.Process.sleep
   and should be set to raise a syscall exception if a signal is received.
   Used only in conjunction with PROCESS_IO_BLOCK. */
#define PROCESS_INTERRUPTABLE   8

/* Mask for these bits. */
#define PROCESS_STATUS \
   (PROCESS_RUNABLE | PROCESS_IO_BLOCK  | \
    PROCESS_BLOCKED | PROCESS_UNBLOCKED | \
    PROCESS_INTERRUPTABLE)

/* 
 * An interruptible process. 
 * (CTRL+C raises an interrupt to all console processes) 
 */
#define PROCESS_IS_CONSOLE 0x4000


#define NO_PROCESS ((ProcessBase *)TAGGED(0).AsObjPtr())
#define NO_SYNCH   ((synchroniser *)TAGGED(0).AsObjPtr())
#define NO_CHANNEL ((ProcessChannel*)TAGGED(0).AsObjPtr())

/**********************************************************************
 *
 * Handles for different 'objects' are of type Handle
 *
 **********************************************************************/
typedef Handle ProcessHandle; /* Handle to (process_base *) */
typedef Handle SynchroHandle; /* Handle to (synchroniser *) */
typedef Handle ChanHandle;    /* Handle to (PolyWord *) */

extern Handle make_new_root_proc(Handle proc);
extern ProcessHandle fork_function(Handle proc, Handle arg);

// Check to see that there is space in the stack.  May GC and may raise a C++ exception.
extern void check_current_stack_size(PolyWord *lower_limit);


extern Handle switch_subshells_c(void);
NORETURNFN(extern Handle kill_selfc(void));
extern Handle send_on_channelc(Handle val, ChanHandle chan);
extern Handle receive_on_channelc(ChanHandle chan);
extern Handle fork_processc(Handle console_handle, Handle proc);
extern Handle choice_processc(Handle proc1, Handle proc2);
extern Handle int_processc(ProcessHandle proc);
NORETURNFN(extern Handle install_rootc(Handle proc));
NORETURNFN(Handle interrupt_console_processes_c(void));
extern Handle install_subshells_c (Handle root_function);
extern Handle shrink_stack_c (Handle reserved_space);

class ScanAddress;

// External interface to the Process module.  These functions are all implemented
// by the Processes class.
class ProcessExternal
{
public:
    virtual ~ProcessExternal() {} // Defined to suppress a warning from GCC
    virtual ProcessBase *CurrentProcess(void) = 0;
    virtual void SetCurrentProcess(ProcessBase *p) = 0;

    // Set up the next process in the chain to run when this returns..
    virtual void select_next_process(void) = 0;

    virtual void add_process(ProcessHandle p_base, unsigned state) = 0;
    virtual void remove_process(ProcessBase *to_kill) = 0;
    virtual void kill_process(ProcessBase *to_kill) = 0;
    virtual void interrupt_console_processes(void) = 0;
    virtual void interrupt_signal_processes(void) = 0;
    virtual void set_process_list(PolyObject *rootFunction) = 0;
    virtual NORETURNFN(void block_and_restart(int fd, int interruptable, int ioCall)) = 0;
    virtual void block_and_restart_if_necessary(int fd, int ioCall) = 0;
};

extern ProcessExternal *processes;

extern StackObject *poly_stack;
extern PolyWord        *end_of_stack;

#define IO_SPACING 8 // This is a bit of a mess.

#endif
