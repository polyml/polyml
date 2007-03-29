/*
    Title:      Thread functions
    Author:     David C.J. Matthews

    Copyright (c) 2007 David C.J. Matthews

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
#include <unistd.h> // Want unistd for _SC_NPROCESSORS_ONLN at least
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

#if (defined(HAVE_LIBPTHREAD) && defined(HAVE_PTHREAD_H))
#define HAVE_PTHREAD 1
#include <pthread.h>
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
#include "locking.h"
#include "profiling.h"

#ifdef WINDOWS_PC
#include "Console.h"
#endif


#define SAVE(x) taskData->saveVec.push(x)
#define SIZEOF(x) (sizeof(x)/sizeof(PolyWord))

// These values are stored in the second word of thread id object as
// a tagged integer.  They may be set and read by the thread in the ML
// code.  
#define PFLAG_BROADCAST     1   // If set, accepts a broadcast
// How to handle interrrupts
#define PFLAG_IGNORE        0   // Ignore interrupts completely
#define PFLAG_SYNCH         2   // Handle synchronously
#define PFLAG_ASYNCH        4   // Handle asynchronously
#define PFLAG_ASYNCH_ONCE   6   // First handle asynchronously then switch to synch.
#define PFLAG_INTMASK       6   // Mask of the above bits


// Other threads may make requests to a thread.
typedef enum {
    kRequestNone = 0, // Increasing severity
    kRequestInterrupt = 1,
    kRequestKill = 2
} ThreadRequests;

class ProcessTaskData: public TaskData
{
public:
    ProcessTaskData();
    ~ProcessTaskData();

    virtual void Lock(void) {}
    virtual void Unlock(void) {}

    virtual void GarbageCollect(ScanAddress *process);

    // If a thread has to block it will block on this.
    PCondVar threadLock;
    // External requests made are stored here until they
    // can be actioned.
    ThreadRequests requests;
    // Pointer to the mutex when blocked. Set to NULL when it doesn't apply.
    PolyObject *blockMutex;
    // This is set to false when a thread blocks or enters foreign code,
    // While it is true the thread can manipulate ML memory so no other
    // thread can garbage collect.
    bool inMLHeap;

    // In Linux, at least, we need to run a separate timer in each thread
    bool runningProfileTimer;

#ifdef WINDOWS_PC
    LONGLONG lastCPUTime; // Used for profiling
#endif
#ifdef HAVE_PTHREAD
    pthread_t pthreadId;
#elif (defined(HAVE_WINDOWS_H))
    HANDLE threadHandle;
#endif
};

class Processes: public ProcessExternal, public RtsModule
{
public:
    Processes(): taskArray(0), taskArraySize(0), interrupt_exn(0), wantGC(false), requestRTSState(krequestRTSNone) {}
    virtual void Init(void);
    virtual void Uninit(void);
    virtual void Reinit(void);
    void GarbageCollect(ScanAddress *process);
public:
    void BroadcastInterrupt(void);
    void BeginRootThread(PolyObject *rootFunction);
    void BlockAndRestart(TaskData *taskData, int fd, bool posixInterruptable, int ioCall);
    void BlockAndRestartIfNecessary(TaskData *taskData, int fd, int ioCall);
    void SwitchSubShells(void);
    // Return the task data for the current thread.
    virtual TaskData *GetTaskDataForThread(void);
    // Get all threads into the RTS.  Sets requestEnterRTS if schedLock is held
    // otherwise interrupts the threads.
    virtual void RequestThreadsEnterRTS(bool isSignal);
    void RequestThreadsEnterRTSInternal(bool isSignal);
    // Get all threads to exit.
    virtual void KillAllThreads(void);
    // ForkFromRTS.  Creates a new thread from within the RTS.
    virtual bool ForkFromRTS(TaskData *taskData, Handle proc, Handle arg);
    // Create a new thread.  The "args" argument is only used for threads
    // created in the RTS by the signal handler.
    Handle ForkThread(ProcessTaskData *taskData, Handle threadFunction,
                    Handle args, PolyWord flags);
    // Process general RTS requests from ML.
    Handle ThreadDispatch(TaskData *taskData, Handle args, Handle code);

    virtual void ThreadUseMLMemory(TaskData *taskData); 
    virtual void ThreadReleaseMLMemory(TaskData *taskData);
    // Storage allocation.  This is in here because we need to synchronise the threads
    // if we want to garbage-collect.
    PolyWord *FindAllocationSpace(TaskData *taskData, POLYUNSIGNED words, bool alwaysInSeg);

    // If the schedule lock is already held we need to use these functions.
    void ThreadUseMLMemoryWithSchedLock(TaskData *taskData);
    void ThreadReleaseMLMemoryWithSchedLock(TaskData *taskData);
    // Begin and End garbage collection or similar actions that require the
    // whole memory.  BeginGC only returns when every other thread has
    // released its use of the ML memory.  They will then be blocked in
    // ThreadUseMLMemory until the GC is complete.  If another thread has
    // requested a GC already BeginGC will block until that has completed
    // and then return false.  A thread should not call EndGC unless BeginGC
    // has returned true.
    virtual bool BeginGC(TaskData *taskData);
    virtual void EndGC(TaskData *taskData);

    // Deal with any interrupt or kill requests.
    virtual void ProcessAsynchRequests(TaskData *taskData);
    // Process an interrupt request synchronously.
    virtual void TestSynchronousRequests(TaskData *taskData);

    // Set a thread to be interrupted or killed.  Wakes up the
    // thread if necessary.  MUST be called with taskArrayLock held.
    void MakeRequest(ProcessTaskData *p, ThreadRequests request);

    // Called when a thread has completed - doesn't return.
    NORETURNFN(void ThreadExit(TaskData *taskData));

    // Profiling control.
    virtual void StartProfiling(void);
    virtual void StopProfiling(void);

#ifdef WINDOWS_PC
    // Windows: Called every millisecond while profiling is on.
    void ProfileInterrupt(void);
#else
    // Unix: Start a profile timer for a thread.
    void StartProfilingTimer(void);
#endif

    // Called if this thread has attempted to allocate some memory, has
    // garbage-collected but still not recovered enough.
    virtual void MemoryExhausted(TaskData *taskData);

    // Find a task that matches the specified identifier and returns
    // it if it exists.  MUST be called with taskArrayLock held.
    ProcessTaskData *TaskForIdentifier(Handle taskId);

    // Each thread has an entry in this array.
    ProcessTaskData **taskArray;
    unsigned taskArraySize; // Current size of the array.

    /* schedLock: This lock must be held when making scheduling decisions.
       It must also be held before adding items to taskArray, removing
       them or scanning the array.
       It must also be held before deleting a TaskData object
       or using it in a thread other than the "owner"  */
    PLock schedLock;
#ifdef HAVE_PTHREAD
    pthread_key_t tlsId;
#elif defined(HAVE_WINDOWS_H)
    DWORD tlsId;
#endif

#ifdef HAVE_WINDOWS_H
    HANDLE hWakeupEvent; // Pulsed to wake up any threads waiting for IO.
#endif

    // We make an exception packet for Interrupt and store it here.
    // This exception can be raised if we run out of store so we need to
    // make sure we have the packet before we do.
    poly_exn *interrupt_exn;

    bool    wantGC;
    PCondVar gcBegin; // One thread blocks on here until all the others have stopped.
    PCondVar gcComplete; // All the threads block on here until the GC has completed.

    // If we've had a signal we need to get a thread to enter the RTS.  Because a
    // signal can arrive at any time we have to be careful how we deal with it.
    enum {
        krequestRTSNone,           // No outstanding request
        krequestRTSInterrupted,    // The threads have been requested to trap
        krequestRTSToInterrupt     // The lock was held and we need to requst a trap
    } requestRTSState;

#if defined(WINDOWS_PC)
    // Used in profiling
    HANDLE hStopEvent; /* Signalled to stop all threads. */
    HANDLE profilingHd;
#endif
};

// Global process data.
static Processes processesModule;
ProcessExternal *processes = &processesModule;

// Get the attribute flags.
static POLYUNSIGNED ThreadAttrs(TaskData *taskData)
{
    return UNTAGGED_UNSIGNED(taskData->threadObject->Get(1));
}

// As far as possible we want locking and unlocking an ML mutex to be fast so
// we try to implement the code in the assembly code using appropriate
// interlocked instructions.  That does mean that if we need to lock and
// unlock an ML mutex in this code we have to use the same, machine-dependent,
// code to do it.  These are defaults that are used where there is no
// machine-specific code.

// Increment the value contained in the first word of the mutex.
// On most platforms this code will be done with a piece of assembly code.
PLock mutexLock;

Handle MachineDependent::AtomicIncrement(TaskData *taskData, Handle mutexp)
{
    mutexLock.Lock();
    PolyObject *p = DEREFHANDLE(mutexp);
    // A thread can only call this once so the values will be short
    PolyWord newValue = TAGGED(UNTAGGED(p->Get(0))+1);
    p->Set(0, newValue);
    mutexLock.Unlock();
    return SAVE(newValue);
}

// Decrement the value contained in the first word of the mutex.
Handle MachineDependent::AtomicDecrement(TaskData *taskData, Handle mutexp)
{
    mutexLock.Lock();
    PolyObject *p = DEREFHANDLE(mutexp);
    PolyWord newValue = TAGGED(UNTAGGED(p->Get(0))-1);
    p->Set(0, newValue);
    mutexLock.Unlock();
    return SAVE(newValue);
}

// Called from interface vector.  Generally the assembly code will be
// used instead of this.
Handle AtomicIncrement(TaskData *taskData, Handle mutexp)
{
    return machineDependent->AtomicIncrement(taskData, mutexp);
}

// Called from interface vector.  Generally the assembly code will be
// used instead of this.
Handle AtomicDecrement(TaskData *taskData, Handle mutexp)
{
    return machineDependent->AtomicDecrement(taskData, mutexp);
}

// Return the thread object for the current thread.
// On most platforms this will be done with a piece of assembly code.
Handle ThreadSelf(TaskData *taskData)
{
    return SAVE(taskData->threadObject);
}

// Called from interface vector.  This is the normal entry point for
// the thread functions.
Handle ThreadDispatch(TaskData *taskData, Handle args, Handle code)
{
    return processesModule.ThreadDispatch(taskData, args, code);
}

Handle Processes::ThreadDispatch(TaskData *taskData, Handle args, Handle code)
{
    int c = get_C_long(taskData, DEREFWORDHANDLE(code));
    ProcessTaskData *ptaskData = (ProcessTaskData *)taskData;
    switch (c)
    {
    case 1: /* A mutex was locked i.e. the count was ~1 or less.  We will have set it to
               ~1. This code blocks if the count is still ~1.  It does actually return
               if another thread tries to lock the mutex and hasn't yet set the value
               to ~1 but that doesn't matter since whenever we return we simply try to
               get the lock again. */
        {
            schedLock.Lock();
            // We have to check the value again with schedLock held rather than
            // simply waiting because otherwise the unlocking thread could have
            // set the variable back to 1 (unlocked) and signalled any waiters
            // before we actually got to wait.  
            if (UNTAGGED(DEREFHANDLE(args)->Get(0)) < 0)
            {
                // Set this so we can see what we're blocked on.
                ptaskData->blockMutex = DEREFHANDLE(args);
                // Now release the ML memory.  A GC can start.
                ThreadReleaseMLMemoryWithSchedLock(ptaskData);
                // Wait until we're woken up.  We mustn't block if we have been
                // interrupted, and are processing interrupts asynchronously, or
                // we've been killed.
                switch (ptaskData->requests)
                {
                case kRequestKill:
                    // We've been killed.  Handle this later.
                    break;
                case kRequestInterrupt:
                    {
                        // We've been interrupted.  
                        POLYUNSIGNED attrs = ThreadAttrs(ptaskData) & PFLAG_INTMASK;
                        if (attrs == PFLAG_ASYNCH || attrs == PFLAG_ASYNCH_ONCE)
                            break;
                        // If we're ignoring interrupts or handling them synchronously
                        // we don't do anything here.
                    }
                case kRequestNone:
                    ptaskData->threadLock.Wait(&schedLock);
                }
                ptaskData->blockMutex = 0; // No longer blocked.
                ThreadUseMLMemoryWithSchedLock(ptaskData);
            }
            // Return and try and get the lock again.
            schedLock.Unlock();
            // Test to see if we have been interrupted and if this thread
            // processes interrupts asynchronously we should raise an exception
            // immediately.  Perhaps we do that whenever we exit from the RTS.
            return SAVE(TAGGED(0));
       }

    case 2: /* Unlock a mutex.  Called after incrementing the count and discovering
               that at least one other thread has tried to lock it.  We may need
               to wake up threads that are blocked. */
        {
            // The caller has already set the variable to 1 (unlocked).
            // We need to acquire schedLock so that we can
            // be sure that any thread that is trying to lock sees either
            // the updated value (and so doesn't wait) or has successfully
            // waited on its threadLock (and so will be woken up).
            schedLock.Lock();
            // Unlock any waiters.
            for (unsigned i = 0; i < taskArraySize; i++)
            {
                ProcessTaskData *p = taskArray[i];
                // If the thread is blocked on this mutex we can signal the thread.
                if (p && p->blockMutex == DEREFHANDLE(args))
                    p->threadLock.Signal();
            }
            schedLock.Unlock();
            return SAVE(TAGGED(0));
       }

    case 3: // Atomically drop a mutex and wait for a wake up.  
        {
            // The argument is a pair of a mutex and the time to wake up.  The time
            // may be zero to indicate an infinite wait.  The return value is unit.
            // It WILL NOT RAISE AN EXCEPTION unless it is set to handle exceptions
            // asynchronously (which it shouldn't do if the ML caller code is correct).
            // It may return as a result of any of the following:
            //      an explicit wake up.
            //      an interrupt, either direct or broadcast
            //      a trap i.e. a request to handle an asynchronous event.
            schedLock.Lock();
            Handle mutexH = SAVE(args->WordP()->Get(0));
            Handle wakeTime = SAVE(args->WordP()->Get(1));
            // Atomically release the mutex.  This is atomic because we hold schedLock
            // so no other thread can call signal or broadcast.
            Handle decrResult = machineDependent->AtomicDecrement(taskData, mutexH);
            if (UNTAGGED(decrResult->Word()) != 1)
            {
                DEREFHANDLE(mutexH)->Set(0, TAGGED(1)); // Set this to released.
                // The mutex was locked so we have to release any waiters.
                // Unlock any waiters.
                for (unsigned i = 0; i < taskArraySize; i++)
                {
                    ProcessTaskData *p = taskArray[i];
                    // If the thread is blocked on this mutex we can signal the thread.
                    if (p && p->blockMutex == DEREFHANDLE(args))
                        p->threadLock.Signal();
                }
            }
            // Wait until we're woken up.  Don't block if we have been interrupted
            // or killed.
            if (ptaskData->requests == kRequestNone)
            {
                // Now release the ML memory.  A GC can start.
                ThreadReleaseMLMemoryWithSchedLock(ptaskData);
                // We pass zero as the wake time to represent infinity.
                if (compareLong(taskData, wakeTime, SAVE(TAGGED(0))) == 0)
                    ptaskData->threadLock.Wait(&schedLock);
                else
                {
#ifdef HAVE_PTHREAD
                    struct timespec tWake;
                    // On Unix we represent times as a number of microseconds.
                    Handle hMillion = Make_arbitrary_precision(taskData, 1000000);
                    tWake.tv_sec =
                        get_C_ulong(taskData, DEREFWORDHANDLE(div_longc(taskData, hMillion, wakeTime)));
                    tWake.tv_nsec =
                        1000*get_C_ulong(taskData, DEREFWORDHANDLE(rem_longc(taskData, hMillion, wakeTime)));
                    ptaskData->threadLock.WaitUntil(&schedLock, &tWake);
#elif defined(HAVE_WINDOWS_H)
                    // On Windows it is the number of 100ns units since the epoch
                    FILETIME tWake;
                    get_C_pair(taskData, DEREFWORDHANDLE(wakeTime),
                        (unsigned long*)&tWake.dwHighDateTime, (unsigned long*)&tWake.dwLowDateTime);
                    ptaskData->threadLock.WaitUntil(&schedLock, &tWake);
#endif
                    // get_C_ulong and get_C_pair could possibly raise exceptions.
                }
                // We want to use the memory again.
                ThreadUseMLMemoryWithSchedLock(ptaskData);
            }
            schedLock.Unlock();
            return SAVE(TAGGED(0));
        }

    case 4: // Wake up the specified thread.  Returns false (0) if the thread has
        // already been interrupted and is not ignoring interrupts or if the thread
        // does not exist (i.e. it's been killed while waiting).  Returns true
        // if it successfully woke up the thread.  The thread may subsequently
        // receive an interrupt but we need to know whether we woke the thread
        // up before that happened.
        {
            int result = 0; // Default to failed.
            // Acquire the schedLock first.  This ensures that this is
            // atomic with respect to waiting.
            schedLock.Lock();
            ProcessTaskData *p = TaskForIdentifier(args);
            if (p && p->threadObject == args->WordP())
            {
                POLYUNSIGNED attrs = ThreadAttrs(p) & PFLAG_INTMASK;
                if (p->requests == kRequestNone ||
                    (p->requests == kRequestInterrupt && attrs == PFLAG_IGNORE))
                {
                    p->threadLock.Signal();
                    result = 1;
                }
            }
            schedLock.Unlock();
            return SAVE(TAGGED(result));
        }

        // 5 and 6 are no longer used.

    case 7: // Fork a new thread.  The arguments are the function to run and the attributes.
            return ForkThread(ptaskData, SAVE(args->WordP()->Get(0)),
                        (Handle)0, args->WordP()->Get(1));

    case 8: // Test if a thread is active
        {
            schedLock.Lock();
            ProcessTaskData *p = TaskForIdentifier(args);
            schedLock.Unlock();
            return SAVE(TAGGED(p != 0));
        }

    case 9: // Send an interrupt to a specific thread
        {
            schedLock.Lock();
            ProcessTaskData *p = TaskForIdentifier(args);
            if (p) MakeRequest(p, kRequestInterrupt);
            schedLock.Unlock();
            if (p == 0)
                raise_exception_string(taskData, EXC_thread, "Thread does not exist");
            return SAVE(TAGGED(0));
        }

    case 10: // Broadcast an interrupt to all threads that are interested.
        BroadcastInterrupt();
        return SAVE(TAGGED(0));

    case 11: // Interrupt this thread now if it has been interrupted
        TestSynchronousRequests(taskData);
        return SAVE(TAGGED(0));

    case 12: // Kill a specific thread
        {
            schedLock.Lock();
            ProcessTaskData *p = TaskForIdentifier(args);
            if (p) MakeRequest(p, kRequestKill);
            schedLock.Unlock();
            if (p == 0)
                raise_exception_string(taskData, EXC_thread, "Thread does not exist");
            return SAVE(TAGGED(0));
        }

    case 13: // Return the number of processors.
        // Returns 1 if there is any problem.
        {
#ifdef WIN32
            SYSTEM_INFO info;
            memset(&info, 0, sizeof(info));
            GetSystemInfo(&info);
            if (info.dwNumberOfProcessors == 0) // Just in case
                info.dwNumberOfProcessors = 1;
            return Make_unsigned(taskData, info.dwNumberOfProcessors);
#elif(defined(_SC_NPROCESSORS_ONLN))
            long res = sysconf(_SC_NPROCESSORS_ONLN);
            if (res <= 0) res = 1;
            return Make_arbitrary_precision(taskData, res);
#elif(defined(HAVE_SYSCTL) && defined(CTL_HW) && defined(HW_NCP))
            static int mib[2] = { CTL_HW, HW_NCP };
            int nCPU = 1;
            size_t len = sizeof(nCPU);
            if (sysctl(mib, 2, &nCPU, &len, NULL, 0) == 0 && len == sizeof(nCPU))
                 return Make_unsigned(taskData, nCPU);
            else return Make_unsigned(taskData, 1);
#else
            // Can't determine.
            return Make_unsigned(taskData, 1);
#endif
        }

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown thread function: %d", c);
            raise_exception_string(taskData, EXC_Fail, msg);
			return 0;
        }
    }
}

ProcessTaskData::ProcessTaskData(): requests(kRequestNone), blockMutex(0), inMLHeap(false),
        runningProfileTimer(false)
{
#ifdef WINDOWS_PC
    lastCPUTime = 0;
#endif
#ifdef HAVE_PTHREAD
    pthreadId = 0;
#elif (defined(HAVE_WINDOWS_H))
    threadHandle = 0;
#endif
}

ProcessTaskData::~ProcessTaskData()
{
#if(!defined(HAVE_PTHREAD) && defined(HAVE_WINDOWS_H))
    if (threadHandle) CloseHandle(threadHandle);
#endif
}


// Find a task that matches the specified identifier and returns
// it if it exists.  MUST be called with taskArrayLock held.
ProcessTaskData *Processes::TaskForIdentifier(Handle taskId)
{
    // The index is in the first word of the thread object.
    unsigned index = UNTAGGED_UNSIGNED(taskId->WordP()->Get(0));
    // Check the index is valid and matches the object stored in the table.
    if (index < taskArraySize)
    {
        ProcessTaskData *p = taskArray[index];
        if (p && p->threadObject == taskId->WordP())
            return p;
    }
    return 0;
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
        machineDependent->SetException(taskData, processesModule.interrupt_exn);
        return;
    }

    POLYUNSIGNED new_len; /* New size */
    for (new_len = old_len; new_len < min_size; new_len *= 2);
    if (new_len > MAX_OBJECT_SIZE) new_len = MAX_OBJECT_SIZE;

    /* Must make a new frame and copy the data over. */
    StackObject *new_stack = // N.B.  May throw a C++ exception.
        (StackObject *)alloc(taskData, new_len, F_MUTABLE_BIT|F_STACK_BIT);
    CopyStackFrame(taskData->stack, new_stack);
    taskData->stack = new_stack;
}

/******************************************************************************/
/*                                                                            */
/*      shrink_stack - called from sparc_assembly.s - allocates               */
/*                                                                            */
/******************************************************************************/
Handle shrink_stack_c(TaskData *taskData, Handle reserved_space)
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
    new_stack = (StackObject *)alloc(taskData, new_len, F_MUTABLE_BIT|F_STACK_BIT);
    CopyStackFrame(taskData->stack, new_stack);
    taskData->stack = new_stack;    
    return SAVE(TAGGED(0));
}

// This is only ever used when compiling the compiler in an
// interactive session.
Handle install_rootc(TaskData *, Handle proc)
/* Installs a new procedure as the root. This involves killing all the processes
   and starting a new process with this root. */
{
    processesModule.BeginRootThread(DEREFHANDLE(proc)); // Should never return.
    finish(0); // If it does.
}

// Broadcast an interrupt to all relevant threads.
void Processes::BroadcastInterrupt(void)
{
    // If a thread is set to accept broadcast interrupts set it to
    // "interrupted".
    schedLock.Lock();
    for (unsigned i = 0; i < taskArraySize; i++)
    {
        ProcessTaskData *p = taskArray[i];
        if (p)
        {
            POLYUNSIGNED attrs = ThreadAttrs(p);
            if (attrs & PFLAG_BROADCAST)
                MakeRequest(p, kRequestInterrupt);
        }
    }
    schedLock.Unlock();
}

// There is no memory to satisfy the request from this thread. Just raises
// an exception in this thread.
void Processes::MemoryExhausted(TaskData *taskData)
{
    // This may raise an exception in a thread that isn't expecting
    // asynchronous exceptions.  Since this is really a last ditch
    // attempt it's probably the easiest way to deal with it.
    machineDependent->SetException(taskData, interrupt_exn);
}

// Set the asynchronous request variable for the thread.  Must be called
// with the schedLock held.  Tries to wake the thread up if possible.
void Processes::MakeRequest(ProcessTaskData *p, ThreadRequests request)
{
    // We don't override a request to kill by an interrupt request.
    if (p->requests < request)
    {
        p->requests = request;
        machineDependent->InterruptCode(p);
        p->threadLock.Signal();
        // Set the value in the ML object as well so the ML code can see it
        p->threadObject->Set(3, TAGGED(request));
    }
#ifdef HAVE_WINDOWS_H
    // Wake any threads waiting for IO
    PulseEvent(hWakeupEvent);
#endif
}

void Processes::ThreadExit(TaskData *taskData)
{
    schedLock.Lock();
    ThreadReleaseMLMemoryWithSchedLock(taskData); // Allow a GC if it was waiting for us.
    // Remove this from the taskArray
    unsigned index = UNTAGGED(taskData->threadObject->Get(0));
    if (index < taskArraySize && taskArray[index] == taskData)
        taskArray[index] = 0;
    delete(taskData);
    schedLock.Unlock();
#ifdef HAVE_PTHREAD
    pthread_exit(0);
#elif defined(HAVE_WINDOWS_H)
    ExitThread(0);
#else
    finish(0);
#endif
}

// These two functions are used for calls from outside where
// the lock has not yet been acquired.
void Processes::ThreadUseMLMemory(TaskData *taskData)
{
    // Trying to acquire the lock here may block if a GC is in progress
    schedLock.Lock();
    ThreadUseMLMemoryWithSchedLock(taskData);
    schedLock.Unlock();
    if (requestRTSState == krequestRTSToInterrupt) // The lock was held when we did this before.
        RequestThreadsEnterRTS(true);
}

void Processes::ThreadReleaseMLMemory(TaskData *taskData)
{
    schedLock.Lock();
    ThreadReleaseMLMemoryWithSchedLock(taskData);
    schedLock.Unlock();
}

void Processes::ThreadUseMLMemoryWithSchedLock(TaskData *taskData)
{
    ProcessTaskData *ptaskData = (ProcessTaskData *)taskData;
    while (wantGC)
    {
        // Allow the GC to happen.  Waiting here on gcComplete ensures that
        // the GC thread has actually been allowed to run.  If it has it
        // will have taken schedLock and we won't have got this far but if
        // we call ThreadReleaseMLMemoryWithSchedLock and then call
        // ThreadUseMLMemoryWithSchedLock shortly afterwards there may not
        // have been enough time for the signal to get through.
        gcBegin.Signal();
        gcComplete.Wait(&schedLock);
    }
    ASSERT(! ptaskData->inMLHeap);
    ptaskData->inMLHeap = true;
}

void Processes::ThreadReleaseMLMemoryWithSchedLock(TaskData *taskData)
{
    ProcessTaskData *ptaskData = (ProcessTaskData *)taskData;
    ASSERT(ptaskData->inMLHeap);
    ptaskData->inMLHeap = false;
    // Put a dummy object in any unused space.  This maintains the
    // invariant that the allocated area is filled with valid objects.
    if (ptaskData->allocPointer > ptaskData->allocLimit)
    {
        PolyObject *dummy = (PolyObject *)(ptaskData->allocLimit+1);
        POLYUNSIGNED space = ptaskData->allocPointer-ptaskData->allocLimit-1;
        // Make this a byte object so it's always skipped.
        dummy->SetLengthWord(space, F_BYTE_BIT);
    }
    if (wantGC) // A thread is waiting to do a GC.  Wake it up because it may now be able to run.
        gcBegin.Signal();
}

bool Processes::BeginGC(TaskData *taskData)
{
    schedLock.Lock();
    bool wasGC = wantGC; // Does another thread want to do a GC?
    // Allow another thread to GC if it wants to.
    ThreadReleaseMLMemoryWithSchedLock(taskData);
    ThreadUseMLMemoryWithSchedLock(taskData);
    // If another thread has just done a GC we may now be able
    // to satisfy our memory needs.  Don't start another GC here.
    if (wasGC)
    {
        schedLock.Unlock(); // Unlock the lock before returning.
        return false;
    }

    // We want to do a garbage collection.
    // We already have schedLock.
    wantGC = true;
    bool allStopped = false;

    while (! allStopped)
    {
        allStopped = true;
        for (unsigned i = 0; i < taskArraySize; i++)
        {
            ProcessTaskData *p = taskArray[i];
            if (p && p != taskData && p->inMLHeap)
            {
                allStopped = false;
                // It must be running - interrupt it.
                machineDependent->InterruptCode(p);
            }
        }
        // Wait until they are all stopped.
        if (! allStopped)
            gcBegin.Wait(&schedLock);
    }
    return true; // Everything else is now stopped.
}

void Processes::EndGC(TaskData *taskData)
{
    wantGC = false;
    gcComplete.Signal(); // Wake up any waiting threads.
    schedLock.Unlock();
}

Handle exitThread(TaskData *taskData)
/* A call to this is put on the stack of a new thread so when the
   thread function returns the thread goes away. */  
{
    processesModule.ThreadExit(taskData);
}


void Processes::BlockAndRestart(TaskData *taskData, int fd, bool posixInterruptable, int ioCall)
/* The process is waiting for IO or for a timer.  Suspend it and
   then restart it later.  fd may be negative if the file descriptor
   value is not relevant.
   If this is interruptable (currently only used for Posix functions)
   the process will be set to raise an exception if a signal is handled. */
{
    machineDependent->SetForRetry(taskData, ioCall);
    TestSynchronousRequests(taskData); // Consider this a blocking call that may raise Interrupt
    ThreadReleaseMLMemory(taskData);
#ifdef WINDOWS_PC
    /* It's too complicated in Windows to try and wait for a stream.
       We simply wait for half a second or until a Windows message
       arrives. */

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

    // Wait until we get input or we're woken up.
    MsgWaitForMultipleObjects(1, &hWakeupEvent, FALSE, 500, QS_ALLINPUT);
#else
    fd_set read_fds, write_fds, except_fds;
    struct timeval toWait = { 0, 500000 }; /* Half a second. */

    FD_ZERO(&read_fds);
    if (fd >= 0) FD_SET(fd, &read_fds);
    FD_ZERO(&write_fds);
    FD_ZERO(&except_fds);
    if (select(FD_SETSIZE, &read_fds, &write_fds, &except_fds, &toWait) < 0 &&
            errno == EINTR && posixInterruptable)
    {
        ThreadUseMLMemory(taskData);
        raise_syscall(taskData, "Call interrupted by signal", EINTR);
    }
#endif
    ThreadUseMLMemory(taskData);
    TestSynchronousRequests(taskData); // Check if we've been interrupted.

    throw IOException(EXC_EXCEPTION);
    /* NOTREACHED */
}

void Processes::BlockAndRestartIfNecessary(TaskData *taskData, int fd, int ioCall)
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

    BlockAndRestart(taskData, fd, false, ioCall);
    /* NOTREACHED */
}

// Get the task data for the current thread.  This is held in
// thread-local storage.  Normally this is passed in taskData but
// in a few cases this isn't available.
TaskData *Processes::GetTaskDataForThread(void)
{
#ifdef HAVE_PTHREAD
    return (TaskData *)pthread_getspecific(tlsId);
#elif defined(HAVE_WINDOWS_H)
    return (TaskData *)TlsGetValue(tlsId);
#else
    // If there's no threading.
    return taskArray[0];
#endif
}

// This function is run when a new thread has been forked.  The
// parameter is the taskData value for the new thread.  This function
// is also called directly for the main thread.
#ifdef HAVE_PTHREAD
static void *NewThreadFunction(void *parameter)
{
    ProcessTaskData *taskData = (ProcessTaskData *)parameter;
    pthread_setspecific(processesModule.tlsId, taskData);
    processes->ThreadUseMLMemory(taskData);
    (void)EnterPolyCode(taskData); // Will normally (always?) call finish directly
    return 0;
}
#elif defined(HAVE_WINDOWS_H)
static DWORD WINAPI NewThreadFunction(void *parameter)
{
    ProcessTaskData *taskData = (ProcessTaskData *)parameter;
    TlsSetValue(processesModule.tlsId, taskData);
    processes->ThreadUseMLMemory(taskData);
    (void)EnterPolyCode(taskData);
    return 0;
}
#else
static void NewThreadFunction(void *parameter)
{
    ProcessTaskData *taskData = (ProcessTaskData *)parameter;
    processes->ThreadUseMLMemory(taskData);
    (void)EnterPolyCode(taskData);
}
#endif

// Sets up the initial thread from the root function.  This is run on
// the initial thread of the process so it will work if we don't
// have pthreads.
void Processes::BeginRootThread(PolyObject *rootFunction)
{
    if (taskArraySize < 1)
    {
        taskArray = (ProcessTaskData **)realloc(taskArray, sizeof(ProcessTaskData *));
        taskArraySize = 1;
    }
    // The root thread has to be handled differently from the other
    // threads because we don't have a taskData object before we start
    ProcessTaskData *taskData = new ProcessTaskData;
    taskData->mdTaskData = machineDependent->CreateTaskData();
    taskData->threadObject = (ThreadObject*)alloc(taskData, 4, F_MUTABLE_BIT);
    taskData->threadObject->index = TAGGED(0); // Index 0
    // The initial thread is set to accept broadcast interrupt requests
    // and handle them synchronously.  This is for backwards compatibility.
    taskData->threadObject->flags = TAGGED(PFLAG_BROADCAST|PFLAG_ASYNCH); // Flags
    taskData->threadObject->threadLocal = TAGGED(0); // Empty thread-local store
    taskData->threadObject->requestCopy = TAGGED(0); // Cleared interrupt state
#ifdef HAVE_PTHREAD
    taskData->pthreadId = pthread_self();
#elif defined(HAVE_WINDOWS_H)
    taskData->threadHandle = hMainThread;
#endif
    taskArray[0] = taskData;

    Handle stack =
        alloc_and_save(taskData, machineDependent->InitialStackSize(), F_MUTABLE_BIT|F_STACK_BIT);
    taskData->stack = (StackObject *)DEREFHANDLE(stack);
    machineDependent->InitStackFrame(taskData, stack,
            taskData->saveVec.push(rootFunction), (Handle)0);

    // Create a packet for the Interrupt exception once so that we don't have to
    // allocate when we need to raise it.
    // We can only do this once the taskData object has been created.
    if (interrupt_exn == 0)
        interrupt_exn =
            DEREFEXNHANDLE(make_exn(taskData, EXC_interrupt, taskData->saveVec.push(TAGGED(0))));

    // Enter the code as if this were a new thread.
    NewThreadFunction(taskData);
}

// Create a new thread.  Returns the ML thread identifier object if it succeeds.
// May raise an exception.
Handle Processes::ForkThread(ProcessTaskData *taskData, Handle threadFunction,
                           Handle args, PolyWord flags)
{
#if (!defined(HAVE_PTHREAD) && ! defined(HAVE_WINDOWS_H))
    raise_exception_string(taskData, EXC_thread, "Threads not available on this platform");
#else
    // Create a taskData object for the new thread
    ProcessTaskData *newTaskData = new ProcessTaskData;
    newTaskData->mdTaskData = machineDependent->CreateTaskData();
    // We allocate the thread object in the PARENT's space
    Handle threadId = alloc_and_save(taskData, 4, F_MUTABLE_BIT);
    newTaskData->threadObject = (ThreadObject*)DEREFHANDLE(threadId);
    newTaskData->threadObject->index = TAGGED(0);
    newTaskData->threadObject->flags = flags; // Flags
    newTaskData->threadObject->threadLocal = TAGGED(0); // Empty thread-local store
    newTaskData->threadObject->requestCopy = TAGGED(0); // Cleared interrupt state

    unsigned thrdIndex;
    schedLock.Lock();
    // See if there's a spare entry in the array.
    for (thrdIndex = 0;
         thrdIndex < taskArraySize && taskArray[thrdIndex] != 0;
         thrdIndex++);

    if (thrdIndex == taskArraySize) // Need to expand the array
    {
        ProcessTaskData **newArray =
            (ProcessTaskData **)realloc(taskArray, sizeof(ProcessTaskData *)*(taskArraySize+1));
        if (newArray)
        {
            taskArray = newArray;
            taskArraySize++;
        }
        else
        {
            delete(newTaskData);
            schedLock.Unlock();
            raise_exception_string(taskData, EXC_thread, "Too many threads");
        }
    }
    // Add into the new entry
    taskArray[thrdIndex] = newTaskData;
    newTaskData->threadObject->Set(0, TAGGED(thrdIndex)); // Set to the index
    schedLock.Unlock();

    Handle stack = // Allocate the stack in the parent's heap.
        alloc_and_save(taskData, machineDependent->InitialStackSize(), F_MUTABLE_BIT|F_STACK_BIT);
    newTaskData->stack = (StackObject *)DEREFHANDLE(stack);
    machineDependent->InitStackFrame(newTaskData, stack, threadFunction, args);

    // Now actually fork the thread.
    bool success = 0;
    schedLock.Lock();
#ifdef HAVE_PTHREAD
    // Create a thread that isn't joinable since we don't want to wait
    // for it to finish.
    pthread_attr_t attrs;
    pthread_attr_init(&attrs);
    pthread_attr_setdetachstate(&attrs, PTHREAD_CREATE_DETACHED);
    success = pthread_create(&newTaskData->pthreadId, &attrs, NewThreadFunction, newTaskData) == 0;
    pthread_attr_destroy(&attrs);
#elif defined(HAVE_WINDOWS_H)
    DWORD dwThrdId; // Have to provide this although we don't use it.
    newTaskData->threadHandle =
        CreateThread(NULL, 0, NewThreadFunction, newTaskData, 0, &dwThrdId);
    success = newTaskData->threadHandle != NULL;
#endif
    if (success)
    {
        schedLock.Unlock();
        return threadId;
    }
    // Thread creation failed.
    taskArray[thrdIndex] = 0;
    delete(newTaskData);
    schedLock.Unlock();
    raise_exception_string(taskData, EXC_thread, "Thread creation failed");
#endif
}

// ForkFromRTS.  Creates a new thread from within the RTS.  This is currently used
// only to run a signal function.
bool Processes::ForkFromRTS(TaskData *taskData, Handle proc, Handle arg)
{
    try {
        (void)ForkThread((ProcessTaskData*)taskData, proc, arg, TAGGED(PFLAG_SYNCH));
        return true;
    } catch (IOException)
    {
        // If it failed
        return false;
    }
}

// Deal with any interrupt or kill requests.
void Processes::ProcessAsynchRequests(TaskData *taskData)
{
    ProcessTaskData *ptaskData = (ProcessTaskData *)taskData;

    schedLock.Lock();
    // Make any calls to the RTS interrupt functions.
    while (requestRTSState != krequestRTSNone)
    {
        // We may get another trap request while processing the last.
        requestRTSState = krequestRTSNone;
        schedLock.Unlock();
        InterruptModules(taskData);
        schedLock.Lock();
    }

    switch (ptaskData->requests)
    {
    case kRequestNone:
        schedLock.Unlock();
        break;

    case kRequestInterrupt:
        {
            // Handle asynchronous interrupts only.
            // We've been interrupted.  
            POLYUNSIGNED attrs = ThreadAttrs(ptaskData);
            POLYUNSIGNED intBits = attrs & PFLAG_INTMASK;
            if (intBits == PFLAG_ASYNCH || intBits == PFLAG_ASYNCH_ONCE)
            {
                if (intBits == PFLAG_ASYNCH_ONCE)
                {
                    // Set this so from now on it's synchronous.
                    // This word is only ever set by the thread itself so
                    // we don't need to synchronise.
                    attrs = (attrs & (~PFLAG_INTMASK)) | PFLAG_SYNCH;
                    ptaskData->threadObject->Set(1, TAGGED(attrs));
                }
                ptaskData->requests = kRequestNone; // Clear this
                ptaskData->threadObject->Set(3, TAGGED(0)); // And in the ML copy
                schedLock.Unlock();
                // Don't actually throw the exception here.
                machineDependent->SetException(taskData, interrupt_exn);
            }
            else schedLock.Unlock();
        }
        break;

    case kRequestKill: // The thread has been asked to stop.
        schedLock.Unlock();
        ThreadExit(taskData);
        // Doesn't return.
    }
    if (requestRTSState == krequestRTSToInterrupt) // The lock was held before
        RequestThreadsEnterRTS(true);

#ifndef WINDOWS_PC
    // Start the profile timer if needed.
    if (profileMode == kProfileTime)
    {
        if (! ptaskData->runningProfileTimer)
        {
            ptaskData->runningProfileTimer = true;
            StartProfilingTimer();
        }
    }
    else ptaskData->runningProfileTimer = false;
    // The timer will be stopped next time it goes off.
#endif
}

// If this thread is processing interrupts synchronously and has been
// interrupted clear the interrupt and raise the exception.  This is
// called from IO routines which may block.
void Processes::TestSynchronousRequests(TaskData *taskData)
{
    ProcessTaskData *ptaskData = (ProcessTaskData *)taskData;
    schedLock.Lock();
    switch (ptaskData->requests)
    {
    case kRequestNone:
        schedLock.Unlock();
        break;

    case kRequestInterrupt:
        {
            // Handle synchronous interrupts only.
            // We've been interrupted.  
            POLYUNSIGNED attrs = ThreadAttrs(ptaskData);
            POLYUNSIGNED intBits = attrs & PFLAG_INTMASK;
            if (intBits == PFLAG_SYNCH)
            {
                ptaskData->requests = kRequestNone; // Clear this
                ptaskData->threadObject->Set(3, TAGGED(0));
                schedLock.Unlock();
                machineDependent->SetException(taskData, interrupt_exn);
                throw IOException(EXC_EXCEPTION);
            }
            else schedLock.Unlock();
        }
        break;

    case kRequestKill: // The thread has been asked to stop.
        schedLock.Unlock();
        ThreadExit(taskData);
        // Doesn't return.
    }
}

/* CALL_IO1(install_subshells_, REF, NOIND) */
Handle install_subshells_c(TaskData *taskData, Handle root_function)
{
    return SAVE(TAGGED(0));
}

/* CALL_IO0(switch_subshells_,NOIND) */
Handle switch_subshells_c(TaskData *taskData)
{
    return SAVE(TAGGED(0));
}

void Processes::RequestThreadsEnterRTSInternal(bool isSignal)
{
    requestRTSState = krequestRTSInterrupted;
    for (unsigned i = 0; i < taskArraySize; i++)
    {
        ProcessTaskData *taskData = taskArray[i];
        if (taskData)
        {
            // Do this with the lock held.
            machineDependent->InterruptCode(taskData);
#ifdef HAVE_PTHREAD
            if (isSignal)
                pthread_kill(taskData->pthreadId, SIGALRM);
#endif
        }
    }
#ifdef HAVE_WINDOWS_H
    // Wake any threads waiting for IO
    PulseEvent(hWakeupEvent);
#endif
}

#if (! defined(HAVE_PTHREAD) && defined(HAVE_WINDOWS_H))
DWORD WINAPI CallRequestEnterRTS(LPVOID)
{
    processesModule.schedLock.Lock();
    processesModule.RequestThreadsEnterRTSInternal(false);
    processesModule.schedLock.Unlock();
    return 0;
}
#endif

// May be called at any time in any state.  In particular schedLock may be held
// at the moment.  In Unix we test the lock and set a flag if the lock is currently
// taken.  Since we must be in the RTS to take the lock we should test the flag
// soon.  In Windows we fork a thread to take the lock and deal with this.
void Processes::RequestThreadsEnterRTS(bool isSignal)
{
#if (! defined(HAVE_PTHREAD) && defined(HAVE_WINDOWS_H))
    // TryLock doesn't work properly in Windows.  Instead we fork off
    // a function to call this when we can get the lock.
    DWORD dwThrdId;
    HANDLE hThread = CreateThread(NULL, 0, CallRequestEnterRTS, 0, 0, &dwThrdId);
    if (hThread) CloseHandle(hThread); // Don't want this.
    else requestRTSState = krequestRTSToInterrupt;
#else
    if (schedLock.Trylock())
    {
        RequestThreadsEnterRTSInternal(isSignal);
        schedLock.Unlock();
    }
    else // schedLock is currently held.
        requestRTSState = krequestRTSToInterrupt;
#endif
}

// Set all threads to exit.
void Processes::KillAllThreads(void)
{
    schedLock.Lock();
    for (unsigned i = 0; i < taskArraySize; i++)
    {
        ProcessTaskData *taskData = taskArray[i];
        if (taskData)
            MakeRequest(taskData, kRequestKill);
    }
    schedLock.Unlock();
}

#ifdef HAVE_PTHREAD
static void catchALRM(SIG_HANDLER_ARGS(sig, context))
// This doesn't need to do anything.
{
    (void)sig;
    (void)context;
}
#endif
/******************************************************************************/
/*                                                                            */
/*      catchVTALRM - handler for alarm-clock signal                          */
/*                                                                            */
/******************************************************************************/
#if !defined(WINDOWS_PC)
static void catchVTALRM(SIG_HANDLER_ARGS(sig, context))
{
    ASSERT(sig == SIGVTALRM);
    if (profileMode != kProfileTime)
    {
        // We stop the timer for this thread on the next signal after we end profile
        static struct itimerval stoptime = {{0, 0}, {0, 0}};
        /* Stop the timer */
        setitimer(ITIMER_VIRTUAL, & stoptime, NULL);
    }
    else {
        TaskData *taskData = processes->GetTaskDataForThread();
        handleProfileTrap(taskData, (SIGNALCONTEXT*)context);
    }
}

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
void Processes::ProfileInterrupt(void)
{               
    // Wait for millisecond or until the stop event is signalled.
    while (WaitForSingleObject(hStopEvent, 1) == WAIT_TIMEOUT)        
    {
        schedLock.Lock();
        for (unsigned i = 0; i < taskArraySize; i++)
        {
            ProcessTaskData *p = taskArray[i];
            if (p && p->threadHandle)
            {
                FILETIME cTime, eTime, kTime, uTime;
                bool includeThread = false;
                // Try to get the thread CPU time if possible.  This isn't supported
                // in Windows 95/98 so if it fails we just include this thread anyway.
                if (GetThreadTimes(p->threadHandle, &cTime, &eTime, &kTime, &uTime))
                {
                    LONGLONG totalTime = 0;
                    LARGE_INTEGER li;
                    li.LowPart = kTime.dwLowDateTime;
                    li.HighPart = kTime.dwHighDateTime;
                    totalTime += li.QuadPart;
                    li.LowPart = uTime.dwLowDateTime;
                    li.HighPart = uTime.dwHighDateTime;
                    totalTime += li.QuadPart;
                    if (totalTime - p->lastCPUTime >= 10000)
                    {
                        includeThread = true;
                        p->lastCPUTime = totalTime;
                    }
                }
                else includeThread = true; // Failed to get thread time, maybe Win95.
                if (includeThread)
                {
                    CONTEXT context;
                    SuspendThread(p->threadHandle);
                    context.ContextFlags = CONTEXT_CONTROL; /* Get Eip and Esp */
                    if (GetThreadContext(p->threadHandle, &context))
                    {
                        handleProfileTrap(p, &context);
                    }
                    ResumeThread(p->threadHandle);
                }
            }
        }
        schedLock.Unlock();
    }
}

DWORD WINAPI ProfilingTimer(LPVOID parm)
{
    processesModule.ProfileInterrupt();
    return 0;
}

#endif

    // Profiling control.
void Processes::StartProfiling(void)
{
#ifdef WINDOWS_PC
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
#else
    // In Linux, at least, we need to run a timer in each thread.
    RequestThreadsEnterRTS(false);
#endif
}

void Processes::StopProfiling(void)
{
#ifdef WINDOWS_PC
    if (hStopEvent) SetEvent(hStopEvent);
    // Wait for the thread to stop
    if (profilingHd) WaitForSingleObject(profilingHd, 10000);
    CloseHandle(profilingHd);
    profilingHd = NULL;
#else
#endif
}

void Processes::Init(void)
{
#ifdef HAVE_WINDOWS_H
    /* Create event to stop timeslice interrupts. */
    hWakeupEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
#endif
#ifdef HAVE_PTHREAD
    // We send a thread an alarm signal to wake it up if
    // it is blocking on an IO function.
    markSignalInuse(SIGALRM);
    setSignalHandler(SIGALRM, catchALRM);
#endif

#ifdef HAVE_PTHREAD
    pthread_key_create(&tlsId, NULL);
#elif defined(HAVE_WINDOWS_H)
    tlsId = TlsAlloc();
#endif

#if defined(WINDOWS_PC) /* PC version */
    // Create stop event for time profiling.
    hStopEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
#else
    // Set up a signal handler.  This will be the same for all threads.
    setSignalHandler(SIGVTALRM, catchVTALRM);
#endif
}

#ifndef WINDOWS_PC
// On Linux, at least, each thread needs to run this.
void Processes::StartProfilingTimer(void)
{
    // set virtual timer to go off every millisecond
    struct itimerval starttime;
    starttime.it_interval.tv_sec = starttime.it_value.tv_sec = 0;
    starttime.it_interval.tv_usec = starttime.it_value.tv_usec = 1000;
    setitimer(ITIMER_VIRTUAL,&starttime,NULL);
}
#endif

void Processes::Reinit(void)
{
}

void Processes::Uninit(void)
{
    // Wait for threads to terminate?
#ifdef HAVE_WINDOWS_H
    if (hWakeupEvent) SetEvent(hWakeupEvent);

    if (hWakeupEvent) CloseHandle(hWakeupEvent);
    hWakeupEvent = NULL;
#else
#endif

#ifdef HAVE_PTHREAD
    pthread_key_delete(tlsId);
#elif defined(HAVE_WINDOWS_H)
    TlsFree(tlsId);
#endif

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
    profileMode = kProfileOff;
    // Make sure the timer is not running
    struct itimerval stoptime;
    memset(&stoptime, 0, sizeof(stoptime));
    setitimer(ITIMER_VIRTUAL, &stoptime, NULL);
#endif
}

void Processes::GarbageCollect(ScanAddress *process)
/* Ensures that all the objects are retained and their addresses updated. */
{   
    /* The interrupt exn */
    if (interrupt_exn != 0) {
        PolyObject *p = interrupt_exn;
        process->ScanRuntimeAddress(&p, ScanAddress::STRENGTH_STRONG);
        interrupt_exn = (PolyException*)p;
    }
    for (unsigned i = 0; i < taskArraySize; i++)
    {
        if (taskArray[i])
            taskArray[i]->GarbageCollect(process);
    }
}

void ProcessTaskData::GarbageCollect(ScanAddress *process)
{
    saveVec.gcScan(process);
    if (stack != 0)
    {
        PolyObject *p = stack;
        process->ScanRuntimeAddress(&p, ScanAddress::STRENGTH_STRONG);
        stack = (StackObject*)p;
    }
    if (threadObject != 0)
    {
        PolyObject *p = threadObject;
        process->ScanRuntimeAddress(&p, ScanAddress::STRENGTH_STRONG);
        threadObject = (ThreadObject*)p;
    }
    if (blockMutex != 0)
        process->ScanRuntimeAddress(&blockMutex, ScanAddress::STRENGTH_STRONG);
    // The allocation spaces are no longer valid.
    allocPointer = 0;
    allocLimit = 0;
    // Divide the allocation size by four. If we have made a single allocation
    // since the last GC the size will have been doubled after the allocation.
    // On average for each thread, apart from the one that ran out of space
    // and requested the GC, half of the space will be unused so reducing by
    // four should give a good estimate for next time.
    if (allocCount != 0)
    { // Do this only once for each GC.
        allocCount = 0;
        allocSize = allocSize/4;
        if (allocSize < MIN_HEAP_SIZE)
            allocSize = MIN_HEAP_SIZE;
    }
}
