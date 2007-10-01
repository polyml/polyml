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

#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
#endif

#include "globals.h"
#include "rts_module.h"
#include "save_vec.h"

#include "noreturn.h"

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class StackObject;
class PolyWord;
class ScanAddress;
class MDTaskData;
class Exporter;

#define MIN_HEAP_SIZE   4096 // Minimum and initial heap segment size (words)

// This is the ML "thread identifier" object.  The fields
// are read and set by the ML code.
class ThreadObject: public PolyObject {
public:
    PolyWord    index;  // Tagged integer with the index into the taskArray
                        // Not used by ML
    PolyWord    flags;  // Tagged integer containing flags indicating how interrupts
                        // are handled.  Set by ML but only by the thread itself
    PolyWord    threadLocal; // Head of a list of thread-local store items.
                        // Handled entirely by ML but only by the thread.
    PolyWord    requestCopy; // A tagged integer copy of the "requests" field.
                        // This is provided so that ML can easily test if there
                        // is an interrupt pending.
};

// Per-thread data.
class TaskData {
public:
    TaskData();
    virtual ~TaskData() {}

    void FillUnusedSpace(void);

    virtual void GarbageCollect(ScanAddress *process) = 0;
    virtual void Lock(void) = 0;
    virtual void Unlock(void) = 0;
    // Increment the allocation count and adjust the allocation size for
    // the next allocation.  Doubles the allocation for next time.
    virtual void IncrementAllocationCount(void) { allocCount++; allocSize = allocSize*2; }

    MDTaskData  *mdTaskData;    // Machine-specific task data.
    SaveVec     saveVec;
    PolyWord    *allocPointer;  // Allocation pointer - decremented towards...
    PolyWord    *allocLimit;    // ... lower limit of allocation
    POLYUNSIGNED allocSize;     // The preferred heap segment size
    unsigned    allocCount;     // The number of allocations since the last GC
    StackObject *stack;
    ThreadObject *threadObject;  // Pointer to the thread object.
    int         lastError;      // Last error from foreign code.
    Handle      x_ehandle, y_ehandle; // Space to extend short precision args.
private:
    PolyWord    x_extend[2], y_extend[2];
    SaveVecEntry x_extend_addr, y_extend_addr;
};

NORETURNFN(extern Handle exitThread(TaskData *mdTaskData));

Handle AtomicIncrement(TaskData *taskData, Handle mutexp);
Handle AtomicDecrement(TaskData *taskData, Handle mutexp);
Handle ThreadSelf(TaskData *taskData);
Handle ThreadDispatch(TaskData *taskData, Handle args, Handle code);

class ScanAddress;

// Data structure used for requests from a thread to the root
// thread.  These are GCs or similar.
class MainThreadRequest
{
public:
    MainThreadRequest (): completed(false) {}
    bool completed;
    virtual void Perform() = 0;
};

// External interface to the Process module.  These functions are all implemented
// by the Processes class.
class ProcessExternal
{
public:
    virtual ~ProcessExternal() {} // Defined to suppress a warning from GCC

    virtual TaskData *GetTaskDataForThread(void) = 0;
    virtual void RequestThreadsEnterRTS(bool isSignal) = 0;
    // Request all ML threads to exit and set the result code.  Does not cause
    // the calling thread itself to exit since this may be called on the GUI thread.
    virtual void Exit(int n) = 0;
    // Exit from this thread.
    virtual NORETURNFN(void ThreadExit(TaskData *taskData)) = 0;

    virtual void BroadcastInterrupt(void) = 0;

    virtual void BeginRootThread(PolyObject *rootFunction) = 0;
    // Called when a thread may block.  Never returns.  May cause a retry.
    virtual NORETURNFN(void BlockAndRestart(TaskData *taskData, int fd,
                    bool posixInterruptable, int ioCall)) = 0;
    // Called when a thread may block.  Returns some time later when perhaps
    // the input is available.
    virtual void ThreadPauseForIO(TaskData *taskData, int fd, bool posixInterruptable=false) = 0;
    // As ThreadPauseForIO but when there is no file descriptor
    virtual void ThreadPause(TaskData *taskData, bool posixInterruptable = false)
    { ThreadPauseForIO(taskData, -1, posixInterruptable); }

    // If a thread is blocking for some time it should release its use
    // of the ML memory.  That allows a GC. ThreadUseMLMemory returns true if
    // a GC was in progress.
    virtual void ThreadUseMLMemory(TaskData *taskData) = 0;
    virtual void ThreadReleaseMLMemory(TaskData *taskData) = 0;

    // Requests from the threads for actions that need to be performed by
    // the root thread.
    virtual void MakeRootRequest(TaskData *taskData, MainThreadRequest *request) = 0;

    // Deal with any interrupt or kill requests.
    virtual bool ProcessAsynchRequests(TaskData *taskData) = 0;
    // Process an interrupt request synchronously.
    virtual void TestSynchronousRequests(TaskData *taskData) = 0;
    
    // ForkFromRTS.  Creates a new thread from within the RTS.
    virtual bool ForkFromRTS(TaskData *taskData, Handle proc, Handle arg) = 0;

    // Profiling control.
    virtual void StartProfiling(void) = 0;
    virtual void StopProfiling(void) = 0;
    
    // Find space for an object.  Returns a pointer to the start.  "words" must include
    // the length word and the result points at where the length word will go.
    // If the allocation succeeds it may update the allocation values in the taskData object.
    // If the heap is exhausted it may set this thread (or other threads) to raise an exception.
    virtual PolyWord *FindAllocationSpace(TaskData *taskData, POLYUNSIGNED words, bool alwaysInSeg) = 0;
};


extern ProcessExternal *processes;

#define IO_SPACING 8 // This is a bit of a mess.

#endif
