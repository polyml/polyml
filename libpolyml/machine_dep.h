/*
    Title:  machine_dep.h - exports signature for machine_dep.c 

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

#ifndef _MACHINE_DEP_H
#define _MACHINE_DEP_H

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef HAVE_UCONTEXT_H
#include <ucontext.h>
#endif

#ifdef HAVE_UCONTEXT_T
#define SIGNALCONTEXT ucontext_t
#elif defined(HAVE_STRUCT_SIGCONTEXT)
#define SIGNALCONTEXT struct sigcontext
#elif defined(HAVE_WINDOWS_H)
#include <windows.h>
#define SIGNALCONTEXT CONTEXT // This is the thread context.
#else
#define SIGNALCONTEXT void
#endif

class ScanAddress;
class TaskData;
class SaveVecEntry;
typedef SaveVecEntry *Handle;
class StackSpace;

class MDTaskData {
public:
    virtual ~MDTaskData() {}
};

// Machine architecture values.
typedef enum {
    MA_Interpreted = 0,
    MA_I386,
    MA_PPC,
    MA_Sparc,
    MA_X86_64
} Architectures;

// Machine-dependent module.
class MachineDependent {
public:
    virtual ~MachineDependent() {} // Keep the compiler happy

    // Create the machine-specific task data object.
    virtual MDTaskData *CreateTaskData(void) = 0;

    virtual unsigned InitialStackSize(void) { return 128; } // Initial size of a stack 
    // Must be > 40 (i.e. 2*min_stack_check) + base area in each stack frame
    virtual void InitInterfaceVector(void) = 0;
    virtual void ResetSignals(void) {}

    /* ScanConstantsWithinCode - update addresses within a code segment.*/
    virtual void ScanConstantsWithinCode(PolyObject *addr, PolyObject *oldAddr, POLYUNSIGNED length, ScanAddress *process) {}
    void  ScanConstantsWithinCode(PolyObject *addr, ScanAddress *process)
        { ScanConstantsWithinCode(addr, addr, addr->Length(), process); } // Common case

    virtual int  GetIOFunctionRegisterMask(int ioCall) = 0;
    virtual void FlushInstructionCache(void *p, POLYUNSIGNED bytes) {}
    virtual Architectures MachineArchitecture(void) = 0; 

    // This is machine-dependent but not always required.  It is used in the code-generator.
    virtual void SetCodeConstant(TaskData *taskData, Handle data, Handle constant, Handle offseth, Handle base) {}

    // Switch to Poly and return with the io function to call.
    virtual int SwitchToPoly(TaskData *taskData) = 0;

    virtual void SetForRetry(TaskData *taskData, int ioCall) = 0;
    virtual void InterruptCode(TaskData *taskData) = 0;
    virtual bool GetPCandSPFromContext(TaskData *taskData, SIGNALCONTEXT *context, PolyWord * &sp,  POLYCODEPTR &pc) = 0;
    // Initialise the stack for a new thread.  Because this is called from the parent thread
    // the task data object passed in is that of the parent.
    virtual void InitStackFrame(TaskData *parentTaskData, StackSpace *space, Handle proc, Handle arg) = 0;
    virtual void SetException(TaskData *taskData, poly_exn *exc) = 0;
    // General RTS functions.
    virtual void CallIO0(TaskData *taskData, Handle(*ioFun)(TaskData *)) = 0;
    virtual void CallIO1(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle)) = 0;
    virtual void CallIO2(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle)) = 0;
    virtual void CallIO3(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle)) = 0;
    virtual void CallIO4(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle, Handle)) = 0;
    virtual void CallIO5(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle, Handle, Handle)) = 0;
    // These next two are sufficiently different that they need to be implemented
    // as special cases.
    virtual void SetExceptionTrace(TaskData *taskData, bool isLegacy) = 0;
    virtual void CallCodeTupled(TaskData *taskData) = 0;
    // This is used to get the argument to the callback_result function.
    virtual Handle CallBackResult(TaskData *taskData) = 0;
    // If a foreign function calls back to ML we need to set up the call to the
    // ML callback function.
    virtual void SetCallbackFunction(TaskData *taskData, Handle func, Handle args) {}

    // Increment or decrement the first word of the object pointed to by the
    // mutex argument and return the new value.
    virtual Handle AtomicIncrement(TaskData *taskData, Handle mutexp) = 0;
    virtual Handle AtomicDecrement(TaskData *taskData, Handle mutexp) = 0;
    // Reset a mutex to one.  This needs to be atomic with respect to the
    // atomic increment and decrement instructions.
    virtual void AtomicReset(TaskData *taskData, Handle mutexp) = 0;
};

extern MachineDependent *machineDependent;

#endif /* _MACHINE_DEP_H */
