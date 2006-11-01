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

/* created 27/10/93 SPF */

#ifndef _MACHINE_DEP_H
#define _MACHINE_DEP_H

#ifdef _WIN32_WCE
#include "winceconfig.h"
#else
#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
#endif
#endif

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
#elif defined(WIN32)
#include <windows.h>
#define SIGNALCONTEXT CONTEXT // This is the thread context.
#else
#define SIGNALCONTEXT void
#endif

class ScanAddress;
class StackObject;

class SaveVecEntry;
typedef SaveVecEntry *Handle;

// Machine architecture values.
typedef enum {
    MA_Interpreted = 0,
    MA_I386,
    MA_PPC,
    MA_Sparc,
    MA_X86_64
} Architectures;

class MachineDependent {
public:
    virtual ~MachineDependent() {} // Keep the compiler happy
    virtual void InitStackFrame(Handle stack, Handle proc, Handle arg) = 0;
    virtual unsigned InitialStackSize(void) { return 128; } // Initial size of a stack 
    // Must be > 40 (i.e. 2*min_stack_check) + base area in each stack frame
    // Switch to Poly and return with the io function to call.
    virtual int SwitchToPoly(void) = 0;
    virtual void SetForRetry(int ioCall) = 0;
    virtual void InitInterfaceVector(void) = 0;
    virtual void SetException(StackObject *stack,poly_exn *exc) = 0;
    virtual void ResetSignals(void) {}

    /* ScanConstantsWithinCode - update addresses within a code segment.*/
    virtual void ScanConstantsWithinCode(PolyObject *addr, PolyObject *oldAddr, POLYUNSIGNED length, ScanAddress *process) {}
    void  ScanConstantsWithinCode(PolyObject *addr, ScanAddress *process)
        { ScanConstantsWithinCode(addr, addr, addr->Length(), process); } // Common case

    virtual void InterruptCode(void) = 0;
    virtual void InterruptCodeUsingContext(SIGNALCONTEXT *context) { InterruptCode(); }
    virtual int  GetIOFunctionRegisterMask(int ioCall) = 0;
    virtual bool GetPCandSPFromContext(SIGNALCONTEXT *context, PolyWord * &sp,  POLYCODEPTR &pc) = 0;
    virtual void FlushInstructionCache(void *p, POLYUNSIGNED bytes) {}
    virtual bool InRunTimeSystem(void) = 0;
    virtual Architectures MachineArchitecture(void) = 0; 

    // General RTS functions.
    virtual void CallIO0(Handle(*ioFun)(void)) = 0;
    virtual void CallIO1(Handle(*ioFun)(Handle)) = 0;
    virtual void CallIO2(Handle(*ioFun)(Handle, Handle)) = 0;
    virtual void CallIO3(Handle(*ioFun)(Handle, Handle, Handle)) = 0;
    virtual void CallIO4(Handle(*ioFun)(Handle, Handle, Handle, Handle)) = 0;
    virtual void CallIO5(Handle(*ioFun)(Handle, Handle, Handle, Handle, Handle)) = 0;
    // These next two are sufficiently different that they need to be implemented
    // as special cases.
    virtual void SetExceptionTrace(void) = 0;
    virtual void CallCodeTupled(void) = 0;
    // This is used to get the argument to the callback_result function.
    virtual Handle CallBackResult(void) = 0;
    // This is machine-dependent but not always required.  It is used in the code-generator.
    virtual void SetCodeConstant(Handle data, Handle constant, Handle offseth, Handle base) {}

    // If a foreign function calls back to ML we need to set up the call to the
    // ML callback function.
    virtual void SetCallbackFunction(Handle func, Handle args) {}
    virtual unsigned char *BuildCallback(int cbEntryNo, Handle cResultType, int nArgsToRemove) { return 0; }
    virtual void GetCallbackArg(void **args, void *argLoc, int nSize) {}
};

extern MachineDependent *machineDependent;

#endif /* _MACHINE_DEP_H */
