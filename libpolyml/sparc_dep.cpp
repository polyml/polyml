/*
    Title:  Sparc dependent code.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright (c) 2000-7
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

#include "config.h"

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define  ASSERT(x)
#endif
#ifdef HAVE_SYS_ERRNO_H
#include <sys/errno.h>
#endif
#ifdef HAVE_UCONTEXT_H
#include <ucontext.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "globals.h"
#include "gc.h"
#include "run_time.h"
#include "mpoly.h"
#include "arb.h"
#include "diagnostics.h"
#include "processes.h"
#include "sys.h"
#include "profiling.h"
#include "sighandler.h"
#include "machine_dep.h"
#include "save_vec.h"
#include "scanaddrs.h"
#include "check_objects.h"
#include "memmgr.h"

// Values for the returnReason
enum RETURN_REASON {
    RETURN_IO_CALL = 0,
    RETURN_IO_EXCEPTION
};

/* Note: we've reserved a slot for %o6, but we're not allowed to change it
  since it's the C stack pointer, so we really only have 17 usable
  tagged registers. SPF 30/1/97.
*/

/* Location of these registers in taskData->stack->p_reg[] */ 
#define OFFSET_REGRESULT    2 /* regResult == argReg0 on SPARC */ 
#define OFFSET_ARGREG0      2
#define OFFSET_ARGREG1      3
#define OFFSET_ARGREG2      4
#define OFFSET_ARGREG3      5
#define OFFSET_REGCLOSURE   7
#define OFFSET_REGRETURN    9
/* slots 0 and 1 are used for %g1 and %g2 */


#define SAVERETURNOFFSET 2

/* The first instruction executed after native code returns
   is the saved return address + 6 */ 
#define RETURNOFFSET    6
#define HANDLEROFFSET   2

/* the amount of ML stack space to reserve for registers,
   C exception handling etc. The compiler requires us to
   reserve 2 stack-frames worth (2 * 20 words) plus whatever
   we require for the register save area. We actually reserve
   slightly more than this. SPF 3/3/97
*/
#define CHECKED_REGS    18
#define UNCHECKED_REGS  3
#define EXTRA_STACK     0

#define OVERFLOW_STACK_SIZE \
  (50 + \
   sizeof(StackObject)/sizeof(PolyWord) + \
   CHECKED_REGS + \
   UNCHECKED_REGS + \
   EXTRA_STACK)

// %i0 points to this structure while in the ML code.  The offsets
// of some items, in particular raiseException, are bound into the
// code and must not be changed.  Other offsets are bound into the
// assembly code.
typedef struct _MemRegisters {
    int inRTS;
        /* This is set when taskData->stack->p_pc and taskData->stack->p_sp are set */
    int requestCode;
    int returnReason;

    PolyWord    *heapPointer;
    POLYUNSIGNED heapSpaceT; // The heap space = -maxint+space
    StackObject *polyStack;
    PolyWord    *stackLimit;
    PolyWord    *stackTop; // Used in "raisex"
    byte        *raiseException; // Called to raise an exception
    byte        *ioEntry; // Called to save the ML state and return to C.
    PolyObject  *threadId; // Pointer to ML thread object.
} MemRegisters;

class SparcTaskData: public MDTaskData {
public:
    SparcTaskData(): allocWords(0)
    {
    memRegisters.inRTS = 1; // We start off in the RTS.
    }
    POLYUNSIGNED allocWords; // The words to allocate.
    MemRegisters memRegisters;
};

class SparcDependent: public MachineDependent {
public:
    SparcDependent() {}

    // Create a task data object.
    virtual MDTaskData *CreateTaskData(void) { return new SparcTaskData(); }

    virtual void InitStackFrame(TaskData *taskData, Handle stack, Handle proc, Handle arg);
    virtual unsigned InitialStackSize(void) { return 128+OVERFLOW_STACK_SIZE; } // Initial size of a stack 
    virtual int SwitchToPoly(TaskData *taskData);
    virtual void SetForRetry(TaskData *taskData, int ioCall);
    virtual void InitInterfaceVector(void);
    virtual void SetException(TaskData *taskData, poly_exn *exc);
    virtual void ResetSignals(void);
    virtual void ScanConstantsWithinCode(PolyObject *addr, PolyObject *oldAddr, POLYUNSIGNED length, ScanAddress *process);
    virtual void InterruptCode(TaskData *taskData);
    virtual int  GetIOFunctionRegisterMask(int ioCall);
    virtual bool GetPCandSPFromContext(TaskData *taskData, SIGNALCONTEXT *context, PolyWord *&sp, POLYCODEPTR &pc);
    virtual void CallIO0(TaskData *taskData, Handle(*ioFun)(TaskData *));
    virtual void CallIO1(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle));
    virtual void CallIO2(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle));
    virtual void CallIO3(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle));
    virtual void CallIO4(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle, Handle));
    virtual void CallIO5(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle, Handle, Handle));
    virtual Handle CallBackResult(TaskData *taskData);
    virtual void SetExceptionTrace(TaskData *taskData);
    virtual void CallCodeTupled(TaskData *taskData);
    virtual Architectures MachineArchitecture(void) { return MA_Sparc; }
    virtual void SetCodeConstant(TaskData *taskData, Handle data, Handle constant, Handle offseth, Handle base);
    virtual void FlushInstructionCache(void *p, POLYUNSIGNED bytes);
    // Increment or decrement the first word of the object pointed to by the
    // mutex argument and return the new value.
    virtual Handle AtomicIncrement(TaskData *taskData, Handle mutexp);
    virtual Handle AtomicDecrement(TaskData *taskData, Handle mutexp);
    // Set a mutex to one.  On the Sparc this requires a memory barrier.
    virtual void SetToReleased(TaskData *taskData, Handle mutexp);

private:
    bool TrapHandle(TaskData *taskData);
    void StartIOCall(TaskData *taskData);
    void SetMemRegisters(TaskData *taskData);
    Handle BuildCodeSegment(TaskData *taskData, const unsigned *code, unsigned codeWords, char functionName);
    Handle BuildKillSelfCode(TaskData *taskData);

};

#define VERSION_NUMBER POLY_version_number

extern "C" {
extern int finisha();
extern int install_roota();
extern int strconcata();
extern int change_dira();
extern int str_comparea();
extern int teststreq();
extern int teststrneq();
extern int teststrgtr();
extern int teststrlss();
extern int teststrgeq();
extern int teststrleq();
extern int locksega();
extern int profilera();
extern int add_long();
extern int sub_long(); 
extern int mult_long();
extern int div_longa();
extern int rem_longa();
extern int quotrem_longa();
extern int neg_long();
extern int or_long();
extern int and_long();
extern int xor_long();
extern int equal_long();
extern int Real_stra();
extern int Real_geqa();
extern int Real_leqa();
extern int Real_gtra();
extern int Real_lssa();
extern int Real_eqa();
extern int Real_neqa();
extern int Real_dispatcha();
extern int Real_adda();
extern int Real_suba();
extern int Real_mula();
extern int Real_diva();
extern int Real_nega();
extern int Real_repra();
extern int Real_conva();
extern int Real_inta();
extern int Real_floata();
extern int Real_sqrta();
extern int Real_sina();
extern int Real_cosa();
extern int Real_arctana();
extern int Real_expa();
extern int Real_lna();
extern int io_operationa();
extern int shift_right_word();
extern int word_neq();
extern int not_bool();
extern int string_length();
extern int int_eq();
extern int int_neq();
extern int int_geq();
extern int int_leq();
extern int int_gtr();
extern int int_lss();
extern int string_suba();
extern int or_word();
extern int and_word();
extern int xor_word();
extern int shift_left_word();
extern int word_eq();
extern int load_byte();
extern int load_word();
extern int assign_byte();
extern int assign_word();
extern int atomic_incr();
extern int atomic_decr();
extern int thread_self();
extern int thread_dispatcha();
extern int kill_selfa();
extern int alloc_store();
extern int alloc_uninit_a();
extern int get_length_a();
extern int get_flags_a();
extern int set_flags_a();
extern int int_to_word();
extern int set_code_constanta();
extern int move_bytes();
extern int move_words();
extern int shift_right_arith_word();
extern int raisex();
extern int exception_tracea();
extern int offset_address();
extern int is_shorta();
extern int is_big_endian();
extern int bytes_per_word();
extern int mul_word();
extern int plus_word();
extern int minus_word();
extern int div_worda();
extern int mod_worda();
extern int word_geq();
extern int word_leq();
extern int word_gtr();
extern int word_lss();

extern int objsize_a();
extern int showsize_a();
extern int timing_dispatch_a();

extern int XWindows_a();                    /* MJC 27/09/90 */

extern int full_gc_a();                     /* MJC 18/03/91 */
extern int stack_trace_a();                 /* MJC 18/03/91 */

extern int foreign_dispatch_a();            /* NIC 22/04/94 */
extern int callcode_tupleda();              /* SPF 07/07/94 - for ML version of compiler */
extern int process_env_dispatch_a();        /* DCJM 25/4/00 */
extern int set_string_length_a();           /* DCJM 28/2/01 */
extern int get_first_long_word_a();         /* DCJM 28/2/01 */
extern int shrink_stack_a();                /* SPF  1/12/96 */

extern int IO_dispatch_a();                 /* DCJM 8/5/00 */
extern int Net_dispatch_a();                /* DCJM 22/5/00 */
extern int OS_spec_dispatch_a();            /* DCJM 22/5/00 */
extern int Sig_dispatch_a();                /* DCJM 18/7/00 */
extern int poly_dispatch_a();
extern int bytevec_eq_a();

extern void SparcAsmSwitchToPoly(MemRegisters *);
extern int SparcAsmSaveStateAndReturn(void);
extern void SparcAsmFlushInstructionCache(void *p, POLYUNSIGNED bytes);

};

void SparcDependent::InitStackFrame(TaskData *taskData, Handle stackh, Handle proc, Handle arg)
/* Initialise stack frame. */
{
   /* This code is pretty tricky. 
        (1) We pretend that the function we want to call is actually an RTS entry
            that has been interrupted, so that it gets called via the RTS call
            retry mechanism.
            
        (2) We have to ensure that if the function returns or is interrupted,
            it gets into the kill_self code.
            
        (3) This is the last exception handler on the stack, so we make it
            point at itself.     
    */
    unsigned i;
    StackObject *stack = (StackObject *)DEREFWORDHANDLE(stackh);

    POLYUNSIGNED stack_size = stack->Length();
    stack->p_space = OVERFLOW_STACK_SIZE;
    stack->p_pc    = PC_RETRY_SPECIAL; /* As if we had called MD_set_for_retry. */
    stack->p_nreg  = CHECKED_REGS;
    stack->p_sp = (PolyWord*)stack + stack_size - 3;
    stack->p_hr = stack->p_sp;
    
    /* Reset all registers since this may be an old stack frame */
    for (i = 0; i < CHECKED_REGS; i++)
        stack->p_reg[i] = TAGGED(0);

    // Set the default handler and return address to point to this code.
    // We have to offset the addresses by two bytes because the code to enter and a handler
    // or return from a function always subtracts two.
    Handle killCode = BuildKillSelfCode(taskData);
    PolyWord killJump = PolyWord::FromUnsigned(killCode->Word().AsUnsigned() | HANDLEROFFSET);
    stack = (StackObject *)DEREFWORDHANDLE(stackh); // In case it's moved

    stack->p_reg[OFFSET_REGCLOSURE] = DEREFWORD(proc); /* Set regClosureto the closure address. */
    stack->p_reg[CHECKED_REGS]      = PolyWord::FromUnsigned(UNCHECKED_REGS);
    // If this function takes an argument store it in the argument register.  If it doesn't
    // we've set argReg0 to TAGGED(0) already.
    if (arg != 0) stack->p_reg[OFFSET_ARGREG0] = DEREFWORD(arg);
    
    /* Since they're unchecked, they don't need to be initialised,
       but set them to 0 anyway. */
    for (i = 0; i < UNCHECKED_REGS; i++)
        stack->p_reg[CHECKED_REGS+1+i] = PolyWord::FromUnsigned(0);
    
    /* Set up exception handler group - there's no previous handler so point
       "previous handler" pointer at itself.*/
    stack->Set(stack_size-1, PolyWord::FromStackAddr(stack->Offset(stack_size-1)));
    stack->Set(stack_size-2, killJump);
    stack->Set(stack_size-3, TAGGED(0)); /* Default handler. */
    
    // Return address into %o7 plus 2 bytes.  A return will always add 6 to the value.
    stack->p_reg[OFFSET_REGRETURN] = killJump;
}

bool SparcDependent::GetPCandSPFromContext(TaskData *taskData, SIGNALCONTEXT *context, PolyWord *&sp, POLYCODEPTR &pc)
{
    SparcTaskData *mdTask = (SparcTaskData*)taskData->mdTaskData;
    if (mdTask->memRegisters.inRTS)
    {
        sp = taskData->stack->p_sp;
        pc = taskData->stack->p_pc;
        return true;
    }
    else /* in poly code or assembly code */
    {
        /* NB Poly/ML uses register g4 as the stack pointer, C appears to use o6.  */
        sp = (PolyWord *)context->uc_mcontext.gregs[REG_G4];
        pc = (byte *)context->uc_mcontext.gregs[REG_PC];
        return true;
    }
}

void SparcDependent::InterruptCode(TaskData *taskData)
{
    SparcTaskData *mdTask = (SparcTaskData*)taskData->mdTaskData;
    if (taskData->stack != 0) 
        mdTask->memRegisters.stackLimit = taskData->stack->Offset(taskData->stack->Length()-1); 
}

// These are just for debugging.  They record the last point before
// the memory was checked.
byte *lastPC;
int lastRequest, lastReason;
PolyWord *lastBase;

int SparcDependent::SwitchToPoly(TaskData *taskData)
/* (Re)-enter the Poly code from C. */
{
    SparcTaskData *mdTask = (SparcTaskData*)taskData->mdTaskData;
    Handle mark = taskData->saveVec.mark();
    while (1)
    {
        taskData->saveVec.reset(mark); // Remove old data e.g. from arbitrary precision.
        CheckMemory(); // Do any memory checking.

        // Remember the position after the last time we checked
        // the memory.
        lastPC = taskData->stack->p_pc;
        lastRequest = mdTask->memRegisters.requestCode;
        lastReason = mdTask->memRegisters.returnReason;
        lastBase = taskData->allocPointer;
        
        SetMemRegisters(taskData);

        SparcAsmSwitchToPoly(&mdTask->memRegisters); // Load registers.  Returns as a result of an RTS call.
        taskData->allocPointer = mdTask->memRegisters.heapPointer - 1; // Get updated limit pointer.
        mdTask->allocWords = 0; // Always zero except if this is a memory trap.

        switch (mdTask->memRegisters.returnReason)
        {
        case RETURN_IO_CALL:
            // The ML code wants an IO call.
            return mdTask->memRegisters.requestCode;
        case RETURN_IO_EXCEPTION:
            try {
                // We have had a trap of some sort
                if (TrapHandle(taskData))
                    return -1; // Safe to process interrupts.
            }
            catch (IOException) {
               // We may get an exception while handling this if we run out of store
            }
        }
    }
}

void SparcDependent::SetMemRegisters(TaskData *taskData)
{
    SparcTaskData *mdTask = (SparcTaskData*)taskData->mdTaskData;
    mdTask->memRegisters.requestCode = 0;
    mdTask->memRegisters.returnReason = RETURN_IO_CALL;

    // If we haven't yet set the allocation area or we don't have enough we need
    // to create one (or a new one).
    if (taskData->allocPointer <= taskData->allocLimit + mdTask->allocWords ||
        (userOptions.debug & DEBUG_FORCEGC))
    {
        if (taskData->allocPointer < taskData->allocLimit)
            Crash ("Bad length in heap overflow trap");

        // Find some space to allocate in.  Updates taskData->allocPointer and
        // returns a pointer to the newly allocated space (if allocWords != 0)
        (void)processes->FindAllocationSpace(taskData, mdTask->allocWords, true);
    }
    else if (mdTask->allocWords != 0) // May just be store profiling.
        taskData->allocPointer -= mdTask->allocWords;
    
    mdTask->memRegisters.polyStack = taskData->stack;
    mdTask->memRegisters.stackTop = taskData->stack->Offset(taskData->stack->Length());

    // Set the raiseException entry to point to the assembly code.
    mdTask->memRegisters.raiseException = (byte*)raisex;
    // Entry point to save the state for an IO call.  This is the common entry
    // point for all the return and IO-call cases.
    mdTask->memRegisters.ioEntry = (byte*)SparcAsmSaveStateAndReturn;

    // If we have run out of store, either just above or while allocating in the RTS,
    // allocPointer and allocLimit will have been set to zero as part of the GC.  We will
    // now be raising an exception which may free some store but we need to come back here
    // before we allocate anything.  To avoid a problem when testing allocPointer and
    // allocLimit below we need to make sure that the values are still non-negative
    // after substracting any object size.
    if (taskData->allocPointer == 0) taskData->allocPointer += MAX_OBJECT_SIZE;
    if (taskData->allocLimit == 0) taskData->allocLimit += MAX_OBJECT_SIZE;

    // Set up heap pointers.
    mdTask->memRegisters.heapPointer = taskData->allocPointer+1; // This points beyond the length word
    // The Sparc version sets %g5 to be -maxint + space.  Each time an object is allocated
    // the size is deducted from this until eventually the space is exhausted.  At that
    // point the subtraction results in an overflow which traps.
    if (profileMode == kProfileStoreAllocation || (userOptions.debug & DEBUG_REGION_CHECK))
        mdTask->memRegisters.heapSpaceT = 0x80000000;
    else
        mdTask->memRegisters.heapSpaceT = ((char*)taskData->allocPointer-(char*)taskData->allocLimit) | 0x80000000;
    
    mdTask->memRegisters.stackLimit = taskData->stack->Offset(taskData->stack->p_space);
    mdTask->memRegisters.threadId = taskData->threadObject;
    
    if (taskData->stack->p_pc == PC_RETRY_SPECIAL)
        // We need to retry the call.  The entry point should be the
        // first word of the closure which is in %o4.
        taskData->stack->p_pc = taskData->stack->p_reg[OFFSET_REGCLOSURE].AsObjPtr()->Get(0).AsCodePtr();
}

// Called as part of the call of an IO function.
void SparcDependent::StartIOCall(TaskData *taskData)
{
    // Set the return address to be the contents of the return register
    // after skipping the CALL instruction and delay slot.
    // This is a real return address which is safe for the p_pc field but
    // not allowed to remain in a register field.  We have to OR in the
    // return offset there.  Because this may be a retry we may already
    // have been here before so we use OR rather than ADD.
    POLYUNSIGNED returnAddr = taskData->stack->p_reg[OFFSET_REGRETURN].AsUnsigned();
    returnAddr |= SAVERETURNOFFSET; // Make it a valid code address.
    taskData->stack->p_reg[OFFSET_REGRETURN] = PolyWord::FromUnsigned(returnAddr);
    taskData->stack->p_pc = (byte*)(returnAddr + RETURNOFFSET);
}

// IO Functions called indirectly from assembly code.
void SparcDependent::CallIO0(TaskData *taskData, Handle (*ioFun)(TaskData *))
{
    StartIOCall(taskData);
    try {
        Handle result = (*ioFun)(taskData);
        taskData->stack->p_reg[OFFSET_REGRESULT] = result->Word();
    }
    catch (IOException exc) {
        switch (exc.m_reason)
        {
        case EXC_EXCEPTION:
            return;
        case EXC_RETRY:
            return;
        }
    }
}

void SparcDependent::CallIO1(TaskData *taskData, Handle (*ioFun)(TaskData *, Handle))
{
    StartIOCall(taskData);
    Handle saved1 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG0]);
    try {
        Handle result = (*ioFun)(taskData, saved1);
        taskData->stack->p_reg[OFFSET_REGRESULT] = result->Word();
    }
    catch (IOException exc) {
        switch (exc.m_reason)
        {
        case EXC_EXCEPTION:
            return;
        case EXC_RETRY:
            return;
        }
    }
}

void SparcDependent::CallIO2(TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle))
{
    StartIOCall(taskData);
    Handle saved1 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG0]);
    Handle saved2 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG1]);
    try {
        Handle result = (*ioFun)(taskData, saved2, saved1);
        taskData->stack->p_reg[OFFSET_REGRESULT] = result->Word();
    }
    catch (IOException exc) {
        switch (exc.m_reason)
        {
        case EXC_EXCEPTION:
            return;
        case EXC_RETRY:
            return;
        }
    }
}

void SparcDependent::CallIO3(TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle, Handle))
{
    StartIOCall(taskData);
    Handle saved1 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG0]);
    Handle saved2 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG1]);
    Handle saved3 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG2]);
    try {
        Handle result = (*ioFun)(taskData, saved3, saved2, saved1);
        taskData->stack->p_reg[OFFSET_REGRESULT] = result->Word();
    }
    catch (IOException exc) {
        switch (exc.m_reason)
        {
        case EXC_EXCEPTION:
            return;
        case EXC_RETRY:
            return;
        }
    }
}

// The only function with four arguments is SetCodeConstant.
void SparcDependent::CallIO4(TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle, Handle, Handle))
{
    StartIOCall(taskData);
    Handle saved1 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG0]);
    Handle saved2 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG1]);
    Handle saved3 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG2]);
    Handle saved4 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG3]);
    try {
        Handle result = (*ioFun)(taskData, saved4, saved3, saved2, saved1);
        taskData->stack->p_reg[OFFSET_REGRESULT] = result->Word();
    }
    catch (IOException exc) {
        switch (exc.m_reason)
        {
        case EXC_EXCEPTION:
            return;
        case EXC_RETRY:
            return;
        }
    }
}

// The only functions with 5 args are move_bytes/word_long
void SparcDependent::CallIO5(TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle, Handle, Handle, Handle))
{
    StartIOCall(taskData);
    Handle saved1 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG0]);
    Handle saved2 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG1]);
    Handle saved3 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG2]);
    Handle saved4 = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG3]);
    Handle saved5 = taskData->saveVec.push(taskData->stack->p_sp[0]);
    try {
        Handle result = (*ioFun)(taskData, saved5, saved4, saved3, saved2, saved1);
        taskData->stack->p_reg[OFFSET_REGRESULT] = result->Word();
        taskData->stack->p_sp++; // Pop the final argument now we're returning.
    }
    catch (IOException exc) {
        switch (exc.m_reason)
        {
        case EXC_EXCEPTION:
            return;
        case EXC_RETRY:
            return;
        }
    }
}

// Return the callback result.  The current ML process (thread) terminates.
Handle SparcDependent::CallBackResult(TaskData *taskData)
{
    return taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG0]); // Argument to return is in %o0.
}

void SparcDependent::SetForRetry(TaskData *taskData, int ioCall)
{
    taskData->stack->p_pc = PC_RETRY_SPECIAL; /* This value is treated specially. */
}

// Call a piece of compiled code.  Note: this doesn't come via CallIO1
// so StartIOCall has not been called.
void SparcDependent::CallCodeTupled(TaskData *taskData)
{
    // The eventual return address is in %o7 - leave it there
    // but call StartIOCall to make sure it's tagged before any possible G.C.
    StartIOCall(taskData);
    PolyObject *argTuple = taskData->stack->p_reg[OFFSET_ARGREG0].AsObjPtr();
    Handle closure = taskData->saveVec.push(argTuple->Get(0));
    Handle argvec = taskData->saveVec.push(argTuple->Get(1));

    if (! IS_INT(DEREFWORD(argvec))) // May be nil if there are no args.
    {
        PolyObject *argv = DEREFHANDLE(argvec);
        POLYUNSIGNED argCount = argv->Length();
        if (argCount > 4)
        {
            // Check we have space for the arguments.  This may result in a GC which
            // in turn may throw a C++ exception.
            try {
                CheckAndGrowStack(taskData, taskData->stack->p_sp - (argCount - 4));
            }
            catch (IOException exc)
            {
                ASSERT(exc.m_reason == EXC_EXCEPTION);  // This should be the only one
                return; // Will have been set up to raise an exception.
            }
        }
        // First argument is in %o0
        taskData->stack->p_reg[OFFSET_ARGREG0] = argv->Get(0);
        // Second arg, if there is one, goes into %o1 etc.
        if (argCount > 1)
            taskData->stack->p_reg[OFFSET_ARGREG1] = argv->Get(1);
        if (argCount > 2)
            taskData->stack->p_reg[OFFSET_ARGREG2] = argv->Get(2);
        if (argCount > 3)
            taskData->stack->p_reg[OFFSET_ARGREG3] = argv->Get(3);
        // Remaining args go on the stack.
        for (POLYUNSIGNED i = 4; i < argCount; i++)
            *(--taskData->stack->p_sp) = argv->Get(i);
    }
    // The closure goes into the closure reg.
    taskData->stack->p_reg[OFFSET_REGCLOSURE] = DEREFWORD(closure);
    // First word of closure is entry point.
    taskData->stack->p_pc = DEREFHANDLE(closure)->Get(0).AsCodePtr(); // pc points to the start of the code
}

// This code is executed if the function returns without raising an exception.  Because
// the normal function return sequence jumps to %o7+6 we have to have two nops at the start.
static unsigned setExceptionCode[] =
{
    0x01000000, // nop
    0x01000000, // nop
    0xde01200c, // ld  [%g4+12],%o7  ! Get original return addr
    0xc6012008, // ld  [%g4+8],%g3   ! Restore previous handler
    0x81c3e006, // jmp %o7+6
    0x88012010  // add %g4,16,%g4    ! Pop these from stack DELAY SLOT
};

// Set up a special handler that will trace any uncaught exception within a function.
void SparcDependent::SetExceptionTrace(TaskData *taskData)
{
    // Save the return address for when we've called the function.  This will
    // be popped by the special "return" code we'll set up.
    *(--taskData->stack->p_sp) =
        PolyWord::FromUnsigned(taskData->stack->p_reg[OFFSET_REGRETURN].AsUnsigned() | SAVERETURNOFFSET);
    *(--taskData->stack->p_sp) = PolyWord::FromStackAddr(taskData->stack->p_hr); // Save previous handler.
    *(--taskData->stack->p_sp) = TAGGED(0); // Push special handler address.
    *(--taskData->stack->p_sp) = TAGGED(0); // Push "catch all" exception id.
    taskData->stack->p_hr = taskData->stack->p_sp; // This is the new handler.
    Handle fun = taskData->saveVec.push(taskData->stack->p_reg[OFFSET_ARGREG0]); // Argument - function to call and trace
    taskData->stack->p_reg[OFFSET_REGCLOSURE] = DEREFWORD(fun); // Closure register must contain the closure
    taskData->stack->p_pc = DEREFHANDLE(fun)->Get(0).AsCodePtr(); // pc points to the start of the code

    Handle retCode = BuildCodeSegment(taskData, setExceptionCode, sizeof(setExceptionCode)/sizeof(unsigned), 'R');

    // Set %o7 so that if the traced function returns normally (i.e. without raising an
    // exception) it will enter the "return" code which will remove this handler.
    taskData->stack->p_reg[OFFSET_REGRETURN] = PolyWord::FromUnsigned(retCode->Word().AsUnsigned() | SAVERETURNOFFSET);
    taskData->stack->p_reg[OFFSET_ARGREG0] = TAGGED(0); // Give the function we're calling a unit argument.
}


// In Solaris the trap instructions result in SIGSEGVs.
static void catchSEGV(SIG_HANDLER_ARGS(s, context))
{
    assert(s == SIGSEGV);
    assert(context != NULL);
    SIGNALCONTEXT *cntxt = (SIGNALCONTEXT *)context;
    TaskData *taskData = processes->GetTaskDataForThread();
    if (taskData == 0)
    {
        signal(SIGSEGV,SIG_DFL);
        return;
    }
    SparcTaskData *mdTask = (SparcTaskData*)taskData->mdTaskData;
    
    if (mdTask->memRegisters.inRTS)
    {
        signal(SIGSEGV,SIG_DFL);
        return;
    }
    
    /* NOW, we are in the run-time system */
    mdTask->memRegisters.inRTS = 1;
    mdTask->memRegisters.returnReason = RETURN_IO_EXCEPTION;
    
    /* This piece of code is extremely messy. It has to get the state when the
       interrupt occured by unwinding the stack. It can then save the registers
       and call ``translate''. */
    taskData->stack->p_pc = (byte*)cntxt->uc_mcontext.gregs[REG_PC]; /* Save trapping pc. */
    cntxt->uc_mcontext.gregs[REG_PC]  = (int)&SparcAsmSaveStateAndReturn; /* Restart in trap_handler. */
    cntxt->uc_mcontext.gregs[REG_nPC] = cntxt->uc_mcontext.gregs[REG_PC] + 4;
}


/******************************************************************************/
/*                                                                            */
/*      catchILL - utility function                                           */
/*                                                                            */
/******************************************************************************/
static void catchILL(SIG_HANDLER_ARGS(s, context))
{
    assert(s == SIGILL);
    assert(context != NULL);
#ifdef SOLARIS
    SIGNALCONTEXT *cntxt = (SIGNALCONTEXT *)context;
#else // Linux
    sigcontext *cntxt = (sigcontext*)context;
#endif

    TaskData *taskData = processes->GetTaskDataForThread();
    if (taskData == 0)
    {
        signal(SIGILL,SIG_DFL);
        return;
    }
    SparcTaskData *mdTask = (SparcTaskData*)taskData->mdTaskData;
    
    /* Shouldn't get stack overflow in run-time system */
    if (mdTask->memRegisters.inRTS)
    {
        { /* use standard SYSV calls */
            sigset_t mask;
            assert(sigemptyset(&mask) == 0);
            assert(sigaddset(&mask,SIGILL) == 0);
            assert(sigprocmask(SIG_UNBLOCK,&mask,NULL) == 0);
        }
        
        if (mainThreadPhase != MTP_USER_CODE)
            printf("\nStack overflow in the garbage collector.\n");
        else
            printf("\nStack overflow in the runtime system.\n");
        
        printf("You may need to increase your stack limit and try again.\n");
        fflush(stdout);
        exit(1);
        /*NOTREACHED*/
    }
    
    /* NOW, we are in the run-time system */
    mdTask->memRegisters.inRTS = 1;
    mdTask->memRegisters.returnReason = RETURN_IO_EXCEPTION;
    
    /* This piece of code is extremely messy. It has to get the state when the
       interrupt occured by unwinding the stack. It can then save the registers
       and call ``translate''. */
#ifdef SOLARIS
    taskData->stack->p_pc = (byte*)cntxt->uc_mcontext.gregs[REG_PC]; /* Save trapping pc. */
    cntxt->uc_mcontext.gregs[REG_PC]  = (int)&SparcAsmSaveStateAndReturn; /* Restart in trap_handler. */
    cntxt->uc_mcontext.gregs[REG_nPC] = cntxt->uc_mcontext.gregs[REG_PC] + 4;
#else
    taskData->stack->p_pc = (byte*)cntxt->si_regs.pc;
    cntxt->si_regs.pc = (int)&SparcAsmSaveStateAndReturn; /* Restart in trap_handler. */
    cntxt->si_regs.npc = cntxt->si_regs.pc + 4;
#endif
    
    /* "returns" to MD_trap_handler */
}

/******************************************************************************/
/*                                                                            */
/*      catchEMT - utility function                                           */
/*                                                                            */
/******************************************************************************/
static void catchEMT(SIG_HANDLER_ARGS(s, context))
{
    assert(s == SIGEMT);
    assert(context != NULL);
#ifdef SOLARIS
    SIGNALCONTEXT *cntxt = (SIGNALCONTEXT *)context;
#else // Linux
    sigcontext *cntxt = (sigcontext*)context;
#endif

    TaskData *taskData = processes->GetTaskDataForThread();
    if (taskData == 0)
    {
        signal(SIGEMT,SIG_DFL);
        return;
    }
    SparcTaskData *mdTask = (SparcTaskData*)taskData->mdTaskData;
    
    /* shouldn't get SIGEMT from run-time system,         */
    /* so reinstall default handler and return for retry, */
    /* which should lead to core dump.                    */
    if (mdTask->memRegisters.inRTS)
    {
        signal(SIGEMT,SIG_DFL);
        return;
    }
    
    /* NOW, we are in the run-time system */
    mdTask->memRegisters.inRTS = 1;
    mdTask->memRegisters.returnReason = RETURN_IO_EXCEPTION;
    
    /* This piece of code is extremely messy. It has to get the state when the
       interrupt occured by unwinding the stack. It can then save the registers
       and call ``translate''. */
#ifdef SOLARIS
    taskData->stack->p_pc = (byte*)cntxt->uc_mcontext.gregs[REG_PC]; /* Save trapping pc. */
    cntxt->uc_mcontext.gregs[REG_PC]  = (int)&SparcAsmSaveStateAndReturn; /* Restart in trap_handler. */
    cntxt->uc_mcontext.gregs[REG_nPC] = cntxt->uc_mcontext.gregs[REG_PC] + 4;
#else
    taskData->stack->p_pc = (byte*)cntxt->si_regs.pc;
    cntxt->si_regs.pc = (int)&SparcAsmSaveStateAndReturn; /* Restart in trap_handler. */
    cntxt->si_regs.npc = cntxt->si_regs.pc + 4;
#endif
 
    /* "returns" to MD_trap_handler */
}

/* end of Solaris2 signal handling */

static PolyWord zero = PolyWord::FromUnsigned(0);

/******************************************************************************/
/*                                                                            */
/*      get_reg - utility function                                            */
/*                                                                            */
/******************************************************************************/
static PolyWord *get_reg(TaskData *taskData, int rno)
/* Returns a pointer to the register given by the 5 bit value rno. */
{
    if (8 <= rno && rno <= 23) /* %o0 - %l7 */
        return &taskData->stack->p_reg[rno-6];
    
    else switch (rno) 
    {
    case 0:  /* %g0 */ return &zero;
    case 1:  /* %g1 */ return &taskData->stack->p_reg[0];
    case 2:  /* %g2 */ return &taskData->stack->p_reg[1];
         /* These last two are used as unchecked work registers. */
    case 28: /* %i4 */ return &taskData->stack->p_reg[CHECKED_REGS+1];
    case 29: /* %i5 */ return &taskData->stack->p_reg[CHECKED_REGS+2];
    case 3:  /* %g3 (hr) */ return (PolyWord*)&taskData->stack->p_hr;
    case 4:  /* %g4 (sp) */ return (PolyWord*)&taskData->stack->p_sp;
    default: Crash("Unknown register %d at %x\n", rno, (int)(taskData->stack->p_pc));
         /*NOTREACHED*/
    }
}

#define ALIGNED(p) ((p.AsUnsigned() & (sizeof(PolyWord)-1)) == 0)

// In many case we will have subtracted one from the argument and need to add it back.
// If this was a short integer that will retag it but if it was actually long
// it will turn it back into an address.
inline PolyWord AddOne(PolyWord p)
{
    return PolyWord::FromUnsigned(p.AsUnsigned() + 1);
}

// The reverse operation to put the result back.
inline PolyWord SubOne(PolyWord p)
{
    return PolyWord::FromUnsigned(p.AsUnsigned() - 1);
}

static void emulate_trap(TaskData *taskData, POLYUNSIGNED instr)
/* Emulate a taddcctv or tsubcctv instruction. */
/* One or both of the arguments may be i4 or i5.  These registers are saved
   by the trap handler but are not preserved by the garbage collector.  */
{
    unsigned rd = (instr >> 25) & 31; /* Destination register. */

    Handle arg1 = taskData->saveVec.push(AddOne(*(get_reg(taskData, (instr >> 14) & 31))));
    Handle arg2;
    
    if (instr & 0x2000)
    {
        /* Immediate - the argument is the value in the instruction with
            the bottom tag bit set. */
        if (instr & 0x1000)
            arg2 = taskData->saveVec.push(PolyWord::FromSigned(-((POLYSIGNED)instr & 0xfff) + 1));
        else
            arg2 = taskData->saveVec.push(PolyWord::FromUnsigned((instr & 0xfff) + 1));
    }
    else 
        arg2 = taskData->saveVec.push(AddOne(*(get_reg(taskData, instr & 31))));
    
    if (rd == 0) /* g0 */
    {
        /* If we are putting the result into g0 it must be because we are
           interested in the condition code.  We do a comparison rather than
           a subtraction. */
        
        int r = compareLong(taskData, arg2, arg1);
        // Put the result of the comparison in the condition code field.  We compare
        // this with zero in MD_switch_to_poly to actually set the codes.
        taskData->stack->p_reg[CHECKED_REGS+3] = PolyWord::FromUnsigned(r);
    }
    else if ((instr & 0x2000) == 0)
    {
        /* We have to be careful here.  Multiplication is done by repeated addition
           using values in i4 and i5.  They are assumed to have had 1 subtracted from
           them, but if we have a garbage collection while doing the long-precision
           operation they may be left pointing in the wrong place.  We save the
           values +1 on the save vec so they will be updated if they are addresses,
           and put them back just before putting in the result. */
        Handle i4v = taskData->saveVec.push(AddOne(taskData->stack->p_reg[CHECKED_REGS+1]));
        Handle i5v = taskData->saveVec.push(AddOne(taskData->stack->p_reg[CHECKED_REGS+2]));
        
        Handle res;
        if (instr & 0x80000) 
            res = sub_longc(taskData, arg2, arg1);
        else
            res = add_longc(taskData, arg2, arg1);
        
        /* Put back the values into i4 and i5, and then put in the result (into 
           either i4 or i5). */
        taskData->stack->p_reg[CHECKED_REGS+1] = SubOne(i4v->Word());
        taskData->stack->p_reg[CHECKED_REGS+2] = SubOne(i5v->Word());
        *(get_reg(taskData, rd)) = SubOne(res->Word());
    }
    else { /* Adding or subtracting a constant - can't be doing a multiplication
              and we must not save i4 or i5 because only one of them is being
              used in this operation and the other may contain anything. */
        Handle res;
        
        if (instr & 0x80000)
            res = sub_longc(taskData, arg2, arg1);
        else
            res = add_longc(taskData, arg2, arg1);
        /* Subtract 1 from the result (we will add it back in the next instr),
           and put it in the destination register. */
        *(get_reg(taskData, rd)) = SubOne(res->Word());
    }
}


bool SparcDependent::TrapHandle(TaskData *taskData)
/* Called from MD_trap_handler after registers have been saved in taskData->stack. */
{
    SparcTaskData *mdTask = (SparcTaskData*)taskData->mdTaskData;
    mdTask->memRegisters.inRTS = 1;
    
    POLYUNSIGNED instr = *(POLYUNSIGNED*)taskData->stack->p_pc; /* instruction that trapped. */
    
    /* Trap instructions can be as a result of stack or heap overflow,
       or may be caused by arithmetic overflows when using tagged numbers.
       Various different traps are in use.
       tlu 24 (previously tlu 16) occurs as a result of stack overflow.
       tsubcctv %g5,?,%g5 occurs as a result of storage allocation.
       taddcctv and tsubcctv occur as a result of arithmetic overflow. */
    
    /* Skip over the trap instruction. */
    taskData->stack->p_pc += 4;
    
    if (instr == 0x8bd02010 || instr == 0x8bd02018)  /* tlu 24 is stack overflow */
    {
        /* Check the size of the stack and expand if necessary. */
        /* We need to examine the previous instruction */
        /* in order to work out the required amount of space. */
        
        instr = *(POLYUNSIGNED*)(taskData->stack->p_pc-8);  /* get previous instruction */
        
        if (instr == 0x80a1001b) /* cmp %g4, %i3 is normal stack check */
            CheckAndGrowStack(taskData, taskData->stack->p_sp); /* may allocate */
        else if (instr == 0x80a7401b) /* cmp %i5, %i3 is large stack check */
            CheckAndGrowStack(taskData, taskData->stack->p_reg[CHECKED_REGS+2].AsStackAddr());
        else Crash ("Bad stack check sequence"); /* may allocate */
        
        // Now handle any interrupts.
        return true;
    }
    else if ((instr & 0xfffff000) == 0x8b196000 /* tsubcctv %g5,len,%g5 */ ||
        (instr & 0xffffe01f) == 0x8b19401d /* tsubcctv %g5,%i5,%g5 */)
    { 
        POLYUNSIGNED len;
        if (instr & 0x00002000) // Immediate data
            len = instr & 0xfff;
        else // In i5.
            len = taskData->stack->p_reg[CHECKED_REGS+2].AsUnsigned();

        len = len / sizeof(PolyWord);
        
        /* printf ("Trap:0x%08x = tsubcctv %%g5,%d,%%g5\n",instr,len); */
        
        if (profileMode == kProfileStoreAllocation)
            add_count(taskData, taskData->stack->p_pc, taskData->stack->p_sp, len);
        else
            if (taskData->allocPointer >= taskData->allocLimit && ! (userOptions.debug & DEBUG_REGION_CHECK))
                Crash ("Spurious %%g5 trap");
            
        if (taskData->allocPointer < taskData->allocLimit)
        {
            taskData->allocPointer += len;
            
            if (taskData->allocPointer < taskData->allocLimit)
                Crash ("Bad length in %%g5 trap");

            mdTask->allocWords = len; // Actually allocate it in SetMemRegisters.
        }
    }
    else if ((instr & 0xc1f00000) == 0x81100000 /* tsubcctv or taddcctv */)
    {
        if (profileMode == kProfileEmulation)
            add_count(taskData, taskData->stack->p_pc, taskData->stack->p_sp, 1);
        
        emulate_trap(taskData, instr);
    }
 
    else Crash("Bad trap pc=%p, instr=%08x",taskData->stack->p_pc-4,instr);
    return false;
}

static void add_function_to_io_area(int x, int (*y)())
{
    add_word_to_io_area(x, PolyWord::FromUnsigned((POLYUNSIGNED)y));
}

void SparcDependent::InitInterfaceVector(void)
{
    add_function_to_io_area(POLY_SYS_exit, &finisha);
    add_function_to_io_area(POLY_SYS_alloc_store, &alloc_store);
    add_function_to_io_area(POLY_SYS_alloc_uninit, &alloc_uninit_a);
    add_function_to_io_area(POLY_SYS_chdir, &change_dira);
    add_function_to_io_area(POLY_SYS_get_length, &get_length_a);
    add_function_to_io_area(POLY_SYS_get_flags, &get_flags_a);
    add_function_to_io_area(POLY_SYS_str_compare, &str_comparea);
    add_function_to_io_area(POLY_SYS_teststreq, &teststreq);
    add_function_to_io_area(POLY_SYS_teststrneq, &teststrneq);
    add_function_to_io_area(POLY_SYS_teststrgtr, &teststrgtr);
    add_function_to_io_area(POLY_SYS_teststrlss, &teststrlss);
    add_function_to_io_area(POLY_SYS_teststrgeq, &teststrgeq);
    add_function_to_io_area(POLY_SYS_teststrleq, &teststrleq);
    add_function_to_io_area(POLY_SYS_exception_trace, &exception_tracea);
    add_function_to_io_area(POLY_SYS_lockseg, &locksega);
    add_function_to_io_area(POLY_SYS_profiler, &profilera);
    add_function_to_io_area(POLY_SYS_aplus, &add_long);
    add_function_to_io_area(POLY_SYS_is_short, &is_shorta);
    add_function_to_io_area(POLY_SYS_quotrem, &quotrem_longa);
    add_function_to_io_area(POLY_SYS_aminus, &sub_long);
    add_function_to_io_area(POLY_SYS_amul, &mult_long);
    add_function_to_io_area(POLY_SYS_adiv, &div_longa);
    add_function_to_io_area(POLY_SYS_amod, &rem_longa);
    add_function_to_io_area(POLY_SYS_aneg, &neg_long);
    add_function_to_io_area(POLY_SYS_equala, &equal_long);
    add_function_to_io_area(POLY_SYS_ora, &or_long);
    add_function_to_io_area(POLY_SYS_anda, &and_long);
    add_function_to_io_area(POLY_SYS_xora, &xor_long);
    add_function_to_io_area(POLY_SYS_Real_str, &Real_stra);
    add_function_to_io_area(POLY_SYS_Real_geq, &Real_geqa);
    add_function_to_io_area(POLY_SYS_Real_leq, &Real_leqa);
    add_function_to_io_area(POLY_SYS_Real_gtr, &Real_gtra);
    add_function_to_io_area(POLY_SYS_Real_lss, &Real_lssa);
    add_function_to_io_area(POLY_SYS_Real_eq, &Real_eqa);
    add_function_to_io_area(POLY_SYS_Real_neq, &Real_neqa);
    add_function_to_io_area(POLY_SYS_Real_Dispatch, &Real_dispatcha);
    add_function_to_io_area(POLY_SYS_Add_real, &Real_adda);
    add_function_to_io_area(POLY_SYS_Sub_real, &Real_suba);
    add_function_to_io_area(POLY_SYS_Mul_real, &Real_mula);
    add_function_to_io_area(POLY_SYS_Div_real, &Real_diva);
    add_function_to_io_area(POLY_SYS_Neg_real, &Real_nega);
    add_function_to_io_area(POLY_SYS_Repr_real, &Real_repra);
    add_function_to_io_area(POLY_SYS_conv_real, &Real_conva);
    add_function_to_io_area(POLY_SYS_real_to_int, &Real_inta);
    add_function_to_io_area(POLY_SYS_int_to_real, &Real_floata);
    add_function_to_io_area(POLY_SYS_sqrt_real, &Real_sqrta);
    add_function_to_io_area(POLY_SYS_sin_real, &Real_sina);
    add_function_to_io_area(POLY_SYS_cos_real, &Real_cosa);
    add_function_to_io_area(POLY_SYS_arctan_real, &Real_arctana);
    add_function_to_io_area(POLY_SYS_exp_real, &Real_expa);
    add_function_to_io_area(POLY_SYS_ln_real, &Real_lna);
    add_function_to_io_area(POLY_SYS_io_operation, &io_operationa);
    add_function_to_io_area(POLY_SYS_atomic_incr, &atomic_incr);
    add_function_to_io_area(POLY_SYS_atomic_decr, &atomic_decr);
    add_function_to_io_area(POLY_SYS_thread_self, &thread_self);
    add_function_to_io_area(POLY_SYS_thread_dispatch, &thread_dispatcha);
    add_function_to_io_area(POLY_SYS_offset_address, &offset_address);
    add_function_to_io_area(POLY_SYS_shift_right_word, &shift_right_word);
    add_function_to_io_area(POLY_SYS_word_neq, &word_neq);
    add_function_to_io_area(POLY_SYS_not_bool, &not_bool);
    add_function_to_io_area(POLY_SYS_string_length, &string_length);
    add_function_to_io_area(POLY_SYS_int_eq, &int_eq);
    add_function_to_io_area(POLY_SYS_int_neq, &int_neq);
    add_function_to_io_area(POLY_SYS_int_geq, &int_geq);
    add_function_to_io_area(POLY_SYS_int_leq, &int_leq);
    add_function_to_io_area(POLY_SYS_int_gtr, &int_gtr);
    add_function_to_io_area(POLY_SYS_int_lss, &int_lss);
    add_function_to_io_area(POLY_SYS_or_word, &or_word);
    add_function_to_io_area(POLY_SYS_and_word, &and_word);
    add_function_to_io_area(POLY_SYS_xor_word, &xor_word);
    add_function_to_io_area(POLY_SYS_shift_left_word, &shift_left_word);
    add_function_to_io_area(POLY_SYS_word_eq, &word_eq);
    add_function_to_io_area(POLY_SYS_load_byte, &load_byte);
    add_function_to_io_area(POLY_SYS_load_word, &load_word);
    add_function_to_io_area(POLY_SYS_is_big_endian, &is_big_endian);
    add_function_to_io_area(POLY_SYS_bytes_per_word, &bytes_per_word);
    add_function_to_io_area(POLY_SYS_assign_byte, &assign_byte);
    add_function_to_io_area(POLY_SYS_assign_word, &assign_word);
    add_function_to_io_area(POLY_SYS_objsize, & objsize_a ); /* MJC 27/04/88 */
    add_function_to_io_area(POLY_SYS_showsize,& showsize_a); /* MJC 09/03/89 */
    add_function_to_io_area(POLY_SYS_timing_dispatch, & timing_dispatch_a); /* DCJM 10/4/00 */
    add_function_to_io_area(POLY_SYS_XWindows,& XWindows_a); /* MJC 27/09/90 */
    add_function_to_io_area(POLY_SYS_full_gc,     & full_gc_a);     /* MJC 18/03/91 */
    add_function_to_io_area(POLY_SYS_stack_trace, & stack_trace_a); /* MJC 18/03/91 */
    add_function_to_io_area(POLY_SYS_foreign_dispatch, &foreign_dispatch_a); /* NIC 22/04/94 */
    add_function_to_io_area(POLY_SYS_callcode_tupled,  &callcode_tupleda);    /* SPF 07/07/94 */
    add_function_to_io_area(POLY_SYS_process_env,      &process_env_dispatch_a); /* DCJM 25/4/00 */
    add_function_to_io_area(POLY_SYS_set_string_length, &set_string_length_a); /* DCJM 28/2/01 */
    add_function_to_io_area(POLY_SYS_get_first_long_word, &get_first_long_word_a); /* DCJM 28/2/01 */
    add_function_to_io_area(POLY_SYS_shrink_stack,     &shrink_stack_a);     /* SPF  9/12/96 */
    add_function_to_io_area(POLY_SYS_code_flags,       &set_flags_a);        /* SPF 12/02/97 */
    add_function_to_io_area(POLY_SYS_shift_right_arith_word, &shift_right_arith_word); /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_int_to_word,      &int_to_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_set_code_constant,&set_code_constanta); /* DCJM 2/1/01 */
    add_function_to_io_area(POLY_SYS_move_bytes,       &move_bytes);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_move_words,       &move_words);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_mul_word,         &mul_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_plus_word,        &plus_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_minus_word,       &minus_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_div_word,         &div_worda);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_mod_word,         &mod_worda);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_word_geq,         &word_geq);
    add_function_to_io_area(POLY_SYS_word_leq,         &word_leq);
    add_function_to_io_area(POLY_SYS_word_gtr,         &word_gtr);
    add_function_to_io_area(POLY_SYS_word_lss,         &word_lss);

    add_function_to_io_area(POLY_SYS_io_dispatch, &IO_dispatch_a); /* DCJM 8/5/00 */
    add_function_to_io_area(POLY_SYS_network, &Net_dispatch_a); /* DCJM 8/5/00 */
    add_function_to_io_area(POLY_SYS_os_specific, &OS_spec_dispatch_a); /* DCJM 8/5/00 */
    add_function_to_io_area(POLY_SYS_signal_handler, &Sig_dispatch_a); /* DCJM 18/7/00 */
    add_function_to_io_area(POLY_SYS_poly_specific, &poly_dispatch_a); /* DCJM 17/6/06 */
    add_function_to_io_area(POLY_SYS_bytevec_eq, &bytevec_eq_a);

    // This is now a "closure" like all the other entries.
    add_function_to_io_area(POLY_SYS_raisex,           raisex);

    // Set up the signal handlers.
    {

      /* Solaris 2 */
      struct sigaction catchvec;
      
      /* disable all interrupts while we are in the interupt handler  */
      sigfillset(&catchvec.sa_mask);
      catchvec.sa_flags = SA_ONSTACK | SA_SIGINFO | SA_RESTART;
      catchvec.sa_sigaction = catchEMT;
      assert(sigaction(SIGEMT, &catchvec, 0) == 0);
      markSignalInuse(SIGEMT);

      sigfillset(&catchvec.sa_mask);
      catchvec.sa_flags = SA_ONSTACK | SA_SIGINFO | SA_RESTART;
      catchvec.sa_sigaction = catchILL;
      assert(sigaction(SIGILL, &catchvec, 0) == 0);
      markSignalInuse(SIGILL);

      /* SIGSEGV seems to be generated by the trap instructions in Solaris. */
      sigemptyset(&catchvec.sa_mask);
      catchvec.sa_flags = SA_ONSTACK | SA_SIGINFO;
      catchvec.sa_sigaction = catchSEGV;
      assert(sigaction(SIGSEGV, &catchvec, 0) == 0);
      markSignalInuse(SIGSEGV);
    }
}

// Build an ML code segment on the heap to hold a copy of a piece of code
Handle SparcDependent::BuildCodeSegment(TaskData *taskData, const unsigned *code, unsigned codeWords, char functionName)
{
    POLYUNSIGNED words = codeWords + 6;
    Handle codeHandle = alloc_and_save(taskData, words, F_CODE_OBJ);
    byte *cp = codeHandle->Word().AsCodePtr();
    memcpy(cp, code, codeWords*sizeof(PolyWord));
    codeHandle->WordP()->Set(codeWords++, PolyWord::FromUnsigned(0)); // Marker word
    codeHandle->WordP()->Set(codeWords, PolyWord::FromUnsigned(codeWords*sizeof(PolyWord))); // Bytes to the start
    codeWords++;
    codeHandle->WordP()->Set(codeWords++, PolyWord::FromUnsigned(0)); // Profile count
    codeHandle->WordP()->Set(codeWords++, TAGGED(functionName)); // Name of function - single character
    codeHandle->WordP()->Set(codeWords++, TAGGED(0)); // Register set
    codeHandle->WordP()->Set(codeWords++, PolyWord::FromUnsigned(2)); // Number of constants
    FlushInstructionCache(cp, codeWords*sizeof(PolyWord));
    return codeHandle;
}

// Generate a code sequence to enter the RTS with a request to kill
// the current process (thread).  This is slightly different to the normal
// RTS call sequence because it is entered as though some previous code had
// RETURNED to it.  That's why we need two nops at the start and don't adjust
// %o7.
static unsigned killSelfCode[] =
{
    0x01000000, // nop
    0x01000000, // nop
    0xf8062024, // ld [36+%i0].%i4
    0xb6102054, // mov POLY_SYS_kill_self,%i3
    0x81c70000, // jmp %i4
    0xf6262004  // st %i3,[%i0+4] ! DELAY SLOT
};

// We need the kill-self code in a little function.
Handle SparcDependent::BuildKillSelfCode(TaskData *taskData)
{
    return BuildCodeSegment(taskData, killSelfCode, sizeof(killSelfCode)/sizeof(unsigned), 'K');
}

void SparcDependent::SetException(TaskData *taskData, poly_exn *exc)
/* Set up the stack of a process to raise an exception. */
{
    taskData->stack->p_reg[OFFSET_REGCLOSURE] = (PolyObject*)IoEntry(POLY_SYS_raisex);
    taskData->stack->p_pc   = PC_RETRY_SPECIAL;
    taskData->stack->p_reg[OFFSET_REGRESULT] = exc;
}

/******************************************************************************/
/*                                                                            */
/*      MD_reset_signals - called by run_time.c                               */
/*                                                                            */
/******************************************************************************/
void SparcDependent::ResetSignals(void)
{
    /* restore default signal handling in child process after a "fork". */
    signal(SIGFPE,  SIG_DFL);
    signal(SIGEMT,  SIG_DFL);
    signal(SIGILL,  SIG_DFL);
    
    /* "just in case" */
    signal(SIGBUS,  SIG_DFL);
    signal(SIGSEGV, SIG_DFL);
}

void SparcDependent::ScanConstantsWithinCode(PolyObject *addr, PolyObject *old,
                                               POLYUNSIGNED length, ScanAddress *process)
{
    PolyWord *pt = (PolyWord*)addr;
    PolyWord *end;
    POLYUNSIGNED unused;
    /* Find the end of the code (before the constants). */
    addr->GetConstSegmentForCode(length, end, unused);
    end -= 3;
    assert(end->AsUnsigned() == 0); /* This should be the marker PolyWord. */
    
    while (pt != end)
    {
        POLYUNSIGNED instr = (*pt).AsUnsigned();
        if ((instr & 0xc1c00000) == 0x01000000) /* sethi instr. */
        {
            unsigned reg = (instr >> 25) & 31;
            // If the register is %i3, %i4 or %i5 the value is an integer even
            // if it is untagged.  If it's %g0 this is simply a nop.
            if (reg < 27 && reg != 0)
            {
                /* Next must be an ADD instruction. */
                assert((pt[1].AsUnsigned() & 0xc1f83000) == 0x80002000);
                /* Process this address. */
                process->ScanConstant((byte*)pt, PROCESS_RELOC_SPARCDUAL);
                pt++; // Skip the second word.
            }
        }
        else if ((instr & 0xc0000000) == 0x40000000) /* Call instr. */
        {
            POLYSIGNED disp = instr & 0x3fffffff; // Word displacement.
            // We're assuming here that multiplying by 4 will turn an unsigned value into a signed.
            PolyWord *absAddr = pt + disp;
            // Ignore it if it's local to this code seg.
            if (! (absAddr >= (PolyWord*)addr && absAddr < end))
            {
                /* The old value of the displacement was relative to the old address.  */
                absAddr = absAddr - (PolyWord*)addr + (PolyWord*)old;
                // We have to correct the displacement for the new location and store
                // that away before we call ScanConstant.
                POLYSIGNED newDisp = absAddr - pt;
                *pt = PolyWord::FromUnsigned((newDisp & 0x3fffffff) | 0x40000000);
                process->ScanConstant((byte*)pt, PROCESS_RELOC_SPARCRELATIVE);
            }
        }
        pt++;
    }
}

/* Store a constant at a specific address in the code.  This is used by
   the code generator.  It needs to be built into the RTS because we
   have to split the value in order to store it into two instructions.
   Separately the two values might well be invalid addresses. */
void SparcDependent::SetCodeConstant(TaskData *taskData, Handle data, Handle constant, Handle offseth, Handle base)
{
    /* The offset is a byte count. */
    unsigned offset = get_C_ulong(taskData, DEREFWORD(offseth));
    POLYUNSIGNED *pointer = (POLYUNSIGNED*)(DEREFWORD(base).AsCodePtr() + offset);
    assert((offset & 3) == 0);
    if (pointer[0] == 0x40000000) /* Call instruction. */
    {
        POLYUNSIGNED *c = (POLYUNSIGNED*)(DEREFWORD(constant).AsCodePtr());
        int disp = c - pointer; /* Signed displacement in words. */
        pointer[0] = (disp & 0x3fffffff) | 0x40000000;
    }
    else
    {
        POLYUNSIGNED c = DEREFWORD(constant).AsUnsigned(); // N.B.  This may well really be an address.
        unsigned hi = c >> 10, lo = c & 0x3ff;
        /* The first PolyWord must be SETHI. */
        assert((pointer[0] & 0xc1ffffff) == 0x01000000);
        /* and the next must be ADD.  The immediate value must currently be tagged(0). */
        assert((pointer[1] & 0xc1f83fff) == 0x80002001);
        pointer[0] |= hi;
        pointer[1] = (pointer[1] & 0xfffff000) | lo;
    }
}

static POLYUNSIGNED AtomicAdd(PolyObject *p, POLYUNSIGNED toAdd)
{
    POLYUNSIGNED result;
    __asm__ __volatile__ (
      "membar #StoreLoad | #LoadLoad\n" 
      "1: ld [%2],%%l0\n"  
      " add %%l0,%1,%%l1\n"
      " cas [%2],%%l0,%%l1\n"
      " cmp %%l0,%%l1\n"
      " bne,pn %%icc,1b\n"
      " nop\n"
      " add %%l0,%1,%0\n"
      " ba,pt %%xcc,1f\n"
      " membar #StoreLoad | #StoreStore\n"
      "1:\n"
    :"=r"(result) // Output
    :"r"(toAdd), "r" (p) // Input
    : "%l0", "%l1", "cc", "memory" // Modifies l0, l1, cc and memory
    );
    return result;
}

// We have assembly code versions of atomic increment and decrement and it's
// important that if we use the same method of locking a mutex whether it's
// done in the assembly code or the RTS.
// Increment the value contained in the first word of the mutex.
Handle SparcDependent::AtomicIncrement(TaskData *taskData, Handle mutexp)
{
    PolyObject *p = DEREFHANDLE(mutexp);
    POLYUNSIGNED result = AtomicAdd(p, 1 << POLY_TAGSHIFT);
    return taskData->saveVec.push(PolyWord::FromUnsigned(result));
}

// Decrement the value contained in the first word of the mutex.
Handle SparcDependent::AtomicDecrement(TaskData *taskData, Handle mutexp)
{
    PolyObject *p = DEREFHANDLE(mutexp);
    POLYUNSIGNED result = AtomicAdd(p, (-1) << POLY_TAGSHIFT);
    return taskData->saveVec.push(PolyWord::FromUnsigned(result));
}

// Release a mutex and if necessary apply a write barrier.
void SparcDependent::SetToReleased(TaskData * /*taskData*/, Handle mutexp)
{
    __asm__ __volatile__ (
      " membar #LoadStore|#StoreStore\n"
    :
    :
    : "memory"
    );
    DEREFHANDLE(mutexp)->Set(0, TAGGED(1)); // Set this to released.
}


void SparcDependent::FlushInstructionCache(void *p, POLYUNSIGNED bytes)
{
    SparcAsmFlushInstructionCache(p, bytes);
}

extern "C" int registerMaskVector[];

int SparcDependent::GetIOFunctionRegisterMask(int ioCall)
{
    return registerMaskVector[ioCall];
}

static SparcDependent sparcDependent;

MachineDependent *machineDependent = &sparcDependent;
