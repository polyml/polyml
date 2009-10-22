/*
    Title:  Machine dependent code for i386 and X64 under Windows and Unix

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <stdio.h>

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef HAVE_ASSERT_H 
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#ifdef HAVE_STRING_H 
#include <string.h>
#endif

#ifdef WINDOWS_PC
#include <windows.h>
#include <excpt.h>
#endif

#include "globals.h"
#include "run_time.h"
#include "mpoly.h"
#include "arb.h"
#include "diagnostics.h"
#include "processes.h"
#include "sys.h"
#include "profiling.h"
#include "sighandler.h"
#include "machine_dep.h"
#include "scanaddrs.h"
#include "gc.h"
#include "check_objects.h"
#include "save_vec.h"
#include "memmgr.h"
#include "foreign.h"

#ifndef HAVE_SIGALTSTACK
// If we can't handle signals on a separate stack make sure there's space
// on the Poly stack.  At the moment this includes Windows although we
// probably don't need this.
#define EXTRA_STACK 1024
#else
#define EXTRA_STACK 0
#endif

#ifndef HOSTARCHITECTURE_X86_64
#define CHECKED_REGS    6
#else /* HOSTARCHITECTURE_X86_64 */
#define CHECKED_REGS    13
#endif /* HOSTARCHITECTURE_X86_64 */
/* The unchecked reg field is used for the condition codes. */
#define UNCHECKED_REGS  1

// Number of arguments passed in registers,
// The remainder go on the stack.
#ifndef HOSTARCHITECTURE_X86_64
#define ARGS_IN_REGS    2
#else /* HOSTARCHITECTURE_X86_64 */
#define ARGS_IN_REGS    5
#endif /* HOSTARCHITECTURE_X86_64 */


/* the amount of ML stack space to reserve for registers,
   C exception handling etc. The compiler requires us to
   reserve 2 stack-frames worth (2 * 20 words) plus whatever
   we require for the register save area. We actually reserve
   slightly more than this. SPF 3/3/97
*/
#define OVERFLOW_STACK_SIZE \
  (50 + \
   sizeof(StackObject)/sizeof(PolyWord) + \
   CHECKED_REGS + \
   UNCHECKED_REGS + \
   EXTRA_STACK)



// These "memory registers" are referenced from the assembly code.
// Some are actually referenced from ML code so the offsets are built in.
typedef struct _MemRegisters {
    // These offsets are built into the code generator and assembly code
    PolyWord    *localMpointer;     // Allocation ptr + 1 word
    PolyWord    *handlerRegister;   // Current exception handler
    // Originally these next two were checked using a BOUNDS instruction.  That compared
    // a value against a lower and upper limit.
    PolyWord    *localMbottom;      // Base of memory + 1 word
    PolyWord    *stackLimit;        // Lower limit of stack
    // We don't actually need to do the check against the upper limit but
    // we need to pass this to the assembly code so this is a convenient place
    // to do it.
    PolyWord    *stackTop;          // Upper limit of stack
    // These offsets are built into the assembly code section
    byte        requestCode;        // IO function to call.
        // The offset (20/40) of requestCode is built into the MAKE_IO_CALL_SEQUENCE macro
    byte        inRTS;              // Flag indicating we're not in ML
    byte        returnReason;       // Reason for returning from ML.
        // The offset (22/42) of returnReason is built into the MAKE_IO_CALL_SEQUENCE macro
    StackObject *polyStack;         // Current stack base
    PolyWord    *savedSp;           // Saved C stack pointer

    byte        *heapOverflow;      // Called when the heap limit is reached
    byte        *stackOverflow;     // Called when the stack limit is reached
    byte        *stackOverflowEx;   // Called when the stack limit is reached (alternate)
    byte        *raiseException;    // Called to raise an exception.  The packet is passed in eax.
    // The offset (48/96) of ioEntry is built into the MAKE_IO_CALL_SEQUENCE macro
    byte        *ioEntry;           // Called for an IO function
    byte        *raiseDiv;          // Called to raise the Div exception.
    byte        *arbEmulation;      // This address is called to emulate an arbitrary precision op
    PolyObject  *threadId;          // My thread id.  Saves having to call into RTS for it.
    POLYSIGNED  real_temp;          // Space used to convert integers to reals.
} MemRegisters;

class X86TaskData: public MDTaskData {
public:
    X86TaskData(): allocReg(0), allocWords(0)
    {
    memRegisters.inRTS = 1; // We start off in the RTS.
    }
    unsigned allocReg; // The register to take the allocated space.
    POLYUNSIGNED allocWords; // The words to allocate.
    Handle callBackResult;
    MemRegisters memRegisters;
};


class X86Dependent: public MachineDependent {
public:
    X86Dependent() {}

    // Create a task data object.
    virtual MDTaskData *CreateTaskData(void) { return new X86TaskData(); }

    virtual unsigned InitialStackSize(void) { return 128+OVERFLOW_STACK_SIZE; } // Initial size of a stack 
    virtual void InitInterfaceVector(void);
    virtual void ResetSignals(void);
    virtual void ScanConstantsWithinCode(PolyObject *addr, PolyObject *oldAddr, POLYUNSIGNED length, ScanAddress *process);
    virtual int  GetIOFunctionRegisterMask(int ioCall);

    virtual Architectures MachineArchitecture(void)
#ifndef HOSTARCHITECTURE_X86_64
         { return MA_I386; }
#else /* HOSTARCHITECTURE_X86_64 */
         { return MA_X86_64; }
#endif /* HOSTARCHITECTURE_X86_64 */
    virtual void SetCodeConstant(TaskData *taskData, Handle data, Handle constant, Handle offseth, Handle base);
#ifndef HOSTARCHITECTURE_X86_64
    virtual unsigned char *BuildCallback(TaskData *taskData, int cbEntryNo, Handle cResultType, int nArgsToRemove);
    virtual void GetCallbackArg(void **args, void *argLoc, int nSize);
#endif

    virtual int SwitchToPoly(TaskData *taskData);
    virtual void SetForRetry(TaskData *taskData, int ioCall);
    virtual void InterruptCode(TaskData *taskData);
    virtual bool GetPCandSPFromContext(TaskData *taskData, SIGNALCONTEXT *context, PolyWord *&sp, POLYCODEPTR &pc);
    virtual void InitStackFrame(TaskData *taskData, Handle stack, Handle proc, Handle arg);
    virtual void SetException(TaskData *taskData, poly_exn *exc);
    virtual void CallIO0(TaskData *taskData, Handle(*ioFun)(TaskData *));
    virtual void CallIO1(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle));
    virtual void CallIO2(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle));
    virtual void CallIO3(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle));
    virtual void CallIO4(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle, Handle));
    virtual void CallIO5(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle, Handle, Handle));
    virtual Handle CallBackResult(TaskData *taskData) { return ((X86TaskData*)taskData->mdTaskData)->callBackResult; } 
    virtual void SetExceptionTrace(TaskData *taskData);
    virtual void CallCodeTupled(TaskData *taskData);
    virtual void SetCallbackFunction(TaskData *taskData, Handle func, Handle args);
    // Increment or decrement the first word of the object pointed to by the
    // mutex argument and return the new value.
    virtual Handle AtomicIncrement(TaskData *taskData, Handle mutexp);
    virtual Handle AtomicDecrement(TaskData *taskData, Handle mutexp);

    void SetMemRegisters(TaskData *taskData);
    void SaveMemRegisters(TaskData *taskData);
    void HeapOverflowTrap(TaskData *taskData);
    void ArbitraryPrecisionTrap(TaskData *taskData);
    PolyWord *get_reg(TaskData *taskData, int n);
    void do_compare(TaskData *taskData, PolyWord v1, PolyWord v2);
    void do_op(TaskData *taskData, int dest, PolyWord v1, PolyWord v2, Handle (*op)(TaskData *, Handle, Handle));
    bool emulate_instrs(TaskData *taskData);
    Handle BuildCodeSegment(TaskData *taskData, const byte *code, unsigned bytes, char functionName);
    Handle BuildKillSelf(TaskData *taskData);

};


// Entry code sequences - copied to memRegisters before entering ML.
static byte *heapOverflow, *stackOverflow, *stackOverflowEx, *raiseDiv, *arbEmulation;


/**********************************************************************
 *
 * Register usage:
 *
 *  %Reax: First argument to function.  Result of function call.
 *  %Rebx: Second argument to function.
 *  %Recx: General register
 *  %Redx: Closure pointer in call.
 *  %Rebp: Points to memory used for extra registers
 *  %Resi: General register.
 *  %Redi: General register.
 *  %Resp: Stack pointer.
 *  The following apply only on the X64
 *  %R8:   Third argument to function
 *  %R9:   Fourth argument to function
 *  %R10:  Fifth argument to function
 *  %R11:  General register
 *  %R12:  General register
 *  %R13:  General register
 *  %R14:  General register
 *  %R15:  Memory allocation pointer

 *
 **********************************************************************/


/**********************************************************************
 *
 * Register fields in the stack. 
 *
 **********************************************************************/
#define PSP_EAX(stack)          (stack)->p_reg[0]
#define PSP_EBX(stack)          (stack)->p_reg[1]
#define PSP_ECX(stack)          (stack)->p_reg[2]
#define PSP_EDX(stack)          (stack)->p_reg[3]
#define PSP_ESI(stack)          (stack)->p_reg[4]
#define PSP_EDI(stack)          (stack)->p_reg[5]

// X64 registers only
#define PSP_R8(stack)           (stack)->p_reg[6]
#define PSP_R9(stack)           (stack)->p_reg[7]
#define PSP_R10(stack)          (stack)->p_reg[8]
#define PSP_R11(stack)          (stack)->p_reg[9]
#define PSP_R12(stack)          (stack)->p_reg[10]
#define PSP_R13(stack)          (stack)->p_reg[11]
#define PSP_R14(stack)          (stack)->p_reg[12]

#define PSP_EFLAGS(stack)       (stack)->p_reg[CHECKED_REGS+1]

#define PSP_IC(stack)           (stack)->p_pc
#define PSP_INCR_PC(stack, n)   (stack)->p_pc += n
#define PSP_SP(stack)           (stack)->p_sp
#define PSP_HR(stack)           (stack)->hr  

// Values for the returnReason byte
enum RETURN_REASON {
    RETURN_IO_CALL = 0,
    RETURN_HEAP_OVERFLOW,
    RETURN_STACK_OVERFLOW,
    RETURN_STACK_OVERFLOWEX,
    RETURN_RAISE_DIV,
    RETURN_ARB_EMULATION,
    RETURN_CALLBACK_RETURN,
    RETURN_CALLBACK_EXCEPTION
};

extern "C" {


    // These are declared in the assembly code segment.
    void X86AsmSwitchToPoly(MemRegisters *);
    void X86AsmSaveStateAndReturn(void);

    // These are declared in the assembly code.  They provide hand coded versions
    // of simple functions.  Some cases, such as adding words, are actually handled by
    // the code generator, so the assembly code versions would only be called when
    // the function is passed as a closure e.g. map (op+) [(1,2),(3,4)]
    extern int alloc_store();
    extern int get_length_a();
    extern int str_compare();
    extern int teststreq(), teststrneq(), teststrgtr(), teststrlss(), teststrgeq(), teststrleq();
    extern int locksega();
    extern int is_shorta();
    extern int add_long(), sub_long(), mult_long(), div_long(), rem_long(), neg_long();
    extern int equal_long(), or_long(), and_long(), xor_long();
    extern int offset_address();
    extern int shift_right_word();
    extern int word_neq();
    extern int not_bool();
    extern int string_length();
    extern int int_eq(), int_neq(), int_geq(), int_leq(), int_gtr(), int_lss();
    extern int or_word(), and_word(), xor_word(), shift_left_word(), shift_right_arith_word();
    extern int word_eq();
    extern int load_byte(), load_word();
    extern int is_big_endian();
    extern int bytes_per_word();
    extern int assign_byte(), assign_word();
    extern int set_string_length_a();
    extern int get_first_long_word_a();
    extern int int_to_word();
    extern int move_bytes(), move_words();
    extern int mul_word(), plus_word(), minus_word(), div_word(), mod_word();
    extern int word_geq(), word_leq(), word_gtr(), word_lss();
    extern int raisex();
    extern int thread_self(), atomic_increment(), atomic_decrement();
    extern int real_add(), real_sub(), real_mul(), real_div(), real_neg();
    extern int real_geq(), real_leq(), real_gtr(), real_lss(), real_eq(), real_neq();
    extern int real_from_int();
};

// Run the current ML process.  X86AsmSwitchToPoly saves the C state so that
// whenever the ML requires assistance from the rest of the RTS it simply
// returns to C with the appropriate values set in memRegisters.requestCode and
// 

int X86Dependent::SwitchToPoly(TaskData *taskData)
// (Re)-enter the Poly code from C.  Returns with the io function to call or
// -1 if we are responding to an interrupt.
{
    X86TaskData *mdTask = (X86TaskData*)taskData->mdTaskData;
    Handle mark = taskData->saveVec.mark();
    do
    {
        taskData->saveVec.reset(mark); // Remove old data e.g. from arbitrary precision.
        CheckMemory(); // Do any memory checking before calling SetMemRegisters
                       // (which may set pc to a temporarily bad value if this is a retry).
        SetMemRegisters(taskData);

        X86AsmSwitchToPoly(&mdTask->memRegisters);

        SaveMemRegisters(taskData); // Update globals from the memory registers.

        // Handle any heap/stack overflows or arbitrary precision traps.
        switch (mdTask->memRegisters.returnReason)
        {

        case RETURN_IO_CALL:
            return mdTask->memRegisters.requestCode;

        case RETURN_HEAP_OVERFLOW:
            // The heap has overflowed.  Pop the return address into the program counter.
            // It may well not be a valid code address anyway.
            PSP_IC(taskData->stack) = (*(PSP_SP(taskData->stack))++).AsCodePtr();
            HeapOverflowTrap(taskData); // Computes a value for allocWords only
            break;

        case RETURN_STACK_OVERFLOW:
            try {
                // The stack check has failed.  This may either be because we really have
                // overflowed the stack or because the stack limit value has been adjusted
                // to result in a call here.
                PSP_IC(taskData->stack) = (*PSP_SP(taskData->stack)++).AsCodePtr();
                CheckAndGrowStack(taskData, taskData->stack->p_sp);
            }
            catch (IOException) {
               // We may get an exception while handling this if we run out of store
            }
            return -1; // We're in a safe state to handle any interrupts.

        case RETURN_STACK_OVERFLOWEX:
            try {
                // Stack limit overflow.  If the required stack space is larger than
                // the fixed overflow size the code will calculate the limit in %EDI.
                // We need to extract that and the clear that register since it may
                // very well be outside the stack and therefore a "bad address".
                PolyWord *stackP = PSP_EDI(taskData->stack).AsStackAddr();
                PSP_EDI(taskData->stack) = TAGGED(0);
                PSP_IC(taskData->stack) = (*PSP_SP(taskData->stack)++).AsCodePtr();
                CheckAndGrowStack(taskData, stackP);
            }
            catch (IOException) {
               // We may get an exception while handling this if we run out of store
            }
            return -1; // We're in a safe state to handle any interrupts.

        case RETURN_RAISE_DIV:
            try {
                // Generally arithmetic operations don't raise exceptions.  Overflow
                // is either ignored, for Word operations, or results in a call to
                // the abitrary precision emulation code.  This is the exception
                // (no pun intended).
                PSP_IC(taskData->stack) = (*PSP_SP(taskData->stack)++).AsCodePtr();
                // Set all the registers to a safe value here.  We will almost certainly
                // have shifted a value in one of the registers before testing it for zero.
                for (POLYUNSIGNED i = 0; i < taskData->stack->p_nreg; i++)
                    taskData->stack->p_reg[i] = TAGGED(0);
                raise_exception0(taskData, EXC_divide);
            }
            catch (IOException) {
                // Handle the C++ exception.
            }
            break;

        case RETURN_ARB_EMULATION:
            try {
                PSP_IC(taskData->stack) = (*PSP_SP(taskData->stack)++).AsCodePtr();
                ArbitraryPrecisionTrap(taskData);
            }
            catch (IOException) {
                // We may get an exception in the trap handler e.g. if we run out of store.
            }
            break;

        case RETURN_CALLBACK_RETURN:
            // Remove the extra exception handler we created in SetCallbackFunction
            ASSERT(taskData->stack->p_hr == PSP_SP(taskData->stack));
            PSP_SP(taskData->stack) += 2;
            taskData->stack->p_hr = (*(PSP_SP(taskData->stack)++)).AsStackAddr(); // Restore the previous handler.
            mdTask->callBackResult = taskData->saveVec.push(PSP_EAX(taskData->stack)); // Argument to return is in EAX.
            // Restore the registers
#ifdef HOSTARCHITECTURE_X86_64
            PSP_R10(taskData->stack) = *PSP_SP(taskData->stack)++;
            PSP_R9(taskData->stack) = *PSP_SP(taskData->stack)++;
            PSP_R8(taskData->stack) = *PSP_SP(taskData->stack)++;
#endif
            PSP_EBX(taskData->stack) = *PSP_SP(taskData->stack)++;
            PSP_EAX(taskData->stack) = *PSP_SP(taskData->stack)++;
            PSP_EDX(taskData->stack) = *PSP_SP(taskData->stack)++;
            taskData->stack->p_pc = (*PSP_SP(taskData->stack)).AsCodePtr(); // Set the return address
            return -2;

        case RETURN_CALLBACK_EXCEPTION:
            // An ML callback has raised an exception.
            SetException(taskData, (poly_exn *)PSP_EAX(taskData->stack).AsObjPtr());
            // Raise a C++ exception.  If the foreign function that called this callback
            // doesn't handle the exception it will be raised in the calling ML function.
            // But if it is caught we may have a problem ...
            throw IOException(EXC_EXCEPTION);

        default:
            Crash("Unknown return reason code %u", mdTask->memRegisters.returnReason);
        }

    } while (1);
}

void X86Dependent::InitStackFrame(TaskData *parentTaskData, Handle stackh, Handle proc, Handle arg)
/* Initialise stack frame. */
{
    StackObject *newStack = (StackObject *)DEREFWORDHANDLE(stackh);
    POLYUNSIGNED stack_size     = newStack->Length();
    POLYUNSIGNED topStack = stack_size-5;
    newStack->p_space = OVERFLOW_STACK_SIZE;
    newStack->p_pc    = PC_RETRY_SPECIAL;
    newStack->p_sp    = newStack->Offset(topStack); 
    newStack->p_hr    = newStack->Offset(topStack)+1;
    newStack->p_nreg  = CHECKED_REGS;

    for (POLYUNSIGNED i = 0; i < CHECKED_REGS; i++) newStack->p_reg[i] = TAGGED(0);

    newStack->p_reg[CHECKED_REGS] = PolyWord::FromUnsigned(UNCHECKED_REGS); /* 1 unchecked register */
    newStack->p_reg[CHECKED_REGS+1] = PolyWord::FromUnsigned(0);
    newStack->p_reg[3] = DEREFWORDHANDLE(proc); /* rdx - closure pointer */

    /* If this function takes an argument store it in the argument register. */
    if (arg != 0) newStack->p_reg[0] = DEREFWORD(arg);

    /* We initialise the end of the stack with a sequence that will jump to
       kill_self whether the process ends with a normal return or by raising an
       exception.
       There's one additional complication.  kill_self is called via CallIO0
       which loads the value at *p_sp into p_pc assuming this is a return address.
       We need to make sure that this value is acceptable since this stack may be
       scanned by a subsequent minor GC if it's already been copied by a minor GC. */
    newStack->Set(topStack+4, TAGGED(0)); // Acceptable value if we've exited by exception.
    /* No previous handler so point it at itself. */
    newStack->Set(topStack+3, PolyWord::FromStackAddr(newStack->Offset(topStack)+3));
    // Set the default handler and return address to point to this code.
    // It's not necessary, on this architecture at least, to make these off-word
    // aligned since we're pointing at the start of some code.  That may be
    // necessary on e.g. the Sparc(?), which always subtracts two from a return
    // address before jumping to it.
    Handle killCode = BuildKillSelf(parentTaskData);
    PolyWord killJump = killCode->Word();
    newStack = (StackObject *)DEREFWORDHANDLE(stackh); // In case it's moved
    newStack->Set(topStack+2, killJump); // Default handler.
    /* Set up exception handler.  This also, conveniently, ends up in p_pc
       if we return normally.  */
    newStack->Set(topStack+1, TAGGED(0)); /* Default handler. */
    // Normal Return address.
    newStack->Set(topStack, killJump);
}

// IO Functions called indirectly from assembly code.
void X86Dependent::CallIO0(TaskData *taskData, Handle (*ioFun)(TaskData *))
{
    // Set the return address now.
    taskData->stack->p_pc = (*PSP_SP(taskData->stack)).AsCodePtr();
    try {
        Handle result = (*ioFun)(taskData);
        PSP_EAX(taskData->stack) = result->Word();
        // If this is a normal return we can pop the return address.
        // If this has raised an exception, set for retry or changed process
        // we mustn't.  N,B, The return address could have changed because of GC
        PSP_SP(taskData->stack)++;
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

void X86Dependent::CallIO1(TaskData *taskData, Handle (*ioFun)(TaskData *, Handle))
{
    taskData->stack->p_pc = (*PSP_SP(taskData->stack)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData->stack));
    try {
        Handle result = (*ioFun)(taskData, saved1);
        PSP_EAX(taskData->stack) = result->Word();
        PSP_SP(taskData->stack)++; // Pop the return address.
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

void X86Dependent::CallIO2(TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle))
{
    taskData->stack->p_pc = (*PSP_SP(taskData->stack)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData->stack));
    Handle saved2 = taskData->saveVec.push(PSP_EBX(taskData->stack));
    try {
        Handle result = (*ioFun)(taskData, saved2, saved1);
        PSP_EAX(taskData->stack) = result->Word();
        PSP_SP(taskData->stack)++;
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

void X86Dependent::CallIO3(TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle, Handle))
{
    taskData->stack->p_pc = (*PSP_SP(taskData->stack)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData->stack));
    Handle saved2 = taskData->saveVec.push(PSP_EBX(taskData->stack));
#ifndef HOSTARCHITECTURE_X86_64
    Handle saved3 = taskData->saveVec.push(PSP_SP(taskData->stack)[1]);
#else /* HOSTARCHITECTURE_X86_64 */
    Handle saved3 = taskData->saveVec.push(PSP_R8(taskData->stack));
#endif /* HOSTARCHITECTURE_X86_64 */
    try {
        Handle result = (*ioFun)(taskData, saved3, saved2, saved1);
        PSP_EAX(taskData->stack) = result->Word();
#ifndef HOSTARCHITECTURE_X86_64
        PSP_SP(taskData->stack) += 2; // Pop the return address and a stack arg.
#else /* HOSTARCHITECTURE_X86_64 */
        PSP_SP(taskData->stack)++;
#endif /* HOSTARCHITECTURE_X86_64 */
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

void X86Dependent::CallIO4(TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle, Handle, Handle))
{
    taskData->stack->p_pc = (*PSP_SP(taskData->stack)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData->stack));
    Handle saved2 = taskData->saveVec.push(PSP_EBX(taskData->stack));
#ifndef HOSTARCHITECTURE_X86_64
    Handle saved3 = taskData->saveVec.push(PSP_SP(taskData->stack)[2]);
    Handle saved4 = taskData->saveVec.push(PSP_SP(taskData->stack)[1]);
#else /* HOSTARCHITECTURE_X86_64 */
    Handle saved3 = taskData->saveVec.push(PSP_R8(taskData->stack));
    Handle saved4 = taskData->saveVec.push(PSP_R9(taskData->stack));
#endif /* HOSTARCHITECTURE_X86_64 */
    try {
        Handle result = (*ioFun)(taskData, saved4, saved3, saved2, saved1);
        PSP_EAX(taskData->stack) = result->Word();
#ifndef HOSTARCHITECTURE_X86_64
        PSP_SP(taskData->stack) += 3; // Pop the return address and two stack args.
#else /* HOSTARCHITECTURE_X86_64 */
        PSP_SP(taskData->stack)++;
#endif /* HOSTARCHITECTURE_X86_64 */
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
void X86Dependent::CallIO5(TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle, Handle, Handle, Handle))
{
    taskData->stack->p_pc = (*PSP_SP(taskData->stack)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData->stack));
    Handle saved2 = taskData->saveVec.push(PSP_EBX(taskData->stack));
#ifndef HOSTARCHITECTURE_X86_64
    Handle saved3 = taskData->saveVec.push(PSP_SP(taskData->stack)[3]);
    Handle saved4 = taskData->saveVec.push(PSP_SP(taskData->stack)[2]);
    Handle saved5 = taskData->saveVec.push(PSP_SP(taskData->stack)[1]);
#else /* HOSTARCHITECTURE_X86_64 */
    Handle saved3 = taskData->saveVec.push(PSP_R8(taskData->stack));
    Handle saved4 = taskData->saveVec.push(PSP_R9(taskData->stack));
    Handle saved5 = taskData->saveVec.push(PSP_R10(taskData->stack));
#endif /* HOSTARCHITECTURE_X86_64 */
    try {
        Handle result = (*ioFun)(taskData, saved5, saved4, saved3, saved2, saved1);
        PSP_EAX(taskData->stack) = result->Word();
#ifndef HOSTARCHITECTURE_X86_64
        PSP_SP(taskData->stack) += 4; // Pop the return address and 3 stack args
#else /* HOSTARCHITECTURE_X86_64 */
        PSP_SP(taskData->stack)++;
#endif /* HOSTARCHITECTURE_X86_64 */
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

// Build an ML code segment to hold a copy of a piece of code
Handle X86Dependent::BuildCodeSegment(TaskData *taskData, const byte *code, unsigned bytes, char functionName)
{
    POLYUNSIGNED codeWords = (bytes + sizeof(PolyWord)-1) / sizeof(PolyWord);
    POLYUNSIGNED words = codeWords + 6;
    Handle codeHandle = alloc_and_save(taskData, words, F_BYTE_OBJ|F_MUTABLE_BIT);
    byte *cp = codeHandle->Word().AsCodePtr();
    memcpy(cp, code, bytes);
    if (bytes % sizeof(PolyWord) != 0) // Fill unused bytes with NOPs
        memset(cp+bytes, 0x90, sizeof(PolyWord)- bytes % sizeof(PolyWord));
    codeHandle->WordP()->Set(codeWords++, PolyWord::FromUnsigned(0)); // Marker word
    codeHandle->WordP()->Set(codeWords, PolyWord::FromUnsigned(codeWords*sizeof(PolyWord))); // Bytes to the start
    codeWords++;
    codeHandle->WordP()->Set(codeWords++, PolyWord::FromUnsigned(0)); // Profile count
    codeHandle->WordP()->Set(codeWords++, TAGGED(functionName)); // Name of function 
    codeHandle->WordP()->Set(codeWords++, TAGGED(0)); // Register set
    codeHandle->WordP()->Set(codeWords++, PolyWord::FromUnsigned(2)); // Number of constants
    CodeSegmentFlags(taskData, taskData->saveVec.push(TAGGED(F_CODE_OBJ)), codeHandle);
    return codeHandle;
}

// Set up a handler that, if it's called, will print an exception trace.
// If the handler isn't called the dummy handler is simply removed.
// This is tricky since when we "return" we actually need to run the new
// function.
void X86Dependent::SetExceptionTrace(TaskData *taskData)
{
    taskData->stack->p_pc = (*PSP_SP(taskData->stack)).AsCodePtr();
    Handle fun = taskData->saveVec.push(PSP_EAX(taskData->stack));
    PolyObject *functToCall = fun->WordP();
    PSP_EDX(taskData->stack) = functToCall; // Closure address
    // Leave the return address where it is on the stack.
    taskData->stack->p_pc = functToCall->Get(0).AsCodePtr(); // First word of closure is entry pt.
    *(--PSP_SP(taskData->stack)) = PolyWord::FromStackAddr(taskData->stack->p_hr); // Create a special handler entry
    // We have to use a special entry here that can be recognised by the exception
    // unwinding code because we want to know the stack pointer that is in effect
    // at the time the exception is raised.  If we simply put a normal handler in here
    // that handler would be called after the stack was unwound.
    *(--PSP_SP(taskData->stack)) = TAGGED(0);
    *(--PSP_SP(taskData->stack)) = TAGGED(0);
    taskData->stack->p_hr = PSP_SP(taskData->stack);
    byte *codeAddr;
#ifndef __GNUC__
#ifdef HOSTARCHITECTURE_X86_64
    ASSERT(0); // Inline assembly not supported on Windows 64-bit
#else
    __asm {
      call endCode
        add  esp,8               // Remove handler
        pop  dword ptr [4+ebp]   // Restore the old handler
        ret                      // Return to the original caller
      endCode: pop eax
        mov codeAddr,eax
    }
#endif
#else
// GCC
    __asm__ __volatile__ (
     "call    1f;"
#ifndef HOSTARCHITECTURE_X86_64
        "addl    $8,%%esp;"
        "popl    4(%%ebp);"
#else /* HOSTARCHITECTURE_X86_64 */
        "addq    $16,%%rsp;"
        "popq    8(%%rbp);"
#endif /* HOSTARCHITECTURE_X86_64 */
        "ret;"
        "nop;"    // Add an extra byte so that we have 8 bytes on both X86 and X86_64
    "1: pop %0"
    :"=r"(codeAddr)
    );
#endif
    Handle retCode = BuildCodeSegment(taskData, codeAddr, 8 /* Code is 8 bytes */, 'R');
    *(--PSP_SP(taskData->stack)) = retCode->WordP(); // Code for normal return.
    PSP_EAX(taskData->stack) = TAGGED(0); // Set the argument of the function to "unit".
}

// In Solaris-x86 the registers are named EIP and ESP.
#if (!defined(REG_EIP) && defined(EIP))
#define REG_EIP EIP
#endif
#if (!defined(REG_ESP) && defined(ESP))
#define REG_ESP ESP
#endif


// Get the PC and SP(stack) from a signal context.  This is needed for profiling.
bool X86Dependent::GetPCandSPFromContext(TaskData *taskData, SIGNALCONTEXT *context, PolyWord * &sp, POLYCODEPTR &pc)
{
    X86TaskData *mdTask = (X86TaskData*)taskData->mdTaskData;
    if (mdTask->memRegisters.inRTS)
    {
        sp = taskData->stack->p_sp;
        pc = taskData->stack->p_pc;
        return true;
    }
// The tests for HAVE_UCONTEXT_T, HAVE_STRUCT_SIGCONTEXT and HAVE_WINDOWS_H need
// to follow the tests in machine_dep.h.
#if defined(HAVE_UCONTEXT_T)
#ifdef HAVE_MCONTEXT_T_GREGS
    // Linux
#ifndef HOSTARCHITECTURE_X86_64
    pc = (byte*)context->uc_mcontext.gregs[REG_EIP];
    sp = (PolyWord*)context->uc_mcontext.gregs[REG_ESP];
#else /* HOSTARCHITECTURE_X86_64 */
    pc = (byte*)context->uc_mcontext.gregs[REG_RIP];
    sp = (PolyWord*)context->uc_mcontext.gregs[REG_RSP];
#endif /* HOSTARCHITECTURE_X86_64 */
#elif defined(HAVE_MCONTEXT_T_MC_ESP)
   // FreeBSD
#ifndef HOSTARCHITECTURE_X86_64
    pc = (byte*)context->uc_mcontext.mc_eip;
    sp = (PolyWord*)context->uc_mcontext.mc_esp;
#else /* HOSTARCHITECTURE_X86_64 */
    pc = (byte*)context->uc_mcontext.mc_rip;
    sp = (PolyWord*)context->uc_mcontext.mc_rsp;
#endif /* HOSTARCHITECTURE_X86_64 */
#else
   // Mac OS X
#ifndef HOSTARCHITECTURE_X86_64
#if(defined(HAVE_STRUCT_MCONTEXT_SS)||defined(HAVE_STRUCT___DARWIN_MCONTEXT32_SS))
    pc = (byte*)context->uc_mcontext->ss.eip;
    sp = (PolyWord*)context->uc_mcontext->ss.esp;
#elif(defined(HAVE_STRUCT___DARWIN_MCONTEXT32___SS))
    pc = (byte*)context->uc_mcontext->__ss.__eip;
    sp = (PolyWord*)context->uc_mcontext->__ss.__esp;
#else
    return false;
#endif
#else /* HOSTARCHITECTURE_X86_64 */
#if(defined(HAVE_STRUCT_MCONTEXT_SS)||defined(HAVE_STRUCT___DARWIN_MCONTEXT64_SS))
    pc = (byte*)context->uc_mcontext->ss.rip;
    sp = (PolyWord*)context->uc_mcontext->ss.rsp;
#elif(defined(HAVE_STRUCT___DARWIN_MCONTEXT64___SS))
    pc = (byte*)context->uc_mcontext->__ss.__rip;
    sp = (PolyWord*)context->uc_mcontext->__ss.__rsp;
#else
    return false;
#endif
#endif /* HOSTARCHITECTURE_X86_64 */
#endif
#elif defined(HAVE_STRUCT_SIGCONTEXT)
    pc = (byte*)context->sc_pc;
    sp = (PolyWord*)context->sc_sp;
#elif defined(HAVE_WINDOWS_H)
#ifdef _WIN64
    sp = (PolyWord *)context->Rsp;
    pc = (POLYCODEPTR)context->Rip;
#else
    // Windows 32 including cygwin.
    sp = (PolyWord *)context->Esp;
    pc = (POLYCODEPTR)context->Eip;
#endif
#else
    // Can't get context.
    return false;
#endif
    // Check the sp value is in the current stack.
    if (sp >= (PolyWord*)taskData->stack && sp < taskData->stack->Offset(taskData->stack->Length()))
        return true;
    else
        return false; // Bad stack pointer
}

void X86Dependent::InterruptCode(TaskData *taskData)
{
    X86TaskData *mdTask = (X86TaskData*)taskData->mdTaskData;
    // Set the stack limit pointer to the top of the stack to cause
    // a trap when we next check for stack overflow.
    // SetMemRegisters actually does this anyway if "pendingInterrupt" is set but
    // it's safe to do this repeatedly.
    if (taskData->stack != 0) 
        mdTask->memRegisters.stackLimit = taskData->stack->Offset(taskData->stack->Length()-1);
    taskData->pendingInterrupt = true;
}

// This is called from SwitchToPoly before we enter the ML code.
void X86Dependent::SetMemRegisters(TaskData *taskData)
{
    X86TaskData *mdTask = (X86TaskData*)taskData->mdTaskData;
    // Copy the current store limits into variables before we go into the assembly code.

    // If we haven't yet set the allocation area or we don't have enough we need
    // to create one (or a new one).
    if (taskData->allocPointer <= taskData->allocLimit + mdTask->allocWords ||
        (userOptions.debug & DEBUG_FORCEGC))
    {
        if (taskData->allocPointer < taskData->allocLimit)
            Crash ("Bad length in heap overflow trap");

        // Find some space to allocate in.  Updates taskData->allocPointer and
        // returns a pointer to the newly allocated space (if allocWords != 0)
        PolyWord *space =
            processes->FindAllocationSpace(taskData, mdTask->allocWords, true);
        if (space == 0)
        {
            // We will now raise an exception instead of returning.
            // Set allocWords to zero so we don't set the allocation register
            // since that could be holding the exception packet.
            mdTask->allocWords = 0;
        }
        // Undo the allocation just now.
        taskData->allocPointer += mdTask->allocWords;
    }

    if (mdTask->allocWords != 0)
    {
        // If we have had a heap trap we actually do the allocation here.
        // We will have already garbage collected and recovered sufficient space.
        // This also happens if we have just trapped because of store profiling.
        taskData->allocPointer -= mdTask->allocWords; // Now allocate
#ifndef HOSTARCHITECTURE_X86_64
        // Set the allocation register to this area.
        *(get_reg(taskData, mdTask->allocReg)) =
            PolyWord::FromStackAddr(taskData->allocPointer + 1); /* remember: it's off-by-one */
#endif
        mdTask->allocWords = 0;
    }

    // If we have run out of store, either just above or while allocating in the RTS,
    // allocPointer and allocLimit will have been set to zero as part of the GC.  We will
    // now be raising an exception which may free some store but we need to come back here
    // before we allocate anything.  The compiled code uses unsigned arithmetic to check for
    // heap overflow but only after subtracting the space required.  We need to make sure
    // that the values are still non-negative after substracting any object size.
    if (taskData->allocPointer == 0) taskData->allocPointer += MAX_OBJECT_SIZE;
    if (taskData->allocLimit == 0) taskData->allocLimit += MAX_OBJECT_SIZE;

    mdTask->memRegisters.localMbottom = taskData->allocLimit + 1;
    mdTask->memRegisters.localMpointer = taskData->allocPointer + 1;
    // If we are profiling store allocation we set mem_hl so that a trap
    // will be generated.
    if (profileMode == kProfileStoreAllocation || (userOptions.debug & (DEBUG_FORCEGC|DEBUG_REGION_CHECK)))
        mdTask->memRegisters.localMbottom = mdTask->memRegisters.localMpointer;

    mdTask->memRegisters.polyStack = taskData->stack;
    // Whenever the ML code enters a function it checks that the stack pointer is above
    // this value.  The default is to set it to the top of the reserved area
    // but if we've had an interrupt we set it to the end of the stack.
    // InterruptCode may be called either when the thread is in the RTS or in ML code.
    mdTask->memRegisters.stackTop = taskData->stack->Offset(taskData->stack->Length() - 1);
    if (taskData->pendingInterrupt)
        mdTask->memRegisters.stackLimit = taskData->stack->Offset(taskData->stack->Length()-1);
    else mdTask->memRegisters.stackLimit = taskData->stack->Offset(taskData->stack->p_space);
    mdTask->memRegisters.handlerRegister = taskData->stack->p_hr;
    mdTask->memRegisters.requestCode = 0; // Clear these because only one will be set.
    mdTask->memRegisters.returnReason = RETURN_IO_CALL;

    // Point "raiseException" at the assembly code for "raisex"
    mdTask->memRegisters.raiseException = (byte*)raisex;
    // Entry point to save the state for an IO call.  This is the common entry
    // point for all the return and IO-call cases.
    mdTask->memRegisters.ioEntry = (byte*)X86AsmSaveStateAndReturn;
    mdTask->memRegisters.heapOverflow = heapOverflow;
    mdTask->memRegisters.stackOverflow = stackOverflow;
    mdTask->memRegisters.stackOverflowEx = stackOverflowEx;
    mdTask->memRegisters.raiseDiv = raiseDiv;
    mdTask->memRegisters.arbEmulation = arbEmulation;

    mdTask->memRegisters.threadId = taskData->threadObject;
 
    // We set the PC to zero to indicate that we should retry the call to the RTS
    // function.  In that case we need to set it back to the code address before we
    // return.  This is also used if we have raised an exception.
    if (PSP_IC(taskData->stack) == PC_RETRY_SPECIAL)
        taskData->stack->p_pc = PSP_EDX(taskData->stack).AsObjPtr()->Get(0).AsCodePtr();
}

// This is called whenever we have returned from ML to C.
void X86Dependent::SaveMemRegisters(TaskData *taskData)
{
    X86TaskData *mdTask = (X86TaskData*)taskData->mdTaskData;
    // Check a few items on the stack to see it hasn't been overwritten
    if (! taskData->stack->IsStackObject() || taskData->stack->p_space != OVERFLOW_STACK_SIZE ||
          taskData->stack->p_nreg != CHECKED_REGS || taskData->stack->p_reg[CHECKED_REGS] != PolyWord::FromUnsigned(UNCHECKED_REGS))
        Crash("Stack overwritten\n");
    taskData->allocPointer = mdTask->memRegisters.localMpointer - 1;
    taskData->stack->p_hr = mdTask->memRegisters.handlerRegister;
    mdTask->allocWords = 0;
}

// Called if we need the ML code to retry an RTS call.
void X86Dependent::SetForRetry(TaskData *taskData, int ioCall)
{
    /* We now have to set the closure entry for the RTS call to work.
       DCJM 4/1/01. */
    PSP_EDX(taskData->stack) = (PolyObject*)IoEntry(ioCall);
    taskData->stack->p_pc = PC_RETRY_SPECIAL; // This value is treated specially in SetMemRegisters
}

PolyWord *X86Dependent::get_reg(TaskData *taskData, int n)
/* Returns a pointer to the register given by n. */
{
  switch (n) 
    {
      case 0: return &PSP_EAX(taskData->stack);
      case 1: return &PSP_ECX(taskData->stack);
      case 2: return &PSP_EDX(taskData->stack);
      case 3: return &PSP_EBX(taskData->stack);
      case 4: return (PolyWord*)&taskData->stack->p_sp;
      case 6: return &PSP_ESI(taskData->stack);
      case 7: return &PSP_EDI(taskData->stack);
#ifdef HOSTARCHITECTURE_X86_64
      case 8: return &PSP_R8(taskData->stack);
      case 9: return &PSP_R9(taskData->stack);
      case 10: return &PSP_R10(taskData->stack);
      case 11: return &PSP_R11(taskData->stack);
      case 12: return &PSP_R12(taskData->stack);
      case 13: return &PSP_R13(taskData->stack);
      case 14: return &PSP_R14(taskData->stack);
      // R15 is the heap pointer so shouldn't occur here.
#endif /* HOSTARCHITECTURE_X86_64 */
      default: 
        Crash("Unknown register %d at %p\n", n, PSP_IC(taskData->stack));
    }
}

// Called as a result of a heap overflow trap
void X86Dependent::HeapOverflowTrap(TaskData *taskData)
{
    X86TaskData *mdTask = (X86TaskData*)taskData->mdTaskData;
    POLYUNSIGNED wordsNeeded = 0;
    // The next instruction, after any branches round forwarding pointers, will
    // be a store of register containing the adjusted heap pointer.  We need to
    // find that register and the value in it in order to find out how big the
    // area we actually wanted is.
    while (PSP_IC(taskData->stack)[0] == 0xeb)
    {
        if (PSP_IC(taskData->stack)[1] >= 128)
            PSP_IC(taskData->stack) += 256 - PSP_IC(taskData->stack)[1] + 2;
        else PSP_IC(taskData->stack) += PSP_IC(taskData->stack)[1] + 2;
    }
#ifndef HOSTARCHITECTURE_X86_64
    // This should be movl REG,0[%ebp].
    ASSERT(PSP_IC(taskData->stack)[0] == 0x89);
    mdTask->allocReg = (PSP_IC(taskData->stack)[1] >> 3) & 7; // Remember this until we allocate the memory
    PolyWord *reg = get_reg(taskData, mdTask->allocReg);
    PolyWord reg_val = *reg;
    // The space we need is the difference between this register
    // and the current value of newptr.
    // The +1 here is because memRegisters.localMpointer is A.M.pointer +1.  The reason
    // is that after the allocation we have the register pointing at the address we will
    // actually use.
    wordsNeeded = (taskData->allocPointer - (PolyWord*)reg_val.AsAddress()) + 1;
    *reg = TAGGED(0); // Clear this - it's not a valid address.
    /* length in words, including length word */

    ASSERT (wordsNeeded <= (1<<24)); /* Max object size including length/flag word is 2^24 words.  */
#else /* HOSTARCHITECTURE_X86_64 */
    // This should be movq Length,-8(%r15)
    ASSERT(PSP_IC(taskData->stack)[0] == 0x49 && PSP_IC(taskData->stack)[1] == 0xc7 && PSP_IC(taskData->stack)[2] == 0x47 && PSP_IC(taskData->stack)[3] == 0xf8);
    // The Length field should be in the next word.  N.B.  This assumes that
    // the length word < 2^31.
    ASSERT((PSP_IC(taskData->stack)[7] & 0x80) == 0); // Should not be negative
    for (unsigned i = 7; i >= 4; i--) wordsNeeded = (wordsNeeded << 8) | PSP_IC(taskData->stack)[i];
    wordsNeeded += 1; // That was the object size. We need to add one for the length word.
#endif /* HOSTARCHITECTURE_X86_64 */
    
    if (profileMode == kProfileStoreAllocation)
        add_count(taskData, PSP_IC(taskData->stack), PSP_SP(taskData->stack), wordsNeeded);

#ifdef HOSTARCHITECTURE_X86_64
    // On the X64 the value that ends up in allocSpace->pointer includes the
    // attempted allocation.  Add back the space we tried to allocate
    taskData->allocPointer += wordsNeeded;
#endif /* HOSTARCHITECTURE_X86_64 */

    mdTask->allocWords = wordsNeeded; // The actual allocation is done in SetMemRegisters.
}


/******************************************************************************/
/*                                                                            */
/*      do_compare - do a "long" comparison, setting the flags register       */
/*                                                                            */
/******************************************************************************/
void X86Dependent::do_compare(TaskData *taskData, PolyWord v1, PolyWord v2)
{
    Handle val1, val2;
    /* Must push these to the save vec.  A persistent store trap
       might cause a garbage collection and move the stack. */
    val1 = taskData->saveVec.push(v1);
    val2 = taskData->saveVec.push(v2);
    int r = compareLong(taskData, val2, val1);
    /* Clear the flags. */
    POLYUNSIGNED flags = PSP_EFLAGS(taskData->stack).AsUnsigned();
    flags &= -256;
    if (r == 0) flags |= 0x40;
    else if (r < 0) flags |= 0x80;
    PSP_EFLAGS(taskData->stack) = PolyWord::FromUnsigned(flags);
}

/******************************************************************************/
/*                                                                            */
/*      do_op - do a "long" operation, setting the destination register       */
/*                                                                            */
/******************************************************************************/
void X86Dependent::do_op(TaskData *taskData, int dest, PolyWord v1, PolyWord v2, Handle (*op)(TaskData *, Handle, Handle))
{
    Handle val1, val2, result;
    /* Must push these to the save vec.  A persistent store trap
       or a garbage collection might move the stack. */
    val1 = taskData->saveVec.push(v1);
    val2 = taskData->saveVec.push(v2);
    /* Clobber the destination which may have overflowed. */
    *(get_reg(taskData, dest)) = TAGGED(0);
    result = op (taskData, val2, val1);     /* N.B parameters are intentionally reversed */
    /* N.B. the stack may have moved so we must recompute get_reg(dest). */
    *(get_reg(taskData, dest)) = DEREFWORD(result);
}

// Emulate a long precision operation.
bool X86Dependent::emulate_instrs(TaskData *taskData)
{
    int src1 = -1, src2 = -1, dest = -1;
    bool doneSubtraction = false;
    while(1) {
        byte rexPrefix = 0;
#ifdef HOSTARCHITECTURE_X86_64
        // Get any REX prefix
        if (PSP_IC(taskData->stack)[0] >= 0x40 && PSP_IC(taskData->stack)[0] <= 0x4f)
        {
            rexPrefix = PSP_IC(taskData->stack)[0];
            PSP_INCR_PC(taskData->stack, 1);
        }
#endif /* HOSTARCHITECTURE_X86_64 */
        // Decode the register fields and include any REX bits
        int bbb = PSP_IC(taskData->stack)[1] & 7;
        if (rexPrefix & 0x1) bbb += 8;
        int rrr = (PSP_IC(taskData->stack)[1] >> 3) & 7;
        if (rexPrefix & 0x4) rrr += 8;


        switch (PSP_IC(taskData->stack)[0]) {
        case 0x03: /* add. */
            if ((PSP_IC(taskData->stack)[1] & 0xc0) != 0xc0)
                Crash("Expected register");
            if (dest != rrr)
                Crash("Expected same destination register.");
            src2 = bbb;
            do_op(taskData, dest, *(get_reg(taskData, src1)), *(get_reg(taskData, src2)), add_longc);
            PSP_INCR_PC(taskData->stack, 2);
            return true;

        case 0x2b: /* Subtraction. */
            if ((PSP_IC(taskData->stack)[1] & 0xc0) != 0xc0)
                Crash("Expected register");
            if (dest != rrr)
                Crash("Expected same destination register.");
            src2 = bbb;
            do_op(taskData, dest, *(get_reg(taskData, src1)), *(get_reg(taskData, src2)), sub_longc);
            PSP_INCR_PC(taskData->stack, 2);
            // The next instruction should be a lea to put on the tag.
            // The result is already tagged so we need to skip that.
            // Previously this removed the tag and returned but CheckRegion
            // didn't like this.
            doneSubtraction = true;
            break;

        case 0x3b: /* Compare. */
            if ((PSP_IC(taskData->stack)[1] & 0xc0) != 0xc0)
                Crash("Expected register");
            src1 = rrr;
            src2 = bbb;
            do_compare(taskData, *(get_reg(taskData, src1)), *(get_reg(taskData, src2)));
            PSP_INCR_PC(taskData->stack, 2);
            return true;

        case 0x8d: /* leal - Used to remove a tag before an add and multiply. */
            // Also used to put the tag on after a subtraction.
            if ((PSP_IC(taskData->stack)[1] & 7) == 4)
            { // R12 (and RSP but that isn't used here) have to be encoded with a SIB byte.
                ASSERT((PSP_IC(taskData->stack)[2] & 7) == 4); // Should be same register
                PSP_INCR_PC(taskData->stack, 1);
            }
            if (doneSubtraction)
            {
                PSP_INCR_PC(taskData->stack, 3);
                return true;
            }
            if (src1 == -1) src1 = bbb; else src2 = bbb;
            dest = rrr;
            ASSERT(PSP_IC(taskData->stack)[2] == 0xff);
            PSP_INCR_PC(taskData->stack, 3);
            break;

        case 0x89: /* movl: move source into dest. */
            if ((PSP_IC(taskData->stack)[1] & 0xc0) != 0xc0)
                 Crash("Can't move into store.");
            dest = bbb;
            if (src1 == -1) src1 = rrr; else src2 = rrr;
            PSP_INCR_PC(taskData->stack, 2);
                /* Next should be add-immediate. */
            break;

        case 0x83: { /* One byte immediate: Add, sub or compare. */
            int cval = PSP_IC(taskData->stack)[2];
            if (cval >= 128) cval -= 256;

            switch (PSP_IC(taskData->stack)[1] & (7 << 3)) // This is a code.  Ignore any REX override.
            {
              case (0 << 3): /* add */
                      {
                if (dest != bbb)
                    Crash("Expected same destination register.");
                /* immediate value is shifted, but hasn't had 1 added;
                   do this now before calling add_longc */
                do_op(taskData, dest, *(get_reg(taskData, src1)), PolyWord::FromSigned(cval+1), add_longc);
                break;
              }

              case (5 << 3): /* sub */
              {
                if (dest != bbb)

                    Crash("Expected same destination register.");
                /* immediate value is shifted, but hasn't had 1 added;
                   do this now before calling sub_longc */
                do_op(taskData, dest, *(get_reg(taskData, src1)), PolyWord::FromSigned(cval+1), sub_longc);
                break;
              }

              case (7 << 3): /* cmp */
                      {
                if ((PSP_IC(taskData->stack)[1] & 0xc0) != 0xc0)
                    Crash("Can't test with store.");
                src1 = bbb;

                /* immediate value is already tagged */
                do_compare(taskData, *(get_reg(taskData, src1)), PolyWord::FromSigned(cval));
                break;
              }

             default: Crash("Unknown instruction after overflow trap");
            }

            PSP_INCR_PC(taskData->stack, 3);
            return true;
            }

        case 0x81: { /* 4 byte immediate: Add, sub or compare. */
            int cval = PSP_IC(taskData->stack)[5];
            if (cval >= 128) cval -= 256;
            cval = cval*256 + PSP_IC(taskData->stack)[4];
            cval = cval*256 + PSP_IC(taskData->stack)[3];
            cval = cval*256 + PSP_IC(taskData->stack)[2];
            if ((PSP_IC(taskData->stack)[1] & 0xc0) != 0xc0)
                Crash("Expected register");

            switch (PSP_IC(taskData->stack)[1] & (7 << 3))
            {
              case (0 << 3): /* add */
              {
                if (dest != bbb)

                    Crash("Expected same destination register.");
                /* immediate value is shifted, but hasn't had 1 added;
                   do this now before calling add_longc */
                do_op(taskData, dest, *(get_reg(taskData, src1)), PolyWord::FromSigned(cval+1), add_longc);
                break;
              }
              case (5 << 3): /* sub */
              {
                if (dest != bbb)
                    Crash("Expected same destination register.");
                /* immediate value is shifted, but hasn't had 1 added;
                   do this now before calling sub_longc */
                do_op(taskData, dest, *(get_reg(taskData, src1)), PolyWord::FromSigned(cval+1), sub_longc);
                break;
              }

              case (7 << 3): /* cmp */
              {
                src1 = bbb;
                /* immediate value is already tagged */
                do_compare(taskData, *(get_reg(taskData, src1)), PolyWord::FromSigned(cval));
                break;
              }

             default: Crash("Unknown instruction after overflow trap");
            }

            PSP_INCR_PC(taskData->stack, 6);
            return true;
            }

        case 0xeb: /* jmp - used in branch forwarding. */
            /* While forwarded jumps are always positive we may use backward
               branches in the future. */
            if (PSP_IC(taskData->stack)[1] >= 128)
                PSP_INCR_PC(taskData->stack, 256 - PSP_IC(taskData->stack)[1] + 2);
            else PSP_INCR_PC(taskData->stack, PSP_IC(taskData->stack)[1] + 2);
            break;

        case 0x50: /* push eax - used before a multiply. */
#ifdef HOSTARCHITECTURE_X86_64
            ASSERT((rexPrefix & 1) == 0); // Check it's not r8
#endif /* HOSTARCHITECTURE_X86_64 */
            *(--PSP_SP(taskData->stack)) = PSP_EAX(taskData->stack);
            PSP_INCR_PC(taskData->stack, 1);
            break;

        case 0x52: /* push edx - used before a multiply. */
#ifdef HOSTARCHITECTURE_X86_64
            ASSERT((rexPrefix & 1) == 0); // Check it's not r10
#endif /* HOSTARCHITECTURE_X86_64 */
            *(--PSP_SP(taskData->stack)) = PSP_EDX(taskData->stack);
            PSP_INCR_PC(taskData->stack, 1);
            break;

        case 0xd1: /* Group1A - must be sar edx before a multiply. */
            if (PSP_IC(taskData->stack)[1] != 0xfa)
                Crash("Unknown instruction after overflow trap");
            PSP_INCR_PC(taskData->stack, 2);
            /* If we haven't moved anything into edx then edx must be
               one of the arguments. */
            if (src2 == -1) src2 = 2; /* edx. */
            break;

        case 0xf7: /* Multiply instruction. */
            if (PSP_IC(taskData->stack)[1] != 0xea)
                Crash("Unknown instruction after overflow trap");
            do_op(taskData, 0 /* eax */, *(get_reg(taskData, src1)), *(get_reg(taskData, src2)), mult_longc);
            /* Subtract one because the next instruction will tag it. */
            PSP_EAX(taskData->stack) = PolyWord::FromUnsigned(PSP_EAX(taskData->stack).AsUnsigned() - 1);
            PSP_INCR_PC(taskData->stack, 2);
            return true;

        default:
            Crash("Unknown instruction after overflow trap");
        }
    }
    return false;
}

void X86Dependent::ArbitraryPrecisionTrap(TaskData *taskData)
{
    // Arithmetic operation has overflowed or detected long values.
    if (profileMode == kProfileEmulation)
        add_count(taskData, PSP_IC(taskData->stack), PSP_SP(taskData->stack), 1);
    // Emulate the arbitrary precision instruction.
    if (! emulate_instrs(taskData))
        Crash("Arbitrary precision emulation fault at %x\n", PSP_IC(taskData->stack));
}

// These macros build small pieces of assembly code for each io call.
// The code simply sets the requestCode value and jumps to
// X86AsmSaveStateAndReturn.  The address of these code pieces is
// stored in iovec.  Values in iovec are never looked at with the
// garbage collector so that's safe.

// N.B.  The length of this code (7) is built into BuildKillSelf
// It's 7 bytes on both x86 and X86_64.
#define MAKE_CALL_SEQUENCE_BYTES     7

#ifndef __GNUC__
// Windows
#ifndef HOSTARCHITECTURE_X86_64

#define MAKE_IO_CALL_SEQUENCE(ioNum, result) \
{ \
    __asm call endCode##ioNum \
    __asm mov  byte ptr [20+ebp],ioNum \
    __asm jmp  dword ptr [48+ebp] \
    __asm endCode##ioNum: pop eax \
    __asm mov result,eax \
}

#define MAKE_EXTRA_CALL_SEQUENCE(exNum, result) \
{ \
    __asm call endCodeX##exNum \
    __asm mov  byte ptr [22+ebp],exNum \
    __asm jmp  dword ptr [48+ebp] \
    __asm endCodeX##exNum: pop eax \
    __asm mov result,eax \
}

#else /* HOSTARCHITECTURE_X86_64 */
// Visual C++ on X64 doesn't support inline assembly code
#endif /* HOSTARCHITECTURE_X86_64 */


#else

#ifndef HOSTARCHITECTURE_X86_64

#define MAKE_IO_CALL_SEQUENCE(ioNum, result) \
{ \
    __asm__ __volatile__ ( "call 1f; " \
          "movb  %1,20(%%ebp); " \
          "jmp  *48(%%ebp); " \
           "1: popl %0" \
           :"=r"(result) \
           :"i"(ioNum) \
           ); \
}

#define MAKE_EXTRA_CALL_SEQUENCE(exNum, result) \
{ \
    __asm__ __volatile__ ( "call 1f; " \
          "movb  %1,22(%%ebp); " \
          "jmp  *48(%%ebp); " \
           "1: popl %0" \
           :"=r"(result) \
           :"i"(exNum) \
           ); \
}

#else /* HOSTARCHITECTURE_X86_64 */

#define MAKE_IO_CALL_SEQUENCE(ioNum, result) \
{ \
    __asm__ __volatile__ ( "call 1f; " \
          "movb  %1,40(%%rbp); " \
          "jmp  *96(%%rbp); " \
           "1: popq %0" \
           :"=r"(result) \
           :"i"(ioNum) \
           ); \
}

#define MAKE_EXTRA_CALL_SEQUENCE(exNum, result) \
{ \
    __asm__ __volatile__ ( "call 1f; " \
          "movb  %1,42(%%rbp); " \
          "jmp  *96(%%rbp); " \
           "1: popq %0" \
           :"=r"(result) \
           :"i"(exNum) \
           ); \
}
#endif /* HOSTARCHITECTURE_X86_64 */

#endif

static void add_function_to_io_area(int x, int (*y)())
{
    add_word_to_io_area(x, PolyWord::FromUnsigned((POLYUNSIGNED)y));
}

/******************************************************************************/
/*                                                                            */
/*      MD_init_interface_vector - called from run-time system                */
/*                                                                            */
/******************************************************************************/
void X86Dependent::InitInterfaceVector(void)
{
    unsigned char *codeAddr;
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_exit, codeAddr);
    add_word_to_io_area(POLY_SYS_exit, PolyWord::FromCodePtr(codeAddr));

    add_function_to_io_area(POLY_SYS_alloc_store, &alloc_store);

    MAKE_IO_CALL_SEQUENCE(POLY_SYS_chdir, codeAddr);
    add_word_to_io_area(POLY_SYS_chdir, PolyWord::FromCodePtr(codeAddr));

    add_function_to_io_area(POLY_SYS_get_length, &get_length_a);

    MAKE_IO_CALL_SEQUENCE(POLY_SYS_get_flags, codeAddr);
    add_word_to_io_area(POLY_SYS_get_flags, PolyWord::FromCodePtr(codeAddr));

    add_function_to_io_area(POLY_SYS_str_compare, str_compare);
    add_function_to_io_area(POLY_SYS_teststreq, &teststreq);
    add_function_to_io_area(POLY_SYS_teststrneq, &teststrneq);
    add_function_to_io_area(POLY_SYS_teststrgtr, &teststrgtr);
    add_function_to_io_area(POLY_SYS_teststrlss, &teststrlss);
    add_function_to_io_area(POLY_SYS_teststrgeq, &teststrgeq);
    add_function_to_io_area(POLY_SYS_teststrleq, &teststrleq);

    MAKE_IO_CALL_SEQUENCE(POLY_SYS_exception_trace, codeAddr);
    add_word_to_io_area(POLY_SYS_exception_trace, PolyWord::FromCodePtr(codeAddr));

    add_function_to_io_area(POLY_SYS_lockseg, &locksega);

    MAKE_IO_CALL_SEQUENCE(POLY_SYS_profiler, codeAddr);
    add_word_to_io_area(POLY_SYS_profiler, PolyWord::FromCodePtr(codeAddr));

    add_function_to_io_area(POLY_SYS_is_short, &is_shorta);
    add_function_to_io_area(POLY_SYS_aplus, &add_long); // Retain
    add_function_to_io_area(POLY_SYS_aminus, &sub_long);
    add_function_to_io_area(POLY_SYS_amul, &mult_long);
    add_function_to_io_area(POLY_SYS_adiv, &div_long);
    add_function_to_io_area(POLY_SYS_amod, &rem_long);
    add_function_to_io_area(POLY_SYS_aneg, &neg_long);
    add_function_to_io_area(POLY_SYS_equala, &equal_long);
    add_function_to_io_area(POLY_SYS_ora, &or_long);
    add_function_to_io_area(POLY_SYS_anda, &and_long);
    add_function_to_io_area(POLY_SYS_xora, &xor_long);

    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Real_str, codeAddr);
    add_word_to_io_area(POLY_SYS_Real_str, PolyWord::FromCodePtr(codeAddr));

    add_function_to_io_area(POLY_SYS_Real_geq, real_geq);
    add_function_to_io_area(POLY_SYS_Real_leq, real_leq);
    add_function_to_io_area(POLY_SYS_Real_gtr, real_gtr);
    add_function_to_io_area(POLY_SYS_Real_lss, real_lss);
    add_function_to_io_area(POLY_SYS_Real_eq,  real_eq);
    add_function_to_io_area(POLY_SYS_Real_neq, real_neq);

    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Real_Dispatch, codeAddr);
    add_word_to_io_area(POLY_SYS_Real_Dispatch, PolyWord::FromCodePtr(codeAddr));

    add_function_to_io_area(POLY_SYS_Add_real, real_add);
    add_function_to_io_area(POLY_SYS_Sub_real, real_sub);
    add_function_to_io_area(POLY_SYS_Mul_real, real_mul);
    add_function_to_io_area(POLY_SYS_Div_real, real_div);
    add_function_to_io_area(POLY_SYS_Neg_real, real_neg);

    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Repr_real, codeAddr);
    add_word_to_io_area(POLY_SYS_Repr_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_conv_real, codeAddr);
    add_word_to_io_area(POLY_SYS_conv_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_real_to_int, codeAddr);
    add_word_to_io_area(POLY_SYS_real_to_int, PolyWord::FromCodePtr(codeAddr));

    add_function_to_io_area(POLY_SYS_int_to_real, real_from_int);

    MAKE_IO_CALL_SEQUENCE(POLY_SYS_sqrt_real, codeAddr);
    add_word_to_io_area(POLY_SYS_sqrt_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_sin_real, codeAddr);
    add_word_to_io_area(POLY_SYS_sin_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_cos_real, codeAddr);
    add_word_to_io_area(POLY_SYS_cos_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_arctan_real, codeAddr);
    add_word_to_io_area(POLY_SYS_arctan_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_exp_real, codeAddr);
    add_word_to_io_area(POLY_SYS_exp_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_ln_real, codeAddr);
    add_word_to_io_area(POLY_SYS_ln_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_io_operation, codeAddr);
    add_word_to_io_area(POLY_SYS_io_operation, PolyWord::FromCodePtr(codeAddr));

    add_function_to_io_area(POLY_SYS_atomic_incr, &atomic_increment);
    add_function_to_io_area(POLY_SYS_atomic_decr, &atomic_decrement);
    add_function_to_io_area(POLY_SYS_thread_self, &thread_self);

    MAKE_IO_CALL_SEQUENCE(POLY_SYS_thread_dispatch, codeAddr);
    add_word_to_io_area(POLY_SYS_thread_dispatch, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_kill_self, codeAddr);
    add_word_to_io_area(POLY_SYS_kill_self, PolyWord::FromCodePtr(codeAddr));
    
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
    
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_objsize, codeAddr);
    add_word_to_io_area(POLY_SYS_objsize, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_showsize, codeAddr);
    add_word_to_io_area(POLY_SYS_showsize, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_timing_dispatch, codeAddr);
    add_word_to_io_area(POLY_SYS_timing_dispatch, PolyWord::FromCodePtr(codeAddr));
    
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_XWindows, codeAddr);
    add_word_to_io_area(POLY_SYS_XWindows, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_full_gc, codeAddr);
    add_word_to_io_area(POLY_SYS_full_gc, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_stack_trace, codeAddr);
    add_word_to_io_area(POLY_SYS_stack_trace, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_foreign_dispatch, codeAddr);
    add_word_to_io_area(POLY_SYS_foreign_dispatch, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_callcode_tupled, codeAddr);
    add_word_to_io_area(POLY_SYS_callcode_tupled, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_process_env, codeAddr);
    add_word_to_io_area(POLY_SYS_process_env, PolyWord::FromCodePtr(codeAddr));

    add_function_to_io_area(POLY_SYS_set_string_length, &set_string_length_a); /* DCJM 28/2/01 */
    add_function_to_io_area(POLY_SYS_get_first_long_word, &get_first_long_word_a); /* DCJM 28/2/01 */
    
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_shrink_stack, codeAddr);
    add_word_to_io_area(POLY_SYS_shrink_stack, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_code_flags, codeAddr);
    add_word_to_io_area(POLY_SYS_code_flags, PolyWord::FromCodePtr(codeAddr));
    
    add_function_to_io_area(POLY_SYS_shift_right_arith_word, &shift_right_arith_word); /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_int_to_word,      &int_to_word);        /* DCJM 10/10/99 */

    MAKE_IO_CALL_SEQUENCE(POLY_SYS_set_code_constant, codeAddr); /* DCJM 2/1/01 */
    add_word_to_io_area(POLY_SYS_set_code_constant, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_poly_specific, codeAddr); /* DCJM 19/6/06 */
    add_word_to_io_area(POLY_SYS_poly_specific, PolyWord::FromCodePtr(codeAddr));

    add_function_to_io_area(POLY_SYS_move_bytes,       &move_bytes);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_move_words,       &move_words);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_mul_word,         &mul_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_plus_word,        &plus_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_minus_word,       &minus_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_div_word,         &div_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_mod_word,         &mod_word);        /* DCJM 10/10/99 */
    add_function_to_io_area(POLY_SYS_word_geq,         &word_geq);
    add_function_to_io_area(POLY_SYS_word_leq,         &word_leq);
    add_function_to_io_area(POLY_SYS_word_gtr,         &word_gtr);
    add_function_to_io_area(POLY_SYS_word_lss,         &word_lss);

    // This used to contain the code itself.  Now this is set up as a "closure"
    // but is only used for exceptions raised within the RTS.
    add_function_to_io_area(POLY_SYS_raisex,           &raisex);
    
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_io_dispatch, codeAddr);
    add_word_to_io_area(POLY_SYS_io_dispatch, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_network, codeAddr);
    add_word_to_io_area(POLY_SYS_network, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_os_specific, codeAddr);
    add_word_to_io_area(POLY_SYS_os_specific, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_signal_handler, codeAddr);
    add_word_to_io_area(POLY_SYS_signal_handler, PolyWord::FromCodePtr(codeAddr));

    // Entries for special cases.  These are generally, but not always, called from
    // compiled code.
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_HEAP_OVERFLOW, heapOverflow);
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_STACK_OVERFLOW, stackOverflow);
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_STACK_OVERFLOWEX, stackOverflowEx);
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_RAISE_DIV, raiseDiv);
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_ARB_EMULATION, arbEmulation);
}

// We need the kill-self code in a little function.
Handle X86Dependent::BuildKillSelf(TaskData *taskData)
{
    byte *codeAddr;
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_kill_self, codeAddr);
    return BuildCodeSegment(taskData, codeAddr, MAKE_CALL_SEQUENCE_BYTES, 'K');
}

void X86Dependent::SetException(TaskData *taskData, poly_exn *exc)
// Set up the stack of a process to raise an exception.
{
    PSP_EDX(taskData->stack) = (PolyObject*)IoEntry(POLY_SYS_raisex);
    PSP_IC(taskData->stack)     = PC_RETRY_SPECIAL;
    PSP_EAX(taskData->stack) = exc; /* put exception data into eax */
}

void X86Dependent::ResetSignals(void)
{
    /* restore default signal handling. */
    /* SIGILL, SIGEMT are not used in PC version */
    signal(SIGFPE,  SIG_DFL);
}

// Call a piece of compiled code.
void X86Dependent::CallCodeTupled(TaskData *taskData)
{
    // The eventual return address is on the stack - leave it there.
    PolyObject *argTuple = PSP_EAX(taskData->stack).AsObjPtr();
    Handle closure = taskData->saveVec.push(argTuple->Get(0));
    Handle argvec = taskData->saveVec.push(argTuple->Get(1));

    if (! IS_INT(DEREFWORD(argvec))) // May be nil if there are no args.
    {
        PolyObject *argv = DEREFHANDLE(argvec);
        POLYUNSIGNED argCount = argv->Length();
        // Check we have space for the arguments.  This may result in a GC which
        // in turn may throw a C++ exception.
        if (argCount > ARGS_IN_REGS)
        {
            try {
                CheckAndGrowStack(taskData, taskData->stack->p_sp - (argCount - ARGS_IN_REGS));
            }
            catch (IOException)
            {
                return; // Will have been set up to raise an exception.
            }
        }

        // First argument is in EAX
        PSP_EAX(taskData->stack) = argv->Get(0);
        // Second arg, if there is one, goes into EBX
        if (argCount > 1)
            PSP_EBX(taskData->stack) = argv->Get(1);
#ifdef HOSTARCHITECTURE_X86_64
        if (argCount > 2)
            PSP_R8(taskData->stack) = argv->Get(2);
        if (argCount > 3)
            PSP_R9(taskData->stack) = argv->Get(3);
        if (argCount > 4)
            PSP_R10(taskData->stack) = argv->Get(4);
#endif /* HOSTARCHITECTURE_X86_64 */
        // Remaining args go on the stack.
        PolyWord returnAddress = *PSP_SP(taskData->stack)++;
        for (POLYUNSIGNED i = ARGS_IN_REGS; i < argCount; i++)
        {
            *(--PSP_SP(taskData->stack)) = argv->Get(i);
        }
        *(--PSP_SP(taskData->stack)) = returnAddress;
    }
    // The closure goes into the closure reg.
    PSP_EDX(taskData->stack) = DEREFWORD(closure);
    // First word of closure is entry point.
    PSP_IC(taskData->stack) = (PSP_EDX(taskData->stack)).AsObjPtr()->Get(0).AsCodePtr();
}

// Sets up a callback function on the current stack.  The present state is that
// the ML code has made a call in to foreign_dispatch.  We need to set the stack
// up so that we will enter the callback (as with CallCodeTupled) but when we return
// the result we enter callback_return. 
void X86Dependent::SetCallbackFunction(TaskData *taskData, Handle func, Handle args)
{
    byte *codeAddr1, *codeAddr2;
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_CALLBACK_RETURN, codeAddr1);
    Handle callBackReturn = BuildCodeSegment(taskData, codeAddr1, MAKE_CALL_SEQUENCE_BYTES, 'C');
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_CALLBACK_EXCEPTION, codeAddr2);
    Handle callBackException = BuildCodeSegment(taskData, codeAddr2, MAKE_CALL_SEQUENCE_BYTES, 'X');
    // Save the closure pointer and argument registers to the stack.  If we have to
    // retry the current RTS call we need these to have their original values.
    *(--PSP_SP(taskData->stack)) = PSP_EDX(taskData->stack);
    *(--PSP_SP(taskData->stack)) = PSP_EAX(taskData->stack);
    *(--PSP_SP(taskData->stack)) = PSP_EBX(taskData->stack);
#ifdef HOSTARCHITECTURE_X86_64
    *(--PSP_SP(taskData->stack)) = PSP_R8(taskData->stack);
    *(--PSP_SP(taskData->stack)) = PSP_R9(taskData->stack);
    *(--PSP_SP(taskData->stack)) = PSP_R10(taskData->stack);
#endif
    // Set up an exception handler so we will enter callBackException if there is an exception.
    *(--PSP_SP(taskData->stack)) = PolyWord::FromStackAddr(taskData->stack->p_hr); // Create a special handler entry
    *(--PSP_SP(taskData->stack)) = callBackException->Word();
    *(--PSP_SP(taskData->stack)) = TAGGED(0);
    taskData->stack->p_hr = PSP_SP(taskData->stack);
    // Push the call to callBackReturn onto the stack as the return address.
    *(--PSP_SP(taskData->stack)) = callBackReturn->Word();
    // Set up the entry point of the callback.
    PolyObject *functToCall = func->WordP();
    PSP_EDX(taskData->stack) = functToCall; // Closure address
    PSP_EAX(taskData->stack) = args->Word();
    taskData->stack->p_pc = functToCall->Get(0).AsCodePtr(); // First word of closure is entry pt.
}

static void skipea(byte **pt, ScanAddress *process)
{
    unsigned int modrm = *((*pt)++);
    unsigned int md = modrm >> 6;
    unsigned int rm = modrm & 7;

    if (md == 3) { } /* Register. */
    else if (rm == 4)
    {
        /* s-i-b present. */
        unsigned int sib = *((*pt)++);

        if (md == 0)
        {
            if ((sib & 7) == 5) 
            {
                /* An immediate address. */
#ifndef HOSTARCHITECTURE_X86_64
                process->ScanConstant(*pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
                (*pt) += 4;
            }
        }
        else if (md == 1) (*pt)++;
        else if (md == 2) (*pt) += 4;
    }
    else if (md == 0 && rm == 5)
    {
#ifndef HOSTARCHITECTURE_X86_64
        /* Absolute address. */
        process->ScanConstant(*pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
        *pt += 4;
    }
    else
    {
        if (md == 1) *pt += 1;
        else if (md == 2) *pt += 4;
    }
}

/* Added to deal with constants within the
   code rather than in the constant area.  The constant
   area is still needed for the function name.
   DCJM 2/1/2001 
*/
void X86Dependent::ScanConstantsWithinCode(PolyObject *addr, PolyObject *old, POLYUNSIGNED length, ScanAddress *process)
{
    byte *pt = (byte*)addr;
    PolyWord *end;
    POLYUNSIGNED unused;
    /* Find the end of the code (before the constants). */
    addr->GetConstSegmentForCode(length, end, unused);
    end -= 3;
    assert(end->AsUnsigned() == 0); /* This should be the marker word. */

    while (pt != (byte*)end)
    {
#ifdef HOSTARCHITECTURE_X86_64
        // REX prefixes.  Set this first.
        byte lastRex;
        if (*pt >= 0x40 && *pt <= 0x4f)
            lastRex = *pt++;
        else
            lastRex = 0;

        //printf("pt=%p *pt=%x\n", pt, *pt);

#endif /* HOSTARCHITECTURE_X86_64 */
        switch (*pt)
        {
        case 0x50: case 0x51: case 0x52: case 0x53:
        case 0x54: case 0x55: case 0x56: case 0x57: /* Push */
        case 0x58: case 0x59: case 0x5a: case 0x5b:
        case 0x5c: case 0x5d: case 0x5e: case 0x5f: /* Pop */
        case 0x90: /* nop */ case 0xc3: /* ret */
        case 0xf9: /* stc */ case 0xce: /* into */
            pt += 1; break;

        case 0x70: case 0x72: case 0x73: case 0x74: case 0x75: case 0x76:
        case 0x77: case 0x7c: case 0x7d: case 0x7e: case 0x7f: case 0xeb:
            /* short jumps. */
        case 0xcd: /* INT */
        case 0xa8: /* TEST_ACC8 */
        case 0x6a: /* PUSH_8 */
            pt += 2; break;

        case 0xc2: /* RET_16 */
            pt += 3; break;

        case 0x03: case 0x0b: case 0x13: case 0x1b:
        case 0x23: case 0x2b: case 0x33: case 0x3b: /* Add r,ea etc. */
        case 0x88: /* MOVB_R_A */ case 0x89: /* MOVL_R_A */
        case 0x8b: /* MOVL_A_R */
        case 0x62: /* BOUNDL */
        case 0xff: /* Group5 */
        case 0xd1: /* Group2_1_A */
        case 0x8f: /* POP_A */
        case 0x8d: /* leal. */
            pt++; skipea(&pt, process); break;

        case 0xf6: /* Group3_a */
            {
                int isTest = 0;
                pt++;
                /* The test instruction has an immediate operand. */
                if ((*pt & 0x38) == 0) isTest = 1;
                skipea(&pt, process);
                if (isTest) pt++;
                break;
            }

        case 0xf7: /* Group3_A */
            {
                int isTest = 0;
                pt++;
                /* The test instruction has an immediate operand. */
                if ((*pt & 0x38) == 0) isTest = 1;
                skipea(&pt, process);
                if (isTest) pt += 4;
                break;
            }

        case 0xc1: /* Group2_8_A */
        case 0xc6: /* MOVB_8_A */
        case 0x83: /* Group1_8_A */
            pt++; skipea(&pt, process); pt++; break;

        case 0x81: /* Group1_32_A */
            {
                pt ++;
                skipea(&pt, process);
                /* Ignore the 32 bit constant here.  It may be
                   untagged and it shouldn't be an address. */
                pt += 4;
                break;
            }

        case 0xe8: case 0xe9:
            // Long jump and call.  These are used to call constant (known) functions
            // and also long jumps within the function.
            {
                pt++;
                POLYSIGNED disp = (pt[3] & 0x80) ? -1 : 0; // Set the sign just in case.
                for(unsigned i = 4; i > 0; i--)
                    disp = (disp << 8) | pt[i-1];
                byte *absAddr = pt + disp + 4; // The address is relative to AFTER the constant

                // If the new address is within the current piece of code we don't do anything
                if (absAddr >= (byte*)addr && absAddr < (byte*)end) {}
                else {
#ifdef HOSTARCHITECTURE_X86_64
                    ASSERT(sizeof(PolyWord) == 4); // Should only be used internally on x64
#endif /* HOSTARCHITECTURE_X86_64 */
                    if (addr != old)
                    {
                        // The old value of the displacement was relative to the old address before
                        // we copied this code segment.
                        // We have to correct it back to the original address.
                        absAddr = absAddr - (byte*)addr + (byte*)old;
                        // We have to correct the displacement for the new location and store
                        // that away before we call ScanConstant.
                        POLYSIGNED newDisp = absAddr - pt - 4;
                        for (unsigned i = 0; i < 4; i++)
                        {
                            pt[i] = (byte)(newDisp & 0xff);
                            newDisp >>= 8;
                        }
                    }
                    process->ScanConstant(pt, PROCESS_RELOC_I386RELATIVE);
                }
                pt += 4;
                break;
            }

        case 0xc7:/* MOVL_32_A */
            {
                pt++;
                if ((*pt & 0xc0) == 0x40 /* Byte offset or sib present */ &&
                    ((*pt & 7) != 4) /* But not sib present */ && pt[1] == 256-sizeof(PolyWord))
                {
                    /* We may use a move instruction to set the length
                       word on a new segment.  We mustn't try to treat this as a constant.  */
                    pt += 6; /* Skip the modrm byte, the offset and the constant. */
                }
                else
                {
                    skipea(&pt, process);
#ifndef HOSTARCHITECTURE_X86_64
                    process->ScanConstant(pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
                    pt += 4;
                }
                break;
            }

        case 0xb8: case 0xb9: case 0xba: case 0xbb:
        case 0xbc: case 0xbd: case 0xbe: case 0xbf: /* MOVL_32_64_R */
            pt ++;
#ifdef HOSTARCHITECTURE_X86_64
            if ((lastRex & 8) == 0)
                pt += 4; // 32-bit mode on 64-bits.  Can this occur?
            else
#endif /* HOSTARCHITECTURE_X86_64 */
            {
                // 32 bits in 32-bit mode, 64-bits in 64-bit mode.
                process->ScanConstant(pt, PROCESS_RELOC_DIRECT);
                pt += sizeof(PolyWord);
            }
            break;

        case 0x68: /* PUSH_32 */
            pt ++;
#ifndef HOSTARCHITECTURE_X86_64
            process->ScanConstant(pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
            pt += 4;
            break;

        case 0x0f: /* ESCAPE */
            {
                pt++;
                switch (*pt)
                {
                case 0xb6: /* movzl */
                    pt++; skipea(&pt, process); break;

                case 0x80: case 0x81: case 0x82: case 0x83:
                case 0x84: case 0x85: case 0x86: case 0x87:
                case 0x88: case 0x89: case 0x8a: case 0x8b:
                case 0x8c: case 0x8d: case 0x8e: case 0x8f:
                    /* Conditional branches with 32-bit displacement. */
                    pt += 5; break;

                default: Crash("Unknown opcode %d at %p\n", *pt, pt);
                }
                break;
            }

        default: Crash("Unknown opcode %d at %p\n", *pt, pt);
        }
    }
}

extern "C" int registerMaskVector[];

int X86Dependent::GetIOFunctionRegisterMask(int ioCall)
{
    return registerMaskVector[ioCall];
}

// Store a constant in the code segment.  This has to be handled specially because
// the constant is probably an address.
// At the moment this assumes we're dealing with a 32-bit constant on a 32-bit machine
// and a 64-bit constant on a 64-bit machine.
void X86Dependent::SetCodeConstant(TaskData *taskData, Handle data, Handle constant, Handle offseth, Handle base)
{
    POLYUNSIGNED offset = get_C_ulong(taskData, DEREFWORD(offseth)); // Byte offset
    byte *pointer = DEREFWORD(base).AsCodePtr() + offset;
    POLYUNSIGNED flags = UNTAGGED(DEREFWORD(data));
    POLYUNSIGNED c = DEREFWORD(constant).AsUnsigned(); // N.B.  This may well really be an address.
    if (flags == 1)
        c -= (POLYUNSIGNED)(pointer + sizeof(PolyWord)); // Relative address.  Relative to AFTER the pointer.
    // Store the value into the code.  It can be on an arbitrary alignment.
    for (unsigned i = 0; i < sizeof(PolyWord); i++)
    {
        pointer[i] = (byte)(c & 255); 
        c >>= 8;
    }
}

#ifndef HOSTARCHITECTURE_X86_64
// This will only work on the i386.  The X86_64 uses different conventions with some arguments
// in registers.

/* We have to compile the callback function dynamically.  This code mallocs the store for it.
   At least at the moment, the store is never freed.  If we decide to garbage collect it
   we could store it in a vol. */
unsigned char *X86Dependent::BuildCallback(TaskData *taskData, int cbEntryNo, Handle cResultType, int nArgsToRemove)
{
    int max_callback_size = 36; /* Sufficient for the largest callback (actually 33 I think).*/
    unsigned char *result = (unsigned char*)malloc(max_callback_size);
    /* TODO: This does not allocate memory with execute permissions and that could cause problems
       in newer operating systems such as Windows XP SP2. DCJM 19/8/04. */
    unsigned char *p = result;
    long cbAddr = (long)&CCallbackFunction;
    /* This code creates a variable on the stack which is initialised to point to the first arg,
       then calls "CCallbackFunction" with the address of this variable and the callback reference number.
       When "CCallbackFunction" returns with the ADDRESS of the result this code extracts the result in
       the appropriate form and returns with it. */
    // We only need one word on the stack but Mac OS X requires the stack to be aligned
    // on a 16-byte boundary,  With the return address, the saved EBP pushed by ENTER and
    // the two words we push as arguments that means we need 16 bytes here.
    *p++ = 0xC8;    /* enter 16, 0 */
    *p++ = 0x10;
    *p++ = 0x00;
    *p++ = 0x00;
    *p++ = 0x8D;    /* lea eax,[ebp+8] */ /* Address of first arg. */
    *p++ = 0x45;
    *p++ = 0x08;
    *p++ = 0x89;    /* mov dword ptr [ebp-4],eax */ /* Store it in the variable. */
    *p++ = 0x45;
    *p++ = 0xFC;
    *p++ = 0x8D;    /* lea ecx,[ebp-4] */ /* Get the address of the variable. */
    *p++ = 0x4D;
    *p++ = 0xFC;
    *p++ = 0x51;    /* push ecx */
    *p++ = 0x68;    /* push cbEntryNo */
    *p++ = cbEntryNo & 0xff;
    cbEntryNo = cbEntryNo >> 8;
    *p++ = cbEntryNo & 0xff;
    cbEntryNo = cbEntryNo >> 8;
    *p++ = cbEntryNo & 0xff;
    cbEntryNo = cbEntryNo >> 8;
    *p++ = cbEntryNo & 0xff;
    /* The call is PC relative so we have to subtract the address of the END of the call instruction. */
    cbAddr -= (long)p + 5; /* The instruction is 5 bytes long. */
    *p++ = 0xE8;    /* call cbAddr */
    *p++ = cbAddr & 0xff;
    cbAddr = cbAddr >> 8;
    *p++ = cbAddr & 0xff;
    cbAddr = cbAddr >> 8;
    *p++ = cbAddr & 0xff;
    cbAddr = cbAddr >> 8;
    *p++ = cbAddr & 0xff;
    *p++ = 0x83;    /* add esp,8 */ /* Probably not needed since we're about to leave. */
    *p++ = 0xC4;
    *p++ = 0x08;
    /* Put in the return sequence.  eax points to the C_pointer for the result. */
    if (! IS_INT(DEREFWORD(cResultType))) {
        /* We might be able to get this to work but it's probably too much effort. */
        raise_exception_string(taskData, EXC_foreign, "Structure results from callbacks are not supported\n");
    }
    else {
        switch (UNTAGGED(DEREFWORD(cResultType))) {
        case Cchar: /* movsbl eax, [eax] */
            *p++ = 0x0f;
            *p++ = 0xbe;
            *p++ = 0x00;
            break;
        case Cshort: /* movswl eax, [eax] */
            *p++ = 0x0f;
            *p++ = 0xbf;
            *p++ = 0x00;
            break;
        case Cfloat: /* flds [eax] */
            *p++ = 0xD9;
            *p++ = 0x00;
            break;
        case Cdouble: /* fldl [eax] */
            *p++ = 0xDD;
            *p++ = 0x00;
            break;
        case Cint: case Cuint: case Clong: case Cpointer:
            *p++ = 0x8B;    /* mov eax,dword ptr [eax] */
            *p++ = 0x00;
            break;
        default: Crash("Unknown C type"); /* This shouldn't happen */
        }
    }
    *p++ = 0xC9;    /* leave */
    // We use a simple "ret" instruction if this is a C function and "ret n" if it's Pascal.
    if (nArgsToRemove == 0) *p++ = 0xC3; /* Usual case for C functions */
    else { /* Pascal calling convention - remove arguments. */
        *p++ = 0xC2;    /* ret n */
        *p++ = nArgsToRemove & 0xff;
        nArgsToRemove = nArgsToRemove >> 8;
        *p++ = nArgsToRemove & 0xff;
    }
    ASSERT(p - result <= max_callback_size);
    return result;
}

/* This function retrieves the callback arguments.  How the arguments to the
   function are stored is likely to be machine-dependent since some arguments are
   likely to be passed in registers and others on the stack. On the i386 it's
   easy - all arguments are on the stack. */
void X86Dependent::GetCallbackArg(void **args, void *argLoc, int nSize)
{
    /* nSize is the size of the result when we copy it into the vol.  char and short
       arguments are always expanded to int.  This code will work for little-endians
       but NOT for big-endian machines. */
    memcpy(argLoc, *args, nSize);
    /* Go on to the next argument. */
    nSize += sizeof(int)-1;
    nSize &= -(int)sizeof(int);
    *args = (void*) (*(char**)args + nSize);
}
#endif

// Atomic addition.  Returns the new value 
static POLYUNSIGNED AtomicAdd(PolyObject *p, POLYUNSIGNED toAdd)
{
    POLYUNSIGNED result;
#ifdef HOSTARCHITECTURE_X86_64
#ifdef __GNUC__
// Unix - x64
    __asm__ __volatile__ (
        "lock xaddq %1,(%2) " // Do atomic addition
        : "=r" (result) // Output
        : "0" (toAdd), "r" (p) // Input
        : "cc", "memory" // Modifies cc and memory
        );
#else
// Visual C++ on X64 doesn't support inline assembly code
#endif // ! __GNUC__
//
#else // ! HOSTARCHITECTURE_X86_64
#ifdef __GNUC__
// Unix - x32
    __asm__ __volatile__ (
        "lock; xaddl %1,(%2) " // Do atomic addition
        : "=r" (result) // Output
        : "0" (toAdd), "r" (p) // Input
        : "cc", "memory" // Modifies cc and memory
        );
#else
// Windows
    __asm {
        mov ebx,toAdd
        mov eax,p
        lock xadd [eax],ebx
        mov result,ebx
    }
#endif // ! __GNUC__
#endif

    return result + toAdd; // Adjust to the new value.
}

Handle X86Dependent::AtomicIncrement(TaskData *taskData, Handle mutexp)
{
    PolyObject *p = DEREFHANDLE(mutexp);
    POLYUNSIGNED result = AtomicAdd(p, 1 << POLY_TAGSHIFT);
    return taskData->saveVec.push(PolyWord::FromUnsigned(result));
}

// Decrement the value contained in the first word of the mutex.
Handle X86Dependent::AtomicDecrement(TaskData *taskData, Handle mutexp)
{
    PolyObject *p = DEREFHANDLE(mutexp);
    POLYUNSIGNED result = AtomicAdd(p, (-1) << POLY_TAGSHIFT);
    return taskData->saveVec.push(PolyWord::FromUnsigned(result));
}


static X86Dependent x86Dependent;

MachineDependent *machineDependent = &x86Dependent;
