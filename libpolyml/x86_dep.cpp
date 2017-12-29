/*
    Title:  Machine dependent code for i386 and X64 under Windows and Unix

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

    Further work copyright David C. J. Matthews 2011-17

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.

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
#elif defined(_WIN32)
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

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#if (defined(_WIN32))
#include <windows.h>
#include <excpt.h>
#endif

#include "globals.h"
#include "run_time.h"
#include "diagnostics.h"
#include "processes.h"
#include "profiling.h"
#include "machine_dep.h"
#include "scanaddrs.h"
#include "memmgr.h"
#include "rtsentry.h"

#include "sys.h" // Temporary


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

#ifdef HOSTARCHITECTURE_X86_64
struct fpSaveArea {
    double fpregister[7]; // Save area for xmm0-6
};
#else
// Structure of floating point save area.
// This is dictated by the hardware.
typedef byte fpregister[10];

struct fpSaveArea {
    unsigned short cw;
    unsigned short _unused0;
    unsigned short sw;
    unsigned short _unused1;
    unsigned short tw;
    unsigned short _unused2;
    unsigned fip;
    unsigned short fcs0;
    unsigned short _unused3;
    unsigned foo;
    unsigned short fcs1;
    unsigned short _unused4;
    fpregister registers[8];
};
#endif

/* the amount of ML stack space to reserve for registers,
   C exception handling etc. The compiler requires us to
   reserve 2 stack-frames worth (2 * 20 words). We actually reserve
   slightly more than this.
*/
#if (!defined(_WIN32) && !defined(HAVE_SIGALTSTACK))
// If we can't handle signals on a separate stack make sure there's space
// on the Poly stack.
#define OVERFLOW_STACK_SIZE (50+1024)
#else
#define OVERFLOW_STACK_SIZE 50
#endif

union stackItem
{
#ifndef POLYML32IN64
    stackItem(PolyWord v) { words[0] = v; };
    PolyWord words[1];
    bool IsPotentialPtr() const { return true; }
#else
    // In 32-in-64 we need to clear the second PolyWord.  This assumes little-endian.
    stackItem(PolyWord v) { words[0] = v; words[1] = PolyWord::FromUnsigned(0); };
    PolyWord words[2];
    // If the top word is anything other than zero we definitely don't have an address.
    bool IsPotentialPtr() const { return words[1] == PolyWord::FromUnsigned(0); }
#endif
    stackItem(): stackItem(TAGGED(0)) {} // Default constructor

    PolyWord w()const { return words[0]; }
    operator PolyWord () { return words[0]; }
    POLYCODEPTR codeAddr; // Return addresses
    stackItem *stackAddr; // Stack addresses
    intptr_t argValue; // Treat an address as an int
};

class X86TaskData;

// This is passed as the argument vector to X86AsmSwitchToPoly.
// The offsets are built into the assembly code and the code-generator.
// localMpointer and stackPtr are updated before control returns to C.
typedef struct _AssemblyArgs {
public:
    PolyWord        *localMpointer;     // Allocation ptr + 1 word
    stackItem       *handlerRegister;   // Current exception handler
    PolyWord        *localMbottom;      // Base of memory + 1 word
    stackItem       *stackLimit;        // Lower limit of stack
    stackItem       exceptionPacket;    // Set if there is an exception
    byte            unusedRequestCode;  // No longer used.
    byte            unusedFlag;         // No longer used
    byte            returnReason;       // Reason for returning from ML.
    byte            unusedRestore;      // No longer used.
    uintptr_t       saveCStack;         // Saved C stack frame.
    PolyObject      *threadId;          // My thread id.  Saves having to call into RTS for it.
    stackItem       *stackPtr;          // Current stack pointer
    byte            *enterInterpreter;  // Temporary
    byte            *heapOverFlowCall;  // These are filled in with the functions.
    byte            *stackOverFlowCall;
    byte            *stackOverFlowCallEx;
    // Saved registers, where applicable.
    stackItem       p_rax;
    stackItem       p_rbx;
    stackItem       p_rcx;
    stackItem       p_rdx;
    stackItem       p_rsi;
    stackItem       p_rdi;
#ifdef HOSTARCHITECTURE_X86_64
    stackItem       p_r8;
    stackItem       p_r9;
    stackItem       p_r10;
    stackItem       p_r11;
    stackItem       p_r12;
    stackItem       p_r13;
    stackItem       p_r14;
#endif
    struct fpSaveArea p_fp;
} AssemblyArgs;

// These next few are temporarily added for the interpreter
// This duplicates some code in reals.cpp but is now updated.
#define DOUBLESIZE (sizeof(double)/sizeof(POLYUNSIGNED))

union realdb { double dble; POLYUNSIGNED puns[DOUBLESIZE]; };

#define LGWORDSIZE (sizeof(uintptr_t) / sizeof(PolyWord))

class X86TaskData: public TaskData {
public:
    X86TaskData();
    unsigned allocReg; // The register to take the allocated space.
    POLYUNSIGNED allocWords; // The words to allocate.
    Handle callBackResult;
    AssemblyArgs assemblyInterface;
    int saveRegisterMask; // Registers that need to be updated by a GC.

    virtual void GarbageCollect(ScanAddress *process);
    void ScanStackAddress(ScanAddress *process, PolyWord &val, StackSpace *stack);
    virtual Handle EnterPolyCode(); // Start running ML
    virtual void InterruptCode();
    virtual bool AddTimeProfileCount(SIGNALCONTEXT *context);
    virtual void InitStackFrame(TaskData *parentTask, Handle proc, Handle arg);
    virtual void SetException(poly_exn *exc);

    // Release a mutex in exactly the same way as compiler code
    virtual Handle AtomicIncrement(Handle mutexp);
    virtual void AtomicReset(Handle mutexp);

    // Return the minimum space occupied by the stack.  Used when setting a limit.
    virtual uintptr_t currentStackSpace(void) const
        { return ((stackItem*)this->stack->top - assemblyInterface.stackPtr) + OVERFLOW_STACK_SIZE; }

    // Increment the profile count for an allocation.  Also now used for mutex contention.
    virtual void addProfileCount(POLYUNSIGNED words)
    { add_count(this, assemblyInterface.stackPtr[0].w().AsCodePtr(), words); }

    // PreRTSCall: After calling from ML to the RTS we need to save the current heap pointer
    virtual void PreRTSCall(void) { SaveMemRegisters(); }
    // PostRTSCall: Before returning we need to restore the heap pointer.
    // If there has been a GC in the RTS call we need to create a new heap area.
    virtual void PostRTSCall(void) { SetMemRegisters(); }

    virtual void CopyStackFrame(StackObject *old_stack, uintptr_t old_length, StackObject *new_stack, uintptr_t new_length);

    virtual Handle EnterCallbackFunction(Handle func, Handle args);

    int SwitchToPoly();

    void HeapOverflowTrap(byte *pcPtr);

    void SetMemRegisters();
    void SaveMemRegisters();
    void SetRegisterMask();

    PLock interruptLock;

    stackItem *get_reg(int n);

    stackItem *&regSP() { return assemblyInterface.stackPtr; }

    stackItem &regAX() { return assemblyInterface.p_rax; }
    stackItem &regBX() { return assemblyInterface.p_rbx; }
    stackItem &regCX() { return assemblyInterface.p_rcx; }
    stackItem &regDX() { return assemblyInterface.p_rdx; }
    stackItem &regSI() { return assemblyInterface.p_rsi; }
    stackItem &regDI() { return assemblyInterface.p_rdi; }
#ifdef HOSTARCHITECTURE_X86_64
    stackItem &reg8() { return assemblyInterface.p_r8; }
    stackItem &reg9() { return assemblyInterface.p_r9; }
    stackItem &reg10() { return assemblyInterface.p_r10; }
    stackItem &reg11() { return assemblyInterface.p_r11; }
    stackItem &reg12() { return assemblyInterface.p_r12; }
    stackItem &reg13() { return assemblyInterface.p_r13; }
    stackItem &reg14() { return assemblyInterface.p_r14; }
#endif

#if (defined(_WIN32) && !defined(__CYGWIN__))
    DWORD savedErrno;
#else
    int savedErrno;
#endif

    bool isMemRegs;

    // Temporary interpreter support
    int Interpret();

    POLYCODEPTR     taskPc; /* Program counter. */
    PolyWord        exception_arg;
    bool            raiseException;

    PolyObject      *overflowPacket, *dividePacket;

    // Allocate memory on the heap.  Returns with the address of the cell. Does not set the
    // length word or any of the data.
    PolyObject *allocateMemory(POLYUNSIGNED words, POLYCODEPTR &pc, stackItem *&sp)
    {
        words++; // Add the size of the length word.
                 // N.B. The allocation area may be empty so that both of these are zero.
        if (this->allocPointer >= this->allocLimit + words + 1)
        {
#ifdef POLYML32IN64
            if (words & 1)
            {
                this->allocPointer[-1] = PolyWord::FromUnsigned(0xcccccccc);
                words++;
            }
#endif
            this->allocPointer -= words;
            return (PolyObject *)(this->allocPointer + 1);
        }
        // Insufficient space.
        SaveInterpreterState(pc, sp);
        // Find some space to allocate in. Returns a pointer to the newly allocated space.
        // N.B. This may return zero if the heap is exhausted and it has set this
        // up for an exception.  Generally it allocates by decrementing allocPointer
        // but if the required memory is large it may allocate in a separate area.
        PolyWord *space = processes->FindAllocationSpace(this, words, true);
        LoadInterpreterState(pc, sp);
        if (space == 0) return 0;
        return (PolyObject *)(space + 1);
    }

    // Put a real result in a "box"
    PolyObject *boxDouble(double d, POLYCODEPTR &pc, stackItem *&sp)
    {
        PolyObject *mem = this->allocateMemory(DOUBLESIZE, pc, sp);
        if (mem == 0) return 0;
        mem->SetLengthWord(DOUBLESIZE, F_BYTE_OBJ);
        union realdb uniondb;
        uniondb.dble = d;
        // Copy the words.  Depending on the word length this may copy one or more words.
        for (unsigned i = 0; i < DOUBLESIZE; i++)
            mem->Set(i, PolyWord::FromUnsigned(uniondb.puns[i]));
        return mem;
    }

    // Extract a double value from a box.
    double unboxDouble(PolyWord p)
    {
        union realdb uniondb;
        for (unsigned i = 0; i < DOUBLESIZE; i++)
            uniondb.puns[i] = p.AsObjPtr()->Get(i).AsUnsigned();
        return uniondb.dble;
    }

    // Update the copies in the task object
    void SaveInterpreterState(POLYCODEPTR pc, stackItem *sp)
    {
        taskPc = pc;
        assemblyInterface.stackPtr = sp;
    }

    // Update the local state
    void LoadInterpreterState(POLYCODEPTR &pc, stackItem *&sp)
    {
        pc = taskPc;
        sp = assemblyInterface.stackPtr;
    }

};

class X86Dependent: public MachineDependent {
public:
    X86Dependent() {}

    // Create a task data object.
    virtual TaskData *CreateTaskData(void) { return new X86TaskData(); }

    virtual unsigned InitialStackSize(void) { return 128+OVERFLOW_STACK_SIZE; } // Initial size of a stack 
    virtual void ScanConstantsWithinCode(PolyObject *addr, PolyObject *oldAddr, POLYUNSIGNED length, ScanAddress *process);

    virtual Architectures MachineArchitecture(void)
#ifndef HOSTARCHITECTURE_X86_64
         { return MA_I386; }
#elif defined(POLYML32IN64)
        { return MA_X86_64_32; }
#else
         { return MA_X86_64; }
#endif
};

// Values for the returnReason byte
enum RETURN_REASON {
    RETURN_IO_CALL_NOW_UNUSED = 0,
    RETURN_HEAP_OVERFLOW = 1,
    RETURN_STACK_OVERFLOW = 2,
    RETURN_STACK_OVERFLOWEX = 3,
    RETURN_ENTER_INTERPRETER = 4,
    RETURN_CALLBACK_RETURN = 6,
    RETURN_CALLBACK_EXCEPTION = 7,
    RETURN_KILL_SELF = 9
};

extern "C" {

    // These are declared in the assembly code segment.
    void X86AsmSwitchToPoly(void *);

    extern int X86AsmKillSelf(void);
    extern int X86AsmCallbackReturn(void);
    extern int X86AsmCallbackException(void);
    extern int X86AsmPopArgAndClosure(void);
    extern int X86AsmRaiseException(void);
    extern int X86AsmCallExtraRETURN_HEAP_OVERFLOW(void);
    extern int X86AsmCallExtraRETURN_STACK_OVERFLOW(void);
    extern int X86AsmCallExtraRETURN_STACK_OVERFLOWEX(void);
    extern int X86AsmCallExtraRETURN_ENTER_INTERPRETER(void);

    POLYUNSIGNED X86AsmAtomicIncrement(PolyObject*);
    POLYUNSIGNED X86AsmAtomicDecrement(PolyObject*);
};

X86TaskData::X86TaskData(): allocReg(0), allocWords(0), saveRegisterMask(0)
{
    assemblyInterface.enterInterpreter = (byte*)X86AsmCallExtraRETURN_ENTER_INTERPRETER;
    assemblyInterface.heapOverFlowCall = (byte*)X86AsmCallExtraRETURN_HEAP_OVERFLOW;
    assemblyInterface.stackOverFlowCall = (byte*)X86AsmCallExtraRETURN_STACK_OVERFLOW;
    assemblyInterface.stackOverFlowCallEx = (byte*)X86AsmCallExtraRETURN_STACK_OVERFLOWEX;
    savedErrno = 0;
    isMemRegs = false;
}

void X86TaskData::GarbageCollect(ScanAddress *process)
{
    TaskData::GarbageCollect(process); // Process the parent first
    assemblyInterface.threadId = threadObject;

    if (stack != 0)
    {
        // Now the values on the stack.
        for (stackItem *q = assemblyInterface.stackPtr; q < (stackItem*)stack->top; q++)
        {
            if (q->IsPotentialPtr())
            {
                PolyWord w = q->w();
                if (w.IsDataPtr())
                {
                    ScanStackAddress(process, w, stack);
                    *q = w;
                }
            }
        }
    }
    // Register mask
    for (int i = 0; i < 16; i++)
    {
        if (saveRegisterMask & (1 << i))
        {
            stackItem *regAddr = get_reg(i);
            if (regAddr->IsPotentialPtr())
            {
                PolyWord w = regAddr->w();
                if (w.IsDataPtr())
                {
                    ScanStackAddress(process, w, stack);
                    *regAddr = w;
                }
            }
        }
    }
}

// Process a value within the stack.
void X86TaskData::ScanStackAddress(ScanAddress *process, PolyWord &val, StackSpace *stack)
{
    // We may have return addresses on the stack which could look like
    // tagged values.  Check whether the value is in the code area before
    // checking whether it is untagged.
    // The -1 here is because we may have a zero-sized cell in the last
    // word of a space.
    MemSpace *space = gMem.SpaceForAddress(val.AsCodePtr()-1);
    if (space == 0) return;
    if (space->spaceType == ST_CODE)
    {
        PolyObject *obj = gMem.FindCodeObject(val.AsCodePtr());
        // If it is actually an integer it might be outside a valid code object.
        if (obj == 0)
        {
            ASSERT(val.IsTagged()); // It must be an integer
        }
        else // Process the address of the start.  Don't update anything.
            process->ScanObjectAddress(obj);
    }
    else if (space->spaceType == ST_LOCAL && val.IsDataPtr())
        // Local values must be word addresses.
        val = process->ScanObjectAddress(val.AsObjPtr());
}


// Copy a stack
void X86TaskData::CopyStackFrame(StackObject *old_stack, uintptr_t old_length, StackObject *new_stack, uintptr_t new_length)
{
    /* Moves a stack, updating all references within the stack */
#ifdef POLYML32IN64
    old_length = old_length / 2;
    new_length = new_length / 2;
#endif

    stackItem *old_base  = (stackItem *)old_stack;
    stackItem *new_base  = (stackItem*)new_stack;
    stackItem *old_top   = old_base + old_length;

    /* Calculate the offset of the new stack from the old. If the frame is
       being extended objects in the new frame will be further up the stack
       than in the old one. */

    uintptr_t offset = new_base - old_base + new_length - old_length;

    stackItem *oldStackPtr = assemblyInterface.stackPtr;

    // Adjust the stack pointer and handler pointer since these point into the stack.
    assemblyInterface.stackPtr = assemblyInterface.stackPtr + offset;
    assemblyInterface.handlerRegister = assemblyInterface.handlerRegister + offset;

    // We need to adjust any values on the stack that are pointers within the stack.
    // Skip the unused part of the stack.

    size_t i = oldStackPtr - old_base;

    ASSERT (i <= old_length);

    i = old_length - i;

    stackItem *old = oldStackPtr;
    stackItem *newp = assemblyInterface.stackPtr;

    while (i--)
    {
        stackItem old_word = *old++;
        if (old_word.w().IsDataPtr() && old_word.stackAddr >= old_base && old_word.stackAddr <= old_top)
            old_word.stackAddr = old_word.stackAddr + offset;
        else if (old_word.w().IsDataPtr() && IsHeapAddress(old_word.stackAddr))
        {
            stackItem *addr = (stackItem*)old_word.w().AsStackAddr();
            if (addr >= old_base && addr <= old_top)
            {
                addr += offset;
                old_word = PolyWord::FromStackAddr((PolyWord*)addr);
            }
        }
        *newp++ = old_word;
    }
    ASSERT(old == ((stackItem*)old_stack)+old_length);
    ASSERT(newp == ((stackItem*)new_stack)+new_length);
    // And change any registers that pointed into the old stack
    for (int j = 0; j < 16; j++)
    {
        if (saveRegisterMask & (1 << j))
        {
            stackItem *regAddr = get_reg(j);
            stackItem old_word = *regAddr;
            if (old_word.w().IsDataPtr() && old_word.stackAddr >= old_base && old_word.stackAddr <= old_top)
                old_word.stackAddr = old_word.stackAddr + offset;
            else if (old_word.w().IsDataPtr() && IsHeapAddress(old_word.stackAddr))
            {
                stackItem *addr = (stackItem*)old_word.w().AsStackAddr();
                if (addr >= old_base && addr <= old_top)
                {
                    addr += offset;
                    old_word = PolyWord::FromStackAddr((PolyWord*)addr);
                }
            }
            *regAddr = old_word;
       }
    }
}

Handle X86TaskData::EnterPolyCode()
/* Called from "main" to enter the code. */
{
    Handle hOriginal = this->saveVec.mark(); // Set this up for the IO calls.
    while (1)
    {
        this->saveVec.reset(hOriginal); // Remove old RTS arguments and results.

        // Run the ML code and return with the function to call.
        this->inML = true;
        int ioFunction = SwitchToPoly();
        this->inML = false;

        try {
            switch (ioFunction)
            {
            case -1:
                // We've been interrupted.  This usually involves simulating a
                // stack overflow so we could come here because of a genuine
                // stack overflow.
                // Previously this code was executed on every RTS call but there
                // were problems on Mac OS X at least with contention on schedLock.
                // Process any asynchronous events i.e. interrupts or kill
                processes->ProcessAsynchRequests(this);
                // Release and re-acquire use of the ML memory to allow another thread
                // to GC.
                processes->ThreadReleaseMLMemory(this);
                processes->ThreadUseMLMemory(this);
                break;

            case -2: // A callback has returned.
                return callBackResult; // Return the saved value. Not used in the new interface.

            default:
                Crash("Unknown io operation %d\n", ioFunction);
            }
        }
        catch (IOException &) {
        }
    }
}

// Run the current ML process.  X86AsmSwitchToPoly saves the C state so that
// whenever the ML requires assistance from the rest of the RTS it simply
// returns to C with the appropriate values set in assemblyInterface.requestCode and
// 

int X86TaskData::SwitchToPoly()
// (Re)-enter the Poly code from C.  Returns with the io function to call or
// -1 if we are responding to an interrupt.
{
    Handle mark = this->saveVec.mark();
    do
    {
        this->saveVec.reset(mark); // Remove old data e.g. from arbitrary precision.
        SetMemRegisters();

        // We need to save the C stack entry across this call in case
        // we're making a callback and the previous C stack entry is
        // for the original call.
        POLYUNSIGNED savedCStack = this->assemblyInterface.saveCStack;
        // Restore the saved error state.
#if (defined(_WIN32) && !defined(__CYGWIN__))
        SetLastError(savedErrno);
#else
        errno = savedErrno;
#endif
        // Enter the ML code.
        X86AsmSwitchToPoly(&this->assemblyInterface);

        this->assemblyInterface.saveCStack = savedCStack;
        // Save the error codes.  We may have made an RTS/FFI call that
        // has set these and we don't want to do anything to change them.
#if (defined(_WIN32) && !defined(__CYGWIN__))
        savedErrno = GetLastError();
#else
        savedErrno = errno;
#endif

        SaveMemRegisters(); // Update globals from the memory registers.

        // Handle any heap/stack overflows or arbitrary precision traps.
        switch (this->assemblyInterface.returnReason)
        {

        case RETURN_HEAP_OVERFLOW:
            // The heap has overflowed.
            SetRegisterMask();
            this->HeapOverflowTrap(assemblyInterface.stackPtr[0].w().AsCodePtr()); // Computes a value for allocWords only
            break;

        case RETURN_STACK_OVERFLOW:
        case RETURN_STACK_OVERFLOWEX:
        {
            SetRegisterMask();
            uintptr_t min_size;
            if (assemblyInterface.returnReason == RETURN_STACK_OVERFLOW)
            {
                min_size = this->stack->top - (PolyWord*)assemblyInterface.stackPtr + OVERFLOW_STACK_SIZE;
            }
            else
            {
                // Stack limit overflow.  If the required stack space is larger than
                // the fixed overflow size the code will calculate the limit in %EDI.
                stackItem *stackP = regDI().stackAddr;
                min_size = this->stack->top - (PolyWord*)stackP + OVERFLOW_STACK_SIZE;
            }
            try {
                // The stack check has failed.  This may either be because we really have
                // overflowed the stack or because the stack limit value has been adjusted
                // to result in a call here.
                CheckAndGrowStack(this, min_size);
            }
            catch (IOException &) {
               // We may get an exception while handling this if we run out of store
            }
            {
                PLocker l(&interruptLock);
                // Set the stack limit.  This clears any interrupt and also sets the
                // correct value if we've grown the stack.
                this->assemblyInterface.stackLimit = (stackItem*)this->stack->bottom + OVERFLOW_STACK_SIZE;
            }
            return -1; // We're in a safe state to handle any interrupts.
        }

        case RETURN_CALLBACK_RETURN:
            // Remove the extra exception handler we created in EnterCallbackFunction
            ASSERT(assemblyInterface.handlerRegister == regSP());
            regSP() += 1;
            assemblyInterface.handlerRegister = (*(regSP()++)).stackAddr; // Restore the previous handler.
            this->callBackResult = this->saveVec.push(regAX()); // Argument to return is in RAX.
            return -2;

        case RETURN_CALLBACK_EXCEPTION:
            // An ML callback has raised an exception.
            // It isn't possible to do anything here except abort.
            Crash("An ML function called from foreign code raised an exception.  Unable to continue.");

        case RETURN_KILL_SELF:
            exitThread(this);

        case RETURN_ENTER_INTERPRETER:
        {
            taskPc = assemblyInterface.stackPtr[0].codeAddr;
            assemblyInterface.stackPtr++; // Pop return address.
            byte reasonCode = *taskPc++;
            // Sort out arguments.
            if (reasonCode == 0xff)
            {
                // Exception handler.
                exception_arg = assemblyInterface.p_rax; // Get the exception packet
                // We're already in the exception handler but we still have to
                // adjust the stack pointer and pop the current exception handler.
                assemblyInterface.stackPtr = assemblyInterface.handlerRegister;
                assemblyInterface.stackPtr++;
                assemblyInterface.handlerRegister = (assemblyInterface.stackPtr++)[0].stackAddr;
                this->raiseException = false;
            }
            else if (reasonCode >= 128)
            {
                // Start of function.
                unsigned numArgs = reasonCode - 128;
                ASSERT(numArgs == 1);
                // We need the stack to contain:
                // The closure, the return address, the arguments.
                // First pop the original return address.
                POLYCODEPTR returnAddr = (assemblyInterface.stackPtr++)[0].codeAddr;
                // Push the register args.
                *(--assemblyInterface.stackPtr) = assemblyInterface.p_rax;
                (--assemblyInterface.stackPtr)[0].codeAddr = returnAddr;
                *(--assemblyInterface.stackPtr) = assemblyInterface.p_rdx; // Closure
            }
            else
            {
                // Return from call.
                ASSERT(0);
            }
            Interpret();
            break;
        }

        default:
            Crash("Unknown return reason code %u", this->assemblyInterface.returnReason);
        }

    } while (1);
}

void X86TaskData::InitStackFrame(TaskData *parentTaskData, Handle proc, Handle arg)
/* Initialise stack frame. */
{
    StackSpace *space = this->stack;
    StackObject * newStack = space->stack();
    uintptr_t stack_size     = space->spaceSize() * sizeof(PolyWord) / sizeof(stackItem);
    uintptr_t topStack = stack_size-6;
    stackItem *stackTop = (stackItem*)newStack + topStack;
    assemblyInterface.stackPtr = stackTop;
    assemblyInterface.stackLimit = (stackItem*)((PolyWord*)space->bottom + OVERFLOW_STACK_SIZE);
    assemblyInterface.handlerRegister = (stackItem*)newStack+topStack+4;

    // Floating point save area.
    memset(&assemblyInterface.p_fp, 0, sizeof(struct fpSaveArea));
#ifndef HOSTARCHITECTURE_X86_64
    // Set the control word for 64-bit precision otherwise we get inconsistent results.
    assemblyInterface.p_fp.cw = 0x027f ; // Control word
    assemblyInterface.p_fp.tw = 0xffff; // Tag registers - all unused
#endif
    // Initial entry point - on the stack.
    stackTop[0].codeAddr = (byte*)&X86AsmPopArgAndClosure;

    // Push the argument and the closure on the stack.  We can't put them into the registers
    // yet because we might get a GC before we actually start the code.
    stackTop[1] = proc->Word(); // Closure
    stackTop[2] = (arg == 0) ? TAGGED(0) : DEREFWORD(arg); // Argument
    /* We initialise the end of the stack with a sequence that will jump to
       kill_self whether the process ends with a normal return or by raising an
       exception.  A bit of this was added to fix a bug when stacks were objects
       on the heap and could be scanned by the GC. */
    stackTop[5] = TAGGED(0); // Probably no longer needed
    // Set the default handler and return address to point to this code.
//    PolyWord killJump(PolyWord::FromCodePtr((byte*)&X86AsmKillSelf));
    // Exception handler.
    stackTop[4].codeAddr = (byte*)&X86AsmKillSelf;
    // Normal return address.  We need a separate entry on the stack from
    // the exception handler because it is possible that the code we are entering
    // may replace this entry with an argument.  The code-generator optimises tail-recursive
    // calls to functions with more args than the called function.
    stackTop[3].codeAddr = (byte*)&X86AsmKillSelf;

#ifdef POLYML32IN64
    // In 32-in-64 RBX always contains the heap base address.
    assemblyInterface.p_rbx.stackAddr = (stackItem*)globalHeapBase;
#endif

    // For the interpreter
    raiseException = false;
    exception_arg = TAGGED(0); /* Used for exception argument. */

    // Make packets for exceptions.             
    overflowPacket = makeExceptionPacket(parentTaskData, EXC_overflow);
    dividePacket = makeExceptionPacket(parentTaskData, EXC_divide);
}

// In Solaris-x86 the registers are named EIP and ESP.
#if (!defined(REG_EIP) && defined(EIP))
#define REG_EIP EIP
#endif
#if (!defined(REG_ESP) && defined(ESP))
#define REG_ESP ESP
#endif


// Get the PC and SP(stack) from a signal context.  This is needed for profiling.
// This version gets the actual sp and pc if we are in ML.
bool X86TaskData::AddTimeProfileCount(SIGNALCONTEXT *context)
{
    stackItem * sp = 0;
    POLYCODEPTR pc = 0;
    if (context != 0)
    {
        // The tests for HAVE_UCONTEXT_T, HAVE_STRUCT_SIGCONTEXT and HAVE_WINDOWS_H need
        // to follow the tests in processes.h.
#if defined(HAVE_WINDOWS_H)
#ifdef _WIN64
        sp = (stackItem *)context->Rsp;
        pc = (POLYCODEPTR)context->Rip;
#else
        // Windows 32 including cygwin.
        sp = (stackItem *)context->Esp;
        pc = (POLYCODEPTR)context->Eip;
#endif
#elif defined(HAVE_UCONTEXT_T)
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
#endif
#else /* HOSTARCHITECTURE_X86_64 */
#if(defined(HAVE_STRUCT_MCONTEXT_SS)||defined(HAVE_STRUCT___DARWIN_MCONTEXT64_SS))
        pc = (byte*)context->uc_mcontext->ss.rip;
        sp = (PolyWord*)context->uc_mcontext->ss.rsp;
#elif(defined(HAVE_STRUCT___DARWIN_MCONTEXT64___SS))
        pc = (byte*)context->uc_mcontext->__ss.__rip;
        sp = (PolyWord*)context->uc_mcontext->__ss.__rsp;
#endif
#endif /* HOSTARCHITECTURE_X86_64 */
#endif
#elif defined(HAVE_STRUCT_SIGCONTEXT)
#if defined(HOSTARCHITECTURE_X86_64) && defined(__OpenBSD__)
        // CPP defines missing in amd64/signal.h in OpenBSD
        pc = (byte*)context->sc_rip;
        sp = (PolyWord*)context->sc_rsp;
#else // !HOSTARCHITEXTURE_X86_64 || !defined(__OpenBSD__)
        pc = (byte*)context->sc_pc;
        sp = (PolyWord*)context->sc_sp;
#endif
#endif
    }
    if (pc != 0)
    {
        // See if the PC we've got is an ML code address.
        MemSpace *space = gMem.SpaceForAddress(pc);
        if (space != 0 && (space->spaceType == ST_CODE || space->spaceType == ST_PERMANENT))
        {
            add_count(this, pc, 1);
            return true;
        }
    }
    // See if the sp value is in the current stack.
    if (sp >= (stackItem*)this->stack->bottom && sp < (stackItem*)this->stack->top)
    {
        // We may be in the assembly code.  The top of the stack will be a return address.
        pc = sp[0].w().AsCodePtr();
        MemSpace *space = gMem.SpaceForAddress(pc);
        if (space != 0 && (space->spaceType == ST_CODE || space->spaceType == ST_PERMANENT))
        {
            add_count(this, pc, 1);
            return true;
        }
    }
    // See if the value of regSP is a valid stack pointer.
    // This works if we happen to be in an RTS call using a "Full" call.
    // It doesn't work if we've used a "Fast" call because that doesn't save the SP.
    sp = assemblyInterface.stackPtr;
    if (sp >= (stackItem*)this->stack->bottom && sp < (stackItem*)this->stack->top)
    {
        // We may be in the run-time system.
        pc = sp[0].w().AsCodePtr();
        MemSpace *space = gMem.SpaceForAddress(pc);
        if (space != 0 && (space->spaceType == ST_CODE || space->spaceType == ST_PERMANENT))
        {
            add_count(this, pc, 1);
            return true;
        }
    }
    // None of those worked
    return false;
}

// This is called from a different thread so we have to be careful.
void X86TaskData::InterruptCode()
{
    PLocker l(&interruptLock);
    // Set the stack limit pointer to the top of the stack to cause
    // a trap when we next check for stack overflow.
    // We use a lock here to ensure that we always use the current value of the
    // stack.  The thread we're interrupting could be growing the stack at this point.
    if (this->stack != 0) 
        this->assemblyInterface.stackLimit = (stackItem*)(this->stack->top-1);
}

// This is called from SwitchToPoly before we enter the ML code.
void X86TaskData::SetMemRegisters()
{
    ASSERT(! isMemRegs);
    isMemRegs = true;
    // Copy the current store limits into variables before we go into the assembly code.

    // If we haven't yet set the allocation area or we don't have enough we need
    // to create one (or a new one).
    if (this->allocPointer <= this->allocLimit + this->allocWords)
    {
        if (this->allocPointer < this->allocLimit)
            Crash ("Bad length in heap overflow trap");

        // Find some space to allocate in.  Updates taskData->allocPointer and
        // returns a pointer to the newly allocated space (if allocWords != 0)
        PolyWord *space =
            processes->FindAllocationSpace(this, this->allocWords, true);
        if (space == 0)
        {
            // We will now raise an exception instead of returning.
            // Set allocWords to zero so we don't set the allocation register
            // since that could be holding the exception packet.
            this->allocWords = 0;
        }
        // Undo the allocation just now.
        this->allocPointer += this->allocWords;
    }

    if (this->allocWords != 0)
    {
        // If we have had a heap trap we actually do the allocation here.
        // We will have already garbage collected and recovered sufficient space.
        // This also happens if we have just trapped because of store profiling.
        this->allocPointer -= this->allocWords; // Now allocate
        // Set the allocation register to this area.
        if (this->allocReg < 15)
            *(get_reg(this->allocReg)) =
                PolyWord::FromStackAddr(this->allocPointer + 1); /* remember: it's off-by-one */
        this->allocWords = 0;
    }

    // If we have run out of store, either just above or while allocating in the RTS,
    // allocPointer and allocLimit will have been set to zero as part of the GC.  We will
    // now be raising an exception which may free some store but we need to come back here
    // before we allocate anything.  The compiled code uses unsigned arithmetic to check for
    // heap overflow but only after subtracting the space required.  We need to make sure
    // that the values are still non-negative after substracting any object size.
    if (this->allocPointer == 0) this->allocPointer += MAX_OBJECT_SIZE;
    if (this->allocLimit == 0) this->allocLimit += MAX_OBJECT_SIZE;

    this->assemblyInterface.localMbottom = this->allocLimit + 1;
    this->assemblyInterface.localMpointer = this->allocPointer + 1;
    // If we are profiling store allocation we set mem_hl so that a trap
    // will be generated.
    if (profileMode == kProfileStoreAllocation)
        this->assemblyInterface.localMbottom = this->assemblyInterface.localMpointer;

    this->assemblyInterface.returnReason = RETURN_IO_CALL_NOW_UNUSED;

    this->assemblyInterface.threadId = this->threadObject;
}

// This is called whenever we have returned from ML to C.
void X86TaskData::SaveMemRegisters()
{
    ASSERT(isMemRegs);
    isMemRegs = false;
    this->allocPointer = this->assemblyInterface.localMpointer - 1;
    this->allocWords = 0;
    this->assemblyInterface.exceptionPacket = TAGGED(0);
    this->saveRegisterMask = 0;
}

// Called on a GC or stack overflow trap.  The register mask
// is in the bytes after the trap call.
void X86TaskData::SetRegisterMask()
{
    byte *pc = assemblyInterface.stackPtr[0].w().AsCodePtr();
    if (*pc == 0xcd) // CD - INT n is used for a single byte
    {
        pc++;
        saveRegisterMask = *pc++;
    }
    else if (*pc == 0xca) // CA - FAR RETURN is used for a two byte mask
    {
        pc++;
        saveRegisterMask = pc[0] | (pc[1] << 8);
        pc += 2;
    }
    assemblyInterface.stackPtr[0] = PolyWord::FromCodePtr(pc);
}

stackItem *X86TaskData::get_reg(int n)
/* Returns a pointer to the register given by n. */
{
    switch (n) 
    {
    case 0: return &assemblyInterface.p_rax;
    case 1: return &assemblyInterface.p_rcx;
    case 2: return &assemblyInterface.p_rdx;
    case 3: return &assemblyInterface.p_rbx;
        // Should not have rsp or rbp.
    case 6: return &assemblyInterface.p_rsi;
    case 7: return &assemblyInterface.p_rdi;
#ifdef HOSTARCHITECTURE_X86_64
    case 8: return &assemblyInterface.p_r8;
    case 9: return &assemblyInterface.p_r9;
    case 10: return &assemblyInterface.p_r10;
    case 11: return &assemblyInterface.p_r11;
    case 12: return &assemblyInterface.p_r12;
    case 13: return &assemblyInterface.p_r13;
    case 14: return &assemblyInterface.p_r14;
    // R15 is the heap pointer so shouldn't occur here.
#endif /* HOSTARCHITECTURE_X86_64 */
    default: Crash("Unknown register %d\n", n);
    }
}

// Called as a result of a heap overflow trap
void X86TaskData::HeapOverflowTrap(byte *pcPtr)
{
    X86TaskData *mdTask = this;
    POLYUNSIGNED wordsNeeded = 0;
    // The next instruction, after any branches round forwarding pointers or pop
    // instructions, will be a store of register containing the adjusted heap pointer.
    // We need to find that register and the value in it in order to find out how big
    // the area we actually wanted is.  N.B.  The code-generator and assembly code
    // must generate the correct instruction sequence.
//    byte *pcPtr = assemblyInterface.programCtr;
    while (true)
    {
        if (pcPtr[0] == 0xeb)
        {
            // Forwarding pointer
            if (pcPtr[1] >= 128) pcPtr += 256 - pcPtr[1] + 2;
            else pcPtr += pcPtr[1] + 2;
        }
        else if ((pcPtr[0] & 0xf8) == 0x58) // Pop instruction.
            pcPtr++;
        else if (pcPtr[0] == 0x41 && ((pcPtr[1] & 0xf8) == 0x58)) // Pop with Rex prefix
            pcPtr += 2;
        else break;
    }
#ifndef HOSTARCHITECTURE_X86_64
    // This should be movl REG,0[%ebp].
    ASSERT(pcPtr[0] == 0x89);
    mdTask->allocReg = (pcPtr[1] >> 3) & 7; // Remember this until we allocate the memory
    PolyWord *reg = get_reg(mdTask->allocReg);
    PolyWord reg_val = *reg;
    // The space we need is the difference between this register
    // and the current value of newptr.
    // The +1 here is because assemblyInterface.localMpointer is A.M.pointer +1.  The reason
    // is that after the allocation we have the register pointing at the address we will
    // actually use.
    wordsNeeded = (this->allocPointer - (PolyWord*)reg_val.AsAddress()) + 1;
    *reg = TAGGED(0); // Clear this - it's not a valid address.
    /* length in words, including length word */

    ASSERT (wordsNeeded <= (1<<24)); /* Max object size including length/flag word is 2^24 words.  */
#else /* HOSTARCHITECTURE_X86_64 */
    ASSERT(pcPtr[1] == 0x89 || pcPtr[1] == 0x8b);
    if (pcPtr[1] == 0x89)
    {
        // New (5.4) format.  This should be movq REG,%r15
        ASSERT(pcPtr[0] == 0x49 || pcPtr[0] == 0x4d);
        mdTask->allocReg = (pcPtr[2] >> 3) & 7; // Remember this until we allocate the memory
        if (pcPtr[0] & 0x4) mdTask->allocReg += 8;
    }
    else
    {
        // Alternative form of movq REG,%r15
        ASSERT(pcPtr[0] == 0x4c || pcPtr[0] == 0x4d);
        mdTask->allocReg = pcPtr[2] & 7; // Remember this until we allocate the memory
        if (pcPtr[0] & 0x1) mdTask->allocReg += 8;
    }
    stackItem *reg = get_reg(this->allocReg);
    stackItem reg_val = *reg;
    wordsNeeded = (this->allocPointer - (PolyWord*)reg_val.stackAddr) + 1;
    *reg = TAGGED(0); // Clear this - it's not a valid address.
 #endif /* HOSTARCHITECTURE_X86_64 */
    if (profileMode == kProfileStoreAllocation)
        addProfileCount(wordsNeeded);

    mdTask->allocWords = wordsNeeded; // The actual allocation is done in SetMemRegisters.
}

void X86TaskData::SetException(poly_exn *exc)
// Set up the stack to raise an exception.
{
    // Do we need to set the PC value any longer?  It may be necessary if
    // we have taken a trap because another thread has sent a broadcast interrupt.
    *(--assemblyInterface.stackPtr) = PolyWord::FromCodePtr((byte*)X86AsmRaiseException);
    regAX() = (PolyWord)exc; /* put exception data into eax */
    assemblyInterface.exceptionPacket = (PolyWord)exc; // Set for direct calls.
    this->raiseException = true;
}

// Sets up a callback function on the current stack.  The present state is that
// the ML code has made a call in to foreign_dispatch.  We need to set the stack
// up so that we will enter the callback (as with CallCodeTupled) but when we return
// the result we enter callback_return. 
Handle X86TaskData::EnterCallbackFunction(Handle func, Handle args)
{
    // If we ever implement a light version of the FFI that allows a call to C
    // code without saving enough to allow allocation in C code we need to ensure
    // that this code doesn't do any allocation.  Essentially we need the values
    // in localMpointer and localMbottom to be valid across a call to C.  If we do
    // a callback the ML callback function would pick up the values saved in the
    // originating call.

    // Set up an exception handler so we will enter callBackException if there is an exception.
    (--regSP())->stackAddr = assemblyInterface.handlerRegister; // Create a special handler entry
    *(--regSP()) = PolyWord::FromCodePtr((byte*)&X86AsmCallbackException);
    assemblyInterface.handlerRegister = regSP();
    // Push the call to callBackReturn onto the stack as the return address.
    *(--regSP()) = PolyWord::FromCodePtr((byte*)&X86AsmCallbackReturn);
    // Set up the entry point of the callback.
    PolyObject *functToCall = func->WordP();
    regDX() = (PolyWord)functToCall; // Closure address
    regAX() = args->Word();
    // Push entry point address
    *(--regSP()) = functToCall->Get(0); // First word of closure is entry pt.

    return EnterPolyCode();
}

// Decode and process an effective address.  There may
// be a constant address in here but in any case we need
// to decode it to work out where the next instruction starts.
// If this is an lea instruction any addresses are just constants
// so must not be treated as addresses.
static void skipea(PolyObject *base, byte **pt, ScanAddress *process, bool lea)
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
                if (! lea) {
#ifndef HOSTARCHITECTURE_X86_64
                    process->ScanConstant(base, *pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
                }
                (*pt) += 4;
            }
        }
        else if (md == 1) (*pt)++;
        else if (md == 2) (*pt) += 4;
    }
    else if (md == 0 && rm == 5)
    {
        if (!lea) {
#ifndef HOSTARCHITECTURE_X86_64
            /* Absolute address. */
            process->ScanConstant(base, *pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
        }
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
    PolyWord *end = addr->Offset(length - 1);
#ifdef POLYML32IN64
    // If this begins with enter-int it's interpreted code - ignore
    if (pt[0] == 0xff && pt[1] == 0x55 && pt[2] == 0x48) return;
#endif

    while (true)
    {
        // Escape prefixes come before any Rex byte
        if (*pt == 0xf2 || *pt == 0xf3 || *pt == 0x66)
            pt++;
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
        case 0x00: return; // This is actually the first byte of the old "marker" word.
        case 0xf4: return; // Halt - now used as a marker.
        case 0x50: case 0x51: case 0x52: case 0x53:
        case 0x54: case 0x55: case 0x56: case 0x57: /* Push */
        case 0x58: case 0x59: case 0x5a: case 0x5b:
        case 0x5c: case 0x5d: case 0x5e: case 0x5f: /* Pop */
        case 0x90: /* nop */ case 0xc3: /* ret */
        case 0xf9: /* stc */ case 0xce: /* into */
        case 0xf0: /* lock. */ case 0xf3: /* rep/repe */
        case 0xa4: case 0xa5: case 0xaa: case 0xab: /* movs/stos */
        case 0xa6: /* cmpsb */ case 0x9e: /* sahf */ case 0x99: /* cqo/cdq */
            pt++; break;

        case 0x70: case 0x71: case 0x72: case 0x73: case 0x74: case 0x75: case 0x76: case 0x77:
        case 0x78: case 0x79: case 0x7a: case 0x7b: case 0x7c: case 0x7d: case 0x7e: case 0x7f:
        case 0xeb:
            /* short jumps. */
        case 0xcd: /* INT - now used for a register mask */
        case 0xa8: /* TEST_ACC8 */
        case 0x6a: /* PUSH_8 */
            pt += 2; break;

        case 0xc2: /* RET_16 */
        case 0xca: /* FAR RET 16 - used for a register mask */
            pt += 3; break;

        case 0x8d: /* leal. */
            pt++; skipea(addr, &pt, process, true); break;

        case 0x03: case 0x0b: case 0x13: case 0x1b:
        case 0x23: case 0x2b: case 0x33: case 0x3b: /* Add r,ea etc. */
        case 0x88: /* MOVB_R_A */ case 0x89: /* MOVL_R_A */
        case 0x8b: /* MOVL_A_R */
        case 0x62: /* BOUNDL */
        case 0xff: /* Group5 */
        case 0xd1: /* Group2_1_A */
        case 0x8f: /* POP_A */
        case 0xd3: /* Group2_CL_A */
        case 0x87: // XCHNG
            pt++; skipea(addr, &pt, process, false); break;

        case 0xf6: /* Group3_a */
            {
                int isTest = 0;
                pt++;
                /* The test instruction has an immediate operand. */
                if ((*pt & 0x38) == 0) isTest = 1;
                skipea(addr, &pt, process, false);
                if (isTest) pt++;
                break;
            }

        case 0xf7: /* Group3_A */
            {
                int isTest = 0;
                pt++;
                /* The test instruction has an immediate operand. */
                if ((*pt & 0x38) == 0) isTest = 1;
                skipea(addr, &pt, process, false);
                if (isTest) pt += 4;
                break;
            }

        case 0xc1: /* Group2_8_A */
        case 0xc6: /* MOVB_8_A */
        case 0x83: /* Group1_8_A */
        case 0x80: /* Group1_8_a */
        case 0x6b: // IMUL Ev,Ib
            pt++; skipea(addr, &pt, process, false); pt++; break;

        case 0x69: // IMUL Ev,Iv
            pt++; skipea(addr, &pt, process, false); pt += 4; break;

        case 0x81: /* Group1_32_A */
            {
                pt ++;
#ifndef HOSTARCHITECTURE_X86_64
                unsigned opCode = *pt;
#endif /* HOSTARCHITECTURE_X86_64 */
                skipea(addr, &pt, process, false);
                // Only check the 32 bit constant if this is a comparison.
                // For other operations this may be untagged and shouldn't be an address.
#ifndef HOSTARCHITECTURE_X86_64
                if ((opCode & 0x38) == 0x38)
                    process->ScanConstant(addr, pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
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
                    process->ScanConstant(addr, pt, PROCESS_RELOC_I386RELATIVE);
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
                    skipea(addr, &pt, process, false);
#ifndef HOSTARCHITECTURE_X86_64
                    process->ScanConstant(addr, pt, PROCESS_RELOC_DIRECT);
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
                process->ScanConstant(addr, pt, PROCESS_RELOC_DIRECT);
                pt += sizeof(PolyWord);
            }
            break;

        case 0x68: /* PUSH_32 */
            pt ++;
#ifndef HOSTARCHITECTURE_X86_64
            process->ScanConstant(addr, pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
            pt += 4;
            break;

        case 0x0f: /* ESCAPE */
            {
                pt++;
                switch (*pt)
                {
                case 0xb6: /* movzl */
                case 0xb7: // movzw
                case 0xc1: /* xaddl */
                case 0xaf: // imul
                    pt++; skipea(addr, &pt, process, false); break;

                case 0x80: case 0x81: case 0x82: case 0x83:
                case 0x84: case 0x85: case 0x86: case 0x87:
                case 0x88: case 0x89: case 0x8a: case 0x8b:
                case 0x8c: case 0x8d: case 0x8e: case 0x8f:
                    /* Conditional branches with 32-bit displacement. */
                    pt += 5; break;

                // These are SSE2 instructions
                case 0x10: case 0x11: case 0x58: case 0x5c: case 0x59: case 0x5e:
                case 0x2e: case 0x2a: case 0x54: case 0x57: case 0x5a:
                    pt++; skipea(addr, &pt, process, false); break;

                default: Crash("Unknown opcode %d at %p\n", *pt, pt);
                }
                break;
            }

        case 0xd8: case 0xd9: case 0xda: case 0xdb:
        case 0xdc: case 0xdd: case 0xde: case 0xdf: // Floating point escape instructions
            {
                pt++;
                if ((*pt & 0xe0) == 0xe0) pt++;
                else skipea(addr, &pt, process, false);
                break;
            }

        default: Crash("Unknown opcode %d at %p\n", *pt, pt);
        }
    }
}

// Increment the value contained in the first word of the mutex.
Handle X86TaskData::AtomicIncrement(Handle mutexp)
{
    PolyObject *p = DEREFHANDLE(mutexp);
    POLYUNSIGNED result = X86AsmAtomicIncrement(p);
    return this->saveVec.push(PolyWord::FromUnsigned(result));
}

// Release a mutex.  Because the atomic increment and decrement
// use the hardware LOCK prefix we can simply set this to one.
void X86TaskData::AtomicReset(Handle mutexp)
{
    DEREFHANDLE(mutexp)->Set(0, TAGGED(1));
}

static X86Dependent x86Dependent;

MachineDependent *machineDependent = &x86Dependent;

// Temporarily add the interpreter

#include "int_opcodes.h"
#include "polystring.h"
#include "arb.h"

#define arg1    (pc[0] + pc[1]*256)
#define arg2    (pc[2] + pc[3]*256)

const PolyWord True = TAGGED(1);
const PolyWord False = TAGGED(0);
const PolyWord Zero = TAGGED(0);

extern "C" {
    typedef POLYUNSIGNED(*callFastRts0)();
    typedef POLYUNSIGNED(*callFastRts1)(intptr_t);
    typedef POLYUNSIGNED(*callFastRts2)(intptr_t, intptr_t);
    typedef POLYUNSIGNED(*callFastRts3)(intptr_t, intptr_t, intptr_t);
    typedef POLYUNSIGNED(*callFastRts4)(intptr_t, intptr_t, intptr_t, intptr_t);
    typedef POLYUNSIGNED(*callFastRts5)(intptr_t, intptr_t, intptr_t, intptr_t, intptr_t);
    typedef POLYUNSIGNED(*callFullRts0)(PolyObject *);
    typedef POLYUNSIGNED(*callFullRts1)(PolyObject *, intptr_t);
    typedef POLYUNSIGNED(*callFullRts2)(PolyObject *, intptr_t, intptr_t);
    typedef POLYUNSIGNED(*callFullRts3)(PolyObject *, intptr_t, intptr_t, intptr_t);
    typedef double(*callRTSFtoF) (double);
    typedef double(*callRTSGtoF) (intptr_t);
}

static PLock mutexLock;

int X86TaskData::Interpret()
/* (Re)-enter the Poly code from C. */
{
    // These are temporary values used where one instruction jumps to
    // common code.
    POLYUNSIGNED    tailCount;
    stackItem       *tailPtr;
    POLYUNSIGNED    returnCount;
    POLYUNSIGNED    storeWords;
    POLYUNSIGNED    stackCheck;
    // Local values.  These are copies of member variables but are used so frequently that
    // it is important that access should be fast.
    POLYCODEPTR     pc;
    stackItem       *sp;

    LoadInterpreterState(pc, sp);

    // We may have taken an interrupt which has set an exception.
    if (this->raiseException) goto RAISE_EXCEPTION;

    for (;;) { /* Each instruction */
               //        char buff[1000];
               //        sprintf(buff, "addr = %p sp=%p instr=%02x *sp=%p\n", pc, sp, *pc, (*sp).stackAddr);
               //        OutputDebugStringA(buff);

        switch (*pc++) {

        case INSTR_enter_int: pc++; /* Skip the argument. */ break;

        case INSTR_jump8false:
        {
            PolyWord u = *sp++; /* Pop argument */
            if (u == True) { pc += 1; break; }
            /* else - false - take the jump */
        }

        case INSTR_jump8: pc += *pc + 1; break;

        case INSTR_jump16false:
        {
            PolyWord u = *sp++; /* Pop argument */
            if (u == True) { pc += 2; break; }
            /* else - false - take the jump */
        }

        case INSTR_jump16:
            pc += arg1 + 2; break;

        case INSTR_jump32False:
        {
            PolyWord u = *sp++; /* Pop argument */
            if (u == True) { pc += 4; break; }
            /* else - false - take the jump */
        }

        case INSTR_jump32:
        {
            // This is a 32-bit signed quantity on both 64-bits and 32-bits.
            POLYSIGNED offset = pc[3] & 0x80 ? -1 : 0;
            offset = (offset << 8) | pc[3];
            offset = (offset << 8) | pc[2];
            offset = (offset << 8) | pc[1];
            offset = (offset << 8) | pc[0];
            pc += offset + 4;
            break;
        }

        case INSTR_push_handler: /* Save the old handler value. */
            (*(--sp)).stackAddr = this->assemblyInterface.handlerRegister; /* Push old handler */
            break;

        case INSTR_setHandler8: /* Set up a handler */
            (*(--sp)).codeAddr = pc + *pc + 1; /* Address of handler */
            this->assemblyInterface.handlerRegister = sp;
            pc += 1;
            break;

        case INSTR_setHandler16: /* Set up a handler */
            (*(--sp)).codeAddr = pc + arg1 + 2; /* Address of handler */
            assemblyInterface.handlerRegister = sp;
            pc += 2;
            break;

        case INSTR_setHandler32: /* Set up a handler */
        {
            POLYUNSIGNED offset = pc[0] + (pc[1] << 8) + (pc[2] << 16) + (pc[3] << 24);
            (*(--sp)).codeAddr = pc + offset + 4; /* Address of handler */
            assemblyInterface.handlerRegister = sp;
            pc += 4;
            break;
        }

        case INSTR_deleteHandler: /* Delete handler retaining the result. */
        {
            stackItem u = *sp++;
            sp = this->assemblyInterface.handlerRegister;
            sp++; // Remove handler entry point
            this->assemblyInterface.handlerRegister = (*sp).stackAddr; // Restore old handler
            *sp = u; // Put back the result
            break;
        }

        case INSTR_case16:
        {
            // arg1 is the largest value that is in the range
            POLYSIGNED u = UNTAGGED(*sp++); /* Get the value */
            if (u > arg1 || u < 0) pc += (arg1 + 2) * 2; /* Out of range */
            else {
                pc += 2;
                pc += /* Index */pc[u * 2] + pc[u * 2 + 1] * 256;
            }
            break;
        }

        case INSTR_case32:
        {
            // arg1 is the number of cases i.e. one more than the largest value
            // This is followed by that number of 32-bit offsets.
            // If the value is out of range the default case is immediately after the table.
            POLYSIGNED u = UNTAGGED(*sp++); /* Get the value */
            if (u >= arg1 || u < 0) pc += 2 + arg1 * 4; /* Out of range */
            else
            {
                pc += 2;
                pc += /* Index */pc[u * 4] + (pc[u * 4 + 1] << 8) + (pc[u * 4 + 2] << 16) + (pc[u * 4 + 3] << 24);
            }
            break;
        }

        case INSTR_tail_3_b:
            tailCount = 3;
            tailPtr = sp + tailCount;
            sp = tailPtr + *pc;
            goto TAIL_CALL;

        case INSTR_tail_3_2:
            tailCount = 3;
            tailPtr = sp + tailCount;
            sp = tailPtr + 2;
            goto TAIL_CALL;

        case INSTR_tail_3_3:
            tailCount = 3;
            tailPtr = sp + tailCount;
            sp = tailPtr + 3;
            goto TAIL_CALL;

        case INSTR_tail_4_b:
            tailCount = 4;
            tailPtr = sp + tailCount;
            sp = tailPtr + *pc;
            goto TAIL_CALL;

        case INSTR_tail_b_b:
            tailCount = *pc;
            tailPtr = sp + tailCount;
            sp = tailPtr + pc[1];
            goto TAIL_CALL;

        case INSTR_tail:
            /* Tail recursive call. */
            /* Move items up the stack. */
            /* There may be an overlap if the function we are calling
            has more args than this one. */
            tailCount = arg1;
            tailPtr = sp + tailCount;
            sp = tailPtr + arg2;
        TAIL_CALL: /* For general case. */
            if (tailCount < 2) Crash("Invalid argument\n");
            for (; tailCount > 0; tailCount--) *(--sp) = *(--tailPtr);
            pc = (*sp++).codeAddr; /* Pop the original return address. */
                                   /* And drop through. */

        case INSTR_call_closure: /* Closure call. */
        {
            PolyObject *closure = (*sp).w().AsObjPtr();
            POLYCODEPTR newPc;
            ASSERT(closure->IsClosureObject());
            if (closure->IsClosureObject())
                newPc = *(POLYCODEPTR*)closure;
            else
                newPc = (*sp).w().AsObjPtr()->Get(0).AsCodePtr();
            sp--;
            *sp = sp[1];      /* Move closure up. */
            sp[1].codeAddr = pc; /* Save return address. */
            pc = newPc;    /* Get entry point. */
            ASSERT(*pc == 0xff); // There's an enter-int at the start of the code.
            this->taskPc = pc; // Update in case we're profiling
            break;
        }

        case INSTR_return_w:
            returnCount = arg1; /* Get no. of args to remove. */

        RETURN: /* Common code for return. */
            {
                stackItem result = *sp++; /* Result */
                sp++; /* Remove the link/closure */
                pc = (*sp++).codeAddr; /* Return address */
                sp += returnCount; /* Add on number of args. */
                if (*pc != 0xff)
                {
                    // We're returning to native code.
                    SaveInterpreterState(pc, sp);
                    assemblyInterface.p_rax = result;
                    (--assemblyInterface.stackPtr)[0].codeAddr = taskPc;
                    return 0;
                }
                *(--sp) = result; /* Result */
                this->taskPc = pc; // Update in case we're profiling
            }
            break;

        case INSTR_return_b: returnCount = *pc; goto RETURN;
        case INSTR_return_0: returnCount = 0; goto RETURN;
        case INSTR_return_1: returnCount = 1; goto RETURN;
        case INSTR_return_2: returnCount = 2; goto RETURN;
        case INSTR_return_3: returnCount = 3; goto RETURN;

        case INSTR_stackSize8:
            stackCheck = *pc++;
            goto STACKCHECK;

        case INSTR_stackSize16:
        {
            stackCheck = arg1; pc += 2;
        STACKCHECK:
            // Check there is space on the stack
            if (sp - stackCheck < assemblyInterface.stackLimit)
            {
                uintptr_t min_size = (this->stack->top - (PolyWord*)sp) + OVERFLOW_STACK_SIZE + stackCheck;
                SaveInterpreterState(pc, sp);
                CheckAndGrowStack(this, min_size);
                LoadInterpreterState(pc, sp);
                assemblyInterface.stackLimit = (stackItem*)this->stack->stack() + OVERFLOW_STACK_SIZE;
            }
            // Check for interrupts removed
            break;
        }

        case INSTR_pad: /* No-op */ break;

        case INSTR_raise_ex:
        {
        RAISE_EXCEPTION:
            this->raiseException = false;
            PolyException *exn = (PolyException*)((*sp).w().AsObjPtr());
            this->exception_arg = exn; /* Get exception data */
            sp = this->assemblyInterface.handlerRegister;
            pc = (*sp++).codeAddr;
            this->assemblyInterface.handlerRegister = (*sp++).stackAddr;
            if (*pc != 0xff)
            {
                SaveInterpreterState(pc, sp);
                // Handler is native code.
                assemblyInterface.p_rax = (PolyWord)exn;
                (--assemblyInterface.stackPtr)[0].codeAddr = taskPc;
                this->assemblyInterface.exceptionPacket = TAGGED(0); // Clear any exception
                return 0;
            }
            break;
        }

        case INSTR_get_store_w:
            // Get_store is now only used for mutually recursive closures.  It allocates mutable store
            // initialised to zero.
        {
            storeWords = arg1;
            pc += 2;
        GET_STORE:
            PolyObject *p = this->allocateMemory(storeWords, pc, sp);
            if (p == 0) goto RAISE_EXCEPTION;
            p->SetLengthWord(storeWords, F_MUTABLE_BIT);
            for (; storeWords > 0; ) p->Set(--storeWords, TAGGED(0)); /* Must initialise store! */
            *(--sp) = (PolyWord)p;
            break;
        }

        case INSTR_get_store_2: storeWords = 2; goto GET_STORE;
        case INSTR_get_store_3: storeWords = 3; goto GET_STORE;
        case INSTR_get_store_4: storeWords = 4; goto GET_STORE;
        case INSTR_get_store_b: storeWords = *pc; pc++; goto GET_STORE;

        case INSTR_tuple_w:
        {
            storeWords = arg1; pc += 2;
        TUPLE: /* Common code for tupling. */
            PolyObject *p = this->allocateMemory(storeWords, pc, sp);
            if (p == 0) goto RAISE_EXCEPTION; // Exception
            p->SetLengthWord(storeWords, 0);
            for (; storeWords > 0; ) p->Set(--storeWords, *sp++);
            *(--sp) = (PolyWord)p;
            break;
        }

        case INSTR_tuple_2: storeWords = 2; goto TUPLE;
        case INSTR_tuple_3: storeWords = 3; goto TUPLE;
        case INSTR_tuple_4: storeWords = 4; goto TUPLE;
        case INSTR_tuple_b: storeWords = *pc; pc++; goto TUPLE;

        case INSTR_local_w:
        {
            stackItem u = sp[arg1];
            *(--sp) = u;
            pc += 2;
            break;
        }

        case INSTR_indirect_w:
            *sp = (*sp).w().AsObjPtr()->Get(arg1); pc += 2; break;

        case INSTR_indContainerW:
            *sp = (*sp).stackAddr[arg1]; pc += 2; break;

        case INSTR_move_to_vec_w:
        {
            PolyWord u = *sp++;
            (*sp).w().AsObjPtr()->Set(arg1, u);
            pc += 2;
            break;
        }

        case INSTR_set_container_w:
        {
            PolyWord u = *sp++;
            (*sp).stackAddr[arg1] = u;
            pc += 2;
            break;
        }

        case INSTR_set_stack_val_w:
        {
            stackItem u = *sp++;
            sp[arg1 - 1] = u;
            pc += 2;
            break;
        }

        case INSTR_reset_w: sp += arg1; pc += 2; break;

        case INSTR_reset_r_w:
        {
            stackItem u = *sp;
            sp += arg1;
            *sp = u;
            pc += 2;
            break;
        }

        case INSTR_constAddr8:
            *(--sp) = *(PolyWord*)(pc + pc[0] + 1); pc += 1; break;

        case INSTR_constAddr16:
            *(--sp) = *(PolyWord*)(pc + arg1 + 2); pc += 2; break;

        case INSTR_constAddr32:
        {
            POLYUNSIGNED offset = pc[0] + (pc[1] << 8) + (pc[2] << 16) + (pc[3] << 24);
            *(--sp) = *(PolyWord*)(pc + offset + 4);
            pc += 4;
            break;
        }

        case INSTR_const_int_w: *(--sp) = TAGGED(arg1); pc += 2; break;

        case INSTR_jump_back8:
            pc -= *pc + 1;
            // Interrupt check removed
            break;

        case INSTR_jump_back16:
            pc -= arg1 + 1;
            // Interrupt check removed
            break;

        case INSTR_lock:
        {
            PolyObject *obj = (*sp).w().AsObjPtr();
            obj->SetLengthWord(obj->LengthWord() & ~_OBJ_MUTABLE_BIT);
            break;
        }

        case INSTR_ldexc: *(--sp) = this->exception_arg; break;

        case INSTR_local_b: { stackItem u = sp[*pc]; *(--sp) = u; pc += 1; break; }

        case INSTR_indirect_b:
            *sp = (*sp).w().AsObjPtr()->Get(*pc); pc += 1; break;

        case INSTR_indContainerB:
            *sp = (*sp).stackAddr[*pc]; pc += 1; break;

        case INSTR_move_to_vec_b:
        { PolyWord u = *sp++; (*sp).w().AsObjPtr()->Set(*pc, u); pc += 1; break; }

        case INSTR_set_container_b:
        { PolyWord u = *sp++; (*sp).stackAddr[*pc] = u; pc += 1; break; }

        case INSTR_set_stack_val_b:
        { stackItem u = *sp++; sp[*pc - 1] = u; pc += 1; break; }

        case INSTR_reset_b: sp += *pc; pc += 1; break;

        case INSTR_reset_r_b:
        { stackItem u = *sp; sp += *pc; *sp = u; pc += 1; break; }

        case INSTR_const_int_b: *(--sp) = TAGGED(*pc); pc += 1; break;

        case INSTR_setclosure:
        {
            PolyObject *srcPtr = (*sp++).w().AsObjPtr();
            // Temporarily we may actually pass the code address here or
            // we may pass an old-format closure.
            if (!srcPtr->IsCodeObject())
            {
                if (srcPtr->IsClosureObject())
                    srcPtr = *(PolyObject**)srcPtr;
                else srcPtr = srcPtr->Get(0).AsObjPtr();
            }
            PolyObject **destPtr = (PolyObject **)(*sp).w().AsObjPtr();
            *destPtr = srcPtr;
            break;
        }

        case INSTR_local_0: { stackItem u = sp[0]; *(--sp) = u; break; }
        case INSTR_local_1: { stackItem u = sp[1]; *(--sp) = u; break; }
        case INSTR_local_2: { stackItem u = sp[2]; *(--sp) = u; break; }
        case INSTR_local_3: { stackItem u = sp[3]; *(--sp) = u; break; }
        case INSTR_local_4: { stackItem u = sp[4]; *(--sp) = u; break; }
        case INSTR_local_5: { stackItem u = sp[5]; *(--sp) = u; break; }
        case INSTR_local_6: { stackItem u = sp[6]; *(--sp) = u; break; }
        case INSTR_local_7: { stackItem u = sp[7]; *(--sp) = u; break; }
        case INSTR_local_8: { stackItem u = sp[8]; *(--sp) = u; break; }
        case INSTR_local_9: { stackItem u = sp[9]; *(--sp) = u; break; }
        case INSTR_local_10: { stackItem u = sp[10]; *(--sp) = u; break; }
        case INSTR_local_11: { stackItem u = sp[11]; *(--sp) = u; break; }

        case INSTR_indirect_0:
            *sp = (*sp).w().AsObjPtr()->Get(0); break;

        case INSTR_indirect_1:
            *sp = (*sp).w().AsObjPtr()->Get(1); break;

        case INSTR_indirect_2:
            *sp = (*sp).w().AsObjPtr()->Get(2); break;

        case INSTR_indirect_3:
            *sp = (*sp).w().AsObjPtr()->Get(3); break;

        case INSTR_indirect_4:
            *sp = (*sp).w().AsObjPtr()->Get(4); break;

        case INSTR_indirect_5:
            *sp = (*sp).w().AsObjPtr()->Get(5); break;

        case INSTR_indContainer0:
            *sp = (*sp).stackAddr[0]; break;

        case INSTR_indContainer1:
            *sp = (*sp).stackAddr[1]; break;

        case INSTR_indContainer2:
            *sp = (*sp).stackAddr[2]; break;

        case INSTR_const_0: *(--sp) = Zero; break;
        case INSTR_const_1: *(--sp) = TAGGED(1); break;
        case INSTR_const_2: *(--sp) = TAGGED(2); break;
        case INSTR_const_3: *(--sp) = TAGGED(3); break;
        case INSTR_const_4: *(--sp) = TAGGED(4); break;
        case INSTR_const_10: *(--sp) = TAGGED(10); break;

            // Move-to-vec is now only used for closures.
        case INSTR_move_to_vec_0: { PolyWord u = *sp++; (*sp).w().AsObjPtr()->Set(0, u); break; }
        case INSTR_move_to_vec_1: { PolyWord u = *sp++; (*sp).w().AsObjPtr()->Set(1, u); break; }
        case INSTR_move_to_vec_2: { PolyWord u = *sp++; (*sp).w().AsObjPtr()->Set(2, u); break; }
        case INSTR_move_to_vec_3: { PolyWord u = *sp++; (*sp).w().AsObjPtr()->Set(3, u); break; }
        case INSTR_move_to_vec_4: { PolyWord u = *sp++; (*sp).w().AsObjPtr()->Set(4, u); break; }
        case INSTR_move_to_vec_5: { PolyWord u = *sp++; (*sp).w().AsObjPtr()->Set(5, u); break; }
        case INSTR_move_to_vec_6: { PolyWord u = *sp++; (*sp).w().AsObjPtr()->Set(6, u); break; }
        case INSTR_move_to_vec_7: { PolyWord u = *sp++; (*sp).w().AsObjPtr()->Set(7, u); break; }

        case INSTR_set_container_0: { PolyWord u = *sp++; (*sp).stackAddr[0] = u; break; }
        case INSTR_set_container_1: { PolyWord u = *sp++; (*sp).stackAddr[1] = u; break; }
        case INSTR_set_container_2: { PolyWord u = *sp++; (*sp).stackAddr[2] = u; break; }
        case INSTR_set_container_3: { PolyWord u = *sp++; (*sp).stackAddr[3] = u; break; }
        case INSTR_set_container_4: { PolyWord u = *sp++; (*sp).stackAddr[4] = u; break; }

        case INSTR_reset_r_1: { stackItem u = *sp; sp += 1; *sp = u; break; }
        case INSTR_reset_r_2: { stackItem u = *sp; sp += 2; *sp = u; break; }
        case INSTR_reset_r_3: { stackItem u = *sp; sp += 3; *sp = u; break; }

        case INSTR_reset_1: sp += 1; break;
        case INSTR_reset_2: sp += 2; break;

        case INSTR_stack_container:
        {
            POLYUNSIGNED words = arg1; pc += 2;
            while (words-- > 0) *(--sp) = Zero;
            sp--;
            (*sp).stackAddr = sp + 1;
            break;
        }

        case INSTR_tuple_container: /* Create a tuple from a container. */
        {
            storeWords = arg1;
            PolyObject *t = this->allocateMemory(storeWords, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(storeWords, 0);
            for (; storeWords > 0; )
            {
                storeWords--;
                t->Set(storeWords, (*sp).stackAddr[storeWords]);
            }
            *sp = (PolyWord)t;
            pc += 2;
            break;
        }

        case INSTR_callFastRTS0:
        {
            callFastRts0 doCall = *(callFastRts0*)(*sp++).w().AsObjPtr();
            POLYUNSIGNED result = doCall();
            *(--sp) = PolyWord::FromUnsigned(result);
            break;
        }

        case INSTR_callFastRTS1:
        {
            callFastRts1 doCall = *(callFastRts1*)(*sp++).w().AsObjPtr();
            intptr_t rtsArg1 = (*sp++).argValue;
            POLYUNSIGNED result = doCall(rtsArg1);
            *(--sp) = PolyWord::FromUnsigned(result);
            break;
        }

        case INSTR_callFastRTS2:
        {
            callFastRts2 doCall = *(callFastRts2*)(*sp++).w().AsObjPtr();
            intptr_t rtsArg2 = (*sp++).argValue; // Pop off the args, last arg first.
            intptr_t rtsArg1 = (*sp++).argValue;
            POLYUNSIGNED result = doCall(rtsArg1, rtsArg2);
            *(--sp) = PolyWord::FromUnsigned(result);
            break;
        }

        case INSTR_callFastRTS3:
        {
            callFastRts3 doCall = *(callFastRts3*)(*sp++).w().AsObjPtr();
            intptr_t rtsArg3 = (*sp++).argValue; // Pop off the args, last arg first.
            intptr_t rtsArg2 = (*sp++).argValue;
            intptr_t rtsArg1 = (*sp++).argValue;
            POLYUNSIGNED result = doCall(rtsArg1, rtsArg2, rtsArg3);
            *(--sp) = PolyWord::FromUnsigned(result);
            break;
        }

        case INSTR_callFastRTS4:
        {
            callFastRts4 doCall = *(callFastRts4*)(*sp++).w().AsObjPtr();
            intptr_t rtsArg4 = (*sp++).argValue; // Pop off the args, last arg first.
            intptr_t rtsArg3 = (*sp++).argValue;
            intptr_t rtsArg2 = (*sp++).argValue;
            intptr_t rtsArg1 = (*sp++).argValue;
            POLYUNSIGNED result = doCall(rtsArg1, rtsArg2, rtsArg3, rtsArg4);
            *(--sp) = PolyWord::FromUnsigned(result);
            break;
        }

        case INSTR_callFastRTS5:
        {
            callFastRts5 doCall = *(callFastRts5*)(*sp++).w().AsObjPtr();
            intptr_t rtsArg5 = (*sp++).argValue; // Pop off the args, last arg first.
            intptr_t rtsArg4 = (*sp++).argValue;
            intptr_t rtsArg3 = (*sp++).argValue;
            intptr_t rtsArg2 = (*sp++).argValue;
            intptr_t rtsArg1 = (*sp++).argValue;
            POLYUNSIGNED result = doCall(rtsArg1, rtsArg2, rtsArg3, rtsArg4, rtsArg5);
            *(--sp) = PolyWord::FromUnsigned(result);
            break;
        }

        case INSTR_callFullRTS0:
        {
            callFullRts0 doCall = *(callFullRts0*)(*sp++).w().AsObjPtr();
            this->raiseException = false;
            SaveInterpreterState(pc, sp);
            SetMemRegisters();
            POLYUNSIGNED result = doCall(this->threadObject);
            stackItem saveEx = assemblyInterface.exceptionPacket;
            SaveMemRegisters();
            assemblyInterface.exceptionPacket = saveEx;
            LoadInterpreterState(pc, sp);
            // If this raised an exception 
            if (this->raiseException)
                return 0;
            *(--sp) = PolyWord::FromUnsigned(result);
            break;
        }

        case INSTR_callFullRTS1:
        {
            callFullRts1 doCall = *(callFullRts1*)(*sp++).w().AsObjPtr();
            intptr_t rtsArg1 = (*sp++).argValue;
            this->raiseException = false;
            SaveInterpreterState(pc, sp);
            SetMemRegisters();
            POLYUNSIGNED result = doCall(this->threadObject, rtsArg1);
            stackItem saveEx = assemblyInterface.exceptionPacket;
            SaveMemRegisters();
            assemblyInterface.exceptionPacket = saveEx;
            LoadInterpreterState(pc, sp);
            // If this raised an exception 
            if (this->raiseException)
                return 0;
            *(--sp) = PolyWord::FromUnsigned(result);
            break;
        }

        case INSTR_callFullRTS2:
        {
            callFullRts2 doCall = *(callFullRts2*)(*sp++).w().AsObjPtr();
            intptr_t rtsArg2 = (*sp++).argValue; // Pop off the args, last arg first.
            intptr_t rtsArg1 = (*sp++).argValue;
            this->raiseException = false;
            SaveInterpreterState(pc, sp);
            SetMemRegisters();
            POLYUNSIGNED result = doCall(this->threadObject, rtsArg1, rtsArg2);
            stackItem saveEx = assemblyInterface.exceptionPacket;
            SaveMemRegisters();
            assemblyInterface.exceptionPacket = saveEx;
            LoadInterpreterState(pc, sp);
            // If this raised an exception
            if (this->raiseException)
                return 0;
            *(--sp) = PolyWord::FromUnsigned(result);
            break;
        }

        case INSTR_callFullRTS3:
        {
            callFullRts3 doCall = *(callFullRts3*)(*sp++).w().AsObjPtr();
            intptr_t rtsArg3 = (*sp++).argValue; // Pop off the args, last arg first.
            intptr_t rtsArg2 = (*sp++).argValue;
            intptr_t rtsArg1 = (*sp++).argValue;
            this->raiseException = false;
            SaveInterpreterState(pc, sp);
            SetMemRegisters();
            POLYUNSIGNED result = doCall(this->threadObject, rtsArg1, rtsArg2, rtsArg3);
            stackItem saveEx = assemblyInterface.exceptionPacket;
            SaveMemRegisters();
            assemblyInterface.exceptionPacket = saveEx;
            LoadInterpreterState(pc, sp);
            // If this raised an exception we return and deal with
            // it as machine code.
            if (this->raiseException)
                return 0;
            *(--sp) = PolyWord::FromUnsigned(result);
            break;
        }

        case INSTR_callFastFtoF:
        {
            // Floating point call.  The call itself does not allocate but we
            // need to put the result into a "box".
            callRTSFtoF doCall = *(callRTSFtoF*)(*sp++).w().AsObjPtr();
            PolyWord rtsArg1 = *sp++;
            double argument = unboxDouble(rtsArg1);
            // Allocate memory for the result.
            double result = doCall(argument);
            PolyObject *t = boxDouble(result, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            *(--sp) = (PolyWord)t;
            break;
        }

        case INSTR_callFastGtoF:
        {
            // Call that takes a POLYUNSIGNED argument and returns a double.
            callRTSGtoF doCall = *(callRTSGtoF*)(*sp++).w().AsObjPtr();
            intptr_t rtsArg1 = (*sp++).argValue;
            // Allocate memory for the result.
            double result = doCall(rtsArg1);
            PolyObject *t = boxDouble(result, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            *(--sp) = (PolyWord)t;
            break;
        }

        case INSTR_notBoolean:
            *sp = ((*sp).w() == True) ? False : True; break;

        case INSTR_isTagged:
            *sp = (*sp).w().IsTagged() ? True : False; break;

        case INSTR_cellLength:
            /* Return the length word. */
            *sp = TAGGED((*sp).w().AsObjPtr()->Length());
            break;

        case INSTR_cellFlags:
        {
            PolyObject *p = (*sp).w().AsObjPtr();
            POLYUNSIGNED f = (p->LengthWord()) >> OBJ_PRIVATE_FLAGS_SHIFT;
            *sp = TAGGED(f);
            break;
        }

        case INSTR_clearMutable:
        {
            PolyObject *obj = (*sp).w().AsObjPtr();
            POLYUNSIGNED lengthW = obj->LengthWord();
            /* Clear the mutable bit. */
            obj->SetLengthWord(lengthW & ~_OBJ_MUTABLE_BIT);
            *sp = Zero;
            break;
        }

        case INSTR_stringLength: // Now replaced by loadUntagged
            *sp = TAGGED(((PolyStringObject*)(*sp).w().AsObjPtr())->length);
            break;

        case INSTR_atomicIncr:
        {
            PLocker l(&mutexLock);
            PolyObject *p = (*sp).w().AsObjPtr();
            PolyWord newValue = TAGGED(UNTAGGED(p->Get(0)) + 1);
            p->Set(0, newValue);
            *sp = newValue;
            break;
        }

        case INSTR_atomicDecr:
        {
            PLocker l(&mutexLock);
            PolyObject *p = (*sp).w().AsObjPtr();
            PolyWord newValue = TAGGED(UNTAGGED(p->Get(0)) - 1);
            p->Set(0, newValue);
            *sp = newValue;
            break;
        }

        case INSTR_atomicReset:
        {
            // This is needed in the interpreted version otherwise there
            // is a chance that we could set the value to zero while another
            // thread is between getting the old value and setting it to the new value.
            PLocker l(&mutexLock);
            PolyObject *p = (*sp).w().AsObjPtr();
            p->Set(0, TAGGED(1)); // Set this to released.
            *sp = TAGGED(0); // Push the unit result
            break;
        }

        case INSTR_longWToTagged:
        {
            // Extract the first word and return it as a tagged value.  This loses the top-bit
            POLYUNSIGNED wx = (*sp).w().AsObjPtr()->Get(0).AsUnsigned();
            *sp = TAGGED(wx);
            break;
        }

        case INSTR_signedToLongW:
        {
            // Shift the tagged value to remove the tag and put it into the first word.
            // The original sign bit is copied in the shift.
            intptr_t wx = (*sp).w().UnTagged();
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(intptr_t*)t = wx;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_unsignedToLongW:
        {
            // As with the above except the value is treated as an unsigned
            // value and the top bit is zero.
            uintptr_t wx = (*sp).w().UnTaggedUnsigned();
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(uintptr_t*)t = wx;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_realAbs:
        {
            PolyObject *t = this->boxDouble(fabs(unboxDouble(*sp)), pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_realNeg:
        {
            PolyObject *t = this->boxDouble(-(unboxDouble(*sp)), pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_floatFixedInt:
        {
            POLYSIGNED u = UNTAGGED(*sp);
            PolyObject *t = this->boxDouble((double)u, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_equalWord:
        {
            PolyWord u = *sp++;
            *sp = u == (*sp).w() ? True : False;
            break;
        }

        case INSTR_lessSigned:
        {
            PolyWord u = *sp++;
            *sp = ((*sp).w().AsSigned() < u.AsSigned()) ? True : False;
            break;
        }

        case INSTR_lessUnsigned:
        {
            PolyWord u = *sp++;
            *sp = ((*sp).w().AsUnsigned() < u.AsUnsigned()) ? True : False;
            break;
        }

        case INSTR_lessEqSigned:
        {
            PolyWord u = *sp++;
            *sp = ((*sp).w().AsSigned() <= u.AsSigned()) ? True : False;
            break;
        }

        case INSTR_lessEqUnsigned:
        {
            PolyWord u = *sp++;
            *sp = ((*sp).w().AsUnsigned() <= u.AsUnsigned()) ? True : False;
            break;
        }

        case INSTR_greaterSigned:
        {
            PolyWord u = *sp++;
            *sp = ((*sp).w().AsSigned() > u.AsSigned()) ? True : False;
            break;
        }

        case INSTR_greaterUnsigned:
        {
            PolyWord u = *sp++;
            *sp = ((*sp).w().AsUnsigned() > u.AsUnsigned()) ? True : False;
            break;
        }

        case INSTR_greaterEqSigned:
        {
            PolyWord u = *sp++;
            *sp = ((*sp).w().AsSigned() >= u.AsSigned()) ? True : False;
            break;
        }

        case INSTR_greaterEqUnsigned:
        {
            PolyWord u = *sp++;
            *sp = ((*sp).w().AsUnsigned() >= u.AsUnsigned()) ? True : False;
            break;
        }

        case INSTR_fixedAdd:
        {
            PolyWord x = *sp++;
            PolyWord y = (*sp).w();
            POLYSIGNED t = UNTAGGED(x) + UNTAGGED(y);
            if (t <= MAXTAGGED && t >= -MAXTAGGED - 1)
                *sp = TAGGED(t);
            else
            {
                *(--sp) = (PolyWord)overflowPacket;
                goto RAISE_EXCEPTION;
            }
            break;
        }

        case INSTR_fixedSub:
        {
            PolyWord x = *sp++;
            PolyWord y = (*sp).w();
            POLYSIGNED t = UNTAGGED(y) - UNTAGGED(x);
            if (t <= MAXTAGGED && t >= -MAXTAGGED - 1)
                *sp = TAGGED(t);
            else
            {
                *(--sp) = (PolyWord)overflowPacket;
                goto RAISE_EXCEPTION;
            }
            break;
        }

        case INSTR_fixedMult:
        {
            // We need to detect overflow.  There doesn't seem to be any convenient way to do
            // this so we use the arbitrary precision package and check whether the result is short.
            // Clang and GCC 5.0 have __builtin_mul_overflow which will do this but GCC 5.0 is not
            // currently (July 2016) in Debian stable.
            Handle reset = this->saveVec.mark();
            Handle pushedArg1 = this->saveVec.push(*sp++);
            Handle pushedArg2 = this->saveVec.push(*sp);
            Handle result = mult_longc(this, pushedArg2, pushedArg1);
            PolyWord res = result->Word();
            this->saveVec.reset(reset);
            if (!res.IsTagged())
            {
                *(--sp) = (PolyWord)overflowPacket;
                goto RAISE_EXCEPTION;
            }
            *sp = res;
            break;
        }

        case INSTR_fixedQuot:
        {
            // Zero and overflow are checked for in ML.
            POLYSIGNED u = UNTAGGED(*sp++);
            PolyWord y = (*sp).w();
            *sp = TAGGED(UNTAGGED(y) / u);
            break;
        }

        case INSTR_fixedRem:
        {
            // Zero and overflow are checked for in ML.
            POLYSIGNED u = UNTAGGED(*sp++);
            PolyWord y = (*sp).w();
            *sp = TAGGED(UNTAGGED(y) % u);
            break;
        }

        case INSTR_wordAdd:
        {
            PolyWord u = *sp++;
            // Because we're not concerned with overflow we can just add the values and subtract the tag.
            *sp = PolyWord::FromUnsigned((*sp).w().AsUnsigned() + u.AsUnsigned() - TAGGED(0).AsUnsigned());
            break;
        }

        case INSTR_wordSub:
        {
            PolyWord u = *sp++;
            *sp = PolyWord::FromUnsigned((*sp).w().AsUnsigned() - u.AsUnsigned() + TAGGED(0).AsUnsigned());
            break;
        }

        case INSTR_wordMult:
        {
            PolyWord u = *sp++;
            *sp = TAGGED(UNTAGGED_UNSIGNED(*sp) * UNTAGGED_UNSIGNED(u));
            break;
        }

        case INSTR_wordDiv:
        {
            POLYUNSIGNED u = UNTAGGED_UNSIGNED(*sp++);
            // Detection of zero is done in ML
            *sp = TAGGED(UNTAGGED_UNSIGNED(*sp) / u); break;
        }

        case INSTR_wordMod:
        {
            POLYUNSIGNED u = UNTAGGED_UNSIGNED(*sp++);
            *sp = TAGGED(UNTAGGED_UNSIGNED(*sp) % u);
            break;
        }

        case INSTR_wordAnd:
        {
            PolyWord u = *sp++;
            // Since both of these should be tagged the tag bit will be preserved.
            *sp = PolyWord::FromUnsigned((*sp).w().AsUnsigned() & u.AsUnsigned());
            break;
        }

        case INSTR_wordOr:
        {
            PolyWord u = *sp++;
            // Since both of these should be tagged the tag bit will be preserved.
            *sp = PolyWord::FromUnsigned((*sp).w().AsUnsigned() | u.AsUnsigned());
            break;
        }

        case INSTR_wordXor:
        {
            PolyWord u = *sp++;
            // This will remove the tag bit so it has to be reinstated.
            *sp = PolyWord::FromUnsigned(((*sp).w().AsUnsigned() ^ u.AsUnsigned()) | TAGGED(0).AsUnsigned());
            break;
        }

        case INSTR_wordShiftLeft:
        {
            // ML requires shifts greater than a word to return zero. 
            // That's dealt with at the higher level.
            PolyWord u = *sp++;
            *sp = TAGGED(UNTAGGED_UNSIGNED(*sp) << UNTAGGED_UNSIGNED(u));
            break;
        }

        case INSTR_wordShiftRLog:
        {
            PolyWord u = *sp++;
            *sp = TAGGED(UNTAGGED_UNSIGNED(*sp) >> UNTAGGED_UNSIGNED(u));
            break;
        }

        case INSTR_wordShiftRArith:
        {
            PolyWord u = *sp++;
            // Strictly speaking, C does not require that this uses
            // arithmetic shifting so we really ought to set the
            // high-order bits explicitly.
            *sp = TAGGED(UNTAGGED(*sp) >> UNTAGGED(u));
            break;
        }

        case INSTR_allocByteMem:
        {
            // Allocate byte segment.  This does not need to be initialised.
            POLYUNSIGNED flags = UNTAGGED_UNSIGNED(*sp++);
            POLYUNSIGNED length = UNTAGGED_UNSIGNED(*sp);
            PolyObject *t = this->allocateMemory(length, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION; // Exception
            t->SetLengthWord(length, (byte)flags);
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_lgWordEqual:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            *sp = wx == wy ? True : False;
            break;
        }

        case INSTR_lgWordNotequal:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            *sp = wx != wy ? True : False;
            break;
        }

        case INSTR_lgWordLess:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            *sp = (wy < wx) ? True : False;
            break;
        }

        case INSTR_lgWordLessEq:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            *sp = (wy <= wx) ? True : False;
            break;
        }

        case INSTR_lgWordGreater:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            *sp = (wy > wx) ? True : False;
            break;
        }

        case INSTR_lgWordGreaterEq:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            *sp = (wy >= wx) ? True : False;
            break;
        }

        case INSTR_lgWordAdd:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(uintptr_t*)t = wy + wx;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_lgWordSub:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(uintptr_t*)t = wy - wx;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_lgWordMult:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(uintptr_t*)t = wy*wx;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_lgWordDiv:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(uintptr_t*)t = wy / wx;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_lgWordMod:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(uintptr_t*)t = wy%wx;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_lgWordAnd:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(uintptr_t*)t = wy&wx;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_lgWordOr:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(uintptr_t*)t = wy | wx;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_lgWordXor:
        {
            uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(uintptr_t*)t = wy^wx;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_lgWordShiftLeft:
        {
            // The shift amount is a tagged word not a boxed large word
            POLYUNSIGNED wx = UNTAGGED_UNSIGNED(*sp++);
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(uintptr_t*)t = wy << wx;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_lgWordShiftRLog:
        {
            // The shift amount is a tagged word not a boxed large word
            POLYUNSIGNED wx = UNTAGGED_UNSIGNED(*sp++);
            uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(uintptr_t*)t = wy >> wx;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_lgWordShiftRArith:
        {
            // The shift amount is a tagged word not a boxed large word
            POLYUNSIGNED wx = UNTAGGED_UNSIGNED(*sp++);
            intptr_t wy = *(intptr_t*)((*sp).w().AsObjPtr());
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(intptr_t*)t = wy >> wx;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_realEqual:
        {
            double u = unboxDouble(*sp++);
            *sp = u == unboxDouble(*sp) ? True : False;
            break;
        }

        case INSTR_realLess:
        {
            double u = unboxDouble(*sp++);
            *sp = unboxDouble(*sp) < u ? True : False;
            break;
        }

        case INSTR_realLessEq:
        {
            double u = unboxDouble(*sp++);
            *sp = unboxDouble(*sp) <= u ? True : False;
            break;
        }

        case INSTR_realGreater:
        {
            double u = unboxDouble(*sp++);
            *sp = unboxDouble(*sp) > u ? True : False;
            break;
        }

        case INSTR_realGreaterEq:
        {
            double u = unboxDouble(*sp++);
            *sp = unboxDouble(*sp) >= u ? True : False;
            break;
        }

        case INSTR_realAdd:
        {
            double u = unboxDouble(*sp++);
            double v = unboxDouble(*sp);
            PolyObject *t = this->boxDouble(v + u, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_realSub:
        {
            double u = unboxDouble(*sp++);
            double v = unboxDouble(*sp);
            PolyObject *t = this->boxDouble(v - u, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_realMult:
        {
            double u = unboxDouble(*sp++);
            double v = unboxDouble(*sp);
            PolyObject *t = this->boxDouble(v*u, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_realDiv:
        {
            double u = unboxDouble(*sp++);
            double v = unboxDouble(*sp);
            PolyObject *t = this->boxDouble(v / u, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_getThreadId:
            *(--sp) = (PolyWord)this->threadObject;
            break;

        case INSTR_allocWordMemory:
        {
            // Allocate word segment.  This must be initialised.
            // We mustn't pop the initialiser until after any potential GC.
            POLYUNSIGNED length = UNTAGGED_UNSIGNED(sp[2].w());
            PolyObject *t = this->allocateMemory(length, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            PolyWord initialiser = *sp++;
            POLYUNSIGNED flags = UNTAGGED_UNSIGNED(*sp++);
            t->SetLengthWord(length, (byte)flags);
            *sp = (PolyWord)t;
            // Have to initialise the data.
            for (; length > 0; ) t->Set(--length, initialiser);
            break;
        }

        case INSTR_alloc_ref:
        {
            // Allocate a single word mutable cell.  This is more common than allocWordMemory on its own.
            PolyObject *t = this->allocateMemory(1, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            PolyWord initialiser = (*sp).w();
            t->SetLengthWord(1, F_MUTABLE_BIT);
            t->Set(0, initialiser);
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_loadMLWord:
        {
            // The values on the stack are base, index and offset.
            POLYUNSIGNED offset = UNTAGGED(*sp++);
            POLYUNSIGNED index = UNTAGGED(*sp++);
            PolyObject *p = (PolyObject*)((*sp).w().AsCodePtr() + offset);
            *sp = p->Get(index);
            break;
        }

        case INSTR_loadMLByte:
        {
            // The values on the stack are base and index.
            POLYUNSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = (*sp).w().AsCodePtr();
            *sp = TAGGED(p[index]); // Have to tag the result
            break;
        }

        case INSTR_loadC8:
        {
            // This is similar to loadMLByte except that the base address is a boxed large-word.
            // Also the index is SIGNED.
            POLYSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = *((byte **)((*sp).w().AsObjPtr()));
            *sp = TAGGED(p[index]); // Have to tag the result
            break;
        }

        case INSTR_loadC16:
        {
            // This and the other loads are similar to loadMLWord with separate
            // index and offset values.
            POLYSIGNED offset = UNTAGGED(*sp++);
            POLYSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = *((byte **)((*sp).w().AsObjPtr())) + offset;
            POLYUNSIGNED r = ((uint16_t*)p)[index];
            *sp = TAGGED(r);
            break;
        }

        case INSTR_loadC32:
        {
            POLYSIGNED offset = UNTAGGED(*sp++);
            POLYSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = *((byte **)((*sp).w().AsObjPtr())) + offset;
            uintptr_t r = ((uint32_t*)p)[index];
#ifdef IS64BITS
            // This is tagged in 64-bit mode
            *sp = TAGGED(r);
#else
            // But boxed in 32-bit mode.
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(uintptr_t*)t = r;
            *sp = (PolyWord)t;
#endif
            break;
        }

#if (defined(IS64BITS) || defined(POLYML32IN64))
        case INSTR_loadC64:
        {
            POLYSIGNED offset = UNTAGGED(*sp++);
            POLYSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = *((byte **)((*sp).w().AsObjPtr())) + offset;
            uintptr_t r = ((uint64_t*)p)[index];
            // This must be boxed.
            PolyObject *t = this->allocateMemory(LGWORDSIZE, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
            *(uintptr_t*)t = r;
            *sp = (PolyWord)t;
            break;
        }
#endif

        case INSTR_loadCFloat:
        {
            POLYSIGNED offset = UNTAGGED(*sp++);
            POLYSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = *((byte **)((*sp).w().AsObjPtr())) + offset;
            double r = ((float*)p)[index];
            // This must be boxed.
            PolyObject *t = this->boxDouble(r, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_loadCDouble:
        {
            POLYSIGNED offset = UNTAGGED(*sp++);
            POLYSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = *((byte **)((*sp).w().AsObjPtr())) + offset;
            double r = ((double*)p)[index];
            // This must be boxed.
            PolyObject *t = this->boxDouble(r, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_loadUntagged:
        {
            // The values on the stack are base, index and offset.
            POLYUNSIGNED offset = UNTAGGED(*sp++);
            POLYUNSIGNED index = UNTAGGED(*sp++);
            PolyObject *p = (PolyObject*)((*sp).w().AsCodePtr() + offset);
            *sp = TAGGED(p->Get(index).AsUnsigned());
            break;
        }

        case INSTR_storeMLWord:
        {
            PolyWord toStore = *sp++;
            POLYUNSIGNED offset = UNTAGGED(*sp++);
            POLYUNSIGNED index = UNTAGGED(*sp++);
            PolyObject *p = (PolyObject*)((*sp).w().AsCodePtr() + offset);
            p->Set(index, toStore);
            *sp = Zero;
            break;
        }

        case INSTR_storeMLByte:
        {
            POLYUNSIGNED toStore = UNTAGGED(*sp++);
            POLYUNSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = (*sp).w().AsCodePtr();
            p[index] = (byte)toStore;
            *sp = Zero;
            break;
        }

        case INSTR_storeC8:
        {
            // Similar to storeMLByte except that the base address is a boxed large-word.
            POLYUNSIGNED toStore = UNTAGGED(*sp++);
            POLYSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = *((byte **)((*sp).w().AsObjPtr()));
            p[index] = (byte)toStore;
            *sp = Zero;
            break;
        }

        case INSTR_storeC16:
        {
            uint16_t toStore = (uint16_t)UNTAGGED(*sp++);
            POLYSIGNED offset = UNTAGGED(*sp++);
            POLYSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = *((byte **)((*sp).w().AsObjPtr())) + offset;
            ((uint16_t*)p)[index] = toStore;
            *sp = Zero;
            break;
        }

        case INSTR_storeC32:
        {
#ifdef IS64BITS
            // This is a tagged value in 64-bit mode.
            uint32_t toStore = (uint32_t)UNTAGGED(*sp++);
#else
            // but a boxed value in 32-bit mode.
            uint32_t toStore = (uint32_t)(*(uintptr_t*)((*sp++).w().AsObjPtr()));
#endif
            POLYSIGNED offset = UNTAGGED(*sp++);
            POLYSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = *((byte **)((*sp).w().AsObjPtr())) + offset;
            ((uint32_t*)p)[index] = toStore;
            *sp = Zero;
            break;
        }

#if (defined(IS64BITS) || defined(POLYML32IN64))
        case INSTR_storeC64:
        {
            // This is a boxed value.
            uint64_t toStore = *(uintptr_t*)((*sp++).w().AsObjPtr());
            POLYSIGNED offset = UNTAGGED(*sp++);
            POLYSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = *((byte **)((*sp).w().AsObjPtr())) + offset;
            ((uint64_t*)p)[index] = toStore;
            *sp = Zero;
            break;
        }
#endif

        case INSTR_storeCFloat:
        {
            // This is a boxed value.
            float toStore = (float)unboxDouble(*sp++);
            POLYSIGNED offset = UNTAGGED(*sp++);
            POLYSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = *((byte **)((*sp).w().AsObjPtr())) + offset;
            ((float*)p)[index] = toStore;
            *sp = Zero;
            break;
        }

        case INSTR_storeCDouble:
        {
            // This is a boxed value.
            double toStore = unboxDouble(*sp++);
            POLYSIGNED offset = UNTAGGED(*sp++);
            POLYSIGNED index = UNTAGGED(*sp++);
            POLYCODEPTR p = *((byte **)((*sp).w().AsObjPtr())) + offset;
            ((double*)p)[index] = toStore;
            *sp = Zero;
            break;
        }

        case INSTR_storeUntagged:
        {
            PolyWord toStore = PolyWord::FromUnsigned(UNTAGGED_UNSIGNED(*sp++));
            POLYUNSIGNED offset = UNTAGGED(*sp++);
            POLYUNSIGNED index = UNTAGGED(*sp++);
            PolyObject *p = (PolyObject*)((*sp).w().AsCodePtr() + offset);
            p->Set(index, toStore);
            *sp = Zero;
            break;
        }

        case INSTR_blockMoveWord:
        {
            // The offsets are byte counts but the the indexes are in words.
            POLYUNSIGNED length = UNTAGGED_UNSIGNED(*sp++);
            POLYUNSIGNED destOffset = UNTAGGED_UNSIGNED(*sp++);
            POLYUNSIGNED destIndex = UNTAGGED_UNSIGNED(*sp++);
            PolyObject *dest = (PolyObject*)((*sp++).w().AsCodePtr() + destOffset);
            POLYUNSIGNED srcOffset = UNTAGGED_UNSIGNED(*sp++);
            POLYUNSIGNED srcIndex = UNTAGGED_UNSIGNED(*sp++);
            PolyObject *src = (PolyObject*)((*sp).w().AsCodePtr() + srcOffset);
            for (POLYUNSIGNED u = 0; u < length; u++) dest->Set(destIndex + u, src->Get(srcIndex + u));
            *sp = Zero;
            break;
        }

        case INSTR_blockMoveByte:
        {
            POLYUNSIGNED length = UNTAGGED_UNSIGNED(*sp++);
            POLYUNSIGNED destOffset = UNTAGGED_UNSIGNED(*sp++);
            POLYCODEPTR dest = (*sp++).w().AsCodePtr();
            POLYUNSIGNED srcOffset = UNTAGGED_UNSIGNED(*sp++);
            POLYCODEPTR src = (*sp).w().AsCodePtr();
            memcpy(dest + destOffset, src + srcOffset, length);
            *sp = Zero;
            break;
        }

        case INSTR_blockEqualByte:
        {
            POLYUNSIGNED length = UNTAGGED_UNSIGNED(*sp++);
            POLYUNSIGNED arg2Offset = UNTAGGED_UNSIGNED(*sp++);
            POLYCODEPTR arg2Ptr = (*sp++).w().AsCodePtr();
            POLYUNSIGNED arg1Offset = UNTAGGED_UNSIGNED(*sp++);
            POLYCODEPTR arg1Ptr = (*sp).w().AsCodePtr();
            *sp = memcmp(arg1Ptr + arg1Offset, arg2Ptr + arg2Offset, length) == 0 ? True : False;
            break;
        }

        case INSTR_blockCompareByte:
        {
            POLYUNSIGNED length = UNTAGGED_UNSIGNED(*sp++);
            POLYUNSIGNED arg2Offset = UNTAGGED_UNSIGNED(*sp++);
            POLYCODEPTR arg2Ptr = (*sp++).w().AsCodePtr();
            POLYUNSIGNED arg1Offset = UNTAGGED_UNSIGNED(*sp++);
            POLYCODEPTR arg1Ptr = (*sp).w().AsCodePtr();
            int result = memcmp(arg1Ptr + arg1Offset, arg2Ptr + arg2Offset, length);
            *sp = result == 0 ? TAGGED(0) : result < 0 ? TAGGED(-1) : TAGGED(1);
            break;
        }

        case 0xff: // Enter interpreter.  A no-op when already in the interpreter
        {
            pc += 3;
            break;
        }

        default: Crash("Unknown instruction %x\n", pc[-1]);

        } /* switch */
    } /* for */
    return 0;
} /* MD_switch_to_poly */
