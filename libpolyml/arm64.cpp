/*
    Machine-dependent code for ARM64

    Copyright David C.J. Matthews 2020-21.

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

// Currently this is just copied from the interpreted version.

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "globals.h"
#include "machine_dep.h"
#include "sys.h"
#include "profiling.h"
#include "arb.h"
#include "processes.h"
#include "run_time.h"
#include "gc.h"
#include "diagnostics.h"
#include "polystring.h"
#include "save_vec.h"
#include "memmgr.h"
#include "scanaddrs.h"
#include "rtsentry.h"
#include "bytecode.h"

/*
* ARM64 register use:
* X0        First argument and return value
* X1-X7     Second-eighth argument
* X8        Indirect result (C), ML closure pointer on entry
* X9-X15    Volatile scratch registers
* X16-17    Intra-procedure-call (C).  Only used for special cases in ML.
* X18       Platform register. Not used in ML.
* X19-X25   Non-volatile (C).  Scratch registers (ML).
* X26       ML assembly interface pointer.  Non-volatile (C).
* X27       ML Heap allocation pointer.  Non-volatile (C).
* X28       ML Stack pointer. Non-volatile (C).
* X29       Frame pointer (C). Not used in ML
* X30       Link register.  Not used in ML.
* X31       Stack pointer (C).  Not used in ML.  Also zero register.
* 
* Floating point registers:
* V0        First argument and return value
* V1-V7     Second-eighth argument
* V8-V15    Non volatile. Not currently used in ML.
* V16-V31   Volatile. Not currently used in ML.
* 
* The ML calling conventions generally follow the C ABI except that
* all registers are volatile and X28 is used for the stack.
*/

/* the amount of ML stack space to reserve for registers,
   C exception handling etc. The compiler requires us to
   reserve 2 stack-frames worth (2 * 20 words). We actually reserve
   slightly more than this.
*/

#define OVERFLOW_STACK_SIZE 50

// X26 always points at this area when executing ML code.
// The offsets are built into the assembly code and some are built into
// the code generator
typedef struct _AssemblyArgs {
public:
    byte*           enterInterpreter;  // These are filled in with the functions.
    byte*           heapOverFlowCall;
    byte*           stackOverFlowCall;
    byte*           stackOverFlowCallEx;
    byte*           trapHandlerEntry;
    stackItem       exceptionPacket;    // Set if there is an exception
    PolyWord        threadId;           // My thread id.  Saves having to call into RTS for it.


    byte            returnReason;       // Reason for returning from ML - Set by assembly code.
}  AssemblyArgs;

class Arm64TaskData: public TaskData, ByteCodeInterpreter {
public:
    Arm64TaskData();
    ~Arm64TaskData() {}

    AssemblyArgs assemblyInterface;
    int saveRegisterMask; // Registers that need to be updated by a GC.

    virtual void GarbageCollect(ScanAddress *process);
    void ScanStackAddress(ScanAddress *process, stackItem& val, StackSpace *stack);
    virtual void EnterPolyCode(); // Start running ML

    virtual void SetException(poly_exn *exc) { assemblyInterface.exceptionPacket = (PolyWord)exc;  }
    virtual void InterruptCode();

    // AddTimeProfileCount is used in time profiling.
    virtual bool AddTimeProfileCount(SIGNALCONTEXT *context);

    virtual void InitStackFrame(TaskData *newTask, Handle proc);

    // Increment or decrement the first word of the object pointed to by the
    // mutex argument and return the new value.
    virtual POLYUNSIGNED AtomicDecrement(PolyObject* mutexp);
    // Set a mutex to zero.
    virtual void AtomicReset(PolyObject* mutexp);
    virtual POLYUNSIGNED AtomicIncrement(PolyObject* mutexp);

    // Return the minimum space occupied by the stack.   Used when setting a limit.
    virtual uintptr_t currentStackSpace(void) const { return ((stackItem*)this->stack->top - this->taskSp) + OVERFLOW_STACK_SIZE; }

    virtual void addProfileCount(POLYUNSIGNED words) { addSynchronousCount(interpreterPc, words); }

    virtual void CopyStackFrame(StackObject *old_stack, uintptr_t old_length, StackObject *new_stack, uintptr_t new_length);

    PLock interruptLock;

    virtual void ClearExceptionPacket() { assemblyInterface.exceptionPacket = TAGGED(0); }
    virtual PolyWord GetExceptionPacket() { return assemblyInterface.exceptionPacket; }
    virtual stackItem* GetHandlerRegister() { return hr; }
    virtual void SetHandlerRegister(stackItem* newHr) { hr = newHr;  }
    virtual void HandleStackOverflow(uintptr_t space);

    stackItem       *taskSp; /* Stack pointer. */
    stackItem       *hr;
    stackItem       *sl; /* Stack limit register. */
};

// Values for the returnReason byte
enum RETURN_REASON {
    RETURN_HEAP_OVERFLOW = 1,
    RETURN_STACK_OVERFLOW = 2,
    RETURN_STACK_OVERFLOWEX = 3,
    RETURN_ENTER_INTERPRETER = 4
};

extern "C" {

    // These are declared in the assembly code segment.
    void Arm64AsmSwitchToPoly(void*);
    int  Arm64AsmCallExtraRETURN_ENTER_INTERPRETER(void);
    int  Arm64AsmCallExtraRETURN_HEAP_OVERFLOW(void);
    int  Arm64AsmCallExtraRETURN_STACK_OVERFLOW(void);
    int  Arm64AsmCallExtraRETURN_STACK_OVERFLOWEX(void);

    // This is declared here and called from the assembly code.
    // It avoids having a call to an external in the assembly code
    // which sometimes gives problems with position-indepent code.
    void  Arm64TrapHandler(PolyWord threadId);
};

Arm64TaskData::Arm64TaskData() : ByteCodeInterpreter(&taskSp, &sl)
{
}

class Arm64Dependent : public MachineDependent {
public:
    Arm64Dependent() {}

    // Create a task data object.
    virtual TaskData *CreateTaskData(void) { return new Arm64TaskData(); }
    virtual Architectures MachineArchitecture(void) { return MA_Arm64; }
};

void Arm64TaskData::InitStackFrame(TaskData *parentTask, Handle proc)
/* Initialise stack frame. */
{
    StackSpace *space = this->stack;
    StackObject *stack = (StackObject *)space->stack();
    PolyObject *closure = DEREFWORDHANDLE(proc);
    uintptr_t stack_size = space->spaceSize() * sizeof(PolyWord) / sizeof(stackItem);
    this->taskSp = (stackItem*)stack + stack_size;
    this->hr = this->taskSp;
    *(--this->taskSp) = (PolyWord)closure; /* Closure address */
    this->interpreterPc = *(POLYCODEPTR*)closure;
    this->assemblyInterface.exceptionPacket = TAGGED(0); /* Used for exception argument. */
}

void Arm64TaskData::HandleStackOverflow(uintptr_t space)
{
    uintptr_t min_size = (this->stack->top - (PolyWord*)taskSp) + OVERFLOW_STACK_SIZE + space;
    try {
        // The stack check has failed.  This may either be because we really have
        // overflowed the stack or because the stack limit value has been adjusted
        // to result in a call here.
        CheckAndGrowStack(this, min_size);
    }
    catch (IOException&) {
        // We may get an exception while handling this if we run out of store
    }
    {
        PLocker l(&interruptLock);
        // Set the stack limit.  This clears any interrupt and also sets the
        // correct value if we've grown the stack.
        sl = (stackItem*)stack->bottom + OVERFLOW_STACK_SIZE;
    }

    try {
        processes->ProcessAsynchRequests(this);
        // Release and re-acquire use of the ML memory to allow another thread
        // to GC.
        processes->ThreadReleaseMLMemory(this);
        processes->ThreadUseMLMemory(this);
    }
    catch (IOException&) {
    }

}

void Arm64TaskData::GarbageCollect(ScanAddress *process)
{
    TaskData::GarbageCollect(process);
    ByteCodeInterpreter::GarbageCollect(process);

    if (assemblyInterface.exceptionPacket.w().IsDataPtr())
    {
        PolyObject* obj = assemblyInterface.exceptionPacket.w().AsObjPtr();
        obj = process->ScanObjectAddress(obj);
        assemblyInterface.exceptionPacket = (PolyWord)obj;
    }

    if (stack != 0)
    {
        stackItem*stackPtr = this->taskSp;
        // Now the values on the stack.
        for (stackItem* q = stackPtr; q < (stackItem*)stack->top; q++)
            ScanStackAddress(process, *q, stack);
    }
}

// Process a value within the stack.
void Arm64TaskData::ScanStackAddress(ScanAddress *process, stackItem& stackItem, StackSpace *stack)
{
    // We may have return addresses on the stack which could look like
// tagged values.  Check whether the value is in the code area before
// checking whether it is untagged.
#ifdef POLYML32IN64
    // In 32-in-64 return addresses always have the top 32 bits non-zero. 
    if (stackItem.argValue < ((uintptr_t)1 << 32))
    {
        // It's either a tagged integer or an object pointer.
        if (stackItem.w().IsDataPtr())
        {
            PolyWord val = process->ScanObjectAddress(stackItem.w().AsObjPtr());
            stackItem = val;
        }
    }
    else
    {
        // Could be a code address or a stack address.
        MemSpace* space = gMem.SpaceForAddress(stackItem.codeAddr - 1);
        if (space == 0 || space->spaceType != ST_CODE) return;
        PolyObject* obj = gMem.FindCodeObject(stackItem.codeAddr);
        ASSERT(obj != 0);
        // Process the address of the start.  Don't update anything.
        process->ScanObjectAddress(obj);
    }
#else
    // The -1 here is because we may have a zero-sized cell in the last
    // word of a space.
    MemSpace* space = gMem.SpaceForAddress(stackItem.codeAddr - 1);
    if (space == 0) return; // In particular we may have one of the assembly code addresses.
    if (space->spaceType == ST_CODE)
    {
        PolyObject* obj = gMem.FindCodeObject(stackItem.codeAddr);
        // If it is actually an integer it might be outside a valid code object.
        if (obj == 0)
        {
            ASSERT(stackItem.w().IsTagged()); // It must be an integer
        }
        else // Process the address of the start.  Don't update anything.
            process->ScanObjectAddress(obj);
    }
    else if (space->spaceType == ST_LOCAL && stackItem.w().IsDataPtr())
        // Local values must be word addresses.
    {
        PolyWord val = process->ScanObjectAddress(stackItem.w().AsObjPtr());
        stackItem = val;
    }
#endif

}

// Copy a stack
void Arm64TaskData::CopyStackFrame(StackObject *old_stack, uintptr_t old_length, StackObject *new_stack, uintptr_t new_length)
{
#ifdef POLYML32IN64
    old_length = old_length / 2;
    new_length = new_length / 2;
#endif
    /* Moves a stack, updating all references within the stack */
    stackItem*old_base = (stackItem*)old_stack;
    stackItem*new_base = (stackItem*)new_stack;
    stackItem*old_top = old_base + old_length;

    /* Calculate the offset of the new stack from the old. If the frame is
    being extended objects in the new frame will be further up the stack
    than in the old one. */

    uintptr_t offset = new_base - old_base + new_length - old_length;
    stackItem *oldSp = this->taskSp;
    this->taskSp = oldSp + offset;
    this->hr = this->hr + offset;

    /* Skip the unused part of the stack. */

    uintptr_t i = oldSp - old_base;

    ASSERT(i <= old_length);

    i = old_length - i;

    stackItem *old = oldSp;
    stackItem *newp = this->taskSp;

    while (i--)
    {
        stackItem old_word = *old++;
        if (old_word.w().IsDataPtr() && old_word.stackAddr >= old_base && old_word.stackAddr <= old_top)
            old_word.stackAddr = old_word.stackAddr + offset;
        else if (old_word.w().IsDataPtr() && IsHeapAddress(old_word.stackAddr))
        {
            stackItem* addr = (stackItem*)old_word.w().AsStackAddr();
            if (addr >= old_base && addr <= old_top)
            {
                addr += offset;
                old_word = PolyWord::FromStackAddr((PolyWord*)addr);
            }
        }
        *newp++ = old_word;
    }
    ASSERT(old == ((stackItem*)old_stack) + old_length);
    ASSERT(newp == ((stackItem*)new_stack) + new_length);
}

void Arm64TaskData::EnterPolyCode()
/* Called from "main" to enter the code. */
{
    sl = (stackItem*)((PolyWord*)this->stack->stack() + OVERFLOW_STACK_SIZE);
    // Run the interpreter.  Currently this can never return.
    // Callbacks are not currently implemented which is the only case that could
    // occur in the pure interpreted version.
    (void)RunInterpreter(this);
    exitThread(this); // This thread is exiting.
    ASSERT(0); // We should never get here.
}

// This is called from a different thread so we have to be careful.
void Arm64TaskData::InterruptCode()
{
    PLocker l(&interruptLock);
    // Set the stack limit pointer to the top of the stack to cause
    // a trap when we next check for stack overflow.
    // We use a lock here to ensure that we always use the current value of the
    // stack.  The thread we're interrupting could be growing the stack at this point.
    if (stack != 0)
        sl = (stackItem*)(stack->top - 1);
}

// As far as possible we want locking and unlocking an ML mutex to be fast so
// we try to implement the code in the assembly code using appropriate
// interlocked instructions.  That does mean that if we need to lock and
// unlock an ML mutex in this code we have to use the same, machine-dependent,
// code to do it.  These are defaults that are used where there is no
// machine-specific code.

#if defined(_MSC_VER)
// This saves having to define it in the MASM assembly code.
static POLYUNSIGNED Arm64AsmAtomicExchangeAndAdd(PolyObject* mutexp, POLYSIGNED addend)
{
#   if (SIZEOF_POLYWORD == 8)
    return InterlockedExchangeAdd64((LONG64*)mutexp, addend);
#   else
    return InterlockedExchangeAdd((LONG*)mutexp, addend);
#  endif
}

#else
extern "C" {
    // This is only defined in the GAS assembly code
    POLYUNSIGNED Arm64AsmAtomicExchangeAndAdd(PolyObject*, POLYSIGNED);
}
#endif

// Decrement the value contained in the first word of the mutex.
// The addend is TAGGED(+/-1) - 1
POLYUNSIGNED Arm64TaskData::AtomicDecrement(PolyObject* mutexp)
{
    return Arm64AsmAtomicExchangeAndAdd(mutexp, -2) - 2;
}

// Increment the value contained in the first word of the mutex.
POLYUNSIGNED Arm64TaskData::AtomicIncrement(PolyObject* mutexp)
{
    return Arm64AsmAtomicExchangeAndAdd(mutexp, 2) + 2;
}

// Release a mutex.  Because the atomic increment and decrement
// use the hardware atomic load-and-add we can simply set this to zero.
void Arm64TaskData::AtomicReset(PolyObject* mutexp)
{
    mutexp->Set(0, TAGGED(0)); // Set this to released.
}

bool Arm64TaskData::AddTimeProfileCount(SIGNALCONTEXT *context)
{
    if (interpreterPc != 0)
    {
        // See if the PC we've got is an ML code address.
        MemSpace *space = gMem.SpaceForAddress(interpreterPc);
        if (space != 0 && (space->spaceType == ST_CODE || space->spaceType == ST_PERMANENT))
        {
            incrementCountAsynch(interpreterPc);
            return true;
        }
    }
    return false;
}

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyInterpretedEnterIntMode();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyEndBootstrapMode(FirstArgument threadId, PolyWord function);
}

// Do we require EnterInt instructions and if so for which architecture?
// 0 = > None; 1 => X86_32, 2 => X86_64. 3 => X86_32_in_64.
POLYUNSIGNED PolyInterpretedEnterIntMode()
{
    return TAGGED(0).AsUnsigned();
}

// This has no effect in the interpreted version.
POLYUNSIGNED PolyEndBootstrapMode(FirstArgument threadId, PolyWord function)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle pushedFunction = taskData->saveVec.push(function);
    taskData->InitStackFrame(taskData, pushedFunction);
    taskData->EnterPolyCode();
    // Should never return.
    ASSERT(0);
    return TAGGED(0).AsUnsigned();
}

static Arm64Dependent arm64Dependent;

MachineDependent *machineDependent = &arm64Dependent;

// No machine-specific calls in the interpreter.
struct _entrypts machineSpecificEPT[] =
{
    { "PolyInterpretedEnterIntMode",    (polyRTSFunction)&PolyInterpretedEnterIntMode },
    { "PolyEndBootstrapMode",           (polyRTSFunction)&PolyEndBootstrapMode },
    { NULL, NULL} // End of list.
};
