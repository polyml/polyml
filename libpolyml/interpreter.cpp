/*
    Architecture independent wrapper for the byte-code interpreter.

    Copyright David C.J. Matthews 2020.

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

#if (SIZEOF_VOIDP == 8 && !defined(POLYML32IN64))
#define IS64BITS 1
#endif

#define CHECKED_REGS 2
#define UNCHECKED_REGS 0

#define EXTRA_STACK 0 // Don't need any extra - signals aren't handled on the Poly stack.

/* the amount of ML stack space to reserve for registers,
   C exception handling etc. The compiler requires us to
   reserve 2 stack-frames worth (2 * 20 words) plus whatever
   we require for the register save area. We actually reserve
   slightly more than this. SPF 3/3/97
*/
#define OVERFLOW_STACK_SIZE \
  (50 + \
   CHECKED_REGS + \
   UNCHECKED_REGS + \
   EXTRA_STACK)


// This duplicates some code in reals.cpp but is now updated.
#define DOUBLESIZE (sizeof(double)/sizeof(POLYUNSIGNED))

union realdb { double dble; POLYUNSIGNED puns[DOUBLESIZE]; };

class IntTaskData: public TaskData, ByteCodeInterpreter {
public:
    IntTaskData() : interrupt_requested(false) {}
    ~IntTaskData() {}

    virtual void GarbageCollect(ScanAddress *process);
    void ScanStackAddress(ScanAddress *process, stackItem& val, StackSpace *stack);
    virtual void EnterPolyCode(); // Start running ML

    virtual void SetException(poly_exn *exc) { exception_arg = exc;  }
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

    virtual void addProfileCount(POLYUNSIGNED words) { addSynchronousCount(taskPc, words); }

    virtual void CopyStackFrame(StackObject *old_stack, uintptr_t old_length, StackObject *new_stack, uintptr_t new_length);

    PLock interruptLock;
    bool interrupt_requested;

    // Update the copies in the task object
    virtual void SaveInterpreterState(POLYCODEPTR pc, stackItem*sp)
    {
        taskPc = pc;
        taskSp = sp;
    }

    // Update the local state
    virtual void LoadInterpreterState(POLYCODEPTR &pc, stackItem*&sp)
    {
        pc = taskPc;
        sp = taskSp;
    }

    virtual void ClearExceptionPacket() { exception_arg = TAGGED(0); }
    virtual PolyWord GetExceptionPacket() { return exception_arg; }
    virtual stackItem* GetHandlerRegister() { return hr; }
    virtual void SetHandlerRegister(stackItem* newHr) { hr = newHr;  }
    virtual void CheckStackAndInterrupt(POLYUNSIGNED space);
    virtual bool TestInterrupt() { return interrupt_requested; }
    bool WasInterrupted();

    POLYCODEPTR     taskPc; /* Program counter. */
    stackItem       *taskSp; /* Stack pointer. */
    stackItem       *hr;
    PolyWord        exception_arg;
    stackItem       *sl; /* Stack limit register. */
};

class Interpreter : public MachineDependent {
public:
    Interpreter() {}

    // Create a task data object.
    virtual TaskData *CreateTaskData(void) { return new IntTaskData(); }
    virtual Architectures MachineArchitecture(void) { return MA_Interpreted; }
};

void IntTaskData::InitStackFrame(TaskData *parentTask, Handle proc)
/* Initialise stack frame. */
{
    StackSpace *space = this->stack;
    StackObject *stack = (StackObject *)space->stack();
    PolyObject *closure = DEREFWORDHANDLE(proc);
    uintptr_t stack_size = space->spaceSize() * sizeof(PolyWord) / sizeof(stackItem);
    this->taskSp = (stackItem*)stack + stack_size;
    this->hr = this->taskSp;
    *(--this->taskSp) = (PolyWord)closure; /* Closure address */
    this->taskPc = *(POLYCODEPTR*)closure;
    this->exception_arg = TAGGED(0); /* Used for exception argument. */
}

void IntTaskData::CheckStackAndInterrupt(POLYUNSIGNED space)
{
    // Check there is space on the stack
    if (taskSp - space < sl)
    {
        uintptr_t min_size = (this->stack->top - (PolyWord*)taskSp) + OVERFLOW_STACK_SIZE + space;
        CheckAndGrowStack(this, min_size);
        sl = (stackItem*)this->stack->stack() + OVERFLOW_STACK_SIZE;
    }
    // Also check for interrupts.
    // Check interrupt_requested first before getting the lock and
    // checking it again.  We may delay seeing an interrupt but taking
    // the lock seems to be expensive.
    if (interrupt_requested && WasInterrupted())
    {
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
}

// This may be called on a different thread.
void IntTaskData::InterruptCode()
{
    PLocker l(&interruptLock);
    interrupt_requested = true;
}

// Check and clear the "interrupt".
bool IntTaskData::WasInterrupted()
{
    PLocker l(&interruptLock);
    bool was = interrupt_requested;
    interrupt_requested = false;
    return was;
}

void IntTaskData::GarbageCollect(ScanAddress *process)
{
    TaskData::GarbageCollect(process);
    ByteCodeInterpreter::GarbageCollect(process);

    if (exception_arg.IsDataPtr())
    {
        PolyObject* obj = exception_arg.AsObjPtr();
        obj = process->ScanObjectAddress(obj);
        exception_arg = obj;
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
void IntTaskData::ScanStackAddress(ScanAddress *process, stackItem& stackItem, StackSpace *stack)
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
void IntTaskData::CopyStackFrame(StackObject *old_stack, uintptr_t old_length, StackObject *new_stack, uintptr_t new_length)
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

void IntTaskData::EnterPolyCode()
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

// As far as possible we want locking and unlocking an ML mutex to be fast so
// we try to implement the code in the assembly code using appropriate
// interlocked instructions.  That does mean that if we need to lock and
// unlock an ML mutex in this code we have to use the same, machine-dependent,
// code to do it.  These are defaults that are used where there is no
// machine-specific code.

// This lock is used to synchronise all atomic operations.
// It is not needed in the X86 version because that can use a global
// memory lock.
static PLock mutexLock;

POLYUNSIGNED IntTaskData::AtomicIncrement(PolyObject* mutexp)
{
    PLocker l(&mutexLock);
    // A thread can only call this once so the values will be short
    PolyWord newValue = TAGGED(UNTAGGED(mutexp->Get(0)) + 1);
    mutexp->Set(0, newValue);
    return newValue.AsUnsigned();
}

POLYUNSIGNED IntTaskData::AtomicDecrement(PolyObject* mutexp)
{
    PLocker l(&mutexLock);
    // A thread can only call this once so the values will be short
    PolyWord newValue = TAGGED(UNTAGGED(mutexp->Get(0)) - 1);
    mutexp->Set(0, newValue);
    return newValue.AsUnsigned();
}

// Release a mutex.  We need to lock the mutex to ensure we don't
// reset it in the time between one of atomic operations reading
// and writing the mutex.
void IntTaskData::AtomicReset(PolyObject* mutexp)
{
    PLocker l(&mutexLock);
    mutexp->Set(0, TAGGED(0)); // Set this to released.
}

bool IntTaskData::AddTimeProfileCount(SIGNALCONTEXT *context)
{
    if (taskPc != 0)
    {
        // See if the PC we've got is an ML code address.
        MemSpace *space = gMem.SpaceForAddress(taskPc);
        if (space != 0 && (space->spaceType == ST_CODE || space->spaceType == ST_PERMANENT))
        {
            incrementCountAsynch(taskPc);
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

static Interpreter interpreterObject;

MachineDependent *machineDependent = &interpreterObject;

// No machine-specific calls in the interpreter.
struct _entrypts machineSpecificEPT[] =
{
    { "PolyInterpretedEnterIntMode",    (polyRTSFunction)&PolyInterpretedEnterIntMode },
    { "PolyEndBootstrapMode",           (polyRTSFunction)&PolyEndBootstrapMode },
    { NULL, NULL} // End of list.
};
