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
#include "int_opcodes.h"

/*
* ARM64 register use:
* X0        First argument and return value
* X1-X7     Second-eighth argument
* X8        Indirect result (C), ML closure pointer on entry
* X9-X15    Volatile scratch registers
* X16-17    Intra-procedure-call (C).  Only used for special cases in ML.
* X18       Platform register. Not used in ML.
* X19-X24   Non-volatile (C).  Scratch registers (ML).
* X25       ML Heap limit pointer
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

// Arm64 instructions are all 32-bit values.
typedef uint32_t* arm64CodePointer;

#define OVERFLOW_STACK_SIZE 50

// X26 always points at this area when executing ML code.
// The offsets are built into the assembly code and some are built into
// the code generator so this must not be changed without checking these.
typedef struct _AssemblyArgs {
public:
    byte*           enterInterpreter;  // These are filled in with the functions.
    byte*           heapOverFlowCall;
    byte*           stackOverFlowCall;
    byte*           stackOverFlowCallEx;
    byte*           trapHandlerEntry;
    stackItem*      handlerRegister;   // Current exception handler
    stackItem*      stackLimit;        // Lower limit of stack
    stackItem       exceptionPacket;    // Set if there is an exception
    PolyWord        threadId;           // My thread id.  Saves having to call into RTS for it.
    stackItem       registers[25];      // Save/load area for registers X0-X25 inclusive
    double          fpRegisters[8];     // Save/load area for floating point regs D0-D7
    PolyWord*       localMbottom;      // Base of memory + 1 word
    PolyWord*       localMpointer;      // X27 Allocation ptr + 1 word
    stackItem*      stackPtr;           // X28 Current stack pointer
    arm64CodePointer linkRegister;       // X30 - Link register (return address)
    arm64CodePointer entryPoint;         // PC address to return to

    byte            returnReason;       // Reason for returning from ML - Set by assembly code.
}  AssemblyArgs;

class Arm64TaskData: public TaskData, ByteCodeInterpreter {
public:
    Arm64TaskData();
    ~Arm64TaskData() {}
    unsigned allocReg; // The register to take the allocated space.
    POLYUNSIGNED allocWords; // The words to allocate.

    AssemblyArgs assemblyInterface;
    uint32_t saveRegisterMask; // Registers that need to be updated by a GC.

    virtual void GarbageCollect(ScanAddress *process);
    void ScanStackAddress(ScanAddress *process, stackItem& val, StackSpace *stack);
    virtual void EnterPolyCode(); // Start running ML

    virtual void SetException(poly_exn *exc) { assemblyInterface.exceptionPacket = (PolyWord)exc;  }
    virtual void InterruptCode();

    // AddTimeProfileCount is used in time profiling.
    virtual bool AddTimeProfileCount(SIGNALCONTEXT *context);

    virtual void InitStackFrame(TaskData *newTask, Handle proc);

    // Atomic exchange-and-add.  Used in the process module to release a mutex and in the
    // interpreter.  It needs to use the same instruction that compiled code uses. 
    virtual POLYSIGNED AtomicExchAdd(PolyObject* mutexp, POLYSIGNED incr);
    // Set a mutex to zero.
    virtual void AtomicReset(PolyObject* mutexp);

    // Return the minimum space occupied by the stack.  Used when setting a limit.
    // N.B. This is PolyWords not native words.
    virtual uintptr_t currentStackSpace(void) const
    {
        return (this->stack->top - (PolyWord*)assemblyInterface.stackPtr) + OVERFLOW_STACK_SIZE;
    }

    virtual void addProfileCount(POLYUNSIGNED words) { addSynchronousCount(interpreterPc, words); }

    virtual void CopyStackFrame(StackObject *old_stack, uintptr_t old_length, StackObject *new_stack, uintptr_t new_length);

    //void HeapOverflowTrap();

    void SetMemRegisters();
    void SaveMemRegisters();

    void HandleTrap();
    // ByteCode overrides.  The interpreter and native code states need to be in sync.
    // The interpreter is only used during the initial bootstrap.
    virtual void ClearExceptionPacket() { assemblyInterface.exceptionPacket = TAGGED(0); }
    virtual PolyWord GetExceptionPacket() { return assemblyInterface.exceptionPacket; }
    virtual stackItem* GetHandlerRegister() { return assemblyInterface.handlerRegister; }
    virtual void SetHandlerRegister(stackItem* hr) { assemblyInterface.handlerRegister = hr; }

    void Interpret();
    void EndBootStrap() { mixedCode = true; }

    PLock interruptLock;

    virtual void HandleStackOverflow(uintptr_t space);
};

class Arm64Dependent : public MachineDependent {
public:
    Arm64Dependent() : mustInterpret(false) {}

    // Create a task data object.
    virtual TaskData* CreateTaskData(void) { return new Arm64TaskData(); }

    virtual Architectures MachineArchitecture(void);

    virtual void SetBootArchitecture(char arch, unsigned wordLength);

    // The ARM has separate instruction and data caches.
    virtual void FlushInstructionCache(void* p, POLYUNSIGNED bytes);

    // During the first bootstrap phase this is interpreted.
    bool mustInterpret;
};

static Arm64Dependent arm64Dependent;

MachineDependent* machineDependent = &arm64Dependent;

Architectures Arm64Dependent::MachineArchitecture(void)
{
    // During the first phase of the bootstrap we
    // compile the interpreted version.
    if (mustInterpret) return MA_Interpreted;
    return MA_Arm64;
}

// Values for the returnReason byte
enum RETURN_REASON {
    RETURN_HEAP_OVERFLOW = 1,
    RETURN_STACK_OVERFLOW = 2,
    RETURN_STACK_OVERFLOWEX = 3,
    RETURN_ENTER_INTERPRETER = 4
};

extern "C" {

    // These are declared in the assembly code segment.
    void Arm64AsmEnterCompiledCode(void*);
    int  Arm64AsmCallExtraRETURN_ENTER_INTERPRETER(void);
    int  Arm64AsmCallExtraRETURN_HEAP_OVERFLOW(void);
    int  Arm64AsmCallExtraRETURN_STACK_OVERFLOW(void);
    int  Arm64AsmCallExtraRETURN_STACK_OVERFLOWEX(void);

    // This is declared here and called from the assembly code.
    // It avoids having a call to an external in the assembly code
    // which sometimes gives problems with position-indepent code.
    void  Arm64TrapHandler(PolyWord threadId);
};

Arm64TaskData::Arm64TaskData() : ByteCodeInterpreter(&assemblyInterface.stackPtr, &assemblyInterface.stackLimit),
    allocReg(0), allocWords(0), saveRegisterMask(0)
{
    assemblyInterface.enterInterpreter = (byte*)Arm64AsmCallExtraRETURN_ENTER_INTERPRETER;
    assemblyInterface.heapOverFlowCall = (byte*)Arm64AsmCallExtraRETURN_HEAP_OVERFLOW;
    assemblyInterface.stackOverFlowCall = (byte*)Arm64AsmCallExtraRETURN_STACK_OVERFLOW;
    assemblyInterface.stackOverFlowCallEx = (byte*)Arm64AsmCallExtraRETURN_STACK_OVERFLOWEX;
    assemblyInterface.trapHandlerEntry = (byte*)Arm64TrapHandler;
    interpreterPc = 0;
    mixedCode = !arm64Dependent.mustInterpret;
}

void Arm64Dependent::SetBootArchitecture(char arch, unsigned wordLength)
{
    if (arch == 'I')
        mustInterpret = true;
    else if (arch != 'A')
        Crash("Boot file has unexpected architecture code: %c", arch);
}

// The ARM has separate instruction and data caches so we must flush
// the cache when creating or modifying code.
void Arm64Dependent::FlushInstructionCache(void* p, POLYUNSIGNED bytes)
{
#ifdef _WIN32
    ::FlushInstructionCache(GetCurrentProcess(), p, bytes);
#elif defined (__GNUC__)
    __builtin___clear_cache(p, (char*)p + bytes);
#elif (defined (__clang__) && defined (__APPLE__))
    sys_icache_invalidate(p, bytes);
#else
#error "No code to flush the instruction cache."
#endif
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
        stackItem*stackPtr = assemblyInterface.stackPtr;
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
    stackItem *oldSp = assemblyInterface.stackPtr;
    assemblyInterface.stackPtr = oldSp + offset;
    assemblyInterface.handlerRegister = assemblyInterface.handlerRegister + offset;

    /* Skip the unused part of the stack. */

    uintptr_t i = oldSp - old_base;

    ASSERT(i <= old_length);

    i = old_length - i;

    stackItem *old = oldSp;
    stackItem *newp = assemblyInterface.stackPtr;

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
    assemblyInterface.stackLimit = (stackItem*)((PolyWord*)this->stack->stack() + OVERFLOW_STACK_SIZE);
    if (arm64Dependent.mustInterpret)
    {
        PolyWord closure = assemblyInterface.registers[8];
        *(--assemblyInterface.stackPtr) = closure; /* Closure address */
        interpreterPc = *(POLYCODEPTR*)closure.AsObjPtr();
        Interpret();
        ASSERT(0); // Should never return
    }
    SetMemRegisters();
    // Jump into the ML code.  This code sets up the registers and puts the
    // address of the assemblyInterface into X26
    Arm64AsmEnterCompiledCode(&assemblyInterface);
    // This should never return
    ASSERT(0);

}

void Arm64TaskData::Interpret()
{
    while (true)
    {
        switch (RunInterpreter(this))
        {
        case ReturnCall:
            // After the call there will be an enter-int instruction so that when this
            // returns we will re-enter the interpreter.  The number of arguments for
            // this call is after that.
            while ((uintptr_t)interpreterPc & 3)
            {
                ASSERT(interpreterPc[0] == INSTR_no_op);
                interpreterPc++;
            }
            ASSERT(interpreterPc[0] == 0xe9);
            numTailArguments = interpreterPc[12];

        case ReturnTailCall:
        {
            ClearExceptionPacket();
            // Pop the closure.
            PolyWord closureWord = *assemblyInterface.stackPtr++;
            PolyObject* closure = closureWord.AsObjPtr();
            arm64CodePointer cp = *(arm64CodePointer*)closure;
            if (cp[0] == 0xAA1E03E9 && cp[1] == 0xF9400350 && cp[2] == 0xD63F0200)
            {
                // If the code we're going to is interpreted push back the closure and
                // continue.
                interpreterPc = (POLYCODEPTR)cp;
                assemblyInterface.stackPtr--;
                HandleStackOverflow(128); // Make sure we have space since we're bypassing the check.
                continue;
            }
            assemblyInterface.registers[8] = closureWord; // Put closure in the closure reg.
            // Pop the return address.  We may need to align this to a word boundary.
            POLYCODEPTR originalReturn = (POLYCODEPTR)((assemblyInterface.stackPtr++)->codeAddr);
            while ((uintptr_t)originalReturn & 3)
            {
                ASSERT(originalReturn[0] == INSTR_no_op); 
                originalReturn++;
            }
            // Get the arguments into the correct registers.
            // Load the register arguments.  The first 8 arguments go into X0-X7.
            // These will have been the first arguments to be pushed so will be
            // furthest away on the stack.
            // Note: we don't currently pass any arguments in the FP regs.
            for (unsigned i = 0; i < numTailArguments && i < 8; i++)
                assemblyInterface.registers[i] = assemblyInterface.stackPtr[numTailArguments - i - 1];
            // If there are any more arguments these need to be shifted down the stack.
            while (numTailArguments > 8)
            {
                numTailArguments--;
                assemblyInterface.stackPtr[numTailArguments] = assemblyInterface.stackPtr[numTailArguments - 8];
            }
            // Remove the register arguments
            assemblyInterface.stackPtr += numTailArguments > 8 ? 8 : numTailArguments;

            assemblyInterface.linkRegister = (arm64CodePointer)originalReturn; // Set the return address to caller
            assemblyInterface.entryPoint = *(arm64CodePointer*)closure; // Entry point to callee
            interpreterPc = 0; // No longer in the interpreter (See SaveMemRegs)
            return;
        }

        case ReturnReturn:
        {
            ClearExceptionPacket();
            // Returning from an interpreted function.  Normally we'll be returning to
            // interpreted code.
            if ((uintptr_t)interpreterPc & 3) // ARM64 addresses will always be 4-byte aligned.
                continue;
            arm64CodePointer cp = (arm64CodePointer)interpreterPc;
            if (cp[0] == 0xAA1E03E9 && cp[1] == 0xF9400350 && cp[2] == 0xD63F0200)
                continue;
            // Pop the value we're returning.  Set the entry point to the code we're returning to.
            assemblyInterface.registers[0] = *assemblyInterface.stackPtr++;
            assemblyInterface.entryPoint = cp;
            interpreterPc = 0; // No longer in the interpreter (See SaveMemRegs)
            return;
        }
        }
    }
}

// Called from the assembly code as a result of a trap i.e. a request for
// a GC or to extend the stack.
void Arm64TrapHandler(PolyWord threadId)
{
    Arm64TaskData* taskData = (Arm64TaskData*)TaskData::FindTaskForId(threadId);
    taskData->HandleTrap();
}

void Arm64TaskData::HandleTrap()
{
    SaveMemRegisters(); // Update globals from the memory registers.

    switch (this->assemblyInterface.returnReason)
    {

    case RETURN_HEAP_OVERFLOW:
        // The heap has overflowed.
        // The register mask is the word after the return.
        saveRegisterMask = *assemblyInterface.entryPoint++;
        ASSERT(0); // TODO
        //HeapOverflowTrap(); // Computes a value for allocWords only
        break;

    case RETURN_STACK_OVERFLOW:
    case RETURN_STACK_OVERFLOWEX:
    {
        // The register mask is the word after the return.
        saveRegisterMask = *assemblyInterface.entryPoint++;
        ASSERT(0); // TODO
        uintptr_t min_size = 0; // Size in PolyWords
#if (0)
        if (assemblyInterface.returnReason == RETURN_STACK_OVERFLOW)
        {
            min_size = (this->stack->top - (PolyWord*)assemblyInterface.stackPtr) +
                OVERFLOW_STACK_SIZE * sizeof(uintptr_t) / sizeof(PolyWord);
        }
        else
        {
            // Stack limit overflow.  If the required stack space is larger than
            // the fixed overflow size the code will calculate the limit in %EDI.
            stackItem* stackP = regDI().stackAddr;
            min_size = (this->stack->top - (PolyWord*)stackP) +
                OVERFLOW_STACK_SIZE * sizeof(uintptr_t) / sizeof(PolyWord);
        }
#endif
        HandleStackOverflow(min_size);
        break;
    }

    case RETURN_ENTER_INTERPRETER:
    {
        interpreterPc = (POLYCODEPTR)assemblyInterface.linkRegister;
        byte reasonCode = *interpreterPc++;
        // Sort out arguments.
        assemblyInterface.exceptionPacket = TAGGED(0);
        if (reasonCode == 0xff)
        {
            // Exception handler.
            ASSERT(0); // Not used
            assemblyInterface.exceptionPacket = assemblyInterface.registers[0]; // Get the exception packet
            // We're already in the exception handler but we still have to
            // adjust the stack pointer and pop the current exception handler.
            assemblyInterface.stackPtr = assemblyInterface.handlerRegister;
            assemblyInterface.stackPtr++;
            assemblyInterface.handlerRegister = (assemblyInterface.stackPtr++)[0].stackAddr;
        }
        else if (reasonCode >= 128)
        {
            // Start of function.
            unsigned numArgs = reasonCode - 128;
            // We need the stack to contain:
            // The closure, the return address, the arguments.
            // Push the register args.
            ASSERT(numArgs == 1); // We only ever call functions with one argument.
            *(--assemblyInterface.stackPtr) = assemblyInterface.registers[0];
            *(--assemblyInterface.stackPtr) = assemblyInterface.registers[9]; // Return address - value of X30 before enter-int
            *(--assemblyInterface.stackPtr) = assemblyInterface.registers[8]; // Closure
        }
        else
        {
            // Return from call. Push X0
            *(--assemblyInterface.stackPtr) = assemblyInterface.registers[0];
        }
        Interpret();
        break;
    }

    default:
        Crash("Unknown return reason code %u", this->assemblyInterface.returnReason);
    }
    SetMemRegisters();
}

void Arm64TaskData::HandleStackOverflow(uintptr_t space)
{
    uintptr_t min_size = (this->stack->top - (PolyWord*)assemblyInterface.stackPtr) + OVERFLOW_STACK_SIZE + space;
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
        assemblyInterface.stackLimit = (stackItem*)stack->bottom + OVERFLOW_STACK_SIZE;
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

void Arm64TaskData::InitStackFrame(TaskData* parentTask, Handle proc)
/* Initialise stack frame. */
{
    StackSpace* space = this->stack;
    StackObject* stack = (StackObject*)space->stack();
    uintptr_t stack_size = space->spaceSize() * sizeof(PolyWord) / sizeof(stackItem);
    assemblyInterface.stackPtr = (stackItem*)stack + stack_size;
    assemblyInterface.stackLimit = (stackItem*)space->bottom + OVERFLOW_STACK_SIZE;
    assemblyInterface.handlerRegister = assemblyInterface.stackPtr;
    // Store the argument and the closure.
    assemblyInterface.registers[8] = proc->Word(); // Closure
    assemblyInterface.registers[0] = TAGGED(0); // Argument
    assemblyInterface.linkRegister = 0; // We never return
    // Have to set the register mask in case we get a GC before the thread starts.
    saveRegisterMask = (1 << 8) | 1; // X8 and X0
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
        assemblyInterface.stackLimit = (stackItem*)(stack->top - 1);
}

// Called before entering ML code from the run-time system
void Arm64TaskData::SetMemRegisters()
{
    // Copy the current store limits into variables before we go into the assembly code.

    // If we haven't yet set the allocation area or we don't have enough we need
    // to create one (or a new one).
    if (allocPointer <= allocLimit + allocWords)
    {
        if (allocPointer < allocLimit)
            Crash("Bad length in heap overflow trap");

        // Find some space to allocate in.  Updates taskData->allocPointer and
        // returns a pointer to the newly allocated space (if allocWords != 0)
        PolyWord* space =
            processes->FindAllocationSpace(this, allocWords, true);
        if (space == 0)
        {
            // We will now raise an exception instead of returning.
            // Set allocWords to zero so we don't set the allocation register
            // since that could be holding the exception packet.
            allocWords = 0;
        }
        // Undo the allocation just now.
        allocPointer += allocWords;
    }
    if (this->allocWords != 0)
    {
        // If we have had a heap trap we actually do the allocation here.
        // We will have already garbage collected and recovered sufficient space.
        // This also happens if we have just trapped because of store profiling.
        allocPointer -= allocWords; // Now allocate
        // Set the allocation register to this area. N.B.  This is an absolute address.
#if (0)
        if (this->allocReg < 15)
            get_reg(this->allocReg)[0].codeAddr = (POLYCODEPTR)(this->allocPointer + 1); /* remember: it's off-by-one */
#endif
        allocWords = 0;
    }

    // If we have run out of store, either just above or while allocating in the RTS,
    // allocPointer and allocLimit will have been set to zero as part of the GC.  We will
    // now be raising an exception which may free some store but we need to come back here
    // before we allocate anything.  The compiled code uses unsigned arithmetic to check for
    // heap overflow but only after subtracting the space required.  We need to make sure
    // that the values are still non-negative after substracting any object size.
    if (allocPointer == 0) allocPointer += MAX_OBJECT_SIZE;
    if (allocLimit == 0) allocLimit += MAX_OBJECT_SIZE;

    assemblyInterface.localMbottom = allocLimit + 1;
    assemblyInterface.localMpointer = allocPointer + 1;
    // If we are profiling store allocation we set mem_hl so that a trap
    // will be generated.
    if (profileMode == kProfileStoreAllocation)
        assemblyInterface.localMbottom = assemblyInterface.localMpointer;

    assemblyInterface.threadId = threadObject;
}

// This is called whenever we have returned from ML to C.
void Arm64TaskData::SaveMemRegisters()
{
    // The normal return is to the link register address.
    assemblyInterface.entryPoint = assemblyInterface.linkRegister;
    if (interpreterPc == 0) // Not if we're already in the interpreter
        allocPointer = assemblyInterface.localMpointer - 1;
    allocWords = 0;
    assemblyInterface.exceptionPacket = TAGGED(0);
    saveRegisterMask = 0;
}

// As far as possible we want locking and unlocking an ML mutex to be fast so
// we try to implement the code in the assembly code using appropriate
// interlocked instructions.  That does mean that if we need to lock and
// unlock an ML mutex in this code we have to use the same, machine-dependent,
// code to do it.  These are defaults that are used where there is no
// machine-specific code.

#if defined(_MSC_VER)
// This saves having to define it in the MASM assembly code.
static POLYSIGNED Arm64AsmAtomicExchangeAndAdd(PolyObject* mutexp, POLYSIGNED addend)
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
    POLYSIGNED Arm64AsmAtomicExchangeAndAdd(PolyObject*, POLYSIGNED);
}
#endif

// Do the exchange-and-add
POLYSIGNED Arm64TaskData::AtomicExchAdd(PolyObject* mutexp, POLYSIGNED incr)
{
    return Arm64AsmAtomicExchangeAndAdd(mutexp, incr - TAGGED(0).AsSigned()/* Remove the tag */);
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
// 0 = > None; 1 => X86_32, 2 => X86_64. 3 => X86_32_in_64. 4 => ARM_64.
POLYUNSIGNED PolyInterpretedEnterIntMode()
{
    return TAGGED(4).AsUnsigned();
}

// End the first stage of bootstrap mode and run a new function.
// The first stage is always interpreted.  Once that is complete every function will have
// at least an executable "enter-interpreter" stub so it can be called as machine code.
POLYUNSIGNED PolyEndBootstrapMode(FirstArgument threadId, PolyWord function)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle pushedFunction = taskData->saveVec.push(function);
    arm64Dependent.mustInterpret = false;
    ((Arm64TaskData*)taskData)->EndBootStrap();
    taskData->InitStackFrame(taskData, pushedFunction);
    taskData->EnterPolyCode();
    // Should never return.
    ASSERT(0);
    return TAGGED(0).AsUnsigned();
}

// No machine-specific calls in the interpreter.
struct _entrypts machineSpecificEPT[] =
{
    { "PolyInterpretedEnterIntMode",    (polyRTSFunction)&PolyInterpretedEnterIntMode },
    { "PolyEndBootstrapMode",           (polyRTSFunction)&PolyEndBootstrapMode },
    { NULL, NULL} // End of list.
};