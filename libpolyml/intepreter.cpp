/*
    Architecture independent wrapper for the byte-code interpreter.

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

    // Switch to Poly and return with the io function to call.
    int SwitchToPoly() {
        sl = (stackItem*)((PolyWord*)this->stack->stack() + OVERFLOW_STACK_SIZE); 
        return RunInterpreter(this);
    }
    virtual void SetException(poly_exn *exc) { exception_arg = exc;  }
    virtual void InterruptCode() { interrupt_requested = true; }

    // AddTimeProfileCount is used in time profiling.
    virtual bool AddTimeProfileCount(SIGNALCONTEXT *context);

    virtual void InitStackFrame(TaskData *newTask, Handle proc, Handle arg);

    // Increment or decrement the first word of the object pointed to by the
    // mutex argument and return the new value.
    virtual Handle AtomicDecrement(Handle mutexp);
    // Set a mutex to zero.
    virtual void AtomicReset(Handle mutexp);

    // Return the minimum space occupied by the stack.   Used when setting a limit.
    virtual uintptr_t currentStackSpace(void) const { return ((stackItem*)this->stack->top - this->taskSp) + OVERFLOW_STACK_SIZE; }

    virtual void addProfileCount(POLYUNSIGNED words) { addSynchronousCount(taskPc, words); }

    virtual void CopyStackFrame(StackObject *old_stack, uintptr_t old_length, StackObject *new_stack, uintptr_t new_length);

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
    virtual bool WasInterrupted() {
        bool was = interrupt_requested; interrupt_requested = false; return was;
    }
    virtual stackItem* StackLimit() { return sl; }
    virtual void GrowStack(POLYUNSIGNED checkSpace);

    POLYCODEPTR     taskPc; /* Program counter. */
    stackItem       *taskSp; /* Stack pointer. */
    stackItem       *hr;
    PolyWord        exception_arg;
    stackItem       *sl; /* Stack limit register. */
};

// This lock is used to synchronise all atomic operations.
// It is not needed in the X86 version because that can use a global
// memory lock.
static PLock mutexLock;

// Special value for return address.
#define SPECIAL_PC_END_THREAD           ((POLYCODEPTR)0)

class Interpreter : public MachineDependent {
public:
    Interpreter() {}

    // Create a task data object.
    virtual TaskData *CreateTaskData(void) { return new IntTaskData(); }
    virtual Architectures MachineArchitecture(void) { return MA_Interpreted; }
};

void IntTaskData::InitStackFrame(TaskData *parentTask, Handle proc, Handle arg)
/* Initialise stack frame. */
{
    StackSpace *space = this->stack;
    StackObject *stack = (StackObject *)space->stack();
    PolyObject *closure = DEREFWORDHANDLE(proc);
    uintptr_t stack_size = space->spaceSize() * sizeof(PolyWord) / sizeof(stackItem);
    this->taskPc = *(POLYCODEPTR*)closure;

    this->exception_arg = TAGGED(0); /* Used for exception argument. */
    this->taskSp = (stackItem*)stack + stack_size;

    /* Set up exception handler */
    /* No previous handler so point it at itself. */
    this->taskSp--;
    this->taskSp->stackAddr = this->taskSp;
    (--this->taskSp)->codeAddr = SPECIAL_PC_END_THREAD; /* Default return address. */
    this->hr = this->taskSp;

    /* If this function takes an argument store it on the stack. */
    if (arg != 0) *(--this->taskSp) = DEREFWORD(arg);

    (*(--this->taskSp)).codeAddr = SPECIAL_PC_END_THREAD; /* Return address. */
    *(--this->taskSp) = (PolyWord)closure; /* Closure address */
}

void IntTaskData::GrowStack(POLYUNSIGNED checkSpace)
{
    uintptr_t min_size = (this->stack->top - (PolyWord*)taskSp) + OVERFLOW_STACK_SIZE + checkSpace;
    CheckAndGrowStack(this, min_size);
    sl = (stackItem*)this->stack->stack() + OVERFLOW_STACK_SIZE;
}

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
    typedef double (*callRTSRtoR) (double);
    typedef double (*callRTSRRtoR) (double, double);
    typedef double (*callRTSGtoR) (intptr_t);
    typedef double (*callRTSRGtoR) (double, intptr_t);
    typedef float(*callRTSFtoF) (float);
    typedef float(*callRTSFFtoF) (float, float);
    typedef float(*callRTSGtoF) (intptr_t);
    typedef float(*callRTSFGtoF) (float, intptr_t);
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
        StackSpace *stackSpace = stack;
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
    if (stackItem.codeAddr == SPECIAL_PC_END_THREAD/* 0 */)
        return;
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
                ASSERT(0); // Callbacks aren't implemented

            default:
                Crash("Unknown io operation %d\n", ioFunction);
            }
        }
        catch (IOException &) {
        }

    }
}

// As far as possible we want locking and unlocking an ML mutex to be fast so
// we try to implement the code in the assembly code using appropriate
// interlocked instructions.  That does mean that if we need to lock and
// unlock an ML mutex in this code we have to use the same, machine-dependent,
// code to do it.  These are defaults that are used where there is no
// machine-specific code.

static Handle ProcessAtomicDecrement(TaskData *taskData, Handle mutexp)
{
    PLocker l(&mutexLock);
    PolyObject *p = DEREFHANDLE(mutexp);
    // A thread can only call this once so the values will be short
    PolyWord newValue = TAGGED(UNTAGGED(p->Get(0))-1);
    p->Set(0, newValue);
    return taskData->saveVec.push(newValue);
}

// Release a mutex.  We need to lock the mutex to ensure we don't
// reset it in the time between one of atomic operations reading
// and writing the mutex.
static Handle ProcessAtomicReset(TaskData *taskData, Handle mutexp)
{
    PLocker l(&mutexLock);
    DEREFHANDLE(mutexp)->Set(0, TAGGED(0)); // Set this to released.
    return taskData->saveVec.push(TAGGED(0)); // Push the unit result
}

Handle IntTaskData::AtomicDecrement(Handle mutexp)
{
    return ProcessAtomicDecrement(this, mutexp);
}

void IntTaskData::AtomicReset(Handle mutexp)
{
    (void)ProcessAtomicReset(this, mutexp);
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
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyInterpretedGetAbiList(FirstArgument threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyInterpretedCreateCIF(FirstArgument threadId, PolyWord abiValue, PolyWord resultType, PolyWord argTypes);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyInterpretedCallFunction(FirstArgument threadId, PolyWord cifAddr, PolyWord cFunAddr, PolyWord resAddr, PolyWord argVec);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyInterpretedEnterIntMode();
}

// FFI
#if (defined(HAVE_LIBFFI) && defined(HAVE_FFI_H))

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <ffi.h>

static struct _abiTable { const char* abiName; ffi_abi abiCode; } abiTable[] =
{
    // Unfortunately the ABI entries are enums rather than #defines so we
    // can't test individual entries.
    #ifdef X86_WIN32
        {"sysv", FFI_SYSV},
        {"stdcall", FFI_STDCALL},
        {"thiscall", FFI_THISCALL},
        {"fastcall", FFI_FASTCALL},
        {"ms_cdecl", FFI_MS_CDECL},
    #elif defined(X86_WIN64)
        {"win64", FFI_WIN64},
    #elif defined(X86_64) || (defined (__x86_64__) && defined (X86_DARWIN))
        {"unix64", FFI_UNIX64},
    #elif defined(X86_ANY)
        {"sysv", FFI_SYSV},
    #endif
        { "default", FFI_DEFAULT_ABI}
};

static Handle mkAbitab(TaskData* taskData, void*, char* p);

static Handle toSysWord(TaskData* taskData, void* p)
{
    return Make_sysword(taskData, (uintptr_t)p);
}

// Convert the Poly type info into ffi_type values.
/*
    datatype cTypeForm =
            CTypeFloatingPt | CTypePointer | CTypeSignedInt | CTypeUnsignedInt
        |   CTypeStruct of cType list | CTypeVoid
        withtype cType = { typeForm: cTypeForm, align: word, size: word }
*/
static ffi_type* decodeType(PolyWord pType)
{
    PolyWord typeForm = pType.AsObjPtr()->Get(2);
    PolyWord typeSize = pType.AsObjPtr()->Get(0);

    if (typeForm.IsDataPtr())
    {
        // Struct
        size_t size = typeSize.UnTaggedUnsigned();
        unsigned short align = (unsigned short)pType.AsObjPtr()->Get(1).UnTaggedUnsigned();
        unsigned nElems = 0;
        PolyWord listStart = typeForm.AsObjPtr()->Get(0);
        for (PolyWord p = listStart; !ML_Cons_Cell::IsNull(p); p = ((ML_Cons_Cell*)p.AsObjPtr())->t)
            nElems++;
        size_t space = sizeof(ffi_type);
        // Add space for the elements plus one extra for the zero terminator.
        space += (nElems + 1) * sizeof(ffi_type*);
        ffi_type* result = (ffi_type*)calloc(1, space);
        // Raise an exception rather than returning zero.
        if (result == 0) return 0;
        ffi_type** elem = (ffi_type**)(result + 1);
        result->size = size;
        result->alignment = align;
        result->type = FFI_TYPE_STRUCT;
        result->elements = elem;
        if (elem != 0)
        {
            for (PolyWord p = listStart; !ML_Cons_Cell::IsNull(p); p = ((ML_Cons_Cell*)p.AsObjPtr())->t)
            {
                PolyWord e = ((ML_Cons_Cell*)p.AsObjPtr())->h;
                ffi_type* t = decodeType(e);
                if (t == 0) return 0;
                *elem++ = t;
            }
            *elem = 0; // Null terminator
        }
        return result;
    }
    else
    {
        switch (typeForm.UnTaggedUnsigned())
        {
        case 0:
        {
            // Floating point
            if (typeSize.UnTaggedUnsigned() == ffi_type_float.size)
                return &ffi_type_float;
            else if (typeSize.UnTaggedUnsigned() == ffi_type_double.size)
                return &ffi_type_double;
            ASSERT(0);
        }
        case 1: // FFI type poiner
            return &ffi_type_pointer;
        case 2: // Signed integer.
        {
            switch (typeSize.UnTaggedUnsigned())
            {
            case 1: return &ffi_type_sint8;
            case 2: return &ffi_type_sint16;
            case 4: return &ffi_type_sint32;
            case 8: return &ffi_type_sint64;
            default: ASSERT(0);
            }
        }
        case 3: // Unsigned integer.
        {
            switch (typeSize.UnTaggedUnsigned())
            {
            case 1: return &ffi_type_uint8;
            case 2: return &ffi_type_uint16;
            case 4: return &ffi_type_uint32;
            case 8: return &ffi_type_uint64;
            default: ASSERT(0);
            }
        }
        case 4: // Void
            return &ffi_type_void;
        }
        ASSERT(0);
    }
    return 0;
}

// Create a CIF.  This contains all the types and some extra information.
// The arguments are the raw ML values.  That does make this dependent on the
// representations used by the compiler.
// This mallocs space for the CIF and the types.  The space is never freed.
// 
POLYUNSIGNED PolyInterpretedCreateCIF(FirstArgument threadId, PolyWord abiValue, PolyWord resultType, PolyWord argTypes)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;
    ffi_abi abi = (ffi_abi)get_C_ushort(taskData, abiValue);

    try {
        unsigned nArgs = 0;
        for (PolyWord p = argTypes; !ML_Cons_Cell::IsNull(p); p = ((ML_Cons_Cell*)p.AsObjPtr())->t)
            nArgs++;
        // Allocate space for the cif followed by the argument type vector
        size_t space = sizeof(ffi_cif) + nArgs * sizeof(ffi_type*);
        ffi_cif* cif = (ffi_cif*)malloc(space);
        if (cif == 0) raise_syscall(taskData, "Insufficient memory", ENOMEM);
        ffi_type* rtype = decodeType(resultType);
        if (rtype == 0) raise_syscall(taskData, "Insufficient memory", ENOMEM);
        ffi_type** atypes = (ffi_type**)(cif + 1);
        // Copy the arguments types.
        ffi_type** at = atypes;
        for (PolyWord p = argTypes; !ML_Cons_Cell::IsNull(p); p = ((ML_Cons_Cell*)p.AsObjPtr())->t)
        {
            PolyWord e = ((ML_Cons_Cell*)p.AsObjPtr())->h;
            ffi_type *atype = decodeType(e);
            if (atype == 0) raise_syscall(taskData, "Insufficient memory", ENOMEM);
            *at++ = atype;
        }
        ffi_status status = ffi_prep_cif(cif, abi, nArgs, rtype, atypes);
        if (status == FFI_BAD_TYPEDEF)
            raise_exception_string(taskData, EXC_foreign, "Bad typedef in ffi_prep_cif");
        else if (status == FFI_BAD_ABI)
            raise_exception_string(taskData, EXC_foreign, "Bad ABI in ffi_prep_cif");
        else if (status != FFI_OK)
            raise_exception_string(taskData, EXC_foreign, "Error in ffi_prep_cif");
        result = toSysWord(taskData, cif);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Call a function.
POLYUNSIGNED PolyInterpretedCallFunction(FirstArgument threadId, PolyWord cifAddr, PolyWord cFunAddr, PolyWord resAddr, PolyWord argVec)
{
    ffi_cif* cif = *(ffi_cif**)cifAddr.AsAddress();
    void* f = *(void**)cFunAddr.AsAddress();
    void* res = *(void**)resAddr.AsAddress();
    void* arg = *(void**)argVec.AsAddress();
    // Poly passes the arguments as values, effectively a single struct.
    // Libffi wants a vector of addresses.
    void** argVector = (void**)calloc(cif->nargs + 1, sizeof(void*));
    unsigned n = 0;
    uintptr_t p = (uintptr_t)arg;
    while (n < cif->nargs)
    {
        uintptr_t align = cif->arg_types[n]->alignment;
        p = (p + align - 1) & (0-align);
        argVector[n] = (void*)p;
        p += cif->arg_types[n]->size;
        n++;
    }
    // The result area we have provided is only as big as required.
    // Libffi may need a larger area.
    if (cif->rtype->size < FFI_SIZEOF_ARG)
    {
        char result[FFI_SIZEOF_ARG];
        ffi_call(cif, FFI_FN(f), &result, argVector);
        if (cif->rtype->type != FFI_TYPE_VOID)
            memcpy(res, result, cif->rtype->size);
    }
    else ffi_call(cif, FFI_FN(f), res, argVector);
    free(argVector);
    return TAGGED(0).AsUnsigned();
}

#else
// Libffi is not present.

// A basic table so that the Foreign structure will compile
static struct _abiTable { const char* abiName; int abiCode; } abiTable[] =
{
        { "default", 0}
};

// Don't raise an exception at this point so we can build calls.
POLYUNSIGNED PolyInterpretedCreateCIF(FirstArgument threadId, PolyWord abiValue, PolyWord resultType, PolyWord argTypes)
{
    return TAGGED(0).AsUnsigned();
}

POLYUNSIGNED PolyInterpretedCallFunction(FirstArgument threadId, PolyWord cifAddr, PolyWord cFunAddr, PolyWord resAddr, PolyWord argVec)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    raise_exception_string(taskData, EXC_foreign, "Foreign function calling is not available.  Libffi is not installled.");
    return TAGGED(0).AsUnsigned();
}

#endif

// Construct an entry in the ABI table.
static Handle mkAbitab(TaskData* taskData, void* arg, char* p)
{
    struct _abiTable* ab = (struct _abiTable*)p;
    // Construct a pair of the string and the code
    Handle name = taskData->saveVec.push(C_string_to_Poly(taskData, ab->abiName));
    Handle code = Make_arbitrary_precision(taskData, ab->abiCode);
    Handle result = alloc_and_save(taskData, 2);
    result->WordP()->Set(0, name->Word());
    result->WordP()->Set(1, code->Word());
    return result;
}

// Get ABI list.  This is called once only before the basis library is built.
POLYUNSIGNED PolyInterpretedGetAbiList(FirstArgument threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = makeList(taskData, sizeof(abiTable) / sizeof(abiTable[0]),
            (char*)abiTable, sizeof(abiTable[0]), 0, mkAbitab);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Do we require EnterInt instructions and if so for which architecture?
// 0 = > None; 1 => X86_32, 2 => X86_64. 3 => X86_32_in_64.
POLYUNSIGNED PolyInterpretedEnterIntMode()
{
    return TAGGED(0).AsUnsigned();
}

static Interpreter interpreterObject;

MachineDependent *machineDependent = &interpreterObject;

// No machine-specific calls in the interpreter.
struct _entrypts machineSpecificEPT[] =
{
    { "PolyInterpretedGetAbiList",           (polyRTSFunction)&PolyInterpretedGetAbiList },
    { "PolyInterpretedCreateCIF",            (polyRTSFunction)&PolyInterpretedCreateCIF },
    { "PolyInterpretedCallFunction",         (polyRTSFunction)&PolyInterpretedCallFunction },
    { "PolyInterpretedEnterIntMode",         (polyRTSFunction)&PolyInterpretedEnterIntMode },
   { NULL, NULL} // End of list.
};
