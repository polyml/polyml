/*
    Title:  Machine dependent code for i386 and X64 under Windows and Unix

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

#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
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

#ifndef X86_64
#define CHECKED_REGS    6
#else /* X86_64 */
#define CHECKED_REGS    13
#endif /* X86_64 */
/* The unchecked reg field is used for the condition codes. */
#define UNCHECKED_REGS  1

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


class X86Dependent: public MachineDependent {
public:
    X86Dependent(): allocSpace(0), allocReg(0), allocWords(0) {}

    virtual void InitStackFrame(Handle stack, Handle proc, Handle arg);
    virtual unsigned InitialStackSize(void) { return 128+OVERFLOW_STACK_SIZE; } // Initial size of a stack 
    virtual int SwitchToPoly(void);
    virtual void SetForRetry(int ioCall);
    virtual void InitInterfaceVector(void);
    virtual void SetException(StackObject *stack,poly_exn *exc);
    virtual void ResetSignals(void);
    virtual void ScanConstantsWithinCode(PolyObject *addr, PolyObject *oldAddr, POLYUNSIGNED length, ScanAddress *process);
    virtual void InterruptCode(void);
    virtual int  GetIOFunctionRegisterMask(int ioCall);
    virtual bool GetPCandSPFromContext(SIGNALCONTEXT *context, PolyWord *&sp, POLYCODEPTR &pc);
    virtual bool InRunTimeSystem(void);
    virtual void CallIO0(Handle(*ioFun)(void));
    virtual void CallIO1(Handle(*ioFun)(Handle));
    virtual void CallIO2(Handle(*ioFun)(Handle, Handle));
    virtual void CallIO3(Handle(*ioFun)(Handle, Handle, Handle));
    virtual void CallIO4(Handle(*ioFun)(Handle, Handle, Handle, Handle));
    virtual void CallIO5(Handle(*ioFun)(Handle, Handle, Handle, Handle, Handle));
    virtual Handle CallBackResult(void) { return callBackResult; } 
    virtual void SetExceptionTrace(void);
    virtual void CallCodeTupled(void);
    virtual Architectures MachineArchitecture(void)
#ifndef X86_64
         { return MA_I386; }
#else /* X86_64 */
         { return MA_X86_64; }
#endif /* X86_64 */
    virtual void SetCodeConstant(Handle data, Handle constant, Handle offseth, Handle base);
    virtual void SetCallbackFunction(Handle func, Handle args);
#ifndef X86_64
    virtual unsigned char *BuildCallback(int cbEntryNo, Handle cResultType, int nArgsToRemove);
    virtual void GetCallbackArg(void **args, void *argLoc, int nSize);
#endif

private:
    void SetMemRegisters(void);
    void SaveMemRegisters(void);
    Handle BuildCodeSegment(const byte *code, unsigned bytes, char functionName);
    Handle BuildKillSelf(void);
    void HeapOverflowTrap(void);
    void ArbitraryPrecisionTrap(void);
    PolyWord *get_reg(int n);
    void do_op(int dest, PolyWord v1, PolyWord v2, Handle (*op)(Handle, Handle));
    bool emulate_instrs(void);

    LocalMemSpace *allocSpace;
    unsigned allocReg; // The register to take the allocated space.
    POLYUNSIGNED allocWords; // The words to allocate.
    Handle callBackResult;
};



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
 * Register fields in poly_stack. 
 *
 **********************************************************************/
#define EAX     poly_stack->p_reg[0]
#define EBX     poly_stack->p_reg[1]
#define ECX     poly_stack->p_reg[2]
#define EDX     poly_stack->p_reg[3]
#define ESI     poly_stack->p_reg[4]
#define EDI     poly_stack->p_reg[5]

// X64 registers only
#define R8      poly_stack->p_reg[6]
#define R9      poly_stack->p_reg[7]
#define R10     poly_stack->p_reg[8]
#define R11     poly_stack->p_reg[9]
#define R12     poly_stack->p_reg[10]
#define R13     poly_stack->p_reg[11]
#define R14     poly_stack->p_reg[12]

#define EFLAGS  poly_stack->p_reg[CHECKED_REGS+1]

#define IC      poly_stack->p_pc
#define INCR_PC(n) poly_stack->p_pc += n
#define SP      poly_stack->p_sp
#define HR      poly_stack->hr  

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

// These "memory registers" are referenced from the assembly code.
// Some are actually referenced from ML code so the offsets are built in.
static struct MemRegisters {
    // These offsets are built into the code generator and assembly code
    PolyWord    *localMpointer;     // Allocation ptr + 1 word
    PolyWord    *handlerRegister;   // Current exception handler
    // These next two are checked using a BOUNDS instruction.  That compares
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
}
  memRegisters;

extern "C" {


    // These are declared in the assembly code segment.
    void X86AsmSwitchToPoly(struct MemRegisters *);
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

};

bool X86Dependent::InRunTimeSystem(void) { return memRegisters.inRTS != 0; }

// Run the current ML process.  X86AsmSwitchToPoly saves the C state so that
// whenever the ML requires assistance from the rest of the RTS it simply
// returns to C with the appropriate values set in memRegisters.requestCode and
// 

int X86Dependent::SwitchToPoly(void)
// (Re)-enter the Poly code from C.  Returns with the io function to call or
// -1 if we are responding to an interrupt.
{
    do
    {
        CheckMemory(); // Do any memory checking before calling SetMemRegisters
                       // (which may set pc to a temporarily bad value if this is a retry).
        SetMemRegisters();

        X86AsmSwitchToPoly(&memRegisters);

        SaveMemRegisters(); // Update globals from the memory registers.

        // Handle any heap/stack overflows or arbitrary precision traps.
        switch (memRegisters.returnReason)
        {

        case RETURN_IO_CALL:
            return memRegisters.requestCode;

        case RETURN_HEAP_OVERFLOW:
            try {
                // The heap has overflowed.  Pop the return address into the program counter.
                // It may well not be a valid code address anyway.
                IC = (*SP++).AsCodePtr();
                HeapOverflowTrap();
            }
            catch (IOException) {
                // We may get an exception while handling this if we run out of store
            }
            break;

        case RETURN_STACK_OVERFLOW:
            try {
                // The stack check has failed.  This may either be because we really have
                // overflowed the stack or because the stack limit value has been adjusted
                // to result in a call here.
                IC = (*SP++).AsCodePtr();
                check_current_stack_size(poly_stack->p_sp);
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
                PolyWord *stackP = EDI.AsStackAddr();
                EDI = TAGGED(0);
                IC = (*SP++).AsCodePtr();
                check_current_stack_size(stackP);
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
                IC = (*SP++).AsCodePtr();
                // Set all the registers to a safe value here.  We will almost certainly
                // have shifted a value in one of the registers before testing it for zero.
                for (POLYUNSIGNED i = 0; i < poly_stack->p_nreg; i++)
                    poly_stack->p_reg[i] = TAGGED(0);
                raise_exception0(EXC_divide);
            }
            catch (IOException) {
                // Handle the C++ exception.
            }
            break;

        case RETURN_ARB_EMULATION:
            try {
                IC = (*SP++).AsCodePtr();
                ArbitraryPrecisionTrap();
            }
            catch (IOException) {
                // We may get an exception in the trap handler e.g. if we run out of store.
            }
            break;

        case RETURN_CALLBACK_RETURN:
            // Remove the extra exception handler we created in SetCallbackFunction
            ASSERT(poly_stack->p_hr == SP);
            SP += 2;
            poly_stack->p_hr = (*(SP++)).AsStackAddr(); // Restore the previous handler.
            callBackResult = gSaveVec->push(EAX); // Argument to return is in EAX.
            // Restore the registers
#ifdef X86_64
            R10 = *SP++;
            R9 = *SP++;
            R8 = *SP++;
#endif
            EBX = *SP++;
            EAX = *SP++;
            EDX = *SP++;
            poly_stack->p_pc = (*SP).AsCodePtr(); // Set the return address
            return -2;

        case RETURN_CALLBACK_EXCEPTION:
            // An ML callback has raised an exception.
            SetException(poly_stack, (poly_exn *)EAX.AsObjPtr());
            // Raise a C++ exception.  If the foreign function that called this callback
            // doesn't handle the exception it will be raised in the calling ML function.
            // But if it is caught we may have a problem ...
            THROW_EXCEPTION;

        default:
            Crash("Unknown return reason code %u", memRegisters.returnReason);
        }

    } while (1);
}

void X86Dependent::InitStackFrame(Handle stackh, Handle proc, Handle arg)
/* Initialise stack frame. */
{
    StackObject *stack = (StackObject *)DEREFWORDHANDLE(stackh);
    POLYUNSIGNED stack_size     = stack->Length();
    stack->p_space = OVERFLOW_STACK_SIZE;
    stack->p_pc    = PC_RETRY_SPECIAL;
    stack->p_sp    = stack->Offset(stack_size-4); 
    stack->p_hr    = stack->Offset(stack_size-3);
    stack->p_nreg  = CHECKED_REGS;

    for (POLYUNSIGNED i = 0; i < CHECKED_REGS; i++) stack->p_reg[i] = TAGGED(0);

    stack->p_reg[CHECKED_REGS] = PolyWord::FromUnsigned(UNCHECKED_REGS); /* 1 unchecked register */
    stack->p_reg[CHECKED_REGS+1] = PolyWord::FromUnsigned(0);
    stack->p_reg[3] = DEREFWORDHANDLE(proc); /* rdx - closure pointer */

    /* If this function takes an argument store it in the argument register. */
    if (arg != 0) stack->p_reg[0] = DEREFWORD(arg);

    /* No previous handler so point it at itself. */
    stack->Set(stack_size-1, PolyWord::FromStackAddr(stack->Offset(stack_size-1)));
    // Set the default handler and return address to point to this code.
    // It's not necessary, on this architecture at least, to make these off-word
    // aligned since we're pointing at the start of some code.  That may be
    // necessary on e.g. the Sparc(?), which always subtracts two from a return
    // address before jumping to it.
    Handle killCode = BuildKillSelf();
    PolyWord killJump = killCode->Word();
    stack = (StackObject *)DEREFWORDHANDLE(stackh); // In case it's moved
    stack->Set(stack_size-2, killJump); // Default handler.
    /* Set up exception handler */
    stack->Set(stack_size-3, TAGGED(0)); /* Default handler. */
    /* Return address. */
    stack->Set(stack_size-4, killJump); // Return address
}

// IO Functions called indirectly from assembly code.
void X86Dependent::CallIO0(Handle (*ioFun)(void))
{
    gSaveVec->init();
    // Set the return address now.
    poly_stack->p_pc = (*SP).AsCodePtr();
    try {
        Handle result = (*ioFun)();
        EAX = result->Word();
        // If this is a normal return we can pop the return address.
        // If this has raised an exception, set for retry or changed process
        // we mustn't.  N,B, The return address could have changed because of GC
        SP++;
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

void X86Dependent::CallIO1(Handle (*ioFun)(Handle))
{
    poly_stack->p_pc = (*SP).AsCodePtr();
    Handle saved1 = gSaveVec->push(EAX);
    try {
        Handle result = (*ioFun)(saved1);
        EAX = result->Word();
        SP++; // Pop the return address.
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

void X86Dependent::CallIO2(Handle (*ioFun)(Handle, Handle))
{
    poly_stack->p_pc = (*SP).AsCodePtr();
    Handle saved1 = gSaveVec->push(EAX);
    Handle saved2 = gSaveVec->push(EBX);
    try {
        Handle result = (*ioFun)(saved2, saved1);
        EAX = result->Word();
        SP++;
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

void X86Dependent::CallIO3(Handle (*ioFun)(Handle, Handle, Handle))
{
    poly_stack->p_pc = (*SP).AsCodePtr();
    Handle saved1 = gSaveVec->push(EAX);
    Handle saved2 = gSaveVec->push(EBX);
#ifndef X86_64
    Handle saved3 = gSaveVec->push(SP[1]);
#else /* X86_64 */
    Handle saved3 = gSaveVec->push(R8);
#endif /* X86_64 */
    try {
        Handle result = (*ioFun)(saved3, saved2, saved1);
        EAX = result->Word();
#ifndef X86_64
        SP += 2; // Pop the return address and a stack arg.
#else /* X86_64 */
        SP++;
#endif /* X86_64 */
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

void X86Dependent::CallIO4(Handle (*ioFun)(Handle, Handle, Handle, Handle))
{
    poly_stack->p_pc = (*SP).AsCodePtr();
    Handle saved1 = gSaveVec->push(EAX);
    Handle saved2 = gSaveVec->push(EBX);
#ifndef X86_64
    Handle saved3 = gSaveVec->push(SP[2]);
    Handle saved4 = gSaveVec->push(SP[1]);
#else /* X86_64 */
    Handle saved3 = gSaveVec->push(R8);
    Handle saved4 = gSaveVec->push(R9);
#endif /* X86_64 */
    try {
        Handle result = (*ioFun)(saved4, saved3, saved2, saved1);
        EAX = result->Word();
#ifndef X86_64
        SP += 3; // Pop the return address and two stack args.
#else /* X86_64 */
        SP++;
#endif /* X86_64 */
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
void X86Dependent::CallIO5(Handle (*ioFun)(Handle, Handle, Handle, Handle, Handle))
{
    poly_stack->p_pc = (*SP).AsCodePtr();
    Handle saved1 = gSaveVec->push(EAX);
    Handle saved2 = gSaveVec->push(EBX);
#ifndef X86_64
    Handle saved3 = gSaveVec->push(SP[3]);
    Handle saved4 = gSaveVec->push(SP[2]);
    Handle saved5 = gSaveVec->push(SP[1]);
#else /* X86_64 */
    Handle saved3 = gSaveVec->push(R8);
    Handle saved4 = gSaveVec->push(R9);
    Handle saved5 = gSaveVec->push(R10);
#endif /* X86_64 */
    try {
        Handle result = (*ioFun)(saved5, saved4, saved3, saved2, saved1);
        EAX = result->Word();
#ifndef X86_64
        SP += 4; // Pop the return address and 3 stack args
#else /* X86_64 */
        SP++;
#endif /* X86_64 */
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
Handle X86Dependent::BuildCodeSegment(const byte *code, unsigned bytes, char functionName)
{
    POLYUNSIGNED codeWords = (bytes + sizeof(PolyWord)-1) / sizeof(PolyWord);
    POLYUNSIGNED words = codeWords + 6;
    Handle codeHandle = alloc_and_save(words, F_BYTE_BIT|F_MUTABLE_BIT);
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
    CodeSegmentFlags(gSaveVec->push(TAGGED(F_CODE_BIT)), codeHandle);
    return codeHandle;
}

// Set up a handler that, if it's called, will print an exception trace.
// If the handler isn't called the dummy handler is simply removed.
// This is tricky since when we "return" we actually need to run the new
// function.
void X86Dependent::SetExceptionTrace()
{
    poly_stack->p_pc = (*SP).AsCodePtr();
    Handle fun = gSaveVec->push(EAX);
    PolyObject *functToCall = fun->WordP();
    EDX = functToCall; // Closure address
    // Leave the return address where it is on the stack.
    poly_stack->p_pc = functToCall->Get(0).AsCodePtr(); // First word of closure is entry pt.
    *(--SP) = PolyWord::FromStackAddr(poly_stack->p_hr); // Create a special handler entry
    // We have to use a special entry here that can be recognised by the exception
    // unwinding code because we want to know the stack pointer that is in effect
    // at the time the exception is raised.  If we simply put a normal handler in here
    // that handler would be called after the stack was unwound.
    *(--SP) = TAGGED(0);
    *(--SP) = TAGGED(0);
    poly_stack->p_hr = SP;
    byte *codeAddr;
#ifndef __GNUC__
#ifdef X86_64
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
#ifndef X86_64
        "addl    $8,%%esp;"
        "popl    4(%%ebp);"
#else /* X86_64 */
        "addq    $16,%%rsp;"
        "popq    8(%%rbp);"
#endif /* X86_64 */
        "ret;"
        "nop;"    // Add an extra byte so that we have 8 bytes on both X86 and X86_64
    "1: pop %0"
    :"=r"(codeAddr)
    );
#endif
    Handle retCode = BuildCodeSegment(codeAddr, 8 /* Code is 8 bytes */, 'R');
    *(--SP) = retCode->WordP(); // Code for normal return.
    EAX = TAGGED(0); // Set the argument of the function to "unit".
}

// Get the PC and SP from a signal context.  This is needed for profiling.
bool X86Dependent::GetPCandSPFromContext(SIGNALCONTEXT *context, PolyWord * &sp, POLYCODEPTR &pc)
{
    if (memRegisters.inRTS)
    {
        sp = poly_stack->p_sp;
        pc = poly_stack->p_pc;
        return true;
    }
#ifdef _WIN64
    sp = (PolyWord *)context->Rsp;
    pc = (POLYCODEPTR)context->Rip;
#elif defined(WINDOWS_PC)
    // Windows 32
    sp = (PolyWord *)context->Esp;
    pc = (POLYCODEPTR)context->Eip;
#elif defined(HAVE_UCONTEXT_T)
#ifdef HAVE_MCONTEXT_T_GREGS
    // Linux
#ifndef X86_64
    pc = (byte*)context->uc_mcontext.gregs[REG_EIP];
    sp = (PolyWord*)context->uc_mcontext.gregs[REG_ESP];
#else /* X86_64 */
    pc = (byte*)context->uc_mcontext.gregs[REG_RIP];
    sp = (PolyWord*)context->uc_mcontext.gregs[REG_RSP];
#endif /* X86_64 */
#elif defined(HAVE_MCONTEXT_T_MC_ESP)
   // FreeBSD
#ifndef X86_64
    pc = (byte*)context->uc_mcontext.mc_eip;
    sp = (PolyWord*)context->uc_mcontext.mc_esp;
#else /* X86_64 */
    pc = (byte*)context->uc_mcontext.mc_rip;
    sp = (PolyWord*)context->uc_mcontext.mc_rsp;
#endif /* X86_64 */
#else
   // Mac OS X
#ifndef X86_64
#ifdef HAVE_X86_THREAD_STATE32_T
    pc = (byte*)context->uc_mcontext->ss.eip;
    sp = (PolyWord*)context->uc_mcontext->ss.esp;
#else
    return false;
#endif
#else /* X86_64 */
#ifdef  HAVE_X86_THREAD_STATE64_T
    pc = (byte*)context->uc_mcontext->ss.rip;
    sp = (PolyWord*)context->uc_mcontext->ss.rsp;
#else
    return false;
#endif
#endif /* X86_64 */
#endif
#elif defined(HAVE_STRUCT_SIGCONTEXT)
    pc = (byte*)context->sc_pc;
    sp = (PolyWord*)context->sc_sp;
#else
    // Can't get context.
    return false;
#endif
    // Check the sp value is in the current stack.
    if (sp >= (PolyWord*)poly_stack && sp < poly_stack->Offset(poly_stack->Length()))
        return true;
    else
        return false; // Bad stack pointer
}

void X86Dependent::InterruptCode(void)
{
    // Set the stack limit pointer to the top of the stack to cause
    // a trap when we next check for stack overflow.
    // SetMemRegisters actually does this anyway if "interrupted" is set but
    // it's safe to do this anyway.
    if (poly_stack != 0) 
        memRegisters.stackLimit = poly_stack->Offset(poly_stack->Length()-1); 
}

// This is called from SwitchToPoly before we enter the ML code.
void X86Dependent::SetMemRegisters(void)
{
    // Copy the current store limits into variables before we go into the assembly code.

    // Find a space to allocate in.  We may have garbage collected in an IO function
    // so the old value of allocSpace could be wrong.
    allocSpace = gMem.GetLargestSpace();

    if (allocWords != 0)
    {
        // If we have had a heap trap we actually do the allocation here.
        // We will have already garbage collected and recovered sufficient space.
        // This also happens if we have just trapped because of store profiling.
        allocSpace->pointer -= allocWords;
        ASSERT(allocSpace->pointer >= allocSpace->bottom);

#ifndef X86_64
        // Set the allocation register to this area.
        *(get_reg(allocReg)) = PolyWord::FromStackAddr(allocSpace->pointer + 1); /* remember: it's off-by-one */
#endif
        allocWords = 0;
    }

    memRegisters.localMbottom = allocSpace->bottom + 1;
    memRegisters.localMpointer = allocSpace->pointer + 1;
    // If we are profiling store allocation we set mem_hl so that a trap
    // will be generated.
    if (store_profiling || (userOptions.debug & (DEBUG_FORCEGC|DEBUG_REGION_CHECK)))
        memRegisters.localMbottom = memRegisters.localMpointer;

    memRegisters.polyStack = poly_stack;
    // Whenever the ML code enters a function it checks that the stack pointer is above
    // this value.  The default is to set it to the top of the reserved area
    // but if we've had an interrupt we set it to the end of the stack.
    memRegisters.stackTop = poly_stack->Offset(poly_stack->Length() - 1);
    memRegisters.stackLimit = poly_stack->Offset(poly_stack->p_space);
    if (interrupted)
        memRegisters.stackLimit = memRegisters.stackTop;
    memRegisters.handlerRegister = poly_stack->p_hr;
    memRegisters.requestCode = 0; // Clear these because only one will be set.
    memRegisters.returnReason = RETURN_IO_CALL;

    // We set the PC to zero to indicate that we should retry the call to the RTS
    // function.  In that case we need to set it back to the code address before we
    // return.  We can't do that earlier because we have written the address to the
    // database.
    if (IC == PC_RETRY_SPECIAL)
        poly_stack->p_pc = EDX.AsObjPtr()->Get(0).AsCodePtr();
}

// This is called whenever we have returned from ML to C.
void X86Dependent::SaveMemRegisters(void)
{
    // Check a few items on the stack to see it hasn't been overwritten
    if (! poly_stack->IsStackObject() || poly_stack->p_space != OVERFLOW_STACK_SIZE ||
          poly_stack->p_nreg != CHECKED_REGS || poly_stack->p_reg[CHECKED_REGS] != PolyWord::FromUnsigned(UNCHECKED_REGS))
        Crash("Stack overwritten\n");
    allocSpace->pointer = memRegisters.localMpointer - 1;
    poly_stack->p_hr = memRegisters.handlerRegister;
    allocWords = 0;
}

// Called if we need the ML code to retry an RTS call.
void X86Dependent::SetForRetry(int ioCall)
{
    /* We now have to set the closure entry for the RTS call to work.
       DCJM 4/1/01. */
    EDX = (PolyObject*)IoEntry(ioCall);
    poly_stack->p_pc = PC_RETRY_SPECIAL; // This value is treated specially in SetMemRegisters
}

PolyWord *X86Dependent::get_reg(int n)
/* Returns a pointer to the register given by n. */
{
  switch (n) 
    {
      case 0: return &EAX;
      case 1: return &ECX;
      case 2: return &EDX;
      case 3: return &EBX;
      case 4: return (PolyWord*)&poly_stack->p_sp;
      case 6: return &ESI;
      case 7: return &EDI;
#ifdef X86_64
      case 8: return &R8;
      case 9: return &R9;
      case 10: return &R10;
      case 11: return &R11;
      case 12: return &R12;
      case 13: return &R13;
      case 14: return &R14;
      // R15 is the heap pointer so shouldn't occur here.
#endif /* X86_64 */
      default: 
        Crash("Unknown register %d at %p\n", n, IC);
    }
}

// Called as a result of a heap overflow trap
void X86Dependent::HeapOverflowTrap(void)
{
    POLYUNSIGNED wordsNeeded = 0;
    // The next instruction, after any branches round forwarding pointers, will
    // be a store of register containing the adjusted heap pointer.  We need to
    // find that register and the value in it in order to find out how big the
    // area we actually wanted is.
    while (IC[0] == 0xeb)
    {
        if (IC[1] >= 128)
            IC += 256 - IC[1] + 2;
        else IC += IC[1] + 2;
    }
#ifndef X86_64
    // This should be movl REG,0[%ebp].
    ASSERT(IC[0] == 0x89);
    allocReg = (IC[1] >> 3) & 7; // Remember this until we allocate the memory
    PolyWord *reg = get_reg(allocReg);
    PolyWord reg_val = *reg;
    // The space we need is the difference between this register
    // and the current value of newptr.
    // The +1 here is because memRegisters.localMpointer is A.M.pointer +1.  The reason
    // is that after the allocation we have the register pointing at the address we will
    // actually use.
    wordsNeeded = (allocSpace->pointer - (PolyWord*)reg_val.AsAddress()) + 1;
    *reg = TAGGED(0); // Clear this - it's not a valid address.
    /* length in words, including length word */

    ASSERT (wordsNeeded <= (1<<24)); /* Max object size including length/flag word is 2^24 words.  */
#else /* X86_64 */
    // This should be movq Length,-8(%r15)
    ASSERT(IC[0] == 0x49 && IC[1] == 0xc7 && IC[2] == 0x47 && IC[3] == 0xf8);
    // The Length field should be in the next word.  N.B.  This assumes that
    // the length word < 2^31.
    ASSERT((IC[7] & 0x80) == 0); // Should not be negative
    for (unsigned i = 7; i >= 4; i--) wordsNeeded = (wordsNeeded << 8) | IC[i];
    wordsNeeded += 1; // That was the object size. We need to add one for the length word.
#endif /* X86_64 */
    
    if (store_profiling)
        add_count(IC, SP, wordsNeeded);

#ifdef X86_64
    // On the X64 the value that ends up in allocSpace->pointer includes the
    // attempted allocation.  Add back the space we tried to allocate
    allocSpace->pointer += wordsNeeded;
#endif /* X86_64 */


    if (allocSpace->pointer < allocSpace->bottom + wordsNeeded || (userOptions.debug & DEBUG_FORCEGC))
        /* a genuine storage request, not just (or as well as) a profiling trap */
    {

        if (allocSpace->pointer < allocSpace->bottom)
            Crash ("Bad length in heap overflow trap");

        // See if we have another space to satisfy the request without a GC.
        if ((userOptions.debug & DEBUG_FORCEGC))
            allocSpace = 0;
        else
            allocSpace = gMem.GetAllocSpace(wordsNeeded);

        if (allocSpace == 0)
        { // No.
            if (! QuickGC(wordsNeeded) ) /* Garbage-collect. */
            {
                fprintf(stderr,"Run out of store - interrupting console processes\n");
                processes->interrupt_console_processes();
                throw IOException(EXC_RETRY);
            }
        }
    }
    allocWords = wordsNeeded; // The actual allocation is done in SetMemRegisters.
}


/******************************************************************************/
/*                                                                            */
/*      do_compare - do a "long" comparison, setting the flags register       */
/*                                                                            */
/******************************************************************************/
static void do_compare(PolyWord v1, PolyWord v2)
{
    Handle val1, val2;
    /* Must push these to the save vec.  A persistent store trap
       might cause a garbage collection and move the stack. */
    val1 = gSaveVec->push(v1);
    val2 = gSaveVec->push(v2);
    int r = compareLong(val2, val1);
    /* Clear the flags. */
    POLYUNSIGNED flags = EFLAGS.AsUnsigned();
    flags &= -256;
    if (r == 0) flags |= 0x40;
    else if (r < 0) flags |= 0x80;
    EFLAGS = PolyWord::FromUnsigned(flags);
}

/******************************************************************************/
/*                                                                            */
/*      do_op - do a "long" operation, setting the destination register       */
/*                                                                            */
/******************************************************************************/
void X86Dependent::do_op(int dest, PolyWord v1, PolyWord v2, Handle (*op)(Handle, Handle))
{
    Handle val1, val2, result;
    /* Must push these to the save vec.  A persistent store trap
       or a garbage collection might move the stack. */
    val1 = gSaveVec->push(v1);
    val2 = gSaveVec->push(v2);
    /* Clobber the destination which may have overflowed. */
    *(get_reg(dest)) = TAGGED(0);
    result = op (val2, val1);     /* N.B parameters are intentionally reversed */
    /* N.B. the stack may have moved so we must recompute get_reg(dest). */
    *(get_reg(dest)) = DEREFWORD(result);
}

/***************************************************************
                                                            
          emulate_instrs - do a "long" instruction               
                                                             
          ARE THESE OK ?????

 ***************************************************************/
 
bool X86Dependent::emulate_instrs(void)
{
    int src1 = -1, src2 = -1, dest = -1;
    bool doneSubtraction = false;
    gSaveVec->init(); /* Reset it. */
    while(1) {
        byte rexPrefix = 0;
#ifdef X86_64
        // Get any REX prefix
        if (IC[0] >= 0x40 && IC[0] <= 0x4f)
        {
            rexPrefix = IC[0];
            INCR_PC(1);
        }
#endif /* X86_64 */
        // Decode the register fields and include any REX bits
        int bbb = IC[1] & 7;
        if (rexPrefix & 0x1) bbb += 8;
        int rrr = (IC[1] >> 3) & 7;
        if (rexPrefix & 0x4) rrr += 8;


        switch (IC[0]) {
        case 0x03: /* add. */
            if ((IC[1] & 0xc0) != 0xc0)
                Crash("Expected register");
            if (dest != rrr)
                Crash("Expected same destination register.");
            src2 = bbb;
            do_op(dest, *(get_reg(src1)), *(get_reg(src2)), add_longc);
            INCR_PC(2);
            return true;

        case 0x2b: /* Subtraction. */
            if ((IC[1] & 0xc0) != 0xc0)
                Crash("Expected register");
            if (dest != rrr)
                Crash("Expected same destination register.");
            src2 = bbb;
            do_op(dest, *(get_reg(src1)), *(get_reg(src2)), sub_longc);
            INCR_PC(2);
            // The next instruction should be a lea to put on the tag.
            // The result is already tagged so we need to skip that.
            // Previously this removed the tag and returned but CheckRegion
            // didn't like this.
            doneSubtraction = true;
            break;

        case 0x3b: /* Compare. */
            if ((IC[1] & 0xc0) != 0xc0)
                Crash("Expected register");
            src1 = rrr;
            src2 = bbb;
            do_compare(*(get_reg(src1)), *(get_reg(src2)));
            INCR_PC(2);
            return true;

        case 0x8d: /* leal - Used to remove a tag before an add and multiply. */
            // Also used to put the tag on after a subtraction.
            if ((IC[1] & 7) == 4)
            { // R12 (and RSP but that isn't used here) have to be encoded with a SIB byte.
                ASSERT((IC[2] & 7) == 4); // Should be same register
                INCR_PC(1);
            }
            if (doneSubtraction)
            {
                INCR_PC(3);
                return true;
            }
            if (src1 == -1) src1 = bbb; else src2 = bbb;
            dest = rrr;
            ASSERT(IC[2] == 0xff);
            INCR_PC(3);
            break;

        case 0x89: /* movl: move source into dest. */
            if ((IC[1] & 0xc0) != 0xc0)
                 Crash("Can't move into store.");
            dest = bbb;
            if (src1 == -1) src1 = rrr; else src2 = rrr;
            INCR_PC(2);
                /* Next should be add-immediate. */
            break;

        case 0x83: { /* One byte immediate: Add, sub or compare. */
            int cval = IC[2];
            if (cval >= 128) cval -= 256;

            switch (IC[1] & (7 << 3)) // This is a code.  Ignore any REX override.
            {
              case (0 << 3): /* add */
                      {
                if (dest != bbb)
                    Crash("Expected same destination register.");
                /* immediate value is shifted, but hasn't had 1 added;
                   do this now before calling add_longc */
                do_op(dest, *(get_reg(src1)), PolyWord::FromSigned(cval+1), add_longc);
                break;
              }

              case (5 << 3): /* sub */
              {
                if (dest != bbb)

                    Crash("Expected same destination register.");
                /* immediate value is shifted, but hasn't had 1 added;
                   do this now before calling sub_longc */
                do_op(dest, *(get_reg(src1)), PolyWord::FromSigned(cval+1), sub_longc);
                break;
              }

              case (7 << 3): /* cmp */
                      {
                if ((IC[1] & 0xc0) != 0xc0)
                    Crash("Can't test with store.");
                src1 = bbb;

                /* immediate value is already tagged */
                do_compare(*(get_reg(src1)), PolyWord::FromSigned(cval));
                break;
              }

             default: Crash("Unknown instruction after overflow trap");
            }

            INCR_PC(3);
            return true;
            }

        case 0x81: { /* 4 byte immediate: Add, sub or compare. */
            int cval = IC[5];
            if (cval >= 128) cval -= 256;
            cval = cval*256 + IC[4];
            cval = cval*256 + IC[3];
            cval = cval*256 + IC[2];
            if ((IC[1] & 0xc0) != 0xc0)
                Crash("Expected register");

            switch (IC[1] & (7 << 3))
            {
              case (0 << 3): /* add */
              {
                if (dest != bbb)

                    Crash("Expected same destination register.");
                /* immediate value is shifted, but hasn't had 1 added;
                   do this now before calling add_longc */
                do_op(dest, *(get_reg(src1)), PolyWord::FromSigned(cval+1), add_longc);
                break;
              }
              case (5 << 3): /* sub */
              {
                if (dest != bbb)
                    Crash("Expected same destination register.");
                /* immediate value is shifted, but hasn't had 1 added;
                   do this now before calling sub_longc */
                do_op(dest, *(get_reg(src1)), PolyWord::FromSigned(cval+1), sub_longc);
                break;
              }

              case (7 << 3): /* cmp */
              {
                src1 = bbb;
                /* immediate value is already tagged */
                do_compare(*(get_reg(src1)), PolyWord::FromSigned(cval));
                break;
              }

             default: Crash("Unknown instruction after overflow trap");
            }

            INCR_PC(6);
            return true;
            }

        case 0xeb: /* jmp - used in branch forwarding. */
            /* While forwarded jumps are always positive we may use backward
               branches in the future. */
            if (IC[1] >= 128)
                INCR_PC(256 - IC[1] + 2);
            else INCR_PC(IC[1] + 2);
            break;

        case 0x50: /* push eax - used before a multiply. */
#ifdef X86_64
            ASSERT((rexPrefix & 1) == 0); // Check it's not r8
#endif /* X86_64 */
            *(--SP) = EAX;
            INCR_PC(1);
            break;

        case 0x52: /* push edx - used before a multiply. */
#ifdef X86_64
            ASSERT((rexPrefix & 1) == 0); // Check it's not r10
#endif /* X86_64 */
            *(--SP) = EDX;
            INCR_PC(1);
            break;

        case 0xd1: /* Group1A - must be sar edx before a multiply. */
            if (IC[1] != 0xfa)
                Crash("Unknown instruction after overflow trap");
            INCR_PC(2);
            /* If we haven't moved anything into edx then edx must be
               one of the arguments. */
            if (src2 == -1) src2 = 2; /* edx. */
            break;

        case 0xf7: /* Multiply instruction. */
            if (IC[1] != 0xea)
                Crash("Unknown instruction after overflow trap");
            do_op(0 /* eax */, *(get_reg(src1)), *(get_reg(src2)), mult_longc);
            /* Subtract one because the next instruction will tag it. */
            EAX = PolyWord::FromUnsigned(EAX.AsUnsigned() - 1);
            INCR_PC(2);
            return true;

        default:
            Crash("Unknown instruction after overflow trap");
        }
    }
    return false;
}

void X86Dependent::ArbitraryPrecisionTrap(void)
{
    // Arithmetic operation has overflowed or detected long values.
    if (emulate_profiling)
        add_count(IC, SP, 1);
    // Emulate the arbitrary precision instruction.  
    if (! emulate_instrs())
        Crash("Arbitrary precision emulation fault at %x\n", IC);
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
#ifndef X86_64

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

#else /* X86_64 */
// Visual C++ on X64 doesn't support inline assembly code
#endif /* X86_64 */


#else

#ifndef X86_64

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

#else /* X86_64 */

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
#endif /* X86_64 */

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
    memRegisters.inRTS = 1; // We start off in the RTS.

    unsigned char *codeAddr;
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_exit, codeAddr);
    add_word_to_io_area(POLY_SYS_exit, PolyWord::FromCodePtr(codeAddr));

    MAKE_IO_CALL_SEQUENCE(POLY_SYS_install_root, codeAddr);
    add_word_to_io_area(POLY_SYS_install_root, PolyWord::FromCodePtr(codeAddr));

    MAKE_IO_CALL_SEQUENCE(POLY_SYS_strconcat, codeAddr);
    add_word_to_io_area(POLY_SYS_strconcat, PolyWord::FromCodePtr(codeAddr));

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
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Real_geq, codeAddr);
    add_word_to_io_area(POLY_SYS_Real_geq, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Real_leq, codeAddr);
    add_word_to_io_area(POLY_SYS_Real_leq, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Real_gtr, codeAddr);
    add_word_to_io_area(POLY_SYS_Real_gtr, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Real_lss, codeAddr);
    add_word_to_io_area(POLY_SYS_Real_lss, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Real_eq, codeAddr);
    add_word_to_io_area(POLY_SYS_Real_eq, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Real_neq, codeAddr);
    add_word_to_io_area(POLY_SYS_Real_neq, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Real_Dispatch, codeAddr);
    add_word_to_io_area(POLY_SYS_Real_Dispatch, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Add_real, codeAddr);
    add_word_to_io_area(POLY_SYS_Add_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Sub_real, codeAddr);
    add_word_to_io_area(POLY_SYS_Sub_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Mul_real, codeAddr);
    add_word_to_io_area(POLY_SYS_Mul_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Div_real, codeAddr);
    add_word_to_io_area(POLY_SYS_Div_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Neg_real, codeAddr);
    add_word_to_io_area(POLY_SYS_Neg_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_Repr_real, codeAddr);
    add_word_to_io_area(POLY_SYS_Repr_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_conv_real, codeAddr);
    add_word_to_io_area(POLY_SYS_conv_real, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_real_to_int, codeAddr);
    add_word_to_io_area(POLY_SYS_real_to_int, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_int_to_real, codeAddr);
    add_word_to_io_area(POLY_SYS_int_to_real, PolyWord::FromCodePtr(codeAddr));
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
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_fork_process, codeAddr);
    add_word_to_io_area(POLY_SYS_fork_process, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_choice_process, codeAddr);
    add_word_to_io_area(POLY_SYS_choice_process, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_int_process, codeAddr);
    add_word_to_io_area(POLY_SYS_int_process, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_send_on_channel, codeAddr);
    add_word_to_io_area(POLY_SYS_send_on_channel, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_receive_on_channel, codeAddr);
    add_word_to_io_area(POLY_SYS_receive_on_channel, PolyWord::FromCodePtr(codeAddr));
    
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

    MAKE_IO_CALL_SEQUENCE(POLY_SYS_string_sub, codeAddr);
    add_word_to_io_area(POLY_SYS_string_sub, PolyWord::FromCodePtr(codeAddr)); // This is really redundant.
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
    
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_interrupt_console_processes, codeAddr);
    add_word_to_io_area(POLY_SYS_interrupt_console_processes, PolyWord::FromCodePtr(codeAddr));
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_install_subshells, codeAddr);
    add_word_to_io_area(POLY_SYS_install_subshells, PolyWord::FromCodePtr(codeAddr));
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
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_HEAP_OVERFLOW, memRegisters.heapOverflow);
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_STACK_OVERFLOW, memRegisters.stackOverflow);
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_STACK_OVERFLOWEX, memRegisters.stackOverflowEx);
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_RAISE_DIV, memRegisters.raiseDiv);
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_ARB_EMULATION, memRegisters.arbEmulation);

    // Point "raiseException" at the assembly code for "raisex"
    memRegisters.raiseException = (byte*)raisex;
    // Entry point to save the state for an IO call.  This is the common entry
    // point for all the return and IO-call cases.
    memRegisters.ioEntry = (byte*)X86AsmSaveStateAndReturn;
}

// We need the kill-self code in a little function.
Handle X86Dependent::BuildKillSelf(void)
{
    byte *codeAddr;
    MAKE_IO_CALL_SEQUENCE(POLY_SYS_kill_self, codeAddr);
    return BuildCodeSegment(codeAddr, MAKE_CALL_SEQUENCE_BYTES, 'K');
}

void X86Dependent::SetException(StackObject *stack, poly_exn *exc)
// Set up the stack of a process to raise an exception.
{
    stack->p_reg[3] = (PolyObject*)IoEntry(POLY_SYS_raisex);
    stack->p_pc     = PC_RETRY_SPECIAL;
    stack->p_reg[0] = exc; /* put exception data into eax */
}

void X86Dependent::ResetSignals(void)
{
    /* restore default signal handling. */
    /* SIGILL, SIGEMT are not used in PC version */
    signal(SIGFPE,  SIG_DFL);
}

// Call a piece of compiled code.
void X86Dependent::CallCodeTupled(void)
{
    // The eventual return address is on the stack - leave it there.
    PolyObject *argTuple = EAX.AsObjPtr();
    Handle closure = gSaveVec->push(argTuple->Get(0));
    Handle argvec = gSaveVec->push(argTuple->Get(1));

    if (! IS_INT(DEREFWORD(argvec))) // May be nil if there are no args.
    {
        PolyObject *argv = DEREFHANDLE(argvec);
        POLYUNSIGNED argCount = argv->Length();
        // Check we have space for the arguments.  This may result in a GC which
        // in turn may throw a C++ exception.
        if (argCount > 2)
        {
            try {
                check_current_stack_size(poly_stack->p_sp - (argCount - 2));
            }
            catch (IOException)
            {
                return; // Will have been set up to raise an exception.
            }
        }

        // First argument is in EAX
        EAX = argv->Get(0);
        // Second arg, if there is one, goes into EBX
        if (argCount > 1)
            EBX = argv->Get(1);
#ifdef X86_64
        if (argCount > 2)
            R8 = argv->Get(2);
        if (argCount > 3)
            R9 = argv->Get(3);
        if (argCount > 4)
            R10 = argv->Get(4);
#endif /* X86_64 */
        // Remaining args go on the stack.
#ifndef X86_64
        for (POLYUNSIGNED i = 2; i < argCount; i++)
            *(--SP) = argv->Get(i+2);
#else /* X86_64 */
        for (POLYUNSIGNED i = 5; i < argCount; i++)
            *(--SP) = argv->Get(i);
#endif /* X86_64 */
    }
    // The closure goes into the closure reg.
    EDX = DEREFWORD(closure);
    // First word of closure is entry point.
    IC = (EDX).AsObjPtr()->Get(0).AsCodePtr();
}

// Sets up a callback function on the current stack.  The present state is that
// the ML code has made a call in to foreign_dispatch.  We need to set the stack
// up so that we will enter the callback (as with CallCodeTupled) but when we return
// the result we enter callback_return. 
void X86Dependent::SetCallbackFunction(Handle func, Handle args)
{
    byte *codeAddr1, *codeAddr2;
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_CALLBACK_RETURN, codeAddr1);
    Handle callBackReturn = BuildCodeSegment(codeAddr1, MAKE_CALL_SEQUENCE_BYTES, 'C');
    MAKE_EXTRA_CALL_SEQUENCE(RETURN_CALLBACK_EXCEPTION, codeAddr2);
    Handle callBackException = BuildCodeSegment(codeAddr2, MAKE_CALL_SEQUENCE_BYTES, 'X');
    // Save the closure pointer and argument registers to the stack.  If we have to
    // retry the current RTS call we need these to have their original values.
    *(--SP) = EDX;
    *(--SP) = EAX;
    *(--SP) = EBX;
#ifdef X86_64
    *(--SP) = R8;
    *(--SP) = R9;
    *(--SP) = R10;
#endif
    // Set up an exception handler so we will enter callBackException if there is an exception.
    *(--SP) = PolyWord::FromStackAddr(poly_stack->p_hr); // Create a special handler entry
    *(--SP) = callBackException->Word();
    *(--SP) = TAGGED(0);
    poly_stack->p_hr = SP;
    // Push the call to callBackReturn onto the stack as the return address.
    *(--SP) = callBackReturn->Word();
    // Set up the entry point of the callback.
    PolyObject *functToCall = func->WordP();
    EDX = functToCall; // Closure address
    EAX = args->Word();
    poly_stack->p_pc = functToCall->Get(0).AsCodePtr(); // First word of closure is entry pt.
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
#ifndef X86_64
                process->ScanConstant(*pt, PROCESS_RELOC_DIRECT);
#endif /* X86_64 */
                (*pt) += 4;
            }
        }
        else if (md == 1) (*pt)++;
        else if (md == 2) (*pt) += 4;
    }
    else if (md == 0 && rm == 5)
    {
#ifndef X86_64
        /* Absolute address. */
        process->ScanConstant(*pt, PROCESS_RELOC_DIRECT);
#endif /* X86_64 */
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
#ifdef X86_64
        // REX prefixes.  Set this first.
        byte lastRex;
        if (*pt >= 0x40 && *pt <= 0x4f)
            lastRex = *pt++;
        else
            lastRex = 0;

        //printf("pt=%p *pt=%x\n", pt, *pt);

#endif /* X86_64 */
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

        case 0x8d: /* leal. */
            {
                pt++;
                if ((*pt & 0x3f) == 0x9)
                {
                    /* leal ecx,n(ecx).  If this is setting ecx
                       to be the constant pointer then we stop
                       processing here.  This isn't just an optimisation
                       for old code segments, it's needed because old
                       code segments used a jump table for indexed jumps
                       (case expressions) and these don't contain valid
                       i386 instructions. */
                    int md = (*pt) & 0xc0;
                    if (md == 0x40 || md == 0x80)
                    {
                        if ((pt[1] & 3) == 2) return;
                    }
                }
                skipea(&pt, process);
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
#ifdef X86_64
                    ASSERT(sizeof(PolyWord) == 4); // Should only be used internally on x64
#endif /* X86_64 */
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
#ifndef X86_64
                    process->ScanConstant(pt, PROCESS_RELOC_DIRECT);
#endif /* X86_64 */
                    pt += 4;
                }
                break;
            }

        case 0xb8: case 0xb9: case 0xba: case 0xbb:
        case 0xbc: case 0xbd: case 0xbe: case 0xbf: /* MOVL_32_64_R */
            pt ++;
#ifdef X86_64
            if ((lastRex & 8) == 0)
                pt += 4; // 32-bit mode on 64-bits.  Can this occur?
            else
#endif /* X86_64 */
            {
                // 32 bits in 32-bit mode, 64-bits in 64-bit mode.
                process->ScanConstant(pt, PROCESS_RELOC_DIRECT);
                pt += sizeof(PolyWord);
            }
            break;

        case 0x68: /* PUSH_32 */
            pt ++;
#ifndef X86_64
            process->ScanConstant(pt, PROCESS_RELOC_DIRECT);
#endif /* X86_64 */
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
void X86Dependent::SetCodeConstant(Handle data, Handle constant, Handle offseth, Handle base)
{
    POLYUNSIGNED offset = get_C_ulong(DEREFWORD(offseth)); // Byte offset
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

#ifndef X86_64
// This will only work on the i386.  The X86_64 uses different conventions with some arguments
// in registers.

/* We have to compile the callback function dynamically.  This code mallocs the store for it.
   At least at the moment, the store is never freed.  If we decide to garbage collect it
   we could store it in a vol. */
unsigned char *X86Dependent::BuildCallback(int cbEntryNo, Handle cResultType, int nArgsToRemove)
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
    cbAddr -= (int)p + 5; /* The instruction is 5 bytes long. */
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
        raise_exception_string(EXC_foreign, "Structure results from callbacks are not supported\n");
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

static X86Dependent x86Dependent;

MachineDependent *machineDependent = &x86Dependent;
