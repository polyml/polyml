/*
    Title:  Machine dependent code for i386 and X64 under Windows and Unix

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

    Further work copyright David C. J. Matthews 2011

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

#if (defined(_WIN32) && ! defined(__CYGWIN__))
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
// The unchecked reg field is used for the condition codes and FP save.
#define UNCHECKED_REGS  1 + (108/sizeof(PolyWord))

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

    virtual int SwitchToPoly(TaskData *taskData);
    virtual void SetForRetry(TaskData *taskData, int ioCall);
    virtual void InterruptCode(TaskData *taskData);
    virtual bool GetPCandSPFromContext(TaskData *taskData, SIGNALCONTEXT *context, PolyWord *&sp, POLYCODEPTR &pc);
    virtual void InitStackFrame(TaskData *taskData, StackSpace *space, Handle proc, Handle arg);
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
    PolyWord *getArgument(TaskData *taskData, unsigned int opByte, unsigned int rexPrefix, bool *inConsts=0);
    void do_compare(TaskData *taskData, PolyWord v1, PolyWord v2);
    void do_op(TaskData *taskData, int dest, PolyWord v1, PolyWord v2, Handle (*op)(TaskData *, Handle, Handle));
    bool emulate_instrs(TaskData *taskData);
    Handle BuildCodeSegment(TaskData *taskData, const byte *code, unsigned bytes, char functionName);
    Handle BuildKillSelf(TaskData *taskData);
    Handle BuildExceptionTrace(TaskData *taskData);
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

// Layout of base of stack on X86.  The general format is
// standard for all architectures to simplify the GC.
class PolyX86Stack: public PolyObject {
public:
    POLYUNSIGNED    p_space;
    POLYCODEPTR     p_pc;
    PolyWord        *p_sp;
    PolyWord        *p_hr;
    POLYUNSIGNED    p_nreg;
    PolyWord        p_eax;
    PolyWord        p_ebx;
    PolyWord        p_ecx;
    PolyWord        p_edx;
    PolyWord        p_esi;
    PolyWord        p_edi;
#ifdef HOSTARCHITECTURE_X86_64
    PolyWord        p_r8;
    PolyWord        p_r9;
    PolyWord        p_r10;
    PolyWord        p_r11;
    PolyWord        p_r12;
    PolyWord        p_r13;
    PolyWord        p_r14;
#endif
    POLYUNSIGNED    p_nUnchecked;
    POLYUNSIGNED    p_flags;
    struct fpSaveArea p_fp;
};

/**********************************************************************
 *
 * Register fields in the stack. 
 *
 **********************************************************************/
// Changed from macros to inline functions to improve type safety
inline PolyX86Stack* x86Stack(TaskData *taskData) { return (PolyX86Stack*)taskData->stack->stack(); }

inline PolyWord& PSP_EAX(TaskData *taskData) { return x86Stack(taskData)->p_eax; }
inline PolyWord& PSP_EBX(TaskData *taskData) { return x86Stack(taskData)->p_ebx; }
inline PolyWord& PSP_ECX(TaskData *taskData) { return x86Stack(taskData)->p_ecx; }
inline PolyWord& PSP_EDX(TaskData *taskData) { return x86Stack(taskData)->p_edx; }
inline PolyWord& PSP_ESI(TaskData *taskData) { return x86Stack(taskData)->p_esi; }
inline PolyWord& PSP_EDI(TaskData *taskData) { return x86Stack(taskData)->p_edi; }

#ifdef HOSTARCHITECTURE_X86_64
// X64 registers only
inline PolyWord& PSP_R8(TaskData *taskData) { return x86Stack(taskData)->p_r8; }
inline PolyWord& PSP_R9(TaskData *taskData) { return x86Stack(taskData)->p_r9; }
inline PolyWord& PSP_R10(TaskData *taskData) { return x86Stack(taskData)->p_r10; }
inline PolyWord& PSP_R11(TaskData *taskData) { return x86Stack(taskData)->p_r11; }
inline PolyWord& PSP_R12(TaskData *taskData) { return x86Stack(taskData)->p_r12; }
inline PolyWord& PSP_R13(TaskData *taskData) { return x86Stack(taskData)->p_r13; }
inline PolyWord& PSP_R14(TaskData *taskData) { return x86Stack(taskData)->p_r14; }
#endif

inline POLYUNSIGNED& PSP_EFLAGS(TaskData *taskData) { return x86Stack(taskData)->p_flags; }

#define EFLAGS_CF               0x0001
#define EFLAGS_PF               0x0004
#define EFLAGS_AF               0x0010
#define EFLAGS_ZF               0x0040
#define EFLAGS_SF               0x0080
#define EFLAGS_OF               0x0800

inline POLYCODEPTR& PSP_IC(TaskData *taskData) { return taskData->stack->stack()->p_pc; }
inline void PSP_INCR_PC(TaskData *taskData, int /* May be -ve */n) { taskData->stack->stack()->p_pc += n; }
inline PolyWord*& PSP_SP(TaskData *taskData) { return taskData->stack->stack()->p_sp; }
inline PolyWord*& PSP_HR(TaskData *taskData) { return taskData->stack->stack()->p_hr; }


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

    unsigned X86AsmGetFPControlWord(void);
    byte *X86AsmRestoreHandlerAfterExceptionTraceCode(void);
    byte *X86AsmGiveExceptionTraceCode(void);
    POLYUNSIGNED X86AsmAtomicIncrement(PolyObject*);
    POLYUNSIGNED X86AsmAtomicDecrement(PolyObject*);

#ifdef _MSC_VER
    byte *X86AsmCallPOLY_SYS_exit(void);
    byte *X86AsmCallPOLY_SYS_chdir(void);
    byte *X86AsmCallPOLY_SYS_get_flags(void);
    byte *X86AsmCallPOLY_SYS_exception_trace(void);
    byte *X86AsmCallPOLY_SYS_profiler(void);
    byte *X86AsmCallPOLY_SYS_Real_str(void);
    byte *X86AsmCallPOLY_SYS_Real_Dispatch(void);
    byte *X86AsmCallPOLY_SYS_Repr_real(void);
    byte *X86AsmCallPOLY_SYS_conv_real(void);
    byte *X86AsmCallPOLY_SYS_real_to_int(void);
    byte *X86AsmCallPOLY_SYS_sqrt_real(void);
    byte *X86AsmCallPOLY_SYS_sin_real(void);
    byte *X86AsmCallPOLY_SYS_cos_real(void);
    byte *X86AsmCallPOLY_SYS_arctan_real(void);
    byte *X86AsmCallPOLY_SYS_exp_real(void);
    byte *X86AsmCallPOLY_SYS_ln_real(void);
    byte *X86AsmCallPOLY_SYS_io_operation(void);
    byte *X86AsmCallPOLY_SYS_thread_dispatch(void);
    byte *X86AsmCallPOLY_SYS_kill_self(void);
    byte *X86AsmCallPOLY_SYS_objsize(void);
    byte *X86AsmCallPOLY_SYS_showsize(void);
    byte *X86AsmCallPOLY_SYS_timing_dispatch(void);
    byte *X86AsmCallPOLY_SYS_XWindows(void);
    byte *X86AsmCallPOLY_SYS_full_gc(void);
    byte *X86AsmCallPOLY_SYS_stack_trace(void);
    byte *X86AsmCallPOLY_SYS_foreign_dispatch(void);
    byte *X86AsmCallPOLY_SYS_callcode_tupled(void);
    byte *X86AsmCallPOLY_SYS_process_env(void);
    byte *X86AsmCallPOLY_SYS_shrink_stack(void);
    byte *X86AsmCallPOLY_SYS_code_flags(void);
    byte *X86AsmCallPOLY_SYS_set_code_constant(void);
    byte *X86AsmCallPOLY_SYS_poly_specific(void);
    byte *X86AsmCallPOLY_SYS_io_dispatch(void);
    byte *X86AsmCallPOLY_SYS_network(void);
    byte *X86AsmCallPOLY_SYS_os_specific(void);
    byte *X86AsmCallPOLY_SYS_signal_handler(void);
    byte *X86AsmCallPOLY_SYS_kill_self(void);

    byte *X86AsmCallExtraRETURN_HEAP_OVERFLOW(void);
    byte *X86AsmCallExtraRETURN_STACK_OVERFLOW(void);
    byte *X86AsmCallExtraRETURN_STACK_OVERFLOWEX(void);
    byte *X86AsmCallExtraRETURN_RAISE_DIV(void);
    byte *X86AsmCallExtraRETURN_ARB_EMULATION(void);
    byte *X86AsmCallExtraRETURN_CALLBACK_RETURN(void);
    byte *X86AsmCallExtraRETURN_CALLBACK_EXCEPTION(void);

#endif

    // These are declared in the assembly code.  They provide hand coded versions
    // of simple functions.  Some cases, such as adding words, are actually handled by
    // the code generator, so the assembly code versions would only be called when
    // the function is passed as a closure e.g. map (op+) [(1,2),(3,4)]
    extern int alloc_store(), alloc_uninit();
    extern int get_length_a();
    extern int str_compare();
    extern int teststreq(), teststrneq(), teststrgtr(), teststrlss(), teststrgeq(), teststrleq();
    extern int locksega();
    extern int is_shorta();
    extern int add_long(), sub_long(), mult_long(), div_long(), rem_long(), neg_long();
    extern int equal_long(), or_long(), and_long(), xor_long(), quotrem_long();
    extern int offset_address();
    extern int shift_right_word();
    extern int word_neq();
    extern int not_bool();
    extern int string_length();
    extern int int_geq(), int_leq(), int_gtr(), int_lss();
    extern int or_word(), and_word(), xor_word(), shift_left_word(), shift_right_arith_word();
    extern int word_eq();
    extern int load_byte(), load_word();
    extern int is_big_endian();
    extern int bytes_per_word();
    extern int assign_byte(), assign_word();
    extern int set_string_length_a();
    extern int get_first_long_word_a();
    extern int int_to_word();
    extern int move_bytes(), move_words(), bytevec_eq();
    extern int mul_word(), plus_word(), minus_word(), div_word(), mod_word();
    extern int word_geq(), word_leq(), word_gtr(), word_lss();
    extern int raisex();
    extern int thread_self(), atomic_increment(), atomic_decrement();
    extern int real_add(), real_sub(), real_mul(), real_div(), real_abs(), real_neg();
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
            PSP_IC(taskData) = (*(PSP_SP(taskData))++).AsCodePtr();
            HeapOverflowTrap(taskData); // Computes a value for allocWords only
            break;

        case RETURN_STACK_OVERFLOW:
            try {
                // The stack check has failed.  This may either be because we really have
                // overflowed the stack or because the stack limit value has been adjusted
                // to result in a call here.
                PSP_IC(taskData) = (*PSP_SP(taskData)++).AsCodePtr();
                CheckAndGrowStack(taskData, taskData->stack->stack()->p_sp);
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
                PolyWord *stackP = PSP_EDI(taskData).AsStackAddr();
                PSP_EDI(taskData) = TAGGED(0);
                PSP_IC(taskData) = (*PSP_SP(taskData)++).AsCodePtr();
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
                PSP_IC(taskData) = (*PSP_SP(taskData)++).AsCodePtr();
                // Set all the registers to a safe value here.  We will almost certainly
                // have shifted a value in one of the registers before testing it for zero.
                for (POLYUNSIGNED i = 0; i < taskData->stack->stack()->p_nreg; i++)
                    taskData->stack->stack()->p_reg[i] = TAGGED(0);
                raise_exception0(taskData, EXC_divide);
            }
            catch (IOException) {
                // Handle the C++ exception.
            }
            break;

        case RETURN_ARB_EMULATION:
            try {
                PSP_IC(taskData) = (*PSP_SP(taskData)++).AsCodePtr();
                ArbitraryPrecisionTrap(taskData);
            }
            catch (IOException) {
                // We may get an exception in the trap handler e.g. if we run out of store.
            }
            break;

        case RETURN_CALLBACK_RETURN:
            // Remove the extra exception handler we created in SetCallbackFunction
            ASSERT(taskData->stack->stack()->p_hr == PSP_SP(taskData));
            PSP_SP(taskData) += 2;
            taskData->stack->stack()->p_hr = (*(PSP_SP(taskData)++)).AsStackAddr(); // Restore the previous handler.
            mdTask->callBackResult = taskData->saveVec.push(PSP_EAX(taskData)); // Argument to return is in EAX.
            // Restore the registers
#ifdef HOSTARCHITECTURE_X86_64
            PSP_R10(taskData) = *PSP_SP(taskData)++;
            PSP_R9(taskData) = *PSP_SP(taskData)++;
            PSP_R8(taskData) = *PSP_SP(taskData)++;
#endif
            PSP_EBX(taskData) = *PSP_SP(taskData)++;
            PSP_EAX(taskData) = *PSP_SP(taskData)++;
            PSP_EDX(taskData) = *PSP_SP(taskData)++;
            taskData->stack->stack()->p_pc = (*PSP_SP(taskData)).AsCodePtr(); // Set the return address
            return -2;

        case RETURN_CALLBACK_EXCEPTION:
            // An ML callback has raised an exception.
            SetException(taskData, (poly_exn *)PSP_EAX(taskData).AsObjPtr());
            // Raise a C++ exception.  If the foreign function that called this callback
            // doesn't handle the exception it will be raised in the calling ML function.
            // But if it is caught we may have a problem ...
            throw IOException(EXC_EXCEPTION);

        default:
            Crash("Unknown return reason code %u", mdTask->memRegisters.returnReason);
        }

    } while (1);
}

void X86Dependent::InitStackFrame(TaskData *parentTaskData, StackSpace *space, Handle proc, Handle arg)
/* Initialise stack frame. */
{
    PolyX86Stack * newStack = (PolyX86Stack*)space->stack();
    POLYUNSIGNED stack_size     = space->spaceSize();
    POLYUNSIGNED topStack = stack_size-2;
    newStack->p_space = OVERFLOW_STACK_SIZE;
    newStack->p_pc    = PC_RETRY_SPECIAL;
    newStack->p_sp    = newStack->Offset(topStack); 
    newStack->p_hr    = newStack->Offset(topStack);
    newStack->p_nreg  = CHECKED_REGS;

    /* If this function takes an argument store it in the argument register. */
    if (arg == 0) newStack->p_eax = TAGGED(0);
    else newStack->p_eax = DEREFWORD(arg);
    newStack->p_ebx = TAGGED(0);
    newStack->p_ecx = TAGGED(0);
    newStack->p_edx = DEREFWORDHANDLE(proc); /* rdx - closure pointer */
    newStack->p_esi = TAGGED(0);
    newStack->p_edi = TAGGED(0);
#ifdef HOSTARCHITECTURE_X86_64
    newStack->p_r8 = TAGGED(0);
    newStack->p_r9 = TAGGED(0);
    newStack->p_r10 = TAGGED(0);
    newStack->p_r11 = TAGGED(0);
    newStack->p_r12 = TAGGED(0);
    newStack->p_r13 = TAGGED(0);
    newStack->p_r14 = TAGGED(0);
#endif

    newStack->p_nUnchecked = UNCHECKED_REGS; // 1 unchecked register plus FP area
    newStack->p_flags = 0;

    // Floating point save area.
    ASSERT(sizeof(struct fpSaveArea) == 108);
    memset(&newStack->p_fp, 0, 108);
    newStack->p_fp.cw = 0x037f ; // Control word
    newStack->p_fp.tw = 0xffff; // Tag registers - all unused

    /* We initialise the end of the stack with a sequence that will jump to
       kill_self whether the process ends with a normal return or by raising an
       exception.
       There's one additional complication.  kill_self is called via CallIO0
       which loads the value at *p_sp into p_pc assuming this is a return address.
       We need to make sure that this value is acceptable since this stack may be
       scanned by a subsequent minor GC if it's already been copied by a minor GC. */
    newStack->Set(topStack+1, TAGGED(0)); // Acceptable value if we've exited by exception.
    // Set the default handler and return address to point to this code.

    Handle killCode = BuildKillSelf(parentTaskData);
    PolyWord killJump = killCode->Word();
    // Normal return address and exception handler.
    newStack->Set(topStack, killJump);
}

// IO Functions called indirectly from assembly code.
void X86Dependent::CallIO0(TaskData *taskData, Handle (*ioFun)(TaskData *))
{
    // Set the return address now.
    taskData->stack->stack()->p_pc = (*PSP_SP(taskData)).AsCodePtr();
    try {
        Handle result = (*ioFun)(taskData);
        PSP_EAX(taskData) = result->Word();
        // If this is a normal return we can pop the return address.
        // If this has raised an exception, set for retry or changed process
        // we mustn't.  N,B, The return address could have changed because of GC
        PSP_SP(taskData)++;
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
    taskData->stack->stack()->p_pc = (*PSP_SP(taskData)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData));
    try {
        Handle result = (*ioFun)(taskData, saved1);
        PSP_EAX(taskData) = result->Word();
        PSP_SP(taskData)++; // Pop the return address.
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
    taskData->stack->stack()->p_pc = (*PSP_SP(taskData)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData));
    Handle saved2 = taskData->saveVec.push(PSP_EBX(taskData));
    try {
        Handle result = (*ioFun)(taskData, saved2, saved1);
        PSP_EAX(taskData) = result->Word();
        PSP_SP(taskData)++;
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
    taskData->stack->stack()->p_pc = (*PSP_SP(taskData)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData));
    Handle saved2 = taskData->saveVec.push(PSP_EBX(taskData));
#ifndef HOSTARCHITECTURE_X86_64
    Handle saved3 = taskData->saveVec.push(PSP_SP(taskData)[1]);
#else /* HOSTARCHITECTURE_X86_64 */
    Handle saved3 = taskData->saveVec.push(PSP_R8(taskData));
#endif /* HOSTARCHITECTURE_X86_64 */
    try {
        Handle result = (*ioFun)(taskData, saved3, saved2, saved1);
        PSP_EAX(taskData) = result->Word();
#ifndef HOSTARCHITECTURE_X86_64
        PSP_SP(taskData) += 2; // Pop the return address and a stack arg.
#else /* HOSTARCHITECTURE_X86_64 */
        PSP_SP(taskData)++;
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
    taskData->stack->stack()->p_pc = (*PSP_SP(taskData)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData));
    Handle saved2 = taskData->saveVec.push(PSP_EBX(taskData));
#ifndef HOSTARCHITECTURE_X86_64
    Handle saved3 = taskData->saveVec.push(PSP_SP(taskData)[2]);
    Handle saved4 = taskData->saveVec.push(PSP_SP(taskData)[1]);
#else /* HOSTARCHITECTURE_X86_64 */
    Handle saved3 = taskData->saveVec.push(PSP_R8(taskData));
    Handle saved4 = taskData->saveVec.push(PSP_R9(taskData));
#endif /* HOSTARCHITECTURE_X86_64 */
    try {
        Handle result = (*ioFun)(taskData, saved4, saved3, saved2, saved1);
        PSP_EAX(taskData) = result->Word();
#ifndef HOSTARCHITECTURE_X86_64
        PSP_SP(taskData) += 3; // Pop the return address and two stack args.
#else /* HOSTARCHITECTURE_X86_64 */
        PSP_SP(taskData)++;
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
    taskData->stack->stack()->p_pc = (*PSP_SP(taskData)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData));
    Handle saved2 = taskData->saveVec.push(PSP_EBX(taskData));
#ifndef HOSTARCHITECTURE_X86_64
    Handle saved3 = taskData->saveVec.push(PSP_SP(taskData)[3]);
    Handle saved4 = taskData->saveVec.push(PSP_SP(taskData)[2]);
    Handle saved5 = taskData->saveVec.push(PSP_SP(taskData)[1]);
#else /* HOSTARCHITECTURE_X86_64 */
    Handle saved3 = taskData->saveVec.push(PSP_R8(taskData));
    Handle saved4 = taskData->saveVec.push(PSP_R9(taskData));
    Handle saved5 = taskData->saveVec.push(PSP_R10(taskData));
#endif /* HOSTARCHITECTURE_X86_64 */
    try {
        Handle result = (*ioFun)(taskData, saved5, saved4, saved3, saved2, saved1);
        PSP_EAX(taskData) = result->Word();
#ifndef HOSTARCHITECTURE_X86_64
        PSP_SP(taskData) += 4; // Pop the return address and 3 stack args
#else /* HOSTARCHITECTURE_X86_64 */
        PSP_SP(taskData)++;
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
    codeHandle->WordP()->Set(codeWords++, TAGGED(functionName)); // Name of function 
    codeHandle->WordP()->Set(codeWords++, TAGGED(0)); // Register set
    codeHandle->WordP()->Set(codeWords++, TAGGED(0)); // No profile counter
    codeHandle->WordP()->Set(codeWords++, PolyWord::FromUnsigned(3)); // Number of constants
    CodeSegmentFlags(taskData, taskData->saveVec.push(TAGGED(F_CODE_OBJ)), codeHandle);
    return codeHandle;
}

// Set up a handler that, if it's called, will print an exception trace.
// If the handler isn't called the dummy handler is simply removed.
// This is tricky since when we "return" we actually need to run the new
// function.
void X86Dependent::SetExceptionTrace(TaskData *taskData)
{
    taskData->stack->stack()->p_pc = (*PSP_SP(taskData)).AsCodePtr();
    Handle fun = taskData->saveVec.push(PSP_EAX(taskData));
    Handle extrace = BuildExceptionTrace(taskData);
    PolyObject *functToCall = fun->WordP();
    PSP_EDX(taskData) = functToCall; // Closure address
    // Leave the return address where it is on the stack.
    taskData->stack->stack()->p_pc = functToCall->Get(0).AsCodePtr(); // First word of closure is entry pt.
    *(--PSP_SP(taskData)) = PolyWord::FromStackAddr(taskData->stack->stack()->p_hr);
    // Handler addresses must be word + 2 byte aligned.
    *(--PSP_SP(taskData)) = PolyWord::FromCodePtr(extrace->WordP()->AsBytePtr()+2);
    taskData->stack->stack()->p_hr = PSP_SP(taskData);
    byte *codeAddr = X86AsmRestoreHandlerAfterExceptionTraceCode();
    Handle retCode = BuildCodeSegment(taskData, codeAddr, 8 /* Code is 8 bytes */, 'R');
    *(--PSP_SP(taskData)) = retCode->WordP(); // Code for normal return.
    PSP_EAX(taskData) = TAGGED(0); // Set the argument of the function to "unit".
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
    // Check carefully for valid pointers.  Because this can be called
    // at any time some of these may be invalid.
    if (mdTask == 0) return false;
    if (mdTask->memRegisters.inRTS)
    {
        if (taskData->stack == 0) return false;
        sp = taskData->stack->stack()->p_sp;
        pc = taskData->stack->stack()->p_pc;
        return true;
    }
    if (context == 0) return false;
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
    if (sp >= (PolyWord*)taskData->stack && sp < taskData->stack->stack()->Offset(taskData->stack->spaceSize()))
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
        mdTask->memRegisters.stackLimit = taskData->stack->stack()->Offset(taskData->stack->spaceSize()-1);
    taskData->pendingInterrupt = true;
}

// This is called from SwitchToPoly before we enter the ML code.
void X86Dependent::SetMemRegisters(TaskData *taskData)
{
    X86TaskData *mdTask = (X86TaskData*)taskData->mdTaskData;
    // Copy the current store limits into variables before we go into the assembly code.

    // If we haven't yet set the allocation area or we don't have enough we need
    // to create one (or a new one).
    if (taskData->allocPointer <= taskData->allocLimit + mdTask->allocWords)
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
        // Set the allocation register to this area.
        if (mdTask->allocReg < 15)
            *(get_reg(taskData, mdTask->allocReg)) =
                PolyWord::FromStackAddr(taskData->allocPointer + 1); /* remember: it's off-by-one */
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
    if (profileMode == kProfileStoreAllocation)
        mdTask->memRegisters.localMbottom = mdTask->memRegisters.localMpointer;

    mdTask->memRegisters.polyStack = taskData->stack->stack();
    // Whenever the ML code enters a function it checks that the stack pointer is above
    // this value.  The default is to set it to the top of the reserved area
    // but if we've had an interrupt we set it to the end of the stack.
    // InterruptCode may be called either when the thread is in the RTS or in ML code.
    mdTask->memRegisters.stackTop = taskData->stack->stack()->Offset(taskData->stack->spaceSize() - 1);
    if (taskData->pendingInterrupt)
        mdTask->memRegisters.stackLimit = taskData->stack->stack()->Offset(taskData->stack->spaceSize()-1);
    else mdTask->memRegisters.stackLimit = taskData->stack->stack()->Offset(taskData->stack->stack()->p_space);
    mdTask->memRegisters.handlerRegister = taskData->stack->stack()->p_hr;
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
    if (PSP_IC(taskData) == PC_RETRY_SPECIAL)
        taskData->stack->stack()->p_pc = PSP_EDX(taskData).AsObjPtr()->Get(0).AsCodePtr();

    // Set the rounding mode to the value set within the RTS.
    x86Stack(taskData)->p_fp.cw &= 0x73ff;
    // Get the rounding mode.
    unsigned short controlWord = X86AsmGetFPControlWord();
    x86Stack(taskData)->p_fp.cw &= 0xf3ff;
    x86Stack(taskData)->p_fp.cw |= controlWord & 0xc00;
}

// This is called whenever we have returned from ML to C.
void X86Dependent::SaveMemRegisters(TaskData *taskData)
{
    X86TaskData *mdTask = (X86TaskData*)taskData->mdTaskData;
    // Check a few items on the stack to see it hasn't been overwritten
    if (taskData->stack->stack()->p_space != OVERFLOW_STACK_SIZE ||
          taskData->stack->stack()->p_nreg != CHECKED_REGS ||
          taskData->stack->stack()->p_reg[CHECKED_REGS] != PolyWord::FromUnsigned(UNCHECKED_REGS))
        Crash("Stack overwritten\n");
    taskData->allocPointer = mdTask->memRegisters.localMpointer - 1;
    taskData->stack->stack()->p_hr = mdTask->memRegisters.handlerRegister;
    mdTask->allocWords = 0;
}

// Called if we need the ML code to retry an RTS call.
void X86Dependent::SetForRetry(TaskData *taskData, int ioCall)
{
    /* We now have to set the closure entry for the RTS call to work.
       DCJM 4/1/01. */
    PSP_EDX(taskData) = (PolyObject*)IoEntry(ioCall);
    taskData->stack->stack()->p_pc = PC_RETRY_SPECIAL; // This value is treated specially in SetMemRegisters
}

PolyWord *X86Dependent::get_reg(TaskData *taskData, int n)
/* Returns a pointer to the register given by n. */
{
    PolyX86Stack *stack = x86Stack(taskData);
    switch (n) 
    {
    case 0: return &stack->p_eax;
    case 1: return &stack->p_ecx;
    case 2: return &stack->p_edx;
    case 3: return &stack->p_ebx;
    case 4: return (PolyWord*)&stack->p_sp;
    case 6: return &stack->p_esi;
    case 7: return &stack->p_edi;
#ifdef HOSTARCHITECTURE_X86_64
    case 8: return &stack->p_r8;
    case 9: return &stack->p_r9;
    case 10: return &stack->p_r10;
    case 11: return &stack->p_r11;
    case 12: return &stack->p_r12;
    case 13: return &stack->p_r13;
    case 14: return &stack->p_r14;
    // R15 is the heap pointer so shouldn't occur here.
#endif /* HOSTARCHITECTURE_X86_64 */
    default: Crash("Unknown register %d at %p\n", n, stack->p_pc);
    }
}

PolyWord *X86Dependent::getArgument(TaskData *taskData, unsigned int modRm,
                                    unsigned int rexPrefix, bool *inConsts)
{
    unsigned int md = modRm >> 6;
    unsigned int rm = modRm & 7;
    if (inConsts) *inConsts = false; // Default
    if (md == 3) // Register
        return get_reg(taskData, rm + (rexPrefix & 0x1)*8);
    else if (rm == 4)
    {
        // s-i-b present.  Used for esp and r12 as well as indexing.
        unsigned int sib = PSP_IC(taskData)[0];
        unsigned int index = (sib >> 3) & 7;
        unsigned int ss = (sib >> 6) & 3;
        unsigned int base = sib & 7;
        PSP_INCR_PC(taskData, 1);
        if (md == 0 && base == 5)
            // This should not occur in either 32 or 64-bit mode.
            Crash("Immediate address in emulated instruction");
        else
        {
            int offset = 0;
            if (md == 1)
            {
                // One byte offset
                offset = PSP_IC(taskData)[0];
                if (offset >= 128) offset -= 256;
                PSP_INCR_PC(taskData, 1);
            }
            else if (md == 2)
            {
                // Four byte offset
                offset = PSP_IC(taskData)[3];
                if (offset >= 128) offset -= 256;
                offset = offset*256 + PSP_IC(taskData)[2];
                offset = offset*256 + PSP_IC(taskData)[1];
                offset = offset*256 + PSP_IC(taskData)[0];
                PSP_INCR_PC(taskData, 4);
            }
            if (ss != 0 || index != 4) Crash("Index register present");
            byte *ea;
            if (rexPrefix & 0x1) base += 8;
            if (base == 4) /* esp */ ea = (byte*)taskData->stack->stack()->p_sp + offset;
            else ea = get_reg(taskData, base)->AsCodePtr()+offset;
            return (PolyWord*)ea;
        }
    }
    else if (md == 0 && rm == 5)
    {
#ifdef HOSTARCHITECTURE_X86_64
        // In 64-bit mode this means PC-relative
        int offset = PSP_IC(taskData)[3];
        if (offset >= 128) offset -= 256;
        offset = offset*256 + PSP_IC(taskData)[2];
        offset = offset*256 + PSP_IC(taskData)[1];
        offset = offset*256 + PSP_IC(taskData)[0];
        PSP_INCR_PC(taskData, 4);
        if (inConsts) *inConsts = true;
        return (PolyWord*)(taskData->stack->stack()->p_pc + offset);
#else
        Crash("Immediate address in emulated instruction");
#endif
    }
    else
    {
        int offset = 0;
        if (md == 1)
        {
            // One byte offset
            offset = PSP_IC(taskData)[0];
            if (offset >= 128) offset -= 256;
            PSP_INCR_PC(taskData, 1);
        }
        else if (md == 2)
        {
            // Four byte offset
            offset = PSP_IC(taskData)[3];
            if (offset >= 128) offset -= 256;
            offset = offset*256 + PSP_IC(taskData)[2];
            offset = offset*256 + PSP_IC(taskData)[1];
            offset = offset*256 + PSP_IC(taskData)[0];
            PSP_INCR_PC(taskData, 4);
        }
        PolyWord base = *(get_reg(taskData, rm + (rexPrefix & 0x1)*8));
        byte *ea = base.AsCodePtr() + offset;
        return (PolyWord*)ea;
    }
}

// Called as a result of a heap overflow trap
void X86Dependent::HeapOverflowTrap(TaskData *taskData)
{
    X86TaskData *mdTask = (X86TaskData*)taskData->mdTaskData;
    PolyX86Stack *stack = x86Stack(taskData);
    POLYUNSIGNED wordsNeeded = 0;
    // The next instruction, after any branches round forwarding pointers, will
    // be a store of register containing the adjusted heap pointer.  We need to
    // find that register and the value in it in order to find out how big the
    // area we actually wanted is.
    while (stack->p_pc[0] == 0xeb)
    {
        if (stack->p_pc[1] >= 128) stack->p_pc += 256 - stack->p_pc[1] + 2;
        else stack->p_pc += stack->p_pc[1] + 2;
    }
#ifndef HOSTARCHITECTURE_X86_64
    // This should be movl REG,0[%ebp].
    ASSERT(stack->p_pc[0] == 0x89);
    mdTask->allocReg = (stack->p_pc[1] >> 3) & 7; // Remember this until we allocate the memory
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
    if (stack->p_pc[1] == 0x89)
    {
        // New (5.4) format.  This should be movq REG,%r15
        ASSERT(stack->p_pc[0] == 0x49 || stack->p_pc[0] == 0x4d);
        mdTask->allocReg = (stack->p_pc[2] >> 3) & 7; // Remember this until we allocate the memory
        if (stack->p_pc[0] & 0x4) mdTask->allocReg += 8;
        PolyWord *reg = get_reg(taskData, mdTask->allocReg);
        PolyWord reg_val = *reg;
        wordsNeeded = (taskData->allocPointer - (PolyWord*)reg_val.AsAddress()) + 1;
        *reg = TAGGED(0); // Clear this - it's not a valid address.
    }
    else
    {
        // Old (pre-5.4) format.
        // This should be movq Length,-8(%r15)
        ASSERT(stack->p_pc[0] == 0x49 && stack->p_pc[1] == 0xc7 && stack->p_pc[2] == 0x47 && stack->p_pc[3] == 0xf8);
        // The Length field should be in the next word.  N.B.  This assumes that
        // the length word < 2^31.
        ASSERT((stack->p_pc[7] & 0x80) == 0); // Should not be negative
        for (unsigned i = 7; i >= 4; i--) wordsNeeded = (wordsNeeded << 8) | stack->p_pc[i];
        wordsNeeded += 1; // That was the object size. We need to add one for the length word.
        mdTask->allocReg = 15; // Don't put it in a register
        // The value that ends up in allocSpace->pointer includes the
        // attempted allocation.  Add back the space we tried to allocate
        taskData->allocPointer += wordsNeeded;
    }
#endif /* HOSTARCHITECTURE_X86_64 */
    if (profileMode == kProfileStoreAllocation)
        add_count(taskData, stack->p_pc, stack->p_sp, wordsNeeded);

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
    POLYUNSIGNED flags = PSP_EFLAGS(taskData);
    flags &= -256;
    if (r == 0) flags |= EFLAGS_ZF;
    else if (r < 0) flags |= EFLAGS_SF;
    PSP_EFLAGS(taskData) = flags;
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
// The instruction formats have changed in 5.4 so this supports
// both 5.3 and earlier and also 5.4 format.
bool X86Dependent::emulate_instrs(TaskData *taskData)
{
    int src1 = -1, src2 = -1, dest = -1;
    bool doneSubtraction = false;
    POLYUNSIGNED flagsWord = PSP_EFLAGS(taskData);
    PSP_EFLAGS(taskData) &= ~EFLAGS_OF; // Make sure the overflow flag is clear.
    while(1) {
        byte rexPrefix = 0;
#ifdef HOSTARCHITECTURE_X86_64
        // Get any REX prefix
        if (PSP_IC(taskData)[0] >= 0x40 && PSP_IC(taskData)[0] <= 0x4f)
        {
            rexPrefix = PSP_IC(taskData)[0];
            PSP_INCR_PC(taskData, 1);
        }
#endif /* HOSTARCHITECTURE_X86_64 */
        // Decode the register fields and include any REX bits
        int bbb = PSP_IC(taskData)[1] & 7;
        if (rexPrefix & 0x1) bbb += 8;
        int rrr = (PSP_IC(taskData)[1] >> 3) & 7;
        if (rexPrefix & 0x4) rrr += 8;

        switch (PSP_IC(taskData)[0]) {
        case 0x03: 
            {
                /* add. */
                PSP_INCR_PC(taskData, 1);
                int modRm = PSP_IC(taskData)[0];
                PSP_INCR_PC(taskData, 1);
                bool inConsts = false;
                PolyWord arg2 = *(getArgument(taskData, modRm, rexPrefix, &inConsts));
                if (dest == -1) { // New format
                    PolyWord *destReg = get_reg(taskData, rrr);
                    PolyWord arg1 = *destReg;
                    // We could have come here because of testing the tags, which happens
                    // before the operation, or as a result of adding two tagged values in which
                    // case arg1 will contain the result after the addition.
                    if (flagsWord & EFLAGS_OF) {
                        if (rrr == bbb) { // Same register
                            POLYUNSIGNED arg = arg1.AsUnsigned()/2;
                            // If the carry flag was set the value was originally negative.
                            if (flagsWord & EFLAGS_CF)
                                arg |= (POLYUNSIGNED)1 << (sizeof(POLYUNSIGNED)*8-1);
                            arg1 = arg2 = PolyWord::FromUnsigned(arg);
                        }
                        else arg1 = PolyWord::FromUnsigned(arg1.AsUnsigned() - arg2.AsUnsigned());
                    }
                    // If this is in the 64-bit non-address area it is a constant with the
                    // tag removed.  Add it back in.
                    if (inConsts) arg2 = PolyWord::FromUnsigned(arg2.AsUnsigned()+1);
                    do_op(taskData, rrr, arg1, arg2, add_longc);
                    // The next operation will subtract the tag.  We need to add in a dummy tag..
                    // This may cause problems with CheckRegion which assumes that every register
                    // contains a valid value.
                    if (! inConsts) {
                        destReg = get_reg(taskData, rrr); // May have moved because of a GC.
                        *destReg = PolyWord::FromUnsigned(destReg->AsUnsigned()+1);
                    }
                }
                else { // Legacy format
                    if (dest != rrr)
                        Crash("Expected same destination register.");
                    do_op(taskData, dest, *(get_reg(taskData, src1)), arg2, add_longc);
                }
                return true;
            }

        case 0x2b: /* Subtraction. */
            {
                PSP_INCR_PC(taskData, 1);
                int modRm = PSP_IC(taskData)[0];
                PSP_INCR_PC(taskData, 1);
                bool inConsts = false;
                PolyWord arg2 = *(getArgument(taskData, modRm, rexPrefix, &inConsts));
                if (dest == -1) { // New format
                    PolyWord *destReg = get_reg(taskData, rrr);
                    PolyWord arg1 = *destReg;
                    // We could have come here because of testing the tags, which happens
                    // before the operation, or as a result of subtracting two tagged values in which
                    // case arg1 will contain the result after the subtraction.
                    if (flagsWord & EFLAGS_OF) {
                        arg1 = PolyWord::FromUnsigned(arg1.AsUnsigned() + arg2.AsUnsigned());
                    }
                    // If this is in the 64-bit non-address area it is a constant with the
                    // tag added.  Subtract it now.
                    if (inConsts) arg2 = PolyWord::FromUnsigned(arg2.AsUnsigned()-1);
                    do_op(taskData, rrr, arg1, arg2, sub_longc);
                    // The next operation will add the tag.  We need to subtract a dummy tag..
                    // This may cause problems with CheckRegion which assumes that every register
                    // contains a valid value.
                    if (! inConsts) {
                        destReg = get_reg(taskData, rrr); // May have moved because of a GC.
                        *destReg = PolyWord::FromUnsigned(destReg->AsUnsigned()-1);
                    }
                    return true;
                }
                else { // Legacy format
                    if (dest != rrr)
                        Crash("Expected same destination register.");
                    do_op(taskData, dest, *(get_reg(taskData, src1)), arg2, sub_longc);
                    doneSubtraction = true;
                    break;
                }
            }

        case 0x3b: /* Compare. */
            {
                PSP_INCR_PC(taskData, 1);
                int modRm = PSP_IC(taskData)[0];
                PSP_INCR_PC(taskData, 1);
                PolyWord arg = *(getArgument(taskData, modRm, rexPrefix));
                do_compare(taskData, *(get_reg(taskData, rrr)), arg);
                return true;
            }

        case 0x8d: /* leal - Used to remove a tag before an add and multiply. */
            // Also used to put the tag on after a subtraction.
            if ((PSP_IC(taskData)[1] & 7) == 4)
            { // R12 (and RSP but that isn't used here) have to be encoded with a SIB byte.
                ASSERT((PSP_IC(taskData)[2] & 7) == 4); // Should be same register
                PSP_INCR_PC(taskData, 1);
            }
            if (doneSubtraction)
            {
                PSP_INCR_PC(taskData, 3);
                return true;
            }
            if (src1 == -1) src1 = bbb; else src2 = bbb;
            dest = rrr;
            ASSERT(PSP_IC(taskData)[2] == 0xff);
            PSP_INCR_PC(taskData, 3);
            break;

        case 0x89: /* movl: move source into dest. */
            if ((PSP_IC(taskData)[1] & 0xc0) != 0xc0)
                 Crash("Can't move into store.");
            dest = bbb;
            if (src1 == -1) src1 = rrr; else src2 = rrr;
            PSP_INCR_PC(taskData, 2);
                /* Next should be add-immediate. */
            break;

        case 0x83: { /* One byte immediate: Add, sub or compare. */
            PSP_INCR_PC(taskData, 1);
            int modRm = PSP_IC(taskData)[0];
            PSP_INCR_PC(taskData, 1);
            PolyWord arg = *(getArgument(taskData, modRm, rexPrefix));

            int cval = PSP_IC(taskData)[0];
            if (cval >= 128) cval -= 256;
            PSP_INCR_PC(taskData, 1);

            switch (modRm & (7 << 3)) // This is a code.  Ignore any REX override.
            {
                case (0 << 3): /* add */
                {
                    if (dest != bbb) { // New format: Same register for source and destination.
                        // We didn't have a move instruction before this.
                        // We may come here either because we had an overflow or because we found
                        // that the argument was long.  If it was oveflow we will have already
                        // added the value so must substract before we redo the operation
                        // as proper long precision.
                        if (arg.IsTagged()) {
                            arg = PolyWord::FromUnsigned(arg.AsUnsigned() - cval);
                        }
                        // Immediate value is shifted, but hasn't had 1 added;
                        // do this now before calling add_longc
                        do_op(taskData, bbb, arg, PolyWord::FromSigned(cval+1), add_longc);
                    }
                    else do_op(taskData, dest, *(get_reg(taskData, src1)), PolyWord::FromSigned(cval+1), add_longc);
                    break;
                }

                case (5 << 3): /* sub */
                {
                    if (dest != bbb) { // New format: Same register for source and destination.
                        // We didn't have a move instruction before this.
                        PolyWord arg = *(get_reg(taskData, bbb));
                        if (arg.IsTagged()) arg = PolyWord::FromUnsigned(arg.AsUnsigned() + cval);
                        // Immediate value is shifted, but hasn't had 1 added;
                        // do this now before calling sub_longc
                        do_op(taskData, bbb, arg, PolyWord::FromSigned(cval+1), sub_longc);
                    }
                    else do_op(taskData, dest, *(get_reg(taskData, src1)), PolyWord::FromSigned(cval+1), sub_longc);
                    break;
                }

                case (7 << 3): /* cmp */
                {
                    /* immediate value is already tagged */
                    do_compare(taskData, arg, PolyWord::FromSigned(cval));
                    break;
                }

                default: Crash("Unknown instruction after overflow trap");
            }
            return true;
            }

        case 0x81: { /* 4 byte immediate: Add, sub or compare. */
            PSP_INCR_PC(taskData, 1);
            int modRm = PSP_IC(taskData)[0];
            PSP_INCR_PC(taskData, 1);
            PolyWord arg = *(getArgument(taskData, modRm, rexPrefix));

            int cval = PSP_IC(taskData)[3];
            if (cval >= 128) cval -= 256;
            cval = cval*256 + PSP_IC(taskData)[2];
            cval = cval*256 + PSP_IC(taskData)[1];
            cval = cval*256 + PSP_IC(taskData)[0];
            PSP_INCR_PC(taskData, 4);

            switch (modRm & (7 << 3))
            {
                case (0 << 3): /* add */
                {
                    if (dest != bbb) { // New format: Same register for source and destination.
                        if (arg.IsTagged()) {
                            arg = PolyWord::FromUnsigned(arg.AsUnsigned() - cval);
                        }
                        do_op(taskData, bbb, arg, PolyWord::FromSigned(cval+1), add_longc);
                    }
                    else do_op(taskData, dest, *(get_reg(taskData, src1)), PolyWord::FromSigned(cval+1), add_longc);
                    break;
                }

                case (5 << 3): /* sub */
                {
                    if (dest != bbb) { // New format: Same register for source and destination.
                        // We didn't have a move instruction before this.
                        if (arg.IsTagged()) arg = PolyWord::FromUnsigned(arg.AsUnsigned() + cval);
                        do_op(taskData, bbb, arg, PolyWord::FromSigned(cval+1), sub_longc);
                    }
                    else do_op(taskData, dest, *(get_reg(taskData, src1)), PolyWord::FromSigned(cval+1), sub_longc);
                    break;
                }

                case (7 << 3): /* cmp */
                {
                    // Immediate value is already tagged or may be an address.
                    do_compare(taskData, arg, PolyWord::FromSigned(cval));
                    break;
                }

                default: Crash("Unknown instruction after overflow trap");
            }
            return true;
            }

        case 0xeb: // jmp - used in branch forwarding.
            // This is used to skip back to the instruction being emulated.
            if (PSP_IC(taskData)[1] >= 128)
                PSP_INCR_PC(taskData, PSP_IC(taskData)[1] - 256 + 2);
            else PSP_INCR_PC(taskData, PSP_IC(taskData)[1] + 2);
            break;

        case 0x50: /* push eax - used before a multiply. */
#ifdef HOSTARCHITECTURE_X86_64
            ASSERT((rexPrefix & 1) == 0); // Check it's not r8
#endif /* HOSTARCHITECTURE_X86_64 */
            *(--PSP_SP(taskData)) = PSP_EAX(taskData);
            PSP_INCR_PC(taskData, 1);
            break;

        case 0x52: /* push edx - used before a multiply. */
#ifdef HOSTARCHITECTURE_X86_64
            ASSERT((rexPrefix & 1) == 0); // Check it's not r10
#endif /* HOSTARCHITECTURE_X86_64 */
            *(--PSP_SP(taskData)) = PSP_EDX(taskData);
            PSP_INCR_PC(taskData, 1);
            break;

        case 0xd1: /* Group1A - must be sar edx before a multiply or sar [esp] before Real.fromInt */
            if (PSP_IC(taskData)[1] == 0xfa) {
                PSP_INCR_PC(taskData, 2);
                /* If we haven't moved anything into edx then edx must be
                   one of the arguments. */
                if (src2 == -1) src2 = 2; /* edx. */
            }
            else if (PSP_IC(taskData)[1] == 0x3c) {
                PSP_INCR_PC(taskData, 3);
            }
            else Crash("Unknown instruction after overflow trap");
            break;

        case 0xf7: /* Multiply instruction. */
            if (PSP_IC(taskData)[1] != 0xea)
                Crash("Unknown instruction after overflow trap");
            do_op(taskData, 0 /* eax */, *(get_reg(taskData, src1)), *(get_reg(taskData, src2)), mult_longc);
            /* Subtract one because the next instruction will tag it. */
            PSP_EAX(taskData) = PolyWord::FromUnsigned(PSP_EAX(taskData).AsUnsigned() - 1);
            PSP_INCR_PC(taskData, 2);
            return true;

        case 0xdb: // Floating point ESCAPE 3
        case 0xdf:
            {
                PolyX86Stack *stack = x86Stack(taskData);
#ifdef HOSTARCHITECTURE_X86_64
                if (stack->p_pc[1] != 0x2c || stack->p_pc[2] != 0x24)
                    Crash("Unknown instruction after overflow trap");
#else
                if (stack->p_pc[1] != 0x04 || stack->p_pc[2] != 0x24)
                    Crash("Unknown instruction after overflow trap");
#endif /* HOSTARCHITECTURE_X86_64 */
                // The operand is on the stack.
                union { double dble; byte bytes[sizeof(double)]; } dValue;
                dValue.dble = get_C_real(taskData, stack->p_sp[0]);
                unsigned top = (stack->p_fp.sw >> 11) & 7;
                top = (top-1) & 0x7;
                stack->p_fp.sw = (stack->p_fp.sw & (~0x3800)) | (top << 11);
                stack->p_fp.tw &= ~(3 << top*2); // Needed?
                // Turn the double precision value into extended precision.  Because
                // the double precision has less precision than the extended it will
                // always fit.  The result is always put into the first register which is
                // the top of the stack.
                memset(stack->p_fp.registers[0], 0, 10);
                if (dValue.dble != 0.0) { // Check for zero although that's short so shouldn't occur.
                    // Since we've converted an integer the exp is always +ve
                    // This works correctly for infinity which can occur with large
                    // arbitrary precision numbers e.g. IntInf.pow(10, 309)
                    int exp = ((dValue.bytes[7] & 0x7f) << 4) | (dValue.bytes[6] >> 4);
                    if (exp != 0) exp = exp - 1023+16383;
                    stack->p_fp.registers[0][9] = (exp >> 8) & 0xff;
                    stack->p_fp.registers[0][8] = exp & 0xff;
                    if (dValue.dble < 0) stack->p_fp.registers[0][9] |= 0x80; // Set the sign bit
                    // Mantissa is shifted down by one bit and the top bit is set.
                    unsigned acc = dValue.bytes[6] | (0x80 >> 3);
                    for (int i = 5; i >= 0; i--) {
                        acc = (acc << 8) | dValue.bytes[i];
                        stack->p_fp.registers[0][i+2] = acc >> 5;
                    }
                    stack->p_fp.registers[0][1] = acc << 3;
                }
                PSP_INCR_PC(taskData, 3);
            }
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
        add_count(taskData, PSP_IC(taskData), PSP_SP(taskData), 1);
    // Emulate the arbitrary precision instruction.
    if (! emulate_instrs(taskData))
        Crash("Arbitrary precision emulation fault at %x\n", PSP_IC(taskData));
}

// These macros build small pieces of assembly code for each io call.
// The code simply sets the requestCode value and jumps to
// X86AsmSaveStateAndReturn.  The address of these code pieces is
// stored in iovec.  Values in iovec are never looked at with the
// garbage collector so that's safe.

// N.B.  The length of this code (7) is built into BuildKillSelf
// It's 7 bytes on both x86 and X86_64.
#define MAKE_CALL_SEQUENCE_BYTES     7

#ifdef _MSC_VER
// Windows.  VC does not support inline assembly on X86-64 so we put these in the assembly code.
#define MAKE_IO_CALL_SEQUENCE(ioNum, result) result = X86AsmCall##ioNum##()
#define MAKE_EXTRA_CALL_SEQUENCE(exNum, result) result = X86AsmCallExtra##exNum##()

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
    add_function_to_io_area(POLY_SYS_alloc_uninit, &alloc_uninit);

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

    add_function_to_io_area(POLY_SYS_quotrem, &quotrem_long);
    add_function_to_io_area(POLY_SYS_is_short, &is_shorta);
    add_function_to_io_area(POLY_SYS_aplus, &add_long);
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
    add_function_to_io_area(POLY_SYS_Abs_real, real_abs);
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
    add_function_to_io_area(POLY_SYS_int_eq, &word_eq);
    add_function_to_io_area(POLY_SYS_int_neq, &word_neq);
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

    add_function_to_io_area(POLY_SYS_bytevec_eq,       &bytevec_eq);
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

// Similarly for the exception trace code.  This is more complicated.
// For backwards compatibility we need the address to be on a word + 2 byte
// boundary.
Handle X86Dependent::BuildExceptionTrace(TaskData *taskData)
{
    byte *codeAddr = X86AsmGiveExceptionTraceCode();
    return BuildCodeSegment(taskData, codeAddr, 20, 'E');
}

void X86Dependent::SetException(TaskData *taskData, poly_exn *exc)
// Set up the stack of a process to raise an exception.
{
    PSP_EDX(taskData) = (PolyObject*)IoEntry(POLY_SYS_raisex);
    PSP_IC(taskData)     = PC_RETRY_SPECIAL;
    PSP_EAX(taskData) = exc; /* put exception data into eax */
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
    PolyObject *argTuple = PSP_EAX(taskData).AsObjPtr();
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
                CheckAndGrowStack(taskData, taskData->stack->stack()->p_sp - (argCount - ARGS_IN_REGS));
            }
            catch (IOException)
            {
                return; // Will have been set up to raise an exception.
            }
        }

        // First argument is in EAX
        PSP_EAX(taskData) = argv->Get(0);
        // Second arg, if there is one, goes into EBX
        if (argCount > 1)
            PSP_EBX(taskData) = argv->Get(1);
#ifdef HOSTARCHITECTURE_X86_64
        if (argCount > 2)
            PSP_R8(taskData) = argv->Get(2);
        if (argCount > 3)
            PSP_R9(taskData) = argv->Get(3);
        if (argCount > 4)
            PSP_R10(taskData) = argv->Get(4);
#endif /* HOSTARCHITECTURE_X86_64 */
        // Remaining args go on the stack.
        PolyWord returnAddress = *PSP_SP(taskData)++;
        for (POLYUNSIGNED i = ARGS_IN_REGS; i < argCount; i++)
        {
            *(--PSP_SP(taskData)) = argv->Get(i);
        }
        *(--PSP_SP(taskData)) = returnAddress;
    }
    // The closure goes into the closure reg.
    PSP_EDX(taskData) = DEREFWORD(closure);
    // First word of closure is entry point.
    PSP_IC(taskData) = (PSP_EDX(taskData)).AsObjPtr()->Get(0).AsCodePtr();
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
    *(--PSP_SP(taskData)) = PSP_EDX(taskData);
    *(--PSP_SP(taskData)) = PSP_EAX(taskData);
    *(--PSP_SP(taskData)) = PSP_EBX(taskData);
#ifdef HOSTARCHITECTURE_X86_64
    *(--PSP_SP(taskData)) = PSP_R8(taskData);
    *(--PSP_SP(taskData)) = PSP_R9(taskData);
    *(--PSP_SP(taskData)) = PSP_R10(taskData);
#endif
    // Set up an exception handler so we will enter callBackException if there is an exception.
    *(--PSP_SP(taskData)) = PolyWord::FromStackAddr(taskData->stack->stack()->p_hr); // Create a special handler entry
    *(--PSP_SP(taskData)) = callBackException->Word();
    *(--PSP_SP(taskData)) = TAGGED(0);
    taskData->stack->stack()->p_hr = PSP_SP(taskData);
    // Push the call to callBackReturn onto the stack as the return address.
    *(--PSP_SP(taskData)) = callBackReturn->Word();
    // Set up the entry point of the callback.
    PolyObject *functToCall = func->WordP();
    PSP_EDX(taskData) = functToCall; // Closure address
    PSP_EAX(taskData) = args->Word();
    taskData->stack->stack()->p_pc = functToCall->Get(0).AsCodePtr(); // First word of closure is entry pt.
}

// Decode and process an effective address.  There may
// be a constant address in here but in any case we need
// to decode it to work out where the next instruction starts.
// If this is an lea instruction any addresses are just constants
// so must not be treated as addresses.
static void skipea(byte **pt, ScanAddress *process, bool lea)
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
                    process->ScanConstant(*pt, PROCESS_RELOC_DIRECT);
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
            process->ScanConstant(*pt, PROCESS_RELOC_DIRECT);
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

    while (true)
    {
        // We've finished when this is word aligned and points to a zero word.
        if (((POLYUNSIGNED)pt & (0-sizeof(POLYUNSIGNED))) && ((PolyWord*)pt)->AsUnsigned() == 0)
            break;
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
        case 0xf0: /* lock. */ case 0xf3: /* rep/repe */
        case 0xa4: case 0xa5: case 0xaa: case 0xab: /* movs/stos */
        case 0xa6: /* cmpsb */ case 0x9e: /* sahf */
            pt++; break;

        case 0x70: case 0x71: case 0x72: case 0x73: case 0x74: case 0x75: case 0x76:
        case 0x77: case 0x7c: case 0x7d: case 0x7e: case 0x7f: case 0xeb:
            /* short jumps. */
        case 0xcd: /* INT */
        case 0xa8: /* TEST_ACC8 */
        case 0x6a: /* PUSH_8 */
            pt += 2; break;

        case 0xc2: /* RET_16 */
            pt += 3; break;

        case 0x8d: /* leal. */
            pt++; skipea(&pt, process, true); break;

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
            pt++; skipea(&pt, process, false); break;

        case 0xf6: /* Group3_a */
            {
                int isTest = 0;
                pt++;
                /* The test instruction has an immediate operand. */
                if ((*pt & 0x38) == 0) isTest = 1;
                skipea(&pt, process, false);
                if (isTest) pt++;
                break;
            }

        case 0xf7: /* Group3_A */
            {
                int isTest = 0;
                pt++;
                /* The test instruction has an immediate operand. */
                if ((*pt & 0x38) == 0) isTest = 1;
                skipea(&pt, process, false);
                if (isTest) pt += 4;
                break;
            }

        case 0xc1: /* Group2_8_A */
        case 0xc6: /* MOVB_8_A */
        case 0x83: /* Group1_8_A */
        case 0x80: /* Group1_8_a */
            pt++; skipea(&pt, process, false); pt++; break;

        case 0x81: /* Group1_32_A */
            {
                pt ++;
#ifndef HOSTARCHITECTURE_X86_64
                unsigned opCode = *pt;
#endif /* HOSTARCHITECTURE_X86_64 */
                skipea(&pt, process, false);
                // Only check the 32 bit constant if this is a comparison.
                // For other operations this may be untagged and shouldn't be an address.
#ifndef HOSTARCHITECTURE_X86_64
                if ((opCode & 0x38) == 0x38)
                    process->ScanConstant(pt, PROCESS_RELOC_DIRECT);
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
                    skipea(&pt, process, false);
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
                case 0xc1: /* xaddl */
                    pt++; skipea(&pt, process, false); break;

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

        case 0xd8: case 0xd9: case 0xda: case 0xdb:
        case 0xdc: case 0xdd: case 0xde: case 0xdf: // Floating point escape instructions
            {
                pt++;
                if ((*pt & 0xe0) == 0xe0) pt++;
                else skipea(&pt, process, false);
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

// Increment the value contained in the first word of the mutex.
Handle X86Dependent::AtomicIncrement(TaskData *taskData, Handle mutexp)
{
    PolyObject *p = DEREFHANDLE(mutexp);
    POLYUNSIGNED result = X86AsmAtomicIncrement(p);
    return taskData->saveVec.push(PolyWord::FromUnsigned(result));
}

// Decrement the value contained in the first word of the mutex.
Handle X86Dependent::AtomicDecrement(TaskData *taskData, Handle mutexp)
{
    PolyObject *p = DEREFHANDLE(mutexp);
    POLYUNSIGNED result = X86AsmAtomicDecrement(p);
    return taskData->saveVec.push(PolyWord::FromUnsigned(result));
}

static X86Dependent x86Dependent;

MachineDependent *machineDependent = &x86Dependent;
