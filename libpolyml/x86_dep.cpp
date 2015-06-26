/*
    Title:  Machine dependent code for i386 and X64 under Windows and Unix

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

    Further work copyright David C. J. Matthews 2011-14

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
#include "reals.h"
#include "polystring.h"
#include "xwindows.h"
#include "objsize.h"
#include "foreign.h"
#include "process_env.h"
#include "basicio.h"
#include "network.h"
#include "os_specific.h"
#include "poly_specific.h"
#include "timing.h"

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

// Layout of base of stack on X86.
class StackObject {
public:
    POLYUNSIGNED    p_unused1;
    POLYCODEPTR     p_pc;
    PolyWord        *p_sp;
    PolyWord        *p_unused2;
    POLYUNSIGNED    p_unused3;
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
    PolyWord    *localMbottom;      // Base of memory + 1 word
    PolyWord    *stackLimit;        // Lower limit of stack
    PolyWord    *unusedNow;         // Previously: Upper limit of stack
    // These offsets are built into the assembly code section
    byte        requestCode;        // IO function to call.
        // The offset (20/40) of requestCode is built into the MAKE_IO_CALL_SEQUENCE macro
    byte        inRTS;              // Flag indicating we're not in ML
    byte        returnReason;       // Reason for returning from ML.
    byte        fullRestore;        // 0 => clear registers, 1 => reload registers
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

class X86TaskData: public TaskData {
public:
    X86TaskData();
    unsigned allocReg; // The register to take the allocated space.
    POLYUNSIGNED allocWords; // The words to allocate.
    Handle callBackResult;
    MemRegisters memRegisters;

    virtual void GCStack(ScanAddress *process);
    virtual Handle EnterPolyCode(); // Start running ML
    virtual void InterruptCode();
    virtual bool GetPCandSPFromContext(SIGNALCONTEXT *context, PolyWord *&sp, POLYCODEPTR &pc);
    virtual void InitStackFrame(TaskData *parentTask, Handle proc, Handle arg);
    virtual void SetException(poly_exn *exc);
    virtual Handle CallBackResult() { return callBackResult; } 
    virtual int  GetIOFunctionRegisterMask(int ioCall);

    // Increment or decrement the first word of the object pointed to by the
    // mutex argument and return the new value.
    virtual Handle AtomicIncrement(Handle mutexp);
    virtual Handle AtomicDecrement(Handle mutexp);
    // Set a mutex to one.
    virtual void AtomicReset(Handle mutexp);

    // These are retained for the moment.
    virtual POLYCODEPTR pc(void) const { return stack->stack()->p_pc; }
    virtual PolyWord *sp(void) const { return stack->stack()->p_sp; }
    virtual PolyWord *hr(void) const { return memRegisters.handlerRegister; }
    virtual void set_hr(PolyWord *hr) { memRegisters.handlerRegister = hr; }
    // Return the minimum space occupied by the stack if we are considering shrinking it.
    virtual POLYUNSIGNED currentStackSpace(void) const { return (this->stack->top - this->stack->stack()->p_sp) + OVERFLOW_STACK_SIZE; }

    virtual void CopyStackFrame(StackObject *old_stack, POLYUNSIGNED old_length, StackObject *new_stack, POLYUNSIGNED new_length);

    void SetExceptionTrace(void);
    void CallCodeTupled();
    virtual void SetCallbackFunction(Handle func, Handle args);

    int SwitchToPoly();

    void HeapOverflowTrap();

    void SetMemRegisters();
    void SaveMemRegisters();

    void ArbitraryPrecisionTrap();
    PolyWord *get_reg(int n);
    PolyWord *getArgument(unsigned int opByte, unsigned int rexPrefix, bool *inConsts=0);
    void do_compare(PolyWord v1, PolyWord v2);
    void do_op(int dest, PolyWord v1, PolyWord v2, Handle (*op)(TaskData *, Handle, Handle));
    bool emulate_instrs();
    Handle BuildCodeSegment(const byte *code, unsigned bytes, char functionName);
    Handle BuildKillSelf();
    Handle BuildExceptionTrace();
};

class X86Dependent: public MachineDependent {
public:
    X86Dependent() {}

    // Create a task data object.
    virtual TaskData *CreateTaskData(void) { return new X86TaskData(); }

    virtual unsigned InitialStackSize(void) { return 128+OVERFLOW_STACK_SIZE; } // Initial size of a stack 
    virtual void InitInterfaceVector(void);
    virtual void ScanConstantsWithinCode(PolyObject *addr, PolyObject *oldAddr, POLYUNSIGNED length, ScanAddress *process);

    virtual Architectures MachineArchitecture(void)
#ifndef HOSTARCHITECTURE_X86_64
         { return MA_I386; }
#else /* HOSTARCHITECTURE_X86_64 */
         { return MA_X86_64; }
#endif /* HOSTARCHITECTURE_X86_64 */
};


// Entry code sequences - copied to memRegisters before entering ML.
static byte *heapOverflow, *stackOverflow, *stackOverflowEx, *raiseDiv, *arbEmulation;

/**********************************************************************
 *
 * Register fields in the stack. 
 *
 **********************************************************************/
// Changed from macros to inline functions to improve type safety
inline StackObject* x86Stack(TaskData *taskData) { return taskData->stack->stack(); }

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

inline POLYCODEPTR& PSP_IC(TaskData *taskData) { return x86Stack(taskData)->p_pc; }
inline void PSP_INCR_PC(TaskData *taskData, int /* May be -ve */n) { x86Stack(taskData)->p_pc += n; }
inline PolyWord*& PSP_SP(TaskData *taskData) { return x86Stack(taskData)->p_sp; }
inline PolyWord*& PSP_HR(X86TaskData *taskData) { return taskData->memRegisters.handlerRegister; }


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

    extern int X86AsmRestoreHandlerAfterExceptionTraceTemplate(void);
    extern int X86AsmGiveExceptionTraceFnTemplate(void);
    extern int X86AsmKillSelfTemplate(void);
    extern int X86AsmCallbackReturnTemplate(void);
    extern int X86AsmCallbackExceptionTemplate(void);

    POLYUNSIGNED X86AsmAtomicIncrement(PolyObject*);
    POLYUNSIGNED X86AsmAtomicDecrement(PolyObject*);

    extern int X86AsmCallPOLY_SYS_exit(void);
    extern int X86AsmCallPOLY_SYS_chdir(void);
    extern int X86AsmCallPOLY_SYS_get_flags(void);
    extern int X86AsmCallPOLY_SYS_exception_trace(void);
    extern int X86AsmCallPOLY_SYS_exception_trace_fn(void);
    extern int X86AsmCallPOLY_SYS_profiler(void);
    extern int X86AsmCallPOLY_SYS_Real_str(void);
    extern int X86AsmCallPOLY_SYS_Real_Dispatch(void);
    extern int X86AsmCallPOLY_SYS_Repr_real(void);
    extern int X86AsmCallPOLY_SYS_conv_real(void);
    extern int X86AsmCallPOLY_SYS_real_to_int(void);
    extern int X86AsmCallPOLY_SYS_sqrt_real(void);
    extern int X86AsmCallPOLY_SYS_sin_real(void);
    extern int X86AsmCallPOLY_SYS_cos_real(void);
    extern int X86AsmCallPOLY_SYS_arctan_real(void);
    extern int X86AsmCallPOLY_SYS_exp_real(void);
    extern int X86AsmCallPOLY_SYS_ln_real(void);
    extern int X86AsmCallPOLY_SYS_io_operation(void);
    extern int X86AsmCallPOLY_SYS_thread_dispatch(void);
    extern int X86AsmCallPOLY_SYS_kill_self(void);
    extern int X86AsmCallPOLY_SYS_objsize(void);
    extern int X86AsmCallPOLY_SYS_showsize(void);
    extern int X86AsmCallPOLY_SYS_timing_dispatch(void);
    extern int X86AsmCallPOLY_SYS_XWindows(void);
    extern int X86AsmCallPOLY_SYS_full_gc(void);
    extern int X86AsmCallPOLY_SYS_stack_trace(void);
    extern int X86AsmCallPOLY_SYS_foreign_dispatch(void);
    extern int X86AsmCallPOLY_SYS_callcode_tupled(void);
    extern int X86AsmCallPOLY_SYS_process_env(void);
    extern int X86AsmCallPOLY_SYS_shrink_stack(void);
    extern int X86AsmCallPOLY_SYS_code_flags(void);
    extern int X86AsmCallPOLY_SYS_set_code_constant(void);
    extern int X86AsmCallPOLY_SYS_poly_specific(void);
    extern int X86AsmCallPOLY_SYS_io_dispatch(void);
    extern int X86AsmCallPOLY_SYS_network(void);
    extern int X86AsmCallPOLY_SYS_os_specific(void);
    extern int X86AsmCallPOLY_SYS_signal_handler(void);
    extern int X86AsmCallPOLY_SYS_kill_self(void);

    extern int X86AsmCallExtraRETURN_HEAP_OVERFLOW(void);
    extern int X86AsmCallExtraRETURN_STACK_OVERFLOW(void);
    extern int X86AsmCallExtraRETURN_STACK_OVERFLOWEX(void);
    extern int X86AsmCallExtraRETURN_RAISE_DIV(void);
    extern int X86AsmCallExtraRETURN_ARB_EMULATION(void);
    extern int X86AsmCallExtraRETURN_CALLBACK_RETURN(void);
    extern int X86AsmCallExtraRETURN_CALLBACK_EXCEPTION(void);

    // These are declared in the assembly code.  They provide hand coded versions
    // of simple functions.  Some cases, such as adding words, are actually handled by
    // the code generator, so the assembly code versions would only be called when
    // the function is passed as a closure e.g. map (op+) [(1,2),(3,4)]
    extern int alloc_store(), alloc_uninit();
    extern int get_length_a();
    extern int str_compare();
    extern int teststrgtr(), teststrlss(), teststrgeq(), teststrleq();
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
    extern int thread_self(), atomic_increment(), atomic_decrement(), atomic_reset();
    extern int real_add(), real_sub(), real_mul(), real_div(), real_abs(), real_neg();
    extern int real_geq(), real_leq(), real_gtr(), real_lss(), real_eq(), real_neq();
    extern int real_from_int();
    extern int eq_longword(), neq_longword(), geq_longword(), leq_longword();
    extern int gt_longword(), lt_longword();
    extern int longword_to_tagged(), signed_to_longword(), unsigned_to_longword();
    extern int plus_longword(), minus_longword(), mul_longword(), div_longword();
    extern int mod_longword(), andb_longword(), orb_longword(), xorb_longword();
    extern int shift_left_longword(), shift_right_longword(), shift_right_arith_longword();
};

X86TaskData::X86TaskData(): allocReg(0), allocWords(0)
{
    memRegisters.inRTS = 1; // We start off in the RTS.
    // Point "raiseException" at the assembly code for "raisex"
    memRegisters.raiseException = (byte*)raisex;
    // Entry point to save the state for an IO call.  This is the common entry
    // point for all the return and IO-call cases.
    memRegisters.ioEntry = (byte*)X86AsmSaveStateAndReturn;
    memRegisters.heapOverflow = heapOverflow;
    memRegisters.stackOverflow = stackOverflow;
    memRegisters.stackOverflowEx = stackOverflowEx;
    memRegisters.raiseDiv = raiseDiv;
    memRegisters.arbEmulation = arbEmulation;
    memRegisters.fullRestore = 0;
}

void X86TaskData::GCStack(ScanAddress *process)
{
    if (stack != 0)
    {
        StackSpace *stackSpace = stack;
        StackObject *stack = (StackObject*)(stackSpace->stack());
        PolyWord *stackPtr = stack->p_sp; // Save this BEFORE we update
        PolyWord *stackEnd = stackSpace->top;

        // Either this is TAGGED(0) indicating a retry or it's a code pointer.
        if (stack->p_pc != TAGGED(0).AsCodePtr())
            stack->p_pc = process->ScanStackAddress (PolyWord::FromCodePtr(stack->p_pc), stackSpace, true).AsCodePtr();

        stack->p_eax = process->ScanStackAddress(stack->p_eax, stackSpace, false);
        stack->p_edx = process->ScanStackAddress(stack->p_edx, stackSpace, false);

        // Process the registers if they have been saved otherwise clear them
        if (this->memRegisters.fullRestore)
        {
            stack->p_ebx = process->ScanStackAddress(stack->p_ebx, stackSpace, false);
            stack->p_ecx = process->ScanStackAddress(stack->p_ecx, stackSpace, false);
            stack->p_esi = process->ScanStackAddress(stack->p_esi, stackSpace, false);
            stack->p_edi = process->ScanStackAddress(stack->p_edi, stackSpace, false);
#ifdef HOSTARCHITECTURE_X86_64
            stack->p_r8 = process->ScanStackAddress(stack->p_r8, stackSpace, false);
            stack->p_r9 = process->ScanStackAddress(stack->p_r9, stackSpace, false);
            stack->p_r10 = process->ScanStackAddress(stack->p_r10, stackSpace, false);
            stack->p_r11 = process->ScanStackAddress(stack->p_r11, stackSpace, false);
            stack->p_r12 = process->ScanStackAddress(stack->p_r12, stackSpace, false);
            stack->p_r13 = process->ScanStackAddress(stack->p_r13, stackSpace, false);
            stack->p_r14 = process->ScanStackAddress(stack->p_r14, stackSpace, false);
#endif
        }
        else
        {
            stack->p_ebx = TAGGED(0);
            stack->p_ecx = TAGGED(0);
            stack->p_esi = TAGGED(0);
            stack->p_edi = TAGGED(0);
#ifdef HOSTARCHITECTURE_X86_64
            stack->p_r8 = TAGGED(0);
            stack->p_r9 = TAGGED(0);
            stack->p_r10 = TAGGED(0);
            stack->p_r11 = TAGGED(0);
            stack->p_r12 = TAGGED(0);
            stack->p_r13 = TAGGED(0);
            stack->p_r14 = TAGGED(0);
#endif
        }

        // Now the values on the stack.
        for (PolyWord *q = stackPtr; q < stackEnd; q++)
            *q = process->ScanStackAddress(*q, stackSpace, false);
     }
}

// Copy a stack
void X86TaskData::CopyStackFrame(StackObject *old_stack, POLYUNSIGNED old_length, StackObject *new_stack, POLYUNSIGNED new_length)
{
    /* Moves a stack, updating all references within the stack */
    PolyWord *old_base  = (PolyWord *)old_stack;
    PolyWord *new_base  = (PolyWord*)new_stack;
    PolyWord *old_top   = old_base + old_length;

    /* Calculate the offset of the new stack from the old. If the frame is
       being extended objects in the new frame will be further up the stack
       than in the old one. */

    POLYSIGNED offset = new_base - old_base + new_length - old_length;

    /* Copy the registers, changing any that point into the stack. */

    new_stack->p_pc    = old_stack->p_pc;
    new_stack->p_sp    = old_stack->p_sp + offset;
    memRegisters.handlerRegister    = memRegisters.handlerRegister + offset;

    POLYUNSIGNED i;
    for (i = 0; i < CHECKED_REGS; i++)
    {
        PolyWord *orr = (&old_stack->p_eax)+i;
        PolyWord *nrr = (&new_stack->p_eax)+i;
        PolyWord R = *orr;

        /* if the register points into the old stack, make the new copy
           point at the same relative offset within the new stack,
           otherwise make the new copy identical to the old version. */

        if (R.IsTagged() || R.AsStackAddr() < old_base || R.AsStackAddr() >= old_top)
            *nrr = R;
        else *nrr = PolyWord::FromStackAddr(R.AsStackAddr() + offset);
    }

    /* Copy unchecked registers. - The next "register" is the number of
       unchecked registers to copy. Unchecked registers are used for 
       values that might look like addresses, i.e. don't have tag bits, 
       but are not. */
    new_stack->p_nUnchecked = old_stack->p_nUnchecked;
    new_stack->p_flags = old_stack->p_flags;
    new_stack->p_fp = old_stack->p_fp;

    /* Skip the unused part of the stack. */

    i = (PolyWord*)old_stack->p_sp - old_base;

    ASSERT (i <= old_length);

    i = old_length - i;

    PolyWord *old = old_stack->p_sp;
    PolyWord *newp= new_stack->p_sp;

    while (i--)
    {
//        ASSERT(old >= old_base && old < old_base+old_length);
//        ASSERT(newp >= new_base && newp < new_base+new_length);
        PolyWord old_word = *old++;
        if (old_word.IsTagged() || old_word.AsStackAddr() < old_base || old_word.AsStackAddr() >= old_top)
            *newp++ = old_word;
        else
            *newp++ = PolyWord::FromStackAddr(old_word.AsStackAddr() + offset);
    }
    ASSERT(old == ((PolyWord*)old_stack)+old_length);
    ASSERT(newp == ((PolyWord*)new_stack)+new_length);
}


// Store a constant in the code segment.  This has to be handled specially because
// the constant is probably an address.
// At the moment this assumes we're dealing with a 32-bit constant on a 32-bit machine
// and a 64-bit constant on a 64-bit machine.
static Handle set_code_constant(TaskData *taskData, Handle data, Handle constant, Handle offseth, Handle base)
{
    POLYUNSIGNED offset = getPolyUnsigned(taskData, DEREFWORD(offseth)); // Byte offset
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
    return taskData->saveVec.push(TAGGED(0));
}

// IO Functions called indirectly from assembly code.
static void CallIO0(X86TaskData *taskData, Handle (*ioFun)(TaskData *))
{
    // Set the return address now.
    PSP_IC(taskData) = (*PSP_SP(taskData)).AsCodePtr();
    Handle result = (*ioFun)(taskData);
    PSP_EAX(taskData) = result->Word();
    // If this is a normal return we can pop the return address.
    // If this has raised an exception, set for retry or changed process
    // we mustn't.  N,B, The return address could have changed because of GC
    PSP_SP(taskData)++;
}

static void CallIO1(X86TaskData *taskData, Handle (*ioFun)(TaskData *, Handle))
{
    PSP_IC(taskData)= (*PSP_SP(taskData)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData));
    Handle result = (*ioFun)(taskData, saved1);
    PSP_EAX(taskData) = result->Word();
    PSP_SP(taskData)++; // Pop the return address.
}

static void CallIO2(X86TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle))
{
    PSP_IC(taskData) = (*PSP_SP(taskData)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData));
    Handle saved2 = taskData->saveVec.push(PSP_EBX(taskData));
    Handle result = (*ioFun)(taskData, saved2, saved1);
    PSP_EAX(taskData) = result->Word();
    PSP_SP(taskData)++;
}

static void CallIO3(X86TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle, Handle))
{
    PSP_IC(taskData) = (*PSP_SP(taskData)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData));
    Handle saved2 = taskData->saveVec.push(PSP_EBX(taskData));
#ifndef HOSTARCHITECTURE_X86_64
    Handle saved3 = taskData->saveVec.push(PSP_SP(taskData)[1]);
#else /* HOSTARCHITECTURE_X86_64 */
    Handle saved3 = taskData->saveVec.push(PSP_R8(taskData));
#endif /* HOSTARCHITECTURE_X86_64 */
    Handle result = (*ioFun)(taskData, saved3, saved2, saved1);
    PSP_EAX(taskData) = result->Word();
#ifndef HOSTARCHITECTURE_X86_64
    PSP_SP(taskData) += 2; // Pop the return address and a stack arg.
#else /* HOSTARCHITECTURE_X86_64 */
    PSP_SP(taskData)++;
#endif /* HOSTARCHITECTURE_X86_64 */
}

static void CallIO4(X86TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle, Handle, Handle))
{
    PSP_IC(taskData) = (*PSP_SP(taskData)).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(PSP_EAX(taskData));
    Handle saved2 = taskData->saveVec.push(PSP_EBX(taskData));
#ifndef HOSTARCHITECTURE_X86_64
    Handle saved3 = taskData->saveVec.push(PSP_SP(taskData)[2]);
    Handle saved4 = taskData->saveVec.push(PSP_SP(taskData)[1]);
#else /* HOSTARCHITECTURE_X86_64 */
    Handle saved3 = taskData->saveVec.push(PSP_R8(taskData));
    Handle saved4 = taskData->saveVec.push(PSP_R9(taskData));
#endif /* HOSTARCHITECTURE_X86_64 */
    Handle result = (*ioFun)(taskData, saved4, saved3, saved2, saved1);
    PSP_EAX(taskData) = result->Word();
#ifndef HOSTARCHITECTURE_X86_64
    PSP_SP(taskData) += 3; // Pop the return address and two stack args.
#else /* HOSTARCHITECTURE_X86_64 */
    PSP_SP(taskData)++;
#endif /* HOSTARCHITECTURE_X86_64 */
}

// The only functions with 5 args are move_bytes/word_long
static void CallIO5(X86TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle, Handle, Handle, Handle))
{
    PSP_IC(taskData) = (*PSP_SP(taskData)).AsCodePtr();
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
    Handle result = (*ioFun)(taskData, saved5, saved4, saved3, saved2, saved1);
    PSP_EAX(taskData) = result->Word();
#ifndef HOSTARCHITECTURE_X86_64
    PSP_SP(taskData) += 4; // Pop the return address and 3 stack args
#else /* HOSTARCHITECTURE_X86_64 */
    PSP_SP(taskData)++;
#endif /* HOSTARCHITECTURE_X86_64 */
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

        if ((debugOptions & DEBUG_RTSCALLS))
            IncrementRTSCallCount(ioFunction);

        try {
            switch (ioFunction)
            {
            case -1:
                // We've been interrupted.  This usually involves simulating a
                // stack overflow so we could come here because of a genuine
                // stack overflow.
                // Previously this code was executed on every RTS call but there
                // were problems on Mac OS X at least with contention on schedLock.
                this->pendingInterrupt = false; // Clear this before we handle these
                // Process any asynchronous events i.e. interrupts or kill
                processes->ProcessAsynchRequests(this);
                // Release and re-acquire use of the ML memory to allow another thread
                // to GC.
                processes->ThreadReleaseMLMemory(this);
                processes->ThreadUseMLMemory(this);
                break;

            case -2: // A callback has returned.
                return CallBackResult();

            case POLY_SYS_exit:
                CallIO1(this, &finishc);
                break;

            case POLY_SYS_alloc_store:
                CallIO3(this, &alloc_store_long_c);
                break;

            case POLY_SYS_alloc_uninit:
                CallIO2(this, &alloc_uninit_c);
                break;

            case POLY_SYS_chdir:
                CallIO1(this, &change_dirc);
                break;

            case POLY_SYS_get_length:
                CallIO1(this, &vec_length_c);
                break;

            case POLY_SYS_get_flags:
                CallIO1(this, &get_flags_c);
                break;

            case POLY_SYS_str_compare:
                CallIO2(this, compareStrings);
                break;

            case POLY_SYS_teststrgtr:
                CallIO2(this, &testStringGreater);
                break;

            case POLY_SYS_teststrlss:
                CallIO2(this, &testStringLess);
                break;

            case POLY_SYS_teststrgeq:
                CallIO2(this, &testStringGreaterOrEqual);
                break;

            case POLY_SYS_teststrleq:
                CallIO2(this, &testStringLessOrEqual);
                break;

            case POLY_SYS_exception_trace_fn: // Special case.
                SetExceptionTrace();
                break;

    //        case POLY_SYS_lockseg: CallIO1(taskData, &locksegc); break;

            case POLY_SYS_profiler:
                CallIO1(this, &profilerc);
                break;

            case POLY_SYS_quotrem:
                CallIO3(this, &quot_rem_c);
                break;

    //        case POLY_SYS_is_short: CallIO1(this, &is_shortc); break;

            case POLY_SYS_aplus:
                CallIO2(this, &add_longc);
                break;

            case POLY_SYS_aminus:
                CallIO2(this, &sub_longc);
                break;

            case POLY_SYS_amul:
                CallIO2(this, &mult_longc);
                break;

            case POLY_SYS_adiv:
                CallIO2(this, &div_longc);
                break;

            case POLY_SYS_amod:
                CallIO2(this, &rem_longc);
                break;

            case POLY_SYS_aneg:
                CallIO1(this, &neg_longc);
                break;

            case POLY_SYS_equala:
                CallIO2(this, &equal_longc);
                break;

            case POLY_SYS_ora:
                CallIO2(this, &or_longc);
                break;

            case POLY_SYS_anda:
                CallIO2(this, &and_longc);
                break;

            case POLY_SYS_xora:
                CallIO2(this, &xor_longc);
                break;

            case POLY_SYS_Real_str:
                CallIO3(this, &Real_strc);
                break;

            case POLY_SYS_Real_geq:
                CallIO2(this, &Real_geqc);
                break;

            case POLY_SYS_Real_leq:
                CallIO2(this, &Real_leqc);
                break;

            case POLY_SYS_Real_gtr:
                CallIO2(this, &Real_gtrc);
                break;

            case POLY_SYS_Real_lss:
                CallIO2(this, &Real_lssc);
                break;

            case POLY_SYS_Real_eq:
                CallIO2(this, &Real_eqc);
                break;

            case POLY_SYS_Real_neq:
                CallIO2(this, &Real_neqc);
                break;

            case POLY_SYS_Real_Dispatch:
                CallIO2(this, &Real_dispatchc);
                break;

            case POLY_SYS_Add_real:
                CallIO2(this, &Real_addc);
                break;

            case POLY_SYS_Sub_real:
                CallIO2(this, &Real_subc);
                break;

            case POLY_SYS_Mul_real:
                CallIO2(this, &Real_mulc);
                break;

            case POLY_SYS_Div_real:
                CallIO2(this, &Real_divc);
                break;

            case POLY_SYS_Abs_real:
                CallIO1(this, &Real_absc);
                break;

            case POLY_SYS_Neg_real:
                CallIO1(this, &Real_negc);
                break;

            case POLY_SYS_Repr_real:
                CallIO1(this, &Real_reprc);
                break;

            case POLY_SYS_conv_real:
                CallIO1(this, &Real_convc);
                break;

            case POLY_SYS_real_to_int:
                CallIO1(this, &Real_intc);
                break;

            case POLY_SYS_int_to_real:
                CallIO1(this, &Real_floatc);
                break;

            case POLY_SYS_sqrt_real:
                CallIO1(this, &Real_sqrtc);
                break;

            case POLY_SYS_sin_real:
                CallIO1(this, &Real_sinc);
                break;

            case POLY_SYS_cos_real:
                CallIO1(this, &Real_cosc);
                break;

            case POLY_SYS_arctan_real:
                CallIO1(this, &Real_arctanc);
                break;

            case POLY_SYS_exp_real:
                CallIO1(this, &Real_expc);
                break;

            case POLY_SYS_ln_real:
                CallIO1(this, &Real_lnc);
                break;

            case POLY_SYS_io_operation:
                CallIO1(this, &io_operation_c);
                break;

            case POLY_SYS_atomic_reset:
                CallIO1(this, &ProcessAtomicReset);
                break;

            case POLY_SYS_atomic_incr:
                CallIO1(this, &ProcessAtomicIncrement);
                break;

            case POLY_SYS_atomic_decr:
                CallIO1(this, &ProcessAtomicDecrement);
                break;

            case POLY_SYS_thread_self:
                CallIO0(this, &ThreadSelf);
                break;

            case POLY_SYS_thread_dispatch:
                CallIO2(this, &ThreadDispatch);
                break;

//            case POLY_SYS_offset_address: CallIO2(this, &offset_addressc); break;

            case POLY_SYS_shift_right_word:
                CallIO2(this, &shift_right_word_c);
                break;
    
            case POLY_SYS_word_neq:
                CallIO2(this, &word_neq_c);
                break;
    
            case POLY_SYS_not_bool:
                CallIO1(this, &not_bool_c);
                break;

            case POLY_SYS_string_length:
                CallIO1(this, &string_length_c);
                break;

            case POLY_SYS_int_eq:
                CallIO2(this, &equal_longc);
                break;

            case POLY_SYS_int_neq:
                CallIO2(this, &not_equal_longc);
                break;

            case POLY_SYS_int_geq:
                CallIO2(this, &ge_longc);
                break;

            case POLY_SYS_int_leq:
                CallIO2(this, &le_longc);
                break;

            case POLY_SYS_int_gtr:
                CallIO2(this, &gt_longc);
                break;

            case POLY_SYS_int_lss:
                CallIO2(this, &ls_longc);
                break;

            case POLY_SYS_or_word:
                CallIO2(this, &or_word_c);
                break;

            case POLY_SYS_and_word:
                CallIO2(this, &and_word_c);
                break;

            case POLY_SYS_xor_word:
                CallIO2(this, &xor_word_c);
                break;

            case POLY_SYS_shift_left_word:
                CallIO2(this, &shift_left_word_c);
                break;

            case POLY_SYS_word_eq:
                CallIO2(this, &word_eq_c);
                break;

            case POLY_SYS_load_byte:
            case POLY_SYS_load_byte_immut:
                CallIO2(this, &load_byte_long_c);
                break;

            case POLY_SYS_load_word:
            case POLY_SYS_load_word_immut:
                CallIO2(this, &load_word_long_c);
                break;

    //        case POLY_SYS_is_big_endian: CallIO0(this, &is_big_endianc); break;
    //        case POLY_SYS_bytes_per_word: CallIO0(this, &bytes_per_wordc); break;

            case POLY_SYS_assign_byte:
                CallIO3(this, &assign_byte_long_c);
                break;

            case POLY_SYS_assign_word:
                CallIO3(this, &assign_word_long_c);
                break;

            // ObjSize and ShowSize are now in the poly_specific functions and
            // probably should be removed from here.
            case POLY_SYS_objsize:
                CallIO1(this, &ObjSize);
                break;

            case POLY_SYS_showsize:
                CallIO1(this, &ShowSize);
                break;

            case POLY_SYS_timing_dispatch:
                CallIO2(this, &timing_dispatch_c);
                break;

            case POLY_SYS_XWindows:
                CallIO1(this, &XWindows_c);
                break;

            case POLY_SYS_full_gc:
                CallIO0(this, &full_gc_c);
                break;

            case POLY_SYS_stack_trace:
                CallIO0(this, & stack_trace_c);
                break;

            case POLY_SYS_foreign_dispatch:
                CallIO2(this, &foreign_dispatch_c);
                break;

            case POLY_SYS_callcode_tupled:
                CallCodeTupled();
                break;

            case POLY_SYS_process_env: CallIO2(this, &process_env_dispatch_c); break;

    //        case POLY_SYS_set_string_length: CallIO2(this, &set_string_length_c); break;

            case POLY_SYS_shrink_stack:
                CallIO1(this, &shrink_stack_c);
                break;

            case POLY_SYS_code_flags:
                CallIO2(this, &CodeSegmentFlags);
                break;

            case POLY_SYS_shift_right_arith_word:
                CallIO2(this, &shift_right_arith_word_c);
                break;

            case POLY_SYS_get_first_long_word:
            case POLY_SYS_int_to_word:
                // POLY_SYS_int_to_word has generally been replaced by POLY_SYS_get_first_long_word.
                // The reason is that POLY_SYS_int_to_word may be applied to either a long or
                // a short argument whereas POLY_SYS_get_first_long_word must be applied to a
                // long argument and can be implemented very easily in the code-generator, at
                // least on a little-endian machine.
                CallIO1(this, &int_to_word_c);
                break;

            case POLY_SYS_poly_specific:
                CallIO2(this, &poly_dispatch_c);
                break;

            case POLY_SYS_bytevec_eq:
                CallIO5(this, &testBytesEqual);
                break;

            case POLY_SYS_set_code_constant:
                CallIO4(this, &set_code_constant);
                break;

            case POLY_SYS_move_bytes:
            case POLY_SYS_move_bytes_overlap:
                CallIO5(this, &move_bytes_long_c);
                break;

            case POLY_SYS_move_words:
            case POLY_SYS_move_words_overlap:
                CallIO5(this, &move_words_long_c);
                break;

            case POLY_SYS_mul_word:
                CallIO2(this, &mul_word_c);
                break;

            case POLY_SYS_plus_word:
                CallIO2(this, &plus_word_c);
                break;

            case POLY_SYS_minus_word:
                CallIO2(this, &minus_word_c);
                break;

            case POLY_SYS_div_word:
                CallIO2(this, &div_word_c);
                break;

            case POLY_SYS_mod_word:
                CallIO2(this, &mod_word_c);
                break;

            case POLY_SYS_word_geq:
                CallIO2(this, &word_geq_c);
                break;

            case POLY_SYS_word_leq:
                CallIO2(this, &word_leq_c);
                break;

            case POLY_SYS_word_gtr:
                CallIO2(this, &word_gtr_c);
                break;

            case POLY_SYS_word_lss:
                CallIO2(this, &word_lss_c);
                break;

            case POLY_SYS_io_dispatch:
                CallIO3(this, &IO_dispatch_c);
                break;

            case POLY_SYS_network:
                CallIO2(this, &Net_dispatch_c);
                break;

            case POLY_SYS_os_specific:
                CallIO2(this, &OS_spec_dispatch_c);
                break;

            case POLY_SYS_signal_handler:
                CallIO2(this, &Sig_dispatch_c);
                break;

            case POLY_SYS_kill_self:
                CallIO0(this, exitThread);
                break;

            case POLY_SYS_eq_longword:
                CallIO2(this, &eqLongWord);
                break;

            case POLY_SYS_neq_longword:
                CallIO2(this, &neqLongWord);
                break;

            case POLY_SYS_geq_longword:
                CallIO2(this, &geqLongWord);
                break;

            case POLY_SYS_leq_longword:
                CallIO2(this, &leqLongWord);
                break;

            case POLY_SYS_gt_longword:
                CallIO2(this, &gtLongWord);
                break;

            case POLY_SYS_lt_longword:
                CallIO2(this, &ltLongWord);
                break;

            case POLY_SYS_plus_longword:
                CallIO2(this, &plusLongWord);
                break;

            case POLY_SYS_minus_longword:
                CallIO2(this, &minusLongWord);
                break;

            case POLY_SYS_mul_longword:
                CallIO2(this, &mulLongWord);
                break;

            case POLY_SYS_div_longword:
                CallIO2(this, &divLongWord);
                break;

            case POLY_SYS_mod_longword:
                CallIO2(this, &modLongWord);
                break;

            case POLY_SYS_andb_longword:
                CallIO2(this, &andbLongWord);
                break;

            case POLY_SYS_orb_longword:
                CallIO2(this, &orbLongWord);
                break;

            case POLY_SYS_xorb_longword:
                CallIO2(this, &xorbLongWord);
                break;

            case POLY_SYS_shift_left_longword:
                CallIO2(this, &shiftLeftLongWord);
                break;

            case POLY_SYS_shift_right_longword:
                CallIO2(this, &shiftRightLongWord);
                break;

            case POLY_SYS_shift_right_arith_longword:
                CallIO2(this, &shiftRightArithLongWord);
                break;

            case POLY_SYS_longword_to_tagged:
                CallIO1(this, &longWordToTagged);
                break;

            case POLY_SYS_signed_to_longword:
                CallIO1(this, &signedToLongWord);
                break;

            case POLY_SYS_unsigned_to_longword:
                CallIO1(this, &unsignedToLongWord);
                break;

            // This is called from assembly code and doesn't actually have an entry in the
            // io vector.
            case POLY_SYS_give_ex_trace_fn:
                CallIO1(this, exceptionToTraceException);
                break;

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
// returns to C with the appropriate values set in memRegisters.requestCode and
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

        X86AsmSwitchToPoly(&this->memRegisters);

        SaveMemRegisters(); // Update globals from the memory registers.

        // Handle any heap/stack overflows or arbitrary precision traps.
        switch (this->memRegisters.returnReason)
        {

        case RETURN_IO_CALL:
            return this->memRegisters.requestCode;

        case RETURN_HEAP_OVERFLOW:
            // The heap has overflowed.  Pop the return address into the program counter.
            // It may well not be a valid code address anyway.
            PSP_IC(this) = (*(PSP_SP(this))++).AsCodePtr();
            this->HeapOverflowTrap(); // Computes a value for allocWords only
            break;

        case RETURN_STACK_OVERFLOW:
            try {
                // The stack check has failed.  This may either be because we really have
                // overflowed the stack or because the stack limit value has been adjusted
                // to result in a call here.
                PSP_IC(this) = (*PSP_SP(this)++).AsCodePtr();
                POLYUNSIGNED min_size = this->stack->top - PSP_SP(this) + OVERFLOW_STACK_SIZE;
                CheckAndGrowStack(this, min_size);
            }
            catch (IOException &) {
               // We may get an exception while handling this if we run out of store
            }
            return -1; // We're in a safe state to handle any interrupts.

        case RETURN_STACK_OVERFLOWEX:
            try {
                // Stack limit overflow.  If the required stack space is larger than
                // the fixed overflow size the code will calculate the limit in %EDI.
                // We need to extract that and the clear that register since it may
                // very well be outside the stack and therefore a "bad address".
                PolyWord *stackP = PSP_EDI(this).AsStackAddr();
                PSP_EDI(this) = TAGGED(0);
                PSP_IC(this) = (*PSP_SP(this)++).AsCodePtr();
                POLYUNSIGNED min_size = this->stack->top - stackP + OVERFLOW_STACK_SIZE;
                CheckAndGrowStack(this, min_size);
            }
            catch (IOException &) {
               // We may get an exception while handling this if we run out of store
            }
            return -1; // We're in a safe state to handle any interrupts.

        case RETURN_RAISE_DIV:
            try {
                // Generally arithmetic operations don't raise exceptions.  Overflow
                // is either ignored, for Word operations, or results in a call to
                // the arbitrary precision emulation code.  This is the exception
                // (no pun intended).
                PSP_IC(this) = (*PSP_SP(this)++).AsCodePtr();
                // Set all the registers to a safe value here.  We will almost certainly
                // have shifted a value in one of the registers before testing it for zero.
                for (POLYUNSIGNED i = 0; i < CHECKED_REGS; i++)
                {
                    PolyWord *pr = (&this->stack->stack()->p_eax)+i;
                    *pr = TAGGED(0);
                }
                raise_exception0(this, EXC_divide);
            }
            catch (IOException &) {
                // Handle the C++ exception.
            }
            break;

        case RETURN_ARB_EMULATION:
            try {
                PSP_IC(this) = (*PSP_SP(this)++).AsCodePtr();
                this->ArbitraryPrecisionTrap();
            }
            catch (IOException &) {
                // We may get an exception in the trap handler e.g. if we run out of store.
            }
            break;

        case RETURN_CALLBACK_RETURN:
            // Remove the extra exception handler we created in SetCallbackFunction
            ASSERT(PSP_HR(this) == PSP_SP(this));
            PSP_SP(this) += 2;
            PSP_HR(this) = (*(PSP_SP(this)++)).AsStackAddr(); // Restore the previous handler.
            this->callBackResult = this->saveVec.push(PSP_EAX(this)); // Argument to return is in EAX.
            // Restore the registers
#ifdef HOSTARCHITECTURE_X86_64
            PSP_R10(this) = *PSP_SP(this)++;
            PSP_R9(this) = *PSP_SP(this)++;
            PSP_R8(this) = *PSP_SP(this)++;
#endif
            PSP_EBX(this) = *PSP_SP(this)++;
            PSP_EAX(this) = *PSP_SP(this)++;
            PSP_EDX(this) = *PSP_SP(this)++;
            PSP_IC(this) = (*PSP_SP(this)).AsCodePtr(); // Set the return address
            return -2;

        case RETURN_CALLBACK_EXCEPTION:
            // An ML callback has raised an exception.
            SetException((poly_exn *)PSP_EAX(this).AsObjPtr());
            // Raise a C++ exception.  If the foreign function that called this callback
            // doesn't handle the exception it will be raised in the calling ML function.
            // But if it is caught we may have a problem ...
            throw IOException();

        default:
            Crash("Unknown return reason code %u", this->memRegisters.returnReason);
        }

    } while (1);
}

void X86TaskData::InitStackFrame(TaskData *parentTaskData, Handle proc, Handle arg)
/* Initialise stack frame. */
{
    StackSpace *space = this->stack;
    StackObject * newStack = space->stack();
    POLYUNSIGNED stack_size     = space->spaceSize();
    POLYUNSIGNED topStack = stack_size-3;
    newStack->p_pc    = PC_RETRY_SPECIAL;
    newStack->p_sp    = (PolyWord*)newStack+topStack; 
    this->memRegisters.handlerRegister    = (PolyWord*)newStack+topStack+1;

    /* If this function takes an argument store it in the argument register. */
    if (arg == 0) newStack->p_eax = TAGGED(0);
    else newStack->p_eax = DEREFWORD(arg);
    newStack->p_ebx = TAGGED(0);
    newStack->p_ecx = TAGGED(0);
    // We may set the function in SetCallbackFunction
    if (proc == 0) newStack->p_edx = TAGGED(0);
    else newStack->p_edx = DEREFWORDHANDLE(proc); /* rdx - closure pointer */
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
       exception.  A bit of this was added to fix a bug when stacks were objects
       on the heap and could be scanned by the GC. */
    ((PolyWord*)newStack)[topStack+2] = TAGGED(0); // Probably no longer needed
    // Set the default handler and return address to point to this code.

    X86TaskData *mdParentTask = (X86TaskData*)parentTaskData;
    Handle killCode = mdParentTask->BuildKillSelf();
    PolyWord killJump = killCode->Word();
    // Exception handler.
    ((PolyWord*)newStack)[topStack+1] = killJump;
    // Normal return address.  We need a separate entry on the stack from
    // the exception handler because it is possible that the code we are entering
    // may replace this entry with an argument.  The code-generator optimises tail-recursive
    // calls to functions with more args than the called function.
    ((PolyWord*)newStack)[topStack] = killJump;
}

// Build an ML code segment to hold a copy of a piece of code
Handle X86TaskData::BuildCodeSegment(const byte *code, unsigned bytes, char functionName)
{
    POLYUNSIGNED codeWords = (bytes + sizeof(PolyWord)-1) / sizeof(PolyWord);
    POLYUNSIGNED words = codeWords + 6;
    Handle codeHandle = alloc_and_save(this, words, F_BYTE_OBJ|F_MUTABLE_BIT);
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
    CodeSegmentFlags(this, this->saveVec.push(TAGGED(F_CODE_OBJ)), codeHandle);
    return codeHandle;
}

// Set up a handler that, if it's called, will print an exception trace.
// If the handler isn't called the dummy handler is simply removed.
// This is tricky since when we "return" we actually need to run the new
// function.
void X86TaskData::SetExceptionTrace()
{
    PSP_IC(this) = (*PSP_SP(this)).AsCodePtr();
    Handle fun = this->saveVec.push(PSP_EAX(this));
    Handle extrace = BuildExceptionTrace();
    PolyObject *functToCall = fun->WordP();
    PSP_EDX(this) = functToCall; // Closure address
    // Leave the return address where it is on the stack.
    PSP_IC(this) = functToCall->Get(0).AsCodePtr(); // First word of closure is entry pt.
    *(--PSP_SP(this)) = PolyWord::FromStackAddr(PSP_HR(this));
    // Handler addresses must be word + 2 byte aligned.
    // Is that still true or only for the old exception mechanism?
    *(--PSP_SP(this)) = PolyWord::FromCodePtr(extrace->WordP()->AsBytePtr()+2);
    PSP_HR(this) = PSP_SP(this);
    byte *codeAddr = (byte*)&X86AsmRestoreHandlerAfterExceptionTraceTemplate;
    Handle retCode = BuildCodeSegment(codeAddr, 8 /* Code is 8 bytes */, 'R');
    *(--PSP_SP(this)) = retCode->WordP(); // Code for normal return.
    PSP_EAX(this) = TAGGED(0); // Set the argument of the function to "unit".
}

// In Solaris-x86 the registers are named EIP and ESP.
#if (!defined(REG_EIP) && defined(EIP))
#define REG_EIP EIP
#endif
#if (!defined(REG_ESP) && defined(ESP))
#define REG_ESP ESP
#endif


// Get the PC and SP(stack) from a signal context.  This is needed for profiling.
bool X86TaskData::GetPCandSPFromContext(SIGNALCONTEXT *context, PolyWord * &sp, POLYCODEPTR &pc)
{
    // Check carefully for valid pointers.  Because this can be called
    // at any time some of these may be invalid.
    if (this->memRegisters.inRTS)
    {
        if (this->stack == 0) return false;
        sp = PSP_SP(this);
        pc = PSP_IC(this);
        return true;
    }
    if (context == 0) return false;
// The tests for HAVE_UCONTEXT_T, HAVE_STRUCT_SIGCONTEXT and HAVE_WINDOWS_H need
// to follow the tests in processes.h.
#if defined(HAVE_WINDOWS_H)
#ifdef _WIN64
    sp = (PolyWord *)context->Rsp;
    pc = (POLYCODEPTR)context->Rip;
#else
    // Windows 32 including cygwin.
    sp = (PolyWord *)context->Esp;
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
#else
    // Can't get context.
    return false;
#endif
    // Check the sp value is in the current stack.
    if (sp >= this->stack->bottom && sp < this->stack->top)
        return true;
    else
        return false; // Bad stack pointer
}

void X86TaskData::InterruptCode()
{
    // Set the stack limit pointer to the top of the stack to cause
    // a trap when we next check for stack overflow.
    // SetMemRegisters actually does this anyway if "pendingInterrupt" is set but
    // it's safe to do this repeatedly.
    if (this->stack != 0) 
        this->memRegisters.stackLimit = this->stack->top-1;
    this->pendingInterrupt = true;
}

// This is called from SwitchToPoly before we enter the ML code.
void X86TaskData::SetMemRegisters()
{
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

    this->memRegisters.localMbottom = this->allocLimit + 1;
    this->memRegisters.localMpointer = this->allocPointer + 1;
    // If we are profiling store allocation we set mem_hl so that a trap
    // will be generated.
    if (profileMode == kProfileStoreAllocation)
        this->memRegisters.localMbottom = this->memRegisters.localMpointer;

    this->memRegisters.polyStack = this->stack->stack();
    // Whenever the ML code enters a function it checks that the stack pointer is above
    // this value.  The default is to set it to the top of the reserved area
    // but if we've had an interrupt we set it to the end of the stack.
    // InterruptCode may be called either when the thread is in the RTS or in ML code.
    if (this->pendingInterrupt) this->memRegisters.stackLimit = this->stack->top - 1;
    else this->memRegisters.stackLimit = this->stack->bottom + OVERFLOW_STACK_SIZE;
    this->memRegisters.requestCode = 0; // Clear these because only one will be set.
    this->memRegisters.returnReason = RETURN_IO_CALL;

    this->memRegisters.threadId = this->threadObject;
 
    // We set the PC to zero to indicate that we should retry the call to the RTS
    // function.  In that case we need to set it back to the code address before we
    // return.  This is also used if we have raised an exception.
    if (PSP_IC(this) == PC_RETRY_SPECIAL)
        PSP_IC(this) = PSP_EDX(this).AsObjPtr()->Get(0).AsCodePtr();
}

// This is called whenever we have returned from ML to C.
void X86TaskData::SaveMemRegisters()
{
    // Check a few items on the stack to see it hasn't been overwritten
    StackObject *st = this->stack->stack();
    if (st->p_nUnchecked != UNCHECKED_REGS)
        Crash("Stack overwritten\n");
    this->allocPointer = this->memRegisters.localMpointer - 1;
    this->allocWords = 0;
    // We need to restore all the registers if we are emulating an instruction or
    // are handling a heap or stack overflow.  For the moment we just consider
    // all cases apart from an RTS call.
    this->memRegisters.fullRestore = this->memRegisters.returnReason != 0 ? 1 : 0;
}

PolyWord *X86TaskData::get_reg(int n)
/* Returns a pointer to the register given by n. */
{
    StackObject *stack = x86Stack(this);
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

PolyWord *X86TaskData::getArgument(unsigned int modRm, unsigned int rexPrefix, bool *inConsts)
{
    unsigned int md = modRm >> 6;
    unsigned int rm = modRm & 7;
    if (inConsts) *inConsts = false; // Default
    if (md == 3) // Register
        return get_reg(rm + (rexPrefix & 0x1)*8);
    else if (rm == 4)
    {
        // s-i-b present.  Used for esp and r12 as well as indexing.
        unsigned int sib = PSP_IC(this)[0];
        unsigned int index = (sib >> 3) & 7;
        unsigned int ss = (sib >> 6) & 3;
        unsigned int base = sib & 7;
        PSP_INCR_PC(this, 1);
        if (md == 0 && base == 5)
            // This should not occur in either 32 or 64-bit mode.
            Crash("Immediate address in emulated instruction");
        else
        {
            int offset = 0;
            if (md == 1)
            {
                // One byte offset
                offset = PSP_IC(this)[0];
                if (offset >= 128) offset -= 256;
                PSP_INCR_PC(this, 1);
            }
            else if (md == 2)
            {
                // Four byte offset
                offset = PSP_IC(this)[3];
                if (offset >= 128) offset -= 256;
                offset = offset*256 + PSP_IC(this)[2];
                offset = offset*256 + PSP_IC(this)[1];
                offset = offset*256 + PSP_IC(this)[0];
                PSP_INCR_PC(this, 4);
            }
            if (ss != 0 || index != 4) Crash("Index register present");
            byte *ea;
            if (rexPrefix & 0x1) base += 8;
            if (base == 4) /* esp */ ea = (byte*)PSP_SP(this) + offset;
            else ea = get_reg(base)->AsCodePtr()+offset;
            return (PolyWord*)ea;
        }
    }
    else if (md == 0 && rm == 5)
    {
#ifdef HOSTARCHITECTURE_X86_64
        // In 64-bit mode this means PC-relative
        int offset = PSP_IC(this)[3];
        if (offset >= 128) offset -= 256;
        offset = offset*256 + PSP_IC(this)[2];
        offset = offset*256 + PSP_IC(this)[1];
        offset = offset*256 + PSP_IC(this)[0];
        PSP_INCR_PC(this, 4);
        if (inConsts) *inConsts = true;
        return (PolyWord*)(this->stack->stack()->p_pc + offset);
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
            offset = PSP_IC(this)[0];
            if (offset >= 128) offset -= 256;
            PSP_INCR_PC(this, 1);
        }
        else if (md == 2)
        {
            // Four byte offset
            offset = PSP_IC(this)[3];
            if (offset >= 128) offset -= 256;
            offset = offset*256 + PSP_IC(this)[2];
            offset = offset*256 + PSP_IC(this)[1];
            offset = offset*256 + PSP_IC(this)[0];
            PSP_INCR_PC(this, 4);
        }
        PolyWord base = *(get_reg(rm + (rexPrefix & 0x1)*8));
        byte *ea = base.AsCodePtr() + offset;
        return (PolyWord*)ea;
    }
}

// Called as a result of a heap overflow trap
void X86TaskData::HeapOverflowTrap()
{
    X86TaskData *mdTask = this;
    StackObject *stack = x86Stack(this);
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
    PolyWord *reg = get_reg(mdTask->allocReg);
    PolyWord reg_val = *reg;
    // The space we need is the difference between this register
    // and the current value of newptr.
    // The +1 here is because memRegisters.localMpointer is A.M.pointer +1.  The reason
    // is that after the allocation we have the register pointing at the address we will
    // actually use.
    wordsNeeded = (this->allocPointer - (PolyWord*)reg_val.AsAddress()) + 1;
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
        PolyWord *reg = get_reg(this->allocReg);
        PolyWord reg_val = *reg;
        wordsNeeded = (this->allocPointer - (PolyWord*)reg_val.AsAddress()) + 1;
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
        this->allocPointer += wordsNeeded;
    }
#endif /* HOSTARCHITECTURE_X86_64 */
    if (profileMode == kProfileStoreAllocation)
        add_count(this, stack->p_pc, stack->p_sp, wordsNeeded);

    mdTask->allocWords = wordsNeeded; // The actual allocation is done in SetMemRegisters.
}


/******************************************************************************/
/*                                                                            */
/*      do_compare - do a "long" comparison, setting the flags register       */
/*                                                                            */
/******************************************************************************/
void X86TaskData::do_compare(PolyWord v1, PolyWord v2)
{
    Handle val1, val2;
    /* Must push these to the save vec.  A persistent store trap
       might cause a garbage collection and move the stack. */
    val1 = this->saveVec.push(v1);
    val2 = this->saveVec.push(v2);
    int r = compareLong(this, val2, val1);
    /* Clear the flags. */
    POLYUNSIGNED flags = PSP_EFLAGS(this);
    flags &= -256;
    if (r == 0) flags |= EFLAGS_ZF;
    else if (r < 0) flags |= EFLAGS_SF;
    PSP_EFLAGS(this) = flags;
}

/******************************************************************************/
/*                                                                            */
/*      do_op - do a "long" operation, setting the destination register       */
/*                                                                            */
/******************************************************************************/
void X86TaskData::do_op(int dest, PolyWord v1, PolyWord v2, Handle (*op)(TaskData *, Handle, Handle))
{
    Handle val1, val2, result;
    /* Must push these to the save vec.  A persistent store trap
       or a garbage collection might move the stack. */
    val1 = this->saveVec.push(v1);
    val2 = this->saveVec.push(v2);
    /* Clobber the destination which may have overflowed. */
    *(get_reg(dest)) = TAGGED(0);
    result = op (this, val2, val1);     /* N.B parameters are intentionally reversed */
    /* N.B. the stack may have moved so we must recompute get_reg(dest). */
    *(get_reg(dest)) = DEREFWORD(result);
}

// Emulate a long precision operation.
// The instruction formats have changed in 5.4 so this supports
// both 5.3 and earlier and also 5.4 format.
bool X86TaskData::emulate_instrs()
{
    int src1 = -1, src2 = -1, dest = -1;
    bool doneSubtraction = false;
    POLYUNSIGNED flagsWord = PSP_EFLAGS(this);
    PSP_EFLAGS(this) &= ~EFLAGS_OF; // Make sure the overflow flag is clear.
    while(1) {
        byte rexPrefix = 0;
#ifdef HOSTARCHITECTURE_X86_64
        // Get any REX prefix
        if (PSP_IC(this)[0] >= 0x40 && PSP_IC(this)[0] <= 0x4f)
        {
            rexPrefix = PSP_IC(this)[0];
            PSP_INCR_PC(this, 1);
        }
#endif /* HOSTARCHITECTURE_X86_64 */
        // Decode the register fields and include any REX bits
        int bbb = PSP_IC(this)[1] & 7;
        if (rexPrefix & 0x1) bbb += 8;
        int rrr = (PSP_IC(this)[1] >> 3) & 7;
        if (rexPrefix & 0x4) rrr += 8;

        switch (PSP_IC(this)[0]) {
        case 0x03: 
            {
                /* add. */
                PSP_INCR_PC(this, 1);
                int modRm = PSP_IC(this)[0];
                PSP_INCR_PC(this, 1);
                bool inConsts = false;
                PolyWord arg2 = *(getArgument(modRm, rexPrefix, &inConsts));
                if (dest == -1 || dest == src1) { // New format.
                    ASSERT(dest == -1 || dest == rrr); // Destination regs should be the same
                    PolyWord *destReg = get_reg(rrr);
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
                        // If we have previously subtracted the tag we have to add it back.
                        if (dest != -1)
                            arg1 = PolyWord::FromUnsigned(arg1.AsUnsigned()+1);
                    }
                    // If this is in the 64-bit non-address area it is a constant with the
                    // tag removed.  Add it back in.
                    if (inConsts) arg2 = PolyWord::FromUnsigned(arg2.AsUnsigned()+1);
                    do_op(rrr, arg1, arg2, add_longc);
                    // The next operation will subtract the tag.  We need to add in a dummy tag..
                    // This may cause problems with CheckRegion which assumes that every register
                    // contains a valid value.
                    if (! inConsts && dest == -1) {
                        destReg = get_reg(rrr); // May have moved because of a GC.
                        *destReg = PolyWord::FromUnsigned(destReg->AsUnsigned()+1);
                    }
                }
                else { // Legacy format
                    if (dest != rrr)
                        Crash("Expected same destination register.");
                    do_op(dest, *(get_reg(src1)), arg2, add_longc);
                }
                return true;
            }

        case 0x2b: /* Subtraction. */
            {
                PSP_INCR_PC(this, 1);
                int modRm = PSP_IC(this)[0];
                PSP_INCR_PC(this, 1);
                bool inConsts = false;
                PolyWord arg2 = *(getArgument(modRm, rexPrefix, &inConsts));
                if (dest == -1) { // New format
                    PolyWord *destReg = get_reg(rrr);
                    PolyWord arg1 = *destReg;
                    // We could have come here because of testing the tags, which happens
                    // before the operation, or as a result of subtracting two tagged values in which
                    // case arg1 will contain the result after the subtraction.
                    if (flagsWord & EFLAGS_OF) {
                        arg1 = PolyWord::FromUnsigned(arg1.AsUnsigned() + arg2.AsUnsigned());
                    }
                    // If this is in the 64-bit non-address area it is a constant with the
                    // tag removed.  Add it back in.  N.B.  In this case we don't have a following
                    // instruction to add the tag.
                    if (inConsts) arg2 = PolyWord::FromUnsigned(arg2.AsUnsigned()+1);
                    do_op(rrr, arg1, arg2, sub_longc);
                    // The next operation will add the tag.  We need to subtract a dummy tag..
                    // This may cause problems with CheckRegion which assumes that every register
                    // contains a valid value.
                    if (! inConsts) {
                        destReg = get_reg(rrr); // May have moved because of a GC.
                        *destReg = PolyWord::FromUnsigned(destReg->AsUnsigned()-1);
                    }
                    return true;
                }
                else { // Legacy format
                    if (dest != rrr)
                        Crash("Expected same destination register.");
                    do_op(dest, *(get_reg(src1)), arg2, sub_longc);
                    doneSubtraction = true;
                    break;
                }
            }

        case 0x3b: /* Compare. */
            {
                PSP_INCR_PC(this, 1);
                int modRm = PSP_IC(this)[0];
                PSP_INCR_PC(this, 1);
                PolyWord arg = *(getArgument(modRm, rexPrefix));
                do_compare(*(get_reg(rrr)), arg);
                return true;
            }

        case 0x8d: /* leal - Used to remove a tag before an add and multiply. */
            // Also used to put the tag on after a subtraction.
            if ((PSP_IC(this)[1] & 7) == 4)
            { // R12 (and RSP but that isn't used here) have to be encoded with a SIB byte.
                ASSERT((PSP_IC(this)[2] & 7) == 4); // Should be same register
                PSP_INCR_PC(this, 1);
            }
            if (doneSubtraction)
            {
                PSP_INCR_PC(this, 3);
                return true;
            }
            if (src1 == -1) src1 = bbb; else src2 = bbb;
            dest = rrr;
            ASSERT(PSP_IC(this)[2] == 0xff);
            PSP_INCR_PC(this, 3);
            break;

        case 0x89: /* movl: move source into dest. */
            if ((PSP_IC(this)[1] & 0xc0) != 0xc0)
                 Crash("Can't move into store.");
            dest = bbb;
            if (src1 == -1) src1 = rrr; else src2 = rrr;
            PSP_INCR_PC(this, 2);
                /* Next should be add-immediate. */
            break;

        case 0x83: { /* One byte immediate: Add, sub or compare. */
            PSP_INCR_PC(this, 1);
            int modRm = PSP_IC(this)[0];
            PSP_INCR_PC(this, 1);
            PolyWord arg = *(getArgument(modRm, rexPrefix));

            int cval = PSP_IC(this)[0];
            if (cval >= 128) cval -= 256;
            PSP_INCR_PC(this, 1);

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
                        do_op(bbb, arg, PolyWord::FromSigned(cval+1), add_longc);
                    }
                    else do_op(dest, *(get_reg(src1)), PolyWord::FromSigned(cval+1), add_longc);
                    break;
                }

                case (5 << 3): /* sub */
                {
                    if (dest != bbb) { // New format: Same register for source and destination.
                        // We didn't have a move instruction before this.
                        PolyWord arg = *(get_reg(bbb));
                        if (arg.IsTagged()) arg = PolyWord::FromUnsigned(arg.AsUnsigned() + cval);
                        // Immediate value is shifted, but hasn't had 1 added;
                        // do this now before calling sub_longc
                        do_op(bbb, arg, PolyWord::FromSigned(cval+1), sub_longc);
                    }
                    else do_op(dest, *(get_reg(src1)), PolyWord::FromSigned(cval+1), sub_longc);
                    break;
                }

                case (7 << 3): /* cmp */
                {
                    /* immediate value is already tagged */
                    do_compare(arg, PolyWord::FromSigned(cval));
                    break;
                }

                default: Crash("Unknown instruction after overflow trap");
            }
            return true;
            }

        case 0x81: { /* 4 byte immediate: Add, sub or compare. */
            PSP_INCR_PC(this, 1);
            int modRm = PSP_IC(this)[0];
            PSP_INCR_PC(this, 1);
            PolyWord arg = *(getArgument(modRm, rexPrefix));

            int cval = PSP_IC(this)[3];
            if (cval >= 128) cval -= 256;
            cval = cval*256 + PSP_IC(this)[2];
            cval = cval*256 + PSP_IC(this)[1];
            cval = cval*256 + PSP_IC(this)[0];
            PSP_INCR_PC(this, 4);

            switch (modRm & (7 << 3))
            {
                case (0 << 3): /* add */
                {
                    if (dest != bbb) { // New format: Same register for source and destination.
                        if (arg.IsTagged()) {
                            arg = PolyWord::FromUnsigned(arg.AsUnsigned() - cval);
                        }
                        do_op(bbb, arg, PolyWord::FromSigned(cval+1), add_longc);
                    }
                    else do_op(dest, *(get_reg(src1)), PolyWord::FromSigned(cval+1), add_longc);
                    break;
                }

                case (5 << 3): /* sub */
                {
                    if (dest != bbb) { // New format: Same register for source and destination.
                        // We didn't have a move instruction before this.
                        if (arg.IsTagged()) arg = PolyWord::FromUnsigned(arg.AsUnsigned() + cval);
                        do_op(bbb, arg, PolyWord::FromSigned(cval+1), sub_longc);
                    }
                    else do_op(dest, *(get_reg(src1)), PolyWord::FromSigned(cval+1), sub_longc);
                    break;
                }

                case (7 << 3): /* cmp */
                {
                    // Immediate value is already tagged or may be an address.
                    do_compare(arg, PolyWord::FromSigned(cval));
                    break;
                }

                default: Crash("Unknown instruction after overflow trap");
            }
            return true;
            }

        case 0xeb: // jmp - used in branch forwarding.
            // This is used to skip back to the instruction being emulated.
            if (PSP_IC(this)[1] >= 128)
                PSP_INCR_PC(this, PSP_IC(this)[1] - 256 + 2);
            else PSP_INCR_PC(this, PSP_IC(this)[1] + 2);
            break;

        case 0x50: /* push eax - used before a multiply. */
#ifdef HOSTARCHITECTURE_X86_64
            ASSERT((rexPrefix & 1) == 0); // Check it's not r8
#endif /* HOSTARCHITECTURE_X86_64 */
            *(--PSP_SP(this)) = PSP_EAX(this);
            PSP_INCR_PC(this, 1);
            break;

        case 0x52: /* push edx - used before a multiply. */
#ifdef HOSTARCHITECTURE_X86_64
            ASSERT((rexPrefix & 1) == 0); // Check it's not r10
#endif /* HOSTARCHITECTURE_X86_64 */
            *(--PSP_SP(this)) = PSP_EDX(this);
            PSP_INCR_PC(this, 1);
            break;

        case 0xd1: /* Group1A - must be sar edx before a multiply or sar [esp] before Real.fromInt */
            if (PSP_IC(this)[1] == 0xfa) {
                PSP_INCR_PC(this, 2);
                /* If we haven't moved anything into edx then edx must be
                   one of the arguments. */
                if (src2 == -1) src2 = 2; /* edx. */
            }
            else if (PSP_IC(this)[1] == 0x3c) {
                PSP_INCR_PC(this, 3);
            }
            else Crash("Unknown instruction after overflow trap");
            break;

        case 0xf7: /* Multiply instruction. */
            if (PSP_IC(this)[1] != 0xea)
                Crash("Unknown instruction after overflow trap");
            do_op(0 /* eax */, *(get_reg(src1)), *(get_reg(src2)), mult_longc);
            /* Subtract one because the next instruction will tag it. */
            PSP_EAX(this) = PolyWord::FromUnsigned(PSP_EAX(this).AsUnsigned() - 1);
            PSP_INCR_PC(this, 2);
            return true;

        case 0xdb: // Floating point ESCAPE 3
        case 0xdf:
            {
                StackObject *stack = x86Stack(this);
#ifdef HOSTARCHITECTURE_X86_64
                if (stack->p_pc[1] != 0x2c || stack->p_pc[2] != 0x24)
                    Crash("Unknown instruction after overflow trap");
#else
                if (stack->p_pc[1] != 0x04 || stack->p_pc[2] != 0x24)
                    Crash("Unknown instruction after overflow trap");
#endif /* HOSTARCHITECTURE_X86_64 */
                // The operand is on the stack.
                union { double dble; byte bytes[sizeof(double)]; } dValue;
                dValue.dble = get_C_real(this, stack->p_sp[0]);
                unsigned top = (stack->p_fp.sw >> 11) & 7;
                top = (top-1) & 0x7;
                stack->p_fp.sw = (stack->p_fp.sw & (~0x3800)) | (top << 11);
                stack->p_fp.tw &= ~(3 << top*2); // Needed?
                // Push the stack down
                for (unsigned i = 7; i != 0; i--)
                    memcpy(stack->p_fp.registers[i], stack->p_fp.registers[i-1], sizeof(fpregister));
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
                PSP_INCR_PC(this, 3);
            }
            return true;

        default:
            Crash("Unknown instruction after overflow trap");
        }
    }
    return false;
}

void X86TaskData::ArbitraryPrecisionTrap()
{
    // Arithmetic operation has overflowed or detected long values.
    if (profileMode == kProfileEmulation)
        add_count(this, PSP_IC(this), PSP_SP(this), 1);
    // Emulate the arbitrary precision instruction.
    if (! emulate_instrs())
        Crash("Arbitrary precision emulation fault at %x\n", PSP_IC(this));
}

// These macros build small pieces of assembly code for each io call.
// The code simply sets the requestCode value and jumps to
// X86AsmSaveStateAndReturn.  The address of these code pieces is
// stored in iovec.  Values in iovec are never looked at with the
// garbage collector so that's safe.

// N.B.  The length of this code (7) is built into BuildKillSelf
// It's 7 bytes on both x86 and X86_64.
#define MAKE_CALL_SEQUENCE_BYTES     7

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
    add_function_to_io_area(POLY_SYS_exit, &X86AsmCallPOLY_SYS_exit);
    add_function_to_io_area(POLY_SYS_alloc_store, &alloc_store);
    add_function_to_io_area(POLY_SYS_alloc_uninit, &alloc_uninit);
    add_function_to_io_area(POLY_SYS_chdir, &X86AsmCallPOLY_SYS_chdir);
    add_function_to_io_area(POLY_SYS_get_length, &get_length_a);
    add_function_to_io_area(POLY_SYS_get_flags, &X86AsmCallPOLY_SYS_get_flags);
    add_function_to_io_area(POLY_SYS_str_compare, str_compare);
    add_function_to_io_area(POLY_SYS_teststrgtr, &teststrgtr);
    add_function_to_io_area(POLY_SYS_teststrlss, &teststrlss);
    add_function_to_io_area(POLY_SYS_teststrgeq, &teststrgeq);
    add_function_to_io_area(POLY_SYS_teststrleq, &teststrleq);
    add_function_to_io_area(POLY_SYS_exception_trace_fn,  &X86AsmCallPOLY_SYS_exception_trace_fn);
    add_function_to_io_area(POLY_SYS_lockseg, &locksega);
    add_function_to_io_area(POLY_SYS_profiler,  &X86AsmCallPOLY_SYS_profiler);
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
    add_function_to_io_area(POLY_SYS_Real_str, &X86AsmCallPOLY_SYS_Real_str);
    add_function_to_io_area(POLY_SYS_Real_geq, real_geq);
    add_function_to_io_area(POLY_SYS_Real_leq, real_leq);
    add_function_to_io_area(POLY_SYS_Real_gtr, real_gtr);
    add_function_to_io_area(POLY_SYS_Real_lss, real_lss);
    add_function_to_io_area(POLY_SYS_Real_eq,  real_eq);
    add_function_to_io_area(POLY_SYS_Real_neq, real_neq);
    add_function_to_io_area(POLY_SYS_Real_Dispatch,  &X86AsmCallPOLY_SYS_Real_Dispatch);
    add_function_to_io_area(POLY_SYS_Add_real, real_add);
    add_function_to_io_area(POLY_SYS_Sub_real, real_sub);
    add_function_to_io_area(POLY_SYS_Mul_real, real_mul);
    add_function_to_io_area(POLY_SYS_Div_real, real_div);
    add_function_to_io_area(POLY_SYS_Abs_real, real_abs);
    add_function_to_io_area(POLY_SYS_Neg_real, real_neg);
    add_function_to_io_area(POLY_SYS_Repr_real, &X86AsmCallPOLY_SYS_Repr_real);
    add_function_to_io_area(POLY_SYS_conv_real, &X86AsmCallPOLY_SYS_conv_real);
    add_function_to_io_area(POLY_SYS_real_to_int, &X86AsmCallPOLY_SYS_real_to_int);
    add_function_to_io_area(POLY_SYS_int_to_real, real_from_int);
    add_function_to_io_area(POLY_SYS_sqrt_real, &X86AsmCallPOLY_SYS_sqrt_real);
    add_function_to_io_area(POLY_SYS_sin_real, &X86AsmCallPOLY_SYS_sin_real);
    add_function_to_io_area(POLY_SYS_cos_real, &X86AsmCallPOLY_SYS_cos_real);
    add_function_to_io_area(POLY_SYS_arctan_real, &X86AsmCallPOLY_SYS_arctan_real);
    add_function_to_io_area(POLY_SYS_exp_real, &X86AsmCallPOLY_SYS_exp_real);
    add_function_to_io_area(POLY_SYS_ln_real, &X86AsmCallPOLY_SYS_ln_real);
    add_function_to_io_area(POLY_SYS_io_operation, &X86AsmCallPOLY_SYS_io_operation);
    add_function_to_io_area(POLY_SYS_atomic_reset, &atomic_reset);
    add_function_to_io_area(POLY_SYS_atomic_incr, &atomic_increment);
    add_function_to_io_area(POLY_SYS_atomic_decr, &atomic_decrement);
    add_function_to_io_area(POLY_SYS_thread_self, &thread_self);
    add_function_to_io_area(POLY_SYS_thread_dispatch, &X86AsmCallPOLY_SYS_thread_dispatch);
    add_function_to_io_area(POLY_SYS_kill_self, &X86AsmCallPOLY_SYS_kill_self);
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
    add_function_to_io_area(POLY_SYS_load_byte_immut, &load_byte);
    add_function_to_io_area(POLY_SYS_load_word_immut, &load_word);
    add_function_to_io_area(POLY_SYS_is_big_endian, &is_big_endian);
    add_function_to_io_area(POLY_SYS_bytes_per_word, &bytes_per_word);
    add_function_to_io_area(POLY_SYS_assign_byte, &assign_byte);
    add_function_to_io_area(POLY_SYS_assign_word, &assign_word);
    add_function_to_io_area(POLY_SYS_objsize, &X86AsmCallPOLY_SYS_objsize);
    add_function_to_io_area(POLY_SYS_showsize, &X86AsmCallPOLY_SYS_showsize);
    add_function_to_io_area(POLY_SYS_timing_dispatch, &X86AsmCallPOLY_SYS_timing_dispatch);
    add_function_to_io_area(POLY_SYS_XWindows, &X86AsmCallPOLY_SYS_XWindows);
    add_function_to_io_area(POLY_SYS_full_gc, &X86AsmCallPOLY_SYS_full_gc);
    add_function_to_io_area(POLY_SYS_stack_trace, &X86AsmCallPOLY_SYS_stack_trace);
    add_function_to_io_area(POLY_SYS_foreign_dispatch, &X86AsmCallPOLY_SYS_foreign_dispatch);
    add_function_to_io_area(POLY_SYS_callcode_tupled, &X86AsmCallPOLY_SYS_callcode_tupled);
    add_function_to_io_area(POLY_SYS_process_env, &X86AsmCallPOLY_SYS_process_env);
    add_function_to_io_area(POLY_SYS_set_string_length, &set_string_length_a);
    add_function_to_io_area(POLY_SYS_get_first_long_word, &get_first_long_word_a);
    add_function_to_io_area(POLY_SYS_shrink_stack, &X86AsmCallPOLY_SYS_shrink_stack);
    add_function_to_io_area(POLY_SYS_code_flags, &X86AsmCallPOLY_SYS_code_flags);
    add_function_to_io_area(POLY_SYS_shift_right_arith_word, &shift_right_arith_word);
    add_function_to_io_area(POLY_SYS_int_to_word,      &int_to_word);
    add_function_to_io_area(POLY_SYS_set_code_constant, &X86AsmCallPOLY_SYS_set_code_constant);
    add_function_to_io_area(POLY_SYS_poly_specific, &X86AsmCallPOLY_SYS_poly_specific);
    add_function_to_io_area(POLY_SYS_bytevec_eq,        &bytevec_eq);
    add_function_to_io_area(POLY_SYS_move_bytes,        &move_bytes); 
    add_function_to_io_area(POLY_SYS_move_bytes_overlap,&move_bytes); 
    add_function_to_io_area(POLY_SYS_move_words,        &move_words);
    add_function_to_io_area(POLY_SYS_move_words_overlap,&move_words);
    add_function_to_io_area(POLY_SYS_mul_word,          &mul_word);
    add_function_to_io_area(POLY_SYS_plus_word,         &plus_word);
    add_function_to_io_area(POLY_SYS_minus_word,        &minus_word);
    add_function_to_io_area(POLY_SYS_div_word,          &div_word);
    add_function_to_io_area(POLY_SYS_mod_word,          &mod_word);
    add_function_to_io_area(POLY_SYS_word_geq,          &word_geq);
    add_function_to_io_area(POLY_SYS_word_leq,          &word_leq);
    add_function_to_io_area(POLY_SYS_word_gtr,          &word_gtr);
    add_function_to_io_area(POLY_SYS_word_lss,          &word_lss);

    // This used to contain the code itself.  Now this is set up as a "closure"
    // but is only used for exceptions raised within the RTS.
    add_function_to_io_area(POLY_SYS_raisex,           &raisex);
    
    add_function_to_io_area(POLY_SYS_io_dispatch, &X86AsmCallPOLY_SYS_io_dispatch);
    add_function_to_io_area(POLY_SYS_network, &X86AsmCallPOLY_SYS_network);
    add_function_to_io_area(POLY_SYS_os_specific, &X86AsmCallPOLY_SYS_os_specific);
    add_function_to_io_area(POLY_SYS_signal_handler, &X86AsmCallPOLY_SYS_signal_handler);
    add_function_to_io_area(POLY_SYS_eq_longword, &eq_longword);
    add_function_to_io_area(POLY_SYS_neq_longword, &neq_longword);
    add_function_to_io_area(POLY_SYS_geq_longword, &geq_longword);
    add_function_to_io_area(POLY_SYS_leq_longword, &leq_longword);
    add_function_to_io_area(POLY_SYS_gt_longword, &gt_longword);
    add_function_to_io_area(POLY_SYS_lt_longword, &lt_longword);
    add_function_to_io_area(POLY_SYS_plus_longword, &plus_longword);
    add_function_to_io_area(POLY_SYS_minus_longword, &minus_longword);
    add_function_to_io_area(POLY_SYS_mul_longword, &mul_longword);
    add_function_to_io_area(POLY_SYS_div_longword, &div_longword);
    add_function_to_io_area(POLY_SYS_mod_longword, &mod_longword);
    add_function_to_io_area(POLY_SYS_andb_longword, &andb_longword);
    add_function_to_io_area(POLY_SYS_orb_longword, &orb_longword);
    add_function_to_io_area(POLY_SYS_xorb_longword, &xorb_longword);
    add_function_to_io_area(POLY_SYS_shift_left_longword, &shift_left_longword);
    add_function_to_io_area(POLY_SYS_shift_right_longword, &shift_right_longword);
    add_function_to_io_area(POLY_SYS_shift_right_arith_longword, &shift_right_arith_longword);
    add_function_to_io_area(POLY_SYS_longword_to_tagged, &longword_to_tagged);
    add_function_to_io_area(POLY_SYS_signed_to_longword, &signed_to_longword);
    add_function_to_io_area(POLY_SYS_unsigned_to_longword, &unsigned_to_longword);

    // Entries for special cases.  These are generally, but not always, called from
    // compiled code.
    heapOverflow = (byte*)&X86AsmCallExtraRETURN_HEAP_OVERFLOW;
    stackOverflow = (byte*)&X86AsmCallExtraRETURN_STACK_OVERFLOW;
    stackOverflowEx = (byte*)X86AsmCallExtraRETURN_STACK_OVERFLOWEX;
    raiseDiv = (byte*)X86AsmCallExtraRETURN_RAISE_DIV;
    arbEmulation = (byte*)X86AsmCallExtraRETURN_ARB_EMULATION;
}

// We need the kill-self code in a little function.
Handle X86TaskData::BuildKillSelf()
{
    byte *codeAddr = (byte*)&X86AsmKillSelfTemplate;
    return BuildCodeSegment(codeAddr, MAKE_CALL_SEQUENCE_BYTES, 'K');
}

// Similarly for the exception trace code.  This is more complicated.
// For backwards compatibility we need the address to be on a word + 2 byte
// boundary.
Handle X86TaskData::BuildExceptionTrace()
{
    byte *codeAddr = (byte*)&X86AsmGiveExceptionTraceFnTemplate;
    return BuildCodeSegment(codeAddr, 9, 'E');
}

void X86TaskData::SetException(poly_exn *exc)
// Set up the stack of a process to raise an exception.
{
    PSP_EDX(this) = (PolyObject*)IoEntry(POLY_SYS_raisex);
    PSP_IC(this)     = PC_RETRY_SPECIAL;
    PSP_EAX(this) = exc; /* put exception data into eax */
}

// Call a piece of compiled code.
void X86TaskData::CallCodeTupled()
{
    // The eventual return address is on the stack - leave it there.
    PolyObject *argTuple = PSP_EAX(this).AsObjPtr();
    Handle closure = this->saveVec.push(argTuple->Get(0));
    Handle argvec = this->saveVec.push(argTuple->Get(1));

    if (! IS_INT(DEREFWORD(argvec))) // May be nil if there are no args.
    {
        PolyObject *argv = DEREFHANDLE(argvec);
        POLYUNSIGNED argCount = argv->Length();
        // Check we have space for the arguments.  This may result in a GC which
        // in turn may throw a C++ exception.
        if (argCount > ARGS_IN_REGS)
        {
            try {
                POLYUNSIGNED min_size =
                    this->stack->top - PSP_SP(this) + OVERFLOW_STACK_SIZE + argCount - ARGS_IN_REGS;
                CheckAndGrowStack(this, min_size);
            }
            catch (IOException &)
            {
                return; // Will have been set up to raise an exception.
            }
        }

        // First argument is in EAX
        PSP_EAX(this) = argv->Get(0);
        // Second arg, if there is one, goes into EBX
        if (argCount > 1)
            PSP_EBX(this) = argv->Get(1);
#ifdef HOSTARCHITECTURE_X86_64
        if (argCount > 2)
            PSP_R8(this) = argv->Get(2);
        if (argCount > 3)
            PSP_R9(this) = argv->Get(3);
        if (argCount > 4)
            PSP_R10(this) = argv->Get(4);
#endif /* HOSTARCHITECTURE_X86_64 */
        // Remaining args go on the stack.
        PolyWord returnAddress = *PSP_SP(this)++;
        for (POLYUNSIGNED i = ARGS_IN_REGS; i < argCount; i++)
        {
            *(--PSP_SP(this)) = argv->Get(i);
        }
        *(--PSP_SP(this)) = returnAddress;
    }
    // The closure goes into the closure reg.
    PSP_EDX(this) = DEREFWORD(closure);
    // First word of closure is entry point.
    PSP_IC(this) = (PSP_EDX(this)).AsObjPtr()->Get(0).AsCodePtr();
}

// Sets up a callback function on the current stack.  The present state is that
// the ML code has made a call in to foreign_dispatch.  We need to set the stack
// up so that we will enter the callback (as with CallCodeTupled) but when we return
// the result we enter callback_return. 
void X86TaskData::SetCallbackFunction(Handle func, Handle args)
{
    byte *codeAddr1 = (byte*)&X86AsmCallbackReturnTemplate;
    byte *codeAddr2 = (byte*)&X86AsmCallbackExceptionTemplate;
    Handle callBackReturn = this->BuildCodeSegment(codeAddr1, MAKE_CALL_SEQUENCE_BYTES, 'C');
    Handle callBackException = this->BuildCodeSegment(codeAddr2, MAKE_CALL_SEQUENCE_BYTES, 'X');
    // Save the closure pointer and argument registers to the stack.  If we have to
    // retry the current RTS call we need these to have their original values.
    *(--PSP_SP(this)) = PSP_EDX(this);
    *(--PSP_SP(this)) = PSP_EAX(this);
    *(--PSP_SP(this)) = PSP_EBX(this);
#ifdef HOSTARCHITECTURE_X86_64
    *(--PSP_SP(this)) = PSP_R8(this);
    *(--PSP_SP(this)) = PSP_R9(this);
    *(--PSP_SP(this)) = PSP_R10(this);
#endif
    // Set up an exception handler so we will enter callBackException if there is an exception.
    *(--PSP_SP(this)) = PolyWord::FromStackAddr(PSP_HR(this)); // Create a special handler entry
    *(--PSP_SP(this)) = callBackException->Word();
    *(--PSP_SP(this)) = TAGGED(0);
    PSP_HR(this) = PSP_SP(this);
    // Push the call to callBackReturn onto the stack as the return address.
    *(--PSP_SP(this)) = callBackReturn->Word();
    // Set up the entry point of the callback.
    PolyObject *functToCall = func->WordP();
    PSP_EDX(this) = functToCall; // Closure address
    PSP_EAX(this) = args->Word();
    PSP_IC(this) = functToCall->Get(0).AsCodePtr(); // First word of closure is entry pt.
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

int X86TaskData::GetIOFunctionRegisterMask(int ioCall)
{
    return registerMaskVector[ioCall];
}

// Increment the value contained in the first word of the mutex.
Handle X86TaskData::AtomicIncrement(Handle mutexp)
{
    PolyObject *p = DEREFHANDLE(mutexp);
    POLYUNSIGNED result = X86AsmAtomicIncrement(p);
    return this->saveVec.push(PolyWord::FromUnsigned(result));
}

// Decrement the value contained in the first word of the mutex.
Handle X86TaskData::AtomicDecrement(Handle mutexp)
{
    PolyObject *p = DEREFHANDLE(mutexp);
    POLYUNSIGNED result = X86AsmAtomicDecrement(p);
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
