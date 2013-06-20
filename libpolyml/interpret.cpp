/*
    Title:  An interpreter for a compact instruction set.
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#include "globals.h"
#include "int_opcodes.h"
#include "machine_dep.h"
#include "sys.h"
#include "profiling.h"
#include "arb.h"
#include "processes.h"
#include "run_time.h"
#include "mpoly.h"
#include "gc.h"
#include "basicio.h"
#include "timing.h"
#include "arb.h"
#include "reals.h"
#include "objsize.h"
#include "xwindows.h"
#include "foreign.h"
#include "process_env.h"
#include "network.h"
#include "basicio.h"
#include "sighandler.h"
#include "os_specific.h"
#include "diagnostics.h"
#include "polystring.h"
#include "save_vec.h"
#include "memmgr.h"

#define VERSION_NUMBER  POLY_version_number

#define arg1    (pc[0] + pc[1]*256)
#define arg2    (pc[2] + pc[3]*256)
#define arg3    (pc[4] + pc[5]*256)
#define arg4    (pc[6] + pc[7]*256)

const PolyWord True = TAGGED(1);
const PolyWord False = TAGGED(0);
const PolyWord Zero = TAGGED(0);

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
   sizeof(StackObject)/sizeof(PolyWord) + \
   CHECKED_REGS + \
   UNCHECKED_REGS + \
   EXTRA_STACK)

class IntTaskData: public MDTaskData {
public:
    IntTaskData(): interrupt_requested(false) {}
    
    bool interrupt_requested;
};

// Special values for return addresses or in the address of an exception handler.
// In an exception handler SPECIAL_PC_TRACE_EX means trace this exception, as
// a return address it means exception_trace has returned.
#define SPECIAL_PC_TRACE_EX    TAGGED(0)
#define SPECIAL_PC_END_THREAD  TAGGED(1)
#define SPECIAL_PC_TRACE_EX_FN TAGGED(2)

class Interpreter : public MachineDependent {
public:
    Interpreter() {}

    // Create a task data object.
    virtual MDTaskData *CreateTaskData(void) { return new IntTaskData(); }

    virtual void InitStackFrame(TaskData *taskData, StackSpace *space, Handle proc, Handle arg);
    // Switch to Poly and return with the io function to call.
    virtual int SwitchToPoly(TaskData *taskData);
    virtual void SetForRetry(TaskData *taskData, int ioCall) {} // Nothing to do
    virtual void InitInterfaceVector(void);
    virtual void SetException(TaskData *taskData, poly_exn *exc);
    virtual void InterruptCode(TaskData *taskData);
    virtual int  GetIOFunctionRegisterMask(int ioCall) { return 0; }
    // GetPCandSPFromContext is used in time profiling.  We can't get accurate info so return false.
    virtual bool GetPCandSPFromContext(TaskData *taskData, SIGNALCONTEXT *context, PolyWord * &sp,  POLYCODEPTR &pc)
        { return false; }
    virtual void CallIO0(TaskData *taskData, Handle(*ioFun)(TaskData *));
    virtual void CallIO1(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle));
    virtual void CallIO2(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle));
    virtual void CallIO3(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle));
    virtual void CallIO4(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle, Handle));
    virtual void CallIO5(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle, Handle, Handle));
    virtual Handle CallBackResult(TaskData *taskData);
    virtual Architectures MachineArchitecture(void) { return MA_Interpreted; }

    // These next two have to be handled as special cases.  They need to jump
    // to CALL_CLOSURE to handle the cases where the functions we are tracing or
    // calling are actually IO functions and don't have code addresses.  Actually
    // we could instead make the interpreter handle small tagged integers as
    // code addresses specially.
    virtual void SetExceptionTrace(TaskData *taskData, bool isLegacy) { ASSERT(false); }
    virtual void CallCodeTupled(TaskData *taskData) { ASSERT(false); }
    // Increment or decrement the first word of the object pointed to by the
    // mutex argument and return the new value.
    virtual Handle AtomicIncrement(TaskData *taskData, Handle mutexp);
    virtual Handle AtomicDecrement(TaskData *taskData, Handle mutexp);
    // Set a mutex to one.
    virtual void AtomicReset(TaskData *taskData, Handle mutexp);
};

void Interpreter::InitStackFrame(TaskData *taskData, StackSpace *space, Handle proc, Handle arg)
/* Initialise stack frame. */
{
    StackObject *stack = (StackObject *)space->stack();
    PolyObject *closure = DEREFWORDHANDLE(proc);
    POLYUNSIGNED stack_size = space->spaceSize();
    stack->p_space = OVERFLOW_STACK_SIZE;
    stack->p_pc = *(byte**)(closure);
    stack->p_sp  = (PolyWord*)stack + stack_size-3; /* sp */
    stack->p_nreg = CHECKED_REGS; /* exception arg and last instruction. */
    stack->p_reg[CHECKED_REGS] = PolyWord::FromUnsigned(UNCHECKED_REGS);
    stack->p_reg[0] = TAGGED(0); /* Used for exception argument. */
    stack->p_reg[1] = TAGGED(256); /* No instruction. */
    stack->p_sp  = (PolyWord*)stack + stack_size;

    /* Set up exception handler */
    /* No previous handler so point it at itself. */
    stack->p_sp--;
    *(stack->p_sp) = PolyWord::FromStackAddr(stack->p_sp);
    *(--stack->p_sp) = SPECIAL_PC_END_THREAD; /* Default return address. */
    *(--stack->p_sp) = Zero; /* Default handler. */
    stack->p_hr = stack->p_sp;

    /* If this function takes an argument store it on the stack. */
    if (arg != 0) *(--stack->p_sp) = DEREFWORD(arg);

    *(--stack->p_sp) = SPECIAL_PC_END_THREAD; /* Return address. */
    *(--stack->p_sp) = closure; /* Closure address */
}

void Interpreter::InterruptCode(TaskData *taskData)
/* Stop the Poly code at a suitable place. */
/* We may get an asynchronous interrupt at any time. */
{
    IntTaskData *itd = (IntTaskData *)taskData->mdTaskData;
    itd->interrupt_requested = true;
}


void Interpreter::SetException(TaskData *taskData, poly_exn *exc)
/* Set up the stack of a process to raise an exception. */
{
    taskData->stack->stack()->p_reg[1] = TAGGED(INSTR_raise_ex);
    *(--taskData->stack->stack()->p_sp) = (PolyWord)exc; /* push exception data */
}

int Interpreter::SwitchToPoly(TaskData *taskData)
/* (Re)-enter the Poly code from C. */
{
    register PolyWord *sp; /* Stack pointer. */
    register byte *pc; /* Program counter. */
    register PolyWord *sl; /* Stack limit register. */
    register int  li;  /* Last instruction. */
    POLYUNSIGNED tailCount;
    PolyWord *tailPtr;
    POLYUNSIGNED returnCount;
    POLYUNSIGNED storeWords = 0;
    int instrBytes;

    RESTART: /* Load or reload the registers and run the code. */

    if (taskData->allocPointer <= taskData->allocLimit + storeWords)
    {
        if (taskData->allocPointer < taskData->allocLimit)
            Crash ("Bad length in heap overflow trap");

        // Find some space to allocate in.  Updates taskData->allocPointer and
        // returns a pointer to the newly allocated space (if allocWords != 0)
        PolyWord *space =
            processes->FindAllocationSpace(taskData, storeWords, true);
        if (space != 0) // Undo the allocation just now.  We'll redo it now we have the store.
            taskData->allocPointer += storeWords;
    }
    storeWords = 0;
    sp = taskData->stack->stack()->p_sp; /* Reload these. */
    pc = taskData->stack->stack()->p_pc;
    li = (unsigned)UNTAGGED(taskData->stack->stack()->p_reg[1]);
    sl = (PolyWord*)taskData->stack->stack()+OVERFLOW_STACK_SIZE;
    IntTaskData *itd = (IntTaskData *)taskData->mdTaskData;

    if (li != 256) goto RETRY; /* Re-execute instruction if necessary. */

    for(;;){ /* Each instruction */

//        char buff[100];
        li = *pc++; /* Get next instruction. */

        RETRY:
//      sprintf(buff, "PC=%x, i=%x\n", pc, li);
//      OutputDebugString(buff);
        // Check for stack overflow and interrupts. These can be done less
        // frequently than every instruction.
        // Don't do this if we're raising an exception: we may be raising
        // it because we can't grow the stack.  Once we've processed the
        // exception and produced any exception trace the stack will have been
        // reset to the last handler.
        if (sp < sl && li != INSTR_raise_ex) {
            taskData->stack->stack()->p_sp = sp;
            taskData->stack->stack()->p_pc = pc;
            taskData->stack->stack()->p_reg[1] = TAGGED(li);
            Handle marker = taskData->saveVec.mark();
            CheckAndGrowStack(taskData, sp);
            taskData->saveVec.reset(marker);
            goto RESTART;
        }

        if (itd->interrupt_requested) {
            itd->interrupt_requested = false;
            taskData->stack->stack()->p_sp = sp;
            taskData->stack->stack()->p_pc = pc;
            taskData->stack->stack()->p_reg[1] = TAGGED(li);
            return -1;
        }

        switch(li) {

        case INSTR_enter_int: pc++; /* Skip the argument. */ break;

        case INSTR_jump_false:
            {
                PolyWord u = *sp++; /* Pop argument */
                if (u == True) { pc += 1; break; }
                /* else - false - take the jump */
            }

        case INSTR_jump: pc += *pc + 1; break;

        case INSTR_push_handler: /* Save the old handler value. */
            *(--sp) = PolyWord::FromStackAddr(taskData->stack->stack()->p_hr); /* Push old handler */
            break;

        case INSTR_set_handler_new: /* Set up a handler */
            *(--sp) = PolyWord::FromCodePtr(pc + *pc + 1); /* Address of handler */
            taskData->stack->stack()->p_hr = sp;
            pc += 1;
            break;

        case INSTR_set_handler_old: /* Set up a handler */
            // Legacy version.  The handler pushes an exception id.
            *(--sp) = PolyWord::FromCodePtr(pc + *pc + 1); /* Address of handler */
            taskData->stack->stack()->p_hr = sp-1; /*Point to identifier about to be pushed*/
            pc += 1;
            break;

        case INSTR_del_handler: /* Delete handler retaining the result. */
            {
                PolyWord u = *sp++;
                sp = taskData->stack->stack()->p_hr;
                PolyWord *endStack = taskData->stack->top;
                if (*sp == TAGGED(0)) sp++; // Legacy
                sp++; // Skip handler entry point
                // Restore old handler
                taskData->stack->stack()->p_hr = (*sp).AsStackAddr();
                *sp = u; // Put back the result
                pc += *pc + 1; /* Skip the handler */
                break;
            }

        case INSTR_jump_i_false:
            if (*sp++ == True) { pc += 1; break; }
            /* else - false - take the jump */

        case INSTR_jump_i_u: /* Indirect jump */
            {
                // This is always a forward jump
                pc += *pc + 1;
                pc += arg1 + 2;
                break;
            }

        case INSTR_jump_i: /* Indirect jump */
            {
                // This is only for backwards compatibility
                pc += *pc + 1;
                /* This may jump backwards. */
                int u = arg1;
                if (u > 32767) u -= 65536;
                pc += u + 2;
                break;
            }

        case INSTR_set_handler_new_i: /* Set up a handler */
            {
                byte *u = pc + *pc + 1;
                *(--sp) = /* Address of handler */
                    PolyWord::FromCodePtr(u + u[0] + u[1]*256 + 2);
                taskData->stack->stack()->p_hr = sp;
                pc += 1;
                break;
            }

        case INSTR_set_handler_old_i: /* Set up a handler */
            {
                byte *u = pc + *pc + 1;
                *(--sp) = /* Address of handler */
                    PolyWord::FromCodePtr(u + u[0] + u[1]*256 + 2);
                taskData->stack->stack()->p_hr = sp-1;
                pc += 1;
                break;
            }

        case INSTR_del_handler_i: /* Delete handler retaining the result. */
            {
                PolyWord u = *sp++;
                PolyWord *t;
                sp = taskData->stack->stack()->p_hr;
                PolyWord *endStack = taskData->stack->top;
                while((t = (*sp).AsStackAddr()) < sp || t > endStack) sp++;
                taskData->stack->stack()->p_hr = t;
                *sp = u;
                pc += *pc + 1; /* Skip the handler */
                pc += arg1 + 2;
                break;
            }

        case INSTR_case:
            {
                POLYSIGNED u = UNTAGGED(*sp++); /* Get the value */
                if (u > arg1 || u < 0) pc += (arg1+2)*2; /* Out of range */
                else {
                    pc += 2;
                    pc += /* Index */pc[u*2]+pc[u*2 + 1]*256; }
                break;
            }

        case INSTR_call_sl: /* Static link call */
            {
                /* Get static link value. */
                PolyWord *t = sp+arg2;
                for(int i = 1; i <= arg3; i++) t = (t[-1]).AsStackAddr();
                PolyWord *constAddr = (PolyWord*)(pc+arg1+2); /* Get entry point. */
                *(--sp) = PolyWord::FromCodePtr(pc+6); /* Push return address to point after instruction. */
                *(--sp) = PolyWord::FromStackAddr(t); /* Push static link */
                pc = (*constAddr).AsCodePtr();
                break;
            }

        case INSTR_call_sl_X:
            {
                /* Get static link value. */
                PolyWord *t = sp+arg3;

                for(int u = 1; u <= arg4; u++) t = (t[-1]).AsStackAddr();

                PolyWord *constAddr = (PolyWord*)(pc+arg2+(arg1+4)*sizeof(PolyWord)+4); /* Get entry point. */
                *(--sp) = PolyWord::FromCodePtr(pc+8); /* Push return address to point after instruction. */
                *(--sp) = PolyWord::FromStackAddr(t); /* Push static link */
                pc = (*constAddr).AsCodePtr();
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
           pc = (*sp++).AsCodePtr(); /* Pop the original return address. */
           li = INSTR_call_closure; /* If we have to re-execute. */
           /* And drop through. */

        case INSTR_call_closure: /* Closure call - may be machine code. */
        CALL_CLOSURE:  /* Jumped to from POLY_SYS_callcode */
            {
                PolyWord *t = (*sp).AsStackAddr(); /* Closure */
                PolyWord u = *t;   /* Get code address. (1st word of closure) */

                if (IS_INT(u)) { /* Closure address is io vector */
                    sp++; /* Remove closure. */
                    POLYSIGNED uu = UNTAGGED(u);
                    switch(uu) {

                    case POLY_SYS_callcode_tupled:
                        {
                            t = (*sp++).AsStackAddr();
                            PolyWord v = t[1]; /* Arguments. */
                            if (v != Zero) /* No args. */
                            {
                                PolyWord *vv = v.AsStackAddr();
                                POLYUNSIGNED u = v.AsObjPtr()->Length(); /* No. of args. */
                                for (; u > 0; u--) *(--sp) = *(vv++);
                            }
                            *(--sp) = t[0]; /* Push closure. */
                            goto CALL_CLOSURE;
                        }
 
                    case POLY_SYS_int_eq: u = *sp++; *sp = (u == *sp)?True:False; break;

                    case POLY_SYS_int_neq: u = *sp++; *sp = (u != *sp)?True:False; break;

                    case POLY_SYS_word_eq: 
                       u = *sp++;
                       *sp = u == *sp ? True : False;
                       break;

                    case POLY_SYS_word_neq:
                       u = *sp++;
                       *sp = u == *sp ? False : True;
                       break;

                    case POLY_SYS_word_geq:
                        u = *sp++; *sp = ((*sp).AsUnsigned() >= u.AsUnsigned())?True:False; break;

                    case POLY_SYS_word_leq:
                        u = *sp++; *sp = ((*sp).AsUnsigned() <= u.AsUnsigned())?True:False; break;

                    case POLY_SYS_word_gtr:
                        u = *sp++; *sp = ((*sp).AsUnsigned() > u.AsUnsigned())?True:False; break;

                    case POLY_SYS_word_lss:
                        u = *sp++; *sp = ((*sp).AsUnsigned() < u.AsUnsigned())?True:False; break;

                    case POLY_SYS_or_word:
                        u = *sp++; *sp = TAGGED(UNTAGGED(*sp) | UNTAGGED(u)); break; 

                    case POLY_SYS_and_word: 
                        u = *sp++; *sp = TAGGED(UNTAGGED(*sp) & UNTAGGED(u)); break; 

                    case POLY_SYS_not_bool:  *sp = (*sp == True) ? False : True; break; 

                    case POLY_SYS_string_length: 
                        /* Length is first word of string unless it is a
                           single character. */
                        if (IS_INT(*sp)) *sp = TAGGED(1);
                        else *sp = TAGGED(((PolyStringObject*)(*sp).AsObjPtr())->length);
                        break; 

                    case POLY_SYS_set_string_length: 
                        {
                            /* Store the length word of a string. */
                            POLYUNSIGNED len = UNTAGGED(*sp++);
                            ((PolyStringObject*)(*sp).AsObjPtr())->length = len;
                            *sp = Zero;
                            break; 
                        }

                    case POLY_SYS_mul_word:
                        u = *sp++; *sp = TAGGED(UNTAGGED_UNSIGNED(*sp) * UNTAGGED_UNSIGNED(u)); break;

                    case POLY_SYS_plus_word: 
                        u = *sp++; *sp = TAGGED(UNTAGGED_UNSIGNED(*sp) + UNTAGGED_UNSIGNED(u)); break;

                    case POLY_SYS_minus_word: 
                        u = *sp++; *sp = TAGGED(UNTAGGED_UNSIGNED(*sp) - UNTAGGED_UNSIGNED(u)); break; 

                    case POLY_SYS_div_word:
                        {
                            POLYUNSIGNED u = UNTAGGED_UNSIGNED(*sp++);
                            if (u == 0)
                                raise_exception0(taskData, EXC_divide);
                            *sp = TAGGED(UNTAGGED_UNSIGNED(*sp) / u); break;
                        }

                    case POLY_SYS_mod_word:
                        {
                            POLYUNSIGNED u = UNTAGGED_UNSIGNED(*sp++);
                            if (u == 0)
                                raise_exception0(taskData, EXC_divide);
                            // It's essential to use UNTAGGED_UNSIGNED here.
                            // The old version used UNTAGGED which uses an arithmetic shift
                            // and produces the wrong answer.
                            *sp = TAGGED(UNTAGGED_UNSIGNED(*sp) % u);
                            break;
                        }

                    case POLY_SYS_xor_word:
                        {
                            PolyWord u = *sp++;
                            *sp = TAGGED(UNTAGGED_UNSIGNED(*sp) ^ UNTAGGED_UNSIGNED(u));
                            break;
                        }

                    case POLY_SYS_shift_left_word: 
                        {
                            PolyWord u = *sp++;
                            if (UNTAGGED_UNSIGNED(u) > sizeof(PolyWord)*8)
                                *sp = Zero;
                            else
                                *sp = TAGGED(UNTAGGED_UNSIGNED(*sp) << UNTAGGED_UNSIGNED(u));
                            break;
                        }

                    case POLY_SYS_shift_right_word:
                        {
                            PolyWord u = *sp++;
                            if (UNTAGGED_UNSIGNED(u) > sizeof(PolyWord)*8)
                                *sp = Zero;
                            else
                                *sp = TAGGED(UNTAGGED_UNSIGNED(*sp) >> UNTAGGED_UNSIGNED(u));
                            break;
                        }

                    case POLY_SYS_shift_right_arith_word:
                        {
                            PolyWord u = *sp++;
                            if (UNTAGGED_UNSIGNED(u) > sizeof(PolyWord)*8)
                            {
                                if (UNTAGGED(*sp) < 0)
                                    *sp = TAGGED(-1);
                                else *sp = Zero;
                            }
                            else
                            // Strictly speaking, C does not require that this uses
                            // arithmetic shifting so we really ought to set the
                            // high-order bits explicitly.
                                *sp = TAGGED(UNTAGGED(*sp) >> UNTAGGED(u));
                            break;
                        }

                    case POLY_SYS_load_byte:
                    case POLY_SYS_load_byte_immut:
                        {
                            POLYUNSIGNED u = UNTAGGED(*sp++);
                            *sp = TAGGED((*sp).AsCodePtr()[u]);
                            break; 
                        }

                    case POLY_SYS_load_word:
                    case POLY_SYS_load_word_immut:
                        {
                            POLYUNSIGNED u = UNTAGGED(*sp++);
                            *sp = (*sp).AsObjPtr()->Get(u);
                            break;
                        }

                    case POLY_SYS_assign_byte: 
                        {
                            POLYUNSIGNED t = UNTAGGED(*sp++);
                            POLYUNSIGNED u = UNTAGGED(*sp++);
                            (*sp).AsCodePtr()[u] = (byte)t;
                            *sp = Zero;
                            break; 
                        }

                    case POLY_SYS_assign_word: 
                        {
                            PolyWord t = *sp++;
                            POLYUNSIGNED u = UNTAGGED(*sp++);
                            (*sp).AsStackAddr()[u] = t;
                            *sp = Zero;
                            break;
                        }

                    case POLY_SYS_lockseg:
                        {
                            PolyObject *obj = (*sp).AsObjPtr();
                            POLYUNSIGNED lengthW = obj->LengthWord();
                            /* Clear the mutable bit. */
                            obj->SetLengthWord(lengthW & ~_OBJ_MUTABLE_BIT);
                            *sp = Zero;
                            break;
                        }

                    case POLY_SYS_get_length: 
                        /* Return the length word. */
                        *sp = TAGGED((*sp).AsObjPtr()->Length());
                        break;

                    case POLY_SYS_is_short:
                        *sp = IS_INT(*sp) ? True : False; break;

                    case POLY_SYS_io_operation:
                        *sp = (PolyObject*)IoEntry((unsigned)UNTAGGED(*sp));
                        break;

                    case POLY_SYS_exception_trace:
                        u = *sp; /* Function to call. */
                        *(--sp) = PolyWord::FromCodePtr(pc); /* Push a return address. */
                        *(--sp) = PolyWord::FromStackAddr(taskData->stack->stack()->p_hr); /* Push old handler */
                        *(--sp) = SPECIAL_PC_TRACE_EX; /* Marks exception trace. */
                        *(--sp) = Zero; /* Catch everything. */
                        taskData->stack->stack()->p_hr = sp; /* Handler is here. */
                        pc = (SPECIAL_PC_TRACE_EX).AsCodePtr(); /* Special return address. */
                        *(--sp) = Zero; /* Unit argument to the function. */
                        *(--sp) = u; /* Push the procedure. */
                        goto CALL_CLOSURE;

                    case POLY_SYS_exception_trace_fn:
                        u = *sp; /* Function to call. */
                        *(--sp) = PolyWord::FromCodePtr(pc); /* Push a return address. */
                        *(--sp) = PolyWord::FromStackAddr(taskData->stack->stack()->p_hr); /* Push old handler */
                        *(--sp) = SPECIAL_PC_TRACE_EX_FN; /* Marks exception trace. */
                        *(--sp) = Zero; /* Catch everything. */
                        taskData->stack->stack()->p_hr = sp; /* Handler is here. */
                        pc = (SPECIAL_PC_TRACE_EX).AsCodePtr(); /* Special return address. */
                        *(--sp) = Zero; /* Unit argument to the function. */
                        *(--sp) = u; /* Push the procedure. */
                        goto CALL_CLOSURE;

                    case POLY_SYS_is_big_endian: {
                        union { unsigned long wrd; char chrs[sizeof(unsigned long)]; } endian;
                        endian.wrd = 1;
                        *(--sp) = endian.chrs[0] == 0 ? True : False;
                        break;
                    }

                    case POLY_SYS_bytes_per_word:
                        *(--sp) = TAGGED(sizeof(PolyWord)); break;
 
                    case POLY_SYS_raisex:
                        goto RAISE_EXCEPTION;

                    case POLY_SYS_aplus:
                        {
                            PolyWord x = sp[0];
                            PolyWord y = sp[1];
                            // If they're both short and no overflow.
                            if (IS_INT(x) && IS_INT(y))
                            {
                                POLYSIGNED t = UNTAGGED(x) + UNTAGGED(y);
                                if (t <= MAXTAGGED && t >= -MAXTAGGED-1)
                                {
                                    sp++;
                                    *sp = TAGGED(t);
                                    break;
                                }
                                else goto FullRTSCall;
                            }
                            else goto FullRTSCall;
                        }

                    case POLY_SYS_aminus:
                        {
                            PolyWord x = sp[0];
                            PolyWord y = sp[1];
                            // If they're both short and no overflow.
                            if (IS_INT(x) && IS_INT(y))
                            {
                                POLYSIGNED t = UNTAGGED(y) - UNTAGGED(x);
                                if (t <= MAXTAGGED && t >= -MAXTAGGED-1)
                                {
                                    sp++;
                                    *sp = TAGGED(t);
                                    break;
                                }
                                else goto FullRTSCall;
                            }
                            else goto FullRTSCall;
                        }

                    case POLY_SYS_equala:
                        {
                            PolyWord x = sp[0];
                            PolyWord y = sp[1];
                            // If either argument is short the values are only equal
                            // if the words contain the same bit pattern.
                            if (IS_INT(x) || IS_INT(y))
                            {
                                sp++;
                                *sp = y == x ? True : False;
                                break;
                            }
                            else goto FullRTSCall;
                        }

                    case POLY_SYS_int_geq:
                        {
                            PolyWord x = sp[0];
                            PolyWord y = sp[1];
                            if (IS_INT(x) && IS_INT(y))
                            {
                                sp++;
                                *sp = UNTAGGED(y) >= UNTAGGED(x) ? True : False;
                                break;
                            }
                            else goto FullRTSCall;
                        }

                    case POLY_SYS_int_leq:
                        {
                            PolyWord x = sp[0];
                            PolyWord y = sp[1];
                            if (IS_INT(x) && IS_INT(y))
                            {
                                sp++;
                                *sp = UNTAGGED(y) <= UNTAGGED(x) ? True : False;
                                break;
                            }
                            else goto FullRTSCall;
                        }

                    case POLY_SYS_int_gtr:
                        {
                            PolyWord x = sp[0];
                            PolyWord y = sp[1];
                            if (IS_INT(x) && IS_INT(y))
                            {
                                sp++;
                                *sp = UNTAGGED(y) > UNTAGGED(x) ? True : False;
                                break;
                            }
                            else goto FullRTSCall;
                        }

                    case POLY_SYS_int_lss:
                        {
                            PolyWord x = sp[0];
                            PolyWord y = sp[1];
                            if (IS_INT(x) && IS_INT(y))
                            {
                                sp++;
                                *sp = UNTAGGED(y) < UNTAGGED(x) ? True : False;
                                break;
                            }
                            else goto FullRTSCall;
                        }

                    default:
                    FullRTSCall:
                        // For all the calls that aren't built in ...
                        /* Save the state so that the instruction can be retried if necessary. */
                        taskData->stack->stack()->p_pc = pc; /* Pc value after instruction. */
                        taskData->stack->stack()->p_reg[1] = TAGGED(li); /* Previous instruction. */
                        taskData->stack->stack()->p_sp = sp-1; /* Include the closure address. */
                        return (int)uu;
                    }
                } /* End of system calls. */
                else {
                    sp--;
                    *sp = sp[1];      /* Move closure up. */
                    sp[1] = PolyWord::FromCodePtr(pc); /* Save return address. */
                    pc = u.AsCodePtr();    /* Get entry point. */
                }
            }
            break;

        case INSTR_return_w:
            returnCount = arg1; /* Get no. of args to remove. */

            RETURN: /* Common code for return. */
            {
                PolyWord result = *sp++; /* Result */
                sp++; /* Remove the link/closure */
                pc = (*sp++).AsCodePtr(); /* Return address */
                sp += returnCount; /* Add on number of args. */
                if (pc == (SPECIAL_PC_END_THREAD).AsCodePtr())
                    exitThread(taskData); // This thread is exiting.
                else if (pc == (SPECIAL_PC_TRACE_EX).AsCodePtr())
                {
                    /* Return from a call to exception_trace when an exception
                       has not been raised. */
                    sp += 1;
                    taskData->stack->stack()->p_hr = (sp[1]).AsStackAddr();
                    *sp = result;
                    returnCount = 1;
                    goto RETURN;
                }
                *(--sp) = result; /* Result */
            }
            break;

        case INSTR_return_b: returnCount = *pc; goto RETURN;
        case INSTR_return_0: returnCount = 0; goto RETURN;
        case INSTR_return_1: returnCount = 1; goto RETURN;
        case INSTR_return_2: returnCount = 2; goto RETURN;
        case INSTR_return_3: returnCount = 3; goto RETURN;

        case INSTR_pad: /* No-op */ break;

        case INSTR_raise_ex:
            {
            RAISE_EXCEPTION:
                PolyException *exn = (PolyException*)((*sp).AsObjPtr());
                taskData->stack->stack()->p_reg[0] = exn; /* Get exception data */
                PolyWord exId = exn->ex_id; /* Get exception identifier. */
                taskData->stack->stack()->p_sp = sp; /* Save this in case of trace. */
                PolyWord *t = taskData->stack->stack()->p_hr;  /* First handler */
                PolyWord *endStack = taskData->stack->top;
                // The legacy version pushes an identifier which is always zero.
                if (*t == Zero) t++;
                if (*t == SPECIAL_PC_TRACE_EX)
                { /* Trace this exception. */ 
                    *sp = PolyWord::FromCodePtr(pc); /* So that this proc. will be included. */
                    t++; /* Next handler. */
                    PolyWord *nextHandler = t;
                    Handle marker = taskData->saveVec.mark();
                    try {
                        ex_tracec(taskData, taskData->saveVec.push(taskData->stack->stack()->p_reg[0]),
                                  taskData->saveVec.push(PolyWord::FromStackAddr(nextHandler)));
                    }
                    catch (IOException) {
                    }
                    taskData->saveVec.reset(marker);
                    // This will have reraised the exception by calling SetException
                    goto RESTART;
                }
                else if (*t == SPECIAL_PC_TRACE_EX_FN)
                {
                    // New exception trace
                    *sp = PolyWord::FromCodePtr(pc); /* So that this function will be included. */
                    // exceptionToTraceException expects the new format of exception handling that is
                    // used in the X86 code-generator.  That does not push an exception id.
                    taskData->stack->stack()->p_hr = t;
                    Handle marker = taskData->saveVec.mark();
                    try {
                        exceptionToTraceException(taskData,
                            taskData->saveVec.push(taskData->stack->stack()->p_reg[0]));
                    }
                    catch (IOException) {
                    }
                    taskData->saveVec.reset(marker);
                    // This will have reraised the exception by calling SetException
                    goto RESTART;
                }
                else if (*t == SPECIAL_PC_END_THREAD)
                    exitThread(taskData);  // Default handler for thread.
                taskData->stack->stack()->p_pc = (*t).AsCodePtr();
                /* Now remove this handler. */
                sp = t;
                while ((t = (*sp).AsStackAddr()) < sp || t > endStack)
                    sp++;
                taskData->stack->stack()->p_hr = t; /* Restore old handler */
                sp++; /* Remove that entry. */
                taskData->stack->stack()->p_sp = sp;
                taskData->stack->stack()->p_reg[1] = TAGGED(256); /* Get the next instruction. */
                goto RESTART; /* Restart in case pc is persistent (??? Still relevant????). */
            }

        case INSTR_get_store_w:
            {
                storeWords = arg1+1;
                instrBytes = 2; // Number of bytes of arg of instruction

                GET_STORE: /* Common code for allocation. */
                taskData->allocPointer -= storeWords;
                if (taskData->allocPointer < taskData->allocLimit)
                {
                    taskData->allocPointer += storeWords;
                    taskData->stack->stack()->p_sp = sp;
                    taskData->stack->stack()->p_pc = pc;
                    taskData->stack->stack()->p_reg[1] = TAGGED(li);
                    goto RESTART;
                }
                pc += instrBytes;
                storeWords--; // Remove the length word from the count
                *taskData->allocPointer = PolyWord::FromUnsigned(storeWords | _OBJ_MUTABLE_BIT); /* Allocation must be mutable! */
                PolyWord *t = taskData->allocPointer+1;
                for(; storeWords > 0; ) t[--storeWords] = PolyWord::FromUnsigned(0); /* Must initialise store! */
                *(--sp) = PolyWord::FromStackAddr(t);
                break;
                }

        case INSTR_get_store_2: storeWords = 2+1; instrBytes = 0; goto GET_STORE;
        case INSTR_get_store_3: storeWords = 3+1; instrBytes = 0; goto GET_STORE;
        case INSTR_get_store_4: storeWords = 4+1; instrBytes = 0; goto GET_STORE;
        case INSTR_get_store_b: storeWords = *pc+1; instrBytes = 1; goto GET_STORE;


        case INSTR_tuple_w:
            {
                storeWords = arg1+1; instrBytes = 2;

                TUPLE: /* Common code for tupling. */
                taskData->allocPointer -= storeWords;
                if (taskData->allocPointer < taskData->allocLimit) {
                    taskData->allocPointer += storeWords;
                    taskData->stack->stack()->p_sp = sp;
                    taskData->stack->stack()->p_pc = pc;
                    taskData->stack->stack()->p_reg[1] = TAGGED(li);
                    goto RESTART;
                }
                pc += instrBytes;
                storeWords--; // Remove the length word from the count
                *taskData->allocPointer = PolyWord::FromUnsigned(storeWords);
                PolyWord *t = taskData->allocPointer+1;
                for(; storeWords > 0; ) t[--storeWords] = *sp++;
                *(--sp) = (PolyObject*)t;
                break;
            }

        case INSTR_tuple_2: storeWords = 2+1; instrBytes = 0; goto TUPLE;
        case INSTR_tuple_3: storeWords = 3+1; instrBytes = 0; goto TUPLE;
        case INSTR_tuple_4: storeWords = 4+1; instrBytes = 0; goto TUPLE;
        case INSTR_tuple_b: storeWords = *pc+1; instrBytes = 1; goto TUPLE;


        case INSTR_non_local:
            {
                PolyWord *t = sp+arg1;
                POLYSIGNED uu;
                for(uu = 1; uu <= arg2; uu++) t = (t[-1]).AsStackAddr();
                uu = arg3; /* Can be negative. */
                if (uu > 32767) uu -= 65536;
                *(--sp) = t[uu];
                pc += 6;
                break;
            }

        case INSTR_local_w:
            {
                PolyWord u = sp[arg1];
                *(--sp) = u;
                pc += 2;
                break;
            }

        case INSTR_indirect_w:
            *sp = (*sp).AsObjPtr()->Get(arg1); pc += 2; break;

        case INSTR_move_to_vec_w:
            {
                PolyWord u = *sp++;
                (*sp).AsObjPtr()->Set(arg1, u);
                pc += 2;
                break;
            }

        case INSTR_set_stack_val_w:
            {
                PolyWord u = *sp++;
                sp[arg1-1] = u;
                pc += 2;
                break;
            }

        case INSTR_reset_w: sp += arg1; pc += 2; break;

        case INSTR_reset_r_w:
            {
                PolyWord u = *sp;
                sp += arg1;
                *sp = u;
                pc += 2;
                break;
            }

        case INSTR_const_addr:
            *(--sp) = *(PolyWord*)(pc + arg1 + 2); pc += 2; break;

        case INSTR_const_addr_Xb:
            *(--sp) = (PolyWord::FromCodePtr(pc + (pc[0]+4)*sizeof(PolyWord) + pc[1] + pc[2]*256 + 3)).AsObjPtr()->Get(0);
            pc += 3;
            break;

        case INSTR_const_addr_Xw:
            *(--sp) = (PolyWord::FromCodePtr(pc + (arg1+4)*sizeof(PolyWord)+arg2 + 4)).AsObjPtr()->Get(0);
            pc += 4;
            break;

        case INSTR_const_int_w: *(--sp) = TAGGED(arg1); pc += 2; break;

        case INSTR_io_vec_entry:
            *(--sp) = (PolyObject*)IoEntry(*pc);
            pc += 1;
            break;

        case INSTR_const_nil: *(--sp) = Zero; break;

        case INSTR_jump_back: pc -= *pc + 1; break;

        case INSTR_jump_back16:
            pc -= arg1 + 1; break;

        case INSTR_lock:
            {
                PolyObject *obj = (*sp).AsObjPtr();
                obj->SetLengthWord(obj->LengthWord() & ~_OBJ_MUTABLE_BIT);
                break;
            }

        case INSTR_ldexc: *(--sp) = taskData->stack->stack()->p_reg[0]; break;

        case INSTR_local_b: { PolyWord u = sp[*pc]; *(--sp) = u; pc += 1; break; }

        case INSTR_indirect_b:
            *sp = (*sp).AsObjPtr()->Get(*pc); pc += 1; break;

        case INSTR_move_to_vec_b:
            { PolyWord u = *sp++; (*sp).AsObjPtr()->Set(*pc, u); pc += 1; break; }

        case INSTR_set_stack_val_b:
            { PolyWord u = *sp++; sp[*pc-1] = u; pc += 1; break; }

        case INSTR_reset_b: sp += *pc; pc += 1; break;

        case INSTR_reset_r_b:
            { PolyWord u = *sp; sp += *pc; *sp = u; pc += 1; break; }

        case INSTR_const_int_b: *(--sp) = TAGGED(*pc); pc += 1; break;

        case INSTR_local_0: { PolyWord u = sp[0]; *(--sp) = u; break; }
        case INSTR_local_1: { PolyWord u = sp[1]; *(--sp) = u; break; }
        case INSTR_local_2: { PolyWord u = sp[2]; *(--sp) = u; break; }
        case INSTR_local_3: { PolyWord u = sp[3]; *(--sp) = u; break; }
        case INSTR_local_4: { PolyWord u = sp[4]; *(--sp) = u; break; }
        case INSTR_local_5: { PolyWord u = sp[5]; *(--sp) = u; break; }
        case INSTR_local_6: { PolyWord u = sp[6]; *(--sp) = u; break; }
        case INSTR_local_7: { PolyWord u = sp[7]; *(--sp) = u; break; }
        case INSTR_local_8: { PolyWord u = sp[8]; *(--sp) = u; break; }
        case INSTR_local_9: { PolyWord u = sp[9]; *(--sp) = u; break; }
        case INSTR_local_10: { PolyWord u = sp[10]; *(--sp) = u; break; }
        case INSTR_local_11: { PolyWord u = sp[11]; *(--sp) = u; break; }

        case INSTR_indirect_0:
            if ((*sp) == PolyWord::FromStackAddr(IoEntry(55)))
                *sp = TAGGED(401); // We still seem to have some of the old AHL version number references.
            else *sp = (*sp).AsObjPtr()->Get(0); break;

        case INSTR_indirect_1:
            *sp = (*sp).AsObjPtr()->Get(1); break;

        case INSTR_indirect_2:
            *sp = (*sp).AsObjPtr()->Get(2); break;

        case INSTR_indirect_3:
            *sp = (*sp).AsObjPtr()->Get(3); break;

        case INSTR_indirect_4:
            *sp = (*sp).AsObjPtr()->Get(4); break;

        case INSTR_indirect_5:
            *sp = (*sp).AsObjPtr()->Get(5); break;

        case INSTR_const_0: *(--sp) = Zero; break;
        case INSTR_const_1: *(--sp) = TAGGED(1); break;
        case INSTR_const_2: *(--sp) = TAGGED(2); break;
        case INSTR_const_3: *(--sp) = TAGGED(3); break;
        case INSTR_const_4: *(--sp) = TAGGED(4); break;
        case INSTR_const_10: *(--sp) = TAGGED(10); break;


        /* move_to_vec only occurs to newly allocated store so there is
           no problem with persistent store faults. */
        case INSTR_move_to_vec_0:  { PolyWord u = *sp++; (*sp).AsObjPtr()->Set(0, u); break; }
        case INSTR_move_to_vec_1: { PolyWord u = *sp++; (*sp).AsObjPtr()->Set(1, u); break; }
        case INSTR_move_to_vec_2: { PolyWord u = *sp++; (*sp).AsObjPtr()->Set(2, u); break; }
        case INSTR_move_to_vec_3: { PolyWord u = *sp++; (*sp).AsObjPtr()->Set(3, u); break; }
        case INSTR_move_to_vec_4: { PolyWord u = *sp++; (*sp).AsObjPtr()->Set(4, u); break; }
        case INSTR_move_to_vec_5: { PolyWord u = *sp++; (*sp).AsObjPtr()->Set(5, u); break; }
        case INSTR_move_to_vec_6: { PolyWord u = *sp++; (*sp).AsObjPtr()->Set(6, u); break; }
        case INSTR_move_to_vec_7: { PolyWord u = *sp++; (*sp).AsObjPtr()->Set(7, u); break; }

        case INSTR_reset_r_1: { PolyWord u = *sp; sp += 1; *sp = u; break; }
        case INSTR_reset_r_2: { PolyWord u = *sp; sp += 2; *sp = u; break; }
        case INSTR_reset_r_3: { PolyWord u = *sp; sp += 3; *sp = u; break; }

        case INSTR_reset_1: sp += 1; break;
        case INSTR_reset_2: sp += 2; break;


        case INSTR_non_local_l_1:
            {
                POLYSIGNED uu = *pc;
                PolyWord u = (sp[uu >> 4]).AsStackAddr()[(uu & 0xf) - 6];
                *(--sp) = u;
                pc += 1;
                break;
            }

        case INSTR_non_local_l_2:
            {
                POLYSIGNED uu = *pc;
                PolyWord *t = sp[uu >> 4].AsStackAddr() -1;
                *(--sp) = (*t).AsStackAddr()[(uu & 0xf) - 6];
                pc += 1;
                break;
            }

        case INSTR_non_local_l_3:
            {
                POLYSIGNED uu = *pc;
                PolyWord *t = sp[uu >> 4].AsStackAddr() -1;
                t = (*t).AsStackAddr() - 1;
                *(--sp) = (*t).AsStackAddr()[(uu & 0xf) - 6];
                pc += 1; break;
            }

        case INSTR_call_sl_c: /* Static link call */
            {
                /* Get static link value. */
                POLYSIGNED uu = pc[2];
                PolyWord *t = sp + (uu >> 4) + 2;
                for(uu = uu & 0xf; uu > 0; uu--) t = t[-1].AsStackAddr();
                PolyWord u = PolyWord::FromCodePtr(pc+arg1+2); /* Get entry point. */
                *(--sp) = PolyWord::FromCodePtr(pc+3); /* Push return address to point after instruction. */
                *(--sp) = PolyWord::FromStackAddr(t); /* Push static link */
                pc = u.AsObjPtr()->Get(0).AsCodePtr();
                break;
            }

        case INSTR_call_sl_cX:
            {
                /* Get static link value. */
                POLYUNSIGNED uu = pc[3];
                PolyWord *t = sp + (uu >> 4) + 2;
                for(uu = uu & 0xf; uu > 0; uu--) t = t[-1].AsStackAddr();
                // This splits the offset into a number of words and a number of bytes
                // That's needed to try to make the code portable between 32 and 64 bit machines.
                PolyWord u = PolyWord::FromCodePtr(pc + (pc[0]+4)*sizeof(PolyWord) + pc[1] + pc[2]*256+3); /* Get entry point. */
                *(--sp) = PolyWord::FromCodePtr(pc+4); /* Push return address to point after instruction. */
                *(--sp) = PolyWord::FromStackAddr(t); /* Push static link */
                pc = u.AsObjPtr()->Get(0).AsCodePtr();
                break;
                }

        case INSTR_io_vec_229: *(--sp) = (PolyObject*)IoEntry(POLY_SYS_int_eq); break;
        case INSTR_io_vec_233: *(--sp) = (PolyObject*)IoEntry(POLY_SYS_int_gtr); break;
        case INSTR_io_vec_236: *(--sp) = (PolyObject*)IoEntry(POLY_SYS_or_word); break;
        case INSTR_io_vec_251: *(--sp) = (PolyObject*)IoEntry(POLY_SYS_word_eq); break;
        case INSTR_io_vec_253: *(--sp) = (PolyObject*)IoEntry(POLY_SYS_load_word); break;
        case INSTR_io_vec_255: *(--sp) = (PolyObject*)IoEntry(POLY_SYS_assign_word); break;

        case INSTR_integer_equal:
            { PolyWord u = *sp++; *sp = (u == *sp)?True:False; break; }

        case INSTR_integer_leq:
            {
                POLYSIGNED uu = UNTAGGED(*sp++);
                *sp = (UNTAGGED(*sp) <= uu)?True:False;
                break;
            }

        case INSTR_integer_greater:
            {
                POLYSIGNED uu = UNTAGGED(*sp++);
                *sp = (UNTAGGED(*sp) > uu)?True:False; break; 
            }

        case INSTR_boolean_or:
            {
                PolyWord u = *sp++; if (u == True) *sp = True; break; 
            }

        case INSTR_word_equal:
            {
                PolyWord u = *sp++;
                if (u == *sp) *sp = True;
                else *sp = u == *sp ? True : False;
                break;
            }

        case INSTR_assign_word:
            {
                PolyWord u = *sp++;
                POLYUNSIGNED uu = UNTAGGED(*sp++);
                (*sp).AsObjPtr()->Set(uu, u);
                *sp = Zero;
                break;
            }

        case INSTR_container: /* Create a container. */
            {
                /* This is supposed to be on the stack but that causes problems in gencde
                   so we create a mutable segment on the heap. */
                storeWords = arg1+1;
                taskData->allocPointer -= storeWords;
                if (taskData->allocPointer < taskData->allocLimit) {
                    taskData->allocPointer += storeWords;
                    taskData->stack->stack()->p_sp = sp;
                    taskData->stack->stack()->p_pc = pc;
                    taskData->stack->stack()->p_reg[1] = TAGGED(li);
                    goto RESTART;
                }
                storeWords--;
                PolyObject *t = (PolyObject*)(taskData->allocPointer+1);
                t->SetLengthWord(storeWords, F_MUTABLE_BIT);
                for(; storeWords > 0; ) t->Set(--storeWords, Zero);
                *(--sp) = t; /* Push the address of the container. */
                pc += 2;
                break;
            }

        case INSTR_set_container: /* Copy a tuple into a container. */
            {
                PolyWord u = *sp++; /* Pop the source tuple address. */
                for (POLYSIGNED uu = arg1; uu > 0; )
                {
                    uu--;
                    (*sp).AsObjPtr()->Set(uu, u.AsObjPtr()->Get(uu)); /* Copy the items. */
                }
                sp++;
                pc += 2;
                break;
            }

        case INSTR_tuple_container: /* Create a tuple from a container. */
            {
                storeWords = arg1+1;
                taskData->allocPointer -= storeWords;
                if (taskData->allocPointer < taskData->allocLimit) {
                    taskData->allocPointer += storeWords;
                    taskData->stack->stack()->p_sp = sp;
                    taskData->stack->stack()->p_pc = pc;
                    taskData->stack->stack()->p_reg[1] = TAGGED(li);
                    goto RESTART;
                }
                storeWords--;
                PolyObject *t = (PolyObject *)(taskData->allocPointer+1);
                t->SetLengthWord(storeWords, 0);
                for(; storeWords > 0; )
                {
                    storeWords--;
                    t->Set(storeWords, (*sp).AsObjPtr()->Get(storeWords));
                }
                *sp = t;
                pc += 2;
                break;
            }

        default: Crash("Unknown instruction %x\n", li);

        } /* switch */
     } /* for */
     return 0;
} /* MD_switch_to_poly */


void Interpreter::CallIO0(TaskData *taskData, Handle(*ioFun)(TaskData *))
{
    try {
        Handle result = (*ioFun)(taskData);
        *(taskData->stack->stack()->p_sp) = result->Word();
        taskData->stack->stack()->p_reg[1] = TAGGED(256); /* Take next instruction. */
    }
    catch (IOException) {
    }
}

void Interpreter::CallIO1(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle))
{
    Handle funarg = taskData->saveVec.push(taskData->stack->stack()->p_sp[1]);
    try {
        Handle result = (*ioFun)(taskData, funarg);
        *(++taskData->stack->stack()->p_sp) = result->Word();
        taskData->stack->stack()->p_reg[1] = TAGGED(256); /* Take next instruction. */
    }
    catch (IOException) {
    }
}

void Interpreter::CallIO2(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle))
{
    Handle funarg1 = taskData->saveVec.push(taskData->stack->stack()->p_sp[1]);
    Handle funarg2 = taskData->saveVec.push(taskData->stack->stack()->p_sp[2]);
    try {
        Handle result = (*ioFun)(taskData, funarg1, funarg2);
        taskData->stack->stack()->p_sp += 2;
        *(taskData->stack->stack()->p_sp) = DEREFWORD(result);
        taskData->stack->stack()->p_reg[1] = TAGGED(256); /* Take next instruction. */
    }
    catch (IOException) {
    }
}

void Interpreter::CallIO3(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle))
{
    Handle funarg1 = taskData->saveVec.push(taskData->stack->stack()->p_sp[1]);
    Handle funarg2 = taskData->saveVec.push(taskData->stack->stack()->p_sp[2]);
    Handle funarg3 = taskData->saveVec.push(taskData->stack->stack()->p_sp[3]);
    try {
        Handle result = (*ioFun)(taskData, funarg1, funarg2, funarg3);
        taskData->stack->stack()->p_sp += 3;
        *(taskData->stack->stack()->p_sp) = DEREFWORD(result);
        taskData->stack->stack()->p_reg[1] = TAGGED(256); /* Take next instruction. */
    }
    catch (IOException) {
    }
}

void Interpreter::CallIO4(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle, Handle))
{
    Handle funarg1 = taskData->saveVec.push(taskData->stack->stack()->p_sp[1]);
    Handle funarg2 = taskData->saveVec.push(taskData->stack->stack()->p_sp[2]);
    Handle funarg3 = taskData->saveVec.push(taskData->stack->stack()->p_sp[3]);
    Handle funarg4 = taskData->saveVec.push(taskData->stack->stack()->p_sp[4]);
    try {
        Handle result = (*ioFun)(taskData, funarg1, funarg2, funarg3, funarg4);
        taskData->stack->stack()->p_sp += 4;
        *(taskData->stack->stack()->p_sp) = DEREFWORD(result);
        taskData->stack->stack()->p_reg[1] = TAGGED(256); /* Take next instruction. */
    }
    catch (IOException) {
    }
}

void Interpreter::CallIO5(TaskData *taskData, Handle(*ioFun)(TaskData *, Handle, Handle, Handle, Handle, Handle))
{
    Handle funarg1 = taskData->saveVec.push(taskData->stack->stack()->p_sp[1]);
    Handle funarg2 = taskData->saveVec.push(taskData->stack->stack()->p_sp[2]);
    Handle funarg3 = taskData->saveVec.push(taskData->stack->stack()->p_sp[3]);
    Handle funarg4 = taskData->saveVec.push(taskData->stack->stack()->p_sp[4]);
    Handle funarg5 = taskData->saveVec.push(taskData->stack->stack()->p_sp[5]);
    try {
        Handle result = (*ioFun)(taskData, funarg1, funarg2, funarg3, funarg4, funarg5);
        taskData->stack->stack()->p_sp += 5;
        *(taskData->stack->stack()->p_sp) = DEREFWORD(result);
        taskData->stack->stack()->p_reg[1] = TAGGED(256); /* Take next instruction. */
    }
    catch (IOException) {
    }
}

// Return the callback result.  The current ML process (thread) terminates.
Handle Interpreter::CallBackResult(TaskData *taskData)
{
    return taskData->saveVec.push(taskData->stack->stack()->p_sp[1]);
}

void Interpreter::InitInterfaceVector(void)
{
    add_word_to_io_area(POLY_SYS_exit, TAGGED(POLY_SYS_exit));
    add_word_to_io_area(POLY_SYS_alloc_store, TAGGED(POLY_SYS_alloc_store));
    add_word_to_io_area(POLY_SYS_alloc_uninit, TAGGED(POLY_SYS_alloc_uninit));
    add_word_to_io_area(POLY_SYS_raisex, TAGGED(POLY_SYS_raisex));
    add_word_to_io_area(POLY_SYS_chdir, TAGGED(POLY_SYS_chdir));
    add_word_to_io_area(POLY_SYS_get_length, TAGGED(POLY_SYS_get_length));
    add_word_to_io_area(POLY_SYS_get_flags, TAGGED(POLY_SYS_get_flags));
    add_word_to_io_area(POLY_SYS_str_compare, TAGGED(POLY_SYS_str_compare));
    add_word_to_io_area(POLY_SYS_teststrgtr, TAGGED(POLY_SYS_teststrgtr));
    add_word_to_io_area(POLY_SYS_teststrlss, TAGGED(POLY_SYS_teststrlss));
    add_word_to_io_area(POLY_SYS_teststrgeq, TAGGED(POLY_SYS_teststrgeq));
    add_word_to_io_area(POLY_SYS_teststrleq, TAGGED(POLY_SYS_teststrleq));
    add_word_to_io_area(POLY_SYS_exception_trace, TAGGED(POLY_SYS_exception_trace));
    add_word_to_io_area(POLY_SYS_exception_trace_fn, TAGGED(POLY_SYS_exception_trace_fn));
    add_word_to_io_area(POLY_SYS_lockseg, TAGGED(POLY_SYS_lockseg));
    add_word_to_io_area(POLY_SYS_profiler, TAGGED(POLY_SYS_profiler));
    add_word_to_io_area(POLY_SYS_is_short, TAGGED(POLY_SYS_is_short));
//    add_word_to_io_area(POLY_SYS_raiseexception, TAGGED(POLY_SYS_raiseexception));
    add_word_to_io_area(POLY_SYS_quotrem, TAGGED(POLY_SYS_quotrem));
    add_word_to_io_area(POLY_SYS_aplus, TAGGED(POLY_SYS_aplus));
    add_word_to_io_area(POLY_SYS_aminus, TAGGED(POLY_SYS_aminus));
    add_word_to_io_area(POLY_SYS_amul, TAGGED(POLY_SYS_amul));
    add_word_to_io_area(POLY_SYS_adiv, TAGGED(POLY_SYS_adiv));
    add_word_to_io_area(POLY_SYS_amod, TAGGED(POLY_SYS_amod));
    add_word_to_io_area(POLY_SYS_aneg, TAGGED(POLY_SYS_aneg));

    add_word_to_io_area(POLY_SYS_xora, TAGGED(POLY_SYS_xora));
    add_word_to_io_area(POLY_SYS_ora, TAGGED(POLY_SYS_ora));
    add_word_to_io_area(POLY_SYS_anda, TAGGED(POLY_SYS_anda));
    add_word_to_io_area(POLY_SYS_Real_str, TAGGED(POLY_SYS_Real_str));
    add_word_to_io_area(POLY_SYS_Real_geq, TAGGED(POLY_SYS_Real_geq));
    add_word_to_io_area(POLY_SYS_Real_leq, TAGGED(POLY_SYS_Real_leq));
    add_word_to_io_area(POLY_SYS_Real_gtr, TAGGED(POLY_SYS_Real_gtr));
    add_word_to_io_area(POLY_SYS_Real_lss, TAGGED(POLY_SYS_Real_lss));
    add_word_to_io_area(POLY_SYS_Real_eq, TAGGED(POLY_SYS_Real_eq));
    add_word_to_io_area(POLY_SYS_Real_neq, TAGGED(POLY_SYS_Real_neq));
    add_word_to_io_area(POLY_SYS_Real_Dispatch, TAGGED(POLY_SYS_Real_Dispatch));

    add_word_to_io_area(POLY_SYS_equala, TAGGED(POLY_SYS_equala));
    add_word_to_io_area(POLY_SYS_Add_real, TAGGED(POLY_SYS_Add_real));
    add_word_to_io_area(POLY_SYS_Sub_real, TAGGED(POLY_SYS_Sub_real));
    add_word_to_io_area(POLY_SYS_Mul_real, TAGGED(POLY_SYS_Mul_real));
    add_word_to_io_area(POLY_SYS_Div_real, TAGGED(POLY_SYS_Div_real));
    add_word_to_io_area(POLY_SYS_Abs_real, TAGGED(POLY_SYS_Abs_real));
    add_word_to_io_area(POLY_SYS_Neg_real, TAGGED(POLY_SYS_Neg_real));
    add_word_to_io_area(POLY_SYS_Repr_real, TAGGED(POLY_SYS_Repr_real));
    add_word_to_io_area(POLY_SYS_conv_real, TAGGED(POLY_SYS_conv_real));
    add_word_to_io_area(POLY_SYS_real_to_int, TAGGED(POLY_SYS_real_to_int));
    add_word_to_io_area(POLY_SYS_int_to_real, TAGGED(POLY_SYS_int_to_real));
    add_word_to_io_area(POLY_SYS_sqrt_real, TAGGED(POLY_SYS_sqrt_real));
    add_word_to_io_area(POLY_SYS_sin_real, TAGGED(POLY_SYS_sin_real));
    add_word_to_io_area(POLY_SYS_cos_real, TAGGED(POLY_SYS_cos_real));
    add_word_to_io_area(POLY_SYS_arctan_real, TAGGED(POLY_SYS_arctan_real));
    add_word_to_io_area(POLY_SYS_exp_real, TAGGED(POLY_SYS_exp_real));
    add_word_to_io_area(POLY_SYS_ln_real, TAGGED(POLY_SYS_ln_real));
    add_word_to_io_area(POLY_SYS_io_operation, TAGGED(POLY_SYS_io_operation));
    add_word_to_io_area(POLY_SYS_atomic_reset, TAGGED(POLY_SYS_atomic_reset));
    add_word_to_io_area(POLY_SYS_atomic_incr, TAGGED(POLY_SYS_atomic_incr));
    add_word_to_io_area(POLY_SYS_atomic_decr, TAGGED(POLY_SYS_atomic_decr));
    add_word_to_io_area(POLY_SYS_thread_self, TAGGED(POLY_SYS_thread_self));
    add_word_to_io_area(POLY_SYS_thread_dispatch, TAGGED(POLY_SYS_thread_dispatch));
    add_word_to_io_area(POLY_SYS_is_big_endian, TAGGED(POLY_SYS_is_big_endian));
    add_word_to_io_area(POLY_SYS_bytes_per_word, TAGGED(POLY_SYS_bytes_per_word));
    add_word_to_io_area(POLY_SYS_offset_address, TAGGED(POLY_SYS_offset_address));
    add_word_to_io_area(POLY_SYS_objsize, TAGGED(POLY_SYS_objsize));
    add_word_to_io_area(POLY_SYS_showsize, TAGGED(POLY_SYS_showsize));
    add_word_to_io_area(POLY_SYS_shift_right_word, TAGGED(POLY_SYS_shift_right_word));
    add_word_to_io_area(POLY_SYS_word_neq, TAGGED(POLY_SYS_word_neq));
    add_word_to_io_area(POLY_SYS_not_bool, TAGGED(POLY_SYS_not_bool));
    add_word_to_io_area(POLY_SYS_string_length, TAGGED(POLY_SYS_string_length));
    add_word_to_io_area(POLY_SYS_int_eq, TAGGED(POLY_SYS_int_eq));
    add_word_to_io_area(POLY_SYS_int_neq, TAGGED(POLY_SYS_int_neq));
    add_word_to_io_area(POLY_SYS_int_geq, TAGGED(POLY_SYS_int_geq));
    add_word_to_io_area(POLY_SYS_int_leq, TAGGED(POLY_SYS_int_leq));
    add_word_to_io_area(POLY_SYS_int_gtr, TAGGED(POLY_SYS_int_gtr));
    add_word_to_io_area(POLY_SYS_int_lss, TAGGED(POLY_SYS_int_lss));
    add_word_to_io_area(POLY_SYS_mul_word, TAGGED(POLY_SYS_mul_word));
    add_word_to_io_area(POLY_SYS_plus_word, TAGGED(POLY_SYS_plus_word));
    add_word_to_io_area(POLY_SYS_minus_word, TAGGED(POLY_SYS_minus_word));
    add_word_to_io_area(POLY_SYS_div_word, TAGGED(POLY_SYS_div_word));
    add_word_to_io_area(POLY_SYS_or_word, TAGGED(POLY_SYS_or_word));
    add_word_to_io_area(POLY_SYS_and_word, TAGGED(POLY_SYS_and_word));
    add_word_to_io_area(POLY_SYS_xor_word, TAGGED(POLY_SYS_xor_word));
    add_word_to_io_area(POLY_SYS_shift_left_word, TAGGED(POLY_SYS_shift_left_word));
    add_word_to_io_area(POLY_SYS_mod_word, TAGGED(POLY_SYS_mod_word));
    add_word_to_io_area(POLY_SYS_word_geq, TAGGED(POLY_SYS_word_geq));
    add_word_to_io_area(POLY_SYS_word_leq, TAGGED(POLY_SYS_word_leq));
    add_word_to_io_area(POLY_SYS_word_gtr, TAGGED(POLY_SYS_word_gtr));
    add_word_to_io_area(POLY_SYS_word_lss, TAGGED(POLY_SYS_word_lss));
    add_word_to_io_area(POLY_SYS_word_eq, TAGGED(POLY_SYS_word_eq));
    add_word_to_io_area(POLY_SYS_load_byte, TAGGED(POLY_SYS_load_byte));
    add_word_to_io_area(POLY_SYS_load_byte_immut, TAGGED(POLY_SYS_load_byte_immut));
    add_word_to_io_area(POLY_SYS_load_word, TAGGED(POLY_SYS_load_word));
    add_word_to_io_area(POLY_SYS_load_word_immut, TAGGED(POLY_SYS_load_word_immut));
    add_word_to_io_area(POLY_SYS_assign_byte, TAGGED(POLY_SYS_assign_byte));
    add_word_to_io_area(POLY_SYS_assign_word, TAGGED(POLY_SYS_assign_word));
    add_word_to_io_area(POLY_SYS_timing_dispatch, TAGGED(POLY_SYS_timing_dispatch));
    add_word_to_io_area(POLY_SYS_XWindows, TAGGED(POLY_SYS_XWindows));
    add_word_to_io_area(POLY_SYS_full_gc,     TAGGED(POLY_SYS_full_gc));
    add_word_to_io_area(POLY_SYS_stack_trace, TAGGED(POLY_SYS_stack_trace));
    add_word_to_io_area(POLY_SYS_foreign_dispatch, TAGGED(POLY_SYS_foreign_dispatch));
    add_word_to_io_area(POLY_SYS_callcode_tupled,  TAGGED(POLY_SYS_callcode_tupled));
    add_word_to_io_area(POLY_SYS_process_env,      TAGGED(POLY_SYS_process_env));
    add_word_to_io_area(POLY_SYS_set_string_length, TAGGED(POLY_SYS_set_string_length));
    add_word_to_io_area(POLY_SYS_get_first_long_word, TAGGED(POLY_SYS_get_first_long_word));
    add_word_to_io_area(POLY_SYS_poly_specific, TAGGED(POLY_SYS_poly_specific));
    add_word_to_io_area(POLY_SYS_bytevec_eq, TAGGED(POLY_SYS_bytevec_eq));
    add_word_to_io_area(POLY_SYS_shrink_stack,     TAGGED(POLY_SYS_shrink_stack));
    add_word_to_io_area(POLY_SYS_code_flags,        TAGGED(POLY_SYS_code_flags));
    add_word_to_io_area(POLY_SYS_shift_right_arith_word, TAGGED(POLY_SYS_shift_right_arith_word));
    add_word_to_io_area(POLY_SYS_int_to_word,      TAGGED(POLY_SYS_int_to_word));
    add_word_to_io_area(POLY_SYS_set_code_constant,TAGGED(POLY_SYS_set_code_constant));
    add_word_to_io_area(POLY_SYS_move_bytes,       TAGGED(POLY_SYS_move_bytes));
    add_word_to_io_area(POLY_SYS_move_words,       TAGGED(POLY_SYS_move_words));
    add_word_to_io_area(POLY_SYS_move_bytes_overlap,        TAGGED(POLY_SYS_move_bytes_overlap));
    add_word_to_io_area(POLY_SYS_move_words_overlap,        TAGGED(POLY_SYS_move_words_overlap));
    add_word_to_io_area(POLY_SYS_eq_longword,               TAGGED(POLY_SYS_eq_longword));
    add_word_to_io_area(POLY_SYS_neq_longword,              TAGGED(POLY_SYS_neq_longword));
    add_word_to_io_area(POLY_SYS_geq_longword,              TAGGED(POLY_SYS_geq_longword));
    add_word_to_io_area(POLY_SYS_leq_longword,              TAGGED(POLY_SYS_leq_longword));
    add_word_to_io_area(POLY_SYS_gt_longword,               TAGGED(POLY_SYS_gt_longword));
    add_word_to_io_area(POLY_SYS_lt_longword,               TAGGED(POLY_SYS_lt_longword));
    add_word_to_io_area(POLY_SYS_plus_longword,             TAGGED(POLY_SYS_plus_longword));
    add_word_to_io_area(POLY_SYS_minus_longword,            TAGGED(POLY_SYS_minus_longword));
    add_word_to_io_area(POLY_SYS_mul_longword,              TAGGED(POLY_SYS_mul_longword));
    add_word_to_io_area(POLY_SYS_div_longword,              TAGGED(POLY_SYS_div_longword));
    add_word_to_io_area(POLY_SYS_mod_longword,              TAGGED(POLY_SYS_mod_longword));
    add_word_to_io_area(POLY_SYS_andb_longword,             TAGGED(POLY_SYS_andb_longword));
    add_word_to_io_area(POLY_SYS_orb_longword,              TAGGED(POLY_SYS_orb_longword));
    add_word_to_io_area(POLY_SYS_xorb_longword,             TAGGED(POLY_SYS_xorb_longword));
    add_word_to_io_area(POLY_SYS_shift_left_longword,       TAGGED(POLY_SYS_shift_left_longword));
    add_word_to_io_area(POLY_SYS_shift_right_longword,      TAGGED(POLY_SYS_shift_right_longword));
    add_word_to_io_area(POLY_SYS_shift_right_arith_longword,TAGGED(POLY_SYS_shift_right_arith_longword));
    add_word_to_io_area(POLY_SYS_longword_to_tagged,        TAGGED(POLY_SYS_longword_to_tagged));
    add_word_to_io_area(POLY_SYS_signed_to_longword,        TAGGED(POLY_SYS_signed_to_longword));
    add_word_to_io_area(POLY_SYS_unsigned_to_longword,      TAGGED(POLY_SYS_unsigned_to_longword));

    add_word_to_io_area(POLY_SYS_io_dispatch, TAGGED(POLY_SYS_io_dispatch));
    add_word_to_io_area(POLY_SYS_network, TAGGED(POLY_SYS_network));
    add_word_to_io_area(POLY_SYS_os_specific, TAGGED(POLY_SYS_os_specific));
    add_word_to_io_area(POLY_SYS_signal_handler, TAGGED(POLY_SYS_signal_handler));
}

// As far as possible we want locking and unlocking an ML mutex to be fast so
// we try to implement the code in the assembly code using appropriate
// interlocked instructions.  That does mean that if we need to lock and
// unlock an ML mutex in this code we have to use the same, machine-dependent,
// code to do it.  These are defaults that are used where there is no
// machine-specific code.

// Increment the value contained in the first word of the mutex.
// On most platforms this code will be done with a piece of assembly code.
static PLock mutexLock;

Handle Interpreter::AtomicIncrement(TaskData *taskData, Handle mutexp)
{
    PLocker l(&mutexLock);
    PolyObject *p = DEREFHANDLE(mutexp);
    // A thread can only call this once so the values will be short
    PolyWord newValue = TAGGED(UNTAGGED(p->Get(0))+1);
    p->Set(0, newValue);
    return taskData->saveVec.push(newValue);
}

// Decrement the value contained in the first word of the mutex.
Handle Interpreter::AtomicDecrement(TaskData *taskData, Handle mutexp)
{
    PLocker l(&mutexLock);
    PolyObject *p = DEREFHANDLE(mutexp);
    PolyWord newValue = TAGGED(UNTAGGED(p->Get(0))-1);
    p->Set(0, newValue);
    return taskData->saveVec.push(newValue);
}

// Release a mutex.  We need to lock the mutex to ensure we don't
// reset it in the time between one of atomic operations reading
// and writing the mutex.
void Interpreter::AtomicReset(TaskData * /*taskData*/, Handle mutexp)
{
    PLocker l(&mutexLock);
    DEREFHANDLE(mutexp)->Set(0, TAGGED(1)); // Set this to released.
}

static Interpreter interpreterObject;

MachineDependent *machineDependent = &interpreterObject;
