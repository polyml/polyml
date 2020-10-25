/*
    Title:  An interpreter for a compact instruction set.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited
    Further development Copyright David C.J. Matthews 2015-18, 2020.

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

/*
#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif
*/
#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

/*
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

#ifdef HAVE_MATH_H
#include <math.h>
#endif
*/
#include <cmath> // Currently just for isnan.


#include "globals.h"
#include "int_opcodes.h"
#include "machine_dep.h"
#include "sys.h"
#include "profiling.h"
#include "arb.h"
#include "reals.h"
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


#define arg1    (pc[0] + pc[1]*256)
#define arg2    (pc[2] + pc[3]*256)

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
   CHECKED_REGS + \
   UNCHECKED_REGS + \
   EXTRA_STACK)


// This duplicates some code in reals.cpp but is now updated.
#define DOUBLESIZE (sizeof(double)/sizeof(POLYUNSIGNED))

union realdb { double dble; POLYUNSIGNED puns[DOUBLESIZE]; };

#define LGWORDSIZE (sizeof(uintptr_t) / sizeof(PolyWord))

// We're using float for Real32 so it needs to be 32-bits.
// Assume that's true for the moment.
#if (SIZEOF_FLOAT != 4)
#error "Float is not 32-bits.  Please report this"
#endif

union flt { float fl; int32_t i; };

ByteCodeInterpreter::ByteCodeInterpreter() : overflowPacket(0), dividePacket(0)
{
#ifdef PROFILEOPCODES
    memset(frequency, 0, sizeof(frequency));
    memset(arg1Value, 0, sizeof(arg1Value));
    memset(arg2Value, 0, sizeof(arg2Value));
#endif
}

ByteCodeInterpreter::~ByteCodeInterpreter()
{
#ifdef PROFILEOPCODES
    OutputDebugStringA("Frequency\n");
    for (unsigned i = 0; i < 256; i++)
    {
        if (frequency[i] != 0)
        {
            char buffer[100];
            sprintf(buffer, "%02X: %u\n", i, frequency[i]);
            OutputDebugStringA(buffer);
        }
    }
    OutputDebugStringA("Arg1\n");
    for (unsigned i = 0; i < 256; i++)
    {
        if (arg1Value[i] != 0)
        {
            char buffer[100];
            sprintf(buffer, "%02X: %u\n", i, arg1Value[i]);
            OutputDebugStringA(buffer);
        }
    }
    OutputDebugStringA("Arg2\n");
    for (unsigned i = 0; i < 256; i++)
    {
        if (arg2Value[i] != 0)
        {
            char buffer[100];
            sprintf(buffer, "%02X: %u\n", i, arg2Value[i]);
            OutputDebugStringA(buffer);
        }
    }
#endif
}

// This lock is used to synchronise all atomic operations.
// It is not needed in the X86 version because that can use a global
// memory lock.
static PLock mutexLock;

// Special value for return address.
#define SPECIAL_PC_END_THREAD           ((POLYCODEPTR)0)


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


// Allocate memory on the heap.  Returns with the address of the cell. Does not set the
// length word or any of the data.
PolyObject* ByteCodeInterpreter::allocateMemory(TaskData * taskData, POLYUNSIGNED words, POLYCODEPTR& pc, stackItem*& sp)
{
    words++; // Add the size of the length word.
    // N.B. The allocation area may be empty so that both of these are zero.
    if (taskData->allocPointer >= taskData->allocLimit + words + 1)
    {
#ifdef POLYML32IN64
        if (words & 1) words++;
#endif
        taskData->allocPointer -= words;
        return (PolyObject*)(taskData->allocPointer + 1);
    }
    // Insufficient space.
    SaveInterpreterState(pc, sp);
    // Find some space to allocate in. Returns a pointer to the newly allocated space.
    // N.B. This may return zero if the heap is exhausted and it has set this
    // up for an exception.  Generally it allocates by decrementing allocPointer
    // but if the required memory is large it may allocate in a separate area.
    PolyWord* space = processes->FindAllocationSpace(taskData, words, true);
    LoadInterpreterState(pc, sp);
    if (space == 0) return 0;
    return (PolyObject*)(space + 1);
}

// Put a real result in a "box"
PolyObject* ByteCodeInterpreter::boxDouble(TaskData* taskData, double d, POLYCODEPTR& pc, stackItem*& sp)
{
    PolyObject* mem = this->allocateMemory(taskData, DOUBLESIZE, pc, sp);
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
double ByteCodeInterpreter::unboxDouble(PolyWord p)
{
    union realdb uniondb;
    for (unsigned i = 0; i < DOUBLESIZE; i++)
        uniondb.puns[i] = p.AsObjPtr()->Get(i).AsUnsigned();
    return uniondb.dble;
}

// Largely copied from reals.cpp

#if (SIZEOF_FLOAT < SIZEOF_POLYWORD)

    // Typically for 64-bit mode.  Use a tagged representation.
    // The code-generator on the X86/64 assumes the float is in the
    // high order word.
#define FLT_SHIFT ((SIZEOF_POLYWORD-SIZEOF_FLOAT)*8)
float ByteCodeInterpreter::unboxFloat(PolyWord p)
{
    union flt argx;
    argx.i = p.AsSigned() >> FLT_SHIFT;
    return argx.fl;
}

PolyObject* ByteCodeInterpreter::boxFloat(TaskData* taskData, float f, POLYCODEPTR& pc, stackItem*& sp)
{
    union flt argx;
    argx.fl = f;
    PolyWord p = PolyWord::FromSigned(((POLYSIGNED)argx.i << FLT_SHIFT) + 1);
    return p.AsObjPtr(); // Temporarily cast it to this even though it isn't really
}
#else
    // Typically for 32-bit mode.  Use a boxed representation.
PolyObject* ByteCodeInterpreter::boxFloat(TaskData* taskData, float f, POLYCODEPTR& pc, stackItem*& sp)
{
    PolyObject* mem = this->allocateMemory(taskData, 1, pc, sp);
    if (mem == 0) return 0;
    mem->SetLengthWord(1, F_BYTE_OBJ);
    union flt argx;
    argx.fl = f;
    mem->Set(0, PolyWord::FromSigned(argx.i));
    return mem;
}

// Extract a double value from a box.
float ByteCodeInterpreter::unboxFloat(PolyWord p)
{
    union flt argx;
    argx.i = (int32_t)p.AsObjPtr()->Get(0).AsSigned();
    return argx.fl;
}

#endif


int ByteCodeInterpreter::RunInterpreter(TaskData *taskData)
/* (Re)-enter the Poly code from C. */
{
    // Make packets for exceptions. 
    if (overflowPacket == 0)
        overflowPacket = makeExceptionPacket(taskData, EXC_overflow);
    if (dividePacket == 0)
        dividePacket = makeExceptionPacket(taskData, EXC_divide);

    // Local values.  These are copies of member variables but are used so frequently that
    // it is important that access should be fast.
    POLYCODEPTR     pc;
    stackItem*sp;

    LoadInterpreterState(pc, sp);

    // We may have taken an interrupt which has set an exception.
    if (GetExceptionPacket().IsDataPtr()) goto RAISE_EXCEPTION;

    for(;;){ /* Each instruction */
#if (0)
        char buff[1000];
        sprintf(buff, "addr = %p sp=%p instr=%02x *sp=%p\n", pc, sp, *pc, (*sp).stackAddr);
        OutputDebugStringA(buff);
#endif
        // These are temporary values used where one instruction jumps to
        // common code.
        POLYUNSIGNED    tailCount;
        stackItem*      tailPtr;
        POLYUNSIGNED    returnCount;
        POLYUNSIGNED    storeWords;
        POLYUNSIGNED    stackCheck;
        PolyObject      *closure;
        double          dv;

#ifdef PROFILEOPCODES
        frequency[*pc]++;
#endif
        switch(*pc++) {

        case INSTR_jump8false:
        {
            PolyWord u = *sp++;
            if (u == True) pc += 1;
            else pc += *pc + 1;
            break;
        }

        case INSTR_jump8: pc += *pc + 1; break;

        case INSTR_jump8True:
        {
            PolyWord u = *sp++;
            if (u == False) pc += 1;
            else pc += *pc + 1;
            break;
        }

        case INSTR_jump16True:
            // Invert the sense of the test and fall through.
            *sp = ((*sp).w() == True) ? False : True;

        case INSTR_jump16false:
        {
            PolyWord u = *sp++; /* Pop argument */
            if (u == True) { pc += 2; break; }
            /* else - false - take the jump */
        }

        case INSTR_jump16:
            pc += arg1 + 2; break;

        case INSTR_push_handler: /* Save the old handler value. */
            (*(--sp)).stackAddr = GetHandlerRegister(); /* Push old handler */
            break;

        case INSTR_setHandler8: /* Set up a handler */
            (*(--sp)).codeAddr = pc + *pc + 1; /* Address of handler */
            SetHandlerRegister(sp);
            pc += 1;
            break;

        case INSTR_setHandler16: /* Set up a handler */
            (*(--sp)).codeAddr = pc + arg1 + 2; /* Address of handler */
            SetHandlerRegister(sp);
            pc += 2;
            break;

        case INSTR_deleteHandler: /* Delete handler retaining the result. */
        {
            stackItem u = *sp++;
            sp = GetHandlerRegister();
            sp++; // Remove handler entry point
            SetHandlerRegister((*sp).stackAddr); // Restore old handler
            *sp = u; // Put back the result
            break;
        }

        case INSTR_case16:
            {
                // arg1 is the largest value that is in the range
                POLYSIGNED u = UNTAGGED(*sp++); /* Get the value */
                if (u >= arg1 || u < 0) pc += 2 + arg1*2; /* Out of range */
                else {
                    pc += 2;
                    pc += /* Index */pc[u*2]+pc[u*2 + 1]*256; }
                break;
            }


        case INSTR_tail_3_bLegacy:
           tailCount = 3;
           tailPtr = sp + tailCount;
           sp = tailPtr + *pc;
           goto TAIL_CALL;

        case INSTR_tail_3_2Legacy:
           tailCount = 3;
           tailPtr = sp + tailCount;
           sp = tailPtr + 2;
           goto TAIL_CALL;

        case INSTR_tail_3_3Legacy:
           tailCount = 3;
           tailPtr = sp + tailCount;
           sp = tailPtr + 3;
           goto TAIL_CALL;

        case INSTR_tail_4_bLegacy:
           tailCount = 4;
           tailPtr = sp + tailCount;
           sp = tailPtr + *pc;
           goto TAIL_CALL;

        case INSTR_tail_b_b:
           tailCount = *pc;
           tailPtr = sp + tailCount;
           sp = tailPtr + pc[1];
       TAIL_CALL: /* For general case. */
           if (tailCount < 2) Crash("Invalid argument\n");
           for (; tailCount > 0; tailCount--) *(--sp) = *(--tailPtr);
           pc = (*sp++).codeAddr; /* Pop the original return address. */
           closure = (*sp++).w().AsObjPtr();
           goto CALL_CLOSURE; /* And drop through. */

        case INSTR_call_closure: /* Closure call. */
        {
            closure = (*sp++).w().AsObjPtr();
            CALL_CLOSURE:
            (--sp)->codeAddr = pc; /* Save return address. */
            *(--sp) = (PolyWord)closure;
            pc = *(POLYCODEPTR*)closure;    /* Get entry point. */
            SaveInterpreterState(pc, sp); // Update in case we're profiling
            // Check that there at least 128 words on the stack
            stackCheck = 128;
            goto STACKCHECK;
        }

        case INSTR_callConstAddr8:
            closure = (*(PolyWord*)(pc + pc[0] + 1)).AsObjPtr(); pc += 1; goto CALL_CLOSURE;

        case INSTR_callConstAddr16:
            closure = (*(PolyWord*)(pc + arg1 + 2)).AsObjPtr(); pc += 2; goto CALL_CLOSURE;

        case INSTR_callLocalB:
        {
            closure = (sp[*pc++]).w().AsObjPtr();
            goto CALL_CLOSURE;
        }

        case INSTR_return_w:
            returnCount = arg1; /* Get no. of args to remove. */

            RETURN: /* Common code for return. */
            {
                stackItem result = *sp++; /* Result */
                sp++; /* Remove the link/closure */
                pc = (*sp++).codeAddr; /* Return address */
                sp += returnCount; /* Add on number of args. */
                if (pc == SPECIAL_PC_END_THREAD)
                    exitThread(taskData); // This thread is exiting.
                *(--sp) = result; /* Result */
                SaveInterpreterState(pc, sp); // Update in case we're profiling
            }
            break;

        case INSTR_return_b: returnCount = *pc; goto RETURN;
        case INSTR_return_0Legacy: returnCount = 0; goto RETURN;
        case INSTR_return_1: returnCount = 1; goto RETURN;
        case INSTR_return_2: returnCount = 2; goto RETURN;
        case INSTR_return_3: returnCount = 3; goto RETURN;

        case INSTR_stackSize8Legacy:
            stackCheck = *pc++;
            goto STACKCHECK;

        case INSTR_stackSize16:
        {
            stackCheck = arg1; pc += 2;
        STACKCHECK:
            // Check there is space on the stack
            if (sp - stackCheck < StackLimit())
            {
                SaveInterpreterState(pc, sp);
                GrowStack(stackCheck);
                LoadInterpreterState(pc, sp);
            }
            // Also check for interrupts
            if (WasInterrupted())
            {
                SaveInterpreterState(pc, sp);
                return -1;
            }
            break;
        }

        case INSTR_raise_ex:
        {
            PolyException *exn = (PolyException*)((*sp).w().AsObjPtr());
            taskData->SetException(exn);
        RAISE_EXCEPTION:
            sp = GetHandlerRegister();
            pc = (*sp++).codeAddr;
            if (pc == SPECIAL_PC_END_THREAD)
                exitThread(taskData);  // Default handler for thread.
            SetHandlerRegister((*sp++).stackAddr);
            break;
        }

        case INSTR_tuple_2: storeWords = 2; goto TUPLE;
        case INSTR_tuple_3: storeWords = 3; goto TUPLE;
        case INSTR_tuple_4: storeWords = 4; goto TUPLE;
        case INSTR_tuple_b: storeWords = *pc; pc++; goto TUPLE;

        case INSTR_closureB:
            storeWords = *pc++;
            goto CREATE_CLOSURE;
            break;

        case INSTR_local_w:
            {
                stackItem u = sp[arg1];
                *(--sp) = u;
                pc += 2;
                break;
            }

        case INSTR_constAddr8:
            *(--sp) = *(PolyWord*)(pc + pc[0] + 1); pc += 1; break;

        case INSTR_constAddr16:
            *(--sp) = *(PolyWord*)(pc + arg1 + 2); pc += 2; break;

        case INSTR_const_int_w: *(--sp) = TAGGED(arg1); pc += 2; break;

        case INSTR_jump_back8:
            pc -= *pc + 1;
            if (WasInterrupted())
            {
                // Check for interrupt in case we're in a loop
                SaveInterpreterState(pc, sp);
                return -1;
            }
            break;

        case INSTR_jump_back16:
            pc -= arg1 + 1;
            if (WasInterrupted())
            {
                // Check for interrupt in case we're in a loop
                SaveInterpreterState(pc, sp);
                return -1;
            }
            break;

        case INSTR_lock:
            {
                PolyObject *obj = (*sp).w().AsObjPtr();
                obj->SetLengthWord(obj->LengthWord() & ~_OBJ_MUTABLE_BIT);
                break;
            }

        case INSTR_ldexc: *(--sp) = GetExceptionPacket(); break;

        case INSTR_local_b: { stackItem u = sp[*pc]; *(--sp) = u; pc += 1; break; }

        case INSTR_indirect_b:
            *sp = (*sp).w().AsObjPtr()->Get(*pc); pc += 1; break;

        case INSTR_indirectLocalBB:
        { PolyWord u = sp[*pc++]; *(--sp) = u.AsObjPtr()->Get(*pc++); break; }

        case INSTR_indirectLocalB0:
        { PolyWord u = sp[*pc++]; *(--sp) = u.AsObjPtr()->Get(0); break; }

        case INSTR_indirect0Local0:
        { PolyWord u = sp[0]; *(--sp) = u.AsObjPtr()->Get(0); break; }

        case INSTR_indirectLocalB1:
        { PolyWord u = sp[*pc++]; *(--sp) = u.AsObjPtr()->Get(1); break; }

        case INSTR_moveToContainerB:
            { PolyWord u = *sp++; (*sp).stackAddr[*pc] = u; pc += 1; break; }

        case INSTR_moveToMutClosureB:
        {
            PolyWord u = *sp++;
            (*sp).w().AsObjPtr()->Set(*pc++ + sizeof(uintptr_t) / sizeof(PolyWord), u);
            break;
        }

        case INSTR_indirectContainerB:
            *sp = (*sp).stackAddr[*pc]; pc += 1; break;

        case INSTR_indirectClosureBB:
        { PolyWord u = sp[*pc++]; *(--sp) = u.AsObjPtr()->Get(*pc++ + sizeof(uintptr_t) / sizeof(PolyWord)); break; }

        case INSTR_indirectClosureB0:
        { PolyWord u = sp[*pc++]; *(--sp) = u.AsObjPtr()->Get(sizeof(uintptr_t) / sizeof(PolyWord)); break; }

        case INSTR_indirectClosureB1:
        { PolyWord u = sp[*pc++]; *(--sp) = u.AsObjPtr()->Get(sizeof(uintptr_t) / sizeof(PolyWord) + 1); break; }

        case INSTR_indirectClosureB2:
        { PolyWord u = sp[*pc++]; *(--sp) = u.AsObjPtr()->Get(sizeof(uintptr_t) / sizeof(PolyWord) + 2); break; }

        case INSTR_set_stack_val_b:
            { PolyWord u = *sp++; sp[*pc-1] = u; pc += 1; break; }

        case INSTR_reset_b: sp += *pc; pc += 1; break;

        case INSTR_reset_r_b:
            { PolyWord u = *sp; sp += *pc; *sp = u; pc += 1; break; }

        case INSTR_const_int_b: *(--sp) = TAGGED(*pc); pc += 1; break;

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
        case INSTR_local_12: { stackItem u = sp[12]; *(--sp) = u; break; }
        case INSTR_local_13: { stackItem u = sp[13]; *(--sp) = u; break; }
        case INSTR_local_14: { stackItem u = sp[14]; *(--sp) = u; break; }
        case INSTR_local_15: { stackItem u = sp[15]; *(--sp) = u; break; }

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

        case INSTR_const_0: *(--sp) = Zero; break;
        case INSTR_const_1: *(--sp) = TAGGED(1); break;
        case INSTR_const_2: *(--sp) = TAGGED(2); break;
        case INSTR_const_3: *(--sp) = TAGGED(3); break;
        case INSTR_const_4: *(--sp) = TAGGED(4); break;
        case INSTR_const_10: *(--sp) = TAGGED(10); break;

        case INSTR_reset_r_1: { PolyWord u = *sp; sp += 1; *sp = u; break; }
        case INSTR_reset_r_2: { PolyWord u = *sp; sp += 2; *sp = u; break; }
        case INSTR_reset_r_3: { PolyWord u = *sp; sp += 3; *sp = u; break; }

        case INSTR_reset_1: sp += 1; break;
        case INSTR_reset_2: sp += 2; break;

        case INSTR_stack_containerB:
        {
            POLYUNSIGNED words = *pc++;
            while (words-- > 0) *(--sp) = Zero;
            sp--;
            (*sp).stackAddr = sp + 1;
            break;
        }

        case INSTR_tuple_containerLegacy: /* Create a tuple from a container. */
            {
                storeWords = arg1;
                PolyObject *t = this->allocateMemory(taskData, storeWords, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(storeWords, 0);
                for(; storeWords > 0; )
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
                ClearExceptionPacket();
                SaveInterpreterState(pc, sp);
                POLYUNSIGNED result = doCall();
                LoadInterpreterState(pc, sp);
                // If this raised an exception 
                if (GetExceptionPacket().IsDataPtr()) goto RAISE_EXCEPTION;
                *(--sp) = PolyWord::FromUnsigned(result);
                break;
            }

        case INSTR_callFastRTS1:
            {
                callFastRts1 doCall = *(callFastRts1*)(*sp++).w().AsObjPtr();
                intptr_t rtsArg1 = (*sp++).argValue;
                ClearExceptionPacket();
                SaveInterpreterState(pc, sp);
                POLYUNSIGNED result = doCall(rtsArg1);
                LoadInterpreterState(pc, sp);
                // If this raised an exception 
                if (GetExceptionPacket().IsDataPtr()) goto RAISE_EXCEPTION;
                *(--sp) = PolyWord::FromUnsigned(result);
                break;
            }

        case INSTR_callFastRTS2:
            {
                callFastRts2 doCall = *(callFastRts2*)(*sp++).w().AsObjPtr();
                intptr_t rtsArg2 = (*sp++).argValue; // Pop off the args, last arg first.
                intptr_t rtsArg1 = (*sp++).argValue;
                ClearExceptionPacket();
                SaveInterpreterState(pc, sp);
                POLYUNSIGNED result = doCall(rtsArg1, rtsArg2);
                LoadInterpreterState(pc, sp);
                // If this raised an exception 
                if (GetExceptionPacket().IsDataPtr()) goto RAISE_EXCEPTION;
                *(--sp) = PolyWord::FromUnsigned(result);
                break;
            }

        case INSTR_callFastRTS3:
            {
                callFastRts3 doCall = *(callFastRts3*)(*sp++).w().AsObjPtr();
                intptr_t rtsArg3 = (*sp++).argValue; // Pop off the args, last arg first.
                intptr_t rtsArg2 = (*sp++).argValue;
                intptr_t rtsArg1 = (*sp++).argValue;
                ClearExceptionPacket();
                SaveInterpreterState(pc, sp);
                POLYUNSIGNED result = doCall(rtsArg1, rtsArg2, rtsArg3);
                LoadInterpreterState(pc, sp);
                // If this raised an exception 
                if (GetExceptionPacket().IsDataPtr()) goto RAISE_EXCEPTION;
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
                ClearExceptionPacket();
                SaveInterpreterState(pc, sp);
                POLYUNSIGNED result = doCall(rtsArg1, rtsArg2, rtsArg3, rtsArg4);
                LoadInterpreterState(pc, sp);
                // If this raised an exception 
                if (GetExceptionPacket().IsDataPtr()) goto RAISE_EXCEPTION;
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
                ClearExceptionPacket();
                SaveInterpreterState(pc, sp);
                POLYUNSIGNED result = doCall(rtsArg1, rtsArg2, rtsArg3, rtsArg4, rtsArg5);
                LoadInterpreterState(pc, sp);
                // If this raised an exception 
                if (GetExceptionPacket().IsDataPtr()) goto RAISE_EXCEPTION;
                *(--sp) = PolyWord::FromUnsigned(result);
                break;
            }

        case INSTR_callFullRTS0:
            {
                callFullRts0 doCall = *(callFullRts0*)(*sp++).w().AsObjPtr();
                ClearExceptionPacket();
                SaveInterpreterState(pc, sp);
                POLYUNSIGNED result = doCall(taskData->threadObject);
                LoadInterpreterState(pc, sp);
                // If this raised an exception 
                if (GetExceptionPacket().IsDataPtr()) goto RAISE_EXCEPTION;
                *(--sp)= PolyWord::FromUnsigned(result);
                break;
            }

        case INSTR_callFullRTS1:
            {
                callFullRts1 doCall = *(callFullRts1*)(*sp++).w().AsObjPtr();
                intptr_t rtsArg1 = (*sp++).argValue;
                ClearExceptionPacket();
                SaveInterpreterState(pc, sp);
                POLYUNSIGNED result = doCall(taskData->threadObject, rtsArg1);
                LoadInterpreterState(pc, sp);
                // If this raised an exception 
                if (GetExceptionPacket().IsDataPtr()) goto RAISE_EXCEPTION;
                *(--sp) = PolyWord::FromUnsigned(result);
                break;
            }

        case INSTR_callFullRTS2:
            {
                callFullRts2 doCall = *(callFullRts2*)(*sp++).w().AsObjPtr();
                intptr_t rtsArg2 = (*sp++).argValue; // Pop off the args, last arg first.
                intptr_t rtsArg1 = (*sp++).argValue;
                ClearExceptionPacket();
                SaveInterpreterState(pc, sp);
                POLYUNSIGNED result = doCall(taskData->threadObject, rtsArg1, rtsArg2);
                LoadInterpreterState(pc, sp);
                // If this raised an exception 
                if (GetExceptionPacket().IsDataPtr()) goto RAISE_EXCEPTION;
                *(--sp) = PolyWord::FromUnsigned(result);
                break;
            }

        case INSTR_callFullRTS3:
            {
                callFullRts3 doCall = *(callFullRts3*)(*sp++).w().AsObjPtr();
                intptr_t rtsArg3 = (*sp++).argValue; // Pop off the args, last arg first.
                intptr_t rtsArg2 = (*sp++).argValue;
                intptr_t rtsArg1 = (*sp++).argValue;
                ClearExceptionPacket();
                SaveInterpreterState(pc, sp);
                POLYUNSIGNED result = doCall(taskData->threadObject, rtsArg1, rtsArg2, rtsArg3);
                LoadInterpreterState(pc, sp);
                // If this raised an exception 
                if (GetExceptionPacket().IsDataPtr()) goto RAISE_EXCEPTION;
                *(--sp) = PolyWord::FromUnsigned(result);
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

//        case INSTR_stringLength: // Now replaced by loadUntagged
//            *sp = TAGGED(((PolyStringObject*)(*sp).AsObjPtr())->length);
//            break;

        case INSTR_atomicIncr:
        {
            PLocker l(&mutexLock);
            PolyObject *p = (*sp).w().AsObjPtr();
            PolyWord newValue = TAGGED(UNTAGGED(p->Get(0))+1);
            p->Set(0, newValue);
            *sp = newValue;
            break;
        }

        case INSTR_atomicDecr:
        {
            PLocker l(&mutexLock);
            PolyObject *p = (*sp).w().AsObjPtr();
            PolyWord newValue = TAGGED(UNTAGGED(p->Get(0))-1);
            p->Set(0, newValue);
            *sp = newValue;
            break;
        }

        case INSTR_equalWord:
        {
            PolyWord u = *sp++;
            *sp = u == (*sp) ? True : False;
            break;
        }

        case INSTR_jumpNEqLocal:
        {
            // Compare a local with a constant and jump if not equal.
            PolyWord u = sp[pc[0]];
            if (u.IsTagged() && u.UnTagged() == pc[1])
                pc += 3;
            else pc += pc[2] + 3;
            break;
        }

        case INSTR_jumpNEqLocalInd:
        {
            // Test the union tag value in the first word of a tuple.
            PolyWord u = sp[pc[0]];
            u = u.AsObjPtr()->Get(0);
            if (u.IsTagged() && u.UnTagged() == pc[1])
                pc += 3;
            else pc += pc[2] + 3;
            break;
        }

        case INSTR_isTaggedLocalB:
        {
            PolyWord u = sp[*pc++];
            *(--sp) = u.IsTagged() ? True : False;
            break;
        }

        case INSTR_jumpTaggedLocal:
        {
            PolyWord u = sp[*pc];
            // Jump if the value is tagged.
            if (u.IsTagged())
                pc += pc[1] + 2;
            else pc += 2;
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
            PolyWord y = (*sp);
            POLYSIGNED t = UNTAGGED(x) + UNTAGGED(y);
            if (t <= MAXTAGGED && t >= -MAXTAGGED-1)
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
            PolyWord y = (*sp);
            POLYSIGNED t = UNTAGGED(y) - UNTAGGED(x);
            if (t <= MAXTAGGED && t >= -MAXTAGGED-1)
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
            POLYSIGNED x = UNTAGGED(*sp++);
            POLYSIGNED y = (*sp).w().AsSigned() - 1; // Just remove the tag
            POLYSIGNED t = x * y;
            if (x != 0 && t / x != y)
            {
                *(--sp) = (PolyWord)overflowPacket;
                goto RAISE_EXCEPTION;
            }
            *sp = PolyWord::FromSigned(t+1); // Add back the tag
            break;
        }

        case INSTR_fixedQuot:
        {
            // Zero and overflow are checked for in ML.
            POLYSIGNED u = UNTAGGED(*sp++);
            PolyWord y = (*sp);
            *sp = TAGGED(UNTAGGED(y) / u);
            break;
        }

        case INSTR_fixedRem:
        {
            // Zero and overflow are checked for in ML.
            POLYSIGNED u = UNTAGGED(*sp++);
            PolyWord y = (*sp);
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

        case INSTR_allocByteMem:
        {
            // Allocate byte segment.  This does not need to be initialised.
            POLYUNSIGNED flags = UNTAGGED_UNSIGNED(*sp++);
            POLYUNSIGNED length = UNTAGGED_UNSIGNED(*sp);
            PolyObject *t = this->allocateMemory(taskData, length, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION; // Exception
            t->SetLengthWord(length, (byte)flags);
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_getThreadId:
            *(--sp) = (PolyWord)taskData->threadObject;
            break;

        case INSTR_allocWordMemory:
        {
            // Allocate word segment.  This must be initialised.
            // We mustn't pop the initialiser until after any potential GC.
            POLYUNSIGNED length = UNTAGGED_UNSIGNED(sp[2]);
            PolyObject *t = this->allocateMemory(taskData, length, pc, sp);
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
            PolyObject *t = this->allocateMemory(taskData, 1, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            PolyWord initialiser = (*sp);
            t->SetLengthWord(1, F_MUTABLE_BIT);
            t->Set(0, initialiser);
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_allocMutClosureB:
        {
            // Allocate memory for a mutable closure and copy in the code address.
            POLYUNSIGNED length = *pc++ + sizeof(uintptr_t) / sizeof(PolyWord);
            PolyObject* t = this->allocateMemory(taskData, length, pc, sp);
            if (t == 0) goto RAISE_EXCEPTION;
            t->SetLengthWord(length, F_CLOSURE_OBJ | F_MUTABLE_BIT);
            PolyObject* srcClosure = (*sp).w().AsObjPtr();
            *(uintptr_t*)t = *(uintptr_t*)srcClosure;
            *sp = (PolyWord)t;
            break;
        }

        case INSTR_loadMLWordLegacy:
        {
            // The values on the stack are base, index and offset.
            POLYUNSIGNED offset = UNTAGGED(*sp++);
            POLYUNSIGNED index = UNTAGGED(*sp++);
            PolyObject *p = (PolyObject*)((*sp).w().AsCodePtr() + offset);
            *sp = p->Get(index);
            break;
        }

        case INSTR_loadMLWord:
        {
            POLYUNSIGNED index = UNTAGGED(*sp++);
            PolyObject* p = (PolyObject*)((*sp).w().AsCodePtr());
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

        case INSTR_loadUntaggedLegacy:
        {
            // The values on the stack are base, index and offset.
            POLYUNSIGNED offset = UNTAGGED(*sp++);
            POLYUNSIGNED index = UNTAGGED(*sp++);
            PolyObject *p = (PolyObject*)((*sp).w().AsCodePtr() + offset);
            *sp = TAGGED(p->Get(index).AsUnsigned());
            break;
        }

        case INSTR_loadUntagged:
        {
            POLYUNSIGNED index = UNTAGGED(*sp++);
            PolyObject* p = (PolyObject*)((*sp).w().AsCodePtr());
            *sp = TAGGED(p->Get(index).AsUnsigned());
            break;
        }

        case INSTR_storeMLWordLegacy: 
        {
            PolyWord toStore = *sp++;
            POLYUNSIGNED offset = UNTAGGED(*sp++);
            POLYUNSIGNED index = UNTAGGED(*sp++);
            PolyObject *p = (PolyObject*)((*sp).w().AsCodePtr() + offset);
            p->Set(index, toStore);
            *sp = Zero;
            break;
        }

        case INSTR_storeMLWord:
        {
            PolyWord toStore = *sp++;
            POLYUNSIGNED index = UNTAGGED(*sp++);
            PolyObject* p = (PolyObject*)((*sp).w().AsCodePtr());
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

        case INSTR_storeUntaggedLegacy: 
        {
            PolyWord toStore = PolyWord::FromUnsigned(UNTAGGED_UNSIGNED(*sp++));
            POLYUNSIGNED offset = UNTAGGED(*sp++);
            POLYUNSIGNED index = UNTAGGED(*sp++);
            PolyObject *p = (PolyObject*)((*sp).w().AsCodePtr() + offset);
            p->Set(index, toStore);
            *sp = Zero;
            break;
        }

        case INSTR_storeUntagged:
        {
            PolyWord toStore = PolyWord::FromUnsigned(UNTAGGED_UNSIGNED(*sp++));
            POLYUNSIGNED index = UNTAGGED(*sp++);
            PolyObject* p = (PolyObject*)((*sp).w().AsCodePtr());
            p->Set(index, toStore);
            *sp = Zero;
            break;
        }

        case INSTR_blockMoveWordLegacy:
        {
            // The offsets are byte counts but the the indexes are in words.
            POLYUNSIGNED length = UNTAGGED_UNSIGNED(*sp++);
            POLYUNSIGNED destOffset = UNTAGGED_UNSIGNED(*sp++);
            POLYUNSIGNED destIndex = UNTAGGED_UNSIGNED(*sp++);
            PolyObject *dest = (PolyObject*)((*sp++).w().AsCodePtr() + destOffset);
            POLYUNSIGNED srcOffset = UNTAGGED_UNSIGNED(*sp++);
            POLYUNSIGNED srcIndex = UNTAGGED_UNSIGNED(*sp++);
            PolyObject *src = (PolyObject*)((*sp).w().AsCodePtr() + srcOffset);
            for (POLYUNSIGNED u = 0; u < length; u++) dest->Set(destIndex+u, src->Get(srcIndex+u));
            *sp = Zero;
            break;
        }

        case INSTR_blockMoveWord:
        {
            POLYUNSIGNED length = UNTAGGED_UNSIGNED(*sp++);
            POLYUNSIGNED destIndex = UNTAGGED_UNSIGNED(*sp++);
            PolyObject* dest = (PolyObject*)((*sp++).w().AsCodePtr());
            POLYUNSIGNED srcIndex = UNTAGGED_UNSIGNED(*sp++);
            PolyObject* src = (PolyObject*)((*sp).w().AsCodePtr());
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
            memcpy(dest+destOffset, src+srcOffset, length);
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
            *sp = memcmp(arg1Ptr+arg1Offset, arg2Ptr+arg2Offset, length) == 0 ? True : False;
            break;
        }

        case INSTR_blockCompareByte:
        {
            POLYUNSIGNED length = UNTAGGED_UNSIGNED(*sp++);
            POLYUNSIGNED arg2Offset = UNTAGGED_UNSIGNED(*sp++);
            POLYCODEPTR arg2Ptr = (*sp++).w().AsCodePtr();
            POLYUNSIGNED arg1Offset = UNTAGGED_UNSIGNED(*sp++);
            POLYCODEPTR arg1Ptr = (*sp).w().AsCodePtr();
            int result = memcmp(arg1Ptr+arg1Offset, arg2Ptr+arg2Offset, length);
            *sp = result == 0 ? TAGGED(0) : result < 0 ? TAGGED(-1) : TAGGED(1);
            break;
        }

        // Backwards compatibility.
        // These are either used in the current compiler or compiled by it
        // while building the basis library.
        case EXTINSTR_stack_containerW:
        case EXTINSTR_reset_r_w:
        case EXTINSTR_tuple_w:
        case EXTINSTR_unsignedToLongW:
        case EXTINSTR_signedToLongW:
        case EXTINSTR_longWToTagged:
        case EXTINSTR_lgWordShiftLeft:
        case EXTINSTR_fixedIntToReal:
        case EXTINSTR_callFastRtoR:
        case EXTINSTR_realMult:
        case EXTINSTR_realDiv:
        case EXTINSTR_realNeg:
        case EXTINSTR_realAbs:
        case EXTINSTR_realToFloat:
        case EXTINSTR_floatDiv:
        case EXTINSTR_floatNeg:
        case EXTINSTR_floatAbs:
        case EXTINSTR_callFastFtoF:
        case EXTINSTR_floatMult:
        case EXTINSTR_callFastGtoR:
        case EXTINSTR_realUnordered:
        case EXTINSTR_realEqual:
        case EXTINSTR_lgWordEqual:
        case EXTINSTR_lgWordOr:
        case EXTINSTR_wordShiftRArith:
        case EXTINSTR_lgWordLess:
            // Back up and handle them as though they were escaped.
            pc--;

        case INSTR_escape:
        {
            switch (*pc++) {

            case EXTINSTR_callFastRRtoR:
            {
                // Floating point call.
                callRTSRRtoR doCall = *(callRTSRRtoR*)(*sp++).w().AsObjPtr();
                PolyWord rtsArg2 = *sp++;
                PolyWord rtsArg1 = *sp++;
                double argument1 = unboxDouble(rtsArg1);
                double argument2 = unboxDouble(rtsArg2);
                // Allocate memory for the result.
                double result = doCall(argument1, argument2);
                PolyObject* t = boxDouble(taskData, result, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *(--sp) = (PolyWord)t;
                break;
            }

            case EXTINSTR_callFastRGtoR:
            {
                // Call that takes a POLYUNSIGNED argument and returns a double.
                callRTSRGtoR doCall = *(callRTSRGtoR*)(*sp++).w().AsObjPtr();
                intptr_t rtsArg2 = (*sp++).w().AsSigned();
                PolyWord rtsArg1 = *sp++;
                double argument1 = unboxDouble(rtsArg1);
                // Allocate memory for the result.
                double result = doCall(argument1, rtsArg2);
                PolyObject* t = boxDouble(taskData, result, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *(--sp) = (PolyWord)t;
                break;
            }

            case EXTINSTR_callFastGtoR:
            {
                // Call that takes a POLYUNSIGNED argument and returns a double.
                callRTSGtoR doCall = *(callRTSGtoR*)(*sp++).w().AsObjPtr();
                intptr_t rtsArg1 = (*sp++).w().AsSigned();
                // Allocate memory for the result.
                double result = doCall(rtsArg1);
                PolyObject* t = boxDouble(taskData, result, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *(--sp) = (PolyWord)t;
                break;
            }

            case EXTINSTR_callFastFtoF:
            {
                // Floating point call.  The call itself does not allocate but we
                // need to put the result into a "box".
                callRTSFtoF doCall = *(callRTSFtoF*)(*sp++).w().AsObjPtr();
                PolyWord rtsArg1 = *sp++;
                float argument = unboxFloat(rtsArg1);
                // Allocate memory for the result.
                float result = doCall(argument);
                PolyObject* t = boxFloat(taskData, result, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *(--sp) = (PolyWord)t;
                break;
            }

            case EXTINSTR_callFastFFtoF:
            {
                // Floating point call.
                callRTSFFtoF doCall = *(callRTSFFtoF*)(*sp++).w().AsObjPtr();
                PolyWord rtsArg2 = *sp++;
                PolyWord rtsArg1 = *sp++;
                float argument1 = unboxFloat(rtsArg1);
                float argument2 = unboxFloat(rtsArg2);
                // Allocate memory for the result.
                float result = doCall(argument1, argument2);
                PolyObject* t = boxFloat(taskData, result, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *(--sp) = (PolyWord)t;
                break;
            }

            case EXTINSTR_callFastGtoF:
            {
                // Call that takes a POLYUNSIGNED argument and returns a double.
                callRTSGtoF doCall = *(callRTSGtoF*)(*sp++).w().AsObjPtr();
                intptr_t rtsArg1 = (*sp++).w().AsSigned();
                // Allocate memory for the result.
                float result = doCall(rtsArg1);
                PolyObject* t = boxFloat(taskData, result, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *(--sp) = (PolyWord)t;
                break;
            }

            case EXTINSTR_callFastFGtoF:
            {
                // Call that takes a POLYUNSIGNED argument and returns a double.
                callRTSFGtoF doCall = *(callRTSFGtoF*)(*sp++).w().AsObjPtr();
                intptr_t rtsArg2 = (*sp++).w().AsSigned();
                PolyWord rtsArg1 = *sp++;
                float argument1 = unboxFloat(rtsArg1);
                // Allocate memory for the result.
                float result = doCall(argument1, rtsArg2);
                PolyObject* t = boxFloat(taskData, result, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *(--sp) = (PolyWord)t;
                break;
            }

            case EXTINSTR_callFastRtoR:
            {
                // Floating point call.  The call itself does not allocate but we
                // need to put the result into a "box".
                callRTSRtoR doCall = *(callRTSRtoR*)(*sp++).w().AsObjPtr();
                PolyWord rtsArg1 = *sp++;
                double argument = unboxDouble(rtsArg1);
                // Allocate memory for the result.
                double result = doCall(argument);
                PolyObject* t = boxDouble(taskData, result, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *(--sp) = (PolyWord)t;
                break;
            }

            case EXTINSTR_atomicReset:
            {
                // This is needed in the interpreted version otherwise there
                // is a chance that we could set the value to zero while another
                // thread is between getting the old value and setting it to the new value.
                PLocker l(&mutexLock);
                PolyObject* p = (*sp).w().AsObjPtr();
                p->Set(0, TAGGED(0)); // Set this to released.
                *sp = TAGGED(0); // Push the unit result
                break;
            }

            case EXTINSTR_longWToTagged:
            {
                // Extract the first word and return it as a tagged value.  This loses the top-bit
                POLYUNSIGNED wx = (*sp).w().AsObjPtr()->Get(0).AsUnsigned();
                *sp = TAGGED(wx);
                break;
            }

            case EXTINSTR_signedToLongW:
            {
                // Shift the tagged value to remove the tag and put it into the first word.
                // The original sign bit is copied in the shift.
                intptr_t wx = (*sp).w().UnTagged();
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(intptr_t*)t = wx;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_unsignedToLongW:
            {
                // As with the above except the value is treated as an unsigned
                // value and the top bit is zero.
                uintptr_t wx = (*sp).w().UnTaggedUnsigned();
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(uintptr_t*)t = wx;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_realAbs:
            {
                PolyObject* t = this->boxDouble(taskData, fabs(unboxDouble(*sp)), pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_realNeg:
            {
                PolyObject* t = this->boxDouble(taskData, -(unboxDouble(*sp)), pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_floatAbs:
            {
                PolyObject* t = this->boxFloat(taskData, fabs(unboxFloat(*sp)), pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_floatNeg:
            {
                PolyObject* t = this->boxFloat(taskData, -(unboxFloat(*sp)), pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_fixedIntToReal:
            {
                POLYSIGNED u = UNTAGGED(*sp);
                PolyObject* t = this->boxDouble(taskData, (double)u, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_fixedIntToFloat:
            {
                POLYSIGNED u = UNTAGGED(*sp);
                PolyObject* t = this->boxFloat(taskData, (float)u, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_floatToReal:
            {
                float u = unboxFloat(*sp);
                PolyObject* t = this->boxDouble(taskData, (double)u, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_wordShiftRArith:
            {
                PolyWord u = *sp++;
                // Strictly speaking, C does not require that this uses
                // arithmetic shifting so we really ought to set the
                // high-order bits explicitly.
                *sp = TAGGED(UNTAGGED(*sp) >> UNTAGGED(u));
                break;
            }


            case EXTINSTR_lgWordEqual:
            {
                uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                *sp = wx == wy ? True : False;
                break;
            }

            case EXTINSTR_lgWordLess:
            {
                uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                *sp = (wy < wx) ? True : False;
                break;
            }

            case EXTINSTR_lgWordLessEq:
            {
                uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                *sp = (wy <= wx) ? True : False;
                break;
            }

            case EXTINSTR_lgWordGreater:
            {
                uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                *sp = (wy > wx) ? True : False;
                break;
            }

            case EXTINSTR_lgWordGreaterEq:
            {
                uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                *sp = (wy >= wx) ? True : False;
                break;
            }

            case EXTINSTR_lgWordAdd:
            {
                uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(uintptr_t*)t = wy + wx;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_lgWordSub:
            {
                uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(uintptr_t*)t = wy - wx;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_lgWordMult:
            {
                uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(uintptr_t*)t = wy * wx;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_lgWordDiv:
            {
                uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(uintptr_t*)t = wy / wx;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_lgWordMod:
            {
                uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(uintptr_t*)t = wy % wx;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_lgWordAnd:
            {
                uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(uintptr_t*)t = wy & wx;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_lgWordOr:
            {
                uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(uintptr_t*)t = wy | wx;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_lgWordXor:
            {
                uintptr_t wx = *(uintptr_t*)((*sp++).w().AsObjPtr());
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(uintptr_t*)t = wy ^ wx;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_lgWordShiftLeft:
            {
                // The shift amount is a tagged word not a boxed large word
                POLYUNSIGNED wx = UNTAGGED_UNSIGNED(*sp++);
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(uintptr_t*)t = wy << wx;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_lgWordShiftRLog:
            {
                // The shift amount is a tagged word not a boxed large word
                POLYUNSIGNED wx = UNTAGGED_UNSIGNED(*sp++);
                uintptr_t wy = *(uintptr_t*)((*sp).w().AsObjPtr());
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(uintptr_t*)t = wy >> wx;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_lgWordShiftRArith:
            {
                // The shift amount is a tagged word not a boxed large word
                POLYUNSIGNED wx = UNTAGGED_UNSIGNED(*sp++);
                intptr_t wy = *(intptr_t*)((*sp).w().AsObjPtr());
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(intptr_t*)t = wy >> wx;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_realEqual:
            {
                double u = unboxDouble(*sp++);
                *sp = u == unboxDouble(*sp) ? True : False;
                break;
            }

            case EXTINSTR_realLess:
            {
                double u = unboxDouble(*sp++);
                *sp = unboxDouble(*sp) < u ? True : False;
                break;
            }

            case EXTINSTR_realLessEq:
            {
                double u = unboxDouble(*sp++);
                *sp = unboxDouble(*sp) <= u ? True : False;
                break;
            }

            case EXTINSTR_realGreater:
            {
                double u = unboxDouble(*sp++);
                *sp = unboxDouble(*sp) > u ? True : False;
                break;
            }

            case EXTINSTR_realGreaterEq:
            {
                double u = unboxDouble(*sp++);
                *sp = unboxDouble(*sp) >= u ? True : False;
                break;
            }

            case EXTINSTR_realUnordered:
            {
                double u = unboxDouble(*sp++);
                double v = unboxDouble(*sp);
                *sp = (std::isnan(u) || std::isnan(v)) ? True : False;
                break;
            }

            case EXTINSTR_realAdd:
            {
                double u = unboxDouble(*sp++);
                double v = unboxDouble(*sp);
                PolyObject* t = this->boxDouble(taskData, v + u, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_realSub:
            {
                double u = unboxDouble(*sp++);
                double v = unboxDouble(*sp);
                PolyObject* t = this->boxDouble(taskData, v - u, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_realMult:
            {
                double u = unboxDouble(*sp++);
                double v = unboxDouble(*sp);
                PolyObject* t = this->boxDouble(taskData, v * u, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_realDiv:
            {
                double u = unboxDouble(*sp++);
                double v = unboxDouble(*sp);
                PolyObject* t = this->boxDouble(taskData, v / u, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_floatEqual:
            {
                float u = unboxFloat(*sp++);
                *sp = u == unboxFloat(*sp) ? True : False;
                break;
            }

            case EXTINSTR_floatLess:
            {
                float u = unboxFloat(*sp++);
                *sp = unboxFloat(*sp) < u ? True : False;
                break;
            }

            case EXTINSTR_floatLessEq:
            {
                float u = unboxFloat(*sp++);
                *sp = unboxFloat(*sp) <= u ? True : False;
                break;
            }

            case EXTINSTR_floatGreater:
            {
                float u = unboxFloat(*sp++);
                *sp = unboxFloat(*sp) > u ? True : False;
                break;
            }

            case EXTINSTR_floatGreaterEq:
            {
                float u = unboxFloat(*sp++);
                *sp = unboxFloat(*sp) >= u ? True : False;
                break;
            }

            case EXTINSTR_floatUnordered:
            {
                float u = unboxFloat(*sp++);
                float v = unboxFloat(*sp);
                *sp = (std::isnan(u) || std::isnan(v)) ? True : False;
                break;
            }

            case EXTINSTR_floatAdd:
            {
                float u = unboxFloat(*sp++);
                float v = unboxFloat(*sp);
                PolyObject* t = this->boxFloat(taskData, v + u, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_floatSub:
            {
                float u = unboxFloat(*sp++);
                float v = unboxFloat(*sp);
                PolyObject* t = this->boxFloat(taskData, v - u, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_floatMult:
            {
                float u = unboxFloat(*sp++);
                float v = unboxFloat(*sp);
                PolyObject* t = this->boxFloat(taskData, v * u, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_floatDiv:
            {
                float u = unboxFloat(*sp++);
                float v = unboxFloat(*sp);
                PolyObject* t = this->boxFloat(taskData, v / u, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_realToFloat:
            {
                // Convert a double to a float.  It's complicated because it depends on the rounding mode.
                int rMode = *pc++;
                int current = getrounding();
                // If the rounding is 4 it means "use current rounding".
                // Don't call unboxDouble until we're set the rounding.  GCC seems to convert it
                // before the actual float cast.
                if (rMode < 4) setrounding(rMode);
                double d = unboxDouble(*sp);
                float v = (float)d; // Convert with the appropriate rounding.
                setrounding(current);
                PolyObject* t = this->boxFloat(taskData, v, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_realToInt:
                dv = unboxDouble(*sp);
                goto realtoint;

            case EXTINSTR_floatToInt:
                dv = (double)unboxFloat(*sp);
            realtoint:
                {
                    // Convert a double or a float to a tagged integer.
                    int rMode = *pc++;
                    // We mustn't try converting a value that will overflow the conversion
                    // but we need to be careful that we don't raise overflow incorrectly due
                    // to rounding.
                    if (dv > (double)(MAXTAGGED + MAXTAGGED / 2) ||
                        dv < -(double)(MAXTAGGED + MAXTAGGED / 2))
                    {
                        *(--sp) = (PolyWord)overflowPacket;
                        goto RAISE_EXCEPTION;
                    }
                    POLYSIGNED p;
                    switch (rMode)
                    {
                    case POLY_ROUND_TONEAREST:
                        p = (POLYSIGNED)round(dv);
                        break;
                    case POLY_ROUND_DOWNWARD:
                        p = (POLYSIGNED)floor(dv);
                        break;
                    case POLY_ROUND_UPWARD:
                        p = (POLYSIGNED)ceil(dv);
                        break;
                    case POLY_ROUND_TOZERO:
                    default:
                        // Truncation is the default for C.
                        p = (POLYSIGNED)dv;
                    }

                    // Check that the value can be tagged.
                    if (p > MAXTAGGED || p < -MAXTAGGED - 1)
                    {
                        *(--sp) = (PolyWord)overflowPacket;
                        goto RAISE_EXCEPTION;
                    }
                    *sp = TAGGED(p);
                    break;
                }

            case EXTINSTR_loadC8:
            {
                // This is similar to loadMLByte except that the base address is a boxed large-word.
                // Also the index is SIGNED.
                POLYSIGNED offset = UNTAGGED(*sp++);
                POLYSIGNED index = UNTAGGED(*sp++);
                POLYCODEPTR p = *((byte**)((*sp).w().AsObjPtr())) + offset;
                *sp = TAGGED(p[index]); // Have to tag the result
                break;
            }

            case EXTINSTR_loadC16:
            {
                // This and the other loads are similar to loadMLWord with separate
                // index and offset values.
                POLYSIGNED offset = UNTAGGED(*sp++);
                POLYSIGNED index = UNTAGGED(*sp++);
                POLYCODEPTR p = *((byte**)((*sp).w().AsObjPtr())) + offset;
                POLYUNSIGNED r = ((uint16_t*)p)[index];
                *sp = TAGGED(r);
                break;
            }

            case EXTINSTR_loadC32:
            {
                POLYSIGNED offset = UNTAGGED(*sp++);
                POLYSIGNED index = UNTAGGED(*sp++);
                POLYCODEPTR p = *((byte**)((*sp).w().AsObjPtr())) + offset;
                uintptr_t r = ((uint32_t*)p)[index];
#ifdef IS64BITS
                // This is tagged in 64-bit mode
                * sp = TAGGED(r);
#else
                // But boxed in 32-bit mode.
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(uintptr_t*)t = r;
                *sp = (PolyWord)t;
#endif
                break;
            }

#if (defined(IS64BITS) || defined(POLYML32IN64))
            case EXTINSTR_loadC64:
            {
                POLYSIGNED offset = UNTAGGED(*sp++);
                POLYSIGNED index = UNTAGGED(*sp++);
                POLYCODEPTR p = *((byte**)((*sp).w().AsObjPtr())) + offset;
                uintptr_t r = ((uint64_t*)p)[index];
                // This must be boxed.
                PolyObject* t = this->allocateMemory(taskData, LGWORDSIZE, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(LGWORDSIZE, F_BYTE_OBJ);
                *(uintptr_t*)t = r;
                *sp = (PolyWord)t;
                break;
            }
#endif

            case EXTINSTR_loadCFloat:
            {
                POLYSIGNED offset = UNTAGGED(*sp++);
                POLYSIGNED index = UNTAGGED(*sp++);
                POLYCODEPTR p = *((byte**)((*sp).w().AsObjPtr())) + offset;
                double r = ((float*)p)[index];
                // This must be boxed.
                PolyObject* t = this->boxDouble(taskData, r, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_loadCDouble:
            {
                POLYSIGNED offset = UNTAGGED(*sp++);
                POLYSIGNED index = UNTAGGED(*sp++);
                POLYCODEPTR p = *((byte**)((*sp).w().AsObjPtr())) + offset;
                double r = ((double*)p)[index];
                // This must be boxed.
                PolyObject* t = this->boxDouble(taskData, r, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_storeC8:
            {
                // Similar to storeMLByte except that the base address is a boxed large-word.
                POLYUNSIGNED toStore = UNTAGGED(*sp++);
                POLYSIGNED offset = UNTAGGED(*sp++);
                POLYSIGNED index = UNTAGGED(*sp++);
                POLYCODEPTR p = *((byte**)((*sp).w().AsObjPtr())) + offset;
                p[index] = (byte)toStore;
                *sp = Zero;
                break;
            }

            case EXTINSTR_storeC16:
            {
                uint16_t toStore = (uint16_t)UNTAGGED(*sp++);
                POLYSIGNED offset = UNTAGGED(*sp++);
                POLYSIGNED index = UNTAGGED(*sp++);
                POLYCODEPTR p = *((byte**)((*sp).w().AsObjPtr())) + offset;
                ((uint16_t*)p)[index] = toStore;
                *sp = Zero;
                break;
            }

            case EXTINSTR_storeC32:
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
                POLYCODEPTR p = *((byte**)((*sp).w().AsObjPtr())) + offset;
                ((uint32_t*)p)[index] = toStore;
                *sp = Zero;
                break;
        }

#if (defined(IS64BITS) || defined(POLYML32IN64))
            case EXTINSTR_storeC64:
            {
                // This is a boxed value.
                uint64_t toStore = *(uintptr_t*)((*sp++).w().AsObjPtr());
                POLYSIGNED offset = UNTAGGED(*sp++);
                POLYSIGNED index = UNTAGGED(*sp++);
                POLYCODEPTR p = *((byte**)((*sp).w().AsObjPtr())) + offset;
                ((uint64_t*)p)[index] = toStore;
                *sp = Zero;
                break;
            }
#endif

            case EXTINSTR_storeCFloat:
            {
                // This is a boxed value.
                float toStore = (float)unboxDouble(*sp++);
                POLYSIGNED offset = UNTAGGED(*sp++);
                POLYSIGNED index = UNTAGGED(*sp++);
                POLYCODEPTR p = *((byte**)((*sp).w().AsObjPtr())) + offset;
                ((float*)p)[index] = toStore;
                *sp = Zero;
                break;
            }

            case EXTINSTR_storeCDouble:
            {
                // This is a boxed value.
                double toStore = unboxDouble(*sp++);
                POLYSIGNED offset = UNTAGGED(*sp++);
                POLYSIGNED index = UNTAGGED(*sp++);
                POLYCODEPTR p = *((byte**)((*sp).w().AsObjPtr())) + offset;
                ((double*)p)[index] = toStore;
                *sp = Zero;
                break;
            }

            case EXTINSTR_jump32True:
                // Invert the sense of the test and fall through.
                *sp = ((*sp).w() == True) ? False : True;

            case EXTINSTR_jump32False:
            {
                PolyWord u = *sp++; /* Pop argument */
                if (u == True) { pc += 4; break; }
                /* else - false - take the jump */
            }

            case EXTINSTR_jump32:
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

            case EXTINSTR_setHandler32: /* Set up a handler */
            {
                POLYUNSIGNED offset = pc[0] + (pc[1] << 8) + (pc[2] << 16) + (pc[3] << 24);
                (--sp)->codeAddr = pc + offset + 4; /* Address of handler */
                SetHandlerRegister(sp);
                pc += 4;
                break;
            }

            case EXTINSTR_case32:
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

            case EXTINSTR_tuple_w:
            {
                storeWords = arg1; pc += 2;
            TUPLE: /* Common code for tupling. */
                PolyObject* p = this->allocateMemory(taskData, storeWords, pc, sp);
                if (p == 0) goto RAISE_EXCEPTION; // Exception
                p->SetLengthWord(storeWords, 0);
                for (; storeWords > 0; ) p->Set(--storeWords, *sp++);
                *(--sp) = (PolyWord)p;
                break;
            }

            case EXTINSTR_indirect_w:
                *sp = (*sp).w().AsObjPtr()->Get(arg1); pc += 2; break;

            case EXTINSTR_moveToContainerW:
            {
                PolyWord u = *sp++;
                (*sp).stackAddr[arg1] =u;
                pc += 2;
                break;
            }

            case EXTINSTR_moveToMutClosureW:
            {
               PolyWord u = *sp++;
                (*sp).w().AsObjPtr()->Set(arg1 + sizeof(uintptr_t)/sizeof(PolyWord), u);
                pc += 2;
                break;
            }

            case EXTINSTR_indirectContainerW:
                *sp = (*sp).stackAddr[arg1]; pc += 2; break;

            case EXTINSTR_indirectClosureW:
                *sp = (*sp).w().AsObjPtr()->Get(arg1+sizeof(uintptr_t)/sizeof(PolyWord)); pc += 2; break;

            case EXTINSTR_set_stack_val_w:
            {
                PolyWord u = *sp++;
                sp[arg1 - 1] = u;
                pc += 2;
                break;
            }

            case EXTINSTR_reset_w: sp += arg1; pc += 2; break;

            case EXTINSTR_reset_r_w:
            {
                PolyWord u = *sp;
                sp += arg1;
                *sp = u;
                pc += 2;
                break;
            }

            case EXTINSTR_stack_containerW:
            {
                POLYUNSIGNED words = arg1; pc += 2;
                while (words-- > 0) *(--sp) = Zero;
                sp--;
                (*sp).stackAddr = sp + 1;
                break;
            }

            case EXTINSTR_constAddr32:
            {
                POLYUNSIGNED offset = pc[0] + (pc[1] << 8) + (pc[2] << 16) + (pc[3] << 24);
                *(--sp) = *(PolyWord*)(pc + offset + 4);
                pc += 4;
                break;
            }

            case EXTINSTR_allocCSpace:
            {
                // Allocate this on the C heap.
                POLYUNSIGNED length = UNTAGGED_UNSIGNED(*sp);
                void* memory = malloc(length);
                *sp = Make_sysword(taskData, (uintptr_t)memory)->Word();
                break;
            }

            case EXTINSTR_freeCSpace:
            {
                // Both the address and the size are passed as arguments.
                sp++; // Size
                PolyWord addr = *sp;
                free(*(void**)(addr.AsObjPtr()));
                *sp = TAGGED(0);
                break;
            }

            case EXTINSTR_tail:
                /* Tail recursive call. */
                /* Move items up the stack. */
                /* There may be an overlap if the function we are calling
                   has more args than this one. */
                tailCount = arg1;
                tailPtr = sp + tailCount;
                sp = tailPtr + arg2;
                goto TAIL_CALL;


            case EXTINSTR_allocMutClosureW:
            {
                // Allocate memory for a mutable closure and copy in the code address.
                POLYUNSIGNED length = arg1 + sizeof(uintptr_t) / sizeof(PolyWord);
                pc += 2;
                PolyObject* t = this->allocateMemory(taskData, length, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(length, F_CLOSURE_OBJ | F_MUTABLE_BIT);
                PolyObject* srcClosure = (*sp).w().AsObjPtr();
                *(uintptr_t*)t = *(uintptr_t*)srcClosure;
                *sp = (PolyWord)t;
                break;
            }

            case EXTINSTR_closureW:
            {
                storeWords = arg1;
                pc += 2;
            CREATE_CLOSURE:
                // Allocate a closure.  storeWords is the number of non-locals.
                POLYUNSIGNED length = storeWords + sizeof(uintptr_t) / sizeof(PolyWord);
                PolyObject* t = this->allocateMemory(taskData, length, pc, sp);
                if (t == 0) goto RAISE_EXCEPTION;
                t->SetLengthWord(length, F_CLOSURE_OBJ);
                for (; storeWords > 0; ) t->Set(--storeWords + sizeof(uintptr_t) / sizeof(PolyWord), *sp++);
                PolyObject* srcClosure = (*sp).w().AsObjPtr();
                *(uintptr_t*)t = *(uintptr_t*)srcClosure;
                *sp = (PolyWord)t;
                break;
            }

            default: Crash("Unknown extended instruction %x\n", pc[-1]);
            }

            break;
        }

        case INSTR_enterIntX86:
            // This is a no-op if we are already interpreting.
            pc += 3; break;

        default: Crash("Unknown instruction %x\n", pc[-1]);

        } /* switch */
     } /* for */
     return 0;
}

void ByteCodeInterpreter::GarbageCollect(ScanAddress* process)
{
    overflowPacket = process->ScanObjectAddress(overflowPacket);
    dividePacket = process->ScanObjectAddress(dividePacket);
}
