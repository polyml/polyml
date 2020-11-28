/*
    Title:  poly_specific.cpp - Poly/ML specific RTS calls.

    Copyright (c) 2006, 2015-17, 2019 David C. J. Matthews

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

/* This module is used for various run-time calls that are either in the
   PolyML structure or otherwise specific to Poly/ML. */

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
#include "poly_specific.h"
#include "arb.h"
#include "mpoly.h"
#include "sys.h"
#include "machine_dep.h"
#include "polystring.h"
#include "run_time.h"
#include "version.h"
#include "save_vec.h"
#include "version.h"
#include "memmgr.h"
#include "processes.h"
#include "gc.h"
#include "rtsentry.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySpecificGeneral(FirstArgument threadId, PolyWord code, PolyWord arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetABI();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyLockMutableCode(FirstArgument threadId, PolyWord byteSeg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyLockMutableClosure(FirstArgument threadId, PolyWord closure);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyCopyByteVecToCode(FirstArgument threadId, PolyWord byteVec);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyCopyByteVecToClosure(FirstArgument threadId, PolyWord byteVec, PolyWord closure);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySetCodeConstant(PolyWord closure, PolyWord offset, PolyWord c, PolyWord flags);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySetCodeByte(PolyWord closure, PolyWord offset, PolyWord c);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetCodeByte(PolyWord closure, PolyWord offset);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySortArrayOfAddresses(PolyWord array);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTest4(FirstArgument threadId, PolyWord arg1, PolyWord arg2, PolyWord arg3, PolyWord arg4);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTest5(FirstArgument threadId, PolyWord arg1, PolyWord arg2, PolyWord arg3, PolyWord arg4, PolyWord arg5);
}

#define SAVE(x) taskData->saveVec.push(x)

#ifndef GIT_VERSION
#define GIT_VERSION             ""
#endif


Handle poly_dispatch_c(TaskData *taskData, Handle args, Handle code)
{
    unsigned c = get_C_unsigned(taskData, DEREFWORD(code));
    switch (c)
    {
    case 9: // Return the GIT version if appropriate
        {
             return SAVE(C_string_to_Poly(taskData, GIT_VERSION));
        }

    case 10: // Return the RTS version string.
        {
            const char *version;
            switch (machineDependent->MachineArchitecture())
            {
            case MA_Interpreted:    version = "Portable-" TextVersion; break;
            case MA_I386:           version = "I386-" TextVersion; break;
            case MA_X86_64:         version = "X86_64-" TextVersion; break;
            default:                version = "Unknown-" TextVersion; break;
            }
            return SAVE(C_string_to_Poly(taskData, version));
        }

    case 12: // Return the architecture
        // Used in InitialPolyML.ML for PolyML.architecture
        {
            const char *arch;
            switch (machineDependent->MachineArchitecture())
            {
            case MA_Interpreted:    arch = "Interpreted"; break;
            case MA_I386:           arch = "I386"; break;
            case MA_X86_64:         arch = "X86_64"; break;
            case MA_X86_64_32:      arch = "X86_64_32"; break;
            default:                arch = "Unknown"; break;
            }
            return SAVE(C_string_to_Poly(taskData, arch));
        }

    case 19: // Return the RTS argument help string.
        return SAVE(C_string_to_Poly(taskData, RTSArgHelp()));

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown poly-specific function: %d", c);
            raise_exception_string(taskData, EXC_Fail, msg);
            return 0;
        }
    }
}

// General interface to poly-specific.  Ideally the various cases will be made into
// separate functions.
POLYUNSIGNED PolySpecificGeneral(FirstArgument threadId, PolyWord code, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedCode = taskData->saveVec.push(code);
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = poly_dispatch_c(taskData, pushedArg, pushedCode);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Return the ABI - i.e. the calling conventions used when calling external functions.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetABI()
{
    // Return the ABI.  For 64-bit we need to know if this is Windows.
#if (SIZEOF_VOIDP == 8)
#if (defined(_WIN32) || defined(__CYGWIN__))
    return TAGGED(2).AsUnsigned(); // 64-bit Windows
#else
    return TAGGED(1).AsUnsigned(); // 64-bit Unix
#endif
#else
    return TAGGED(0).AsUnsigned(); // 32-bit Unix and Windows
#endif
}

// Code generation - Code is initially allocated in a byte segment.  When all the
// values have been set apart from any addresses the byte segment is copied into
// a mutable code segment.
// PolyCopyByteVecToCode is now replaced by PolyCopyByteVecToClosure
POLYEXTERNALSYMBOL POLYUNSIGNED PolyCopyByteVecToCode(FirstArgument threadId, PolyWord byteVec)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(byteVec);
    PolyObject *result = 0;

    try {
        if (!pushedArg->WordP()->IsByteObject())
            raise_fail(taskData, "Not byte data area");
        do {
            PolyObject *initCell = pushedArg->WordP();
            POLYUNSIGNED requiredSize = initCell->Length();
            result = gMem.AllocCodeSpace(requiredSize);
            if (result == 0)
            {
                // Could not allocate - must GC.
                if (!QuickGC(taskData, pushedArg->WordP()->Length()))
                    raise_fail(taskData, "Insufficient memory");
            }
            else memcpy(result, initCell, requiredSize * sizeof(PolyWord));
        } while (result == 0);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return ((PolyWord)result).AsUnsigned();
}

// Copy the byte vector into code space.
POLYUNSIGNED PolyCopyByteVecToClosure(FirstArgument threadId, PolyWord byteVec, PolyWord closure)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedByteVec = taskData->saveVec.push(byteVec);
    Handle pushedClosure = taskData->saveVec.push(closure);
    PolyObject *result = 0;

    try {
        if (!pushedByteVec->WordP()->IsByteObject())
            raise_fail(taskData, "Not byte data area");
        if (pushedClosure->WordP()->Length() != sizeof(PolyObject*)/sizeof(PolyWord))
            raise_fail(taskData, "Invalid closure size");
        if (!pushedClosure->WordP()->IsMutable())
            raise_fail(taskData, "Closure is not mutable");
        do {
            PolyObject *initCell = pushedByteVec->WordP();
            POLYUNSIGNED requiredSize = initCell->Length();
            result = gMem.AllocCodeSpace(requiredSize);
            if (result == 0)
            {
                // Could not allocate - must GC.
                if (!QuickGC(taskData, pushedByteVec->WordP()->Length()))
                    raise_fail(taskData, "Insufficient memory");
            }
            else memcpy(gMem.SpaceForObjectAddress(result)->writeAble((byte*)result), initCell, requiredSize * sizeof(PolyWord));
        } while (result == 0);
    }
    catch (...) {} // If an ML exception is raised

    // Store the code address in the closure.
    *((PolyObject**)pushedClosure->WordP()) = result;
    // Lock the closure.
    pushedClosure->WordP()->SetLengthWord(pushedClosure->WordP()->LengthWord() & ~_OBJ_MUTABLE_BIT);

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

// Code generation - Lock a mutable code segment and return the original address.
// Currently this does not allocate so other than the exception it could
// be a fast call.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyLockMutableCode(FirstArgument threadId, PolyWord byteSeg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(byteSeg);
    Handle result = 0;

    try {
        PolyObject *codeObj = pushedArg->WordP();
        if (!codeObj->IsCodeObject() || !codeObj->IsMutable())
            raise_fail(taskData, "Not mutable code area");
        POLYUNSIGNED segLength = codeObj->Length();
        codeObj->SetLengthWord(segLength, F_CODE_OBJ);
        // This is really a legacy of the PPC code-generator.
        machineDependent->FlushInstructionCache(codeObj, segLength * sizeof(PolyWord));
        // In the future it may be necessary to return a different address here.
        // N.B.  The code area should only have execute permission in the native
        // code version, not the interpreted version.
        result = pushedArg; // Return the original address.
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Replacement for above
POLYEXTERNALSYMBOL POLYUNSIGNED PolyLockMutableClosure(FirstArgument threadId, PolyWord closure)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    PolyObject *codeObj = *(PolyObject**)(closure.AsObjPtr());

    try {
        if (!codeObj->IsCodeObject() || !codeObj->IsMutable())
            raise_fail(taskData, "Not mutable code area");
        POLYUNSIGNED segLength = codeObj->Length();
        gMem.SpaceForObjectAddress(codeObj)->writeAble(codeObj)->SetLengthWord(segLength, F_CODE_OBJ);
        // In the future it may be necessary to return a different address here.
        // N.B.  The code area should only have execute permission in the native
        // code version, not the interpreted version.
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

// Set code constant.  This can be a fast call.
// This is in the RTS both because we pass a closure in here and cannot have
// code addresses in 32-in-64 and also because we need to ensure there is no
// possibility of a GC while the code is an inconsistent state.
POLYUNSIGNED PolySetCodeConstant(PolyWord closure, PolyWord offset, PolyWord cWord, PolyWord flags)
{
    byte *pointer;
    // Previously we passed the code address in here and we need to
    // retain that for legacy code.  This is now the closure.
    if (closure.AsObjPtr()->IsCodeObject())
        pointer = closure.AsCodePtr();
    else pointer = *(POLYCODEPTR*)(closure.AsObjPtr());
    // pointer is the start of the code segment.
    // c will usually be an address.
    // offset is a byte offset
    pointer += offset.UnTaggedUnsigned();
    byte* writeable = gMem.SpaceForAddress(pointer)->writeAble(pointer);
    switch (UNTAGGED(flags))
    {
        case 0: // Absolute constant - size PolyWord
        {
            POLYUNSIGNED c = cWord.AsUnsigned();
#ifdef WORDS_BIGENDIAN
            // This is used to store constants in the constant area
            // on the interpreted version. 
            for (unsigned i = sizeof(PolyWord); i > 0; i--)
            {
                writeable[i-1] = (byte)(c & 255);
                c >>= 8;
            }
#else
            for (unsigned i = 0; i < sizeof(PolyWord); i++)
            {
                writeable[i] = (byte)(c & 255);
                c >>= 8;
            }
#endif
            break;
        }
        case 1: // Relative constant - X86 - size 4 bytes
        {
            // The offset is relative to the END of the constant.
            byte *target;
            // In 32-in-64 we pass in the closure address here
            // rather than the code address.
            if (cWord.AsObjPtr()->IsCodeObject())
                target = cWord.AsCodePtr();
            else target = *(POLYCODEPTR*)(cWord.AsObjPtr());
            size_t c = target - pointer - 4;
            for (unsigned i = 0; i < 4; i++)
            {
                writeable[i] = (byte)(c & 255);
                c >>= 8;
            }
            break;
        }
        case 2: // Absolute constant - size uintptr_t
            // This is the same as case 0 except in 32-in-64 when
            // it is an absolute address rather than an object pointer.
        {
            uintptr_t c = (uintptr_t)(cWord.AsObjPtr());
            for (unsigned i = 0; i < sizeof(uintptr_t); i++)
            {
                pointer[i] = (byte)(c & 255);
                c >>= 8;
            }
            break;
        }
    }
    return TAGGED(0).AsUnsigned();
}

// Set a code byte.  This needs to be in the RTS because it uses the closure
POLYEXTERNALSYMBOL POLYUNSIGNED PolySetCodeByte(PolyWord closure, PolyWord offset, PolyWord cWord)
{
    byte *pointer = *(POLYCODEPTR*)(closure.AsObjPtr());
    byte* writable = gMem.SpaceForAddress(pointer)->writeAble(pointer);
    writable[UNTAGGED_UNSIGNED(offset)] = (byte)UNTAGGED_UNSIGNED(cWord);
    return TAGGED(0).AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetCodeByte(PolyWord closure, PolyWord offset)
{
    byte *pointer = *(POLYCODEPTR*)(closure.AsObjPtr());
    return TAGGED(pointer[UNTAGGED_UNSIGNED(offset)]).AsUnsigned();
}

static int compare(const void *a, const void *b)
{
    PolyWord *av = (PolyWord*)a;
    PolyWord *bv = (PolyWord*)b;
    if ((*av).IsTagged() || (*bv).IsTagged()) return 0; // Shouldn't happen
    PolyObject *ao = (*av).AsObjPtr(), *bo = (*bv).AsObjPtr();
    if (ao->Length() < 1 || bo->Length() < 1) return 0; // Shouldn't happen
    if (ao->Get(0).AsUnsigned() < bo->Get(0).AsUnsigned())
        return -1;
    if (ao->Get(0).AsUnsigned() > bo->Get(0).AsUnsigned())
        return 1;
    return 0;
}

// Sort an array of addresses.  This is used in the code-generator to search for
// duplicates in the address area.  The argument is an array of pairs.  The first
// item of each pair is an address, the second is an identifier of some kind.
POLYEXTERNALSYMBOL POLYUNSIGNED PolySortArrayOfAddresses(PolyWord array)
{
    if (!array.IsDataPtr()) return(TAGGED(0)).AsUnsigned();
    PolyObject *arrayP = array.AsObjPtr();
    POLYUNSIGNED numberOfItems = arrayP->Length();
    if (!arrayP->IsMutable()) return(TAGGED(0)).AsUnsigned();
    qsort(arrayP, numberOfItems, sizeof(PolyWord), compare);
    return (TAGGED(1)).AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyTest4(FirstArgument threadId, PolyWord arg1, PolyWord arg2, PolyWord arg3, PolyWord arg4)
{
    switch (arg1.UnTaggedUnsigned())
    {
    case 1: return arg1.AsUnsigned();
    case 2: return arg2.AsUnsigned();
    case 3: return arg3.AsUnsigned();
    case 4: return arg4.AsUnsigned();
    default: return TAGGED(0).AsUnsigned();
    }
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyTest5(FirstArgument threadId, PolyWord arg1, PolyWord arg2, PolyWord arg3, PolyWord arg4, PolyWord arg5)
{
    switch (arg1.UnTaggedUnsigned())
    {
    case 1: return arg1.AsUnsigned();
    case 2: return arg2.AsUnsigned();
    case 3: return arg3.AsUnsigned();
    case 4: return arg4.AsUnsigned();
    case 5: return arg5.AsUnsigned();
    default: return TAGGED(0).AsUnsigned();
    }

}


struct _entrypts polySpecificEPT[] =
{
    { "PolySpecificGeneral",            (polyRTSFunction)&PolySpecificGeneral},
    { "PolyGetABI",                     (polyRTSFunction)&PolyGetABI },
    { "PolyCopyByteVecToCode",          (polyRTSFunction)&PolyCopyByteVecToCode },
    { "PolyCopyByteVecToClosure",       (polyRTSFunction)&PolyCopyByteVecToClosure },
    { "PolyLockMutableCode",            (polyRTSFunction)&PolyLockMutableCode },
    { "PolyLockMutableClosure",         (polyRTSFunction)&PolyLockMutableClosure },
    { "PolySetCodeConstant",            (polyRTSFunction)&PolySetCodeConstant },
    { "PolySetCodeByte",                (polyRTSFunction)&PolySetCodeByte },
    { "PolyGetCodeByte",                (polyRTSFunction)&PolyGetCodeByte },
    { "PolySortArrayOfAddresses",       (polyRTSFunction)&PolySortArrayOfAddresses },
    { "PolyTest4",                      (polyRTSFunction)&PolyTest4 },
    { "PolyTest5",                      (polyRTSFunction)&PolyTest5 },

    { NULL, NULL} // End of list.
};
