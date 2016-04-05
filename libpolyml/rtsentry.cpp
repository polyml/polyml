/*
    Title:  rtsentry.cpp - Entry points to the run-time system

    Copyright (c) 2016 David C. J. Matthews

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

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)

#else
#define ASSERT(x)
#endif

#include "rtsentry.h"
#include "save_vec.h"
#include "polystring.h"
#include "processes.h"
#include "run_time.h"
#include "globals.h"
#include "arb.h"

extern "C" {
    POLYUNSIGNED PolyChDir();
    POLYUNSIGNED PolyFinish();
    POLYUNSIGNED PolyAddArbitrary();
    POLYUNSIGNED PolySubtractArbitrary();
    POLYUNSIGNED PolyMultiplyArbitrary();
    POLYUNSIGNED PolyDivideArbitrary();
    POLYUNSIGNED PolyRemainderArbitrary();
    POLYUNSIGNED PolyQuotRemArbitrary();
    POLYUNSIGNED PolyCompareArbitrary();
    POLYUNSIGNED PolyGCDArbitrary();
    POLYUNSIGNED PolyGCDArbitrary();
    POLYUNSIGNED PolyLCMArbitrary();
    POLYUNSIGNED PolyTest0();
    POLYUNSIGNED PolyTest1();
    POLYUNSIGNED PolyTest2();
    POLYUNSIGNED PolyTest3();
    POLYUNSIGNED PolyTest4();
}

static struct _entrypts {
    const char *name;
    POLYUNSIGNED (*entry)();
} entryPtTable[] =
{
    { "PolyChDir", &PolyChDir},
    { "PolyFinish", &PolyFinish},
    { "PolyAddArbitrary", &PolyAddArbitrary},
    { "PolySubtractArbitrary", &PolySubtractArbitrary},
    { "PolyMultiplyArbitrary", &PolyMultiplyArbitrary},
    { "PolyDivideArbitrary", &PolyDivideArbitrary},
    { "PolyRemainderArbitrary", &PolyRemainderArbitrary},
    { "PolyQuotRemArbitrary", &PolyQuotRemArbitrary},
    { "PolyCompareArbitrary", &PolyCompareArbitrary},
    { "PolyGCDArbitrary", &PolyGCDArbitrary},
    { "PolyLCMArbitrary", &PolyLCMArbitrary},
    { "PolyTest0", &PolyTest0},
    { "PolyTest1", &PolyTest1},
    { "PolyTest2", &PolyTest2},
    { "PolyTest3", &PolyTest3},
    { "PolyTest4", &PolyTest4}
};

// Find an entry point in the table.  We could use the system to find these because
// each entry is a system entry point but we need the table to do the reverse lookup.
// This is currently called in two different cases.
// When the code is created it is called with nil as the ref argument and a new
// reference is made.
// When the code is executed it checks to see if the first word of the ref is
// zero and calls this function if it is in order to set the value.  Because the
// ref is weak it may have been cleared as part of the loading process.
Handle makeEntryPoint(TaskData *taskData, Handle refH, Handle entryH)
{
    if (refH->Word() == TAGGED(0))
    {
        TempCString entryName(Poly_string_to_C_alloc(entryH->WordP()));
        if ((const char *)entryName == 0) raise_syscall(taskData, "Insufficient memory", ENOMEM);
        // Create space for the address followed by the name as a C string.
        POLYUNSIGNED space = 1 + (strlen(entryName) + 1 + sizeof(PolyWord) - 1) / sizeof(PolyWord);
        // Allocate a byte, weak, mutable, no-overwrite cell.  It's not clear if
        // it actually needs to be mutable but if it is it needs to be no-overwrite.
        refH = alloc_and_save(taskData, space, F_BYTE_OBJ|F_WEAK_BIT|F_MUTABLE_BIT|F_NO_OVERWRITE);
        strcpy((char*)(refH->WordP()->AsBytePtr() + sizeof(PolyWord)), entryName);
    }
    if (! setEntryPoint(refH->WordP()))
        raise_fail(taskData, "entry point not found");
    return refH;
}

// This is a legacy entry.  Previously the code took a single argument
// that was the string and returned the entry point in a single word object.
Handle oldGetEntryPoint(TaskData *taskData, Handle entryH)
{
    Handle refH = taskData->saveVec.push(TAGGED(0));
    return makeEntryPoint(taskData, refH, entryH);
}

// Return the string entry point.
const char *getEntryPointName(PolyObject *p)
{
    if (p->Length() <= 1) return 0; // Doesn't contain an entry point
    return (const char *)(p->AsBytePtr() + sizeof(PolyWord));
}

// Sets the address of the entry point in an entry point object.
bool setEntryPoint(PolyObject *p)
{
    if (p->Length() == 0) return false;
    p->Set(0, PolyWord::FromSigned(0)); // Clear it by default
    if (p->Length() == 1) return false;
    const char *entryName = (const char*)(p->AsBytePtr()+sizeof(PolyWord));
    for (unsigned i = 0; i < sizeof(entryPtTable)/sizeof(entryPtTable[0]); i++)
    {
        if (strcmp(entryName, entryPtTable[i].name) == 0)
        {
            *(uintptr_t*)p = (uintptr_t)entryPtTable[i].entry;
            return true;
        }
    }
    return false;
}