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

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#if (defined(_WIN32) && ! defined(__CYGWIN__))
#include <windows.h>
#include "Console.h" /* For hApplicationInstance. */
#endif

#include "rtsentry.h"
#include "save_vec.h"
#include "polystring.h"
#include "processes.h"
#include "run_time.h"
#include "globals.h"
#include "arb.h"

// Create an entry point containing the address of the entry and the
// string name.  Having the string in there allows us to export the entry.
Handle creatEntryPointObject(TaskData *taskData, Handle entryH)
{
    TempCString entryName(Poly_string_to_C_alloc(entryH->WordP()));
    if ((const char *)entryName == 0) raise_syscall(taskData, "Insufficient memory", ENOMEM);
    // Create space for the address followed by the name as a C string.
    POLYUNSIGNED space = 1 + (strlen(entryName) + 1 + sizeof(PolyWord) - 1) / sizeof(PolyWord);
    // Allocate a byte, weak, mutable, no-overwrite cell.  It's not clear if
    // it actually needs to be mutable but if it is it needs to be no-overwrite.
    Handle refH = alloc_and_save(taskData, space, F_BYTE_OBJ|F_WEAK_BIT|F_MUTABLE_BIT|F_NO_OVERWRITE);
    strcpy((char*)(refH->WordP()->AsBytePtr() + sizeof(PolyWord)), entryName);
    if (! setEntryPoint(refH->WordP()))
        raise_fail(taskData, "entry point not found");
    return refH;
}

// Return the string entry point.
const char *getEntryPointName(PolyObject *p)
{
    if (p->Length() <= 1) return 0; // Doesn't contain an entry point
    return (const char *)(p->AsBytePtr() + sizeof(PolyWord));
}

static void *getSymbol(const char *entryName)
{
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    HINSTANCE lib = hApplicationInstance;
    void *sym = (void*)GetProcAddress(lib, entryName);
    if (sym != NULL) return sym;
    lib = LoadLibraryA("PolyLib");
    sym = (void*)GetProcAddress(lib, entryName);
    if (sym != NULL) return sym;
#else
    void *lib = dlopen(NULL, RTLD_LAZY);
    void *sym = dlsym(lib, entryName);
    if (sym != NULL) return sym;
    lib = dlopen("libpolyml", RTLD_LAZY);
    sym = dlsym(lib, entryName);
    if (sym != NULL) return sym;
#endif
    return 0;
}

// Sets the address of the entry point in an entry point object.
bool setEntryPoint(PolyObject *p)
{
    if (p->Length() == 0) return false;
    p->Set(0, PolyWord::FromSigned(0)); // Clear it by default
    if (p->Length() == 1) return false;
    const char *entryName = (const char*)(p->AsBytePtr()+sizeof(PolyWord));
    void *sym = getSymbol(entryName);
    if (sym == 0) return false;
    *(void**)p = sym;
    return true;
}

extern "C" {
#ifdef _MSC_VER
    __declspec(dllexport)
#endif
    POLYUNSIGNED PolyCreateEntryPointObject(PolyObject *threadId, PolyWord arg);
};

// External call
POLYUNSIGNED PolyCreateEntryPointObject(PolyObject *threadId, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = creatEntryPointObject(taskData, pushedArg);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}