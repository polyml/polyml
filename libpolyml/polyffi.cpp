/*
    Title:  New Foreign Function Interface

    Copyright (c) 2015, 2018, 2019  David C.J. Matthews

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

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "globals.h"
// TODO: Do we need this??
// We need to include globals.h before <new> in mingw64 otherwise
// it messes up POLYUFMT/POLYSFMT.

#include <ffi.h>
#include <new>

#include "arb.h"
#include "save_vec.h"
#include "polyffi.h"
#include "run_time.h"
#include "sys.h"
#include "processes.h"
#include "polystring.h"

#if (defined(_WIN32))
#include <windows.h>
#include "winstartup.h" /* For hApplicationInstance. */
#endif

#include "scanaddrs.h"
#include "diagnostics.h"
#include "reals.h"
#include "rts_module.h"
#include "rtsentry.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFIGeneral(FirstArgument threadId, PolyWord code, PolyWord arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySizeFloat();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySizeDouble();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFIGetError(PolyWord addr);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFISetError(PolyWord err);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFICreateExtFn(FirstArgument threadId, PolyWord arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFICreateExtData(FirstArgument threadId, PolyWord arg);
    POLYEXTERNALSYMBOL void PolyFFICallbackException();
}

static struct _abiTable { const char *abiName; ffi_abi abiCode; } abiTable[] =
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
#elif defined(X86_ANY)
#if (defined(__i386__) || defined(__i386))
    {"sysv", FFI_SYSV},
#else
    {"unix64", FFI_UNIX64},
#endif
#endif
    { "default", FFI_DEFAULT_ABI}
};

// Table of constants returned by call 51
static int constantTable[] =
{
    FFI_DEFAULT_ABI,    // Default ABI
    FFI_TYPE_VOID,      // Type codes
    FFI_TYPE_INT,
    FFI_TYPE_FLOAT,
    FFI_TYPE_DOUBLE,
    FFI_TYPE_UINT8,
    FFI_TYPE_SINT8,
    FFI_TYPE_UINT16,
    FFI_TYPE_SINT16,
    FFI_TYPE_UINT32,
    FFI_TYPE_SINT32,
    FFI_TYPE_UINT64,
    FFI_TYPE_SINT64,
    FFI_TYPE_STRUCT,
    FFI_TYPE_POINTER,
    FFI_SIZEOF_ARG      // Minimum size for result space
};

// Table of predefined ffi types
static ffi_type *ffiTypeTable[] =
{
    &ffi_type_void,
    &ffi_type_uint8,
    &ffi_type_sint8,
    &ffi_type_uint16,
    &ffi_type_sint16,
    &ffi_type_uint32,
    &ffi_type_sint32,
    &ffi_type_uint64,
    &ffi_type_sint64,
    &ffi_type_float,
    &ffi_type_double,
    &ffi_type_pointer,
    &ffi_type_uchar, // These are all aliases for the above
    &ffi_type_schar,
    &ffi_type_ushort,
    &ffi_type_sshort,
    &ffi_type_uint,
    &ffi_type_sint,
    &ffi_type_ulong,
    &ffi_type_slong
};

// Callback entry table
static struct _cbStructEntry {
    PolyWord    mlFunction;         // The ML function to call
    void        *closureSpace;      // Space allocated for the closure
    void        *resultFunction;    // Executable address for the function.  Needed to free.
} *callbackTable;
static unsigned callBackEntries = 0;
static PLock callbackTableLock; // Mutex to protect table.


static Handle mkAbitab(TaskData *taskData, void*, char *p);

static Handle toSysWord(TaskData *taskData, void *p)
{
    return Make_sysword(taskData, (uintptr_t)p);
}

static Handle poly_ffi(TaskData *taskData, Handle args, Handle code)
{
    unsigned c = get_C_unsigned(taskData, code->Word());
    switch (c)
    {
    case 0: // malloc
        {
            POLYUNSIGNED size = getPolyUnsigned(taskData, args->Word());
            return toSysWord(taskData, malloc(size));
        }
    case 1: // free
        {
            void *mem = *(void**)(args->WordP());
            free(mem);
            return taskData->saveVec.push(TAGGED(0));
        }

    case 2: // Load library
        {
            TempString libName(args->Word());
#if (defined(_WIN32))
            HINSTANCE lib = LoadLibrary(libName);
            if (lib == NULL)
            {
                char buf[256];
#if (defined(UNICODE))
                _snprintf(buf, sizeof(buf), "Loading <%S> failed. Error %lu", (LPCTSTR)libName, GetLastError());
#else
                _snprintf(buf, sizeof(buf), "Loading <%s> failed. Error %lu", (const char*)libName, GetLastError());
#endif
                buf[sizeof(buf)-1] = 0; // Terminate just in case
                raise_exception_string(taskData, EXC_foreign, buf);
            }
#else
            void *lib = dlopen(libName, RTLD_LAZY);
            if (lib == NULL)
            {
                char buf[256];
                snprintf(buf, sizeof(buf), "Loading <%s> failed: %s", (const char *)libName, dlerror());
                buf[sizeof(buf)-1] = 0; // Terminate just in case
                raise_exception_string(taskData, EXC_foreign, buf);
            }
#endif
            return toSysWord(taskData, lib);
        }

    case 3: // Load address of executable.
        {
#if (defined(_WIN32))
            HINSTANCE lib = hApplicationInstance;
#else
            void *lib = dlopen(NULL, RTLD_LAZY);
            if (lib == NULL)
            {
                char buf[256];
                snprintf(buf, sizeof(buf), "Loading address of executable failed: %s", dlerror());
                buf[sizeof(buf)-1] = 0; // Terminate just in case
                raise_exception_string(taskData, EXC_foreign, buf);
            }
#endif
            return toSysWord(taskData, lib);
        }
    case 4: // Unload library - Is this actually going to be used?
        {
#if (defined(_WIN32))
            HMODULE hMod = *(HMODULE*)(args->WordP());
            if (! FreeLibrary(hMod))
                raise_syscall(taskData, "FreeLibrary failed", GetLastError());
#else
            void *lib = *(void**)(args->WordP());
            if (dlclose(lib) != 0)
            {
                char buf[256];
                snprintf(buf, sizeof(buf), "dlclose failed: %s", dlerror());
                buf[sizeof(buf)-1] = 0; // Terminate just in case
                raise_exception_string(taskData, EXC_foreign, buf);
            }
#endif
            return taskData->saveVec.push(TAGGED(0));
        }
    case 5: // Load the address of a symbol from a library.
        {
            TempCString symName(args->WordP()->Get(1));
#if (defined(_WIN32))
            HMODULE hMod = *(HMODULE*)(args->WordP()->Get(0).AsAddress());
            void *sym = (void*)GetProcAddress(hMod, symName);
            if (sym == NULL)
            {
                char buf[256];
                _snprintf(buf, sizeof(buf), "Loading symbol <%s> failed. Error %lu", (LPCSTR)symName, GetLastError());
                buf[sizeof(buf)-1] = 0; // Terminate just in case
                raise_exception_string(taskData, EXC_foreign, buf);
            }
#else
            void *lib = *(void**)(args->WordP()->Get(0).AsAddress());
            void *sym = dlsym(lib, symName);
            if (sym == NULL)
            {
                char buf[256];
                snprintf(buf, sizeof(buf), "load_sym <%s> : %s", (const char *)symName, dlerror());
                buf[sizeof(buf)-1] = 0; // Terminate just in case
                raise_exception_string(taskData, EXC_foreign, buf);
            }
#endif
            return toSysWord(taskData, sym);
        }

        // Libffi functions
    case 50: // Return a list of available ABIs
            return makeList(taskData, sizeof(abiTable)/sizeof(abiTable[0]),
                            (char*)abiTable, sizeof(abiTable[0]), 0, mkAbitab);

    case 51: // A constant from the table
        {
            unsigned index = get_C_unsigned(taskData, args->Word());
            if (index >= sizeof(constantTable) / sizeof(constantTable[0]))
                raise_exception_string(taskData, EXC_foreign, "Index out of range");
            return Make_arbitrary_precision(taskData, constantTable[index]);
        }

    case 52: // Return an FFI type
        {
            unsigned index = get_C_unsigned(taskData, args->Word());
            if (index >= sizeof(ffiTypeTable) / sizeof(ffiTypeTable[0]))
                raise_exception_string(taskData, EXC_foreign, "Index out of range");
            return toSysWord(taskData, ffiTypeTable[index]);
        }

    case 53: // Extract fields from ffi type.
        {
            ffi_type *ffit = *(ffi_type**)(args->WordP());
            Handle sizeHandle = Make_arbitrary_precision(taskData, ffit->size);
            Handle alignHandle = Make_arbitrary_precision(taskData, ffit->alignment);
            Handle typeHandle = Make_arbitrary_precision(taskData, ffit->type);
            Handle elemHandle = toSysWord(taskData, ffit->elements);
            Handle resHandle = alloc_and_save(taskData, 4);
            resHandle->WordP()->Set(0, sizeHandle->Word());
            resHandle->WordP()->Set(1, alignHandle->Word());
            resHandle->WordP()->Set(2, typeHandle->Word());
            resHandle->WordP()->Set(3, elemHandle->Word());
            return resHandle;
        }

    case 54: // Construct an ffi type.
        {
            // This is probably only used to create structs.
            size_t size = getPolyUnsigned(taskData, args->WordP()->Get(0));
            unsigned short align = get_C_ushort(taskData, args->WordP()->Get(1));
            unsigned short type = get_C_ushort(taskData, args->WordP()->Get(2));
            unsigned nElems = 0;
            for (PolyWord p = args->WordP()->Get(3); !ML_Cons_Cell::IsNull(p); p = ((ML_Cons_Cell*)p.AsObjPtr())->t)
                nElems++;
            size_t space = sizeof(ffi_type);
            // If we need the elements add space for the elements plus
            // one extra for the zero terminator.
            if (nElems != 0) space += (nElems+1) * sizeof(ffi_type *);
            ffi_type *result = (ffi_type*)calloc(1, space);
            // Raise an exception rather than returning zero.
            if (result == 0) raise_syscall(taskData, "Insufficient memory", ENOMEM);
            ffi_type **elem = 0;
            if (nElems != 0) elem = (ffi_type **)(result+1);
            result->size = size;
            result->alignment = align;
            result->type = type;
            result->elements = elem;
            if (elem != 0)
            {
                for (PolyWord p = args->WordP()->Get(3); !ML_Cons_Cell::IsNull(p); p = ((ML_Cons_Cell*)p.AsObjPtr())->t)
                {
                    PolyWord e = ((ML_Cons_Cell*)p.AsObjPtr())->h;
                    *elem++ = *(ffi_type**)(e.AsAddress());
                }
                *elem = 0;
            }
            return toSysWord(taskData, result);
        }

    case 55: // Create a CIF.  This contains all the types and some extra information.
        // The result is in allocated memory followed immediately by the argument type vector.
        {
            ffi_abi abi = (ffi_abi)get_C_ushort(taskData, args->WordP()->Get(0));
            ffi_type *rtype = *(ffi_type **)args->WordP()->Get(1).AsAddress();
            unsigned nArgs = 0;
            for (PolyWord p = args->WordP()->Get(2); !ML_Cons_Cell::IsNull(p); p = ((ML_Cons_Cell*)p.AsObjPtr())->t)
                nArgs++;
            // Allocate space for the cif followed by the argument type vector
            size_t space = sizeof(ffi_cif) + nArgs * sizeof(ffi_type*);
            ffi_cif *cif = (ffi_cif *)malloc(space);
            if (cif == 0) raise_syscall(taskData, "Insufficient memory", ENOMEM);
            ffi_type **atypes = (ffi_type **)(cif+1);
            // Copy the arguments types.
            ffi_type **at = atypes;
            for (PolyWord p = args->WordP()->Get(2); !ML_Cons_Cell::IsNull(p); p = ((ML_Cons_Cell*)p.AsObjPtr())->t)
            {
                PolyWord e = ((ML_Cons_Cell*)p.AsObjPtr())->h;
                *at++ = *(ffi_type**)(e.AsAddress());
            }
            ffi_status status = ffi_prep_cif(cif, abi, nArgs, rtype, atypes);
            if (status == FFI_BAD_TYPEDEF)
                raise_exception_string(taskData, EXC_foreign, "Bad typedef in ffi_prep_cif");
            else if (status == FFI_BAD_ABI)
                raise_exception_string(taskData, EXC_foreign, "Bad ABI in ffi_prep_cif");
            else if (status != FFI_OK)
                raise_exception_string(taskData, EXC_foreign, "Error in ffi_prep_cif");
            return toSysWord(taskData, cif);
        }

    case 56: // Call a function.
        {
            ffi_cif *cif = *(ffi_cif **)args->WordP()->Get(0).AsAddress();
            void *f = *(void**)args->WordP()->Get(1).AsAddress();
            void *res = *(void**)args->WordP()->Get(2).AsAddress();
            void **arg = *(void***)args->WordP()->Get(3).AsAddress();
            ffi_call(cif, FFI_FN(f), res, arg);
            // Do we need to save the value of errno/GetLastError here?
            return taskData->saveVec.push(TAGGED(0));
        }

    case 57: // Create a callback.
        {
            raise_exception_string(taskData, EXC_foreign, "This is no longer used");
        }

    case 58: // Free an existing callback.
        {
            // The address returned from call 57 above is the executable address that can
            // be passed as a callback function.  The writable memory address returned
            // as the result of ffi_closure_alloc may or may not be the same.  To be safe
            // we need to search the table.
            void *resFun = *(void**)args->Word().AsAddress();
            PLocker pLocker(&callbackTableLock);
            for (unsigned i = 0; i < callBackEntries; i++)
            {
                if (callbackTable[i].resultFunction == resFun)
                {
                    ffi_closure_free(callbackTable[i].closureSpace);
                    callbackTable[i].closureSpace = 0;
                    callbackTable[i].resultFunction = 0;
                    callbackTable[i].mlFunction = TAGGED(0); // Release the ML function
                    return taskData->saveVec.push(TAGGED(0));
                }
            }
            raise_exception_string(taskData, EXC_foreign, "Invalid callback entry");
        }

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown ffi function: %d", c);
            raise_exception_string(taskData, EXC_foreign, msg);
            return 0;
        }
    }
}

// Construct an entry in the ABI table.
static Handle mkAbitab(TaskData *taskData, void *arg, char *p)
{
    struct _abiTable *ab = (struct _abiTable *)p;
    // Construct a pair of the string and the code
    Handle name = taskData->saveVec.push(C_string_to_Poly(taskData, ab->abiName));
    Handle code = Make_arbitrary_precision(taskData, ab->abiCode);
    Handle result = alloc_and_save(taskData, 2);
    result->WordP()->Set(0, name->Word());
    result->WordP()->Set(1, code->Word());
    return result;
}

class PolyFFI: public RtsModule
{
public:
    virtual void GarbageCollect(ScanAddress *process);
};

// Declare this.  It will be automatically added to the table.
static PolyFFI polyFFIModule;

// We need to scan the callback table.
void PolyFFI::GarbageCollect(ScanAddress *process)
{
    for (unsigned i = 0; i < callBackEntries; i++)
        process->ScanRuntimeWord(&callbackTable[i].mlFunction);
}

// General interface to IO.  Ideally the various cases will be made into
// separate functions.
POLYUNSIGNED PolyFFIGeneral(FirstArgument threadId, PolyWord code, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedCode = taskData->saveVec.push(code);
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = poly_ffi(taskData, pushedArg, pushedCode);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// These functions are needed in the compiler
POLYUNSIGNED PolySizeFloat()
{
    return TAGGED((POLYSIGNED)ffi_type_float.size).AsUnsigned();
}

POLYUNSIGNED PolySizeDouble()
{
    return TAGGED((POLYSIGNED)ffi_type_double.size).AsUnsigned();
}

// Get either errno or GetLastError
POLYUNSIGNED PolyFFIGetError(PolyWord addr)
{
#if (defined(_WIN32))
    addr.AsObjPtr()->Set(0, PolyWord::FromUnsigned(GetLastError()));
#else
    addr.AsObjPtr()->Set(0, PolyWord::FromUnsigned((POLYUNSIGNED)errno));
#endif
    return 0;
}

// The argument is a SysWord.word value i.e. the address of a byte cell.
POLYUNSIGNED PolyFFISetError(PolyWord err)
{
#if (defined(_WIN32))
    SetLastError((DWORD)(err.AsObjPtr()->Get(0).AsUnsigned()));
#else
    errno = err.AsObjPtr()->Get(0).AsSigned();
#endif
    return 0;
}

// Create an external function reference.  The value returned has space for
// an address followed by the name of the external symbol.  Because the
// address comes at the beginning it can be used in the same way as the
// SysWord value returned by the get-symbol call from a library.
POLYUNSIGNED PolyFFICreateExtFn(FirstArgument threadId, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = creatEntryPointObject(taskData, pushedArg, true);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Create an external reference to data.  On a small number of platforms
// different forms of relocation are needed for data and for functions.
POLYUNSIGNED PolyFFICreateExtData(FirstArgument threadId, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = creatEntryPointObject(taskData, pushedArg, false);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}


// Called if a callback raises an exception.  There's nothing we
// can do because we don't have anything to pass back to C.
void PolyFFICallbackException()
{
    Crash("An ML function called from foreign code raised an exception.  Unable to continue.");
}

struct _entrypts polyFFIEPT[] =
{
    { "PolyFFIGeneral",                 (polyRTSFunction)&PolyFFIGeneral},
    { "PolySizeFloat",                  (polyRTSFunction)&PolySizeFloat},
    { "PolySizeDouble",                 (polyRTSFunction)&PolySizeDouble},
    { "PolyFFIGetError",                (polyRTSFunction)&PolyFFIGetError},
    { "PolyFFISetError",                (polyRTSFunction)&PolyFFISetError},
    { "PolyFFICreateExtFn",             (polyRTSFunction)&PolyFFICreateExtFn},
    { "PolyFFICreateExtData",           (polyRTSFunction)&PolyFFICreateExtData },
    { "PolyFFICallbackException",       (polyRTSFunction)&PolyFFICallbackException },

    { NULL, NULL} // End of list.
};

