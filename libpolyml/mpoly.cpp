/*
    Title:      Main program

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

#ifdef _WIN32_WCE
#include "winceconfig.h"
#include "wincelib.h"
#else
#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
#endif
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#ifdef HAVE_TCHAR_H
#include <tchar.h>
#endif

#include "globals.h"
#include "sys.h"
#include "gc.h"
#include "run_time.h"
#include "machine_dep.h"
#include "version.h"
#include "diagnostics.h"
#include "processes.h"
#include "mpoly.h"
#include "scanaddrs.h"
#include "save_vec.h"
#include "polyexports.h"
#include "memmgr.h"
#include "pexport.h"

#ifdef WINDOWS_PC
#include "Console.h"
#endif

static void  InitHeaderFromExport(exportDescription *exports);
NORETURNFN(static void Usage(char *message));

// Return the entry in the io vector corresponding to the Poly system call.
PolyWord *IoEntry(unsigned sysOp)
{
    MemSpace *space = gMem.IoSpace();
    return space->bottom + sysOp * IO_SPACING;
}

/* This macro must make a whole number of chunks */
#define K_to_words(k) ROUNDUP((k) * (1024 / sizeof(PolyWord)),BITSPERWORD)

struct _userOptions userOptions;

UNSIGNEDADDR exportTimeStamp;

#ifdef INTERPRETED
#define ARCH "Portable-"
#elif WINDOWS_PC
#define ARCH "Windows-"
#elif defined(i386)
#define ARCH "I386-"
#elif defined(SPARC)
#define ARCH "Sparc-"
#elif defined(POWER2)
#define ARCH "PPC-"
#elif defined(X86_64)
#define ARCH "X86_64-"
#endif

char *poly_runtime_system_version = ARCH TextVersion;

char *poly_runtime_system_copyright =
"Copyright (c) 2002-6 CUTS, David C.J. Matthews and contributors.";

unsigned hsize, isize, msize;

struct __argtab {
    const char *argName, *argHelp;
    unsigned scale, *argVal;
} argTable[] =
{
    { "-H",             "Initial heap size (MB)",                1024,   &hsize }, // Leave this for the moment
    { "--heap",         "Initial heap size (MB)",                1024,   &hsize },
    { "--immutable",    "Initial size of immutable buffer (MB)", 1024,   &isize },
    { "--mutable",      "Initial size of mutable buffer(MB)",    1024,   &msize },
    { "--debug",        "Debug options",                         1,      &userOptions.debug },
    { "--timeslice",    "Time slice (ms)",                       1,      &userOptions.timeslice }
};


/* In the Windows version this is called from WinMain in Console.c */
int polymain(int argc, char **argv, exportDescription *exports)
{

    POLYUNSIGNED memsize = GetPhysicalMemorySize();
    if (memsize == 0) // Unable to determine memory size so default to 64M.
        memsize = 64 * 1024 * 1024;

#ifdef _WIN32_WCE
    hsize = memsize / 4 / 1024;
    isize = 3 * 1024;
    msize = 1 * 1024;
#else
    // Set the default initial size to half the memory.
    hsize = memsize / 2 / 1024;
    isize = 0; /* use standard default */
    msize = 0; /* use standard default */
#endif
    /* Get arguments. */
    memset(&userOptions, 0, sizeof(userOptions)); /* Reset it */

    // Get the program name for CommandLine.name.  This is allowed to be a full path or
    // just the last component so we return whatever the system provides.
    if (argc > 0) 
        userOptions.programName = argv[0];
    else
        userOptions.programName = ""; // Set it to a valid empty string
    
    char *importFileName = 0;
    userOptions.debug       = 0;
    userOptions.noDisplay   = false;
    userOptions.timeslice   = 1000; /* default timeslice = 1000ms */

    userOptions.user_arg_count   = 0;
    userOptions.user_arg_strings = (char**)malloc(argc * sizeof(char*)); // Enough room for all of them

    // Process the argument list removing those recognised by the RTS and adding the
    // remainder to the user argument list.
    for (int i = 1; i < argc; i++)
    {
        if (argv[i][0] == '-')
        {
            bool argUsed = false;
            for (unsigned j = 0; j < sizeof(argTable)/sizeof(argTable[0]); j++)
            {
                unsigned argl = strlen(argTable[j].argName);
                if (strncmp(argv[i], argTable[j].argName, argl) == 0)
                {
                    char *p;
                    if (strlen(argv[i]) == argl)
                    { // If it has used all the argument pick the next
                        i++;
                        p = argv[i];
                    }
                    else
                    {
                        p = argv[i]+argl;
                        if (*p == '=') p++; // Skip an equals sign
                    }
                    if (i >= argc)
                        printf("Incomplete %s option\n", argTable[j].argName);
                    else
                        *(argTable[j].argVal) = atoi(p) * argTable[j].scale;
                    argUsed = true;
                    break;
                }
            }
            if (! argUsed) // Add it to the user args.
                userOptions.user_arg_strings[userOptions.user_arg_count++] = argv[i];
        }
        else if (exports == 0 && importFileName == 0)
            importFileName = argv[i];
        else
            userOptions.user_arg_strings[userOptions.user_arg_count++] = argv[i];
    }

    if (exports == 0 && importFileName == 0)
        Usage("Missing import file name");
    
    if (hsize < 500) Usage ("Invalid heap-size value");
    
    // DCJM: Now allow a timeslice of 0 to disable asynchronous interrupts.
    // They make it very difficult to single step in the debugger.
    if (userOptions.timeslice < 0 || userOptions.timeslice > 10000)
        Usage ("Timeslice must be in the range 0 .. 10000ms");
    
    if (hsize < isize) hsize = isize;
    if (hsize < msize) hsize = msize;
    
    if (msize == 0) msize = 4 * 1024 + hsize / 5;  /* set default mutable buffer size */
    if (isize == 0) isize = hsize - msize;  /* set default immutable buffer size */
    
    // Set the heap size and segment sizes.  We allocate in units of this size,
    userOptions.heapSize           = K_to_words(hsize);
    userOptions.immutableSegSize   = K_to_words(isize);
    userOptions.mutableSegSize     = K_to_words(msize);

    // The space we need to have free at the end of a partial collection.  If we have less
    // than this we do a full GC.
    // For an immutable area this is zero.  For the mutable area, though, this is 80% of the
    // mutable segment size since we allocate new objects in the mutable area and this
    // determines how soon we will need to do another GC.
    userOptions.immutableMinFree = 0;
    userOptions.mutableMinFree = userOptions.mutableSegSize - userOptions.mutableSegSize / 5;

    // This is the space we try to have free at the end of a major collection.  If
    // we have less than this we allocate another segment.
    userOptions.immutableFreeSpace = userOptions.immutableSegSize/2; // 50% full
    if (userOptions.immutableFreeSpace < userOptions.immutableMinFree)
        userOptions.immutableFreeSpace = userOptions.immutableMinFree;
    // For the mutable area it is 90% of the segment size.
    userOptions.mutableFreeSpace   = userOptions.mutableSegSize - userOptions.mutableSegSize/10;
    if (userOptions.mutableFreeSpace < userOptions.mutableMinFree)
        userOptions.mutableFreeSpace = userOptions.mutableMinFree;
   
    /* initialise the run-time system before opening the database */
    init_run_time_system();
    
    CreateHeap();
    
    PolyObject *rootFunction = 0;

    if (exports != 0)
    {
        InitHeaderFromExport(exports);
        rootFunction = (PolyObject *)exports->rootFunction;
    }
    else
    {
        if (importFileName != 0)
            rootFunction = ImportPortable(importFileName);
        if (rootFunction == 0)
            exit(1);
    }
   
    /* Initialise the interface vector. */
    machineDependent->InitInterfaceVector(); /* machine dependent entries. */
    
    // This word has a zero value and is used for null strings.
    add_word_to_io_area(POLY_SYS_emptystring, PolyWord::FromUnsigned(0));
    
    // This is used to represent zero-sized vectors.
    // N.B. This assumes that the word before is zero because it's
    // actually the length word we want to be zero here.
    add_word_to_io_area(POLY_SYS_nullvector, PolyWord::FromUnsigned(0));
    
    /* The standard input and output streams are persistent i.e. references
       to the the standard input in one session will refer to the standard
       input in the next. */
    add_word_to_io_area(POLY_SYS_stdin,  PolyWord::FromUnsigned(0));
    add_word_to_io_area(POLY_SYS_stdout, PolyWord::FromUnsigned(1));
    add_word_to_io_area(POLY_SYS_stderr, PolyWord::FromUnsigned(2));
    
    re_init_run_time_system();
    
    // Set up the initial process to run the root function.
    processes->set_process_list(rootFunction);
    
    (void)EnterPolyCode(); // Will normally (always?) call finish directly
    finish(0);
    
    /*NOTREACHED*/
    return 0; /* just to keep lint happy */
}

void Uninitialise(void)
// Close down everything and free all resources.  Stop any threads or timers.
{
    uninit_run_time_system();
}

void finish (int n)
{
    // Make sure we don't get any interrupts once the destructors are
    // applied to globals or statics.
    Uninitialise();
#if defined(WINDOWS_PC)
    ExitThread(n);
#else
    exit (n);
#endif
}

// Print a message and exit if an argument is malformed.
void Usage(char *message)
{
    if (message)
        printf("%s\n", message);
    for (unsigned j = 0; j < sizeof(argTable)/sizeof(argTable[0]); j++)
    {
        printf("%s <%s>\n", argTable[j].argName, argTable[j].argHelp);
    }
    fflush(stdout);
    
#if defined(WINDOWS_PC)
    if (useConsole)
    {
        MessageBox(hMainWindow, _T("Poly/ML has exited"), _T("Poly/ML"), MB_OK);
    }
#endif
    exit (1);
}

// Return a string containing the argument names.  Can be printed out in response
// to a --help argument.  It is up to the ML application to do that since it may well
// want to produce information about any arguments it chooses to process.
char *RTSArgHelp(void)
{
    static char buff[2000];
    char *p = buff;
    for (unsigned j = 0; j < sizeof(argTable)/sizeof(argTable[0]); j++)
    {
        int spaces = sprintf(p, "%s <%s>\n", argTable[j].argName, argTable[j].argHelp);
        p += spaces;
    }
    ASSERT((unsigned)(p - buff) < (unsigned)sizeof(buff));
    return buff;
}

void InitHeaderFromExport(exportDescription *exports)
{
    // Check the structure sizes stored in the export structure match the versions
    // used in this library.
    if (exports->structLength != sizeof(exportDescription) ||
        exports->memTableSize != sizeof(memoryTableEntry))
    {
        Exit("The exported object file does not match this version of the library");
    }
    // We could also check the RTS version and the architecture.
    exportTimeStamp = exports->timeStamp; // Needed for load and save.

    memoryTableEntry *memTable = exports->memTable;
    for (unsigned i = 0; i < exports->memTableEntries; i++)
    {
        // Construct a new space for each of the entries.
        if (i == exports->ioIndex)
            (void)gMem.InitIOSpace((PolyWord*)memTable[i].mtAddr, memTable[i].mtLength/sizeof(PolyWord));
        else
            (void)gMem.NewPermanentSpace((PolyWord*)memTable[i].mtAddr,
                memTable[i].mtLength/sizeof(PolyWord), (memTable[i].mtFlags & MTF_WRITEABLE) != 0);
    }
}
