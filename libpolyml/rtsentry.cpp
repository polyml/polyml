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

#include "save_vec.h"
#include "polystring.h"
#include "processes.h"
#include "run_time.h"
#include "globals.h"

extern "C" {
    POLYUNSIGNED PolyChDir();
    POLYUNSIGNED PolyFinish();
}

static struct _entrypts {
    const char *name;
    POLYUNSIGNED (*entry)();
} entryPtTable[] =
{
    { "PolyChDir", &PolyChDir},
    { "PolyFinish", &PolyFinish}
};

// Find an entry point in the table.  We could use the system to find these because
// each entry is a system entry point but we need the table to do the reverse lookup.
Handle getEntryPoint(TaskData *taskData, Handle arg)
{
    TempCString entryName(Poly_string_to_C_alloc(arg->WordP()));
    if ((const char *)entryName == 0) raise_syscall(taskData, "Insufficient memory", ENOMEM);

    for (unsigned i = 0; i < sizeof(entryPtTable)/sizeof(entryPtTable[0]); i++)
    {
        if (strcmp(entryName, entryPtTable[i].name) == 0)
            return Make_sysword(taskData, (uintptr_t)entryPtTable[i].entry);
    }
    raise_fail(taskData, "entry point not found");
}
