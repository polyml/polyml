/*
    Title:  savestate.cpp - Save and Load state

    Copyright (c) 2007 David C.J. Matthews

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

#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#include "globals.h"
#include "savestate.h"
#include "processes.h"
#include "run_time.h"
#include "sys.h"
#include "polystring.h"

#define SAVE(x) taskData->saveVec.push(x)

Handle SaveState(TaskData *taskData, Handle args)
// Write a saved state file.
{
    raise_exception_string(taskData, EXC_Fail, "SaveState is not yet implemented");
    return SAVE(TAGGED(0));
}

Handle LoadState(TaskData *taskData, Handle hFileName)
// Load a saved state file and any ancestors.
{
    raise_exception_string(taskData, EXC_Fail, "LoadState is not yet implemented");
    return SAVE(TAGGED(0));
}

Handle ShowHierarchy(TaskData *taskData)
// Show the hierarchy.
{
    Handle list = SAVE(ListNull);
    // Not yet implemented: always return empty list.
    return list;
}

Handle RenameParent(TaskData *taskData, Handle args)
// Change the name of the immediate parent stored in a child
{
    raise_exception_string(taskData, EXC_Fail, "RenameParent is not yet implemented");
    return SAVE(TAGGED(0));
}

Handle ShowParent(TaskData *taskData, Handle hFileName)
// Return the name of the immediate parent stored in a child
{
    raise_exception_string(taskData, EXC_Fail, "ShowParent is not yet implemented");
    return SAVE(C_string_to_Poly(taskData, ""));
}
