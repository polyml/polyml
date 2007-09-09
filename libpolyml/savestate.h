/*
    Title:  savestate.h - Save and Load state

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

#ifndef SAVESTATE_H_INCLUDED
#define SAVESTATE_H_INCLUDED

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

// Write a saved state file.
Handle SaveState(TaskData *taskData, Handle args);

// Load a saved state file and any ancestors.
Handle LoadState(TaskData *taskData, Handle hFileName);

// Show the hierarchy.
Handle ShowHierarchy(TaskData *taskData);

// Change the name of the immediate parent stored in a child
Handle RenameParent(TaskData *taskData, Handle args);

// Return the name of the immediate parent stored in a child
Handle ShowParent(TaskData *taskData, Handle hFileName);

#endif

