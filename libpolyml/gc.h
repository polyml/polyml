/*
    Title:  gc.h - exports signature for gc.cpp

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

#ifndef GC_H_INCLUDED
#define GC_H_INCLUDED

class StackObject;
#include "globals.h" // For POLYUNSIGNED

class TaskData;

extern void CopyStackFrame(StackObject *old_stack, StackObject *new_stack);

// Make a request for a full garbage collection.
extern void FullGC(TaskData *taskData);
// Make a request for a partial garbage collection.
extern bool QuickGC(TaskData *taskData, POLYUNSIGNED words_needed);
extern void CreateHeap(unsigned hsize, unsigned isize, unsigned msize);

extern bool convertedWeak;

#endif
