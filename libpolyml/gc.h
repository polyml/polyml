/*
    Title:  gc.h - exports signature for gc.cpp

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

    Further development Copyright David C.J. Matthews 2010

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
extern void CreateHeap(unsigned hsize, unsigned isize, unsigned msize, unsigned rsize);

extern bool convertedWeak;

extern POLYUNSIGNED GetPhysicalMemorySize(void);

// Multi-thread GC.
extern bool doMultithreadGC(bool doFullGC, const POLYUNSIGNED wordsRequiredToAllocate);
extern void initialiseMultithreadGC(unsigned threads);

// The task farm for the GC.  The threads are left waiting for the GC,
class GCTaskFarm;
extern GCTaskFarm *gpTaskFarm;

// GC Phases.
extern void GCMarkPhase(void);
extern void GCheckWeakRefs(void);
extern void GCCopyPhase(POLYUNSIGNED &immutable_overflow);
extern void GCUpdatePhase(void);

// The multi-thread GC needs the heap to be broken into multiple segments so
// that the segments can be processed in parallel during the copy and update phases.
// This defines the minimum number of segments for each thread. 
#define MIN_MUTABLE_SEGS_PER_THREAD     2
#define MIN_IMMUTABLE_SEGS_PER_THREAD   2

#endif
