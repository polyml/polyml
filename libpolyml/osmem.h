/*
    Title:  osomem.h - Interface to OS memory management

    Copyright (c) 2006, 2017-18, 2020 David C.J. Matthews

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

#ifndef OS_MEM_H_INCLUDED
#define OS_MEM_H_INCLUDED


// We need size_t so include these two here.
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef POLYML32IN64
#include "bitmap.h"
#include "locking.h"
#endif

// This class provides access to the memory management provided by the
// operating system.  It would be nice if we could always use malloc and
// free for this but we need to have execute permission on the code
// objects.

class OSMem {

public:
    OSMem() {}
    ~OSMem() {}

    bool Initialise(bool requiresExecute, size_t space = 0, void** pBase = 0);

    // Allocate space and return a pointer to it.  The size is the minimum
    // size requested in bytes and it is updated with the actual space allocated.
    // Returns NULL if it cannot allocate the space.
    void *AllocateDataArea(size_t& bytes);

    // Release the space previously allocated.  This must free the whole of
    // the segment.  The space must be the size actually allocated.
    bool FreeDataArea(void* p, size_t space);

    // Enable/disable writing.  This must apply to the whole of a segment.
    // Only for data areas.
    bool EnableWrite(bool enable, void* p, size_t space);

    // Allocate code area.  Some systems will not allow both write and execute permissions
    // on the same page.  On those systems we have to allocate two regions of shared memory,
    // one with read+execute permission and the other with read+write.
    void *AllocateCodeArea(size_t& bytes, void*& shadowArea);

    // Free the allocated areas.
    bool FreeCodeArea(void* codeAddr, void* dataAddr, size_t space);

    // Remove write access.  This is used after the permanent code area has been created
    // either from importing a portable export file or copying the area in 32-in-64.
    bool DisableWriteForCode(void* codeAddr, void* dataAddr, size_t space);

protected:
    size_t pageSize;

#ifdef POLYML32IN64
    size_t PageSize();
    void* ReserveHeap(size_t space);
    bool UnreserveHeap(void* baseAddr, size_t space);
    void* CommitPages(void* baseAddr, size_t space, bool allowExecute);
    bool UncommitPages(void* baseAddr, size_t space);

    Bitmap pageMap;
    uintptr_t lastAllocated;
    char* memBase;
    PLock bitmapLock;
#endif

};

#endif
