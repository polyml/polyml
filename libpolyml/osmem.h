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

#include "bitmap.h"
#include "locking.h"


// This class provides access to the memory management provided by the
// operating system.  It would be nice if we could always use malloc and
// free for this but we need to have execute permission on the code
// objects.

class OSMem {

public:
    OSMem() {}
    virtual ~OSMem() {}

    enum _MemUsage {
        UsageData,          // Data or code in the interpreted version
        UsageStack,         // Stack
        UsageExecutableCode // Code in the native code versions.
    };

    // Allocate space and return a pointer to it.  The size is the minimum
    // size requested in bytes and it is updated with the actual space allocated.
    // Returns NULL if it cannot allocate the space.
    virtual void *AllocateDataArea(size_t& bytes) = 0;

    // Release the space previously allocated.  This must free the whole of
    // the segment.  The space must be the size actually allocated.
    virtual bool FreeDataArea(void* p, size_t space) = 0;

    // Enable/disable writing.  This must apply to the whole of a segment.
    // Only for data areas.
    virtual bool EnableWrite(bool enable, void* p, size_t space) = 0;

    // Allocate code area.  Some systems will not allow both write and execute permissions
    // on the same page.  On those systems we have to allocate two regions of shared memory,
    // one with read+execute permission and the other with read+write.
    virtual void *AllocateCodeArea(size_t& bytes, void*& shadowArea) = 0;

    // Free the allocated areas.
    virtual bool FreeCodeArea(void* codeAddr, void* dataAddr, size_t space) = 0;

    // Remove write access.  This is used after the permanent code area has been created
    // either from importing a portable export file or copying the area in 32-in-64.
    virtual bool DisableWriteForCode(void* codeAddr, void* dataAddr, size_t space) = 0;

protected:
    size_t pageSize;
    enum _MemUsage memUsage;

};

// Allows the system to allocate pages.
class OSMemUnrestricted: public OSMem {

public:
    OSMemUnrestricted();
    virtual ~OSMemUnrestricted();

    bool Initialise(enum _MemUsage usage);
    virtual void* AllocateDataArea(size_t& bytes);
    virtual bool FreeDataArea(void* p, size_t space);
    virtual bool EnableWrite(bool enable, void* p, size_t space);
    virtual void* AllocateCodeArea(size_t& bytes, void*& shadowArea);
    virtual bool FreeCodeArea(void* codeAddr, void* dataAddr, size_t space);
    virtual bool DisableWriteForCode(void* codeAddr, void* dataAddr, size_t space);

protected:

#ifndef _WIN32
    // If we need to use dual areas because WRITE+EXECUTE permission is not allowed.
    int shadowFd;
    PLock allocLock;
    size_t allocPtr;
#endif

};

// Pages are allocated within a region.  This is used for 32-in-64 and
// for code in native X86 64-bit.
class OSMemInRegion: public OSMem {

public:
    OSMemInRegion();
    virtual ~OSMemInRegion();

    bool Initialise(enum _MemUsage usage, size_t space, void** pBase);
    virtual void* AllocateDataArea(size_t& bytes);
    virtual bool FreeDataArea(void* p, size_t space);
    virtual bool EnableWrite(bool enable, void* p, size_t space);
    virtual void* AllocateCodeArea(size_t& bytes, void*& shadowArea);
    virtual bool FreeCodeArea(void* codeAddr, void* dataAddr, size_t space);
    virtual bool DisableWriteForCode(void* codeAddr, void* dataAddr, size_t space);

protected:

#ifndef _WIN32
    // If we need to use dual areas because WRITE+EXECUTE permission is not allowed.
    int shadowFd;
    PLock allocLock;
    size_t allocPtr;
#endif

    Bitmap pageMap;
    uintptr_t lastAllocated;
    char* memBase, * shadowBase;
    PLock bitmapLock;
};

#endif
