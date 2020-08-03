/*
    Title:  osomem.cpp - Interface to OS memory management

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include "osmem.h"
#include "bitmap.h"
#include "locking.h"

// Linux prefers MAP_ANONYMOUS to MAP_ANON 
#ifndef MAP_ANON
#ifdef MAP_ANONYMOUS
#define MAP_ANON MAP_ANONYMOUS 
#endif
#endif


#ifdef POLYML32IN64

OSMem::OSMem()
{
    memBase = 0;
}

bool OSMem::Initialise(bool requiresExecute, size_t space /* = 0 */, void **pBase /* = 0 */)
{
    needExecute = requiresExecute;
    pageSize = PageSize();
    memBase = (char*)ReserveHeap(space);
    if (memBase == 0)
        return 0;

    if (pBase != 0) *pBase = memBase;

    // Create a bitmap with a bit for each page.
    if (!pageMap.Create(space / pageSize))
        return false;
    lastAllocated = space / pageSize; // Beyond the last page in the area
    // Set the last bit in the area so that we don't use it.
    // This is effectively a work-around for a problem with the heap.
    // If we have a zero-sized cell at the end of the memory its address is
    // going to be zero.  This causes problems with forwarding pointers.
    // There may be better ways of doing this.
    pageMap.SetBit(space / pageSize - 1);
    return true;
}

void *OSMem::AllocateDataArea(size_t& space)
{
    char *baseAddr;
    {
        PLocker l(&bitmapLock);
        uintptr_t pages = (space + pageSize - 1) / pageSize;
        // Round up to an integral number of pages.
        space = pages * pageSize;
        // Find some space
        while (pageMap.TestBit(lastAllocated - 1)) // Skip the wholly allocated area.
            lastAllocated--;
        uintptr_t free = pageMap.FindFree(0, lastAllocated, pages);
        if (free == lastAllocated)
            return 0; // Can't find the space.
        pageMap.SetBits(free, pages);
        // TODO: Do we need to zero this?  It may have previously been set.
        baseAddr = memBase + free * pageSize;
    }
    return CommitPages(baseAddr, space, false);
}

bool OSMem::FreeDataArea(void *p, size_t space)
{
    char *addr = (char*)p;
    uintptr_t offset = (addr - memBase) / pageSize;
    if (!UncommitPages(p, space))
        return false;
    uintptr_t pages = space / pageSize;
    {
        PLocker l(&bitmapLock);
        pageMap.ClearBits(offset, pages);
        if (offset + pages > lastAllocated) // We allocate from the top down.
            lastAllocated = offset + pages;
    }
    return true;
}

void * OSMem::AllocateCodeArea(size_t& space, void*& shadowArea)
{
    char* baseAddr;
    {
        PLocker l(&bitmapLock);
        uintptr_t pages = (space + pageSize - 1) / pageSize;
        // Round up to an integral number of pages.
        space = pages * pageSize;
        // Find some space
        while (pageMap.TestBit(lastAllocated - 1)) // Skip the wholly allocated area.
            lastAllocated--;
        uintptr_t free = pageMap.FindFree(0, lastAllocated, pages);
        if (free == lastAllocated)
            return 0; // Can't find the space.
        pageMap.SetBits(free, pages);
        // TODO: Do we need to zero this?  It may have previously been set.
        baseAddr = memBase + free * pageSize;
    }
    void *dataArea = CommitPages(baseAddr, space, needExecute);
    shadowArea = dataArea;
    return dataArea;
}

bool OSMem::FreeCodeArea(void* codeAddr, void* dataAddr, size_t space)
{
    ASSERT(codeAddr == dataAddr);
    char* addr = (char*)codeAddr;
    uintptr_t offset = (addr - memBase) / pageSize;
    if (!UncommitPages(codeAddr, space))
        return false;
    uintptr_t pages = space / pageSize;
    {
        PLocker l(&bitmapLock);
        pageMap.ClearBits(offset, pages);
        if (offset + pages > lastAllocated) // We allocate from the top down.
            lastAllocated = offset + pages;
    }
    return true;
}

#endif

#if (defined(HAVE_MMAP) && defined(MAP_ANON))
// We don't use autoconf's test for mmap here because that tests for
// file mapping.  Instead the test simply tests for the presence of an mmap
// function.
// We also insist that the OS supports MAP_ANON or MAP_ANONYMOUS.  Older
// versions of Solaris required the use of /dev/zero instead.  We don't
// support that.

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "polystring.h" // For TempCString

// How do we get the page size?
#ifndef HAVE_GETPAGESIZE
#ifdef _SC_PAGESIZE
#define getpagesize() sysconf(_SC_PAGESIZE)
#else
// If this fails we're stuck
#define getpagesize() PAGESIZE
#endif
#endif

#ifdef SOLARIS
#define FIXTYPE (caddr_t)
#else
#define FIXTYPE
#endif

#ifdef POLYML32IN64
// Unix-specific implementation of the subsidiary functions.

size_t OSMem::PageSize()
{
    return getpagesize();
}

void *OSMem::ReserveHeap(size_t space)
{
    return mmap(0, space, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
}

bool OSMem::UnreserveHeap(void *p, size_t space)
{
    return munmap(FIXTYPE p, space) == 0;
}

void *OSMem::CommitPages(void *baseAddr, size_t space, bool allowExecute)
{
    int prot = PROT_READ | PROT_WRITE;
    if (allowExecute) prot |= PROT_EXEC;
    if (mmap(baseAddr, space, prot, MAP_FIXED|MAP_PRIVATE|MAP_ANON, -1, 0) == MAP_FAILED)
        return 0;
    msync(baseAddr, space, MS_SYNC|MS_INVALIDATE);

    return baseAddr;
}

bool OSMem::UncommitPages(void *p, size_t space)
{
    // Remap the pages as new entries.  This should remove the old versions.
    if (mmap(p, space, PROT_NONE, MAP_FIXED|MAP_PRIVATE|MAP_ANON, -1, 0) == MAP_FAILED)
        return false;
    msync(p, space, MS_SYNC|MS_INVALIDATE);
    return true;
}

bool OSMem::EnableWrite(bool enable, void* p, size_t space)
{
    int res = mprotect(FIXTYPE p, space, enable ? PROT_READ|PROT_WRITE: PROT_READ);
    return res != -1;
}

bool OSMem::DisableWriteForCode(void* codeAddr, void* dataAddr, size_t space)
{
    int prot = PROT_READ;
    if (needExecute) prot |= PROT_EXEC;
    int res = mprotect(FIXTYPE codeAddr, space, prot);
    return res != -1;
}

#else
OSMem::OSMem()
{
    allocPtr = 0;
    needExecute = false;
    shadowFd = -1;
}

// Open a temporary file, unlink it and return the file descriptor.
static int openTmpFile(const char *dirName)
{
    const char *template_subdir =  "/mlMapXXXXXX";
    TempString buff((char *)malloc(strlen(dirName) + strlen(template_subdir) + 1));
    if (buff == 0) return -1; // Unable to allocate
    strcpy(buff, dirName);
    strcat(buff, template_subdir);
    int fd = mkstemp(buff);
    if (fd == -1) return -1;
    unlink(buff);
    return fd;
}

bool OSMem::Initialise(bool requiresExecute, size_t space /* = 0 */, void **pBase /* = 0 */)
{
    needExecute = requiresExecute;
    pageSize = getpagesize();
    if (! needExecute) return true;
    // Can we allocate memory with write+execute?
    int fd = -1; // This value is required by FreeBSD.  Linux doesn't care
    void *test = mmap(0, pageSize, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANON, fd, 0);
    if (test != MAP_FAILED)
    {
        // Don't require shadow area
        munmap(FIXTYPE test, pageSize);
        return true;
    }
    if (errno != ENOTSUP) // This fails with ENOTSUPP on OpenBSD.
        return false;
    // Check that read-write works.
    test = mmap(0, pageSize, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, fd, 0);
    if (test == MAP_FAILED)
        return false; // There's a problem.
    munmap(FIXTYPE test, pageSize);
    // Need to create a file descriptor for mapping.
    char *tmpDir = getenv("TMPDIR");
    if (tmpDir != NULL)
    {
        shadowFd = openTmpFile(tmpDir);
        if (shadowFd != -1) return true;
    }
#ifdef P_tmpdir
    shadowFd = openTmpFile(P_tmpdir);
    if (shadowFd != -1) return true;
#endif
    shadowFd = openTmpFile("/tmp");
    if (shadowFd != -1) return true;
    shadowFd = openTmpFile("/var/tmp");
    if (shadowFd != -1) return true;
    return false;
}

// Allocate space and return a pointer to it.  The size is the minimum
// size requested and it is updated with the actual space allocated.
// Returns NULL if it cannot allocate the space.
void *OSMem::AllocateDataArea(size_t &space)
{
    // Round up to an integral number of pages.
    space = (space + pageSize-1) & ~(pageSize-1);
    int fd = -1; // This value is required by FreeBSD.  Linux doesn't care
    void *result = mmap(0, space, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, fd, 0);
    // Convert MAP_FAILED (-1) into NULL
    if (result == MAP_FAILED)
        return 0;
    return result;
}

// Release the space previously allocated.  This must free the whole of
// the segment.  The space must be the size actually allocated.
bool OSMem::FreeDataArea(void *p, size_t space)
{
    return munmap(FIXTYPE p, space) == 0;
}

bool OSMem::EnableWrite(bool enable, void* p, size_t space)
{
    int res = mprotect(FIXTYPE p, space, enable ? PROT_READ|PROT_WRITE: PROT_READ);
    return res != -1;
}

void *OSMem::AllocateCodeArea(size_t &space, void*& shadowArea)
{
    // Round up to an integral number of pages.
    space = (space + pageSize-1) & ~(pageSize-1);

    if (shadowFd == -1)
    {
        int fd = -1; // This value is required by FreeBSD.  Linux doesn't care
        int prot = PROT_READ | PROT_WRITE;
        if (needExecute) prot |= PROT_EXEC;
        void *result = mmap(0, space, prot, MAP_PRIVATE|MAP_ANON, fd, 0);
        // Convert MAP_FAILED (-1) into NULL
        if (result == MAP_FAILED)
            return 0;
        shadowArea = result;
        return result;
    }

    // Have to use dual areas.
    size_t allocAt;
    {
        PLocker lock(&allocLock);
        allocAt = allocPtr;
        allocPtr += space;
    }
    if (ftruncate(shadowFd, allocAt + space) == -1)
        return 0;
    void *readExec = mmap(0, space, PROT_READ|PROT_EXEC, MAP_SHARED, shadowFd, allocAt);
    if (readExec == MAP_FAILED)
        return 0;
    void *readWrite = mmap(0, space, PROT_READ|PROT_WRITE, MAP_SHARED, shadowFd, allocAt);
    if (readWrite == MAP_FAILED)
    {
        munmap(FIXTYPE readExec, space);
        return 0;
    }
    shadowArea = readWrite;
    return readExec;
}

bool OSMem::FreeCodeArea(void *codeArea, void *dataArea, size_t space)
{
    bool freeCode = munmap(FIXTYPE codeArea, space) == 0;
    if (codeArea == dataArea) return freeCode;
    return (munmap(FIXTYPE dataArea, space) == 0) & freeCode;
}

bool OSMem::DisableWriteForCode(void* codeAddr, void* dataAddr, size_t space)
{
    int prot = PROT_READ;
    if (needExecute) prot |= PROT_EXEC;
    int res = mprotect(FIXTYPE codeAddr, space, prot);
    return res != -1;
}

#endif

#elif defined(_WIN32)
// Use Windows memory management.
#include <windows.h>

#ifdef POLYML32IN64

// Windows-specific implementations of the subsidiary functions.
size_t OSMem::PageSize()
{
    // Get the page size and round up to that multiple.
    SYSTEM_INFO sysInfo;
    GetSystemInfo(&sysInfo);
    // Get the page size.  Put it in a size_t variable otherwise the rounding
    // up of "space" may go wrong on 64-bits.
    return sysInfo.dwPageSize;
}

void *OSMem::ReserveHeap(size_t space)
{
    void *memBase = VirtualAlloc(0, space, MEM_RESERVE, PAGE_NOACCESS);
    if (memBase == 0) return 0;
    // We need the heap to be such that the top 32-bits are non-zero.
    if ((uintptr_t)memBase >= ((uintptr_t)1 << 32))
        return memBase;
    // Allocate again.
    void *newSpace = ReserveHeap(space);
    UnreserveHeap(memBase, space); // Free the old area that isn't suitable.
    // Return what we got, or zero if it failed.
    return newSpace;
}

bool OSMem::UnreserveHeap(void *p, size_t space)
{
    return VirtualFree(p, 0, MEM_RELEASE) == TRUE;
}

void *OSMem::CommitPages(void *baseAddr, size_t space, bool allowExecute)
{
    return VirtualAlloc(baseAddr, space, MEM_COMMIT, allowExecute ? PAGE_EXECUTE_READWRITE : PAGE_READWRITE);
}

bool OSMem::UncommitPages(void *baseAddr, size_t space)
{
    return VirtualFree(baseAddr, space, MEM_DECOMMIT) == TRUE;
}

bool OSMem::EnableWrite(bool enable, void* p, size_t space)
{
    DWORD oldProtect;
    return VirtualProtect(p, space, enable ? PAGE_READWRITE : PAGE_READONLY, &oldProtect) == TRUE;
}

bool OSMem::DisableWriteForCode(void* codeAddr, void* dataAddr, size_t space)
{
    ASSERT(codeAddr == dataAddr);
    DWORD oldProtect;
    return VirtualProtect(codeAddr, space, needExecute ? PAGE_EXECUTE_READ : PAGE_READONLY, &oldProtect) == TRUE;
}

#else

OSMem::OSMem()
{
}

bool OSMem::Initialise(bool requiresExecute, size_t space /* = 0 */, void **pBase /* = 0 */)
{
    needExecute = requiresExecute;
    // Get the page size and round up to that multiple.
    SYSTEM_INFO sysInfo;
    GetSystemInfo(&sysInfo);
    // Get the page size.  Put it in a size_t variable otherwise the rounding
    // up of "space" may go wrong on 64-bits.
    pageSize = sysInfo.dwPageSize;
    return true;
}

// Allocate space and return a pointer to it.  The size is the minimum
// size requested and it is updated with the actual space allocated.
// Returns NULL if it cannot allocate the space.
void *OSMem::AllocateDataArea(size_t &space)
{
    space = (space + pageSize - 1) & ~(pageSize - 1);
    DWORD options = MEM_RESERVE | MEM_COMMIT;
    return VirtualAlloc(0, space, options, PAGE_READWRITE);
}

// Release the space previously allocated.  This must free the whole of
// the segment.  The space must be the size actually allocated.
bool OSMem::FreeDataArea(void *p, size_t space)
{
    return VirtualFree(p, 0, MEM_RELEASE) == TRUE;
}

// Adjust the permissions on a segment.  This must apply to the
// whole of a segment.
bool OSMem::EnableWrite(bool enable, void* p, size_t space)
{
    DWORD oldProtect;
    return VirtualProtect(p, space, enable ? PAGE_READWRITE: PAGE_READONLY, &oldProtect) == TRUE;
}

void* OSMem::AllocateCodeArea(size_t& space, void*& shadowArea)
{
    space = (space + pageSize - 1) & ~(pageSize - 1);
    DWORD options = MEM_RESERVE | MEM_COMMIT;
    void * dataAddr = VirtualAlloc(0, space, options, needExecute ? PAGE_EXECUTE_READWRITE : PAGE_READWRITE);
    shadowArea = dataAddr;
    return dataAddr;
}

bool OSMem::FreeCodeArea(void* codeAddr, void* dataAddr, size_t space)
{
    ASSERT(codeAddr == dataAddr);
    return VirtualFree(codeAddr, 0, MEM_RELEASE) == TRUE;
}

bool OSMem::DisableWriteForCode(void* codeAddr, void* dataAddr, size_t space)
{
    ASSERT(codeAddr == dataAddr);
    DWORD oldProtect;
    return VirtualProtect(codeAddr, space, needExecute ? PAGE_EXECUTE_READ : PAGE_READONLY, &oldProtect) == TRUE;
}

#endif

#else

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef POLYML32IN64
#error "32 bit in 64-bits requires either mmap or VirtualAlloc"
#endif

// Use calloc to allocate the memory.  Using calloc ensures the memory is
// zeroed and is compatible with the other allocators.
void *OSMem::Allocate(size_t &bytes, unsigned permissions)
{
    return calloc(bytes, 1);
}

bool OSMem::Free(void *p, size_t/*space*/)
{
    free(p);
    return true;
}

// We can't do this if we don't have mprotect.
bool OSMem::SetPermissions(void *p, size_t space, unsigned permissions)
{
    return true; // Let's hope this is all right.
}

#endif
