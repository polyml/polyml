/*
    Title:  osomem.cpp - Interface to OS memory management - Unix version

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
#else
#error "No configuration file"
#endif

#if defined __linux__ && !defined _GNU_SOURCE
// _GNU_SOURCE must be defined before #include <fcntl.h> to get O_TEMPFILE etc.
#define _GNU_SOURCE 1
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

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

// Linux prefers MAP_ANONYMOUS to MAP_ANON
#ifndef MAP_ANON
#ifdef MAP_ANONYMOUS
#define MAP_ANON MAP_ANONYMOUS
#endif
#endif

// Assume that mmap is supported.  If it isn't we can't run.

#include "osmem.h"
#include "bitmap.h"
#include "locking.h"
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

// Open a temporary file, unlink it and return the file descriptor.
static int openTmpFile(const char* dirName)
{
#ifdef O_TMPFILE
    int flags = 0;
#ifdef O_CLOEXEC
    flags |= O_CLOEXEC;
#endif
    int tfd = open(dirName, flags | O_TMPFILE | O_RDWR | O_EXCL, 0700);
    if (tfd != -1)
        return tfd;
#endif
    const char* template_subdir = "/mlMapXXXXXX";
    TempString buff((char*)malloc(strlen(dirName) + strlen(template_subdir) + 1));
    if (buff == 0) return -1; // Unable to allocate
    strcpy(buff, dirName);
    strcat(buff, template_subdir);
    int fd = mkstemp(buff);
    if (fd == -1) return -1;
    unlink(buff);
    return fd;
}

static int createTemporaryFile()
{
    char *tmpDir = getenv("TMPDIR");
    int fd;
    if (tmpDir != NULL)
    {
        fd = openTmpFile(tmpDir);
        if (fd != -1) return fd;
    }
#ifdef P_tmpdir
    fd = openTmpFile(P_tmpdir);
    if (fd != -1) return fd;
#endif
    fd = openTmpFile("/tmp");
    if (fd != -1) return fd;
    fd = openTmpFile("/var/tmp");
    if (fd != -1) return fd;

    return -1;
}

#ifdef POLYML32IN64
OSMem::OSMem()
{
    memBase = 0;
    shadowFd = -1;
}

OSMem::~OSMem()
{
}

bool OSMem::Initialise(enum _MemUsage usage, size_t space /* = 0 */, void** pBase /* = 0 */)
{
    memUsage = usage;
    pageSize = getpagesize();
    bool simpleMmap;
    if (usage != UsageExecutableCode) simpleMmap = true;
    else
    {
        // Can we allocate memory with write+execute?
        void *test = mmap(0, pageSize, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANON, -1, 0);
        if (test != MAP_FAILED)
        {
            munmap(FIXTYPE test, pageSize);
            simpleMmap = true;
        }
        else simpleMmap = false;
    }
    
    if (simpleMmap)
    {
        // Don't require shadow area.  Can use mmap
        memBase = (char*)mmap(0, space, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
        if (memBase == MAP_FAILED) return false;
        // We need the heap to be such that the top 32-bits are non-zero.
        if ((uintptr_t)memBase < ((uintptr_t)1 << 32))
        {
            // Allocate again.
            void* newSpace = mmap(0, space, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
            munmap(FIXTYPE memBase, space); // Free the old area that isn't suitable.
            // Return what we got, or zero if it failed.
            memBase = (char*)newSpace;
        }
        shadowBase = memBase;
    }
    else
    {
        // More difficult - require file mapping
        shadowFd = createTemporaryFile();
        if (shadowFd == -1) return false;
        if (ftruncate(shadowFd, space) == -1) return false;
        void *readWrite = mmap(0, space, PROT_NONE, MAP_SHARED, shadowFd, 0);
        if (readWrite == MAP_FAILED) return 0;
        memBase = (char*)mmap(0, space, PROT_NONE, MAP_SHARED, shadowFd, 0);
        if (memBase == MAP_FAILED)
        {
            munmap(FIXTYPE readWrite, space);
            return false;
        }
        // This should be above 32-bits.
        ASSERT((uintptr_t)memBase >= ((uintptr_t)1 << 32));
        shadowBase = (char*)readWrite;
    }

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

void* OSMem::AllocateDataArea(size_t& space)
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
    int prot = PROT_READ | PROT_WRITE;
    int flags = MAP_FIXED | MAP_PRIVATE | MAP_ANON;
#if defined(MAP_STACK) && defined(__OpenBSD__)
    // On OpenBSD the stack must be mapped with MAP_STACK otherwise it
    // segfaults.  On FreeBSD, though, this isn't necessary and causes problems.
    if (memUsage == UsageStack) flags |= MAP_STACK;
#endif
    if (mmap(baseAddr, space, prot, flags, -1, 0) == MAP_FAILED)
        return 0;
    msync(baseAddr, space, MS_SYNC | MS_INVALIDATE);
    return baseAddr;
}

bool OSMem::FreeDataArea(void* p, size_t space)
{
    char* addr = (char*)p;
    uintptr_t offset = (addr - memBase) / pageSize;
    // Remap the pages as new entries.  This should remove the old versions.
    if (mmap(p, space, PROT_NONE, MAP_FIXED | MAP_PRIVATE | MAP_ANON, -1, 0) == MAP_FAILED)
        return false;
    msync(p, space, MS_SYNC | MS_INVALIDATE);
    uintptr_t pages = space / pageSize;
    {
        PLocker l(&bitmapLock);
        pageMap.ClearBits(offset, pages);
        if (offset + pages > lastAllocated) // We allocate from the top down.
            lastAllocated = offset + pages;
    }
    return true;
}

void* OSMem::AllocateCodeArea(size_t& space, void*& shadowArea)
{
    uintptr_t offset;
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
        offset = free * pageSize;
    }
    
    if (shadowFd == -1)
    {
        char *baseAddr = memBase + offset;
        int prot = PROT_READ | PROT_WRITE;
        if (memUsage == UsageExecutableCode) prot |= PROT_EXEC;
        if (mmap(baseAddr, space, prot, MAP_FIXED | MAP_PRIVATE | MAP_ANON, -1, 0) == MAP_FAILED)
            return 0;
        msync(baseAddr, space, MS_SYNC | MS_INVALIDATE);
        shadowArea = baseAddr;
        return baseAddr;
    }
    else
    {
        char *baseAddr = memBase + offset;
        char *readWriteArea = shadowBase + offset;
        if (mmap(baseAddr, space, PROT_READ|PROT_EXEC, MAP_FIXED | MAP_SHARED, shadowFd, offset) == MAP_FAILED)
            return 0;
        msync(baseAddr, space, MS_SYNC | MS_INVALIDATE);
        if (mmap(readWriteArea, space, PROT_READ|PROT_WRITE, MAP_FIXED | MAP_SHARED, shadowFd, offset) == MAP_FAILED)
            return 0;
        msync(readWriteArea, space, MS_SYNC | MS_INVALIDATE);
        shadowArea = readWriteArea;
        return baseAddr;
    }
}

bool OSMem::FreeCodeArea(void* codeAddr, void* dataAddr, size_t space)
{
    // Free areas by mapping them with PROT_NONE.
    uintptr_t offset = ((char*)codeAddr - memBase) / pageSize;
    if (shadowFd == -1)
    {
        mmap(codeAddr, space, PROT_NONE, MAP_FIXED | MAP_PRIVATE | MAP_ANON, -1, 0);
        msync(codeAddr, space, MS_SYNC | MS_INVALIDATE);
    }
    else
    {

        mmap(codeAddr, space, PROT_NONE, MAP_SHARED, shadowFd, offset);
        msync(codeAddr, space, MS_SYNC | MS_INVALIDATE);
        mmap(dataAddr, space, PROT_NONE, MAP_SHARED, shadowFd, offset);
        msync(dataAddr, space, MS_SYNC | MS_INVALIDATE);
    }
    uintptr_t pages = space / pageSize;
    {
        PLocker l(&bitmapLock);
        pageMap.ClearBits(offset, pages);
        if (offset + pages > lastAllocated) // We allocate from the top down.
            lastAllocated = offset + pages;
    }
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
    if (memUsage == UsageExecutableCode) prot |= PROT_EXEC;
    int res = mprotect(FIXTYPE codeAddr, space, prot);
    return res != -1;
}

#else

// Native address versions

OSMem::OSMem()
{
    allocPtr = 0;
    shadowFd = -1;
}

OSMem::~OSMem()
{
    if (shadowFd != -1) close(shadowFd);
}

bool OSMem::Initialise(enum _MemUsage usage, size_t space /* = 0 */, void **pBase /* = 0 */)
{
    memUsage = usage;
    pageSize = getpagesize();
    if (usage != UsageExecutableCode) return true;
    // Can we allocate memory with write+execute?
    void *test = mmap(0, pageSize, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANON, -1, 0);
    if (test != MAP_FAILED)
    {
        // Don't require shadow area
        munmap(FIXTYPE test, pageSize);
        return true;
    }
    if (errno != ENOTSUP && errno != EACCES) // Fails with ENOTSUPP on OpenBSD and EACCES in SELinux.
        return false;
    // Check that read-write works.
    test = mmap(0, pageSize, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
    if (test == MAP_FAILED)
        return false; // There's a problem.
    munmap(FIXTYPE test, pageSize);
    // Need to create a file descriptor for mapping.
    shadowFd = createTemporaryFile();
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
    int flags = MAP_PRIVATE | MAP_ANON;
#if defined(MAP_STACK) && defined(__OpenBSD__)
    // On OpenBSD the stack must be mapped with MAP_STACK otherwise it
    // segfaults.  On FreeBSD, though, this isn't necessary and causes problems.
    if (memUsage == UsageStack) flags |= MAP_STACK;
#endif
    void *result = mmap(0, space, PROT_READ|PROT_WRITE, flags, fd, 0);
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
        if (memUsage == UsageExecutableCode) prot |= PROT_EXEC;
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
    if (memUsage == UsageExecutableCode) prot |= PROT_EXEC;
    int res = mprotect(FIXTYPE codeAddr, space, prot);
    return res != -1;
}

#endif
