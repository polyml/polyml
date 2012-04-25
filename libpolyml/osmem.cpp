/*
    Title:  osomem.cpp - Interface to OS memory management

    Copyright (c) 2006 David C.J. Matthews

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


#include "osmem.h"

// Linux prefers MAP_ANONYMOUS to MAP_ANON 
#ifndef MAP_ANON
#ifdef MAP_ANONYMOUS
#define MAP_ANON MAP_ANONYMOUS 
#endif
#endif

#if defined(HAVE_MMAP) && defined(MAP_ANON)
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

// How do we get the page size?
#ifndef HAVE_GETPAGESIZE
#ifdef _SC_PAGESIZE
#define getpagesize() sysconf(_SC_PAGESIZE)
#else
// If this fails we're stuck
#define getpagesize() PAGESIZE
#endif
#endif

static int ConvertPermissions(unsigned perm)
{
    int res = 0;
    if (perm & PERMISSION_READ)
        res |= PROT_READ;
    if (perm & PERMISSION_WRITE)
        res |= PROT_WRITE;
    if (perm & PERMISSION_EXEC)
        res |= PROT_EXEC;
    return res;
}

// Allocate space and return a pointer to it.  The size is the minimum
// size requested and it is updated with the actual space allocated.
// Returns NULL if it cannot allocate the space.
void *OSMem::Allocate(size_t &space, unsigned permissions)
{
    int prot = ConvertPermissions(permissions);
    // Round up to an integral number of pages.
    int pageSize = getpagesize();
    space = (space + pageSize-1) & ~(pageSize-1);
    int fd = -1; // This value is required by FreeBSD.  Linux doesn't care
    void *result = mmap(0, space, prot, MAP_PRIVATE|MAP_ANON, fd, 0);
    // Convert MAP_FAILED (-1) into NULL
    if (result == MAP_FAILED)
        return 0;
    return result;
}

#ifdef SOLARIS
#define FIXTYPE (caddr_t)
#else
#define FIXTYPE
#endif

// Release the space previously allocated.  This must free the whole of
// the segment.  The space must be the size actually allocated.
bool OSMem::Free(void *p, size_t space)
{
    return munmap(FIXTYPE p, space) == 0;
}

// Adjust the permissions on a segment.  This must apply to the
// whole of a segment.
bool OSMem::SetPermissions(void *p, size_t space, unsigned permissions)
{
    int res = mprotect(FIXTYPE p, space, ConvertPermissions(permissions));
    return res != -1;
}


#elif defined(_WIN32)
// Use Windows memory management.
#include <windows.h>

static int ConvertPermissions(unsigned perm)
{
    if (perm & PERMISSION_WRITE)
    {
        // Write.  Always includes read permission.
        if (perm & PERMISSION_EXEC)
            return PAGE_EXECUTE_READWRITE;
        else
            return PAGE_READWRITE;
    }
    else if (perm & PERMISSION_EXEC)
    {
        // Execute but not write.
        if (perm & PERMISSION_READ)
            return PAGE_EXECUTE_READ;
        else
            return PAGE_EXECUTE; // Execute only
    }
    else if(perm & PERMISSION_READ)
        return PAGE_READONLY;
    else 
        return PAGE_NOACCESS;
}

// Allocate space and return a pointer to it.  The size is the minimum
// size requested and it is updated with the actual space allocated.
// Returns NULL if it cannot allocate the space.
void *OSMem::Allocate(size_t &space, unsigned permissions)
{
    // Get the page size and round up to that multiple.
    SYSTEM_INFO sysInfo;
    GetSystemInfo(&sysInfo);
    DWORD pageSize = sysInfo.dwPageSize;
    space = (space + pageSize-1) & ~(pageSize-1);
    return VirtualAlloc(0, space, MEM_RESERVE|MEM_COMMIT, ConvertPermissions(permissions));
}

// Release the space previously allocated.  This must free the whole of
// the segment.  The space must be the size actually allocated.
bool OSMem::Free(void *p, size_t space)
{
    return VirtualFree(p, 0, MEM_RELEASE) == TRUE;
}

// Adjust the permissions on a segment.  This must apply to the
// whole of a segment.
bool OSMem::SetPermissions(void *p, size_t space, unsigned permissions)
{
    DWORD oldProtect;
    return VirtualProtect(p, space, ConvertPermissions(permissions), &oldProtect) == TRUE;
}


#else

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
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

// Create the global object for the memory manager.
static OSMem osmemMan;
OSMem *osMemoryManager = &osmemMan;
