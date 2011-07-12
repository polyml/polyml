/*
    Title:  statics.cpp - Profiling statistics

    Copyright (c) 2011 David C.J. Matthews

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
#elif defined(WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#if defined(HAVE_MMAP)
// How do we get the page size?
#ifndef HAVE_GETPAGESIZE
#ifdef _SC_PAGESIZE
#define getpagesize() sysconf(_SC_PAGESIZE)
#else
// If this fails we're stuck
#define getpagesize() PAGESIZE
#endif
#endif
#endif

#include "rts_module.h"
#include "statistics.h"
#include "../polystatistics.h"

// For the moment we don't bother to interlock access to the statistics memory.
// Other processes only read the memory and at worst they may get a glitch in
// the values.

Statistics::Statistics()
{
    statMemory = 0;
#ifdef HAVE_WINDOWS_H
    hFileMap = NULL;
    // Get the process ID to use in the shared memory name
    DWORD pid = ::GetCurrentProcessId();
    char shmName[MAX_PATH];
    if (_snprintf(shmName, sizeof(shmName), "poly-%lu", pid) >= sizeof(shmName))
        return;

    // Create a piece of shared memory
    hFileMap = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE,
                                 0, sizeof(polystatistics), shmName);
    if (hFileMap == NULL) return;

    // If it already exists it's the wrong one.
    if (GetLastError() == ERROR_ALREADY_EXISTS) 
    { 
        CloseHandle(hFileMap);
        hFileMap = NULL;
        return;
    }

    statMemory = (polystatistics*)MapViewOfFile(hFileMap, FILE_MAP_ALL_ACCESS, 0, 0, sizeof(polystatistics));
    if (statMemory == NULL)
    {
        CloseHandle(hFileMap);
        hFileMap = NULL;
        return;
    }
    memset(statMemory, 0, sizeof(polystatistics)); // Zero the memory - probably unnecessary
    statMemory->psSize = sizeof(polystatistics);

#elif HAVE_MMAP
    // Create the shared memory in the /tmp directory
    mapFd = -1;
    mapFileName[0] = 0;
    int pageSize = getpagesize();
    memSize = (sizeof(polystatistics) + pageSize-1) & ~(pageSize-1);
    if (snprintf(mapFileName, sizeof(mapFileName), "/tmp/poly-%d", getpid()) >= (int)sizeof(mapFileName))
        return;
    mapFd = open(mapFileName, O_RDWR|O_CREAT|O_EXCL, 0444);
    if (mapFd == -1) return;
    // Write enough of the file to fill the space.
    char ch = 0;
    for (size_t i = 0; i < memSize; i++) write(mapFd, &ch, 1);
    statMemory = (polystatistics*)mmap(0, memSize, PROT_READ|PROT_WRITE, MAP_SHARED, mapFd, 0);
    if (statMemory == MAP_FAILED)
    {
        statMemory = 0;
        return;
    }
#else
    return;
#endif
    memset(statMemory, 0, sizeof(polystatistics)); // Zero the memory - probably unnecessary
    statMemory->psSize = sizeof(polystatistics);
}

Statistics::~Statistics()
{
#ifdef HAVE_WINDOWS_H
    if (hFileMap != NULL) ::CloseHandle(hFileMap);
#elif HAVE_MMAP
    if (statMemory != 0 && statMemory != MAP_FAILED) munmap(statMemory, memSize);
    if (mapFd != -1) close(mapFd);
    if (mapFileName[0] != 0) unlink(mapFileName);
#endif
}

// Copy the local statistics into the buffer
bool Statistics::getLocalsStatistics(struct polystatistics *statCopy)
{
    if (statMemory == 0) return false;
    // We don't have to check the sizes because we created it
    memcpy(statCopy, statMemory, sizeof(polystatistics));
    return true;
}

// Get statistics for a remote instance
bool Statistics::getRemoteStatistics(POLYUNSIGNED pid, struct polystatistics *statCopy)
{
#ifdef HAVE_WINDOWS_H
    char shmName[MAX_PATH];
    if (_snprintf(shmName, sizeof(shmName), "poly-%" POLYUFMT, pid) >= sizeof(shmName))
        return false;
    HANDLE hRemMemory = OpenFileMapping(FILE_MAP_READ, FALSE, shmName);
    if (hRemMemory == NULL) return false;

    polystatistics *sMem = (polystatistics*)MapViewOfFile(hRemMemory, FILE_MAP_READ, 0, 0, sizeof(polystatistics));
    if (sMem == NULL)
    {
        CloseHandle(hRemMemory);
        return false;
    }
    // It's possible that the remote is using a different version so we
    // have to check the size before copying.
    size_t bytes = sizeof(polystatistics);
    memset(statCopy, 0, bytes);
    if ((size_t)sMem->psSize < bytes) bytes = (size_t)sMem->psSize;
    memcpy(statCopy, sMem, sizeof(polystatistics));

    CloseHandle(hRemMemory);
    return true;
#elif HAVE_MMAP
    // Find the shared memory in the /tmp directory
    int remMapFd = -1;
    char remMapFileName[40];
    remMapFileName[0] = 0;
    if (snprintf(remMapFileName, sizeof(remMapFileName), "/tmp/poly-%d", pid) >= (int)sizeof(remMapFileName))
        return false;
    remMapFd = open(remMapFileName, O_RDONLY);
    if (remMapFd == -1) return false;
    polystatistics *sMem = (polystatistics*)mmap(0, memSize, PROT_READ, MAP_PRIVATE, remMapFd, 0);
    if (sMem == MAP_FAILED)
    {
        close(remMapFd);
        return false;
    }
    size_t bytes = sizeof(polystatistics);
    memset(statCopy, 0, bytes);
    if ((size_t)sMem->psSize < bytes) bytes = (size_t)sMem->psSize;
    memcpy(statCopy, sMem, sizeof(polystatistics));
    munmap(sMem, memSize);
    close(remMapFd);
    return true;
#else
    return false;
#endif
}


// Create the global statistics object.
static Statistics globalStats;
Statistics *gStats = &globalStats;
