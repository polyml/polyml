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
#elif defined(_WIN32)
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


#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
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

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include "rts_module.h"
#include "timing.h"
#include "statistics.h"
#include "../polystatistics.h"

// For the moment we don't bother to interlock access to the statistics memory.
// Other processes only read the memory and at worst they may get a glitch in
// the values.

Statistics::Statistics(): accessLock("Statistics")
{
    statMemory = 0;
#ifdef HAVE_WINDOWS_H
    hFileMap = NULL;
    // Get the process ID to use in the shared memory name
    DWORD pid = ::GetCurrentProcessId();
    char shmName[MAX_PATH];
    sprintf(shmName, POLY_STATS_NAME "%lu", pid);

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
    // Create the shared memory in the user's .polyml directory
    mapFd = -1;
    mapFileName = 0;
    int pageSize = getpagesize();
    memSize = (sizeof(polystatistics) + pageSize-1) & ~(pageSize-1);
    char *homeDir = getenv("HOME");
    if (homeDir == NULL) return;
    mapFileName = (char*)malloc(strlen(homeDir) + 100);
    strcpy(mapFileName, homeDir);
    strcat(mapFileName, "/.polyml");
    mkdir(mapFileName, 0777); // Make the directory to ensure it exists
    sprintf(mapFileName + strlen(mapFileName), "/" POLY_STATS_NAME "%d", getpid());
    // Open the file.  Truncates it if it already exists.  That should only happen
    // if a previous run with the same process id crashed.
    mapFd = open(mapFileName, O_RDWR|O_CREAT, 0444);
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
    statMemory->magic = POLY_STATS_MAGIC;
}

Statistics::~Statistics()
{
#ifdef HAVE_WINDOWS_H
    if (statMemory != NULL) ::UnmapViewOfFile(statMemory);
    if (hFileMap != NULL) ::CloseHandle(hFileMap);
#elif HAVE_MMAP
    if (statMemory != 0 && statMemory != MAP_FAILED) munmap(statMemory, memSize);
    if (mapFd != -1) close(mapFd);
    if (mapFileName != 0) unlink(mapFileName);
    free(mapFileName);
#endif
}

// Counters.  These are used for thread state so need interlocks
void Statistics::incCount(int which)
{
    if (statMemory)
    {
        PLocker lock(&accessLock);
        statMemory->psCounters[which]++;
    }
}

void Statistics::decCount(int which)
{
    if (statMemory)
    {
        PLocker lock(&accessLock);
        statMemory->psCounters[which]--;
    }
}

// Sizes.  Some of these are only set during GC so may not need interlocks
void Statistics::setSize(int which, size_t s)
{
    if (statMemory)
    {
        PLocker lock(&accessLock);
        statMemory->psSizes[which] = s;
    }
}

void Statistics::incSize(int which, size_t s)
{
    if (statMemory)
    {
        PLocker lock(&accessLock);
        statMemory->psSizes[which] += s;
    }
}

void Statistics::decSize(int which, size_t s)
{
    if (statMemory)
    {
        PLocker lock(&accessLock);
        ASSERT(s <= statMemory->psSizes[which]);
        statMemory->psSizes[which] -= s;
    }
}

size_t Statistics::getSize(int which)
{
    if (statMemory)
    {
        PLocker lock(&accessLock);
        return statMemory->psSizes[which];
    }
    else return 0;
}


#if (defined(_WIN32) && ! defined(__CYGWIN__))
// Native Windows
void Statistics::copyGCTimes(const FILETIME &gcUtime, const FILETIME &gcStime)
{
    if (statMemory)
    {
        PLocker lock(&accessLock);
        statMemory->psTimers[PST_GC_UTIME] = gcUtime;
        statMemory->psTimers[PST_GC_STIME] = gcStime;
    }
}
#elif defined(HAVE_WINDOWS_H)
// Cygwin.  The statistics are held in the Windows format so that they can be accessed
// by the performance monitor.
void Statistics::copyGCTimes(const struct timeval &gcUtime, const struct timeval &gcStime)
{
    if (statMemory)
    {
        FILETIME ftU, ftS;
        ULARGE_INTEGER lU, lS;
        lU.QuadPart = (ULONGLONG)gcUtime.tv_sec * 10000000 + (ULONGLONG)gcUtime.tv_usec * 10;
        ftU.dwHighDateTime = lU.HighPart;
        ftU.dwLowDateTime = lU.LowPart;
        lS.QuadPart = (ULONGLONG)gcStime.tv_sec * 10000000 + (ULONGLONG)gcStime.tv_usec * 10;
        ftS.dwHighDateTime = lS.HighPart;
        ftS.dwLowDateTime = lS.LowPart;
        PLocker lock(&accessLock);
        statMemory->psTimers[PST_GC_UTIME] = ftU;
        statMemory->psTimers[PST_GC_STIME] = ftS;
    }
}
#else
// Unix
void Statistics::copyGCTimes(const struct timeval &gcUtime, const struct timeval &gcStime)
{
    if (statMemory)
    {
        PLocker lock(&accessLock);
        statMemory->psTimers[PST_GC_UTIME] = gcUtime;
        statMemory->psTimers[PST_GC_STIME] = gcStime;
    }
}
#endif

// Update the statistics that are not otherwise copied.  Called from the
// root thread every second.
void Statistics::updatePeriodicStats(POLYUNSIGNED freeWords, unsigned threadsInML)
{
    if (statMemory)
    {
        PLocker lock(&accessLock);
        statMemory->psSizes[PSS_ALLOCATION_FREE] = freeWords*sizeof(PolyWord);
#ifdef HAVE_WINDOWS_H
        FILETIME ct, et, st, ut;
        GetProcessTimes(GetCurrentProcess(), &ct, &et, &st, &ut);
        // Subtract the GC times and then write it into the shared memory.
        // Since we don't interlock reads from the shared memory this extra
        // step reduces the chances of glitches.
        subFiletimes(&st, &statMemory->psTimers[PST_GC_STIME]);
        subFiletimes(&ut, &statMemory->psTimers[PST_GC_UTIME]);
        statMemory->psTimers[PST_NONGC_STIME] = st;
        statMemory->psTimers[PST_NONGC_UTIME] = ut;
#elif HAVE_GETRUSAGE
        struct rusage usage;
        getrusage(RUSAGE_SELF, &usage);
        subTimevals(&usage.ru_stime, &statMemory->psTimers[PST_GC_STIME]);
        subTimevals(&usage.ru_utime, &statMemory->psTimers[PST_GC_UTIME]);
        statMemory->psTimers[PST_NONGC_UTIME] = usage.ru_utime;
        statMemory->psTimers[PST_NONGC_STIME] = usage.ru_stime;
#endif
        statMemory->psCounters[PSC_THREADS_IN_ML] = threadsInML;
    }
}

void Statistics::setUserCounter(unsigned which, int value)
{
    if (statMemory)
    {
        PLocker lock(&accessLock); // Not really needed
        statMemory->psUser[which] = value;
    }
}

// Copy the local statistics into the buffer
bool Statistics::getLocalsStatistics(struct polystatistics *statCopy)
{
    if (statMemory == 0) return false;
    PLocker lock(&accessLock);
    // We don't have to check the magic number because we created it
    memcpy(statCopy, statMemory, sizeof(polystatistics));
    return true;
}

// Get statistics for a remote instance.  We don't do any locking 
bool Statistics::getRemoteStatistics(POLYUNSIGNED pid, struct polystatistics *statCopy)
{
#ifdef HAVE_WINDOWS_H
    char shmName[MAX_PATH];
    sprintf(shmName, POLY_STATS_NAME "%" POLYUFMT, pid);
    HANDLE hRemMemory = OpenFileMapping(FILE_MAP_READ, FALSE, shmName);
    if (hRemMemory == NULL) return false;

    polystatistics *sMem = (polystatistics*)MapViewOfFile(hRemMemory, FILE_MAP_READ, 0, 0, sizeof(polystatistics));
    CloseHandle(hRemMemory);

    if (sMem == NULL) return false;
    // Check the magic number.
    if (sMem->magic != POLY_STATS_MAGIC)
    {
        UnmapViewOfFile(sMem);
        return false;
    }
    // It's possible that the remote is using a different version so we
    // have to check the size before copying.
    size_t bytes = sizeof(polystatistics);
    memset(statCopy, 0, bytes);
    if ((size_t)sMem->psSize < bytes) bytes = (size_t)sMem->psSize;
    memcpy(statCopy, sMem, sizeof(polystatistics));

    UnmapViewOfFile(sMem);
    return true;
#elif HAVE_MMAP
    // Find the shared memory in the user's home directory
    int remMapFd = -1;
    char remMapFileName[MAXPATHLEN];
    remMapFileName[0] = 0;
    char *homeDir = getenv("HOME");
    if (homeDir == NULL) return false;
    sprintf(remMapFileName, "%s/.polyml/" POLY_STATS_NAME "%" POLYUFMT, homeDir, pid);
    remMapFd = open(remMapFileName, O_RDONLY);
    if (remMapFd == -1) return false;
    polystatistics *sMem = (polystatistics*)mmap(0, memSize, PROT_READ, MAP_PRIVATE, remMapFd, 0);
    if (sMem == MAP_FAILED)
    {
        close(remMapFd);
        return false;
    }
    // Check the magic number.
    if (sMem->magic != POLY_STATS_MAGIC)
    {
        munmap(sMem, memSize);
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
Statistics globalStats;

