/*
    Title:  statics.h - Interface to profiling statistics

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

#ifndef STATISTICS_INCLUDED
#define STATISTICS_INCLUDED

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

#include "globals.h"
#include "locking.h"
#include "../polystatistics.h"
enum {
    PSC_THREADS = 0,                // Total number of threads
    PSC_THREADS_IN_ML,              // Threads running ML code
    PSC_THREADS_WAIT_IO,            // Threads waiting for IO
    PSC_THREADS_WAIT_MUTEX,         // Threads waiting for a mutex
    PSC_THREADS_WAIT_CONDVAR,       // Threads waiting for a condition var
    PSC_THREADS_WAIT_SIGNAL,        // Special case - signal handling thread
    PSC_GC_FULLGC,                  // Number of full garbage collections
    PSC_GC_PARTIALGC,               // Number of partial GCs

    PSS_TOTAL_HEAP,                 // Total size of the local heap
    PSS_AFTER_LAST_GC,              // Space free after last GC
    PSS_AFTER_LAST_FULLGC,          // Space free after the last full GC
    PSS_ALLOCATION,                 // Size of allocation space
    PSS_ALLOCATION_FREE,            // Space available in allocation area
    N_PS_INTS
};

enum {
    PST_NONGC_UTIME,
    PST_NONGC_STIME,
    PST_GC_UTIME,
    PST_GC_STIME,
    N_PS_TIMES
};

// A few counters that can be used by the application
#define N_PS_USER   8

class TaskData;
class SaveVecEntry;
typedef SaveVecEntry *Handle;

class Statistics  
{
public:
    Statistics();
    ~Statistics();

    Handle getLocalStatistics(TaskData *taskData);
    Handle getRemoteStatistics(TaskData *taskData, POLYUNSIGNED processId);

    void incCount(int which);
    void decCount(int which);

    void setSize(int which, size_t s);
    void incSize(int which, size_t s);
    void decSize(int which, size_t s);
    size_t getSize(int which);

    void setUserCounter(unsigned which, POLYSIGNED value);

#if (defined(_WIN32) && ! defined(__CYGWIN__))
    // Native Windows
    void copyGCTimes(const FILETIME &gcUtime, const FILETIME &gcStime);
    FILETIME gcUserTime, gcSystemTime;
#else
    // Unix and Cygwin
    void copyGCTimes(const struct timeval &gcUtime, const struct timeval &gcStime);
    struct timeval gcUserTime, gcSystemTime;
#endif
    
    void updatePeriodicStats(POLYUNSIGNED freeSpace, unsigned threadsInML);

private:
    PLock accessLock;
#ifdef HAVE_WINDOWS_H
    // File mapping handle
    HANDLE hFileMap;
#else
    char *mapFileName;
    int mapFd;
#endif
    size_t memSize;
    unsigned char *statMemory;
    unsigned char *newPtr;

    unsigned char *counterAddrs[N_PS_INTS];
    struct { unsigned char *secAddr; unsigned char *usecAddr; } timeAddrs[N_PS_TIMES];
    unsigned char *userAddrs[N_PS_USER];

    Handle returnStatistics(TaskData *taskData, unsigned char *stats);
    void addCounter(int cEnum, unsigned statId, const char *name);
    void addSize(int cEnum, unsigned statId, const char *name);
    void addTime(int cEnum, unsigned statId, const char *name);
    void addUser(int n, unsigned statId, const char *name);

    size_t getSizeWithLock(int which);
    void setSizeWithLock(int which, size_t s);
    void setTimeValue(int which, unsigned long secs, unsigned long usecs);
};

extern Statistics globalStats;

#endif // STATISTICS_INCLUDED
