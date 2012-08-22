/*
    Title:  polystatics.h - Layout of statistics data in shared memory
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

#ifndef POLY_STATISTICS_INCLUDED
#define POLY_STATISTICS_INCLUDED

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

// Name of shared memory file.  This has the process ID appended.
// In Unix this is in /tmp
#define POLY_STATS_NAME "poly-stats-"

enum {
    PSC_THREADS = 0,                // Total number of threads
    PSC_THREADS_IN_ML,              // Threads running ML code
    PSC_THREADS_WAIT_IO,            // Threads waiting for IO
    PSC_THREADS_WAIT_MUTEX,         // Threads waiting for a mutex
    PSC_THREADS_WAIT_CONDVAR,       // Threads waiting for a condition var
    PSC_THREADS_WAIT_SIGNAL,        // Special case - signal handling thread
    PSC_GC_FULLGC,                  // Number of full garbage collections
    PSC_GC_PARTIALGC,               // Number of partial GCs
    N_PS_COUNTERS
};

enum {
    PSS_TOTAL_HEAP = 0,             // Total size of the local heap
    PSS_AFTER_LAST_GC,              // Space free after last GC
    PSS_AFTER_LAST_FULLGC,          // Space free after the last full GC
    PSS_ALLOCATION,                 // Size of allocation space
    PSS_ALLOCATION_FREE,            // Space available in allocation area
    N_PS_SIZES
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

// A random number to help identify a valid shared memory block.
#define POLY_STATS_MAGIC    0x8022E96B

typedef struct polystatistics {
    unsigned psSize; // Size of the data structure
    unsigned magic; // Magic number
    unsigned long psCounters[N_PS_COUNTERS];
    size_t psSizes[N_PS_SIZES];
#if defined(HAVE_WINDOWS_H)
    FILETIME psTimers[N_PS_TIMES];
#elif defined(HAVE_GETRUSAGE)
    struct timeval psTimers[N_PS_TIMES];
#else
    int psTimers[N_PS_TIMES];
#endif
    int psUser[N_PS_USER];
} polystatistics;

#endif // POLY_STATISTICS_INCLUDED


