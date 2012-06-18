/*
    Title:  heapsizing.h - parameters to adjust heap size

    Copyright (c) Copyright David C.J. Matthews 2012

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

#ifndef HEAPSIZING_H_INCLUDED
#define HEAPSIZING_H_INCLUDED 1

#include "timing.h"

class LocalMemSpace;

enum {
    HEAPSIZING_DEFAULT,
    HEAPSIZING_FIXED,
    HEAPSIZING_PID
};

class HeapSizeParameters {
public:
    HeapSizeParameters();

    // Extract timing information for ML.
    Handle getGCUtime(TaskData *taskData) const;
    Handle getGCStime(TaskData *taskData) const;

    void SetInitialSize(unsigned hsize);
    void SetReservation(unsigned rsize);
    void SetSharingOption(bool gcShare) { performSharingPass = gcShare; }
    void SetHeapSizingOption(unsigned h) { heapSizeOption = h; }

    // Called in the minor GC if a GC thread needs to grow the heap.
    // Returns zero if the heap cannot be grown.
    LocalMemSpace *AddSpaceInMinorGC(POLYUNSIGNED space, bool isMutable);

    // Called in the major GC before the copy phase if the heap is more than
    // 90% full.  This should improve the efficiency of copying.
    LocalMemSpace *AddSpaceBeforeCopyPhase(bool isMutable);

    bool PerformSharingPass() const { return performSharingPass; }
    void AdjustSizeAfterMajorGC();
    void AdjustSizeAfterMinorGC(POLYUNSIGNED spaceAfterGC, POLYUNSIGNED spaceBeforeGC);

    // Returns true if we should run a major GC at this point
    bool RunMajorGCImmediately();

    /* Called by the garbage collector at the beginning and
       end of garbage collection. */
    typedef enum __gcTime {
        GCTimeStart,
        GCTimeIntermediate,
        GCTimeEnd
    } gcTime;

    void RecordGCTime(gcTime isEnd, const char *stage = "");
    
    void resetMinorTimingData(void);
    void resetMajorTimingData(void);

    void Init(void);
    void Final(void);

private:

    bool fullGCNextTime;

    // Whether a sharing pass should be performed.  Currently this is set by
    // a user option.
    bool performSharingPass;

    unsigned heapSizeOption;

    // For the current, crude heap allocation strategy this is the heap
    // size given on the command line.
    POLYUNSIGNED freeHeapSpace;

    TIMEDATA startTime;

    TIMEDATA minorNonGCUserCPU;
    TIMEDATA minorNonGCSystemCPU;
    TIMEDATA minorNonGCReal;
    TIMEDATA minorGCUserCPU;
    TIMEDATA minorGCSystemCPU;
    TIMEDATA minorGCReal;

    TIMEDATA majorNonGCUserCPU;
    TIMEDATA majorNonGCSystemCPU;
    TIMEDATA majorNonGCReal;
    TIMEDATA majorGCUserCPU;
    TIMEDATA majorGCSystemCPU;
    TIMEDATA majorGCReal;

    TIMEDATA totalGCUserCPU;
    TIMEDATA totalGCSystemCPU;
    TIMEDATA totalGCReal;

#if (defined(_WIN32) && ! defined(__CYGWIN__))
    FILETIME startUsageU, startUsageS, lastUsageU, lastUsageS;
    FILETIME startRTime, lastRTime;
    DWORD startPF;
#else
    struct rusage startUsage, lastUsage;
    struct timeval startRTime, lastRTime;
    long startPF;
#endif

    bool lastAllocationSucceeded;
};

extern HeapSizeParameters gHeapSizeParameters;
#endif
