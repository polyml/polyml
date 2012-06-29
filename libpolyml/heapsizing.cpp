/*
    Title:  heapsizing.cpp - parameters to adjust heap size

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

/*
This module is intended to deal with heap sizing based on measurements of the time taken
in the GC compared with the application code.  Currently it is very basic.
This also provides GC timing information to the ML code as well as statistics and
debugging.
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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h> // For sysconf
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_SYSCTL_H
#include <sys/sysctl.h>
#endif

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

#ifdef HAVE_MATH_H
#include <math.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include "globals.h"
#include "arb.h"
#include "diagnostics.h"
#include "rts_module.h"
#include "timing.h"
#include "heapsizing.h"
#include "statistics.h"
#include "memmgr.h"

// The one and only parameter object
HeapSizeParameters gHeapSizeParameters;

#ifdef HAVE_WINDOWS_H
// Getting hard page counts in Windows is not easy.  Cygwin uses
// GetProcessMemoryInfo to return the value in ru_majflt but this
// is actually incorrect because it returns the soft page count not
// the hard page count.  We use NtQuerySystemInformation to get the
// total paging on the system on the basis that this is more useful
// than the soft page faults for the process.
// This is an undocumented interface and it's all a bit of a mess.

typedef struct  {
    INT64 pad1[4];
    DWORD pad2[12];
    DWORD pagesRead;
    DWORD pad3[60];
} SystemPerformanceInfo;

typedef int (WINAPI *NtSystemInfo)(int, SystemPerformanceInfo *, ULONG, void*);
static NtSystemInfo pFunctionPtr;

static long GetPaging(long)
{
    if (pFunctionPtr == 0)
        pFunctionPtr = (NtSystemInfo) GetProcAddress(GetModuleHandle("Ntdll.dll"), "NtQuerySystemInformation");

    if (pFunctionPtr != 0)
    {
        SystemPerformanceInfo pInfo;
        int result = (*pFunctionPtr)(2/*SystemPerformanceInformation*/, &pInfo, sizeof(pInfo), 0);
        if (result == 0)
            return pInfo.pagesRead;
    }
    return 0;
}
#else
inline long GetPaging(long rusagePage)
{
    return rusagePage;
}
#endif

HeapSizeParameters::HeapSizeParameters()
{
    startPF = GetPaging(0);
    fullGCNextTime = false;
    performSharingPass = false;
    lastAllocationSucceeded = true;
    minHeapSize = 0;
    maxHeapSize = 0; // Unlimited
    lastFreeSpace = 0;
    pagingLimitSize = 0;
    highWaterMark = 0;
}

/* This macro must make a whole number of chunks */
#define K_to_words(k) ROUNDUP((k) * (1024 / sizeof(PolyWord)),BITSPERWORD)

// Returns physical memory size in bytes
static POLYUNSIGNED GetPhysicalMemorySize(void);

// These are the maximum values for the number of words.
#if (SIZEOF_VOIDP == 4)
#   define MAXIMUMADDRESS   0x3fffffff
#else
#   define MAXIMUMADDRESS   0x1fffffffffffffff
#endif

// Set the initial size based on any parameters specified on the command line
void HeapSizeParameters::SetHeapParameters(unsigned minsize, unsigned maxsize, unsigned percent)
{
    minHeapSize = K_to_words(minsize); // If these overflow assume the result will be zero
    maxHeapSize = K_to_words(maxsize);

    POLYUNSIGNED memsize = 0;
    if (minHeapSize == 0 || maxHeapSize == 0)
        memsize = GetPhysicalMemorySize() / sizeof(PolyWord);

    // If no maximum is given default it to 100% of the physical memory
    if (maxHeapSize == 0 || maxHeapSize > MAXIMUMADDRESS)
    {
        if (memsize == 0) maxHeapSize = MAXIMUMADDRESS;
        else maxHeapSize = memsize /*- memsize / 5*/;
    }

    // Set the initial size to the minimum if that has been provided.
    POLYUNSIGNED initialSize = minHeapSize;

    if (initialSize == 0) {
        // If no -H option was given set the default initial size to a quarter of the memory.
        if (memsize == 0) // Unable to determine memory size so default to 64M.
            initialSize = 64 * 1024 * 1024;
        else initialSize =  memsize / 4;
    }

    // Initially we divide the space equally between the major and
    // minor heaps.  That means that there will definitely be space
    // for the first minor GC to copy its data.  This division can be
    // changed later on.
    gMem.SetSpaceForHeap(initialSize);
    gMem.SetSpaceBeforeMinorGC(initialSize/2);
    lastFreeSpace = initialSize;
    highWaterMark = initialSize;

    if (percent == 0)
        userGCRatio = 0.25F; // Default to 20% GC to 80% application
    else
        userGCRatio = (float)percent / (float)(100 - percent);

    lastMajorGCRatio = userGCRatio;

    if (debugOptions & DEBUG_HEAPSIZE)
    {
        Log("Heap: Initial settings: Initial heap ");
        LogSize(initialSize);
        Log(" minimum ");
        LogSize(minHeapSize);
        Log(" maximum ");
        LogSize(maxHeapSize);
        Log(" target ratio %f\n", userGCRatio);
    }
}

void HeapSizeParameters::SetReservation(unsigned rsize)
{
    gMem.SetReservation(K_to_words(rsize));
}

// Called in the minor GC if a GC thread needs to grow the heap.
// Returns zero if the heap cannot be grown. "space" is the space required for the
// object (and length field) in case this is larger than the default size.
LocalMemSpace *HeapSizeParameters::AddSpaceInMinorGC(POLYUNSIGNED space, bool isMutable)
{
    // See how much space is allocated to the major heap.
    POLYUNSIGNED spaceAllocated = gMem.CurrentHeapSize() - gMem.CurrentAllocSpace();

    // The new segment is either the default size or as large as
    // necessary for the object.
    POLYUNSIGNED spaceSize = gMem.DefaultSpaceSize();
    if (space > spaceSize) spaceSize = space;

    // We allow for extension if the total heap size after extending it
    // plus one allocation area of the default size would not be more
    // than the allowed heap size.
    if (spaceAllocated + spaceSize + gMem.DefaultSpaceSize() <= gMem.SpaceForHeap())
    {
        LocalMemSpace *sp = gMem.NewLocalSpace(spaceSize, isMutable); // Return the space or zero if it failed
        lastAllocationSucceeded = sp != 0;
        if ((debugOptions & DEBUG_HEAPSIZE) && ! lastAllocationSucceeded)
            Log("Heap: Allocation of new heap segment failed.  Limit reached?\n");
        return sp;
    }
    return 0; // Insufficient space
}

// Called in the major GC before the copy phase if the heap is more than
// 90% full.  This should improve the efficiency of copying.
LocalMemSpace *HeapSizeParameters::AddSpaceBeforeCopyPhase(bool isMutable)
{
    LocalMemSpace *sp = gMem.NewLocalSpace(gMem.DefaultSpaceSize(), isMutable);
    lastAllocationSucceeded = sp != 0;
    if ((debugOptions & DEBUG_HEAPSIZE) && ! lastAllocationSucceeded)
        Log("Heap: Allocation of new heap segment failed.  Limit reached?\n");
    return sp;
}

// The steepness of the curve.
#define PAGINGCOSTSTEEPNESS 20.0
// The additional cost at the boundary
#define PAGINGCOSTFACTOR    1.0
// The number of pages at the boundary
#define PAGINGCOUNTFACTOR   1000.0

// Called at the end of collection.  This is where we should do the
// fine adjustment of the heap size to minimise the GC time.
// Growing the heap is just a matter of adjusting the limits.  We
// don't actually need to allocate the space here.
// See also adjustHeapSizeAfterMinorGC for adjustments after a minor GC.
void HeapSizeParameters::AdjustSizeAfterMajorGC()
{
    TIMEDATA gc, nonGc;
    gc.add(majorGCSystemCPU);
    gc.add(majorGCUserCPU);
    nonGc.add(majorNonGCSystemCPU);
    nonGc.add(majorNonGCUserCPU);
    if (gc.toSeconds() != 0.0 && nonGc.toSeconds() != 0.0)
        lastMajorGCRatio = gc.toSeconds() / nonGc.toSeconds();

    if (highWaterMark < gMem.CurrentHeapSize()) highWaterMark = gMem.CurrentHeapSize();

    POLYUNSIGNED heapSpace = gMem.SpaceForHeap() < highWaterMark ? gMem.SpaceForHeap() : highWaterMark;
    currentSpaceUsed = 0;
    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        currentSpaceUsed += gMem.lSpaces[i]->allocatedSpace();
    }
    POLYUNSIGNED currentFreeSpace = heapSpace - currentSpaceUsed;

    if (debugOptions & DEBUG_HEAPSIZE)
    {
        Log("Heap: GC cpu time %2.3f non-gc time %2.3f ratio %0.3f for free space ",
            gc.toSeconds(), nonGc.toSeconds(), lastMajorGCRatio);
        LogSize((lastFreeSpace + currentFreeSpace)/2);
        Log("\n");
        Log("Heap: GC real time %2.3f non-gc time %2.3f ratio %0.3f\n",
            majorGCReal.toSeconds(), majorNonGCReal.toSeconds(), majorGCReal.toSeconds()/majorNonGCReal.toSeconds());
    }

    // Calculate the paging threshold.
    if (pagingLimitSize != 0 || majorGCPageFaults != 0)
    {
        if (majorGCPageFaults == 0) majorGCPageFaults = 1; // Less than one
        // Some paging detected.  The expression here is the inverse of the one used to
        // compute the paging contribution in the cost function.
        double scaleFactor = 1.0 + log((double)majorGCPageFaults / PAGINGCOUNTFACTOR) / PAGINGCOSTSTEEPNESS;
        ASSERT(scaleFactor > 0.0);
        POLYUNSIGNED newLimit = (POLYUNSIGNED)((double)heapSpace / scaleFactor);
        if (pagingLimitSize == 0)
            pagingLimitSize = newLimit;
        else 
            pagingLimitSize = (newLimit + pagingLimitSize) / 2;

        if (debugOptions & DEBUG_HEAPSIZE)
        {
            Log("Heap: Paging threshold adjusted to ");
            LogSize(pagingLimitSize);
            Log(" with %ld page faults\n", majorGCPageFaults);
        }
    }

    // Calculate a new heap size.  We allow a maximum doubling or halving of size.
    // It's probably more important to limit the increase in case we hit paging.
    POLYUNSIGNED sizeMax = heapSpace * 2;
    if (sizeMax > maxHeapSize) sizeMax = maxHeapSize;
    POLYUNSIGNED sizeMin = heapSpace / 2;
    if (sizeMin < minHeapSize) sizeMin = minHeapSize;

    double costMin = costFunction(sizeMin);
    if (costMin > userGCRatio)
        // If the cost of the minimum is below the target we
        // use that and don't need to look further.
    {
        double costMax = costFunction(sizeMax);
        while (sizeMax - sizeMin > gMem.DefaultSpaceSize())
        {
            POLYUNSIGNED sizeNext = (sizeMin + sizeMax) / 2;
            double cost = costFunction(sizeNext);
            if (cost < userGCRatio || (costMax > costMin && costMax > userGCRatio))
            {
                sizeMax = sizeNext;
                costMax = cost;
            }
            else
            {
                sizeMin = sizeNext;
                costMin = cost;
            }
            ASSERT(costMin > userGCRatio);
        }
    }
    ASSERT(sizeMin >= minHeapSize && sizeMin <= maxHeapSize);

    // If the estimated cost is large run the sharing pass next time.
    // This will happen if the heap cannot be extended because we've
    // reached the maximum or reached the paging limit.
    // If we ran it last time we will overestimate the GC cost because
    // the cost of the sharing pass is included in the GC cost so if
    // we ran it last time we only run it next time if the estimated cost is
    // very large.
    // TODO: We also don't want to run it just because we're limiting the growth of the heap
    // to a factor of two and the previous cost was high.
    // We also want to run this if we haven't been able to extend the heap because
    // we've hit some limit (lastAllocationSucceeded is false) and the actual
    // cost was large.  We may compute a smaller heap size but not be able to
    // achieve it.
    performSharingPass =
        (lastAllocationSucceeded ? costMin: lastMajorGCRatio) > userGCRatio * (performSharingPass ? 8 : 4);

    if (debugOptions & DEBUG_HEAPSIZE)
    {
        if (performSharingPass)
            Log("Heap: Next full GC will enable the sharing pass\n");
        Log("Heap: Resizing from ");
        LogSize(gMem.SpaceForHeap());
        Log(" to ");
        LogSize(sizeMin);
        Log(".  Estimated ratio %2.2f\n", costMin);
    }
    // Set the sizes.
    gMem.SetSpaceForHeap(sizeMin);
    // Set the minor space size.  It can potentially use the whole of the
    // rest of the available heap but there could be a problem if that exceeds
    // the available memory and causes paging.  We need to raise the limit carefully.
    POLYUNSIGNED nextLimit = highWaterMark + highWaterMark / 32;
    if (nextLimit > sizeMin) nextLimit = sizeMin;
    gMem.SetSpaceBeforeMinorGC(nextLimit-gMem.CurrentHeapSize());

    lastFreeSpace = sizeMin - currentSpaceUsed;
}

// Called after a minor GC.  Currently does nothing.
// See also adjustHeapSize for adjustments after a major GC.
bool HeapSizeParameters::AdjustSizeAfterMinorGC(POLYUNSIGNED spaceAfterGC, POLYUNSIGNED spaceBeforeGC)
{
    POLYUNSIGNED spaceCopiedOut = spaceAfterGC-spaceBeforeGC;
    TIMEDATA gc, total;
    minorGCsSinceMajor++;
    // The major costs are cumulative so we use those
    gc.add(majorGCSystemCPU);
    gc.add(majorGCUserCPU);
    total.add(gc);
    total.add(majorNonGCSystemCPU);
    total.add(majorNonGCUserCPU);
    float g = gc.toSeconds() / total.toSeconds();

    if (debugOptions & DEBUG_HEAPSIZE)
    {
        Log("Heap: Space before ");
        LogSize(spaceBeforeGC);
        Log(", space after ");
        LogSize(spaceAfterGC);
        Log("\n");
        Log("Heap: Minor resizing factors g = %f, recent pf = %ld, cumulative pf = %ld\n",
            g, minorGCPageFaults, majorGCPageFaults);
    }

    if (highWaterMark < gMem.CurrentHeapSize()) highWaterMark = gMem.CurrentHeapSize();

    POLYUNSIGNED nextLimit = highWaterMark + highWaterMark / 32;
    if (nextLimit > gMem.SpaceForHeap()) nextLimit = gMem.SpaceForHeap();

    // Set the space available for the allocation area to be the difference between the
    // total heap size and the allowed heap size together with as much space as we copied
    // on this GC.  That allows for the next minor GC to copy the same amount without
    // extending the heap.  If the next minor GC adds more than this the heap will be
    // extended and a corresponding amount deducted so that the heap shrinks again.
    POLYUNSIGNED nonAlloc = gMem.CurrentHeapSize() - gMem.CurrentAllocSpace() + spaceCopiedOut;
    // TODO: If we have limited the space to the high water mark + 1/32 but that is less
    // than we really need we should increase it further.
    POLYUNSIGNED allowedAlloc = nonAlloc >= nextLimit ? 0 : nextLimit - nonAlloc;
    if (gMem.CurrentAllocSpace() != allowedAlloc)
    {
        if (debugOptions & DEBUG_HEAPSIZE)
        {
            Log("Heap: Adjusting space for allocation area from ");
            LogSize(gMem.SpaceBeforeMinorGC());
            Log(" to ");
            LogSize(allowedAlloc);
            Log("\n");
        }
        gMem.SetSpaceBeforeMinorGC(allowedAlloc);
        if (allowedAlloc < gMem.DefaultSpaceSize() * 2 || minorGCPageFaults > 100)
            return false; // Trigger full GC immediately.
     }

    // Trigger a full GC if the live data is very large or if we have exceeeded
    // the target ratio over several GCs (this smooths out small variations).
    if ((minorGCsSinceMajor > 4 && g > userGCRatio*0.8) || majorGCPageFaults > 100)
        fullGCNextTime = true;
    return true;
}

// Estimate the GC cost for a given heap size.  The result is the ratio of
// GC time to application time.
// This is really guesswork.
double HeapSizeParameters::costFunction(POLYUNSIGNED heapSize)
{
    POLYUNSIGNED heapSpace = gMem.SpaceForHeap() < highWaterMark ? gMem.SpaceForHeap() : highWaterMark;
    POLYUNSIGNED currentFreeSpace = heapSpace - currentSpaceUsed;
    POLYUNSIGNED averageFree = (lastFreeSpace + currentFreeSpace) / 2;
    if (heapSize <= currentSpaceUsed)
        return 1.0E6;
    POLYUNSIGNED estimatedFree = heapSize - currentSpaceUsed;
    // The cost scales as the inverse of the amount of free space.
    double result = lastMajorGCRatio * (double)averageFree / (double)estimatedFree;

    // The paging contribution depends on the page limit
    double pagingCost = 0.0;
    if (pagingLimitSize != 0)
    {
        double factor = ((double)heapSize - (double)pagingLimitSize) / (double)pagingLimitSize * PAGINGCOSTSTEEPNESS;
        pagingCost = PAGINGCOSTFACTOR * exp(factor);
        result += pagingCost;
    }

    if (debugOptions & DEBUG_HEAPSIZE)
    {
        Log("Heap: Cost for heap of size ");
        LogSize(heapSize);
        Log(" is %2.2f with paging contributing %2.2f\n", result, pagingCost);
    }
    return result;
}

bool HeapSizeParameters::RunMajorGCImmediately()
{
    if (fullGCNextTime)
    {
        fullGCNextTime = false;
        return true;
    }
    return false;
}

// This function is called at the beginning and end of garbage
// collection to record the time used.
// This also reports the GC time if GC debugging is enabled.
void HeapSizeParameters::RecordGCTime(gcTime isEnd, const char *stage)
{
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    FILETIME kt, ut;
    FILETIME ct, et; // Unused
    FILETIME rt;

    GetProcessTimes(GetCurrentProcess(), &ct, &et, &kt, &ut);
    GetSystemTimeAsFileTime(&rt);

    switch (isEnd)
    {
    case GCTimeStart:
        {
            // Start of GC
            long pageCount = GetPaging(0);
            lastUsageU = ut;
            lastUsageS = kt;
            lastRTime = rt;
            subFiletimes(&ut, &startUsageU);
            subFiletimes(&kt, &startUsageS);
            subFiletimes(&rt, &startRTime);
            if (debugOptions & DEBUG_GC)
            {
                float userTime = filetimeToSeconds(&ut);
                float systemTime = filetimeToSeconds(&kt);
                float realTime = filetimeToSeconds(&rt);
                Log("GC: Non-GC time: CPU user: %0.3f system: %0.3f real: %0.3f page faults: %ld\n",
                    userTime, systemTime, realTime, pageCount - startPF);
                // Add to the statistics.
            }
            minorNonGCUserCPU.add(ut);
            majorNonGCUserCPU.add(ut);
            minorNonGCSystemCPU.add(kt);
            majorNonGCSystemCPU.add(kt);
            minorNonGCReal.add(rt);
            majorNonGCReal.add(rt);
            startUsageU = lastUsageU;
            startUsageS = lastUsageS;
            startRTime = lastRTime;
            // Page faults in the application are included
            minorGCPageFaults += pageCount - startPF;
            majorGCPageFaults += pageCount - startPF;
            startPF = pageCount;
            break;
        }

    case GCTimeIntermediate:
        // Report intermediate GC time for debugging
        if (debugOptions & DEBUG_GC)
        {
            FILETIME nextU = ut, nextS = kt, nextR = rt;
            subFiletimes(&ut, &lastUsageU);
            subFiletimes(&kt, &lastUsageS);
            subFiletimes(&rt, &lastRTime);

            float userTime = filetimeToSeconds(&ut);
            float systemTime = filetimeToSeconds(&kt);
            float realTime = filetimeToSeconds(&rt);

            Log("GC: (%s) CPU user: %0.3f system: %0.3f real: %0.3f speed up %0.1f\n", stage, userTime, 
                systemTime, realTime, realTime == 0.0 ? 0.0 : (userTime + systemTime) / realTime);
            lastUsageU = nextU;
            lastUsageS = nextS;
            lastRTime = nextR;
        }
        break;

    case GCTimeEnd: // End of GC.
        {
            long pageCount = GetPaging(0);

            lastUsageU = ut;
            lastUsageS = kt;
            lastRTime = rt;
            subFiletimes(&ut, &startUsageU);
            subFiletimes(&kt, &startUsageS);
            subFiletimes(&rt, &startRTime);
            totalGCUserCPU.add(ut);
            totalGCSystemCPU.add(kt);
            totalGCReal.add(rt);

            if (debugOptions & DEBUG_GC)
            {
                float userTime = filetimeToSeconds(&ut);
                float systemTime = filetimeToSeconds(&kt);
                float realTime = filetimeToSeconds(&rt);
                Log("GC: CPU user: %0.3f system: %0.3f real: %0.3f speed up %0.1f page faults %ld\n", userTime, 
                    systemTime, realTime, realTime == 0.0 ? 0.0 : (userTime + systemTime) / realTime,
                    pageCount - startPF);
            }
            minorGCUserCPU.add(ut);
            majorGCUserCPU.add(ut);
            minorGCSystemCPU.add(kt);
            majorGCSystemCPU.add(kt);
            minorGCReal.add(rt);
            majorGCReal.add(rt);
            startUsageU = lastUsageU;
            startUsageS = lastUsageS;
            startRTime = lastRTime;
            minorGCPageFaults += pageCount - startPF;
            majorGCPageFaults += pageCount - startPF;
            startPF = pageCount;
            globalStats.copyGCTimes(totalGCUserCPU, totalGCSystemCPU);
        }
        break;
    }
#else
    switch (isEnd)
    {
    case GCTimeStart:
        {            
            // Start of GC
            struct rusage rusage;
            if (getrusage(RUSAGE_SELF, &rusage) != 0)
                return;
            lastUsage = rusage;
            long pageFaults = GetPaging(rusage.ru_majflt);
            subTimevals(&rusage.ru_utime, &startUsage.ru_utime);
            subTimevals(&rusage.ru_stime, &startUsage.ru_stime);
            struct timeval tv;
            if (gettimeofday(&tv, NULL) != 0)
                return;
            lastRTime = tv;
            subTimevals(&tv, &startRTime);

            if (debugOptions & DEBUG_GC)
            {
                float userTime = timevalToSeconds(&rusage.ru_utime);
                float systemTime = timevalToSeconds(&rusage.ru_stime);
                float realTime = timevalToSeconds(&tv);
                Log("GC: Non-GC time: CPU user: %0.3f system: %0.3f real: %0.3f page faults: %ld\n", userTime, 
                    systemTime, realTime, pageFaults - startPF);
            }
            minorNonGCUserCPU.add(rusage.ru_utime);
            majorNonGCUserCPU.add(rusage.ru_utime);
            minorNonGCSystemCPU.add(rusage.ru_stime);
            majorNonGCSystemCPU.add(rusage.ru_stime);
            minorNonGCReal.add(tv);
            majorNonGCReal.add(tv);
            startUsage = lastUsage;
            startRTime = lastRTime;
            // Page faults in the application are included
            minorGCPageFaults += pageFaults - startPF;
            majorGCPageFaults += pageFaults - startPF;
            startPF = pageFaults;
            break;
         }

    case GCTimeIntermediate:
        // Report intermediate GC time for debugging
        if (debugOptions & DEBUG_GC)
        {
            struct rusage rusage;
            struct timeval tv;
            if (getrusage(RUSAGE_SELF, &rusage) != 0 || gettimeofday(&tv, NULL) != 0)
                return;
            struct rusage nextUsage = rusage;
            struct timeval nextTime = tv;
            subTimevals(&rusage.ru_utime, &lastUsage.ru_utime);
            subTimevals(&rusage.ru_stime, &lastUsage.ru_stime);
            subTimevals(&tv, &lastRTime);

            float userTime = timevalToSeconds(&rusage.ru_utime);
            float systemTime = timevalToSeconds(&rusage.ru_stime);
            float realTime = timevalToSeconds(&tv);
            Log("GC: %s CPU user: %0.3f system: %0.3f real: %0.3f speed up %0.1f\n", stage, userTime, 
                systemTime, realTime, realTime == 0.0 ? 0.0 : (userTime + systemTime) / realTime);
            lastUsage = nextUsage;
            lastRTime = nextTime;
        }
        break;

    case GCTimeEnd:
        {
            struct rusage rusage;
            if (getrusage(RUSAGE_SELF, &rusage) != 0)
                return;
            lastUsage = rusage;
            long pageFaults = GetPaging(rusage.ru_majflt);
            subTimevals(&rusage.ru_utime, &startUsage.ru_utime);
            subTimevals(&rusage.ru_stime, &startUsage.ru_stime);
            totalGCUserCPU.add(rusage.ru_utime);
            totalGCSystemCPU.add(rusage.ru_stime);
            struct timeval tv;
            if (gettimeofday(&tv, NULL) != 0)
                return;
            lastRTime = tv;
            subTimevals(&tv, &startRTime);
            totalGCReal.add(tv);

            if (debugOptions & DEBUG_GC)
            {
                float userTime = timevalToSeconds(&rusage.ru_utime);
                float systemTime = timevalToSeconds(&rusage.ru_stime);
                float realTime = timevalToSeconds(&tv);
                Log("GC: CPU user: %0.3f system: %0.3f real: %0.3f speed up %0.1f page faults %ld\n", userTime, 
                    systemTime, realTime, realTime == 0.0 ? 0.0 :(userTime + systemTime) / realTime,
                    pageFaults-startPF);
            }
            minorGCUserCPU.add(rusage.ru_utime);
            majorGCUserCPU.add(rusage.ru_utime);
            minorGCSystemCPU.add(rusage.ru_stime);
            majorGCSystemCPU.add(rusage.ru_stime);
            minorGCReal.add(tv);
            majorGCReal.add(tv);
            minorGCPageFaults += pageFaults - startPF;
            majorGCPageFaults += pageFaults - startPF;
            startRTime = lastRTime;
            startPF = pageFaults;
            startUsage = lastUsage;
            globalStats.copyGCTimes(totalGCUserCPU, totalGCSystemCPU);
        }
    }
#endif
}

Handle HeapSizeParameters::getGCUtime(TaskData *taskData) const
{
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    return Make_arb_from_pair(taskData, ((FILETIME)totalGCUserCPU).dwHighDateTime, ((FILETIME)totalGCUserCPU).dwLowDateTime);
#else
    return Make_arb_from_pair_scaled(taskData, ((struct timeval)totalGCUserCPU).tv_sec, ((struct timeval)totalGCUserCPU).tv_usec, 1000000);
#endif
}

Handle HeapSizeParameters::getGCStime(TaskData *taskData) const
{
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    return Make_arb_from_pair(taskData, ((FILETIME)totalGCSystemCPU).dwHighDateTime, ((FILETIME)totalGCSystemCPU).dwLowDateTime);
#else
    return Make_arb_from_pair_scaled(taskData, ((struct timeval)totalGCSystemCPU).tv_sec, ((struct timeval)totalGCSystemCPU).tv_usec, 1000000);
#endif
}

void HeapSizeParameters::Init()
{
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    // Record an initial time of day to use as the basis of real timing
    FILETIME s;
    GetSystemTimeAsFileTime(&s);
#else
    struct timeval s;
    gettimeofday(&s, NULL);
#endif
    startTime.add(s);

    resetMajorTimingData();
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    memset(&startUsageU, 0, sizeof(startUsageU));
    memset(&startUsageS, 0, sizeof(startUsageS));
    memset(&lastUsageU, 0, sizeof(lastUsageU));
    memset(&lastUsageS, 0, sizeof(lastUsageS));
    memset(&lastRTime, 0, sizeof(lastRTime));
    GetSystemTimeAsFileTime(&startRTime);
    startPF = GetPaging(0);
#else
    memset(&startUsage, 0, sizeof(startUsage));
    memset(&lastUsage, 0, sizeof(lastUsage));
    memset(&lastRTime, 0, sizeof(lastRTime));
    gettimeofday(&startRTime, NULL);
    startPF = GetPaging(0);
#endif
}

void HeapSizeParameters::Final()
{
    // Print the overall statistics
    if (debugOptions & (DEBUG_GC|DEBUG_HEAPSIZE))
    {
        TIMEDATA userTime, systemTime, realTime;
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        FILETIME kt, ut;
        FILETIME ct, et; // Unused
        FILETIME rt;
        GetProcessTimes(GetCurrentProcess(), &ct, &et, &kt, &ut);
        GetSystemTimeAsFileTime(&rt);
        userTime.add(ut);
        systemTime.add(kt);
        realTime.add(rt);
 #else
        struct rusage rusage;
        struct timeval tv;
        if (getrusage(RUSAGE_SELF, &rusage) != 0 || gettimeofday(&tv, NULL) != 0)
            return;
        userTime.add(rusage.ru_utime);
        systemTime.add(rusage.ru_stime);
        realTime.add(tv);
#endif
        realTime.sub(startTime);
        userTime.sub(totalGCUserCPU);
        systemTime.sub(totalGCSystemCPU);
        realTime.sub(totalGCReal);
        if (debugOptions & DEBUG_GC)
        {
            Log("GC (Total): Non-GC time: CPU user: %0.3f system: %0.3f real: %0.3f\n",
                userTime.toSeconds(), systemTime.toSeconds(), realTime.toSeconds());
            Log("GC (Total): GC time: CPU user: %0.3f system: %0.3f real: %0.3f\n",
                totalGCUserCPU.toSeconds(), totalGCSystemCPU.toSeconds(), totalGCReal.toSeconds());
        }
        if (debugOptions & DEBUG_HEAPSIZE)
        {
            TIMEDATA gc, nonGc;
            gc.add(totalGCUserCPU);
            gc.add(totalGCSystemCPU);
            nonGc.add(userTime);
            nonGc.add(systemTime);
            Log("Heap: Total CPU GC time %0.3fsecs,  Non-GC %0.3fsecs, ratio %0.3f\n",
                gc.toSeconds(), nonGc.toSeconds(), gc.toSeconds() / nonGc.toSeconds());
        }
    }
}


void HeapSizeParameters::resetMinorTimingData(void)
{
    minorNonGCUserCPU.fromSeconds(0);
    minorNonGCSystemCPU.fromSeconds(0);
    minorNonGCReal.fromSeconds(0);
    minorGCUserCPU.fromSeconds(0);
    minorGCSystemCPU.fromSeconds(0);
    minorGCReal.fromSeconds(0);
    minorGCPageFaults = 0;
}

void HeapSizeParameters::resetMajorTimingData(void)
{
    resetMinorTimingData();
    majorNonGCUserCPU.fromSeconds(0);
    majorNonGCSystemCPU.fromSeconds(0);
    majorNonGCReal.fromSeconds(0);
    majorGCUserCPU.fromSeconds(0);
    majorGCSystemCPU.fromSeconds(0);
    majorGCReal.fromSeconds(0);
    majorGCPageFaults = 0;
    minorGCsSinceMajor = 0;
}


class HeapSizing: public RtsModule
{
public:
    virtual void Init(void);
    virtual void Stop(void);
};

// Declare this.  It will be automatically added to the table.
static HeapSizing heapSizeModule;

void HeapSizing::Init(void)
{
    gHeapSizeParameters.Init();
}

void HeapSizing::Stop()
{
    gHeapSizeParameters.Final();
}


// Return the physical memory size.  Returns the maximum unsigned integer value if
// it won't .
#if defined(HAVE_WINDOWS_H)

// Define this here rather than attempting to use MEMORYSTATUSEX since
// it may not be in the include and we can't easily test.  The format
// of MEMORYSTATUSVLM is the same.
typedef struct _MyMemStatusEx {
    DWORD dwLength;
    DWORD dwMemoryLoad;
    DWORDLONG ullTotalPhys;
    DWORDLONG ullAvailPhys;
    DWORDLONG ullTotalPageFile;
    DWORDLONG ullAvailPageFile;
    DWORDLONG ullTotalVirtual;
    DWORDLONG ullAvailVirtual;
    DWORDLONG ullAvailExtendedVirtual;
} MyMemStatusEx;

typedef VOID (WINAPI *GLOBALMEMSLVM)(MyMemStatusEx *);
typedef BOOL (WINAPI *GLOBALMEMSEX)(MyMemStatusEx *);
#endif


static POLYUNSIGNED GetPhysicalMemorySize(void)
{
    POLYUNSIGNED maxMem = 0-1; // Maximum unsigned value.
#if defined(HAVE_WINDOWS_H)
    {
        // This is more complicated than it needs to be.  GlobalMemoryStatus
        // returns silly values if there is more than 4GB so GlobalMemoryStatusEx
        // is preferred.  However, it is not in all the include files and may not
        // be in kernel32.dll in pre-XP versions.  Furthermore at one point it was
        // called GlobalMemoryStatusVlm.  The only way to do this portably is the
        // hard way.
        HINSTANCE hlibKernel = LoadLibrary("kernel32.dll");
        if (hlibKernel)
        {
            MyMemStatusEx memStatEx;
            memset(&memStatEx, 0, sizeof(memStatEx));
            memStatEx.dwLength = sizeof(memStatEx);
            GLOBALMEMSEX globalMemStatusEx =
                (GLOBALMEMSEX)GetProcAddress(hlibKernel, "GlobalMemoryStatusEx");
            GLOBALMEMSLVM globalMemStatusVlm =
                (GLOBALMEMSLVM)GetProcAddress(hlibKernel, "GlobalMemoryStatusVlm");
            if (globalMemStatusEx && ! (*globalMemStatusEx)(&memStatEx))
                memStatEx.ullTotalPhys = 0; // Clobber any rubbish since it says it failed.
            else if (globalMemStatusVlm)
                // GlobalMemoryStatusVlm returns VOID so we assume it worked
                (*globalMemStatusVlm) (&memStatEx);
            FreeLibrary(hlibKernel);
            if (memStatEx.ullTotalPhys) // If it's non-zero assume it succeeded
            {
                DWORDLONG dwlMax = maxMem;
                if (memStatEx.ullTotalPhys > dwlMax)
                    return maxMem;
                else
                    return (POLYUNSIGNED)memStatEx.ullTotalPhys;
           }
        }
        // Fallback if that fails.

        MEMORYSTATUS memStatus;
        memset(&memStatus, 0, sizeof(memStatus));
        GlobalMemoryStatus(&memStatus);
        if (memStatus.dwTotalPhys > maxMem)
            return maxMem;
        else
            return (POLYUNSIGNED)memStatus.dwTotalPhys;
    }

#endif
#if defined(_SC_PHYS_PAGES) && defined(_SC_PAGESIZE)
    {
        // Linux and Solaris.  This gives a silly value in Cygwin.
        long physPages      = sysconf(_SC_PHYS_PAGES);
        long physPagesize   = sysconf(_SC_PAGESIZE);
        if (physPages != -1 && physPagesize != -1)
        {
            unsigned long maxPages = maxMem / physPagesize;
            if ((unsigned long)physPages > maxPages)
                return maxMem;
            else // We've checked it won't overflow.
                return physPages*physPagesize;
        }
    }
#endif
#if defined(HAVE_SYSCTL) && defined(CTL_HW)
    // FreeBSD and Mac OS X.  It seems HW_MEMSIZE has been added to
    // Max OS X to return a 64-bit value.
#ifdef HW_MEMSIZE
    {
        static int mib[2] = { CTL_HW, HW_MEMSIZE };
        uint64_t physMem = 0;
        size_t len = sizeof(physMem);
        if (sysctl(mib, 2, &physMem, &len, NULL, 0) == 0 && len == sizeof(physMem))
        {
            if (physMem > (uint64_t)maxMem)
                return maxMem;
            else
                return (POLYUNSIGNED)physMem;
        }
    }
#endif
#ifdef HW_PHYSMEM
    // If HW_MEMSIZE isn't there or the call failed try this.
    {
        static int mib[2] = { CTL_HW, HW_PHYSMEM };
        unsigned int physMem = 0;
        size_t len = sizeof(physMem);
        if (sysctl(mib, 2, &physMem, &len, NULL, 0) == 0 && len == sizeof(physMem))
        {
            if (physMem > maxMem)
                return maxMem;
            else
                return physMem;
        }
    }
#endif
#endif
    return 0; // Unable to determine
}

