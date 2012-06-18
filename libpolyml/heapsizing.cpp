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
    heapSizeOption = HEAPSIZING_DEFAULT;
    lastAllocationSucceeded = true;
}

/* This macro must make a whole number of chunks */
#define K_to_words(k) ROUNDUP((k) * (1024 / sizeof(PolyWord)),BITSPERWORD)

static POLYUNSIGNED GetPhysicalMemorySize(void);

// Set the initial size based on any parameters specified on the command line
void HeapSizeParameters::SetInitialSize(unsigned hsize)
{
    // If no -H option was given set the default initial size to half the memory.
    if (hsize == 0) {
        POLYUNSIGNED memsize = GetPhysicalMemorySize();
        if (memsize == 0) // Unable to determine memory size so default to 64M.
            memsize = 64 * 1024 * 1024;
        hsize = (unsigned)(memsize / 2 / 1024);
    }
    // Initially we divide the space equally between the major and
    // minor heaps.  That means that there will definitely be space
    // for the first minor GC to copy its data.  This division can be
    // changed later on.
    freeHeapSpace = K_to_words(hsize);
    gMem.SetSpaceForHeap(freeHeapSpace);
    gMem.SetSpaceBeforeMinorGC(freeHeapSpace/2);
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

#define HEURISTIC_GC_LOAD_FACTOR    10

// Called at the end of collection.  This is where we should do the
// fine adjustment of the heap size to minimise the GC time.
// Growing the heap is just a matter of adjusting the limits.  We
// don't actually need to allocate the space here.
// See also adjustHeapSizeAfterMinorGC for adjustments after a minor GC.
void HeapSizeParameters::AdjustSizeAfterMajorGC()
{
    TIMEDATA gc, nonGc, total;
    gc.add(majorGCSystemCPU);
    gc.add(majorGCUserCPU);
    nonGc.add(majorNonGCSystemCPU);
    nonGc.add(majorNonGCUserCPU);
    total.add(gc);
    total.add(nonGc);
    float g = gc.toSeconds() / total.toSeconds();
//    float desiredLoad = ((float)HEURISTIC_GC_LOAD_FACTOR) / (float)100.0;

    // A very crude adjustment at this stage:
    // Keep the allocation area the same,
    // Set the major GC size (mutable+immutable area) so
    // that there is a fixed amount free.
    POLYUNSIGNED spaceUsed = 0;
    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        spaceUsed += gMem.lSpaces[i]->allocatedSpace();
    }
    float l = (float)spaceUsed / (float)gMem.SpaceForHeap();
    if (debugOptions & DEBUG_HEAPSIZE)
    {
        Log("Heap: Major resizing factors g = %f, l = %f\n", g, l);
        Log("Heap: Resizing from %"POLYUFMT" to %"POLYUFMT"\n", gMem.SpaceForHeap(), freeHeapSpace+spaceUsed);
    }
    // Set the sizes.
    gMem.SetSpaceForHeap(freeHeapSpace+spaceUsed);
}


// Trigger a full GC next time if this GC took more than this
// percentage of time.
#define HEURISTIC_TRIGGER_FULL_GC_PERCENT   50

// Called after a minor GC.  Currently does nothing.
// See also adjustHeapSize for adjustments after a major GC.
void HeapSizeParameters::AdjustSizeAfterMinorGC(POLYUNSIGNED spaceAfterGC, POLYUNSIGNED spaceBeforeGC)
{
    POLYUNSIGNED spaceCopiedOut = spaceAfterGC-spaceBeforeGC;
    TIMEDATA gc, total;
    gc.add(minorGCSystemCPU);
    gc.add(minorGCUserCPU);
    total.add(gc);
    total.add(minorNonGCSystemCPU);
    total.add(minorNonGCUserCPU);
    float g = gc.toSeconds() / total.toSeconds();
    // In this case we compute l as the live memory in the allocation area
    // divided by the size of the allocation area.
    float l = (float)spaceCopiedOut / (float)gMem.SpaceBeforeMinorGC();
    if (debugOptions & DEBUG_HEAPSIZE)
    {
        Log("Heap: Space before %"POLYUFMT", space after %"POLYUFMT"\n", spaceBeforeGC, spaceAfterGC);
        Log("Heap: Minor resizing factors g = %f, l = %f\n", g, l);
    }
    // Set the space available for the allocation area to be the difference between the
    // total heap size and the allowed heap size together with as much space as we copied
    // on this GC.  That allows for the next minor GC to copy the same amount without
    // extending the heap.  If the next minor GC adds more than this the heap will be
    // extended and a corresponding amount deducted so that the heap shrinks again.
    POLYUNSIGNED nonAlloc = gMem.CurrentHeapSize() - gMem.CurrentAllocSpace() + spaceCopiedOut;
    POLYUNSIGNED allowedAlloc =
        nonAlloc >= gMem.SpaceForHeap() ? 0 : gMem.SpaceForHeap() - nonAlloc;
    if (gMem.CurrentAllocSpace() != allowedAlloc)
    {
        if (debugOptions & DEBUG_HEAPSIZE)
            Log("Heap: Adjusting space for allocation area from %"POLYUFMT" to %"POLYUFMT"\n",
                gMem.SpaceBeforeMinorGC(), allowedAlloc);
        gMem.SetSpaceBeforeMinorGC(allowedAlloc);
     }

    // If we took more than this time we probably want a full GC next time.
    if (g > ((float)HEURISTIC_TRIGGER_FULL_GC_PERCENT)/100.0)
        fullGCNextTime = true;
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
                Log("GC: Non-GC time: CPU user: %0.3f system: %0.3f real: %0.3f page faults: %u\n",
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
    if (debugOptions & DEBUG_GC)
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
        Log("GC (Total): Non-GC time: CPU user: %0.3f system: %0.3f real: %0.3f\n",
            userTime.toSeconds(), systemTime.toSeconds(), realTime.toSeconds());
        Log("GC (Total): GC time: CPU user: %0.3f system: %0.3f real: %0.3f\n",
            totalGCUserCPU.toSeconds(), totalGCSystemCPU.toSeconds(), totalGCReal.toSeconds());
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
}

void HeapSizeParameters::resetMajorTimingData(void)
{
    minorNonGCUserCPU.fromSeconds(0);
    minorNonGCSystemCPU.fromSeconds(0);
    minorNonGCReal.fromSeconds(0);
    minorGCUserCPU.fromSeconds(0);
    minorGCSystemCPU.fromSeconds(0);
    minorGCReal.fromSeconds(0);
    majorNonGCUserCPU.fromSeconds(0);
    majorNonGCSystemCPU.fromSeconds(0);
    majorNonGCReal.fromSeconds(0);
    majorGCUserCPU.fromSeconds(0);
    majorGCSystemCPU.fromSeconds(0);
    majorGCReal.fromSeconds(0);
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

