/*
    Title:      Multi-Threaded Garbage Collector

    Copyright (c) 2010-12 David C. J. Matthews

    Based on the original garbage collector code
        Copyright 2000-2008
        Cambridge University Technical Services Limited

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

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h> // Used in both Windows and Cygwin
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
#include "run_time.h"
#include "machine_dep.h"
#include "diagnostics.h"
#include "processes.h"
#include "timing.h"
#include "gc.h"
#include "scanaddrs.h"
#include "check_objects.h"
#include "osmem.h"
#include "bitmap.h"
#include "rts_module.h"
#include "memmgr.h"
#include "gctaskfarm.h"
#include "mpoly.h"
#include "statistics.h"
#include "profiling.h"

// The immutable and mutable segment sizes are the default units of allocation
// for new address spaces.
//static POLYUNSIGNED    immutableSegSize, mutableSegSize;
//static POLYUNSIGNED    immutableFreeSpace, mutableFreeSpace;
//static POLYUNSIGNED    immutableMinFree, mutableMinFree; // Probably remove

static GCTaskFarm gTaskFarm; // Global task farm.
GCTaskFarm *gpTaskFarm = &gTaskFarm;

// If the GC converts a weak ref from SOME to NONE it sets this ref.  It can be
// cleared by the signal handler thread.  There's no need for a lock since it
// is only set during GC and only cleared when not GCing.
bool convertedWeak = false;

/*
    How the garbage collector works.
    The GC has two phases.  The minor (quick) GC is a copying collector that
    copies data from the allocation area into the mutable and immutable area.
    The major collector is started when either the mutable or the immutable
    area is full.  The major collector uses a mark/sweep scheme.
    The GC has three phases:

    1.  Mark phase.
    Working from the roots; which are the the permanent mutable segments and
    the RTS roots (e.g. thread stacks), mark all reachable cells.
    Marking involves setting bits in the bitmap for reachable words.

    2. Compact phase.
    Marked objects are copied to try to compact, upwards, the heap segments.  When
    an object is moved the length word of the object in the old location is set as
    a tombstone that points to its new location.  In particular this means that we
    cannot reuse the space where an object previously was during the compaction phase.
    Immutable objects are moved into immutable segments.  When an object is moved
    to a new location the bits are set in the bitmap as though the object had been
    marked at that location.

    3. Update phase.
    The roots and objects marked during the first two phases are scanned and any
    addresses for moved objects are updated.  The lowest address used in the area
    then becomes the base of the area for future allocations.

    Updated DCJM 02/02/12

*/

#define INRANGE(val,start,end) ((start) <= (val) && (val) < (end))

// For the current, crude heap allocation strategy this is the heap
// size (less the allocation area) given on the command line.
static POLYUNSIGNED majorGCFree;

// Called at the end of collection.  This is where we should do the
// fine adjustment of the heap size to minimise the GC time.
// Growing the heap is just a matter of adjusting the limits.  We
// don't actually need to allocate the space here.
// See also adjustHeapSizeAfterMinorGC for adjustments after a minor GC.
static void adjustHeapSize()
{
    TIMEDATA gc, total;
    gc.add(gcTimeData.majorGCSystemCPU);
    gc.add(gcTimeData.majorGCUserCPU);
    total.add(gc);
    total.add(gcTimeData.majorNonGCSystemCPU);
    total.add(gcTimeData.majorNonGCUserCPU);
    float g = gc.toSeconds() / total.toSeconds();
    // A very crude adjustment at this stage:
    // Keep the allocation area the same,
    // Set the major GC size (mutable+immutable area) so
    // that there is a fixed amount free.
    POLYUNSIGNED spaceUsed = 0;
    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        spaceUsed += gMem.lSpaces[i]->allocatedSpace();
    }
    float l = (float)spaceUsed / (float)gMem.SpaceBeforeMajorGC();
    if (debugOptions & DEBUG_HEAPSIZE)
    {
        Log("Heap: Major resizing factors g = %f, l = %f\n", g, l);
        Log("Heap: Resizing from %"POLYUFMT" to %"POLYUFMT"\n", gMem.SpaceBeforeMajorGC(), majorGCFree+spaceUsed);
    }
    // Set the sizes.
    gMem.SetSpaceSizes(
        gMem.SpaceBeforeMinorGC()/* i.e. unchanged*/, majorGCFree+spaceUsed);
}

static bool doGC(const POLYUNSIGNED wordsRequiredToAllocate)
{
    unsigned j;

    gcTimeData.RecordGCTime(GcTimeData::GCTimeStart);
    globalStats.incCount(PSC_GC_FULLGC);

    if (debugOptions & DEBUG_GC)
        Log("GC: Full GC, %lu words required %u spaces\n", wordsRequiredToAllocate, gMem.nlSpaces);

/*
 * There is a really weird bug somewhere.  An extra bit may be set in the bitmap during
 * the mark phase.  It seems to be related to heavy swapping activity.  Duplicating the
 * bitmap causes it to occur only in one copy and write-protecting the bitmap apart from
 * when it is actually being updated does not result in a seg-fault.  So far I've only
 * seen it on 64-bit Linux but it may be responsible for other crashes.  The work-around
 * is to check the number of bits set in the bitmap and repeat the mark phase if it does
 * not match.
 */
    
    for (unsigned p = 3; p > 0; p--)
    {
        for(j = 0; j < gMem.nlSpaces; j++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j];
            ASSERT (lSpace->top >= lSpace->upperAllocPtr);
            ASSERT (lSpace->upperAllocPtr >= lSpace->lowerAllocPtr);
            ASSERT (lSpace->lowerAllocPtr >= lSpace->bottom);
            // Set upper and lower limits of weak refs.
            lSpace->highestWeak = lSpace->bottom;
            lSpace->lowestWeak = lSpace->top;
            lSpace->fullGCLowerLimit = lSpace->top;
            // Put dummy objects in the unused space.  This allows
            // us to scan over the whole of the space.
            gMem.FillUnusedSpace(lSpace->lowerAllocPtr,
                lSpace->upperAllocPtr-lSpace->lowerAllocPtr);
        }

        // Set limits of weak refs.
        for (j = 0; j < gMem.npSpaces; j++)
        {
            PermanentMemSpace *pSpace = gMem.pSpaces[j];
            pSpace->highestWeak = pSpace->bottom;
            pSpace->lowestWeak = pSpace->top;
        }

        /* Mark phase */
        GCMarkPhase();
        
        POLYUNSIGNED bitCount = 0, markCount = 0;
        
        for (j = 0; j < gMem.nlSpaces; j++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j]; 
            markCount += lSpace->i_marked + lSpace->m_marked;
            bitCount += lSpace->bitmap.CountSetBits(lSpace->spaceSize());
        }
        
        if (markCount == bitCount)
            break;
        else
        {
            // Report an error.  If this happens again we crash.
            Log("GC: Count error for space %u - mark count %lu, bitCount %lu\n", j, markCount, bitCount);
            if (p == 1)
            {
                ASSERT(markCount == bitCount);
            }
        }
    }
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        // Reset the allocation pointers.  They will be set to the
        // limits of the retained data.
        lSpace->lowerAllocPtr = lSpace->bottom;
        lSpace->upperAllocPtr = lSpace->top;
    }

    if (debugOptions & DEBUG_GC) Log("GC: Check weak refs\n");
    /* Detect unreferenced streams, windows etc. */
    GCheckWeakRefs();

    // Check that the heap is not overfull.  We make sure the marked
    // mutable and immutable data is no more than 90% of the
    // corresponding areas.  This is a very coarse adjustment.
    {
        POLYUNSIGNED iMarked = 0, mMarked = 0;
        POLYUNSIGNED iSpace = 0, mSpace = 0;
        for (unsigned i = 0; i < gMem.nlSpaces; i++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[i];
            iMarked += lSpace->i_marked;
            mMarked += lSpace->m_marked;
            if (! lSpace->allocationSpace)
            {
                if (lSpace->isMutable)
                    mSpace += lSpace->spaceSize();
                else
                    iSpace += lSpace->spaceSize();
            }
        }
        // Add space if necessary and possible.
        while (iMarked > iSpace - iSpace/10 && gMem.NewLocalSpace(gMem.DefaultSpaceSize(), false) != 0)
            iSpace += gMem.DefaultSpaceSize();
        while (mMarked > mSpace - mSpace/10 && gMem.NewLocalSpace(gMem.DefaultSpaceSize(), true) != 0)
            mSpace += gMem.DefaultSpaceSize();
    }

    /* Compact phase */
    POLYUNSIGNED immutable_overflow = 0; // The immutable space we couldn't copy out.
    GCCopyPhase();

    gcTimeData.RecordGCTime(GcTimeData::GCTimeIntermediate, "Copy");

    // Update Phase.
    if (debugOptions & DEBUG_GC) Log("GC: Update\n");
    GCUpdatePhase();

    gcTimeData.RecordGCTime(GcTimeData::GCTimeIntermediate, "Update");

    {
        POLYUNSIGNED iUpdated = 0, mUpdated = 0, iMarked = 0, mMarked = 0;
        for(j = 0; j < gMem.nlSpaces; j++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j];
            iMarked += lSpace->i_marked;
            mMarked += lSpace->m_marked;
            if (lSpace->isMutable)
                mUpdated += lSpace->updated;
            else
                iUpdated += lSpace->updated;
        }
        ASSERT(iUpdated+mUpdated == iMarked+mMarked);
    }

    // Delete empty spaces.
    gMem.RemoveEmptyLocals();

    {
        // Make sure there is at least one mutable and one immutable area
        // for each thread so that the minor GC has something to copy into.
        unsigned nMutable = 0, nImmutable = 0;
        for (unsigned s = gMem.nlSpaces; s > 0; s--)
        {
            LocalMemSpace *space = gMem.lSpaces[s-1];
            if (space->freeSpace() > 1000)
            {
                // Some useful free space in the area.
                if (space->isMutable)
                    nMutable++;
                else nImmutable++;
            }
        }
        while (nImmutable < userOptions.gcthreads &&
               gMem.NewLocalSpace(gMem.DefaultSpaceSize(), false) != 0)
            nImmutable++;
        while (nMutable < userOptions.gcthreads &&
               gMem.NewLocalSpace(gMem.DefaultSpaceSize(), true) != 0)
            nMutable++;
    }

    // The allocation areas should have been emptied and deallocated.  We
    // want to create one that is big enough for the required data
    // to check that it's possible.  If it isn't then we've run out of
    // memory.
    bool haveSpace = true;
    if (wordsRequiredToAllocate != 0)
    {
        POLYUNSIGNED needed = gMem.DefaultSpaceSize();
        if (wordsRequiredToAllocate > needed)
            needed = wordsRequiredToAllocate;
        haveSpace = gMem.CreateAllocationSpace(needed) != 0;
    }

    if (debugOptions & DEBUG_GC)
    {
        for(j = 0; j < gMem.nlSpaces; j++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j];
            Log("GC: %s space %p %d free in %d words %2.1f%% full\n", lSpace->spaceTypeString(),
                lSpace, lSpace->freeSpace(), lSpace->spaceSize(),
                ((float)lSpace->allocatedSpace()) * 100 / (float)lSpace->spaceSize());
        }
    }

    // Compute values for statistics
    globalStats.setSize(PSS_AFTER_LAST_GC, 0);
    globalStats.setSize(PSS_AFTER_LAST_FULLGC, 0);
    globalStats.setSize(PSS_ALLOCATION, 0);
    globalStats.setSize(PSS_ALLOCATION_FREE, 0);

    for (j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[j];
        POLYUNSIGNED free = space->freeSpace();
        globalStats.incSize(PSS_AFTER_LAST_GC, free*sizeof(PolyWord));
        globalStats.incSize(PSS_AFTER_LAST_FULLGC, free*sizeof(PolyWord));
        if (space->allocationSpace)
        {
            globalStats.incSize(PSS_ALLOCATION, free*sizeof(PolyWord));
            globalStats.incSize(PSS_ALLOCATION_FREE, free*sizeof(PolyWord));
        }
#ifdef FILL_UNUSED_MEMORY
        memset(space->bottom, 0xaa, (char*)space->upperAllocPtr - (char*)space->bottom);
#endif
        if (debugOptions & DEBUG_GC)
            Log("GC: %s space %p %d free in %d words %2.1f%% full\n", space->spaceTypeString(),
                space, space->freeSpace(), space->spaceSize(),
                ((float)space->allocatedSpace()) * 100 / (float)space->spaceSize());
    }

    // End of garbage collection
    gcTimeData.RecordGCTime(GcTimeData::GCTimeEnd);

    // Now we've finished we can adjust the heap sizes.
    adjustHeapSize();
    gcTimeData.resetMajorTimingData();

    // Invariant: the bitmaps are completely clean.
    if (debugOptions & DEBUG_GC)
    {
        if (haveSpace)
            Log("GC: Completed successfully\n");
        else Log("GC: Completed with insufficient space\n");
    }

    if (profileMode == kProfileLiveData || profileMode == kProfileLiveMutables)
        printprofile();

    return haveSpace; // Completed
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


POLYUNSIGNED GetPhysicalMemorySize(void)
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

/* This macro must make a whole number of chunks */
#define K_to_words(k) ROUNDUP((k) * (1024 / sizeof(PolyWord)),BITSPERWORD)

// Create the initial heap.  hsize, isize and msize are the requested heap sizes
// from the user arguments in units of kbytes.
// Fills in the defaults and attempts to allocate the heap.  If the heap size
// is too large it allocates as much as it can.  The default heap size is half the
// physical memory.
void CreateHeap(unsigned hsize, unsigned isize, unsigned msize, unsigned asize, unsigned rsize)
{
    // If no -H option was given set the default initial size to half the memory.
    if (hsize == 0) {
        POLYUNSIGNED memsize = GetPhysicalMemorySize();
        if (memsize == 0) // Unable to determine memory size so default to 64M.
            memsize = 64 * 1024 * 1024;
        hsize = memsize / 2 / 1024;
    }
    isize += msize; msize = 0;
    if (hsize < isize+asize) hsize = isize+asize;

    // Defaults are half the heap for the immutable buffer, half for allocation
    if (isize == 0) isize = hsize / 2;  /* set default immutable buffer size */
    if (asize == 0) asize = hsize - isize;

    majorGCFree = K_to_words(isize);

    gMem.SetSpaceSizes(K_to_words(asize), majorGCFree);
    gMem.SetReservation(K_to_words(rsize));

    // Create an initial allocation space.
    if (gMem.CreateAllocationSpace(gMem.DefaultSpaceSize()) == 0)
        Exit("Insufficient memory to allocate the heap");

    // Try to create one mutable and one immutable area for each GC thread.
    // This is not essential but allows the minor GC to distibute work for
    // the GC threads.  Additional areas up to the maximum will be
    // allocated by the minor GC.  Leaving it to there means that the
    // proportion of mutable and immutable areas will depend on the
    // application.
    for (unsigned i = 0; i < userOptions.gcthreads; i++)
    {
        if (gMem.NewLocalSpace(gMem.DefaultSpaceSize(), false) == 0)
            break;
        if (gMem.NewLocalSpace(gMem.DefaultSpaceSize(), true) == 0)
            break;
    }

    // Create the task farm if required
    if (userOptions.gcthreads != 1)
        initialiseMultithreadGC(userOptions.gcthreads);
}

class FullGCRequest: public MainThreadRequest
{
public:
    FullGCRequest(): MainThreadRequest(MTP_GCPHASEMARK) {}
    virtual void Perform()
    {
        doGC (0);
    }
};

class QuickGCRequest: public MainThreadRequest
{
public:
    QuickGCRequest(POLYUNSIGNED words): MainThreadRequest(MTP_GCPHASEMARK), wordsRequired(words) {}

    virtual void Perform()
    {
        result =
#ifndef DEBUG_ONLY_FULL_GC
// If DEBUG_ONLY_FULL_GC is defined then we skip the partial GC.
            RunQuickGC() ||
#endif
            doGC (wordsRequired);
    }

    bool result;
    POLYUNSIGNED wordsRequired;
};

// Perform a full garbage collection.  This is called either from ML via the full_gc RTS call
// or from various RTS functions such as open_file to try to recover dropped file handles.
void FullGC(TaskData *taskData)
{
    FullGCRequest request;
    processes->MakeRootRequest(taskData, &request);

    if (convertedWeak)
        // Notify the signal thread to broadcast on the condition var when
        // the GC is complete.  We mustn't call SignalArrived within the GC
        // because it locks schedLock and the main GC thread already holds schedLock.
        processes->SignalArrived();
}

// This is the normal call when memory is exhausted and we need to garbage collect.
bool QuickGC(TaskData *taskData, POLYUNSIGNED wordsRequiredToAllocate)
{
    QuickGCRequest request(wordsRequiredToAllocate);
    processes->MakeRootRequest(taskData, &request);

    if (convertedWeak)
        processes->SignalArrived();

    return request.result;
}

void initialiseMultithreadGC(unsigned threads)
{
    if (! gTaskFarm.Initialise(threads, 100))
        Crash("Unable to initialise the GC task farm");
}

