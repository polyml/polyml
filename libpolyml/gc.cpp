/*
    Title:      Multi-Threaded Garbage Collector

    Copyright (c) 2010, 2011 David C. J. Matthews

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
static POLYUNSIGNED    immutableSegSize, mutableSegSize;
static POLYUNSIGNED    immutableFreeSpace, mutableFreeSpace;
static POLYUNSIGNED    immutableMinFree, mutableMinFree; // Probably remove

static GCTaskFarm gTaskFarm; // Global task farm.
GCTaskFarm *gpTaskFarm = &gTaskFarm;

// If the GC converts a weak ref from SOME to NONE it sets this ref.  It can be
// cleared by the signal handler thread.  There's no need for a lock since it
// is only set during GC and only cleared when not GCing.
bool convertedWeak = false;

/*
    How the garbage collector works.
    The GC is generational.  There are two modes: minor and full.  Most of the
    code is the same for both.  There are two types of local heap: mutable and
    immutable.  ML and RTS code always allocate new objects in a mutable heap.
    Note allocation is from the top of the area down.
    Originally, there were just two areas but now there may be multiple
    heap segments of each type.  The GC has three phases:

    1.  Mark phase.
    Working from the roots; which are the the permanent mutable segments, the RTS
    roots (e.g. thread stacks) and, if this is a minor collection, mutable objects
    collected in previous collections ("gen_top" to "top"), mark all reachable cells.
    Marking involves setting bits in the bitmap for reachable words.  If this is a
    minor collection we only follow cells that are in the current generation
    ("gen_bottom" to "gen_top").

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

    Typically, a minor GC moves immutable data into the immutable area and leaves
    mutable data behind.  The immutable data moved is considered "old" and not
    scanned until a major collection.  However, if a collection finds that there
    are significant holes in the heap (these holes must be in the mutable area)
    it is better to try to recollect the current generation.  In this case the
    immutable data moved during this collection are considered as "new" in the
    next minor collection.  Even though we're only concerned there with compacting
    the mutable area we have to process immutable objects that may contain their
    addresses.
    DCJM 27/6/09
*/

/*
  The comments below may still be relevant.  I've left them in because they
  contain notes about optimisations that were tried in the past and no longer
  apply.

  How the garbage collector works.

  Phase 1: Starting from the roots in the old mutable area, and
           any pointers kept by the runtime system, we mark all
           objects that are found within the two GC areas.
           The GC areas extend from the allocation pointer
           to the top of the area.

  Phase 2: Then we scan the immutable object bitmap. When we find
           a mutable object we try to find space for it in the mutable
           area, if we find an immutable object we try to find space
           for it further up the immutable area. We may have to extend
           the mutable area to make room for objects since we must not commit
           and leave mutable objects in the immutable area.

           Then we do the same for the mutable area, copying immutable objects
           out into the immutable area, and moving mutable objects up.

           We keep track of the lowest object that could not be moved.
           The allocation pointers will be reset to the lowest kept objects,
           and the area below is taken to be free.

  Phase 3: Then we start from the roots and runtime system objects and
           look for pointers into the GC areas that point to tombstones.
           These pointers are changed to point to the new position of
           the objects. Then we process all the objects in the areas
           doing the same thing.

Further notes:

  The order of processing the immutable and mutable area has been changed
  since the above comment was written (by Dave Matthews?).

  It would be nice to combine phases 2 and 3 - we could traverse the
  reachable data-structures, starting at the roots, adjusting pointers
  as we go (rather like copyGC). We would only use the bitmap created
  in phase 1 to tell us where to find space to move the new objects.

  The main advantage of this approach is that is likely to be
  quicker - we only have to traverse the new (small?) data-structure
  rather than scanning the (large) mutable buffer.

  The disadvantage is that it would leave part of the heap dirty,
  and I think parts of the RTS may depend on any unused heap
  word containing zeroes. I'll have to look at this very closely!
  Note that this is a different issue from the compiler requiring
  the area below the allocation area to be zeroed. (Should we
  fix this?) Here we are talking about the area *above* the
  allocation pointer, which may contain objects, tombstones
  and zero words only.

  A second disadvantage is that the "compress" pass may not give
  as good compression as currently, because it wouldn't explicitly
  start at the bottom and work up. In recompense, we would be able
  to recycle all but the length word of a tombstone, so our
  actual space usage might improve.

  SPF 21/10/96

  I've now deleted all that carefully optimised code that used to zero the
  heap - it's now the responsibility of the compiler (and alloc) to ensure
  that the store is correctly initialised whenever a GC might occur.

  SPF 22/10/96

  The GC is required to "clean" each area of the heap between pointer and top;
  this area may only contain objects, tombstones and zero words. The GC
  currently does this for the benefit of OpMutableBlock, but this behaviour
  is also required for the PrintLocalProfileCounts in run_time.c to avoid
  core dumps.

  SPF 23/10/96

  Let's try to improve the design of the garbage collector, by doing partial GCs
  in 5 phases:

     (1) Mark
     (2) CopyImmutables
     (3) FixupImmutable
     (4) CopyMutables
     (5) FixupMutables

   What are the advantages/disadvantages of the new approach?

       Advantage:

           We can copy mutables into the holes left by copying-out immutables,
           which gives better compaction of the mutable area. The inability
           to do this is currently a problem for some applications because
           it triggers far too many full GCs.

       Disadvantage:

           We have to run the copy and fix-up phases twice. This may be expensive.

   Can we get the advantage without the disadvantage by only splitting the Copy
   and Fixup phases when this looks like a win?

   Note: we have to separate the Mark and Copy phases, as otherwise we won't be
   able to handle weak pointers. Shame!

   SPF 17/12/1997
*/

#define INRANGE(val,start,end) ((start) <= (val) && (val) < (end))
// Try to allocate another heap segment.  It tries to allocate the requested size

// but if that fails it allocates what it can.
static bool TryMoreHeap(POLYUNSIGNED size, bool mut)
{
    do {
        if (gMem.NewLocalSpace(size, mut))
            return true;
        // Otherwise try with half the size and stop when
        // it's less than 64k words.
        size = size / 2;
    } while (size > 64*1024);

    return false;
}

/* The problem with this version of PossiblyExpandArea is that it doesn't always expand
   it enough for the subsequent compaction phase to actually liberate wordsRequiredToAllocate
   of free space. SPF 31/7/96
*/

// This function is called after the mark phase of a FULL garbage collection to
// expand the immutable area if necessary.  wordsNeeded is the amount of immutable
// data detected during the mark phase.
static void PossiblyExpandImmutableArea(const POLYUNSIGNED wordsNeeded)
{
    POLYUNSIGNED currentSize = 0;
    unsigned nISpaces = 0; // Number of immutable spaces already
    for (unsigned j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[j];
        if (! space->isMutable)
        {
            currentSize += space->top - space->bottom;
            nISpaces++;
        }
    }

    if (immutableFreeSpace + wordsNeeded > currentSize) // need to get some more space
    {
        // We want to ensure that we have immutableFreeSpace free after this
        // collection.  We allocate in units of immutableSegSize so as not to
        // have too many small segments.
        POLYUNSIGNED requestedGrowth = immutableFreeSpace + wordsNeeded - currentSize;
        if (requestedGrowth < immutableSegSize)
            requestedGrowth = immutableSegSize;
        // Make the segments larger if we have already allocated several.
        // The factors here are a guess.  Maybe tune them more carefully
        unsigned spaceFactor = nISpaces / (3 * userOptions.gcthreads);
        while (spaceFactor > 0) { requestedGrowth += immutableSegSize; spaceFactor--; }

        POLYUNSIGNED chunks  = ROUNDUP_UNITS(requestedGrowth, BITSPERWORD);
        POLYUNSIGNED words   = chunks * BITSPERWORD;

        // Allocate a new space for each GC thread
        for (unsigned i = 0; i < userOptions.gcthreads; i++)
            (void)TryMoreHeap(words, false); // If this fails just carry on with what we have.
    }
}


/* This function CHECKS whether we have enough space AFTER the compaction phase.
   Returns the number of spaces we need to create. */
static unsigned BufferIsReallyFull(bool mutableRegion, POLYUNSIGNED wordsNeeded, const bool fullGC)
{
    // This is the space we need to be free.  If this is a mutable area wordsNeeded is the
    // space needed to allocate the object whose attempted allocation triggered this collection.
    // It needs to be available in at least one mutable area.  If this is an immutable area
    // wordsNeeded is the amount of space needed for immutable objects that couldn't be copied
    // out of the mutable area so doesn't need to be contiguous.
    POLYUNSIGNED requiredFree;
    if (mutableRegion)
        requiredFree = fullGC ? mutableFreeSpace: mutableMinFree;
    else
    {
        requiredFree = fullGC ? immutableFreeSpace: immutableMinFree;
        requiredFree += wordsNeeded / userOptions.gcthreads;
        wordsNeeded = 0;
    }
    // We need the minimum free space in as many spaces as we have GC threads.
    // Because we allocate in the spaces in a fixed order if we are short of
    // space that will be because all but the last few spaces are completely full.
    unsigned spaceAvailableIn = 0;

    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        LocalMemSpace *space = gMem.lSpaces[i];
        if (space->isMutable == mutableRegion)
        {
            POLYUNSIGNED currentlyFree = space->freeSpace();
            if (currentlyFree >= wordsNeeded)
            {
                currentlyFree -= wordsNeeded;
                wordsNeeded = 0;
            }
            if (currentlyFree >= requiredFree)
                spaceAvailableIn++;
        }
    }
    if (spaceAvailableIn < userOptions.gcthreads) // Insufficient space
        return userOptions.gcthreads - spaceAvailableIn;
    else if (wordsNeeded != 0)
        return 1; // We need a space to allocate the new object
    else
        return 0;
}

// AFTER a full GC, make sure we have a full buffer's worth of free space available.
static void AdjustHeapSize(bool isMutableSpace, POLYUNSIGNED wordsRequired)
{
    // Do we need any new spaces?
    unsigned newSpaces = BufferIsReallyFull(isMutableSpace, wordsRequired, true);
    const POLYUNSIGNED segSize =
        isMutableSpace ? mutableSegSize : immutableSegSize;

    if (newSpaces > 0)
    {
        // We need some more space
        for (unsigned j = 0; j < newSpaces; j++)
        {    // expand the heap.
            POLYUNSIGNED requestedGrowth = wordsRequired;
            wordsRequired = 0; // We don't need to include this in any other spaces.
            if (requestedGrowth < segSize)
                requestedGrowth = segSize;
            POLYUNSIGNED chunks  = ROUNDUP_UNITS(requestedGrowth, BITSPERWORD);
            POLYUNSIGNED words   = chunks * BITSPERWORD;
            (void)TryMoreHeap(words, isMutableSpace); // If this fails just carry on with what we have.
        }
    }
    else
    {
        // The reason for shrinking the heap is to reduce the swap space and
        // possibly the address space requirements.  This may be necessary if
        // we have finished building a large data structure and now want to
        // export it.  The export code requires buffer space and may need the
        // space we're using.
        // Another reason is to get rid of old saved state areas that have been
        // converted into local areas.  These are likely to be small compared with the
        // heap and result in fragmentation of the address space.
        // Count the number of empty spaces.  We don't allow this to get below the number
        // of GC threads.
        unsigned nSpaces = 0;
        POLYUNSIGNED allocSpace = 0, freeSpace = 0;
        for (unsigned j = 0; j < gMem.nlSpaces; j++)
        {
            LocalMemSpace *space = gMem.lSpaces[j];
            if (space->isMutable == isMutableSpace)
            {
                if (space->allocatedSpace() == 0) nSpaces++;
                else allocSpace += space->allocatedSpace();
                freeSpace += space->freeSpace();
            }
        }
        // Delete spaces provided we have at least one for each thread.  A space can be deleted if
        // it is small or if the available free space is at least a fifth of the allocated space.
        // Don't delete allocation spaces.  We need at least one of these.
        for (unsigned k = gMem.nlSpaces; k > 0 && nSpaces > userOptions.gcthreads; k--)
        {
            LocalMemSpace *space = gMem.lSpaces[k-1];
            if (space->isMutable == isMutableSpace && space->allocatedSpace() == 0 /* It's completely empty */
                && (space->spaceSize() < segSize || freeSpace > allocSpace/5) && ! space->allocationSpace)
            {
                freeSpace -= space->freeSpace();
                gMem.DeleteLocalSpace(space);
                nSpaces--;
            }
        }
    }
}

static bool doGC(const POLYUNSIGNED wordsRequiredToAllocate)
{
    /* Invariant: the bitmaps are completely clean. */
    /* Note: this version of doGC does NOT clean the store
    - that's now the user's resposibility SPF 22/10/96
    */
    unsigned j;

    record_gc_time(GCTimeStart);
    globalStats.incCount(PSC_GC_FULLGC);

    if (debugOptions & DEBUG_GC)
        Log("GC: Full GC, %lu words required %u spaces\n", wordsRequiredToAllocate, gMem.nlSpaces);
    /* Invariant: the bitmaps are completely clean. */

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
        // Reset the allocation pointers.  They will be set to the
        // limits of the retained data.
        lSpace->lowerAllocPtr = lSpace->bottom;
        lSpace->upperAllocPtr = lSpace->top;
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

    if (debugOptions & DEBUG_GC) Log("GC: Check weak refs\n");
    /* Detect unreferenced streams, windows etc. */
    GCheckWeakRefs();

    record_gc_time(GCTimeIntermediate, "Mark");

    /* If we are doing a full GC we expand the immutable area now, so that there's
       enough room to copy the immutables that are currently in the mutable buffer.
       There's no point expanding the mutable buffer now - we'll do that later
       when we know *exactly* how large we want it to be. */
    {
        POLYUNSIGNED immutableData = 0;
        for(j = 0; j < gMem.nlSpaces; j++)
            immutableData += gMem.lSpaces[j]->i_marked;
        PossiblyExpandImmutableArea(immutableData);
    }

    /* Compact phase */
    POLYUNSIGNED immutable_overflow = 0; // The immutable space we couldn't copy out.
    GCCopyPhase(immutable_overflow);

    record_gc_time(GCTimeIntermediate, "Copy");

    // Update Phase.
    if (debugOptions & DEBUG_GC) Log("GC: Update\n");
    GCUpdatePhase();

    record_gc_time(GCTimeIntermediate, "Update");

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
        ASSERT(iUpdated == iMarked - immutable_overflow);
        ASSERT(mUpdated == mMarked + immutable_overflow);
    }
    /* Invariant: the bitmaps are completely clean */

    {
        /* If we've had an immutable overflow, allow for this when we grow the heap */
        AdjustHeapSize(false /* immutable space*/, immutable_overflow);
        bool iFull = BufferIsReallyFull(false /* immutable area */, immutable_overflow, true) != 0;
        bool mFull = BufferIsReallyFull(true /* mutable area */, wordsRequiredToAllocate, true) != 0;

        if (iFull || ! mFull)
            AdjustHeapSize(true /* mutable space */, wordsRequiredToAllocate);
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
    globalStats.setSize(PSS_ALLOCATION_UNRESERVED, 0);
    for (j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[j];
        POLYUNSIGNED free = space->freeSpace();
        globalStats.incSize(PSS_AFTER_LAST_GC, free*sizeof(PolyWord));
        globalStats.incSize(PSS_AFTER_LAST_FULLGC, free*sizeof(PolyWord));
        if (space->allocationSpace)
        {
            globalStats.incSize(PSS_ALLOCATION, free*sizeof(PolyWord));
            globalStats.incSize(PSS_ALLOCATION_UNRESERVED, free*sizeof(PolyWord));
        }
    }

    /* Have we cleared enough space? */
    {
        bool iFull = BufferIsReallyFull(false /* immutable area */, immutable_overflow, true) != 0;
        bool mFull = BufferIsReallyFull(true /* mutable area */, wordsRequiredToAllocate, true) != 0;

        if (iFull || mFull)
        {
            iFull = BufferIsReallyFull(false /* immutable area */, 0, false) != 0;
            mFull = BufferIsReallyFull(true /* mutable area */, wordsRequiredToAllocate, false) != 0;
            if (iFull || mFull)
            {
                // No we don't even have that - interrupt console processes and end GC here.
                record_gc_time(GCTimeEnd);
                if (debugOptions & DEBUG_GC)
                    Log("GC: Completed - insufficient space in buffer(s): %s%s\n",
                        iFull ? "immutable " : "", mFull ? "mutable " : "");
                if (profileMode == kProfileLiveData || profileMode == kProfileLiveMutables)
                    printprofile();
                return false;
            }
        }
    }

    // Do we have enough space for the original allocation request?
    bool haveSpace = false;
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[j];
        if (space->allocationSpace)
        {
            if (space->freeSpace() >= wordsRequiredToAllocate)
            {
                haveSpace = true;
                break;
            }
        }
#ifdef FILL_UNUSED_MEMORY
        memset(space->bottom, 0xaa, (char*)space->upperAllocPtr - (char*)space->bottom);
#endif
    }

    /* End of garbage collection */
    record_gc_time(GCTimeEnd);

    /* Invariant: the bitmaps are completely clean */
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

    if (hsize < isize+msize+asize) hsize = isize+msize+asize;

    // The space is divided between the threads so that the total
    // is the value given in the option.
    hsize = hsize / userOptions.gcthreads;
    msize = msize / userOptions.gcthreads;
    isize = isize / userOptions.gcthreads;
    asize = asize / userOptions.gcthreads;
    // Defaults are half the heap for the immutable buffer, 5% for mutable and 45% for allocation
    if (msize == 0) msize = hsize / 20;  /* set default mutable buffer size */
    if (isize == 0) isize = hsize / 2;  /* set default immutable buffer size */
    if (asize == 0) asize = (hsize / 2 - hsize / 20) * userOptions.gcthreads;

    // Set the segment sizes.  We allocate in units of this size,
    immutableSegSize   = K_to_words(isize);
    mutableSegSize     = K_to_words(msize);
    POLYUNSIGNED allocSegSize = K_to_words(asize);
    gMem.SetReservation(K_to_words(rsize));

    // Try allocating the space.  If it fails try something smaller.
    for(;;) {
        bool allocationFailed = false;
        // Delete any partially allocated spaces.
        while (gMem.nlSpaces > 0) gMem.DeleteLocalSpace(gMem.lSpaces[gMem.nlSpaces-1]);
        // Allocate the reservation space.

        // Allocate as many segments as we have threads running the GC.
        ASSERT(userOptions.gcthreads >= 1);
        // Immutable space
        POLYUNSIGNED immutSize = ROUNDDOWN(immutableSegSize, BITSPERWORD);
        POLYUNSIGNED mutSize = ROUNDDOWN(mutableSegSize, BITSPERWORD);

        // Allocate one allocation space plus one immutable and one mutable space per thread
        globalStats.setSize(PSS_ALLOCATION, 0);
        globalStats.setSize(PSS_ALLOCATION_UNRESERVED, 0);
        if (! allocationFailed)
            allocationFailed = gMem.CreateAllocationSpace(allocSegSize) == 0;
        for(unsigned j = 0; j < userOptions.gcthreads; j++)
        {
            if (gMem.NewLocalSpace(immutSize, false) == 0 ||
                gMem.NewLocalSpace(mutSize, true) == 0)
            {
                allocationFailed = true;
                break;
            }
        }

        if (allocationFailed)
        {
            if (immutableSegSize < 1024 || mutableSegSize < 512 || allocSegSize < 1024) {
                // Too small to be able to run.
                Exit("Insufficient memory to allocate the heap");
            }
            // Make spaces smaller.  It may be that there's space for some but not all.
            immutableSegSize = immutableSegSize/2;
            mutableSegSize = mutableSegSize/2;
            allocSegSize = allocSegSize/2;
        }
        else // Succeeded.
        {
            // Record the space as though we'd had a GC
            globalStats.setSize(PSS_AFTER_LAST_GC, globalStats.getSize(PSS_TOTAL_HEAP));
            globalStats.setSize(PSS_AFTER_LAST_FULLGC, globalStats.getSize(PSS_TOTAL_HEAP));
            break;
        }
    }

    // Heap allocation has succeeded.
    // The space we need to have free at the end of a partial collection.  If we have less
    // than this we do a full GC.
    // For an immutable area this is zero.  For the mutable area, though, this is 80% of the
    // mutable segment size since we allocate new objects in the mutable area and this
    // determines how soon we will need to do another GC.
    immutableMinFree = mutableSegSize = 0;

    // This is the space we try to have free at the end of a major collection.  If
    // we have less than this we allocate another segment.
    immutableFreeSpace = immutableSegSize/2; // 50% full
    if (immutableFreeSpace < immutableMinFree)
        immutableFreeSpace = immutableMinFree;
    mutableFreeSpace   = mutableSegSize/2;
    if (mutableFreeSpace < mutableMinFree)
        mutableFreeSpace = mutableMinFree;

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
        result = RunQuickGC() || doGC (wordsRequired);
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

