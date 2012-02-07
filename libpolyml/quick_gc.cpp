/*
    Title:      Quick copying garbage collector

    Copyright (c) 2011-12 David C. J. Matthews

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
This is a quick copying garbage collector that moves all the data out of
the allocation areas and into the mutable and immutable areas.  If either of
these has filled up it fails and a full garbage collection must be done.
*/
#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include "globals.h"
#include "processes.h"
#include "gc.h"
#include "scanaddrs.h"
#include "check_objects.h"
#include "bitmap.h"
#include "memmgr.h"
#include "diagnostics.h"
#include "timing.h"
#include "gctaskfarm.h"
#include "statistics.h"

// This protects access to the gMem.lSpace table.
static PLock localTableLock;

class QuickGCScanner: public ScanAddress
{
public:
    QuickGCScanner(bool r): rootScan(r), succeeded(true) {}
    virtual ~QuickGCScanner() {}

    // Overrides for ScanAddress class
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);
    virtual PolyObject *ScanObjectAddress(PolyObject *base);
private:
    PolyObject *FindNewAddress(PolyObject *obj, POLYUNSIGNED L, LocalMemSpace *srcSpace);
    virtual LocalMemSpace *FindSpace(POLYUNSIGNED length, bool isMutable) = 0;
protected:
    bool objectCopied;
    bool rootScan;
public:
    bool succeeded;
};

class RootScanner: public QuickGCScanner
{
public:
    RootScanner(): QuickGCScanner(true), startPosn(0) {}
private:
    virtual LocalMemSpace *FindSpace(POLYUNSIGNED length, bool isMutable);
    unsigned startPosn; // Used to distribute data between spaces.
};

class ThreadScanner: public QuickGCScanner
{
public:
    ThreadScanner(GCTaskId* id): QuickGCScanner(false), taskID(id), mutableSpace(0), immutableSpace(0),
        spaceTable(0), nOwnedSpaces(0) {}
    virtual ~ThreadScanner() { free(spaceTable); }

    void ScanCopiedArea(LocalMemSpace *initialSpace);
private:
    virtual LocalMemSpace *FindSpace(POLYUNSIGNED length, bool isMutable);
    bool TakeOwnership(LocalMemSpace *space);

    GCTaskId *taskID;
    LocalMemSpace *mutableSpace, *immutableSpace;
    LocalMemSpace **spaceTable;
    unsigned nOwnedSpaces;
};

PolyObject *QuickGCScanner::FindNewAddress(PolyObject *obj, POLYUNSIGNED L, LocalMemSpace *srcSpace)
{
    bool isMutable = OBJ_IS_MUTABLE_OBJECT(L);
    POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);
    LocalMemSpace *lSpace = FindSpace(n, isMutable);
    if (lSpace == 0)
        return obj; // Unable to move it.
    PolyObject *newObject = (PolyObject*)(lSpace->lowerAllocPtr+1);

    // It's possible that another thread may have actually copied the 
    // object since we loaded the length word so we check it again.
    // If this is a mutable we must ensure that checking the forwarding
    // pointer here and updating it if necessary is atomic.  We don't need
    // to do that for immutable data so there is a small chance that an
    // object may be copied twice.  That's not a problem for immutable data.
    // Also lock this if it's code.  This may not be necessary but code objects
    // are rare. Updating the addresses in code objects is complicated and
    // it's possible that there are assumptions somewhere that there's only one
    // copy.
    bool requireLock = isMutable || OBJ_IS_CODE_OBJECT(L);

    if (requireLock) srcSpace->spaceLock.Lock();
    if (obj->ContainsForwardingPtr())
    {
        newObject = obj->GetForwardingPtr();
        if (requireLock) srcSpace->spaceLock.Unlock();
        if (debugOptions & DEBUG_GC_DETAIL)
            Log("GC: Quick: %p %lu %u has already moved to %p\n", obj, n, GetTypeBits(L), newObject);
        objectCopied = false;
        return obj->GetForwardingPtr();
    }
    obj->SetForwardingPtr(newObject);
    if (requireLock) srcSpace->spaceLock.Unlock();

    lSpace->lowerAllocPtr += n+1;
    CopyObjectToNewAddress(obj, newObject, L);
    objectCopied = true;
    return newObject;
}

// When scanning the roots we want to distribute the data among the immutable and mutable areas
// so that the work is distributed for the scanning threads.
LocalMemSpace *RootScanner::FindSpace(POLYUNSIGNED n, bool isMutable)
{
    startPosn++;
    if (startPosn >= gMem.nlSpaces) startPosn = 0;

    POLYUNSIGNED spaceAllocated = 0;

    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[(i + startPosn) % gMem.nlSpaces];
        if (lSpace->isMutable == isMutable &&
            ! lSpace->allocationSpace && lSpace->freeSpace() > n /* At least n+1*/)
            return lSpace; // Can use this

        if (! lSpace->allocationSpace)
            spaceAllocated += lSpace->spaceSize();
    }

    if (spaceAllocated < gMem.SpaceBeforeMajorGC())
    {
        // Allocate a new space for this.
        POLYUNSIGNED spaceSize = gMem.DefaultSpaceSize();
        if (n+1 > spaceSize) spaceSize = n+1;
        LocalMemSpace *lSpace = gMem.NewLocalSpace(spaceSize, isMutable);
        if (lSpace == 0)
            return 0; // Couldn't allocate it.
        lSpace->partialGCTop = lSpace->upperAllocPtr;
        lSpace->partialGCScan = lSpace->lowerAllocPtr;
        lSpace->spaceOwner = 0;
        return lSpace;
    }
    return 0; // Insufficient space
}

// When scanning within a thread we don't want to be searching the space table.
LocalMemSpace *ThreadScanner::FindSpace(POLYUNSIGNED n, bool isMutable)
{
    LocalMemSpace *lSpace = isMutable ? mutableSpace : immutableSpace;

    if (lSpace != 0)
    {
        // See if there's space in the existing area.
        if (lSpace->freeSpace() > n /* At least n+1*/)
            return lSpace;
    }

    for (unsigned i = 0; i < nOwnedSpaces; i++)
    {
        lSpace = spaceTable[i];
        if (lSpace->isMutable == isMutable &&
            ! lSpace->allocationSpace && lSpace->freeSpace() > n /* At least n+1*/)
        {
            if (n < 10)
            {
                // We use this space for further allocations unless we are trying to
                // allocate a "large" object.
                if (isMutable) mutableSpace = lSpace; else immutableSpace = lSpace;
            }
            return lSpace;
        }
    }

    PLocker l(&localTableLock);
    // Another thread may allocate a new area, reallocating gMem.lSpaces so we
    // we need a lock here.
    if (taskID != 0)
    {
        // See if we can take a space that is currently unused.
        for (unsigned i = 0; i < gMem.nlSpaces; i++)
        {
            lSpace = gMem.lSpaces[i];
            if (lSpace->spaceOwner == 0 && lSpace->isMutable == isMutable &&
                ! lSpace->allocationSpace && lSpace->freeSpace() > n /* At least n+1*/)
            {
                if (debugOptions & DEBUG_GC)
                    Log("GC: Quick: Thread %p is taking ownership of space %p\n", taskID, lSpace);
                if (! TakeOwnership(lSpace))
                    return 0;
                return lSpace;
            }
        }
    }

    POLYUNSIGNED spaceAllocated = 0;
    for (unsigned k = 0; k < gMem.nlSpaces; k++)
    {
        LocalMemSpace *space = gMem.lSpaces[k];
        if (! space->allocationSpace)
            spaceAllocated += space->spaceSize();
    }

    if (spaceAllocated < gMem.SpaceBeforeMajorGC())
    {
        // Allocate a new space for this.
        POLYUNSIGNED spaceSize = gMem.DefaultSpaceSize();
        if (n+1 > spaceSize) spaceSize = n+1;
        lSpace = gMem.NewLocalSpace(spaceSize, isMutable);
        if (lSpace == 0)
            return 0; // Couldn't allocate it.
        lSpace->partialGCTop = lSpace->upperAllocPtr;
        lSpace->partialGCScan = lSpace->lowerAllocPtr;
        lSpace->spaceOwner = 0;
        if (! TakeOwnership(lSpace))
            return 0;
        return lSpace;
    }

    return 0; // Insufficient space
}

// Copy all the objects.
POLYUNSIGNED QuickGCScanner::ScanAddressAt(PolyWord *pt)
{
    POLYUNSIGNED n = 1; // Set up the loop to process one word at *pt
    pt++;
    
    while (n-- != 0)
    {
        PolyWord val = *(--pt);
        if (! val.IsTagged())
        {
            LocalMemSpace *space = gMem.LocalSpaceForAddress(val.AsAddress());

            // We only copy it if it is in a local allocation space and not in the
            // "overflow" area of data that could not copied by the last full GC.
            if (space != 0 && space->allocationSpace && val.AsAddress() <= space->upperAllocPtr)
            {
                // We shouldn't get code addresses since we handle code
                // segments separately so if this isn't an integer it must be an object address.
                ASSERT(OBJ_IS_DATAPTR(val));

                PolyObject *obj = val.AsObjPtr();
                // Load the length word without any interlock.  We can't assume that
                // another thread won't also copy this at the same time.
                POLYUNSIGNED L = obj->LengthWord();

                // Has it been moved already? N.B.  Another thread may be in the process of
                // moving it so the new object may not be fully copied.
                if (OBJ_IS_POINTER(L))
                    *pt = OBJ_GET_POINTER(L);
                else
                {
                    // We need to copy this object.
                    //CheckPointer (val); 

                    PolyObject *newObject = obj; // New address of object.

                    // See if we can find space.
                    newObject = FindNewAddress(obj, L, space);

                    if (newObject == obj) // Couldn't copy it - not enough space.
                        succeeded = false;

                    *pt = newObject; // Update the pointer to the object

                    if (debugOptions & DEBUG_GC_DETAIL)
                    {
                        if (*pt == obj)
                            Log("GC: Quick: Insufficient space to move %p %lu %u\n", obj, n, GetTypeBits(L));
                        else Log("GC: Quick: %p %lu %u moved to %p\n", obj, n, GetTypeBits(L), newObject);
                    }

                    // Stop now unless this is a simple word object we have been able to move.
                    // Also stop if we're just scanning the roots.
                    if (! rootScan && newObject != obj && ! OBJ_IS_MUTABLE_OBJECT(L) && 
                        GetTypeBits(L) == 0 && objectCopied)
                    {
                        // We can simply return zero in which case this performs a breadth-first scan.
                        // A breadth-first scan distributes the objects through the memory so
                        // to retain some degree of locality we try to copy some object pointed at
                        // by this one.  We work from the end back so that we follow the tail pointers
                        // for lists.
                        n = OBJ_OBJECT_LENGTH(L); // Object length
                        pt = (PolyWord*)newObject + n;
                    }
                }
            }
        }
    }
    // We've reached the end without finding a pointer to follow
    return 0;
}

// The initial entry to process the roots.  Also used when processing the addresses
// in objects that can't be handled by ScanAddressAt.
PolyObject *QuickGCScanner::ScanObjectAddress(PolyObject *base)
{
    PolyWord val = base;
    // Scan this as an address.
    (void)QuickGCScanner::ScanAddressAt(&val);
    // Ignore the result of ScanAddressAt which is always zero and
    // just return the updated address.
    return val.AsObjPtr();
}

class QuickGCRecovery: public ScanAddress
{
public:
    virtual PolyObject *ScanObjectAddress(PolyObject *base);
};

PolyObject *QuickGCRecovery::ScanObjectAddress(PolyObject *base)
{
    if (base->ContainsForwardingPtr())
        return base->GetForwardingPtr();
    else return base;
}

bool ThreadScanner::TakeOwnership(LocalMemSpace *space)
{
    ASSERT(space->spaceOwner == 0);
    LocalMemSpace **v = (LocalMemSpace**)realloc(spaceTable, (nOwnedSpaces+1)*sizeof(LocalMemSpace*));
    if (v == 0)
        return false;
    spaceTable = v;
    space->spaceOwner = taskID;
    spaceTable[nOwnedSpaces++] = space;
    return true;
}

void ThreadScanner::ScanCopiedArea(LocalMemSpace *initialSpace)
{
    {
        PLocker lock(&localTableLock);
        // Take ownership of this space unless another thread
        // is already copying into it.  In that case that thread
        // will be responsible for it.
        if (initialSpace->spaceOwner != 0)
            return;
        if (! TakeOwnership(initialSpace))
        {
            succeeded = false;
            return;
        }
    }

    if (debugOptions & DEBUG_GC)
        Log("GC: Quick: Thread %p is processing space %p\n", taskID, initialSpace);

    while (true)
    {
        bool allDone = true;
        // We're finished when there is no unscanned data in any space we own.
        for (unsigned k = 0; k < nOwnedSpaces && allDone; k++)
        {
            LocalMemSpace *space = spaceTable[k];
            allDone = space->partialGCScan == space->lowerAllocPtr &&
                      space->partialGCTop == space->top;
        }
        if (allDone) break;

        // Scan each area that has had data added to it.
        for (unsigned l = 0; l < nOwnedSpaces; l++)
        {
            LocalMemSpace *space = spaceTable[l];
            // Scan the old mutable area if it's a mutable space
            if (space->partialGCTop != space->top)
            {
                ScanAddressesInRegion(space->partialGCTop, space->top);
                space->partialGCTop = space->top;
            }
            // Scan the area.  This may well result in more data being added
            while (space->partialGCScan != space->lowerAllocPtr)
            {
                PolyWord *lastAllocPtr = space->lowerAllocPtr;
                // Scan the area.  This may well result in more data being added
                ScanAddressesInRegion(space->partialGCScan, lastAllocPtr);
                // If we ran out of space stop here.  We need to rescan any objects
                // that contain addresses we couldn't move because another thread
                // may have done.
                if (! succeeded)
                {
                    if (debugOptions & DEBUG_GC)
                        Log("GC: Quick: Thread %p has insufficient space\n", taskID);
                    return;
                }
                space->partialGCScan = lastAllocPtr;
            }
        }
    }
    // Release the spaces we're holding in case another thread wants to use them.
    for (unsigned m = 0; m < nOwnedSpaces; m++)
    {
        LocalMemSpace *space = spaceTable[m];
        space->spaceOwner = 0;
    }
    nOwnedSpaces = 0;
}

static void scanCopiedArea(GCTaskId *id, void *arg1, void *arg2)
{
    ThreadScanner marker(id);
    marker.ScanCopiedArea((LocalMemSpace *)arg1);
}

// Called after a minor GC.  Currently does nothing.
// See also adjustHeapSize for adjustments after a major GC.
static void adjustHeapSizeAfterMinorGC(POLYUNSIGNED spaceCopiedOut)
{
    TIMEDATA gc, total;
    gc.add(gcTimeData.minorGCSystemCPU);
    gc.add(gcTimeData.minorGCUserCPU);
    total.add(gc);
    total.add(gcTimeData.minorNonGCSystemCPU);
    total.add(gcTimeData.minorNonGCUserCPU);
    float g = gc.toSeconds() / total.toSeconds();
    // In this case we compute l as the live memory in the allocation area
    // divided by the size of the allocation area.
    float l = (float)spaceCopiedOut / (float)gMem.SpaceBeforeMinorGC();
    if (debugOptions & DEBUG_HEAPSIZE)
        Log("Heap: Minor resizing factors g = %f, l = %f\n", g, l);

}


bool RunQuickGC(void)
{
    gcTimeData.RecordGCTime(GcTimeData::GCTimeStart);
    globalStats.incCount(PSC_GC_PARTIALGC);
    mainThreadPhase = MTP_GCQUICK;

    if (debugOptions & DEBUG_GC)
        Log("GC: Beginning quick GC\n");

    POLYUNSIGNED spaceBeforeGC = 0;

    for(unsigned k = 0; k < gMem.nlSpaces; k++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[k];
        ASSERT (lSpace->top >= lSpace->upperAllocPtr);
        ASSERT (lSpace->upperAllocPtr >= lSpace->lowerAllocPtr);
        ASSERT (lSpace->lowerAllocPtr >= lSpace->bottom);
        // Remember the top before we started this GC.  It's
        // only relevant for mutable areas.  It avoids us rescanning
        // objects that may have been added to the space as a result of
        // scanning another space.
        if (lSpace->isMutable)
            lSpace->partialGCTop = lSpace->upperAllocPtr;
        else lSpace->partialGCTop = lSpace->top;
        // If we're scanning a space this is where we start.
        // For immutable areas this only includes newly added
        // data but for mutable areas we have to scan data added
        // by previous GCs.
        if (lSpace->isMutable && ! lSpace->allocationSpace)
            lSpace->partialGCScan = lSpace->bottom;
        else lSpace->partialGCScan = lSpace->lowerAllocPtr;
        lSpace->spaceOwner = 0; // Not currently owned
        // Add up the space in the mutable and immutable areas
        if (! lSpace->allocationSpace)
            spaceBeforeGC += lSpace->allocatedSpace();
    }

    // First scan the roots, copying the data into the mutable and immutable areas.
    RootScanner rootScan;
    // Scan the permanent mutable areas.
    for (unsigned j = 0; j < gMem.npSpaces; j++)
    {
        PermanentMemSpace *space = gMem.pSpaces[j];
        if (space->isMutable && ! space->byteOnly)
            rootScan.ScanAddressesInRegion(space->bottom, space->top);
    }

    // Scan RTS addresses.  This will include the thread stacks.
    GCModules(&rootScan);

    // At this point the immutable and mutable areas will have some root objects
    // in the space between partialGCScan (the old value of lowerAllocPtr) and
    // lowerAllocPtr.  These will contain the addresses of objects in the allocation
    // areas.  We need to scan these root objects and then any new objects we copy
    // until there are no objects left to scan.
    for (unsigned l = 0; l < gMem.nlSpaces; l++)
    {
        LocalMemSpace *space = gMem.lSpaces[l];
        if (space->partialGCScan != space->lowerAllocPtr || space->partialGCTop != space->top)
            gpTaskFarm->AddWorkOrRunNow(&scanCopiedArea, space, 0);
    }

    gpTaskFarm->WaitForCompletion();

    // Did we manage to copy everything?
    for (unsigned m = 0; m < gMem.nlSpaces; m++)
    {
        LocalMemSpace *space = gMem.lSpaces[m];
        if (space->partialGCScan != space->lowerAllocPtr || space->partialGCTop != space->top)
            rootScan.succeeded = false;
    }

    gcTimeData.RecordGCTime(GcTimeData::GCTimeEnd);

    if (rootScan.succeeded)
    {

        globalStats.setSize(PSS_AFTER_LAST_GC, 0);
        globalStats.setSize(PSS_ALLOCATION, 0);
        globalStats.setSize(PSS_ALLOCATION_FREE, 0);
        // If it succeeded the allocation areas are now empty.
        POLYUNSIGNED spaceAfterGC = 0;
        for(unsigned l = 0; l < gMem.nlSpaces; l++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[l];
            POLYUNSIGNED free;
            if (lSpace->allocationSpace)
            {
                lSpace->lowerAllocPtr = lSpace->bottom;
                free = lSpace->freeSpace();
#ifdef FILL_UNUSED_MEMORY
                // This provides extra checking if we have dangling pointers
                memset(lSpace->bottom, 0xaa, (char*)lSpace->upperAllocPtr - (char*)lSpace->bottom);
#endif
                globalStats.incSize(PSS_ALLOCATION, free*sizeof(PolyWord));
                globalStats.incSize(PSS_ALLOCATION_FREE, free*sizeof(PolyWord));
            }
            else free = lSpace->freeSpace();
            if (debugOptions & DEBUG_GC)
                Log("GC: %s space %p %d free in %d words %2.1f%% full\n", lSpace->spaceTypeString(),
                    lSpace, lSpace->freeSpace(), lSpace->spaceSize(),
                    ((float)lSpace->allocatedSpace()) * 100 / (float)lSpace->spaceSize());
            globalStats.incSize(PSS_AFTER_LAST_GC, free*sizeof(PolyWord));
            spaceAfterGC += lSpace->allocatedSpace();
        }
        adjustHeapSizeAfterMinorGC(spaceAfterGC-spaceBeforeGC); // Adjust the allocation size.
        gcTimeData.resetMinorTimingData();
        // Remove allocation spaces that are larger than the default
        // and any excess over the current size of the allocation area.
        gMem.RemoveExcessAllocation();
    }
    else
    {
        // Else: if we failed to find space we have data in the allocation areas that have not been
        // moved.  Since they are not scanned they may contain the addresses of objects that have
        // been moved i.e. containing forwarding pointers.  There's also the case where one thread
        // has run out of space and so it leaves pointers into the allocation area in its objects
        // only for another thread to copy them later.
        // We need to scan all the mutable data because we may not have completed that.
        QuickGCRecovery recovery;
        for (unsigned l = 0; l < gMem.nlSpaces; l++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[l];
            if (lSpace->isMutable)
            {
                recovery.ScanAddressesInRegion(lSpace->upperAllocPtr, lSpace->top);
                recovery.ScanAddressesInRegion(lSpace->bottom, lSpace->lowerAllocPtr);
            }
            else recovery.ScanAddressesInRegion(lSpace->partialGCScan, lSpace->lowerAllocPtr);
        }
    }

    if (debugOptions & DEBUG_GC)
    {
        if (rootScan.succeeded) Log("GC: Completed successfully\n");
        else Log("GC: Quick GC failed\n");
    }

    return rootScan.succeeded;
}
