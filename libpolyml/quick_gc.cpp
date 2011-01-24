/*
    Title:      Quick copying garbage collector

    Copyright (c) 2011 David C. J. Matthews

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

/* start <= val < end */
#define INRANGE(val,start,end) ((start) <= (val) && (val) < (end))
inline POLYUNSIGNED BITNO(LocalMemSpace *area, PolyWord *pt) { return pt - area->bottom; }

class QuickGCScanner: public ScanAddress
{
public:
    QuickGCScanner(): succeeded(true) {}

    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);
    virtual PolyObject *ScanObjectAddress(PolyObject *base);

    bool succeeded;
private:
    PolyObject *CopyToGCArea(PolyObject *obj);
};

PolyObject *QuickGCScanner::CopyToGCArea(PolyObject *obj)
{
    bool isMutable = obj->IsMutable();
    POLYUNSIGNED length = obj->Length();

    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[i];
        if (lSpace->isMutable == isMutable && ! lSpace->allocationSpace &&
            lSpace->freeSpace() > length /* At least length+1*/)
        {
            // Can use this space.
            PolyObject *destAddress = (PolyObject*)(lSpace->lowerAllocPtr+1);
            lSpace->lowerAllocPtr += length+1;
            CopyObjectToNewAddress(obj, destAddress);
            return destAddress;
        }
    }
    // Not enough space.
    succeeded = false;
    return obj;
}

// Copy all the objects.
POLYUNSIGNED QuickGCScanner::ScanAddressAt(PolyWord *pt)
{
    POLYUNSIGNED n = 1; // Set up the loop to process one word at *pt
    pt++;
    
    while (n-- != 0)
    {
        PolyWord val = *(--pt);
        if (val.IsTagged())
            continue;

        LocalMemSpace *space = gMem.LocalSpaceForAddress(val.AsAddress());

        // We only copy it if it is in a local allocation space and not in the
        // "overflow" area of data that could not copied by the last full GC.
        if (space == 0 || ! space->allocationSpace || val.AsAddress() >= space->upperAllocPtr)
            continue;

        // We shouldn't get code addresses since we handle code
        // segments separately so if this isn't an integer it must be an object address.
        ASSERT(OBJ_IS_DATAPTR(val));

        PolyObject *obj = val.AsObjPtr();

        // Has it been moved already?
        if (obj->ContainsForwardingPtr())
        {
            PolyObject *newAddress = obj->GetForwardingPtr();
            ASSERT(newAddress->ContainsNormalLengthWord());
            *pt = newAddress;
            continue;
        }

        CheckPointer (val);

        POLYUNSIGNED L = obj->LengthWord();
        n = OBJ_OBJECT_LENGTH(L);

        // Copy the object.  Returns the new address unless there wasn't enough space
        // in which case it returns the original address.
        // Copy the object.
        PolyObject *newObject = CopyToGCArea(obj);

        CheckObject(newObject);

        *pt = newObject;

        if (debugOptions & DEBUG_GC_DETAIL)
        {
            if (*pt == obj)
                Log("GC: Quick: Insufficient space to move %p %lu %u\n", obj, n, GetTypeBits(L));
            else Log("GC: Quick: %p %lu %u moved to %p\n", obj, n, GetTypeBits(L), newObject);
        }

        // Stop now unless this is a simple word object we have been able to move.
        if (newObject == obj || OBJ_IS_MUTABLE_OBJECT(L) || GetTypeBits(L) != 0)
            return 0;

        // We can simply return zero in which case this performs a breadth-first scan.
        // A breadth-first scan distributes the objects through the memory so
        // to retain some degree of locality we try to copy some object pointed at
        // by this one.  We work from the end back so that we follow the tail pointers
        // for lists.
        pt = (PolyWord*)newObject + n;
    }

    return 0;
}

// The initial entry to process the roots.  Also used when processing the addresses
// in objects that can't be handled by ScanAddressAt.
PolyObject *QuickGCScanner::ScanObjectAddress(PolyObject *base)
{
    PolyWord val = base;
    // Scan this as an address.
    POLYUNSIGNED lengthWord = QuickGCScanner::ScanAddressAt(&val);
    if (lengthWord)
        ScanAddressesInObject(val.AsObjPtr(), lengthWord);
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

bool RunQuickGC(void)
{
    record_gc_time(GCTimeStart);
    mainThreadPhase = MTP_GCQUICK;

    if (debugOptions & DEBUG_GC)
        Log("GC: Beginning quick GC\n");

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
        lSpace->partialGCTop = lSpace->upperAllocPtr;
        // If we're scanning a space this is where we start.
        lSpace->partialGCScan = lSpace->lowerAllocPtr;
    }

    // First scan the roots, copying the data into the mutable and immutable areas.
    QuickGCScanner marker;

    // Scan the local mutable including any overflow area in the allocation areas.
    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        LocalMemSpace *space = gMem.lSpaces[i];
        if (space->isMutable)
        {
            // The upper area contains data copied by the last major GC.
            marker.ScanAddressesInRegion(space->partialGCTop, space->top);
            // The lower area contains data copied by previous minor GCs.
            if (! space->allocationSpace)
                marker.ScanAddressesInRegion(space->bottom, space->lowerAllocPtr);
        }
    }
    // Scan the permanent mutable areas.
    for (unsigned j = 0; j < gMem.npSpaces; j++)
    {
        MemSpace *space = gMem.pSpaces[j];
        if (space->isMutable)
            marker.ScanAddressesInRegion(space->bottom, space->top);
    }

    // Scan RTS addresses.  This will include the thread stacks.
    GCModules(&marker);

    // Now iterate over the areas scanning and copying until everything is copied.
    // We have to continue this even if we've run out of space to ensure that all
    // the addresses of objects that have been moved have been updated.
    while (true)
    {
        bool allDone = true;
        // We're finished when there is no unscanned data
        for (unsigned k = 0; k < gMem.nlSpaces && allDone; k++)
        {
            LocalMemSpace *space = gMem.lSpaces[k];
            allDone = space->partialGCScan == space->lowerAllocPtr;
        }
        if (allDone) break;

        // Scan each area that has had data added to it.
        for (unsigned l = 0; l < gMem.nlSpaces; l++)
        {
            LocalMemSpace *space = gMem.lSpaces[l];

            while (space->partialGCScan != space->lowerAllocPtr)
            {
                PolyWord *lastAllocPtr = space->lowerAllocPtr;
                // Scan the area.  This may well result in more data being added
                marker.ScanAddressesInRegion(space->partialGCScan, lastAllocPtr);
                space->partialGCScan = lastAllocPtr;
            }
        }
    }

    record_gc_time(GCTimeEnd);

    if (marker.succeeded)
    {
        // If it succeeded the allocation areas are now empty.
        for(unsigned l = 0; l < gMem.nlSpaces; l++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[l];
            if (lSpace->allocationSpace)
            {
                lSpace->lowerAllocPtr = lSpace->bottom;
#ifdef FILL_UNUSED_MEMORY
                // This provides extra checking if we have dangling pointers
                memset(lSpace->bottom, 0xaa, (char*)lSpace->upperAllocPtr - (char*)lSpace->bottom);
#endif
            }
            if (debugOptions & DEBUG_GC)
                Log("GC: %s space %p %d free in %d words %2.1f%% full\n", lSpace->isMutable ? "Mutable" : "Immutable",
                    lSpace, lSpace->freeSpace(), lSpace->spaceSize(),
                    ((float)lSpace->allocatedSpace()) * 100 / (float)lSpace->spaceSize());
        }
    }
    else
    {
        // We are going to have to do a full GC because there wasn't space.  The objects
        // we haven't been able to move won't have been scanned so we have to make sure
        // we've updated addresses within them before we return.
        QuickGCRecovery recovery;
        for (unsigned l = 0; l < gMem.nlSpaces; l++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[l];
            if (lSpace->allocationSpace)
                recovery.ScanAddressesInRegion(lSpace->bottom, lSpace->lowerAllocPtr);
        }
    }

    if (debugOptions & DEBUG_GC)
    {
        if (marker.succeeded) Log("GC: Completed successfully\n");
        else Log("GC: Quick GC failed\n");
    }

    return marker.succeeded;
}
