/*
    Title:      Multi-Threaded Garbage Collector - Mark phase

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
/*
This is the first, mark, phase of the garbage collector.  It detects all
reachable cells in the area being collected.  At the end of the phase the
bit-maps associated with the areas will have ones for words belonging to cells
that must be retained and zeros for words that can be reused.

This is now multi-threaded.  The mark phase involves setting a bit in the header
of each live cell and then a pass over the memory building the bitmaps and clearing
this bit.  It is unfortunate that we cannot use the GC-bit that is used in
forwarding pointers but we may well have forwarded pointers left over from a
partially completed minor GC.  Using a bit in the header avoids the need for
locking since at worst it may involve two threads duplicating some marking.

Marking can potentially recurse as deeply as the data structure.  Scanaddrs
uses tail recursion on the last pointer in a cell which works well for lists
but does not deal with trees.  This code keeps a recursion count and triggers
a rescan on the range of addresses that could not be fully marked. 
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
#include "gctaskfarm.h"
#include "profiling.h"
#include "timing.h"


// Recursion limit.  This needs to be chosen so that the stack will not
// overflow on any platform.
#define RECURSION_LIMIT 2500

class MTGCProcessMarkPointers: public ScanAddress
{
public:
    MTGCProcessMarkPointers(): recursion(0) {}
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt) { return DoScanAddressAt(pt, false); }
    virtual void ScanRuntimeAddress(PolyObject **pt, RtsStrength weak);
    virtual PolyObject *ScanObjectAddress(PolyObject *base);

    POLYUNSIGNED DoScanAddressAt(PolyWord *pt, bool isWeak);
    virtual void ScanAddressesInObject(PolyObject *base, POLYUNSIGNED lengthWord);
    // Have to redefine this for some reason.
    void ScanAddressesInObject(PolyObject *base) { ScanAddressesInObject(base, base->LengthWord()); }

public:
    static void MarkPermanentMutableAreaTask(GCTaskId *, void *arg1, void *);

    static void MarkPointersTask(GCTaskId *, void *arg1, void *arg2);

private:
    unsigned recursion;
};

// Task to mark pointers in a permanent mutable area.
void MTGCProcessMarkPointers::MarkPermanentMutableAreaTask(GCTaskId *, void *arg1, void *)
{
    MemSpace *space = (MemSpace *)arg1;
    MTGCProcessMarkPointers marker;
    marker.ScanAddressesInRegion(space->bottom, space->top);
}

// Task to mark pointers.
void MTGCProcessMarkPointers::MarkPointersTask(GCTaskId *, void *arg1, void *arg2)
{
    PolyObject *obj = (PolyObject *)arg1;
    MTGCProcessMarkPointers marker;
    marker.ScanAddressesInObject(obj);
}

// Mark all pointers in the heap.
POLYUNSIGNED MTGCProcessMarkPointers::DoScanAddressAt(PolyWord *pt, bool isWeak)
{
    if ((*pt).IsTagged())
        return 0;

    LocalMemSpace *space = gMem.LocalSpaceForAddress((*pt).AsAddress());
    if (space == 0)
        return 0; // Ignore it if it points to a permanent area

    // This could contain a forwarding pointer if it points into an
    // allocation area and has been moved by the minor GC.
    PolyObject *obj = (*pt).AsObjPtr();
    if (obj->ContainsForwardingPtr())
    {
        *pt = obj->GetForwardingPtr();
        space = gMem.LocalSpaceForAddress((*pt).AsAddress());
        obj = (*pt).AsObjPtr();
    }

    // We shouldn't get code addresses since we handle stacks and code
    // segments separately so if this isn't an integer it must be an object address.
    POLYUNSIGNED L = obj->LengthWord();
    if (L & _OBJ_GC_MARK)
        return 0; // Already marked
    obj->SetLengthWord(L | _OBJ_GC_MARK); // Mark it

    POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);
    ASSERT(obj+n <= (PolyObject*)space->top); // Check the length is sensible

    if (debugOptions & DEBUG_GC_DETAIL)
        Log("GC: Mark: %p %" POLYUFMT " %u\n", obj, n, GetTypeBits(L));

    if (isWeak) // This is a SOME within a weak reference.
        return 0;

    if (OBJ_IS_BYTE_OBJECT(L))
        return 0; // We've done as much as we need
    else if (OBJ_IS_CODE_OBJECT(L) || OBJ_IS_WEAKREF_OBJECT(L))
    {
        // Have to handle these specially.  If we can't add it to
        // the task queue process it recursively using the current
        // recursion count.
        if (! gpTaskFarm->AddWork(&MarkPointersTask, obj, 0))
            ScanAddressesInObject(obj);
        return 0; // Already done it.
    }
    else
        return L | _OBJ_GC_MARK;
}

// The initial entry to process the roots.  Also used when processing the addresses
// in objects that can't be handled by ScanAddressAt.
PolyObject *MTGCProcessMarkPointers::ScanObjectAddress(PolyObject *obj)
{
    PolyWord val = obj;
    LocalMemSpace *space = gMem.LocalSpaceForAddress(val.AsAddress());
    if (space == 0)
        return obj; // Ignore it if it points to a permanent area

    // We may have a forwarding pointer if this has been moved by the
    // minor GC.
    if (obj->ContainsForwardingPtr())
    {
        obj = obj->GetForwardingPtr();
        val = obj;
        space = gMem.LocalSpaceForAddress(val.AsAddress());
    }

    ASSERT(obj->ContainsNormalLengthWord());

    CheckObject (obj);

    POLYUNSIGNED L = obj->LengthWord();
    if (L & _TOP_BYTE(0x04))
        return obj; // Already marked
    obj->SetLengthWord(L | _TOP_BYTE(0x04)); // Mark it

    if (profileMode == kProfileLiveData || (profileMode == kProfileLiveMutables && obj->IsMutable()))
        AddObjectProfile(obj);

    if ((PolyWord*)obj <= space->fullGCLowerLimit)
        space->fullGCLowerLimit = (PolyWord*)obj-1;

    POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);
    if (debugOptions & DEBUG_GC_DETAIL)
        Log("GC: Mark: %p %" POLYUFMT " %u\n", obj, n, GetTypeBits(L));

    // Process the addresses in this object.  We could short-circuit things
    // for word objects by calling ScanAddressesAt directly.
    ScanAddressesInObject(obj);

    return obj;
}


// These functions are only called with pointers held by the runtime system.
// Weak references can occur in the runtime system, eg. streams and windows.
// Weak references are not marked and so unreferenced streams and windows
// can be detected and closed.
void MTGCProcessMarkPointers::ScanRuntimeAddress(PolyObject **pt, RtsStrength weak)
{
    PolyObject *val = *pt;
    CheckPointer (val);
    if (weak == STRENGTH_WEAK) return;
    *pt = ScanObjectAddress(*pt);
}

// This is called both for objects in the local heap and also for mutables
// in the permanent area and, for partial GCs, for mutables in other areas.
void MTGCProcessMarkPointers::ScanAddressesInObject(PolyObject *base, POLYUNSIGNED L)
{
    if (recursion >= RECURSION_LIMIT)
    {
        LocalMemSpace *space = gMem.LocalSpaceForAddress(base);
        ASSERT(space != 0);
        PLocker lock(&space->spaceLock);
        // Have to include this in the range to rescan.
        if (space->fullGCRescanStart > ((PolyWord*)base) - 1)
            space->fullGCRescanStart = ((PolyWord*)base) - 1;
        POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);
        if (space->fullGCRescanEnd < ((PolyWord*)base) + n)
            space->fullGCRescanEnd = ((PolyWord*)base) + n;
        ASSERT(base->LengthWord() & _OBJ_GC_MARK); // Should have been marked.
        if (debugOptions & DEBUG_GC)
            Log("GC: Mark: Recursion limit reached.  Rescan for %p\n", base);
        return;
    }
    recursion ++;
    if (OBJ_IS_WEAKREF_OBJECT(L))
    {
        ASSERT(OBJ_IS_MUTABLE_OBJECT(L)); // Should be a mutable.
        ASSERT(OBJ_IS_WORD_OBJECT(L)); // Should be a plain object.
        // We need to mark the "SOME" values in this object but we don't mark
        // the references contained within the "SOME".
        POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);
        PolyWord *baseAddr = (PolyWord*)base;
        for (POLYUNSIGNED i = 0; i < n; i++)
            DoScanAddressAt(baseAddr+i, true);
        // Add this to the limits for the containing area.
        MemSpace *space = gMem.SpaceForAddress(baseAddr);
        PolyWord *startAddr = baseAddr-1; // Must point AT length word.
        PolyWord *endObject = baseAddr + n;
        if (startAddr < space->lowestWeak) space->lowestWeak = startAddr;
        if (endObject > space->highestWeak) space->highestWeak = endObject;
    }
    else ScanAddress::ScanAddressesInObject(base, L);
    recursion--;
}

static void SetBitmaps(LocalMemSpace *space, PolyWord *pt, PolyWord *top)
{
    while (pt < top)
    {
        PolyObject *obj = (PolyObject*)++pt;
        // If it has been copied by a minor collection skip it
        if (obj->ContainsForwardingPtr())
            pt += obj->GetForwardingPtr()->Length();
        else
        {
            POLYUNSIGNED L = obj->LengthWord();
            POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);
            if (L & _OBJ_GC_MARK)
            {
                obj->SetLengthWord(L & ~(_OBJ_GC_MARK));
                POLYUNSIGNED bitno = space->wordNo(pt);
                space->bitmap.SetBits(bitno - 1, n + 1);

                if (OBJ_IS_MUTABLE_OBJECT(L))
                    space->m_marked += n + 1;
                else
                    space->i_marked += n + 1;

                if ((PolyWord*)obj <= space->fullGCLowerLimit)
                    space->fullGCLowerLimit = (PolyWord*)obj-1;
            }
            pt += n;
        }
    }
}

static void CreateBitmapsTask(GCTaskId *, void *arg1, void *arg2)
{
    LocalMemSpace *lSpace = (LocalMemSpace *)arg1;
    lSpace->bitmap.ClearBits(0, lSpace->spaceSize());
    SetBitmaps(lSpace, lSpace->bottom, lSpace->top);
}

class RescanMarked: public MTGCProcessMarkPointers
{
private:
    virtual void ScanAddressesInObject(PolyObject *obj, POLYUNSIGNED lengthWord);
};

// Called to rescan objects that have already been marked
// before but are within the range of those that were
// skipped because of the recursion limit.  We process them
// again in case there are unmarked addresses in them.
void RescanMarked::ScanAddressesInObject(PolyObject *obj, POLYUNSIGNED lengthWord)
{
    if (lengthWord &_OBJ_GC_MARK)
        MTGCProcessMarkPointers::ScanAddressesInObject(obj, lengthWord);
}

void GCMarkPhase(void)
{
    mainThreadPhase = MTP_GCPHASEMARK;

    // Clear the mark counters and set the rescan limits.
    for(unsigned k = 0; k < gMem.nlSpaces; k++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[k];
        lSpace->i_marked = lSpace->m_marked = 0;
        lSpace->fullGCRescanStart = lSpace->top;
        lSpace->fullGCRescanEnd = lSpace->bottom;
    }
    
    // Do the actual marking

    // Scan the permanent mutable areas.
    for (unsigned j = 0; j < gMem.npSpaces; j++)
    {
        PermanentMemSpace *space = gMem.pSpaces[j];
        if (space->isMutable && ! space->byteOnly)
            gpTaskFarm->AddWorkOrRunNow(&MTGCProcessMarkPointers::MarkPermanentMutableAreaTask, space, 0);
    }

    MTGCProcessMarkPointers marker;
    GCModules(&marker);

    gpTaskFarm->WaitForCompletion();

    // Do we have to recan?
    while (true)
    {
        bool rescan = false;
        RescanMarked rescanner;
        for (unsigned m = 0; m < gMem.nlSpaces; m++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[m];
            if (lSpace->fullGCRescanStart < lSpace->fullGCRescanEnd)
            {
                PolyWord *start = lSpace->fullGCRescanStart;
                PolyWord *end = lSpace->fullGCRescanEnd;
                lSpace->fullGCRescanStart = lSpace->top;
                lSpace->fullGCRescanEnd = lSpace->bottom;
                rescan = true;
                if (debugOptions & DEBUG_GC)
                    Log("GC: Mark: Rescanning from %p to %p\n", start, end);
                rescanner.ScanAddressesInRegion(start, end);
            }
        }
        if (! rescan)
            break;
    }

    gcTimeData.RecordGCTime(GcTimeData::GCTimeIntermediate, "Mark");

    // Turn the marks into bitmap entries.
    for (unsigned i = 0; i < gMem.nlSpaces; i++)
        gpTaskFarm->AddWorkOrRunNow(&CreateBitmapsTask, gMem.lSpaces[i], 0);

    gpTaskFarm->WaitForCompletion();

    gcTimeData.RecordGCTime(GcTimeData::GCTimeIntermediate, "Bitmap");

    POLYUNSIGNED totalLive = 0;
    for(unsigned l = 0; l < gMem.nlSpaces; l++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[l];
        if (! lSpace->isMutable) ASSERT(lSpace->m_marked == 0);
        totalLive += lSpace->m_marked + lSpace->i_marked;
        if (debugOptions & DEBUG_GC)
            Log("GC: Mark: %s space %p: %" POLYUFMT " immutable words marked, %" POLYUFMT " mutable words marked\n",
                                lSpace->spaceTypeString(), lSpace,
                                lSpace->i_marked, lSpace->m_marked);
    }
    if (debugOptions & DEBUG_GC)
        Log("GC: Mark: Total live data %" POLYUFMT " words\n", totalLive);
}
