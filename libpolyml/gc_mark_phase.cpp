/*
    Title:      Multi-Threaded Garbage Collector - Mark phase

    Copyright (c) 2010 David C. J. Matthews

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
#include "gctaskfarm.h"

/* start <= val < end */
#define INRANGE(val,start,end) ((start) <= (val) && (val) < (end))
inline POLYUNSIGNED BITNO(LocalMemSpace *area, PolyWord *pt) { return pt - area->bottom; }

class MTGCProcessMarkPointers: public ScanAddress
{
public:
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt) { return DoScanAddressAt(pt, false); }
    virtual void ScanRuntimeAddress(PolyObject **pt, RtsStrength weak);
    virtual PolyObject *ScanObjectAddress(PolyObject *base);
private:
    POLYUNSIGNED DoScanAddressAt(PolyWord *pt, bool isWeak);
    virtual void ScanAddressesInObject(PolyObject *base, POLYUNSIGNED lengthWord);
    // Have to redefine this for some reason.
    void ScanAddressesInObject(PolyObject *base) { ScanAddressesInObject(base, base->LengthWord()); }

    static void MarkPointersTask(void *arg1, void *arg2);
};


// Task to mark pointers.
void MTGCProcessMarkPointers::MarkPointersTask(void *arg1, void *arg2)
{
    MTGCProcessMarkPointers *processMark = (MTGCProcessMarkPointers *)arg1;
    PolyObject *obj = (PolyObject *)arg2;
    processMark->ScanAddressesInObject(obj);
}


// Mark all pointers in the heap.
POLYUNSIGNED MTGCProcessMarkPointers::DoScanAddressAt(PolyWord *pt, bool isWeak)
{
    PolyWord val = *pt;
    CheckPointer (val);
    
    if (val.IsTagged())
        return 0;

    LocalMemSpace *space = gMem.LocalSpaceForAddress(val.AsAddress());
    if (space == 0)
        return 0; // Ignore it if it points to a permanent area

    // Ignore it if it's outside the range we're currently collecting.
    if (! INRANGE(val.AsStackAddr(), space->gen_bottom, space->gen_top))
        return 0;

    // We shouldn't get code addresses since we handle stacks and code
    // segments separately so if this isn't an integer it must be an object address.
    POLYUNSIGNED new_bitno = BITNO(space, val.AsStackAddr());

    space->bitmapLock.Lock();

    if (space->bitmap.TestBit(new_bitno))
    {
        space->bitmapLock.Unlock();
        return 0; // Already marked
    }

    PolyObject *obj = val.AsObjPtr();
    POLYUNSIGNED L = obj->LengthWord();
    POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);

    /* Add up the objects to be moved into the mutable area. */
    if (OBJ_IS_MUTABLE_OBJECT(L))
        space->m_marked += n + 1;
    else
        space->i_marked += n + 1;

    /* Mark the segment including the length word. */
    space->bitmap.SetBits(new_bitno - 1, n + 1);

    space->bitmapLock.Unlock();

    if (isWeak) // This is a SOME within a weak reference.
        return 0;

    if (OBJ_IS_BYTE_OBJECT(L))
        return 0; // We've done as much as we need
    else if (OBJ_IS_CODE_OBJECT(L) || OBJ_IS_STACK_OBJECT(L) || OBJ_IS_WEAKREF_OBJECT(L))
    {
        // Have to handle these specially.
        gpTaskFarm->AddWorkOrRunNow(&MarkPointersTask, this, obj);
        return 0; // Already done it.
    }
    // If we've space in the task queue defer it otherwise process it
    // immediately but avoiding recursion if we can.
    else if (gpTaskFarm->AddWork(&MarkPointersTask, this, obj))
        return 0;
    else
        return L;
}

// The initial entry to process the roots.  Also used when processing the addresses
// in objects that can't be handled by ScanAddressAt.
PolyObject *MTGCProcessMarkPointers::ScanObjectAddress(PolyObject *obj)
{
    PolyWord val = obj;
    LocalMemSpace *space = gMem.LocalSpaceForAddress(val.AsAddress());
    if (space == 0)
        return obj; // Ignore it if it points to a permanent area
    // Ignore it if it's outside the range we're currently collecting.
    if (! INRANGE(val.AsStackAddr(), space->gen_bottom, space->gen_top))
        return obj;

    ASSERT(obj->ContainsNormalLengthWord());

    CheckObject (obj);

    POLYUNSIGNED bitno = BITNO(space, val.AsStackAddr());

    {
        PLocker lock(&space->bitmapLock);

        if (space->bitmap.TestBit(bitno)) return obj; /* Already marked */

        POLYUNSIGNED L = obj->LengthWord();
        ASSERT (OBJ_IS_LENGTH(L));

        POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);
        ASSERT (n != 0);

        /* Mark the segment including the length word. */
        space->bitmap.SetBits (bitno - 1, n + 1);

        /* Add up the objects to be moved into the mutable area. */
        if (OBJ_IS_MUTABLE_OBJECT(L))
            space->m_marked += n + 1;
        else
            space->i_marked += n + 1;
    }

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
    LocalMemSpace *space = gMem.LocalSpaceForAddress(val);
    if (space != 0)
    {
        PolyWord w = val;
        if (INRANGE(w.AsStackAddr(), space->gen_bottom, space->gen_top))
        {
            POLYUNSIGNED lengthWord = ScanAddressAt(&w);
            if (lengthWord)
                ScanAddressesInObject(val, lengthWord);
            *pt = w.AsObjPtr();
        }
    }
}

// This is called both for objects in the local heap and also for mutables
// in the permanent area and, for partial GCs, for mutables in other areas.
void MTGCProcessMarkPointers::ScanAddressesInObject(PolyObject *base, POLYUNSIGNED L)
{
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
}

void GCMarkPhase(void)
{
    mainThreadPhase = MTP_GCPHASEMARK;
        
    /* Bitmaps are allocated in InitialiseGC and are zeroed
       at the END of each GC, because that way we know how much
       of each bitmap (not all!) we need to touch.
       SPF 3/10/96 */
    for(unsigned k = 0; k < gMem.nlSpaces; k++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[k];
        lSpace->i_marked = lSpace->m_marked = 0;
    }
    
    /* Do the actual marking */
    MTGCProcessMarkPointers marker;

    // Scan the local mutable areas.  It won't do anything if this is a full
    // GC since gen_top == top.
    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        LocalMemSpace *space = gMem.lSpaces[i];
        if (space->isMutable)
            marker.ScanAddressesInRegion(space->gen_top, space->top);
    }
    // Scan the permanent mutable areas.
    for (unsigned j = 0; j < gMem.npSpaces; j++)
    {
        MemSpace *space = gMem.pSpaces[j];
        if (space->isMutable)
            marker.ScanAddressesInRegion(space->bottom, space->top);
    }

    GCModules(&marker);
    gpTaskFarm->WaitForCompletion();

    /* Invariant: at most the first (gen_top - bottom) bits of the each bitmap can be dirty here. */
    
    // Mutable areas can contain mutable or immutable objects.  Immutable areas
    // should only contain immutable objects.  Verify this.
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        if (! lSpace->isMutable) ASSERT(lSpace->m_marked == 0);
    }
}
