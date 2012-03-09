/*
    Title:      Multi-Threaded Garbage Collector - Copy phase

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
This is the second, copy, phase of the garbage collector.  The previous,
mark, phase has identified all the live data and set the bits in the bit-maps.
This phase compacts the memory by copying cells from lower in the segment or
from other segments.  When a cell is copied the length-word is modified to be
a tomb-stone that gives the new location for the cell.  Cells can be copied to
areas of memory that are shown as free in the bit-maps and the destination area
is then marked as allocated.  Because there are tomb-stones left behind the original
location of a cell must remain as allocated and its space cannot be reused until the
GC is complete.

We copy cells in a defined order to avoid copying loops.
The ordering on the addresses is:
    Immutable areas (for immutable cells) (highest)
    Mutable areas
    Allocation areas (lowest)
Within each group a lower position in the gMem.lSpaces is higher
MemMgr::AddLocalSpace enters spaces gMem.lSpaces such that immutable
areas come before mutable areas which come before allocation areas
so this reduces to the order in that table.
Within a space higher addresses are higher.
So we try to copy data out of the allocation areas and to copy any
cells that are now immutable out of the mutable areas.  We try to copy
data out of higher numbered spaces in order to try to free them
completely and to compact data towards the top of a space if we
can't.

Once a thread has started copying into or out of an area it takes
ownership of the area and no other thread can use the area.  This
avoids 
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
#include "machine_dep.h"
#include "processes.h"
#include "gc.h"
#include "scanaddrs.h"
#include "bitmap.h"
#include "memmgr.h"
#include "gctaskfarm.h"
#include "locking.h"
#include "diagnostics.h"

static PLock copyLock;

/* start <= val <= end */
#define INSOFTRANGE(val,start,end) ((start) <= (val) && (val) <= (end))

/* Search the area downwards looking for n consecutive free words.          */
/* Return the bitmap index if successful or 0 (should we use -1?) on failure. */
static inline PolyWord *FindFreeAndAllocate(LocalMemSpace *dst, POLYUNSIGNED limit, POLYUNSIGNED n)
{
    if (dst == 0) return 0; // No current space

    /* SPF's version of the start caching code. SPF 2/10/96 */
    /* Invariant: dst->start[0] .. dst->start[dst->start_index] is a descending sequence. */
    POLYUNSIGNED truncated_n = (n < NSTARTS) ? n : NSTARTS - 1;
    
    ASSERT(0 <= limit);
    
    /* Invariant: dst->start[0] .. dst->start[dst->start_index] is a descending sequence. */ 
    
    /* 
    Update the starting array, so that the first few entries are valid.
    The starting point for a given size of hole must be at least as
    small (late) as the starting point for smaller holes.
    We remember the start_index of our previous allocation, so
    that if we have the same size object again, this loop becomes
    trivial. SPF 2/10/96
    */ 
    for (POLYUNSIGNED i = dst->start_index; i < truncated_n; i ++)
    {
        if (dst->start[i] < dst->start[i+1])
        {
            dst->start[i+1] = dst->start[i];
        }
    }
    
    /* Invariant: dst->start[0] .. dst->start[truncated_n] is a descending sequence. */
    dst->start_index = truncated_n;
    /* Invariant: dst->start[0] .. dst->start[dst->start_index] is a descending sequence. */ 
    
    /* Start our search at the appropriate point. */
    POLYUNSIGNED start = dst->start[truncated_n];
    
    /* If we can't copy UP, give up immediately. It's important that we DON'T
    update dst->start[n], because that might INCREASE it, which isn't
    allowed. SPF 19/11/1997
    */
    if (start <= limit)
    {
        return 0;
    }
    
    POLYUNSIGNED free = dst->bitmap.FindFree(limit, start, n);
    /* free == 0 || limit <= free && free < start */
    
    /* 
    We DON'T update the array for big allocations, because this would cause
    us to skip holes that are actually large enough for slightly smaller
    (but still big) allocations. An allocation is "big" if it doesn't
    have its own dedicated slot in the start array. This won't actually
    cost us much, provided there's enough small allocations between
    the big ones, as these will cause the pointer to be advanced.
    SPF 2/10/96
    */
    /* dst->start[0] .. dst->start[dst->start_index] is a descending sequence */
    if (n < NSTARTS)
    {
        /* free == 0 || limit <= free && free < start */
        ASSERT(n == dst->start_index);
        dst->start[n] = (free == 0) ? limit : free;
        /* Writing "dst->start[n] = free;" is attractive but wrong. The problem
           is that even if we can't compact the immutables much, we may still
           be able to copy immutables from the mutable area into the immutable
           area, but setting dst->start[n] to 0 would prevent this.
           SPF 19/11/1997 */
    }
    /* dst->start[0] .. dst->start[dst->start_index] is still is a descending sequence */

    if (free == 0)
        return 0;

    // Allocate the space.
    dst->bitmap.SetBits(free, n);

    PolyWord *newp = dst->wordAddr(free); /* New object address */
        
    // Update dst->upperAllocPtr, so the new object doesn't get trampled.
    if (newp < dst->upperAllocPtr)
        dst->upperAllocPtr = newp;

    return newp;
}

// This does nothing to the addresses but by applying it in ScanConstantsWithinCode we
// adjust any relative addresses so they are relative to the new location.
class MTGCProcessIdentity: public ScanAddress {
public:
   virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }
};

// Copy a cell to its new address.
void CopyObjectToNewAddress(PolyObject *srcAddress, PolyObject *destAddress, POLYUNSIGNED L)
{
    destAddress->SetLengthWord(L); /* copy length word */

    POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);

    for (POLYUNSIGNED i = 0; i < n; i++)
        destAddress->Set(i, srcAddress->Get(i));
    
    // If this is a code object flush out anything from the instruction cache
    // that might previously have been at this address
    if (OBJ_IS_CODE_OBJECT(L))
    {
        MTGCProcessIdentity identity;
        machineDependent->FlushInstructionCache(destAddress, n * sizeof(PolyWord));
        // We have to update any relative addresses in the code.
        machineDependent->ScanConstantsWithinCode(destAddress, srcAddress, OBJ_OBJECT_LENGTH(L), &identity);
    }
}

// Find the next space in the sequence.  It may return with the space unchanged if it
// is unable to find a suitable space.
static bool FindNextSpace(LocalMemSpace *src, LocalMemSpace **dst, bool isMutable, GCTaskId *id)
{
    unsigned m = 0;
    // If we're compressing the space and it's full that's it.
    if (*dst == src)
        return false;
    if (*dst != 0)
    {
        // Find the next space after this
        while (gMem.lSpaces[m] != *dst) m++;
        m++;
    }
    for (; m < gMem.nlSpaces; m++) {
        LocalMemSpace *lSpace = gMem.lSpaces[m];
        if (lSpace == src)
        {
            // The only possibility is to compact this area.
            ASSERT(!isMutable || src->isMutable);
            *dst = src;
            return true; // We already own it
        }
        if (lSpace->isMutable == isMutable && !lSpace->allocationSpace && lSpace->spaceOwner == 0)
        {
            // Now acquire the lock.  We have to retest spaceOwner with the lock held.
            PLocker lock(&copyLock);
            if (lSpace->spaceOwner == 0)
            {
                // Change the space.
                lSpace->spaceOwner = id;
                *dst = lSpace; // Return the space
                if (debugOptions & DEBUG_GC)
                    Log("GC: Copy: copying %s cells from %p to %p\n",
                                isMutable ? "mutable" : "immutable", src, lSpace);
                return true;
            }
        }
    }
    return false;
}

// Copy objects from the source space into an earlier space or up within the
// current space.
static void copyAllData(GCTaskId *id, void * /*arg1*/, void * /*arg2*/)
{
    LocalMemSpace *mutableDest = 0, *immutableDest = 0;

    for (unsigned i = gMem.nlSpaces; i > 0; i--)
    {
        LocalMemSpace *src = gMem.lSpaces[i-1];

        if (src->spaceOwner == 0)
        {
            PLocker lock(&copyLock);
            if (src->spaceOwner == 0)
                src->spaceOwner = id;
            else continue;
        }
        else if (src->spaceOwner != id)
            continue;

        if (debugOptions & DEBUG_GC)
            Log("GC: Copy: copying area %p (thread %p) %s \n", src, id, src->spaceTypeString());

        // We start at fullGCLowerLimit which is the lowest marked object in the heap
        // N.B.  It's essential that the first set bit at or above this corresponds
        // to the length word of a real object.
        POLYUNSIGNED  bitno   = src->wordNo(src->fullGCLowerLimit);
        // Set the limit to the top so we won't rescan this.  That can
        // only happen if copying takes a very short time and the same
        // thread runs multiple tasks.
        src->fullGCLowerLimit = src->top;

        // src->highest is the bit position that corresponds to the top of
        // generation we're copying.
        POLYUNSIGNED  highest = src->wordNo(src->top);

        for (;;)
        {
            if (bitno >= highest) break;

            /* SPF version; Invariant: 0 < highest - bitno */
            bitno += src->bitmap.CountZeroBits(bitno, highest - bitno);

            if (bitno >= highest) break;

            /* first set bit corresponds to the length word */
            PolyWord *old = src->wordAddr(bitno); /* Old object address */

            PolyObject *obj = (PolyObject*)(old+1);
        
            POLYUNSIGNED L = obj->LengthWord();
            ASSERT (OBJ_IS_LENGTH(L));
        
            POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L) + 1 ;/* Length of allocation (including length word) */
            bitno += n;

            // Find a mutable space for the mutable objects and an immutable space for
            // the immutables.  We copy objects into earlier spaces or within its own
            // space but we don't copy an object to a later space.  This avoids the
            // risk of copying an object multiple times.  Previously this copied objects
            // into later spaces but that doesn't work well if we have converted old
            // saved state segments into local areas.  It's much better to delete them
            // if possible.
            bool isMutable = OBJ_IS_MUTABLE_OBJECT(L);
            LocalMemSpace *destSpace = isMutable || immutableDest == 0 ? mutableDest : immutableDest;
            PolyWord *newp = FindFreeAndAllocate(destSpace, (src == destSpace) ? bitno : 0, n);
            if (newp == 0 && src != destSpace)
            {
                // See if we can find a different space.
                // N.B.  FindNextSpace side-effects mutableDest/immutableDest to give the next space.
                if (FindNextSpace(src, isMutable ? &mutableDest : &immutableDest, isMutable, id))
                {
                    bitno -= n; // Redo this object
                    continue;
                }
                // else just leave it
            }

            if (newp == 0) /* no room */
            {
                // We're not going to move this object
                // Update src->upperAllocPtr, so the old object doesn't get trampled.
                if (old < src->upperAllocPtr)
                    src->upperAllocPtr = old;

                // Previously this continued compressing to try to make space available
                // on the next GC.  Normally full GCs are infrequent so the chances are
                // that at the next GC other data will have been freed.  Just stop at
                // this point.
                // However if we're compressing a mutable area and there is immutable
                // data in it we should move those out because the mutable area is scanned
                // on every partial GC.
                if (! src->isMutable || src->i_marked == 0)
                    break;
            }
            else
            {
                PolyObject *destAddress = (PolyObject*)(newp+1);
                obj->SetForwardingPtr(destAddress);
                CopyObjectToNewAddress(obj, destAddress, L);

                if (debugOptions & DEBUG_GC_DETAIL)
                    Log("GC: Copy: %p %lu %u -> %p\n", obj, OBJ_OBJECT_LENGTH(L),
                                GetTypeBits(L), destAddress);
            }
        }

        if (mutableDest == src)
            mutableDest = 0;
        if (immutableDest == src)
            immutableDest = 0;
    }
}

void GCCopyPhase()
{
    unsigned j;
    mainThreadPhase = MTP_GCPHASECOMPACT;

    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        POLYUNSIGNED highest = lSpace->wordNo(lSpace->top);
        for (unsigned i = 0; i < NSTARTS; i++)
            lSpace->start[i] = highest;
        lSpace->start_index = NSTARTS - 1;
        lSpace->spaceOwner = 0;
        // Reset the allocation pointers. This puts garbage (and real data) below them.
        // At the end of the compaction the allocation pointer will point below the
        // lowest real data.
        lSpace->upperAllocPtr = lSpace->top;
    }
 
    // Copy the mutable data into a lower area if possible.
    if (gpTaskFarm->ThreadCount() == 0)
        copyAllData(globalTask, 0, 0);
    else
    {
        // We start as many tasks as we have threads.  If the amount of work to
        // be done is very small one thread could process more than one task.
        // Have to be careful because we use the task ID to decide which space
        // to scan.
        for (j = 0; j < gpTaskFarm->ThreadCount(); j++)
            gpTaskFarm->AddWorkOrRunNow(&copyAllData, 0, 0);
    }

    gpTaskFarm->WaitForCompletion();
}
