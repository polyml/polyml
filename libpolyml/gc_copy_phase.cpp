/*
    Title:      Multi-Threaded Garbage Collector - Copy phase

    Copyright (c) 2010-11 David C. J. Matthews

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
Copying starts at the bottom of the highest numbered space and cells are copied into
the top of the lowest numbered space.  The idea is to free spaces so they can be
deallocated if they are not required and to compact memory within the spaces that
are retained.  By following this ordering we avoid moving an object twice since if
it has been moved it will have been moved into the furthest possible space for it.

Multithreading.
We want to keep the operations independent to avoid the need for locking.
So, generally, cells should be kept within their own space.
Give each thread its own mutable and immutable space to process.
It can copy cells 
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
#include "check_objects.h"
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

    dst->copied += n;

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

    if (debugOptions & DEBUG_GC_DETAIL)
        Log("GC: Copy: %p %lu %u -> %p\n", srcAddress, OBJ_OBJECT_LENGTH(L),
                    GetTypeBits(L), destAddress);

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

    // We mustn't check the object until after we've adjusted any relative offsets.
    CheckObject(destAddress);
}

// Find the next space in the sequence.  It may return with the space unchanged if it
// is unable to find a suitable space.
// The copyLock must be held before this function is called.
static bool FindNextSpace(LocalMemSpace *src, LocalMemSpace **dst, bool isMutable, GCTaskId *id)
{
    unsigned m = 0;
    if (*dst != 0)
    {
        // Find the next space after this
        while (gMem.lSpaces[m] != *dst) m++;
        m++;
    }
    for (; m < gMem.nlSpaces; m++) {
        LocalMemSpace *lSpace = gMem.lSpaces[m];
        if (lSpace->isMutable == isMutable && !lSpace->allocationSpace)
        {
            // The new space must be below the space we're copying from.
            if (lSpace == src)
                return false;
            if (! lSpace->spaceInUse)
            {
                // Change the space.
                if (*dst)
                {
                    // Release any old space.
                    ASSERT((*dst)->spaceOwner == id);
                    (*dst)->spaceOwner = 0;
                    (*dst)->spaceInUse = false;
                }
                (*dst) = lSpace;
                lSpace->spaceInUse = true;
                lSpace->spaceOwner = id;
                // We need to set copiedIn to true here while we still hold the lock.
                // The effect of this is to prevent this space from being processed
                // during the "copy-to-new-space" pass and instead process it during
                // the compaction pass.  If we don't set it here we could start copying
                // out of the space in a new thread while we're still copying into it.
                lSpace->copiedIn = true;
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
static void CopyObjects(LocalMemSpace *src, LocalMemSpace *mutableDest, LocalMemSpace *immutableDest, GCTaskId *id)
{
    {
        PLocker lock(&copyLock);
        if (mutableDest == 0 && immutableDest == 0 && src->copiedIn)
            // We're copying out of this area.  We can't process it if we've
            // aready copied into it.  It will be processed later.
            return;
        ASSERT(! src->spaceInUse);
        ASSERT(src->spaceOwner == 0);
        // If we're trying to copy out of this area we need an immutable area.
        if (immutableDest == 0)
        {
            if (! FindNextSpace(src, &immutableDest, false, id))
            {
                // If we have a mutable area we can use that if there's
                // nothing else.
                if (mutableDest == 0)
                    return;
            }
        }
        // If this is a mutable area we may also need a mutable space
        if (src->isMutable && mutableDest == 0)
        {
            if (! FindNextSpace(src, &mutableDest, true, id))
            {
                // Release the immutable area before returning.
                immutableDest->spaceInUse = false;
                ASSERT(immutableDest->spaceOwner == id);
                immutableDest->spaceOwner = 0;
                return;
            }
        }
        src->spaceInUse = true;
        src->spaceOwner = id;
        ASSERT(! src->copiedOut);
        src->copiedOut = true;
    }

    /* Invariant: at this point there are no objects below src->fullGCLowerLimit */
    // We start at fullGCLowerLimit which is the lowest marked object in the heap
    // N.B.  It's essential that the first set bit at or above this corresponds
    // to the length word of a real object.
    POLYUNSIGNED  bitno   = src->wordNo(src->fullGCLowerLimit);
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
        CheckObject(obj);
        
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
            PLocker lock(&copyLock);
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
            obj->SetForwardingPtr((PolyObject*)(newp+1));
            CopyObjectToNewAddress(obj, (PolyObject*)(newp+1), L);
        }
    }

    {
        PLocker lock(&copyLock);
        if (mutableDest && mutableDest != src)
        {
            mutableDest->spaceInUse = false;
            ASSERT(mutableDest->spaceOwner == id);
            mutableDest->spaceOwner = 0;
        }
        if (immutableDest && immutableDest != src)
        {
            immutableDest->spaceInUse = false;
            ASSERT(immutableDest->spaceOwner == id);
            immutableDest->spaceOwner = 0;
        }
        src->spaceInUse = false;
        ASSERT(src->spaceOwner == id);
        src->spaceOwner = 0;
    }
}

// Copy mutable areas into a new area.  If the area to be copied has
// already received data copied from elsewhere it isn't copied and is
// processed in copyMutableArea.
static void copyMutableAreaToNewArea(GCTaskId *id, void *arg1, void * /*arg2*/)
{
    LocalMemSpace *lSpace = (LocalMemSpace *)arg1;
    if (debugOptions & DEBUG_GC)
        Log("GC: Copy: copying mutable area %p to new area\n", lSpace);
    CopyObjects(lSpace, 0, 0, id);
}

// Similar to copyMutableAreaToNewArea but for immutable data.
static void copyImmutableAreaToNewArea(GCTaskId *id, void *arg1, void * /*arg2*/)
{
    LocalMemSpace *lSpace = (LocalMemSpace *)arg1;
    if (debugOptions & DEBUG_GC)
        Log("GC: Copy: copying immutable area %p to new area\n", lSpace);
    CopyObjects(lSpace, 0, 0, id);
}

static void copyMutableArea(GCTaskId *id, void *arg1, void * /*arg2*/)
{
    LocalMemSpace *lSpace = (LocalMemSpace *)arg1;
    if (debugOptions & DEBUG_GC)
        Log("GC: Copy: compressing mutable area %p\n", lSpace);
    CopyObjects(lSpace, lSpace, 0, id);
}

static void copyImmutableArea(GCTaskId *id, void *arg1, void * /*arg2*/)
{
    LocalMemSpace *lSpace = (LocalMemSpace *)arg1;
    if (debugOptions & DEBUG_GC)
        Log("GC: Copy: compressing immutable area %p\n", lSpace);
    CopyObjects(lSpace, 0, lSpace, id);
}

void GCCopyPhase(POLYUNSIGNED &immutable_overflow)
{
    unsigned j;
    mainThreadPhase = MTP_GCPHASECOMPACT;

    /* Invariant: at most the first (gen_top - bottom) bits of each bitmap can be dirty here. */
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        POLYUNSIGNED highest = lSpace->wordNo(lSpace->top);
        for (unsigned i = 0; i < NSTARTS; i++)
            lSpace->start[i] = highest;
        lSpace->start_index = NSTARTS - 1;
        lSpace->copied = 0;
        lSpace->copiedIn = lSpace->copiedOut = false;
        lSpace->spaceOwner = 0;
        ASSERT(! lSpace->spaceInUse);
    }
    /* Invariant: lSpace->start[0] .. lSpace->start[lSpace->start_index] is a descending sequence. */ 
    
    /* Invariant: there are no objects below lSpace->fullGCLowerLimit. */

    // First, process the mutable areas, copying immutable data into the immutable areas
    // and compacting mutable objects within the area.
    // "immutable_overflow" is the immutable space that is left in the mutable area
    // because the immutable segments were not large enough or were full.
    {
        POLYUNSIGNED immutableFree = 0, immutableNeeded = 0;
        for(j = 0; j < gMem.nlSpaces; j++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j];
            if (lSpace->isMutable)
                // Mutable area - add up the immutables to be moved out
                immutableNeeded += lSpace->i_marked;
            else
            { // Immutable area - calculate the number of unallocated words WITHIN the area
                // immutableSpace is the size of the allocated area for this space BEFORE the GC.
                POLYUNSIGNED immutableSpace = lSpace->top - lSpace->fullGCLowerLimit;
                POLYUNSIGNED immutableUsed = lSpace->i_marked;
                immutableFree += immutableSpace - immutableUsed;
            }
        }
        
        // Reset the allocation pointers. This puts garbage (and real data) below them.
        // At the end of the compaction the allocation pointer will point below the
        // lowest real data.
        for(j = 0; j < gMem.nlSpaces; j++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j];
            lSpace->upperAllocPtr = lSpace->top;
        }

        // Do the copying.
        // Invariant: there are no objects below fullGCLowerLimit.
        // Copy the mutable data into a lower area if possible.
        for(j = gMem.nlSpaces; j > 0; j--)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j-1];
            if (lSpace->isMutable)
                gpTaskFarm->AddWorkOrRunNow(&copyMutableAreaToNewArea, lSpace, 0);
        }
        gpTaskFarm->WaitForCompletion();

        // For areas that have received copied data try to compact them but
        // don't move data out.
        for(j = gMem.nlSpaces; j > 0; j--)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j-1];
            if (lSpace->isMutable && ! lSpace->copiedOut)
                gpTaskFarm->AddWorkOrRunNow(&copyMutableArea, lSpace, 0);
        }
        gpTaskFarm->WaitForCompletion();

        // Calculate the amount copied.
        // markedImmut is the amount of immutable data in the mutable area
        // markedMut is the amount of mutable data in the mutable area
        // copiedToI is the amount of immutable data added to the immutable area
        // copiedToM is the amount of mutable data copied
        POLYUNSIGNED markedImmut = 0, markedMut = 0, copiedToI = 0, copiedToM = 0;
        for(j = 0; j < gMem.nlSpaces; j++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j];
            if (lSpace->isMutable)
            {
                markedImmut += lSpace->i_marked;
                markedMut += lSpace->m_marked;
                copiedToM += lSpace->copied;
            }
            else
                copiedToI += lSpace->copied;
        }
        
        ASSERT(copiedToM + copiedToI <= markedMut + markedImmut);
        ASSERT(copiedToI <= markedImmut);
        ASSERT(copiedToI != markedImmut || copiedToM <= markedMut);
        /* We may have A.M.copied > A.M.m_marked, if the immutable buffer overflows */
        
        // We should have copied all the immutable objects from the mutable
        // areas but if we didn't have enough space in the immutable areas
        // this will record the extra space we need.
        immutable_overflow = markedImmut - copiedToI;
    }

    for(j = 0; j < gMem.nlSpaces; j++)
        gMem.lSpaces[j]->copied = 0;

    // Process any immutable areas that have not yet received any data.  If they
    // have received data or receive it as a result of copying from another immutable
    // space they won't be processed at this stage.
    for(j = gMem.nlSpaces; j > 0; j--)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j-1];
        if (! lSpace->isMutable)
            gpTaskFarm->AddWorkOrRunNow(&copyImmutableAreaToNewArea, lSpace, 0);
    }
    gpTaskFarm->WaitForCompletion();

    // Complete processing the immutable spaces including those that have received data.
    for(j = gMem.nlSpaces; j > 0; j--)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j-1];
        if (! lSpace->isMutable)
        {
            // If the current allocation pointer is below the lowest marked object then
            // we must already have copied some data into this area and filled all the
            // holes or at least found something that was too big to fit in the holes.
            // Compacting the area would not free any space but might allow the next
            // GC to recover something.
            if (lSpace->fullGCLowerLimit <= lSpace->upperAllocPtr)
            {
                /* Invariant: there are no objects below lSpace->fullGCLowerLimit. */
                if (! lSpace->copiedOut)
                    gpTaskFarm->AddWorkOrRunNow(&copyImmutableArea, lSpace, 0);
            }
        }
    }
    gpTaskFarm->WaitForCompletion();

    POLYUNSIGNED iCopied = 0, iMarked = 0;
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        if (! lSpace->isMutable)
        {
            iMarked += lSpace->i_marked;
            iCopied += lSpace->copied;
        }
    }
    ASSERT(iCopied <= iMarked);
}
