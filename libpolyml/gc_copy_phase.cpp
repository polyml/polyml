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

inline POLYUNSIGNED BITNO(LocalMemSpace *area, PolyWord *pt) { return pt - area->bottom; }
inline PolyWord *BIT_ADDR(LocalMemSpace *area, POLYUNSIGNED bitno) { return area->bottom + bitno; }

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

    PolyWord *newp = BIT_ADDR(dst, free); /* New object address */
        
    // Update dst->pointer, so the new object doesn't get trampled.
    if (newp < dst->pointer)
        dst->pointer = newp;

    // If we are copying into a later area we may copy into an area
    // that crosses gen_bottom for that area.  We need to adjust gen_bottom
    // since we assume above that gen_bottom points to a valid object.
    // DCJM: This may no longer be necessary since we don't copy into
    // a later area.
    if (newp < dst->gen_bottom && newp+n > dst->gen_bottom)
        dst->gen_bottom = newp+n;

    dst->copied += n;
    dst->copiedIn = true;

    return newp;
}

// This does nothing to the addresses but by applying it in ScanConstantsWithinCode we
// adjust any relative addresses so they are relative to the new location.
class MTGCProcessIdentity: public ScanAddress {
public:
   virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }
};

// Copy a cell to its new address.
void CopyObjectToNewAddress(PolyObject *srcAddress, PolyObject *destAddress)
{
    POLYUNSIGNED L = srcAddress->LengthWord();
    destAddress->SetLengthWord(L); /* copy length word */

    if (debugOptions & DEBUG_GC_DETAIL)
        Log("GC: Copy: %p %lu %u -> %p\n", srcAddress, OBJ_OBJECT_LENGTH(L),
                    GetTypeBits(L), destAddress);

    POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);

    for (POLYUNSIGNED i = 0; i < n; i++)
        destAddress->Set(i, srcAddress->Get(i));

    srcAddress->SetForwardingPtr(destAddress);
    
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
static bool FindNextSpace(LocalMemSpace *src, LocalMemSpace * &dst, bool isMutable)
{
    unsigned m = 0;
    if (dst != 0)
    {
        // Find the next space after this
        while (gMem.lSpaces[m] != dst) m++;
        m++;
    }
    for (; m < gMem.nlSpaces; m++) {
        LocalMemSpace *lSpace = gMem.lSpaces[m];
        if (lSpace->isMutable == isMutable && !lSpace->allocationSpace)
        {
            // The new space must be below the space we're copying from.
            if (lSpace == src) return false;
            if (! lSpace->spaceInUse)
            {
                // Change the space.
                if (dst) dst->spaceInUse = false;
                dst = lSpace;
                dst->spaceInUse = true;
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
// current space.  compressImmutables determines whether we should search for
// gaps when copying into a immutable area or simply add the cells onto the
// end of the area.
static void CopyObjects(LocalMemSpace *src, LocalMemSpace *mutableDest,
                        LocalMemSpace *immutableDest, bool compressImmutables)
{
    {
        PLocker lock(&copyLock);
        if (mutableDest == 0 && immutableDest == 0 && src->copiedIn)
            // We're copying out of this area.  We can't process it if we've
            // aready copied into it.  It will be processed later.
            return;
        // If we're trying to copy out of this area we need an immutable area.
        if (immutableDest == 0)
        {
            if (! FindNextSpace(src, immutableDest, false))
                return;
            immutableDest->copiedIn = true;
        }
        // If this is a mutable area we may also need a mutable space
        if (src->isMutable && mutableDest == 0)
        {
            if (! FindNextSpace(src, mutableDest, true))
            {
                // Release the immutable area before returning.
                immutableDest->spaceInUse = false;
                return;
            }
            // At least in the case of a mutable space we have to set this
            // as "copiedIn" so that another thread doesn't try to copy out of
            // it.
            mutableDest->copiedIn = true;
        }
        src->spaceInUse = true;
        ASSERT(! src->copiedOut);
        src->copiedOut = true;
    }

    /* Start scanning the bitmap from the very bottom since it is    */
    /* likely that very recently created objects need copying.       */
    /* Skip whole words of zeroes since these may be quite common if */
    /* the objects to be copied are sparsely separated.              */

    /* Invariant: at this point there are no objects below src->gen_bottom */
    POLYUNSIGNED  bitno   = BITNO(src,src->gen_bottom);
    // src->highest is the bit position that corresponds to the top of
    // generation we're copying.
    POLYUNSIGNED  highest = src->highest;

    for (;;)
    {
        if (bitno >= highest) break;

        /* SPF version; Invariant: 0 < highest - bitno */
        bitno += src->bitmap.CountZeroBits(bitno, highest - bitno);

        if (bitno >= highest) break;

        ASSERT (src->bitmap.TestBit(bitno));

        /* first set bit corresponds to the length word */
        PolyWord *old = BIT_ADDR(src, bitno); /* Old object address */

        PolyObject *obj = (PolyObject*)(old+1);
        
        POLYUNSIGNED L = obj->LengthWord();
        ASSERT (OBJ_IS_LENGTH(L));
        CheckObject(obj);
        
        POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L) + 1 ;/* Length of allocation (including length word) */
        bitno += n;
        
        PolyWord *newp = 0;

        // Find a mutable space for the mutable objects and an immutable space for
        // the immutables.  We copy objects into earlier spaces or within its own
        // space but we don't copy an object to a later space.  This avoids the
        // risk of copying an object multiple times.  Previously this copied objects
        // into later spaces but that doesn't work well if we have converted old
        // saved state segments into local areas.  It's much better to delete them
        // if possible.
        if (OBJ_IS_MUTABLE_OBJECT(L))
        {
            // Mutable object - need a mutable space for it.
            newp = FindFreeAndAllocate(mutableDest, (src == mutableDest) ? bitno : 0, n);
            if (newp == 0 && src != mutableDest)
            {
                // See if we can find a different space.
                PLocker lock(&copyLock);
                if (FindNextSpace(src, mutableDest, true))
                {
                    bitno -= n; // Redo this object
                    continue;
                }
                // else just leave it
            }
        }
        else
        {
            // Immutable object
            if (compressImmutables)
                newp = FindFreeAndAllocate(immutableDest, (src == immutableDest) ? bitno : 0, n);
            else if (immutableDest != 0)
            {
                // Not compressing the immutables so just allocate at the bottom of the area
                POLYUNSIGNED dest_bitno = BITNO(immutableDest, immutableDest->pointer);
                ASSERT(src->isMutable); // Only if we're copying from mutable area
                if (n < dest_bitno)
                {
                    POLYUNSIGNED free = dest_bitno - n;
                    immutableDest->bitmap.SetBits(free, n);
                    newp = BIT_ADDR(immutableDest, free); /* New object address */

                    // Update dst->pointer, so the new object doesn't get trampled.
                    if (newp < immutableDest->pointer)
                        immutableDest->pointer = newp;

                    // If we are copying into a later area we may copy into an area
                    // that crosses gen_bottom for that area.  We need to adjust gen_bottom
                    // since we assume above that gen_bottom points to a valid object.
                    // DCJM: This may no longer be necessary since we don't copy into
                    // a later area.
                    if (newp < immutableDest->gen_bottom && newp+n > immutableDest->gen_bottom)
                        immutableDest->gen_bottom = newp+n;

                    immutableDest->copied += n;
                    immutableDest->copiedIn = true;
                }
            }
            if (newp == 0 && src != immutableDest)
            {
                // Have to search in a different space.
                PLocker lock(&copyLock);
                if (FindNextSpace(src, immutableDest, false))
                {
                    bitno -= n; // Redo this object
                    continue;
                }
            }
        }
        if (newp == 0) /* no room */
        {
            // We're not going to move this object
            // Update src->pointer, so the old object doesn't get trampled.
            if (old < src->pointer)
                src->pointer = old;

            /* We haven't been able to move this object on this GC, but we might    */
            /* still be able to move some smaller objects, which might free enough  */
            /* space that we'll be able to move this object on the next GC, even if */
            /* nothing becomes garbage before then. SPF 19/11/1997                  */
            continue;
        }

        CopyObjectToNewAddress(obj, (PolyObject*)(newp+1));
    }

    {
        PLocker lock(&copyLock);
        if (mutableDest) mutableDest->spaceInUse = false;
        if (immutableDest) immutableDest->spaceInUse = false;
        src->spaceInUse = false;
    }
}

// Copy mutable areas into a new area.  If the area to be copied has
// already received data copied from elsewhere it isn't copied and is
// processed in copyMutableArea.
static void copyMutableAreaToNewArea(void *arg1, void *arg2)
{
    LocalMemSpace *lSpace = (LocalMemSpace *)arg1;
    bool compressImmutables = *(bool*)arg2;
    if (debugOptions & DEBUG_GC)
        Log("GC: Copy: copying mutable area %p to new area\n", lSpace);
    CopyObjects(lSpace, 0, 0, compressImmutables);
}

// Similar to copyMutableAreaToNewArea but for immutable data.
static void copyImmutableAreaToNewArea(void *arg1, void * /*arg2*/)
{
    LocalMemSpace *lSpace = (LocalMemSpace *)arg1;
    if (debugOptions & DEBUG_GC)
        Log("GC: Copy: copying immutable area %p to new area\n", lSpace);
    CopyObjects(lSpace, 0, 0, true);
}

static void copyMutableArea(void *arg1, void *arg2)
{
    LocalMemSpace *lSpace = (LocalMemSpace *)arg1;
    bool compressImmutables = *(bool*)arg2;
    if (debugOptions & DEBUG_GC)
        Log("GC: Copy: compressing mutable area %p\n", lSpace);
    CopyObjects(lSpace, lSpace, 0, compressImmutables);
}

static void copyImmutableArea(void *arg1, void * /*arg2*/)
{
    LocalMemSpace *lSpace = (LocalMemSpace *)arg1;
    if (debugOptions & DEBUG_GC)
        Log("GC: Copy: compressing immutable area %p\n", lSpace);
    CopyObjects(lSpace, 0, lSpace, true);
}

void GCCopyPhase(POLYUNSIGNED &immutable_overflow)
{
    unsigned j;
    mainThreadPhase = MTP_GCPHASECOMPACT;

    /* Invariant: at most the first (gen_top - bottom) bits of each bitmap can be dirty here. */
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        lSpace->highest = BITNO(lSpace, lSpace->gen_top);
        for (unsigned i = 0; i < NSTARTS; i++)
            lSpace->start[i] = lSpace->highest;
        lSpace->start_index = NSTARTS - 1;
        lSpace->copied = 0;
        lSpace->copiedIn = lSpace->copiedOut = false;
        ASSERT(! lSpace->spaceInUse);
    }
    /* Invariant: lSpace->start[0] .. lSpace->start[lSpace->start_index] is a descending sequence. */ 
    
    /* Invariant: there are no objects below lSpace->gen_bottom. */

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
                POLYUNSIGNED immutableSpace = lSpace->gen_top - lSpace->gen_bottom;
                POLYUNSIGNED immutableUsed = lSpace->i_marked;
                immutableFree += immutableSpace - immutableUsed;
            }
        }
        // This is an optimisation.  If we have a small amount of immutable data
        // to move from the mutable area relative to the size of gaps in the
        // immutable area we use a compacting copy which tries to use these gaps.
        // If there is a larger amount of immutable data to move we simply add them
        // on at the bottom.  The idea is to reduce the cost of finding spaces to
        // copy these objects.
        bool compressImmutables = immutableNeeded / 2 < immutableFree ; /* Needs tuning!!! */
        
        // Reset the allocation pointers. This puts garbage (and real data) below them.
        // At the end of the compaction the allocation pointer will point below the
        // lowest real data.
        for(j = 0; j < gMem.nlSpaces; j++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j];
            if (lSpace->isMutable || compressImmutables)
                lSpace->pointer = lSpace->gen_top;
        }

        // Do the copying.
        // Invariant: there are no objects below gen_bottom.
        // Copy the mutable data into a lower area if possible.
        for(j = gMem.nlSpaces; j > 0; j--)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j-1];
            if (lSpace->isMutable)
                gpTaskFarm->AddWorkOrRunNow(&copyMutableAreaToNewArea, lSpace, &compressImmutables);
        }
        gpTaskFarm->WaitForCompletion();

        // For areas that have received copied data try to compact them but
        // don't move data out.
        for(j = gMem.nlSpaces; j > 0; j--)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j-1];
            if (lSpace->isMutable && ! lSpace->copiedOut)
                gpTaskFarm->AddWorkOrRunNow(&copyMutableArea, lSpace, &compressImmutables);
        }
        gpTaskFarm->WaitForCompletion();

        // Calculate the amount copied.
        // markedImmut is the amount of immutable data in the mutable area
        // markedMut is the amount of mutable data in the mutable area
        // copiedToI is the amount of immutable data added to the immutable area
        // copiedToM is the amount of mutable data copied
        unsigned markedImmut = 0, markedMut = 0, copiedToI = 0, copiedToM = 0;
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
    
    
    /* The area between A.M.gen_bottom and A.M.pointer may contain
       tombstones, so we daren't increase A.M.gen_bottom. */
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        if (lSpace->isMutable)
        {
            // We may have copied mutable objects from an earlier space
            if (lSpace->pointer < lSpace->gen_bottom)
                lSpace->gen_bottom = lSpace->pointer;
        }
    }
    
    /* If we've copied an object from the mutable area below the previous
       limit of the immutable area using a "non-compressing" copy,
       it would be unsafe to attempt to compress the immutable area (we
       might get a double indirection).
    
       However, it *is* safe if we've used a "compressing" copy from
       the mutables buffer. We won't move anything twice, because each
       object goes into the first "big enough" hole on each pass. If
       the second pass finds a "big enough" hole above the object, the
       first pass would have found this hole too, and used it.
     
       This is slightly tricky reasoning, so be careful!
      
       SPF 19/12/1997
    */
    
    /* Reclaim the genuine data from the immutable buffer. */
    for(j = 0; j < gMem.nlSpaces; j++)
        gMem.lSpaces[j]->copied = 0;

    POLYUNSIGNED immutable_space = 0, immutable_used = 0, immutable_needed = 0;
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        if (! lSpace->isMutable)
        {
            // If we have copied immutable objects out of the mutable buffer
            // below gen_bottom we need to reset that.
//            if (lSpace->pointer < lSpace->gen_bottom)
//               lSpace->gen_bottom = lSpace->pointer;
            immutable_space  += lSpace->gen_top - lSpace->gen_bottom;
            immutable_used   += lSpace->i_marked + lSpace->copied;
            immutable_needed += lSpace->i_marked;
        }
    }

    POLYUNSIGNED immutable_free = immutable_space - immutable_used;
    bool compressImmutables = immutable_needed / 4 < immutable_free ; /* Needs tuning!!! */

    // Process any immutable areas that have not yet received any data.  If they
    // have received data or receive it as a result of copying from another immutable
    // space they won't be processed at this stage.
    if (compressImmutables)
    {
        for(j = gMem.nlSpaces; j > 0; j--)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j-1];
            if (! lSpace->isMutable)
                gpTaskFarm->AddWorkOrRunNow(&copyImmutableAreaToNewArea, lSpace, 0);
        }
    }
    gpTaskFarm->WaitForCompletion();

    // Complete processing the immutable spaces including those that have received data.
    for(j = gMem.nlSpaces; j > 0; j--)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j-1];
        if (! lSpace->isMutable)
        {
            if (lSpace->gen_bottom <= lSpace->pointer)
            {
                if (compressImmutables)
                {
                    /* Invariant: there are no objects below lSpace->gen_bottom. */
                    if (! lSpace->copiedOut)
                        gpTaskFarm->AddWorkOrRunNow(&copyImmutableArea, lSpace, 0);
                }
                else // simply reclaim the immutable data (with its embedded garbage)
                    lSpace->pointer = lSpace->gen_bottom;

                ASSERT(lSpace->gen_bottom <= lSpace->pointer);
                // The area between lSpace->gen_bottom and lSpace->pointer may contain
                // tombstones, so we daren't increase lSpace->gen_bottom.
            }
            else // We may have copied immutable objects from an earlier space.
                lSpace->gen_bottom = lSpace->pointer;
        }
    }
    gpTaskFarm->WaitForCompletion();

    // An extra little check.
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        if (! lSpace->isMutable)
        {
            ASSERT(lSpace->gen_bottom <= lSpace->pointer);
        }
    }

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

    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        ASSERT(INSOFTRANGE(lSpace->pointer, lSpace->bottom, lSpace->gen_top));
    }    
}
