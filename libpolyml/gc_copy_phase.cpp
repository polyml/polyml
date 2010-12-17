/*
    Title:      Multi-Threaded Garbage Collector - Copy phase

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

#include "globals.h"
#include "machine_dep.h"
#include "processes.h"
#include "gc.h"
#include "scanaddrs.h"
#include "check_objects.h"
#include "bitmap.h"
#include "memmgr.h"
#include "gctaskfarm.h"

inline POLYUNSIGNED BITNO(LocalMemSpace *area, PolyWord *pt) { return pt - area->bottom; }
inline PolyWord *BIT_ADDR(LocalMemSpace *area, POLYUNSIGNED bitno) { return area->bottom + bitno; }

/* Search the area downwards looking for n consecutive free words.          */
/* Return the bitmap index if successful or 0 (should we use -1?) on failure. */
static inline POLYUNSIGNED FindFreeInArea(LocalMemSpace *dst, POLYUNSIGNED limit, POLYUNSIGNED n)
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
    
    return free;
}

// This does nothing to the addresses but by applying it in ScanConstantsWithinCode we
// adjust any relative addresses so they are relative to the new location.
class MTGCProcessIdentity: public ScanAddress {
public:
   virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }
};

// Copy objects from the source space into an earlier space or up within the
// current space.  compressImmutables determines whether we should search for
// gaps when copying into a immutable area or simply add the cells onto the
// end of the area.
static void CopyObjectsInArea(LocalMemSpace *src, bool compressImmutables)
{
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
        if (bitno >= highest) return;
        
        /* SPF version; Invariant: 0 < highest - bitno */
        bitno += src->bitmap.CountZeroBits(bitno, highest - bitno);
        
        if (bitno >= highest) return;
        
        ASSERT (src->bitmap.TestBit(bitno));
        
        /* first set bit corresponds to the length word */
        PolyWord *old = BIT_ADDR(src, bitno); /* Old object address */

        PolyObject *obj = (PolyObject*)(old+1);
        
        POLYUNSIGNED L = obj->LengthWord();
        ASSERT (OBJ_IS_LENGTH(L));
        CheckObject(obj);
        
        POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L) + 1 ;/* Length of allocation (including length word) */
        bitno += n;
        
        POLYUNSIGNED free = 0;  /* Bitmap index of new allocation */

        // The destination space if either a mutable space if this is a mutable
        // or an immutable space if it's immutable.
        LocalMemSpace *dst = 0;   /* New object allocation area */
        // Find a mutable space for the mutable objects and an immutable space for
        // the immutables.  We copy objects into earlier spaces or within its own
        // space but we don't copy an object to a later space.  This avoids the
        // risk of copying an object multiple times.  Previously this copied objects
        // into later spaces but that doesn't work well if we have converted old
        // saved state segments into local areas.  It's much better to delete them
        // if possible.
        for (unsigned i = 0; i < gMem.nlSpaces; i++)
        {
            dst = gMem.lSpaces[i];
            if (OBJ_IS_MUTABLE_OBJECT(L))
            {
                // Mutable object
                if (dst->isMutable)
                {
                    ASSERT(src->isMutable); // Should come from a mutable area
                    free = FindFreeInArea(dst, (src == dst) ? bitno : 0, n);
                    if (free)
                        break; // Found space.
                    if (src == dst)
                        break; // We mustn't copy it to an earlier area.
                }
            }
            else 
            {
                // Immutable object.
                if (! dst->isMutable)
                {
                    /* If we're copying mutables to the immutable area and we're just doing sequential
                       allocations at the bottom, we can optimise out all that "clever" search
                       code in FindFreeInArea. */
                    if (! compressImmutables)
                    {
                        POLYUNSIGNED dest_bitno = BITNO(dst, dst->pointer);
                        ASSERT(src->isMutable); // Only if we're copying from mutable area
                        if (n < dest_bitno)
                        {
                            free = dest_bitno - n;
                            break;
                        }
                    }
                    else // It's a full GC, so try to be compact within the immutable area. 
                    {
                        free = FindFreeInArea(dst, (src == dst) ? bitno : 0, n);
                        if (free)
                            break;
                    }
                    // We mustn't copy it to an earlier area.  N.B. If we're copying from
                    // a mutable area we CAN copy it to an immutable area earlier in
                    // the sequence.
                    if (src == dst)
                        break;
                }
            }
        }
        
        if (free == 0) /* no room */
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
        
        /* allocate object in the bitmap */
        dst->bitmap.SetBits(free, n);
        PolyWord *newp = BIT_ADDR(dst, free); /* New object address */
        
        /* Update dst->pointer, so the new object doesn't get trampled. SPF 4/10/96 */
        if (newp < dst->pointer)
            dst->pointer = newp;

        // If we are copying into a later area we may copy into an area
        // that crosses gen_bottom for that area.  We need to adjust gen_bottom
        // since we assume above that gen_bottom points to a valid object.
        if (newp < dst->gen_bottom && newp+n > dst->gen_bottom)
            dst->gen_bottom = newp+n;

        PolyObject *newObj = (PolyObject*)(newp+1);
        
        if (OBJ_IS_STACK_OBJECT(L))
        {
            newObj ->SetLengthWord(L); /* copy length word */
            CopyStackFrame ((StackObject *)obj,(StackObject *)newObj);
            obj->SetForwardingPtr(newObj);
        }
        else /* not a stack object */
        {
            for (POLYUNSIGNED i = 0; i < n; i++)
                newp[i] = old[i];
            
            ASSERT((*newp).AsUnsigned() == L);
            obj->SetForwardingPtr(newObj);
            
            // If this is a code object flush out anything from the instruction cache
            // that might previously have been at this address
            if (OBJ_IS_CODE_OBJECT(L))
            {
                MTGCProcessIdentity identity;
                machineDependent->FlushInstructionCache(newp, n * sizeof(PolyWord));
                // We have to update any relative addresses in the code.
                machineDependent->ScanConstantsWithinCode(newObj, obj, OBJ_OBJECT_LENGTH(L), &identity);
            }

            // We mustn't check the object until after we've adjusted any relative offsets.
            CheckObject((PolyObject*)(BIT_ADDR(dst, free) + 1));
        }
        
        dst->copied += n;
  }
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
        
        /* Reset the allocation pointers. This puts garbage (and real data) below them. */
        for(j = 0; j < gMem.nlSpaces; j++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j];
            if (lSpace->isMutable || compressImmutables)
                lSpace->pointer = lSpace->gen_top;
        }

        /* Invariant: there are no objects below A.M.gen_bottom. */
        for(j = gMem.nlSpaces; j > 0; j--)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j-1];
            if (lSpace->isMutable)
                CopyObjectsInArea(lSpace, compressImmutables);
        }

        // Calculate the amount copied.
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
        
        // If we didn't have enough space in the immutable areas to copy out the
        // immutable objects this will record the extra space we would need.
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

    for(j = gMem.nlSpaces; j > 0; j--)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j-1];
        if (! lSpace->isMutable)
        {
            if (lSpace->gen_bottom <= lSpace->pointer)
            {
                if (compressImmutables)
                {
                    lSpace->copied = 0;
                    /* Invariant: there are no objects below lSpace->gen_bottom. */
                    CopyObjectsInArea(lSpace, true);
                }
                else // simply reclaim the immutable data (with its embedded garbage)
                    lSpace->pointer = lSpace->gen_bottom;

                ASSERT(lSpace->gen_bottom <= lSpace->pointer);
                /* The area between lSpace->gen_bottom and lSpace->pointer may contain
                   tombstones, so we daren't increase lSpace->gen_bottom. */
            }
            else // We may have copied immutable objects from an earlier space.
                lSpace->gen_bottom = lSpace->pointer;
        }
    }
    // An extra little check.
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        if (! lSpace->isMutable)
        {
            ASSERT(lSpace->gen_bottom <= lSpace->pointer);
        }
    }
}
