/*
    Title:      Garbage Collector

    Copyright (c) 2000-8
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

// Settings moved from userOptions.
static unsigned long    heapSize, immutableSegSize, mutableSegSize;
static unsigned long    immutableFreeSpace, mutableFreeSpace;
static unsigned long    immutableMinFree, mutableMinFree; // Probably remove

static POLYUNSIGNED GetPhysicalMemorySize(void);

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

/* start <= val < end */
#define INRANGE(val,start,end) ((start) <= (val) && (val) < (end))
  
/* start <= val <= end */
#define INSOFTRANGE(val,start,end) ((start) <= (val) && (val) <= (end))

/* Code pointers are usually aligned to 2 mod 4 
   However stack->p_pc is not necessarily aligned, so we have to 
   be careful */
//#define IN_GC_AREA(_pt) (! IS_INT(_pt) && (IN_GC_IAREA((_pt).AsAddress()) || IN_GC_MAREA((_pt).AsAddress())))

inline POLYUNSIGNED BITNO(LocalMemSpace *area, PolyWord *pt) { return pt - area->bottom; }
inline PolyWord *BIT_ADDR(LocalMemSpace *area, POLYUNSIGNED bitno) { return area->bottom + bitno; }


void CopyStackFrame(StackObject *old_stack, StackObject *new_stack)
{
  /* Moves a stack, updating all references within the stack */
    PolyWord *old_base  = (PolyWord *)old_stack;
    PolyWord *new_base = (PolyWord*)new_stack;
    POLYUNSIGNED old_length = old_stack->Length();
    POLYUNSIGNED new_length = new_stack->Length();
    PolyWord        *old_top    = old_base + old_length;

    CheckObject (old_stack);

    ASSERT (old_stack->IsStackObject());
    ASSERT (new_stack->IsStackObject());
#if 0
    /* This doesn't hold if we a copying a "frozen" stack on system start-up */
    ASSERT (OBJ_IS_MUTABLE_OBJECT(old_base[-1]));
#endif
    ASSERT (new_stack->IsMutable());

    /* Calculate the offset of the new stack from the old. If the frame is
       being extended objects in the new frame will be further up the stack
       than in the old one. */

    POLYSIGNED offset = new_base - old_base + new_length - old_length;

    /* Copy the registers, changing any that point into the stack. */

    new_stack->p_space = old_stack->p_space;
    new_stack->p_pc    = old_stack->p_pc;
    new_stack->p_sp    = old_stack->p_sp + offset;
    new_stack->p_hr    = old_stack->p_hr + offset;
    new_stack->p_nreg  = old_stack->p_nreg;

    /* p_nreg contains contains the number of CHECKED registers */
//    ASSERT(new_stack->p_nreg == CHECKED_REGS);

    POLYUNSIGNED i;
    for (i = 0; i < new_stack->p_nreg; i++)
    {
        PolyWord R = old_stack->p_reg[i];

        /* if the register points into the old stack, make the new copy
           point at the same relative offset within the new stack,
           otherwise make the new copy identical to the old version. */

        if (R.IsTagged() || ! INRANGE(R.AsAddress(),old_base,old_top))
            new_stack->p_reg[i] = R;
        else new_stack->p_reg[i] = PolyWord::FromStackAddr(R.AsStackAddr() + offset);
    }

    /* Copy unchecked registers. - The next "register" is the number of
       unchecked registers to copy. Unchecked registers are used for 
       values that might look like addresses, i.e. don't have tag bits, 
       but are not. */

    POLYUNSIGNED n = old_stack->p_reg[i].AsUnsigned();
    new_stack->p_reg[i] = old_stack->p_reg[i];
    i++;
    ASSERT (n < 100);
    while (n--)
    { 
        new_stack->p_reg[i] = old_stack->p_reg[i];
        i++;
    }

    /* Skip the unused part of the stack. */

    i = (PolyWord*)old_stack->p_sp - old_base;

    ASSERT (i <= old_length);

    i = old_length - i;

    PolyWord *old = old_stack->p_sp;
    PolyWord *newp= new_stack->p_sp;

    while (i--)
    {
        PolyWord old_word = *old++;
        if (old_word.IsTagged() || ! INRANGE(old_word.AsAddress(),old_base,old_top))
            *newp++ = old_word;
        else
            *newp++ = PolyWord::FromStackAddr(old_word.AsStackAddr() + offset);
    }

    CheckObject (new_stack);
}


/**************************************************************************/
/* This function finds all the mutable objects in the local mutable area. */
/* These are scanned since they may contain references into the gc area.  */
/**************************************************************************/
// Mark these mutables.
static void OpMutables(ScanAddress *process)
{
    // Scan the local mutable areas.  It won't do anything if this is a full
    // GC since gen_top == top.
    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        LocalMemSpace *space = gMem.lSpaces[i];
        if (space->isMutable)
            process->ScanAddressesInRegion(space->gen_top, space->top);
    }
    // Scan the permanent mutable areas.
    for (unsigned j = 0; j < gMem.npSpaces; j++)
    {
        MemSpace *space = gMem.pSpaces[j];
        if (space->isMutable)
            process->ScanAddressesInRegion(space->bottom, space->top);
    }
}

class ProcessMarkPointers: public ScanAddress
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
};

// Mark all pointers in the heap.
POLYUNSIGNED ProcessMarkPointers::DoScanAddressAt(PolyWord *pt, bool isWeak)
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
    if (space->bitmap.TestBit(new_bitno))
        return 0; // Already marked

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

    if (isWeak) // This is a SOME within a weak reference.
        return 0;

    if (OBJ_IS_BYTE_OBJECT(L))
        return 0; // We've done as much as we need
    else if (OBJ_IS_CODE_OBJECT(L) || OBJ_IS_STACK_OBJECT(L) || OBJ_IS_WEAKREF_OBJECT(L))
    {
        // Have to handle these specially.
        (void)ScanAddressesInObject(obj, L);
        return 0; // Already done it.
    }
    else
        return L;
}

// The initial entry to process the roots.  Also used when processing the addresses
// in objects that can't be handled by ScanAddressAt.
PolyObject *ProcessMarkPointers::ScanObjectAddress(PolyObject *obj)
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

    // Process the addresses in this object.  We could short-circuit things
    // for word objects by calling ScanAddressesAt directly.
    ScanAddressesInObject(obj);

    return obj;
}


// These functions are only called with pointers held by the runtime system.
// Weak references can occur in the runtime system, eg. streams and windows.
// Weak references are not marked and so unreferenced streams and windows
// can be detected and closed.
void ProcessMarkPointers::ScanRuntimeAddress(PolyObject **pt, RtsStrength weak)
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
void ProcessMarkPointers::ScanAddressesInObject(PolyObject *base, POLYUNSIGNED L)
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

// Check for weak references that are no longer referenced.
class CheckWeakRef: public ScanAddress {
public:
    void ScanAreas(void);
private:
    virtual void ScanRuntimeAddress(PolyObject **pt, RtsStrength weak);
    // This has to be defined since it's virtual.
    virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }
    virtual void ScanAddressesInObject(PolyObject *obj, POLYUNSIGNED lengthWord);
};

// This deals with weak references within the run-time system.
void CheckWeakRef::ScanRuntimeAddress(PolyObject **pt, RtsStrength weak)
{
    /* If the object has not been marked and this is only a weak reference */
    /* then the pointer is set to zero. This allows streams or windows     */
    /* to be closed if there is no other reference to them.                */
    
    PolyObject *val = *pt;
    PolyWord w = val;
    
    CheckPointer (val);
    
    if (weak == STRENGTH_STRONG)
        return;

    LocalMemSpace *space = gMem.LocalSpaceForAddress(w.AsStackAddr());
    if (space == 0)
        return; // Not in local area
    if (! INRANGE(w.AsStackAddr(), space->gen_bottom, space->gen_top))
        return; // Not in area we're currently collecting.
    // If it hasn't been marked set it to zero.
    if (! space->bitmap.TestBit(BITNO(space, w.AsStackAddr())))
         *pt = 0;
}

// Deal with weak objects
void CheckWeakRef::ScanAddressesInObject(PolyObject *obj, POLYUNSIGNED L)
{
    if (! OBJ_IS_WEAKREF_OBJECT(L)) return;
    ASSERT(OBJ_IS_MUTABLE_OBJECT(L)); // Should be a mutable.
    ASSERT(OBJ_IS_WORD_OBJECT(L)); // Should be a plain object.
    // See if any of the SOME objects contain unreferenced refs.
    POLYUNSIGNED length = OBJ_OBJECT_LENGTH(L);
    PolyWord *baseAddr = (PolyWord*)obj;
    for (POLYUNSIGNED i = 0; i < length; i++)
    {
        PolyWord someAddr = baseAddr[i];
        if (someAddr.IsDataPtr())
        {
            LocalMemSpace *someSpace = gMem.LocalSpaceForAddress(someAddr.AsAddress());
            if (someSpace != 0 &&
                    INRANGE(someAddr.AsStackAddr(), someSpace->gen_bottom, someSpace->gen_top))
            {
                PolyObject *someObj = someAddr.AsObjPtr();
                // If this is a weak object the SOME value may refer to an unreferenced
                // ref.  If so we have to set this entry to NONE.  For safety we also
                // set the contents of the SOME to TAGGED(0).
                ASSERT(someObj->Length() == 1 && someObj->IsWordObject()); // Should be a SOME node.
                PolyWord refAddress = someObj->Get(0);
                LocalMemSpace *space = gMem.LocalSpaceForAddress(refAddress.AsAddress());
                if (space != 0 &&
                    INRANGE(refAddress.AsStackAddr(), space->gen_bottom, space->gen_top))
                    // If the ref is permanent it's always there.
                {
                    POLYUNSIGNED new_bitno = BITNO(space, refAddress.AsStackAddr());
                    if (! space->bitmap.TestBit(new_bitno))
                    {
                        // It wasn't marked so it's otherwise unreferenced.
                        baseAddr[i] = TAGGED(0); // Set it to NONE.
                        someObj->Set(0, TAGGED(0)); // For safety.
                        convertedWeak = true;
                    }
                }
            }
        }
    }
}

// We need to check any weak references both in the areas we are
// currently collecting and any other areas.  This actually checks
// weak refs in the area we're collecting even if they are not
// actually reachable any more.  N.B.  This differs from OpMutables
// because it also scans the area we're collecting.
void CheckWeakRef::ScanAreas(void)
{
    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        LocalMemSpace *space = gMem.lSpaces[i];
        if (space->isMutable)
            ScanAddressesInRegion(space->lowestWeak, space->highestWeak);
    }
    // Scan the permanent mutable areas.
    for (unsigned j = 0; j < gMem.npSpaces; j++)
    {
        MemSpace *space = gMem.pSpaces[j];
        if (space->isMutable)
            ScanAddressesInRegion(space->lowestWeak, space->highestWeak);
    }
}

class ProcessUpdate: public ScanAddress
{
public:
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);
    virtual void ScanRuntimeAddress(PolyObject **pt, RtsStrength weak);
    virtual PolyObject *ScanObjectAddress(PolyObject *base);

    void UpdateObjectsInArea(LocalMemSpace *area);
};

/*********************************************************************/
/* This function is called in the update phase to update pointers to */
/* objects in the gc area that are in old mutable segments.          */
/*********************************************************************/
PolyObject *ProcessUpdate::ScanObjectAddress(PolyObject *obj)
{
    PolyWord val = obj;

    LocalMemSpace *space = gMem.LocalSpaceForAddress(val.AsStackAddr());
    if (space != 0)
    {
        if (obj->ContainsForwardingPtr())
            obj = obj->GetForwardingPtr();
        else ASSERT(obj->ContainsNormalLengthWord());
    
        CheckObject (obj);
    }
    return obj;
}

void ProcessUpdate::ScanRuntimeAddress(PolyObject **pt, RtsStrength/* weak*/)
/* weak is not used, but needed so type of the function is correct */
{
    PolyWord w = *pt;
    LocalMemSpace *space = gMem.LocalSpaceForAddress(w.AsStackAddr());
    if (space != 0)
    {
        PolyObject *obj = *pt;
        
        if (obj->ContainsForwardingPtr())
            *pt = obj->GetForwardingPtr();
        else ASSERT(obj->ContainsNormalLengthWord()); /* SPF 24/1/95 */
        
        CheckObject (*pt);
    }
}  

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
class ProcessIdentity: public ScanAddress {
public:
   virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }
};

static void CopyObjectsInArea(LocalMemSpace *src, bool compressImmutables)
{
    /* Start scanning the bitmap from the very bottom since it is    */
    /* likely that very recently created objects need copying.       */
    /* Skip whole words of zeroes since these may be quite common if */
    /* the objects to be copied are sparsely separated.              */
    
    /* Invariant: at this point there are no objects below src->gen_bottom */
    POLYUNSIGNED  bitno   = BITNO(src,src->gen_bottom);
    POLYUNSIGNED  highest = src->highest;
//    Bitmap *bitmap  = &src->bitmap;
    
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
                ProcessIdentity identity;
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

// Update the addresses in a group of words.
POLYUNSIGNED ProcessUpdate::ScanAddressAt(PolyWord *pt)
{
    PolyWord val = *pt;
    Check (val);

    if (val.IsTagged())
        return 0;

    // It looked like it would be possible to simplify this code and
    // just call ContainsForwardingPtr on any address.
    // Profiling shows that it's quite important to avoid calling
    // ContainsForwardingPtr unnecessarily. I guess the reason is that
    // it actually accesses the memory referenced by the address and it
    // is unlikely to be in the cache.

    LocalMemSpace *space = gMem.LocalSpaceForAddress(val.AsStackAddr());
    if (space == 0)
        return 0;

    if (! INRANGE(val.AsStackAddr(), space->gen_bottom, space->gen_top))
        return 0;

    PolyObject *obj = val.AsObjPtr();
    
    if (obj->ContainsForwardingPtr())
    {
        *pt = obj->GetForwardingPtr();
        CheckObject (pt->AsObjPtr());
    }
    else
    {
        ASSERT(obj->ContainsNormalLengthWord());
        CheckObject(obj);
    }

    return 0;
}

// Updates the addresses for objects in the area with the "allocated" bit set.
// It processes the area between area->pointer and the bit corresponding to area->highest.
void ProcessUpdate::UpdateObjectsInArea(LocalMemSpace *area)
{
    PolyWord *pt      = area->pointer;
    POLYUNSIGNED   bitno   = BITNO(area, pt);
    POLYUNSIGNED   highest = area->highest;
    
    for (;;)
    {
        ASSERT(bitno <= highest); /* SPF */
        
       /* Zero freed space. This is necessary for OpMutableBlock,
          which expects the old mutable area to contain only
          genuine objects, tombstones and zero words. This is
          all rather sad, since zeroing the mutable buffer in
          this manner may well be one of the hot-spots of the GC.
          At least we only start at area->pointer, so we shouldn't
          normally have to zap *too* much store.
          SPF 22/10/96
        */
        /*
          The alternative, of making these dummy byte objects in which
          case it is only the length word that needs to be set, didn't
          seem to make any difference.  The CPU is probably writing back
          whole cache lines so setting the length word probably means
          the whole cache line has to be written anyway.  DCJM 2/6/06.
        */
        while (bitno < highest && !area->bitmap.TestBit(bitno))
        {
            *pt++ = PolyWord::FromUnsigned(0);
            bitno++;
        }
        
        if (bitno == highest)
            return;
        
        /* first set bit corresponds to the length word */
        pt++;
        PolyObject *obj = (PolyObject*)pt;
        POLYUNSIGNED L = obj->LengthWord();
        bitno++;
        
        if (obj->ContainsForwardingPtr())    /* skip over moved object */
        {
            obj = obj->GetForwardingPtr();
            CheckObject (obj);
            
            POLYUNSIGNED length = obj->Length();
            pt    += length;
            bitno += length;
        }
        else /* !OBJ_IS_POINTER(L) */
        {
            CheckObject (obj);
            
            if (OBJ_IS_WORD_OBJECT(L))
            {
                POLYUNSIGNED length = OBJ_OBJECT_LENGTH(L);
                
                area->updated += length+1;
                
                while (length--)
                {
                    PolyWord val = *pt;
                    Check (val);

                    if (! val.IsTagged() && val != PolyWord::FromUnsigned(0))
                    {
                        LocalMemSpace *space = gMem.LocalSpaceForAddress(val.AsAddress());
                        if (space != 0 &&
                              INRANGE(val.AsStackAddr(), space->gen_bottom, space->gen_top))
                        {
                            PolyObject *obj = val.AsObjPtr();
                        
                            if (obj->ContainsForwardingPtr())
                            {
                                *pt = obj->GetForwardingPtr();
                                CheckObject (pt->AsObjPtr());
                            }
                            else
                            {
                                ASSERT(obj->ContainsNormalLengthWord());
                                CheckObject(obj);
                            }
                        }
                    }
                    
                    pt++;
                    bitno++;
                }
            }
            
            else /* !OBJ_IS_WORD_OBJECT(L) */
            {
                POLYUNSIGNED length = OBJ_OBJECT_LENGTH(L);
                area->updated += length+1;
                ScanAddressesInObject(obj, L);
                pt    += length;
                bitno += length;
            } /* !OBJ_IS_WORD_OBJECT(L) */
        }  /* !OBJ_IS_POINTER(L) */
    } /* for loop */
}

#define GC_START   1
#define GC_NEWLINE 2
#define GC_FULL    4

// Try to allocate another heap segment.  It tries to allocate the requested size
// but if that fails it allocates what it can.
static bool TryMoreHeap(POLYUNSIGNED size, bool mut)
{
    if (userOptions.debug & DEBUG_NOGROW) return false; // No heap growing.

    do {
        // Return if this succeeded.
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

// This function is called after the mark phase of a full garbage collection to
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
        unsigned spaceFactor = nISpaces / 3;
        while (spaceFactor > 0) { requestedGrowth += immutableSegSize; spaceFactor--; }

        POLYUNSIGNED chunks  = ROUNDUP_UNITS(requestedGrowth, BITSPERWORD);
        POLYUNSIGNED words   = chunks * BITSPERWORD;

        (void)TryMoreHeap(words, false); // If this fails just carry on with what we have.
    }
}


/* This function CHECKS whether we have enough space AFTER the compaction phase. */
static bool BufferIsReallyFull(bool mutableRegion, POLYUNSIGNED wordsNeeded, const bool fullGC)
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
        requiredFree += wordsNeeded;
        wordsNeeded = 0;
    }

    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        LocalMemSpace *space = gMem.lSpaces[i];
        if (space->isMutable == mutableRegion)
        {
            POLYUNSIGNED currentlyFree = space->pointer - space->bottom;
            if (currentlyFree >= wordsNeeded)
            {
                currentlyFree -= wordsNeeded;
                wordsNeeded = 0;
            }
            if (currentlyFree >= requiredFree)
                requiredFree = 0;
            else requiredFree -= currentlyFree;
        }
    }
    return wordsNeeded != 0 || requiredFree != 0;
}

// AFTER a full GC, make sure we have a full buffer's worth of free space available.
static void AdjustHeapSize(bool isMutableSpace, POLYUNSIGNED wordsRequired)
{
    POLYUNSIGNED currentSize = 0, currentlyFree = 0;
    unsigned nSpaces = 0;
    POLYUNSIGNED largestFree = 0;
    for (unsigned j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[j];
        if (space->isMutable == isMutableSpace)
        {
            POLYUNSIGNED spaceSize = space->top - space->bottom;
            POLYUNSIGNED spaceFree = space->pointer - space->bottom;
            currentSize += spaceSize;
            currentlyFree += spaceFree;
            if (largestFree < spaceFree) largestFree = spaceFree;
            nSpaces++;
        }
    }
    
    const POLYUNSIGNED requiredFree = wordsRequired +
        (isMutableSpace ? mutableFreeSpace : immutableFreeSpace);
    
    /* Basic sanity checks. */
    ASSERT(0 <= wordsRequired);
    ASSERT(0 <= requiredFree);
    
    // We may be trying to allocate a very large object, e.g. a new stack segment, in
    // which case we must ensure that we have enough space in at least one space.
    // Otherwise we just check we have enough free overall.
    if (requiredFree > currentlyFree || (isMutableSpace && largestFree < wordsRequired))
    {    // expand the heap.
        POLYUNSIGNED requestedGrowth = requiredFree - currentlyFree;
        const POLYUNSIGNED segSize =
            isMutableSpace ? mutableSegSize : immutableSegSize;
        if (requestedGrowth < segSize)
            requestedGrowth = segSize;
        // Make the segments larger if we have already allocated several.
        // The factors here are a guess.  Maybe tune them more carefully
        unsigned spaceFactor = nSpaces / 3;
        while (spaceFactor > 0) { requestedGrowth += segSize; spaceFactor--; }
        if (requestedGrowth < wordsRequired) requestedGrowth = wordsRequired;

        POLYUNSIGNED chunks  = ROUNDUP_UNITS(requestedGrowth, BITSPERWORD);
        POLYUNSIGNED words   = chunks * BITSPERWORD;

        (void)TryMoreHeap(words, isMutableSpace); // If this fails just carry on with what we have.
    }
    else // currentlyFree >= requiredFree
    {
        // The reason for shrinking the stack is to reduce the swap space and
        // possibly the address space requirements.  This may be necessary if
        // we have finished building a large data structure and now want to
        // export it.  The export code requires buffer space and may need the
        // space we're using.
        // Another reason is to get rid of old saved state areas that have been
        // converted into local areas.  These are likely to be small compared with the
        // heap and result in fragmentation of the address space.
        // TODO: We should perhaps deallocate small areas even if that would bring
        // us under the limit because it would be better to reallocate a larger area.
        POLYUNSIGNED requestedShrink = currentlyFree - requiredFree;
        // Delete the most recent space first.
        for (unsigned k = gMem.nlSpaces; k > 0; k--)
        {
            LocalMemSpace *space = gMem.lSpaces[k-1];
            if (space->isMutable == isMutableSpace &&
                space->pointer == space->top /* It's completely empty */ &&
                (POLYUNSIGNED)(space->top - space->bottom) <= requestedShrink)
            {
                // We can free this space without going under our limit
                requestedShrink -= space->top - space->bottom;
                gMem.DeleteLocalSpace(space);
            }
        }
    }
}


void OpGCProcs (ScanAddress *process)
{
    GCModules(process);
}

static int RecollectThisGeneration(unsigned thisGeneration)
{
    if (thisGeneration > 3)
        return false;

    POLYUNSIGNED total = 0, updated = 0;
    for(unsigned j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        total += lSpace->gen_top - lSpace->pointer;
        updated += lSpace->updated;
    }
    if (total == 0)
        return false;
    /* I think the idea here is that if we have a significant number of
       objects in the current generation which have not actually been
       visited to have their addresses updated we should not merge
       this generation with the old data and treat them as "old" but
       instead treat them as "new".
       If we have allocated a large object in the mutable area we
       may not have a gap big enough to move it to.  We may though
       have created enough space in this minor GC to move it next time.
       That's because if we have moved an object we can't use the space
       until after the update phase has finished with the tombstone.
       DCJM 27/6/09. */
    return updated * 2 < total; // Less than 50% updated
}

static bool doGC(bool doFullGC, const POLYUNSIGNED wordsRequiredToAllocate)
{
    /* Invariant: the bitmaps are completely clean. */
    /* Note: this version of doGC does NOT clean the store 
    - that's now the user's resposibility SPF 22/10/96
    */
    unsigned j;
    POLYUNSIGNED gcflags = GC_START;
    static bool doFullGCNextTime = 0;
    static unsigned this_generation = 0;
    
    record_gc_time(false);

GC_AGAIN:
    /* Invariant: the bitmaps are completely clean. */
    
    /* At this point, we should have
       lSpace->bottom <= lSpace->pointer <= lSpace->gen_top <= lSpace->top       
    
       lSpace->gen_top divides the current generation from the old one.
       lSpace->pointer is the current allocation pointer.
    */

    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        ASSERT (lSpace->top     >= lSpace->gen_top);
        ASSERT (lSpace->gen_top >= lSpace->pointer);
        ASSERT (lSpace->pointer >= lSpace->bottom);
        // Record low-water mark before we change anything.
        // gen_bottom is the lowest object actually allocated in the
        // area.
        lSpace->gen_bottom = lSpace->pointer;
        // Set upper and lower limits of weak refs.
        lSpace->highestWeak = lSpace->bottom;
        lSpace->lowestWeak = lSpace->top;
    }

    // Set limits of weak refs.
    for (j = 0; j < gMem.npSpaces; j++)
    {
        PermanentMemSpace *pSpace = gMem.pSpaces[j];
        pSpace->highestWeak = pSpace->bottom;
        pSpace->lowestWeak = pSpace->top;
    }        
    
    
    /* Our recovery actions may insist on a full GC */
    if (doFullGCNextTime)
    {
        doFullGC = true;
        doFullGCNextTime = false;
    }
    
    /* Mark phase */
    mainThreadPhase = MTP_GCPHASEMARK;
    
    gcflags |= GC_NEWLINE;
    
    if (doFullGC)
    {
        gcflags |= GC_FULL;
        
        /* Collect everything */
        for(j = 0; j < gMem.nlSpaces; j++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j];
            lSpace->gen_top = lSpace->top;
        }
    }
    
    gcflags &= ~GC_START;
    gcflags &= ~GC_NEWLINE;
        
    /* Bitmaps are allocated in InitialiseGC and are zeroed
       at the END of each GC, because that way we know how much
       of each bitmap (not all!) we need to touch.
       SPF 3/10/96 */
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        lSpace->i_marked = lSpace->m_marked = 0;
    }
    
    /* Do the actual marking */
    ProcessMarkPointers marker;
    OpMutables(&marker);
    OpGCProcs(&marker);
    /* Invariant: at most the first (gen_top - bottom) bits of the each bitmap can be dirty here. */
    
    // Mutable areas can contain mutable or immutable objects.  Immutable areas
    // should only contain immutable objects.  Verify this.
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        if (! lSpace->isMutable) ASSERT(lSpace->m_marked == 0);
    }
    
    /* Compact phase */
    mainThreadPhase = MTP_GCPHASECOMPACT;
    
    /* Detect unreferenced streams, windows etc. */
    CheckWeakRef checkRef;
    OpGCProcs(&checkRef);
    checkRef.ScanAreas();

    if (convertedWeak)
        // Notify the signal thread to broadcast on the condition var when
        // the GC is complete.
        processes->SignalArrived();
    
    /* If we are doing a full GC we expand the immutable area now, so that there's
       enough room to copy the immutables that are currently in the mutable buffer.
       There's no point expanding the mutable buffer now - we'll do that later 
       when we know *exactly* how large we want it to be. */ 
    if (doFullGC) 
    {
        POLYUNSIGNED immutableData = 0;
        for(j = 0; j < gMem.nlSpaces; j++)
            immutableData += gMem.lSpaces[j]->i_marked;
        PossiblyExpandImmutableArea(immutableData);
    }

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
    POLYUNSIGNED immutable_overflow = 0; // The immutable space we couldn't copy out.
    // I think immutable overflow was a problem in the old version of the GC with only
    // a single segment.  It ought to be possible to change this so it doesn't happen now.
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

    POLYUNSIGNED mCopied = 0, iCopied = 0, iMarked = 0;
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        if (lSpace->isMutable)
            mCopied += lSpace->copied;
        else
        {
            iMarked += lSpace->i_marked;
            iCopied += lSpace->copied;
        }
    }
    ASSERT(mCopied == 0);
    ASSERT(iCopied <= iMarked);

    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        ASSERT(INSOFTRANGE(lSpace->pointer, lSpace->bottom, lSpace->gen_top));
    }    
    
    /* Update phase */
    mainThreadPhase = MTP_GCPHASEUPDATE;
    
    /* Invariant: at most the first (gen_top - bottom) bits of each bitmap can be dirty here. */
    for(j = 0; j < gMem.nlSpaces; j++)
        gMem.lSpaces[j]->updated = 0;
       
    ProcessUpdate processUpdate;
    OpMutables(&processUpdate);

    for(j = 0; j < gMem.nlSpaces; j++)
        processUpdate.UpdateObjectsInArea(gMem.lSpaces[j]);

    OpGCProcs(&processUpdate);

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

    /* Invariant: at most the first (gen_top - bottom) bits of the each bitmap can be dirty. */
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[j];
        lSpace->bitmap.ClearBits(0, lSpace->gen_top - lSpace->bottom);
    }
    /* Invariant: the bitmaps are completely clean */

    if (doFullGC)
    {
        /* If we've had an immutable overflow, allow for this when we grow the heap */
        AdjustHeapSize(false /* immutable space*/, immutable_overflow);
        bool iFull = BufferIsReallyFull(false /* immutable area */, immutable_overflow, doFullGC);
        bool mFull = BufferIsReallyFull(true /* mutable area */, wordsRequiredToAllocate, doFullGC);
        
        /* If we're going to recollect the current generation, don't adjust the mutable buffer size yet. */
        /* We'll (probably) do that on the next collection. SPF 22/12/1997 */
        if (iFull || ! mFull || ! RecollectThisGeneration(this_generation))
            AdjustHeapSize(true /* mutable space */, wordsRequiredToAllocate);
    }

    CheckMemory();
    
    /* Have we cleared enough space? */
    {
        bool iFull = BufferIsReallyFull(false /* immutable area */, immutable_overflow, doFullGC);
        bool mFull = BufferIsReallyFull(true /* mutable area */, wordsRequiredToAllocate, doFullGC);
        
        if (iFull || mFull)
        {
            /* Recovery actions */
            if (!iFull && RecollectThisGeneration(this_generation)) /* Needs tuning!!! */
            {
                /* The next GC will re-collect THIS generation, which should be
                   enough to recover properly. */
            }
            else if (! doFullGC) // Do a full GC next time
                doFullGCNextTime = true;
            else // It was a full GC but we don't have as much free space as we normally
                 // want at the end of a full GC.  Do we have as much as we would want at the
                 // end of a partial GC?
            if (BufferIsReallyFull(false /* immutable area */, 0, false) ||
                BufferIsReallyFull(true /* mutable area */, wordsRequiredToAllocate, false))
            {
                // No we don't even have that - interrupt console processes and end GC here.
                record_gc_time(true);
                return false;
            }
        }
    }
    

    if (RecollectThisGeneration(this_generation))
    {
        /* Generally we treat all the objects we have left after this GC as "old" for
           the purposes of subsequent minor GCs.  If, though, a collection has left us
           with significant gaps we don't do that merge and instead on the next GC we
           recollect everything since the last collection. */
        /* If this was a full GC, make sure the next one is too, as we may
           need to reconfigure the mutable buffer size. If we only did a
           partial next, we would still have to mark all the immutables again
           (they would still be new) which is the main cost of a full GC.
            */
        doFullGCNextTime |= doFullGC;
        this_generation++;
    }
    else
    {
        /* Merge this generation with the old one */
        for(j = 0; j < gMem.nlSpaces; j++)
        {
            LocalMemSpace *lSpace = gMem.lSpaces[j];
            lSpace->gen_top = lSpace->pointer;
        }
        this_generation = 0;
    }
    
    // Do we have enough space for the original allocation request?
    bool haveSpace = false;
    for(j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[j];
        if (space->isMutable)
        {
            if ((POLYUNSIGNED)(space->pointer - space->bottom) >= wordsRequiredToAllocate)
            {
                haveSpace = true;
                break;
            }
        }
    }
    if (! haveSpace)
        /* Try our recovery action immediately */
        goto GC_AGAIN;
    
    /* If the heap is very close to what we can handle on this machine,
       do the full GC immediately, because if we wait, we'll generate
       more data in the mutable buffer which will make the thrashing caused
       by the inevitable full GC even worse. SPF 2/3/1998  */
    if (doFullGCNextTime)
    {
        POLYUNSIGNED memSize = GetPhysicalMemorySize();
        // Ignore this if we can't determine.or if we have more memory than the address space.
        if (memSize != 0 && memSize+1 != 0)
        {
            POLYUNSIGNED memWords = memSize/sizeof(PolyWord);
            POLYUNSIGNED spaceUsed = 0;
            unsigned i;
            for (i = 0; i < gMem.npSpaces; i++)
            {
                MemSpace *space = gMem.pSpaces[i];
                spaceUsed += space->top - space->bottom;
            }
            for (i = 0; i < gMem.nlSpaces; i++)
            {
                LocalMemSpace *space = gMem.lSpaces[i];
                // For mutable segments include all the space since
                // that's going to be used for allocation.  For immutable
                // spaces include only the area currently in use
                if (space->isMutable)
                    spaceUsed += space->top - space->bottom;
                else
                    spaceUsed += space->top - space->pointer;
            }
        
            // This crude estimate leaves out C heap, space for executable etc.
            // We used to include the bitmaps here as well.  Since that's a fixed percentage of
            // the sizes it could easily be taken account of by reducing the percentage of real
            // pages that cause a full collection.
            POLYUNSIGNED heapLoad;
            if (memWords < 100) heapLoad = 100;
            else heapLoad = spaceUsed / (memWords/100);
            // If we're more than 80% full.
            if (heapLoad > 80)
                goto GC_AGAIN;
        }
    }

    /* End of garbage collection */
    record_gc_time(true);
    
    /* Invariant: the bitmaps are completely clean */
    return true; // Completed
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


static POLYUNSIGNED GetPhysicalMemorySize(void)
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
void CreateHeap(unsigned hsize, unsigned isize, unsigned msize)
{
    // If no -H option was given set the default initial size to half the memory.
    if (hsize == 0) {
        POLYUNSIGNED memsize = GetPhysicalMemorySize();
        if (memsize == 0) // Unable to determine memory size so default to 64M.
            memsize = 64 * 1024 * 1024;
        hsize = memsize / 2 / 1024;
    }
    
    if (hsize < isize) hsize = isize;
    if (hsize < msize) hsize = msize;
    
    if (msize == 0) msize = 4 * 1024 + hsize / 5;  /* set default mutable buffer size */
    if (isize == 0) isize = hsize - msize;  /* set default immutable buffer size */
    
    // Set the heap size and segment sizes.  We allocate in units of this size,
    heapSize           = K_to_words(hsize);
    immutableSegSize   = K_to_words(isize);
    mutableSegSize     = K_to_words(msize);

    // Try allocating the space.  If it fails try something smaller.
    LocalMemSpace *iSpace = 0, *mSpace = 0;

    while (iSpace == 0 || mSpace == 0) {
        if (iSpace != 0) { gMem.DeleteLocalSpace(iSpace); iSpace = 0; }
        if (mSpace != 0) { gMem.DeleteLocalSpace(mSpace); mSpace = 0; }

        // Immutable space
        POLYUNSIGNED immutSize = ROUNDDOWN(immutableSegSize, BITSPERWORD);
        iSpace = gMem.NewLocalSpace(immutSize, false);
        // Mutable space
        POLYUNSIGNED mutSize = ROUNDDOWN(mutableSegSize, BITSPERWORD);
        mSpace = gMem.NewLocalSpace(mutSize, true);

        if (iSpace == 0 || mSpace == 0)
        {
            if (immutableSegSize < 1024 || mutableSegSize < 512) {
                // Too small to be able to run.
                Exit("Insufficient memory to allocate the heap");
            }
            // Make both spaces smaller.  It may be that there's space for one but not both.
            immutableSegSize = immutableSegSize/2;
            mutableSegSize = mutableSegSize/2;
        }
    }
    // Heap allocation has succeeded.

    // The space we need to have free at the end of a partial collection.  If we have less
    // than this we do a full GC.
    // For an immutable area this is zero.  For the mutable area, though, this is 80% of the
    // mutable segment size since we allocate new objects in the mutable area and this
    // determines how soon we will need to do another GC.
    immutableMinFree = 0;
    mutableMinFree = mutableSegSize - mutableSegSize / 5;

    // This is the space we try to have free at the end of a major collection.  If
    // we have less than this we allocate another segment.
    immutableFreeSpace = immutableSegSize/2; // 50% full
    if (immutableFreeSpace < immutableMinFree)
        immutableFreeSpace = immutableMinFree;
    // For the mutable area it is 90% of the segment size.
    mutableFreeSpace   = mutableSegSize - mutableSegSize/10;
    if (mutableFreeSpace < mutableMinFree)
        mutableFreeSpace = mutableMinFree;
}

class FullGCRequest: public MainThreadRequest
{
public:
    FullGCRequest(): MainThreadRequest(MTP_GCPHASEMARK) {}
    virtual void Perform() { doGC (true,0); }
};

class QuickGCRequest: public MainThreadRequest
{
public:
    QuickGCRequest(POLYUNSIGNED words): MainThreadRequest(MTP_GCPHASEMARK), wordsRequired(words) {}

    virtual void Perform() { result = doGC (false, wordsRequired); }
    bool result;
    POLYUNSIGNED wordsRequired;
};

// Perform a full garbage collection.  This is called either from ML via the full_gc RTS call
// or from various RTS functions such as open_file to try to recover dropped file handles.
void FullGC(TaskData *taskData)
{
    FullGCRequest request;
    processes->MakeRootRequest(taskData, &request);
}

// This is the normal call when memory is exhausted and we need to garbage collect.
bool QuickGC(TaskData *taskData, POLYUNSIGNED wordsRequiredToAllocate)
{
    QuickGCRequest request(wordsRequiredToAllocate);
    processes->MakeRootRequest(taskData, &request);
    return request.result;
}

