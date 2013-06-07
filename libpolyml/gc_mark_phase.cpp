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

The code ensures that each reachable cell is marked at least once but with
multiple threads a cell may be marked by more than once cell if the
memory is not fully up to date.  Each thread has a stack on which it
remembers cells that have been marked but not fully scanned.  If a
thread runs out of cells of its own to scan it can pick a pointer off
the stack of another thread and scan that.  The original thread will
still scan it some time later but it should find that the addresses
in it have all been marked and it can simply pop this off.  This is
all done without locking.  Stacks are only modified by the owning
thread and when they pop anything they write zero in its place.
Other threads only need to search for a zero to find if they are
at the top and if they get a pointer that has already been scanned
then this is safe.  The only assumption made about the memory is
that all the bits of a word are updated together so that a thread
will always read a value that is a valid pointer.

Many of the ideas are drawn from Flood, Detlefs, Shavit and Zhang 2001
"Parallel Garbage Collection for Shared Memory Multiprocessors".
*/
#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
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
#include "heapsizing.h"

#define MARK_STACK_SIZE 3000
#define LARGECACHE_SIZE 20

class MTGCProcessMarkPointers: public ScanAddress
{
public:
    MTGCProcessMarkPointers();

    virtual void ScanRuntimeAddress(PolyObject **pt, RtsStrength weak);
    virtual PolyObject *ScanObjectAddress(PolyObject *base);

    virtual void ScanAddressesInObject(PolyObject *base, POLYUNSIGNED lengthWord);
    // Have to redefine this for some reason.
    void ScanAddressesInObject(PolyObject *base)
        { MTGCProcessMarkPointers::ScanAddressesInObject(base, base->LengthWord()); }

    virtual void ScanConstant(byte *addressOfConstant, ScanRelocationKind code);

    static void MarkPointersTask(GCTaskId *, void *arg1, void *arg2);

    static void InitStatics(unsigned threads)
    {
        markStacks = new MTGCProcessMarkPointers[threads];
        nInUse = 0;
        nThreads = threads;
    }

    static void MarkRoots(void);

private:
    bool TestForScan(PolyWord *pt);
    void MarkAndTestForScan(PolyWord *pt);

    void PushToStack(PolyObject *obj, PolyWord *currentPtr = 0)
    {
        // If we don't have all the threads running we start a new one but
        // only once we have several items on the stack.  Otherwise we
        // can end up creating a task that terminates almost immediately.
        if (nInUse >= nThreads || msp < 2 || ! ForkNew(obj))
        {
            if (msp < MARK_STACK_SIZE)
            {
                markStack[msp++] = obj;
                if (currentPtr != 0)
                {
                    locPtr++;
                    if (locPtr == LARGECACHE_SIZE) locPtr = 0;
                    largeObjectCache[locPtr].base = obj;
                    largeObjectCache[locPtr].current = currentPtr;
                }
            }
            else StackOverflow(obj);
        }
        // else the new task is processing it.
    }

    static void StackOverflow(PolyObject *obj);
    bool ForkNew(PolyObject *obj);    

    PolyObject *markStack[MARK_STACK_SIZE];
    unsigned msp;
    bool active;

    // For the typical small cell it's easier just to rescan from the start
    // but that can be expensive for large cells.  This caches the offset for
    // large cells.
    static const POLYUNSIGNED largeObjectSize = 50;
    struct { PolyObject *base; PolyWord *current; } largeObjectCache[LARGECACHE_SIZE];
    unsigned locPtr;

    static MTGCProcessMarkPointers *markStacks;
protected:
    static unsigned nThreads, nInUse;
    static PLock stackLock;
};

MTGCProcessMarkPointers *MTGCProcessMarkPointers::markStacks;
unsigned MTGCProcessMarkPointers::nThreads, MTGCProcessMarkPointers::nInUse;
PLock MTGCProcessMarkPointers::stackLock("GC mark stack");

// It is possible to have two levels of forwarding because
// we could have a cell in the allocation area that has been moved
// to the immutable area and then shared with another cell.
inline PolyObject *FollowForwarding(PolyObject *obj)
{
    while (obj->ContainsForwardingPtr())
        obj = obj->GetForwardingPtr();
    return obj;
}

MTGCProcessMarkPointers::MTGCProcessMarkPointers(): msp(0), active(false), locPtr(0)
{
    // Clear the mark stack
    for (unsigned i = 0; i < MARK_STACK_SIZE; i++)
        markStack[i] = 0;
}

// Called when the stack has overflowed.  We need to include this
// in the range to be rescanned.
void MTGCProcessMarkPointers::StackOverflow(PolyObject *obj)
{
    LocalMemSpace *space = gMem.LocalSpaceForAddress(obj);
    ASSERT(space != 0);
    PLocker lock(&space->spaceLock);
    // Have to include this in the range to rescan.
    if (space->fullGCRescanStart > ((PolyWord*)obj) - 1)
        space->fullGCRescanStart = ((PolyWord*)obj) - 1;
    POLYUNSIGNED n = obj->Length();
    if (space->fullGCRescanEnd < ((PolyWord*)obj) + n)
        space->fullGCRescanEnd = ((PolyWord*)obj) + n;
    ASSERT(obj->LengthWord() & _OBJ_GC_MARK); // Should have been marked.
    if (debugOptions & DEBUG_GC)
        Log("GC: Mark: Stack overflow.  Rescan for %p\n", obj);
}

// Fork a new task.  Because we've checked nInUse without taking the lock
// we may find that we can no longer create a new task.
bool MTGCProcessMarkPointers::ForkNew(PolyObject *obj)
{
    MTGCProcessMarkPointers *marker = 0;
    {
        PLocker lock(&stackLock);
        if (nInUse == nThreads)
            return false;
        for (unsigned i = 0; i < nThreads; i++)
        {
            if (! markStacks[i].active)
            {
                marker = &markStacks[i];
                break;
            }
        }
        ASSERT(marker != 0);
        marker->active = true;
        nInUse++;
    }
    bool test = gpTaskFarm->AddWork(&MTGCProcessMarkPointers::MarkPointersTask, marker, obj);
    ASSERT(test);
    return true;
}

// Main marking task.  This is forked off initially to scan a specific object and
// anything reachable from it but once that has finished it tries to find objects
// on other stacks to scan.
void MTGCProcessMarkPointers::MarkPointersTask(GCTaskId *, void *arg1, void *arg2)
{
    MTGCProcessMarkPointers *marker = (MTGCProcessMarkPointers*)arg1;
    marker->locPtr = 0;
    marker->largeObjectCache[0].base = 0;
    marker->ScanAddressesInObject((PolyObject*)arg2);

    while (true)
    {
        // Look for a stack that has at least one item on it
        MTGCProcessMarkPointers *steal = 0;
        for (unsigned i = 0; i < nThreads && steal == 0; i++)
        {
            if (markStacks[i].markStack[0] != 0)
                steal = &markStacks[i];
        }
        // We're finished if they're all done.
        if (steal == 0)
            break;
        // Look for items on this stack
        for (unsigned j = 0; j < MARK_STACK_SIZE; j++)
        {
            // Pick the item off the stack.
            // N.B. The owning thread may update this to zero
            // at any time.
            PolyObject *toSteal = steal->markStack[j];
            if (toSteal == 0) break; // Nothing more on the stack
            marker->ScanAddressesInObject(toSteal);
        }
    }

    PLocker lock(&stackLock);
    marker->active = false; // It's finished
    nInUse--;
    ASSERT(marker->markStack[0] == 0);
}

// Tests if this needs to be scanned.  It marks it if it has not been marked
// unless it has to be scanned.
bool MTGCProcessMarkPointers::TestForScan(PolyWord *pt)
{
    if ((*pt).IsTagged())
        return false;

    // This could contain a forwarding pointer if it points into an
    // allocation area and has been moved by the minor GC.
    // We have to be a little careful.  Another thread could also
    // be following any forwarding pointers here.  However it's safe
    // because they will update it with the same value.
    PolyObject *obj = (*pt).AsObjPtr();
    if (obj->ContainsForwardingPtr())
    {
        obj = FollowForwarding(obj);
        *pt = obj;
    }

    if (gMem.LocalSpaceForAddress(obj) == 0)
        return false; // Ignore it if it points to a permanent area

    POLYUNSIGNED L = obj->LengthWord();
    if (L & _OBJ_GC_MARK)
        return false; // Already marked

    if (debugOptions & DEBUG_GC_DETAIL)
        Log("GC: Mark: %p %" POLYUFMT " %u\n", obj, OBJ_OBJECT_LENGTH(L), GetTypeBits(L));

    if (OBJ_IS_BYTE_OBJECT(L))
    {
        obj->SetLengthWord(L | _OBJ_GC_MARK); // Mark it
        return false; // We've done as much as we need
    }
    return true;
}

void MTGCProcessMarkPointers::MarkAndTestForScan(PolyWord *pt)
{
    if (TestForScan(pt))
    {
        PolyObject *obj = (*pt).AsObjPtr();
        obj->SetLengthWord(obj->LengthWord() | _OBJ_GC_MARK);
    }
}

// The initial entry to process the roots.  These may be RTS addresses or addresses in
// a thread stack.  Also called recursively to process the addresses of constants in
// code segments.  This is used in situations where a scanner may return the
// updated address of an object.
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
        obj = FollowForwarding(obj);
        val = obj;
        space = gMem.LocalSpaceForAddress(val.AsAddress());
    }

    ASSERT(obj->ContainsNormalLengthWord());

    POLYUNSIGNED L = obj->LengthWord();
    if (L & _OBJ_GC_MARK)
        return obj; // Already marked
    obj->SetLengthWord(L | _OBJ_GC_MARK); // Mark it

    if (profileMode == kProfileLiveData || (profileMode == kProfileLiveMutables && obj->IsMutable()))
        AddObjectProfile(obj);

    POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);
    if (debugOptions & DEBUG_GC_DETAIL)
        Log("GC: Mark: %p %" POLYUFMT " %u\n", obj, n, GetTypeBits(L));

    if (OBJ_IS_BYTE_OBJECT(L))
        return obj;

    // If we already have something on the stack we must being called
    // recursively to process a constant in a code segment.  Just push
    // it on the stack and let the caller deal with it.
    if (msp != 0)
        PushToStack(obj); // Can't check this because it may have forwarding ptrs.
    else
    {
        MTGCProcessMarkPointers::ScanAddressesInObject(obj, L);

        // We can only check after we've processed it because if we
        // have addresses left over from an incomplete partial GC they
        // may need to forwarded.
        CheckObject (obj);
    }

    return obj;
}

// These functions are only called with pointers held by the runtime system.
// Weak references can occur in the runtime system, eg. streams and windows.
// Weak references are not marked and so unreferenced streams and windows
// can be detected and closed.
void MTGCProcessMarkPointers::ScanRuntimeAddress(PolyObject **pt, RtsStrength weak)
{
    if (weak == STRENGTH_WEAK) return;
    *pt = ScanObjectAddress(*pt);
    CheckPointer (*pt); // Check it after any forwarding pointers have been followed.
}

// This is called via ScanAddressesInRegion to process the permanent mutables.  It is
// also called from ScanObjectAddress to process root addresses.
// It processes all the addresses reachable from the object.
void MTGCProcessMarkPointers::ScanAddressesInObject(PolyObject *obj, POLYUNSIGNED lengthWord)
{
    if (OBJ_IS_BYTE_OBJECT(lengthWord))
        return;

    while (true)
    {
        ASSERT (OBJ_IS_LENGTH(lengthWord));

        // Get the length and base address.  N.B.  If this is a code segment
        // these will be side-effected by GetConstSegmentForCode.
        POLYUNSIGNED length = OBJ_OBJECT_LENGTH(lengthWord);

        if (OBJ_IS_WEAKREF_OBJECT(lengthWord))
        {
            // Special case.  
            ASSERT(OBJ_IS_MUTABLE_OBJECT(lengthWord)); // Should be a mutable.
            ASSERT(OBJ_IS_WORD_OBJECT(lengthWord)); // Should be a plain object.
            // We need to mark the "SOME" values in this object but we don't mark
            // the references contained within the "SOME".
            PolyWord *baseAddr = (PolyWord*)obj;
            // Mark every word but ignore the result.
            for (POLYUNSIGNED i = 0; i < length; i++)
                (void)MarkAndTestForScan(baseAddr+i);
            // We've finished with this.
            length = 0;
        }

        else if (OBJ_IS_CODE_OBJECT(lengthWord))
        {
            // It's better to process the whole code object in one go.
            ScanAddress::ScanAddressesInObject(obj, lengthWord);
            length = 0; // Finished
        }

        // else it's a normal object,

        // If there are only two addresses in this cell that need to be
        // followed we follow them immediately and treat this cell as done.
        // If there are more than two we push the address of this cell on
        // the stack, follow the first address and then rescan it.  That way
        // list cells are processed once only but we don't overflow the
        // stack by pushing all the addresses in a very large vector.
        PolyWord *baseAddr = (PolyWord*)obj;
        PolyWord *endWord = baseAddr + length;
        PolyObject *firstWord = 0;
        PolyObject *secondWord = 0;
        PolyWord *restartAddr = 0;

        if (obj == largeObjectCache[locPtr].base)
        {
            baseAddr = largeObjectCache[locPtr].current;
            ASSERT(baseAddr > (PolyWord*)obj && baseAddr < ((PolyWord*)obj)+length);
            if (locPtr == 0) locPtr = LARGECACHE_SIZE-1; else locPtr--;
        }

        while (baseAddr != endWord)
        {
            PolyWord wordAt = *baseAddr;

            if (wordAt.IsDataPtr() && wordAt != PolyWord::FromUnsigned(0))
            {
                // Normal address.  We can have words of all zeros at least in the
                // situation where we have a partially constructed code segment where
                // the constants at the end of the code have not yet been filled in.
                if (TestForScan(baseAddr))
                {
                    if (firstWord == 0)
                        firstWord = baseAddr->AsObjPtr();
                    else if (secondWord == 0)
                    {
                        // If we need to rescan because there are three or more words to do
                        // this is the place we need to restart (or the start of the cell if it's
                        // small).
                        restartAddr = baseAddr;
                        secondWord = baseAddr->AsObjPtr();
                    }
                    else break;  // More than two words.
                }
            }
            else if (wordAt.IsCodePtr())
            {
                // If we're processing the constant area of a code segment this could
                // be a code address.
                // Check that this is actually an address.  If we have had a bad pointer
                // earlier we may treat some length fields as values.
                ASSERT(gMem.SpaceForAddress(wordAt.AsCodePtr()) != 0);
                PolyObject *oldObject = ObjCodePtrToPtr(wordAt.AsCodePtr());
                // Calculate the byte offset of this value within the code object.
                POLYUNSIGNED offset = wordAt.AsCodePtr() - (byte*)oldObject;
                wordAt = oldObject;
                bool test = TestForScan(&wordAt);
                // If we've changed it because we had a left-over forwarding pointer
                // we need to update the original.
                PolyObject *newObject = wordAt.AsObjPtr();
                wordAt = PolyWord::FromCodePtr((byte*)newObject + offset);
                if (wordAt != *baseAddr)
                    *baseAddr = wordAt;
                if (test)
                {
                    if (firstWord == 0)
                        firstWord = newObject;
                    else if (secondWord == 0)
                    {
                        restartAddr = baseAddr;
                        secondWord = newObject;
                    }
                    else break;
                }
            }
            baseAddr++;
        }

        if (baseAddr != endWord)
            // Put this back on the stack while we process the first word
            PushToStack(obj, length < largeObjectSize ? 0 : restartAddr);
        else if (secondWord != 0)
        {
            // Mark it now because we will process it.
            secondWord->SetLengthWord(secondWord->LengthWord() | _OBJ_GC_MARK);
            // Put this on the stack.  If this is a list node we will be
            // pushing the tail.
            PushToStack(secondWord);
        }

        if (firstWord != 0)
        {
            // Mark it and process it immediately.
            firstWord->SetLengthWord(firstWord->LengthWord() | _OBJ_GC_MARK);
            obj = firstWord;
        }
        else if (msp == 0)
        {
            markStack[msp] = 0; // Really finished
            return;
        }
        else
        {
            // Clear the item above the top.  This really is finished.
            if (msp < MARK_STACK_SIZE) markStack[msp] = 0;
            // Pop the item from the stack but don't overwrite it yet.
            // This allows another thread to steal it if there really
            // is nothing else to do.  This is only really important
            // for large objects.
            obj = markStack[--msp]; // Pop something.
        }

        lengthWord = obj->LengthWord();
    }
}

// Process a constant within the code.  This is a direct copy of ScanAddress::ScanConstant
// with the addition of the locking.
void MTGCProcessMarkPointers::ScanConstant(byte *addressOfConstant, ScanRelocationKind code)
{
    // If this code is in the local area there's the possibility that
    // ScanObjectAddress could return an updated address for a
    // constant within the code.  This could happen if the code is
    // in the allocation area or if it has been moved into the
    // mutable/immutable area by the last incomplete partial GC.
    // Constants can be aligned on any byte offset so another thread
    // scanning the same code could see an invalid address if it read
    // the constant while it was being updated.  We put a lock round
    // this just in case.
    LocalMemSpace *space = gMem.LocalSpaceForAddress(addressOfConstant);

    if (space != 0)
        space->spaceLock.Lock();
    PolyWord p = GetConstantValue(addressOfConstant, code);
    if (space != 0)
        space->spaceLock.Unlock();

    if (! IS_INT(p))
    {
        PolyWord oldValue = p;
        ScanAddress::ScanAddressAt(&p);
        if (p != oldValue) // Update it if it has changed.
        {
            if (space != 0)
                space->spaceLock.Lock();
            SetConstantValue(addressOfConstant, p, code);
            if (space != 0)
                space->spaceLock.Unlock();
        }
    }
}

// Mark all the roots.  This is run in the main thread and has the effect
// of starting new tasks as the scanning runs.
void MTGCProcessMarkPointers::MarkRoots(void)
{
    ASSERT(nThreads >= 1);
    ASSERT(nInUse == 0);
    MTGCProcessMarkPointers *marker = &markStacks[0];
    marker->active = true;
    nInUse = 1;

    // Scan the permanent mutable areas.
    for (unsigned j = 0; j < gMem.npSpaces; j++)
    {
        PermanentMemSpace *space = gMem.pSpaces[j];
        if (space->isMutable && ! space->byteOnly)
            marker->ScanAddressesInRegion(space->bottom, space->top);
    }

    // Scan the RTS roots.
    GCModules(marker);

    ASSERT(marker->markStack[0] == 0);

    // When this has finished there may well be other tasks running.
    PLocker lock(&stackLock);
    marker->active = false;
    nInUse--;
}

static void SetBitmaps(LocalMemSpace *space, PolyWord *pt, PolyWord *top)
{
    while (pt < top)
    {
        PolyObject *obj = (PolyObject*)++pt;
        // If it has been copied by a minor collection skip it
        if (obj->ContainsForwardingPtr())
        {
            obj = FollowForwarding(obj);
            ASSERT(obj->ContainsNormalLengthWord());
            pt += obj->Length();
        }
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

                if (OBJ_IS_WEAKREF_OBJECT(L))
                {
                    // Add this to the limits for the containing area.
                    PolyWord *baseAddr = (PolyWord*)obj;
                    PolyWord *startAddr = baseAddr-1; // Must point AT length word.
                    PolyWord *endObject = baseAddr + n;
                    if (startAddr < space->lowestWeak) space->lowestWeak = startAddr;
                    if (endObject > space->highestWeak) space->highestWeak = endObject;
                }
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
public:
    bool RunRescan(void);
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

// Check whether the stack has overflowed and rescan any words
// in the overflow area.  Returns true if this has happened and
// we need to try again.
bool RescanMarked::RunRescan()
{
    nInUse = 1;
    bool rescan = false;
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
            ScanAddressesInRegion(start, end);
        }
    }
    {
        PLocker lock(&stackLock);
        nInUse--;
    }
    gpTaskFarm->WaitForCompletion();
    return rescan;
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
    
    MTGCProcessMarkPointers::MarkRoots();

    gpTaskFarm->WaitForCompletion();

    // Do we have to recan?
    RescanMarked rescanner;
    while (rescanner.RunRescan()) ;

    gHeapSizeParameters.RecordGCTime(HeapSizeParameters::GCTimeIntermediate, "Mark");

    // Turn the marks into bitmap entries.
    for (unsigned i = 0; i < gMem.nlSpaces; i++)
        gpTaskFarm->AddWorkOrRunNow(&CreateBitmapsTask, gMem.lSpaces[i], 0);

    gpTaskFarm->WaitForCompletion();

    gHeapSizeParameters.RecordGCTime(HeapSizeParameters::GCTimeIntermediate, "Bitmap");

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

// Set up the stacks.
void initialiseMarkerTables()
{
    unsigned threads = gpTaskFarm->ThreadCount();
    if (threads == 0) threads = 1;
    MTGCProcessMarkPointers::InitStatics(threads);
}
