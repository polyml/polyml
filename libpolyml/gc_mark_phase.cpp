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

#define MARK_STACK_SIZE 3000

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

    static void MarkPermanentMutableAreaTask(GCTaskId *, void *arg1, void *);

    static void MarkPointersTask(GCTaskId *, void *arg1, void *arg2);

private:
    bool TestForScan(PolyWord *pt);
    void MarkAndTestForScan(PolyWord *pt);

    void PushToStack(PolyObject *obj)
    {
        if (msp < MARK_STACK_SIZE)
            markStack[msp++] = obj;
        else StackOverflow(obj);
    }

    static void StackOverflow(PolyObject *obj);

    PolyObject *markStack[MARK_STACK_SIZE];
    unsigned msp;
};

MTGCProcessMarkPointers::MTGCProcessMarkPointers(): msp(0)
{
    // Clear the mark stack
//    for (unsigned i = 0; i < MARK_STACK_SIZE; i++)
//        markStack[i] = 0;
}

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

// Tests if this needs to be scans.  It marks it if it has not been marked
// unless it has to be scanned.
bool MTGCProcessMarkPointers::TestForScan(PolyWord *pt)
{
    if ((*pt).IsTagged())
        return false;

    LocalMemSpace *space = gMem.LocalSpaceForAddress((*pt).AsAddress());
    if (space == 0)
        return false; // Ignore it if it points to a permanent area

    // This could contain a forwarding pointer if it points into an
    // allocation area and has been moved by the minor GC.
    PolyObject *obj = (*pt).AsObjPtr();
    if (obj->ContainsForwardingPtr())
    {
        *pt = obj->GetForwardingPtr();
        space = gMem.LocalSpaceForAddress((*pt).AsAddress());
        obj = (*pt).AsObjPtr();
    }

    POLYUNSIGNED L = obj->LengthWord();
    if (L & _OBJ_GC_MARK)
        return false; // Already marked

    POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);
    ASSERT(obj+n <= (PolyObject*)space->top); // Check the length is sensible

    if (debugOptions & DEBUG_GC_DETAIL)
        Log("GC: Mark: %p %" POLYUFMT " %u\n", obj, n, GetTypeBits(L));

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
        obj = obj->GetForwardingPtr();
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

    if ((PolyWord*)obj <= space->fullGCLowerLimit)
        space->fullGCLowerLimit = (PolyWord*)obj-1;

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
    PolyObject *val = *pt;
    CheckPointer (val);
    if (weak == STRENGTH_WEAK) return;
    *pt = ScanObjectAddress(*pt);
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
        PolyWord *baseAddr = (PolyWord*)obj;

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
            // Add this to the limits for the containing area.
            MemSpace *space = gMem.SpaceForAddress(baseAddr);
            PolyWord *startAddr = baseAddr-1; // Must point AT length word.
            PolyWord *endObject = baseAddr + length;
            if (startAddr < space->lowestWeak) space->lowestWeak = startAddr;
            if (endObject > space->highestWeak) space->highestWeak = endObject;

            // We've finished with this.
            length = 0;
        }

        else if (OBJ_IS_CODE_OBJECT(lengthWord))
        {
            // Scan constants within the code.  Ultimately this calls ScanObjectAddress.
            machineDependent->ScanConstantsWithinCode(obj, obj, length, this);

            // Skip to the constants and get ready to scan them.
            // Updates length and baseAddr
            obj->GetConstSegmentForCode(length, baseAddr, length);

        }

        // else it's a normal object,

        // If there are only two addresses in this cell that need to be
        // followed we follow them immediately and treat this cell as done.
        // If there are more than two we push the address of this cell on
        // the stack, follow the first address and then rescan it.  That way
        // list cells are processed once only but we don't overflow the
        // stack by pushing all the addresses in a very large vector.
        PolyWord *endWord = baseAddr + length;
        PolyObject *firstWord = 0;
        PolyObject *secondWord = 0;

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
                        secondWord = baseAddr->AsObjPtr();
                    else break;  // More than two words.
                }
            }
            else if (wordAt.IsCodePtr())
            {
                // If we're processing the constant area of a code segment this could
                // be a code address.
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
                        secondWord = newObject;
                    else break;
                }
            }
            baseAddr++;
        }

        if (baseAddr != endWord)
            // Put this back on the stack while we process the first word
            PushToStack(obj);
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
            return;
        else obj = markStack[--msp]; // Pop something.

        lengthWord = obj->LengthWord();
    }
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
