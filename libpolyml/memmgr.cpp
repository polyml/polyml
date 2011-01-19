/*
    Title:  memmgr.cpp   Memory segment manager

    Copyright (c) 2006-7, 2011 David C. J. Matthews

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

#include <new>

#include "globals.h"
#include "memmgr.h"
#include "osmem.h"
#include "scanaddrs.h"
#include "bitmap.h"
#include "mpoly.h"
#include "diagnostics.h"

MemSpace::MemSpace()
{
    spaceType = ST_PERMANENT;
    isMutable = false;
    bottom = 0;
    top = 0;
    isOwnSpace = false;
}

MemSpace::~MemSpace()
{
    if (isOwnSpace && bottom != 0)
        osMemoryManager->Free(bottom, (char*)top - (char*)bottom);
}

LocalMemSpace::LocalMemSpace()
{
    spaceType = ST_LOCAL;
    gen_top = 0; 
    gen_bottom = 0;
    highest = 0;
    pointer = 0;
    for (unsigned i = 0; i < NSTARTS; i++)
        start[i] = 0;
    start_index = 0;
    i_marked = m_marked = copied = updated = 0;
    copiedOut = false;
    copiedIn = false;
    spaceInUse = false;
}

bool LocalMemSpace::InitSpace(POLYUNSIGNED size, bool mut)
{
    isMutable = mut;

    // Allocate the heap itself.
    size_t iSpace = size*sizeof(PolyWord);
    bottom  =
        (PolyWord*)osMemoryManager->Allocate(iSpace, PERMISSION_READ|PERMISSION_WRITE|PERMISSION_EXEC);

    if (bottom == 0)
        return false;
    isOwnSpace = true; // Deallocate when we're finished.

    // The size may have been rounded up to a block boundary.
    size = iSpace/sizeof(PolyWord);

    top = bottom + size;
    gen_top = top;
    pointer = top;
    gen_bottom = top;

    allocationSpace = false;
    
    // Bitmap for the space.
    return bitmap.Create(size);
}

MemMgr::MemMgr()
{
    npSpaces = nlSpaces = nsSpaces = 0;
    minLocal = maxLocal = 0;
    pSpaces = 0;
    lSpaces = 0;
    eSpaces = 0;
    sSpaces = 0;
    nextIndex = 0;
    reservedSpace = 0;
    nextAllocator = 0;
}

MemMgr::~MemMgr()
{
    unsigned i;
    for (i = 0; i < npSpaces; i++)
        delete(pSpaces[i]);
    free(pSpaces);
    for (i = 0; i < nlSpaces; i++)
        delete(lSpaces[i]);
    free(lSpaces);
    for (i = 0; i < neSpaces; i++)
        delete(eSpaces[i]);
    free(eSpaces);
    for (i = 0; i < nsSpaces; i++)
        delete(sSpaces[i]);
    free(sSpaces);
}

// Create and initialise a new local space and add it to the table.
LocalMemSpace* MemMgr::NewLocalSpace(POLYUNSIGNED size, bool mut)
{
    try {
        LocalMemSpace *space = new LocalMemSpace;
        // Before trying to allocate the heap temporarily allocate the
        // reserved space.  This ensures that this much space will always
        // be available for C stacks and the C++ heap.
        void *reservation = 0;
        size_t rSpace = reservedSpace*sizeof(PolyWord);

        if (reservedSpace != 0) {
            reservation = osMemoryManager->Allocate(rSpace, PERMISSION_READ);
            if (reservation == 0) {
                // Insufficient space for the reservation.  Can't allocate this local space.
                if (debugOptions & DEBUG_MEMMGR)
                    Log("MMGR: New local %smutable space: insufficient reservation space\n");
                delete space;
                return 0;
            }
        }

        bool success = space->InitSpace(size, mut) && AddLocalSpace(space);
        if (reservation != 0) osMemoryManager->Free(reservation, rSpace);
        if (success)
        {
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: New local %smutable space %p, size=%luk words, bottom=%p, top=%p\n", mut ? "": "im",
                    space, space->spaceSize()/1024, space->bottom, space->top);
            return space;
        }

        // If something went wrong.
        delete space;
        if (debugOptions & DEBUG_MEMMGR)
            Log("MMGR: New local %smutable space: insufficient space\n", mut ? "": "im");
        return 0;
    }
    catch (std::bad_alloc a) {
        if (debugOptions & DEBUG_MEMMGR)
            Log("MMGR: New local %smutable space: \"new\" failed\n", mut ? "": "im");
        return 0;
    }
}

// Create a local space for initial allocation.
LocalMemSpace *MemMgr::CreateAllocationSpace(POLYUNSIGNED size)
{
    LocalMemSpace *result = NewLocalSpace(size, true);
    if (result) result->allocationSpace = true;
    return result;
}

// Add a local memory space to the table.
bool MemMgr::AddLocalSpace(LocalMemSpace *space)
{
    // Compute the maximum and minimum local addresses.  The idea
    // is to speed up LocalSpaceForAddress which is likely to be a hot-spot
    // in the GC.
    if (nlSpaces == 0)
    {
        // First space
        minLocal = space->bottom;
        maxLocal = space->top;
    }
    else
    {
        if (space->bottom < minLocal)
            minLocal = space->bottom;
        if (space->top > maxLocal)
            maxLocal = space->top;
    }

    // Add to the table.
    LocalMemSpace **table = (LocalMemSpace **)realloc(lSpaces, (nlSpaces+1) * sizeof(LocalMemSpace *));
    if (table == 0) return false;
    lSpaces = table;
    lSpaces[nlSpaces++] = space;
    return true;
}


// Create an entry for a permanent space.
PermanentMemSpace* MemMgr::NewPermanentSpace(PolyWord *base, POLYUNSIGNED words,
                                             bool mut, bool noOv, unsigned index, unsigned hierarchy /*= 0*/)
{
    try {
        PermanentMemSpace *space = new PermanentMemSpace;
        space->bottom = base;
        space->topPointer = space->top = space->bottom + words;
        space->spaceType = ST_PERMANENT;
        space->isMutable = mut;
        space->noOverwrite = noOv;
        space->index = index;
        space->hierarchy = hierarchy;
        if (index >= nextIndex) nextIndex = index+1;

        // Extend the permanent memory table and add this space to it.
        PermanentMemSpace **table =
            (PermanentMemSpace **)realloc(pSpaces, (npSpaces+1) * sizeof(PermanentMemSpace *));
        if (table == 0)
        {
            delete space;
            return 0;
        }
        pSpaces = table;
        pSpaces[npSpaces++] = space;
        return space;
    }
    catch (std::bad_alloc a) {
        return 0;
    }
}

// Delete a local space and remove it from the table.
bool MemMgr::DeleteLocalSpace(LocalMemSpace *sp)
{
    for (unsigned i = 0; i < nlSpaces; i++)
    {
        if (lSpaces[i] == sp)
        {
            bool wasMutable = sp->isMutable;
            delete sp;
            nlSpaces--;
            while (i < nlSpaces)
            {
                lSpaces[i] = lSpaces[i+1];
                i++;
            }
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: Deleted local %smutable space %p\n", wasMutable ? "": "im", sp);
            return true;
        }
    }
    if (debugOptions & DEBUG_MEMMGR)
        Log("MMGR: Deleting local %smutable space: not found in table\n");
    return false;
}

// Create an entry for the IO space.
MemSpace* MemMgr::InitIOSpace(PolyWord *base, POLYUNSIGNED words)
{
    ioSpace.bottom = base;
    ioSpace.top = ioSpace.bottom + words;
    ioSpace.spaceType = ST_IO;
    ioSpace.isMutable = false;
    return &ioSpace;
}


// Create and initialise a new export space and add it to the table.
PermanentMemSpace* MemMgr::NewExportSpace(POLYUNSIGNED size, bool mut, bool noOv)
{
    try {
        PermanentMemSpace *space = new PermanentMemSpace;
        space->spaceType = ST_EXPORT;
        space->isMutable = mut;
        space->noOverwrite = noOv;
        space->index = nextIndex++;
        // Allocate the memory itself.
        size_t iSpace = size*sizeof(PolyWord);
        space->bottom  =
            (PolyWord*)osMemoryManager->Allocate(iSpace, PERMISSION_READ|PERMISSION_WRITE|PERMISSION_EXEC);

        if (space->bottom == 0)
        {
            delete space;
            return 0;
        }
        space->isOwnSpace = true;
 
        // The size may have been rounded up to a block boundary.
        size = iSpace/sizeof(PolyWord);
        space->top = space->bottom + size;
        space->topPointer = space->bottom;

        // Add to the table.
        PermanentMemSpace **table = (PermanentMemSpace **)realloc(eSpaces, (neSpaces+1) * sizeof(PermanentMemSpace *));
        if (table == 0)
        {
            delete space;
            return 0;
        }
        eSpaces = table;
        eSpaces[neSpaces++] = space;
        return space;
    }
    catch (std::bad_alloc a) {
        return 0;
    }
}

void MemMgr::DeleteExportSpaces(void)
{
    while (neSpaces > 0)
        delete(eSpaces[--neSpaces]);
}

// If we have saved the state rather than exported a function we turn the exported
// spaces into permanent ones, removing existing permanent spaces at the same or
// lower level.
bool MemMgr::PromoteExportSpaces(unsigned hierarchy)
{
    // Create a new table big enough to hold all the permanent and export spaces
    PermanentMemSpace **pTable =
        (PermanentMemSpace **)calloc(npSpaces+neSpaces, sizeof(PermanentMemSpace *));
    if (pTable == 0) return false;
    unsigned newSpaces = 0;
    // Save permanent spaces at a lower hierarchy.  Others are converted into
    // local spaces.  Most or all items will have been copied from these spaces
    // into an export space but there could be items reachable only from the stack.
    for (unsigned i = 0; i < npSpaces; i++)
    {
        PermanentMemSpace *pSpace = pSpaces[i];
        if (pSpace->hierarchy < hierarchy)
            pTable[newSpaces++] = pSpace;
        else
        {
            try {
                // Turn this into a local space.
                LocalMemSpace *space = new LocalMemSpace;
                space->top = space->gen_top = space->gen_bottom = pSpace->top;
                space->bottom = space->pointer = pSpace->bottom;
                space->isMutable = pSpace->isMutable;
                space->isOwnSpace = true;
                if (! space->bitmap.Create(space->top-space->bottom) || ! AddLocalSpace(space))
                    return false;
            }
            catch (std::bad_alloc a) {
                return false;
            }
        }
    }
    // Save newly exported spaces.
    for (unsigned j = 0; j < neSpaces; j++)
    {
        PermanentMemSpace *space = eSpaces[j];
        space->hierarchy = hierarchy; // Set the hierarchy of the new spaces.
        space->spaceType = ST_PERMANENT;
        // Put a dummy object to fill up the unused space.
        if (space->topPointer != space->top)
            FillUnusedSpace(space->topPointer, space->top - space->topPointer);
        // Put in a dummy object to fill the rest of the space.
        pTable[newSpaces++] = space;
    }
    neSpaces = 0;
    npSpaces = newSpaces;
    free(pSpaces);
    pSpaces = pTable;

    return true;
}


// Before we import a hierarchical saved state we need to turn any previously imported
// spaces into local spaces.
bool MemMgr::DemoteImportSpaces()
{
    // Create a new permanent space table.
    PermanentMemSpace **table =
        (PermanentMemSpace **)calloc(npSpaces, sizeof(PermanentMemSpace *));
    if (table == NULL) return false;
    unsigned newSpaces = 0;
    for (unsigned i = 0; i < npSpaces; i++)
    {
        PermanentMemSpace *pSpace = pSpaces[i];
        if (pSpace->hierarchy == 0) // Leave truly permanent spaces
            table[newSpaces++] = pSpace;
        else
        {
            try {
                // Turn this into a local space.
                LocalMemSpace *space = new LocalMemSpace;
                space->top = pSpace->top;
                // Space is allocated in local areas from the top down.  This area is full and
                // all data is in the old generation.  The area can be recovered by a full GC.
                space->bottom = space->pointer = space->gen_top = space->gen_bottom = pSpace->bottom;
                space->isMutable = pSpace->isMutable;
                space->isOwnSpace = true;
                if (! space->bitmap.Create(space->top-space->bottom) || ! AddLocalSpace(space))
                {
                    if (debugOptions & DEBUG_MEMMGR)
                        Log("MMGR: Unable to convert saved state space %p into local space\n", pSpace);
                    return false;
                }
                if (debugOptions & DEBUG_MEMMGR)
                    Log("MMGR: Converted saved state space %p into local %smutable space %p\n",
                            pSpace, pSpace->isMutable ? "im": "", space);
            }
            catch (std::bad_alloc a) {
                if (debugOptions & DEBUG_MEMMGR)
                    Log("MMGR: Unable to convert saved state space %p into local space (\"new\" failed)\n", pSpace);
                return false;
            }
        }
    }
    npSpaces = newSpaces;
    free(pSpaces);
    pSpaces = table;

    return true;
}

// Return the space the address is in or NULL if none.
// We have to check here against the bottom of the area rather
// than the allocation because, when called from CopyObject,
// the "pointer" field points to the top of the area.
// N.B.  By checking for pt < space->top we are assuming that we don't have
// zero length objects.  A zero length object at the top of the space would
// have its length word as the last word in the space and the address of the
// object would be == space->top.
MemSpace *MemMgr::SpaceForAddress(const void *pt)
{
    unsigned i;
    for (i = 0; i < nlSpaces; i++)
    {
        MemSpace *space = lSpaces[i];
        if (pt >= space->bottom && pt < space->top)
            return space;
    }
    for (i = 0; i < npSpaces; i++)
    {
        MemSpace *space = pSpaces[i];
        if (pt >= space->bottom && pt < space->top)
            return space;
    }
    for (i = 0; i < neSpaces; i++)
    {
        MemSpace *space = eSpaces[i];
        if (pt >= space->bottom && pt < space->top)
            return space;
    }
    if (pt >= ioSpace.bottom && pt < ioSpace.top)
        return &ioSpace;
    return 0; // Not in any space
}

bool MemMgr::IsPermanentMemoryPointer(const void *pt)
{
    for (unsigned i = 0; i < npSpaces; i++)
    {
        MemSpace *space = pSpaces[i];
        if (pt >= space->bottom && pt < space->top)
            return true;
    }
    return false;
}

// Return the space for a given index
PermanentMemSpace *MemMgr::SpaceForIndex(unsigned index)
{
    for (unsigned i = 0; i < npSpaces; i++)
    {
        PermanentMemSpace *space = pSpaces[i];
        if (space->index == index)
            return space;
    }
    return NULL;
}

// In several places we assume that segments are filled with valid
// objects.  This fills unused memory with one or more "byte" objects.
void MemMgr::FillUnusedSpace(PolyWord *base, POLYUNSIGNED words)
{
    PolyWord *pDummy = base+1;
    while (words > 0)
    {
        POLYUNSIGNED oSize = words;
        // If the space is larger than the maximum object size
        // we will need several objects.
        if (words > MAX_OBJECT_SIZE) oSize = MAX_OBJECT_SIZE;
        else oSize = words-1;
        // Make this a byte object so it's always skipped.
        ((PolyObject*)pDummy)->SetLengthWord(oSize, F_BYTE_OBJ);
        words -= oSize+1;
        pDummy += oSize+1;
    }
}

// Allocate an area of the heap of at least minWords and at most maxWords.
// This is used both when allocating single objects (when minWords and maxWords
// are the same) and when allocating heap segments.  If there is insufficient
// space to satisfy the minimum it will return 0.
PolyWord *MemMgr::AllocHeapSpace(POLYUNSIGNED minWords, POLYUNSIGNED &maxWords)
{
    allocLock.Lock();
    // We try to distribute the allocations between the memory spaces
    // so that at the next GC we don't have all the most recent cells in
    // one space.  The most recent cells will be more likely to survive a
    // GC so distibuting them improves the load balance for a multi-thread GC.
    nextAllocator++;
    if (nextAllocator > gMem.nlSpaces) nextAllocator = 0;

    for (unsigned j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[(j + nextAllocator) % gMem.nlSpaces];
        if (space->allocationSpace)
        {
            POLYUNSIGNED available = space->pointer - space->bottom;
            if (available > 0 && available >= minWords)
            {
                // Reduce the maximum value if we had less than that.
                if (available < maxWords)
                    maxWords = available;
                space->pointer -= maxWords; // Allocate it.
                PolyWord *result = space->pointer; // Return the address.
                allocLock.Unlock();
                return result;
            }
        }
    }
    allocLock.Unlock();
    return 0; // There isn't space even for the minimum.
}

StackSpace *MemMgr::NewStackSpace(POLYUNSIGNED size)
{
    try {
        StackSpace *space = new StackSpace;
        size_t iSpace = size*sizeof(PolyWord);
        space->bottom =
            (PolyWord*)osMemoryManager->Allocate(iSpace, PERMISSION_READ|PERMISSION_WRITE);
        if (space->bottom == 0)
        {
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: New stack space: insufficient space\n");
            delete space;
            return false;
        }

        // The size may have been rounded up to a block boundary.
        size = iSpace/sizeof(PolyWord);
        space->top = space->bottom + size;
        space->spaceType = ST_STACK;
        space->isMutable = true;

        // Extend the permanent memory table and add this space to it.
        StackSpace **table =
            (StackSpace **)realloc(sSpaces, (nsSpaces+1) * sizeof(StackSpace *));
        if (table == 0)
        {
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: New stack space: table realloc failed\n");
            delete space;
            return 0;
        }
        sSpaces = table;
        sSpaces[nsSpaces++] = space;
        if (debugOptions & DEBUG_MEMMGR)
            Log("MMGR: New stack space %p allocated at %p size %lu\n", space, space->bottom, space->spaceSize());
        return space;
    }
    catch (std::bad_alloc a) {
        if (debugOptions & DEBUG_MEMMGR)
            Log("MMGR: New stack space: \"new\" failed\n");
        return 0;
    }
}

// Copy a stack
static void CopyStackFrame(StackObject *old_stack, POLYUNSIGNED old_length, StackObject *new_stack, POLYUNSIGNED new_length)
{
  /* Moves a stack, updating all references within the stack */
    PolyWord *old_base  = (PolyWord *)old_stack;
    PolyWord *new_base  = (PolyWord*)new_stack;
    PolyWord *old_top   = old_base + old_length;

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

    POLYUNSIGNED i;
    for (i = 0; i < new_stack->p_nreg; i++)
    {
        PolyWord R = old_stack->p_reg[i];

        /* if the register points into the old stack, make the new copy
           point at the same relative offset within the new stack,
           otherwise make the new copy identical to the old version. */

        if (R.IsTagged() || R.AsStackAddr() < old_base || R.AsStackAddr() >= old_top)
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
//        ASSERT(old >= old_base && old < old_base+old_length);
//        ASSERT(newp >= new_base && newp < new_base+new_length);
        PolyWord old_word = *old++;
        if (old_word.IsTagged() || old_word.AsStackAddr() < old_base || old_word.AsStackAddr() >= old_top)
            *newp++ = old_word;
        else
            *newp++ = PolyWord::FromStackAddr(old_word.AsStackAddr() + offset);
    }
    ASSERT(old == ((PolyWord*)old_stack)+old_length);
    ASSERT(newp == ((PolyWord*)new_stack)+new_length);
}


bool MemMgr::GrowOrShrinkStack(StackSpace *space, POLYUNSIGNED newSize)
{
    size_t iSpace = newSize*sizeof(PolyWord);
    PolyWord *newSpace = (PolyWord*)osMemoryManager->Allocate(iSpace, PERMISSION_READ|PERMISSION_WRITE);
    if (newSpace == 0)
    {
        if (debugOptions & DEBUG_MEMMGR)
            Log("MMGR: Unable to change size of stack %p from %lu to %lu: insufficient space\n",
                space, space->spaceSize(), newSize);
        return false;
    }
    // The size may have been rounded up to a block boundary.
    newSize = iSpace/sizeof(PolyWord);
    CopyStackFrame(space->stack(), space->spaceSize(), (StackObject*)newSpace, newSize);
    if (debugOptions & DEBUG_MEMMGR)
        Log("MMGR: Size of stack %p changed from %lu to %lu at %p\n", space, space->spaceSize(), newSize, newSpace);
    osMemoryManager->Free(space->bottom, (char*)space->top - (char*)space->bottom);
    space->bottom = newSpace;
    space->top = newSpace+newSize;
    return true;
}


// Delete a stack when a thread has finished.
bool MemMgr::DeleteStackSpace(StackSpace *space)
{
    for (unsigned i = 0; i < nsSpaces; i++)
    {
        if (sSpaces[i] == space)
        {
            delete space;
            nsSpaces--;
            while (i < nsSpaces)
            {
                sSpaces[i] = sSpaces[i+1];
                i++;
            }
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: Deleted stack space %p\n", space);
            return true;
        }
    }
    if (debugOptions & DEBUG_MEMMGR)
        Log("MMGR: Deleting stack space %p: not found in table\n", space);
    return false;
}

MemMgr gMem; // The one and only memory manager object

