/*
    Title:  memmgr.cpp   Memory segment manager

    Copyright (c) 2006-7 David C. J. Matthews

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

#include "globals.h"
#include "memmgr.h"
#include "osmem.h"
#include "scanaddrs.h"
#include "bitmap.h"

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
    
    // Bitmap for the space.
    return bitmap.Create(size);
}

MemMgr::MemMgr()
{
    npSpaces = nlSpaces = 0;
    minLocal = maxLocal = 0;
    pSpaces = 0;
    lSpaces = 0;
    eSpaces = 0;
    nextIndex = 0;
}

MemMgr::~MemMgr()
{
    unsigned i;
    for (i = 0; i < npSpaces; i++)
        delete(pSpaces[i]);
    for (i = 0; i < nlSpaces; i++)
        delete(lSpaces[i]);
    for (i = 0; i < neSpaces; i++)
        delete(eSpaces[i]);
}

// Create and initialise a new local space and add it to the table.
LocalMemSpace* MemMgr::NewLocalSpace(POLYUNSIGNED size, bool mut)
{
    LocalMemSpace *space = new LocalMemSpace;
    if (space->InitSpace(size, mut) && AddLocalSpace(space))
        return space;
    // If something went wrong.
    delete space;
    return 0;
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

// Delete a local space and remove it from the table.
bool MemMgr::DeleteLocalSpace(LocalMemSpace *sp)
{
    for (unsigned i = 0; i < nlSpaces; i++)
    {
        if (lSpaces[i] == sp)
        {
            delete sp;
            nlSpaces--;
            while (i < nlSpaces)
            {
                lSpaces[i] = lSpaces[i+1];
                i++;
            }
            return true;
        }
    }
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
            // Turn this into a local space.
            LocalMemSpace *space = new LocalMemSpace;
            space->top = space->gen_top = space->gen_bottom = pSpace->top;
            space->bottom = space->pointer = pSpace->bottom;
            space->isMutable = pSpace->isMutable;
            space->isOwnSpace = true;
            if (! space->bitmap.Create(space->top-space->bottom) || ! AddLocalSpace(space))
                return false;
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
            // Turn this into a local space.
            LocalMemSpace *space = new LocalMemSpace;
            space->top = pSpace->top;
            // Space is allocated in local areas from the top down.  This area is full and
            // all data is in the old generation.  The area can be recovered by a full GC.
            space->bottom = space->pointer = space->gen_top = space->gen_bottom = pSpace->bottom;
            space->isMutable = pSpace->isMutable;
            space->isOwnSpace = true;
            if (! space->bitmap.Create(space->top-space->bottom) || ! AddLocalSpace(space))
                return false;
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
    for (unsigned j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[j];
        if (space->isMutable)
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

MemMgr gMem; // The one and only memory manager object

