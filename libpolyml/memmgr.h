/*
    Title:  memmgr.h   Memory segment manager

    Copyright (c) 2006-8 David C. J. Matthews

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

#ifndef MEMMGR_H
#define MEMMGR_H

#include "bitmap.h"
#include "locking.h"

class ScanAddress;

typedef enum {
    ST_IO,          // The io area forms an interface with the RTS
    ST_PERMANENT,   // Permanent areas are part of the object code
                    // Also loaded saved state.
    ST_LOCAL,       // Local heaps contain volatile data
    ST_EXPORT       // Temporary export area
} SpaceType;

// Base class for the various memory spaces.
class MemSpace
{
protected:
    MemSpace();
    virtual ~MemSpace();

public:
    SpaceType       spaceType;
    bool            isMutable;
    bool            isOwnSpace; // True if this has been allocated.

    PolyWord        *bottom;    // Bottom of area
    PolyWord        *top;       // Top of area.

    // These next two are used in the GC to limit scanning for
    // weak refs.
    PolyWord        *lowestWeak, *highestWeak;

    friend class MemMgr;
};

// Permanent memory space.  Either linked into the executable program or
// loaded from a saved state file.
class PermanentMemSpace: public MemSpace
{
protected:
    PermanentMemSpace(): index(0), hierarchy(0), noOverwrite(false), topPointer(0) {}
public:
    unsigned    index;      // An identifier for the space.  Used when saving and loading.
    unsigned    hierarchy;  // The hierarchy number: 0=from executable, 1=top level saved state, ...
    bool        noOverwrite; // Don't save this in deeper hierarchies.

    // When exporting or saving state we copy data into a new area.
    // This area grows upwards unlike the local areas that grow down.
    PolyWord    *topPointer;

    friend class MemMgr;
};

#define NSTARTS 1024

// Local areas can be garbage collected.
class LocalMemSpace: public MemSpace
{
protected:
    LocalMemSpace();
    virtual ~LocalMemSpace() {}
    bool InitSpace(POLYUNSIGNED size, bool mut);

public:
    PolyWord    *pointer;         // Allocation pointer. Objects are allocated AFTER this.
    PolyWord    *gen_top;         /* top    of area to garbage collect.                */
    PolyWord    *gen_bottom;      /* lowest object in area before copying.             */
    Bitmap       bitmap;          /* bitmap with one bit for each word in the GC area. */
    POLYUNSIGNED highest;         /* index of top word in bitmap.                      */
    POLYUNSIGNED start[NSTARTS];  /* starting points for bit searches.                 */
    POLYUNSIGNED start_index;     /* last index used to index start array              */
    POLYUNSIGNED i_marked;        /* count of immutable words marked.                  */
    POLYUNSIGNED m_marked;        /* count of mutable words marked.                    */
    POLYUNSIGNED copied;          /* count of words copied.                            */
    POLYUNSIGNED updated;         /* count of words updated.                           */

    friend class MemMgr;
};

class MemMgr
{
public:
    MemMgr();
    ~MemMgr();

    // Create and initialise a new local space and add it to the table.
    LocalMemSpace *NewLocalSpace(POLYUNSIGNED size, bool mut);
    // Create an entry for a permanent space.
    PermanentMemSpace *NewPermanentSpace(PolyWord *base, POLYUNSIGNED words, bool mut,
        bool noOv, unsigned index, unsigned hierarchy = 0);
    // Create an entry for the IO space.
    MemSpace   *InitIOSpace(PolyWord *base, POLYUNSIGNED words);
    // Delete a local space
    bool DeleteLocalSpace(LocalMemSpace *sp);

    // Allocate an area of the heap of at least minWords and at most maxWords.
    // This is used both when allocating single objects (when minWords and maxWords
    // are the same) and when allocating heap segments.  If there is insufficient
    // space to satisfy the minimum it will return 0.  Updates "maxWords" with
    // the space actually allocated
    PolyWord *AllocHeapSpace(POLYUNSIGNED minWords, POLYUNSIGNED &maxWords);
    PolyWord *AllocHeapSpace(POLYUNSIGNED words)
        { POLYUNSIGNED allocated = words; return AllocHeapSpace(words, allocated); }

    // Create and delete export spaces
    PermanentMemSpace *NewExportSpace(POLYUNSIGNED size, bool mut, bool noOv);
    void DeleteExportSpaces(void);
    bool PromoteExportSpaces(unsigned hierarchy); // Turn export spaces into permanent spaces.
    bool DemoteImportSpaces(void); // Turn previously imported spaces into local.

    MemSpace *SpaceForAddress(const void *pt); // Return the space the address is in or NULL if none.
    PermanentMemSpace *SpaceForIndex(unsigned index); // Return the space for a given index

    // See if the address is in a local space.  This is used in the GC and
    // needs to be fast.
    LocalMemSpace *LocalSpaceForAddress(const void *pt)
    {
        if (pt >= minLocal && pt <= maxLocal)
        {
            for (unsigned i = 0; i < nlSpaces; i++)
            {
                LocalMemSpace *space = lSpaces[i];
                if (pt >= space->bottom && pt < space->top)
                    return space;
            }
        }
        return 0;
    }

    bool IsIOPointer(const void *pt) { return pt >= ioSpace.bottom && pt < ioSpace.top; }
    bool IsPermanentMemoryPointer(const void *pt);
    bool IsLocalMutable(const void *pt)
    { LocalMemSpace *space = LocalSpaceForAddress(pt); return space != 0 && space->isMutable; }

    // In several places we assume that segments are filled with valid
    // objects.  This fills unused memory with one or more "byte" objects.
    void FillUnusedSpace(PolyWord *base, POLYUNSIGNED words);

    MemSpace *IoSpace() { return &ioSpace; } // Return pointer to the IO space.

    MemSpace ioSpace; // The IO space

    // Table for permanent spaces
    PermanentMemSpace **pSpaces;
    unsigned npSpaces;

    // Table for local spaces
    LocalMemSpace **lSpaces;
    unsigned nlSpaces;

    // Table for export spaces
    PermanentMemSpace **eSpaces;
    unsigned neSpaces;

    // Used for quick check for local addresses.
    PolyWord *minLocal, *maxLocal;

    // Storage manager lock.
    PLock allocLock;

    unsigned nextIndex; // Used when allocating new permanent spaces.

private:
    bool AddLocalSpace(LocalMemSpace *space);
};

extern MemMgr gMem;

#endif
