/*
    Title:  memmgr.h   Memory segment manager

    Copyright (c) 2006-8, 2010-11 David C. J. Matthews

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
class GCTaskId;

typedef enum {
    ST_IO,          // The io area forms an interface with the RTS
    ST_PERMANENT,   // Permanent areas are part of the object code
                    // Also loaded saved state.
    ST_LOCAL,       // Local heaps contain volatile data
    ST_EXPORT,      // Temporary export area
    ST_STACK        // ML Stack for a thread
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
    
    POLYUNSIGNED spaceSize(void)const { return top-bottom; } // No of words

    // These next two are used in the GC to limit scanning for
    // weak refs.
    PolyWord        *lowestWeak, *highestWeak;

    // Used when printing debugging info
    virtual const char *spaceTypeString() { return isMutable ? "mutable" : "immutable"; }

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
    // Allocation.  The minor GC allocates at the bottom of the areas while the
    // major GC and initial allocations are made at the top.  The reason for this
    // is that it's only possible to scan objects from the bottom up and the minor
    // GC combines scanning with allocation whereas the major GC compacts from the
    // bottom into the top of an area.
    PolyWord    *upperAllocPtr;   // Allocation pointer. Objects are allocated AFTER this.
    PolyWord    *lowerAllocPtr;   // Allocation pointer. Objects are allocated BEFORE this.

    PolyWord    *fullGCLowerLimit;// Lowest object in area before copying.
    PolyWord    *partialGCTop;    // Value of lowerAllocPtr before the current partial GC.
    PolyWord    *partialGCScan;   // Scan pointer used in minor GC
    PLock       spaceLock;        // Lock used to protect forwarding pointers
    GCTaskId    *spaceOwner;      // The thread that "owns" this space during a GC.

    Bitmap       bitmap;          /* bitmap with one bit for each word in the GC area. */
    bool         allocationSpace; // True if this is (mutable) space for initial allocation
    bool         copiedOut;       // Copy phase of GC: true if we've copied out of this.
    bool         copiedIn;        // Copy phase of GC: true if we've copied into this.
    bool         spaceInUse;      // Copy phase of GC: true if a thread is copying into this.
    POLYUNSIGNED start[NSTARTS];  /* starting points for bit searches.                 */
    POLYUNSIGNED start_index;     /* last index used to index start array              */
    POLYUNSIGNED i_marked;        /* count of immutable words marked.                  */
    POLYUNSIGNED m_marked;        /* count of mutable words marked.                    */
    POLYUNSIGNED copied;          /* count of words copied into this area.             */
    POLYUNSIGNED updated;         /* count of words updated.                           */
    
    POLYUNSIGNED allocatedSpace(void)const // Words allocated
        { return (top-upperAllocPtr) + (lowerAllocPtr-bottom); }
    POLYUNSIGNED freeSpace(void)const // Words free
        { return upperAllocPtr-lowerAllocPtr; }

    virtual const char *spaceTypeString()
        { return allocationSpace ? "allocation" : MemSpace::spaceTypeString(); }

    friend class MemMgr;
};

// For historical reasons the saved state is held in the base of the
// stack.  This defines the layout.  Machine-dependent modules may extend
// this class.
class StackBase
{
public:
    POLYUNSIGNED    p_space;    // space available
    POLYCODEPTR     p_pc;       // Program counter (instruction pointer)
    PolyWord        *p_sp;      // stack pointer
    PolyWord        *p_hr;      // handler pointer
    POLYUNSIGNED    p_nreg;     // number of checked registers

    PolyWord Get(POLYUNSIGNED i) const { return ((PolyWord*)this)[i]; }
    void Set(POLYUNSIGNED i, PolyWord v) { ((PolyWord*)this)[i] = v; }
    PolyWord *Offset(POLYUNSIGNED i) const { return ((PolyWord*)this)+i; } // Backwards compatibility
};

class StackObject: public StackBase
{
public:
    PolyWord        p_reg[1];
};

// Stack spaces.  These are managed by the thread module
class StackSpace: public MemSpace
{
public:
    StackSpace() { isOwnSpace = true; }

    StackObject *stack()const { return (StackObject *)bottom; }
};

class MemMgr
{
public:
    MemMgr();
    ~MemMgr();

    // Create a local space for initial allocation.
    LocalMemSpace *CreateAllocationSpace(POLYUNSIGNED size);
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

    // Allocate space for the initial stack for a thread.  The caller must
    // initialise the new stack.  Returns 0 if allocation fails.
    StackSpace *NewStackSpace(POLYUNSIGNED size);

    // Adjust the space for a stack.  Returns true if it succeeded.  If it failed
    // it leaves the stack untouched.
    bool GrowOrShrinkStack(StackSpace *space, POLYUNSIGNED newSize);

    // Delete a stack when a thread has finished.
    bool DeleteStackSpace(StackSpace *space);

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

    void SetReservation(POLYUNSIGNED words) { reservedSpace = words; }

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

    // Table for stack spaces
    StackSpace **sSpaces;
    unsigned nsSpaces;
    PLock stackSpaceLock;

    // Used for quick check for local addresses.
    PolyWord *minLocal, *maxLocal;

    // Storage manager lock.
    PLock allocLock;

    unsigned nextIndex; // Used when allocating new permanent spaces.

private:
    bool AddLocalSpace(LocalMemSpace *space);

    POLYUNSIGNED reservedSpace;
    unsigned nextAllocator;
};

extern MemMgr gMem;

#endif
