/*
    Title:      Multi-Threaded Garbage Collector - Update phase

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
This is the third, update, phase of the garbage collector.  The previous, copy,
phase will have moved cells in memory.  The update phase goes through all cells
that could contain an address of a cell that has been moved and looks for a
tomb-stone that contains its new location. 
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
#include "run_time.h"
#include "processes.h"
#include "gc.h"
#include "scanaddrs.h"
#include "check_objects.h"
#include "bitmap.h"
#include "memmgr.h"
#include "gctaskfarm.h"
#include "diagnostics.h"

inline POLYUNSIGNED BITNO(LocalMemSpace *area, PolyWord *pt) { return pt - area->bottom; }

class MTGCProcessUpdate: public ScanAddress
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
PolyObject *MTGCProcessUpdate::ScanObjectAddress(PolyObject *obj)
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

void MTGCProcessUpdate::ScanRuntimeAddress(PolyObject **pt, RtsStrength/* weak*/)
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

// Update the addresses in a group of words.
POLYUNSIGNED MTGCProcessUpdate::ScanAddressAt(PolyWord *pt)
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
// area->highest corresponds to gen_top i.e. we don't process older generations.
void MTGCProcessUpdate::UpdateObjectsInArea(LocalMemSpace *area)
{
    PolyWord *pt      = area->upperAllocPtr;
    POLYUNSIGNED   bitno   = BITNO(area, pt);
    POLYUNSIGNED   highest = BITNO(area, area->top);

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
        
        if (bitno == highest) {
            // Have reached the top of the area
            ASSERT(pt == area->top);
            break;
        }
        
        /* first set bit corresponds to the length word */
        pt++;
        PolyObject *obj = (PolyObject*)pt;
        POLYUNSIGNED L = obj->LengthWord();
        bitno++;
        
        if (obj->ContainsForwardingPtr())    /* skip over moved object */
        {
            obj = obj->GetForwardingPtr();
            ASSERT(obj->ContainsNormalLengthWord()); // Only one level of forwarding
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
                        if (space != 0)
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

    // Clear the bitmaps ready for the next collection.  Doing it now
    // means we can avoid clearing more than we really need.
    if (area->i_marked != 0 || area->m_marked != 0)
        // We've marked something so we may have set a bit below the current pointer
        area->bitmap.ClearBits(0, area->top - area->bottom);
    else // Otherwise we only need to clear in the area we've newly allocated.  This is
        // the case when we're doing a partial collection and copying into the
        // immutable area.
        area->bitmap.ClearBits(area->upperAllocPtr - area->bottom, area->top - area->upperAllocPtr);
}

// Task to update addresses in a local area.
static void updateLocalArea(void *arg1, void *arg2)
{
    MTGCProcessUpdate *processUpdate = (MTGCProcessUpdate *)arg1;
    LocalMemSpace *space = (LocalMemSpace *)arg2;
    if (debugOptions & DEBUG_GC)
        Log("GC: Update local area %p\n", space);
    // Process the current generation for mutable or immutable areas.
    processUpdate->UpdateObjectsInArea(space);
    if (debugOptions & DEBUG_GC)
        Log("GC: Completed local update for %p. %lu words updated\n", space, space->updated);
}

// Task to update addresses in a non-local area.
static void updateNonLocalMutableArea(void *arg1, void *arg2)
{
    MTGCProcessUpdate *processUpdate = (MTGCProcessUpdate *)arg1;
    MemSpace *space = (MemSpace *)arg2;
    if (debugOptions & DEBUG_GC)
        Log("GC: Update non-local mutable area %p\n", space);
    processUpdate->ScanAddressesInRegion(space->bottom, space->top);
    if (debugOptions & DEBUG_GC)
        Log("GC: Completed non-local mutable update for %p\n", space);
}

// Task to update addresses maintained by the RTS itself.
static void updateGCProcAddresses(void *arg1, void *)
{
    MTGCProcessUpdate *processUpdate = (MTGCProcessUpdate *)arg1;
    GCModules(processUpdate);
}

void GCUpdatePhase()
{
    unsigned j;

    /* Update phase */
    mainThreadPhase = MTP_GCPHASEUPDATE;
    
    /* Invariant: at most the first (gen_top - bottom) bits of each bitmap can be dirty here. */
    for(j = 0; j < gMem.nlSpaces; j++)
        gMem.lSpaces[j]->updated = 0;

    // We can do the updates in parallel since they don't interfere at all.
    MTGCProcessUpdate processUpdate;

    // Process local areas.
    for (j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[j];
        // As well as updating the addresses this also clears the bitmaps.
        gpTaskFarm->AddWorkOrRunNow(&updateLocalArea, &processUpdate, space);
    }
    // Scan the permanent mutable areas.
    for (j = 0; j < gMem.npSpaces; j++)
    {
        MemSpace *space = gMem.pSpaces[j];
        if (space->isMutable)
            gpTaskFarm->AddWorkOrRunNow(&updateNonLocalMutableArea, &processUpdate, space);
    }
    // Update addresses in RTS modules.
    gpTaskFarm->AddWorkOrRunNow(&updateGCProcAddresses, &processUpdate, 0);
    // Wait for these to complete before proceeding.
    gpTaskFarm->WaitForCompletion();
}
