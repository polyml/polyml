/*
    Title:      Validate addresses in objects.

    Copyright (c) 2006
        David C.J. Matthews

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
#include "diagnostics.h"
#include "machine_dep.h"
#include "scanaddrs.h"
#include "memmgr.h"

#define INRANGE(val,start,end)\
  (start <= val && val < end)

static void CheckAddress(const void *pt)
{
    if (gMem.SpaceForAddress(pt) == 0)
        Crash ("Bad pointer 0x%08x found", pt);
}

void DoCheck (const PolyWord pt)
{
    if (pt == PolyWord::FromUnsigned(0)) return;

    if (pt.IsTagged()) return;

    /* Test corrected 20/2/95 - the stack contains code pointers 
     (for return addresses) as well as simple data pointers */
    ASSERT (pt.IsCodePtr() || pt.IsDataPtr());
    CheckAddress(pt.AsAddress());
} 

class ScanCheckAddress: public ScanAddress
{
public:
    virtual PolyObject *ScanObjectAddress(PolyObject *pt) { DoCheck(pt); return pt; }
};

void DoCheckObject (const PolyObject *base, POLYUNSIGNED L)
{

    PolyWord *pt  = (PolyWord*)base;
    CheckAddress(pt-1);

    ASSERT (OBJ_IS_LENGTH(L));

    POLYUNSIGNED n   = OBJ_OBJECT_LENGTH(L);
    if (n == 0) return;

    ASSERT (n > 0);

    PolyWord *end = pt + n;

    CheckAddress (end-1);

    byte flags = GetTypeBits(L);  /* discards GC flag and mutable bit */

    if (flags == F_BYTE_OBJ) /* possibly signed byte object */
        return; /* Nothing more to do */

    if (flags == F_STACK_OBJ)
    {
        StackObject *stack = (StackObject *) base;
        POLYUNSIGNED     skip  = stack->p_sp - (PolyWord*)base;

        ASSERT (INRANGE(stack->p_sp, (PolyWord*)base, (PolyWord*)end));
        ASSERT (INRANGE(stack->p_hr, (PolyWord*)base, (PolyWord*)end));

        /* Skip to the start of the stack. */
        ASSERT (skip < n);

        pt += skip; 
        n  -= skip;
    }
    else if (flags == F_CODE_OBJ) /* code object */
    {
        ScanCheckAddress checkAddr;
        /* We flush the instruction cache here in case we change any of the
          instructions when we update addresses. */
        machineDependent->FlushInstructionCache(pt, (n + 1) * sizeof(PolyWord));
        machineDependent->ScanConstantsWithinCode((PolyObject *)base, (PolyObject *)base, n, &checkAddr);
        /* Skip to the constants. */
        base->GetConstSegmentForCode(n, pt, n);
    }
    else ASSERT (flags == 0); /* ordinary word object */

    while (n--) DoCheck (*pt++);
}

void DoCheckPointer (const PolyWord pt)
{
    if (pt == PolyWord::FromUnsigned(0)) return;

    if (OBJ_IS_AN_INTEGER(pt)) return;

    if (gMem.IsIOPointer(pt.AsAddress())) return;

    DoCheck (pt);

    if (pt.IsDataPtr())
    {
        PolyObject *obj = pt.AsObjPtr();
        DoCheckObject (obj, obj->LengthWord());
    }
}

// Check all the objects in the memory.  Used to check the garbage collector
//
void DoCheckMemory()
{
    ScanCheckAddress memCheck;
    // Scan the local mutable area.  This is where new objects are created.
    // The immutable areas are only modified by the GC.
    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        LocalMemSpace *space = gMem.lSpaces[i];
        if (space->isMutable)
            memCheck.ScanAddressesInRegion(space->pointer, space->top);
    }
    // Scan the permanent mutable areas.
    for (unsigned j = 0; j < gMem.npSpaces; j++)
    {
        MemSpace *space = gMem.pSpaces[j];
        if (space->isMutable)
            memCheck.ScanAddressesInRegion(space->bottom, space->top);
    }
}
