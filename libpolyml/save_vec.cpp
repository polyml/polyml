/*
    Title:  save_vec.cpp - The save vector holds temporary values that may move as
    the result of a garbage collection.

    Copyright (c) 2006, 2010 David C.J. Matthews

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
#include "save_vec.h"
#include "check_objects.h"
#include "scanaddrs.h"


#define SVEC_SIZE 1000

SaveVec::SaveVec()
{
    save_vec = new SaveVecEntry[SVEC_SIZE];
    save_vec_addr = save_vec;
}

SaveVec::~SaveVec()
{
    delete[](save_vec);
}

// DCJM - I've used this in a few cases where we iterate over a list
// and want to avoid overflowing the save vec.  I've assumed that simply
// resetting the list doesn't actually destroy the entry on the save vec
// and it's safe to still use it provided that doesn't result in allocation.
void SaveVec::reset(Handle old_value)
{
    ASSERT(old_value >= save_vec && old_value <= save_vec_addr);
    save_vec_addr = old_value;
}

Handle SaveVec::push(PolyWord valu) /* Push a PolyWord onto the save vec. */
{
    ASSERT(save_vec_addr < save_vec+SVEC_SIZE);

    Check(valu);

    *save_vec_addr = SaveVecEntry(valu);
    return save_vec_addr++;
}

POLYUNSIGNED SaveVecEntry::ReplaceStackHandle(const StackObject *stack)
{
    PolyWord *addr = Word().AsStackAddr();
    POLYUNSIGNED offset = 0;
    if (addr > (PolyWord*)stack && addr < stack->Offset(stack->Length()))
    {
        offset = addr-(PolyWord*)stack;
        m_Handle = (PolyObject*)stack;
    }
    return offset;
}

void SaveVec::gcScan(ScanAddress *process)
/* Ensures that all the objects are retained and their addresses updated. */
{
    for (Handle sv = save_vec; sv < save_vec_addr; sv++)
    {
        PolyWord *saved = &(sv->m_Handle);
        if ((*saved).IsTagged()) {} // Don't need to do anything
        else if ((*saved).IsCodePtr())
        {
            // We can have code pointers in set_code_address.
            // Find the start of the code segment
            PolyObject *obj = ObjCodePtrToPtr(saved->AsCodePtr());
            // Calculate the byte offset of this value within the code object.
            POLYUNSIGNED offset = saved->AsCodePtr() - (byte*)obj;
            process->ScanRuntimeAddress(&obj, ScanAddress::STRENGTH_STRONG);
            *saved = PolyWord::FromCodePtr((byte*)obj + offset);

        }
        else {
            ASSERT((*saved).IsDataPtr());
            PolyObject *obj = (*saved).AsObjPtr();
            process->ScanRuntimeAddress(&obj, ScanAddress::STRENGTH_STRONG);
            *saved = obj;
        }
    }
}

// We just have one of these.
static SaveVec save;

SaveVec *gSaveVec = &save;
