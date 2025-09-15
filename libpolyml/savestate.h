/*
    Title:  savestate.h - Save and Load state

    Copyright (c) 2007, 2015, 2019 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/

#ifndef SAVESTATE_H_INCLUDED
#define SAVESTATE_H_INCLUDED

PolyObject *InitHeaderFromExport(struct _exportDescription *exports);

extern struct _entrypts savestateEPT[];

// Shared with modules system

// Work around bug in Mac OS when reading into MAP_JIT memory.
extern size_t readData(void* ptr, size_t size, FILE* stream);

#include "scanaddrs.h"

class ClearVolatile : public ScanAddress
{
public:
    ClearVolatile() {}
    virtual PolyObject* ScanObjectAddress(PolyObject* base) { return base; }
    virtual void ScanAddressesInObject(PolyObject* base, POLYUNSIGNED lengthWord);
};

#endif

