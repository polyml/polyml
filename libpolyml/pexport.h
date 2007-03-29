/*
    Title:     Export memory in a portable format
    Author:    David C. J. Matthews.

    Copyright (c) 2006 David C. J. Matthews


    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR H PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef PEXPORT_H_INCLUDED
#define PEXPORT_H_INCLUDED

#include "scanaddrs.h" // For base class
#include "exporter.h"
#include "globals.h"

class PExport: public Exporter, public ScanAddress
{
public:
    PExport();
    virtual ~PExport();
public:
    virtual void exportStore(void);

private:
    // ScanAddress overrides
    virtual void ScanConstant(byte *addrOfConst, ScanRelocationKind code);
    // At the moment we should only get calls to ScanConstant.
    virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }

private:
    unsigned long getIndex(PolyObject *p);
    void printCodeAddr(byte *q);
    void printAddress(void *p);
    void printValue(PolyWord q);
    void printObject(PolyObject *p);

    // We don't use the relocation code so just provide a dummy function here.
    virtual PolyWord createRelocation(PolyWord p, void *relocAddr) { return p; }

    PolyObject **pMap;
    unsigned long nMapSize, nObjects, totalBytes;
    unsigned *indexOrder;

};

// Import a file in the portable format and return a pointer to the root object.
PolyObject *ImportPortable(const char *fileName);

#endif
