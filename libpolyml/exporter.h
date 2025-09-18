/*
    Title:  exporter.h - Export a function as an object or C file

    Copyright (c) 2006, 2015-17, 2020-21 David C.J. Matthews

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

#ifndef EXPORTER_H_INCLUDED
#define EXPORTER_H_INCLUDED

#include "globals.h" // For PolyWord and also int types
#include "memmgr.h" // For ModuleId

#ifdef HAVE_STDIO_H
#include <stdio.h> // For FILE
#endif

#include <map>

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

extern Handle exportNative(TaskData *mdTaskData, Handle args);
extern Handle exportPortable(TaskData *mdTaskData, Handle args);

class ExportMemTable
{
public:
    ExportMemTable() : mtCurrentAddr(0), mtOriginalAddr(0), mtLength(0), 
        mtFlags(0), mtIndex(0) {
    }

    // This previously used the memTable in polyexports.h
    void* mtCurrentAddr;             // The address of the area of memory
    void* mtOriginalAddr;            // The original address, for saved states and 32-in-64.
    uintptr_t mtLength;              // The length in bytes of the area
    unsigned mtFlags;               // Flags describing the area.
    unsigned mtIndex;               // An index to identify permanent spaces.
    ModuleId mtModId;               // The source module
};

// This is the base class for the exporters for the various object-code formats.
class Exporter
{
public:
    Exporter();
    virtual ~Exporter();
    virtual void exportStore(void) = 0;

    // Called by the root thread to do the work.
    void RunExport(PolyObject *rootFunction);

protected:
    virtual PolyWord createRelocation(PolyWord p, void *relocAddr) = 0;
    virtual void relocateValue(PolyWord *pt); // Overridden in ModuleExport
    virtual void relocateObject(PolyObject *p); // Overridden in ModuleExport
    void createRelocation(PolyWord *pt);
    unsigned findArea(void *p); // Find index of area that address is in.
    virtual void addExternalReference(void *p, const char *entryPoint, bool isFuncPtr) {}
    static void revertToLocal();

public:
    FILE     *exportFile;
    const char *errorMessage;

protected:
    ExportMemTable *memTable;
    unsigned memTableEntries;
    PolyObject *rootFunction; // Address of the root function.
    ModuleId exportModId;
};

// The object-code exporters all use a similar string table format
// consisting of null-terminated C-strings.
class ExportStringTable
{
public:
    ExportStringTable();
    ~ExportStringTable();
    unsigned long makeEntry(const char *str);

    char *strings;
    unsigned long stringSize, stringAvailable;
};

#include "scanaddrs.h"

// Because permanent immutable areas are read-only we need to
// have somewhere else to hold the tomb-stones.
class GraveYard {
public:
    GraveYard() { graves = 0; }
    ~GraveYard();
    PolyWord *graves;
    PolyWord *startAddr, *endAddr;
};

class PermanentMemSpace;

class CopyScan: public ScanAddress
{
public:
    CopyScan(unsigned h=0);
    void initialise(bool isExport=true);
    ~CopyScan();
protected:
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);
    // Have to follow pointers from closures into code.
    virtual POLYUNSIGNED ScanCodeAddressAt(PolyObject **pt);
    POLYUNSIGNED ScanAddress(PolyObject **pt);
private:
    enum _newAddrType {
        NAWord,
        NAMutable,
        NANoOverwriteMutable,
        NAByte,
        NACode,
        NACodeConst
    };

    PolyObject* newAddressForObject(POLYUNSIGNED words, enum _newAddrType naType);

public:
    virtual PolyObject *ScanObjectAddress(PolyObject *base);

    // Default sizes of the segments.
    uintptr_t defaultImmSize, defaultCodeSize, defaultMutSize, defaultNoOverSize;
    unsigned hierarchy;

    GraveYard *graveYard;
    unsigned tombs;

    // The module IDs of the permanent spaces referenced.  This is currently only relevant for modules.
    std::map<ModuleId, bool> externalRefs;

private:
    // Local values for hash computation
    uint32_t hash_a, hash_b, hash_c;
    unsigned hash_posn;
    void addToHash(uint32_t v);
    void addWordToHash(POLYUNSIGNED p);
public:
    struct _moduleId extractHash();
};

extern struct _entrypts exporterEPT[];

#endif
