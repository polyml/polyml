/*
    Title:  savestate.cpp - Save and Load state

    Copyright (c) 2007 David C.J. Matthews

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


#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h> // For MAX_PATH
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include "globals.h"
#include "savestate.h"
#include "processes.h"
#include "run_time.h"
#include "polystring.h"
#include "scanaddrs.h"
#include "arb.h"
#include "memmgr.h"
#include "polyexports.h"
#include "mpoly.h" // For exportTimeStamp
#include "exporter.h" // For CopyScan
#include "machine_dep.h"
#include "osmem.h"

#if(!defined(MAXPATHLEN) && defined(MAX_PATH))
#define MAXPATHLEN MAX_PATH
#endif


#define SAVEDSTATESIGNATURE "POLYSAVE"
#define SAVEDSTATEVERSION   1

// File header for a saved state file.  This appears as the first entry
// in the file.
typedef struct _savedStateHeader
{
    // These entries are primarily to check that we have a valid
    // saved state file before we try to interpret anything else.
    char        headerSignature[8];     // Should contain SAVEDSTATESIGNATURE
    unsigned    headerVersion;          // Should contain SAVEDSTATEVERSION
    unsigned    headerLength;           // Number of bytes in the header
    unsigned    segmentDescrLength;     // Number of bytes in a descriptor

    // These entries contain the real data.
    off_t       segmentDescr;           // Position of segment descriptor table
    unsigned    segmentDescrCount;      // Number of segment descriptors in the table
    off_t       stringTable;            // Pointer to the string table (zero if none)
    size_t      stringTableSize;        // Size of string table
    unsigned    parentNameEntry;        // Position of parent name in string table (0 if top)
    UNSIGNEDADDR timeStamp;            // The time stamp for this file.
    UNSIGNEDADDR fileSignature;        // The signature for this file.
    UNSIGNEDADDR parentTimeStamp;      // The time stamp for the parent.
    UNSIGNEDADDR parentSignature;      // The signature for the parent.
} SavedStateHeader;

// Entry for segment table.  This describes the segments on the disc that
// need to be loaded into memory.
typedef struct _savedStateSegmentDescr
{
    off_t       segmentData;            // Position of the segment data
    size_t      segmentSize;            // Size of the segment data
    off_t       relocations;            // Position of the relocation table
    unsigned    relocationCount;        // Number of entries in relocation table
    unsigned    relocationSize;         // Size of a relocation entry
    unsigned    segmentFlags;           // Segment flags (see SSF_ values)
    unsigned    segmentIndex;           // The index of this segment or the segment it overwrites
} SavedStateSegmentDescr;

#define SSF_WRITABLE    1               // The segment contains mutable data
#define SSF_OVERWRITE   2               // The segment overwrites the data (mutable) in a parent.

typedef struct _relocationEntry
{
    // Each entry indicates a location that has to be set to an address.
    // The location to be set is determined by adding "relocAddress" to the base address of
    // this segment (the one to which these relocations apply) and the value to store
    // by adding "targetAddress" to the base address of the segment indicated by "targetSegment".
    POLYUNSIGNED    relocAddress;       // The (byte) offset in this segment that we will set
    POLYUNSIGNED    targetAddress;      // The value to add to the base of the destination segment
    unsigned        targetSegment;      // The base segment.  0 is IO segment. 
    ScanRelocationKind relKind;         // The kind of relocation (processor dependent).
} RelocationEntry;

#define SAVE(x) taskData->saveVec.push(x)


class SaveStateExport: public Exporter, public ScanAddress
{
public:
    SaveStateExport(): relocationCount(0) {}
public:
    virtual void exportStore(void) {} // Not used.

private:
    // ScanAddress overrides
    virtual void ScanConstant(byte *addrOfConst, ScanRelocationKind code);
    // At the moment we should only get calls to ScanConstant.
    virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }

private:
    void setRelocationAddress(void *p, POLYUNSIGNED *reloc);
    PolyWord createRelocation(PolyWord p, void *relocAddr);
    unsigned relocationCount;

    friend Handle SaveState(TaskData *taskData, Handle args);
};

// Generate the address relative to the start of the segment.
void SaveStateExport::setRelocationAddress(void *p, POLYUNSIGNED *reloc)
{
    unsigned area = findArea(p);
    POLYUNSIGNED offset = (char*)p - (char*)memTable[area].mtAddr;
    *reloc = offset;
}


// Create a relocation entry for an address at a given location.
PolyWord SaveStateExport::createRelocation(PolyWord p, void *relocAddr)
{
    RelocationEntry reloc;
    // Set the offset within the section we're scanning.
    setRelocationAddress(relocAddr, &reloc.relocAddress);
    void *addr = p.AsAddress();
    unsigned addrArea = findArea(addr);
    reloc.targetAddress = (char*)addr - (char*)memTable[addrArea].mtAddr;
    reloc.targetSegment = addrArea;
    reloc.relKind = PROCESS_RELOC_DIRECT;
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;
    return p; // Don't change the contents
}


/* This is called for each constant within the code. 
   Print a relocation entry for the word and return a value that means
   that the offset is saved in original word. */
void SaveStateExport::ScanConstant(byte *addr, ScanRelocationKind code)
{
    PolyWord p = GetConstantValue(addr, code);

    if (IS_INT(p) || p == PolyWord::FromUnsigned(0))
        return;

    void *a = p.AsAddress();
    unsigned aArea = findArea(a);

    // We don't need a relocation if this is relative to the current segment
    // since the relative address will already be right.
    if (code == PROCESS_RELOC_I386RELATIVE && aArea == findArea(addr))
        return;

    // Set the value at the address to the offset relative to the symbol.
    RelocationEntry reloc;
    setRelocationAddress(addr, &reloc.relocAddress);
    reloc.targetAddress = (char*)a - (char*)memTable[aArea].mtAddr;
    reloc.targetSegment = aArea;
    reloc.relKind = code;
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;
}



Handle SaveState(TaskData *taskData, Handle args)
// Write a saved state file.
{
    char fileNameBuff[MAXPATHLEN];
    POLYUNSIGNED length =
        Poly_string_to_C(DEREFHANDLE(args)->Get(0), fileNameBuff, MAXPATHLEN);
    if (length > MAXPATHLEN)
        raise_syscall(taskData, "File name too long", ENAMETOOLONG);
    // The value of depth is zero for top-level save so we need to add one for hierarchy.
    unsigned newHierarchy = get_C_ulong(taskData, DEREFHANDLE(args)->Get(1)) + 1;
    // We don't support hierarchical saving at the moment.
    if (newHierarchy != 1)
        raise_fail(taskData, "Depth must be zero");

    // TODO: This must be handled on the root thread.

    // Scan over the permanent mutable area copying all reachable data that is
    // not in a lower hierarchy into new permanent segments.
    CopyScan copyScan(newHierarchy);
    bool success = true;
    try {
        for (unsigned i = 0; i < gMem.npSpaces; i++)
        {
            PermanentMemSpace *space = gMem.pSpaces[i];
            if (space->isMutable)
                copyScan.ScanAddressesInRegion(space->bottom, space->top-space->bottom);
        }
    }
    catch (MemoryException)
    {
        success = false;
    }

    SaveStateExport exports;

    // Copy the areas into the export object.  Make sufficient space for
    // the largest possible number of entries.
    exports.memTable = new memoryTableEntry[gMem.neSpaces+gMem.npSpaces+1];
    exports.ioMemEntry = 0;
    // The IO vector.
    unsigned memTableCount = 0;
    MemSpace *ioSpace = gMem.IoSpace();
    exports.memTable[0].mtAddr = ioSpace->bottom;
    exports.memTable[0].mtLength = (char*)ioSpace->top - (char*)ioSpace->bottom;
    exports.memTable[0].mtFlags = 0;
    exports.memTable[0].mtIndex = 0;
    memTableCount++;

    // Permanent spaces at higher level.  These have to have entries although
    // only the mutable entries will be written.
    for (unsigned w = 0; w < gMem.npSpaces; w++)
    {
        PermanentMemSpace *space = gMem.pSpaces[w];
        if (space->hierarchy < newHierarchy)
        {
            memoryTableEntry *entry = &exports.memTable[memTableCount++];
            entry->mtAddr = space->bottom;
            entry->mtLength = (space->top-space->bottom)*sizeof(PolyWord);
            entry->mtIndex = space->index;
            if (space->isMutable)
                entry->mtFlags = MTF_WRITEABLE;
            else
                entry->mtFlags = MTF_EXECUTABLE;
        }
    }
    unsigned permanentEntries = memTableCount; // Remember where new entries start.

    // Newly created spaces.
    for (unsigned i = 0; i < gMem.neSpaces; i++)
    {
        memoryTableEntry *entry = &exports.memTable[memTableCount++];
        ExportMemSpace *space = gMem.eSpaces[i];
        entry->mtAddr = space->pointer;
        entry->mtLength = (space->top-space->pointer)*sizeof(PolyWord);
        entry->mtIndex = space->index;
        if (space->isMutable)
            entry->mtFlags = MTF_WRITEABLE;
        else
            entry->mtFlags = MTF_EXECUTABLE;
    }

    exports.memTableEntries = memTableCount;
    exports.ioSpacing = IO_SPACING;

    // Update the global memory space table.  Old segments at the same level
    // or lower are removed.  The new segments become permanent.
    if (! success || ! gMem.PromoteExportSpaces(newHierarchy))
    {
        // TODO: Revert any moves by replacing forwarding pointers
        // by ordinary length words.
        raise_syscall(taskData, "Out of Memory", ENOMEM);
    }

    // TODO: Fix up forwarding pointers from the local heap to objects in
    // the new spaces.
    // We may have references to moved objects in local memory.  In particular
    // the stack will not have been copied so that remains in local memory.
    // If this failed we must fix up all forwarding pointers
    // because the destructor for CopyScan deletes all export spaces.

    // Open the file.
    exports.exportFile = fopen(fileNameBuff, "wb");
    if (exports.exportFile == NULL)
        raise_syscall(taskData, "Cannot open save file", errno);

    // Write out the file header.
    SavedStateHeader saveHeader;
    memset(&saveHeader, 0, sizeof(saveHeader));
    saveHeader.headerLength = sizeof(saveHeader);
    strncpy(saveHeader.headerSignature,
        SAVEDSTATESIGNATURE, sizeof(saveHeader.headerSignature));
    saveHeader.headerVersion = SAVEDSTATEVERSION;
    saveHeader.segmentDescrLength = sizeof(SavedStateSegmentDescr);
    saveHeader.parentTimeStamp = exportTimeStamp; // TODO: Change for children
    saveHeader.timeStamp = time(NULL);
    saveHeader.segmentDescrCount = exports.memTableEntries; // One segment for each space.
    // Write out the header.
    fwrite(&saveHeader, sizeof(saveHeader), 1, exports.exportFile);

    // We need a segment header for each permanent area whether it is
    // actually in this file or not.
    SavedStateSegmentDescr *descrs = new SavedStateSegmentDescr [exports.memTableEntries];

    for (unsigned j = 0; j < exports.memTableEntries; j++)
    {
        memoryTableEntry *entry = &exports.memTable[j];
        memset(&descrs[j], 0, sizeof(SavedStateSegmentDescr));
        descrs[j].relocationSize = sizeof(RelocationEntry);
        descrs[j].segmentIndex = entry->mtIndex;
        descrs[j].segmentSize = entry->mtLength; // Set this even if we don't write it.
        if (entry->mtFlags & MTF_WRITEABLE)
        {
            descrs[j].segmentFlags |= SSF_WRITABLE;
            if (j < permanentEntries)
                descrs[j].segmentFlags |= SSF_OVERWRITE;
        }
    }
    // Write out temporarily. Will be overwritten at the end.
    saveHeader.segmentDescr = ftell(exports.exportFile);
    fwrite(descrs, sizeof(SavedStateSegmentDescr), exports.memTableEntries, exports.exportFile);

    // Write out the relocations.
    for (unsigned k = 1 /* Not IO area */; k < exports.memTableEntries; k++)
    {
        memoryTableEntry *entry = &exports.memTable[k];
        if ((entry->mtFlags & MTF_WRITEABLE) != 0 || k >= permanentEntries)
        {
            descrs[k].relocations = ftell(exports.exportFile);
            // Have to write this out.
            exports.relocationCount = 0;
            // Create the relocation table.
            char *start = (char*)entry->mtAddr;
            char *end = start + entry->mtLength;
            for (PolyWord *p = (PolyWord*)start; p < (PolyWord*)end; )
            {
                p++;
                PolyObject *obj = (PolyObject*)p;
                POLYUNSIGNED length = obj->Length();
                exports.relocateObject(obj);
                if (length != 0 && obj->IsCodeObject())
                    machineDependent->ScanConstantsWithinCode(obj, &exports);
                p += length;
            }
            descrs[k].relocationCount = exports.relocationCount;
            // Write out the data.
            descrs[k].segmentData = ftell(exports.exportFile);
            fwrite(entry->mtAddr, entry->mtLength, 1, exports.exportFile);
       }
    }

    // Rewrite the header and the segment tables now they're complete.
    fseek(exports.exportFile, 0, SEEK_SET);
    fwrite(&saveHeader, sizeof(saveHeader), 1, exports.exportFile);
    fwrite(descrs, sizeof(SavedStateSegmentDescr), exports.memTableEntries, exports.exportFile);

    delete[](descrs);
    return SAVE(TAGGED(0));
}

class StateLoader
{
public:
    StateLoader(): loadFile(0), errorResult(0), descrs(0) {}
    ~StateLoader();

    void DoLoad(void);

    FILE *loadFile;
    const char *errorResult;
    SavedStateHeader header;
    SavedStateSegmentDescr *descrs;
};

StateLoader::~StateLoader()
{
    if (loadFile) fclose(loadFile);
    if (descrs) delete[](descrs);
}

// TODO: This must be handled on the root thread.  It affects the
// global memory so all other threads must be stopped.
void StateLoader::DoLoad(void)
{
    // Read the header and check the signature.
    if (fread(&header, sizeof(SavedStateHeader), 1, loadFile) != 1)
    {
        errorResult = "Unable to load header";
        return;
    }
    if (strncmp(header.headerSignature, SAVEDSTATESIGNATURE, sizeof(header.headerSignature)) != 0)
    {
        errorResult = "File is not a saved state";
        return;
    }
    if (header.headerVersion != SAVEDSTATEVERSION ||
        header.headerLength != sizeof(SavedStateHeader) ||
        header.segmentDescrLength != sizeof(SavedStateSegmentDescr))
    {
        errorResult = "Mismatched version";
        return;
    }
    if (header.parentTimeStamp != exportTimeStamp)
    {
        errorResult = 
            "Saved state was exported from a different executable or the executable has changed";
        return;
    }

    // Now have a valid, matching saved state.
    // Load the segment descriptors.
    unsigned nDescrs = header.segmentDescrCount;
    descrs = new SavedStateSegmentDescr[nDescrs];
    if (fseek(loadFile, header.segmentDescr, SEEK_SET) != 0 ||
        fread(descrs, sizeof(SavedStateSegmentDescr), nDescrs, loadFile) != nDescrs)
    {
        errorResult = "Unable to read segment descriptors";
        return;
    }
    // TODO: Any existing spaces at this level or greater must be turned
    // into local spaces.  Do this here and flush the hierarchy table.

    // Read in and create the new segments first.  If we have problems,
    // in particular if we have run out of memory, then it's easier to recover.  
    for (unsigned i = 0; i < nDescrs; i++)
    {
        SavedStateSegmentDescr *descr = &descrs[i];
        MemSpace *space =
            descr->segmentIndex == 0 ? gMem.IoSpace() : gMem.SpaceForIndex(descr->segmentIndex);

        if (descr->segmentData == 0)
        { // No data - just an entry in the index.
            if (space == NULL ||
                descr->segmentSize != (size_t)((char*)space->top - (char*)space->bottom))
            {
                errorResult = "Mismatch for existing memory space";
                return;
            }
        }
        else if ((descr->segmentFlags & SSF_OVERWRITE) == 0)
        { // New segment.
            if (space != NULL)
            {
                errorResult = "Segment already exists";
                return;
            }
            // Allocate memory for the new segment.
            size_t actualSize = descr->segmentSize;
            PolyWord *mem  =
                (PolyWord*)osMemoryManager->Allocate(actualSize,
                                PERMISSION_READ|PERMISSION_WRITE|PERMISSION_EXEC);
            if (mem == 0)
            {
                errorResult = "Unable to allocate memory";
                return;
            }
            if (fseek(loadFile, descr->segmentData, SEEK_SET) != 0 ||
                fread(mem, descr->segmentSize, 1, loadFile) != 1)
            {
                errorResult = "Unable to read segment";
                osMemoryManager->Free(mem, descr->segmentSize);
                return;
            }
            // At the moment we leave all segments with write access.
            space = gMem.NewPermanentSpace(mem, actualSize / sizeof(PolyWord),
                        (descr->segmentFlags & SSF_WRITABLE) != 0, descr->segmentIndex);
        }
    }

    // Now read in the mutable overwrites and process the relocations.
    for (unsigned j = 0; j < nDescrs; j++)
    {
        SavedStateSegmentDescr *descr = &descrs[j];
        MemSpace *space =
            descr->segmentIndex == 0 ? gMem.IoSpace() : gMem.SpaceForIndex(descr->segmentIndex);
        ASSERT(space != NULL); // We should have created it.
        if (descr->segmentFlags & SSF_OVERWRITE)
        {
            if (fseek(loadFile, descr->segmentData, SEEK_SET) != 0 ||
                fread(space->bottom, descr->segmentSize, 1, loadFile) != 1)
            {
                errorResult = "Unable to read segment";
                return;
            }
        }
        // Relocate addresses in this segment.
        if (descr->relocations)
        {
            if (fseek(loadFile, descr->relocations, SEEK_SET) != 0)
            {
                errorResult = "Unable to read relocation segment";
                return;
            }
            for (unsigned k = 0; k < descr->relocationCount; k++)
            {
                RelocationEntry reloc;
                if (fread(&reloc, sizeof(reloc), 1, loadFile) != 1)
                {
                    errorResult = "Unable to read relocation segment";
                    return;
                }
                MemSpace *toSpace =
                    reloc.targetSegment == 0 ? toSpace = gMem.IoSpace() : gMem.SpaceForIndex(reloc.targetSegment);
                if (toSpace == NULL)
                {
                    errorResult = "Unknown space reference in relocation";
                    return;
                }
                byte *setAddress = (byte*)space->bottom + reloc.relocAddress;
                byte *targetAddress = (byte*)toSpace->bottom + reloc.targetAddress;
                if (setAddress >= (byte*)space->top || targetAddress >= (byte*)toSpace->top)
                {
                    errorResult = "Bad relocation";
                    return;
                }
                ScanAddress::SetConstantValue(setAddress, PolyWord::FromCodePtr(targetAddress), reloc.relKind);
            }
        }
    }
}

Handle LoadState(TaskData *taskData, Handle hFileName)
// Load a saved state file and any ancestors.
{
    // Open the load file
    char fileNameBuff[MAXPATHLEN];
    POLYUNSIGNED length =
        Poly_string_to_C(DEREFHANDLE(hFileName), fileNameBuff, MAXPATHLEN);
    if (length > MAXPATHLEN)
        raise_syscall(taskData, "File name too long", ENAMETOOLONG);

    StateLoader loader;
    loader.loadFile = fopen(fileNameBuff, "rb");
    if (loader.loadFile == NULL)
        raise_syscall(taskData, "Cannot open load file", errno);

    // Do the load.  This may set the error string if it failed.
    loader.DoLoad();
    if (loader.errorResult != 0)
        raise_fail(taskData, loader.errorResult);

    return SAVE(TAGGED(0));
}

Handle ShowHierarchy(TaskData *taskData)
// Show the hierarchy.
{
    Handle list = SAVE(ListNull);
    // Not yet implemented: always return empty list.
    return list;
}

Handle RenameParent(TaskData *taskData, Handle args)
// Change the name of the immediate parent stored in a child
{
    raise_fail(taskData, "RenameParent is not yet implemented");
    return SAVE(TAGGED(0));
}

Handle ShowParent(TaskData *taskData, Handle hFileName)
// Return the name of the immediate parent stored in a child
{
    raise_fail(taskData, "ShowParent is not yet implemented");
    return SAVE(C_string_to_Poly(taskData, ""));
}

