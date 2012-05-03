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


#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h> // For MAX_PATH
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h> // For MAX_PATH
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

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
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
#include "mpoly.h" // For exportTimeStamp
#include "exporter.h" // For CopyScan
#include "machine_dep.h"
#include "osmem.h"
#include "gc.h" // For FullGC.

#if(!defined(MAXPATHLEN) && defined(MAX_PATH))
#define MAXPATHLEN MAX_PATH
#endif

#ifdef _MSC_VER
// Don't tell me about ISO C++ changes.
#pragma warning(disable:4996)
#endif

// Helper class to close files on exit.
class AutoClose {
public:
    AutoClose(FILE *f = 0): m_file(f) {}
    ~AutoClose() { if (m_file) ::fclose(m_file); }

    operator FILE*() { return m_file; }

private:
    FILE *m_file;
};

// This is probably generally useful so may be moved into
// a general header file.
template<typename BASE> class AutoFree
{
public:
    AutoFree(BASE p = 0): m_value(p) {}
    ~AutoFree() { free(m_value); }

    // Automatic conversions to the base type.
    operator BASE() { return m_value; }
    BASE operator = (BASE p)  { return (m_value = p); }

private:
    BASE m_value;
};

/*
 *  Structure definitions for the saved state files.
 */

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
    time_t      timeStamp;            // The time stamp for this file.
    uintptr_t   fileSignature;        // The signature for this file.
    time_t      parentTimeStamp;      // The time stamp for the parent.
    uintptr_t   parentSignature;      // The signature for the parent.
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
    void        *originalAddress;       // The base address when the segment was written.
} SavedStateSegmentDescr;

#define SSF_WRITABLE    1               // The segment contains mutable data
#define SSF_OVERWRITE   2               // The segment overwrites the data (mutable) in a parent.
#define SSF_NOOVERWRITE 4               // The segment must not be further overwritten
#define SSF_BYTES       8               // The segment containing only byte data

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

/*
 *  Hierarchy table: contains information about last loaded or saved state.
 */

// Pointer to list of files loaded in last load.
// There's no need for a lock since the update is only made when all
// the ML threads have stopped.
class HierarchyTable
{
public:
    HierarchyTable(const char *file, time_t time):
      fileName(strdup(file)), timeStamp(time) { }
    AutoFree<char*> fileName;
    time_t          timeStamp;
};

HierarchyTable **hierarchyTable;

static unsigned hierarchyDepth;

static bool AddHierarchyEntry(const char *fileName, time_t timeStamp)
{
    // Add an entry to the hierarchy table for this file.
    HierarchyTable *newEntry = new HierarchyTable(fileName, timeStamp);
    if (newEntry == 0) return false;
    HierarchyTable **newTable =
        (HierarchyTable **)realloc(hierarchyTable, sizeof(HierarchyTable *)*(hierarchyDepth+1));
    if (newTable == 0) return false;
    hierarchyTable = newTable;
    hierarchyTable[hierarchyDepth++] = newEntry;
    return true;
}

// Test whether we're overwriting a parent of ourself.
#ifdef HAVE_WINDOWS_H
static bool sameFile(const char *x, const char *y)
{
    // Get the lengths and return if either does not exist.
    LPSTR filePart;
    DWORD dwxLen = GetFullPathName(x, 1, 0, 0);
    if (dwxLen == 0) return false;
    DWORD dwyLen = GetFullPathName(y, 0, 0, 0);
    if (dwyLen == 0) return false;
    if (dwxLen != dwyLen) return false;
    AutoFree<char*> xName = (char*)malloc(dwxLen+1);
    GetFullPathName(x, dwxLen+1, xName, &filePart);
    AutoFree<char*> yName = (char*)malloc(dwyLen+1);
    GetFullPathName(y, dwyLen+1, yName, &filePart);
    return strcmpi(xName, yName) == 0;
}
#else
static bool sameFile(const char *x, const char *y)
{
    struct stat xStat, yStat;
    // If either file does not exist that's fine.
    if (stat(x, &xStat) != 0 || stat(y, &yStat) != 0)
        return false;
    return (xStat.st_dev == yStat.st_dev && xStat.st_ino == yStat.st_ino);
}
#endif

/*
 *  Saving state.
 */

// This class is used to create the relocations.  It uses Exporter
// for this but this may perhaps be too heavyweight.
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

    friend class SaveRequest;
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
    reloc.targetSegment = (unsigned)memTable[addrArea].mtIndex;
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
    reloc.targetSegment = (unsigned)memTable[aArea].mtIndex;
    reloc.relKind = code;
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;
}

// Request to the main thread to save data.
class SaveRequest: public MainThreadRequest
{
public:
    SaveRequest(const char *name, unsigned h): MainThreadRequest(MTP_SAVESTATE),
        fileName(name), newHierarchy(h),
        errorMessage(0), errCode(0) {}

    virtual void Perform();
    const char *fileName;
    unsigned newHierarchy;
    const char *errorMessage;
    int errCode;
};

// This class is used to update references to objects that have moved.  If
// we have copied an object into the area to be exported we may still have references
// to it from the stack or from RTS data structures.  We have to ensure that these
// are updated.
// This is very similar to ProcessFixupAddress in sharedata.cpp
class SaveFixupAddress: public ScanAddress
{
protected:
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);
    virtual PolyObject *ScanObjectAddress(PolyObject *base)
        { return GetNewAddress(base).AsObjPtr(); }
    PolyWord GetNewAddress(PolyWord old);
};


POLYUNSIGNED SaveFixupAddress::ScanAddressAt(PolyWord *pt)
{
    *pt = GetNewAddress(*pt);
    return 0;
}

// Returns the new address if the argument is the address of an object that
// has moved, otherwise returns the original.
PolyWord SaveFixupAddress::GetNewAddress(PolyWord old)
{
    if (old.IsTagged() || old == PolyWord::FromUnsigned(0) || gMem.IsIOPointer(old.AsAddress()))
        return old; //  Nothing to do.

    // When we are updating addresses in the stack or in code segments we may have
    // code pointers.
    if (old.IsCodePtr())
    {
        // Find the start of the code segment
        PolyObject *oldObject = ObjCodePtrToPtr(old.AsCodePtr());
        // Calculate the byte offset of this value within the code object.
        POLYUNSIGNED offset = old.AsCodePtr() - (byte*)oldObject;
        PolyWord newObject = GetNewAddress(oldObject);
        return PolyWord::FromCodePtr(newObject.AsCodePtr() + offset);
    }

    ASSERT(old.IsDataPtr());

    PolyObject *obj = old.AsObjPtr();
    
    if (obj->ContainsForwardingPtr()) // tombstone is a pointer to a moved object
    {
        PolyObject *newp = obj->GetForwardingPtr();
        ASSERT (newp->ContainsNormalLengthWord());
        return newp;
    }
    
    ASSERT (obj->ContainsNormalLengthWord()); // object is not moved
    return old;
}

// Called by the root thread to actually save the state and write the file.
void SaveRequest::Perform()
{
    // Check that we aren't overwriting our own parent.
    for (unsigned q = 0; q < newHierarchy-1; q++) {
        if (sameFile(hierarchyTable[q]->fileName, fileName))
        {
            errorMessage = "File being saved is used as a parent of this file";
            errCode = 0;
            return;
        }
    }

    SaveStateExport exports;
    // Open the file.  This could quite reasonably fail if the path is wrong.
    exports.exportFile = fopen(fileName, "wb");
    if (exports.exportFile == NULL)
    {
        errorMessage = "Cannot open save file";
        errCode = errno;
        return;
    }

    // Scan over the permanent mutable area copying all reachable data that is
    // not in a lower hierarchy into new permanent segments.
    CopyScan copyScan(newHierarchy);
    copyScan.initialise(false);
    bool success = true;
    try {
        for (unsigned i = 0; i < gMem.npSpaces; i++)
        {
            PermanentMemSpace *space = gMem.pSpaces[i];
            if (space->isMutable && ! space->noOverwrite && ! space->byteOnly)
                copyScan.ScanAddressesInRegion(space->bottom, space->top);
        }
    }
    catch (MemoryException)
    {
        success = false;
    }

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
            entry->mtLength = (space->topPointer-space->bottom)*sizeof(PolyWord);
            entry->mtIndex = space->index;
            if (space->isMutable)
            {
                entry->mtFlags = MTF_WRITEABLE;
                if (space->noOverwrite) entry->mtFlags |= MTF_NO_OVERWRITE;
                if (space->byteOnly) entry->mtFlags |= MTF_BYTES;
            }
            else
                entry->mtFlags = MTF_EXECUTABLE;
        }
    }
    unsigned permanentEntries = memTableCount; // Remember where new entries start.

    // Newly created spaces.
    for (unsigned i = 0; i < gMem.neSpaces; i++)
    {
        memoryTableEntry *entry = &exports.memTable[memTableCount++];
        PermanentMemSpace *space = gMem.eSpaces[i];
        entry->mtAddr = space->bottom;
        entry->mtLength = (space->topPointer-space->bottom)*sizeof(PolyWord);
        entry->mtIndex = space->index;
        if (space->isMutable)
        {
            entry->mtFlags = MTF_WRITEABLE;
            if (space->noOverwrite) entry->mtFlags |= MTF_NO_OVERWRITE;
            if (space->byteOnly) entry->mtFlags |= MTF_BYTES;
        }
        else
            entry->mtFlags = MTF_EXECUTABLE;
    }

    exports.memTableEntries = memTableCount;
    exports.ioSpacing = IO_SPACING;

    // Update references to moved objects.
    SaveFixupAddress fixup;
    for (unsigned l = 0; l < gMem.nlSpaces; l++)
    {
        LocalMemSpace *space = gMem.lSpaces[l];
        fixup.ScanAddressesInRegion(space->bottom, space->lowerAllocPtr);
        fixup.ScanAddressesInRegion(space->upperAllocPtr, space->top);
    }
    GCModules(&fixup);

    // Update the global memory space table.  Old segments at the same level
    // or lower are removed.  The new segments become permanent.
    // Try to promote the spaces even if we've had a failure because export
    // spaces are deleted in ~CopyScan and we may have already copied
    // some objects there.
    if (! gMem.PromoteExportSpaces(newHierarchy) || ! success)
    {
        errorMessage = "Out of Memory";
        errCode = ENOMEM;
        return;
    }
    // Remove any deeper entries from the hierarchy table.
    while (hierarchyDepth > newHierarchy-1)
    {
        hierarchyDepth--;
        delete(hierarchyTable[hierarchyDepth]);
        hierarchyTable[hierarchyDepth] = 0;
    }

    // Write out the file header.
    SavedStateHeader saveHeader;
    memset(&saveHeader, 0, sizeof(saveHeader));
    saveHeader.headerLength = sizeof(saveHeader);
    strncpy(saveHeader.headerSignature,
        SAVEDSTATESIGNATURE, sizeof(saveHeader.headerSignature));
    saveHeader.headerVersion = SAVEDSTATEVERSION;
    saveHeader.segmentDescrLength = sizeof(SavedStateSegmentDescr);
    if (newHierarchy == 1)
        saveHeader.parentTimeStamp = exportTimeStamp;
    else
    {
        saveHeader.parentTimeStamp = hierarchyTable[newHierarchy-2]->timeStamp;
        saveHeader.parentNameEntry = 1; // Always the first entry.
    }
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
        descrs[j].segmentIndex = (unsigned)entry->mtIndex;
        descrs[j].segmentSize = entry->mtLength; // Set this even if we don't write it.
        descrs[j].originalAddress = entry->mtAddr;
        if (entry->mtFlags & MTF_WRITEABLE)
        {
            descrs[j].segmentFlags |= SSF_WRITABLE;
            if (entry->mtFlags & MTF_NO_OVERWRITE)
                descrs[j].segmentFlags |= SSF_NOOVERWRITE;
            if (j < permanentEntries && (entry->mtFlags & MTF_NO_OVERWRITE) == 0)
                descrs[j].segmentFlags |= SSF_OVERWRITE;
            if (entry->mtFlags & MTF_BYTES)
                descrs[j].segmentFlags |= SSF_BYTES;
        }
    }
    // Write out temporarily. Will be overwritten at the end.
    saveHeader.segmentDescr = ftell(exports.exportFile);
    fwrite(descrs, sizeof(SavedStateSegmentDescr), exports.memTableEntries, exports.exportFile);

    // Write out the relocations and the data.
    for (unsigned k = 1 /* Not IO area */; k < exports.memTableEntries; k++)
    {
        memoryTableEntry *entry = &exports.memTable[k];
        // Write out the contents if this is new or if it is a normal, overwritable
        // mutable area.
        if (k >= permanentEntries ||
            (entry->mtFlags & (MTF_WRITEABLE|MTF_NO_OVERWRITE)) == MTF_WRITEABLE)
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
                // Most relocations can be computed when the saved state is
                // loaded so we only write out the difficult ones: those that
                // occur within compiled code.
                //  exports.relocateObject(obj);
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

    // If this is a child we need to write a string table containing the parent name.
    if (newHierarchy > 1)
    {
        saveHeader.stringTable = ftell(exports.exportFile);
        fputc(0, exports.exportFile); // First byte of string table is zero
        fputs(hierarchyTable[newHierarchy-2]->fileName, exports.exportFile);
        fputc(0, exports.exportFile); // A terminating null.
        saveHeader.stringTableSize = strlen(hierarchyTable[newHierarchy-2]->fileName) + 2;
    }

    // Rewrite the header and the segment tables now they're complete.
    fseek(exports.exportFile, 0, SEEK_SET);
    fwrite(&saveHeader, sizeof(saveHeader), 1, exports.exportFile);
    fwrite(descrs, sizeof(SavedStateSegmentDescr), exports.memTableEntries, exports.exportFile);

    // Add an entry to the hierarchy table for this file.
    (void)AddHierarchyEntry(fileName, saveHeader.timeStamp);

    delete[](descrs);
}

Handle SaveState(TaskData *taskData, Handle args)
{
    char fileNameBuff[MAXPATHLEN];
    POLYUNSIGNED length =
        Poly_string_to_C(DEREFHANDLE(args)->Get(0), fileNameBuff, MAXPATHLEN);
    if (length > MAXPATHLEN)
        raise_syscall(taskData, "File name too long", ENAMETOOLONG);
    // The value of depth is zero for top-level save so we need to add one for hierarchy.
    unsigned newHierarchy = get_C_unsigned(taskData, DEREFHANDLE(args)->Get(1)) + 1;

    if (newHierarchy > hierarchyDepth+1)
        raise_fail(taskData, "Depth must be no more than the current hierarchy plus one");

    // Request a full GC first.  The main reason is to avoid running out of memory as a
    // result of repeated saves.  Old export spaces are turned into local spaces and
    // the GC will delete them if they are completely empty
    FullGC(taskData);

    SaveRequest request(fileNameBuff, newHierarchy);
    processes->MakeRootRequest(taskData, &request);
    if (request.errorMessage)
        raise_syscall(taskData, request.errorMessage, request.errCode);
    return SAVE(TAGGED(0));
}

/*
 *  Loading saved state files.
 */

class StateLoader: public MainThreadRequest
{
public:
    StateLoader(const char *file): MainThreadRequest(MTP_LOADSTATE),
        errorResult(0), errNumber(0) { strcpy(fileName, file); }

    virtual void Perform(void);
    bool LoadFile(bool isInitial, time_t requiredStamp);
    const char *errorResult;
    // The fileName here is the last file loaded.  As well as using it
    // to load the name can also be printed out at the end to identify the
    // particular file in the hierarchy that failed.
    char fileName[MAXPATHLEN];
    int errNumber;
};

// Called by the main thread once all the ML threads have stopped.
void StateLoader::Perform(void)
{
    (void)LoadFile(true, 0);
}

// This class is used to relocate addresses in areas that have been loaded.
class LoadRelocate
{
public:
    LoadRelocate(): descrs(0), nDescrs(0), errorMessage(0) {}
    ~LoadRelocate();

    void RelocateObject(PolyObject *p);
    void RelocateAddressAt(PolyWord *pt);

    SavedStateSegmentDescr *descrs;
    unsigned nDescrs;
    const char *errorMessage;
};

LoadRelocate::~LoadRelocate()
{
    if (descrs) delete[](descrs);
}

// Update the addresses in a group of words.
void LoadRelocate::RelocateAddressAt(PolyWord *pt)
{
    PolyWord val = *pt;

    if (val.IsTagged()) return;

    // Which segment is this address in?
    unsigned i;
    for (i = 0; i < nDescrs; i++)
    {
        SavedStateSegmentDescr *descr = &descrs[i];
        if (val.AsAddress() > descr->originalAddress &&
            val.AsAddress() <= (char*)descr->originalAddress + descr->segmentSize)
        {
            // It's in this segment: relocate it to the current position.
            MemSpace *space =
                descr->segmentIndex == 0 ? gMem.IoSpace() : gMem.SpaceForIndex(descr->segmentIndex);
            // Error if this doesn't match.
            byte *setAddress = (byte*)space->bottom + ((char*)val.AsAddress() - (char*)descr->originalAddress);
            *pt = PolyWord::FromCodePtr(setAddress);
            break;
        }
    }
    if (i == nDescrs)
    {
        // Error: Not found.
        errorMessage = "Unmatched address";
    }
}

// This is based on Exporter::relocateObject but does the reverse.
// It attempts to adjust all the addresses in the object when it has
// been read in.
void LoadRelocate::RelocateObject(PolyObject *p)
{
    if (p->IsByteObject())
    {
    }
    else if (p->IsCodeObject())
    {
        POLYUNSIGNED constCount;
        PolyWord *cp;
        ASSERT(! p->IsMutable() );
        p->GetConstSegmentForCode(cp, constCount);
        /* Now the constant area. */
        for (POLYUNSIGNED i = 0; i < constCount; i++) RelocateAddressAt(&(cp[i]));
        // N.B. This does not deal with constants within the code.  These have
        // to be handled by real relocation entries.
    }
    else /* Ordinary objects, essentially tuples. */
    {
        POLYUNSIGNED length = p->Length();
        for (POLYUNSIGNED i = 0; i < length; i++) RelocateAddressAt(p->Offset(i));
    }
}

// Load a saved state file.  Calls itself to handle parent files.
bool StateLoader::LoadFile(bool isInitial, time_t requiredStamp)
{
    LoadRelocate relocate;
    AutoFree<char*> thisFile(strdup(fileName));

    AutoClose loadFile(fopen(fileName, "rb"));
    if ((FILE*)loadFile == NULL)
    {
        errorResult = "Cannot open load file";
        errNumber = errno;
        return false;
    }

    SavedStateHeader header;
    // Read the header and check the signature.
    if (fread(&header, sizeof(SavedStateHeader), 1, loadFile) != 1)
    {
        errorResult = "Unable to load header";
        return false;
    }
    if (strncmp(header.headerSignature, SAVEDSTATESIGNATURE, sizeof(header.headerSignature)) != 0)
    {
        errorResult = "File is not a saved state";
        return false;
    }
    if (header.headerVersion != SAVEDSTATEVERSION ||
        header.headerLength != sizeof(SavedStateHeader) ||
        header.segmentDescrLength != sizeof(SavedStateSegmentDescr))
    {
        errorResult = "Unsupported version of saved state file";
        return false;
    }

    // Check that we have the required stamp before loading any children.
    // If a parent has been overwritten we could get a loop.
    if (! isInitial && header.timeStamp != requiredStamp)
    {
        // Time-stamps don't match.
        errorResult = "The parent for this saved state does not match or has been changed";
        return false;
    }

    // Have verified that this is a reasonable saved state file.  If it isn't a
    // top-level file we have to load the parents first.
    if (header.parentNameEntry != 0)
    {
        size_t toRead = header.stringTableSize-header.parentNameEntry;
        if (MAXPATHLEN < toRead) toRead = MAXPATHLEN;

        if (header.parentNameEntry >= header.stringTableSize /* Bad entry */ ||
            fseek(loadFile, header.stringTable + header.parentNameEntry, SEEK_SET) != 0 ||
            fread(fileName, 1, toRead, loadFile) != toRead)
        {
            errorResult = "Unable to read parent file name";
            return false;
        }
        fileName[toRead] = 0; // Should already be null-terminated, but just in case.

        if (! LoadFile(false, header.parentTimeStamp))
            return false;

        // Check the parent time stamp.
        ASSERT(hierarchyDepth > 0 && hierarchyTable[hierarchyDepth-1] != 0);
    }
    else // Top-level file
    {
        if (header.parentTimeStamp != exportTimeStamp)
        {
            // Time-stamp does not match executable.
            errorResult = 
                    "Saved state was exported from a different executable or the executable has changed";
            return false;
        }

        // Any existing spaces at this level or greater must be turned
        // into local spaces.  We may have references from the stack to objects that
        // have previously been imported but otherwise these spaces are no longer
        // needed.
        gMem.DemoteImportSpaces();
        // Clean out the hierarchy table.
        for (unsigned h = 0; h < hierarchyDepth; h++)
        {
            delete(hierarchyTable[h]);
            hierarchyTable[h] = 0;
        }
        hierarchyDepth = 0;
    }

    // Now have a valid, matching saved state.
    // Load the segment descriptors.
    relocate.nDescrs = header.segmentDescrCount;
    relocate.descrs = new SavedStateSegmentDescr[relocate.nDescrs];

    if (fseek(loadFile, header.segmentDescr, SEEK_SET) != 0 ||
        fread(relocate.descrs, sizeof(SavedStateSegmentDescr), relocate.nDescrs, loadFile) != relocate.nDescrs)
    {
        errorResult = "Unable to read segment descriptors";
        return false;
    }

    // Read in and create the new segments first.  If we have problems,
    // in particular if we have run out of memory, then it's easier to recover.  
    for (unsigned i = 0; i < relocate.nDescrs; i++)
    {
        SavedStateSegmentDescr *descr = &relocate.descrs[i];
        MemSpace *space =
            descr->segmentIndex == 0 ? gMem.IoSpace() : gMem.SpaceForIndex(descr->segmentIndex);

        if (descr->segmentData == 0)
        { // No data - just an entry in the index.
            if (space == NULL/* ||
                descr->segmentSize != (size_t)((char*)space->top - (char*)space->bottom)*/)
            {
                errorResult = "Mismatch for existing memory space";
                return false;
            }
        }
        else if ((descr->segmentFlags & SSF_OVERWRITE) == 0)
        { // New segment.
            if (space != NULL)
            {
                errorResult = "Segment already exists";
                return false;
            }
            // Allocate memory for the new segment.
            size_t actualSize = descr->segmentSize;
            PolyWord *mem  =
                (PolyWord*)osMemoryManager->Allocate(actualSize,
                                PERMISSION_READ|PERMISSION_WRITE|PERMISSION_EXEC);
            if (mem == 0)
            {
                errorResult = "Unable to allocate memory";
                return false;
            }
            if (fseek(loadFile, descr->segmentData, SEEK_SET) != 0 ||
                fread(mem, descr->segmentSize, 1, loadFile) != 1)
            {
                errorResult = "Unable to read segment";
                osMemoryManager->Free(mem, descr->segmentSize);
                return false;
            }
            // Fill unused space to the top of the area.
            gMem.FillUnusedSpace(mem+descr->segmentSize/sizeof(PolyWord),
                (actualSize-descr->segmentSize)/sizeof(PolyWord));
            // At the moment we leave all segments with write access.
            unsigned mFlags =
                (descr->segmentFlags & SSF_WRITABLE ? MTF_WRITEABLE : 0) |
                (descr->segmentFlags & SSF_NOOVERWRITE ? MTF_NO_OVERWRITE : 0) |
                (descr->segmentFlags & SSF_BYTES ? MTF_BYTES : 0);
            space = gMem.NewPermanentSpace(mem, actualSize / sizeof(PolyWord), mFlags,
                        descr->segmentIndex, hierarchyDepth+1);
        }
    }

    // Now read in the mutable overwrites and relocate.

    for (unsigned j = 0; j < relocate.nDescrs; j++)
    {
        SavedStateSegmentDescr *descr = &relocate.descrs[j];
        MemSpace *space =
            descr->segmentIndex == 0 ? gMem.IoSpace() : gMem.SpaceForIndex(descr->segmentIndex);
        ASSERT(space != NULL); // We should have created it.
        if (descr->segmentFlags & SSF_OVERWRITE)
        {
            if (fseek(loadFile, descr->segmentData, SEEK_SET) != 0 ||
                fread(space->bottom, descr->segmentSize, 1, loadFile) != 1)
            {
                errorResult = "Unable to read segment";
                return false;
            }
        }

        // Relocation.
        if (descr->segmentData != 0)
        {
            // Adjust the addresses in the loaded segment.
            for (PolyWord *p = space->bottom; p < space->top; )
            {
                p++;
                PolyObject *obj = (PolyObject*)p;
                POLYUNSIGNED length = obj->Length();
                relocate.RelocateObject(obj);
                p += length;
            }
        }

        // Process explicit relocations.
        // If we get errors just skip the error and continue rather than leave
        // everything in an unstable state.
        if (descr->relocations)
        {
            if (fseek(loadFile, descr->relocations, SEEK_SET) != 0)
            {
                errorResult = "Unable to read relocation segment";
            }
            for (unsigned k = 0; k < descr->relocationCount; k++)
            {
                RelocationEntry reloc;
                if (fread(&reloc, sizeof(reloc), 1, loadFile) != 1)
                {
                    errorResult = "Unable to read relocation segment";
                }
                MemSpace *toSpace =
                    reloc.targetSegment == 0 ? gMem.IoSpace() : gMem.SpaceForIndex(reloc.targetSegment);
                if (toSpace == NULL)
                {
                    errorResult = "Unknown space reference in relocation";
                    continue;
                }
                byte *setAddress = (byte*)space->bottom + reloc.relocAddress;
                byte *targetAddress = (byte*)toSpace->bottom + reloc.targetAddress;
                if (setAddress >= (byte*)space->top || targetAddress >= (byte*)toSpace->top)
                {
                    errorResult = "Bad relocation";
                    continue;
                }
                ScanAddress::SetConstantValue(setAddress, PolyWord::FromCodePtr(targetAddress), reloc.relKind);
            }
        }
    }

    // Add an entry to the hierarchy table for this file.
    if (! AddHierarchyEntry(thisFile, header.timeStamp))
        return false;

    return true; // Succeeded
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

    StateLoader loader(fileNameBuff);

    // Request the main thread to do the load.  This may set the error string if it failed.
    processes->MakeRootRequest(taskData, &loader);

    if (loader.errorResult != 0)
    {
        if (loader.errNumber == 0)
            raise_fail(taskData, loader.errorResult);
        else
        {
            char buff[MAXPATHLEN+100];
            strcpy(buff, loader.errorResult);
            strcat(buff, ": ");
            strcat(buff, loader.fileName);
            raise_syscall(taskData, buff, loader.errNumber);
        }
    }

    return SAVE(TAGGED(0));
}

/*
 *  Additional functions to provide information or change saved-state files.
 */

// These functions do not affect the global state so can be executed by
// the ML threads directly.

Handle ShowHierarchy(TaskData *taskData)
// Return the list of files in the hierarchy.
{
    Handle saved = taskData->saveVec.mark();
    Handle list  = SAVE(ListNull);

    // Process this in reverse order.
    for (unsigned i = hierarchyDepth; i > 0; i--)
    {
        Handle value = SAVE(C_string_to_Poly(taskData, hierarchyTable[i-1]->fileName));
        Handle next  = alloc_and_save(taskData, sizeof(ML_Cons_Cell)/sizeof(PolyWord));
        DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(value); 
        DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);
        taskData->saveVec.reset(saved);
        list = SAVE(DEREFHANDLE(next));
    }
    return list;
}

Handle RenameParent(TaskData *taskData, Handle args)
// Change the name of the immediate parent stored in a child
{
    char fileNameBuff[MAXPATHLEN], parentNameBuff[MAXPATHLEN];
    // The name of the file to modify.
    POLYUNSIGNED fileLength =
        Poly_string_to_C(DEREFHANDLE(args)->Get(0), fileNameBuff, MAXPATHLEN);
    if (fileLength > MAXPATHLEN)
        raise_syscall(taskData, "File name too long", ENAMETOOLONG);
    // The new parent name to insert.
    POLYUNSIGNED parentLength =
        Poly_string_to_C(DEREFHANDLE(args)->Get(1), parentNameBuff, MAXPATHLEN);
    if (parentLength > MAXPATHLEN)
        raise_syscall(taskData, "Parent name too long", ENAMETOOLONG);

    AutoClose loadFile(fopen(fileNameBuff, "r+b")); // Open for reading and writing
    if ((FILE*)loadFile == NULL)
    {
        char buff[MAXPATHLEN+1+23];
        strcpy(buff, "Cannot open load file: ");
        strcat(buff, fileNameBuff);
        raise_syscall(taskData, buff, errno);
    }

    SavedStateHeader header;
    // Read the header and check the signature.
    if (fread(&header, sizeof(SavedStateHeader), 1, loadFile) != 1)
        raise_fail(taskData, "Unable to load header");

    if (strncmp(header.headerSignature, SAVEDSTATESIGNATURE, sizeof(header.headerSignature)) != 0)
        raise_fail(taskData, "File is not a saved state");

    if (header.headerVersion != SAVEDSTATEVERSION ||
        header.headerLength != sizeof(SavedStateHeader) ||
        header.segmentDescrLength != sizeof(SavedStateSegmentDescr))
    {
        raise_fail(taskData, "Unsupported version of saved state file");
    }

    // Does this actually have a parent?
    if (header.parentNameEntry == 0)
        raise_fail(taskData, "File does not have a parent");

    // At the moment the only entry in the string table is the parent
    // name so we can simply write a new one on the end of the file.
    // This makes the file grow slightly each time but it shouldn't be
    // significant.
    fseek(loadFile, 0, SEEK_END);
    header.stringTable = ftell(loadFile); // Remember where this is
    fputc(0, loadFile); // First byte of string table is zero
    fputs(parentNameBuff, loadFile);
    fputc(0, loadFile); // A terminating null.
    header.stringTableSize = strlen(parentNameBuff) + 2;

    // Now rewind and write the header with the revised string table.
    fseek(loadFile, 0, SEEK_SET);
    fwrite(&header, sizeof(header), 1, loadFile);

    return SAVE(TAGGED(0));
}

Handle ShowParent(TaskData *taskData, Handle hFileName)
// Return the name of the immediate parent stored in a child
{
    char fileNameBuff[MAXPATHLEN+1];
    POLYUNSIGNED length =
        Poly_string_to_C(DEREFHANDLE(hFileName), fileNameBuff, MAXPATHLEN);
    if (length > MAXPATHLEN)
        raise_syscall(taskData, "File name too long", ENAMETOOLONG);

    AutoClose loadFile(fopen(fileNameBuff, "rb"));
    if ((FILE*)loadFile == NULL)
    {
        char buff[MAXPATHLEN+1+23];
        strcpy(buff, "Cannot open load file: ");
        strcat(buff, fileNameBuff);
        raise_syscall(taskData, buff, errno);
    }

    SavedStateHeader header;
    // Read the header and check the signature.
    if (fread(&header, sizeof(SavedStateHeader), 1, loadFile) != 1)
        raise_fail(taskData, "Unable to load header");

    if (strncmp(header.headerSignature, SAVEDSTATESIGNATURE, sizeof(header.headerSignature)) != 0)
        raise_fail(taskData, "File is not a saved state");

    if (header.headerVersion != SAVEDSTATEVERSION ||
        header.headerLength != sizeof(SavedStateHeader) ||
        header.segmentDescrLength != sizeof(SavedStateSegmentDescr))
    {
        raise_fail(taskData, "Unsupported version of saved state file");
    }

    // Does this have a parent?
    if (header.parentNameEntry != 0)
    {
        char parentFileName[MAXPATHLEN+1];
        size_t toRead = header.stringTableSize-header.parentNameEntry;
        if (MAXPATHLEN < toRead) toRead = MAXPATHLEN;

        if (header.parentNameEntry >= header.stringTableSize /* Bad entry */ ||
            fseek(loadFile, header.stringTable + header.parentNameEntry, SEEK_SET) != 0 ||
            fread(parentFileName, 1, toRead, loadFile) != toRead)
        {
            raise_fail(taskData, "Unable to read parent file name");
        }
        parentFileName[toRead] = 0; // Should already be null-terminated, but just in case.
        // Convert the name into a Poly string and then build a "Some" value.
        // It's possible, although silly, to have the empty string as a parent name.
        Handle resVal = SAVE(C_string_to_Poly(taskData, parentFileName));
        Handle result = alloc_and_save(taskData, 1);
        DEREFHANDLE(result)->Set(0, DEREFWORDHANDLE(resVal));
        return result;
    }
    else return SAVE(NONE_VALUE);
}

