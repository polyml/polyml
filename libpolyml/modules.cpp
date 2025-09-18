/*
    Title:  modules.cpp - Load and save modules

    Copyright (c) 2015, 2017-19, 2021, 2025 David C.J. Matthews

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


#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include "globals.h"
#include "modules.h"
#include "rtsentry.h"
#include "processes.h"
#include "polystring.h"
#include "run_time.h"
#include "timing.h" // For getBuildTime
#include "machine_dep.h"
#include "exporter.h" // For CopyScan
#include "scanaddrs.h"
#include "memmgr.h"
#include "savestate.h"

#include "mpoly.h" // For exportTimeStamp - This may no longer be needed

#include "../polyexports.h"

#define SAVE(x) taskData->saveVec.push(x)

#if (defined(_WIN32))
#include <tchar.h>
#define ERRORNUMBER _doserrno
#define NOMEMORY ERROR_NOT_ENOUGH_MEMORY
#else
typedef char TCHAR;
#define _T(x) x
#define _tfopen fopen
#define _tcslen strlen
#define ERRORNUMBER errno
#define NOMEMORY ENOMEM
#endif

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyStoreModule(POLYUNSIGNED threadId, POLYUNSIGNED name, POLYUNSIGNED contents);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyLoadModule(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetModuleDirectory(POLYUNSIGNED threadId);
}

// This is probably generally useful so may be moved into
// a general header file.
template<typename BASE> class AutoFree
{
public:
    AutoFree(BASE p = 0) : m_value(p) {}
    ~AutoFree() { free(m_value); }

    // Automatic conversions to the base type.
    operator BASE() { return m_value; }
    BASE operator = (BASE p) { return (m_value = p); }

private:
    BASE m_value;
};

// Module system
#define MODULESIGNATURE "POLYMODU"
#define MODULEVERSION   3

typedef struct _moduleHeader
{
    // These entries are primarily to check that we have a valid
    // saved state file before we try to interpret anything else.
    char        headerSignature[8];     // Should contain MODULESIGNATURE
    unsigned    headerVersion;          // Should contain MODULEVERSION
    unsigned    headerLength;           // Number of bytes in the header
    unsigned    segmentDescrLength;     // Number of bytes in a descriptor

    // These entries contain the real data.
    off_t       segmentDescr;           // Position of segment descriptor table
    unsigned    segmentDescrCount;      // Number of segment descriptors in the table
    struct _moduleId timeStamp;              // The time stamp for this file.
    struct _moduleId executableTimeStamp;    // The time stamp for the parent executable.
    // Root
    uintptr_t   rootSegment;
    POLYUNSIGNED     rootOffset;
} ModuleHeader;

// Entry for segment table.  This describes the segments on the disc that
// need to be loaded into memory.
typedef struct _moduleSegmentDescr
{
    off_t       segmentData;            // Position of the segment data
    size_t      segmentSize;            // Size of the segment data
    off_t       relocations;            // Position of the relocation table
    unsigned    relocationCount;        // Number of entries in relocation table
    unsigned    relocationSize;         // Size of a relocation entry
    unsigned    segmentFlags;           // Segment flags (see MSF_ values)
    unsigned    segmentIndex;           // The index of this segment or the segment it overwrites
    struct _moduleId moduleIden;               // The module this came from.
} ModuleSegmentDescr;

#define MSF_WRITABLE    1               // The segment contains mutable data
#define MSF_OVERWRITE   2               // The segment overwrites the data (mutable) in a parent.
#define MSF_NOOVERWRITE 4               // The segment must not be further overwritten
#define MSF_BYTES       8               // The segment contains only byte data
#define MSF_CODE        16              // The segment contains only code

typedef struct _modrelocationEntry
{
    // Each entry indicates a location that has to be set to an address.
    // The location to be set is determined by adding "relocAddress" to the base address of
    // this segment (the one to which these relocations apply) and the value to store
    // by adding "targetAddress" to the base address of the segment indicated by "targetSegment".
    POLYUNSIGNED    relocAddress;       // The (byte) offset in this segment that we will set
    POLYUNSIGNED    targetAddress;      // The value to add to the base of the destination segment
    unsigned        targetSegment;      // The base segment.  0 is IO segment. 
    ScanRelocationKind relKind;         // The kind of relocation (processor dependent).
} ModRelocationEntry;

// Store a module
class ModuleStorer : public MainThreadRequest
{
public:
    ModuleStorer(const TCHAR* file, Handle r) :
        MainThreadRequest(MTP_STOREMODULE), fileName(file), root(r), errorMessage(0), errCode(0) {}

    virtual void Perform();

    const TCHAR* fileName;
    Handle root;
    const char* errorMessage;
    int errCode;
};

class ModuleExport : public Exporter, public ScanAddress
{
public:
    ModuleExport() : relocationCount(0) {}
    void RunModuleExport(PolyObject* rootFunction);
    virtual void exportStore(void) {}

protected:
    // These have to be overridden to work properly for compact 32-bit
    virtual void relocateValue(PolyWord* pt);
    virtual void relocateObject(PolyObject* p);

private:
    // ScanAddress overrides
    virtual void ScanConstant(PolyObject* base, byte* addrOfConst, ScanRelocationKind code, intptr_t displacement);
    // At the moment we should only get calls to ScanConstant.
    virtual PolyObject* ScanObjectAddress(PolyObject* base) { return base; }

protected:
    void setRelocationAddress(void* p, POLYUNSIGNED* reloc);
    PolyWord createRelocation(PolyWord p, void* relocAddr); // Override for Exporter
    void createActualRelocation(void* addr, void* relocAddr, ScanRelocationKind kind);
    unsigned relocationCount;

    friend class SaveRequest;
};


// Generate the address relative to the start of the segment.
void ModuleExport::setRelocationAddress(void* p, POLYUNSIGNED* reloc)
{
    unsigned area = findArea(p);
    POLYUNSIGNED offset = (POLYUNSIGNED)((char*)p - (char*)memTable[area].mtOriginalAddr);
    *reloc = offset;
}

// Create a relocation entry for an address at a given location.
// In compact-32 bit mode this will only be called for modules.
void ModuleExport::createActualRelocation(void* addr, void* relocAddr, ScanRelocationKind kind)
{
    ModRelocationEntry reloc;
    // Set the offset within the section we're scanning.
    setRelocationAddress(relocAddr, &reloc.relocAddress);

    unsigned addrArea = findArea(addr);
    reloc.targetAddress = (POLYUNSIGNED)((char*)addr - (char*)memTable[addrArea].mtOriginalAddr);
    // Module exports use the offset in their own table.
    reloc.targetSegment = addrArea;
    reloc.relKind = kind;
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;
}

PolyWord ModuleExport::createRelocation(PolyWord p, void* relocAddr)
{
#ifdef POLYML32IN64
    createActualRelocation(p.AsAddress(), relocAddr, PROCESS_RELOC_C32ADDR);
#else
    createActualRelocation(p.AsAddress(), relocAddr, PROCESS_RELOC_DIRECT);
#endif
    return p; // Don't change the contents
}

/* This is called for each constant within the code.
   Print a relocation entry for the word and return a value that means
   that the offset is saved in original word. */
void ModuleExport::ScanConstant(PolyObject* base, byte* addr, ScanRelocationKind code, intptr_t displacement)
{
    PolyObject* p = GetConstantValue(addr, code, displacement);

    if (p == 0)
        return;

    void* a = p;
    unsigned aArea = findArea(a);

    // We don't need a relocation if this is relative to the current segment
    // since the relative address will already be right.
    if (code == PROCESS_RELOC_I386RELATIVE && aArea == findArea(addr))
        return;

    // Set the value at the address to the offset relative to the symbol.
    ModRelocationEntry reloc;
    setRelocationAddress(addr, &reloc.relocAddress);
    reloc.targetAddress = (POLYUNSIGNED)((char*)a - (char*)memTable[aArea].mtOriginalAddr);
    reloc.targetSegment = aArea;
    reloc.relKind = code;
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;
}

// This is called by the initial thread to actually do the export.
void ModuleExport::RunModuleExport(PolyObject* rootFn)
{
    PolyObject* copiedRoot = 0;
    CopyScan copyScan(1); // Exclude the parent state

    try {
        copyScan.initialise();
        // Copy the root and everything reachable from it into the temporary area.
        copiedRoot = copyScan.ScanObjectAddress(rootFn);
    }
    catch (MemoryException&)
    {
        errorMessage = "Insufficient Memory";
        // Reset the forwarding pointers so that the original data structure is in
        // local storage.
        revertToLocal();
        return;
    }

    ModuleId expModuleId(copyScan.extractHash());

    // Copy the areas into the export object.
    size_t tableSize = gMem.eSpaces.size() + gMem.pSpaces.size();
    unsigned memEntry = 0;
    memTable = new ExportMemTable[tableSize];

    // If we're constructing a module we need to include the global spaces.
    // Permanent spaces from the dependencies
    for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
    {
        PermanentMemSpace* space = *i;
        if (space->hierarchy == 0 && copyScan.externalRefs[space->moduleIdentifier])
        {
            ExportMemTable* entry = &memTable[memEntry++];
            entry->mtOriginalAddr = entry->mtCurrentAddr = space->bottom;
            entry->mtLength = (space->topPointer - space->bottom) * sizeof(PolyWord);
            entry->mtIndex = space->index;
            entry->mtModId = space->moduleIdentifier;
            entry->mtFlags = 0;
            if (space->isMutable) entry->mtFlags |= MTF_WRITEABLE;
            if (space->isCode) entry->mtFlags |= MTF_EXECUTABLE;
        }
    }
    unsigned newAreas = memEntry;

    for (std::vector<PermanentMemSpace*>::iterator i = gMem.eSpaces.begin(); i < gMem.eSpaces.end(); i++)
    {
        ExportMemTable* entry = &memTable[memEntry++];
        PermanentMemSpace* space = *i;
        entry->mtOriginalAddr = entry->mtCurrentAddr = space->bottom;
        entry->mtLength = (space->topPointer - space->bottom) * sizeof(PolyWord);
        // The spaces need to be renumbered and the module id set.
        space->index = memEntry - newAreas - 1;
        space->moduleIdentifier = ModuleId(expModuleId);
        entry->mtIndex = space->index;
        entry->mtModId = space->moduleIdentifier;
        entry->mtFlags = 0;
        if (space->isMutable)
        {
            entry->mtFlags = MTF_WRITEABLE;
            if (space->noOverwrite) entry->mtFlags |= MTF_NO_OVERWRITE;
        }
        if (space->isCode && !space->constArea) entry->mtFlags |= MTF_EXECUTABLE;
        if (space->byteOnly) entry->mtFlags |= MTF_BYTES;
    }

    ASSERT(memEntry <= tableSize);
    memTableEntries = memEntry;
    rootFunction = copiedRoot;

    // Write the data out.  This handles memory exceptions but we should also
    // handle write failures.  That could happen if the disc became full.
    try {
        ModuleHeader modHeader;
        memset(&modHeader, 0, sizeof(modHeader));
        modHeader.headerLength = sizeof(modHeader);
        memcpy(modHeader.headerSignature,
            MODULESIGNATURE, sizeof(modHeader.headerSignature));
        modHeader.headerVersion = MODULEVERSION;
        modHeader.segmentDescrLength = sizeof(ModuleSegmentDescr);
        modHeader.executableTimeStamp = exportTimeStamp;
        {
            unsigned rootArea = findArea(this->rootFunction);
            ExportMemTable* mt = &memTable[rootArea];
            modHeader.rootSegment = rootArea;
            modHeader.rootOffset = (POLYUNSIGNED)((char*)this->rootFunction - (char*)mt->mtOriginalAddr);
        }
        modHeader.timeStamp = expModuleId;
        modHeader.segmentDescrCount = this->memTableEntries; // One segment for each space.
        // Write out the header.
        fwrite(&modHeader, sizeof(modHeader), 1, this->exportFile);

        ModuleSegmentDescr* descrs = new ModuleSegmentDescr[this->memTableEntries];
        // We need an entry in the descriptor tables for each segment in the executable because
        // we may have relocations that refer to addresses in it.
        for (unsigned j = 0; j < this->memTableEntries; j++)
        {
            ModuleSegmentDescr* thisDescr = &descrs[j];
            ExportMemTable* entry = &this->memTable[j];
            memset(thisDescr, 0, sizeof(ModuleSegmentDescr));
            thisDescr->relocationSize = sizeof(ModRelocationEntry);
            thisDescr->segmentIndex = entry->mtIndex;
            thisDescr->segmentSize = entry->mtLength; // Set this even if we don't write it.
            thisDescr->moduleIden = entry->mtModId;
            if (entry->mtFlags & MTF_WRITEABLE)
            {
                thisDescr->segmentFlags |= MSF_WRITABLE;
                if (entry->mtFlags & MTF_NO_OVERWRITE)
                    thisDescr->segmentFlags |= MSF_NOOVERWRITE;
                if ((entry->mtFlags & MTF_NO_OVERWRITE) == 0)
                    thisDescr->segmentFlags |= MSF_OVERWRITE;
                if (entry->mtFlags & MTF_BYTES)
                    thisDescr->segmentFlags |= MSF_BYTES;
            }
            if (entry->mtFlags & MTF_EXECUTABLE)
                thisDescr->segmentFlags |= MSF_CODE;
        }
        // Write out temporarily. Will be overwritten at the end.
        modHeader.segmentDescr = ftell(this->exportFile);
        fwrite(descrs, sizeof(ModuleSegmentDescr), this->memTableEntries, this->exportFile);

        // Write out the relocations and the data.
        for (unsigned k = 0; k < this->memTableEntries; k++)
        {
            ModuleSegmentDescr* thisDescr = &descrs[k];
            ExportMemTable* entry = &this->memTable[k];
            if (k >= newAreas) // Not permanent areas
            {
                thisDescr->relocations = ftell(this->exportFile);
                // Have to write this out.
                this->relocationCount = 0;
                // Create the relocation table.
                char* start = (char*)entry->mtOriginalAddr;
                char* end = start + entry->mtLength;
                for (PolyWord* p = (PolyWord*)start; p < (PolyWord*)end; )
                {
                    p++;
                    PolyObject* obj = (PolyObject*)p;
                    POLYUNSIGNED length = obj->Length();
                    // For saved states we don't include explicit relocations except
                    // in code but it's easier if we do for modules.
                    if (length != 0 && obj->IsCodeObject())
                        machineDependent->ScanConstantsWithinCode(obj, this);
                    relocateObject(obj);
                    p += length;
                }
                thisDescr->relocationCount = this->relocationCount;
                // Write out the data.
                thisDescr->segmentData = ftell(exportFile);
                fwrite(entry->mtOriginalAddr, entry->mtLength, 1, exportFile);
            }
        }

        // Rewrite the header and the segment tables now they're complete.
        fseek(exportFile, 0, SEEK_SET);
        fwrite(&modHeader, sizeof(modHeader), 1, exportFile);
        fwrite(descrs, sizeof(ModuleSegmentDescr), this->memTableEntries, exportFile);
        delete[](descrs);

        fclose(exportFile); exportFile = NULL;

        // If it all succeeded we can switch over to the permanent spaces.
        if (gMem.PromoteNewExportSpaces(0)) // Turn them into hierarchy zero.
            switchLocalsToPermanent();
        else revertToLocal();
    }
    catch (MemoryException&) {
        errorMessage = "Insufficient Memory";
        revertToLocal();
    }
}

void ModuleStorer::Perform()
{
    ModuleExport exporter;
#if (defined(_WIN32) && defined(UNICODE))
    exporter.exportFile = _wfopen(fileName, L"wb");
#else
    exporter.exportFile = fopen(fileName, "wb");
#endif
    if (exporter.exportFile == NULL)
    {
        errorMessage = "Cannot open export file";
        errCode = ERRORNUMBER;
        return;
    }
    // RunExport copies everything reachable from the root, except data from
    // the executable because we've set the hierarchy to 1, using CopyScan.
    // It builds the tables in the export data structure then calls exportStore
    // to actually write the data.
    if (!root->Word().IsDataPtr())
    {
        // If we have a completely empty module the list may be null.
        // This needs to be dealt with at a higher level.
        errorMessage = "Module root is not an address";
        return;
    }
    exporter.RunModuleExport(root->WordP());
    errorMessage = exporter.errorMessage; // This will be null unless there's been an error.
}

// Override for Exporter::relocateValue.  That function does not do anything for compact 32-bit because
// the operating system object module formats do not support a suitable relocation format.
void ModuleExport::relocateValue(PolyWord* pt)
{
    PolyWord q = *pt;
    if (IS_INT(q) || q == PolyWord::FromUnsigned(0)) {}
    else *gMem.SpaceForAddress(pt)->writeAble(pt) = createRelocation(*pt, pt);
}

// We need to override this since Exporter::relocateObject doesn't work 
void ModuleExport::relocateObject(PolyObject* p)
{
    if (p->IsClosureObject())
    {
        POLYUNSIGNED length = p->Length();
        // The first word is an absolute code address.
        PolyWord* pt = p->Offset(0);
        void* addr = *(void**)pt;
        createActualRelocation(addr, pt, PROCESS_RELOC_DIRECT);

        for (POLYUNSIGNED i = sizeof(uintptr_t) / sizeof(PolyWord); i < length; i++) relocateValue(p->Offset(i));
    }
    else Exporter::relocateObject(p);
}

// Store a module
POLYUNSIGNED PolyStoreModule(POLYUNSIGNED threadId, POLYUNSIGNED name, POLYUNSIGNED contents)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedContents = taskData->saveVec.push(contents);

    try {
        TempString fileName(PolyWord::FromUnsigned(name));
        ModuleStorer storer(fileName, pushedContents);
        processes->MakeRootRequest(taskData, &storer);
        if (storer.errorMessage)
            raise_syscall(taskData, storer.errorMessage, storer.errCode);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

// Load a module.
class ModuleLoader : public MainThreadRequest
{
public:
    ModuleLoader(TaskData* taskData, const TCHAR* file) :
        MainThreadRequest(MTP_LOADMODULE), callerTaskData(taskData), fileName(file),
        errorResult(NULL), errNumber(0), rootHandle(0) {}

    virtual void Perform();

    TaskData* callerTaskData;
    const TCHAR* fileName;
    const char* errorResult;
    int errNumber;
    Handle rootHandle;
};

// Contains tables that need to be deleted when the function exits.
class ModuleLoadData
{
public:
    ModuleLoadData(unsigned int nDescrs);
    ~ModuleLoadData();
    ModuleSegmentDescr* descrs;
    PolyWord** targetAddresses;
};

ModuleLoadData::ModuleLoadData(unsigned int nDescrs)
{
    descrs = 0;
    targetAddresses = 0;
    descrs = new ModuleSegmentDescr[nDescrs];
    targetAddresses = new PolyWord * [nDescrs];
    for (unsigned i = 0; i < nDescrs; i++) targetAddresses[i] = 0;
}

ModuleLoadData::~ModuleLoadData()
{
    if (descrs) delete[](descrs);
    if (targetAddresses) delete[](targetAddresses);
}

void ModuleLoader::Perform()
{
    AutoClose loadFile(_tfopen(fileName, _T("rb")));
    if ((FILE*)loadFile == NULL)
    {
        errorResult = "Cannot open load file";
        errNumber = ERRORNUMBER;
        return;
    }

    ModuleHeader header;
    // Read the header and check the signature.
    if (fread(&header, sizeof(ModuleHeader), 1, loadFile) != 1)
    {
        errorResult = "Unable to load header";
        return;
    }
    if (strncmp(header.headerSignature, MODULESIGNATURE, sizeof(header.headerSignature)) != 0)
    {
        errorResult = "File is not a Poly/ML module";
        return;
    }
    if (header.headerVersion != MODULEVERSION ||
        header.headerLength != sizeof(ModuleHeader) ||
        header.segmentDescrLength != sizeof(ModuleSegmentDescr))
    {
        errorResult = "Unsupported version of module file";
        return;
    }
    if (ModuleId(header.executableTimeStamp) != exportTimeStamp)
    {
        // Time-stamp does not match executable.
        errorResult =
            "Module was exported from a different executable or the executable has changed";
        return;
    }

    ModuleLoadData modload(header.segmentDescrCount);
    // Read all the descriptors first.  We need to be able to seek to the actual data for
    // each space as we create them.
    if (fseek(loadFile, header.segmentDescr, SEEK_SET) != 0 ||
        fread(modload.descrs, sizeof(ModuleSegmentDescr), header.segmentDescrCount, loadFile) != header.segmentDescrCount)
    {
        errorResult = "Unable to read segment descriptors";
        return;
    }

    // Read in and create the new segments first.  If we have problems,
    // in particular if we have run out of memory, then it's easier to recover.  
    for (unsigned i = 0; i < header.segmentDescrCount; i++)
    {
        ModuleSegmentDescr* descr = &modload.descrs[i];
        MemSpace* space = gMem.SpaceForIndex(descr->segmentIndex, descr->moduleIden);

        if (descr->segmentData == 0)
        { // No data - just an entry in the index.
            if (space == NULL/* ||
                descr->segmentSize != (size_t)((char*)space->top - (char*)space->bottom)*/)
            {
                errorResult = "Mismatch for existing memory space";
                return;
            }
            else modload.targetAddresses[i] = space->bottom;
        }
        else
        { // New segment.
            if (space != NULL)
            {
                errorResult = "Segment already exists";
                return;
            }
            // Allocate memory for the new segment.
            unsigned mFlags =
                (descr->segmentFlags & MSF_WRITABLE ? MTF_WRITEABLE : 0) |
                (descr->segmentFlags & MSF_NOOVERWRITE ? MTF_NO_OVERWRITE : 0) |
                (descr->segmentFlags & MSF_BYTES ? MTF_BYTES : 0) |
                (descr->segmentFlags & MSF_CODE ? MTF_EXECUTABLE : 0);
            // TODO: This creates the new space as though it existed in the original executable.
            // We need to distinguish it somehow so we know which module it came from.
            PermanentMemSpace* newSpace =
                gMem.AllocateNewPermanentSpace(descr->segmentSize, mFlags, descr->segmentIndex, header.timeStamp, 0 /* Hierarchy 0 */);
            if (newSpace == 0)
            {
                errorResult = "Unable to allocate memory";
                return;
            }
            if (fseek(loadFile, descr->segmentData, SEEK_SET) != 0)
            {
                errorResult = "Unable to seek to segment";
                return;
            }
            if (readData(newSpace->bottom, descr->segmentSize, loadFile) != 1)
            {
                errorResult = "Unable to read segment";
                return;
            }
            modload.targetAddresses[i] = newSpace->bottom;
            if (newSpace->isMutable && (descr->segmentFlags & MSF_BYTES) != 0)
            {
                ClearVolatile cwbr;
                cwbr.ScanAddressesInRegion(newSpace->bottom, (PolyWord*)((byte*)newSpace->bottom + descr->segmentSize));
            }
        }
    }
    // Now deal with relocation.
    for (unsigned j = 0; j < header.segmentDescrCount; j++)
    {
        ModuleSegmentDescr* descr = &modload.descrs[j];
        PolyWord* baseAddr = modload.targetAddresses[j];
        ASSERT(baseAddr != NULL); // We should have created it.
        // Process explicit relocations.
        // If we get errors just skip the error and continue rather than leave
        // everything in an unstable state.
        if (descr->relocations)
        {
            if (fseek(loadFile, descr->relocations, SEEK_SET) != 0)
                errorResult = "Unable to read relocation segment";
            for (unsigned k = 0; k < descr->relocationCount; k++)
            {
                ModRelocationEntry reloc;
                if (fread(&reloc, sizeof(reloc), 1, loadFile) != 1)
                    errorResult = "Unable to read relocation segment";
                byte* setAddress = (byte*)baseAddr + reloc.relocAddress;
                ASSERT(reloc.targetSegment < header.segmentDescrCount);
                byte* targetAddress = (byte*)modload.targetAddresses[reloc.targetSegment] + reloc.targetAddress;
                ScanAddress::SetConstantValue(setAddress, (PolyObject*)(targetAddress), reloc.relKind);
            }
        }
    }

    // Get the root address.  Push this to the caller's save vec.  If we put the
    // newly created areas into local memory we could get a GC as soon as we
    // complete this root request.
    {
        ASSERT(header.rootSegment < header.segmentDescrCount);
        PolyWord* baseAddr = modload.targetAddresses[header.rootSegment];
        rootHandle = callerTaskData->saveVec.push((PolyObject*)((byte*)baseAddr + header.rootOffset));
    }
}

static Handle LoadModule(TaskData* taskData, Handle args)
{
    TempString fileName(args->Word());
    ModuleLoader loader(taskData, fileName);
    processes->MakeRootRequest(taskData, &loader);

    if (loader.errorResult != 0)
    {
        if (loader.errNumber == 0)
            raise_fail(taskData, loader.errorResult);
        else
        {
            AutoFree<char*> buff((char*)malloc(strlen(loader.errorResult) + 2 + _tcslen(loader.fileName) * sizeof(TCHAR) + 1));
#if (defined(_WIN32) && defined(UNICODE))
            sprintf(buff, "%s: %S", loader.errorResult, loader.fileName);
#else
            sprintf(buff, "%s: %s", loader.errorResult, loader.fileName);
#endif
            raise_syscall(taskData, buff, loader.errNumber);
        }
    }

    return loader.rootHandle;
}

// Load a module
POLYUNSIGNED PolyLoadModule(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = LoadModule(taskData, pushedArg);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}


// Return the system directory for modules.  This is configured differently
// in Unix and in Windows.
POLYUNSIGNED PolyGetModuleDirectory(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
#if (defined(MODULEDIR))
        result = SAVE(C_string_to_Poly(taskData, MODULEDIR));
#elif (defined(_WIN32))
        {
            // This registry key is configured when Poly/ML is installed using the installer.
            // It gives the path to the Poly/ML installation directory.  We return the
            // Modules subdirectory.
            HKEY hk;
            if (RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                _T("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\PolyML.exe"), 0,
                KEY_QUERY_VALUE, &hk) == ERROR_SUCCESS)
            {
                DWORD valSize;
                if (RegQueryValueEx(hk, _T("Path"), 0, NULL, NULL, &valSize) == ERROR_SUCCESS)
                {
#define MODULEDIR _T("Modules")
                    TempString buff((TCHAR*)malloc(valSize + (_tcslen(MODULEDIR) + 1) * sizeof(TCHAR)));
                    DWORD dwType;
                    if (RegQueryValueEx(hk, _T("Path"), 0, &dwType, (LPBYTE)(LPTSTR)buff, &valSize) == ERROR_SUCCESS)
                    {
                        // The registry entry should end with a backslash.
                        _tcscat(buff, MODULEDIR);
                        result = SAVE(C_string_to_Poly(taskData, buff));
                    }
                }
                RegCloseKey(hk);
            }
            result = SAVE(C_string_to_Poly(taskData, ""));
        }
#else
        result = SAVE(C_string_to_Poly(taskData, ""));
#endif
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

struct _entrypts modulesEPT[] =
{
    { "PolyStoreModule",                (polyRTSFunction)&PolyStoreModule },
    { "PolyLoadModule",                 (polyRTSFunction)&PolyLoadModule },
    { "PolyGetModuleDirectory",         (polyRTSFunction)&PolyGetModuleDirectory },

    { NULL, NULL } // End of list.
};

