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

#include <vector>
#include <string>

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

#include "mpoly.h" // For exportSignature

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
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyStoreModule(POLYUNSIGNED threadId, POLYUNSIGNED filename, POLYUNSIGNED modName, POLYUNSIGNED contents);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyLoadModule(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetModuleDirectory(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyShowLoadedModules(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetModuleInfo(POLYUNSIGNED threadId, POLYUNSIGNED arg);
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

#ifdef HAVE__FTELLI64
// fseek and ftell are only 32-bits in Windows.
#define off_t   __int64
#define fseek _fseeki64
#define ftell _ftelli64
#endif

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

    struct _moduleId thisModuleId;      // The id for this module.
    struct _moduleId executableModId;    // The id for the parent executable.

    off_t       stringTable;            // Pointer to the string table (zero if none)
    size_t      stringTableSize;        // Size of string table
    unsigned    moduleNameEntry;        // Position in the string table of the name of this module

    off_t       dependencies;           // Location of dependency table, if any
    unsigned    dependencyCount;

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
    unsigned        targetSegment;      // Index into the address table
    ScanRelocationKind relKind;         // The kind of relocation (processor dependent).
} ModRelocationEntry;

// Entry for a dependency.  This is really a guide to help to load the dependencies
// automatically.  The segment table is used to check that the memory segment needed
// have actually been loaded.
typedef struct _modDependencyEntry
{
    unsigned            depNameEntry;   // Index into the string table for the name
    struct _moduleId    depId;          // Id for this dependency
} ModDependencyEntry;

// Information about currently loaded modules.  There will also
// be permanent memory spaces with the same Ids in gMem.
class LoadedModule {
public:
    LoadedModule() {}
    std::string moduleName;
    ModuleId moduleIden;
};

static std::vector<LoadedModule> loadedModules;

// Store a module
class ModuleStorer : public MainThreadRequest
{
public:
    ModuleStorer(const TCHAR* file, const char* modName, Handle r) :
        MainThreadRequest(MTP_STOREMODULE), fileName(file), moduleName(modName), root(r), errorMessage(0), errCode(0) {}

    virtual void Perform();

    const TCHAR* fileName;
    std::string moduleName;
    Handle root;
    const char* errorMessage;
    int errCode;
};

class ModuleExport : public Exporter, public ScanAddress
{
public:
    ModuleExport(std::string mName) : relocationCount(0), moduleName(mName) {}
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
    std::string moduleName;

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
    checkedFwrite(&reloc, sizeof(reloc), 1);
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
        modHeader.executableModId = exportSignature;
        {
            unsigned rootArea = findArea(this->rootFunction);
            ExportMemTable* mt = &memTable[rootArea];
            modHeader.rootSegment = rootArea;
            modHeader.rootOffset = (POLYUNSIGNED)((char*)this->rootFunction - (char*)mt->mtOriginalAddr);
        }
        modHeader.thisModuleId = expModuleId;
        modHeader.segmentDescrCount = this->memTableEntries; // One segment for each space.
        // Write out the header.
        if (!checkedFwrite(&modHeader, sizeof(modHeader), 1))
            throw IOException();

        std::vector<ModuleSegmentDescr> descrs;
        descrs.reserve(this->memTableEntries);
        // We need an entry in the descriptor tables for each segment in the executable because
        // we may have relocations that refer to addresses in it.
        for (unsigned j = 0; j < this->memTableEntries; j++)
        {
            ModuleSegmentDescr thisDescr;;
            ExportMemTable* entry = &this->memTable[j];
            memset(&thisDescr, 0, sizeof(ModuleSegmentDescr));
            thisDescr.relocationSize = sizeof(ModRelocationEntry);
            thisDescr.segmentIndex = entry->mtIndex;
            thisDescr.segmentSize = entry->mtLength; // Set this even if we don't write it.
            thisDescr.moduleIden = entry->mtModId;
            if (entry->mtFlags & MTF_WRITEABLE)
            {
                thisDescr.segmentFlags |= MSF_WRITABLE;
                if (entry->mtFlags & MTF_NO_OVERWRITE)
                    thisDescr.segmentFlags |= MSF_NOOVERWRITE;
                if ((entry->mtFlags & MTF_NO_OVERWRITE) == 0)
                    thisDescr.segmentFlags |= MSF_OVERWRITE;
                if (entry->mtFlags & MTF_BYTES)
                    thisDescr.segmentFlags |= MSF_BYTES;
            }
            if (entry->mtFlags & MTF_EXECUTABLE)
                thisDescr.segmentFlags |= MSF_CODE;
            descrs.push_back(thisDescr);
        }
        // Write out temporarily. Will be overwritten at the end.
        modHeader.segmentDescr = ftell(this->exportFile);
        if (!checkedFwrite(descrs.data(), sizeof(ModuleSegmentDescr), this->memTableEntries))
            throw IOException();

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
                if (!checkedFwrite(entry->mtOriginalAddr, entry->mtLength, 1))
                    throw IOException();
            }
        }
        // Write the string table and construct the dependency table at the same time
        std::vector<ModDependencyEntry> dependencyTable;

        modHeader.stringTable = ftell(exportFile);
        unsigned stringPos = 0;
        fputc(0, exportFile); // First byte of string table is zero
        stringPos++;
        modHeader.moduleNameEntry = stringPos; // Position of string
        fputs(moduleName.c_str(), exportFile);
        stringPos += (unsigned)moduleName.size();
        fputc(0, exportFile); // A terminating null.
        stringPos++;
        for (std::vector<LoadedModule>::iterator i = loadedModules.begin(); i < loadedModules.end(); i++)
        {
            if (copyScan.externalRefs[i->moduleIden])
            {
                ModDependencyEntry depEntry;
                depEntry.depNameEntry = stringPos;
                fputs(i->moduleName.c_str(), exportFile);
                stringPos += (unsigned)i->moduleName.size();
                fputc(0, exportFile); // A terminating null.
                stringPos++;
                depEntry.depId = i->moduleIden;
                dependencyTable.push_back(depEntry);
            }
        }
        modHeader.stringTableSize = stringPos;
        if (ferror(exportFile))
        {
            errNumber = ERRORNUMBER;
            errorMessage = "Error while writing string table";
            throw IOException();
        }
        // And the dependency table itself if needed.
        if (!dependencyTable.empty())
        {
            modHeader.dependencies = ftell(exportFile);
            modHeader.dependencyCount = (unsigned)dependencyTable.size();
            if (!checkedFwrite(dependencyTable.data(), sizeof(ModDependencyEntry), dependencyTable.size()))
                throw IOException();
        }

        // Rewrite the header and the segment tables now they're complete.
        fseek(exportFile, 0, SEEK_SET);
        if (!checkedFwrite(&modHeader, sizeof(modHeader), 1))
            throw IOException();
        if (!checkedFwrite(descrs.data(), sizeof(ModuleSegmentDescr), this->memTableEntries))
            throw IOException();

        fclose(exportFile); exportFile = NULL;

        // Add this to the loaded module table.
        LoadedModule newModule;
        newModule.moduleName = moduleName;
        newModule.moduleIden = expModuleId;
        loadedModules.push_back(newModule);

        // If it all succeeded we can switch over to the permanent spaces.
        if (gMem.PromoteNewExportSpaces(0)) // Turn them into hierarchy zero.
            switchLocalsToPermanent();
        else revertToLocal();
    }
    catch (IOException&)
    {
        revertToLocal();
    }
    catch (MemoryException&) {
        errorMessage = "Insufficient Memory";
        revertToLocal();
    }
}

void ModuleStorer::Perform()
{
    ModuleExport exporter(moduleName);
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
    errCode = exporter.errNumber;
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
POLYUNSIGNED PolyStoreModule(POLYUNSIGNED threadId, POLYUNSIGNED filename, POLYUNSIGNED modName, POLYUNSIGNED contents)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedContents = taskData->saveVec.push(contents);

    try {
        TempString fileName(PolyWord::FromUnsigned(filename));
        TempCString moduleName(PolyWord::FromUnsigned(modName));
        ModuleStorer storer(fileName, moduleName, pushedContents);
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

// Data needed for relocation during load.
class ModuleLoadData {
public:
    ModuleLoadData() : relocations(0), relocationCount(0), targetAddr(0) {}

    off_t       relocations;        // Copied from descriptor
    unsigned    relocationCount;
    PolyWord* targetAddr;           // Actual address of the segment
};

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
    // Check to see if this is already loaded
    for (std::vector<LoadedModule>::iterator i = loadedModules.begin(); i < loadedModules.end(); i++)
    {
        if (i->moduleIden == header.thisModuleId)
        {
            errorResult = "A module with this signature is already loaded";
            return;
        }
    }
    if (ModuleId(header.executableModId) != exportSignature)
    {
        // Time-stamp does not match executable.
        errorResult =
            "Module was exported from a different executable or the executable has changed";
        return;
    }

    std::string thisModuleName;
    if (header.stringTableSize == 0)
        thisModuleName = "";
    else
    {
        AutoFree<char*> stringTab = (char*)malloc(header.stringTableSize);
        if (stringTab == 0)
        {
            errorResult = "Unable to allocate memory";
            return;
        }
        if (fseek(loadFile, header.stringTable, SEEK_SET) != 0 ||
            fread(stringTab, 1, header.stringTableSize, loadFile) != header.stringTableSize)
        {
            errorResult = "Unable to read string table";
            return;
        }
        // Check that there is at least one terminator
        if (stringTab[header.stringTableSize - 1] != '\0')
        {
            errorResult = "Malformed string table";
            return;
        }
        thisModuleName = stringTab + header.moduleNameEntry;
    }

    // We could check the dependency table at this point but other than perhaps improving
    // the error message if a dependency is missing there's no point.
 
    // Read all the descriptors first.  We need to be able to seek to the actual data for
    // each space as we create them.
    std::vector<ModuleSegmentDescr> descrs;
    descrs.reserve(header.segmentDescrCount);
    if (fseek(loadFile, header.segmentDescr, SEEK_SET) != 0)
    {
        errorResult = "Unable to read segment descriptors";
        return;
    }
    for (unsigned i = 0; i < header.segmentDescrCount; i++)
    {
        ModuleSegmentDescr msd;
        if (fread(&msd, sizeof(ModuleSegmentDescr), 1, loadFile) != 1)
        {
            errorResult = "Unable to read segment descriptors";
            return;
        }
        descrs.push_back(msd);
    }

    std::vector<ModuleLoadData> loadData;
    loadData.reserve(header.segmentDescrCount);

    // Read in and create the new segments first.  If we have problems,
    // in particular if we have run out of memory, then it's easier to recover.
    for(std::vector<ModuleSegmentDescr>::iterator descr = descrs.begin(); descr < descrs.end(); descr++)
    {
        MemSpace* space = gMem.SpaceForIndex(descr->segmentIndex, descr->moduleIden);
        ModuleLoadData load;
        load.relocationCount = descr->relocationCount;
        load.relocations = descr->relocations;

        if (descr->segmentData == 0)
        { // No data - just an entry in the index.
            if (space == NULL)
            {
                errorResult = "Required memory space is not present";
                return;
            }
            load.targetAddr = space->bottom;
        }
        else
        { // New segment.
            if (space != NULL)
            {
                errorResult = "Memory space with this signature already exists";
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
                gMem.AllocateNewPermanentSpace(descr->segmentSize, mFlags, descr->segmentIndex, header.thisModuleId, 0 /* Hierarchy 0 */);
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
            if (newSpace->isMutable && (descr->segmentFlags & MSF_BYTES) != 0)
            {
                ClearVolatile cwbr;
                cwbr.ScanAddressesInRegion(newSpace->bottom, (PolyWord*)((byte*)newSpace->bottom + descr->segmentSize));
            }
            load.targetAddr = newSpace->bottom;
        }
        loadData.push_back(load);
    }
    // Now deal with relocation.
    for (std::vector<ModuleLoadData>::iterator i = loadData.begin(); i < loadData.end(); i++)
    {
        PolyWord* baseAddr = i->targetAddr;
        ASSERT(baseAddr != NULL); // We should have created it.
        // Process explicit relocations.
        // If we get errors just skip the error and continue rather than leave
        // everything in an unstable state.
        if (i->relocations)
        {
            if (fseek(loadFile, i->relocations, SEEK_SET) != 0)
                errorResult = "Unable to read relocation segment";
            for (unsigned k = 0; k < i->relocationCount; k++)
            {
                ModRelocationEntry reloc;
                if (fread(&reloc, sizeof(reloc), 1, loadFile) != 1)
                    errorResult = "Unable to read relocation segment";
                byte* setAddress = (byte*)baseAddr + reloc.relocAddress;
                ASSERT(reloc.targetSegment < header.segmentDescrCount);
                byte* targetAddress = (byte*)loadData[reloc.targetSegment].targetAddr + reloc.targetAddress;
                ScanAddress::SetConstantValue(setAddress, (PolyObject*)(targetAddress), reloc.relKind);
            }
        }
    }

    // Get the root address.  Push this to the caller's save vec.  If we put the
    // newly created areas into local memory we could get a GC as soon as we
    // complete this root request.
    {
        ASSERT(header.rootSegment < header.segmentDescrCount);
        PolyWord* baseAddr = loadData[header.rootSegment].targetAddr;
        rootHandle = callerTaskData->saveVec.push((PolyObject*)((byte*)baseAddr + header.rootOffset));
    }

    // Add to the loaded module table
    LoadedModule newMod;
    newMod.moduleName = thisModuleName;
    newMod.moduleIden = header.thisModuleId;
    loadedModules.push_back(newMod);
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

static Handle moduleIdAsByteVector(TaskData *taskData, struct _moduleId modId)
{
    union {
        struct _moduleId mId;
        char chars[sizeof(struct _moduleId)];
    } asVector;
    asVector.mId = modId;
    // Convert the module Id to a vector of bytes.  The representation is the same as a string.
    return taskData->saveVec.push(C_string_to_Poly(taskData, asVector.chars, sizeof(struct _moduleId)));
}

// Return the list of loaded modules as a list of pairs of name and signature.
POLYUNSIGNED PolyShowLoadedModules(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = taskData->saveVec.push(ListNull); // Head of list

    try {
        // Iterate over the table in reverse order so  the resulting list is in the order the modules were loaded.
        for (std::vector<LoadedModule>::reverse_iterator i = loadedModules.rbegin(); i < loadedModules.rend(); i++)
        {
            // Convert the module Id to a vector of bytes.  The representation is the same as a string.
            Handle idHandle = moduleIdAsByteVector(taskData, i->moduleIden);
            Handle nameHandle = taskData->saveVec.push(C_string_to_Poly(taskData, i->moduleName.c_str()));
            Handle pairHandle = alloc_and_save(taskData, 2);
            pairHandle->WordP()->Set(0, nameHandle->Word());
            pairHandle->WordP()->Set(1, idHandle->Word());
            ML_Cons_Cell* next = (ML_Cons_Cell*)alloc(taskData, sizeof(ML_Cons_Cell) / sizeof(PolyWord));
            next->h = pairHandle->Word();
            next->t = result->Word();
            // Reset the save vec to keep it bounded
            taskData->saveVec.reset(reset);
            result = taskData->saveVec.push(next);
        }
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

static Handle GetModInfo(TaskData* taskData, Handle hFileName)
{
    AutoFree<TCHAR*> fileNameBuff(Poly_string_to_T_alloc(hFileName->Word()));
    if (fileNameBuff == NULL)
        raise_syscall(taskData, "Insufficient memory", NOMEMORY);

    AutoClose loadFile(_tfopen(fileNameBuff, _T("rb")));
    if ((FILE*)loadFile == NULL)
        raise_syscall(taskData, "Cannot open file", ERRORNUMBER); // The string will be set from the error code
    ModuleHeader header;

    // Read the header and check the signature.
    if (fread(&header, sizeof(ModuleHeader), 1, loadFile) != 1)
        raise_fail(taskData, "Unable to load header");
    if (strncmp(header.headerSignature, MODULESIGNATURE, sizeof(header.headerSignature)) != 0)
        raise_fail(taskData, "File is not a Poly/ML module");
    if (header.headerVersion != MODULEVERSION ||
        header.headerLength != sizeof(ModuleHeader) ||
        header.segmentDescrLength != sizeof(ModuleSegmentDescr))
        raise_fail(taskData, "Unsupported version of module file");
    // Check that the module came from this executable.  We could include the
    // signature of the executable in the result but then we would also have to
    // have some way to provide the current value of exportSignature in order to check it.
    if (ModuleId(header.executableModId) != exportSignature)
        // Time-stamp does not match executable.
        raise_fail(taskData, "Module was exported from a different executable or the executable has changed");

    if (header.stringTableSize == 0)
        raise_fail(taskData, "Missing string table");
    AutoFree<char*> stringTab = (char*)malloc(header.stringTableSize);
    if (stringTab == 0)
        raise_fail(taskData, "Unable to allocate memory");
    if (fseek(loadFile, header.stringTable, SEEK_SET) != 0 ||
        fread(stringTab, 1, header.stringTableSize, loadFile) != header.stringTableSize)
        raise_fail(taskData, "Unable to read string table");
    // Check that there is at least one terminator
    if (stringTab[header.stringTableSize - 1] != '\0')
        raise_fail(taskData, "Malformed string table");

    Handle reset = taskData->saveVec.mark();
    Handle depListHandle = taskData->saveVec.push(ListNull);
    if (fseek(loadFile, header.dependencies, SEEK_SET) != 0)
        raise_fail(taskData, "Unable to access dependency table");

    for (unsigned i = 0; i < header.dependencyCount; i++)
    {
        ModDependencyEntry modDepEntry;
        if (fread(&modDepEntry, sizeof(ModDependencyEntry), 1, loadFile) != 1)
            raise_fail(taskData, "Unable to load dependency entry");
        // Convert the module Id to a vector of bytes.  The representation is the same as a string.
        Handle idHandle = moduleIdAsByteVector(taskData, modDepEntry.depId);
        if (modDepEntry.depNameEntry >= header.stringTableSize)
            raise_fail(taskData, "Invalid string table entry");
        Handle nameHandle = taskData->saveVec.push(C_string_to_Poly(taskData, ((const char *)stringTab) + modDepEntry.depNameEntry));
        Handle pairHandle = alloc_and_save(taskData, 2);
        pairHandle->WordP()->Set(0, nameHandle->Word());
        pairHandle->WordP()->Set(1, idHandle->Word());
        ML_Cons_Cell* next = (ML_Cons_Cell*)alloc(taskData, sizeof(ML_Cons_Cell) / sizeof(PolyWord));
        next->h = pairHandle->Word();
        next->t = depListHandle->Word();
        // Reset the save vec to keep it bounded
        taskData->saveVec.reset(reset);
        depListHandle = taskData->saveVec.push(next);
    }

    if (header.moduleNameEntry >= header.stringTableSize)
        raise_fail(taskData, "Invalid string table entry");
    Handle modNameHandle = taskData->saveVec.push(C_string_to_Poly(taskData, ((const char*)stringTab) + header.moduleNameEntry));
    Handle modIdHandle = moduleIdAsByteVector(taskData, header.thisModuleId);

    Handle resultHandle = alloc_and_save(taskData, 3);
    resultHandle->WordP()->Set(0, modNameHandle->Word());
    resultHandle->WordP()->Set(1, modIdHandle->Word());
    resultHandle->WordP()->Set(2, depListHandle->Word());

    return resultHandle;
}

// Open a module file and read the module information and dependencies
POLYUNSIGNED PolyGetModuleInfo(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = GetModInfo(taskData, pushedArg);
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
    { "PolyShowLoadedModules",          (polyRTSFunction)&PolyShowLoadedModules },
    { "PolyGetModuleInfo",              (polyRTSFunction)&PolyGetModuleInfo },

    { NULL, NULL } // End of list.
};
