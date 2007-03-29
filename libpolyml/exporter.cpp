/*
    Title:  exporter.cpp - Export a function as an object or C file

    Copyright (c) 2006 David C.J. Matthews

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

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)

#else
#define ASSERT(x)
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "exporter.h"
#include "save_vec.h"
#include "polystring.h"
#include "run_time.h"
#include "sys.h"
#include "osmem.h"
#include "scanaddrs.h"
#include "gc.h"
#include "machine_dep.h"
#include "diagnostics.h"
#include "memmgr.h"
#include "processes.h" // For IO_SPACING

#include "pexport.h"

#ifdef HAVE_PECOFF
#include "pecoffexport.h"
#elif defined(HAVE_ELF_H)
#include "elfexport.h"
#elif defined(HAVE_MACH_O_RELOC_H)
#include "machoexport.h"
#endif

#if(!defined(MAXPATHLEN) && defined(MAX_PATH))
#define MAXPATHLEN MAX_PATH
#endif

/*
To export the function and everything reachable from it we need to copy
all the objects into a new area.  We leave tombstones in the original
objects by overwriting the length word.  That prevents us from copying an
object twice and breaks loops.  Once we've copied the objects we then
have to go back over the memory and turn the tombstones back into length
words.
*/

class CopyScan: public ScanAddress
{
public:
    CopyScan();
    ~CopyScan();
protected:
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);
public:
    virtual PolyObject *ScanObjectAddress(PolyObject *base);

    // Default sizes of the segments.
    POLYUNSIGNED defaultImmSize, defaultMutSize;
};

CopyScan::CopyScan()
{
    ASSERT(gMem.neSpaces == 0);
    // Set the space sizes to a quarter the space currently in use.  Making
    // the spaces too large may be a problem if we're very close to the maximum
    // address space.  Making them too small may increase the cost of linking.
    defaultImmSize = defaultMutSize = 0;
    unsigned i;
    for (i = 0; i < gMem.npSpaces; i++)
    {
        MemSpace *space = gMem.pSpaces[i];
        POLYUNSIGNED size = (space->top-space->bottom)/4;
        if (space->isMutable)
            defaultMutSize += size;
        else
            defaultImmSize += size;
    }
    for (i = 0; i < gMem.nlSpaces; i++)
    {
        LocalMemSpace *space = gMem.lSpaces[i];
        POLYUNSIGNED size = (space->top-space->pointer)/4;
        if (space->isMutable)
            defaultMutSize += size;
        else
            defaultImmSize += size;
    }
    if (defaultMutSize < 1024*1024)
        defaultMutSize = 1024*1024;
    if (defaultImmSize < 1024*1024)
        defaultImmSize = 1024*1024;
}

CopyScan::~CopyScan()
{
    gMem.DeleteExportSpaces();
}


// This function is called for each address in an object
// once it has been copied to its new location.  We copy first
// then scan to update the addresses.
POLYUNSIGNED CopyScan::ScanAddressAt(PolyWord *pt)
{
    PolyWord val = *pt;
    // Ignore integers.
    if (IS_INT(val) || val == PolyWord::FromUnsigned(0))
        return 0;
    // Ignore pointers to the IO area.  They will be relocated
    // when we write out the memory
    MemSpace *space = gMem.SpaceForAddress(val.AsAddress());
    ASSERT(space != 0);
    if (space->spaceType == ST_IO)
        return 0;

    // We may sometimes get addresses that have already been updated
    // to point to the new area.  e.g. (only?) in the case of constants
    // that have been updated in ScanConstantsWithinCode.
    if (space->spaceType == ST_EXPORT)
        return 0;

    if (val.IsCodePtr())
    {
        // Find the start of the code segment
        PolyObject *oldObject = ObjCodePtrToPtr(val.AsCodePtr());
        // Calculate the byte offset of this value within the code object.
        POLYUNSIGNED offset = val.AsCodePtr() - (byte*)oldObject;
        PolyObject *newObject = ScanObjectAddress(oldObject);
        *pt = PolyWord::FromCodePtr((byte*)newObject + offset);
        return 0;
    }

    ASSERT(OBJ_IS_DATAPTR(val));

    // Have we already scanned this?
    PolyObject *obj = val.AsObjPtr();
    if (obj->ContainsForwardingPtr())
    {
        // Update the address to the new value.
        PolyObject *newAddr = obj->GetForwardingPtr();
        *pt = newAddr;
        return 0; // No need to scan it again.
    }
    // No, we need to copy it.
    ASSERT(space->spaceType == ST_LOCAL || space->spaceType == ST_PERMANENT);
    POLYUNSIGNED lengthWord = obj->LengthWord();
    POLYUNSIGNED words = OBJ_OBJECT_LENGTH(lengthWord);

    PolyObject *newObj = 0;
    bool isMutableObj = obj->IsMutable();
    // Allocate a new address for the object.
    for (unsigned i = 0; i < gMem.neSpaces; i++)
    {
        ExportMemSpace *space = gMem.eSpaces[i];
        if (isMutableObj == space->isMutable)
        {
            ASSERT(space->pointer <= space->top && space->pointer >= space->bottom);
            POLYUNSIGNED spaceLeft = space->pointer - space->bottom;
            if (spaceLeft > words)
            {
                space->pointer -= words+1;
                newObj = (PolyObject*)(space->pointer+1);
                break;
            }
        }
    }
    if (newObj == 0)
    {
        // Didn't find room in the existing spaces.  Create a new space.
        POLYUNSIGNED spaceWords = isMutableObj ? defaultMutSize : defaultImmSize;
        if (spaceWords <= words)
            spaceWords = words+1; // Make sure there's space for this object.
        ExportMemSpace *space = gMem.NewExportSpace(spaceWords, isMutableObj);
        if (space == 0)
        {
            // Unable to allocate this.
            throw MemoryException();
        }
        space->pointer -= words+1;
        newObj = (PolyObject*)(space->pointer+1);
        ASSERT(space->pointer <= space->top && space->pointer >= space->bottom);
    }

    newObj->SetLengthWord(lengthWord); // copy length word

    if (OBJ_IS_STACK_OBJECT(lengthWord))
        CopyStackFrame ((StackObject *)obj,(StackObject *)newObj);
    else
        memcpy(newObj, obj, words*sizeof(PolyWord));

    obj->SetForwardingPtr(newObj); // Put forwarding pointer in old object.

    if (OBJ_IS_CODE_OBJECT(lengthWord))
    {
        // We don't need to worry about flushing the instruction cache
        // since we're not going to execute this code here.
        // We do have to update any relative addresses within the code
        // to take account of its new position.  We have to do that now
        // even though ScanAddressesInObject will do it again because this
        // is the only point where we have both the old and the new addresses.
        machineDependent->ScanConstantsWithinCode(newObj, obj, words, this);
    }
    *pt = newObj; // Update it to the newly copied object.
    return lengthWord;  // This new object needs to be scanned.
}


PolyObject *CopyScan::ScanObjectAddress(PolyObject *base)
{
    PolyWord val = base;
    // Scan this as an address. 
    POLYUNSIGNED lengthWord = CopyScan::ScanAddressAt(&val);
    if (lengthWord)
        ScanAddressesInObject(val.AsObjPtr(), lengthWord);
    return val.AsObjPtr();
}

#define MAX_EXTENSION   4 // The longest extension we may need to add is ".obj"

// Convert the forwarding pointers in a region back into length words.

// Generally if this object has a forwarding pointer that's
// because we've moved it into the export region.  We can,
// though, get multiple levels of forwarding if there is an object
// that has been shifted up by a garbage collection, leaving a forwarding
// pointer and then that object has been moved to the export region.
static POLYUNSIGNED GetObjLength(PolyObject *obj)
{
    if (obj->ContainsForwardingPtr())
    {
        POLYUNSIGNED length = GetObjLength(obj->GetForwardingPtr());
        obj->SetLengthWord(length);
        return length;
    }
    else return obj->LengthWord();
}

static void FixForwarding(PolyWord *pt, POLYUNSIGNED space)
{
    while (space)
    {
        pt++;
        PolyObject *obj = (PolyObject*)pt;
        POLYUNSIGNED length = OBJ_OBJECT_LENGTH(GetObjLength(obj));
        pt += length;
        space -= length+1;
    }
}

static void exporter(TaskData *taskData, Handle args, bool isNative, const char *extension, Exporter *exports)
{
    char fileNameBuff[MAXPATHLEN+MAX_EXTENSION];
    POLYUNSIGNED length =
        Poly_string_to_C(DEREFHANDLE(args)->Get(0), fileNameBuff, MAXPATHLEN);
    if (length > MAXPATHLEN)
        raise_syscall(taskData, "File name too long", ENAMETOOLONG);

    // Does it already have the extension?  If not add it on.
    if (length < strlen(extension) || strcmp(fileNameBuff + length - strlen(extension), extension) != 0)
        strcat(fileNameBuff, extension);

    // Do a full GC to minimise the size of the heaps when we
    // come to rewrite the length words.

    // Although we don't actually need to repeat the GC we
    // do need to exclude any other thread from the memory until
    // we've finished.
    while (! processes->BeginGC(taskData)) {}
    FullGC();

    try {

        PolyObject *rootFunction = args->WordP()->Get(1).AsObjPtr();

        // Copy the root and everything reachable from it into the temporary area.
        CopyScan copyScan;

        PolyObject *copiedRoot = 0;
        try {
            copiedRoot = copyScan.ScanObjectAddress(rootFunction);
        }
        catch (MemoryException)
        {
            // If we run out of memory.
            copiedRoot = 0;
            raise_exception_string(taskData, EXC_Fail, "Insufficient memory for export");
        }

        // Fix the forwarding pointers.
        unsigned j;
        for (j = 0; j < gMem.nlSpaces; j++)
        {
            LocalMemSpace *space = gMem.lSpaces[j];
            // Local areas only have objects from the allocation pointer to the top.
            FixForwarding(space->pointer, space->top - space->pointer);
        }
        for (j = 0; j < gMem.npSpaces; j++)
        {
            MemSpace *space = gMem.pSpaces[j];
            // Permanent areas are filled with objects from the bottom.
            FixForwarding(space->bottom, space->top - space->bottom);
        }

        // Reraise the exception after cleaning up the forwarding pointers.
        if (copiedRoot == 0)
            throw IOException(EXC_EXCEPTION);

        // Copy the areas into the export object.
        exports->memTable = new memoryTableEntry[gMem.neSpaces+1];
        exports->ioMemEntry = 0;
        // The IO vector.  Should we actually create a blank area?  This needs to be
        // writable by the RTS but not normally by ML.
        MemSpace *ioSpace = gMem.IoSpace();
        exports->memTable[0].mtAddr = ioSpace->bottom;
        exports->memTable[0].mtLength = (char*)ioSpace->top - (char*)ioSpace->bottom;
        exports->memTable[0].mtFlags = 0;

        for (unsigned i = 0; i < gMem.neSpaces; i++)
        {
            memoryTableEntry *entry = &exports->memTable[i+1];
            ExportMemSpace *space = gMem.eSpaces[i];
            entry->mtAddr = space->pointer;
            entry->mtLength = (space->top-space->pointer)*sizeof(PolyWord);
            if (space->isMutable)
                entry->mtFlags = MTF_WRITEABLE;
            else
                entry->mtFlags = MTF_EXECUTABLE;
        }

        exports->memTableEntries = gMem.neSpaces+1;

        exports->ioSpacing = IO_SPACING;
        exports->rootFunction = copiedRoot;

        exports->exportFile = fopen(fileNameBuff, "wb");
        if (exports->exportFile == NULL)
            raise_syscall(taskData, "Cannot open export file", errno);
        exports->exportStore();
        processes->EndGC(taskData);
    }
    catch (...)
    {
        processes->EndGC(taskData);
        throw;
    }
}

// Functions called via the RTS call.
Handle exportNative(TaskData *taskData, Handle args)
{
#ifdef HAVE_PECOFF
    // Windows including Cygwin
#ifdef WINDOWS_PC
    const char *extension = ".obj"; // Windows
#else
    const char *extension = ".o"; // Cygwin
#endif
    PECOFFExport exports;
    exporter(taskData, args, true, extension, &exports);
#elif defined(HAVE_ELF_H)
    // Most Unix including Linux, FreeBSD and Solaris.
    const char *extension = ".o";
    ELFExport exports(taskData);
    exporter(taskData, args, true, extension, &exports);
#elif defined(HAVE_MACH_O_RELOC_H)
    // Mac OS-X
    const char *extension = ".o";
    MachoExport exports(taskData);
    exporter(taskData, args, true, extension, &exports);
#else
    raise_exception_string (taskData, EXC_Fail, "Native export not available for this platform");
#endif
    return taskData->saveVec.push(TAGGED(0));
}

Handle exportPortable(TaskData *taskData, Handle args)
{
    PExport exports;
    exporter(taskData, args, false, ".txt", &exports);
    return taskData->saveVec.push(TAGGED(0));
}


// Helper functions for exporting.  We need to produce relocation information
// and this code is common to every method.
Exporter::Exporter(): exportFile(NULL), memTable(0)
{
}

Exporter::~Exporter()
{
    delete[](memTable);
    if (exportFile)
        fclose(exportFile);
}

void Exporter::relocateValue(PolyWord *pt)
{
    PolyWord q = *pt;
    if (IS_INT(q) || q == PolyWord::FromUnsigned(0)) {}
    else createRelocation(pt);
}

// Check through the areas to see where the address is.  It must be
// in one of them.
unsigned Exporter::findArea(void *p)
{
    for (unsigned i = 0; i < memTableEntries; i++)
    {
        if (p > memTable[i].mtAddr &&
            p <= (char*)memTable[i].mtAddr + memTable[i].mtLength)
            return i;
    }
    { ASSERT(0); }
    return 0;
}

void Exporter::relocateObject(PolyObject *p)
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
        /* Now the constants. */
        for (POLYUNSIGNED i = 0; i < constCount; i++) relocateValue(&(cp[i]));

    }
    else if (p->IsStackObject())
    {
        StackObject *s = (StackObject*)p;
        POLYUNSIGNED length = p->Length();

        ASSERT(! p->IsMutable()); // Should have been frozen
        /* First the standard registers, space, pc, sp, hr. */
        // pc may be TAGGED(0) indicating a retry.
        PolyWord pc = PolyWord::FromCodePtr(s->p_pc);
        if (pc != TAGGED(0))
            s->p_pc = createRelocation(pc, &s->p_pc).AsCodePtr();

        PolyWord *stackPtr = s->p_sp; // Save this before we change it.
        s->p_sp = createRelocation(PolyWord::FromStackAddr(s->p_sp), &s->p_sp).AsStackAddr();
        s->p_hr = createRelocation(PolyWord::FromStackAddr(s->p_hr), &s->p_hr).AsStackAddr();

        /* Checked registers. */
        PolyWord *stackStart = (PolyWord*)p;
        PolyWord *stackEnd = stackStart+length;
        for (POLYUNSIGNED i = 0; i < s->p_nreg; i++)
        {
            PolyWord r = s->p_reg[i];
            if (r.AsStackAddr() >= stackStart && r.AsStackAddr() < stackEnd)
                createRelocation(&s->p_reg[i]);
            /* It seems we can have zeros in the registers, at least on the i386. */
            else if (r == PolyWord::FromUnsigned(0)) {}
            else relocateValue(&(s->p_reg[i]));
        }
        /* Now the values on the stack. */
        for (PolyWord *q = stackPtr; q < stackEnd; q++)
        {
            /* A stack may contain a value which is an offset. */
            if ((*q).AsStackAddr() >= stackStart && (*q).AsStackAddr() < stackEnd)
                createRelocation(q);
            else relocateValue(q);
        }
    }
    else /* Ordinary objects, essentially tuples. */
    {
        POLYUNSIGNED length = p->Length();
        for (POLYUNSIGNED i = 0; i < length; i++) relocateValue(p->Offset(i));
    }
}

ExportStringTable::ExportStringTable(): strings(0), stringSize(0), stringAvailable(0)
{
}

ExportStringTable::~ExportStringTable()
{
    free(strings);
}

// Add a string to the string table, growing it if necessary.
unsigned long ExportStringTable::makeEntry(const char *str)
{
    int len = strlen(str);
    unsigned long entry = stringSize;
    if (stringSize + len + 1 > stringAvailable)
    {
        stringAvailable = stringAvailable+stringAvailable/2;
        if (stringAvailable < stringSize + len + 1)
            stringAvailable = stringSize + len + 1 + 500;
        strings = (char*)realloc(strings, stringAvailable);
        if (strings == 0)
            throw MemoryException();
     }
    strcpy(strings + stringSize, str);
    stringSize += len + 1;
    return entry;
}

