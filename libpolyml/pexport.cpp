/*
    Title:     Export and import memory in a portable format
    Author:    David C. J. Matthews.

    Copyright (c) 2006-7 David C. J. Matthews


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
#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x)
#endif

#include "globals.h"
#include "pexport.h"
#include "machine_dep.h"
#include "scanaddrs.h"
#include "run_time.h"
#include "../polyexports.h"
#include "version.h"
#include "sys.h"
#include "polystring.h"
#include "processes.h" // For IO_SPACING
#include "memmgr.h"
#include "osmem.h"

/*
This file contains the code both to export the file and to import it
in a new session.
*/

PExport::PExport()
{
    pMap = 0;
    nMapSize = 0;
    nObjects = 0;
    indexOrder = 0;
}

PExport::~PExport()
{
    free(pMap);
    free(indexOrder);
}


// Get the index corresponding to an address.
unsigned long PExport::getIndex(PolyObject *p)
{
    // Binary chop to find the index from the address.
    unsigned long lower = 0, upper = nObjects;
    while (1)
    {
        ASSERT(lower < upper);
        unsigned long middle = (lower+upper)/2;
        ASSERT(middle >= 0 && middle < nObjects);
        if (p < pMap[middle])
        {
            // Use lower to middle
            upper = middle; 
        }
        else if (p > pMap[middle])
        {
            // Use middle+1 to upper
            lower = middle+1;
        }
        else // Found it
            return middle;
    }
}

void PExport::printCodeAddr(byte *q)
// Address into code.  Either the pc field of a stack segment or
//  a word + 2 format address.
{
    PolyObject *obj = ObjCodePtrToPtr(q);
    unsigned long a = getIndex(obj);
    fprintf(exportFile, "$%lu+%lu", a, (POLYUNSIGNED)(q - (byte*)obj));
}

/* Get the index corresponding to an address. */
void PExport::printAddress(void *p)
{
    unsigned area = findArea(p);
    if (area == ioMemEntry)
    {
        // Is it an IO entry?
        POLYUNSIGNED byteOffset = (char*)p - (char*)memTable[area].mtAddr;
        unsigned ioEntry = byteOffset / (ioSpacing*sizeof(PolyWord));
        unsigned ioOffset = byteOffset - ioEntry * (ioSpacing*sizeof(PolyWord));
        ASSERT(ioEntry >= 0 && ioEntry < POLY_SYS_vecsize);
        if (ioOffset == 0)
           fprintf(exportFile, "I%d", ioEntry);
        else
            fprintf(exportFile, "J%d+%d", ioEntry, ioOffset); // Can this happen now?
    }
    else
        fprintf(exportFile, "@%lu", getIndex((PolyObject*)p));
}

void PExport::printValue(PolyWord q)
{
    if (IS_INT(q) || q == PolyWord::FromUnsigned(0))
        fprintf(exportFile, "%ld", UNTAGGED(q));
    else if (OBJ_IS_CODEPTR(q))
        printCodeAddr(q.AsCodePtr());
    else
        printAddress(q.AsAddress());
}

void PExport::printObject(PolyObject *p)
{
    POLYUNSIGNED length = p->Length();
    POLYUNSIGNED i;

    unsigned long myIndex = getIndex(p);

    fprintf(exportFile, "%lu:", myIndex);

    if (p->IsMutable())
        putc('M', exportFile);
    if (OBJ_IS_NEGATIVE(p->LengthWord()))
        putc('N', exportFile);
    if (OBJ_IS_WEAKREF_OBJECT(p->LengthWord()))
        putc('W', exportFile);
    if (OBJ_IS_NO_OVERWRITE(p->LengthWord()))
        putc('V', exportFile);

    if (p->IsByteObject())
    {
        /* May be a string, a long format arbitrary precision
           number or a real number. */
        PolyStringObject* ps = (PolyStringObject*)p;
        /* See if the first word is a possible length.  The length
           cannot be one because single character strings are
           represented by the character. */
        /* This is not infallible but it seems to be good enough
           to detect the strings. */
        if (ps->length > 1 &&
            (POLYUNSIGNED)((ps->length + sizeof(PolyWord) -1) / sizeof(PolyWord)) == length-1)
        {
            /* Looks like a string. */
            fprintf(exportFile, "S%lu|", ps->length);
            for (unsigned i = 0; i < ps->length; i++)
            {
                char ch = ps->chars[i];
                fprintf(exportFile, "%02x", ch);
            }
        }
        else
        {
            /* Not a string. May be an arbitrary precision integer.
               If the source and destination word lengths differ we
               could find that some long-format arbitrary precision
               numbers could be represented in the tagged short form
               or vice-versa.  The former case might give rise to
               errors because when comparing two arbitrary precision
               numbers for equality we assume that they are not equal
               if they have different representation.  The latter
               case could be a problem because we wouldn't know whether
               to convert the tagged form to long form, which would be
               correct if the value has type "int" or to truncate it
               which would be correct for "word".
               It could also be a real number but that doesn't matter
               if we recompile everything on the new machine.
            */
            byte *u = (byte*)p;
            putc('B', exportFile);
            fprintf(exportFile, "%lu|", length*sizeof(PolyWord));
            for (unsigned i = 0; i < (unsigned)(length*sizeof(PolyWord)); i++)
            {
                fprintf(exportFile, "%02x", u[i]);
            }
        }
    }
    else if (p->IsCodeObject())
    {
        POLYUNSIGNED constCount, i;
        PolyWord *cp;
        ASSERT(! p->IsMutable() );
        /* Work out the number of bytes in the code and the
           number of constants. */
        p->GetConstSegmentForCode(cp, constCount);
        /* The byte count is the length of the segment minus the
           number of constants minus one for the constant count.
           It includes the marker word, byte count, profile count
           and, on the X86/64 at least, any non-address constants.
           These are actually word values. */
        POLYUNSIGNED byteCount = (length - constCount - 1) * sizeof(PolyWord);
        fprintf(exportFile, "D%lu,%lu|", constCount, byteCount);

        // First the code.
        byte *u = (byte*)p;
        for (i = 0; i < byteCount; i++)
            fprintf(exportFile, "%02x", u[i]);

        putc('|', exportFile);
        // Now the constants.
        for (i = 0; i < constCount; i++)
        {
            printValue(cp[i]);
            if (i < constCount-1)
                putc(',', exportFile);
        }
        putc('|', exportFile);
        // Finally any constants in the code object.
        machineDependent->ScanConstantsWithinCode(p, this);
    }
    else if (p->IsStackObject())
    {
        StackObject *s = (StackObject*)p;
        PolyWord *q;
        ASSERT(! p->IsMutable());
        fprintf(exportFile, "Q%lu|", length);
        /* First the standard registers, space, pc, sp, hr. */
        fprintf(exportFile, "%lu,", s->p_space);

        /* pc may be TAGGED(0) indicating a retry. */
        PolyWord pc = PolyWord::FromCodePtr(s->p_pc);
        if (pc == TAGGED(0))
            fprintf(exportFile, "%lu,\n", TAGGED(0).AsUnsigned());
        else printCodeAddr(s->p_pc);

        putc(',', exportFile);
        fprintf(exportFile, "%%%lu+%lu,", myIndex, (POLYUNSIGNED)(s->p_sp-(PolyWord*)p)); /* Word offset of sp. */
        fprintf(exportFile, "%%%lu+%lu", myIndex, (POLYUNSIGNED)(s->p_hr-(PolyWord*)p)); /* Word offset of hr. */

        /* Checked registers. */
        fprintf(exportFile, " %lu|", s->p_nreg);
        PolyWord *stackStart = (PolyWord*)p;
        PolyWord *stackEnd = stackStart+length;
        for (i = 0; i < s->p_nreg; i++)
        {
            PolyWord r = s->p_reg[i];
            if (r.AsStackAddr() >= stackStart && r.AsStackAddr() < stackEnd)
                fprintf(exportFile, "%%%lu+%lu", myIndex, (POLYUNSIGNED)(r.AsStackAddr() - (PolyWord*)p));
            /* It seems we can have zeros in the registers, at least on the i386. */
            else if (r == PolyWord::FromUnsigned(0))
                fprintf(exportFile, "0");
            else
                printValue(r);
            if (i < s->p_nreg-1)
                putc(',', exportFile);
        }
        /* Unchecked registers, just as numbers. */
        POLYUNSIGNED nUnchecked = s->p_reg[i++].AsUnsigned();
        printf(" %lu|", nUnchecked);
        nUnchecked += i;
        for (; i < nUnchecked; i++)
        {
            fprintf(exportFile, "%lu", s->p_reg[i].AsUnsigned());
            if (i < nUnchecked-1)
                putc(',', exportFile);
        }
        q = s->p_sp;
        /* Now the values on the stack. */
        POLYUNSIGNED stackLength = length - (s->p_sp-stackStart);
        fprintf(exportFile, " %lu|", stackLength);
        q = s->p_sp;
        for (i = 0; i < stackLength; i++)
        {
            PolyWord r = q[i];
            /* A stack may contain a value which is an offset. */
            if (r.AsStackAddr() >= stackStart && r.AsStackAddr() < stackEnd)
                fprintf(exportFile, "%%%lu+%lu", myIndex, (POLYUNSIGNED)(r.AsStackAddr() - (PolyWord*)p));
            else printValue(r);
            if (i < stackLength-1)
                putc(',', exportFile);
        }

    }
    else /* Ordinary objects, essentially tuples. */
    {
        fprintf(exportFile, "O%lu|", length);
        for (i = 0; i < length; i++)
        {
            printValue(p->Get(i));
            if (i < length-1)
                putc(',', exportFile);
        }
    }
    fprintf(exportFile, "\n");
}

/* This is called for each constant within the code. 
   Print a relocation entry for the word and return a value that means
   that the offset is saved in original word. */
void PExport::ScanConstant(byte *addr, ScanRelocationKind code)
{
    PolyWord p = GetConstantValue(addr, code);
    // We put in all the values including tagged constants.
    PolyObject *obj = ObjCodePtrToPtr(addr);
    // Put in the byte offset and the relocation type code.
    fprintf(exportFile, "%lu,%d,", (POLYUNSIGNED)(addr - (byte*)obj), code);
    printValue(p); // The value to plug in.
    fprintf(exportFile, " ");
}

void PExport::exportStore(void)
{
    unsigned i;
    time_t now;
    time(&now);

    // Calculate a first guess for the map size based on the space size
    totalBytes = 0;
    void *startAddr = 0;
    for (i = 0; i < memTableEntries; i++)
    {
        if (i != ioMemEntry)
        {
            totalBytes += memTable[i].mtLength;
            // Get the lowest address.
            if (startAddr == 0 || memTable[i].mtAddr < startAddr)
                startAddr = memTable[i].mtAddr;
        }
    }
    // Create a map entry for each entry.  Allow five words per object.
    nMapSize = totalBytes/(sizeof(PolyWord)*5);
    pMap = (PolyObject **)malloc(sizeof(PolyObject*)*nMapSize);

    if (pMap == 0)
        throw MemoryException();

    // We want the entries in pMap to be in ascending
    // order of address to make searching easy so we need to process the areas
    // in order of increasing address, which may not be the order in memTable.
    indexOrder = (unsigned*)calloc(sizeof(unsigned), memTableEntries-1);
    if (indexOrder == 0)
        throw MemoryException();

    unsigned items = 0;
    for (i = 0; i < memTableEntries; i++)
    {
        if (i != ioMemEntry)
        {
            unsigned j = items;
            while (j > 0 && memTable[i].mtAddr < memTable[indexOrder[j-1]].mtAddr)
            {
                indexOrder[j] = indexOrder[j-1];
                j--;
            }
            indexOrder[j] = i;
            items++;
        }
    }
    ASSERT(items == memTableEntries-1);

    // Process the area in order of ascending address.
    for (i = 0; i < items; i++)
    {
        unsigned index = indexOrder[i];
        char *start = (char*)memTable[index].mtAddr;
        char *end = start + memTable[index].mtLength;
        for (PolyWord *p = (PolyWord*)start; p < (PolyWord*)end; )
        {
            p++;
            PolyObject *obj = (PolyObject*)p;
            if (nObjects == nMapSize)
            {
                // Need to expand the array.
                PolyObject **newMap =
                    (PolyObject **)realloc(pMap, (nMapSize + nMapSize/2)*sizeof(PolyObject*));
                if (newMap == 0)
                    throw MemoryException();
                pMap = newMap;

            }
            POLYUNSIGNED length = obj->Length();
            pMap[nObjects++] = obj;
            p += length;
        }
    }

    /* Start writing the information. */
    fprintf(exportFile, "Objects\t%lu\n", nObjects);
    fprintf(exportFile, "Root\t%lu\n", getIndex(rootFunction));


    // Generate each of the areas apart from the IO area.
    for (i = 0; i < memTableEntries; i++)
    {
        if (i != ioMemEntry) // Don't relocate the IO area
        {
            char *start = (char*)memTable[i].mtAddr;
            char *end = start + memTable[i].mtLength;
            for (PolyWord *p = (PolyWord*)start; p < (PolyWord*)end; )
            {
                p++;
                PolyObject *obj = (PolyObject*)p;
                POLYUNSIGNED length = obj->Length();
                printObject(obj);
                p += length;
            }
        }
    }

    fclose(exportFile); exportFile = NULL;
}


/*
Import a portable export file and load it into memory.
Creates "permanent" address entries in the global memory table.
*/

class SpaceAlloc
{
public:
    SpaceAlloc(bool isMut, POLYUNSIGNED def);
    ~SpaceAlloc();
    PolyObject *NewObj(POLYUNSIGNED objWords);
    bool AddToTable(void);

    POLYUNSIGNED defaultSize;
    POLYUNSIGNED currentSize;
    PolyWord *base;
    POLYUNSIGNED used;
    bool isMutable;
    unsigned spaceIndex;
};

SpaceAlloc::SpaceAlloc(bool isMut, POLYUNSIGNED def)
{
    isMutable = isMut;
    defaultSize = def;
    base = 0;
    currentSize = 0;
    used = 0;
    spaceIndex = 1;
}

SpaceAlloc::~SpaceAlloc()
{
    if (base)
        osMemoryManager->Free(base, currentSize*sizeof(PolyWord));
}

bool SpaceAlloc::AddToTable(void)
{
    if (base != 0)
    {
        // Add the new space to the permanent memory table.
        MemSpace* space = gMem.NewPermanentSpace(base, used, isMutable, false, spaceIndex++);
        if (space == 0)
        {
            fprintf(stderr, "Insufficient memory\n");
            return false;
        }
    }
    base = 0;
    return true;
}

// Allocate a new object.  May create a new space and add the old one to the permanent
// memory table if this is exhausted.
PolyObject *SpaceAlloc::NewObj(POLYUNSIGNED objWords)
{
    if (currentSize - used <= objWords)
    {
        // Need some more space.
        if (! AddToTable())
            return 0;
        POLYUNSIGNED size = defaultSize;
        if (size <= objWords)
            size = objWords+1;
        size_t iSpace = size*sizeof(PolyWord);
        base = (PolyWord*)osMemoryManager->Allocate(iSpace, PERMISSION_READ|PERMISSION_WRITE|PERMISSION_EXEC);
        currentSize = iSpace/sizeof(PolyWord);
        used = 0;
    }
    ASSERT(currentSize - used > objWords);
    PolyObject *newObj = (PolyObject*)(base+used+1);
    used += objWords+1;
    return newObj;
}

class PImport
{
public:
    PImport();
    ~PImport();
    bool DoImport(void);
    FILE *f;
    PolyObject *Root(void) { return objMap[nRoot]; }
private:
    PolyObject *NewObject(POLYUNSIGNED words, bool isMutable);
    bool ReadValue(PolyObject *p, POLYUNSIGNED i);
    bool GetValue(PolyWord *result);
    
    POLYUNSIGNED nObjects, nRoot;
    PolyObject **objMap;

    SpaceAlloc mutSpace, immutSpace;
};

PImport::PImport(): mutSpace(true, 1024*1024), immutSpace(false, 1024*1024)
{
    f = NULL;
    objMap = 0;
}

PImport::~PImport()
{
    if (f)
        fclose(f);
    free(objMap);
}

PolyObject *PImport::NewObject(POLYUNSIGNED words, bool isMutableObj)
{
    PolyObject *newObj = 0;
    if (isMutableObj)
        newObj = mutSpace.NewObj(words);
    else
        newObj = immutSpace.NewObj(words);
    if (newObj == 0)
        return 0;

    return newObj;

}

bool PImport::GetValue(PolyWord *result)
{
    int ch = getc(f);
    if (ch == '@')
    {
        /* Address of an object. */
        POLYUNSIGNED obj;
        fscanf(f, "%lu", &obj);
        ASSERT(obj < nObjects);
        *result = objMap[obj];
    }
    else if (ch == '$')
    {
        /* Code address. */
        POLYUNSIGNED obj, offset;
        fscanf(f, "%lu+%lu", &obj, &offset);
        ASSERT(obj < nObjects);
        PolyObject *q = objMap[obj];
        ASSERT(q->IsCodeObject());
        *result = PolyWord::FromCodePtr((PolyWord(q)).AsCodePtr() + offset); /* The offset is in bytes. */
    }
    else if ((ch >= '0' && ch <= '9') || ch == '-')
    {
        /* Tagged integer. */
        POLYSIGNED j;
        ungetc(ch, f);
        fscanf(f, "%ld", &j);
        /* The assertion may be false if we are porting to a machine
           with a shorter tagged representation. */
        ASSERT(j >= -MAXTAGGED-1 && j <= MAXTAGGED);
        *result = TAGGED(j);
    }
    else if (ch == '%')
    {
        /* Offset within the object.  Only in a stack. */
        POLYUNSIGNED obj, offset;
        fscanf(f, "%lu+%lu", &obj, &offset);
        ASSERT(obj < nObjects);
        PolyObject *q = objMap[obj];
        ASSERT(q->IsStackObject());
        ASSERT(offset >= 0 && offset < q->Length());
        *result = PolyWord::FromStackAddr(((PolyWord)q).AsStackAddr() + offset);
    }
    else if (ch == 'I')
    {
        /* IO entry number. */
        POLYUNSIGNED j;
        fscanf(f, "%lu", &j);
        ASSERT(j >= 0 && j < POLY_SYS_vecsize);
        *result = (PolyObject*)&gMem.ioSpace.bottom[j * IO_SPACING];
    }
    else if (ch == 'J')
    {
        /* IO entry number with offset. */
        POLYUNSIGNED j, offset;
        fscanf(f, "%lu+%lu", &j, &offset);
        ASSERT(j >= 0 && j < POLY_SYS_vecsize);
        PolyWord base = (PolyObject*)&gMem.ioSpace.bottom[j * IO_SPACING];
        *result = PolyWord::FromCodePtr(base.AsCodePtr() + offset);
    }
    else
    {
        fprintf(stderr, "Unexpected character in stream");
        return false;
    }
    return true;
}

/* Read a value and store it at the specified word. */
bool PImport::ReadValue(PolyObject *p, POLYUNSIGNED i)
{
    PolyWord result = TAGGED(0);
    if (GetValue(&result))
    {
        p->Set(i, result);
        return true;
    }
    else return false;
}

bool PImport::DoImport()
{
    int ch;
    POLYUNSIGNED objNo;

    ASSERT(gMem.npSpaces == 0);
    ASSERT(gMem.neSpaces == 0);
    ASSERT(gMem.ioSpace.bottom == 0);
    PolyWord *ioSpace = (PolyWord*)calloc(POLY_SYS_vecsize*IO_SPACING, sizeof(PolyWord));
    if (ioSpace == 0)
    {
        fprintf(stderr, "Unable to allocate memory\n");
        return false;
    }
    gMem.InitIOSpace(ioSpace, POLY_SYS_vecsize*IO_SPACING);

    ch = getc(f);
    /* Skip the "Mapping" line. */
    if (ch == 'M') { while (getc(f) != '\n') ; ch = getc(f); }
    ASSERT(ch == 'O'); /* Number of objects. */
    while (getc(f) != '\t') ;
    fscanf(f, "%lu", &nObjects);
    /* Create a mapping table. */
    objMap = (PolyObject**)calloc(nObjects, sizeof(PolyObject*));
    if (objMap == 0)
    {
        fprintf(stderr, "Unable to allocate memory\n");
        return false;
    }

    do
    {
        ch = getc(f);
    } while (ch == '\n');
    ASSERT(ch == 'R'); /* Root object number. */
    while (getc(f) != '\t') ;
    fscanf(f, "%lu", &nRoot);

    /* Now the objects themselves. */
    while (1)
    {
        bool     isMutable = false;
        unsigned    objBits = 0;
        POLYUNSIGNED  nWords, nBytes;
        do
        {
            ch = getc(f);
        } while (ch == '\r' || ch == '\n');
        if (ch == EOF) break;
        ungetc(ch, f);
        fscanf(f, "%lu", &objNo);
        ch = getc(f);
        ASSERT(ch == ':');
        ASSERT(objNo < nObjects);

        /* Modifiers, MNVW. */
        do
        {
            ch = getc(f);
            if (ch == 'M') { isMutable = true; objBits |= F_MUTABLE_BIT; }
            else if (ch == 'N') objBits |= F_NEGATIVE_BIT;
            if (ch == 'V') objBits |= F_NO_OVERWRITE;
            if (ch == 'W') objBits |= F_WEAK_BIT;
        } while (ch == 'M' || ch == 'N' || ch == 'L' || ch == 'V' || ch == 'W');

        /* Object type. */
        switch (ch)
        {
        case 'Q': /* Stack segment. */
            objBits |= F_STACK_OBJ;
        case 'O': /* Simple object. */
            fscanf(f, "%lu", &nWords);
            break;

        case 'B': /* Byte segment. */
            objBits |= F_BYTE_OBJ;
            fscanf(f, "%lu", &nBytes);
            /* Round up to appropriate number of words. */
            nWords = (nBytes + sizeof(PolyWord) -1) / sizeof(PolyWord);
            break;

        case 'S': /* String. */
            objBits |= F_BYTE_OBJ;
            /* The length is the number of characters. */
            fscanf(f, "%lu", &nBytes);
            /* Round up to appropriate number of words.  Need to add
               one PolyWord for the length PolyWord.  */
            nWords = (nBytes + sizeof(PolyWord) -1) / sizeof(PolyWord) + 1;
            break;

        case 'C': /* Code segment (old form). */
        case 'D': /* Code segment (new form). */
            objBits |= F_CODE_OBJ;
            /* Read the number of bytes of code and the number of words
               for constants. */
            fscanf(f, "%lu,%lu", &nWords, &nBytes);
            nWords += ch == 'C' ? 4 : 1; /* Add words for extras. */
            /* Add in the size of the code itself. */
            nWords += (nBytes + sizeof(PolyWord) -1) / sizeof(PolyWord);
            break;

        default:
            fprintf(stderr, "Invalid object type\n");
            return false;
        }

        PolyObject  *p = NewObject(nWords, isMutable);
        if (p == 0)
            return false;
        objMap[objNo] = p;
        /* Put in length PolyWord and flag bits. */
        p->SetLengthWord(nWords, objBits);

        /* Skip the object contents. */
        while (getc(f) != '\n') ;
    }

    /* Second pass - fill in the contents. */
    fseek(f, 0, SEEK_SET);
    /* Skip the information at the start. */
    ch = getc(f);
    if (ch == 'M')
    {
        while (getc(f) != '\n') ;
        ch = getc(f);
    }
    ASSERT(ch == 'O'); /* Number of objects. */
    while (getc(f) != '\n');
    ch = getc(f);
    ASSERT(ch == 'R'); /* Root object number. */
    while (getc(f) != '\n') ;

    while (1)
    {
        POLYUNSIGNED  nWords, nBytes, i;
        if (feof(f))
            break;
        fscanf(f, "%lu", &objNo);
        if (feof(f))
            break;
        ch = getc(f);
        ASSERT(ch == ':');
        ASSERT(objNo < nObjects);
        PolyObject * p = objMap[objNo];

        /* Modifiers, M or N. */
        do
        {
            ch = getc(f);
        } while (ch == 'M' || ch == 'N' || ch == 'L' || ch == 'V' || ch == 'W');

        /* Object type. */
        switch (ch)
        {
        case 'O': /* Simple object. */
            fscanf(f, "%lu", &nWords);
            ch = getc(f);
            ASSERT(ch == '|');
            ASSERT(nWords == p->Length());

            for (i = 0; i < nWords; i++)
            {
                if (! ReadValue(p, i))
                    return false;
                ch = getc(f);
                ASSERT((ch == ',' && i < nWords-1) ||
                       (ch == '\n' && i == nWords-1));
            }

            break;

        case 'B': /* Byte segment. */
            {
                byte *u = (byte*)p;
                fscanf(f, "%lu", &nBytes);
                ch = getc(f); ASSERT(ch == '|');
                for (i = 0; i < nBytes; i++)
                {
                    int n;
                    fscanf(f, "%02x", &n);
                    u[i] = n;
                }
                ch = getc(f);
                ASSERT(ch == '\n');
                break;
            }

        case 'S': /* String. */
            {
                PolyStringObject * ps = (PolyStringObject *)p;
                /* The length is the number of characters. */
                fscanf(f, "%lu", &nBytes);
                ch = getc(f); ASSERT(ch == '|');
                ps->length = nBytes;
                for (i = 0; i < nBytes; i++)
                {
                    int n;
                    fscanf(f, "%02x", &n);
                    ps->chars[i] = n;
                }
                ch = getc(f);
                ASSERT(ch == '\n');
                break;
            }

        case 'C': /* Code segment. */
        case 'D':
            {
                bool oldForm = ch == 'C';
                byte *u = (byte*)p;
                POLYUNSIGNED length = p->Length();
                /* Read the number of bytes of code and the number of words
                   for constants. */
                fscanf(f, "%lu,%lu", &nWords, &nBytes);
                /* Read the code. */
                ch = getc(f); ASSERT(ch == '|');
                for (i = 0; i < nBytes; i++)
                {
                    int n;
                    fscanf(f, "%02x", &n);
                    u[i] = n;
                }
                machineDependent->FlushInstructionCache(u, nBytes);
                ch = getc(f);
                ASSERT(ch == '|');
                /* Set the constant count. */
                p->Set(length-1, PolyWord::FromUnsigned(nWords));
                if (oldForm)
                {
                    p->Set(length-1-nWords-1, PolyWord::FromUnsigned(0)); /* Profile count. */
                    p->Set(length-1-nWords-3, PolyWord::FromUnsigned(0)); /* Marker word. */
                    p->Set(length-1-nWords-2, PolyWord::FromUnsigned((length-1-nWords-2)*sizeof(PolyWord)));
                    /* Check - the code should end at the marker word. */
                    ASSERT(nBytes == ((length-1-nWords-3)*sizeof(PolyWord)));
                }
                /* Read in the constants. */
                for (i = 0; i < nWords; i++)
                {
                    if (! ReadValue(p, i+length-nWords-1))
                        return false;
                    ch = getc(f);
                    ASSERT((ch == ',' && i < nWords-1) ||
                           ((ch == '\n' || ch == '|') && i == nWords-1));
                }
                // Read in any constants in the code.
                if (ch == '|')
                {
                    ch = getc(f);
                    while (ch != '\n')
                    {
                        ungetc(ch, f);
                        unsigned long offset;
                        int code;
                        fscanf(f, "%lu,%d", &offset, &code);
                        ch = getc(f);
                        ASSERT(ch == ',');
                        PolyWord constVal = TAGGED(0);
                        if (! GetValue(&constVal))
                            return false;
                        byte *toPatch = (byte*)p + offset;
                        ScanAddress::SetConstantValue(toPatch, constVal, (ScanRelocationKind)code);

                        do ch = getc(f); while (ch == ' ');
                    }
                }
                break;
            }

        case 'Q': /* Stack segment. */
            {
                StackObject *s = (StackObject*)p;
                POLYUNSIGNED n;
                POLYUNSIGNED length = p->Length();
                fscanf(f, "%lu", &nWords);
                ch = getc(f); ASSERT(ch == '|');

                /* Standard fields: size, pc, sp, hr. */
                fscanf(f, "%lu", &s->p_space);
                ch = getc(f); ASSERT(ch == ',');
                if (! ReadValue(p, (PolyWord*)&s->p_pc - (PolyWord*)p))
                    return false;
                ch = getc(f); ASSERT(ch == ',');
                if (! ReadValue(p, (PolyWord*)&s->p_sp - (PolyWord*)p))
                    return false;
                ch = getc(f); ASSERT(ch == ',');
                if (! ReadValue(p, (PolyWord*)&s->p_hr - (PolyWord*)p))
                    return false;

                /* Checked registers. */
                fscanf(f, "%lu", &n);
                s->p_nreg = n;
                ch = getc(f); ASSERT(ch == '|');
                for (i = 0; i < n; i++)
                {
                    if (! ReadValue(p, &s->p_reg[i] - (PolyWord*)p))
                        return false;
                    ch = getc(f);
                    ASSERT((ch == ',' && i < n-1) ||
                           (ch == ' ' && i == n-1));
                }
                /* Unchecked registers. */
                fscanf(f, "%lu", &n);
                s->p_reg[i] = PolyWord::FromUnsigned(n);
                ch = getc(f); ASSERT(ch == '|');
                for (i = 0; i < n; i++)
                {
                    POLYSIGNED n;
                    fscanf(f, "%ld", &n);
                    s->p_reg[s->p_nreg+i+1] = PolyWord::FromSigned(n);
                    ch = getc(f);
                }
                /* Stack values. */
                fscanf(f, "%lu", &n);
                ASSERT(n == length - (s->p_sp-(PolyWord*)p));
                ch = getc(f); ASSERT(ch == '|');
                for (i = 0; i < n; i++)
                {
                    if(! ReadValue(p, length-n+i))
                        return false;
                    ch = getc(f);
                    ASSERT((ch == ',' && i < n-1) ||
                           (ch == '\n' && i == n-1));
                }

                break;
            }

        default:
            fprintf(stderr, "Invalid object type\n");
            return false;
        }
    }
    return mutSpace.AddToTable() && immutSpace.AddToTable();
}

// Import a file in the portable format and return a pointer to the root object.
PolyObject *ImportPortable(const char *fileName)
{
    PImport pImport;
    pImport.f = fopen(fileName, "r");
    if (pImport.f == 0)
    {
        fprintf(stderr, "Unable to open file: %s\n", fileName);
        return 0;
    }
    if (pImport.DoImport())
        return pImport.Root();
    else
        return 0;
}
