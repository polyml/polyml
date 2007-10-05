/*
    Title:     Write out a database as a Mach object file
    Author:    David Matthews.

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

#include "config.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x)
#endif

// If we haven't got the Mach header files we shouldn't be building this.
#include <mach-o/loader.h>
#include <mach-o/reloc.h>
#include <mach-o/nlist.h>
#include <mach-o/ppc/reloc.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif

#include "globals.h"

#include "diagnostics.h"
#include "sys.h"
#include "machine_dep.h"
#include "gc.h"
#include "mpoly.h"
#include "scanaddrs.h"
#include "machoexport.h"
#include "run_time.h"
#include "version.h"
#include "polystring.h"

// We generate separate sections for each data area and an extra section for
// the tables.

// Generate the address relative to the start of the segment.
void MachoExport::setRelocationAddress(void *p, int32_t *reloc)
{
    unsigned area = findArea(p);
    POLYUNSIGNED offset = (char*)p - (char*)memTable[area].mtAddr;
    *reloc = offset;
}

/* Get the index corresponding to an address. */
PolyWord MachoExport::createRelocation(PolyWord p, void *relocAddr)
{
    void *addr = p.AsAddress();
    unsigned addrArea = findArea(addr);
    POLYUNSIGNED offset = (char*)addr - (char*)memTable[addrArea].mtAddr;

    // It looks as though struct relocation_info entries are only used
    // with GENERIC_RELOC_VANILLA types.
    struct relocation_info relInfo;
    setRelocationAddress(relocAddr, &relInfo.r_address);
    relInfo.r_symbolnum = addrArea + 1; // Section numbers start at 1
    relInfo.r_pcrel = 0;
    relInfo.r_length = 2; // 4 bytes
    relInfo.r_type = GENERIC_RELOC_VANILLA;
    relInfo.r_extern = 0; // r_symbolnum is a section number.  It should be 1 if we make the IO area a common.

    fwrite(&relInfo, sizeof(relInfo), 1, exportFile);
    relocationCount++;
    return PolyWord::FromUnsigned(offset);
}

/* This is called for each constant within the code. 
   Print a relocation entry for the word and return a value that means
   that the offset is saved in original word. */
void MachoExport::ScanConstant(byte *addr, ScanRelocationKind code)
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
    POLYUNSIGNED offset = (char*)a - (char*)memTable[aArea].mtAddr;

    switch (code)
    {
    case PROCESS_RELOC_DIRECT: // 32 bit address of target
        {
            struct relocation_info reloc;
            setRelocationAddress(addr, &reloc.r_address);
            reloc.r_symbolnum = aArea + 1; // Section numbers start at 1
            reloc.r_pcrel = 0;
            reloc.r_length = 2; // 4 bytes
            reloc.r_type = GENERIC_RELOC_VANILLA;
            reloc.r_extern = 0; // r_symbolnum is a section number.  It should be 1 if we make the IO area a common.

            for (unsigned i = 0; i < sizeof(PolyWord); i++)
            {
                addr[i] = (byte)(offset & 0xff);
                offset >>= 8;
            }
            fwrite(&reloc, sizeof(reloc), 1, exportFile);
            relocationCount++;
        }
        break;
#if(defined(HOSTARCHITECTURE_X86) || defined(HOSTARCHITECTURE_X86_64))
     case PROCESS_RELOC_I386RELATIVE:         // 32 bit relative address
        {
            // For Mach-O we seem to need to subtract the offset of this instruction
            // from the base of the segment.
            offset -= ((char*)addr - (char*)memTable[findArea(addr)].mtAddr) + 4;
            struct relocation_info reloc;
            setRelocationAddress(addr, &reloc.r_address);
            reloc.r_symbolnum = aArea + 1; // Section numbers start at 1
            reloc.r_pcrel = 1; // It's PC-relative.
            reloc.r_length = 2; // 4 bytes
            reloc.r_type = GENERIC_RELOC_VANILLA;
            reloc.r_extern = 0; // r_symbolnum is a section number.  It should be 1 if we make the IO area a common.

            for (unsigned i = 0; i < sizeof(PolyWord); i++)
            {
                addr[i] = (byte)(offset & 0xff);
                offset >>= 8;
            }
            fwrite(&reloc, sizeof(reloc), 1, exportFile);
            relocationCount++;
        }
        break;
#endif
#ifdef HOSTARCHITECTURE_PPC
    case PROCESS_RELOC_PPCDUAL16SIGNED:       // Power PC - two consecutive words
    case PROCESS_RELOC_PPCDUAL16UNSIGNED:
        {
            struct relocation_info reloc;
            setRelocationAddress(addr, &reloc.r_address);
            POLYUNSIGNED hi = offset >> 16; // N.B. No adjustment yet.
            POLYUNSIGNED lo = offset & 0xffff;
            // We use two consecutive words for our address but Mach-O requires separate
            // relocations for each.  It stores one half of the address in the instruction
            // itself and the other half is carried in a PPC_RELOC_PAIR relocation entry.
            // We need four relocations here in total.
            reloc.r_symbolnum = aArea + 1; // Section numbers start at 1
            reloc.r_extern = 0; // r_symbolnum is a section number.
            reloc.r_pcrel = 0;
            reloc.r_length = 2; // 4 bytes
            reloc.r_type = code == PROCESS_RELOC_PPCDUAL16SIGNED ? PPC_RELOC_HA16 : PPC_RELOC_HI16;
            fwrite(&reloc, sizeof(reloc), 1, exportFile);
            relocationCount++;
            // Next must be a "pair" containing the low-order part of the address.
            // The high-order part is stored in the instruction.
            reloc.r_symbolnum = 0xffffff; // Not sure why 
            reloc.r_type = PPC_RELOC_PAIR;
            reloc.r_extern = 0;
            reloc.r_pcrel = 0;
            reloc.r_address = lo;
            fwrite(&reloc, sizeof(reloc), 1, exportFile);
            relocationCount++;

            // Now the low-order part.
            setRelocationAddress(addr+sizeof(PolyWord), &reloc.r_address);
            reloc.r_symbolnum = aArea + 1; // Section numbers start at 1
            reloc.r_extern = 0; // r_symbolnum is a section number.
            reloc.r_pcrel = 0;
            reloc.r_length = 2; // 4 bytes
            reloc.r_type = PPC_RELOC_LO16;
            fwrite(&reloc, sizeof(reloc), 1, exportFile);
            relocationCount++;
            // Finally a "pair" containing the high-order part of the address to
            // match the low-order part in the instruction.
            reloc.r_symbolnum = 0xffffff; // Not sure why 
            reloc.r_type = PPC_RELOC_PAIR;
            reloc.r_extern = 0;
            reloc.r_pcrel = 0;
            reloc.r_address = hi; // Must NOT be adjusted for sign extension.
            fwrite(&reloc, sizeof(reloc), 1, exportFile);
            relocationCount++;
             // Adjust for sign extension and store in the instruction.
            if ((lo & 0x8000) && (code == PROCESS_RELOC_PPCDUAL16SIGNED)) hi++;
            POLYUNSIGNED *pt = (POLYUNSIGNED *)addr;
            // Store the offset into the instructions.
            pt[0] = (pt[0] & 0xffff0000) | hi;
            pt[1] = (pt[1] & 0xffff0000) | lo;
       }
        break;
#endif
        default:
            ASSERT(0); // Wrong type of relocation for this architecture.
    }
}

void MachoExport::writeSymbol(const char *symbolName, uint nType, uint nSect, uint offset)
{
    struct nlist symbol;
    memset(&symbol, 0, sizeof(symbol)); // Zero unused fields
    symbol.n_un.n_strx = stringTable.makeEntry(symbolName);
    symbol.n_type = nType;
    symbol.n_sect = nSect+1; // Sections count from 1.
    symbol.n_desc = REFERENCE_FLAG_DEFINED;
    symbol.n_value = offset;
    fwrite(&symbol, sizeof(symbol), 1, exportFile);
    symbolCount++;
}

// Set the file alignment.
void MachoExport::alignFile(int align)
{
    char pad[32]; // Maximum alignment
    int offset = ftell(exportFile);
    memset(pad, 0, sizeof(pad));
    if ((offset % align) == 0) return;
    fwrite(&pad, align - (offset % align), 1, exportFile);
}

void MachoExport::createStructsRelocation(unsigned sect, POLYUNSIGNED offset)
{
    struct relocation_info reloc;
    reloc.r_address = offset;
    reloc.r_symbolnum = sect+1; // Section numbers start at 1
    reloc.r_pcrel = 0;
    reloc.r_length = 2; // 4 bytes
    reloc.r_type = GENERIC_RELOC_VANILLA;
    reloc.r_extern = 0; // r_symbolnum is a section number.

    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;
}

void MachoExport::exportStore(void)
{
    PolyWord    *p;
    struct mach_header fhdr;
    struct segment_command sHdr;
    struct symtab_command symTab;
    struct section *sections = 0;
    unsigned i;

    // Write out initial values for the headers.  These are overwritten at the end.
    // File header
    memset(&fhdr, 0, sizeof(fhdr));
    fhdr.magic = MH_MAGIC; // Feed Face (0xfeedface)
    fhdr.filetype = MH_OBJECT;
    fhdr.ncmds = 2; // One for the segment and one for the symbol table.
    fhdr.sizeofcmds = sizeof(struct segment_command) + sizeof(struct section)*(memTableEntries+1)
        + sizeof(struct symtab_command);
    fhdr.flags = 0;
    // The machine needs to match the machine we're compiling for
    // even if this is actually portable code.
#if defined(HOSTARCHITECTURE_X86)
    fhdr.cputype = CPU_TYPE_I386;
    fhdr.cpusubtype = CPU_SUBTYPE_I386_ALL;
#elif defined(HOSTARCHITECTURE_PPC)
    fhdr.cputype = CPU_TYPE_POWERPC;
    fhdr.cpusubtype = CPU_SUBTYPE_POWERPC_ALL;
#else
#error "No support for exporting on this architecture"
#endif
    fwrite(&fhdr, sizeof(fhdr), 1, exportFile); // Write it for the moment.

    // Segment header.
    memset(&sHdr, 0, sizeof(struct segment_command));
    sHdr.cmd = LC_SEGMENT;
    sHdr.nsects = memTableEntries+1;
    sHdr.cmdsize = sizeof(struct segment_command) + sizeof(struct section) * sHdr.nsects;
    // Add up the sections to give the file size
    sHdr.filesize = 0;
    for (i = 0; i < memTableEntries; i++)
        sHdr.filesize += memTable[i].mtLength; // Do we need any alignment?
    sHdr.filesize += sizeof(exportDescription) + memTableEntries * sizeof(memoryTableEntry);
    sHdr.vmsize = sHdr.filesize; // Set them the same since we don't have any "common" area.
    // sHdr.fileOff is set later.
    sHdr.maxprot = VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE;
    sHdr.initprot = VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE;
    sHdr.flags = 0;

    // Write it initially.
    fwrite(&sHdr, sizeof(sHdr), 1, exportFile);

    sections = new struct section [memTableEntries+1]; // Plus one for the tables.

    // Section headers.
    for (i = 0; i < memTableEntries; i++)
    {
        memset(&sections[i], 0, sizeof(struct section));
        sprintf(sections[i].sectname, "poly%1u", i);
        sprintf(sections[i].segname, "POLY%1u", i);
        //sections[i].offset is set later
        //sections[i].reloff is set later (except for IO area which doesn't have any)
        //sections[i].nreloc is set later (except for IO area which doesn't have any)
        sections[i].align = 3; // 8 byte alignment
        sections[i].size = memTable[i].mtLength;
        sections[i].flags = S_ATTR_LOC_RELOC | S_ATTR_SOME_INSTRUCTIONS | S_REGULAR; // 
    }

    // Final section for descriptor,
    memset(&sections[memTableEntries], 0, sizeof(struct section));
    sprintf(sections[memTableEntries].sectname, "__data");
    sprintf(sections[memTableEntries].segname, "__DATA");
    sections[memTableEntries].size = sizeof(exportDescription) + memTableEntries * sizeof(memoryTableEntry);
    //sections.offset is set later
    //sections.reloff is set later
    sections[memTableEntries].flags = S_ATTR_LOC_RELOC | S_REGULAR; // 
    sections[memTableEntries].align = 3; // 8 byte alignment

    // Write it out for the moment.
    fwrite(sections, sizeof(struct section), memTableEntries+1, exportFile);

    // Symbol table header.
    memset(&symTab, 0, sizeof(struct symtab_command));
    symTab.cmd = LC_SYMTAB;
    symTab.cmdsize = sizeof(struct symtab_command);
    //symTab.symoff is set later
    //symTab.nsyms is set later
    //symTab.stroff is set later
    //symTab.strsize is set later
    fwrite(&symTab, sizeof(struct symtab_command), 1, exportFile);

    // Create the symbol table first before we mess up the addresses by turning them
    // into relocations.
    symTab.symoff = ftell(exportFile);
    // Global symbols: Just one.  Mach prefixes symbols with an underscore.
    writeSymbol("_poly_exports", N_EXT | N_SECT, memTableEntries, 0); // The export table comes first
    // We create local symbols because they make debugging easier.  They may also
    // mean that we can use the usual Unix profiling tools.
    writeSymbol("memTable", N_SECT, memTableEntries, sizeof(exportDescription)); // Then the memTable.
    for (i = 0; i < memTableEntries; i++)
    {
        if (i == ioMemEntry)
            writeSymbol("ioarea", N_SECT, i, 0);
        else {
            char buff[50];
            sprintf(buff, "area%0d", i);
            writeSymbol(buff, N_SECT, i, 0);
            // See if we can find the names of any functions.
            char *start = (char*)memTable[i].mtAddr;
            char *end = start + memTable[i].mtLength;
            for (p = (PolyWord*)start; p < (PolyWord*)end; )
            {
                p++;
                PolyObject *obj = (PolyObject*)p;
                POLYUNSIGNED length = obj->Length();
                if (length != 0 && obj->IsCodeObject())
                {
                    PolyWord *name = obj->ConstPtrForCode();
                    // Copy as much of the name as will fit and ignore any extra.
                    // Do we need to worry about duplicates?
                    (void)Poly_string_to_C(*name, buff, sizeof(buff));
                    writeSymbol(buff, N_SECT, i, (char*)p - start);
                }
                p += length;
            }
        }
    }
    symTab.nsyms = symbolCount;

    // Create and write out the relocations.
    for (i = 0; i < memTableEntries; i++)
    {
        if (i != ioMemEntry) // Don't relocate the IO area
        {
            sections[i].reloff = ftell(exportFile);
            relocationCount = 0;
            // Create the relocation table and turn all addresses into offsets.
            char *start = (char*)memTable[i].mtAddr;
            char *end = start + memTable[i].mtLength;
            for (p = (PolyWord*)start; p < (PolyWord*)end; )
            {
                p++;
                PolyObject *obj = (PolyObject*)p;
                POLYUNSIGNED length = obj->Length();
                relocateObject(obj);
                if (length != 0 && obj->IsCodeObject())
                    machineDependent->ScanConstantsWithinCode(obj, this);
                p += length;
            }
            sections[i].nreloc = relocationCount;
        }
    }

    // Additional relocations for the descriptors.
    relocationCount = 0;
    sections[memTableEntries].reloff = ftell(exportFile);

    // Address of "memTable" within "exports". We can't use createRelocation because
    // the position of the relocation is not in either the mutable or the immutable area.
    createStructsRelocation(memTableEntries, offsetof(exportDescription, memTable));

    // Address of "rootFunction" within "exports"
    unsigned rootAddrArea = findArea(rootFunction);
    POLYSIGNED rootOffset = (char*)rootFunction - (char*)memTable[rootAddrArea].mtAddr;
    createStructsRelocation(rootAddrArea, offsetof(exportDescription, rootFunction));

    // Addresses of the areas within memtable.
    for (i = 0; i < memTableEntries; i++)
    {
        createStructsRelocation(i,
            sizeof(exportDescription) + i * sizeof(memoryTableEntry) + offsetof(memoryTableEntry, mtAddr));
    }
    sections[memTableEntries].nreloc = relocationCount;

    // The symbol name table
    symTab.stroff = ftell(exportFile);
    fwrite(stringTable.strings, stringTable.stringSize, 1, exportFile);
    symTab.strsize = stringTable.stringSize;
    alignFile(4);

    // Now the binary data.
    for (i = 0; i < memTableEntries; i++)
    {
        // Record where it starts and write it.
        sections[i].offset = ftell(exportFile);
        if (i == 0) sHdr.fileoff = sections[i].offset; // I don't know if this is necessary
        fwrite(memTable[i].mtAddr, 1, memTable[i].mtLength, exportFile);
    }

    exportDescription exports;
    memset(&exports, 0, sizeof(exports));
    memset(memTable, 0, sizeof(memTable));
    exports.structLength = sizeof(exportDescription);
    exports.memTableSize = sizeof(memoryTableEntry);
    exports.memTableEntries = memTableEntries;
    exports.ioIndex = 0; // The io entry is the first in the memory table
    exports.memTable = (memoryTableEntry *)sizeof(exportDescription); // It follows immediately after this.
    // Set the value to be the offset relative to the base of the area.  We have set a relocation
    // already which will add the base of the area.
    exports.rootFunction = (void*)rootOffset;
    exports.timeStamp = time(NULL);
    exports.ioSpacing = ioSpacing;
    exports.architecture = machineDependent->MachineArchitecture();
    exports.rtsVersion = POLY_version_number;

    // Set the address values to zero before we write.  They will always
    // be relative to their section.
    for (i = 0; i < memTableEntries; i++)
        memTable[i].mtAddr = 0;

    sections[memTableEntries].offset = ftell(exportFile);
    fwrite(&exports, sizeof(exports), 1, exportFile);
    fwrite(memTable, sizeof(memoryTableEntry), memTableEntries, exportFile);

    // Rewind to rewrite the headers with the actual offsets.
    rewind(exportFile);
    fwrite(&fhdr, sizeof(fhdr), 1, exportFile); // File header
    fwrite(&sHdr, sizeof(sHdr), 1, exportFile); // Segment header
    fwrite(sections, sizeof(struct section), memTableEntries+1, exportFile); // Section headers
    fwrite(&symTab, sizeof(struct symtab_command), 1, exportFile); // Symbol table header
    fclose(exportFile); exportFile = NULL;

    delete[](sections);
}
