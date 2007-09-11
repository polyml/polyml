/*
    Title:     Export memory as an ELF object file
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

#ifndef ELFExport_H_INCLUDED
#define ELFExport_H_INCLUDED

#include "config.h"

#include "scanaddrs.h" // For base class
#include "exporter.h"
#include <elf.h>

// Select 32 or 64 bit version depending on the word length
#if (SIZEOF_VOIDP == 8)
#define ElfXX_Addr  Elf64_Addr
#define ElfXX_Rel   Elf64_Rel
#define ElfXX_Rela  Elf64_Rela
#define ElfXX_Sym   Elf64_Sym
#define ElfXX_Ehdr  Elf64_Ehdr
#define ElfXX_Shdr  Elf64_Shdr
#define ELFXX_R_INFO(_y, _z)    ELF64_R_INFO(_y, _z)
#define ELFXX_ST_INFO(_y, _z)   ELF64_ST_INFO(_y, _z)
#define ELFCLASSXX      ELFCLASS64
#else
#define ElfXX_Addr  Elf32_Addr
#define ElfXX_Rel   Elf32_Rel
#define ElfXX_Rela  Elf32_Rela
#define ElfXX_Sym   Elf32_Sym
#define ElfXX_Ehdr  Elf32_Ehdr
#define ElfXX_Shdr  Elf32_Shdr
#define ELFXX_R_INFO(_y, _z)    ELF32_R_INFO(_y, _z)
#define ELFXX_ST_INFO(_y, _z)   ELF32_ST_INFO(_y, _z)
#define ELFCLASSXX              ELFCLASS32
#endif

class TaskData;

class ELFExport: public Exporter, public ScanAddress
{
public:
    ELFExport(): relocationCount(0), symbolCount(0), directReloc(0) {}
public:
    virtual void exportStore(void);

private:
    // ScanAddress overrides
    virtual void ScanConstant(byte *addrOfConst, ScanRelocationKind code);
    // At the moment we should only get calls to ScanConstant.
    virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }

private:
    void setRelocationAddress(void *p, ElfXX_Addr *reloc);
    PolyWord createRelocation(PolyWord p, void *relocAddr);
    void writeSymbol(const char *symbolName, long value, long size, int binding, int sttype, int section);
    unsigned long makeStringTableEntry(const char *str, ExportStringTable *stab);
    void alignFile(int align);
    void createStructsRelocation(unsigned area, POLYUNSIGNED offset, POLYSIGNED addend);

    unsigned relocationCount;
    unsigned symbolCount;

    // There are two tables - one is used for section names, the other for symbol names.
    ExportStringTable symStrings, sectionStrings;
    unsigned directReloc;
    bool useRela; // True if we should ElfXX_Rela rather than ElfXX_Rel    TaskData *taskData; // Needed for exceptions.
};

#endif
