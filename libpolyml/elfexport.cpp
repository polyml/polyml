/*
    Title:     Write out a database as an ELF object file
    Author:    David Matthews.

    Copyright (c) 2006-7, 2011 David C. J. Matthews

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

#ifdef HAVE_ELF_H
#include <elf.h>
#elif defined(HAVE_ELF_ABI_H)
#include <elf_abi.h>
#include <machine/reloc.h>

#ifndef EM_X86_64
#define EM_X86_64 EM_AMD64
#endif

#if defined(HOSTARCHITECTURE_X86_64)

#ifndef R_386_PC32
#define R_386_PC32 R_X86_64_PC32
#endif

#ifndef R_386_32
#define R_386_32 R_X86_64_32
#endif

#ifndef R_X86_64_64
#define R_X86_64_64 R_X86_64_64
#endif

#endif /* HOSTARCHITECTURE_X86_64 */

#endif

// Solaris seems to put processor-specific constants in separate files
#ifdef HAVE_SYS_ELF_SPARC_H
#include <sys/elf_SPARC.h>
#endif
#ifdef HAVE_SYS_ELF_386_H
#include <sys/elf_386.h>
#endif
#ifdef HAVE_SYS_ELF_AMD64_H
#include <sys/elf_amd64.h>
#endif

// Android has the ARM relocation symbol here
#ifdef HAVE_ASM_ELF_H
#include <asm/elf.h>
#endif

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
#include "elfexport.h"
#include "run_time.h"
#include "version.h"
#include "polystring.h"

#define sym_last_local_sym sym_data_section

// The first two symbols are special:
// Zero is always special in ELF
// 1 is used for the data section
#define EXTRA_SYMBOLS   2
static unsigned AreaToSym(unsigned area) { return area+EXTRA_SYMBOLS; }

// Section table entries
enum {
    sect_initial = 0,
    sect_sectionnametable,
    sect_stringtable,
    // Data and relocation entries come in here.
    sect_data
    // Finally the symbol table
};

// Generate the address relative to the start of the segment.
void ELFExport::setRelocationAddress(void *p, ElfXX_Addr *reloc)
{
    unsigned area = findArea(p);
    POLYUNSIGNED offset = (char*)p - (char*)memTable[area].mtAddr;
    *reloc = offset;
}

/* Get the index corresponding to an address. */
PolyWord ELFExport::createRelocation(PolyWord p, void *relocAddr)
{
    void *addr = p.AsAddress();
    unsigned addrArea = findArea(addr);
    POLYUNSIGNED offset = (char*)addr - (char*)memTable[addrArea].mtAddr;

    if (useRela)
    {
        ElfXX_Rela reloc;
        // Set the offset within the section we're scanning.
        setRelocationAddress(relocAddr, &reloc.r_offset);
#ifdef HOSTARCHITECTURE_MIPS64
        reloc.r_sym = AreaToSym(addrArea);
        reloc.r_ssym = 0;
        reloc.r_type = directReloc;
        reloc.r_type2 = 0;
        reloc.r_type3 = 0;
#else
        reloc.r_info = ELFXX_R_INFO(AreaToSym(addrArea), directReloc);
#endif
        reloc.r_addend = offset;
        fwrite(&reloc, sizeof(reloc), 1, exportFile);
        relocationCount++;
        return PolyWord::FromUnsigned(0);
    }
    else {
        ElfXX_Rel reloc;
        setRelocationAddress(relocAddr, &reloc.r_offset);
#ifdef HOSTARCHITECTURE_MIPS64
        reloc.r_sym = AreaToSym(addrArea);
        reloc.r_ssym = 0;
        reloc.r_type = directReloc;
        reloc.r_type2 = 0;
        reloc.r_type3 = 0;
#else
        reloc.r_info = ELFXX_R_INFO(AreaToSym(addrArea), directReloc);
#endif
        fwrite(&reloc, sizeof(reloc), 1, exportFile);
        relocationCount++;
        return PolyWord::FromUnsigned(offset);
    }
}

/* This is called for each constant within the code. 
   Print a relocation entry for the word and return a value that means
   that the offset is saved in original word. */
void ELFExport::ScanConstant(byte *addr, ScanRelocationKind code)
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
    case PROCESS_RELOC_DIRECT: // 32 or 64 bit address of target
        {
            PolyWord r = createRelocation(p, addr);
            POLYUNSIGNED w = r.AsUnsigned();
            for (unsigned i = 0; i < sizeof(PolyWord); i++)
            {
                addr[i] = (byte)(w & 0xff);
                w >>= 8;
            }
        }
        break;
#if(defined(HOSTARCHITECTURE_X86) || defined(HOSTARCHITECTURE_X86_64))
#ifdef HOSTARCHITECTURE_X86
#define R_PC_RELATIVE R_386_PC32
#else
#define R_PC_RELATIVE R_X86_64_PC32
#endif
     case PROCESS_RELOC_I386RELATIVE:         // 32 bit relative address
        {
            if (useRela)
            {
                ElfXX_Rela reloc;
                setRelocationAddress(addr, &reloc.r_offset);
                // We seem to need to subtract 4 bytes to get the correct offset in ELF
                offset -= 4;
                reloc.r_info = ELFXX_R_INFO(AreaToSym(aArea), R_PC_RELATIVE);
                reloc.r_addend = offset;
                // Clear the field.  Even though it's not supposed to be used with Rela the
                // Linux linker at least seems to add the value in here sometimes.
                memset(addr, 0, 4);
                fwrite(&reloc, sizeof(reloc), 1, exportFile);
            }
            else
            {
                ElfXX_Rel reloc;
                setRelocationAddress(addr, &reloc.r_offset);
                 // We seem to need to subtract 4 bytes to get the correct offset in ELF
                offset -= 4;
                reloc.r_info = ELFXX_R_INFO(AreaToSym(aArea), R_PC_RELATIVE);
                for (unsigned i = 0; i < 4; i++)
                {
                    addr[i] = (byte)(offset & 0xff);
                    offset >>= 8;
                }
                fwrite(&reloc, sizeof(reloc), 1, exportFile);
            }
            relocationCount++;
        }
        break;
#endif
        default:
            ASSERT(0); // Wrong type of relocation for this architecture.
    }
}

unsigned long ELFExport::makeStringTableEntry(const char *str, ExportStringTable *stab)
{
    if (str == NULL || str[0] == 0)
        return 0; // First entry is the null string.
    else
        return stab->makeEntry(str);
}

void ELFExport::writeSymbol(const char *symbolName, long value, long size, int binding, int sttype, int section)
{
    ElfXX_Sym symbol;
    memset(&symbol, 0, sizeof(symbol)); // Zero unused fields
    symbol.st_name = makeStringTableEntry(symbolName, &symStrings);
    symbol.st_value = value;
    symbol.st_size = size;
    symbol.st_info = ELFXX_ST_INFO(binding, sttype);
    symbol.st_other = 0;
    symbol.st_shndx = section;
    fwrite(&symbol, sizeof(symbol), 1, exportFile);
    symbolCount++;
}

// Set the file alignment.
void ELFExport::alignFile(int align)
{
    char pad[32] = {0}; // Maximum alignment
    int offset = ftell(exportFile);
    if ((offset % align) == 0) return;
    fwrite(&pad, align - (offset % align), 1, exportFile);
}

void ELFExport::createStructsRelocation(unsigned sym, POLYUNSIGNED offset, POLYSIGNED addend)
{
    if (useRela)
    {
        ElfXX_Rela reloc;
#ifdef HOSTARCHITECTURE_MIPS64
        reloc.r_sym = sym;
        reloc.r_ssym = 0;
        reloc.r_type = directReloc;
        reloc.r_type2 = 0;
        reloc.r_type3 = 0;
#else
        reloc.r_info = ELFXX_R_INFO(sym, directReloc);
#endif
        reloc.r_offset = offset;
        reloc.r_addend = addend;
        fwrite(&reloc, sizeof(reloc), 1, exportFile);
        relocationCount++;
    }
    else
    {
        ElfXX_Rel reloc;
#ifdef HOSTARCHITECTURE_MIPS64
        reloc.r_sym = sym;
        reloc.r_ssym = 0;
        reloc.r_type = directReloc;
        reloc.r_type2 = 0;
        reloc.r_type3 = 0;
#else
        reloc.r_info = ELFXX_R_INFO(sym, directReloc);
#endif
        reloc.r_offset = offset;
        fwrite(&reloc, sizeof(reloc), 1, exportFile);
        relocationCount++;
    }
}

void ELFExport::exportStore(void)
{
    PolyWord    *p;
    ElfXX_Ehdr fhdr;
    ElfXX_Shdr *sections = 0;
    unsigned numSections = 6 + 2*memTableEntries;
    // The symbol table comes at the end.
    unsigned sect_symtab = sect_data + 2*memTableEntries + 2;
    
    unsigned i;

    // Both the string tables have an initial null entry.
    symStrings.makeEntry("");
    sectionStrings.makeEntry("");

    // Write out initial values for the headers.  These are overwritten at the end.
    // File header
    memset(&fhdr, 0, sizeof(fhdr));
    fhdr.e_ident[EI_MAG0] = 0x7f;
    fhdr.e_ident[EI_MAG1] = 'E';
    fhdr.e_ident[EI_MAG2] = 'L';
    fhdr.e_ident[EI_MAG3] = 'F';
    fhdr.e_ident[EI_CLASS] = ELFCLASSXX; // ELFCLASS32 or ELFCLASS64
    fhdr.e_ident[EI_VERSION] = EV_CURRENT;
    {
        union { unsigned long wrd; char chrs[sizeof(unsigned long)]; } endian;
        endian.wrd = 1;
        if (endian.chrs[0] == 0)
            fhdr.e_ident[EI_DATA] = ELFDATA2MSB; // Big endian
        else
            fhdr.e_ident[EI_DATA] = ELFDATA2LSB; // Little endian
    }
    fhdr.e_type = ET_REL;
    // The machine needs to match the machine we're compiling for
    // even if this is actually portable code.
#if defined(HOSTARCHITECTURE_X86)
    fhdr.e_machine = EM_386;
    directReloc = R_386_32;
    useRela = false;
#elif defined(HOSTARCHITECTURE_PPC)
    fhdr.e_machine = EM_PPC;
    directReloc = R_PPC_ADDR32;
    useRela = true;
#elif defined(HOSTARCHITECTURE_PPC64)
    fhdr.e_machine = EM_PPC64;
    directReloc = R_PPC64_ADDR64;
    useRela = true;
#elif defined(HOSTARCHITECTURE_S390)
    fhdr.e_machine = EM_S390;
    directReloc = R_390_32;
    useRela = true;
#elif defined(HOSTARCHITECTURE_S390X)
    fhdr.e_machine = EM_S390;
    directReloc = R_390_64;
    useRela = true;
#elif defined(HOSTARCHITECTURE_SPARC)
    fhdr.e_machine = EM_SPARC;
    directReloc = R_SPARC_32;
    useRela = true;
    /* Sparc/Solaris, at least 2.8, requires ELF32_Rela relocations.  For some reason,
       though, it adds the value in the location being relocated (as with ELF32_Rel
       relocations) as well as the addend. To be safe, whenever we use an ELF32_Rela
       relocation we always zero the location to be relocated. */
#elif defined(HOSTARCHITECTURE_X86_64)
    /* It seems Solaris/X86-64 only supports ELF64_Rela relocations.  It appears that
       Linux will support either so we now use Rela on X86-64. */
    fhdr.e_machine = EM_X86_64;
    directReloc = R_X86_64_64;
    useRela = true;
#elif defined(HOSTARCHITECTURE_ARM)
#ifndef EF_ARM_EABI_VER4 
#define EF_ARM_EABI_VER4     0x04000000
#endif
    // When linking ARM binaries the linker checks the ABI version.  We
    // need to set the version to the same as the libraries. 
    // GCC currently uses version 4.
    fhdr.e_machine = EM_ARM;
    directReloc = R_ARM_ABS32;
    useRela = false;
    fhdr.e_flags = EF_ARM_EABI_VER4;
#elif defined(HOSTARCHITECTURE_IA64)
    fhdr.e_machine = EM_IA_64;
    directReloc = R_IA64_DIR64LSB;
    fhdr.e_flags = EF_IA_64_ABI64;
    useRela = true;
#elif defined(HOSTARCHITECTURE_AARCH64)
    fhdr.e_machine = EM_AARCH64;
    directReloc = R_AARCH64_ABS64;
    useRela = true;
#elif defined(HOSTARCHITECTURE_M68K)
    fhdr.e_machine = EM_68K;
    directReloc = R_68K_32;
    useRela = true;
#elif defined(HOSTARCHITECTURE_MIPS)
    fhdr.e_machine = EM_MIPS;
    directReloc = R_MIPS_32;
#ifdef __PIC__
    fhdr.e_flags = EF_MIPS_CPIC;
#endif
    useRela = true;
#elif defined(HOSTARCHITECTURE_MIPS64)
    fhdr.e_machine = EM_MIPS;
    directReloc = R_MIPS_64;
    fhdr.e_flags = EF_MIPS_ARCH_64;
#ifdef __PIC__
    fhdr.e_flags |= EF_MIPS_CPIC;
#endif
    useRela = true;
#elif defined(HOSTARCHITECTURE_ALPHA)
    fhdr.e_machine = EM_ALPHA;
    directReloc = R_ALPHA_REFQUAD;
    useRela = true;
#else
#error "No support for exporting on this architecture"
#endif
    fhdr.e_version = EV_CURRENT;
    fhdr.e_shoff = sizeof(fhdr); // Offset to section header - immediately follows
    fhdr.e_ehsize = sizeof(fhdr);
    fhdr.e_shentsize = sizeof(ElfXX_Shdr);
    fhdr.e_shnum = numSections;
    fhdr.e_shstrndx = sect_sectionnametable; // Section name table section index;
    fwrite(&fhdr, sizeof(fhdr), 1, exportFile); // Write it for the moment.

    sections = new ElfXX_Shdr[numSections];
    memset(sections, 0, sizeof(ElfXX_Shdr) * numSections); // Necessary?

    // Set up the section header but don't write it yet.

    // Section 0 - all zeros
    sections[sect_initial].sh_type = SHT_NULL;
    sections[sect_initial].sh_link = SHN_UNDEF;

    // Section name table.
    sections[sect_sectionnametable].sh_name = makeStringTableEntry(".shstrtab", &sectionStrings);
    sections[sect_sectionnametable].sh_type = SHT_STRTAB;
    sections[sect_sectionnametable].sh_addralign = sizeof(char);
    // sections[sect_sectionnametable].sh_offset is set later
    // sections[sect_sectionnametable].sh_size is set later

    // Symbol name table.
    sections[sect_stringtable].sh_name = makeStringTableEntry(".strtab", &sectionStrings);
    sections[sect_stringtable].sh_type = SHT_STRTAB;
    sections[sect_stringtable].sh_addralign = sizeof(char);
    // sections[sect_stringtable].sh_offset is set later
    // sections[sect_stringtable].sh_size is set later

    unsigned long dataName = makeStringTableEntry(".data", &sectionStrings);
    unsigned long dataRelName = makeStringTableEntry(useRela ? ".rela.data" : ".rel.data", &sectionStrings);
    unsigned long textName = makeStringTableEntry(".text", &sectionStrings);
    unsigned long textRelName = makeStringTableEntry(useRela ? ".rela.text" : ".rel.text", &sectionStrings);

    // Main data sections.  Each one has a relocation section.
    for (i=0; i < memTableEntries; i++)
    {
        unsigned s = sect_data + i*2;
        sections[s].sh_type = SHT_PROGBITS;
        sections[s].sh_addralign = 8; // 8-byte alignment

        if (memTable[i].mtFlags & MTF_WRITEABLE)
        {
            // Mutable areas
            ASSERT(!(memTable[i].mtFlags & MTF_EXECUTABLE)); // Executable areas can't be writable.
            sections[s].sh_name = dataName;
            sections[s].sh_flags = SHF_WRITE | SHF_ALLOC;
            sections[s+1].sh_name = dataRelName; // Name of relocation section
        }
        else
        {
            // Immutable areas are marked as executable.
            sections[s].sh_name = textName;
            sections[s].sh_flags = SHF_ALLOC | SHF_EXECINSTR;
            sections[s+1].sh_name = textRelName; // Name of relocation section
        }
        // sections[s].sh_size is set later
        // sections[s].sh_offset is set later.
        // sections[s].sh_size is set later.

        // Relocation section
        sections[s+1].sh_type = useRela ? SHT_RELA : SHT_REL; // Contains relocation with/out explicit addends (ElfXX_Rel)
        sections[s+1].sh_link = sect_symtab; // Index to symbol table
        sections[s+1].sh_info = s; // Applies to the data section
        sections[s+1].sh_addralign = sizeof(long); // Align to a word
        sections[s+1].sh_entsize = useRela ? sizeof(ElfXX_Rela) : sizeof(ElfXX_Rel);
        // sections[s+1].sh_offset is set later.
        // sections[s+1].sh_size is set later.
    }

    // Table data - Poly tables that describe the memory layout.
    unsigned sect_table_data = sect_data+2*memTableEntries;

    sections[sect_table_data].sh_name = dataName;
    sections[sect_table_data].sh_type = SHT_PROGBITS;
    sections[sect_table_data].sh_flags = SHF_WRITE | SHF_ALLOC;
    sections[sect_table_data].sh_addralign = 8; // 8-byte alignment
    // Table relocation
    sections[sect_table_data+1].sh_name = dataRelName;
    sections[sect_table_data+1].sh_type = useRela ? SHT_RELA : SHT_REL; // Contains relocation with/out explicit addends (ElfXX_Rel)
    sections[sect_table_data+1].sh_link = sect_symtab; // Index to symbol table
    sections[sect_table_data+1].sh_info = sect_table_data; // Applies to table section
    sections[sect_table_data+1].sh_addralign = sizeof(long); // Align to a word
    sections[sect_table_data+1].sh_entsize = useRela ? sizeof(ElfXX_Rela) : sizeof(ElfXX_Rel);

    // Symbol table.
    sections[sect_symtab].sh_name = makeStringTableEntry(".symtab", &sectionStrings);
    sections[sect_symtab].sh_type = SHT_SYMTAB;
    sections[sect_symtab].sh_link = sect_stringtable; // String table to use
    sections[sect_symtab].sh_addralign = sizeof(long); // Align to a word
    sections[sect_symtab].sh_entsize = sizeof(ElfXX_Sym);
    // sections[sect_symtab].sh_info is set later
    // sections[sect_symtab].sh_size is set later
    // sections[sect_symtab].sh_offset is set later

    // First the symbol table.
    alignFile(sections[sect_symtab].sh_addralign);
    sections[sect_symtab].sh_offset = ftell(exportFile);
    writeSymbol("", 0, 0, 0, 0, 0); // Initial symbol
    // Write the local symbols first.
    writeSymbol("", 0, 0, STB_LOCAL, STT_SECTION, sect_data); // .data section

    // Create symbols for the address areas.  AreaToSym assumes these come first.
    for (i = 0; i < memTableEntries; i++)
    {
        if (i == ioMemEntry)
            writeSymbol("ioarea", 0, 0, STB_LOCAL, STT_OBJECT, sect_data+i*2);
        else {
            char buff[50];
            sprintf(buff, "area%1u", i);
            writeSymbol(buff, 0, 0, STB_LOCAL, STT_OBJECT, sect_data+i*2);
        }
    }

    // Extra symbols to help debugging.
    for (i = 0; i < memTableEntries; i++)
    {
        if (i != ioMemEntry)
        {
            char buff[50];
            // Write the names of the functions as local symbols.  This isn't necessary
            // but it makes debugging easier since the function names appear in gdb.
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
                    writeSymbol(buff, ((char*)p - start), 0, STB_LOCAL, STT_OBJECT, sect_data+i*2);
                }
                p += length;
            }
        }
    }

    // Global symbols - Just one: poly_exports
    writeSymbol("poly_exports", 0, 
        sizeof(exportDescription)+sizeof(memoryTableEntry)*memTableEntries,
        STB_GLOBAL, STT_OBJECT, sect_table_data);

    sections[sect_symtab].sh_info = symbolCount-1; // One more than last local sym
    sections[sect_symtab].sh_size = sizeof(ElfXX_Sym) * symbolCount;

    // Write the relocations.

    for (i = 0; i < memTableEntries; i++)
    {
        unsigned relocSection = sect_data+i*2+1;
        alignFile(sections[relocSection].sh_addralign);
        sections[relocSection].sh_offset = ftell(exportFile);
        relocationCount = 0;
        if (i != ioMemEntry) // Don't relocate the IO area
        {
            // Create the relocation table and turn all addresses into offsets.
            char *start = (char*)memTable[i].mtAddr;
            char *end = start + memTable[i].mtLength;
            for (p = (PolyWord*)start; p < (PolyWord*)end; )
            {
                p++;
                PolyObject *obj = (PolyObject*)p;
                POLYUNSIGNED length = obj->Length();
                // Update any constants before processing the object
                // We need that for relative jumps/calls in X86/64.
                if (length != 0 && obj->IsCodeObject())
                    machineDependent->ScanConstantsWithinCode(obj, this);
                relocateObject(obj);
                p += length;
            }
        }
        sections[relocSection].sh_size =
            relocationCount * (useRela ? sizeof(ElfXX_Rela) : sizeof(ElfXX_Rel));
    }

    // Relocations for "exports" and "memTable";
    alignFile(sections[sect_table_data+1].sh_addralign);
    sections[sect_table_data+1].sh_offset = ftell(exportFile);
    relocationCount = 0;
    // TODO: This won't be needed if we put these in a separate section.
    POLYUNSIGNED areaSpace = 0;
    for (i = 0; i < memTableEntries; i++)
        areaSpace += memTable[i].mtLength;

    // Address of "memTable" within "exports". We can't use createRelocation because
    // the position of the relocation is not in either the mutable or the immutable area.
    POLYSIGNED memTableOffset = (POLYSIGNED)sizeof(exportDescription); // It follows immediately after this.
    createStructsRelocation(symbolCount-1 /* Last symbol */, offsetof(exportDescription, memTable), memTableOffset);

    // Address of "rootFunction" within "exports"
    unsigned rootAddrArea = findArea(rootFunction);
    POLYSIGNED rootOffset = (char*)rootFunction - (char*)memTable[rootAddrArea].mtAddr;
    createStructsRelocation(AreaToSym(rootAddrArea), offsetof(exportDescription, rootFunction), rootOffset);

    // Addresses of the areas within memtable.
    for (i = 0; i < memTableEntries; i++)
    {
        createStructsRelocation(AreaToSym(i),
            sizeof(exportDescription) + i * sizeof(memoryTableEntry) + offsetof(memoryTableEntry, mtAddr),
            0 /* No offset relative to base symbol*/);
    }

    sections[sect_table_data+1].sh_size =
        relocationCount * (useRela ? sizeof(ElfXX_Rela) : sizeof(ElfXX_Rel));

    // Now the binary data.

    // Now the binary data.
    for (i = 0; i < memTableEntries; i++)
    {
        unsigned dataSection = sect_data+i*2;
        alignFile(sections[dataSection].sh_addralign);
        sections[dataSection].sh_offset = ftell(exportFile);
        sections[dataSection].sh_size = memTable[i].mtLength;
        fwrite(memTable[i].mtAddr, 1, memTable[i].mtLength, exportFile);
    }

    exportDescription exports;
    memset(&exports, 0, sizeof(exports));
    exports.structLength = sizeof(exportDescription);
    exports.memTableSize = sizeof(memoryTableEntry);
    exports.memTableEntries = memTableEntries;
    exports.ioIndex = 0; // The io entry is the first in the memory table
    exports.memTable = useRela ? 0 : (memoryTableEntry *)memTableOffset;
    // Set the value to be the offset relative to the base of the area.  We have set a relocation
    // already which will add the base of the area.
    exports.rootFunction = useRela ? 0 : (void*)rootOffset;
    exports.timeStamp = time(NULL);
    exports.ioSpacing = ioSpacing;
    exports.architecture = machineDependent->MachineArchitecture();
    exports.rtsVersion = POLY_version_number;

    // Set the address values to zero before we write.  They will always
    // be relative to their base symbol.
    for (i = 0; i < memTableEntries; i++)
        memTable[i].mtAddr = 0;

    // Now the binary data.
    alignFile(sections[sect_table_data].sh_addralign);
    sections[sect_table_data].sh_offset = ftell(exportFile);
    sections[sect_table_data].sh_size = sizeof(exportDescription) + memTableEntries*sizeof(memoryTableEntry);

    fwrite(&exports, sizeof(exports), 1, exportFile);
    fwrite(memTable, sizeof(memoryTableEntry), memTableEntries, exportFile);

    // The section name table
    sections[sect_sectionnametable].sh_offset = ftell(exportFile);
    fwrite(sectionStrings.strings, sectionStrings.stringSize, 1, exportFile);
    sections[sect_sectionnametable].sh_size = sectionStrings.stringSize;

    // The symbol name table
    sections[sect_stringtable].sh_offset = ftell(exportFile);
    fwrite(symStrings.strings, symStrings.stringSize, 1, exportFile);
    sections[sect_stringtable].sh_size = symStrings.stringSize;

    // Finally the section headers.
    alignFile(4);
    fhdr.e_shoff = ftell(exportFile);
    fwrite(sections, sizeof(ElfXX_Shdr) * numSections, 1, exportFile);

    // Rewind to rewrite the file header with the offset of the section headers.
    rewind(exportFile);
    fwrite(&fhdr, sizeof(fhdr), 1, exportFile);
    fclose(exportFile); exportFile = NULL;

    delete[]sections;
}
