/*
    Title:  polyexports.h 

    Copyright (c) 2006, 2011, 2015 David C.J. Matthews

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

/*
This header contains the structures used in saved state created by "export".
*/

#ifndef _STANDALONE_H
#define _STANDALONE_H 1

// Get time_t
#ifdef HAVE_TIME_H
#include <time.h>
#endif

// Get uintptr_t
#if HAVE_STDINT_H
#  include <stdint.h>
#endif

#if HAVE_INTTYPES_H
#  ifndef __STDC_FORMAT_MACROS
#    define __STDC_FORMAT_MACROS
#  endif
#  include <inttypes.h>
#endif

#ifdef HAVE_STDDEF_H
#  include <stddef.h>
#endif

#if defined(HAVE_WINDOWS_H)
#  include <windows.h>
#endif

// There are several entries 
typedef struct _memTableEntry {
    void *mtAddr;                       // The address of the area of memory
    uintptr_t mtLength;              // The length in bytes of the area
    uintptr_t mtFlags;               // Flags describing the area.
    uintptr_t mtIndex;               // An index to identify permanent spaces.
} memoryTableEntry;

#define MTF_WRITEABLE         0x00000001  // The area is writeable by ML code
#define MTF_EXECUTABLE        0x00000002  // The area contains executable code
#define MTF_NO_OVERWRITE      0x00000004  // With MTF_WRITEABLE: Don't load over the top
#define MTF_BYTES             0x00000008  // Contains only byte data and no addresses

typedef struct _exportDescription {
    unsigned structLength;         // The length of this structure
    unsigned memTableSize;         // The size of each entry in the memory table
    unsigned memTableEntries;      // The number of entries in the memory table
    unsigned ioIndex;              // The index in the memory table for the io interface area
    memoryTableEntry *memTable;    // Pointer to the memory table.
    void *rootFunction;            // Points to the start-up function
    time_t timeStamp;        // Creation time stamp
    unsigned architecture;         // Machine architecture
    unsigned rtsVersion;           // Run-time system version
    unsigned ioSpacing;            // Size of each entry in the io interface area.
} exportDescription;

extern exportDescription poly_exports;

#ifdef __cplusplus
extern "C" {
#endif

#if (defined(_WIN32) && ! defined(__CYGWIN__))
#include <windows.h>

# ifdef LIBPOLYML_BUILD
#  ifdef DLL_EXPORT
#   define POLYLIB_API            __declspec (dllexport)
#  endif
# elif defined _MSC_VER
    // Visual C - POLYLIB_EXPORTS is defined in the library project settings
#  ifdef POLYLIB_EXPORTS
#   define POLYLIB_API             __declspec (dllexport)
#  else
#   define POLYLIB_API             __declspec (dllimport)
#  endif
# elif defined DLL_EXPORT
#  define POLYLIB_API             __declspec (dllimport)
# else
#  define POLYLIB_API
# endif

extern POLYLIB_API int PolyWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    LPSTR lpCmdLine, int nCmdShow, exportDescription *exports);
#else
int polymain(int argc, char *argv[], exportDescription *exports);
#endif

#ifdef __cplusplus
};
#endif

#endif
