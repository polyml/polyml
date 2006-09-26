/*
    Title:  standalone.h 

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

/*
This header contains the structures used in saved state created by "export".
*/

#ifndef _STANDALONE_H
#define _STANDALONE_H 1

#ifdef WIN32
#include <windows.h>
#ifdef _WIN32_WCE
typedef unsigned long UNSIGNEDADDR;
#else
typedef UINT_PTR UNSIGNEDADDR;
#endif
#else
typedef unsigned long UNSIGNEDADDR;
#endif

// There are several entries 
typedef struct _memTableEntry {
    void *mtAddr;                       // The address of the area of memory
    UNSIGNEDADDR mtLength;              // The length in bytes of the area
    UNSIGNEDADDR mtFlags;               // Flags describing the area.
} memoryTableEntry;

#define MTF_WRITEABLE         0x00000001  // The area is writeable by ML code
#define MTF_EXECUTABLE        0x00000002  // The area contains executable code

typedef struct _exportDescription {
    unsigned structLength;         // The length of this structure
    unsigned memTableSize;         // The size of each entry in the memory table
    unsigned memTableEntries;      // The number of entries in the memory table
    unsigned ioIndex;              // The index in the memory table for the io interface area
    memoryTableEntry *memTable;    // Pointer to the memory table.
    void *rootFunction;            // Points to the start-up function
    UNSIGNEDADDR timeStamp;        // Creation time stamp
    unsigned architecture;         // Machine architecture
    unsigned rtsVersion;           // Run-time system version
    void *rootObject;              // Backwards compatibility - points to the root object
    unsigned ioSpacing;            // Size of each entry in the io interface area.
    unsigned long *patchTable;     // Pointer to patch table if needed.
} exportDescription;

extern exportDescription poly_exports;

#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32

#ifdef NO_POLY_LIBRARY
#define POLYLIB_API
#else
#ifdef POLYLIB_EXPORTS
#define POLYLIB_API __declspec(dllexport)
#else
#define POLYLIB_API __declspec(dllimport)
#endif
#endif

extern POLYLIB_API int PolyWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    LPTSTR lpCmdLine, int nCmdShow, exportDescription *exports);
#endif

    int polymain(int argc, char *argv[], exportDescription *exports);


#ifdef __cplusplus
};
#endif

#endif
