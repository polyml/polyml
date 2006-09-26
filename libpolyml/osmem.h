/*
    Title:  osomem.h - Interface to OS memory management

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

#ifndef OS_MEM_H_INCLUDED
#define OS_MEM_H_INCLUDED


// We need size_t so include these two here.
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

// This class provides access to the memory management provided by the
// operating system.  It would be nice if we could always use malloc and
// free for this but we need to have execute permission on the code
// objects.

#define PERMISSION_READ     1
#define PERMISSION_WRITE    2
#define PERMISSION_EXEC     4

class OSMem
{
public:
    // Allocate space and return a pointer to it.  The size is the minimum
    // size requested in bytes and it is updated with the actual space allocated.
    // Returns NULL if it cannot allocate the space.
    void *Allocate(size_t &bytes, unsigned permissions);

    // Release the space previously allocated.  This must free the whole of
    // the segment.  The space must be the size actually allocated.
    bool Free(void *p, size_t space);

    // Adjust the permissions on a segment.  This must apply to the
    // whole of a segment.
    bool SetPermissions(void *p, size_t space, unsigned permissions);
};


extern OSMem *osMemoryManager;

#endif
