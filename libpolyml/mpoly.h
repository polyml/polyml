/*
    Title:  exports signature for mpoly.c

    Copyright (c) 2000
        Cambridge University Technical Services Limited

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
#ifndef _MPOLY_H_DEFINED
#define _MPOLY_H_DEFINED

#include "noreturn.h"

extern struct _userOptions {
    unsigned    user_arg_count;
    char        **user_arg_strings;
    char        *programName;
    unsigned    debug;              /* debugging  flags                       */
    bool        noDisplay;          /* X display flag                         */
    unsigned    timeslice;          /* Poly/ML process timeslice (in ms)      */
    // Not all of these can be set any longer.
    unsigned    heapSize, immutableFreeSpace, mutableFreeSpace;
    unsigned    immutableMinFree, mutableMinFree; // Probably remove
} userOptions;

// Values for debugging flags
#define DEBUG_CHECK_OBJECTS 1
#define DEBUG_REGION_CHECK  2
#define DEBUG_FORCEGC       16
#define DEBUG_X             128

class PolyWord;

// Return the entry in the io vector corresponding to the Poly system call.
extern PolyWord *IoEntry(unsigned SYS_op);

NORETURNFN(extern void finish(int n));

extern char *RTSArgHelp(void);

#endif /* _MPOLY_H_DEFINED */
