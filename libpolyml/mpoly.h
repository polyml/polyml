/*
    Title:  exports signature for mpoly.c

    Copyright (c) 2000-7
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
#include "../polyexports.h"

extern struct _userOptions {
    unsigned    user_arg_count;
    char        **user_arg_strings;
    const char  *programName;
    unsigned    gcthreads;    // Number of threads to use for gc
} userOptions;

class PolyWord;

// Return the entry in the io vector corresponding to the Poly system call.
extern PolyWord *IoEntry(unsigned SYS_op);

NORETURNFN(extern void finish(int n));

extern char *RTSArgHelp(void);

extern time_t exportTimeStamp;

#endif /* _MPOLY_H_DEFINED */
