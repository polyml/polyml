/*
    Title:  proper_io.h

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

#ifndef _PROPER_IO_H_DEFINED
#define _PROPER_IO_H_DEFINED 1

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


#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

int proper_stat(char *, struct stat *);
int proper_lstat(char *, struct stat *);

#ifdef HAVE_GETRUSAGE
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
int proper_getrusage(int who, struct rusage *rusage);
#endif


#define proper_fwrite_chars(ptr,nchars,stream) (int)fwrite(ptr,1,nchars,stream)


#endif /* _PROPER_IO_H_DEFINED */
