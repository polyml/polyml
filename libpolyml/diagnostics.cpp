/*
    Title:      Diagnostics

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

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

#if defined(WINDOWS_PC)
#include "Console.h"
#endif

#ifdef HAVE_TCHAR_H
#include <tchar.h>
#endif


/***********************************************************************
 *
 *  FAILURE FUNCTIONS (moved here from mmap.c)
 *
 ***********************************************************************/

void Exit(const char *msg, ...)
{
    va_list vl;
    printf("\n");
    va_start(vl, msg);
    vprintf(msg, vl);
    va_end(vl);
    printf("\n");
    fflush(stdout);
    #if defined(WINDOWS_PC)
    if (useConsole)
    {
        MessageBox(hMainWindow, _T("Poly/ML has exited"), _T("Poly/ML"), MB_OK);
    }
    #endif
    exit(1);
}

// Error condition.  This should really be replaced either with ASSERTs
// or exceptions.
void Crash(const char *msg, ...)
{
    va_list vl;
    printf("\n");
    va_start(vl, msg);
    vprintf(msg, vl);
    va_end(vl);
    printf("\n");
    fflush(stdout);

    #if defined(WINDOWS_PC)
    if (useConsole)
    {
        MessageBox(hMainWindow, _T("Poly/ML has exited"), _T("Poly/ML"), MB_OK);
    }
    #else    
    {
        sigset_t set;
        sigemptyset(&set);
        sigprocmask(SIG_SETMASK,&set,NULL);
    }
    #endif    

    abort();
    exit(1);
}
