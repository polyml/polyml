/*
    Title:      Diagnostics

    Copyright (c) 2011 David C.J. Matthews

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
#elif defined(_WIN32)
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

#if (defined(_WIN32) && ! defined(__CYGWIN__))
#include "Console.h"
#endif

#ifdef HAVE_TCHAR_H
#include <tchar.h>
#endif

#include "errors.h"
#include "noreturn.h"
#include "globals.h"

unsigned debugOptions = 0; // Debugging options requested on command line.

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
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    if (useConsole)
    {
        MessageBox(hMainWindow, "Poly/ML has exited", "Poly/ML", MB_OK);
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

#if (defined(_WIN32) && ! defined(__CYGWIN__))
    if (useConsole)
    {
        MessageBox(hMainWindow, "Poly/ML has exited", "Poly/ML", MB_OK);
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

void ExitWithError(const char *msg, int err)
{
    puts("\n");
    puts(msg);
    const char *errorMsg = stringFromErrorCode(err);
    if (errorMsg != NULL) puts(errorMsg);

    puts("\n");
    fflush(stdout);
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    if (useConsole)
    {
        MessageBox(hMainWindow, "Poly/ML has exited", "Poly/ML", MB_OK);
    }
#endif
    exit(1);
}

#if (defined(_WIN32) && ! defined(__CYGWIN__))
// Default is to log with OutputDebugString
static FILE *logStream = NULL;
#else
// Default is to log to stdout
static FILE *logStream = stdout;
#endif

void SetLogFile(const char *fileName)
{
    FILE *stream = fopen(fileName, "w");
    if (stream == NULL)
        printf("Unable to open debug file %s\n", fileName);
    else logStream = stream;
}

// For the moment log to stdout
void Log(const char *msg, ...)
{
    va_list vl;
    va_start(vl, msg);
    if (logStream) vfprintf(logStream, msg, vl);
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    char buff[1024];
    if (_vsnprintf(buff, sizeof(buff), msg, vl) > 0)
        ::OutputDebugString(buff);
#endif
    va_end(vl);
    if (logStream) fflush(logStream);
}

// Log the size of a space as a comprehensible number
void LogSize(POLYUNSIGNED wordSize)
{
    POLYUNSIGNED size = wordSize * sizeof(PolyWord);
    if (size < 10*1024)
        Log("%"POLYUFMT, size);
    else
    {
        double s = (double)size;
        if (s < 1024000.0)
            Log("%1.2fK", s / 1024.0);
        else if (s < 1000.0 * 1024.0 * 1024.0)
            Log("%1.2fM", s / (1024.0 * 1024.0));
        else Log("%1.2fG", s / (1024.0 * 1024.0 * 1024.0));
    }
}
