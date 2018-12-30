/*
    Title:      Data structures shared between basioio.c and network.c.

    Copyright (c) 2000, 2016, 2018 David C. J. Matthews
    Portions of this code are derived from the original stream io
    package copyright CUTS 1983-2000.

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

#ifndef IO_INTERNAL_H
#define IO_INTERNAL_H

#include "locking.h" // For PLock

#define IO_BIT_OPEN         1
#define IO_BIT_READ         2
#define IO_BIT_WRITE        4
#define IO_BIT_SOCKET       16 /* Is it a socket? */

#if (defined(_WIN32) && ! defined(__CYGWIN__))

#define IO_BIT_PIPE         128
#define IO_BIT_DEV          256
#define IO_BIT_GUI_CONSOLE  512

#include <winsock2.h>

#else

typedef int SOCKET;
//#include <dirent.h>

#endif

typedef struct basic_io_struct
{
    PolyWord token; // Either a pointer or a tagged int for stdIn etc.
    int ioBits; /* Flag bits */
    union {
        int ioDesc; /* File descriptor. */
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        SOCKET sock;
#else
#define sock ioDesc
#endif
    } device;
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    HANDLE hAvailable; // Used to signal available data
#endif
} IOSTRUCT, *PIOSTRUCT;

class TaskData;

#define isOpen(s)   ((s)->ioBits & IO_BIT_OPEN)
#define isRead(s)   ((s)->ioBits & IO_BIT_READ)
#define isWrite(s)  ((s)->ioBits & IO_BIT_WRITE)
#define isSocket(s) ((s)->ioBits & IO_BIT_SOCKET)

#if (defined(_WIN32) && ! defined(__CYGWIN__))
// Needed because testing for available input is different depending on the device.
// Console here means our Windows GUI.
#define isPipe(s)       ((s)->ioBits & IO_BIT_PIPE)
#define isDevice(s)     ((s)->ioBits & IO_BIT_DEV)
#define isConsole(s)    ((s)->ioBits & IO_BIT_GUI_CONSOLE)
#endif

// Token representing a closed stream.
#define ClosedToken TAGGED(-1)

extern PIOSTRUCT get_stream(PolyWord token);

extern Handle make_stream_entry(TaskData *mdTaskData);
extern void free_stream_entry(POLYUNSIGNED stream_no);
extern void close_stream(PIOSTRUCT str);

extern PIOSTRUCT basic_io_vector;
extern PLock ioLock;

// This is used in both basicio and unix-specific
#if defined(HAVE_STRUCT_STAT_ST_ATIM)
# define STAT_SECS(stat,kind)    (stat)->st_##kind##tim.tv_sec
# define STAT_USECS(stat,kind) (((stat)->st_##kind##tim.tv_nsec + 500) / 1000)
#elif defined(HAVE_STRUCT_STAT_ST_ATIMENSEC)
# define STAT_SECS(stat,kind)    (stat)->st_##kind##time
# define STAT_USECS(stat,kind) (((stat)->st_##kind##timensec + 500) / 1000)
#elif defined(HAVE_STRUCT_STAT_ST_ATIMESPEC)
# define STAT_SECS(stat,kind)    (stat)->st_##kind##timespec.tv_sec
# define STAT_USECS(stat,kind) (((stat)->st_##kind##timespec.tv_nsec + 500) / 1000)
#elif defined(HAVE_STRUCT_STAT_ST_ATIME_N)
# define STAT_SECS(stat,kind)    (stat)->st_##kind##time
# define STAT_USECS(stat,kind) (((stat)->st_##kind##time_n + 500) / 1000)
#elif defined(HAVE_STRUCT_STAT_ST_UATIME)
# define STAT_SECS(stat,kind)    (stat)->st_##kind##time
# define STAT_USECS(stat,kind)   (stat)->st_u##kind##time
#else
# define STAT_SECS(stat,kind)    (stat)->st_##kind##time
# define STAT_USECS(stat,kind)   0
#endif

#endif
