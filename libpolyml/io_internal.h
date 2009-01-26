/*
    Title:      Data structures shared between basioio.c and network.c.

    Copyright (c) 2000 David C. J. Matthews
    Portions of this code are derived from the original stream io
    package copyright CUTS 1983-2000.

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

#ifndef IO_INTERNAL_H
#define IO_INTERNAL_H


#define IO_BIT_OPEN         1
#define IO_BIT_READ         2
#define IO_BIT_WRITE        4
#define IO_BIT_DIR          8 /* Is it a directory entry? */
#define IO_BIT_SOCKET       16 /* Is it a socket? */
#define IO_BIT_INPROGRESS   64 /* "connect" in progress on socket. */

#ifdef WINDOWS_PC

#define IO_BIT_PIPE         128
#define IO_BIT_DEV          256
#define IO_BIT_CONSOLE      512

#ifdef USEWINSOCK2
#include <winsock2.h>
#else
#include <winsock.h>
#endif

#else

typedef int SOCKET;
//#include <dirent.h>

#endif

#ifdef HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# if HAVE_SYS_NDIR_H
# include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
# include <sys/dir.h>
# endif
# if HAVE_NDIR_H
# include <ndir.h>
# endif
#endif


typedef struct basic_io_struct
{
    PolyObject *token; /* pointer into ML heap */
    int ioBits; /* Flag bits */
    union {
        int ioDesc; /* File descriptor. */
#ifdef WINDOWS_PC
        struct {
            HANDLE  hFind; /* FindFirstFile handle */
            WIN32_FIND_DATA lastFind;
            int fFindSucceeded;
        } directory;
        SOCKET sock;
#else
#define sock ioDesc
        DIR *ioDir; /* Directory entry. */
#endif
    } device;
#ifdef WINDOWS_PC
    HANDLE hAvailable; // Used to signal available data
#endif
} IOSTRUCT, *PIOSTRUCT;

class TaskData;

#define isOpen(s)   ((s)->ioBits & IO_BIT_OPEN)
#define isRead(s)   ((s)->ioBits & IO_BIT_READ)
#define isWrite(s)  ((s)->ioBits & IO_BIT_WRITE)
#define isDirectory(s)  ((s)->ioBits & IO_BIT_DIR)
#define isSocket(s) ((s)->ioBits & IO_BIT_SOCKET)

#ifdef WINDOWS_PC
#define isPipe(s)   ((s)->ioBits & IO_BIT_PIPE)
#define isDevice(s) ((s)->ioBits & IO_BIT_DEV)
#define isConsole(s)    ((s)->ioBits & IO_BIT_CONSOLE)
#endif

extern PIOSTRUCT get_stream(PolyObject *obj);

extern Handle make_stream_entry(TaskData *mdTaskData);
extern void free_stream_entry(unsigned stream_no);
extern void close_stream(PIOSTRUCT str);

extern PIOSTRUCT basic_io_vector;

extern bool emfileFlag;
#endif
