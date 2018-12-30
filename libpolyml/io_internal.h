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

#if (defined(_WIN32) && ! defined(__CYGWIN__))


#else

extern Handle wrapFileDescriptor(TaskData *taskData, int fd);
// Get a file descriptor and raise an exception if it is closed.
extern int getStreamFileDescriptor(TaskData *taskData, PolyWord strm);
extern int getStreamFileDescriptorWithoutCheck(PolyWord strm);

#endif

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
