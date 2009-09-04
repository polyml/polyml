/*
    Title:      Wrap-arounds for I/O functions

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

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h> /* Freebsd 2.1 at least requires this before stat.h */
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif


#include "proper_io.h"

// DCJM 11/5/06 - Do we really need this "fix" for a bug ten years ago in an OS that's
// probably never used now?

int proper_stat(char *filename, struct stat *fbp)
{
	int res = stat(filename, fbp);
	while (res != 0 && errno == EINTR) 
	{ 
		res = stat(filename, fbp);
		/* There's a bug in SunOS 5.3 (at least) whereby the stat following
		an interrupted stat call can give junk results (ENOENT on a real file).
		I suspect a timing-related problem on automounted file systems.
		So let's try one more time, just to be sure.
		SPF 16/12/96
		*/
		if (res != 0)
		{
			res = stat(filename, fbp);
		}
	};
	return res;
}

#ifndef WINDOWS_PC
/* I don't know whether the same applies to lstat but we'll define
   it in the same way just in case. DCJM 16/5/00. */
int proper_lstat(char *filename, struct stat *fbp)
{
	int res = lstat(filename, fbp);
	while (res != 0 && errno == EINTR) 
	{ 
		res = lstat(filename, fbp);
		if (res != 0)
		{
			res = lstat(filename, fbp);
		}
	};
	return res;
}
#endif

/* There is a problem with getrusage on Solaris where it sometimes fails with EINTR.
// This is despite the documentation which doesn't mention this and is probably because
// it is implemented by opening a special device.  We need to handle this and there's really
// no harm in doing that on all (Unix) systems.  DCJM 27/2/03.
*/
#ifndef WINDOWS_PC

int proper_getrusage(int who, struct rusage *rusage)
{
    while (1) {
        int res = getrusage(who, rusage);
        if (res == 0 || errno != EINTR) return res;
    }
}

#endif
