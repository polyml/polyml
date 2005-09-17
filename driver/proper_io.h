/*
    Title: 	proper_io.h

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

#include <sys/types.h>
#include <stdio.h>

typedef struct stat statstruct;
int proper_stat(char *, statstruct *);
int proper_lstat(char *, statstruct *);

#ifndef WINDOWS_PC
#include <sys/time.h>
#include <sys/resource.h>
int proper_getrusage(int who, struct rusage *rusage);
#endif


/* Level 2 (file descriptor) functions */
#define proper_open(path,flag,mode) open(path,flag,mode)
#define proper_close(fildes) close(fildes)
#define proper_dup2(fildes,fildes2) dup2(fildes,fildes2)
#define proper_ioctl(fildes,request,arg) ioctl(fildes,request,arg)
#define proper_fcntl(fildes,request,arg) fcntl(fildes,request,arg)
#define proper_read(fildes,buf,nbyte) read(fildes,buf,nbyte)
#define proper_write(fildes,buf,nbyte) write(fildes,buf,nbyte)

/* Level 3 (stream) functions */
#define proper_fopen(pathname,type) fopen(pathname,type)
#define proper_fclose(stream) fclose(stream)
#define proper_fflush(stream) fflush(stream)
#define proper_fprintf fprintf
#define proper_printf printf

#define proper_getc(stream) getc(stream)
#define proper_ungetc(c, stream) ungetc(c, stream)
#define proper_putc(c,stream) putc(c,stream)
#define proper_fputs(s,stream) fputs(s,stream)

#define proper_fwrite_chars(ptr,nchars,stream) (int)fwrite(ptr,1,nchars,stream)


#endif /* _PROPER_IO_H_DEFINED */
