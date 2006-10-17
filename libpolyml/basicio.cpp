/*
    Title:      Basic IO.

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

/*
This module replaces the old stream IO based on stdio.  It works at a
lower level with the buffering being done in ML.
Sockets are generally dealt with in network.c but it is convenient to
use the same table for them particularly since it simplifies the
implementation of "poll".
Directory operations are also included in here.
DCJM May 2000. 
*/
#ifdef _WIN32_WCE
#include "winceconfig.h"
#include "wincelib.h"
#else
#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
#endif
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#ifdef HAVE_IO_H
#include <io.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_POLL_H
#include <poll.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_DIRECT_H
#include <direct.h>
#endif

#ifdef HAVE_TCHAR_H
#include <tchar.h>
#else
#ifdef UNICODE
typedef short TCHAR;
#else
typedef char TCHAR;
#endif
#endif

#ifdef WIN32
#ifdef USEWINSOCK2
#include <winsock2.h>
#else
#include <winsock.h>
#endif
#endif

#if(!defined(MAXPATHLEN) && defined(MAX_PATH))
#define MAXPATHLEN MAX_PATH
#endif

#ifndef O_BINARY
#define O_BINARY    0 /* Not relevant. */
#endif
#ifndef INFTIM
#define INFTIM (-1)
#endif


#include "globals.h"
#include "basicio.h"
#include "sys.h"
#include "proper_io.h"
#include "gc.h"
#include "run_time.h"
#include "machine_dep.h"
#include "arb.h"
#include "processes.h"
#include "diagnostics.h"
#include "io_internal.h"
#include "scanaddrs.h"
#include "polystring.h"
#include "mpoly.h"
#include "save_vec.h"
#include "rts_module.h"

#ifdef WINDOWS_PC
#include "Console.h"
#endif

#ifndef O_ACCMODE
#define O_ACCMODE   (O_RDONLY|O_RDWR|O_WRONLY)
#endif

#define STREAMID(x) (DEREFSTREAMHANDLE(x)->streamNo)

#define SAVE(x) gSaveVec->push(x)

/* Points to tokens which represent the streams and the stream itself. 
   For each stream a single word token is made containing the file 
   number, and its address is put in here. When the stream is closed 
   the entry is overwritten. Any further activity will be trapped 
   because the address in the vector will not be the same as the 
   address of the token. This also prevents streams other than stdin 
   and stdout from being made persistent. stdin, stdout and stderr are
   treated specially.  The tokens for them are entries in the
   interface vector and so can be made persistent. */
/*
I've tried various ways of getting asynchronous IO to work in a
consistent manner across different kinds of IO devices in Windows.
It is possible to pass some kinds of handles to WaitForMultipleObjects
but not all.  Anonymous pipes, for example, cannot be used in Windows 95
and don't seem to do what is expected in Windows NT (they return signalled
even when there is no input).  The console is even more of a mess. The
handle is signalled when there are any events (such as mouse movements)
available but these are ignored by ReadFile, which may then block.
Conversely using ReadFile to read less than a line causes the handle
to be unsignalled, supposedly meaning that no input is available, yet
ReadFile will return subsequent characters without blocking.  The eventual
solution was to replace the console completely.
DCJM May 2000 
*/

PIOSTRUCT basic_io_vector;

#ifdef WINDOWS_PC

/* Deal with the various cases to see if input is available. */
static int isAvailable(PIOSTRUCT strm)
{
    HANDLE  hFile = (HANDLE)_get_osfhandle(strm->device.ioDesc);
    if (isPipe(strm))
    {
#ifdef _WIN32_WCE
		// PeekNamedPipe isn't supported in Windows CE.
		return 1;
#else
        DWORD dwAvail;
        int err;
        if (PeekNamedPipe(hFile, NULL, 0, NULL, &dwAvail, NULL))
        {
            return (dwAvail == 0 ? 0 : 1);
        }
        err = GetLastError();
        /* Windows returns ERROR_BROKEN_PIPE on input whereas Unix
           only returns it on output and treats it as EOF.  We
           follow Unix here.  */
        if (err == ERROR_BROKEN_PIPE)
            return 1; /* At EOF - will not block. */
        else raise_syscall("PeekNamedPipe failed", -err);
        /*NOTREACHED*/
#endif
    }
    else if (isConsole(strm)) return isConsoleInput();
    else if (isDevice(strm))
    {
        if (WaitForSingleObject(hFile, 0) == WAIT_OBJECT_0)
            return 1;
        else return 0;
    }
    else
        /* File - We may be at end-of-file but we won't block. */
        return 1;
}
#endif

static unsigned max_streams;

/* If we try opening a stream and it fails with EMFILE (too many files
   open) we may be able to recover by garbage-collecting and closing some
   unreferenced streams.  This flag is set to indicate that we have had
   an EMFILE error and is cleared whenever a file is closed or opened
   successfully.  It prevents infinite looping if we really have too
   many files. */
int emfileFlag = 0;

/* Close a stream, either explicitly or as a result of detecting an
   unreferenced stream in the g.c.  Doesn't report any errors. */
void close_stream(PIOSTRUCT str)
{
    if (!isOpen(str)) return;
    if (isDirectory(str))
    {
#ifdef WINDOWS_PC
        FindClose(str->device.directory.hFind);
#else
        closedir(str->device.ioDir);
#endif
    }
#ifdef WINDOWS_PC
    else if (isSocket(str))
    {
        closesocket(str->device.sock);
    }
    else if (isConsole(str)) return;
#endif
    else close(str->device.ioDesc);
    str->ioBits = 0;
    str->token = 0;
    emfileFlag = 0;
}


/******************************************************************************/
/*                                                                            */
/*      get_stream - utility function - doesn't allocate                      */
/*                                                                            */
/******************************************************************************/
PIOSTRUCT get_stream(PolyObject *stream_token)
/* Checks that the stream number is valid and returns the actual stream. 
   Returns NULL if the stream is closed. */
{
    POLYUNSIGNED stream_no = ((StreamToken*)stream_token)->streamNo;

    if (stream_no >= max_streams ||
        basic_io_vector[stream_no].token != stream_token ||
        ! isOpen(&basic_io_vector[stream_no])) 
        return 0; /* Closed. */

    return &basic_io_vector[stream_no];
}

/******************************************************************************/
/*                                                                            */
/*      make_stream_entry - utility function - allocates in Poly heap         */
/*                                                                            */
/******************************************************************************/
Handle make_stream_entry(void)
/* Find a free entry in the stream vector and return a token for it. The
   address of the token is preserved on the save vector so it will not be
   deleted if there is a garbage collection (Entries in the stream vector
   itself are "weak". */ 
{
    unsigned stream_no;
    bool have_collected = false;

    do {
        for(stream_no = 0;
            stream_no < max_streams && basic_io_vector[stream_no].token != 0;
            stream_no++);
            
        /* Check we have enough space. */
        if (stream_no >= max_streams)
        { /* No space. */
           /* See if we have unreferenced streams. */
            if (! have_collected)
            {
                FullGC();
                have_collected = true;
            }
            else /* No space - expand vector. */
            {
                int oldMax = max_streams;
                max_streams += max_streams/2;
                basic_io_vector =
                    (PIOSTRUCT)realloc(basic_io_vector,
                                    max_streams*sizeof(IOSTRUCT));
                /* Clear the new space. */
                memset(basic_io_vector+oldMax, 0,
                        (max_streams-oldMax)*sizeof(IOSTRUCT));
            }
        }
    } while (stream_no >= max_streams);
     
    Handle str_token = alloc_and_save(1, F_BYTE_BIT);
    STREAMID(str_token) = stream_no;

    ASSERT(!isOpen(&basic_io_vector[stream_no]));
    /* Clear the entry then set the token. */
    memset(&basic_io_vector[stream_no], 0, sizeof(IOSTRUCT));
    basic_io_vector[stream_no].token = DEREFWORDHANDLE(str_token);
    return str_token;
}

/******************************************************************************/
/*                                                                            */
/*      free_stream_entry - utility function                                  */
/*                                                                            */
/******************************************************************************/
/* Free an entry in the stream vector - used when openstreamc grabs a
   stream vector entry, but then fails to open the associated file. 
   (This happens frequently when we are using the Poly make system.)
   If we don't recycle the stream vector entries immediately we quickly
   run out and must perform a full garbage collection to recover
   the unused ones. SPF 12/9/95
*/ 
void free_stream_entry(unsigned stream_no)
{
   ASSERT(0 <= stream_no && stream_no < max_streams);

   basic_io_vector[stream_no].token  = 0;
   basic_io_vector[stream_no].ioBits = 0;
}

#ifdef WINDOWS_PC
static int getFileType(int stream)
{
    if (stream == 0 && useConsole)
        /* If this is stdio and we're using our own console.*/
        return IO_BIT_CONSOLE;
#ifdef _WIN32_WCE
	// GetFileType isn't implemented in Windows CE
	return 0;
#else
    switch (GetFileType((HANDLE)_get_osfhandle(stream))
                & ~FILE_TYPE_REMOTE)
    {
        case FILE_TYPE_PIPE: return IO_BIT_PIPE;
        case FILE_TYPE_CHAR: return IO_BIT_DEV;
        default: return 0;
    }
#endif
}
#endif

// Retry the call to IO dispatch.  Called if an IO call is interrupted by
// the user pressing ^C.  This allows the code to have an exception raised in
// it if the user has typed "f" to the prompt.
static void retry_rts_call(void)
{
    machineDependent->SetForRetry(POLY_SYS_io_dispatch);
    execute_pending_interrupts();
    THROW_RETRY;
}


/* Copy a file name to a buffer.  Raises an exception if
   the string will not fit. */
static void getFileName(Handle name, TCHAR *buff, POLYUNSIGNED buffSize)
{
    POLYUNSIGNED length = Poly_string_to_C(DEREFWORD(name), buff, buffSize);
    if (length > buffSize)
        raise_syscall("File name too long", ENAMETOOLONG);
}

/* Open a file in the required mode. */
static Handle open_file(Handle filename, int mode, int access, int isPosix)
{
    TCHAR string_buffer[MAXPATHLEN];
    int stream;

TryAgain:
    /* Copy the string and check the length. */
    getFileName(filename, string_buffer, MAXPATHLEN);

    {
        Handle str_token = make_stream_entry();
        POLYUNSIGNED stream_no    = STREAMID(str_token);
        stream = open(string_buffer, mode, access);
        if (stream >= 0)
        {
            PIOSTRUCT strm = &basic_io_vector[stream_no];
            strm->device.ioDesc = stream;
            strm->ioBits = IO_BIT_OPEN;
            if ((mode & O_ACCMODE) != O_WRONLY)
                strm->ioBits |= IO_BIT_READ;
            if ((mode & O_ACCMODE) != O_RDONLY)
                strm->ioBits |= IO_BIT_WRITE;
#ifdef WINDOWS_PC
            strm->ioBits |= getFileType(stream);
#else
            if (! isPosix)
            {
                /* Set the close-on-exec flag.  We don't set this if we are being
                   called from one of the low level functions in the Posix structure.
                   I assume that if someone is using those functions they know what
                   they're doing and would expect the behaviour to be close to that
                   of the underlying function. */
                fcntl(stream, F_SETFD, 1);
            }
#endif
            emfileFlag = 0; /* Successful open. */
            return(str_token);
        }

        free_stream_entry(stream_no); /* SPF 12/9/95 */
        switch (errno)
        {
        case EINTR:
            {
                retry_rts_call();
                /*NOTREACHED*/
            }
        case EMFILE: /* too many open files */
            {
                if (emfileFlag) /* Previously had an EMFILE error. */
                    raise_syscall("Cannot open", EMFILE);
                emfileFlag = 1;
                FullGC(); /* May clear emfileFlag if we close a file. */
                goto TryAgain;
            }
        default:
            raise_syscall("Cannot open", errno);
           /*NOTREACHED*/
			return 0;
        }
    }
}

/* Close the stream unless it is stdin or stdout or already closed. */
static Handle close_file(Handle stream)
{
    PIOSTRUCT strm = get_stream(DEREFHANDLE(stream));
    int stream_no = STREAMID(stream);

    if (strm != NULL && stream_no > 2)
        /* Ignore closed streams, stdin, stdout or stderr. */
    {
        close_stream(strm);
    }

    return Make_arbitrary_precision(0);
} /* close_file */

/* Call the underlying "read" function and handle any errors. */
static POLYUNSIGNED readToMem(PIOSTRUCT strm, byte *buff, POLYUNSIGNED count)
{
    POLYSIGNED haveRead;
    int fd = strm->device.ioDesc;
    int err;

#ifdef WINDOWS_PC
    if (isConsole(strm))
    {
        if (! isConsoleInput())
            processes->block_and_restart_if_necessary(-1, POLY_SYS_io_dispatch);
        haveRead = getConsoleInput((char*)buff, count);
        err = errno;
    }
    else
    {
        if (! isAvailable(strm))
            processes->block_and_restart_if_necessary(strm->device.ioDesc, POLY_SYS_io_dispatch);
        haveRead = read(fd, buff, count);
        err = errno;
    }
#else
    /* Unix. */
    {
        process_may_block(fd, POLY_SYS_io_dispatch);
        haveRead = read(fd, buff, count);
        err = errno;
    }
#endif
    if (haveRead >= 0) return haveRead;
    /* If it was an interrupt then go off to the interrupt handler. If there
       is nothing else to do this code will be re-entered.
       Note in 4.2 Unix "read" does not seem to do this despite what it
       says in the manual. */
    if (err == EINTR)
    {
        retry_rts_call();
        /*NOTREACHED*/
    }
    raise_syscall("Error while reading", err);
    /*NOTREACHED*/
	return 0;
}


/* Read into an array. */
static Handle readArray(Handle stream, Handle args, bool/*isText*/)
{
    /* The isText argument is ignored in both Unix and Windows but
       is provided for future use.  Windows remembers the mode used
       when the file was opened to determine whether to translate
       CRLF into LF. */
    byte    *base = (byte*)DEREFHANDLE(args)->Get(0).AsObjPtr()->AsBytePtr();
    POLYUNSIGNED offset = get_C_ulong(DEREFWORDHANDLE(args)->Get(1));
    POLYUNSIGNED length = get_C_ulong(DEREFWORDHANDLE(args)->Get(2));
    PIOSTRUCT   strm = get_stream(DEREFHANDLE(stream));
    POLYSIGNED haveRead;
    /* Raise an exception if the stream has been closed. */
    if (strm == NULL) raise_syscall("Stream is closed", EBADF);

    haveRead = readToMem(strm, base+offset, length);
    return Make_arbitrary_precision(haveRead);
}

/* Return input as a string. We don't actually need both readArray and
   readString but it's useful to have both to reduce unnecessary garbage.
   The IO library will construct one from the other but the higher levels
   choose the appropriate function depending on need. */
static Handle readString(Handle stream, Handle args, bool/*isText*/)
{
    POLYUNSIGNED length = get_C_ulong(DEREFWORD(args));
    POLYSIGNED haveRead;
    PIOSTRUCT strm = get_stream(stream->WordP());
    byte *buff;
    /* Limit the length to 1M. Calling alloca with a very large size
       can cause a crash if there's a limit on the stack. In any
       case "read" may well only give us a single block. */
    if (length > 1048576) length = 1048576;
    buff = (byte*)alloca(length);
    /* Raise an exception if the stream has been closed. */
    if (strm == NULL) raise_syscall("Stream is closed", EBADF);
    haveRead = readToMem(strm, buff, length);
    return(SAVE(Buffer_to_Poly((char*)buff, haveRead)));
}

static Handle writeArray(Handle stream, Handle args, bool/*isText*/)
{
    /* The isText argument is ignored in both Unix and Windows but
       is provided for future use.  Windows remembers the mode used
       when the file was opened to determine whether to translate
       LF into CRLF. */
    PolyWord base = DEREFWORDHANDLE(args)->Get(0);
    POLYUNSIGNED    offset = get_C_ulong(DEREFWORDHANDLE(args)->Get(1));
    POLYUNSIGNED    length = get_C_ulong(DEREFWORDHANDLE(args)->Get(2));
    PIOSTRUCT       strm = get_stream(stream->WordP());
    POLYSIGNED      haveWritten;
    byte    ch;
    /* Raise an exception if the stream has been closed. */
    if (strm == NULL) raise_syscall("Stream is closed", EBADF);

    /* We don't actually handle cases of blocking on output. */
    /* process_may_block(strm); */
    byte *toWrite;
    if (IS_INT(base))
    {
        /* To allow this function to work on strings as well as
           vectors we have to be able to handle the special case of
           a single character string. */
        ch = (byte)(UNTAGGED(base));
        toWrite = &ch;
        offset = 0;
        length = 1;
    }
    else toWrite = base.AsObjPtr()->AsBytePtr();
    haveWritten = write(strm->device.ioDesc, toWrite+offset, length);
    if (haveWritten < 0) raise_syscall("Error while writing", errno);

    return Make_arbitrary_precision(haveWritten);
}


/* Test whether we can read without blocking.  Returns 0 if it will block,
   1 if it will not. */
static int canInput(Handle stream)
{
    PIOSTRUCT strm = get_stream(stream->WordP());
    if (strm == NULL) raise_syscall("Stream is closed", EBADF);

#ifdef WINDOWS_PC
    return isAvailable(strm);
#else
    {
        /* Unix - use "select" to find out if there is input available. */
        struct timeval delay = { 0, 0 };
        fd_set read_fds;
        int sel;
        FD_ZERO(&read_fds);
        FD_SET(strm->device.ioDesc, &read_fds);
        sel = select(FD_SETSIZE, &read_fds, NULL, NULL, &delay);
        if (sel < 0 && errno != EINTR)
            raise_syscall("select failed", errno);
        else if (sel > 0) return 1;
        else return 0;
    }
#endif
}

/* Test whether we can write without blocking.  Returns 0 if it will block,
   1 if it will not. */
static int canOutput(Handle stream)
{
    PIOSTRUCT strm = get_stream(stream->WordP());
    if (strm == NULL) raise_syscall("Stream is closed", EBADF);

#ifdef WINDOWS_PC
    /* There's no way I can see of doing this in Windows. */
    return 1;
#else
    {
        /* Unix - use "select" to find out if output is possible. */
        struct timeval delay = { 0, 0 };
        fd_set read_fds, write_fds, except_fds;
        int sel;
        FD_ZERO(&read_fds);
        FD_ZERO(&write_fds);
        FD_ZERO(&except_fds);
        FD_SET(strm->device.ioDesc, &write_fds);
        sel = select(FD_SETSIZE,&read_fds,&write_fds,&except_fds,&delay);
        if (sel < 0 && errno != EINTR)
            raise_syscall("select failed", errno);
        else if (sel > 0) return 1;
        else return 0;
    }
#endif
}

static long seekStream(PIOSTRUCT strm, long pos, int origin)
{
    long lpos;
    lpos = lseek(strm->device.ioDesc, pos, origin);
    if (lpos < 0) raise_syscall("Position error", errno);
    return lpos;
}

/* Return the number of bytes available on the device.  Works only for
   files since it is meaningless for other devices. */
static Handle bytesAvailable(Handle stream)
{
    long original, endOfStream;
    PIOSTRUCT strm = get_stream(stream->WordP());
    if (strm == NULL) raise_syscall("Stream is closed", EBADF);

    /* Remember our original position, seek to the end, then seek back. */
    original = seekStream(strm, 0L, SEEK_CUR);
    endOfStream = seekStream(strm, 0L, SEEK_END);
    if (seekStream(strm, original, SEEK_SET) != original) 
        raise_syscall("Position error", errno);
    return Make_arbitrary_precision(endOfStream-original);
}


#define FILEKIND_FILE   0
#define FILEKIND_DIR    1
#define FILEKIND_LINK   2
#define FILEKIND_TTY    3
#define FILEKIND_PIPE   4
#define FILEKIND_SKT    5
#define FILEKIND_DEV    6
#define FILEKIND_ERROR  (-1)

static Handle fileKind(Handle stream)
{
    PIOSTRUCT strm = get_stream(stream->WordP());
    if (strm == NULL) raise_syscall("Stream is closed", EBADF);
#ifdef WINDOWS_PC
    {
        if (isPipe(strm))
            return Make_arbitrary_precision(FILEKIND_PIPE);
        else if (isDevice(strm)) /* Character devices other than console. */
            return Make_arbitrary_precision(FILEKIND_DEV);
        else if (isConsole(strm))
            return Make_arbitrary_precision(FILEKIND_TTY);
        else
            /* Should we try to distinguish a file from a directory?
               At the moment we don't seem to be able to open a
               directory, at least in NT 4.0. */
            return Make_arbitrary_precision(FILEKIND_FILE);
    }
#else
    {
        struct stat statBuff;
        if (fstat(strm->device.ioDesc, &statBuff) < 0) raise_syscall("Stat failed", errno);
        switch (statBuff.st_mode & S_IFMT)
        {
        case S_IFIFO:
            return Make_arbitrary_precision(FILEKIND_PIPE);
        case S_IFCHR:
        case S_IFBLK:
            if (isatty(strm->device.ioDesc))
                return Make_arbitrary_precision(FILEKIND_TTY);
            else return Make_arbitrary_precision(FILEKIND_DEV);
        case S_IFDIR:
            return Make_arbitrary_precision(FILEKIND_DIR);
        case S_IFREG:
            return Make_arbitrary_precision(FILEKIND_FILE);
        case S_IFLNK:
            return Make_arbitrary_precision(FILEKIND_LINK);
        case S_IFSOCK:
            return Make_arbitrary_precision(FILEKIND_SKT);
        default:
            return Make_arbitrary_precision(-1);
        }
    }
#endif
}

/* Polling.  For the moment this applies only to objects which can
   be opened in the file system.  It may need to be extended to sockets
   later.  */
#define POLL_BIT_IN     1
#define POLL_BIT_OUT    2
#define POLL_BIT_PRI    4
/* Find out what polling options, if any, are allowed on this
   file descriptor.  We assume that polling is allowed on all
   descriptors, either for reading or writing depending on how
   the stream was opened. */
Handle pollTest(Handle stream)
{
    PIOSTRUCT strm = get_stream(stream->WordP());
    int nRes = 0;
    if (strm == NULL) return Make_arbitrary_precision(0);
    /* Allow for the possibility of both being set in the future. */
    if (isRead(strm)) nRes |= POLL_BIT_IN;
    if (isWrite(strm)) nRes |= POLL_BIT_OUT;
        /* For the moment we don't allow POLL_BIT_PRI.  */
    return Make_arbitrary_precision(nRes);
}

/* Do the polling.  Takes a vector of io descriptors, a vector of bits to test
   and a time to wait and returns a vector of results. */
static Handle pollDescriptors(Handle args, int blockType)
{
    PolyObject  *strmVec = DEREFHANDLE(args)->Get(0).AsObjPtr();
    PolyObject  *bitVec =  DEREFHANDLE(args)->Get(1).AsObjPtr();
    POLYUNSIGNED nDesc = strmVec->Length();
    ASSERT(nDesc ==  bitVec->Length());
    
    /* Simply do a non-blocking poll. */
#ifdef WINDOWS_PC
    {
        /* Record the results in this vector. */
        char *results;
        int haveResult = 0;
        Handle  resVec;
        if (nDesc > 0)
        {
            results = (char*)alloca(nDesc);
            memset(results, 0, nDesc);
        }
        
        for (POLYUNSIGNED i = 0; i < nDesc; i++)
        {
            Handle marker = gSaveVec->mark();
            PIOSTRUCT strm = get_stream(strmVec->Get(i).AsObjPtr());
            gSaveVec->reset(marker);
            int bits = UNTAGGED(bitVec->Get(i));
            if (strm == NULL) raise_syscall("Stream is closed", EBADF);
            
            if (isSocket(strm))
            {
                SOCKET sock = strm->device.sock;
                if (bits & POLL_BIT_PRI)
                {
                    u_long atMark = 0;
                    ioctlsocket(sock, SIOCATMARK, &atMark);
                    if (atMark) { haveResult = 1; results[i] |= POLL_BIT_PRI; }
                }
                if (bits & (POLL_BIT_IN|POLL_BIT_OUT))
                {
                    FD_SET readFds, writeFds;
                    TIMEVAL poll = {0, 0};
                    FD_ZERO(&readFds); FD_ZERO(&writeFds);
                    if (bits & POLL_BIT_IN) FD_SET(sock, &readFds);
                    if (bits & POLL_BIT_OUT) FD_SET(sock, &writeFds);
                    if (select(FD_SETSIZE, &readFds, &writeFds, NULL, &poll) > 0)
                    {
                        haveResult = 1;
                        /* N.B. select only tells us about out-of-band data if
                        SO_OOBINLINE is FALSE. */
                        if (FD_ISSET(sock, &readFds)) results[i] |= POLL_BIT_IN;
                        if (FD_ISSET(sock, &writeFds)) results[i] |= POLL_BIT_OUT;
                    }
                }
            }
            else
            {
                if ((bits & POLL_BIT_IN) && isRead(strm) && isAvailable(strm))
                {
                    haveResult = 1;
                    results[i] |= POLL_BIT_IN;
                }
                if ((bits & POLL_BIT_OUT) && isWrite(strm))
                {
                    /* I don't know if there's any way to do this. */
                    if (WaitForSingleObject(
                        (HANDLE)_get_osfhandle(strm->device.ioDesc), 0) == WAIT_OBJECT_0)
                    {
                        haveResult = 1;
                        results[i] |= POLL_BIT_OUT;
                    }
                }
                /* PRIORITY doesn't make sense for anything but a socket. */
            }
        }
        if (haveResult == 0)
        {
            /* Poll failed - treat as time out. */
            switch (blockType)
            {
            case 0: /* Check the time out. */
                {
                    /* The time argument is an absolute time. */
                    FILETIME ftTime, ftNow;
                    /* Get the file time. */
                    get_C_pair(DEREFHANDLE(args)->Get(2),
                        &ftTime.dwHighDateTime, &ftTime.dwLowDateTime);
                    GetSystemTimeAsFileTime(&ftNow);
                    /* If the timeout time is earlier than the current time
                    we must return, otherwise we block. */
                    if (CompareFileTime(&ftTime, &ftNow) <= 0)
                        break; /* Return the empty set. */
                    /* else drop through and block. */
                }
            case 1: /* Block until one of the descriptors is ready. */
                processes->block_and_restart(-1, 0, POLY_SYS_io_dispatch);
                /*NOTREACHED*/
            case 2: /* Just a simple poll - drop through. */
                break;
            }
        }
        /* Copy the results to a result vector. */
        if (nDesc == 0) return gSaveVec->push(EmptyString()); /* Empty vector. */
        resVec = alloc_and_save(nDesc);
        for (POLYUNSIGNED j = 0; j < nDesc; j++)
            (DEREFWORDHANDLE(resVec))->Set(j, TAGGED(results[j]));
        return resVec;
    }
#elif (! defined(HAVE_POLL_H))
    /* Unix but poll isn't provided, e.g. Mac OS X.  Implement in terms of "select" as far as we can. */
    {
        fd_set readFds, writeFds, exceptFds;
        struct timeval poll = {0, 0};
        int selectRes = 0;
        FD_ZERO(&readFds); FD_ZERO(&writeFds); FD_ZERO(&exceptFds);

        for (POLYUNSIGNED i = 0; i < nDesc; i++)
        {
            PIOSTRUCT strm = get_stream(strmVec->Get(i).AsObjPtr());
            int bits = UNTAGGED(bitVec->Get(i));
            if (strm == NULL) raise_syscall("Stream is closed", EBADF);
            if (bits & POLL_BIT_IN) FD_SET(strm->device.ioDesc, &readFds);
            if (bits & POLL_BIT_OUT) FD_SET(strm->device.ioDesc, &writeFds);
        }
        /* Simply check the status without blocking. */
        if (nDesc > 0) selectRes = select(FD_SETSIZE, &readFds, &writeFds, &exceptFds, &poll);
        if (selectRes < 0) raise_syscall("select failed", errno);
        /* What if nothing was ready? */
        if (selectRes == 0)
        {
            switch (blockType)
            {
            case 0: /* Check the timeout. */
                {
                    struct timeval tv;
                    struct timezone tz;
                    /* We have a value in microseconds.  We need to split
                    it into seconds and microseconds. */
                    Handle hTime = SAVE(DEREFWORDHANDLE(args)->Get(2));
                    Handle hMillion = Make_arbitrary_precision(1000000);
                    unsigned long secs =
                        get_C_ulong(DEREFWORDHANDLE(div_longc(hMillion, hTime)));
                    unsigned long usecs =
                        get_C_ulong(DEREFWORDHANDLE(rem_longc(hMillion, hTime)));
                        /* If the timeout time is earlier than the current time
                    we must return, otherwise we block. */
                    if (gettimeofday(&tv, &tz) != 0)
                        raise_syscall("gettimeofday failed", errno);
                    if ((unsigned long)tv.tv_sec > secs ||
                        ((unsigned long)tv.tv_sec == secs && (unsigned long)tv.tv_usec >= usecs))
                        break;
                    /* else block. */
                }
            case 1: /* Block until one of the descriptors is ready. */
                processes->block_and_restart(-1, 0, POLY_SYS_io_dispatch);
                /*NOTREACHED*/
            case 2: /* Just a simple poll - drop through. */
                break;
            }
        }
        /* Copy the results. */
        if (nDesc == 0) return gSaveVec->push(EmptyString());
        /* Construct a result vector. */
        Handle resVec = alloc_and_save(nDesc);
        for (POLYUNSIGNED i = 0; i < nDesc; i++)
        {
            POLYUNSIGNED res = 0;
            POLYUNSIGNED bits = UNTAGGED(bitVec->Get(i));
            PIOSTRUCT strm = get_stream(strmVec->Get(i).AsObjPtr());
            if ((bits & POLL_BIT_IN) && FD_ISSET(strm->device.ioDesc, &readFds)) res |= POLL_BIT_IN;
            if ((bits & POLL_BIT_OUT) && FD_ISSET(strm->device.ioDesc, &writeFds)) res |= POLL_BIT_OUT;
            DEREFWORDHANDLE(resVec)->Set(i, TAGGED(res));
        }
        return resVec;
    }
#else
    /* Unix */
    {
        int pollRes = 0;
        struct pollfd * fds = 0;
        if (nDesc > 0)
            fds = (struct pollfd *)alloca(nDesc * sizeof(struct pollfd));
        
        /* Set up the request vector. */
        for (unsigned i = 0; i < nDesc; i++)
        {
            PIOSTRUCT strm = get_stream(strmVec->Get(i).AsObjPtr());
            POLYUNSIGNED bits = UNTAGGED(bitVec->Get(i));
            if (strm == NULL) raise_syscall("Stream is closed", EBADF);
            fds[i].fd = strm->device.ioDesc;
            fds[i].events = 0;
            if (bits & POLL_BIT_IN) fds[i].events |= POLLIN; /* | POLLRDNORM??*/
            if (bits & POLL_BIT_OUT) fds[i].events |= POLLOUT;
            if (bits & POLL_BIT_PRI) fds[i].events |= POLLPRI;
            fds[i].revents = 0;
        }
        /* Poll the descriptors. */
        if (nDesc > 0) pollRes = poll(fds, nDesc, 0);
        if (pollRes < 0) raise_syscall("poll failed", errno);
        /* What if nothing was ready? */
        if (pollRes == 0)
        {
            switch (blockType)
            {
            case 0: /* Check the timeout. */
                {
                    struct timeval tv;
                    struct timezone tz;
                    /* We have a value in microseconds.  We need to split
                    it into seconds and microseconds. */
                    Handle hTime = SAVE(DEREFWORDHANDLE(args)->Get(2));
                    Handle hMillion = Make_arbitrary_precision(1000000);
                    unsigned long secs =
                        get_C_ulong(DEREFWORDHANDLE(div_longc(hMillion, hTime)));
                    unsigned long usecs =
                        get_C_ulong(DEREFWORDHANDLE(rem_longc(hMillion, hTime)));
                        /* If the timeout time is earlier than the current time
                    we must return, otherwise we block. */
                    if (gettimeofday(&tv, &tz) != 0)
                        raise_syscall("gettimeofday failed", errno);
                    if ((unsigned long)tv.tv_sec > secs ||
                        ((unsigned long)tv.tv_sec == secs && (unsigned long)tv.tv_usec >= usecs))
                        break;
                    /* else block. */
                }
            case 1: /* Block until one of the descriptors is ready. */
                processes->block_and_restart(-1, 0, POLY_SYS_io_dispatch);
                /*NOTREACHED*/
            case 2: /* Just a simple poll - drop through. */
                break;
            }
        }
        /* Copy the results. */
        if (nDesc == 0) return gSaveVec->push(EmptyString());
        /* Construct a result vector. */
        Handle resVec = alloc_and_save(nDesc);
        for (unsigned i = 0; i < nDesc; i++)
        {
            int res = 0;
            if (fds[i].revents & POLLIN) res = POLL_BIT_IN;
            if (fds[i].revents & POLLOUT) res = POLL_BIT_OUT;
            if (fds[i].revents & POLLPRI) res = POLL_BIT_PRI;
            DEREFWORDHANDLE(resVec)->Set(i, TAGGED(res));
        }
        return resVec;
    }
#endif
}


/* Directory functions. */
/* Open a directory. */
static Handle openDirectory(Handle dirname)
{
    TCHAR string_buffer[MAXPATHLEN+2];
#ifndef WINDOWS_PC
TryAgain:
#endif
    /* Copy the string and check the length. */
    getFileName(dirname, string_buffer, MAXPATHLEN+2);
    {
        Handle str_token = make_stream_entry();
        int stream_no    = STREAMID(str_token);
        PIOSTRUCT strm = &basic_io_vector[stream_no];
#ifdef WINDOWS_PC
        {
            HANDLE hFind;
            /* Tack on \* to the end so that we find all files in
               the directory. */
            lstrcat(string_buffer, _T("\\*"));
            hFind = FindFirstFile(string_buffer,
                        &strm->device.directory.lastFind);
            if (hFind == INVALID_HANDLE_VALUE)
                raise_syscall("FindFirstFile failed", -(int)GetLastError());
            strm->device.directory.hFind = hFind;
            /* There must be at least one file which matched. */
            strm->device.directory.fFindSucceeded = 1;
        }
#else
        DIR *dirp = opendir(string_buffer);
        if (dirp == NULL)
        {
            free_stream_entry(stream_no);
            switch (errno)
            {
            case EINTR:
                {
                    retry_rts_call();
                    /*NOTREACHED*/
                }
            case EMFILE:
                {
                    if (emfileFlag) /* Previously had an EMFILE error. */
                        raise_syscall("Cannot open", EMFILE);
                    emfileFlag = 1;
                    FullGC(); /* May clear emfileFlag if we close a file. */
                    goto TryAgain;
                }
            default:
                raise_syscall("opendir failed", errno);
            }
        }
        strm->device.ioDir = dirp;
#endif
        strm->ioBits = IO_BIT_OPEN | IO_BIT_DIR;
        return(str_token);
    }
}

/* Return the next entry from the directory, ignoring current and
   parent arcs ("." and ".." in Windows and Unix) */
Handle readDirectory(Handle stream)
{
    PIOSTRUCT strm = get_stream(stream->WordP());
#ifdef WINDOWS_PC
    Handle result = NULL;
#endif
    /* Raise an exception if the stream has been closed. */
    if (strm == NULL) raise_syscall("Stream is closed", EBADF);
#ifdef WINDOWS_PC
    /* The next entry to read is already in the buffer. FindFirstFile
       both opens the directory and returns the first entry. If
       fFindSucceeded is false we have already reached the end. */
    if (! strm->device.directory.fFindSucceeded)
        return SAVE(EmptyString());
    while (result == NULL)
    {
        WIN32_FIND_DATA *pFind = &strm->device.directory.lastFind;
        if (!((pFind->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) &&
            (lstrcmp(pFind->cFileName, _T(".")) == 0 ||
             lstrcmp(pFind->cFileName, _T("..")) == 0)))
        {
            result = SAVE(C_string_to_Poly(pFind->cFileName));
        }
        /* Get the next entry. */
        if (! FindNextFile(strm->device.directory.hFind, pFind))
        {
            DWORD dwErr = GetLastError();
            if (dwErr == ERROR_NO_MORE_FILES)
            {
                strm->device.directory.fFindSucceeded = 0;
                if (result == NULL) return SAVE(EmptyString());
            }
        }
    }
    return result;
#else
    while (1)
    {
        struct dirent *dp = readdir(strm->device.ioDir);
        int len;
        if (dp == NULL) return gSaveVec->push(EmptyString());
        len = NAMLEN(dp);
        if (!((len == 1 && strncmp(dp->d_name, ".", 1) == 0) ||
              (len == 2 && strncmp(dp->d_name, "..", 2) == 0)))
            return SAVE(Buffer_to_Poly(dp->d_name, len));
    }
#endif
}

Handle rewindDirectory(Handle stream, Handle dirname)
{
    PIOSTRUCT strm = get_stream(stream->WordP());
    /* Raise an exception if the stream has been closed. */
    if (strm == NULL) raise_syscall("Stream is closed", EBADF);
#ifdef WINDOWS_PC
    {
        TCHAR string_buffer[MAXPATHLEN+2];
        HANDLE hFind;
        /* There's no rewind - close and reopen. */
        FindClose(strm->device.directory.hFind);
        strm->ioBits = 0;

        getFileName(dirname, string_buffer, MAXPATHLEN+2);
        /* Tack on \* to the end so that we find all files in
           the directory. */
        lstrcat(string_buffer, _T("\\*"));
        hFind = FindFirstFile(string_buffer,
                    &strm->device.directory.lastFind);
        if (hFind == INVALID_HANDLE_VALUE)
            raise_syscall("FindFirstFile failed", -(int)GetLastError());
        strm->device.directory.hFind = hFind;
        /* There must be at least one file which matched. */
        strm->device.directory.fFindSucceeded = 1;
        strm->ioBits = IO_BIT_OPEN | IO_BIT_DIR;
    }
#else
    rewinddir(strm->device.ioDir);
#endif
    return Make_arbitrary_precision(0);
}

/* change_dirc - this is called directly and not via the dispatch
   function. */
Handle change_dirc(Handle name)
/* Change working directory. */
{
    TCHAR string_buffer[MAXPATHLEN];
    getFileName(name, string_buffer, MAXPATHLEN);
#ifdef _WIN32_WCE
	// Windows CE does not support the "current directory" concept.
	raise_syscall("SetCurrentDirectory not implemented", 0);
#elif defined(WINDOWS_PC)
    if (SetCurrentDirectory(string_buffer) == FALSE)
       raise_syscall("SetCurrentDirectory failed", -(int)GetLastError());
#else
    if (chdir(string_buffer) != 0)
        raise_syscall("chdir failed", errno);
#endif
    return SAVE(TAGGED(0));
}

/* Test for a directory. */
Handle isDir(Handle name)
{
    TCHAR string_buffer[MAXPATHLEN];
    getFileName(name, string_buffer, MAXPATHLEN);

#ifdef WINDOWS_PC
    {
        DWORD dwRes = GetFileAttributes(string_buffer);
        if (dwRes == 0xFFFFFFFF)
            raise_syscall("GetFileAttributes failed", -(int)GetLastError());
        if (dwRes & FILE_ATTRIBUTE_DIRECTORY)
            return Make_arbitrary_precision(1);
        else return Make_arbitrary_precision(0);
    }
#else
    {
        struct stat fbuff;
        if (proper_stat(string_buffer, &fbuff) != 0)
            raise_syscall("stat failed", errno);
        if ((fbuff.st_mode & S_IFMT) == S_IFDIR)
            return Make_arbitrary_precision(1);
        else return Make_arbitrary_precision(0);
    }
#endif
}

/* Get absolute canonical path name. */
Handle fullPath(Handle filename)
{
    TCHAR string_buffer[MAXPATHLEN], resBuf[MAXPATHLEN];
    getFileName(filename, string_buffer, MAXPATHLEN);

    /* Special case of an empty string. */
    if (string_buffer[0] == '\0') { string_buffer[0] = '.'; string_buffer[1] = 0;}

#ifdef _WIN32_WCE
	// Windows CE does not support the "current directory" concept.
	raise_syscall("GetFullPathName not implemented", 0);
#elif defined(WINDOWS_PC)
    {
        LPTSTR lastPart;
        DWORD dwRes =
            GetFullPathName(string_buffer, MAXPATHLEN,
                resBuf, &lastPart);
        if (dwRes > MAXPATHLEN)
            raise_syscall("GetFullPathName failed", ENAMETOOLONG);
        if (dwRes == 0)
            raise_syscall("GetFullPathName failed", -(int)GetLastError());
        /* Check that the file exists.  GetFullPathName doesn't do that. */
        dwRes = GetFileAttributes(string_buffer);
        if (dwRes == 0xffffffff)
            raise_syscall("File does not exist", ENOENT);
    }
#else
    {
        struct stat fbuff;
        if (realpath(string_buffer, resBuf) == NULL)
            raise_syscall("realpath failed", errno);
        /* Some versions of Unix don't check the final component
           of a file.  To be consistent try doing a "stat" of
           the resulting string to check it exists. */
        if (proper_stat(resBuf, &fbuff) != 0)
            raise_syscall("stat failed", errno);
    }
#endif
    return(SAVE(C_string_to_Poly(resBuf)));
}

/* Get file modification time.  This returns the value in the
   time units and from the base date used by timing.c. c.f. filedatec */
Handle modTime(Handle filename)
{
    TCHAR string_buffer[MAXPATHLEN];
    getFileName(filename, string_buffer, MAXPATHLEN);
#ifdef WINDOWS_PC
    {
        /* There are two ways to get this information.
           We can either use GetFileTime if we are able
           to open the file for reading but if it is locked
           we won't be able to.  FindFirstFile is the other
           alternative.  We have to check that the file name
           does not contain '*' or '?' otherwise it will try
           to "glob" this, which isn't what we want here. */
        WIN32_FIND_DATA wFind;
        HANDLE hFind;
        TCHAR *p;
        for(p = string_buffer; *p; p++)
            if (*p == '*' || *p == '?')
                raise_syscall("Invalid filename", EBADF);
        hFind = FindFirstFile(string_buffer, &wFind);
        if (hFind == INVALID_HANDLE_VALUE)
            raise_syscall("FindFirstFile failed", -(int)GetLastError());
        FindClose(hFind);
        return Make_arb_from_pair(wFind.ftLastWriteTime.dwHighDateTime,
                                  wFind.ftLastWriteTime.dwLowDateTime);
    }
#else
    {
        struct stat fbuff;
        if (proper_stat(string_buffer, &fbuff) != 0)
            raise_syscall("stat failed", errno);
        /* Convert to microseconds. */
        return Make_arb_from_pair_scaled(fbuff.st_mtime, 0, 1000000);
    }
#endif
}

/* Get file size. */
Handle fileSize(Handle filename)
{
    TCHAR string_buffer[MAXPATHLEN];
    getFileName(filename, string_buffer, MAXPATHLEN);
#ifdef WINDOWS_PC
    {
        /* Similar to modTime*/
        WIN32_FIND_DATA wFind;
        HANDLE hFind;
        TCHAR *p;
        for(p = string_buffer; *p; p++)
            if (*p == '*' || *p == '?')
                raise_syscall("Invalid filename", EBADF);
        hFind = FindFirstFile(string_buffer, &wFind);
        if (hFind == INVALID_HANDLE_VALUE)
            raise_syscall("FindFirstFile failed", -(int)GetLastError());
        FindClose(hFind);
        return Make_arb_from_pair(wFind.nFileSizeHigh, wFind.nFileSizeLow);
    }
#else
    {
    struct stat fbuff;
    if (proper_stat(string_buffer, &fbuff) != 0)
        raise_syscall("stat failed", errno);
    return Make_arbitrary_precision(fbuff.st_size);
    }
#endif
}

/* Set file modification and access times. */
Handle setTime(Handle fileName, Handle fileTime)
{
    TCHAR buff[MAXPATHLEN];
    getFileName(fileName, buff, MAXPATHLEN);

#ifdef WINDOWS_PC
    /* The only way to set the time is to open the file and
       use SetFileTime. */
    {
        FILETIME ft;
        HANDLE hFile;
        /* Get the file time. */
        get_C_pair(DEREFWORDHANDLE(fileTime),
                    &ft.dwHighDateTime, &ft.dwLowDateTime);
        /* Open an existing file with write access. We need that
           for SetFileTime. */
        hFile = CreateFile(buff, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
                    FILE_ATTRIBUTE_NORMAL, NULL);
        if (hFile == INVALID_HANDLE_VALUE)
            raise_syscall("CreateFile failed", -(int)GetLastError());
        /* Set the file time. */
        if (!SetFileTime(hFile, NULL, &ft, &ft))
        {
            int nErr = GetLastError();
            CloseHandle(hFile);
            raise_syscall("SetFileTime failed", -nErr);
        }
        CloseHandle(hFile);
    }
#else
    {
        struct timeval times[2];
        /* We have a value in microseconds.  We need to split
           it into seconds and microseconds. */
        Handle hTime = fileTime;
        Handle hMillion = Make_arbitrary_precision(1000000);
        /* N.B. Arguments to div_longc and rem_longc are in reverse order. */
        unsigned secs =
            get_C_ulong(DEREFWORDHANDLE(div_longc(hMillion, hTime)));
        unsigned usecs =
            get_C_ulong(DEREFWORDHANDLE(rem_longc(hMillion, hTime)));
        times[0].tv_sec = times[1].tv_sec = secs;
        times[0].tv_usec = times[1].tv_usec = usecs;
        if (utimes(buff, times) != 0)
            raise_syscall("utimes failed", errno);
    }
#endif
    return Make_arbitrary_precision(0);
}

/* Rename a file. */
Handle renameFile(Handle oldFileName, Handle newFileName)
{
    TCHAR oldName[MAXPATHLEN], newName[MAXPATHLEN];
    getFileName(oldFileName, oldName, MAXPATHLEN);
    getFileName(newFileName, newName, MAXPATHLEN);
#ifdef WINDOWS_PC
    /* This is defined to delete any existing file with the new name.
       We can do this with MoveFileEx but that isn't supported on
       Windows 95.  We have to use DeleteFile followed by MoveFile in
       that case.
       This means we can't guarantee that a file with
       the new name will always exist because we might fail between
       deleting the original file and renaming the old one. */
    {
        DWORD dwErr;
        /* Try using MoveFile and see if that works. */
        if (MoveFile(oldName, newName))
            return Make_arbitrary_precision(0);
        dwErr = GetLastError();
        if (dwErr != /* ERROR_FILE_EXISTS */ ERROR_ALREADY_EXISTS)
            raise_syscall("MoveFile failed", -(int)dwErr);
        /* Failed because destination file exists. */
#ifndef _WIN32_WCE
		if (_osver & 0x8000)
#endif
        {
            /* Windows 95 - must use delete. */
            if (!DeleteFile(newName))
                raise_syscall("DeleteFile failed", -(int)GetLastError());
            if (!MoveFile(oldName, newName))
                raise_syscall("MoveFile failed", -(int)GetLastError());
        }
#ifndef _WIN32_WCE
        else /* Windows NT.  Although it's not defined to be atomic
                there's a better chance that it will be. */
            if (! MoveFileEx(oldName, newName, MOVEFILE_REPLACE_EXISTING))
                raise_syscall("MoveFileEx failed", -(int)GetLastError());
#endif

    }
#else
    if (rename(oldName, newName) != 0)
        raise_syscall("rename failed", errno);
#endif
    return Make_arbitrary_precision(0);
}

/* Access right requests passed in from ML. */
#define FILE_ACCESS_READ    1
#define FILE_ACCESS_WRITE   2
#define FILE_ACCESS_EXECUTE 4

/* Get access rights to a file. */
Handle fileAccess(Handle name, Handle rights)
{
    TCHAR string_buffer[MAXPATHLEN];
    int rts = get_C_ulong(DEREFWORD(rights));
    getFileName(name, string_buffer, MAXPATHLEN);

#ifdef WINDOWS_PC
    {
        /* Test whether the file is read-only.  This is, of course,
           not what was asked but getting anything more is really
           quite complicated.  I don't see how we can find out if
           a file is executable (maybe check if the extension is
           .exe, .com or .bat?).  It would be possible, in NT, to
           examine the access structures but that seems far too
           complicated.  Leave it for the moment. */
        DWORD dwRes = GetFileAttributes(string_buffer);
        if (dwRes == 0xffffffff)
            return Make_arbitrary_precision(0);
        /* If we asked for write access but it is read-only we
           return false. */
        if ((dwRes & FILE_ATTRIBUTE_READONLY) &&
            (rts & FILE_ACCESS_WRITE))
            return Make_arbitrary_precision(0);
        else return Make_arbitrary_precision(1);
    }
#else
    {
        int mode = 0;
        if (rts & FILE_ACCESS_READ) mode |= R_OK;
        if (rts & FILE_ACCESS_WRITE) mode |= W_OK;
        if (rts & FILE_ACCESS_EXECUTE) mode |= X_OK;
        if (mode == 0) mode = F_OK;
        /* Return true if access is allowed, otherwise false
           for any other error. */
        if (access(string_buffer, mode) == 0)
            return Make_arbitrary_precision(1);
        else return Make_arbitrary_precision(0);
    }
#endif

}



/* IO_dispatchc.  Called from assembly code module. */
Handle IO_dispatch_c(Handle args, Handle strm, Handle code)
{
    int c = get_C_long(DEREFWORD(code));
    switch (c)
    {
    case 0: /* Return standard input */
        return SAVE((PolyObject*)IoEntry(POLY_SYS_stdin));
    case 1: /* Return standard output */
        return SAVE((PolyObject*)IoEntry(POLY_SYS_stdout));
    case 2: /* Return standard error */
        return SAVE((PolyObject*)IoEntry(POLY_SYS_stderr));
    case 3: /* Open file for text input. */
        return open_file(args, O_RDONLY, 0666, 0);
    case 4: /* Open file for binary input. */
        return open_file(args, O_RDONLY | O_BINARY, 0666, 0);
    case 5: /* Open file for text output. */
        return open_file(args, O_WRONLY | O_CREAT | O_TRUNC, 0666, 0);
    case 6: /* Open file for binary output. */
        return open_file(args, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0666, 0);
    case 7: /* Close file */
        return close_file(strm);
    case 8: /* Read text into an array. */
        return readArray(strm, args, true);
    case 9: /* Read binary into an array. */
        return readArray(strm, args, false);
    case 10: /* Get text as a string. */
        return readString(strm, args, true);
    case 11: /* Write from memory into a text file. */
        return writeArray(strm, args, true);
    case 12: /* Write from memory into a binary file. */
        return writeArray(strm, args, false);
    case 13: /* Open text file for appending. */
        /* The IO library definition leaves it open whether this
           should use "append mode" or not.  */
        return open_file(args, O_WRONLY | O_CREAT | O_APPEND, 0666, 0);
    case 14: /* Open binary file for appending. */
        return open_file(args, O_WRONLY | O_CREAT | O_APPEND | O_BINARY, 0666, 0);
    case 15: /* Return recommended buffer size. */
        /* TODO: This should try to find a sensible number based on
           the stream handle passed in. Leave it at 1k for
           the moment. */
        /* Try increasing to 4k. */
        return Make_arbitrary_precision(/*1024*/4096);
    case 16: /* See if we can get some input. */
        return Make_arbitrary_precision(canInput(strm));
    case 17: /* Return the number of bytes available.  */
        return bytesAvailable(strm);

    case 18: /* Get position on stream. */
        {
            /* Get the current position in the stream.  This is used to test
               for the availability of random access so it should raise an
               exception if setFilePos or endFilePos would fail. */
            long pos;
            PIOSTRUCT str = get_stream(strm->WordP());
            if (str == NULL) raise_syscall("Stream is closed", EBADF);

            pos = seekStream(str, 0L, SEEK_CUR);
            return Make_arbitrary_precision(pos);
        }

    case 19: /* Seek to position on stream. */
        {
            long position = get_C_long(DEREFWORD(args));
            long newpos;
            PIOSTRUCT str = get_stream(strm->WordP());
            if (str == NULL) raise_syscall("Stream is closed", EBADF);

            newpos = seekStream(str, position, SEEK_SET);
            return Make_arbitrary_precision(0);
        }

    case 20: /* Return position at end of stream. */
        {
            long original, endOfStream;
            PIOSTRUCT str = get_stream(strm->WordP());
            if (str == NULL) raise_syscall("Stream is closed", EBADF);

            /* Remember our original position, seek to the end, then seek back. */
            original = seekStream(str, 0L, SEEK_CUR);
            endOfStream = seekStream(str, 0L, SEEK_END);
            if (seekStream(str, original, SEEK_SET) != original) 
                raise_syscall("Position error", errno);
            return Make_arbitrary_precision(endOfStream);
        }

    case 21: /* Get the kind of device underlying the stream. */
        return fileKind(strm);
    case 22: /* Return the polling options allowed on this descriptor. */
        return pollTest(strm);
    case 23: /* Poll the descriptor, waiting forever. */
        return pollDescriptors(args, 1);
    case 24: /* Poll the descriptor, waiting for the time requested. */
        return pollDescriptors(args, 0);
    case 25: /* Poll the descriptor, returning immediately.*/
        return pollDescriptors(args, 2);
    case 26: /* Get binary as a vector. */
        return readString(strm, args, false);

    case 27: /* Block until input is available. */
        {
            PIOSTRUCT str = get_stream(strm->WordP());
            if (str == NULL) raise_syscall("Stream is closed", EBADF);
            if (canInput(strm) == 0)
                processes->block_and_restart(str->device.ioDesc, 0, POLY_SYS_io_dispatch);
            return Make_arbitrary_precision(0);
        }

    case 28: /* Test whether output is possible. */
        return Make_arbitrary_precision(canOutput(strm));

    case 29: /* Block until output is possible. */
        if (canOutput(strm) == 0)
            processes->block_and_restart(-1, 0, POLY_SYS_io_dispatch);
        return Make_arbitrary_precision(0);


        /* Functions added for Posix structure. */
    case 30: /* Return underlying file descriptor. */
        /* This is now also used internally to test for
           stdIn, stdOut and stdErr. */
        {
            PIOSTRUCT str = get_stream(strm->WordP());
            if (str == NULL) raise_syscall("Stream is closed", EBADF);
            return Make_arbitrary_precision(str->device.ioDesc);
        }

    case 31: /* Make an entry for a given descriptor. */
        {
            int ioDesc = get_C_long(DEREFWORD(args));
            PIOSTRUCT str;
            /* First see if it's already in the table. */
            for (unsigned i = 0; i < max_streams; i++)
            {
                str = &(basic_io_vector[i]);
                if (str->token != 0 && str->device.ioDesc == ioDesc)
                    return gSaveVec->push(str->token);
            }
            /* Have to make a new entry. */
            Handle str_token = make_stream_entry();
            unsigned stream_no    = STREAMID(str_token);
            str = &basic_io_vector[stream_no];
            str->device.ioDesc = get_C_long(DEREFWORD(args));
            /* We don't know whether it's open for read, write or even if
               it's open at all. */
            str->ioBits = IO_BIT_OPEN | IO_BIT_READ | IO_BIT_WRITE ;
#ifdef WINDOWS_PC
            str->ioBits |= getFileType(ioDesc);
#endif
            return str_token;
        }


    /* Directory functions. */
    case 50: /* Open a directory. */
        return openDirectory(args);

    case 51: /* Read a directory entry. */
        return readDirectory(strm);

    case 52: /* Close the directory */
        return close_file(strm);

    case 53: /* Rewind the directory. */
        return rewindDirectory(strm, args);

    case 54: /* Get current working directory. */
        {
            char string_buffer[MAXPATHLEN+1];
#ifdef _WIN32_WCE
			// Windows CE does not support the "current directory" concept.
			raise_syscall("GetCurrentDirectory not implemented", 0);
#elif defined(WINDOWS_PC)
            if (GetCurrentDirectory(MAXPATHLEN+1, string_buffer) == 0)
               raise_syscall("GetCurrentDirectory failed", -(int)GetLastError());
#else
            if (getcwd(string_buffer, MAXPATHLEN+1) == NULL)
               raise_syscall("getcwd failed", errno);
#endif
            return SAVE(C_string_to_Poly(string_buffer));
        }

    case 55: /* Create a new directory. */
        {
            TCHAR string_buffer[MAXPATHLEN];
            getFileName(args, string_buffer, MAXPATHLEN);

#ifdef WINDOWS_PC
			if (! CreateDirectory(string_buffer, NULL))
			   raise_syscall("CreateDirectory failed", -(int)GetLastError());
#else
            if (mkdir(string_buffer, 0777) != 0)
#endif
                raise_syscall("mkdir failed", errno);

            return Make_arbitrary_precision(0);
        }

    case 56: /* Delete a directory. */
        {
            TCHAR string_buffer[MAXPATHLEN];
            getFileName(args, string_buffer, MAXPATHLEN);

#ifdef WINDOWS_PC
			if (! RemoveDirectory(string_buffer))
			   raise_syscall("RemoveDirectory failed", -(int)GetLastError());
#else
            if (rmdir(string_buffer) != 0)
                raise_syscall("rmdir failed", errno);
#endif

            return Make_arbitrary_precision(0);
        }

    case 57: /* Test for directory. */
        return isDir(args);

    case 58: /* Test for symbolic link. */
        {
            TCHAR string_buffer[MAXPATHLEN];
            getFileName(args, string_buffer, MAXPATHLEN);
#ifdef WINDOWS_PC
            {
                /* Windows does not have symbolic links.  Raise an
                   exception if the file does not exist but otherwise
                   return false.  */
                DWORD dwRes = GetFileAttributes(string_buffer);
                if (dwRes == 0xFFFFFFFF)
                    raise_syscall("GetFileAttributes failed", -(int)GetLastError());
                return Make_arbitrary_precision(0);
            }
#else
            {
            struct stat fbuff;
                if (proper_lstat(string_buffer, &fbuff) != 0)
                    raise_syscall("stat failed", errno);
                if ((fbuff.st_mode & S_IFMT) == S_IFLNK)
                    return Make_arbitrary_precision(1);
                else return Make_arbitrary_precision(0);
            }
#endif
        }

    case 59: /* Read a symbolic link. */
        {
#ifdef WINDOWS_PC
            /* Windows does not have symbolic links. Raise an exception. */
            raise_syscall("Not implemented", 0);
            return gSaveVec->push(TAGGED(0)); /* To keep compiler happy. */
#else
            int nLen;
            char string_buffer[MAXPATHLEN], resBuf[MAXPATHLEN];
            getFileName(args, string_buffer, MAXPATHLEN);

            nLen = readlink(string_buffer, resBuf, sizeof(resBuf));
            if (nLen < 0) raise_syscall("readlink failed", errno);
            return(SAVE(Buffer_to_Poly(resBuf, nLen)));
#endif
        }

    case 60: /* Return the full absolute path name. */
        return fullPath(args);

    case 61: /* Modification time. */
        return modTime(args);

    case 62: /* File size. */
        return fileSize(args);

    case 63: /* Set file time. */
        return setTime(strm, args);

    case 64: /* Delete a file. */
        {
            TCHAR string_buffer[MAXPATHLEN];
            getFileName(args, string_buffer, MAXPATHLEN);

#ifdef WINDOWS_PC
			if (! DeleteFile(string_buffer))
			   raise_syscall("DeleteFile failed", 0-GetLastError());
#else
			if (unlink(string_buffer) != 0)
				raise_syscall("unlink failed", errno);
#endif

            return Make_arbitrary_precision(0);
        }

    case 65: /* rename a file. */
        return renameFile(strm, args);

    case 66: /* Get access rights. */
        return fileAccess(strm, args);

    case 67: /* Return a temporary file name. */
        {
#ifdef _WIN32_WCE
			// Windows CE does not support tempnam
			raise_syscall("Not implemented", 0);
#else
            TCHAR buff[MAXPATHLEN];

#ifdef WINDOWS_PC
            if (GetTempPath(sizeof(buff) - 14, buff) == 0)
                raise_syscall("GetTempPath failed", -(int)(GetLastError()));
            lstrcat(buff, _T("\\"));
#else
#ifdef P_tempdir
            strcpy(buff, P_tempdir);
            strcat(buff, "\\");
#else
            strcpy(buff, "/tmp/");
#endif
#endif
            strcat(buff, "MLTEMPXXXXXX");
#ifdef HAVE_MKSTEMP
            // Set the umask to mask out access by anyone else.
            // mkstemp generally does this anyway.
            mode_t oldMask = umask(0077);
            int fd = mkstemp(buff);
            int wasError = errno;
            (void)umask(oldMask);
            if (fd != -1) close(fd);
            else raise_syscall("mkstemp failed", wasError);
#else
            if (mktemp(buff) == 0)
                raise_syscall("mktemp failed", errno);
            int fd = open(buff, O_RDWR | O_CREAT | O_EXCL, 00600);
            if (fd != -1) close(fd);
            else raise_syscall("Temporary file creation failed", errno);
#endif
            Handle res = SAVE(C_string_to_Poly(buff));
            return res;
#endif // _WIN32_WCE
        }

    case 68: /* Get the file id. */
        {
#ifdef WINDOWS_PC
            /* This concept does not exist in Windows. */
            /* Return a negative number. This is interpreted
               as "not implemented". */
            return Make_arbitrary_precision(-1);
#else
            struct stat fbuff;
            char string_buffer[MAXPATHLEN];
            getFileName(args, string_buffer, MAXPATHLEN);
            if (proper_stat(string_buffer, &fbuff) != 0)
                raise_syscall("stat failed", errno);
            /* Assume that inodes are always non-negative. */
            return Make_arbitrary_precision(fbuff.st_ino);
#endif
        }

    case 69: /* Return an index for a token. */
        return Make_arbitrary_precision(STREAMID(strm));

    case 70: /* Posix.FileSys.openf - open a file with given mode. */
        {
            Handle name = gSaveVec->push(DEREFWORDHANDLE(args)->Get(0));
            int mode = get_C_ulong(DEREFWORDHANDLE(args)->Get(1));
            return open_file(name, mode, 0666, 1);
        }

    case 71: /* Posix.FileSys.createf - create a file with given mode and access. */
        {
            Handle name = gSaveVec->push(DEREFWORDHANDLE(args)->Get(0));
            int mode = get_C_ulong(DEREFWORDHANDLE(args)->Get(1));
            int access = get_C_ulong(DEREFWORDHANDLE(args)->Get(2));
            return open_file(name, mode|O_CREAT, access, 1);
        }

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown io function: %d", c);
            raise_exception_string(EXC_Fail, msg);
			return 0;
        }
    }
}

class BasicIO: public RtsModule
{
public:
    virtual void Init(void);
    virtual void Uninit(void);
    virtual void Reinit(void);
    void GarbageCollect(ScanAddress *process);
};

// Declare this.  It will be automatically added to the table.
static BasicIO basicIOModule;

void BasicIO::Init(void)
{    
    max_streams = 20; // Initialise to the old Unix maximum. Will grow if necessary.
    /* A vector for the streams (initialised by calloc) */
    basic_io_vector = (PIOSTRUCT)calloc(max_streams, sizeof(IOSTRUCT));
}

void BasicIO::Reinit(void)
{
    /* The interface map is recreated after the database
       is committed. */
    basic_io_vector[0].token  = (PolyObject*)IoEntry(POLY_SYS_stdin);
    basic_io_vector[0].device.ioDesc = 0;
    basic_io_vector[0].ioBits = IO_BIT_OPEN | IO_BIT_READ;
#ifdef WINDOWS_PC
    basic_io_vector[0].ioBits |= getFileType(0);
#endif

    basic_io_vector[1].token  = (PolyObject*)IoEntry(POLY_SYS_stdout);
    basic_io_vector[1].device.ioDesc = 1;
    basic_io_vector[1].ioBits = IO_BIT_OPEN | IO_BIT_WRITE;
#ifdef WINDOWS_PC
    basic_io_vector[1].ioBits |= getFileType(1);
#endif

    basic_io_vector[2].token  = (PolyObject*)IoEntry(POLY_SYS_stderr);
    basic_io_vector[2].device.ioDesc = 2;
    basic_io_vector[2].ioBits = IO_BIT_OPEN | IO_BIT_WRITE;
#ifdef WINDOWS_PC
    basic_io_vector[2].ioBits |= getFileType(2);
#endif
    return;
}

/* Release all resources.  Not strictly necessary since the OS should
   do this but probably a good idea. */
void BasicIO::Uninit(void)
{
    if (basic_io_vector)
    {
        for (unsigned i = 0; i < max_streams; i++)
        {
            if (isOpen(&basic_io_vector[i]))
                close_stream(&basic_io_vector[i]);
        }
        free(basic_io_vector);
    }
    basic_io_vector = NULL;
}

void BasicIO::GarbageCollect(ScanAddress *process)
/* Ensures that all the objects are retained and their addresses updated. */
{
    /* Entries in the file table. These are marked as weak references so may
       return 0 for unreferenced streams. */
    for(unsigned i = 0; i < max_streams; i++)
    {
        PIOSTRUCT str = &(basic_io_vector[i]);
        
        if (str->token != 0)
        {
            process->ScanRuntimeAddress(&str->token, ScanAddress::STRENGTH_WEAK);
            
            /* Unreferenced streams may return zero. */ 
            if (str->token == 0 && isOpen(str))
                close_stream(str);
        }
    }
}
