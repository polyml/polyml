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

// Bits to define tests and results in poll.
// These are the values passed to and from ML.
#define POLL_BIT_IN     1
#define POLL_BIT_OUT    2
#define POLL_BIT_PRI    4

// Return values from fileKind
#define FILEKIND_FILE   0
#define FILEKIND_DIR    1
#define FILEKIND_LINK   2
#define FILEKIND_TTY    3
#define FILEKIND_PIPE   4
#define FILEKIND_SKT    5
#define FILEKIND_DEV    6
#define FILEKIND_UNKNOWN 7
#define FILEKIND_ERROR  (-1)


#if (defined(_WIN32) && ! defined(__CYGWIN__))

#include <WinSock2.h>

// Unlike Unix where select and poll can be used on both sockets and other
// streams, in Windows there is no single way of testing different sorts of
// streams.
class WinStreamBase
{
public:
    virtual ~WinStreamBase() {} // Quieten some warnings
    virtual int pollTest() { // Return the valid options for this descriptor
        return 0;
    }
    virtual int poll(int test) { // Return the values set
        return 0;
    }
    // These are not currently used but could be used to poll
    // multiple sockets or streams.
    virtual SOCKET getSocket() {
        return INVALID_SOCKET;
    }
    virtual HANDLE getHandle() {
        return INVALID_HANDLE_VALUE;
    }
};

typedef enum { OPENREAD, OPENWRITE } openMode;

// Normal Windows stream opened on an external file or device.
// 
class WinStream : public WinStreamBase
{
public:
    WinStream() {}
    WinStream(int i) : ioDesc(i) {}

    virtual void openEntry(TaskData * taskData, TCHAR *name, openMode mode, bool isAppend, bool isBinary);
    virtual void closeEntry(TaskData *taskData);
    virtual void waitUntilAvailable(TaskData *taskData);
    virtual void waitUntilOutputPossible(TaskData *taskData);
    virtual ssize_t readStream(TaskData *taskData, byte *base, size_t length);
    virtual long seekStream(TaskData *taskData, long pos, int origin);
    virtual ssize_t writeStream(TaskData *taskData, byte *base, size_t length);
    virtual int fileKind();
    static int fileTypeOfHandle(HANDLE hStream);

    // In general this class does not support polling.
    // We return true for both of these so we will block.
    virtual bool isAvailable(TaskData *taskData) {
        return true; // No general way to test this
    }

    virtual bool canOutput(TaskData *taskData) {
        // There doesn't seem to be a way to do this in Windows.
        return true;
    }

protected:
    int ioDesc;
};

// Windows stream input using overlapped IO and the Windows calls.
class WinInStream : public WinStream
{
public:
    WinInStream();
    ~WinInStream();
    virtual void closeEntry(TaskData *taskData);
    virtual void openEntry(TaskData * taskData, TCHAR *name, bool text);
    virtual ssize_t readStream(TaskData *taskData, byte *base, size_t length);
    virtual bool isAvailable(TaskData *taskData);
    virtual void waitUntilAvailable(TaskData *taskData);
    virtual long seekStream(TaskData *taskData, long pos, int origin);

    virtual ssize_t writeStream(TaskData *taskData, byte *base, size_t length) {
        return -1;
    }

    virtual int fileKind() {
        return WinStream::fileTypeOfHandle(hStream);
    }

    virtual int poll(int test) {
        // We can poll this to test for input.
        return POLL_BIT_IN;
    }

    virtual HANDLE getHandle() {
        return hEvent;
    }

protected:
    void beginReading(TaskData *taskData);

    bool isText; // Remove CRs?
    byte *buffer;
    unsigned buffSize, currentInBuffer, currentPtr;
    bool endOfStream;
    HANDLE hStream;
    HANDLE hEvent;
    OVERLAPPED overlap;
};

// Stream opened on an internal pipe.  This is used for standard input
// whether from the GUI console or the original standard input and also
// for the output from a process created with Windows.execute.
// An event handle is created and signalled whenever input is available.
class WinCopyInStream : public WinStream
{
public:
    WinCopyInStream(int i, HANDLE hEvent) : WinStream(i), hInputAvailable(hEvent) {}

    virtual bool isAvailable(TaskData *taskData);
    virtual void waitUntilAvailable(TaskData *taskData);
    virtual void closeEntry(TaskData *taskData);

    virtual int fileKind() {
        return FILEKIND_PIPE;
    }

protected:
    int ioDesc;
    HANDLE hInputAvailable;
};


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
