/*
    Title:  Header for time functions

    Copyright (c) 2000
        Cambridge University Technical Services Limited
    Further development Copyright David C.J. Matthews 2011-12.

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

#ifndef _TIMING_H_DEFINED
#define _TIMING_H_DEFINED 1

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

/* time functions etc */

extern Handle timing_dispatch_c(TaskData *taskData, Handle args, Handle code);

/* Called by the garbage collector at the beginning and
   end of garbage collection. */
typedef enum __gcTime {
    GCTimeStart,
    GCTimeIntermediate,
    GCTimeEnd
} gcTime;

extern void record_gc_time(gcTime isEnd, const char *stage = "");

/* This contains the information about the time used for GC and
   non-gc time to allow the heap resizing code to make a decision.
   Some of this may be removed once we have a final algorithm.
   There are separate figures for the time since the last minor
   GC and the last major GC.
*/

#ifdef HAVE_WINDOWS_H
class FileTimeTime {
public:
    FileTimeTime() { t.dwLowDateTime = t.dwHighDateTime = 0; }
    FileTimeTime(const FILETIME f) { t = f; }
    void fromSeconds(unsigned u);
    void add(const FileTimeTime &);
    void sub(const FileTimeTime &);
    float toSeconds(void);
protected:
    FILETIME t;
};
#endif

#ifdef HAVE_SYS_TIME_H
class TimeValTime {
public:
    TimeValTime() { t.tv_sec = 0; t.tv_usec = 0; }
    TimeValTime(const timeval f) { t = f; }
    void fromSeconds(unsigned u) { t.tv_sec = u; t.tv_usec = 0; }
    void add(const TimeValTime &);
    void sub(const TimeValTime &);
    float toSeconds(void) { return (float)t.tv_sec + (float)t.tv_usec / 1.0E6; }
protected:
    struct timeval t;
};
#endif


#ifdef WINDOWS_PC /* Native windows */
#define TIMEDATA FileTimeTime
#else /* Unix and Cygwin. */
#define TIMEDATA TimeValTime
#endif

class GcTimeData {
public:
    TIMEDATA minorNonGCUserCPU;
    TIMEDATA minorNonGCSystemCPU;
    TIMEDATA minorNonGCReal;
    TIMEDATA minorGCUserCPU;
    TIMEDATA minorGCSystemCPU;
    TIMEDATA minorGCReal;

    TIMEDATA majorNonGCUserCPU;
    TIMEDATA majorNonGCSystemCPU;
    TIMEDATA majorNonGCReal;
    TIMEDATA majorGCUserCPU;
    TIMEDATA majorGCSystemCPU;
    TIMEDATA majorGCReal;

    void resetMinorTimingData(void);
    void resetMajorTimingData(void);
};

extern GcTimeData gcTimeData;

#ifdef HAVE_WINDOWS_H
extern void addFiletimes(FILETIME *result, const FILETIME *x);
extern void subFiletimes(FILETIME *result, const FILETIME *x);
extern float filetimeToSeconds(const FILETIME *x);
#endif

#ifdef HAVE_SYS_TIME_H
extern void addTimevals(struct timeval *result, const struct timeval *x);
extern void subTimevals(struct timeval *result, const struct timeval *x);
extern float timevalToSeconds(const struct timeval *x);
#endif

#endif
