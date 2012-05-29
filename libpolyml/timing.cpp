/*
    Title:      Time functions.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development copyright David C.J. Matthews 2011,12

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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_SYS_SIGNAL_H
#include <sys/signal.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

#include "locking.h"
#include "globals.h"
#include "mpoly.h"
#include "arb.h"
#include "machine_dep.h"
#include "run_time.h"
#include "sys.h"
#include "timing.h"
#include "polystring.h"
#include "save_vec.h"
#include "rts_module.h"
#include "processes.h"
#include "diagnostics.h"
#include "statistics.h"

#if (defined(_WIN32) && ! defined(__CYGWIN__))
/* Windows file times are 64-bit numbers representing times in
   tenths of a microsecond. */
#define TICKS_PER_MICROSECOND 10

#ifdef __GNUC__
#define SECSSINCE1601 11644473600LL
#else
#define SECSSINCE1601 11644473600
#endif

#else
/* For Unix return times in microseconds. */
#define TICKS_PER_MICROSECOND 1
#endif

/*
    The original Poly timing functions used a variety of timing bases
    (e.g. seconds, tenths of a second).  The old functions have been
    retained but the intention is to phase them out in favour of new
    functions.  Most of these are handled through the timing_dispatch
    function.

    The intention behind the timing functions is to make use of the
    arbitrary precision arithmetic to allow for a wider range of dates
    than the usual mktime range of 1970 to 2036.  We also want to handle
    more accurate timing than per second or per microsecond where the
    operating system provides it.
*/

#if (defined(_WIN32) && ! defined(__CYGWIN__))
static FILETIME startTime;
static FILETIME gcUTime, gcSTime, gcRTime;
#else
static struct timeval startTime;
static struct timeval gcUTime, gcSTime, gcRTime;
#endif

GcTimeData gcTimeData; 

#if(!(defined(HAVE_GMTIME_R) && defined(HAVE_LOCALTIME_R)))
// gmtime and localtime are not re-entrant so if we don't have the
// re-entrant versions we need to use a lock.
static PLock timeLock("Timing");
#endif


/* There is a problem with getrusage on Solaris where it sometimes fails with EINTR.
// This is despite the documentation which doesn't mention this and is probably because
// it is implemented by opening a special device.  We need to handle this and there's really
// no harm in doing that on all (Unix) systems.  DCJM 27/2/03.
*/
#if (! defined(_WIN32) || defined(__CYGWIN__))
static int proper_getrusage(int who, struct rusage *rusage)
{
    while (1) {
        int res = getrusage(who, rusage);
        if (res == 0 || errno != EINTR) return res;
    }
}

#endif

Handle timing_dispatch_c(TaskData *taskData, Handle args, Handle code)
{
    int c = get_C_int(taskData, DEREFWORDHANDLE(code));
    switch (c)
    {
    case 0: /* Get ticks per microsecond. */
        return Make_arbitrary_precision(taskData, TICKS_PER_MICROSECOND);
    case 1: /* Return time since the time base. */
        {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            FILETIME ft;
            GetSystemTimeAsFileTime(&ft);
            return Make_arb_from_pair(taskData, ft.dwHighDateTime, ft.dwLowDateTime);
#else
            struct timeval tv;
            if (gettimeofday(&tv, NULL) != 0)
                raise_syscall(taskData, "gettimeofday failed", errno);
            return Make_arb_from_pair_scaled(taskData, tv.tv_sec, tv.tv_usec, 1000000);
#endif
        }
    case 2: /* Return the base year.  This is the year which corresponds to
               zero in the timing sequence. */
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        return Make_arbitrary_precision(taskData, 1601);
#else
        return Make_arbitrary_precision(taskData, 1970);
#endif

    case 3: /* In both Windows and Unix the time base is 1st of January
               in the base year.  This function is provided just in case
               we are running on a system with a different base.  It
               returns the number of seconds after 1st January of the
               base year that corresponds to zero of the time base. */
        return Make_arbitrary_precision(taskData, 0);

    case 4: /* Return the time offset which applied/will apply at the
               specified time (in seconds). */
        {
            int localoff = 0;
            time_t theTime;
            int day = 0;
#if (defined(HAVE_GMTIME_R) || defined(HAVE_LOCALTIME_R))
            struct tm result;
#endif
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            /* Although the offset is in seconds it is since 1601. */
            ULARGE_INTEGER   liTime;
            get_C_pair(taskData, DEREFWORDHANDLE(args), (unsigned long*)&liTime.HighPart, (unsigned long*)&liTime.LowPart); /* May raise exception. */
            theTime = (long)(liTime.QuadPart - SECSSINCE1601);
#else
            theTime = get_C_long(taskData, DEREFWORDHANDLE(args)); /* May raise exception. */
#endif

            {
#ifdef HAVE_GMTIME_R
                struct tm *loctime = gmtime_r(&theTime, &result);
#else
                PLocker lock(&timeLock);
                struct tm *loctime = gmtime(&theTime);
#endif
                if (loctime == NULL) raise_exception0(taskData, EXC_size);
                localoff = (loctime->tm_hour*60 + loctime->tm_min)*60 + loctime->tm_sec;
                day = loctime->tm_yday;
            }

            {

#ifdef HAVE_LOCALTIME_R
                struct tm *loctime = localtime_r(&theTime, &result);
#else
                PLocker lock(&timeLock);
                struct tm *loctime = localtime(&theTime);
#endif
                if (loctime == NULL) raise_exception0(taskData, EXC_size);
                localoff -= (loctime->tm_hour*60 + loctime->tm_min)*60 + loctime->tm_sec;
                if (loctime->tm_yday != day)
                {
                    // Different day - have to correct it.  We can assume that there
                    // is at most one day to correct.
                    if (day == loctime->tm_yday+1 || (day == 0 && loctime->tm_yday >= 364))
                        localoff += 24*60*60;
                    else localoff -= 24*60*60;
                }
            }

            return Make_arbitrary_precision(taskData, localoff);
        }

    case 5: /* Find out if Summer Time (daylight saving) was/will be in effect. */
        {
            time_t theTime;
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            /* Although the offset is in seconds it is since 1601. */
            ULARGE_INTEGER   liTime;
            get_C_pair(taskData, DEREFWORDHANDLE(args), (unsigned long*)&liTime.HighPart, (unsigned long*)&liTime.LowPart); /* May raise exception. */
            theTime = (long)(liTime.QuadPart - SECSSINCE1601);
#else
            theTime = get_C_long(taskData, DEREFWORDHANDLE(args)); /* May raise exception. */
#endif
            int isDst = 0;
#ifdef HAVE_LOCALTIME_R
            struct tm result;
            struct tm *loctime = localtime_r(&theTime, &result);
            isDst = loctime->tm_isdst;
#else
            {
                PLocker lock(&timeLock);
                struct tm *loctime = localtime(&theTime);
                if (loctime == NULL) raise_exception0(taskData, EXC_size);
                isDst = loctime->tm_isdst;
            }
#endif
            return Make_arbitrary_precision(taskData, isDst);
        }

    case 6: /* Call strftime.  It would be possible to do much of this in
               ML except that it requires the current locale. */
        {
            struct  tm time;
            char    *format, buff[2048];
            Handle  resString;
            /* Get the format string. */
            format = Poly_string_to_C_alloc(DEREFHANDLE(args)->Get(0));

            /* Copy the time information. */
            time.tm_year = get_C_int(taskData, DEREFHANDLE(args)->Get(1)) - 1900;
            time.tm_mon = get_C_int(taskData, DEREFHANDLE(args)->Get(2));
            time.tm_mday = get_C_int(taskData, DEREFHANDLE(args)->Get(3));
            time.tm_hour = get_C_int(taskData, DEREFHANDLE(args)->Get(4));
            time.tm_min = get_C_int(taskData, DEREFHANDLE(args)->Get(5));
            time.tm_sec = get_C_int(taskData, DEREFHANDLE(args)->Get(6));
            time.tm_wday = get_C_int(taskData, DEREFHANDLE(args)->Get(7));
            time.tm_yday = get_C_int(taskData, DEREFHANDLE(args)->Get(8));
            time.tm_isdst = get_C_int(taskData, DEREFHANDLE(args)->Get(9));
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            _tzset(); /* Make sure we set the current locale. */
#else
            setlocale(LC_TIME, "");
#endif
            /* It would be better to dynamically allocate the string rather
               than use a fixed size but Unix unlike Windows does not distinguish
               between an error in the input and the buffer being too small. */
            if (strftime(buff, sizeof(buff), format, &time) <= 0)
            {
                /* Error */
                free(format);
                raise_exception0(taskData, EXC_size);
            }
            resString = taskData->saveVec.push(C_string_to_Poly(taskData, buff));
            free(format);
            return resString;
        }

    case 7: /* Return User CPU time since the start. */
        {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            FILETIME ut, ct, et, kt;
            if (! GetProcessTimes(GetCurrentProcess(), &ct, &et, &kt, &ut))
                raise_syscall(taskData, "GetProcessTimes failed", 0-GetLastError());
            return Make_arb_from_pair(taskData, ut.dwHighDateTime, ut.dwLowDateTime);
#else
            struct rusage rusage;
            if (proper_getrusage(RUSAGE_SELF, &rusage) != 0)
                raise_syscall(taskData, "getrusage failed", errno);
            return Make_arb_from_pair_scaled(taskData, rusage.ru_utime.tv_sec,
                        rusage.ru_utime.tv_usec, 1000000);
#endif
        }

    case 8: /* Return System CPU time since the start. */
        {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            FILETIME ct, et, kt, ut;
            if (! GetProcessTimes(GetCurrentProcess(), &ct, &et, &kt, &ut))
                raise_syscall(taskData, "GetProcessTimes failed", 0-GetLastError());
            return Make_arb_from_pair(taskData, kt.dwHighDateTime, kt.dwLowDateTime);
#else
            struct rusage rusage;
            if (proper_getrusage(RUSAGE_SELF, &rusage) != 0)
                raise_syscall(taskData, "getrusage failed", errno);
            return Make_arb_from_pair_scaled(taskData, rusage.ru_stime.tv_sec,
                        rusage.ru_stime.tv_usec, 1000000);
#endif
        }

    case 9: /* Return GC time since the start. */
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        return Make_arb_from_pair(taskData, gcUTime.dwHighDateTime, gcUTime.dwLowDateTime);
#else
        return Make_arb_from_pair_scaled(taskData, gcUTime.tv_sec, gcUTime.tv_usec, 1000000);
#endif

    case 10: /* Return real time since the start. */
        {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            FILETIME ft;
            GetSystemTimeAsFileTime(&ft);
            subFiletimes(&ft, &startTime);
            return Make_arb_from_pair(taskData, ft.dwHighDateTime, ft.dwLowDateTime);
#else
            struct timeval tv;
            if (gettimeofday(&tv, NULL) != 0)
                raise_syscall(taskData, "gettimeofday failed", errno);
            subTimevals(&tv, &startTime);
            return Make_arb_from_pair_scaled(taskData, tv.tv_sec, tv.tv_usec, 1000000);
#endif
        }

        /* These next two are used only in the Posix structure. */
    case 11: /* Return User CPU time used by child processes. */
        {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            return Make_arbitrary_precision(taskData, 0);
#else
            struct rusage rusage;
            if (proper_getrusage(RUSAGE_CHILDREN, &rusage) != 0)
                raise_syscall(taskData, "getrusage failed", errno);
            return Make_arb_from_pair_scaled(taskData, rusage.ru_utime.tv_sec,
                        rusage.ru_utime.tv_usec, 1000000);
#endif
        }

    case 12: /* Return System CPU time used by child processes. */
        {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            return Make_arbitrary_precision(taskData, 0);
#else
            struct rusage rusage;
            if (proper_getrusage(RUSAGE_CHILDREN, &rusage) != 0)
                raise_syscall(taskData, "getrusage failed", errno);
            return Make_arb_from_pair_scaled(taskData, rusage.ru_stime.tv_sec,
                        rusage.ru_stime.tv_usec, 1000000);
#endif
        }

    case 13: /* Return GC system time since the start. */
        /* This function was added in Gansner & Reppy. */
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        return Make_arb_from_pair(taskData, gcSTime.dwHighDateTime, gcSTime.dwLowDateTime);
#else
        return Make_arb_from_pair_scaled(taskData, gcSTime.tv_sec, gcSTime.tv_usec, 1000000);
#endif

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown timing function: %d", c);
            raise_exception_string(taskData, EXC_Fail, msg);
			return 0;
        }
    }
}

#ifdef HAVE_WINDOWS_H
// Getting hard page counts in Windows is not easy.  Cygwin uses
// GetProcessMemoryInfo to return the value in ru_majflt but this
// is actually incorrect because it returns the soft page count not
// the hard page count.  We use NtQuerySystemInformation to get the
// total paging on the system on the basis that this is more useful
// than the soft page faults for the process.
// This is an undocumented interface and it's all a bit of a mess.

typedef struct  {
    INT64 pad1[4];
    DWORD pad2[12];
    DWORD pagesRead;
    DWORD pad3[60];
} SystemPerformanceInfo;

typedef int (WINAPI *NtSystemInfo)(int, SystemPerformanceInfo *, ULONG, void*);
static NtSystemInfo pFunctionPtr;

static long GetPaging(long)
{
    if (pFunctionPtr == 0)
        pFunctionPtr = (NtSystemInfo) GetProcAddress(GetModuleHandle("Ntdll.dll"), "NtQuerySystemInformation");

    if (pFunctionPtr != 0)
    {
        SystemPerformanceInfo pInfo;
        int result = (*pFunctionPtr)(2/*SystemPerformanceInformation*/, &pInfo, sizeof(pInfo), 0);
        if (result == 0)
            return pInfo.pagesRead;
    }
    return 0;
}
#else
inline long GetPaging(long rusagePage)
{
    return rusagePage;
}
#endif

GcTimeData::GcTimeData()
{
    startPF = GetPaging(0);
}

// This function is called at the beginning and end of garbage
// collection to record the time used.
// This also reports the GC time if GC debugging is enabled.
void GcTimeData::RecordGCTime(gcTime isEnd, const char *stage)
{
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    FILETIME kt, ut;
    FILETIME ct, et; // Unused
    FILETIME rt;

    GetProcessTimes(GetCurrentProcess(), &ct, &et, &kt, &ut);
    GetSystemTimeAsFileTime(&rt);

    switch (isEnd)
    {
    case GCTimeStart:
        {
            // Start of GC
            long pageCount = GetPaging(0);
            lastUsageU = ut;
            lastUsageS = kt;
            lastRTime = rt;
            subFiletimes(&ut, &startUsageU);
            subFiletimes(&kt, &startUsageS);
            subFiletimes(&rt, &startRTime);
            if (debugOptions & DEBUG_GC)
            {
                float userTime = filetimeToSeconds(&ut);
                float systemTime = filetimeToSeconds(&kt);
                float realTime = filetimeToSeconds(&rt);
                Log("GC: Non-GC time: CPU user: %0.3f system: %0.3f real: %0.3f page faults: %u\n",
                    userTime, systemTime, realTime, pageCount - startPF);
                // Add to the statistics.
            }
            minorNonGCUserCPU.add(ut);
            majorNonGCUserCPU.add(ut);
            minorNonGCSystemCPU.add(kt);
            majorNonGCSystemCPU.add(kt);
            minorNonGCReal.add(rt);
            majorNonGCReal.add(rt);
            startUsageU = lastUsageU;
            startUsageS = lastUsageS;
            startRTime = lastRTime;
            startPF = pageCount;
            break;
        }

    case GCTimeIntermediate:
        // Report intermediate GC time for debugging
        if (debugOptions & DEBUG_GC)
        {
            FILETIME nextU = ut, nextS = kt, nextR = rt;
            subFiletimes(&ut, &lastUsageU);
            subFiletimes(&kt, &lastUsageS);
            subFiletimes(&rt, &lastRTime);

            float userTime = filetimeToSeconds(&ut);
            float systemTime = filetimeToSeconds(&kt);
            float realTime = filetimeToSeconds(&rt);

            Log("GC: (%s) CPU user: %0.3f system: %0.3f real: %0.3f speed up %0.1f\n", stage, userTime, 
                systemTime, realTime, realTime == 0.0 ? 0.0 : (userTime + systemTime) / realTime);
            lastUsageU = nextU;
            lastUsageS = nextS;
            lastRTime = nextR;
        }
        break;

    case GCTimeEnd: // End of GC.
        {
            long pageCount = GetPaging(0);

            lastUsageU = ut;
            lastUsageS = kt;
            lastRTime = rt;
            subFiletimes(&ut, &startUsageU);
            subFiletimes(&kt, &startUsageS);
            subFiletimes(&rt, &startRTime);
            addFiletimes(&gcUTime, &ut);
            addFiletimes(&gcSTime, &kt);
            addFiletimes(&gcRTime, &rt);

            if (debugOptions & DEBUG_GC)
            {
                float userTime = filetimeToSeconds(&ut);
                float systemTime = filetimeToSeconds(&kt);
                float realTime = filetimeToSeconds(&rt);
                Log("GC: CPU user: %0.3f system: %0.3f real: %0.3f speed up %0.1f page faults %ld\n", userTime, 
                    systemTime, realTime, realTime == 0.0 ? 0.0 : (userTime + systemTime) / realTime,
                    pageCount - startPF);
            }
            minorGCUserCPU.add(ut);
            majorGCUserCPU.add(ut);
            minorGCSystemCPU.add(kt);
            majorGCSystemCPU.add(kt);
            minorGCReal.add(rt);
            majorGCReal.add(rt);
            startUsageU = lastUsageU;
            startUsageS = lastUsageS;
            startRTime = lastRTime;
            startPF = pageCount;
            globalStats.copyGCTimes(gcUTime, gcSTime);
        }
        break;
    }
#else
    switch (isEnd)
    {
    case GCTimeStart:
        {            
            // Start of GC
            struct rusage rusage;
            if (proper_getrusage(RUSAGE_SELF, &rusage) != 0)
                return;
            lastUsage = rusage;
            long pageFaults = GetPaging(rusage.ru_majflt);
            subTimevals(&rusage.ru_utime, &startUsage.ru_utime);
            subTimevals(&rusage.ru_stime, &startUsage.ru_stime);
            struct timeval tv;
            if (gettimeofday(&tv, NULL) != 0)
                return;
            lastRTime = tv;
            subTimevals(&tv, &startRTime);

            if (debugOptions & DEBUG_GC)
            {
                float userTime = timevalToSeconds(&rusage.ru_utime);
                float systemTime = timevalToSeconds(&rusage.ru_stime);
                float realTime = timevalToSeconds(&tv);
                Log("GC: Non-GC time: CPU user: %0.3f system: %0.3f real: %0.3f page faults: %ld\n", userTime, 
                    systemTime, realTime, pageFaults - startPF);
            }
            minorNonGCUserCPU.add(rusage.ru_utime);
            majorNonGCUserCPU.add(rusage.ru_utime);
            minorNonGCSystemCPU.add(rusage.ru_stime);
            majorNonGCSystemCPU.add(rusage.ru_stime);
            minorNonGCReal.add(tv);
            majorNonGCReal.add(tv);
            startUsage = lastUsage;
            startRTime = lastRTime;
            startPF = pageFaults;
            break;
         }

    case GCTimeIntermediate:
        // Report intermediate GC time for debugging
        if (debugOptions & DEBUG_GC)
        {
            struct rusage rusage;
            struct timeval tv;
            if (proper_getrusage(RUSAGE_SELF, &rusage) != 0 || gettimeofday(&tv, NULL) != 0)
                return;
            struct rusage nextUsage = rusage;
            struct timeval nextTime = tv;
            subTimevals(&rusage.ru_utime, &lastUsage.ru_utime);
            subTimevals(&rusage.ru_stime, &lastUsage.ru_stime);
            subTimevals(&tv, &lastRTime);

            float userTime = timevalToSeconds(&rusage.ru_utime);
            float systemTime = timevalToSeconds(&rusage.ru_stime);
            float realTime = timevalToSeconds(&tv);
            Log("GC: %s CPU user: %0.3f system: %0.3f real: %0.3f speed up %0.1f\n", stage, userTime, 
                systemTime, realTime, realTime == 0.0 ? 0.0 : (userTime + systemTime) / realTime);
            lastUsage = nextUsage;
            lastRTime = nextTime;
        }
        break;

    case GCTimeEnd:
        {
            struct rusage rusage;
            if (proper_getrusage(RUSAGE_SELF, &rusage) != 0)
                return;
            lastUsage = rusage;
            long pageFaults = GetPaging(rusage.ru_majflt);
            subTimevals(&rusage.ru_utime, &startUsage.ru_utime);
            subTimevals(&rusage.ru_stime, &startUsage.ru_stime);
            addTimevals(&gcUTime, &rusage.ru_utime);
            addTimevals(&gcSTime, &rusage.ru_stime);
            struct timeval tv;
            if (gettimeofday(&tv, NULL) != 0)
                return;
            lastRTime = tv;
            subTimevals(&tv, &startRTime);
            addTimevals(&gcRTime, &tv);

            if (debugOptions & DEBUG_GC)
            {
                float userTime = timevalToSeconds(&rusage.ru_utime);
                float systemTime = timevalToSeconds(&rusage.ru_stime);
                float realTime = timevalToSeconds(&tv);
                Log("GC: CPU user: %0.3f system: %0.3f real: %0.3f speed up %0.1f page faults %ld\n", userTime, 
                    systemTime, realTime, realTime == 0.0 ? 0.0 :(userTime + systemTime) / realTime,
                    pageFaults-startPF);
            }
            minorGCUserCPU.add(rusage.ru_utime);
            majorGCUserCPU.add(rusage.ru_utime);
            minorGCSystemCPU.add(rusage.ru_stime);
            majorGCSystemCPU.add(rusage.ru_stime);
            minorGCReal.add(tv);
            majorGCReal.add(tv);
            startRTime = lastRTime;
            startPF = pageFaults;
            startUsage = lastUsage;
            globalStats.copyGCTimes(gcUTime, gcSTime);
        }
    }
#endif
}

void GcTimeData::Init()
{
    resetMajorTimingData();
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    memset(&startUsageU, 0, sizeof(startUsageU));
    memset(&startUsageS, 0, sizeof(startUsageS));
    memset(&lastUsageU, 0, sizeof(lastUsageU));
    memset(&lastUsageS, 0, sizeof(lastUsageS));
    memset(&lastRTime, 0, sizeof(lastRTime));
    GetSystemTimeAsFileTime(&startRTime);
    startPF = GetPaging(0);
#else
    memset(&startUsage, 0, sizeof(startUsage));
    memset(&lastUsage, 0, sizeof(lastUsage));
    memset(&lastRTime, 0, sizeof(lastRTime));
    gettimeofday(&startRTime, NULL);
    startPF = GetPaging(0);
#endif
}

void GcTimeData::Final()
{
    // Print the overall statistics
    if (debugOptions & DEBUG_GC)
    {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        FILETIME kt, ut;
        FILETIME ct, et; // Unused
        FILETIME rt;
        GetProcessTimes(GetCurrentProcess(), &ct, &et, &kt, &ut);
        GetSystemTimeAsFileTime(&rt);
        subFiletimes(&ut, &gcUTime);
        subFiletimes(&kt, &gcSTime);
        subFiletimes(&rt, &startTime);
        subFiletimes(&rt, &gcRTime);
        Log("GC (Total): Non-GC time: CPU user: %0.3f system: %0.3f real: %0.3f\n",
            filetimeToSeconds(&ut), filetimeToSeconds(&kt), filetimeToSeconds(&rt));
        Log("GC (Total): GC time: CPU user: %0.3f system: %0.3f real: %0.3f\n",
            filetimeToSeconds(&gcUTime), filetimeToSeconds(&gcSTime), filetimeToSeconds(&gcRTime));
#else
        struct rusage rusage;
        struct timeval tv;
        if (proper_getrusage(RUSAGE_SELF, &rusage) != 0 || gettimeofday(&tv, NULL) != 0)
            return;
        subTimevals(&rusage.ru_utime, &gcUTime);
        subTimevals(&rusage.ru_stime, &gcSTime);
        subTimevals(&tv, &startTime);
        subTimevals(&tv, &gcRTime);
        Log("GC (Total): Non-GC time: CPU user: %0.3f system: %0.3f real: %0.3f\n",
            timevalToSeconds(&rusage.ru_utime), timevalToSeconds(&rusage.ru_stime), timevalToSeconds(&tv));
        Log("GC (Total): GC time: CPU user: %0.3f system: %0.3f real: %0.3f\n",
            timevalToSeconds(&gcUTime), timevalToSeconds(&gcSTime), timevalToSeconds(&gcRTime));
#endif
    }
}


#ifdef HAVE_WINDOWS_H
void addFiletimes(FILETIME *result, const FILETIME *x)
{
    ULARGE_INTEGER liA, liB;
    liA.LowPart = result->dwLowDateTime;
    liA.HighPart = result->dwHighDateTime;
    liB.LowPart = x->dwLowDateTime;
    liB.HighPart = x->dwHighDateTime;
    liA.QuadPart += liB.QuadPart;
    result->dwLowDateTime = liA.LowPart;
    result->dwHighDateTime = liA.HighPart;
}

void subFiletimes(FILETIME *result, const FILETIME *x)
{
    ULARGE_INTEGER liA, liB;
    liA.LowPart = result->dwLowDateTime;
    liA.HighPart = result->dwHighDateTime;
    liB.LowPart = x->dwLowDateTime;
    liB.HighPart = x->dwHighDateTime;
    liA.QuadPart -= liB.QuadPart;
    result->dwLowDateTime = liA.LowPart;
    result->dwHighDateTime = liA.HighPart;
}

float filetimeToSeconds(const FILETIME *x)
{
    ULARGE_INTEGER ul;
    ul.LowPart = x->dwLowDateTime;
    ul.HighPart = x->dwHighDateTime;
    return (float)ul.QuadPart / (float)1.0E7;
}

void FileTimeTime::fromSeconds(unsigned u)
{
    ULARGE_INTEGER li;
    li.QuadPart = (ULONGLONG)u * TICKS_PER_MICROSECOND * 1000000;
    t.dwLowDateTime = li.LowPart;
    t.dwHighDateTime = li.HighPart;
}

void FileTimeTime::add(const FileTimeTime &f)
{
    addFiletimes(&t, &f.t);
}

void FileTimeTime::sub(const FileTimeTime &f)
{
    subFiletimes(&t, &f.t);
}

float FileTimeTime::toSeconds(void)
{
    return filetimeToSeconds(&t);
}

#endif

#ifdef HAVE_SYS_TIME_H
void addTimevals(struct timeval *result, const struct timeval *x)
{
    long uSecs = result->tv_usec + x->tv_usec;
    result->tv_sec += x->tv_sec;
    if (uSecs >= 1000000) { result->tv_sec++; uSecs -= 1000000; }
    result->tv_usec = uSecs;
}

void subTimevals(struct timeval *result, const struct timeval *x)
{
    long uSecs = result->tv_usec - x->tv_usec;
    result->tv_sec -= x->tv_sec;
    if (uSecs < 0) { result->tv_sec--; uSecs += 1000000; }
    result->tv_usec = uSecs;
}

float timevalToSeconds(const struct timeval *x)
{
    return (float)x->tv_sec + (float)x->tv_usec / 1.0E6;
}

void TimeValTime::add(const TimeValTime &f)
{
    addTimevals(&t, &f.t);
}

void TimeValTime::sub(const TimeValTime &f)
{
    subTimevals(&t, &f.t);
}

#endif

void GcTimeData::resetMinorTimingData(void)
{
    minorNonGCUserCPU.fromSeconds(0);
    minorNonGCSystemCPU.fromSeconds(0);
    minorNonGCReal.fromSeconds(0);
    minorGCUserCPU.fromSeconds(0);
    minorGCSystemCPU.fromSeconds(0);
    minorGCReal.fromSeconds(0);
}

void GcTimeData::resetMajorTimingData(void)
{
    minorNonGCUserCPU.fromSeconds(0);
    minorNonGCSystemCPU.fromSeconds(0);
    minorNonGCReal.fromSeconds(0);
    minorGCUserCPU.fromSeconds(0);
    minorGCSystemCPU.fromSeconds(0);
    minorGCReal.fromSeconds(0);
    majorNonGCUserCPU.fromSeconds(0);
    majorNonGCSystemCPU.fromSeconds(0);
    majorNonGCReal.fromSeconds(0);
    majorGCUserCPU.fromSeconds(0);
    majorGCSystemCPU.fromSeconds(0);
    majorGCReal.fromSeconds(0);
}


class Timing: public RtsModule
{
public:
    virtual void Init(void);
    virtual void Stop(void);
};

// Declare this.  It will be automatically added to the table.
static Timing timingModule;

void Timing::Init(void)
{
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    // Record an initial time of day to use as the basis of real timing
    GetSystemTimeAsFileTime(&startTime);
#else
    gettimeofday(&startTime, NULL);
#endif
    gcTimeData.Init();
}

void Timing::Stop()
{
    gcTimeData.Final();
}
