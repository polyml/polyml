/*
    Title:  PolyPerf.cpp

    Copyright (c) 2011 David C.J. Matthews

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

#include <windows.h>
#include <winperf.h>
#include <stdlib.h>
#include <malloc.h>
#include <psapi.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>

#include "../polystatistics.h"

// Statistics currently provided.  These are copied from statistics.cpp
// although the statistics returned may not match.
enum {
    PSC_THREADS = 0,                // Total number of threads
    PSC_THREADS_IN_ML,              // Threads running ML code
    PSC_THREADS_WAIT_IO,            // Threads waiting for IO
    PSC_THREADS_WAIT_MUTEX,         // Threads waiting for a mutex
    PSC_THREADS_WAIT_CONDVAR,       // Threads waiting for a condition var
    PSC_THREADS_WAIT_SIGNAL,        // Special case - signal handling thread
    PSC_GC_FULLGC,                  // Number of full garbage collections
    PSC_GC_PARTIALGC,               // Number of partial GCs
    N_PS_COUNTERS
};

enum {
    PSS_TOTAL_HEAP = 0,                 // Total size of the local heap
    PSS_AFTER_LAST_GC,              // Space free after last GC
    PSS_AFTER_LAST_FULLGC,          // Space free after the last full GC
    PSS_ALLOCATION,                 // Size of allocation space
    PSS_ALLOCATION_FREE,            // Space available in allocation area
    N_PS_SIZES
};

enum {
    PST_NONGC_UTIME = 0,
    PST_NONGC_STIME,
    PST_GC_UTIME,
    PST_GC_STIME,
    N_PS_TIMES
};

#define N_PS_USER   8

/*
This DLL is a plug-in for Windows performance monitoring.  The whole
interface is extremely messy and seems to have remained unchanged
from NT 3.5.  The localised string names displayed in the performance
monitor are held in the registry and need to be set up before this
DLL can be loaded.  Wix v3 supports the strings directly and this
replaces the old method of using lodctr with .ini and .h files.
The DLL is loaded by the performance monitor.  In XP that seems to
be part of the management console application mmc.exe and perfmon.exe
seems to be just a stub.
*/

extern "C" {
    /* These are the functions exported from the DLL.  The names here appear in the
       HKLM\SYSTEM\CurrentControlSet\Services\PolyML\Performance registry key.
       This DLL is loaded by mmc.exe and these functions are called to begin and
       end monitoring and to extract the current performance values from the
       shared memory. */
    __declspec(dllexport) PM_OPEN_PROC OpenPolyPerfMon;
    __declspec(dllexport) PM_CLOSE_PROC ClosePolyPerfMon;
    __declspec(dllexport) PM_COLLECT_PROC CollectPolyPerfMon;
};

// Export the functions without any decoration.
#ifndef _WIN64
#pragma comment(linker, "/export:OpenPolyPerfMon=_OpenPolyPerfMon@4")
#pragma comment(linker, "/export:ClosePolyPerfMon=_ClosePolyPerfMon@0")
#pragma comment(linker, "/export:CollectPolyPerfMon=_CollectPolyPerfMon@16")
#endif

class PolyProcess {
public:
    ~PolyProcess();
    static PolyProcess* CreateProcessEntry(DWORD pID);

    PolyProcess();

    unsigned char *sharedMem; // Pointer to shared memory
    DWORD processID; // Process ID
    WCHAR *processName; // Unicode name
};

// This is the structure of the decoded statistics.  These values
// are set from the ASN1 coding.
// The types of the fields must match the types set in the registry
// by the installer (PolyML.wxs).  For counters that is numberOfItems32
// and for sizes they have to be DWORDs.  The information we display
// for sizes is always a ratio of two sizes (i.e. % full) so we
// have to scale the values consistently to get them to fit.
// Since we also provide type information in the PERF_COUNTER_DEFINITION
// structure it's possible we may be able to override that but it's
// not clear.
typedef struct {
    PERF_COUNTER_BLOCK header;
    // Statistics decoded
    UINT32 psCounters[N_PS_COUNTERS];       // numberOfItems32
#define SIZEOF_COUNTER  (sizeof(UINT32))
    DWORD psSizes[N_PS_SIZES];              // rawFraction/rawBase (i.e. 32-bits)
#define SIZEOF_SIZE     (sizeof(DWORD))
    FILETIME psTimers[N_PS_TIMES];          // timer100Ns
#define SIZEOF_TIME     (sizeof(FILETIME))
    UINT32 psUser[N_PS_USER];               // numberOfItems32
#define SIZEOF_USER     (sizeof(UINT32))
} statistics;

PolyProcess::PolyProcess()
{
    sharedMem = NULL;
    processName = NULL;
}

PolyProcess::~PolyProcess()
{
    if (sharedMem)
        ::UnmapViewOfFile(sharedMem);
    free(processName);
}

// Try to open the shared memory and if it succeeds create an entry for
// this process.
PolyProcess *PolyProcess::CreateProcessEntry(DWORD pId)
{
    char shmName[MAX_PATH];
    sprintf(shmName, POLY_STATS_NAME "%lu", pId);
    HANDLE hRemMemory = OpenFileMapping(FILE_MAP_READ, FALSE, shmName);
    if (hRemMemory == NULL)
        return NULL; // Probably not a Poly/ML process

    unsigned char *sMem = (unsigned char*)MapViewOfFile(hRemMemory, FILE_MAP_READ, 0, 0, 0);
    CloseHandle(hRemMemory); // We don't need this whether it succeeded or not
    if (sMem == NULL)
        return NULL;
    if (*sMem != POLY_STATS_C_STATISTICS)
    {
        UnmapViewOfFile(sMem);
        return NULL;
    }
    // Looks good.
    PolyProcess *result = new PolyProcess;
    result->processID = pId;
    result->sharedMem = sMem;

    // Find the name of the process.
    HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, pId);
    if (hProcess != NULL)
    {
        HMODULE hMod;
        DWORD cbNeeded;
        if (EnumProcessModules(hProcess, &hMod, sizeof(hMod),  &cbNeeded))
        {
            WCHAR processName[MAX_PATH];
            size_t len = 0;
            processName[0] = 0;
            if (GetModuleBaseNameW(hProcess, hMod, processName, sizeof(processName)/sizeof(WCHAR)) != 0)
            {
                // Remove any ".exe" or similar at the end
                len = wcslen(processName);
                if (len > 4 && processName[len-4] == '.')
                    len -= 4;
                processName[len] = 0;
            }
            // Add the process Id in the name
            _snwprintf(processName+len, MAX_PATH-len, L" (%lu)", pId);
            // Copy it into the heap
            result->processName = _wcsdup(processName);
        }
        CloseHandle(hProcess);
    }

    return result;
}

class ASN1Parse {
public:
    ASN1Parse (statistics *s, unsigned char *p): stats(s), ptr(p) {}
    void asn1Decode();
    unsigned getLength();
    INT64 parseInt(unsigned length);
    UINT32 parseUnsigned(unsigned length);
    DWORD parseSize(unsigned length);
    void parseAStatistic(int subTag, unsigned statlen);
    void parseTime(FILETIME *ft, unsigned length);

    statistics *stats;
    unsigned char *ptr;
};

// Decode the ASN1 encoding.  If the decoding fails we just leave the
// values as zero rather than returning any error.
void ASN1Parse::asn1Decode()
{
    unsigned char ch = *ptr++;
    if (ch != POLY_STATS_C_STATISTICS) return;
    unsigned overallLength = getLength();
    unsigned char *endOfData = ptr+overallLength;
    while (ptr < endOfData)
    {
        // Decode a statistic
        unsigned tag = *ptr++;
        unsigned statLen = getLength();
        switch (tag)
        {
        case POLY_STATS_C_COUNTERSTAT:
            parseAStatistic(POLY_STATS_C_COUNTER_VALUE, statLen);
            break;
        case POLY_STATS_C_SIZESTAT:
            parseAStatistic(POLY_STATS_C_BYTE_COUNT, statLen);
            break;
        case POLY_STATS_C_TIMESTAT:
            parseAStatistic(POLY_STATS_C_TIME, statLen);
            break;
        case POLY_STATS_C_USERSTAT:
            parseAStatistic(POLY_STATS_C_COUNTER_VALUE, statLen);
            break; 
        default: ptr += statLen; // Skip it; it's not known
        }
    }
}

// Return the length of the next item
unsigned ASN1Parse::getLength()
{
    unsigned ch = *ptr++;
    if (ch & 0x80)
    {
        int lengthOfLength = ch & 0x7f;
        unsigned length = 0;
        // Ignore "indefinite length", it's not used here.
        while (lengthOfLength--)
        {
            ch = *ptr++;
            length = (length << 8) | ch;
        }
        return length;
    }
    else return ch;
}

// General case for integer.
INT64 ASN1Parse::parseInt(unsigned length)
{
    if (length == 0) return 0;
    INT64 result = *ptr & 0x80 ? -1 : 0;
    while (length--) result = (result << 8) | *ptr++;
    return result;
}

UINT32 ASN1Parse::parseUnsigned(unsigned length)
{
    INT64 value = parseInt(length);
    if (value < 0) return 0; // Can't display negative nos
    return (UINT32)value;
}

DWORD ASN1Parse::parseSize(unsigned length)
{
    INT64 value = parseInt(length);
    if (value < 0) return 0; // Can't display negative nos
    return (DWORD)(value / 1024); // Return kilobytes
}

void ASN1Parse::parseTime(FILETIME *ft, unsigned length)
{
    unsigned char *end = ptr+length;
    UINT32 seconds = 0, useconds = 0;
    while (ptr < end)
    {
        unsigned char tag = *ptr++;
        unsigned elemLen = getLength();
        switch (tag)
        {
        case POLY_STATS_C_SECONDS:
            seconds = parseUnsigned(elemLen);
            break;
        case POLY_STATS_C_MICROSECS:
            useconds = parseUnsigned(elemLen);
            break;
        default: ptr += elemLen;
        }
    }
    ULARGE_INTEGER li;
    li.QuadPart = (ULONGLONG)seconds * 10000000 + (ULONGLONG)useconds * 10;
    ft->dwHighDateTime = li.HighPart;
    ft->dwLowDateTime = li.LowPart;
}

void ASN1Parse::parseAStatistic(int subTag, unsigned statLen)
{
    unsigned char *endOfStat = ptr+statLen;
    unsigned tagId = 0;
    while (ptr < endOfStat)
    {
        unsigned char tag = *ptr++;
        unsigned elemLen = getLength();
        switch (tag)
        {
        case POLY_STATS_C_IDENTIFIER:
            // The identifier of the statistic
            // We rely on the fact that the Id occurs before the value.
            tagId = parseUnsigned(elemLen);
            break;

        case POLY_STATS_C_COUNTER_VALUE:
            if (subTag = POLY_STATS_C_COUNTER_VALUE)
            {
                UINT32 cValue = parseUnsigned(elemLen);
                // A counter value occurs in these statistics
                switch (tagId)
                {
                case POLY_STATS_ID_THREADS:
                    stats->psCounters[PSC_THREADS] = cValue; break;
                case POLY_STATS_ID_THREADS_IN_ML:
                    stats->psCounters[PSC_THREADS_IN_ML] = cValue; break;
                case POLY_STATS_ID_THREADS_WAIT_IO:
                    stats->psCounters[PSC_THREADS_WAIT_IO] = cValue; break;
                case POLY_STATS_ID_THREADS_WAIT_MUTEX:
                    stats->psCounters[PSC_THREADS_WAIT_MUTEX] = cValue; break;
                case POLY_STATS_ID_THREADS_WAIT_CONDVAR:
                    stats->psCounters[PSC_THREADS_WAIT_CONDVAR] = cValue; break;
                case POLY_STATS_ID_THREADS_WAIT_SIGNAL:
                    stats->psCounters[PSC_THREADS_WAIT_SIGNAL] = cValue; break;
                case POLY_STATS_ID_GC_FULLGC:
                    stats->psCounters[PSC_GC_FULLGC] = cValue; break;
                case POLY_STATS_ID_GC_PARTIALGC:
                    stats->psCounters[PSC_GC_PARTIALGC] = cValue; break;
                case POLY_STATS_ID_USER0:
                    stats->psUser[0] = cValue; break;
                case POLY_STATS_ID_USER1:
                    stats->psUser[1] = cValue; break;
                case POLY_STATS_ID_USER2:
                    stats->psUser[2] = cValue; break;
                case POLY_STATS_ID_USER3:
                    stats->psUser[3] = cValue; break;
                case POLY_STATS_ID_USER4:
                    stats->psUser[4] = cValue; break;
                case POLY_STATS_ID_USER5:
                    stats->psUser[5] = cValue; break;
                case POLY_STATS_ID_USER6:
                    stats->psUser[6] = cValue; break;
                case POLY_STATS_ID_USER7:
                    stats->psUser[7] = cValue; break;
                // Anything else is an unknown tag; skip
                }
            }
            else ptr += elemLen; // Skip it - not expected here
            break;

        case POLY_STATS_C_BYTE_COUNT:
            if (subTag == POLY_STATS_C_BYTE_COUNT)
            {
                DWORD cValue = parseSize(elemLen);
                switch (tagId)
                {
                case POLY_STATS_ID_TOTAL_HEAP:
                    stats->psSizes[PSS_TOTAL_HEAP] = cValue; break;
                case POLY_STATS_ID_AFTER_LAST_GC:
                    stats->psSizes[PSS_AFTER_LAST_GC] = cValue; break;
                case POLY_STATS_ID_AFTER_LAST_FULLGC:
                    stats->psSizes[PSS_AFTER_LAST_FULLGC] = cValue; break;
                case POLY_STATS_ID_ALLOCATION:
                    stats->psSizes[PSS_ALLOCATION] = cValue; break;
                case POLY_STATS_ID_ALLOCATION_FREE:
                    stats->psSizes[PSS_ALLOCATION_FREE] = cValue; break;
                }
            }
            else ptr += elemLen; // Skip it - not expected here
            break;

        case POLY_STATS_C_TIME:
            if (subTag == POLY_STATS_C_TIME)
            {
                FILETIME ft = { 0, 0};
                parseTime(&ft, elemLen);
                switch (tagId)
                {
                case POLY_STATS_ID_NONGC_UTIME:
                    stats->psTimers[PST_NONGC_UTIME] = ft; break;
                case POLY_STATS_ID_NONGC_STIME:
                    stats->psTimers[PST_NONGC_STIME] = ft; break;
                case POLY_STATS_ID_GC_UTIME:
                    stats->psTimers[PST_GC_UTIME] = ft; break;
                case POLY_STATS_ID_GC_STIME:
                    stats->psTimers[PST_GC_STIME] = ft; break;
                }
            }
            else ptr += elemLen;
            break;

        default: ptr += elemLen; // Unknown - skip
        }
    }
}

// Pointer to table of processes with the Poly/ML run-time
static PolyProcess **polyProcesses;
static DWORD numProcesses;

// Open: Find the current ML instances.
DWORD APIENTRY OpenPolyPerfMon(LPWSTR lpInstanceNames)
{
    // Get the list of all process IDs.  Because we don't know
    // how many there are we increase the buffer size until the
    // size returned is less than the buffer size.
    DWORD buffItems = 10, numItems;
    DWORD *processIds = NULL;
    while (true) {
        processIds = (DWORD*)malloc(buffItems * sizeof(DWORD));
        if (processIds == NULL)
            return ERROR_NOT_ENOUGH_MEMORY;
        DWORD bytesNeeded;
        if (! EnumProcesses(processIds, buffItems * sizeof(DWORD), &bytesNeeded))
            return GetLastError();
        if (bytesNeeded < buffItems * sizeof(DWORD))
        {
            numItems = bytesNeeded / sizeof(DWORD);
            break;
        }
        buffItems = buffItems * 2;
        free(processIds);
    }
    // How many of these processes provide the Poly/ML shared memory?
    // Make an array big enough for all processes to simplify allocation.
    polyProcesses = (PolyProcess **)malloc(numItems * sizeof(PolyProcess*));
    if (polyProcesses == NULL)
    {
        free(processIds);
        free(polyProcesses);
        return ERROR_NOT_ENOUGH_MEMORY;
    }

    for (DWORD dw = 0; dw < numItems; dw++)
    {
        // See if this is a Poly/ML process
        PolyProcess *pProc = PolyProcess::CreateProcessEntry(processIds[dw]);
        if (pProc != NULL) // We can use this
            polyProcesses[numProcesses++] = pProc;
    }

    free(processIds);
    return ERROR_SUCCESS;
}

// Delete the entries.
DWORD APIENTRY ClosePolyPerfMon(void)
{
    if (polyProcesses != NULL)
    {
        for (DWORD dw = 0; dw < numProcesses; dw++)
            delete(polyProcesses[dw]);
        free(polyProcesses);
    }
    polyProcesses = NULL;
    numProcesses = 0;

    return ERROR_SUCCESS;
}

static LPVOID allocBuffSpace(LPVOID * &lppData, LPDWORD &lpcbTotalBytes, DWORD &dwBytesAvailable, DWORD size)
{
    if (dwBytesAvailable < size) return NULL;
    LPVOID pResult = *lppData;
    *lppData = (LPVOID)((char*)pResult + size);
    memset(pResult, 0, size);
    *lpcbTotalBytes += size;
    dwBytesAvailable -= size;
    return pResult;
}

// This is the entry that actually does the work.
DWORD APIENTRY CollectPolyPerfMon(
   /* IN     */LPWSTR lpRequest,
   /* lpRequest is either "Global" (all counters) or a list of counter numbers to return.
      These are the indexes into counter table in the Perflib\009 registry entry. */
   /* IN OUT */LPVOID* lppData,
   /* IN OUT */LPDWORD lpcbTotalBytes,
   /* OUT    */LPDWORD lpNumObjectTypes)
{
    DWORD dwBytesAvailable = *lpcbTotalBytes;
    LPVOID lpDataStart = *lppData;
    *lpcbTotalBytes = 0;        // Bytes written
    *lpNumObjectTypes = 0;      // Object types written
    // For the moment we ignore the lpRequest argument and return all the counters.

    // First find out where our strings are in the list.  This depends on
    // the strings installed by other applications/services so will vary
    // from machine to machine.  The installer will have added keys under
    // our "service".  If these can't be read then there's nothing we can do.

    HKEY hkPerform;
    LONG err;

    err = RegOpenKeyEx(HKEY_LOCAL_MACHINE,
            "SYSTEM\\CurrentControlSet\\Services\\PolyML\\Performance", 0,
            KEY_READ, &hkPerform);
    if (err != ERROR_SUCCESS)
        return err;

    DWORD dwType, dwSize, dwFirstCounter, dwFirstHelp;

    dwSize = sizeof(dwFirstCounter);
    err = RegQueryValueEx(hkPerform, "First Counter", 0, &dwType, (LPBYTE)&dwFirstCounter, &dwSize);
    if (err != ERROR_SUCCESS)
    {
        RegCloseKey(hkPerform);
        return err;
    }

    dwSize = sizeof(dwFirstHelp);
    err = RegQueryValueEx(hkPerform, "First Help", 0, &dwType, (LPBYTE)&dwFirstHelp, &dwSize);
    if (err != ERROR_SUCCESS)
    {
        RegCloseKey(hkPerform);
        return err;
    }
    RegCloseKey(hkPerform);

    // The actual strings are inserted by the installer.  See PolyML.wxs.
    unsigned stringCount = 0;

    // Object header.  Just one object.
    PERF_OBJECT_TYPE *pObjectType = 
        (PERF_OBJECT_TYPE*)allocBuffSpace(lppData, lpcbTotalBytes, dwBytesAvailable, sizeof(PERF_OBJECT_TYPE));
    if (pObjectType == NULL) return ERROR_MORE_DATA;
    pObjectType->HeaderLength = sizeof(PERF_OBJECT_TYPE);
    pObjectType->ObjectNameTitleIndex = dwFirstCounter + stringCount*2; // First string is the name of the object
    pObjectType->ObjectHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
    pObjectType->DetailLevel = PERF_DETAIL_NOVICE;
    pObjectType->NumCounters = 0;
    pObjectType->DefaultCounter = -1;
    pObjectType->NumInstances = numProcesses;

    // Counter block for each counter.
    // First the numbers
    PERF_COUNTER_DEFINITION *pCounters =
        (PERF_COUNTER_DEFINITION*)allocBuffSpace(lppData, lpcbTotalBytes, dwBytesAvailable,
            sizeof(PERF_COUNTER_DEFINITION) * N_PS_COUNTERS);
    if (pCounters == NULL) return ERROR_MORE_DATA;
    for (unsigned i = 0; i < N_PS_COUNTERS; i++)
    {
        pCounters[i].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
        pCounters[i].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
        pCounters[i].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
        pCounters[i].DetailLevel = PERF_DETAIL_NOVICE;
        pCounters[i].CounterType = PERF_COUNTER_RAWCOUNT;
        pCounters[i].CounterSize = SIZEOF_COUNTER;
        pCounters[i].CounterOffset = offsetof(statistics, psCounters)+i*SIZEOF_COUNTER;
        pObjectType->NumCounters++;
    }

    // The sizes are dealt with specially.  We need to divide the values and express
    // them as percentages.  Each displayed value is followed by a base value.
    PERF_COUNTER_DEFINITION *pSizes =
        (PERF_COUNTER_DEFINITION*)allocBuffSpace(lppData, lpcbTotalBytes, dwBytesAvailable,
            sizeof(PERF_COUNTER_DEFINITION) * 6);
    if (pSizes == NULL) return ERROR_MORE_DATA;
    // First - Heap usage after last GC
    pSizes[0].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
    pSizes[0].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
    pSizes[0].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
    pSizes[0].DetailLevel = PERF_DETAIL_NOVICE;
    pSizes[0].CounterType = PERF_RAW_FRACTION;
    pSizes[0].CounterSize = SIZEOF_SIZE;
    pSizes[0].CounterOffset =
        offsetof(statistics, psSizes)+PSS_AFTER_LAST_GC*SIZEOF_SIZE;
    pObjectType->NumCounters++;
    pSizes[1].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
    pSizes[1].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
    pSizes[1].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
    pSizes[1].DetailLevel = PERF_DETAIL_NOVICE;
    pSizes[1].CounterType = PERF_RAW_BASE;
    pSizes[1].CounterSize = SIZEOF_SIZE;
    pSizes[1].CounterOffset =
        offsetof(statistics, psSizes)+PSS_TOTAL_HEAP*SIZEOF_SIZE;
    pObjectType->NumCounters++;
    // Second - Heap usage after last full GC
    pSizes[2].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
    pSizes[2].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
    pSizes[2].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
    pSizes[2].DetailLevel = PERF_DETAIL_NOVICE;
    pSizes[2].CounterType = PERF_RAW_FRACTION;
    pSizes[2].CounterSize = SIZEOF_SIZE;
    pSizes[2].CounterOffset =
        offsetof(statistics, psSizes)+PSS_AFTER_LAST_FULLGC*SIZEOF_SIZE;
    pObjectType->NumCounters++;
    pSizes[3].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
    pSizes[3].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
    pSizes[3].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
    pSizes[3].DetailLevel = PERF_DETAIL_NOVICE;
    pSizes[3].CounterType = PERF_RAW_BASE;
    pSizes[3].CounterSize = SIZEOF_SIZE;
    pSizes[3].CounterOffset =
        offsetof(statistics, psSizes)+PSS_TOTAL_HEAP*SIZEOF_SIZE;
    pObjectType->NumCounters++;
    // Third - Unreserved space in allocation area
    pSizes[4].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
    pSizes[4].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
    pSizes[4].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
    pSizes[4].DetailLevel = PERF_DETAIL_NOVICE;
    pSizes[4].CounterType = PERF_RAW_FRACTION;
    pSizes[4].CounterSize = SIZEOF_SIZE;
    pSizes[4].CounterOffset =
        offsetof(statistics, psSizes)+PSS_ALLOCATION_FREE*SIZEOF_SIZE;
    pObjectType->NumCounters++;
    pSizes[5].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
    pSizes[5].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
    pSizes[5].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
    pSizes[5].DetailLevel = PERF_DETAIL_NOVICE;
    pSizes[5].CounterType = PERF_RAW_BASE;
    pSizes[5].CounterSize = SIZEOF_SIZE;
    pSizes[5].CounterOffset =
        offsetof(statistics, psSizes)+PSS_ALLOCATION*SIZEOF_SIZE;
    pObjectType->NumCounters++;

    // Then the times
    PERF_COUNTER_DEFINITION *pTimes =
        (PERF_COUNTER_DEFINITION*)allocBuffSpace(lppData, lpcbTotalBytes, dwBytesAvailable,
            sizeof(PERF_COUNTER_DEFINITION) * N_PS_TIMES);
    if (pTimes == NULL) return ERROR_MORE_DATA;
    for (unsigned k = 0; k < N_PS_TIMES; k++)
    {
        pTimes[k].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
        pTimes[k].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
        pTimes[k].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
        pTimes[k].DetailLevel = PERF_DETAIL_NOVICE;
        pTimes[k].CounterType = PERF_100NSEC_TIMER;
        pTimes[k].CounterSize = SIZEOF_TIME;
        pTimes[k].CounterOffset = offsetof(statistics, psTimers)+k*SIZEOF_TIME;
        pObjectType->NumCounters++;
    }

    // Finally the user counters
    PERF_COUNTER_DEFINITION *pUsers =
        (PERF_COUNTER_DEFINITION*)allocBuffSpace(lppData, lpcbTotalBytes, dwBytesAvailable,
            sizeof(PERF_COUNTER_DEFINITION) * N_PS_USER);
    if (pUsers == NULL) return ERROR_MORE_DATA;
    for (unsigned l = 0; l < N_PS_USER; l++)
    {
        pUsers[l].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
        pUsers[l].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
        pUsers[l].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
        pUsers[l].DetailLevel = PERF_DETAIL_NOVICE;
        pUsers[l].CounterType = PERF_COUNTER_RAWCOUNT;
        pUsers[l].CounterSize = SIZEOF_USER;
        pUsers[l].CounterOffset = offsetof(statistics, psUser)+l*SIZEOF_USER;
        pObjectType->NumCounters++;
    }

    pObjectType->DefinitionLength = *lpcbTotalBytes; // End of definitions; start of instance data

    // Instance data - One entry for each process.  Includes the instance name (i.e. the process)
    // and the counter data.
    for (DWORD dw = 0; dw < numProcesses; dw++)
    {
        PERF_INSTANCE_DEFINITION *pInst =
            (PERF_INSTANCE_DEFINITION*)allocBuffSpace(lppData, lpcbTotalBytes, dwBytesAvailable, sizeof(PERF_INSTANCE_DEFINITION));
        if (pInst == NULL) return ERROR_MORE_DATA;
        PolyProcess *pProc = polyProcesses[dw];
        pInst->UniqueID = PERF_NO_UNIQUE_ID; // Better to show the name
        pInst->NameOffset = sizeof(PERF_INSTANCE_DEFINITION); // Name follows
        DWORD len = (DWORD)wcslen(pProc->processName);
        DWORD byteLength = (len+1)*sizeof(WCHAR); // Length including terminators
        pInst->NameLength = byteLength;
        byteLength = (byteLength + 7) / 8 * 8; // Must be rounded up to an eight-byte boundary.
        pInst->ByteLength = byteLength + sizeof(PERF_INSTANCE_DEFINITION);
        WCHAR *pName = (WCHAR*)allocBuffSpace(lppData, lpcbTotalBytes, dwBytesAvailable, byteLength);
        wcscpy(pName, pProc->processName);

        // Now the statistics including a PERF_COUNTER_BLOCK
        DWORD statSize = (sizeof(statistics) + 7) / 8 * 8;
        statistics *pStats  =
            (statistics*)allocBuffSpace(lppData, lpcbTotalBytes, dwBytesAvailable, statSize);
        if (pStats == NULL) return ERROR_MORE_DATA;

        pStats->header.ByteLength = sizeof(PERF_COUNTER_BLOCK)+statSize;
        ASN1Parse decode(pStats, pProc->sharedMem);
        decode.asn1Decode();
    }

    pObjectType->TotalByteLength = *lpcbTotalBytes;
    *lpNumObjectTypes = 1; // Single object
    return ERROR_SUCCESS;
}
