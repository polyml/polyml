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

There are still some issues with 32-bit/64-bit compatibility.  Currently,
the counters are all defined as 32-bit quantities although the actual
values in psSizes will be 64-bit on a 64-bit system.  Since the x86 is
little-endian that will work so long as the values fit in the low-order
word.  It isn't clear if Wix supports 64-bit values for the base and
fraction.
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

    polystatistics *statistics; // Pointer to shared memory
    DWORD processID; // Process ID
    WCHAR *processName; // Unicode name
};


PolyProcess::PolyProcess()
{
    statistics = NULL;
    processName = NULL;
}

PolyProcess::~PolyProcess()
{
    if (statistics)
        ::UnmapViewOfFile(statistics);
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

    polystatistics *sMem = (polystatistics*)MapViewOfFile(hRemMemory, FILE_MAP_READ, 0, 0, sizeof(polystatistics));
    CloseHandle(hRemMemory); // We don't need this whether it succeeded or not
    if (sMem == NULL)
        return NULL;
    if (sMem->magic != POLY_STATS_MAGIC || sMem->psSize < sizeof(polystatistics))
    {
        UnmapViewOfFile(sMem);
        return NULL;
    }
    // Looks good.
    PolyProcess *result = new PolyProcess;
    result->processID = pId;
    result->statistics = sMem;

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
            result->processName = wcsdup(processName);
        }
        CloseHandle(hProcess);
    }

    return result;
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
    polyProcesses = (PolyProcess **)malloc(numItems * sizeof(polystatistics));
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

static LPVOID allocBuffSpace(LPVOID * &lppData, LPDWORD &lpcbTotalBytes, DWORD &dwBytesAvailable, size_t size)
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
        pCounters[i].CounterOffset =
            sizeof(PERF_COUNTER_BLOCK)+offsetof(polystatistics, psCounters)+i*sizeof(unsigned long);
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
    pSizes[0].CounterOffset =
        sizeof(PERF_COUNTER_BLOCK)+offsetof(polystatistics, psSizes)+PSS_AFTER_LAST_GC*sizeof(size_t);
    pObjectType->NumCounters++;
    pSizes[1].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
    pSizes[1].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
    pSizes[1].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
    pSizes[1].DetailLevel = PERF_DETAIL_NOVICE;
    pSizes[1].CounterType = PERF_RAW_BASE;
    pSizes[1].CounterOffset =
        sizeof(PERF_COUNTER_BLOCK)+offsetof(polystatistics, psSizes)+PSS_TOTAL_HEAP*sizeof(size_t);
    pObjectType->NumCounters++;
    // Second - Heap usage after last full GC
    pSizes[2].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
    pSizes[2].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
    pSizes[2].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
    pSizes[2].DetailLevel = PERF_DETAIL_NOVICE;
    pSizes[2].CounterType = PERF_RAW_FRACTION;
    pSizes[2].CounterOffset =
        sizeof(PERF_COUNTER_BLOCK)+offsetof(polystatistics, psSizes)+PSS_AFTER_LAST_FULLGC*sizeof(size_t);
    pObjectType->NumCounters++;
    pSizes[3].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
    pSizes[3].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
    pSizes[3].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
    pSizes[3].DetailLevel = PERF_DETAIL_NOVICE;
    pSizes[3].CounterType = PERF_RAW_BASE;
    pSizes[3].CounterOffset =
        sizeof(PERF_COUNTER_BLOCK)+offsetof(polystatistics, psSizes)+PSS_TOTAL_HEAP*sizeof(size_t);
    pObjectType->NumCounters++;
    // Third - Unreserved space in allocation area
    pSizes[4].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
    pSizes[4].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
    pSizes[4].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
    pSizes[4].DetailLevel = PERF_DETAIL_NOVICE;
    pSizes[4].CounterType = PERF_RAW_FRACTION;
    pSizes[4].CounterOffset =
        sizeof(PERF_COUNTER_BLOCK)+offsetof(polystatistics, psSizes)+PSS_ALLOCATION_FREE*sizeof(size_t);
    pObjectType->NumCounters++;
    pSizes[5].ByteLength = sizeof(PERF_COUNTER_DEFINITION);
    pSizes[5].CounterNameTitleIndex = dwFirstCounter + stringCount*2;
    pSizes[5].CounterHelpTitleIndex = dwFirstHelp + (stringCount++)*2;
    pSizes[5].DetailLevel = PERF_DETAIL_NOVICE;
    pSizes[5].CounterType = PERF_RAW_BASE;
    pSizes[5].CounterOffset =
        sizeof(PERF_COUNTER_BLOCK)+offsetof(polystatistics, psSizes)+PSS_ALLOCATION*sizeof(size_t);
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
        pTimes[k].CounterOffset =
            sizeof(PERF_COUNTER_BLOCK)+offsetof(polystatistics, psTimers)+k*sizeof(FILETIME);
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
        pUsers[l].CounterOffset =
            sizeof(PERF_COUNTER_BLOCK)+offsetof(polystatistics, psUser)+l*sizeof(int);
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
        DWORD len = wcslen(pProc->processName);
        DWORD byteLength = (len+1)*sizeof(WCHAR); // Length including terminators
        pInst->NameLength = byteLength;
        byteLength = (byteLength + 7) / 8 * 8; // Must be rounded up to an eight-byte boundary.
        pInst->ByteLength = byteLength + sizeof(PERF_INSTANCE_DEFINITION);
        WCHAR *pName = (WCHAR*)allocBuffSpace(lppData, lpcbTotalBytes, dwBytesAvailable, byteLength);
        wcscpy(pName, pProc->processName);

        // Now a PERF_COUNTER_BLOCK
        PERF_COUNTER_BLOCK *pCb =
            (PERF_COUNTER_BLOCK*)allocBuffSpace(lppData, lpcbTotalBytes, dwBytesAvailable, sizeof(PERF_COUNTER_BLOCK));
        if (pCb == NULL) return ERROR_MORE_DATA;
        pCb->ByteLength = sizeof(PERF_COUNTER_BLOCK)+sizeof(polystatistics);

        // Now the actual counters.  Currently we just copy the data from the shared memory.
        polystatistics *pStats =
            (polystatistics*)allocBuffSpace(lppData, lpcbTotalBytes, dwBytesAvailable, sizeof(polystatistics));
        if (pStats == NULL) return ERROR_MORE_DATA;
        memcpy(pStats, pProc->statistics, sizeof(polystatistics));
    }

    pObjectType->TotalByteLength = *lpcbTotalBytes;
    *lpNumObjectTypes = 1; // Single object
    return ERROR_SUCCESS;
}
