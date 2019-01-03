/*
    Title:      Operating Specific functions: Windows version.

    Copyright (c) 2000, 2015, 2018, 2019 David C. J. Matthews

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
#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#include <winsock2.h>
#include <windows.h>

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_TCHAR_H
#include <tchar.h>
#endif

#ifdef HAVE_IO_H
#include <io.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <lmcons.h>

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#include <new>

#include "globals.h"
#include "arb.h"
#include "gc.h"
#include "run_time.h"
#include "io_internal.h"
#include "os_specific.h"
#include "sys.h"
#include "processes.h"
#include "winguiconsole.h"
#include "mpoly.h"
#include "diagnostics.h"
#include "scanaddrs.h"
#include "polystring.h"
#include "save_vec.h"
#include "rts_module.h"
#include "machine_dep.h"
#include "rtsentry.h"
#include "winstartup.h"

#define SAVE(x) taskData->saveVec.push(x)
#define SIZEOF(x) (sizeof(x)/sizeof(word))

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyOSSpecificGeneral(PolyObject *threadId, PolyWord code, PolyWord arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetOSType();
}

typedef enum
{
    HE_UNUSED,
    HE_PROCESS
} HANDENTRYTYPE;

typedef struct {
    HANDLE hProcess, hInput, hOutput;
} PROCESSDATA;

static Handle execute(TaskData *taskData, Handle pname);
static Handle simpleExecute(TaskData *taskData, Handle args);
static Handle openProcessHandle(TaskData *taskData, Handle args, bool fIsRead, bool fIsText);
static Handle openRegistryKey(TaskData *taskData, Handle args, HKEY hkParent);
static Handle createRegistryKey(TaskData *taskData, Handle args, HKEY hkParent);
static Handle queryRegistryKey(TaskData *taskData, Handle args, HKEY hkParent);
static Handle setRegistryKey(TaskData *taskData, Handle args, HKEY hkParent);
static Handle deleteRegistryKey(TaskData *taskData, Handle args, HKEY hkParent);
static Handle deleteRegistryValue(TaskData *taskData, Handle args, HKEY hkParent);
static Handle enumerateRegistry(TaskData *taskData, Handle args, HKEY hkey, BOOL isKey);

// Vector of constants returned by call1006
static POLYUNSIGNED winConstVec[] =
{
    KEY_ALL_ACCESS, // 0
    KEY_CREATE_LINK,
    KEY_CREATE_SUB_KEY,
    KEY_ENUMERATE_SUB_KEYS,
    KEY_EXECUTE,
    KEY_NOTIFY,
    KEY_QUERY_VALUE,
    KEY_READ,
    KEY_SET_VALUE,
    KEY_WRITE, // 9

    STATUS_ACCESS_VIOLATION, // 10
    STATUS_ARRAY_BOUNDS_EXCEEDED,
    STATUS_BREAKPOINT,
    STATUS_CONTROL_C_EXIT,
    STATUS_DATATYPE_MISALIGNMENT,
    STATUS_FLOAT_DENORMAL_OPERAND,
    STATUS_FLOAT_DIVIDE_BY_ZERO,
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_OVERFLOW,
    STATUS_FLOAT_STACK_CHECK,
    STATUS_FLOAT_UNDERFLOW,
    STATUS_GUARD_PAGE_VIOLATION,
    STATUS_INTEGER_DIVIDE_BY_ZERO,
    STATUS_INTEGER_OVERFLOW,
    STATUS_ILLEGAL_INSTRUCTION,
    STATUS_INVALID_DISPOSITION,
#ifdef STATUS_INVALID_HANDLE
    STATUS_INVALID_HANDLE,
#else
    0, // Not defined in Win CE
#endif
    STATUS_IN_PAGE_ERROR,
    STATUS_NONCONTINUABLE_EXCEPTION,
    STATUS_PENDING,
    STATUS_PRIVILEGED_INSTRUCTION,
    STATUS_SINGLE_STEP,
    STATUS_STACK_OVERFLOW,
    STATUS_TIMEOUT,
    STATUS_USER_APC, // 35

    VER_PLATFORM_WIN32s, // 36
    VER_PLATFORM_WIN32_WINDOWS,
    VER_PLATFORM_WIN32_NT, // 38
    // VER_PLATFORM_WIN32_CE is only defined in the Windows CE headers 
#ifdef VER_PLATFORM_WIN32_CE
    VER_PLATFORM_WIN32_CE, // 39
#else
    3, // 39
#endif
};

HKEY hkPredefinedKeyTab[] =
{
    HKEY_CLASSES_ROOT,
    HKEY_CURRENT_USER,
    HKEY_LOCAL_MACHINE,
    HKEY_USERS,
#ifdef HKEY_PERFORMANCE_DATA
    HKEY_PERFORMANCE_DATA,
#else
    0, // Not defined in Win CE
#endif
#ifdef HKEY_CURRENT_CONFIG
    HKEY_CURRENT_CONFIG,
#else
    0,
#endif
#ifdef HKEY_DYN_DATA
    HKEY_DYN_DATA
#else
    0
#endif
};


Handle OS_spec_dispatch_c(TaskData *taskData, Handle args, Handle code)
{
    unsigned c = get_C_unsigned(taskData, DEREFWORD(code));
    switch (c)
    {
    case 0: /* Return our OS type.  Not in any structure. */
        return Make_fixed_precision(taskData, 1); /* 1 for Windows. */

        /* Windows-specific functions. */
    case 1000: /* execute */
        return execute(taskData, args);

    case 1001: /* Get input stream as text. */
        return openProcessHandle(taskData, args, true, true);

    case 1002: /* Get output stream as text. */
        return openProcessHandle(taskData, args, false, true);

    case 1003: /* Get input stream as binary. */
        return openProcessHandle(taskData, args, true, false);

    case 1004: /* Get output stream as binary. */
        return openProcessHandle(taskData, args, false, false);

    case 1005: /* Get result of process. */
        {
            PROCESSDATA *hnd = *(PROCESSDATA**)(args->WordP());
            *(PROCESSDATA**)(args->WordP()) = 0; // Mark as inaccessible.
            if (hnd == 0)
                raise_syscall(taskData, "Process is closed", ERROR_INVALID_HANDLE);
            // Close the streams. Either of them may have been
            // passed to the stream package.
            if (hnd->hInput != INVALID_HANDLE_VALUE)
                CloseHandle(hnd->hInput);
            hnd->hInput = INVALID_HANDLE_VALUE;
            if (hnd->hOutput != INVALID_HANDLE_VALUE)
                CloseHandle(hnd->hOutput);
            hnd->hOutput = INVALID_HANDLE_VALUE;

            // See if it's finished.
            while (true) {
                DWORD dwResult;
                if (GetExitCodeProcess(hnd->hProcess, &dwResult) == 0)
                    raise_syscall(taskData, "GetExitCodeProcess failed", GetLastError());
                if (dwResult != STILL_ACTIVE) {
                    // Finished - return the result.
                    // Remove the process object.  The result is cached in ML.
                    free(hnd);
                    return Make_fixed_precision(taskData, dwResult);
                }
                // Block and try again.
                WaitHandle waiter(hnd->hProcess);
                processes->ThreadPauseForIO(taskData, &waiter);
            }
        }

    case 1006: /* Return a constant. */
        {
            unsigned i = get_C_unsigned(taskData, DEREFWORD(args));
            if (i >= sizeof(winConstVec)/sizeof(winConstVec[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return Make_arbitrary_precision(taskData, winConstVec[i]);
        }

        /* Registry functions. */
    case 1007: // Open a key within one of the roots.
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            // This should only ever happen as a result of a fault in
            // the Windows structure.
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return openRegistryKey(taskData, args, hkPredefinedKeyTab[keyIndex]);
        }

    case 1008: // Open a subkey of an opened key.
        {
            HKEY hKey = *(HKEY*)(args->WordP()->Get(0).AsObjPtr());
            if (hKey == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return openRegistryKey(taskData, args, hKey);
        }

    case 1009: // Create a subkey within one of the roots.
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            // This should only ever happen as a result of a fault in
            // the Windows structure.
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return createRegistryKey(taskData, args, hkPredefinedKeyTab[keyIndex]);
        }

    case 1010: // Create a subkey within an opened key.
        {
            HKEY hKey = *(HKEY*)(args->WordP()->Get(0).AsObjPtr());
            if (hKey == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return createRegistryKey(taskData, args, hKey);
        }

    case 1011: // Close a registry handle.
        {
            HKEY hKey = *(HKEY*)(args->WordP());
            if (hKey != 0)
            {
                RegCloseKey(hKey);
                *(void**)(args->WordP()) = 0;
            }
            return Make_fixed_precision(taskData, 0);
        }

    case 1012: // Get a value
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            // This should only ever happen as a result of a fault in
            // the Windows structure.
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return queryRegistryKey(taskData, args, hkPredefinedKeyTab[keyIndex]);
        }

    case 1013: // Get a value
        {
            HKEY hKey = *(HKEY*)(args->WordP()->Get(0).AsObjPtr());
            if (hKey == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return queryRegistryKey(taskData, args, hKey);
        }

    case 1014: // Delete a subkey
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            // This should only ever happen as a result of a fault in
            // the Windows structure.
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return deleteRegistryKey(taskData, args, hkPredefinedKeyTab[keyIndex]);
        }

    case 1015: // Delete a subkey
        {
            HKEY hKey = *(HKEY*)(args->WordP()->Get(0).AsObjPtr());
            if (hKey == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return deleteRegistryKey(taskData, args, hKey);
        }

    case 1016: // Set a value
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            // This should only ever happen as a result of a fault in
            // the Windows structure.
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return setRegistryKey(taskData, args, hkPredefinedKeyTab[keyIndex]);
        }

    case 1017: // Set a value
        {
            HKEY hKey = *(HKEY*)(args->WordP()->Get(0).AsObjPtr());
            if (hKey == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return setRegistryKey(taskData, args, hKey);
        }

    case 1018: // Enumerate a key in the predefined keys
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return enumerateRegistry(taskData, args, hkPredefinedKeyTab[keyIndex], TRUE);
        }

    case 1019: // Enumerate a key in an opened key
        {
            HKEY hKey = *(HKEY*)(args->WordP()->Get(0).AsObjPtr());
            if (hKey == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return enumerateRegistry(taskData, args, hKey, TRUE);
        }

    case 1020: // Enumerate a value in the predefined keys
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return enumerateRegistry(taskData, args, hkPredefinedKeyTab[keyIndex], FALSE);
        }

    case 1021: // Enumerate a value in an opened key
        {
            HKEY hKey = *(HKEY*)(args->WordP()->Get(0).AsObjPtr());
            if (hKey == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return enumerateRegistry(taskData, args, hKey, FALSE);
        }

    case 1022: // Delete a value
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            // This should only ever happen as a result of a fault in
            // the Windows structure.
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return deleteRegistryValue(taskData, args, hkPredefinedKeyTab[keyIndex]);
        }

    case 1023: // Delete a value
        {
            HKEY hKey = *(HKEY*)(args->WordP()->Get(0).AsObjPtr());
            if (hKey == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return deleteRegistryValue(taskData, args, hKey);
        }


    case 1030: // Convert UTC time values to local time. -- No longer used??
        {
            FILETIME ftUTC, ftLocal;
            /* Get the file time. */
            getFileTimeFromArb(taskData, args, &ftUTC);
            if (! FileTimeToLocalFileTime(&ftUTC, &ftLocal))
                raise_syscall(taskData, "FileTimeToLocalFileTime failed", GetLastError());
            return Make_arb_from_Filetime(taskData, ftLocal);
        }

    case 1031: // Convert local time values to UTC. -- No longer used??
        {
            FILETIME ftUTC, ftLocal;
            /* Get the file time. */
            getFileTimeFromArb(taskData, args, &ftLocal);
            if (! LocalFileTimeToFileTime(&ftLocal, &ftUTC))
                raise_syscall(taskData, "LocalFileTimeToFileTime failed", GetLastError());
            return Make_arb_from_Filetime(taskData, ftUTC);
        }

    case 1032: // Get volume information.
        {
            TCHAR rootName[MAX_PATH], volName[MAX_PATH], sysName[MAX_PATH];
            DWORD dwVolSerial, dwMaxComponentLen, dwFlags;
            Handle volHandle, sysHandle, serialHandle, maxCompHandle;
            Handle resultHandle;
            POLYUNSIGNED length = Poly_string_to_C(DEREFWORD(args), rootName, MAX_PATH);
            if (length > MAX_PATH)
                raise_syscall(taskData, "Root name too long", ERROR_BAD_LENGTH);
            
            if (!GetVolumeInformation(rootName, volName, MAX_PATH,
                    &dwVolSerial, &dwMaxComponentLen, &dwFlags,
                    sysName, MAX_PATH))
                raise_syscall(taskData, "GetVolumeInformation failed", GetLastError());
            volHandle = SAVE(C_string_to_Poly(taskData, volName));
            sysHandle = SAVE(C_string_to_Poly(taskData, sysName));
            serialHandle = Make_arbitrary_precision(taskData, dwVolSerial);
            maxCompHandle = Make_arbitrary_precision(taskData, dwMaxComponentLen);
            resultHandle = alloc_and_save(taskData, 4);
            DEREFHANDLE(resultHandle)->Set(0, volHandle->Word());
            DEREFHANDLE(resultHandle)->Set(1, sysHandle->Word());
            DEREFHANDLE(resultHandle)->Set(2, serialHandle->Word());
            DEREFHANDLE(resultHandle)->Set(3, maxCompHandle->Word());
            return resultHandle;
        }

    case 1033:
        {
            TCHAR fileName[MAX_PATH], execName[MAX_PATH];
            POLYUNSIGNED length = Poly_string_to_C(DEREFWORD(args), fileName, MAX_PATH);
            HINSTANCE hInst;
            if (length > MAX_PATH)
                raise_syscall(taskData, "File name too long", ERROR_BAD_LENGTH);
            hInst = FindExecutable(fileName, NULL, execName);
            if ((uintptr_t)hInst <= 32)
            {
                int error = 0;
                switch ((uintptr_t)hInst) {
                case SE_ERR_FNF: error = ERROR_FILE_NOT_FOUND; break;
                case SE_ERR_PNF: error = ERROR_PATH_NOT_FOUND; break;
                case SE_ERR_ACCESSDENIED: error = ERROR_ACCESS_DENIED; break;
                case SE_ERR_OOM: error = ERROR_NOT_ENOUGH_MEMORY; break;
                case SE_ERR_NOASSOC: error = ERROR_NO_ASSOCIATION; break;
                }
                raise_syscall(taskData, "FindExecutable failed", error);
            }
            return SAVE(C_string_to_Poly(taskData, execName));
        }

    case 1034: // Open a document
        {
            SHELLEXECUTEINFO shellEx;
            memset(&shellEx, 0, sizeof(shellEx));
            shellEx.cbSize = sizeof(shellEx);
            shellEx.lpVerb = _T("open");
            shellEx.lpFile = Poly_string_to_T_alloc(DEREFWORD(args));
            shellEx.hwnd = hMainWindow;
            shellEx.nShow = SW_SHOWNORMAL;
            BOOL fRes = ShellExecuteEx(&shellEx);
            free((void*)shellEx.lpFile);
            if (! fRes)
                raise_syscall(taskData, "ShellExecuteEx failed", GetLastError());
            return Make_fixed_precision(taskData, 0);
        }

    case 1035: // Launch an application.
        {
            SHELLEXECUTEINFO shellEx;
            memset(&shellEx, 0, sizeof(shellEx));
            shellEx.cbSize = sizeof(shellEx);
            shellEx.lpVerb = _T("open");
            shellEx.lpFile = Poly_string_to_T_alloc(args->WordP()->Get(0));
            shellEx.lpParameters = Poly_string_to_T_alloc(args->WordP()->Get(1));
            shellEx.nShow = SW_SHOWNORMAL;
            BOOL fRes = ShellExecuteEx(&shellEx);
            free((void*)shellEx.lpFile);
            free((void*)shellEx.lpParameters);
            if (! fRes)
                raise_syscall(taskData, "ShellExecuteEx failed", GetLastError());
            return Make_fixed_precision(taskData, 0);
        }

    case 1036: // Does the process have its own console?
        return Make_fixed_precision(taskData, hMainWindow != NULL ? 1: 0);

    case 1037: // Simple execute.
        return simpleExecute(taskData, args);


        // DDE
    case 1038: // Start DDE dialogue.
        {
            TCHAR *serviceName = Poly_string_to_T_alloc(args->WordP()->Get(0));
            TCHAR *topicName = Poly_string_to_T_alloc(args->WordP()->Get(1));
            /* Send a request to the main thread to do the work. */
            HCONV hcDDEConv = StartDDEConversation(serviceName, topicName);
            free(serviceName); free(topicName);
            if (hcDDEConv == 0) raise_syscall(taskData, "DdeConnect failed", 0);
            // Create an entry to return the conversation.
            return MakeVolatileWord(taskData, hcDDEConv);
        }

    case 1039: // Send DDE execute request.
        {
            HCONV hcDDEConv = *(HCONV*)(args->WordP()->Get(0).AsObjPtr());
            if (hcDDEConv == 0) raise_syscall(taskData, "DDE Conversation is closed", 0);
            char *command = Poly_string_to_C_alloc(args->WordP()->Get(1));
            /* Send a request to the main thread to do the work. */
            LRESULT res = ExecuteDDE(command, hcDDEConv);
            free(command);
            if (res == -1) raise_syscall(taskData, "DdeClientTransaction failed", 0);
            else return Make_arbitrary_precision(taskData, res);
        }

    case 1040: // Close a DDE conversation.
        {
            HCONV hcDDEConv = *(HCONV*)(args->WordP()->Get(0).AsObjPtr());
            if (hcDDEConv != 0)
            {
                CloseDDEConversation(hcDDEConv);
                *(void**)(args->WordP()->Get(0).AsObjPtr()) = 0;
            }
            return Make_fixed_precision(taskData, 0);
        }


        // Configuration functions.
    case 1050: // Get version data
        {
            OSVERSIONINFO osver;
            ZeroMemory(&osver, sizeof(OSVERSIONINFO));
            osver.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
            // GetVersionEx is deprecated in Windows 8.1
            if (! GetVersionEx(&osver))
                raise_syscall(taskData, "GetVersionEx failed", GetLastError());
            Handle major = Make_fixed_precision(taskData, osver.dwMajorVersion);
            Handle minor = Make_fixed_precision(taskData, osver.dwMinorVersion);
            Handle build = Make_fixed_precision(taskData, osver.dwBuildNumber);
            Handle platform = Make_fixed_precision(taskData, osver.dwPlatformId);
            Handle version = SAVE(C_string_to_Poly(taskData, osver.szCSDVersion));
            Handle resVal = alloc_and_save(taskData, 5);
            DEREFHANDLE(resVal)->Set(0, major->Word());
            DEREFHANDLE(resVal)->Set(1, minor->Word());
            DEREFHANDLE(resVal)->Set(2, build->Word());
            DEREFHANDLE(resVal)->Set(3, platform->Word());
            DEREFHANDLE(resVal)->Set(4, version->Word());
            return resVal;
        }

    case 1051: // Get windows directory
        {
            TCHAR path[MAX_PATH+1];
            if (GetWindowsDirectory(path, sizeof(path)/sizeof(TCHAR)) == 0)
                raise_syscall(taskData, "GetWindowsDirectory failed", GetLastError());
            return SAVE(C_string_to_Poly(taskData, path));
        }

    case 1052: // Get system directory
        {
            TCHAR path[MAX_PATH+1];
            if (GetSystemDirectory(path, sizeof(path)/sizeof(TCHAR)) == 0)
                raise_syscall(taskData, "GetSystemDirectory failed", GetLastError());
            return SAVE(C_string_to_Poly(taskData, path));
        }

    case 1053: // Get computer name
        {
            TCHAR name[MAX_COMPUTERNAME_LENGTH +1];
            DWORD dwSize = MAX_COMPUTERNAME_LENGTH +1;
            if (GetComputerName(name, &dwSize) == 0)
                raise_syscall(taskData, "GetComputerName failed", GetLastError());
            return SAVE(C_string_to_Poly(taskData, name));
        }

    case 1054: // Get user name
        {
            TCHAR name[UNLEN +1];
            DWORD dwSize = UNLEN +1;
            if (GetUserName(name, &dwSize) == 0)
                raise_syscall(taskData, "GetUserName failed", GetLastError());
            return SAVE(C_string_to_Poly(taskData, name));
        }

    case 1100: // Get the error result from the last call.
               // This is saved when we make a call to a foreign function.
        {
            return(SAVE(TAGGED(taskData->lastError)));
        }

    case 1101: // Wait for a message.
        {
            HWND hwnd = *(HWND*)(DEREFWORDHANDLE(args)->Get(0).AsCodePtr());
            UINT wMsgFilterMin = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(1));
            UINT wMsgFilterMax = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
            while (1)
            {
                MSG msg;
                processes->ThreadReleaseMLMemory(taskData);
                // N.B.  PeekMessage may directly call the window proc resulting in a
                // callback to ML.  For this to work a callback must not overwrite "args".
                BOOL result = PeekMessage(&msg, hwnd, wMsgFilterMin, wMsgFilterMax, PM_NOREMOVE);
                processes->ThreadUseMLMemory(taskData);
                if (result) return Make_fixed_precision(taskData, 0);
                // Pause until a message arrives.
                processes->ThreadPause(taskData);
            }
        }

    // case 1102: // Return the address of the window callback function.

    case 1103: // Return the application instance.
        {
            Handle result = alloc_and_save(taskData, 1, F_BYTE_OBJ);
            *(HINSTANCE*)(result->Word().AsCodePtr()) = hApplicationInstance;
            return result;
        }

    case 1104: // Return the main window handle
        {
            Handle result = alloc_and_save(taskData, 1, F_BYTE_OBJ);
            *(HWND*)(result->Word().AsCodePtr()) = hMainWindow;
            return result;
        }

//    case 1105: // Set the callback function

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown windows-specific function: %d", c);
            raise_exception_string(taskData, EXC_Fail, msg);
            return 0;
        }
    }
}

// General interface to Windows OS-specific.  Ideally the various cases will be made into
// separate functions.
POLYUNSIGNED PolyOSSpecificGeneral(PolyObject *threadId, PolyWord code, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedCode = taskData->saveVec.push(code);
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = OS_spec_dispatch_c(taskData, pushedArg, pushedCode);
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // Call 1005 may test for kill
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyGetOSType()
{
    return TAGGED(1).AsUnsigned(); // Return 1 for Windows
}

/*
The Windows version of this is more complicated than the Unix version because
we can't manipulate the pipe handles in the child process.  Everything has to be
set up in the parent.  As with Unix we create two pipes and pass one end of each
pipe to the child.  The end we pass to the child is "inheritable" (i.e. duplicated
in the child as with Unix file descriptors) while the ends we keep in the parent
are non-inheritable (i.e. not duplicated in the child). 
DCJM: December 1999.
This now uses overlapped IO for the streams.
*/
static Handle execute(TaskData *taskData, Handle args)
{
    LPCSTR lpszError = "";
    HANDLE hWriteToChild = INVALID_HANDLE_VALUE,
           hReadFromParent = INVALID_HANDLE_VALUE,
           hWriteToParent = INVALID_HANDLE_VALUE,
           hReadFromChild = INVALID_HANDLE_VALUE;
    STARTUPINFO startupInfo;
    PROCESS_INFORMATION processInfo;
    PROCESSDATA *pProcData = 0;

    LPTSTR commandName = Poly_string_to_T_alloc(args->WordP()->Get(0));
    LPTSTR arguments = Poly_string_to_T_alloc(args->WordP()->Get(1));

    TCHAR toChildPipeName[MAX_PATH], fromChildPipeName[MAX_PATH];
    newPipeName(toChildPipeName);
    newPipeName(fromChildPipeName);
    // Create the pipes as inheritable handles.  These will be passed to the child.
    SECURITY_ATTRIBUTES secure;
    secure.nLength = sizeof(SECURITY_ATTRIBUTES);
    secure.lpSecurityDescriptor = NULL;
    secure.bInheritHandle = TRUE;
    hReadFromParent =
        CreateNamedPipe(toChildPipeName, PIPE_ACCESS_INBOUND | FILE_FLAG_FIRST_PIPE_INSTANCE,
            PIPE_READMODE_BYTE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS, 1, 4096, 4096, 0, &secure);
    if (hReadFromParent == INVALID_HANDLE_VALUE)
    {
        lpszError = "CreateNamedPipe failed";
        goto error;
    }
    hWriteToChild =
        CreateFile(toChildPipeName, GENERIC_WRITE, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED, NULL);
    if (hWriteToChild == INVALID_HANDLE_VALUE)
    {
        lpszError = "CreateFile failed";
        goto error;
    }
    hWriteToParent =
        CreateNamedPipe(fromChildPipeName, PIPE_ACCESS_OUTBOUND | FILE_FLAG_FIRST_PIPE_INSTANCE,
            PIPE_READMODE_BYTE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS, 1, 4096, 4096, 0, &secure);
    if (hWriteToParent == INVALID_HANDLE_VALUE)
    {
        lpszError = "CreateNamedPipe failed";
        goto error;
    }
    hReadFromChild =
        CreateFile(fromChildPipeName, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED, NULL);
    if (hReadFromChild == INVALID_HANDLE_VALUE)
    {
        lpszError = "CreateFile failed";
        goto error;
    }

    // Create a STARTUPINFO structure in which to pass the pipes as stdin
    // and stdout to the new process.
    memset(&startupInfo, 0, sizeof(startupInfo));
    startupInfo.cb = sizeof(startupInfo);
    startupInfo.dwFlags = STARTF_USESTDHANDLES;
    startupInfo.hStdInput = hReadFromParent;
    startupInfo.hStdOutput = hWriteToParent;
    // What should we do about the stderr?  For the moment, inherit the original.
    startupInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    // Treat the empty string as NULL.  This is non-standard.
    if (!CreateProcess(commandName[0] == 0 ? NULL: commandName,
            arguments[0] == 0 ? NULL: arguments, // Command line
            NULL, NULL, TRUE, // Security attributes. Inherit handles
            CREATE_NO_WINDOW, // creation flags
            NULL, NULL, // Inherit our environment and directory
            &startupInfo,
            &processInfo)) {
        lpszError = "Could not create process";
        goto error;
    }
    pProcData = (PROCESSDATA *)malloc(sizeof(PROCESSDATA));
    if (pProcData == 0)
    {
        lpszError = "Insufficient memory";
        SetLastError(ERROR_NOT_ENOUGH_MEMORY);
        goto error;
    }

    pProcData->hProcess = processInfo.hProcess;
    pProcData->hInput = hReadFromChild;
    pProcData->hOutput = hWriteToChild;

    // Everything has gone well - remove what we don't want
    free(commandName);
    free(arguments);
    /* Close thread handle since we don't need it. */
    CloseHandle(processInfo.hThread);
    /* Close the sides of the pipes we don't use in the parent. */
    CloseHandle(hReadFromParent);
    CloseHandle(hWriteToParent);

    return(MakeVolatileWord(taskData, pProcData));

error:
    {
        int err = GetLastError();
        free(commandName);
        free(arguments);
        free(pProcData);
        // Close all the pipe handles.
        if (hWriteToChild != INVALID_HANDLE_VALUE) CloseHandle(hWriteToChild);
        if (hReadFromParent != INVALID_HANDLE_VALUE) CloseHandle(hReadFromParent);
        if (hWriteToParent != INVALID_HANDLE_VALUE) CloseHandle(hWriteToParent);
        if (hReadFromChild != INVALID_HANDLE_VALUE) CloseHandle(hReadFromChild);
        raise_syscall(taskData, lpszError, err);
        return NULL; // Never reached.
    }
}

static Handle simpleExecute(TaskData *taskData, Handle args)
{
    HANDLE hNull = INVALID_HANDLE_VALUE;
    PROCESS_INFORMATION processInfo;
    TCHAR *commandName = Poly_string_to_T_alloc(args->WordP()->Get(0));
    TCHAR *arguments = Poly_string_to_T_alloc(args->WordP()->Get(1));

    STARTUPINFO startupInfo;
    // Open a handle to NUL for input and output.
    hNull = CreateFile(_T("NUL"), GENERIC_READ|GENERIC_WRITE,
                FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
                FILE_ATTRIBUTE_NORMAL, NULL);

    // Create a STARTUPINFO structure in which to pass hNULL as stdin
    // and stdout to the new process.
    // TODO: The handles should really be open on "NUL".
    memset(&startupInfo, 0, sizeof(startupInfo));
    startupInfo.cb = sizeof(startupInfo);
    startupInfo.dwFlags = STARTF_USESTDHANDLES;
    startupInfo.hStdInput = hNull;
    startupInfo.hStdOutput = hNull;
    startupInfo.hStdError = hNull;
    STARTUPINFO *start = &startupInfo;

    // Treat the empty string as NULL.  This is non-standard.
    if (!CreateProcess(commandName[0] == 0 ? NULL : commandName,
            arguments[0] == 0 ? NULL : arguments, // Command line
            NULL, NULL, // Security attributes
            TRUE, CREATE_NO_WINDOW, // Inherit handles, creation flags
            NULL, NULL, // Inherit our environment and directory
            start,
            &processInfo)) {
        int nErr = GetLastError();
        // Clean up
        free(commandName);
        free(arguments);
        CloseHandle(hNull);
        raise_syscall(taskData, "CreateProcess failed", nErr);
    }

    free(commandName);
    free(arguments);
    /* Close thread handle since we don't need it. */
    CloseHandle(processInfo.hThread);
#ifndef _WIN32_WCE
    CloseHandle(hNull); // We no longer need this
#endif

    PROCESSDATA *pProcData = (PROCESSDATA *)malloc(sizeof(PROCESSDATA));
    if (pProcData == 0)
        raise_syscall(taskData, "Insufficient memory", ERROR_NOT_ENOUGH_MEMORY);

    pProcData->hProcess = processInfo.hProcess;
    // We only use the process handle entry.
    pProcData->hInput = INVALID_HANDLE_VALUE;
    pProcData->hOutput = INVALID_HANDLE_VALUE;

    return(MakeVolatileWord(taskData, pProcData));
}

/* Return a stream, either text or binary, connected to an open process. */
static Handle openProcessHandle(TaskData *taskData, Handle args, bool fIsRead, bool fIsText)
{
    PROCESSDATA *hnd = *(PROCESSDATA**)(args->WordP());
    if (hnd == 0)
        raise_syscall(taskData, "Process is closed", ERROR_INVALID_HANDLE);
    // We allow multiple streams on the handles.  Since they are duplicated by openHandle that's safe.
    // A consequence is that closing the stream does not close the pipe as far as the child is
    // concerned.  That only happens when we close the final handle in reap.
    try
    {
        WinInOutStream *stream = new WinInOutStream;
        bool result;
        if (fIsRead) result = stream->openHandle(hnd->hInput, OPENREAD, fIsText);
        else result = stream->openHandle(hnd->hOutput, OPENWRITE, fIsText);
        if (!result)
        {
            delete(stream);
            raise_syscall(taskData, "openHandle failed", GetLastError());
        }

        return MakeVolatileWord(taskData, stream);
    }
    catch (std::bad_alloc&)
    {
        raise_syscall(taskData, "Insufficient memory", ERROR_NOT_ENOUGH_MEMORY);
    }
}

// Open a registry key and make an entry in the table for it.
static Handle openRegistryKey(TaskData *taskData, Handle args, HKEY hkParent)
{
    TCHAR keyName[MAX_PATH];
    REGSAM sam = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
    POLYUNSIGNED length = Poly_string_to_C(args->WordP()->Get(1), keyName, MAX_PATH);
    if (length > MAX_PATH)
        raise_syscall(taskData, "Key name too long", ERROR_BAD_LENGTH);
    // Try opening the key.
    HKEY hkey;
    LONG lRes = RegOpenKeyEx(hkParent, keyName, 0, sam, &hkey);
    if (lRes != ERROR_SUCCESS)
        raise_syscall(taskData, "RegOpenKeyEx failed", lRes);

    return MakeVolatileWord(taskData, hkey);
}

// Create a registry key and make an entry in the table for it.
static Handle createRegistryKey(TaskData *taskData, Handle args, HKEY hkParent)
{
    TCHAR keyName[MAX_PATH];
    HKEY hkey;
    DWORD dwDisp;
    REGSAM sam = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(3));
    unsigned opt = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
    POLYUNSIGNED length = Poly_string_to_C(args->WordP()->Get(1), keyName, MAX_PATH);
    if (length > MAX_PATH)
        raise_syscall(taskData, "Key name too long", ERROR_BAD_LENGTH);

    // Try opening the key.
    LONG lRes = RegCreateKeyEx(hkParent, keyName, 0, NULL,
                opt ? REG_OPTION_NON_VOLATILE : REG_OPTION_VOLATILE,
                sam, NULL, &hkey, &dwDisp);
    if (lRes != ERROR_SUCCESS)
        raise_syscall(taskData, "RegCreateKeyEx failed", lRes);

    // Make an entry in the table.
    Handle keyResult = MakeVolatileWord(taskData, hkey);
    // Record whether this was new or old.
    Handle dispRes = Make_fixed_precision(taskData, dwDisp == REG_CREATED_NEW_KEY ? 0: 1);
    /* Return a pair of the disposition and the token. */
    Handle pair = alloc_and_save(taskData, 2);
    DEREFHANDLE(pair)->Set(0, dispRes->Word());
    DEREFHANDLE(pair)->Set(1, keyResult->Word());
    return pair;
}

// Delete a key.  Note that in Windows NT (but not 95) this will fail if
// the key has subkeys.
static Handle deleteRegistryKey(TaskData *taskData, Handle args, HKEY hkParent)
{
    TCHAR keyName[MAX_PATH];
    LONG lRes;
    POLYUNSIGNED length = Poly_string_to_C(args->WordP()->Get(1), keyName, MAX_PATH);
    if (length > MAX_PATH)
        raise_syscall(taskData, "Key name too long", ERROR_BAD_LENGTH);

    // Try deleting the key.  
    lRes = RegDeleteKey(hkParent, keyName);
    if (lRes != ERROR_SUCCESS)
        /* Return the error. */
        raise_syscall(taskData, "RegDeleteKey failed", lRes);
    return Make_fixed_precision(taskData, 0);
}

static Handle deleteRegistryValue(TaskData *taskData, Handle args, HKEY hkParent)
{
    TCHAR keyName[MAX_PATH];
    LONG lRes;
    POLYUNSIGNED length = Poly_string_to_C(args->WordP()->Get(1), keyName, MAX_PATH);
    if (length > MAX_PATH)
        raise_syscall(taskData, "Key name too long", ERROR_BAD_LENGTH);

    // Try deleting the value.
    lRes = RegDeleteValue(hkParent, keyName);
    if (lRes != ERROR_SUCCESS)
        /* Return the original error. */
        raise_syscall(taskData, "RegDeleteValue failed", lRes);
    return Make_fixed_precision(taskData, 0);
}

static Handle queryRegistryKey(TaskData *taskData, Handle args, HKEY hkey)
{
    TCHAR valName[MAX_PATH];
    byte *keyValue = 0;
    LONG lRes;
    DWORD valSize;
    Handle result, resVal, resType;
    DWORD dwType;
    POLYUNSIGNED length = Poly_string_to_C(args->WordP()->Get(1), valName, MAX_PATH);
    if (length > MAX_PATH)
        raise_syscall(taskData, "Value name too long", ERROR_BAD_LENGTH);

    // How long is the entry?
    lRes = RegQueryValueEx(hkey, valName, 0, NULL, NULL, &valSize);
    // When opening HKEY_PERFORMANCE_DATA we don't get a sensible
    // answer here.
    if (lRes == ERROR_MORE_DATA) valSize = 1024; // Guess
    else if (lRes != ERROR_SUCCESS)
        raise_syscall(taskData, "RegQueryValueEx failed", lRes);
    // Allocate that much store and get the value.  We could
    // try reading directly into ML store to save copying but
    // it hardly seems worthwhile.
    // Note: It seems that valSize can be zero for some items.
    if (valSize == 0) resVal = SAVE(C_string_to_Poly(taskData, "", 0));
    else
    {
        do {
            byte *newAlloc = (byte*)realloc(keyValue, valSize);
            if (newAlloc == 0)
            {
                free(keyValue);
                raise_syscall(taskData, "Insufficient memory", ERROR_NOT_ENOUGH_MEMORY);
            }
            keyValue = newAlloc;
            lRes = RegQueryValueEx(hkey, valName, 0, &dwType, keyValue, &valSize);
            // In the special case of HKEY_PERFORMANCE_DATA we may need to keep
            // growing the buffer.
            if (lRes == ERROR_MORE_DATA) valSize = valSize + 1024;
        } while (lRes == ERROR_MORE_DATA);

        if (lRes != ERROR_SUCCESS)
        {
            free(keyValue);
            raise_syscall(taskData, "RegQueryValue failed", lRes);
        }
        // If we have a string we have to convert this to ANSI/utf-8.
        if (dwType == REG_SZ || dwType == REG_MULTI_SZ || dwType == REG_EXPAND_SZ)
            resVal = SAVE(C_string_to_Poly(taskData, (TCHAR*)keyValue, valSize / sizeof(TCHAR)));
        else resVal = SAVE(C_string_to_Poly(taskData, (char*)keyValue, valSize));
        free(keyValue);
    }

    /* Create a pair containing the type and the value. */
    resType = Make_fixed_precision(taskData, dwType);
    result = alloc_and_save(taskData, 2);
    DEREFHANDLE(result)->Set(0, resType->Word());
    DEREFHANDLE(result)->Set(1, resVal->Word());
    return result;
}

static Handle setRegistryKey(TaskData *taskData, Handle args, HKEY hkey)
{
    TCHAR valName[MAX_PATH];
    LONG lRes;
    PolyWord str = args->WordP()->Get(3);
    POLYUNSIGNED length = Poly_string_to_C(args->WordP()->Get(1), valName, MAX_PATH);
    DWORD dwType = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
    if (length > MAX_PATH)
        raise_syscall(taskData, "Value name too long", ERROR_BAD_LENGTH);

    // The value is binary.  Strings will already have had a null added.
    if (IS_INT(str))
    {
        byte b = (byte)UNTAGGED(str);
        // Single byte value.
        lRes = RegSetValueEx(hkey, valName, 0, dwType, &b, 1);
    }
    else
    {
        PolyStringObject *ps = (PolyStringObject*)str.AsObjPtr();
        lRes = RegSetValueEx(hkey, valName, 0, dwType,
                            (CONST BYTE *)ps->chars, (DWORD)ps->length);
    }

    if (lRes != ERROR_SUCCESS)
        raise_syscall(taskData, "RegSetValue failed", lRes);

    return Make_fixed_precision(taskData, 0);
}

// Enumerate a key or a value.  Returns a string option containing NONE if
// no key/value could be found or SOME s where s is the name of the key/value.
static Handle enumerateRegistry(TaskData *taskData, Handle args, HKEY hkey, BOOL isKey)
{
    DWORD num = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(1));
    LONG lRes;
    TCHAR keyName[MAX_PATH];
    DWORD dwLength = sizeof(keyName)/sizeof(keyName[0]);
    Handle result, resVal;
    if (isKey)
    {
        FILETIME ftMod;
        lRes = RegEnumKeyEx(hkey, num, keyName, &dwLength, NULL, NULL, NULL, &ftMod);
        if (lRes != ERROR_SUCCESS && lRes != ERROR_NO_MORE_ITEMS)
            raise_syscall(taskData, "RegEnumKeyEx failed", lRes);
    }
    else
    {
        lRes = RegEnumValue(hkey, num, keyName, &dwLength, NULL, NULL, NULL, NULL);
        if (lRes != ERROR_SUCCESS && lRes != ERROR_NO_MORE_ITEMS)
            raise_syscall(taskData, "RegEnumValue failed", lRes);
    }
    if (lRes == ERROR_NO_MORE_ITEMS)
        return SAVE(NONE_VALUE); /* NONE. */
    resVal = SAVE(C_string_to_Poly(taskData, keyName));
    result = alloc_and_save(taskData, 1);
    DEREFHANDLE(result)->Set(0, resVal->Word());
    return result;
}

struct _entrypts osSpecificEPT[] =
{
    { "PolyGetOSType",                  (polyRTSFunction)&PolyGetOSType},
    { "PolyOSSpecificGeneral",          (polyRTSFunction)&PolyOSSpecificGeneral},

    { NULL, NULL} // End of list.
};
