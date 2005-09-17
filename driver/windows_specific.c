/*
    Title:      Operating Specific functions: Windows version.

	Copyright (c) 2000 David C. J. Matthews

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
#ifdef USEWINSOCK2
#include <winsock2.h>
#else
#include <winsock.h>
#endif
#include <io.h>
#include <fcntl.h>
#include <errno.h>
#include <lmcons.h>


#include "globals.h"
#include "arb.h"
#include "gc.h"
#include "run_time.h"
#include "io_internal.h"
#include "proper_io.h"
#include "os_specific.h"
#include "sys.h"
#include "processes.h"
#include "Console.h"
#include "mpoly.h"

#define STREAMID(x) (*DEREFSTREAMHANDLE(x))

#define SAVE(x) push_to_save_vec((word)(x))
#define ALLOC(n) alloc_and_save(n)
#define SIZEOF(x) (sizeof(x)/sizeof(word))

#define EMPTYSTRING interface_map[POLY_SYS_nullorzero]

typedef enum
{
	HE_UNUSED,
	HE_REGISTRY,
	HE_PROCESS,
	HE_DDECONVERSATION
} HANDENTRYTYPE;

/* Table of open handles.
   This is modelled after the IO table in basicio.c and performs a
   similar function.  Each resource has an entry in here and there
   is a token which is an ML object.  The token is simply a word
   containing the index in the table.  It is the token itself which
   represents the entry within the ML world.  The token is checked
   against the entry whenever it is used since it is possible for
   the tokens to be persistent although the corresponding entry in
   the table will not make sense in a subsequent session.  This table
   also allows us to garbage-collect entries since if the token becomes
   unreachable we know that there is no longer a reference to the
   entry within ML.
*/
typedef struct {
	word *token; /* pointer into ML heap */
	HANDENTRYTYPE  entryType; /* Type of handle */
	union {
		HKEY	hKey; /* Registry key. */

		struct {
			/* Process and IO channels. */
			HANDLE hProcess, hInput, hOutput;
			word *readToken, *writeToken;
		} process;

		HCONV hcDDEConv; /* DDE Conversation. */
	} entry;
} HANDLETAB, *PHANDLETAB;


static PHANDLETAB handleTable;
static int maxHandleTab;

/* General "close" function which may be called from the
   garbage-collector. */
static void close_handle(PHANDLETAB pTab)
{
	switch (pTab->entryType)
	{
	case HE_REGISTRY:
		RegCloseKey(pTab->entry.hKey);
		break;

	case HE_PROCESS:
		if (pTab->entry.process.hProcess)
			CloseHandle(pTab->entry.process.hProcess);
		if (pTab->entry.process.hInput != INVALID_HANDLE_VALUE)
			CloseHandle(pTab->entry.process.hInput);
		if (pTab->entry.process.hOutput != INVALID_HANDLE_VALUE)
			CloseHandle(pTab->entry.process.hOutput);
		break;

	case HE_DDECONVERSATION:
		CloseDDEConversation(pTab->entry.hcDDEConv);
		break;
	}
	pTab->token = 0;
	pTab->entryType = HE_UNUSED;
}

static PHANDLETAB get_handle(Handle hstream, HANDENTRYTYPE heType)
{
	word *handle_token = DEREFSTREAMHANDLE(hstream);
	int   handle_no    = *handle_token;

    if (handle_no >= maxHandleTab ||
        handleTable[handle_no].token != handle_token ||
		handleTable[handle_no].entryType != heType) 
        return 0;

    return &handleTable[handle_no];
}

static Handle make_handle_entry(void)
{
    int handle_no, have_collected;
    Handle str_token;
    
    have_collected = 0;

    do {
        for(handle_no = 0;
            handle_no < maxHandleTab && handleTable[handle_no].token != 0;
            handle_no++);
            
		/* Check we have enough space. */
		if (handle_no >= maxHandleTab)
		{ /* No space. */
           /* See if we have unreferenced streams. */
			if (! have_collected)
			{
				FullGC();
				have_collected = 1;
			}
			else /* No space - expand vector. */
			{
				int oldMax = maxHandleTab;
				maxHandleTab += maxHandleTab/2;
				handleTable =
					(PHANDLETAB)realloc(handleTable,
									maxHandleTab*sizeof(HANDLETAB));
				/* Clear the new space. */
				memset(handleTable+oldMax, 0,
						(maxHandleTab-oldMax)*sizeof(HANDLETAB));
			}
		}
	} while (handle_no >= maxHandleTab);
     
    str_token = ALLOC(1 | OBJ_BYTE_BIT);
    STREAMID(str_token) = handle_no;

	/* Clear the entry then set the token. */
	memset(&handleTable[handle_no], 0, sizeof(HANDLETAB));
    handleTable[handle_no].token = DEREFSTREAMHANDLE(str_token);
    return str_token;
}

static Handle execute(Handle pname);
static Handle simpleExecute(Handle args);
static Handle openProcessHandle(Handle args, BOOL fIsRead, BOOL fIsText);
static Handle openRegistryKey(Handle args, HKEY hkParent);
static Handle createRegistryKey(Handle args, HKEY hkParent);
static Handle queryRegistryKey(Handle args, HKEY hkParent);
static Handle setRegistryKey(Handle args, HKEY hkParent);
static Handle deleteRegistryKey(Handle args, HKEY hkParent);
static Handle deleteRegistryValue(Handle args, HKEY hkParent);
static Handle enumerateRegistry(Handle args, HKEY hkey, BOOL isKey);
static void deliverResult(Handle res);
static LRESULT CALLBACK windowProcedure(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

static word *myCallback;
static int processStackPtr = 0;
static process_base *saved_process[100];

// Vector of constants returned by call1006
static int winConstVec[] =
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


Handle OS_spec_dispatch_c(Handle args, Handle code)
{
	int c = get_C_long(DEREFWORDHANDLE(code));
	switch (c)
	{
	case 0: /* Return our OS type.  Not in any structure. */
		return Make_arbitrary_precision(1); /* 1 for Windows. */

		/* Windows-specific functions. */
	case 1000: /* execute */
		return execute(args);

	case 1001: /* Get input stream as text. */
		return openProcessHandle(args, TRUE, TRUE);

	case 1002: /* Get output stream as text. */
		return openProcessHandle(args, FALSE, TRUE);

	case 1003: /* Get input stream as binary. */
		return openProcessHandle(args, TRUE, FALSE);

	case 1004: /* Get output stream as binary. */
		return openProcessHandle(args, FALSE, FALSE);

	case 1005: /* Get result of process. */
		{
			PHANDLETAB hnd = get_handle(args, HE_PROCESS);
			DWORD dwResult;
			if (hnd == 0)
				raise_syscall("Process is closed", EINVAL);
			// Close the streams. Either of them may have been
			// passed to the stream package.
			if (hnd->entry.process.hInput != INVALID_HANDLE_VALUE)
				CloseHandle(hnd->entry.process.hInput);
			hnd->entry.process.hInput = INVALID_HANDLE_VALUE;
			if (hnd->entry.process.readToken)
			{
				PIOSTRUCT strm =
					get_stream((Handle)&hnd->entry.process.readToken);
				if (strm != NULL) close_stream(strm);
			}
			hnd->entry.process.readToken = 0;
			if (hnd->entry.process.hOutput != INVALID_HANDLE_VALUE)
				CloseHandle(hnd->entry.process.hOutput);
			hnd->entry.process.hOutput = INVALID_HANDLE_VALUE;
			if (hnd->entry.process.writeToken)
			{
				PIOSTRUCT strm =
					get_stream((Handle)&hnd->entry.process.writeToken);
				if (strm != NULL) close_stream(strm);
			}
			hnd->entry.process.writeToken = 0;
			// See if it's finished.
			if (GetExitCodeProcess(hnd->entry.process.hProcess,
					&dwResult) == 0)
				raise_syscall("GetExitCodeProcess failed",
						-(int)GetLastError());
			if (dwResult == STILL_ACTIVE)
				/* Run some other ML processes and come back in at
				   the top some time later*/
				block_and_restart(-1, 0, POLY_SYS_os_specific);
			/* Finished - return the result. */
			/* Note: we haven't closed the handle because we might want to ask
			   for the result again.  We only close it when we've garbage-collected
			   the token.  Doing this runs the risk of running out of handles.
			   Maybe change it and remember the result in ML. */
			return Make_unsigned(dwResult);
		}

	case 1006: /* Return a constant. */
		{
			int i = get_C_long(DEREFWORDHANDLE(args));
			if (i < 0 || i >= sizeof(winConstVec)/sizeof(winConstVec[0]))
				raise_syscall("Invalid index", 0);
			return Make_unsigned(winConstVec[i]);
		}

		/* Registry functions. */
	case 1007: // Open a key within one of the roots.
		{
			unsigned keyIndex = get_C_ulong(DEREFHANDLE(args)[0]);
			// This should only ever happen as a result of a fault in
			// the Windows structure.
			if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
				raise_syscall("Invalid index", 0);
			return openRegistryKey(args, hkPredefinedKeyTab[keyIndex]);
		}

	case 1008: // Open a subkey of an opened key.
		{
			PHANDLETAB hnd =
				get_handle((Handle)&DEREFHANDLE(args)[0], HE_REGISTRY);
			if (hnd == 0)
				raise_syscall("Handle is closed", -ERROR_INVALID_HANDLE);
			return openRegistryKey(args, hnd->entry.hKey);
		}

	case 1009: // Create a subkey within one of the roots.
		{
			unsigned keyIndex = get_C_ulong(DEREFHANDLE(args)[0]);
			// This should only ever happen as a result of a fault in
			// the Windows structure.
			if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
				raise_syscall("Invalid index", 0);
			return createRegistryKey(args, hkPredefinedKeyTab[keyIndex]);
		}

	case 1010: // Create a subkey within an opened key.
		{
			PHANDLETAB hnd =
				get_handle((Handle)&DEREFHANDLE(args)[0], HE_REGISTRY);
			if (hnd == 0)
				raise_syscall("Handle is closed", -ERROR_INVALID_HANDLE);
			return createRegistryKey(args, hnd->entry.hKey);
		}

	case 1011: // Close a registry handle.
		{
			PHANDLETAB hnd = get_handle(args, HE_REGISTRY);
			if (hnd != 0) close_handle(hnd);
			return Make_arbitrary_precision(0);
		}

	case 1012: // Get a value
		{
			unsigned keyIndex = get_C_ulong(DEREFHANDLE(args)[0]);
			// This should only ever happen as a result of a fault in
			// the Windows structure.
			if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
				raise_syscall("Invalid index", 0);
			return queryRegistryKey(args, hkPredefinedKeyTab[keyIndex]);
		}

	case 1013: // Get a value
		{
			PHANDLETAB hnd =
				get_handle((Handle)&DEREFHANDLE(args)[0], HE_REGISTRY);
			if (hnd == 0)
				raise_syscall("Handle is closed", -ERROR_INVALID_HANDLE);
			return queryRegistryKey(args, hnd->entry.hKey);
		}

	case 1014: // Delete a subkey
		{
			unsigned keyIndex = get_C_ulong(DEREFHANDLE(args)[0]);
			// This should only ever happen as a result of a fault in
			// the Windows structure.
			if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
				raise_syscall("Invalid index", 0);
			return deleteRegistryKey(args, hkPredefinedKeyTab[keyIndex]);
		}

	case 1015: // Delete a subkey
		{
			PHANDLETAB hnd =
				get_handle((Handle)&DEREFHANDLE(args)[0], HE_REGISTRY);
			if (hnd == 0)
				raise_syscall("Handle is closed", -ERROR_INVALID_HANDLE);
			return deleteRegistryKey(args, hnd->entry.hKey);
		}

	case 1016: // Set a value
		{
			unsigned keyIndex = get_C_ulong(DEREFHANDLE(args)[0]);
			// This should only ever happen as a result of a fault in
			// the Windows structure.
			if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
				raise_syscall("Invalid index", 0);
			return setRegistryKey(args, hkPredefinedKeyTab[keyIndex]);
		}

	case 1017: // Set a value
		{
			PHANDLETAB hnd =
				get_handle((Handle)&DEREFHANDLE(args)[0], HE_REGISTRY);
			if (hnd == 0)
				raise_syscall("Handle is closed", -ERROR_INVALID_HANDLE);
			return setRegistryKey(args, hnd->entry.hKey);
		}

	case 1018: // Enumerate a key in the predefined keys
		{
			unsigned keyIndex = get_C_ulong(DEREFHANDLE(args)[0]);
			if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
				raise_syscall("Invalid index", 0);
			return enumerateRegistry(args, hkPredefinedKeyTab[keyIndex], TRUE);
		}

	case 1019: // Enumerate a key in an opened key
		{
			PHANDLETAB hnd =
				get_handle((Handle)&DEREFHANDLE(args)[0], HE_REGISTRY);
			if (hnd == 0)
				raise_syscall("Handle is closed", -ERROR_INVALID_HANDLE);
			return enumerateRegistry(args, hnd->entry.hKey, TRUE);
		}

	case 1020: // Enumerate a value in the predefined keys
		{
			unsigned keyIndex = get_C_ulong(DEREFHANDLE(args)[0]);
			if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
				raise_syscall("Invalid index", 0);
			return enumerateRegistry(args, hkPredefinedKeyTab[keyIndex], FALSE);
		}

	case 1021: // Enumerate a value in an opened key
		{
			PHANDLETAB hnd =
				get_handle((Handle)&DEREFHANDLE(args)[0], HE_REGISTRY);
			if (hnd == 0)
				raise_syscall("Handle is closed", -ERROR_INVALID_HANDLE);
			return enumerateRegistry(args, hnd->entry.hKey, FALSE);
		}

	case 1022: // Delete a value
		{
			unsigned keyIndex = get_C_ulong(DEREFHANDLE(args)[0]);
			// This should only ever happen as a result of a fault in
			// the Windows structure.
			if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
				raise_syscall("Invalid index", 0);
			return deleteRegistryValue(args, hkPredefinedKeyTab[keyIndex]);
		}

	case 1023: // Delete a value
		{
			PHANDLETAB hnd =
				get_handle((Handle)&DEREFHANDLE(args)[0], HE_REGISTRY);
			if (hnd == 0)
				raise_syscall("Handle is closed", -ERROR_INVALID_HANDLE);
			return deleteRegistryValue(args, hnd->entry.hKey);
		}


	case 1030: // Convert UTC time values to local time.
		{
			FILETIME ftUTC, ftLocal;
			/* Get the file time. */
			get_C_pair(DEREFWORDHANDLE(args),
					&ftUTC.dwHighDateTime, &ftUTC.dwLowDateTime);
			if (! FileTimeToLocalFileTime(&ftUTC, &ftLocal))
				raise_syscall("FileTimeToLocalFileTime failed",
						-(int)GetLastError());
			return Make_arb_from_pair(ftLocal.dwHighDateTime,
						ftLocal.dwLowDateTime);
		}

	case 1031: // Convert local time values to UTC.
		{
			FILETIME ftUTC, ftLocal;
			/* Get the file time. */
			get_C_pair(DEREFWORDHANDLE(args),
					&ftLocal.dwHighDateTime, &ftLocal.dwLowDateTime);
			if (! LocalFileTimeToFileTime(&ftLocal, &ftUTC))
				raise_syscall("LocalFileTimeToFileTime failed",
						-(int)GetLastError());
			return Make_arb_from_pair(ftUTC.dwHighDateTime,
						ftUTC.dwLowDateTime);
		}

	case 1032: // Get volume information.
		{
			char rootName[MAX_PATH], volName[MAX_PATH], sysName[MAX_PATH];
			DWORD dwVolSerial, dwMaxComponentLen, dwFlags;
			Handle volHandle, sysHandle, serialHandle, maxCompHandle;
			Handle resultHandle;
			int length = Poly_string_to_C(DEREFSTRINGHANDLE(args),
							rootName, MAX_PATH);
			if (length > MAX_PATH)
				raise_syscall("Root name too long", ENAMETOOLONG);
			
			if (!GetVolumeInformation(rootName, volName, MAX_PATH,
					&dwVolSerial, &dwMaxComponentLen, &dwFlags,
					sysName, MAX_PATH))
				raise_syscall("GetVolumeInformation failed",
					-(int)GetLastError());
			volHandle = SAVE(C_string_to_Poly(volName));
			sysHandle = SAVE(C_string_to_Poly(sysName));
			serialHandle = Make_unsigned(dwVolSerial);
			maxCompHandle = Make_unsigned(dwMaxComponentLen);
			resultHandle = ALLOC(4);
			DEREFHANDLE(resultHandle)[0] = DEREFWORDHANDLE(volHandle);
			DEREFHANDLE(resultHandle)[1] = DEREFWORDHANDLE(sysHandle);
			DEREFHANDLE(resultHandle)[2] = DEREFWORDHANDLE(serialHandle);
			DEREFHANDLE(resultHandle)[3] = DEREFWORDHANDLE(maxCompHandle);
			return resultHandle;
		}

	case 1033:
		{
			char fileName[MAX_PATH], execName[MAX_PATH];
			int length = Poly_string_to_C(DEREFSTRINGHANDLE(args),
							fileName, MAX_PATH);
			HINSTANCE hInst;
			if (length > MAX_PATH)
				raise_syscall("File name too long", ENAMETOOLONG);
			hInst = FindExecutable(fileName, NULL, execName);
			if ((unsigned)hInst <= 32)
			{
				raise_syscall("FindExecutable failed", -(int)hInst);
			}
			return SAVE(C_string_to_Poly(execName));
		}

	case 1034: // Open a document
		{
			char *docName =
				Poly_string_to_C_alloc(DEREFSTRINGHANDLE(args));
			HINSTANCE hInst =
				ShellExecute(hMainWindow, "open", docName,
						NULL, NULL, SW_SHOWNORMAL);
			free(docName);
			if ((unsigned)hInst >= 32)
				return Make_arbitrary_precision(0);
			switch ((unsigned)hInst)
			{
			case SE_ERR_FNF:
			case SE_ERR_PNF:
			case SE_ERR_ACCESSDENIED:
				/* These correspond to the Windows error codes. */
				raise_syscall("ShellExecute failed", -(int)hInst);
			default:
				raise_syscall("ShellExecute failed", 0);
			}
		}

	case 1035: // Launch an application.
		{
			char *commandName =
				Poly_string_to_C_alloc((pstring)DEREFWORDHANDLE(args)[0]);
			char *arguments =
				Poly_string_to_C_alloc((pstring)DEREFWORDHANDLE(args)[1]);
			HINSTANCE hInst =
				ShellExecute(hMainWindow, "open", commandName,
						arguments, NULL, SW_SHOWNORMAL);
			free(commandName);
			free(arguments);
			if ((unsigned)hInst >= 32)
				return Make_arbitrary_precision(0);
			switch ((unsigned)hInst)
			{
			case SE_ERR_FNF:
			case SE_ERR_PNF:
			case SE_ERR_ACCESSDENIED:
				/* These correspond to the Windows error codes. */
				raise_syscall("ShellExecute failed", -(int)hInst);
			default:
				raise_syscall("ShellExecute failed", 0);
			}
		}

	case 1036: // Does the process have its own console?
		return Make_arbitrary_precision(hMainWindow != NULL ? 1: 0);

	case 1037: // Simple execute.
		return simpleExecute(args);


		// DDE
	case 1038: // Start DDE dialogue.
		{
			Handle handToken;
			PHANDLETAB pTab;
			HCONV hcDDEConv;
			char *serviceName =
				Poly_string_to_C_alloc((pstring)DEREFWORDHANDLE(args)[0]);
			char *topicName =
				Poly_string_to_C_alloc((pstring)DEREFWORDHANDLE(args)[1]);
			/* Send a request to the main thread to do the work. */
			hcDDEConv = StartDDEConversation(serviceName, topicName);
			free(serviceName); free(topicName);
			if (hcDDEConv == 0) raise_syscall("DdeConnect failed", 0);
			// Create an entry to return the conversation.
			handToken = make_handle_entry();
			pTab = &handleTable[STREAMID(handToken)];
			pTab->entryType = HE_DDECONVERSATION;
			pTab->entry.hcDDEConv = hcDDEConv;
			return handToken;
		}

	case 1039: // Send DDE execute request.
		{
			PHANDLETAB hnd =
				get_handle((Handle)&DEREFHANDLE(args)[0], HE_DDECONVERSATION);
			int res;
			char *command;
			if (hnd == NULL)
			{
				raise_syscall("DDE Conversation is closed", 0);
			}
			command = Poly_string_to_C_alloc((pstring)DEREFWORDHANDLE(args)[1]);
			/* Send a request to the main thread to do the work. */
			res = ExecuteDDE(command, hnd->entry.hcDDEConv);
			free(command);
			if (res == -1) raise_syscall("DdeClientTransaction failed", 0);
			else return Make_arbitrary_precision(res);
		}

	case 1040: // Close a DDE conversation.
		{
			PHANDLETAB hnd = get_handle(args, HE_DDECONVERSATION);
			if (hnd != 0) close_handle(hnd);
			return Make_arbitrary_precision(0);
		}


		// Configuration functions.
	case 1050: // Get version data
		{
			OSVERSIONINFO osver;
			Handle resVal, major, minor, build, platform, version;
			osver.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
			if (! GetVersionEx(&osver))
				raise_syscall("GetVersionEx failed", -(int)GetLastError());
			major = Make_unsigned(osver.dwMajorVersion);
			minor = Make_unsigned(osver.dwMinorVersion);
			build = Make_unsigned(osver.dwBuildNumber);
			platform = Make_unsigned(osver.dwPlatformId);
			version = SAVE(C_string_to_Poly(osver.szCSDVersion));
			resVal = ALLOC(5);
			DEREFHANDLE(resVal)[0] = DEREFWORDHANDLE(major);
			DEREFHANDLE(resVal)[1] = DEREFWORDHANDLE(minor);
			DEREFHANDLE(resVal)[2] = DEREFWORDHANDLE(build);
			DEREFHANDLE(resVal)[3] = DEREFWORDHANDLE(platform);
			DEREFHANDLE(resVal)[4] = DEREFWORDHANDLE(version);
			return resVal;
		}

	case 1051: // Get windows directory
		{
			char path[MAX_PATH+1];
			if (GetWindowsDirectory(path, sizeof(path)/sizeof(char)) == 0)
				raise_syscall("GetWindowsDirectory failed", -(int)GetLastError());
			return SAVE(C_string_to_Poly(path));
		}

	case 1052: // Get system directory
		{
			char path[MAX_PATH+1];
			if (GetSystemDirectory(path, sizeof(path)/sizeof(char)) == 0)
				raise_syscall("GetSystemDirectory failed", -(int)GetLastError());
			return SAVE(C_string_to_Poly(path));
		}

	case 1053: // Get computer name
		{
			char name[MAX_COMPUTERNAME_LENGTH +1];
			DWORD dwSize = MAX_COMPUTERNAME_LENGTH +1;
			if (GetComputerName(name, &dwSize) == 0)
				raise_syscall("GetComputerName failed", -(int)GetLastError());
			return SAVE(C_string_to_Poly(name));
		}

	case 1054: // Get user name
		{
			char name[UNLEN +1];
			DWORD dwSize = UNLEN +1;
			if (GetUserName(name, &dwSize) == 0)
				raise_syscall("GetUserName failed", -(int)GetLastError());
			return SAVE(C_string_to_Poly(name));
		}

	case 1100: // Get the error result from the last call.
			   // This is saved when we make a call to a foreign function.
		{
			return(SAVE(DEREFPROCHANDLE(processes)->lastErrcode));
		}

	case 1101: // Wait for a message.
		{
			HWND hwnd = (HWND)get_C_long(DEREFHANDLE(args)[0]); /* Handles are treated as SIGNED. */
			UINT wMsgFilterMin = get_C_ulong(DEREFHANDLE(args)[1]);
			UINT wMsgFilterMax = get_C_ulong(DEREFHANDLE(args)[2]);
			MSG msg;
			if (PeekMessage(&msg, hwnd, wMsgFilterMin, wMsgFilterMax, PM_NOREMOVE))
			{
				return Make_arbitrary_precision(0);
			}
			// Run another ML process until a message arrives.
			block_and_restart(-1, 0, POLY_SYS_os_specific);
		}

	case 1102: // Return the address of the window callback function.
		return Make_arbitrary_precision((int)windowProcedure);

	case 1103: // Return the application instance.
		return Make_arbitrary_precision((int)hApplicationInstance);

	case 1104: // Return the main window handle
		return Make_arbitrary_precision((int)hMainWindow);

	case 1105: // Set the callback function
		myCallback = DEREFWORDHANDLE(args);
		return Make_arbitrary_precision(0);

	case 1106: // Deliver the result - does not return.
		deliverResult(args);
		crash("Result delivery returned");

	default:
		raise_syscall("Unimplemented function", 0);
	}
}

/*
The Windows version of this is more complicated than the Unix version because
we can't manipulate the pipe handles in the child process.  Everything has to be
set up in the parent.  As with Unix we create two pipes and pass one end of each
pipe to the child.  The end we pass to the child is "inheritable" (i.e. duplicated
in the child as with Unix file descriptors) while the ends we keep in the parent
are non-inheritable (i.e. not duplicated in the child). 
DCJM: December 1999.  
*/
static Handle execute(Handle args)
{
	char *commandName = 0, *arguments = 0;
	LPTSTR lpszError = "";
	HANDLE hWriteToChild = INVALID_HANDLE_VALUE,
		   hReadFromParent = INVALID_HANDLE_VALUE,
		   hWriteToParent = INVALID_HANDLE_VALUE,
		   hReadFromChild = INVALID_HANDLE_VALUE;
	HANDLE hTemp;
	STARTUPINFO	startupInfo;
	PROCESS_INFORMATION processInfo;
	Handle handToken = make_handle_entry();
	PHANDLETAB pTab = &handleTable[STREAMID(handToken)];

	commandName = Poly_string_to_C_alloc((pstring)DEREFWORDHANDLE(args)[0]);
	arguments = Poly_string_to_C_alloc((pstring)DEREFWORDHANDLE(args)[1]);

	// Create pipes for connection. Setting the security argument to NULL creates
	// the pipe handles as non-inheritable.  We have to make sure that the
	// child process does not inherit handles for the parent end of the
	// connection otherwise the pipe will remain open after the parent has
	// closed its end, causing the child process to sit around even after
	// the parent process has gone away.
    if (!CreatePipe(&hReadFromParent, &hWriteToChild, NULL, 0)) {
		lpszError = "Could not create pipe";
		goto error;
    }
    if (!CreatePipe(&hReadFromChild, &hWriteToParent, NULL, 0)) {
		lpszError = "Could not create pipe";
		goto error;
    }
	// Convert the handles we want to pass to the child into inheritable
	// handles by duplicating and replacing them with the duplicates.
	if (! DuplicateHandle(GetCurrentProcess(), hWriteToParent, GetCurrentProcess(),
                          &hTemp, 0, TRUE, // inheritable
                          DUPLICATE_SAME_ACCESS )) {
		lpszError = "Could not create pipe";
		goto error;
	}
	CloseHandle(hWriteToParent);
	hWriteToParent = hTemp;
	if (! DuplicateHandle(GetCurrentProcess(), hReadFromParent, GetCurrentProcess(),
                          &hTemp, 0, TRUE, // inheritable
                          DUPLICATE_SAME_ACCESS )) {
		lpszError = "Could not create pipe";
		goto error;
	}
	CloseHandle(hReadFromParent);
	hReadFromParent = hTemp;

	// Create a STARTUPINFO structure in which to pass the pipes as stdin
	// and stdout to the new process.
	memset(&startupInfo, 0, sizeof(startupInfo));
	startupInfo.cb = sizeof(startupInfo);
	startupInfo.dwFlags = STARTF_USESTDHANDLES;
	startupInfo.hStdInput = hReadFromParent;
	startupInfo.hStdOutput = hWriteToParent;
	// What should we do about the stderr?  For the moment, inherit the original.
	startupInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);

	// Try starting the process first using the given name.
	if (!CreateProcess(commandName,
			arguments, // Command line
			NULL, NULL, TRUE, // Security attributes. Inherit handles
			CREATE_NO_WINDOW, // creation flags
			NULL, NULL, // Inherit our environment and directory
			&startupInfo,
			&processInfo)) {
		lpszError = "Could not create process";
		goto error;
	}

	free(commandName);
	free(arguments);
	/* Close thread handle since we don't need it. */
	CloseHandle(processInfo.hThread);
	/* Close the sides of the pipes we don't use in the parent. */
	CloseHandle(hReadFromParent);
	CloseHandle(hWriteToParent);
	pTab->entryType = HE_PROCESS;
	pTab->entry.process.hProcess = processInfo.hProcess;
	pTab->entry.process.hInput = hReadFromChild;
	pTab->entry.process.hOutput = hWriteToChild;
	pTab->entry.process.readToken = 0;
	pTab->entry.process.writeToken = 0;

 	return(handToken);

error:
	{
		int err = GetLastError();
		free(commandName);
		free(arguments);
  		// Close all the pipe handles.
		if (hWriteToChild != INVALID_HANDLE_VALUE) CloseHandle(hWriteToChild);
		if (hReadFromParent != INVALID_HANDLE_VALUE) CloseHandle(hReadFromParent);
		if (hWriteToParent != INVALID_HANDLE_VALUE) CloseHandle(hWriteToParent);
		if (hReadFromChild != INVALID_HANDLE_VALUE) CloseHandle(hReadFromChild);
		raise_syscall(lpszError, -err);
		return NULL; // Never reached.
	}
}

static Handle simpleExecute(Handle args)
{
	char *commandName = 0, *arguments = 0;
	LPTSTR lpszError = "";
	HANDLE hNull = INVALID_HANDLE_VALUE;
	STARTUPINFO	startupInfo;
	PROCESS_INFORMATION processInfo;
	Handle handToken;
	PHANDLETAB pTab;

	commandName = Poly_string_to_C_alloc((pstring)DEREFWORDHANDLE(args)[0]);
	arguments = Poly_string_to_C_alloc((pstring)DEREFWORDHANDLE(args)[1]);

	// Open a handle to NUL for input and output.
	hNull = CreateFile("NUL", GENERIC_READ|GENERIC_WRITE,
				FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
				FILE_ATTRIBUTE_NORMAL, NULL);

	// Create a STARTUPINFO structure in which to pass hNULL as stdin
	// and stdout to the new process.
	memset(&startupInfo, 0, sizeof(startupInfo));
	startupInfo.cb = sizeof(startupInfo);
	startupInfo.dwFlags = STARTF_USESTDHANDLES;
	startupInfo.hStdInput = hNull;
	startupInfo.hStdOutput = hNull;
	startupInfo.hStdError = hNull;

	// Try starting the process first using the given name.
	if (!CreateProcess(commandName,
			arguments, // Command line
			NULL, NULL, TRUE, // Security attributes. Inherit handles
			CREATE_NO_WINDOW, // creation flags
			NULL, NULL, // Inherit our environment and directory
			&startupInfo,
			&processInfo)) {
		int nErr = GetLastError();
		// Clean up
		free(commandName);
		free(arguments);
		CloseHandle(hNull);
		raise_syscall("CreateProcess failed", -nErr);
	}

	free(commandName);
	free(arguments);
	/* Close thread handle since we don't need it. */
	CloseHandle(processInfo.hThread);
	CloseHandle(hNull); // We no longer need this

	handToken = make_handle_entry();
	pTab = &handleTable[STREAMID(handToken)];
	pTab->entryType = HE_PROCESS;
	pTab->entry.process.hProcess = processInfo.hProcess;
	// We only use the process handle entry.
	pTab->entry.process.hInput = INVALID_HANDLE_VALUE;
	pTab->entry.process.hOutput = INVALID_HANDLE_VALUE;
	pTab->entry.process.readToken = 0;
	pTab->entry.process.writeToken = 0;

 	return(handToken);
}

/* Return a stream, either text or binary, connected to an open process. */
static Handle openProcessHandle(Handle args, BOOL fIsRead, BOOL fIsText)
{
	PHANDLETAB hnd = get_handle(args, HE_PROCESS);
	Handle str_token;
	PIOSTRUCT strm;
	HANDLE hStream;
	int mode = 0, ioBits = 0;
	if (hnd == 0)
		raise_syscall("Process is closed", EINVAL);

	if (fIsRead) hStream = hnd->entry.process.hInput;
	else hStream = hnd->entry.process.hOutput;
	/* The definition is unclear about whether it's possible
	   to get the same handle twice.  I've assumed not since
	   we could get problems with closing the same handle twice. */
	if (hStream == INVALID_HANDLE_VALUE)
		raise_syscall("Process is closed", EBADF);

	if (fIsRead) { mode = _O_RDONLY; ioBits = IO_BIT_READ; }
	else { mode = 0; ioBits = IO_BIT_WRITE; }
	if (fIsText) mode |= _O_TEXT; else mode |= _O_BINARY;

	str_token = make_stream_entry();
	strm = &basic_io_vector[STREAMID(str_token)];
	strm->device.ioDesc = _open_osfhandle ((long)hStream, mode);
	if (strm->device.ioDesc == -1)
		raise_syscall("_open_osfhandle failed", errno);
	strm->ioBits = ioBits | IO_BIT_OPEN | IO_BIT_PIPE;
	/* The responsibility for closing the handle is passed to
	   the stream package.  We need to retain a pointer to the
	   stream entry so that we can close the stream in "reap". */
	if (fIsRead)
	{
		hnd->entry.process.hInput = INVALID_HANDLE_VALUE;
		hnd->entry.process.readToken = strm->token;
	}
	else
	{
		hnd->entry.process.hOutput = INVALID_HANDLE_VALUE;
		hnd->entry.process.writeToken = strm->token;
	}

	return str_token;
}

// Open a registry key and make an entry in the table for it.
static Handle openRegistryKey(Handle args, HKEY hkParent)
{
	TCHAR keyName[MAX_PATH];
	LONG lRes;
	Handle result;
	PHANDLETAB pTab;
	HKEY hkey;
	REGSAM sam = get_C_ulong(DEREFHANDLE(args)[2]);
	int length = Poly_string_to_C((pstring)DEREFHANDLE(args)[1],
					keyName, MAX_PATH);
	if (length > MAX_PATH)
		raise_syscall("Key name too long", ENAMETOOLONG);

	// Try opening the key.
	lRes = RegOpenKeyEx(hkParent, keyName, 0, sam, &hkey);
	if (lRes != ERROR_SUCCESS)
		raise_syscall("RegOpenKeyEx failed", -lRes);

	// Make an entry in the table.
	result = make_handle_entry();
	pTab = &handleTable[STREAMID(result)];
	pTab->entryType = HE_REGISTRY;
	pTab->entry.hKey = hkey;
	return result;
}

// Create a registry key and make an entry in the table for it.
static Handle createRegistryKey(Handle args, HKEY hkParent)
{
	TCHAR keyName[MAX_PATH];
	LONG lRes;
	Handle keyResult, dispRes, pair;
	PHANDLETAB pTab;
	HKEY hkey;
	DWORD dwDisp;
	REGSAM sam = get_C_ulong(DEREFHANDLE(args)[3]);
	int opt = get_C_ulong(DEREFHANDLE(args)[2]);
	int length = Poly_string_to_C((pstring)DEREFHANDLE(args)[1],
						keyName, MAX_PATH);
	if (length > MAX_PATH)
		raise_syscall("Key name too long", ENAMETOOLONG);

	// Try opening the key.
	lRes = RegCreateKeyEx(hkParent, keyName, 0, NULL,
				opt ? REG_OPTION_NON_VOLATILE : REG_OPTION_VOLATILE,
				sam, NULL, &hkey, &dwDisp);
	if (lRes != ERROR_SUCCESS)
		raise_syscall("RegCreateKeyEx failed", -lRes);

	// Make an entry in the table.
	keyResult = make_handle_entry();
	pTab = &handleTable[STREAMID(keyResult)];
	pTab->entryType = HE_REGISTRY;
	pTab->entry.hKey = hkey;
	// Record whether this was new or old.
	dispRes = Make_unsigned(dwDisp == REG_CREATED_NEW_KEY ? 0: 1);
	/* Return a pair of the disposition and the token. */
	pair = ALLOC(2);
	DEREFHANDLE(pair)[0] = DEREFWORDHANDLE(dispRes);
	DEREFHANDLE(pair)[1] = DEREFWORDHANDLE(keyResult);
	return pair;
}

// Delete a key.  Note that in Windows NT (but not 95) this will fail if
// the key has subkeys.
static Handle deleteRegistryKey(Handle args, HKEY hkParent)
{
	TCHAR keyName[MAX_PATH];
	LONG lRes;
	int length = Poly_string_to_C((pstring)DEREFHANDLE(args)[1],
					keyName, MAX_PATH);
	if (length > MAX_PATH)
		raise_syscall("Key name too long", ENAMETOOLONG);

	// Try deleting the key.  
	lRes = RegDeleteKey(hkParent, keyName);
	if (lRes != ERROR_SUCCESS)
		/* Return the error. */
		raise_syscall("RegDeleteKey failed", -lRes);
	return Make_arbitrary_precision(0);
}

static Handle deleteRegistryValue(Handle args, HKEY hkParent)
{
	TCHAR keyName[MAX_PATH];
	LONG lRes;
	int length = Poly_string_to_C((pstring)DEREFHANDLE(args)[1],
					keyName, MAX_PATH);
	if (length > MAX_PATH)
		raise_syscall("Key name too long", ENAMETOOLONG);

	// Try deleting the value.
	lRes = RegDeleteValue(hkParent, keyName);
	if (lRes != ERROR_SUCCESS)
		/* Return the original error. */
		raise_syscall("RegDeleteValue failed", -lRes);
	return Make_arbitrary_precision(0);
}

static Handle queryRegistryKey(Handle args, HKEY hkey)
{
	TCHAR valName[MAX_PATH];
	char *keyValue = 0;
	LONG lRes, valSize;
	Handle result, resVal, resType;
	DWORD dwType;
	int length = Poly_string_to_C((pstring)DEREFHANDLE(args)[1],
					valName, MAX_PATH);
	if (length > MAX_PATH)
		raise_syscall("Value name too long", ENAMETOOLONG);

	// How long is the entry?
	lRes = RegQueryValueEx(hkey, valName, 0, NULL, NULL, &valSize);
	// When opening HKEY_PERFORMANCE_DATA we don't get a sensible
	// answer here.
	if (lRes == ERROR_MORE_DATA) valSize = 1024; // Guess
 	else if (lRes != ERROR_SUCCESS)
		raise_syscall("RegQueryValueEx failed", -lRes);
	// Allocate that much store and get the value.  We could
	// try reading directly into ML store to save copying but
	// it hardly seems worthwhile.
	// Note: It seems that valSize can be zero for some items.
	if (valSize == 0) resVal = SAVE(Buffer_to_Poly("", 0));
	else
	{
		do {
			keyValue = realloc(keyValue, valSize);
			lRes = RegQueryValueEx(hkey, valName, 0, &dwType, keyValue, &valSize);
			// In the special case of HKEY_PERFORMANCE_DATA we may need to keep
			// growing the buffer.
			if (lRes == ERROR_MORE_DATA) valSize = valSize + 1024;
		} while (lRes == ERROR_MORE_DATA);

 		if (lRes != ERROR_SUCCESS)
		{
			free(keyValue);
			raise_syscall("RegQueryValue failed", -lRes);
		}
		resVal = SAVE(Buffer_to_Poly(keyValue, valSize));
		free(keyValue);
	}

	/* Create a pair containing the type and the value. */
	resType = Make_arbitrary_precision(dwType);
	result = ALLOC(2);
	DEREFHANDLE(result)[0] = DEREFWORDHANDLE(resType);
	DEREFHANDLE(result)[1] = DEREFWORDHANDLE(resVal);
	return result;
}

static Handle setRegistryKey(Handle args, HKEY hkey)
{
	TCHAR valName[MAX_PATH];
	LONG lRes;
	pstring ps = (pstring)DEREFHANDLE(args)[3];
	int length = Poly_string_to_C((pstring)DEREFHANDLE(args)[1],
					valName, MAX_PATH);
	DWORD dwType = get_C_ulong(DEREFHANDLE(args)[2]);
	if (length > MAX_PATH)
		raise_syscall("Value name too long", ENAMETOOLONG);

	// The value is binary.  Strings will already have had a null added.
	if (IS_INT(ps))
	{
		// Single byte value.
		lRes = RegSetValueEx(hkey, valName, 0, dwType, (CONST BYTE *)&ps, 1);
	}
	else
	{
		lRes = RegSetValueEx(hkey, valName, 0, dwType,
							(CONST BYTE *)ps->chars, ps->length);
	}

 	if (lRes != ERROR_SUCCESS)
		raise_syscall("RegSetValue failed", -lRes);

	return Make_arbitrary_precision(0);
}

// Enumerate a key or a value.  Returns a string option containing NONE if
// no key/value could be found or SOME s where s is the name of the key/value.
static Handle enumerateRegistry(Handle args, HKEY hkey, BOOL isKey)
{
	DWORD num = get_C_ulong(DEREFHANDLE(args)[1]);
	LONG lRes;
	TCHAR keyName[MAX_PATH];
	DWORD dwLength = sizeof(keyName)/sizeof(keyName[0]);
	Handle result, resVal;
	if (isKey)
	{
		FILETIME ftMod;
		lRes = RegEnumKeyEx(hkey, num, keyName, &dwLength, NULL, NULL, NULL, &ftMod);
		if (lRes != ERROR_SUCCESS && lRes != ERROR_NO_MORE_ITEMS)
			raise_syscall("RegEnumKeyEx failed", -lRes);
	}
	else
	{
		lRes = RegEnumValue(hkey, num, keyName, &dwLength, NULL, NULL, NULL, NULL);
		if (lRes != ERROR_SUCCESS && lRes != ERROR_NO_MORE_ITEMS)
			raise_syscall("RegEnumValue failed", -lRes);
	}
	if (lRes == ERROR_NO_MORE_ITEMS)
		return SAVE(NONE_VALUE); /* NONE. */
	resVal = SAVE(C_string_to_Poly(keyName));
	result = ALLOC(1);
	DEREFHANDLE(result)[0] = DEREFWORDHANDLE(resVal);
	return result;
}

static JMP_BUF returnBuf;

static void deliverResult(Handle res)
{
	LONGJMP(returnBuf, (int)res);
}

static LRESULT CALLBACK windowProcedure(
  HWND hwnd,  // handle to window
  UINT uMsg,     // message
  WPARAM wParam, // first message parameter
  LPARAM lParam  // second message parameter
)
{
	JMP_BUF oldBuf1, oldBuf2;
	Handle resultHandle, originalProcess;
	Handle h = SAVE(myCallback);
	Handle hwHandle = Make_unsigned((int)hwnd);
	Handle umsgHandle = Make_unsigned(uMsg);
	Handle wParamHandle = Make_arbitrary_precision(wParam); /* Treat as SIGNED. */
	Handle lParamHandle = Make_arbitrary_precision(lParam); /* Treat as SIGNED. */
	Handle msg;

	if (myCallback == 0) return 0;

	memcpy(&oldBuf1, &returnBuf, sizeof(JMP_BUF));
	memcpy(&oldBuf2, &re_enter_poly, sizeof(JMP_BUF));
	msg = ALLOC(4);
	DEREFHANDLE(msg)[0] = DEREFWORDHANDLE(hwHandle);
	DEREFHANDLE(msg)[1] = DEREFWORDHANDLE(umsgHandle);
	DEREFHANDLE(msg)[2] = DEREFWORDHANDLE(wParamHandle);
	DEREFHANDLE(msg)[3] = DEREFWORDHANDLE(lParamHandle);
	/* Forking a process to run the callback is very expensive since it
	   will involve allocating a new process base and stack.  We might
	   be able to use the caller's stack by arranging for call_sym to
	   return a callback function and for the ML code to call the
	   callback. */
	(void)fork_function(h, msg);

	/* Set up a jump to receive the result. */
	resultHandle = (Handle)SETJMP(returnBuf);
	if (resultHandle == 0) /* Initial case. */
	{
		process_base *this_process = DEREFPROCHANDLE(processes);
		saved_process[processStackPtr++] = this_process;
		/*MD_set_for_retry(ioCall);*/
		remove_process(this_process);
		/* First time - enter the Poly code. */
		select_next_process();
		/* We need to use enter_poly_code here to set up a new longjmp
		   for RTS calls. */
		enter_poly_code();
		/*NOTREACHED*/
	}
	/* We've returned.  Restore the state. */
	memcpy(&returnBuf, &oldBuf1, sizeof(JMP_BUF));
	memcpy(&re_enter_poly, &oldBuf2, sizeof(JMP_BUF));
	/* Restore the original process. */
	originalProcess = SAVE(saved_process[--processStackPtr]);
	add_process(originalProcess, PROCESS_RUNABLE);
	/* Kill the one which returned the result. */
	kill_process(DEREFPROCHANDLE(processes));
	/* We must make sure that "processes" points to the process we suspended.
	   That process is the one that will receive the result when the C
	   function eventually returns. */
	select_next_process();
	DEREFPROCHANDLE(processes) = DEREFPROCHANDLE(originalProcess);
	return get_C_long(DEREFWORDHANDLE(resultHandle));
}

static int CALLBACK editWordBreakProc(  LPTSTR lpch,     // pointer to edit text
  int ichCurrent,  // index of starting point
  int cch,         // length in characters of edit text
  int code         // action to take
)
{
	return 0;
}


static void win_gc(GCOpFunc op)
/* Ensures that all the objects are retained and their addresses updated. */
{
    int i;
    /* Entries in the file table. These are marked as weak references so may
       return 0 for unreferenced streams. */
    for(i = 0; i < maxHandleTab; i++)
	{
		PHANDLETAB str = &(handleTable[i]);
		if (str->token != 0)
		{
			if (str->entryType == HE_PROCESS)
			{
				/* Update the references to opened streams but
				   do this only as weak references.  If the stream
				   has gone away then that's fine. */
				if (str->entry.process.readToken)
					op(&str->entry.process.readToken, 1);
				if (str->entry.process.writeToken)
					op(&str->entry.process.writeToken, 1);
			}
			op(&str->token, 1 /* Weak */);
            /* Unreferenced entries may return zero. */ 
            if (str->token == 0 && str->entryType != HE_UNUSED)
				close_handle(str);
        }
    }

	/* Callback function. */
	if (myCallback != 0) op(&myCallback, 0);
	for (i = 0; i < processStackPtr; i++) op((word**)&(saved_process[i]), 0);
}

void uninit_os_specific(void)
{
	if (handleTable)
	{
		int i;
		for (i = 0; i < maxHandleTab; i++)
		{
			if (handleTable[i].token != 0)
				close_handle(&handleTable[i]);
		}
		free(handleTable);
	}
	handleTable = NULL;
}

void init_os_specific(void)
{
    maxHandleTab = 5; /* Initialise to a small number. */
    /* A vector for the streams (initialised by calloc) */
    handleTable = (PHANDLETAB)calloc(maxHandleTab,sizeof(HANDLETAB));
    /* Register procedure to garbage-collect the entries. */
    RegisterGCProc(win_gc);
}

void re_init_os_specific(void)
{
}

