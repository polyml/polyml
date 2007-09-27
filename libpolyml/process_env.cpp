/*
    Title:      Process environment.
    Copyright (c) 2000
        David C. J. Matthews

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
#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

// Include this next before errors.h since in WinCE at least the winsock errors are defined there.
#ifdef WINDOWS_PC
#ifdef USEWINSOCK2
#include <winsock2.h>
#else
#include <winsock.h>
#endif
#endif


#include "globals.h"
#include "sys.h"
#include "run_time.h"
#include "process_env.h"
#include "arb.h"
#include "mpoly.h"
#include "errors.h"
#include "gc.h"
#include "scanaddrs.h"
#include "polystring.h"
#include "save_vec.h"
#include "process_env.h"
#include "rts_module.h"
#include "machine_dep.h"
#include "processes.h"
#include "locking.h"

#include "poly_specific.h" // For the functions that have been moved.

#define SAVE(x) mdTaskData->saveVec.push(x)
#define ALLOC(n) alloc_and_save(mdTaskData, n)

#if(!defined(MAXPATHLEN) && defined(MAX_PATH))
#define MAXPATHLEN MAX_PATH
#endif

#ifdef WINDOWS_PC
#define ISPATHSEPARATOR(c)  ((c) == '\\' || (c) == '/')
#define DEFAULTSEPARATOR    "\\"
#else
#define ISPATHSEPARATOR(c)  ((c) == '/')
#define DEFAULTSEPARATOR    "/"
#endif

// "environ" is declared in the headers on some systems but not all.
// Oddly, declaring it within process_env_dispatch_c causes problems
// on mingw where "environ" is actually a function.
extern char **environ;

/* Functions registered with atExit are added to this list. */
static PolyWord at_exit_list = TAGGED(0);
/* Once "exit" is called this flag is set and no further
   calls to atExit are allowed. */
static bool exiting = false;

static PLock atExitLock; // Thread lock for above.

Handle process_env_dispatch_c(TaskData *mdTaskData, Handle args, Handle code)
{
    int c = get_C_long(mdTaskData, DEREFWORDHANDLE(code));
    switch (c)
    {
    case 0: /* Return the program name. */
        return SAVE(C_string_to_Poly(mdTaskData, userOptions.programName));

    case 1: /* Return the argument list. */
        return convert_string_list(mdTaskData, userOptions.user_arg_count, userOptions.user_arg_strings);

    case 14: /* Return a string from the environment. */
        {
            char buff[MAXPATHLEN], *res;
            /* Get the string. */
            int length =
                Poly_string_to_C(DEREFWORDHANDLE(args), buff, sizeof(buff));
            if (length >= (int)sizeof(buff)) raise_syscall(mdTaskData, "Not Found", 0);
            res = getenv(buff);
            if (res == NULL) raise_syscall(mdTaskData, "Not Found", 0);
            else return SAVE(C_string_to_Poly(mdTaskData, res));
        }

    case 21: /* Return the whole environment. */
        {
            /* Count the environment strings */
            int env_count = 0;
            while (environ[env_count] != NULL) env_count++;
            return convert_string_list(mdTaskData, env_count, environ);
        }

    case 15: /* Return the success value. */
        return Make_arbitrary_precision(mdTaskData, EXIT_SUCCESS);

    case 16: /* Return a failure value. */
        return Make_arbitrary_precision(mdTaskData, EXIT_FAILURE);

    case 17: /* Run command. */
        {
            char buff[MAXPATHLEN];
            int res;
            /* Get the string. */
            int length =
                Poly_string_to_C(DEREFWORD(args), buff, sizeof(buff));
            if (length >= (int)sizeof(buff))
                raise_syscall(mdTaskData, "Command too long", ENAMETOOLONG);
            res = system(buff);
            if (res == -1)
                raise_syscall(mdTaskData, "Function system failed", errno);
            return Make_arbitrary_precision(mdTaskData, res);
        }

    case 18: /* Register function to run at exit. */
        {
            PLocker locker(&atExitLock);
            if (! exiting)
            {
                PolyObject *cell = alloc(mdTaskData, 2);
                cell->Set(0, at_exit_list);
                cell->Set(1, DEREFWORD(args));
                at_exit_list = cell;
            }
            return Make_arbitrary_precision(mdTaskData, 0);
        }

    case 19: /* Return the next function in the atExit list and set the
                "exiting" flag to true. */
        {
            PLocker locker(&atExitLock);
            Handle res;
            exiting = true; /* Ignore further calls to atExit. */
            if (at_exit_list == TAGGED(0))
                raise_syscall(mdTaskData, "List is empty", 0);
            PolyObject *cell = at_exit_list.AsObjPtr();
            res = SAVE(cell->Get(1));
            at_exit_list = cell->Get(0);
            return res;
        }

    case 20: /* Terminate without running the atExit list or flushing buffers. */
        {
            /* I don't like terminating without some sort of clean up
               but we'll do it this way for the moment. */
            int i = get_C_long(mdTaskData, DEREFWORDHANDLE(args));
			_exit(i);
        }

        /************ Error codes **************/

    case 2: /* Get the name of a numeric error message. */
        {
            char buff[40];
            int e = get_C_long(mdTaskData, DEREFWORDHANDLE(args));
            Handle  res;
            unsigned i;
            /* First look to see if we have the name in
               the error table. They should generally all be
               there. */
            for (i = 0; i < sizeof(errortable)/sizeof(errortable[0]); i++)
                if (errortable[i].errorNum == e)
                    return SAVE(C_string_to_Poly(mdTaskData, errortable[i].errorString));
            /* We get here if there's an error which isn't in the table. */
#ifdef WINDOWS_PC
            /* In the Windows version we may have both errno values
               and also GetLastError values.  We convert the latter into
               negative values before returning them. */
            if (e < 0)
            {
                sprintf(buff, "WINERROR%0d", -e);
                res = SAVE(C_string_to_Poly(mdTaskData, buff));
                return res;
            }
            else
#endif
            {
                sprintf(buff, "ERROR%0d", e);
                res = SAVE(C_string_to_Poly(mdTaskData, buff));
            }
            return res;
        }

    case 3: /* Get the explanatory message for an error. */
        {
            int e = get_C_long(mdTaskData, DEREFWORDHANDLE(args));
            Handle  res;
#ifdef WINDOWS_PC
            /* In the Windows version we may have both errno values
               and also GetLastError values.  We convert the latter into
               negative values before returning them. */
            if (e < 0)
            {
                LPTSTR lpMsg = NULL;
                TCHAR *p;
                if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
                        FORMAT_MESSAGE_ALLOCATE_BUFFER |
                        FORMAT_MESSAGE_IGNORE_INSERTS,
                        NULL, (DWORD)(-e), 0, (LPTSTR)&lpMsg, 1, NULL) > 0)
                {
                    /* The message is returned with CRLF at the end.  Remove them. */
                    for (p = lpMsg; *p != '\0' && *p != '\n' && *p != '\r'; p++);
                    *p = '\0';
                    res = SAVE(C_string_to_Poly(mdTaskData, lpMsg));
                    LocalFree(lpMsg);
                    return res;
                }
            }
#endif
            res = SAVE(C_string_to_Poly(mdTaskData, strerror(e)));
            return res;
        }
    case 4: /* Try to convert an error string to an error number. */
        {
            char buff[40];
            /* Get the string. */
            Poly_string_to_C(DEREFWORD(args), buff, sizeof(buff));
            /* Look the string up in the table. */
            for (unsigned i = 0; i < sizeof(errortable)/sizeof(errortable[0]); i++)
                if (strcmp(buff, errortable[i].errorString) == 0)
                    return Make_arbitrary_precision(mdTaskData, errortable[i].errorNum);
            /* If we don't find it then it may have been a constructed
               error name. */
            if (strncmp(buff, "ERROR", 5) == 0)
            {
                int i = atoi(buff+5);
                if (i > 0) return Make_arbitrary_precision(mdTaskData, i);
            }
#ifdef WINDOWS_PC
            if (strncmp(buff, "WINERROR", 8) == 0)
            {
                int i = atoi(buff+8);
                if (i > 0) return Make_arbitrary_precision(mdTaskData, -i);
            }
#endif
            return Make_arbitrary_precision(mdTaskData, 0);
        }

        /************ Directory/file paths **************/

    case 5: /* Return the string representing the current arc. */
        return SAVE(C_string_to_Poly(mdTaskData, "."));

    case 6: /* Return the string representing the parent arc. */
        /* I don't know that this exists in MacOS. */
        return SAVE(C_string_to_Poly(mdTaskData, ".."));

    case 7: /* Return the string representing the directory separator. */
        return SAVE(C_string_to_Poly(mdTaskData, DEFAULTSEPARATOR));

    case 8: /* Test the character to see if it matches a separator. */
        {
            int e = get_C_long(mdTaskData, DEREFWORDHANDLE(args));
            if (ISPATHSEPARATOR(e))
                return Make_arbitrary_precision(mdTaskData, 1);
            else return Make_arbitrary_precision(mdTaskData, 0);
        }

    case 9: /* Are names case-sensitive? */
#ifdef WINDOWS_PC
        /* Windows - no. */
        return Make_arbitrary_precision(mdTaskData, 0);
#else
        /* Unix - yes. */
        return Make_arbitrary_precision(mdTaskData, 1);
#endif

    case 10: /* Are empty arcs redundant? */
        /* Unix and Windows - yes. */
        return Make_arbitrary_precision(mdTaskData, 1);

    case 11: /* Match the volume name part of a path. */
        {
            char *volName = NULL;
            int  isAbs = 0;
            int  toRemove = 0;
            PolyWord path = DEREFHANDLE(args);
            /* This examines the start of a string and determines
               how much of it represents the volume name and returns
               the number of characters to remove, the volume name
               and whether it is absolute.
               One would assume that if there is a volume name then it
               is absolute but there is a peculiar form in Windows/DOS
               (e.g. A:b\c) which means the file b\c relative to the
               currently selected directory on the volume A.
            */
#ifdef WINDOWS_PC
            char buff[MAXPATHLEN];
            int length;
            length = Poly_string_to_C(path, buff, MAXPATHLEN);
            /* Ignore the case where the whole path is too long. */
            if (length >= 2 && buff[1] == ':')
            { /* Volume name? */
                if (length >= 3 && ISPATHSEPARATOR(buff[2]))
                {
                    /* Absolute path. */
                    toRemove = 3; isAbs = 1;
                }
                else { toRemove = 2; isAbs = 0; }
                volName = buff; buff[2] = '\0';
            }
            else if (length > 3 &&
                     ISPATHSEPARATOR(buff[0]) &&
                     ISPATHSEPARATOR(buff[1]) &&
                     ! ISPATHSEPARATOR(buff[2]))
            { /* UNC name? */
                int i;
                /* Skip the server name. */
                for (i = 3; buff[i] != 0 && !ISPATHSEPARATOR(buff[i]); i++);
                if (ISPATHSEPARATOR(buff[i]))
                {
                    i++;
                    /* Skip the share name. */
                    for (; buff[i] != 0 && !ISPATHSEPARATOR(buff[i]); i++);
                    toRemove = i;
                    if (buff[i] != 0) toRemove++;
                    isAbs = 1;
                    volName = buff;
                    buff[i] = '\0';
                }
            }
            else if (ISPATHSEPARATOR(buff[0]))
                /* \a\b strictly speaking is relative to the
                   current drive.  It's much easier to treat it
                   as absolute. */
                { toRemove = 1; isAbs = 1; volName = ""; }
#else
            /* Unix - much simpler. */
            char toTest = 0;
            if (IS_INT(path)) toTest = UNTAGGED(path);
            else {
                PolyStringObject * ps = (PolyStringObject *)path.AsObjPtr();
                if (ps->length > 1) toTest = ps->chars[0];
            }
            if (ISPATHSEPARATOR(toTest))
                { toRemove = 1; isAbs = 1; volName = ""; }
#endif
            /* Construct the result. */
            {
                Handle sVol = SAVE(C_string_to_Poly(mdTaskData, volName));
                Handle sRes = ALLOC(3);
                DEREFWORDHANDLE(sRes)->Set(0, TAGGED(toRemove));
                DEREFHANDLE(sRes)->Set(1, DEREFWORDHANDLE(sVol));
                DEREFWORDHANDLE(sRes)->Set(2, TAGGED(isAbs));
                return sRes;
            }
        }

    case 12: /* Construct a name from a volume and whether it is
                absolute. */
        {
            unsigned isAbs = get_C_ulong(mdTaskData, DEREFHANDLE(args)->Get(1));
            PolyWord volName = DEREFHANDLE(args)->Get(0);
            /* In Unix the volume name will always be empty. */
            if (isAbs == 0)
                return SAVE(volName);
            /* N.B. The arguments to strconcatc are in reverse. */
            else return strconcatc(mdTaskData,
                                   SAVE(C_string_to_Poly(mdTaskData, DEFAULTSEPARATOR)),
                                   SAVE(volName));
        }

    case 13: /* Is the string a valid file name? */
        {
            /* Should we check for special names such as aux, con, prn ?? */
            PolyWord volName = DEREFWORD(args);
            if (IS_INT(volName))
            {
                char ch = (char)UNTAGGED(volName);
#ifdef WINDOWS_PC
                if (ch == '<' || ch == '>' || ch == '|' ||
                    ch == '"' || ch < ' ')
#else
                /* Basically, everything is allowed in Unix
                   except NULL. */
                if (ch == '\0')
#endif
                    return Make_arbitrary_precision(mdTaskData, 0);
            }
            else
            {
                PolyStringObject * volume = (PolyStringObject *)(volName.AsObjPtr());
                for (POLYUNSIGNED i = 0; i < volume->length; i++)
                {
                    char ch = volume->chars[i];
#ifdef WINDOWS_PC
                    if (ch == '<' || ch == '>' || ch == '|' ||
                        ch == '"' || ch < ' ')
#else
                    if (ch == '\0')
#endif
                        return Make_arbitrary_precision(mdTaskData, 0);
                }
            }
            return Make_arbitrary_precision(mdTaskData, 1);
        }

        // A group of calls have now been moved to poly_specific.
        // This entry is returned for backwards compatibility.
    case 100: case 101: case 102: case 103: case 104: case 105:
        return poly_dispatch_c(mdTaskData, args, code);

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown environment function: %d", c);
            raise_exception_string(mdTaskData, EXC_Fail, msg);
			return 0;
        }
    }
}

/* Terminate normally with a result code. */
Handle finishc(TaskData *taskData, Handle h)
{
    int i = get_C_long(taskData, DEREFWORDHANDLE(h));
    // Cause the other threads to exit.
    processes->Exit(i);
    // Exit this thread
    processes->ThreadExit(taskData); // Doesn't return.
    // Push a dummy result to keep lint happy
    return taskData->saveVec.push(TAGGED(0));
}

class ProcessEnvModule: public RtsModule
{
public:
    void GarbageCollect(ScanAddress *process);
};

// Declare this.  It will be automatically added to the table.
static ProcessEnvModule processModule;

void ProcessEnvModule::GarbageCollect(ScanAddress *process)
/* Ensures that all the objects are retained and their addresses updated. */
{
    if (at_exit_list.IsDataPtr())
    {
        PolyObject *obj = at_exit_list.AsObjPtr();
        process->ScanRuntimeAddress(&obj, ScanAddress::STRENGTH_STRONG);
        at_exit_list = obj;
    }
}
