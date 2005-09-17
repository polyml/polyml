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
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#ifndef WINDOWS_PC
#include <sys/param.h>
#include <unistd.h>
#endif

#include "globals.h"
#include "sys.h"
#include "run_time.h"
#include "process_env.h"
#include "arb.h"
#include "mpoly.h"
#include "errors.h"
#include "gc.h"
#include "machine_dep.h"

#define SAVE(x) push_to_save_vec((word)(x))
#define ALLOC(n) alloc_and_save(n)
#define SIZEOF(x) (sizeof(x)/sizeof(word))
#define EMPTYSTRING interface_map[POLY_SYS_nullorzero]


#ifdef WINDOWS_PC
#define MAXPATHLEN MAX_PATH
#define ISPATHSEPARATOR(c)	((c) == '\\' || (c) == '/')
#define DEFAULTSEPARATOR	"\\"
#else
#define ISPATHSEPARATOR(c)	((c) == '/')
#define DEFAULTSEPARATOR	"/"
#endif

/* Functions registered with atExit are added to this list. */
static word *at_exit_list = (word*)TAGGED(0);
/* Once "exit" is called this flag is set and no further
   calls to atExit are allowed. */
static int exiting = 0;

Handle process_env_dispatch_c(Handle args, Handle code)
{
	int c = get_C_long(DEREFWORDHANDLE(code));
	switch (c)
	{
	case 0: /* Return the program name. */
		return SAVE(C_string_to_Poly(programName));

	case 1: /* Return the argument list. */
		return convert_string_list(user_arg_count, user_arg_strings);

	case 14: /* Return a string from the environment. */
		{
			char buff[MAXPATHLEN], *res;
			/* Get the string. */
			int length =
				Poly_string_to_C(DEREFSTRINGHANDLE(args), buff, sizeof(buff));
			if (length >= sizeof(buff)) raise_syscall("Not Found", 0);
			res = getenv(buff);
			if (res == NULL) raise_syscall("Not Found", 0);
			else return SAVE(C_string_to_Poly(res));
		}

	case 21: /* Return the whole environment. */
		{
			extern char **environ;
			/* Count the environment strings */
			int env_count = 0;
			while (environ[env_count] != NULL) env_count++;
			return convert_string_list(env_count, environ);
		}

	case 15: /* Return the success value. */
		return Make_arbitrary_precision(EXIT_SUCCESS);

	case 16: /* Return a failure value. */
		return Make_arbitrary_precision(EXIT_FAILURE);

	case 17: /* Run command. */
		{
			char buff[MAXPATHLEN];
			int res;
			/* Get the string. */
			int length =
				Poly_string_to_C(DEREFSTRINGHANDLE(args), buff, sizeof(buff));
			if (length >= sizeof(buff))
				raise_syscall("Command too long", ENAMETOOLONG);
			res = system(buff);
			if (res == -1)
				raise_syscall("Function system failed", errno);
			return Make_arbitrary_precision(res);
		}

	case 18: /* Register function to run at exit. */
		{
			if (exiting == 0)
			{
				word *cell = alloc(2);
				cell[0] = (word)at_exit_list;
				cell[1] = (word)DEREFWORDHANDLE(args);
				at_exit_list = cell;
			}
			return Make_arbitrary_precision(0);
		}

	case 19: /* Return the next function in the atExit list and set the
				"exiting" flag to true. */
		{
			Handle res;
			exiting = 1; /* Ignore further calls to atExit. */
			if (at_exit_list == (word*)TAGGED(0))
				raise_syscall("List is empty", 0);
			res = SAVE(at_exit_list[1]);
			at_exit_list = (word*)(at_exit_list[0]);
			return res;
		}

	case 20: /* Terminate without running the atExit list or flushing buffers. */
		{
			/* I don't like terminating without some sort of clean up
			   but we'll do it this way for the moment. */
			int i = get_C_long(DEREFWORDHANDLE(args));
			_exit(i);
		}

		/************ Error codes **************/

	case 2: /* Get the name of a numeric error message. */
		{
			char buff[40];
			int e = get_C_long(DEREFWORDHANDLE(args));
			Handle	res;
			int i;
			/* First look to see if we have the name in
			   the error table. They should generally all be
			   there. */
			for (i = 0; i < sizeof(errortable)/sizeof(errortable[0]); i++)
				if (errortable[i].errorNum == e)
					return SAVE(C_string_to_Poly(errortable[i].errorString));
			/* We get here if there's an error which isn't in the table. */
#ifdef WINDOWS_PC
			/* In the Windows version we may have both errno values
			   and also GetLastError values.  We convert the latter into
			   negative values before returning them. */
			if (e < 0)
			{
				sprintf(buff, "WINERROR%0d", -e);
				res = SAVE(C_string_to_Poly(buff));
				return res;
			}
			else
#endif
			{
				sprintf(buff, "ERROR%0d", e);
				res = SAVE(C_string_to_Poly(buff));
			}
			return res;
		}

	case 3: /* Get the explanatory message for an error. */
		{
			int e = get_C_long(DEREFWORDHANDLE(args));
			Handle	res;
#ifdef WINDOWS_PC
			/* In the Windows version we may have both errno values
			   and also GetLastError values.  We convert the latter into
			   negative values before returning them. */
			if (e < 0)
			{
				LPTSTR lpMsg = NULL;
				char *p;
				if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
						FORMAT_MESSAGE_ALLOCATE_BUFFER |
						FORMAT_MESSAGE_IGNORE_INSERTS,
						NULL, (DWORD)(-e), 0, (LPTSTR)&lpMsg, 1, NULL) > 0)
				{
					/* The message is returned with CRLF at the end.  Remove them. */
					for (p = lpMsg; *p != '\0' && *p != '\n' && *p != '\r'; p++);
					*p = '\0';
					res = SAVE(C_string_to_Poly(lpMsg));
					LocalFree(lpMsg);
					return res;
				}
			}
#endif
			res = SAVE(C_string_to_Poly(strerror(e)));
			return res;
		}
	case 4: /* Try to convert an error string to an error number. */
		{
			char buff[40];
			int i;
			/* Get the string. */
			Poly_string_to_C(DEREFSTRINGHANDLE(args), buff, sizeof(buff));
			/* Look the string up in the table. */
			for (i = 0; i < sizeof(errortable)/sizeof(errortable[0]); i++)
				if (strcmp(buff, errortable[i].errorString) == 0)
					return Make_arbitrary_precision(errortable[i].errorNum);
			/* If we don't find it then it may have been a constructed
			   error name. */
			if (strncmp(buff, "ERROR", 5) == 0)
			{
				i = atoi(buff+5);
				if (i > 0) return Make_arbitrary_precision(i);
			}
#ifdef WINDOWS_PC
			if (strncmp(buff, "WINERROR", 8) == 0)
			{
				i = atoi(buff+8);
				if (i > 0) return Make_arbitrary_precision(-i);
			}
#endif
			return Make_arbitrary_precision(0);
		}

		/************ Directory/file paths **************/

	case 5: /* Return the string representing the current arc. */
		return SAVE(C_string_to_Poly("."));

	case 6: /* Return the string representing the parent arc. */
		/* I don't know that this exists in MacOS. */
		return SAVE(C_string_to_Poly(".."));

	case 7: /* Return the string representing the directory separator. */
		return SAVE(C_string_to_Poly(DEFAULTSEPARATOR));

	case 8: /* Test the character to see if it matches a separator. */
		{
			int e = get_C_long(DEREFWORDHANDLE(args));
			if (ISPATHSEPARATOR(e))
				return Make_arbitrary_precision(1);
			else return Make_arbitrary_precision(0);
		}

	case 9: /* Are names case-sensitive? */
#ifdef WINDOWS_PC
		/* Windows - no. */
		return Make_arbitrary_precision(0);
#else
		/* Unix - yes. */
		return Make_arbitrary_precision(1);
#endif

	case 10: /* Are empty arcs redundant? */
		/* Unix and Windows - yes. */
		return Make_arbitrary_precision(1);

	case 11: /* Match the volume name part of a path. */
		{
			char *volName = NULL;
			int  isAbs = 0;
			int  toRemove = 0;
			pstring path = DEREFSTRINGHANDLE(args);
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
			else if (path->length > 1) toTest = path->chars[0];
			if (ISPATHSEPARATOR(toTest))
				{ toRemove = 1; isAbs = 1; volName = ""; }
#endif
			/* Construct the result. */
			{
				Handle sVol = SAVE(C_string_to_Poly(volName));
				Handle sRes = ALLOC(3);
				DEREFWORDHANDLE(sRes)[0] = TAGGED(toRemove);
				DEREFHANDLE(sRes)[1] = DEREFWORDHANDLE(sVol);
				DEREFWORDHANDLE(sRes)[2] = TAGGED(isAbs);
				return sRes;
			}
		}

	case 12: /* Construct a name from a volume and whether it is
				absolute. */
		{
			unsigned isAbs = get_C_ulong(DEREFHANDLE(args)[1]);
			pstring volName = (pstring)DEREFHANDLE(args)[0];
			/* In Unix the volume name will always be empty. */
			if (isAbs == 0)
				return SAVE(volName);
			/* N.B. The arguments to strconcatc are in reverse. */
			else return strconcatc(SAVE(C_string_to_Poly(DEFAULTSEPARATOR)),
								   SAVE(volName));
		}

	case 13: /* Is the string a valid file name? */
		{
			/* Should we check for special names such as aux, con, prn ?? */
			pstring volName = DEREFSTRINGHANDLE(args);
			if (IS_INT(volName))
			{
				char ch = UNTAGGED(volName);
#ifdef WINDOWS_PC
				if (ch == '<' || ch == '>' || ch == '|' ||
					ch == '"' || ch < ' ')
#else
				/* Basically, everything is allowed in Unix
				   except NULL. */
				if (ch == '\0')
#endif
					return Make_arbitrary_precision(0);
			}
			else
			{
				int i;
				for (i = 0; i < volName->length; i++)
				{
					char ch = volName->chars[i];
#ifdef WINDOWS_PC
					if (ch == '<' || ch == '>' || ch == '|' ||
						ch == '"' || ch < ' ')
#else
					if (ch == '\0')
#endif
						return Make_arbitrary_precision(0);
				}
			}
			return Make_arbitrary_precision(1);
		}

			/* These next ones don't really logically belong here
			   but there's no other obvious place for them. */
	case 100: /* Return the maximum word segment size. */
		return Make_arbitrary_precision(OBJ_PRIVATE_LENGTH_MASK);
	case 101: /* Return the maximum string size (in bytes).
				 It is the maximum number of bytes in a segment
				 less one word for the length field. */
		return Make_arbitrary_precision(
				(OBJ_PRIVATE_LENGTH_MASK)*sizeof(word) - sizeof(word));
	case 102: /* Test whether the supplied address is in the io area.
			     This was previously done by having get_flags return
				 256 but this was changed so that get_flags simply
				 returns the top byte of the length word. */
		{
			word *pt = DEREFWORDHANDLE(args);
			if (IsIOPointer(H, pt))
				return Make_arbitrary_precision(1);
			else return Make_arbitrary_precision(0);
		}
	case 103: /* Return the register mask for the given function.
				 This is used by the code-generator to find out
				 which registers are modified by the function and
				 so need to be saved if they are used by the caller. */
		{
#if (1)
			word *pt = DEREFWORDHANDLE(args);
			if (IsIOPointer(H, pt))
			{
				/* IO area.  We need to get this from the vector. */
				int i;
				for (i=0; i < POLY_SYS_vecsize; i++)
				{
					if (pt == interface_map[i])
					{
						return Make_arbitrary_precision(registerMaskVector[i]);
					}
				}
				raise_syscall("Io pointer not found", 0);
			}
			else
			{
				word l = pt[-1]; /* Length word. */
				/* We may have a pointer to the code or a pointer to
				   a closure.  If it's a closure we have to find the
				   code. */
				if (! OBJ_IS_CODE_OBJECT(l) && ! OBJ_IS_BYTE_OBJECT(l))
				{
					pt = (word*)pt[0];
					l = pt[-1];
				}
				/* Should now be a code object. */
				if (OBJ_IS_CODE_OBJECT(l))
				{
					/* Compiled code.  This is the second constant in the
					   constant area. */
					word *codePt, mask;
					word obj_length = OBJ_OBJECT_LENGTH(pt[-1]);
					word *last_word = &(pt[obj_length - 1]);
					int consts_count = (int)*last_word;
					codePt = (word *)(last_word - consts_count);
					/* OBJ_PTR_TO_CONSTS_PTR(pt, codePt); */
					/* Older segments may not have the mask. */
					if (consts_count < 2)
						return Make_arbitrary_precision(-1);
					mask = codePt[1];
					/* A real mask will be an integer.  For backwards
					   compatibility if we find something that isn't we
					   treat it as all registers. */
					if (IS_INT(mask))
					{
						return SAVE(mask);
					}
					else return Make_arbitrary_precision(-1);
				}
				else raise_syscall("Not a code pointer", 0);
			}
#else
			return Make_arbitrary_precision(-1);
#endif
		}

	case 104: return Make_arbitrary_precision(POLY_version_number);

	case 105: /* Get the name of the function. */
		{
			word *pt = DEREFWORDHANDLE(args);
			if (IsIOPointer(H, pt))
			{
				/* IO area. */
				int i;
				for (i=0; i < POLY_SYS_vecsize; i++)
				{
					if (pt == interface_map[i])
					{
						char buff[8];
						sprintf(buff, "RTS%d", i);
						return SAVE(C_string_to_Poly(buff));
					}
				}
				raise_syscall("Io pointer not found", 0);
			}
			else
			{
				word l = pt[-1]; /* Length word. */
				/* Should now be a code object. */
				if (OBJ_IS_CODE_OBJECT(l))
				{
					/* Compiled code.  This is the first constant in the
					   constant area. */
					word *codePt, name;
					word obj_length = OBJ_OBJECT_LENGTH(pt[-1]);
					word *last_word = &(pt[obj_length - 1]);
					int consts_count = (int)*last_word;
					codePt = (word *)(last_word - consts_count);
					name = codePt[0];
					/* May be zero indicating an anonymous segment - return null string. */
					if (name == 0) return SAVE(C_string_to_Poly(""));
					else return SAVE(name);
				}
				else raise_syscall("Not a code pointer", 0);
			}
		}

	default: crash("Unknown environment function: %d\n", c);
	}
}

/* Terminate normally with a result code. */
void finishc(Handle h)
{
  int i = get_C_long(DEREFWORDHANDLE(h));
  finish(i);
}


static void proc_gc(GCOpFunc op)
/* Ensures that all the objects are retained and their addresses updated. */
{
	op(&at_exit_list, 0 /* Strong reference */);
}


void re_init_proc_system(void)
{
}

void init_proc_system(void)
{    
    /* Register procedure to ensure that the atExit list
	   is passed to the garbage collector. */
    RegisterGCProc(proc_gc);
}

/* Release all resources.  Not strictly necessary since the OS should
   do this but probably a good idea. */
void uninit_proc_system(void)
{
}

