/*
    Title:      Diagnostics

	Copyright (c) 2000
		Cambridge University Technical Services Limited

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

/*------------------------------------------------------------------------------
  diagnostics.c

  File intended to collect misc (common) debug/diagnostic things.
------------------------------------------------------------------------------*/

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "proper_io.h"

#include <string.h>

/*------------------------------------------------------------------------------
  define prototypes that dont seem to be available
------------------------------------------------------------------------------*/

#if defined(WINDOWS_PC)
#include <windows.h>
#include "Console.h"
#endif

/*------------------------------------------------------------------------------
  syserror: given a system errno return a descriptive string, if possible.
------------------------------------------------------------------------------*/  
  
const char* syserror(int err)
{
   const char* error = strerror(err);
   if (error != NULL)
	   return error;
   else
    	return "<sys error message unavailable>";
}	


/***********************************************************************
 *
 *  FAILURE FUNCTIONS (moved here from mmap.c)
 *
 ***********************************************************************/

void Exit(msg,a,b,c,d,e) /* VARARGS1 */
char *msg;
int  *a,*b,*c,*d,*e;
{
  proper_printf("\n");
  proper_printf(msg, a, b, c, d, e);
  proper_printf("\n");
  proper_fflush(stdout);
#if defined(WINDOWS_PC)
  if (useConsole)
  {
	  MessageBox(hMainWindow, "Poly/ML has exited", "Poly/ML", MB_OK);
  }
#endif
  exit(1);
}


void SysError(msg,a,b,c,d,e) /* VARARGS1 */
char *msg;
int  *a,*b,*c,*d,*e;
{
	int er = errno;
	char buff[50];
	proper_fflush(stdout);
	proper_printf("\n");
	proper_printf(msg, a, b, c, d, e);
	proper_printf("\n");
	proper_fflush(stdout);
	sprintf(buff,"Error number %i",er);
	perror(buff);
	proper_fflush(stderr);
#if defined(WINDOWS_PC) /* PC version */
  if (useConsole)
  {
	  MessageBox(hMainWindow, "Poly/ML has exited", "Poly/ML", MB_OK);
  }
#endif
	exit(1);
}

void Crash(msg,a,b,c,d,e) /* VARARGS1 */
char *msg;
int  *a,*b,*c,*d,*e;
{
  proper_printf("\n");
  proper_printf(msg, a, b, c, d, e);
  proper_printf("\n");
  proper_fflush(stdout);
#if defined(WINDOWS_PC)
  if (useConsole)
  {
	  MessageBox(hMainWindow, "Poly/ML has exited", "Poly/ML", MB_OK);
  }
#endif
  abort();
  exit(1);
}

void Usage (char *message)
{
  if (message) proper_printf("%s\n", message);
  proper_printf("Normal usage: poly [-r] database\n");
  proper_printf("Compact a database: poly -d [-c] database\n");
  proper_printf("Change parent database: poly -p filename parent\n");
  proper_printf("\n");
  proper_printf("Special optional parameters:\n");
  proper_printf("  -noDisplay\n");
  proper_printf("  -H  <heap-size-in-MB>\n");
  proper_printf("  -IB <immutables-buffer-in-MB>\n");
  proper_printf("  -MB <mutables-buffer-in-KB>\n");
  proper_printf("  -h  <heap-size-in-KB>\n");
  proper_printf("  -ib <immutables-buffer-in-KB>\n");
  proper_printf("  -mb <mutables-buffer-in-KB>\n");
  proper_printf("  -ip <immutables-percent>\n");
  proper_printf("  -mp <mutables-percent>\n");
  proper_printf("  -s  <statistics-level>\n");
  proper_printf("  -sf <statistics-file>\n");
  proper_printf("  -t  <timeslice>\n");
  proper_printf("  -D  <debugging-level>\n");
  proper_printf("\n");
  proper_printf("Examples:\n");
  proper_printf("  poly my_dbase              : write back to my_dbase when commiting\n");
  proper_printf("  poly -r my_dbase           : never write back to my_dbase\n");
  proper_printf("  poly my_dbase -noDisplay   : do not connect to X display\n");
  proper_printf("  poly my_dbase -h 2000      : heap limit of 2000K\n");
  proper_printf("  poly my_dbase -H 20        : heap limit of 20M\n");
  proper_printf("  poly my_dbase -s 2         : show garbage collector statistics\n");
  proper_printf("  poly my_dbase -D 3         : set debugging level\n");
  proper_printf("\n");
  proper_printf("  poly -d my_dbase           : compact my_dbase\n");
  proper_printf("  poly -d -c my_dbase        : compact my_dbase and share common data structures\n");
  proper_printf("\n");
  proper_fflush(stdout);

#if defined(WINDOWS_PC)
  if (useConsole)
  {
	  MessageBox(hMainWindow, "Poly/ML has exited", "Poly/ML", MB_OK);
  }
#endif
  exit (1);
}
