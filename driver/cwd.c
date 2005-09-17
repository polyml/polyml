/*
    Title:      Working directory manipulation

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

#if defined(WINDOWS_PC)
/* PC version implemented OK - 09/02/96 */

#include <windows.h>
#include <errno.h>
#include "diagnostics.h" /* for Crash */

void get_working_directory(char *buffer)
{
   if (GetCurrentDirectory(MAX_PATH+1,buffer) == 0)
  {
     Crash("get_working_directory: GetCurrentDirectory failed, errno = %i",
     GetLastError());
  }
}

void set_working_directory(const char *buffer)
{
  if (SetCurrentDirectory(buffer) == FALSE)
  {
     Crash("set_working_directory: SetCurrentDirectory failed for directory %s, errno = %i,", 
           buffer, GetLastError());
  }
}

static char home_directory[MAX_PATH+1];

void init_working_directory(void)
{
  get_working_directory(home_directory);
}

void reset_working_directory(void)
{
  set_working_directory(home_directory);
}


#else /* UNIX */

#include <sys/param.h>
#include <unistd.h>
#include <errno.h>
#include "diagnostics.h" /* for Crash */

void get_working_directory(char *buffer)
{
  if (getcwd(buffer,MAXPATHLEN+1) == (char *)NULL)
  {
     Crash("get_working_directory: getcwd failed, errno = %i,", errno);
  }
}

void set_working_directory(const char *buffer)
{
  if (chdir(buffer) != 0)
  {
     Crash("set_working_directory: chdir failed for directory %s, errno = %i,", 
           buffer, errno);
  }
}


static char home_directory[MAXPATHLEN+1];

void init_working_directory(void)
{
  get_working_directory(home_directory);
}

void reset_working_directory(void)
{
  set_working_directory(home_directory);
}

#endif /* not PC */





