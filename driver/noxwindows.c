/*
    Title:      Exclude X-windows module

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

/* noxwindows.c */

/* added 26/10/93 SPF */
#include "globals.h"
#include "run_time.h"
#include "sys.h"

Handle XWindows_c(Handle params)
/* params is not used, but is needed to keep the types right. */
{
  raise_exception_string(EXC_XWindows, "Not implemented");
  
  /*NOTREACHED*/
  return (Handle)TAGGED(0); /* just to keep lint happy */
}

void init_xwindow_system(void){}
void re_init_xwindow_system(void){} /* added 18/11/93 SPF */
