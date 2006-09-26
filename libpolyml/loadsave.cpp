/*
    Title:  loadsave.cpp - Save and reload the heap

    Copyright (c) 2006 David C.J. Matthews

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

#ifdef _WIN32_WCE
#include "winceconfig.h"
#else
#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
#endif
#endif

#include "loadsave.h"
#include "save_vec.h"
#include "polystring.h"
#include "run_time.h"
#include "sys.h"


Handle saveHeap(Handle args)
{
    raise_exception_string (EXC_Fail,"Not implemented");
	return 0;
}

Handle loadHeap(Handle args)
{
    raise_exception_string (EXC_Fail,"Not implemented");
	return 0;
}

