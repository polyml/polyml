/*
    Title: 	profiling.h

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

#ifndef _PROFILING_H_DEFINED
#define _PROFILING_H_DEFINED 1

/* shared data objects */
extern int          store_profiling;
extern int          emulate_profiling;


extern void stop_profiling(int print);
extern word profilerc(Handle mode_handle);
extern void add_count(byte *pc,word *sp,int incr);

extern void init_profiling_system(void);
extern void uninit_profiling_system(void);

#endif /* _PROFILING_H_DEFINED */
