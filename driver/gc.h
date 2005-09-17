/*
    Title: 	gc.h - exports signature for gc.c

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

/* created 26/10/93 SPF */

extern void CopyStackFrame(StackObject *old_stack,StackObject *new_stack);
extern void RegisterGCProc(GCMapFunc);
extern void OpGCProcs(GCOpFunc op);
extern void SetImmutables(int writeable);
extern void FullGC(void);
extern void QuickGC(int words_needed);
extern void ForConstant(word **pt, void (*op)());

extern void CreateHeap(void);
extern void DestroyHeap(void);
