/*
    Title: 	mmap.h - exports list for mmap.c

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

#ifndef MMAP_H_DEFINED
#define MMAP_H_DEFINED

#include "globals.h"
#include "mm.h"

#if defined(DEBUG) || defined(_DEBUG)
extern word IsDatabasePointer(const Header H, const word *pt);
#endif

extern void    DisplayHeader(const Header H);
extern Header  OpenMappedDatabase(char *filename,const int flags,const Header up);

extern int     GrowSpace(const Header, Space *, const int);

extern int     ReserveMLSpaces(void);

extern void MapIOArea(const Header H);
extern void FreezeIOArea(const Header H);
extern void UnMapIOArea(const GCConst *C);

extern void    CommitMappedDatabase(const Header H);
extern void    AssignMappedDatabaseWord(const Header H,word *base,int offset,const word value);
extern void    AssignMappedDatabaseByte(const Header H,byte *base,int offset,const byte value);
extern void    IncrementProfileCount(const Header H,word *pt,const word value,int *unknown_count);
extern void    RemoveObjectProtection(const Header H, byte *pt);

extern void    OpOldMutables(const Header H,void (*op)(word **pt, int offset));
extern void    OpOldProfileCounts(const Header H,void (*op)(word *pt));

extern void    CreateNewCopyDatabase(char *filename,const Header H);
extern void    CreateMappedDatabase(char *filename);
extern CStatus CreateChildDatabase(char *filename,const Header H);

extern GCSpace  NewDataSpaces(const Header H);
extern root_struct *Root(const Header H);
extern void     SetRoot(Header H,root_struct *newRoot);
extern word     IsNil(const Header H,const word *pt);
extern word     IsIOPointer(const Header H,const word *pt);
extern word     IsDataPointer(const Header H,const word *pt);
extern Header   ParentDatabase(const Header H);

extern word *AllocateLocalMutableChunks(const int);
extern word *AllocateLocalImmutableChunks(const int);
extern word *FreeLocalMutableChunks(const int);
extern word *FreeLocalImmutableChunks(const int);
extern void UnmapSpace(const Header H, const Space *s);
extern void UninitMemoryMapping(Header H);
extern void Uninitdbase(Header H);

extern void SetProtectionReadOnly(char *addr, long length, char *where);
extern void SetProtectionWriteCopy(char *addr, long length, char *where);

#define IS_LOCAL_MUTABLE(pt) (Bytes(pt) >= Bytes(A.M.pointer) && Bytes(pt) < Bytes(A.M.top))

/* The number of pages to be allocated to new databases.  Currently this can
   only be set when running the disc garbage collector. */
extern int dbMutablePages, dbImmutablePages;
#endif
