/*
    Title:      Database garbage collector.

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

#include <stdio.h>
#if !defined(WINDOWS_PC)
#include <unistd.h>
#include <sys/param.h>
#endif
#include <signal.h>
#include <assert.h>
#include <string.h>
#include <errno.h>

/* filemode stuff */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>

#if (defined(FREEBSD) || defined(MACOSX))
#elif defined(WINDOWS_PC)
#include <malloc.h>
#include <io.h>
/* Just use built-in alloca without declaration */
#else
#include <alloca.h>
#endif

#include "globals.h"
#include "mmap.h"
#include "alloc.h"

#include "gc.h"
#include "copygc.h"
#include "version.h"
#include "improve.h"
#include "diagnostics.h"
#include "proper_io.h"
#include "discgc.h"
#include "addresses.h"

#ifdef WINDOWS_PC
#include "Console.h"
#endif


/* imports */

static Header A = 0;
static Header B = 0;
static Header C = 0;

static struct HeaderStruct memHdr;

#if defined(WINDOWS_PC) /* PC version */
#define InstallInterruptHandler() signal(SIGINT,SIG_IGN)   
#elif (defined(SOLARIS2))
#define InstallInterruptHandler() sigignore(SIGINT)
#endif

static void CopyRootA2B(root_struct *(*copy)())
{
  SetRoot ( B,(* copy)(Root(A)) );
}

static void CopyRootB2C(root_struct *(*copy)())
{
  SetRoot ( C,(* copy)(Root(B)) );
}

static void ImproveRootA(void (*improve)())
{
  (* improve)(Root(A));
}

static char *tempfilename   = NULL;
static int temporary_exists = 0;

static void catchINT(int sig)
{
   assert(sig == SIGINT);
   
   if (temporary_exists)
   {
      assert(tempfilename != NULL);
      temporary_exists = unlink(tempfilename);
   }
   
   if (temporary_exists)
   {
      Exit("Interrupted. Quitting. Warning: unable to remove %s",tempfilename);
   }
   else
   {
      Exit("Interrupted. Quitting.");
   }
}

/* CopyGC assumes it is copying between databases.  This constructs a header which looks like
   a database so that we can copy to it and then from it.  */
static Header OpenMemoryDatabase(Header A)
{
	memset(&memHdr, 0, sizeof(memHdr));

	memHdr.page_size = A->page_size;
	strcpy(memHdr.parent, A->parent);
	memHdr.gc_space.parent = A->gc_space.parent;
	memHdr.up = A->up;
	memHdr.gc_const = A->gc_const;

	memHdr.i_space.top = LOCAL_ITOP;
	memHdr.i_space.bottom = memHdr.i_space.pointer = LOCAL_ITOP - (A->i_space.top - A->i_space.bottom);
	memHdr.m_space.top = LOCAL_MTOP;
	memHdr.m_space.bottom = memHdr.m_space.pointer = LOCAL_MTOP - (A->m_space.top - A->m_space.bottom);
	memHdr.m_space.bitmap_bytes = BITMAP_BYTES(memHdr.m_space.pointer - memHdr.m_space.bottom);
	memHdr.i_space.bitmap_bytes = BITMAP_BYTES(memHdr.i_space.pointer - memHdr.i_space.bottom);
	memHdr.i_space.page_table = malloc(memHdr.i_space.bitmap_bytes);
	memHdr.m_space.page_table = malloc(memHdr.m_space.bitmap_bytes);
	memHdr.i_space.max_pages = A->i_space.max_pages;
	memHdr.m_space.max_pages = A->m_space.max_pages;
	memHdr.gc_space.h_bottom = memHdr.gc_space.i_bottom = memHdr.i_space.bottom;
	memHdr.gc_space.i_top = memHdr.i_space.top;
	memHdr.gc_space.m_bottom = memHdr.m_space.bottom;
	memHdr.gc_space.m_top = memHdr.m_space.top;

	return &memHdr;
}

int discgarb(int argc, char **argv)
{
  char     extension[] = "~";
  char    *filename    = NULL;
  int      doSharePhase = 0;
  int      verbose = 0;
  word     total;
  GCConst *a, *b, *c;
  GCSpace *old;
  GCSpace  new;
  int i;
  /* Database size in kBytes. */
  int dbSize = 0; /* Zero means use existing. */
  int dbPages;
  int dbMin = 0, dbMax = 0; /* Set to 1 if -Smin or -Smax are given. */

  /* Buffer to hold stat's about the old database permissions */
  struct stat statbuff;
#ifdef WINDOWS_PC
	int saved_mode;
#else
  mode_t saved_mode;
#endif

  signal(SIGINT,catchINT);

  proper_printf("Database garbage collector version %s\n",
                  poly_runtime_system_version);

  for ( i = 1; i < argc; i++ )
  {
    if ( argv[i][0] == '-' )
    {
      if ( ! strcmp(argv[i],"-d") )
      {
		  /* Skip the discgarb option. */
      }

      else if ( ! strcmp(argv[i],"-c") ) doSharePhase = 1;

      else if ( ! strcmp(argv[i],"-v") ) verbose = 1;

	  else if ( strncmp(argv[i],"-S", 2) == 0 || strncmp(argv[i],"-s", 2) == 0 )
	  {
		  int multiplier = 1;
		  char *p;
		  /* For backwards compatibility -Sn is treated as megabytes while
		     -sn is treated as kilobytes. */
		  if (strncmp(argv[i],"-S", 2) == 0) multiplier = 1024;
		  if (strlen(argv[i]) == 2) { i++; p = argv[i]; } else p = argv[i]+2;
		  if (strcmp(p, "min") == 0) dbMin = 1;
		  else if (strcmp(p, "max") == 0) dbMax = 1;
		  else
		  {
			  int value, converted;
			  char kM;
			  converted = sscanf(p, "%d%c", &value, &kM);
			  if (converted == 1) dbSize = value*multiplier;
			  else if (converted == 2 && kM == 'k') dbSize = value;
			  else if (converted == 2 && kM == 'M') dbSize = value * 1024;
			  else proper_printf("Unknown option %s\n", argv[i]);
		  }
	  }

      else proper_printf("Unknown option %s\n", argv[i]);
    }
    else filename = argv[i];
  }

#ifdef WINDOWS_PC
  /* Try asking for it. */
  if (filename == 0)
  {
		static char filebuff[MAX_PATH];
		if (! getDBFileName(filebuff, MAX_PATH))
			Usage("Usage: No database supplied");
		else filename = filebuff;
  }
#else
  if (filename == NULL)
  {
     Usage("Usage: No database supplied");
  }
#endif

  // If the file name is a symbolic link we will open the database it refers to
  // but write back the changed file to the link not the linked file.  The result will
  // be that the original database will be unchanged.  The easiest way to deal with that
  // is to set "filename" to be the destination of the link.
#ifndef WINDOWS_PC
  if (proper_lstat(filename, &statbuff) != 0) {
     SysError("Can't continue: stat failed on %s", filename);
  }
  else if ((statbuff.st_mode & S_IFMT) == S_IFLNK) {
	  static char resBuf[MAXPATHLEN];
	  int nLen;
	  nLen = readlink(filename, resBuf, sizeof(resBuf)-1);
	  if (nLen < 0) {
	     SysError("Can't continue: readlink failed on %s", filename);
	  }
	  resBuf[nLen] = 0;
	  filename = resBuf;
  }
#endif

  tempfilename = (char *)alloca(strlen(filename) + strlen(extension) + 1);
  strcpy(tempfilename,filename);
  strcat(tempfilename,extension);

  if (proper_stat(filename, &statbuff))
  {
     SysError("Can't continue: stat failed on %s", filename);
  }
  else
  {
     saved_mode = statbuff.st_mode;
  }
  /* Perhaps we should check we have the right permission to carry on? */
  
  if (!proper_stat(tempfilename, &statbuff))
  {
     SysError("Can't continue: %s already exists", tempfilename);
  }

  A = OpenMappedDatabase(filename, M_DISCGARB1, 0);
  
  if (verbose)
  {
    DisplayHeader(A);
  }

  if (dbMin == 1)
  {
	  /* Set the size to the smallest possible.  Actually we may be able to get it
	     smaller because of common sub-expression removal. */
	  dbMutablePages = (A->m_space.pointer - A->m_space.bottom) / PAGEWORDS;
	  dbImmutablePages = (A->i_space.pointer - A->i_space.bottom) / PAGEWORDS;
  }
  else if (dbMax == 1)
  {
	  /* Create a database of the maximum size. */
	  /* With the current allocation scheme the address space is partitioned with the
	     top-level database first and children allocated afterwards.  So the maximum space
	     is simply the space available between the start of this database and the end of the
	     region.  We may be collecting a database using the old allocation method so for
	     security we need to check the parents just in case they are located afterwards.  */
	  word *upperLimit = A->gc_space.h_bottom;
	  int freePages;
	  Header h;
	  for (h = A->up; h != 0; h = h->up)
	  {
		  if (h->i_space.top > upperLimit) upperLimit = h->i_space.top;
	  }
	  freePages = (H_TOP-upperLimit)/PAGEWORDS;
	  /* Deduct overhead size from freePages.  Since the overhead size depends to some
	     extent on the size remaining we may have to loop here. */
	  dbPages = freePages;
	  while (1)
	  {
			int immPageTable, mutPageTable, dbHeaderPages;
			dbMutablePages = dbPages / 9;
			dbImmutablePages = dbPages - dbMutablePages;
			/* We need page tables whose sizes depend on the number of pages. */
		  	immPageTable = ROUNDUP_UNITS(dbImmutablePages, PAGEWORDS);
			mutPageTable = ROUNDUP_UNITS(dbMutablePages, PAGEWORDS);
			dbHeaderPages = H_SIZE + immPageTable + mutPageTable;
			if (dbPages == freePages - dbHeaderPages) break;
			dbPages = freePages - dbHeaderPages;
	  }
  }
  else if (dbSize == 0) /* No explicit size given - use existing sizes. */
  {
	  dbMutablePages = A->m_space.max_pages;
	  dbImmutablePages = A->i_space.max_pages;
  }
  else /* Explicit size given. */
  {
	  /* Set the database size.  We divide the space between mutable and immutable
		 in the ratio 1:8. */
	  dbPages = ROUNDUP_UNITS(dbSize * 1024, PAGEBYTES);
	  dbMutablePages = dbPages / 9;
	  dbImmutablePages = dbPages - dbMutablePages;
  }

  a = &A->gc_const;
  
  if (doSharePhase)
  {
    old = &A->gc_space;
	if (old->parent == 0)
		ImproveSharing (ImproveRootA,a,0,old,verbose);
    else ImproveSharing (ImproveRootA,a,&old->parent->gc_space,old,verbose);
  }

  /* Now copy the database into the local memory area.  This allows us to reuse the
     portion of the address space currently in use by the database. */
  B = OpenMemoryDatabase(A);
  a = &A->gc_const;
  b = &B->gc_const;

  old = &A->gc_space;
  new = NewDataSpaces(B);
  total = CopyGC ((OpRootsFunc)CopyRootA2B, a, b, old->parent ? & old->parent->gc_space : 0, old, &new,
		  B, NewMutable, NewImmutable, 0);
  /* Close and unmap the source database. */
  A->up = 0; /* But not the parents. */
  Uninitdbase(A);

  old = &B->gc_space;
  /* Create the new database, with a temporary name */
  CreateNewCopyDatabase(tempfilename, old->parent/* B->up ?????? */);
  temporary_exists = 1;

  /* Do we need these special flags? */
  C = OpenMappedDatabase(tempfilename,M_WRITABLE | M_DISCGARB2,0);
  c = &C->gc_const;
  
  assert ( b->io_spacing == c->io_spacing );
  assert ( b->nil        == c->nil        );
  
  old = &B->gc_space;
  new = NewDataSpaces(C);
  
  ResetAllocator();
  
  /* Copy the reachable heap from old to new;
     don't bother to restore old afterwards. */
  total = CopyGC ((OpRootsFunc)CopyRootB2C, b, c, old->parent ? & old->parent->gc_space : 0, old, &new,
		  C, NewMutable, NewImmutable, 0);

  /* Close the new database */
  CommitMappedDatabase(C);
  
  /* Rename it */
  proper_printf("Renaming %s to %s\n",tempfilename, filename);
#ifdef WINDOWS_PC
	/* In Windows 95 there doesn't seem to be a way of atomically
	   replacing an existing file.  rename and MoveFile will only work
	   if the destination file does not already exist.  MoveFileEx will
	   do what we want in NT and 98.
	   We delete the old file first and then do the rename. */
	if (unlink(filename))
	{
		SysError("Unable to delete %s", filename);
	}
#endif
  if (rename(tempfilename,filename))
  {
    SysError("Unable to rename %s to %s", tempfilename, filename);
  }
  else temporary_exists = 0;

/* 
 SPF 3/5/95: deleted unnecessary call, which doesn't work
 properly for child databases (because it attempts to remap
 the parent database header). If we have to reinstate this call,
 must specify the M_DISCGARB2 flag to prevent this. 
 
 SPF 3/4/96: reinstated for verbose mode, because we want to
 display the new database header (and CommitMappedDatabase
 unmaps it!).
 
 SPF 4/4/96: we have to do this before we reset the file
 permissions, because otherwise we might not be able to get write
 permission, which M_DISCGARB2 requires (yeuch - must sort that out).
*/
  if (verbose)
  {
     B = OpenMappedDatabase (filename, M_WRITABLE | M_DISCGARB2, 0);
     DisplayHeader(B);
  }

  /* Change the database's file permissions to match the orginal ones */
  /* This works in Windows but is not much practical use since it
     only affects Read/Write permission for the caller and if the caller
	 does not have Read+Write access s/he can't discgarb it. */
  proper_printf("Resetting file permissions on %s\n",filename);
  if (chmod(filename, saved_mode))
  {
     SysError("Warning: couldn't reset file permissions for %s", filename);
  }
  proper_fflush(stdout);

  return 0;
}

int changeparent(int argc,char **argv)
{
  char  *filename;
  char  *parent;
  Header H,P;
#ifdef WINDOWS_PC
  char pathbuff[_MAX_PATH];
#endif

  /* We may have got here either because we were invoked as
     changeParent with two arguments or as poly with three. */
  if (argc == 3 || argc == 4)
  {
    filename = argv[argc-2];
    parent   = argv[argc-1];
  }
  else Usage("Wrong number of arguments");

#ifdef WINDOWS_PC
   /* Get the full path name if only a relative path was provided. */
  parent = _fullpath(pathbuff, parent, sizeof(pathbuff)/sizeof(pathbuff[0]));
  if (parent == NULL)
  {
     Exit("Invalid file name for parent");
  }
#else /* UNIX */
  /* The presence of symbolic links in Unix means there is no
     way of creating the "real" absolute path name from a relative
     path.  It is possible to create an absolute path name by following
     the ".." entries but there were serious problems with this when the
     automounter was used since the next time the directory was mounted it
     might have a different pathname.  It is better to insist on the user
     providing a full path. DCJM 22/3/00 */
  if (*parent != '/')
  {
     Exit("Parent name must be an absolute pathname");
  }
#endif
  
  P = OpenMappedDatabase(parent,0,0);
  H = OpenMappedDatabase(filename,M_WRITABLE,P);
    
  strcpy(H->parent,parent);
  
  CommitMappedDatabase(H);
  
  return 0;
}

