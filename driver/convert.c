/*
    Title:      convert.c

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

/*
I think this was intended to copy my old format database and generate a
new format.  Since PortCopyGC isn't defined anywhere I'm going to remove
this.  DCJM 12/4/00.
*/

#if !defined(WINDOWS_PC) 
#include <unistd.h>
#endif
#include <signal.h>
#include <assert.h>
#include <string.h>
#include <malloc.h>
#include <errno.h>

/* filemode stuff */
#include <sys/types.h>
#include <sys/stat.h>

#if defined(FREEBSD)
#include <stdlib.h>
#elif defined(WINDOWS_PC)
#include <malloc.h>
#include <io.h>
#else
/* Just use built-in alloca without declaration */
#include <alloca.h>
#endif

#include "globals.h"

#if defined(WINDOWS_PC)
/* why? */
#include "machine_dep.h"
#endif

#include "mmap.h"
#include "alloc.h"

/* added SPF 26/10/93 */
#include "gc.h"
#include "copygc.h"
#include "run_time.h"
#include "version.h"
#include "improve.h"
#include "diagnostics.h"
#include "proper_io.h"

/* This should be in copy_gc.h, but that file doesn't exist! */
extern word PortCopyGC
(
  OpRootsFunc OpCopyRoots,    /* copy these roots */
  GCConst  *,
  GCConst  *,
  GCSpace  *,
  GCSpace  *,
  GCSpace  *,
  Header,
  AllocFunc,
  AllocFunc,
  int
);


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

/* imports */

static Header A = 0;
static Header B = 0;

#define InstallInterruptHandler() sigignore(SIGINT)

static void CopyRoot(root_struct *(*copy)())
{
  SetRoot ( B,(* copy)(Root(A)) );
}

/******************************************************************************/
/*                                                                            */
/*      main                                                                  */
/*                                                                            */
/******************************************************************************/
int main (int argc, char **argv)
{
  char     extension[] = "~";
  char    *filename    = NULL;
  word     total;
  GCConst *a;
  GCConst *b;
  GCSpace *old;
  GCSpace  new;
  int i;
  
  /* Buffer to hold stat's about the old database permissions */
  struct stat statbuff;

  char *newfilename;

  signal(SIGINT,catchINT);

  proper_printf("Database conversion program version %s\n",
                poly_runtime_system_version );

  for ( i = 1; i < argc; i++ )
  {
    if ( argv[i][0] == '-' )
    {
      proper_printf("Unknown option %s\n", argv[i]);
    }
    else filename = argv[i];
  }

  if (filename == NULL)
  {
     Exit ("Usage: convert <dbase>");
  }
  else
  {
    int filenamelength = strlen(filename);
    newfilename = (char *)malloc(filenamelength+5);
    strcpy(newfilename,filename);
    strcat(newfilename+filenamelength,"_new");
  }

  tempfilename = (char *)alloca(strlen(filename) + strlen(extension) + 1);
  strcpy(tempfilename,filename);
  strcat(tempfilename,extension);

  if (proper_stat(filename, &statbuff))
  {
     SysError("Can't continue: stat failed on %s", filename);
  }
  /* Perhaps we should check we have the right permission to carry on? */
  
  if (!proper_stat(tempfilename, &statbuff))
  {
     SysError("Can't continue: %s already exists", tempfilename);
  }
  /* Perhaps we should investigate the real reason for stat failure? */

  A = OpenMappedDatabase ( filename,M_DISCGARB1,0 );
  
  a = &A->gc_const;
  
  /* Create the new database, with a temporary name */
  CreateNewCopyDatabase(tempfilename, A);
  temporary_exists = 1;
  
  B = OpenMappedDatabase (tempfilename,M_WRITABLE | M_DISCGARB2,0 );
  b = &B->gc_const;

  assert ( a->nil        == b->nil        );
  assert ( a->io_spacing == b->io_spacing );
#if 0
/* These don't hold for the conversion program! */  
  assert ( a->io_top     == b->io_top     );
  assert ( a->io_bottom  == b->io_bottom  );
#endif
  
  proper_printf("\nA\n");
  DisplayHeader(A);
  proper_printf("\nB\n");
  DisplayHeader(B);
  
  old = &A->gc_space;
  new = NewDataSpaces(B);
  
  ResetAllocator();
  
  /* Copy the reachable heap from old to new;
     don't bother to restore old afterwards. */
  total = PortCopyGC ((OpRootsFunc)CopyRoot, a, b, old->parent, old, &new,
                      B, NewMutable, NewImmutable, 0);
  
  proper_printf("%d bytes copied\n",total * sizeof(word));
  
  CommitMappedDatabase ( B );

  /* Rename it */
  proper_printf("Renaming %s to %s\n", tempfilename, newfilename);
  if (rename(tempfilename,newfilename))
  {
    SysError("Unable to rename %s to %s", tempfilename, newfilename);
  }
  else temporary_exists = 0;

  /* Change the database's file permissions to match the orginal ones */
  proper_printf(" Resetting file permissions on %s\n", newfilename);
  if (chmod(newfilename, (mode_t)(statbuff.st_mode)))
  {
     SysError("Warning: couldn't reset file permissions for %s", newfilename);
  }
  
  return 0;
}
