/*
    Title:      Memory mapping for database

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

/***********************************************************************
 *  DISCGARB 
 * 
 *  THE 2 PHASES OF DISCGARB :
 * 
 *  Phase 1 : the source database has its pages mapped RO 
 *  and then copied to fresh swap pages so that the database can 
 *  then be overwritten. 
 *  
 *  Phase 2 : the target database has its pages mapped RW 
 *  and SHARED so they do not need swap space. The pages are written
 *  out by the operating system 'update' process, and when the file 
 *  is closed in 'commit'.
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Attribute stuff
 *
 ***********************************************************************/

#ifndef __GNUC__
#ifndef __attribute__
#define __attribute__(attribute) /* attribute */
#endif
#endif

/***********************************************************************
 *
 * OS Header Files
 *
 ***********************************************************************/

#if defined(WINDOWS_PC)
/* PC version */
#include <windows.h>
#include <io.h>
#include <stdio.h> /* needed? */
#include <time.h>

#else
/* UNIX version */
#include <sys/types.h> 
#include <sys/stat.h> 
#include <sys/mman.h> 
#include <sys/file.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/uio.h>
#include <sys/param.h>
#include <unistd.h> 
#ifdef MACOSX
#include <limits.h>
#else
#ifndef FREEBSD
#include <values.h>
#endif
#endif
#endif

#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

/***********************************************************************
 *
 * PolyML Runtime System Header Files
 *
 ***********************************************************************/

#include "globals.h"
#include "diagnostics.h"
#include "mm.h"

/* added 26/7/95 SPF to replace in-line constants */
#include "addresses.h"

/* added 18/12/95 SPF for MD_flush_instruction_cache */
#ifndef DONT_FLUSH_CACHE
#include "machine_assembly.h"
#endif

#include "proper_io.h"
#include "mmap.h"
#include "run_time.h"
#include "sighandler.h"

#if (defined(MAP_ANON) && ! defined(MAP_ANONYMOUS))
#define MAP_ANONYMOUS	MAP_ANON
#endif

/* Default paths to search for databases.  These apply if the POLYPATH environment
   variable is not set and the file name does not contain a path separator (/ or \ on
   Windows). */
#ifndef DEFAULT_POLYPATH
#ifdef WINDOWS_PC
#define DEFAULT_POLYPATH	".:C:\\Program Files\\Poly\\Databases"
#else
#define DEFAULT_POLYPATH	".:/usr/lib/poly:/usr/local/lib/poly"
#endif
#endif

#ifdef WINDOWS_PC
typedef char *caddr_t;
#define MAXPATHLEN MAX_PATH 
#endif

#if !defined(MAP_VARIABLE)
#define MAP_VARIABLE 0
#endif

extern int be_silent;

/**********************************************************************
 *
 * Macros to manipulate the file pointer
 *
 **********************************************************************/

/* Set file pointer to end of file */
#if defined(WINDOWS_PC)
#define SetFilePointerEnd(fd) ((long)SetFilePointer((fd),0L,NULL,FILE_END))
#else
#define SetFilePointerEnd(fd) ((long)lseek((fd),0L,SEEK_END))
#endif

/* Set file pointer to posn bytes from beginning of the file */
#if defined(WINDOWS_PC)
#define SetFilePointerPosn(fd,posn) ((long)SetFilePointer((fd),(posn),NULL,FILE_BEGIN))
#else
#define SetFilePointerPosn(fd,posn) ((long)lseek((fd),(posn),SEEK_SET))
#endif

/* Sizes of mutable and immutable segments of databases.  Currently these
   can only be set by options when running the disc garbage collector. */
int dbMutablePages = H_MSPACE_PAGES, dbImmutablePages = H_ISPACE_PAGES;

/**********************************************************************
 *
 * Functions to set page protection
 *
 **********************************************************************/

/* Note 1:
     on WNT, pages allocated by VirtualAlloc can have the
     EXECUTE attribute set e.g. using PAGE_EXECUTE_READ,
     but pages allocated by CreateFileMapping can't so
     we have to use PAGE_READONLY for these. However, the
     underlying PC hardware doesn't appear to support the
     EXECUTE attribute, which is just as well, since we
     need to execute code from file-mapped pages. For
     simplicity, the following macros all use PAGE_READONLY
     even when PAGE_EXECUTE_READ would be more appropriate.
     If the hardware changes, we'll have to rethink this.
     SPF 5/5/95 
   
   Note 2:
     on WNT, the "copy on write" attribute is associated with
     the protection of each individual page. With UNIX, this
     attribute is associated with the mmap call. ???
     SPF 5/5/95  
 */

#if defined(WINDOWS_PC)

void SetProtectionReadOnly(char *addr, long length, char *where)
{
  DWORD oldprot;
  if (VirtualProtect(addr, length,PAGE_EXECUTE_READ,&oldprot) == FALSE)
     SysError("VirtualProtect failed in %s  - Error %d\n", where, GetLastError());
}

/*
In Windows 95 we have to read the database into local storage rather than
mapping it as we do in Windows NT.  The easiest way to handle that is first
to try setting PAGE_WRITECOPY and if that fails try again with PAGE_READWRITE.
DCJM 2/12/99.
*/

void SetProtectionWriteCopy(char *addr, long length, char *where)
{
  DWORD _oldprot;
  if (VirtualProtect(addr,length,PAGE_WRITECOPY,&_oldprot) == FALSE &&
      VirtualProtect(addr,length,PAGE_READWRITE,&_oldprot) == FALSE)
     SysError("VirtualProtect failed in %s  - Error %d\\n",where, GetLastError());
}

void SetProtectionReadWrite(char *addr, long length, char *where)
{
  DWORD _oldprot;
  if (VirtualProtect(addr,length,PAGE_READWRITE,&_oldprot) == FALSE)
     SysError("VirtualProtect failed in %s  - Error %d\\n",where, GetLastError());
}

#else /* UNIX */

void SetProtectionReadOnly(char *addr, long length, char *where)
{
   if (mprotect(addr,length,PROT_READ | PROT_EXEC) != 0)
      SysError("mprotect failed in %s\n",where);
}

void SetProtectionWriteCopy(char *addr, long length, char *where)
{
   if (mprotect(addr,length,PROT_READ | PROT_WRITE | PROT_EXEC) != 0)
      SysError("mprotect failed in %s\n",where);
}

void SetProtectionReadWrite(char *addr, long length, char *where)
{
   if (mprotect(addr,length,PROT_READ | PROT_WRITE | PROT_EXEC) != 0)
      SysError("mprotect failed in %s\n",where);
}

#endif


/**********************************************************************
 *
 * Macros to open files
 *
 **********************************************************************/

#if defined(WINDOWS_PC)
/* Open existing file for read-only access */
#define OpenExistingFileReadOnly(filename) \
           CreateFile((filename),GENERIC_READ,FILE_SHARE_READ,NULL, \
                     OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,(HANDLE) NULL)

/* Open existing file for read and write access */
#define OpenExistingFileReadWrite(filename) \
           CreateFile((filename),GENERIC_READ | GENERIC_WRITE,FILE_SHARE_READ,NULL, \
                     OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,(HANDLE) NULL)

/* Open a new file for read and write access (zap any existing file) */
#define OpenCreateFileReadWrite(filename) \
           CreateFile((filename),GENERIC_READ | GENERIC_WRITE,FILE_SHARE_READ,NULL, \
                     CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL,(HANDLE) NULL)

/* Open a new file for read and write access (but don't zap!) */
#define OpenNewFileReadWrite(filename) \
           CreateFile((filename),GENERIC_READ | GENERIC_WRITE,FILE_SHARE_READ,NULL, \
                     CREATE_NEW,FILE_ATTRIBUTE_NORMAL,(HANDLE) NULL)
#else
/* UNIX */
/* Open existing file for read-only access */
#define OpenExistingFileReadOnly(filename) \
           open((filename),O_RDONLY,0666)

/* Open existing file for read and write access */
#define OpenExistingFileReadWrite(filename) \
           open((filename),O_RDWR,0666)

/* Open a new file for read and write access (zap any existing file) */
#define OpenCreateFileReadWrite(filename) \
          open((filename),O_RDWR | O_CREAT | O_TRUNC,0666)

/* Open a new file for read and write access (but don't zap!) */
#define OpenNewFileReadWrite(filename) \
          open((filename),O_RDWR | O_CREAT | O_EXCL,0666)
#endif

/***********************************************************************
 *
 * Library Function Declarations
 *
 ***********************************************************************/

/***********************************************************************
 *
 *  Forward declarations
 *
 ***********************************************************************/

static int MapFixedZeroes(char *, const long);

/***********************************************************************
 *
 *  Definitions and Statics 
 *
 ***********************************************************************/
#ifdef _DEBUG
/* MS C defines _DEBUG for debug builds. */
#define DEBUG
#endif

#ifdef DEBUG
#define ASSERT(x) assert(x)
#else
#define ASSERT(x)
#endif

/*
 * Used by UNIX version 
 */
#if ! defined(MAP_ANONYMOUS) && ! defined(WINDOWS_PC)
static int Z_fd = -1;
static void OpenZeroDevice(void)
{
  /* Sun-specific */
  if (Z_fd < 0)
  {
     do
     {
		Z_fd = open("/dev/zero",O_RDONLY,0666);
     } while (Z_fd == NULLFD && errno == EINTR);
     
     if (Z_fd < 0)
     {
		SysError("Unable to open /dev/zero");
     }
  }
}
#else
  /* Do nothing - the mechanism above is Sun specific */
#define OpenZeroDevice() { /* do nothing */ }
#endif

#if (defined(SOLARIS2) || defined(LINUX))
#if defined(__GNUC__)
__inline__
#endif
#ifndef MAP_NORESERVE
/* MAP_NORESERVE isn't defined in Linux, nor is it relevant. */
#define MAP_NORESERVE 0
#endif /* MAP_NORESERVE */
static int ReserveAddressSpace(const caddr_t addr, const size_t len)
{
   /* Hold onto this address space - we'll need it later. */
   /* If we wanted to support HPUX, we would have to do this in chunks,
      (see MMAP for details), but we're currently only supporting Solaris.
      SPF 17/2/1998
   */
#if defined(MAP_ANONYMOUS)
   caddr_t pa = mmap((caddr_t)addr, len, PROT_NONE,
   			MAP_PRIVATE | MAP_FIXED | MAP_NORESERVE | MAP_ANONYMOUS, -1, 0L);
#else
   caddr_t pa = mmap((caddr_t)addr, len, PROT_NONE, MAP_PRIVATE | MAP_FIXED | MAP_NORESERVE, Z_fd, 0L);
#endif
   return (pa == addr) ? 0 : -1;
}

static int ReserveMLSpace(caddr_t bottom, caddr_t top)
{
   const unsigned length = (unsigned)top - (unsigned)bottom;

   if (length <= (unsigned)(MAXINT))
   {
      return ReserveAddressSpace(bottom, (size_t)length);
   }
   else
   {
      /* If we have a really large address space, we have to map
         it in two chunks, because "mmap" expects the size to be
         *signed*. That was a mistake in the API design, wasn't it?
         SPF 17/2/1998
      */
      size_t bigchunk  = (size_t)ROUNDDOWN(MAXINT,PAGEBYTES);
      size_t remainder = (size_t)(length - (unsigned)bigchunk);
      assert(bigchunk  <= MAXINT);
      assert(remainder <= MAXINT);
      {
         caddr_t bottom2 = (caddr_t)((unsigned)bottom + (unsigned)bigchunk);
         int res1 = ReserveAddressSpace(bottom,  bigchunk);
         int res2 = ReserveAddressSpace(bottom2, remainder);
         return (res1 | res2);
      }
   } 
}

int ReserveMLSpaces(void)
{  
   OpenZeroDevice ();
   {
      /* int res1 = */ ReserveMLSpace((caddr_t)LOCAL_IBOTTOM, (caddr_t)LOCAL_ITOP);
      /* int res2 = */ ReserveMLSpace((caddr_t)LOCAL_MBOTTOM, (caddr_t)LOCAL_MTOP);
      /* int res3 = */ ReserveMLSpace((caddr_t)IO_BOTTOM,     (caddr_t)IO_TOP);
      /* int res4 = */ ReserveMLSpace((caddr_t)H_BOTTOM,      (caddr_t)H_TOP);
	  /* It seems that ReserveMLSpace may fail if the user (or the system
	     administrator) has set the virtual memory limit.  Now ignore
		 errors. */
      return 0 /*(res1 | res2 | res3 | res4) */;
   }
}

#if 0
#if defined(__GNUC__)
__inline__
#endif
static int GrabReservedSpace(const caddr_t addr, const size_t len)
{
   /* We need the previously-reserved address space now */
   int res = munmap(addr, len);
   return res;
}
#endif

#else /* not SOLARIS2 */

int ReserveMLSpaces(void)
{
   return 0;
}

#endif /* not SOLARIS2 */


#if !defined(WINDOWS_PC)


/*------------------------------------------------------------------------------
INDEX: MMAP
  mmap sometimes fails when there is insufficient swap available.
  It is undesireable to crash out of the ML, losing work, because this could be
  recovered if the user closed some other application.

  MMAP tries to do mmap and if it fails it warns the user, waits, and
  retries.
------------------------------------------------------------------------------*/


/*------------------------------------------------------------------------------
INDEX: sleepSeconds
------------------------------------------------------------------------------*/

/* Simon's version */
static void sleepSeconds(const unsigned n)
{
  time_t now  = time((time_t *)NULL);
  time_t then = now + n;

  /* Need to loop because sleep can
     terminate before the required time
     has expired (due to interrupts). */
  while (now < then)
  {
     sleep(then - now);
     now = time((time_t *)NULL);
  }
}


static caddr_t MMAP
  (      caddr_t addr,
   const size_t len,
   const int prot,
   const int flags,
   const int fd,
   const off_t off)
{ 
  caddr_t pa;
  int retry_delay = 2; /* initial retry delay on entry to MMAP */
  
#if defined(EXTRA_DEBUG)
  proper_fprintf(stderr, "MMAP; addr=%p, len=%08x, prot=%08x, flags=%08x, fd=%i, off=%08lx\n",
                 addr, len, prot, flags, fd, off);
#endif

  /* James's work-around for running out of virtual memory */
  while (((pa = mmap(addr, len, prot, flags, fd, off)) == Bytes(-1)) && retry_delay <= 4)
  {
    proper_fprintf(stderr, "** MMAP: Unable to map file: %d to address "ZERO_X"%p\n", (int)off, addr);
    proper_fprintf(stderr, "** errno=%d: %s\n", errno, syserror(errno));
    proper_fprintf(stderr, "** This may be due to insufficient swap space, will retry in %d seconds ...\n", retry_delay);
    sleepSeconds(retry_delay);
    proper_fprintf(stderr,"\n");
    retry_delay *= 2; /* double retry delay for each iteration of this loop */
  }
  
  if (pa == Bytes(-1))
  {
    proper_fprintf(stderr, "** MMAP: too many unsuccessful retries - giving up\n");
  }
   
#ifdef EXTRA_DEBUG
  if (pa != addr)
  {
     proper_fprintf(stderr, "MMAP; addr=%p, pa=%p\n", addr, pa);
  }
#endif

  return pa;
}

#if defined(__GNUC__)
__inline__
#endif
static int MUNMAP(caddr_t addr, const size_t len, int hold)
{ 
   int res;
#if defined(EXTRA_DEBUG)
   proper_fprintf(stderr, "MUNMAP; addr=%p, len=%08x, hold=%i\n", addr, len, hold);
#endif
#if (defined(SOLARIS2))
   if (hold)
   {
      /* Hold onto this address space - we'll need it again. The call
	 to mmap in ReserveAddressSpace performs an implicit munmap,
	 so we don't need an explicit munmap call here.
	 SPF 17/2/1998
      */
      res = ReserveAddressSpace(addr, len);
   }
   else
#endif
   {
      res = munmap(addr, len);
   } 
   return res;
}

#endif /* not WINDOWS_PC */

static char *MM_MAGIC = "mm17";

#if defined(WINDOWS_PC)

/* 
 * We do NOT use the "execute" form of the following flags 
 * because they are used by CreateFilemapping, which doesn't allow 
 * it. 
 */
 
 /* Legal combinations:
      PF + RC (mutables in databases, heap)
      PF + RO (immutables in databases)
      SF + RW (DISCGARB2 only)
      SF + RO (not used - could be used for immutables in databases,
               if we didn't have to worry about profiling)
 */
 
/*******************************************************************
  Memory mapping under Windows 95

  The page size is 0x1000               (as in Windows NT)
  The allocation granularity is 0x10000 (as in Windows NT)

  The ML database is mapped in pages of size PAGEBYTES (0x10000) 


  Windows 95 differs from Windows NT :

    VirtualAlloc (used under Windows NT to zero-initialise pages)
	and MapViewOfFile(Ex) use different address spaces.
	
    VirtualAlloc - addresses in range     0x00400000 - 0x7FFFFFFF

    MapViewOfFile(Ex) -addresses in range 0x80000000 - 0xBFFFFFFF
	visible by all Win32 processes - potential problems ?
	
	Zero-initialised pages are obtained using CreateFileMapping with
	a handle of 0xFFFFFFFFFF which creates pages that are memory
	mapped to the swap file.

  95 BUG : UnmapViewOfFile should return bool if called with an address
  previously returned by MapViewOfFile(Ex). What it actually does is
  return FALSE on failure and a random pointer value on SUCCESS.
  So failure test is implemented with in(equality) with FALSE. 

  95 BUG : MapViewOfFile should return an address which is a multiple
  of the system allocation granularity. It DOES NOT !
  So the implementation of Choose Address is different to return
  a valid address. If n pages are needed we call MapViewOfFile with
  the length of (n+1) pages and return the address returned rounded
  up to meet the system allocation granularity.

  95 Feature (documented) : If MapViewOfFile cannot find a region
  large enough to contain the entire file-mapping object, it fails,
  regardless of the view requested.
  We cannot use multiple mapping objects to map different views of
  the file in successive pages. So a unique mmap object is obtained
  with CreateFileMapping for every file (ML dbase). 

  The type mmapped (int under UNIX and HANDLE under Windows NT) has
  been implemented as a structure which contains a handle to the
  file (hf) and a handle to the file mapping object (hmap).

  Windows 95 (being less safe than Windows NT) does not support 
  all the protection - mode access combinations.
  ONLY legal combinations under Windows 95 are (tested) :
  (else error 87 = ROR_INVALID_PARAMETER returned) 

	  PAGE_READONLY  (RO) with ()   FILE_MAP_READ

      PAGE_READWRITE (RW) with ()   FILE_MAP_READ
      PAGE_READWRITE (RW) with (SF) FILE_MAP_WRITE

      PAGE_WRITECOPY (RC) with (PF) FILE_MAP_READ
      PAGE_WRITECOPY (RC) with (PF) FILE_MAP_COPY
  
  Combinations used for ML dbase :
	
      PF + RC                (mutables in databases, heap)
      PF + RC (RO under WNT) (immutables in databases)
      SF + RW                (DISCGARB2 only)

  The ML dbase areas used under W95 are (see addresses.h) :

  IO_BOTTOM  Words(0x9D000000)
  IO_TOP     Words(0x9D002000)

  32MB immutable local heap
  LOCAL_IBOTTOM   Words(0x91000000) 
  LOCAL_ITOP      Words(0x93000000)
  16MB mutable local heap
  LOCAL_MBOTTOM   Words(0x93000000)
  LOCAL_MTOP      Words(0x94000000)

  Database areas start above the local heap 
  H_BOTTOM        LOCAL_MTOP

  and extend as far as the IO area.
  H_TOP           IO_BOTTOM

  Windows 95 : need to be careful and close all open handles
               when not needed because they cause dangerous
			   memory leaks.

  Common runtime errors in debugging :
    6   : ERROR_INVALID_HANDLE    (should check for valid handles)
	8   : ERROR_NOT_ENOUGH_MEMORY 
	87  : ERROR_INVALID_PARAMETER (usually flag)
	487 : ERROR_INVALID_ADDRESS


   On Windows 95, once the initial address for the
   view is determined, additional views of the mapping will be mapped to
   the same address in each process anyway, and there is no benefit in
   trying to force the initial mapping to a specific address. For the
   second and subsequent views of a mapping object, if the address
   specified does not match the actual address where Windows 95
   has mapped the view, then MapViewOfFileEx() fails, and GetLastError()
   returns ERROR_INVALID_ADDRESS (487). Additionally, when attempting to
   map the first view at a pre-determined address, that address may already
   be in use by other components of Windows 95 which use the shared virtual
   arena. Future updates to Windows 95 may change the mapping range to 0-2 GB,
   as on Windows NT.

   - This causes problems when we open a mapped database ...

 Panos 22/02/96

 After a discussion with Simon it looks that W95 does not support memory
 mapped files the way we want to use them for the ML database.
 The main problem is that W95 do not allow views of a file to be
 mapped at arbitrary addresses.

 The solution is to use VirtualAlloc and ReadFile
 and use the address space in the range : 0x00400000 to 0x7FFFFFFF

 Flags used for ML dbase are now :
	
  RW (mutables in databases, heap)
  RO (immutables in databases)
  RW (DISCGARB2)

  The ML dbase areas used under W95 are the same as those 
  used by NT (see addresses.h).

 Panos 26/02/96
 
 The WNT/W95 version still has a lot of references to the PAGEBYTES.
 Since I'm not currently maintaining this version, I'm not going
 to try to fix this. SPF 22/5/97


DCJM 7/7/99.  I've made a number of changes so not all of the
above is true any longer.  I think it's important to be able to
use the same driver and database on NT and 95 so it now attempts
to map the database at the addresses used by NT.  That will
succeed in NT but probably fail in 95 so the fall-back
is to allocate local memory at the address and read the database
in.  This means that the start-up time is longer for Windows 95
than for NT but otherwise it all works.
 ******************************************************************/
 
#define RW PAGE_READWRITE  /* obsolete */  
#define RO PAGE_READONLY   /* read-only */
#define RC PAGE_WRITECOPY  /* read-write. */
#define PF 0	 /* Unused */
#define SF 0	 /* Unused */

#else
/* UNIX version */
#define RW (PROT_READ   | PROT_WRITE | PROT_EXEC) /* only used with SF in DISCGARB phase2 */
#define RC (PROT_READ   | PROT_WRITE | PROT_EXEC) /* read-write (private); used with PF */
#define RO (PROT_READ   | PROT_EXEC)
#define PF (MAP_PRIVATE | MAP_FIXED)
#define SF (MAP_SHARED  | MAP_FIXED)
#endif

/* Bottom(of Space) + n words(int) */
#define GET_PAGE_ADDRESS(s,n,page_bytes) ((void *)(Bytes((s)->bottom) + (n) * (page_bytes)))
#define GET_PAGE_NUMBER(s,p,page_bytes)((unsigned) ((Bytes(p) - Bytes((s)->bottom)) / (page_bytes)))
#define GET_WORDNUMBER(s,p) ((p) - (s)->bottom)
#define IS_MUTABLE_SPACE(s) (s->type == Mutable)

/***********************************************************************
 *
 *  FAILURE FUNCTIONS 
 *
 ***********************************************************************/

void _Assert(msg,file,line)
char *msg;
char *file;
int   line;
{
  Crash("Assertion %s failed at %s:%d",msg,file,line);
}

/***********************************************************************
 *
 *  CheckStack - used in the UNIX version 
 *
 ***********************************************************************/

#if !defined(SOLARIS2)
#define CheckStack(high) while(0){}
#else /* SOLARIS2 version  */

static void StackTooBig(void)
{
  Exit("Stack size too big - reduce your limit");
}

/* Modified to use unsigned comparisons 12/11/93 SPF */
static void CheckStack(const char *high)
{
  struct rlimit rlimit;
  unsigned stack;

  getrlimit(RLIMIT_STACK,&rlimit);
  
  if (rlimit.rlim_cur == RLIM_INFINITY)
  {
    proper_printf("Assuming a 1Gb stack limit\n");
    rlimit.rlim_cur = MBytes(1024);
  }
  
  stack = (unsigned)(&stack);
  
  if (stack < (unsigned)rlimit.rlim_cur)
  {
    StackTooBig();
  }
  
  
  stack -= (unsigned)rlimit.rlim_cur;
  
  /* Take off some more for shared libraries */
  if (stack < MBytes(6))
  {
     StackTooBig();
  }
  
  stack -= MBytes(6);
  
  if (stack <= (unsigned)high)
  {
     StackTooBig();
  }
}
#endif /* Sun */

/**********************************************************************
 *
 * MEMORY MAPPING for WINDOWS NT
 *
 **********************************************************************/

/* 
 * Memory is allocated in pages (1 page = x10000 = 64Kb) and
 * each page (the whole page) is at a particular state at runtime.
 */


/* 
 * the following is used for debugging purposes under Windows NT
 */

#ifdef WNTDEBUG
void PrintRealState(char *addr,char *funcall)
{
  MEMORY_BASIC_INFORMATION info;
  VirtualQuery(addr,&info,sizeof(info));
 
  proper_printf("a=%x,len=%x,p1=", info.BaseAddress,info. RegionSize);
  switch(info.AllocationProtect)	/* initial protection */
   {
    case PAGE_READONLY          : proper_printf("R  "); break;
    case PAGE_READWRITE         : proper_printf("RW "); break;
    case PAGE_WRITECOPY         : proper_printf("WC "); break;
    case PAGE_EXECUTE           : proper_printf("E  "); break;
    case PAGE_EXECUTE_READ      : proper_printf("ER "); break;  
    case PAGE_EXECUTE_READWRITE : proper_printf("ERW"); break;  
    case PAGE_EXECUTE_WRITECOPY : proper_printf("EWC"); break;  
    case PAGE_NOACCESS          : proper_printf("(/)"); break;
    default                     : proper_printf("???"); break;
   }
  proper_printf(",p2=");
  switch(info.Protect)  /* current protection */
   {
    case PAGE_READONLY          : proper_printf("R  "); break;
    case PAGE_READWRITE         : proper_printf("RW "); break;
    case PAGE_WRITECOPY         : proper_printf("WC "); break;
    case PAGE_EXECUTE           : proper_printf("E  "); break;
    case PAGE_EXECUTE_READ      : proper_printf("ER "); break;  
    case PAGE_EXECUTE_READWRITE : proper_printf("ERW"); break;  
    case PAGE_EXECUTE_WRITECOPY : proper_printf("EWC"); break;  
    case PAGE_NOACCESS          : proper_printf("(/)"); break;
    default                     : proper_printf("???"); break;
   }
  proper_printf(",st=");	/* state of pages */
  switch(info.State)
   {
    case MEM_COMMIT  : proper_printf("C"); break;
    case MEM_FREE    : proper_printf("F"); break;
    case MEM_RESERVE : proper_printf("R"); break;
    default          : proper_printf("?"); break;
   }
  proper_printf(",t=");
  switch(info.Type) /* type */
   {
    case MEM_IMAGE   : proper_printf("IM"); break;
    case MEM_MAPPED  : proper_printf("MM"); break;
    case MEM_PRIVATE : proper_printf("PR"); break;
    default          : proper_printf("??"); break;
   }
  proper_printf(",fun=%s\n",funcall);
}
#endif

#if defined(WINDOWS_PC)

/* find a region of the address space big enough for our allocation */
void *ChooseAddress(const int bytes_to_allocate)
{
/* first reserve a region of virtual address space in a single chunk,
   to ensure that we get contiguous memory. */
  void *addr = VirtualAlloc(NULL,bytes_to_allocate,MEM_RESERVE,RW);
  if (addr == NULL)
   {
      SysError("VirtualAlloc failed in ChooseAddress, bytes_to_allocate = %i\n",
                bytes_to_allocate);
   }

/* next, free the space, so we can allocate it in single page chunks,
   which are much easier to map and unmap */
 {
   BOOL b = VirtualFree(addr, 0, MEM_RELEASE);
   if (b != TRUE)
   {
      SysError("VirtualFree failed in ChooseAddress, addr = %8x\n",
                addr);
   }
 }

/* finally, return the address */
 return addr;
}

/***********************************************************************
 *
 *  Mmap & Munmap for Windows NT & Windows 95
 *
 ***********************************************************************/

char *PCmmap
(
        char *addr,
  const long length,
  const int prot,
  const MAPPEDFD fd,
  const unsigned long posn,
  const char *call)
{
	HANDLE hmapObject = NULL;
	char	*pageaddr = 0;

	ASSERT(addr != NULL);

#ifdef WNTDEBUG
	proper_printf("PCmmap at %x,length=%d,%i pages,prot=%x,mode=%x\n",
	          addr, length, pages, prot, mode);
#endif
 
	hmapObject = CreateFileMapping(fd,NULL,prot,0,0,NULL);
	if (hmapObject == NULL) goto Exit;

#ifdef WNTDEBUG
     proper_printf("before-");
     PrintRealState(addr,"PCMMAP");
#endif 

    pageaddr = MapViewOfFileEx(hmapObject,
		prot == PAGE_READONLY ? FILE_MAP_READ : FILE_MAP_COPY,0, posn,length,addr);

	if (pageaddr == 0)
	{
		if (GetLastError() == ERROR_INVALID_ADDRESS)
		{
		/* MapViewOfFileEx may fail because we are mapping to the wrong address,
		   particularly if we are running on Windows 95 which uses a different
		   region from Windows NT.  */
			long noread;
			/* Reserve space for the whole 64k segment.
			   Map this a page at a time because it's possible that some of
			   the pages may already be mapped but not others. */
			char *endspace = (char *)ROUNDUP((int)addr+length, PAGEBYTES);
			char *startspace = (char *)ROUNDDOWN((int)addr, PAGEBYTES);
			while (startspace < endspace)
			{
				if (! VirtualAlloc(startspace, PAGEBYTES, MEM_RESERVE|MEM_COMMIT, RW))
					goto Exit;
				startspace += PAGEBYTES;
			}
			if (SetFilePointerPosn(fd,posn) != (long)posn) goto Exit;

			if (! ReadFile(fd,addr,length,&noread,NULL)) goto Exit;
			pageaddr = addr;
		}
	}

#ifdef WNTDEBUG
    proper_printf("after -");
    PrintRealState(addr,"PCMMAP");
#endif

Exit:
	{
		int nErr = GetLastError();
		/* Make sure this handle is closed. */
		CloseHandle(hmapObject);
		SetLastError(nErr);
		return pageaddr;
	}
}

void PCmunmap(const char *addr, long length, const char *call)
{
	/* This may be used to free mapped or allocated pages.  We have to find out
	   which.  */
	MEMORY_BASIC_INFORMATION	memInfo;
	BOOL b;
	/* According to the documentation, it should be possible to deallocate
	   multiple regions in a single call to VirtualFree.  This seems not
	   to work, perhaps because we reserve memory a page at a time.  It
	   would be much better to separate reserving memory from committing it
	   but for the moment they are done together.  We are therefore forced
	   to deallocate memory a page at a time. */
	while (length > 0)
	{
#ifdef WNTDEBUG
		proper_printf("\nbefore-");
		PrintRealState(addr,"MUNMAP");
#endif
		if (! VirtualQuery(addr, &memInfo, sizeof(memInfo)) ) {
			 SysError("PCmunmap failure (VirtualQuery),%s,addr=%x, code = %ld\n",
					  call,addr,GetLastError());
		}
		if (memInfo.State == MEM_FREE) {
			/* This can happen when called from CommitSpace.  It seems that
			   when we extend the database we allocate a whole 64k chunk but
			   only the pages that have been modified are included in RegionSize
			   for the last 64k chunk.  UnmapViewOfFile, though, always frees a
			   whole 64k chunk so the remaining pages are now marked as free.  */
			; /* Do nothing */
		}
		else if (memInfo.Type == MEM_MAPPED) {
			/* Memory mapped area.  Free using UnmapViewOfFile.  */
			b = UnmapViewOfFile((LPVOID)addr);
			if (! b) {
				 SysError("PCmunmap failure (UnmapViewOfFile),%s,addr=%x, code = %ld\n",
						  call,addr,GetLastError());
			}
			/* Although memInfo.RegionSize may be less than 64k UnmapViewOfFile will
			   always free a whole 64k chunk. */
		}
		else {
			DWORD dwFree = memInfo.RegionSize;
			if ((DWORD)length < dwFree) dwFree = (DWORD)length;
			/* The region size is the size of memory with the same page
			   attributes and may be bigger than we want. */
			b = VirtualFree((LPVOID)addr, dwFree, MEM_DECOMMIT);
			if (! b) {
				 SysError("PCmunmap failure (VirtualFree),%s,addr=%x, code = %ld\n",
						  call,addr,GetLastError());
			}
			/* If this is really local store we don't need to release the memory but
			   if this has been allocated to write it to the database we
			   must release it in order to be able to map it back in again. */
			/* Changed this somewhat.  We now reserve local memory so we
			   also have to release it as well. */
			if (addr == memInfo.AllocationBase/* && (DWORD)length == memInfo.RegionSize*/) {
				b = VirtualFree((LPVOID)addr, 0, MEM_RELEASE);
				if (! b) {
					 SysError("PCmunmap failure (VirtualFree),%s,addr=%x, code = %ld\n",
							  call,addr,GetLastError());
				}
			}
		}
#ifdef WNTDEBUG
		proper_printf("after -");
		PrintRealState(addr,"MUNMAP");
#endif
		length -= memInfo.RegionSize;
		addr += memInfo.RegionSize;
	}
}
#endif

/***********************************************************************
 *
 *  OS dependent functions 
 *
 ***********************************************************************/
void CloseFD(const MAPPEDFD fd, const char *filename, const char *where)
{
#if defined(WINDOWS_PC) 
  if (CloseHandle(fd) == FALSE)
  {
    SysError("Close failed on file %s, at %s\n",filename,where);
  }
#else
  if (close(fd) != 0)
  {
    SysError("Close failed on file %s, at %s\n",filename,where);
  }
#endif        
}

void ReadFD
(
  const MAPPEDFD fd,
        char    *buffer,
  const long     size,
  const char    *filename
)
{
#if defined(WINDOWS_PC)
  long noread;
  
  /* This message is confusing - file may locked by another application */
  if (ReadFile (fd,buffer,size,&noread,NULL) == FALSE)
    Exit("%s is too short to be a mapped database",filename);
#else  /* UNIX version */
 if (read(fd,buffer,size) != size)
    Exit("%s is too short to be a mapped database",filename);
#endif
}

void FileTruncate(const MAPPEDFD fd, const long length)
{
#if defined(WINDOWS_PC)
  if (SetFilePointerPosn(fd,length) == 0)
     SysError("ftruncate stage1 failed\n");
     
  if (SetEndOfFile(fd) == FALSE)
     SysError("ftruncate stage2 failed\n");
#else
   if (ftruncate(fd,length) != 0) 
      SysError("ftruncate failed\n");
#endif
}	 



/* 
 * Allocate byte_count bytes of ZERO memory 
 */
 
#if defined(WINDOWS_PC)
static word *AllocateZeroes(const int byte_count)
{
  char *addr;
  ASSERT(byte_count > 0);

  /* first reserve a region of virtual address space. */
  addr = (char *)ChooseAddress(byte_count);

  /* then simply map zeroes into it */
  {
     int res = MapFixedZeroes(addr, byte_count);

     if (res != 0)
     {
        SysError("Cannot map to address %p", addr);
     }
  }

#ifdef EXTRA_DEBUG
  proper_fprintf(stderr, "allocated 0x%x floating bytes starting at address %p\n",
                  byte_count, addr);
#endif  

  ASSERT(ALIGNED(addr));
  return Words(addr);
}

#elif ! defined(MAP_ANONYMOUS)
/* Sun-specific version */
static word *AllocateZeroes(const int byte_count)
{
  char *addr;
  ASSERT(byte_count > 0);

  /* found in sys/mman.h */
  addr = MMAP(0,byte_count,RW,MAP_PRIVATE,Z_fd,0L); 
                      
  if (addr == Bytes(-1))
  {
     SysError("mmap failed in AllocateZeroes(), mmap !!!\n");
  }

  ASSERT(ALIGNED(addr));
  return Words(addr);
}

#else
/* Normal UNIX version */
static word *AllocateZeroes(const int byte_count)
{
  char *addr;
  ASSERT(byte_count > 0);

  /* Try to put the space near the I/O area i.e. NOT where we want to extend the heap */
  addr = MMAP((caddr_t)IO_TOP, byte_count, RW, MAP_PRIVATE | MAP_VARIABLE | MAP_ANONYMOUS, -1, 0L);
                      
  if (addr == Bytes(-1))
  {
     SysError("mmap failed in AllocateZeroes(), mmap !!!\n");
  }

  ASSERT(ALIGNED(addr));
  return Words(addr);
}
#endif


/*
 * Free byte_count bytes starting at address zeroes
 */
static void FreeZeroes(const word *zeroes, const int byte_count, const int hold)
{
  ASSERT(byte_count > 0);

#if defined(WINDOWS_PC)
  PCmunmap(Bytes(zeroes),byte_count,"FreeZeroes");
#else
  /* UNIX version */
  if (MUNMAP(Bytes(zeroes),byte_count,hold) != 0)
  { 
    SysError("munmap failed in FreeZeroes,mmap \n");
   }
#endif
}

/*
 * Map byte_count bytes starting at pointer and initialise 
 * to ZERO.
 */
/*
   This seems to be used both for local memory and also to create new
   pages to be written to the database during commit.  
*/
#if defined(WINDOWS_PC) 
static int MapFixedZeroes(char *pointer, const long byte_count)
{
  ASSERT(byte_count > 0);
  {
    int i;
    int pages = ROUNDUP_UNITS(byte_count,PAGEBYTES);
  
#ifdef WNTDEBUG
    proper_printf("MapFixedZeroes at %x,%i pages\n", pointer, pages);
#endif
  
    /* Allocate the pages one at a time.  This seems to be necessary
	   because Windows keeps track of the base address used both for
	   reserving and for committing memory and will only allow
	   deallocation on the those boundaries.  We want to be able to allocate
	   and deallocate individual pages.  This is really only an issue if
	   we want to combine allocation (committing) with reserving memory.  */
    for(i = 0 ; i < pages ; i++)
    {
		char *base = pointer + i * PAGEBYTES;
		char *addr;
#ifdef CONVERTING
		/* Reserve the space first.  Ignore the error because it may already
		   have been reserved.  Should really reserve the whole address space
		   at the start. */
		VirtualAlloc(base, PAGEBYTES, MEM_RESERVE, RW);
		/* Now actually allocate the store. Allow allocation of less
		   than a full page.  This simplifies conversion of Linux
		   databases. */
		addr = VirtualAlloc(base,
					byte_count > PAGEBYTES ? PAGEBYTES: byte_count,
					MEM_COMMIT, RW);
#else
		addr = VirtualAlloc(base, PAGEBYTES, MEM_RESERVE|MEM_COMMIT, RW);
#endif
		if (addr != base)
		{   
			if (i > 0) FreeZeroes((const int*) pointer, i * PAGEBYTES, 0);
			return -1;
        }
    }
    return 0;
  }
}



#elif ! defined(MAP_ANON)
/* Sun-specific use of Z_fd */
static int MapFixedZeroes(char *pointer, const long byte_count)
{
  ASSERT(byte_count > 0);
  {
     char *addr = MMAP(pointer,byte_count,RW,PF,Z_fd,0L); /* RC? */
     return (addr != pointer);
  }
}
#else
/* UNIX version */
static int MapFixedZeroes(char *pointer, const long byte_count)
{
  ASSERT(byte_count > 0);
  {
    char *addr = MMAP((caddr_t)pointer,byte_count,RW,PF | MAP_ANONYMOUS, -1,0L);
    return (addr != pointer);
  }
}
#endif

/***********************************************************************
 *
 * MapIn maps (file->memory) length bytes 
 * from file fd at address addr
 *
 * The parameters passed in are :
 * filename - name of file database
 * addr     - starting address
 * length   - number of bytes to map
 * prot	    - protection flags
 * mode	    - type of access
 *            PC version allows specific prot*mode combinations 
 * fd	    - file descriptor int(UNIX) HANDLE(PC)
 * posn	    - file offset where mapping	is to begin
 *            In PC version this offset must be a multiple
 *            of the system's memory allocation granularity
 *
 ***********************************************************************/
static void MapIn
(
  const char *filename,
        char *addr,
  const long  length,
  const int   prot,
  const int   mode,
  const MAPPEDFD fd,
  const unsigned long posn
)
{
  char *base;

#if defined(WINDOWS_PC)
  /* Just map directly to the required address */
  base = PCmmap(addr,length,prot,fd,posn,"MapIn2");
  if (base != addr)
  { 
     SysError("Unable to map %s:%d to addr 0x%08x, base=0x%08x, code = %ld\n",
	      filename,posn,addr,base,GetLastError());
  }
#else /* UNIX version */
  /* Just map directly to the required address */
  base = MMAP(addr,length,prot,mode,fd,posn);

  if (base != addr)
  {
    SysError("Unable to map %s:%d to address 0x%08x",filename,posn,addr);
  }
#endif  
}

/***********************************************************************
 *
 * ReadIn is like MapIn, except that it reads the specified file segment
 * rather than mapping it - this appears to be important for those
 * operating systems (HPUX!) that get confused when we use a mixture of
 * mapping and writing. Since we're using temporary store, we don't need
 * the prot and mode flags. (We just make the buffer area always writable.)
 *
 * The parameters passed in are :
 * filename - name of file database
 * addr     - starting address
 * length   - number of bytes to map
 * fd	    - file descriptor int(UNIX) HANDLE(PC)
 * posn	    - file offset where mapping	is to begin
 *            In PC version this offset must be a multiple
 *            of the system's memory allocation granularity
 *
 ***********************************************************************/
static void ReadIn
(
  const char *filename,
        char *addr,
  const long  length,
  const MAPPEDFD fd,
  const unsigned long posn
)
{
   int res = MapFixedZeroes(addr, length);
  
   if (res != 0)
   {
      SysError("Cannot map to address %p", addr);
   }
  
   if (SetFilePointerPosn(fd, posn) != (long)posn)
   {
     SysError("SetFilePointerPosn %s:%d failed",filename,posn);
   }
 
   ReadFD(fd, addr, length, filename);

  /* prot? mode? */  
}



/*************************************************************************
 * 
 * Writes length bytes from buffer to the file with
 * file descriptor fd (HANDLE in PC, int in UNIX) 
 *
 *************************************************************************/
static void Write
(
  const char *filename,
  const MAPPEDFD fd,
  const char *buffer,
  const long  length
)
{
#if defined(WINDOWS_PC)
   /* Number of bytes written. If WriteFile 
     succeeds nowritten = length */
   DWORD nowritten; 
   BOOL success = WriteFile(fd,buffer,length,&nowritten,NULL);
  
   if (success == FALSE)
   {
     SysError("Write %s:%d failed",filename,length);
   }

#else /* UNIX version */
  /* It is implemented this way to allow for interrupts in
     the write operation. */
  for (;;)
  {
    long w = write(fd,buffer,length);
    if (w == length) break;
    if (w == -1 && errno == EINTR) continue;
    SysError("write %s:%d failed",filename,length);
  }
#endif
}

/***************************************************************************
 *
 * WriteOut maps out (memory->file) length bytes to file fd 
 * starting at address addr and file offset posn
 *
 * filename - name of file 
 * addr     - starting address to map out from
 * length   - no of bytes to map out
 * fd       - file descriptor
 * posn     - file offset - position of file pointer
 *
 * This function has PAGEBYTES hard-coded in, but that doesn't matter, since
 * it's only being used to set the size of a buffer. Actually, it might well
 * be better to make the buffer somewhat larger? SPF 22/5/97
 * 
 * MJC's comment:
 * 
 * I have tried several versions of this function.  Initially I used
 * bcopy to copy to SHARED mappings of the file pages.  This took a lot
 * of system time, presumably because I was copying between two
 * different mappings of the same pages. Then I tried lseek and write
 * and this was 3 times quicker, but still used some system time.  Now I
 * copy the mapped pages to a static buffer first, then use lseek and
 * write to copy then to the file. This is the quickest and uses very
 * little system time. It also has the advantage of extending the file
 * at the same time so I don't need ftruncate to do this for me.
 * 
 ***************************************************************************/
static void WriteOut
(
  const char *filename,
  const char *addr,
  const long  map_length,
  const MAPPEDFD fd,
  const long  posn
)
{
  static char buffer[PAGEBYTES] = { 0 };
  int length = map_length;

#ifdef EXTRA_DEBUG
  proper_fprintf(stderr, "setting file pointer to byte %08lx\n", posn);
#endif

  if (SetFilePointerPosn(fd,posn) != posn)
  {
    SysError("SetFilePointerPosn %s:%d failed",filename,posn);
  }

#ifdef EXTRA_DEBUG
  proper_fprintf(stderr, "writing %08x bytes from address %p\n", length, addr);
#endif

  while(length)
  {
    long n = (length > PAGEBYTES) ? PAGEBYTES : length;
    
    memcpy(buffer,addr,n);

    Write(filename,fd,buffer,n);
    
    length -= n;
    addr   += n;
  }
}
  
/*
 * GrowSpace
 * H      - Header (structure found in mm.h)
 * s      -	Space (structure found in mm.h)
 * pages  - number of pages to allocate
 */
/* Called from alloc. */
int GrowSpace(const Header H, Space *s, const int words_requested)
{
  int page_bytes = H->page_size;
  int bytes_requested = words_requested * sizeof(word);
  int bytes_allocated = ROUNDUP(bytes_requested, page_bytes);
  int words_allocated = bytes_allocated / sizeof(word);
  
  word *old   = s->pointer;
  word *new   = s->pointer + words_allocated;
  
  assert(s->bottom <= old && old <= s->top);
  if (new > s->top) Exit("No more room for pages in database. Try running discgarb.");
  assert(s->bottom <= new && new <= s->top);

  {
   /* Not in DISCGARB2 phase :
      initialise bytes memory starting at address Bytes(old)
      N.B. no need to call MapFixedZeroes for each page (on PC),
           because MapFixedZeroes itself is now breaks responsible
           for breaking the request into its constituent pages.
           SPF 4/5/95 */
    int res = MapFixedZeroes(Bytes(old),bytes_allocated);
    if (res != 0)
    {
       SysError("Cannot map to address %p", old);
    }

  }
  
  /* The space pointer now points to the new address */
  s->pointer = new;
    
  return words_allocated;
}


/*********************************************************************
 *
 * Return the maximum of mutable top and immutable top
 * in the Header.
 *
 *********************************************************************/
static word *Highest(const Header H)
{
  word *high = H->gc_const.io_top;
  
  if (H->i_space.top > high) high = H->i_space.top;
  if (H->m_space.top > high) high = H->m_space.top;
  
  return high;
}

/*********************************************************************
 *
 * DEBUGGING FUNCTIONS
 *
 *********************************************************************/

/* seldom used */
static void DisplaySpace(const Space *s, const char prefix, const int page_bytes)
{
  proper_printf("%cpage_offset   is %ld\n", prefix, s->page_offset);
  proper_printf("%cmax_pages     is %d\n", prefix, s->max_pages);
  proper_printf("%cpage_table    is "ZERO_X"%p.."ZERO_X"%p\n", prefix, s->page_table, &s->page_table[s->max_pages]);
  proper_printf("%c_bottom       is "ZERO_X"%p\n", prefix, s->bottom);
  proper_printf("%c_top          is "ZERO_X"%p\n", prefix, s->top);

#define Pages(s) ROUNDUP_UNITS(Bytes(s->pointer)-Bytes(s->bottom),page_bytes)
  proper_printf("%c_pointer      is "ZERO_X"%p (%d pages)\n", prefix, s->pointer, Pages(s));
#undef Pages
}

/*
 * Return the minimum of mutable top and immutable top
 * in the Header. 
 */
static word *Lowest(const Header H)
{
  word *low = H->gc_const.io_bottom;
  
  if (H->i_space.bottom < low) low = H->i_space.bottom;
  if (H->m_space.bottom < low) low = H->m_space.bottom;
  
  return low;
}

static char *TimeString(const time_t t)
{
  char *s = ctime(&t);
  
  s[strlen(s)-1] = '\0';   /* clobber the newline */
  
  return s;
}

/* 
 * Display information about the Header found in its structure
 */
void DisplayHeader(const Header H)
{
  int page_bytes = H->page_size;

  proper_printf("Header record at: "ZERO_X"%p\n",H); proper_fflush(stdout);
  proper_printf("header         is "ZERO_X"%p\n",H->header); proper_fflush(stdout);
  proper_printf("header_length  is %ld\n",H->header_length); proper_fflush(stdout);
  proper_printf("initial_length is %ld\n",H->initial_length); proper_fflush(stdout);
  proper_printf("file_length    is %ld\n",H->file_length); proper_fflush(stdout);
  
  DisplaySpace(&H->i_space,'i',page_bytes);
  DisplaySpace(&H->m_space,'m',page_bytes);
  
  proper_printf("next_page      is %d\n",H->next_page);
  proper_printf("Lowest         is "ZERO_X"%p\n",Lowest(H));
  proper_printf("Highest        is "ZERO_X"%p\n",Highest(H));
  proper_printf("root           is "ZERO_X"%p\n",H->root);
  proper_printf("nil            is "ZERO_X"%p\n",H->gc_const.nil);
  proper_printf("io_bottom      is "ZERO_X"%p\n",H->gc_const.io_bottom);
  proper_printf("io_top         is "ZERO_X"%p\n",H->gc_const.io_top);
  proper_printf("io_spacing     is %d\n",H->gc_const.io_spacing);
  proper_printf("created_date   is %s\n",TimeString(H->created_date));
  proper_printf("modified_date  is %s\n",TimeString(H->modified_date));
  proper_printf("cpu            is %s\n",H->cpu);
  proper_printf("parent         is %s\n",H->parent);
  proper_printf("page_size      is %i\n",H->page_size);
  proper_printf("RTS_IF_version is %i\n",H->RTS_IF_version);
  proper_printf("\n");
  proper_fflush(stdout);
}

/**********************************************************************
 *
 * Check that the pages in the space are right 
 *
 **********************************************************************/
static void CheckSpace(const Header H, const Space *s)
{
  unsigned i;
  word *pt;
  
  int page_bytes = H->page_size;
  int page_words = page_bytes / sizeof(word);
  int first_old_page = H->initial_length / page_bytes;
  int first_new_page = H->file_length    / page_bytes;

  assert(s->pointer >= s->bottom);
  assert(s->pointer <= s->top);
  assert(PAGE_NUMBER(s->max_pages) == s->max_pages);

  for (i = 0; i < s->max_pages; i++)
  {
    word p = s->page_table[i];
    
    if (p != UNALLOCATED_PAGE)
    {
      word  n = PAGE_NUMBER(p);
      word *m = Words(GET_PAGE_ADDRESS(s,i,page_bytes));
      
      assert(s->bottom <= m && m < s->pointer);
      assert(first_old_page <= n && n < first_new_page);
    }
  }
  
  for (pt = s->bottom; pt < s->pointer; pt += page_words)
  {
    unsigned n = GET_PAGE_NUMBER(s,pt,page_bytes);
  
    assert(n < s->max_pages);
    assert(s->page_table[n] != UNALLOCATED_PAGE);
  }
}

/**********************************************************************
 * 
 * Check the Database 
 *
 **********************************************************************/
static void CheckDatabase(const Header H)
{
  static char *exec_cpu = MM_CPU_TYPE;

  GCConst *C = &H->gc_const;
  
  /* Move file pointer to the end of the file */
  long length = SetFilePointerEnd(H->fd);
  
  /* length = actual length
     H->file_length = recorded length */
  if (length != H->file_length)
  {
    proper_printf("%s:File length %ld does not agree with recorded length %ld\n",
                    H->filename, length, H->file_length);
  }

  assert(C->io_top - C->io_bottom == 256 * C->io_spacing);
  
  assert(C->io_top <= H->i_space.bottom || H->i_space.top <= C->io_bottom);
  assert(C->io_top <= H->m_space.bottom || H->m_space.top <= C->io_bottom);
  
  assert(C->nil < H->i_space.bottom || H->i_space.top < C->nil);
  assert(C->nil < H->m_space.bottom || H->m_space.top < C->nil);
  
  CheckSpace(H,&H->i_space);
  CheckSpace(H,&H->m_space);

  if (strcmp(H->cpu,exec_cpu))
  {
     proper_printf("Database   CPU type = %s\n",H->cpu);
     proper_printf("Executable CPU type = %s\n",exec_cpu);
    
#if !defined(PORTING)
#if defined(LINUX)
    if (H->RTS_IF_version != 0)
#endif /* Linux hack */
      Exit("Incompatible CPU type in %s",H->filename);
#endif /* Porting hack */
     proper_printf("Ignoring incompatibility!\n");
  }
}
  
/**************************************************************
 *
 * Return address space (mutable or immutable) in which pt lies
 *
 **************************************************************/
static Space *FindAddressSpace(const Header H, const void *pt)
{
  if (H == 0) Crash("Address 0x%08x not found",pt);
  
  if ((void *)H->m_space.bottom <= pt && pt < (void *)H->m_space.pointer)
    {
      return &H->m_space;
    }
    
  if ((void *)H->i_space.bottom <= pt && pt < (void *)H->i_space.pointer)
    {
      return &H->i_space;
    }
  
  return FindAddressSpace(H->up,pt);
}

/**************************************************************
 *
 * IncrementProfileCount
 *
 **************************************************************/
void IncrementProfileCount
(
  const Header H,
        word  *pt,
  const word   incr,
        int   *unknown_count
)
{
  word   n;
  word   p;
  Space *s = FindAddressSpace(H,pt);
  int page_bytes = H->page_size;
  
  if (IS_MUTABLE_SPACE(s)) 
  {
     *unknown_count += incr;
     return;
  }
  
  if (s->bitmap == 0)
  {
     *unknown_count += incr;
     return;
  }
  
  n = GET_PAGE_NUMBER(s,pt,page_bytes);
  
  assert((unsigned) n < s->max_pages);
  
  p = s->page_table[n];
  
  assert(p != UNALLOCATED_PAGE);
  
  if (PAGE_IS_CLEAN(p))
  {
    /* set writable attribute on page */
    char *page = Bytes(GET_PAGE_ADDRESS(s,n,page_bytes));
    SetProtectionWriteCopy(page,page_bytes,"IncrementProfileCount");
  }

  *pt += incr;

  SET_DIRTY(s->page_table[n]);

  n = GET_WORDNUMBER(s,pt);
  
  SetBit(s->bitmap,n);
}

/*****************************************************************
 *
 * AssignMappedDatabaseWord
 *
 *****************************************************************/
void AssignMappedDatabaseWord(const Header H, word *base, int offset, const word value)
{
	RemoveObjectProtection(H, (byte*)base);
	base[offset] = value;
}

/*****************************************************************
 *
 * AssignMappedDatabaseByte
 *
 *****************************************************************/
/* lint seems to have problems with old-style definition */
void AssignMappedDatabaseByte(const Header H, byte *base, int offset, const byte value)
{
	RemoveObjectProtection(H, base);
	base[offset] = value;
}

/*****************************************************************
 *
 * RemoveObjectProtection
 *
 *****************************************************************/
void RemoveObjectProtection(const Header H, byte *base)
{
	word   n;
	word   *page;
	Space *s = FindAddressSpace(H,base);
	int page_bytes = H->page_size;
	word *last;
	int lenFirst;
	assert(IS_MUTABLE_SPACE(s));

	/* Find the page containing the start. */
	n = GET_PAGE_NUMBER(s,Words(base),page_bytes);
	assert((unsigned) n < s->max_pages);

	/* If this page is dirty then the protection will have been removed. */
	if (! PAGE_IS_DIRTY(s->page_table[n]))
	{
		page = (word*)Bytes(GET_PAGE_ADDRESS(s,n,page_bytes));
		/* "page" should now be the length word of an object. */
		lenFirst = OBJ_OBJECT_LENGTH(*page) + 1;
		last = page + lenFirst; /* The end of the first object. */

		/* Do this for each page in the range up to the end. */
		while (page < last)
		{
			/* Remove the write protection. */
			SetProtectionWriteCopy((byte*)page,page_bytes,"RemoveObjectProtection");
			SET_DIRTY(s->page_table[n]);
			page += page_bytes / sizeof(word);
			n++;
		}
	}
}

/********************************************************************
 *
 * Map the allocated pages in the space
 *
 ********************************************************************/
static void MapSpace(const Header H, const Space *s)
{
  word  i;
  word  n = 1;
  int   max = s->max_pages;
  word *pt  = s->page_table;
  /* Old versions of the database header didn't contain page_size */
  int   page_bytes = H->page_size;
  
#ifdef EXTRA_DEBUG
  proper_fprintf(stderr, "Reading page table; page: %li, length: %08lx\n",
                 s->page_offset / page_bytes, s->length / page_bytes);
#endif

  /* read in the page table */
  ReadIn(H->filename,Bytes(pt),(long)(s->length),
         H->fd,(unsigned long)s->page_offset);
  
#ifdef EXTRA_DEBUG
  proper_fprintf(stderr, "Mapping in pages\n");
#endif

  for (i = 0; i < max; i += n)
  {
    word t = pt[i];
    n = 1; /* default - increment by one page */
    
    if (t != UNALLOCATED_PAGE)
    {
      word p = PAGE_NUMBER(t);
      
/*#if !defined(WINDOWS_PC) && !defined(HPUX) */
/* UNIX - try to map several pages with one system call.
   Don't try this on the PC because PCmmap only actually
   maps one page at a time anyway. Don't try this on HPUX
   because "mprotect" apparently only works if given the
   whole address range produced by a single call to "mmap"
   and we want to use "mprotect" on single pages. (The
   manual page isn't very clear on this point.)
*/
     {
        word m = p + 1;
        word j = i + 1;

        while (j < max && 
               pt[j] != UNALLOCATED_PAGE && 
               PAGE_NUMBER(pt[j]) == m)
        {
          j++; m++; n++;
        }
     }
/* UNIX optimisation */
      
#ifdef WNTDEBUG
      proper_fprintf(stdout," %i to address %p\n", p, GET_PAGE_ADDRESS(s,i,page_bytes));
#endif

      MapIn(H->filename,Bytes(GET_PAGE_ADDRESS(s,i,page_bytes)),(long)(n*page_bytes),
              s->prot,s->mode,H->fd,(long)p*page_bytes);
    }
  }
#ifdef WNTDEBUG
  proper_printf("\n");
#endif
}

/***********************************************************************
 *
 * Open the mapped database
 *
 ***********************************************************************/
Header OpenMappedDatabase(char *filename, const int db_flags, const Header up)
{
  MAPPEDFD fd = NULLFD;
  char   magic[MM_LENGTH];
  Header H;
  long   length;
  int flags = db_flags;
  char *path;
  char filepath[MAXPATHLEN+1];
  int i;
  
  OpenZeroDevice();

  /* See if the file name contains a path separator. */
  for (i = 0; filename[i] != 0; i++)
  {
  	if (filename[i] == '/') break;
#ifdef WINDOWS_PC
  	if (filename[i] == '\\') break;
#endif
  }
  /* If it does the path does not apply, otherwise get a path. */
  if (filename[i] != 0) path = 0;
  else
  {
	  path = getenv("POLYPATH");
	  if (path == 0) path = DEFAULT_POLYPATH;
  }

  do {
	if (path == 0) strncpy(filepath, filename, MAXPATHLEN);
	else
	{
		/* Copy the path entry into the filename. */
		i = 0;
		while (*path != ':' && *path != 0 && i < MAXPATHLEN-1) filepath[i++] = *path++;
		if (*path == ':') path++;
		if (i != 0) filepath[i++] = '/';
		filepath[i] = 0;
		strncat(filepath, filename, MAXPATHLEN-i);
	}
	if (WRITABLE(flags))
	{
		do
		{
			fd = OpenExistingFileReadWrite(filepath);
		} while (fd == NULLFD && errno == EINTR);
	}
	if (fd == NULLFD)
	{
		/* Can we open it for reading? */
		do {
			fd = OpenExistingFileReadOnly(filepath);
		} while (fd == NULLFD && errno == EINTR);
  
		if (fd != NULLFD && (WRITABLE(flags)))
		{
			/* We asked to write but we can only get read access. */
			flags &= ~M_WRITABLE;
			proper_printf("WARNING:%s: Write permission denied.\n", filepath);
			proper_printf("WARNING:%s: Opened for reading only.\n", filepath);
		}
	}
   } while (fd == NULLFD && path != 0 && *path != 0);
   
   if (fd == NULLFD)
   {
	Exit("Unable to open %s",filename);
   }
  
  {
     if (! be_silent)
     {
     proper_printf("Mapping %s\n", filepath);
  }
  }
  
 (void) proper_fflush(stdout);
  
  /* Read the magic number from the mapped database 
     and check if copied correctly */
  ReadFD(fd,magic,MM_LENGTH,filepath);
  
  if (magic[0] != MM_MAGIC[0] || magic[1] != MM_MAGIC[1] ||
      magic[2] != MM_MAGIC[2] || magic[3] != MM_MAGIC[3])
  {
     Exit("%s is not a mapped database",filepath);
  }

  /* Read the Header from the mapped database 
     and check if copied correctly */
  ReadFD(fd,Bytes(&H),sizeof(Header),filepath);
  ReadFD(fd,Bytes(&length),sizeof(long),filepath);

  /* Not used in PC version */
  CheckStack(Bytes(H) + length);
  
#ifdef EXTRA_DEBUG
  proper_fprintf(stderr, "Reading in header: %08lx bytes\n", length);
#endif

  /* Read in the Header structure from the new database */
  ReadIn(filename,Bytes(H),length,fd,0L);
  
  if (H->RTS_IF_version != POLY_version_number)
  {
     if (DISCGARB1(flags) && H->RTS_IF_version < POLY_version_number)
     {
        /* It's only a warning, as otherwise we would never be able
           to increase the RTS version number when we bootstrap the compiler.
        */
        proper_printf("Warning: %s database version = %i, discgarb version = %i.\n",
                      filepath, H->RTS_IF_version, POLY_version_number);
     }
     else
     {
        Exit("Database %s (version %i) is incompatible with this RTS (version %i).\n",
             filepath, H->RTS_IF_version, POLY_version_number);
     }
  }

  CheckStack(Bytes(Highest(H)));
  
  H->fd                   = fd;
  H->flags                = flags;
  H->filename             = filename; /* Use the unexpanded name. */
  H->i_space.bitmap_bytes = 0;
  H->i_space.bitmap       = 0;
  H->m_space.bitmap_bytes = 0;
  H->m_space.bitmap       = 0;
  H->i_space.prot         = RO;
  H->i_space.mode         = PF;
  H->m_space.prot         = RO; /* Mutable area must be write protected. */
  H->m_space.mode         = PF;
  H->up                   = up;

  /* In DISCGARB1 mode the source database has its pages mapped RC since
     we'll be turning even the immutable objects into tombstones. 
     SPF 16/12/96
  */

  if (DISCGARB1(flags))
  {
     H->i_space.prot = RC;
     H->m_space.prot = RC;
  }
  
  /* In DISCGARB2 mode the target database has its pages mapped RW and SHARED   */
  /* so they do not need swap space. The pages are written out by the operating */
  /* system 'update' process, and when the file is closed in 'commit'.          */
  
  if (DISCGARB2(flags))
  {
    /* The "safer" version. SPF 1/3/96 */
    H->i_space.prot = RC;
    H->m_space.prot = RC;
    
    if (! WRITABLE(flags))
    {
      Exit("%s must be writable",filepath);
    }
    
    /* set virtual length to real length */
    length = SetFilePointerEnd(fd);
    
    H->file_length = length;
  }
  
  
#ifdef EXTRA_DEBUG
  proper_fprintf(stderr, "Mapping in H->i_space\n");
#endif
  
  MapSpace(H,&H->i_space);
  
#ifdef EXTRA_DEBUG
  proper_fprintf(stderr,"Mapping in H->m_space\n");
#endif 
  
  MapSpace(H,&H->m_space);
  
  H->m_space.bitmap_bytes = BITMAP_BYTES(H->m_space.pointer - H->m_space.bottom);
  H->i_space.bitmap_bytes = BITMAP_BYTES(H->i_space.pointer - H->i_space.bottom);
  
  if (! DISCGARB1(flags) && H->m_space.bitmap_bytes)
  {
#ifdef EXTRA_DEBUG
    proper_fprintf(stderr, "allocating H->m_space.bitmap\n");
#endif 
    H->m_space.bitmap = AllocateZeroes(H->m_space.bitmap_bytes);
  }
  
  if (PROFILED(flags) && H->i_space.bitmap_bytes)
  {
#ifdef EXTRA_DEBUG
    proper_fprintf(stderr, "allocating H->i_space.bitmap\n");
#endif 
    H->i_space.bitmap = AllocateZeroes(H->i_space.bitmap_bytes);
  }
  
  CheckDatabase(H);
  
  H->gc_space.h_bottom = Words(H);
  H->gc_space.i_bottom = H->i_space.bottom;
  H->gc_space.i_top    = H->i_space.pointer;
  H->gc_space.m_bottom = H->m_space.bottom;
  H->gc_space.m_top    = H->m_space.pointer;
  H->gc_space.parent   = 0;

  /* bugfix: don't map parent database in discgarb phase 2,
     because we've already mapped it during phase 1. SPF 3/5/95 */
  if (*H->parent && up == 0 && !DISCGARB2(flags))
  {
    word newFlags = flags & ~(M_WRITABLE|M_DISCGARB1|M_DISCGARB2);
    char *parentName = strdup(H->parent);
    /* Save the parent name in case loading it overwrites H.  We have to keep it on the
       heap because it may be copied to a new database if we are discgarbing. */
    
    H->up = OpenMappedDatabase(parentName,newFlags,(Header)NULL);

    /* If we've garbage collected the parent then we could find that the
       H spaces have overlapped. */
    if ((H->up >= H && H->up < H + 1) || (H >=H->up && H < H->up + 1))
    {
    	Exit("Database mismatch:\n%s is not the parent of %s or has been compacted.\n",
    		parentName, filepath);
    }
    if (H->up->created_date != H->parent_created_date)
    {
      /* These buffers must be "at least 26 bytes" large.
         We have to do this horrible stuff, because ctime
         uses a static buffer, so we can't call it twice. */
      char required_creation_time[100];
      char actual_creation_time[100];
      
      strncpy(required_creation_time,
              ctime(&(H->parent_created_date)),
              sizeof(required_creation_time) - 1);
      required_creation_time[sizeof(required_creation_time)-1] = '\0';      
      
      strncpy(actual_creation_time,
              ctime(&(H->up->created_date)),
              sizeof(actual_creation_time) - 1);
      actual_creation_time[sizeof(actual_creation_time)-1] = '\0';      
      
      Exit("Database timestamp mismatch:\n%s requires parent created at %s%s actually created at %s",
             filepath,  required_creation_time,
             parentName, actual_creation_time);
    }

    /* Doesn't this lose the parent database mapping under AIX? */
    CloseFD(H->up->fd,H->parent,"(closing parent database)");
    H->up->fd = NULLFD;
    
  }
  
  if (H->up) H->gc_space.parent = H->up;
  
  if (DISCGARB1(flags))
  {
    /* Close the file since it is going to be overwritten. */
    /* We have copied all of the file pages to swap pages. */
    CloseFD(fd,filepath,"(closing database in order to overwrite it)");
    H->fd = NULLFD;
  }

  return H;
}

/****************************************************************************
 *
 * Initialise the IO area
 * io_bottom and io_top determine the address of entries in the 
 * io_vector used to map Poly/ML system calls to their corresponding 
 * runtime addresses. 
 *
 ****************************************************************************/
void MapIOArea(const Header H)
{
  GCConst *C = &H->gc_const;
  
  int res = MapFixedZeroes(Bytes(C->io_bottom),
                (long)(Bytes(C->io_top)-Bytes(C->io_bottom)));
  if (res != 0)
  {
     SysError("Cannot map to address %p", C->io_bottom);
  }
}

/***************************************************************************
 *
 * Protect the IO area from writing (read and execute allowed only)
 *
 ***************************************************************************/
void FreezeIOArea(const Header H)
{
  GCConst *C      = &H->gc_const;
  long     length = Bytes(C->io_top) - Bytes(C->io_bottom);
  
  /* 
     Do we need explicit execute permission here too? SPF 5/5/95
     No, because SetProtectionReadOnly actually sets both Read
     *and* Execute permissions. SPF 12/9/95
  */
  SetProtectionReadOnly(Bytes(C->io_bottom),length,"FreezeIOArea");

#ifndef DONT_FLUSH_CACHE
  /* 
     We execute code from the I/O area (e.g. to raise an exception),
     so we have to flush the instruction cache because we've
     changed the contents of the I/O area. SPF 12/9/95
  */
  MD_flush_instruction_cache(Bytes(C->io_bottom),length);
#endif
  
}

/**************************************************************************
 *
 * Unmap the IO area
 *
 **************************************************************************/
void UnMapIOArea(const GCConst *C)
{
  FreeZeroes(C->io_bottom,Bytes(C->io_top)-Bytes(C->io_bottom),1);
}

/**************************************************************************
 *
 * Save the pages back to disk
 *
 **************************************************************************/
static void CommitSpace(const Header H, const Space *s)
{
  /* Old versions of the database header didn't contain page_size */
  int   page_bytes = H->page_size;
  int   max     = s->max_pages;
  word *pt      = s->page_table;
  int   max_new = GET_PAGE_NUMBER(s,s->pointer,page_bytes);
  int  i;
 
  /* First write out all the new pages */
  for (i = 0; i < max_new; i ++)
  {
    if (pt[i] == UNALLOCATED_PAGE)
    {
      pt[i] = H->next_page++;
      {
         int   p = PAGE_NUMBER(pt[i]);
         word *q = Words(GET_PAGE_ADDRESS(s,i,page_bytes));
      
#ifdef EXTRA_DEBUG
         proper_fprintf(stderr,"Writing new page from address %p to file page %i\n", q, p);
#endif
         WriteOut(H->filename,(char *)q,(long)page_bytes,H->fd,(long)(p*page_bytes));
      }
    }
  }
 
  /* Then write out all the dirty (changed) pages */
  for (i = 0; i < max; i ++)
  {
    if (pt[i] != UNALLOCATED_PAGE && PAGE_IS_DIRTY(pt[i]))
    {
      SET_CLEAN(pt[i]);
      {
         int   p = PAGE_NUMBER(pt[i]);
         word *q = Words(GET_PAGE_ADDRESS(s,i,page_bytes));
      
#ifdef EXTRA_DEBUG
         proper_fprintf(stderr, "Writing dirty page from address %p to file page %i\n", q, p);
#endif
         WriteOut(H->filename,(char *)q,(long)page_bytes,H->fd,(long)(p*page_bytes));
      }
    }
  }
  

#ifdef EXTRA_DEBUG 
  proper_fprintf(stderr, "Writing out page table: %li pages starting at page %li from address %p\n",
                   s->length / page_bytes, s->page_offset / page_bytes, &pt);
#endif

  WriteOut(H->filename,Bytes(pt),s->length,H->fd,s->page_offset);

  /* Then unmap all the pages */
  UnmapSpace(H, s);
}

/*
   Unmap all mapped pages.  Apart from Commit it is also used, at least in
   the Windows version of discgarb, to unmap all the pages from a database
   before deleting it.
*/
void UnmapSpace(const Header H, const Space *s)
{
  int   page_bytes = H->page_size ;
  int   max     = s->max_pages;
  int  i, n;
  word *pt      = s->page_table;
  for (i = 0; i < max; i += n)
  {
    word t = pt[i];

    if (t != UNALLOCATED_PAGE)
    {
      word j = i+1;
      
      /* Find a sequence of allocated pages */
      for (n = 1; j < max && pt[j] != UNALLOCATED_PAGE; j++, n++);

#if defined(WINDOWS_PC)
      PCmunmap(Bytes(GET_PAGE_ADDRESS(s,i,page_bytes)),n*page_bytes,"CommitSpace");
#else
      if (MUNMAP(Bytes(GET_PAGE_ADDRESS(s,i,page_bytes)), n*page_bytes, 1) != 0)
      {
         SysError("munmap failed");
      }
#endif
    }
    else n = 1;
  }
#if defined(WINDOWS_PC)
  PCmunmap(Bytes(pt),s->length,"CommitSpace");
#else  /* UNIX version */
  if (MUNMAP(Bytes(pt), s->length, 1) != 0)
  {
     SysError("munmap failed\n");
  }
#endif  
}


/**********************************************************************
 *
 * CountDirtyPages(written), ResetDirtyPages
 *
 **********************************************************************/
static word CountDirtyPages(const Space *s)
{
  unsigned i;
  word count = 0;

  for (i = 0; i < s->max_pages; i++)
  {
    word p = s->page_table[i];
    
    if (p != UNALLOCATED_PAGE && PAGE_IS_DIRTY(p)) count++;
  }
  
  return count;
}

static void ResetDirtyPages(const Space *s)
{
  unsigned i;

  for (i = 0; i < s->max_pages; i++)
  {
    word p = s->page_table[i];
    
    if (p != UNALLOCATED_PAGE && PAGE_IS_DIRTY(p))
    {
       SET_CLEAN(s->page_table[i]);
    }
  }
  return;
}

/********************************************************************
 * 
 * Save the mapped database to disk
 *
 ********************************************************************/
void CommitMappedDatabase(const Header H)
{
  int page_bytes = H->page_size;

  long length;

  /* disable SIGINT for duration of commit */
#if defined(WINDOWS_PC)
  /* disable SIGINT (CTRL+C) for duration of commit */
  SetResetCC(0);
#else
  { /* use standard SYSV calls */
    sigset_t mask;
    assert(sigemptyset(&mask) == 0);
    assert(sigaddset(&mask,SIGINT) == 0);
    assert(sigprocmask(SIG_BLOCK,&mask,NULL) == 0);
  }
#endif  

  if (DISCGARB1(H->flags)) 
  {
    Crash("Bad discgarb mode in commit.");
  }
  
  if (! WRITABLE(H->flags))
  {
    Crash("Bad flags in commit.");
  }
  
  if (DISCGARB2(H->flags))
  {
    /* The pages should be all clean (and all new!) */
    assert(CountDirtyPages(&H->i_space) == 0);
    assert(CountDirtyPages(&H->m_space) == 0);
  }
  else if (PROFILED(H->flags))
  {
    /* reset pages made dirty by profiling */
    ResetDirtyPages(&H->i_space);
  }
  else
  {
    assert(CountDirtyPages(&H->i_space) == 0);
  }

  /* CommitSpace copies all the dirty pages back to the file */
  CommitSpace(H,&H->i_space);
  CommitSpace(H,&H->m_space);
  
  length = H->next_page * page_bytes;
  {
	  int msize = H->m_space.pointer - H->m_space.bottom;
	  int mmax = H->m_space.top - H->m_space.bottom;
	  int isize = H->i_space.pointer - H->i_space.bottom;
	  int imax = H->i_space.top - H->i_space.bottom;
	  /* It's useful to know how full the database is getting. */
	  proper_printf("%s:%ld bytes.\nImmutable area %2.0f%% full, mutable area %2.0f%% full\n",
		  H->filename, length, (float)isize*100.0/(float)imax,
		  (float)msize*100.0/(float)mmax);
  }

  if (length < H->file_length)
  {
    FileTruncate(H->fd,length);
  }
  
  H->file_length = length;

  H->modified_date = time(0);
  
#ifdef EXTRA_DEBUG
  proper_printf("Writing out header: page: 0, length: %08x\n", H->header_length/page_bytes);
#endif
  WriteOut(H->filename,Bytes(H),H->header_length,H->fd,0L);

  proper_printf("Closing %s now\n",H->filename);
  (void) proper_fflush(stdout);

#if defined(WINDOWS_PC)
  /* do nothing */
#else  /* UNIX version */
  if (fsync(H->fd) != 0)
  { 
     SysError("fsync failed on %s",H->filename);
  }
#endif
  
  CloseFD(H->fd,H->filename,"(closing database to commit changes)");
  
  proper_printf("\n");
  
  if (H->m_space.bitmap)
  {
     FreeZeroes(H->m_space.bitmap,H->m_space.bitmap_bytes, 0);
  }
  if (H->i_space.bitmap)
  {
     FreeZeroes(H->i_space.bitmap,H->i_space.bitmap_bytes, 0);
  }
  
#if defined(WINDOWS_PC)
  PCmunmap(Bytes(H),H->header_length,"CommitMappedDatabase");
#else /* UNIX version */
  if (MUNMAP(Bytes(H), H->header_length, 1) != 0)
  {
     SysError("munmap failed");
  }
#endif

  /* re-enable SIGINT */
#if defined(WINDOWS_PC)
 SetResetCC(1);  
#else
  { /* use standard SYSV calls */
    sigset_t mask;
    assert(sigemptyset(&mask) == 0);
    assert(sigaddset(&mask,SIGINT) == 0);
    assert(sigprocmask(SIG_UNBLOCK,&mask,NULL) == 0);
  }
#endif 
}

/****************************************************************************/
/* This function finds all the old mutable segments that are dirty.         */
/* These are scanned since they may contain references into the gc area.    */
/* Clean old mutable or immutable segments cannot point into the gc area.   */
/****************************************************************************/
/* The correctness of this hangs by a thread - mutables are only allowed in */
/* the mutable section of the database, while code segments are only        */
/* allowed in the immutable section. SPF 10/12/96                           */
/****************************************************************************/
void OpOldMutables(const Header H, void(*op)(word **pt, int offset))
{
	Space *s = &H->m_space;
    /* Rewritten DCJM 20/1/01. */
	int   page_bytes = H->page_size;
	int   page_words = page_bytes / sizeof(word);

	unsigned i = 0;
	while (i < s->max_pages)
	{
		word p = s->page_table[i];

		if (p != UNALLOCATED_PAGE && PAGE_IS_DIRTY(p))
		{
			/* We always allocate objects in the mutable area so
			   that the start of a page corresponds to the start
			   of an object.  The only exception is objects larger
			   than a page.  In that case we don't use the rest of
			   the last page for anything.  This means that the
			   first time we get an assignment to an object we can
			   round down the base address (N.B. NOT the address
			   of the word actually being modified) and mark the
			   page dirty. Hence if we find a dirty page we know
			   that it represents the start of an object. */
			word *pt = Words(GET_PAGE_ADDRESS(s,i,page_bytes));
			int lenFirst = OBJ_OBJECT_LENGTH(*pt) + 1;
			int pages = ROUNDUP_UNITS(lenFirst,page_words);
			word *end = Words(GET_PAGE_ADDRESS(s,i+pages,page_bytes));
			assert(pages > 0);
			while (pt < end)
			{
				word L = *pt++;
				int len = OBJ_OBJECT_LENGTH(L);
				/* This code assumes that we don't have code or stack objects
				   in the mutable area. */
				if (OBJ_IS_WORD_OBJECT(L))
				{
					int i;
					for (i = 0; i < len; i++) (* op)((word**)pt, i);
				}
				pt += len;
			}
			i += pages;
		}
		else i++;
	}
  
	if (H->up) OpOldMutables(H->up,op);
}

/****************************************************************************/
/* This function finds all the old immutable profile counts that are dirty. */
/****************************************************************************/
/* The correctness of this hangs by a thread - mutables are only allowed in */
/* the mutable section of the database, while code segments are only        */
/* allowed in the immutable section. SPF 10/12/96                           */
/****************************************************************************/
void OpOldProfileCounts(const Header H, void (*op)(word *pt))
{
  Space *s = &H->i_space;

  int   page_bytes = H->page_size;
  int   page_words = page_bytes / sizeof(word);

  unsigned i;
  
  if (s->bitmap == 0) return;
  
  for (i = 0; i < s->max_pages; i++)
  {
    word p = s->page_table[i];
    
    if (p != UNALLOCATED_PAGE && PAGE_IS_DIRTY(p))
    {
      word *pt = Words(GET_PAGE_ADDRESS(s,i,page_bytes));
      word  n  = GET_WORDNUMBER(s,pt);
      word *bm = s->bitmap;
      
      int j;
      for (j = 0; j < page_words; j++)
      {
        if (TestBit(bm,n))
        {
         (* op)(&(pt[j]));
        }
        n++;
      }
    }
  }

  if (H->up) OpOldProfileCounts(H->up,op);
}

/****************************************************************************
 *
 * VIRTUAL MEMORY ADDRESSES AND ALLOCATION FUNCTIONS 
 *
 * We need to choose values for the fixed address spaces
 * for the mutable and immutable data. These values have
 * to be sufficiently high to be above the code and sbrk data,
 * but lower than the stack and shared libraries. 
 * nil is chosen to be outside of the mutable, immutable and all other
 * areas so that it gives a segmentation fault/bus error when accessed.
 *
 ***************************************************************************/

static GCConst K =
{
  NIL,
  IO_BOTTOM,
  IO_TOP,
  IO_SPACING
};

/* We can unmap all the chunks in a single operation, even under HPUX */
static void UnMapFixedZeroChunks(caddr_t addr, size_t chunks)
{
  FreeZeroes((word *)addr, chunks * CHUNKBYTES, 1);
}


/* We don't need to work-around the inefficiency caused by a working-around a crappy O/S kernel. */
static int MapFixedZeroChunks(caddr_t addr, size_t chunks)
{
   return MapFixedZeroes((char *)addr,(long)(chunks * CHUNKBYTES));
}

#define LOCAL_LOCAL_ITOP ((caddr_t)LOCAL_ITOP)
#define LOCAL_LOCAL_MTOP ((caddr_t)LOCAL_MTOP)
#define LOCAL_LOCAL_IBOTTOM ((caddr_t)LOCAL_IBOTTOM)
#define LOCAL_LOCAL_MBOTTOM ((caddr_t)LOCAL_MBOTTOM)

static caddr_t local_ipointer = LOCAL_LOCAL_ITOP;
static caddr_t local_mpointer = LOCAL_LOCAL_MTOP;

/*
 * Initialise memory for local immutable pages
 */
word *AllocateLocalImmutableChunks(const int chunks)
{
  caddr_t new = (caddr_t)((char *)local_ipointer - chunks * CHUNKBYTES);
  
  assert(LOCAL_LOCAL_IBOTTOM <= local_ipointer && local_ipointer <= LOCAL_LOCAL_ITOP);
  
  /* Requested allocation is too large - just return 0 */
  if (new < LOCAL_LOCAL_IBOTTOM) return 0;

  /* A very large allocation request could cause the arithmetic to wrap. */
  assert(new <= local_ipointer);
  
  {
     int res = MapFixedZeroChunks(new, (size_t)chunks);

     if (res != 0)
     {
	return 0; /* Allocation failed */
     }
  }
  
  local_ipointer = new;
  return (word *)local_ipointer;
}

/*
 * Initialise memory for local mutable pages
 */
word *AllocateLocalMutableChunks(const int chunks)
{
  caddr_t new = (caddr_t)((char *)local_mpointer - chunks * CHUNKBYTES);
  
  assert(LOCAL_LOCAL_MBOTTOM <= local_mpointer && local_mpointer <= LOCAL_LOCAL_MTOP);

  /* Requested allocation is too large - just return 0 */
  if (new < LOCAL_LOCAL_MBOTTOM) return 0;

  assert(new <= LOCAL_LOCAL_MTOP);
  
  {
     int res = MapFixedZeroChunks(new, (size_t)chunks);
     if (res != 0)
     {
	return 0; /* Allocation failed */
     }
  }

  local_mpointer = new;
  return (word *)local_mpointer;
}

/*
 * free memory for local immutable pages
 */
word *FreeLocalImmutableChunks(const int chunks)
{
  caddr_t new = (caddr_t)((char *)local_ipointer + chunks * CHUNKBYTES);

  assert(LOCAL_LOCAL_IBOTTOM <= local_ipointer && local_ipointer <= LOCAL_LOCAL_ITOP);
  assert(local_ipointer <= new && new <= LOCAL_LOCAL_ITOP);
  
  UnMapFixedZeroChunks(local_ipointer,(size_t)chunks);
  local_ipointer = new;

  return (word *)local_ipointer;
}

/*
 * free memory for local mutable pages
 */
word *FreeLocalMutableChunks(const int chunks)
{
  caddr_t new = (caddr_t)((char *)local_mpointer + chunks * CHUNKBYTES);
  
  assert(LOCAL_LOCAL_MBOTTOM <= local_mpointer && local_mpointer <= LOCAL_LOCAL_MTOP);
  assert(local_mpointer <= new && local_mpointer <= LOCAL_LOCAL_MTOP);
  
  UnMapFixedZeroChunks(local_mpointer,(size_t)chunks);
  local_mpointer = new;

  return (word *)local_mpointer;
}

/* Check for overlapping addresses.  Returns the next possibly free address. */
static word* AddressesOverlap(const Header old, const GCSpace *new)
{
  if (old == 0) return 0; /* No further parent to check - no overlap. */

  /* If there is an overlap return the top of this database as the next possible
     base.  This will always be greater than the previous base. */
  if ((new->h_bottom <= old->gc_space.h_bottom && old->gc_space.h_bottom < new->i_top) ||
	  (new->h_bottom >= old->gc_space.h_bottom && new->h_bottom < old->i_space.top))
	  return old->i_space.top;

  return AddressesOverlap(old->gc_space.parent,new);
}

/* Get a new database space.  Used when running the database garbage collector,
   discgarb, or when creating a child database. */
static GCSpace *GetNextAddressSpace(const Header old, GCSpace *new)
{
	word *h_bottom = H_BOTTOM;

	/* The header, the immutable page table and the mutable page table are
	   each rounded up to a multiple of the page size. */
	int immPageTable = ROUNDUP_UNITS(dbImmutablePages, PAGEWORDS);
	int mutPageTable = ROUNDUP_UNITS(dbMutablePages, PAGEWORDS);
	int dbHeaderWords = (H_SIZE + immPageTable + mutPageTable) * PAGEWORDS;
	int mutableWords = dbMutablePages * PAGEWORDS;
	int immutableWords = dbImmutablePages * PAGEWORDS;

	while (1)
	{
		/* Create a new area for the database. */
		new->h_bottom = h_bottom;
		new->m_bottom = h_bottom      + dbHeaderWords;
		new->m_top    = new->m_bottom + mutableWords;
		new->i_bottom = new->m_top;
		new->i_top    = new->i_bottom + immutableWords;
		new->parent   = 0;

		if (new->i_top > H_TOP) /* too high, overlaps IO area */
			return 0; /* no address spaces left */

		/* Check that it doesn't overlap the existing database or any parent. */
		h_bottom = AddressesOverlap(old, new);
		if (h_bottom == 0) return new; /* No overlap - success. */

		assert(h_bottom > new->h_bottom);
	}
}

/**********************************************************************
 *
 * Creates a database and writes the header and the pagetables
 * for mutables and immutables.
 * if no_trunc = 2 create new file (mustn't have existing file)
 * if no_trunc = 1 open existing file
 * if no_trunc = 0 create new file (zaps any existing file)
 *
 * This OUGHT to be the only function that uses PAGEBYTES/PAGEWORDS -
 * eveything esle should refer to H->page_size. Unfortunately, this is
 * not yet the case. SPF 22/5/97
 *
 **********************************************************************/
static int CreateDatabase
(
        char    *filename,
  const Header   parent,
  const GCSpace *C,
  const word     no_trunc
)
{
  MAPPEDFD fd;
  HEADER H;
  unsigned  i;
  word   pad;
  unsigned max_ipages;
  unsigned max_mpages;
  word  *ipage_table;
  word  *mpage_table;
  word  *zeroes;
  int iPageTablePages = ROUNDUP_UNITS(dbImmutablePages, PAGEWORDS);
  int mPageTablePages = ROUNDUP_UNITS(dbMutablePages, PAGEWORDS);
  
  OpenZeroDevice();
    
  assert(C->i_bottom < C->i_top);
  assert(C->m_bottom < C->m_top);
  
  assert(K.io_top - K.io_bottom == 256 * K.io_spacing);
  
  max_ipages = (C->i_top - C->i_bottom) / PAGEWORDS;
  max_mpages = (C->m_top - C->m_bottom) / PAGEWORDS;
  
  assert(PAGE_NUMBER(max_ipages) == max_ipages);
  assert(PAGE_NUMBER(max_mpages) == max_mpages);
  
  assert(sizeof(HEADER) <= H_SIZE * PAGEBYTES);
  
  switch (no_trunc)
  {
     case 0:
       /* create new file regardless */
       do {
          fd = OpenCreateFileReadWrite(filename);
       } while (fd == NULLFD && errno == EINTR);
       break;
  
     case 1: 
       /* error if file *doesn't* already exist */ 
       do {
          fd = OpenExistingFileReadWrite(filename);
       } while (fd == NULLFD && errno == EINTR);
       break;
       
     case 2:
       /* error if file *does* already exist */
       do {
          fd = OpenNewFileReadWrite(filename);
       } while (fd == NULLFD && errno == EINTR);
       break;
       
     default:
       Crash("Bad parameter to CreateDatabase: no_trunc=%i\n",no_trunc);
       break;
  }
  
  if (fd == NULLFD) return 0;
  
  H.magic[0]             = MM_MAGIC[0];
  H.magic[1]             = MM_MAGIC[1];
  H.magic[2]             = MM_MAGIC[2];
  H.magic[3]             = MM_MAGIC[3];
  H.header               =(Header) C->h_bottom;
  H.header_length        = H_SIZE * PAGEBYTES;
  H.fd                   = fd;
  H.flags                = 0;
  H.filename             = filename;
  H.i_space.max_pages    = max_ipages;
  H.m_space.max_pages    = max_mpages;
  H.i_space.length       = iPageTablePages * PAGEBYTES; /* Page table size in bytes. */
  H.m_space.length       = mPageTablePages * PAGEBYTES;
  H.i_space.page_offset  = H.header_length;
  H.m_space.page_offset  = H.header_length + H.i_space.length;
  H.initial_length       = H.header_length + H.i_space.length + H.m_space.length;
  H.i_space.type         = Immutable;
  H.m_space.type         = Mutable;
  H.i_space.prot         = 0;
  H.i_space.mode         = 0;
  H.m_space.prot         = 0;
  H.m_space.mode         = 0;
  H.i_space.page_table   = C->h_bottom + H_SIZE * PAGEWORDS;
  H.m_space.page_table   = C->h_bottom + H_SIZE * PAGEWORDS + iPageTablePages * PAGEWORDS;
  H.i_space.bottom       = C->i_bottom;
  H.i_space.top          = C->i_top;
  H.i_space.pointer      = C->i_bottom;
  H.i_space.bitmap_bytes = 0;
  H.i_space.bitmap       = 0;
  H.m_space.bottom       = C->m_bottom;
  H.m_space.top          = C->m_top;
  H.m_space.pointer      = C->m_bottom;
  H.m_space.bitmap_bytes = 0;
  H.m_space.bitmap       = 0;
  H.next_page            = H_SIZE + iPageTablePages + mPageTablePages;
  H.gc_const             = K;
  H.root                 = 0;
  H.created_date         = time(0);
  H.modified_date        = time(0);
  H.file_length          = H.initial_length;
  H.up                   = 0;
  H.parent_created_date  = parent ? parent->created_date : 0;
  H.page_size            = PAGEBYTES;
  H.RTS_IF_version       = POLY_version_number;
  
  strcpy(H.cpu,MM_CPU_TYPE);
  strcpy(H.parent, parent ? parent->filename : "");
  
  assert((max_ipages*sizeof(word)) <= (unsigned) H.i_space.length);
  assert((max_mpages*sizeof(word)) <= (unsigned) H.m_space.length);

  assert(max_ipages < (unsigned)UNALLOCATED_PAGE);
  assert(max_mpages < (unsigned)UNALLOCATED_PAGE);
  
  ipage_table = AllocateZeroes((int)H.i_space.length);
  mpage_table = AllocateZeroes((int)H.m_space.length);
  
  /* mark all mutable and immutable pages UNALLOCATED */
  for (i = 0; i < max_ipages; i++) 
  {
     ipage_table[i] = UNALLOCATED_PAGE;
  }
  for (i = 0; i < max_mpages; i++) 
  {
     mpage_table[i] = UNALLOCATED_PAGE;
  }
  
#ifdef EXTRA_DEBUG
  proper_fprintf(stderr, "Writing header, %i bytes\n", sizeof(HEADER));
#endif
  /* Write Header first */
  Write(filename,fd,Bytes(&H),(long)sizeof(HEADER));
  
  pad = PAGEBYTES - sizeof(HEADER); /* pad out to one page */
  
  zeroes = AllocateZeroes(pad);
  
#ifdef EXTRA_DEBUG
  proper_fprintf(stderr, "Writing padding, %i bytes\n", pad);
#endif
  Write(filename,fd,Bytes(zeroes),(long)pad);
  
#ifdef EXTRA_DEBUG
  proper_fprintf(stderr, "Writing ipage_table, %li bytes\n", H.i_space.length);
#endif
  Write(filename,fd,Bytes(ipage_table),H.i_space.length);
  
#ifdef EXTRA_DEBUG
  proper_fprintf(stderr, "Writing mpage_table, %li bytes\n", H.m_space.length);
#endif
  Write(filename,fd,Bytes(mpage_table),H.m_space.length);
  
  /* Explicitly fsync file before closing it. SPF 4/9/95 */

#if !defined(WINDOWS_PC)
  if (fsync(fd) != 0)
  { 
     SysError("fsync failed on file %s\n",filename);
  }
#endif
  
  CloseFD(fd,filename,"(closing newly created database)");

  FreeZeroes(ipage_table,(int)H.i_space.length, 0);
  FreeZeroes(mpage_table,(int)H.m_space.length, 0);
  FreeZeroes(zeroes,pad,0);
  
  return 1;
}

/**************************************************************************
 *
 * The following functions use CreateDatabase in different instances
 *
 **************************************************************************/
/* CreateMappedDatabase is only used when creating a database from some
   other format. */
void CreateMappedDatabase(char *filename)
{
  GCSpace  M;
  GCSpace *master = GetNextAddressSpace((Header)NULL,&M);
  
  assert(master);
  
  if (! CreateDatabase(filename,NULL,master,0))
  {
    Exit("Unable to create %s",filename);
  }
}

/* CreateNewCopyDatabase is like CreateCopyDatabase, except that
   it won't overwrite an existing database. */
void CreateNewCopyDatabase(char *filename, const Header H)
{
  GCSpace  C;
  GCSpace *copy = GetNextAddressSpace(H,&C);
  
  /* should always be able to make a copy for discgarb */
  if (copy == 0)
  { 
     Exit("Not enough address spaces");
  }
  
  if (! CreateDatabase(filename,H,copy,2))
  {
     Exit("Unable to create %s",filename);
  }
}

CStatus CreateChildDatabase(char *filename, const Header H)
{
  GCSpace A,*child;
  
  child = GetNextAddressSpace(H,&A);
  
  if (child == 0) return NoMoreChildren;
  
  child->parent = H;

  if (! CreateDatabase(filename,H,child,0)) return CannotCreate;
  
  return CreatedOk;
}

void Uninitdbase(Header H)
{
	MAPPEDFD fd = H->fd;
	if (H->up) Uninitdbase(H->up);
	UnmapSpace(H, &H->i_space);
	UnmapSpace(H, &H->m_space);

	/* Unmap the database header. */
#if defined(WINDOWS_PC)
	PCmunmap(Bytes(H),H->header_length,"uninitdbase");
#else  /* UNIX version */
	if (MUNMAP(Bytes(H), H->header_length, 1) != 0)
	{
		SysError("munmap failed\n");
	}
#endif  

#if defined(WINDOWS_PC)
	CloseHandle(fd);
#else
	close(fd);
#endif        
}

void UninitMemoryMapping(Header H)
{
	if (H)
	{
		GCConst C = H->gc_const;
  
		Uninitdbase(H);
		UnMapIOArea(&C);
	}
}

/************************************************************************
 *
 * The following functions extract or set components 
 * from the a variable of type Header (structure)
 *
 ************************************************************************/
GCSpace NewDataSpaces(const Header H)
{
  GCSpace S;
  
  S.h_bottom = 0;
  S.i_bottom = H->i_space.pointer;
  S.i_top    = H->i_space.top;
  S.m_bottom = H->m_space.pointer;
  S.m_top    = H->m_space.top;
  S.parent   = 0;

  return S;
}

root_struct *Root(const Header H)
{
  return H->root;
}

void SetRoot(Header H, root_struct *newRoot)

{
  H->root = newRoot;
  return;
}

word IsNil(const Header H, const word *pt)
{
  return(pt == H->gc_const.nil);
}

word IsIOPointer(const Header H, const word *pt)
{
  return(pt >= H->gc_const.io_bottom && pt < H->gc_const.io_top);
}

word IsDataPointer(const Header H, const word *pt)
{
  if (H->i_space.bottom <= pt && pt < H->i_space.pointer) return 1;
  if (H->m_space.bottom <= pt && pt < H->m_space.pointer) return 1;

  if (H->up) return IsDataPointer(H->up,pt);
  
  return 0;
}

#ifdef DEBUG
word IsDatabasePointer(const Header H, const word *pt)
{
  return IsNil(H,pt) || IsIOPointer(H,pt) || IsDataPointer(H,pt);
}
#endif

Header ParentDatabase(const Header H)
{
  return H->up;
}
