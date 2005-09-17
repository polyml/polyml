/*
    Title:      Main program

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
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>

#if defined(WINDOWS_PC)
#include <time.h>
#include <float.h>
#include <malloc.h>
#else
/* UNIX */
#include <sys/param.h>
#include <sys/file.h>

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/times.h>
#include <unistd.h>

/* end UNIX-specific */
#endif


#if defined(FREEBSD)
/* ru_maxrss is returned as kbytes not pages. */
#define PAGESIZE 1024
#endif

#include "globals.h"
#include "memory.h"
#include "sys.h"
#include "addresses.h"
#include "alloc.h"
#include "gc.h"
#include "run_time.h"
#include "xwindows.h"
#include "arb.h"
#include "copygc.h"
#include "mmap.h"
#include "machine_dep.h"
#include "version.h"
#include "objects.h"
#include "cwd.h"
#include "diagnostics.h"
#include "proper_io.h"
#include "processes.h"
#include "profiling.h"
#include "mpoly.h"
#include "discgc.h"

#ifdef WINDOWS_PC
#include "Console.h"
#endif

#define KB(words) ((words)*(int)sizeof(word)/1024)

#ifndef __GNUC__
#ifndef __attribute__
#define __attribute__(attribute) /* attribute */
#endif
#endif

#if	(defined(_MSC_EXTENSIONS) && (_MSC_VER >= 1200))
#define NORETURN	__declspec(noreturn)
#else
#define NORETURN
#endif

#if defined(WINDOWS_PC) 
#define MAXPATHLEN MAX_PATH 
#endif

/* Forward declaration so we can add the attribute */
NORETURN void finish(int n) __attribute__((noreturn));


word *nil_value = 0; /* needed in assembly code */

/* 
  Store areas
  LocalMemory structure defined in memory.h
  Header structure defined in mm.h
 */
LocalMemory A = { 0 };
Header      H = 0;

/* 
  Map of io operations
  sys.h contains all the codes used
 */
word *interface_map[POLY_SYS_vecsize] = { 0 }; 

/* used heavily by MD_init_interface_vector in machine_dep.c */
void add_word_to_io_area (int  SYS_op, word val)
{
  static int last_op    = 0;
  static int last_index = 0;

  if (SYS_op >= 0)
  {
    last_op    = SYS_op;
    last_index = 0;
  }

  assert (last_op    < 256);
  assert (last_index < H->gc_const.io_spacing);

  interface_map[last_op][last_index++] = val;
}

static root_struct *newRoot = NULL;

static void CopyNewRoot (CopyFunc copy)
{
  newRoot = (root_struct *)(* copy)((word *)newRoot);
}


/* CALL_IO3(createf, REF, REF, REF, NOIND) */
word createfc /* Make a new database */
(
  Handle db_specific_list,
  Handle root_function,
  Handle child            /* handle to pstring */
)
{
  Header   N;
  ProcessHandle root_process;
  char     buffer[MAXPATHLEN+1];
  int      len;
  word     total;
  GCConst *consts;
  GCSpace *db;
  GCSpace  old;
  GCSpace  new;
  CStatus  status;
  
  
  len = Poly_string_to_C(DEREFSTRINGHANDLE(child),buffer,sizeof(buffer));

  if (len == 0 || len >= MAXPATHLEN)
  {
    raise_exception_string (EXC_commit,"Bad file name");
  }

  /* Create a root process for the new database. */
  
  root_process = make_new_root_proc (root_function);

  newRoot = (root_struct *)alloc (255 | OBJ_MUTABLE_BIT);
  
  /* process list */
  newRoot->process_list = DEREFPROCHANDLE(root_process);
  
  /* console chain */
  newRoot->console_chain = DEREFPROCHANDLE(root_process);
  
  /* database specific entries */
  newRoot->db_specific_list = DEREFWORDHANDLE(db_specific_list);

  status = CreateChildDatabase (buffer,H);
  
  if (status == NoMoreChildren)
  {
    raise_exception_string (EXC_commit,"No more address spaces");
  }
  
  if (status == CannotCreate)
  {
    raise_exception_string (EXC_commit,"Cannot create file");
  }
  
  assert (status == CreatedOk);
  
  N = OpenMappedDatabase (buffer,A.flags | M_WRITABLE,H);
  
  consts = &H->gc_const;
  db     = &H->gc_space;
  new    = NewDataSpaces(N);

  old.h_bottom = 0;
  old.i_bottom = A.I.pointer;
  old.i_top    = A.I.top;
  old.m_bottom = A.M.pointer;
  old.m_top    = A.M.top;
  old.parent   = 0;

  if (A.debug & DEBUG_IMMRO) 
  {
    SetImmutables (1);
  }
 
  ResetAllocator();
  
  /* There was a serious problem here. We copy everything that is reachable
     from the new root into the new database area. This corrupts the old
     copies (by replacing them with forwarding pointers). Unfortunately, the
     old copies may still be reachable via the old roots, which means
     that the system can crash when we return from make_database.
     This has now been fixed by adding the "restore" flag to CopyGC, which
     requests it to restore the heap to its previous state. SPF 14/6/95 */
  total = CopyGC (CopyNewRoot, consts, consts, db, &old, &new,
                  N, NewMutable, NewImmutable, 1);
                  
  /* We *don't* have to flush the instruction cache here, because CopyGC
     hasn't touched any existing instructions (only the length words)
     and because CopyGC itself will ensure cache consistency for
     the instructions at their new addresses. SPF 15/12/95
  */
  
  SetRoot (N,newRoot);
  
  CommitMappedDatabase (N);
  
  return TAGGED(0);
}


static void RunMappedDatabase (Header parent)
{
  word     i;
  word     n = 50000000;
  GCConst *consts;
  Header space;
  
#if NEVER
/* removed SPF 23/11/93 */
  if ((A.debug & DEBUG_SET_BREAK) && brk (n) != 0)
  {
    perror ("brk");
    exit (1);
  }
#endif
    
  H = OpenMappedDatabase (A.filename,A.flags,parent);
  
  A.flags = H->flags;
    
  if (A.debug & DEBUG_SET_BREAK) 
  {
    for (space = H; space; space = space->gc_space.parent)
    {
      if (Word(space->gc_space.i_top) > n || Word(space->gc_space.m_top) > n)
      {
        Exit ("Database is too big for brk limit");
      }
    }
  }

  consts = &H->gc_const;

  nil_value = consts->nil; /* needed in assembly code */
  
  MapIOArea (H);
  
  for (i = 0; i < POLY_SYS_vecsize; i++)
  {
    interface_map[i] = consts->io_bottom + i * consts->io_spacing;
  }

  /* Initialise the interface vector. */
    
  MD_init_interface_vector(); /* machine dependent entries. */

  /* Add a zero entry.  N.B. As well as creating an entry pointing
     to a zero word this also assumes that the PREVIOUS word is
     zero.  That's because this value is used to represent zero-sized
     vectors. */
  add_word_to_io_area(POLY_SYS_nullorzero, 0);

  /* The standard input and output streams are persistent i.e. references
     to the the standard input in one session will refer to the standard
     input in the next. */
       
  add_word_to_io_area(POLY_SYS_stdin, 0);
  add_word_to_io_area(POLY_SYS_stdout, 1);
  add_word_to_io_area(POLY_SYS_stderr, 2);
    
  FreezeIOArea (H);

  CreateHeap();
  
  re_init_run_time_system();

  /* Now run the root processes. */
  
  set_process_list (Root(H));
}

static CopyFunc copyOp = 0;

static void CopyRuntimeObject(word **pt, int weak)
/* weak is not used, but is needed so function has the right type */
{
  *pt = (* copyOp) (*pt);
}

static void CopyOldMutable (word **pt, int offset)
{
  word *old = pt[offset];
  word *new = (* copyOp) (old);
  
  if (new != old) AssignMappedDatabaseWord (H,(word *)pt,offset,(word)new);
}

static void CopyRoots(CopyFunc copy)
{
  copyOp = copy;
  
  OpOldMutables (H,CopyOldMutable);
  
  OpGCProcs (CopyRuntimeObject);
}

int commitc(void)
{
  static int retry  = 1;
  static int result = TAGGED(0);
  
  if (retry) /* returning from a persistent commit */
  {
    retry = 0;
    return result;
  }
  else if (! WRITABLE(A.flags))
  {
    raise_exception_string (EXC_commit,"Database is not opened for writing");
    /*NOTREACHED*/
  }
  else
  {
    word     total;
    GCConst *consts;
    GCSpace *db;
    GCSpace  new;
    GCSpace  old;
    Header   parent;
    
    MD_set_for_retry(POLY_SYS_commit);
    
    stop_profiling(0);
    
    UnMapIOArea (&H->gc_const);
    
    parent = ParentDatabase(H);
    consts = &H->gc_const;
    db     = &H->gc_space;
    new    = NewDataSpaces(H);

    old.h_bottom = 0;
    old.i_bottom = A.I.pointer;
    old.i_top    = A.I.top;
    old.m_bottom = A.M.pointer;
    old.m_top    = A.M.top;
    old.parent   = 0;
   
    proper_printf("\n");
   
    if (A.debug & DEBUG_IMMRO)
    {
      SetImmutables (1);
    }

    ResetAllocator();

    /* Copy the heap into the database, corrupting the heap.
       Don't bother to restore the heap afterwards. */
    total = CopyGC (CopyRoots, consts, consts, db, &old, &new,
                    H, NewMutable, NewImmutable, 0);
    
  /* We *don't* have to flush the instruction cache here, because CopyGC
     hasn't corrupted any existing instructions (only the length words)
     and because CopyGC itself will ensure cache consistency for
     the instructions at their new addresses. Even if the forthcoming
     munmap/mmap pair doesn't flush the cache (which it certainly
     should), we'll be mapping the same instructions back to the same
     addresses so everything should be O.K. (I think!). SPF 15/12/95
  */
  
    /* Commit the new database pages. */
    CommitMappedDatabase (H);
    
    if (A.debug & DEBUG_SET_BREAK)
    {
      Exit ("Commit must not return when using -d 4");
    }
    
    retry = 1;
    result = TAGGED(1);
    
    /* Discard the old heap */
    DestroyHeap();
    
    {
       char current_directory[MAXPATHLEN+1];
       
       /* remember the current working directory */
       get_working_directory(current_directory);
    
       /* 
          go back to original working directory, since
          the database name may use relative paths
       */
       reset_working_directory(); 
    
       /* Map the database back into memory, and create a new heap */
       RunMappedDatabase (parent);
       
       /* restore the current working directory */
       set_working_directory(current_directory);
    }
    
    select_a_new_process();
    /*NOTREACHED*/
  }
}

#define GETNUMBER(opt,scale,value) \
{ char *p; unsigned len = strlen(opt); \
  if (strlen(argv[i]) == len) { i++; p = argv[i]; } else p = argv[i]+len;\
  if (i >= argc) \
    proper_printf("Incomplete %s option\n",opt); \
  else \
    value = atoi(p) * (scale); \
}

#define GETSTRING(opt,value) \
{ \
  i++; \
  if (i >= argc) \
    proper_printf("Incomplete %s option\n",opt); \
  else \
    value = argv[i]; \
}

/* This macro must make a whole number of chunks */
#define K_to_words(k) ROUNDUP((k) * (1024 / sizeof(word)),CHUNKWORDS)

int user_arg_count;
int be_silent = 0;
char **user_arg_strings;
char *programName;
int timeslice_milliseconds;

#ifdef WINDOWS_PC
static char pathbuff[_MAX_PATH];
#endif

/* In the Windows version this is called from WinMain in Console.c */
int main (int argc, char **argv)
{
  word i;

  /* Good old Poly/ML defaults */
  word hsize = 10 * 1024;  /* 10MB (was 6MB SPF 31/7/96) */
  word isize = 0; /* use standard default */
  word msize = 0; /* use standard default */

  if (argc > 0) 
  {
	  char *p;
	  programName = argv[0];  /* Remember the program name. */
	  /* Find the last component of the name. */
	  for (p = programName; *p != 0; p++); /* End of string. */
	  while (p != programName)
	  {
			if (p[-1] == '/'
#ifdef WINDOWS_PC
			  || p [-1] == '\\'
#endif
			  ) break;
			p--;
	  }
	  /* If the program was invoked as discgarb it automatically
	     assumes the discgarb option. Otherwise if the -d option
	     is given assume discgarb option. */
	  if (strcmp("discgarb", p) == 0 ||
		  (argc > 1 && strcmp(argv[1], "-d") == 0))
		  return discgarb(argc, argv);
	  if (strcmp("changeParent", p) == 0 ||
		  (argc > 1 && strcmp(argv[1], "-p") == 0))
		  return changeparent(argc, argv);

  }

  /* Get arguments. */
  memset(&A, 0, sizeof(A)); /* Reset it */
  
  A.I.percent   = 100; /* Was 90%. SPF 7/10/96 */
  A.M.percent   = 20; /* do a full GC if mutable   buffer more than 20% full after partial GC */
  A.debug       = 0;
  A.filename    = 0;
  A.stats_flags = 0;
  A.stats_file  = 0;
  A.stats_name  = 0;
  A.noDisplay   = 0;
  A.timeslice   = 1000; /* default timeslice = 1000ms */

/* These default settings give the following buffer sizes:

       2.0MB Immutable
       5.0MB Mutable
       
   Full GC triggered if we have less than:
   
       0.2MB Immutable
       2.5MB Mutable
       
   available after the partial GC. The full GC will attempt to ensure that there is:
   
      1.0MB or more Immutable
      5.0MB or more Mutable
      
  free when it returns, but will prevent either area growing larger than:
  
     10.0MB
     
  Perhaps the full GC still grows the areas a little aggressively? (Or perhaps,
  doesn't shrink it aggressively enough - it's not going to shrink the mutable
  area when using these defaults!)
  
  Full GCs basically get triggered by the immutable buffer filling up with stuff
  copied from the mutable buffer. During compilation, at least, the full GC
  doesn't appear to be able to free up any more storage than the partial GCs -
  it just grows the heap. This being the case, we want to have as few full GCs
  as possible, which means having a fairly large immutable buffer. However,
  since this buffer doesn't actually get used in the same way that the mutable
  buffer does, we don't want to make it *too* big.
     
  SPF 31/7/96
  
  The improvements to gc.c may have changed the trade-offs here. Previously,
  a partial GC used to zero both buffers, which made it more expensive to
  have a big immutable buffer.
  
  For a partial GC, the costs are essentially:
  
     (1) Marking - from external roots, and all mutables.
     (2) Copying mutable objects to immutable area.
     (3) Updating references to the moved objects.
     (4) Zeroing the mutable buffer.
     
  Normally these costs will be roughly proportional to the size of the mutable
  buffer, since we expect to have to copy a roughly constant percentage of
  the mutable buffer to the immutable buffer on each GC. There's a fixed
  cost for scanning the old mutables, but that's normally low.
     
  For a full GC, the costs are:
  
     (1) Marking - from external roots.
     (2) Compacting the immutable buffer.
     (3) Copying mutable objects to immutable area.
     (4) Updating references to the moved objects.
     (5) Zeroing both buffers.
     
  Here we have to mark everything and zero the immutable buffer, both of
  which costs are independent of the amount of space we free in the mutable
  buffer.
  
  Making the mutable buffer big reduces the number of partial GCs; making
  the immutable buffer big reduces the number of full GCs. Currently, it
  looks like a better bet to do the latter, so let's try it! Well I did,
  and it was a disaster - GC time went from 23% to 53% on my standard
  job (remake ML compiler using a 10MB heap). The problem seems to be
  that the extra partial GCs were just too expensive.
  
  However, I don't see any point in setting the immutable percentage to
  less than 100% - this just seems to reduce the effective size of the
  immutable buffer. Conversely, we want to set the mutable percentage low
  to stop the mutable buffer filling up with junk, which would increase
  the costs of every subsequent partial GC.
  
  SPF 7/10/96
*/

  /* We always check the mutable area now. */
  A.flags = M_WRITABLE | M_PROFILED;

  user_arg_count   = 0;
  user_arg_strings = 0;
  
  for (i = 1; i < argc; i++)
  {
    if (argv[i][0] == '-')
    {
           if (! strcmp(argv[i],"-r")           ) A.flags &= ~M_WRITABLE;
      
      else if (! strncmp(argv[i],"-H",2)        ) GETNUMBER("-H" ,1024,hsize)
      else if (! strncmp(argv[i],"-IB",3)       ) GETNUMBER("-IB",1024,isize)
      else if (! strncmp(argv[i],"-MB",3)       ) GETNUMBER("-MB",1024,msize)
      
      else if (! strncmp(argv[i],"-h",2)        ) GETNUMBER("-h" ,1,hsize)
      else if (! strncmp(argv[i],"-ib",3)       ) GETNUMBER("-ib",1,isize)
      else if (! strncmp(argv[i],"-mb",3)       ) GETNUMBER("-mb",1,msize)
      
      else if (! strncmp(argv[i],"-ip",3)       ) GETNUMBER("-ip",1,A.I.percent)
      else if (! strncmp(argv[i],"-mp",3)       ) GETNUMBER("-mp",1,A.M.percent)
      
      else if (! strcmp(argv[i],"-sf")          ) GETSTRING("-sf",A.stats_name)
      else if (! strncmp(argv[i],"-s",2)        ) GETNUMBER("-s" ,1,A.stats_flags)
      else if (! strncmp(argv[i],"-D",2)        ) GETNUMBER("-D" ,1,A.debug)
      else if (! strncmp(argv[i],"-t",2)        ) GETNUMBER("-t" ,1,A.timeslice)
      else if (! strcmp(argv[i],"-noDisplay")) { A.noDisplay = 1; }
      else if (! strncmp(argv[i],"-q",2))	{ be_silent = 1; }
      else if (! strncmp(argv[i],"-v",2)) 
      {
	      proper_printf("Poly/ML RTS version %s\n",poly_runtime_system_version);
	      Exit(0);
      }
	  else if (! strcmp(argv[i],"-?")) { Usage(0); }
	  else if (! strcmp(argv[i],"--"))
        {
          user_arg_count   = argc - (i + 1);
          user_arg_strings = &(argv[i+1]);
          break;
        }

      else proper_printf("Unknown option %s\n",argv[i]);
    }
    else A.filename = argv[i];
  }

  if (! be_silent)
  {
  proper_printf("Poly/ML RTS version %s\n",poly_runtime_system_version);
  }

  if (A.filename == 0)
  {
#ifdef WINDOWS_PC
  /* Try asking for it. */
		static char filename[MAX_PATH];
		if (getDBFileName(filename, MAX_PATH)) A.filename = filename;
#else
	/* Default to ML_dbase */
  	A.filename = "ML_dbase";
#endif
  }
  if (A.filename == 0) Usage("No database supplied");

  if (hsize < 500) Usage ("Invalid heap-size value");
  if (100 < A.I.percent) Usage ("Invalid immutables-percent value");
  if (100 < A.M.percent) Usage ("Invalid mutables-percent value");
  
  if (1 <= A.timeslice && A.timeslice <= 10000)
  {
     /*do nothing, carefully */
  }
  else
  {
     Usage ("Timeslice must be in the range 1 .. 10000ms");
  }

  if (hsize < isize) hsize = isize;
  if (hsize < msize) hsize = msize;
  
  if (isize == 0) isize = hsize / 5;  /* set default immutable buffer size */
#if 0
  /* This gets too expensive for very large heaps */
  if (msize == 0) msize = 2 * hsize / 5;  /* set default mutable buffer size */
#else
  /* This is slightly less efficient for small heaps,
     but may work better for large ones. SPF 12/1/1998
  */
  if (msize == 0) msize = 4 * 1024 + hsize / 5;  /* set default mutable buffer size */
#endif  
  /* Reduce requests to system maxima. Note that I_ABSOLUTE_MAXIMUM etc. 
     are defined in WORDS, but isize etc. are defined in K.
  */
#define MIN2(x,y) ((x) < (y) ? (x) : (y)) 
#define MAX2(x,y) ((x) < (y) ? (y) : (x)) 
#define COUNTCHUNKS(x) ROUNDDOWN_UNITS(x, CHUNKWORDS)
#define K_PER_CHUNK ROUNDDOWN_UNITS(CHUNKBYTES, 1024)
  isize = MIN2(isize, COUNTCHUNKS(I_ABSOLUTE_MAXIMUM) * K_PER_CHUNK);
  msize = MIN2(msize, COUNTCHUNKS(M_ABSOLUTE_MAXIMUM) * K_PER_CHUNK);
  hsize = MIN2(hsize, COUNTCHUNKS(MAX2(I_ABSOLUTE_MAXIMUM, M_ABSOLUTE_MAXIMUM)) * K_PER_CHUNK);
#undef MIN2
#undef MAX2
#undef COUNTCHUNKS
#undef K_PER_CHUNK

  if (! be_silent)
  {
  proper_printf("Running with heap parameters (h=%dK,ib=%dK,ip=%d%%,mb=%dK,mp=%d%%)\n",
           hsize,
           isize,
           A.I.percent,
           msize,
           A.M.percent);
  }
  A.heap_size = K_to_words(hsize);
  A.I.bsize   = K_to_words(isize);
  A.M.bsize   = K_to_words(msize);
  
  assert ((Bytes(& A)+4)  == Bytes(& A.M.bottom) ); /* needed in assembly code */
  assert ((Bytes(& A)+8)  == Bytes(& A.M.pointer)); /* needed in assembly code */
  assert ((Bytes(& A)+12) == Bytes(& A.M.top)    ); /* needed in assembly code */
    
  /* reserve the address space used by the ML heap, databases etc. */
  {
     int res = ReserveMLSpaces();
     if (res != 0)
     {
        Crash("Unable to reserve ML address spaces\n");
     }
  }

  /* initialise the run-time system before opening the database */
  init_run_time_system();

  if (A.stats_name)
  {
    A.stats_file = fopen (A.stats_name,"a");
    
    if (A.stats_file == NULL) Exit ("Cannot open %s",A.stats_name);
  }
  else A.stats_file = stdout;

  RunMappedDatabase ((Header)NULL);
  
  enter_poly_code(); /* does not return */

  Crash ("Oh yes it does");

  /*NOTREACHED*/
  return 0; /* just to keep lint happy */
}

void Uninitialise(void)
/* Close down everything and free all resources. */
{
	uninit_run_time_system();
	UninitMemoryMapping(H);
}

#if defined(WINDOWS_PC) /* PC version	*/
static char *PrintTime(long tv)
{
  static char buffer[10];

  long tenths = tv / 100;
  
  sprintf (buffer,"%4ld.%1ld",tenths/10,tenths%10);
  return buffer;
}

static int GCPercent(long total, long gc)
{
  long ttotal = total / 100;
  long tgc    = gc / 100;
  
  if (ttotal == 0) return 0;
  
  return 100 * tgc / ttotal;
}

void finish (int n)
{
  if (A.stats_flags)
  {
    word   gc_u;
    word   gc_s;

    FILETIME cT,eT,sT,uT;
    long Rsys, Rusr;
    HANDLE h = GetCurrentProcess();
    GetProcessTimes(h,&cT,&eT,&sT,&uT);

    /* Use 32(dwLowDateTime) + 8(from dwHighDateTime) bits of FILENAME */
    /* expressed in milliseconds */
    Rsys = ((long)_scalb((sT.dwHighDateTime && 0x000000FF),8) + 
                          sT.dwLowDateTime) / 10000;
                          
    Rusr = ((long)_scalb((uT.dwHighDateTime && 0x000000FF),8) + 
                          uT.dwLowDateTime) / 10000;

    gc_u = GCPercent(Rsys, A.gc_stime);
    gc_s = GCPercent(Rusr, A.gc_utime);

    proper_fprintf(A.stats_file,"\n");
    proper_fprintf(A.stats_file,"Heap:    %6dK immutables\n" ,KB(A.I.top - A.I.bottom));
    proper_fprintf(A.stats_file,"Heap:    %6dK mutables\n"   ,KB(A.M.top - A.M.bottom));
    proper_fprintf(A.stats_file,"Heap:    %6d  partial GCs\n",A.partial_gcs);
    proper_fprintf(A.stats_file,"Heap:    %6d  full GCs\n"   ,A.full_gcs);

    proper_fprintf(A.stats_file,"GC Time: %su %d%%\n"        ,PrintTime (A.gc_utime),gc_u);
    proper_fprintf(A.stats_file,"GC Time: %ss %d%%\n"        ,PrintTime (A.gc_stime),gc_s);
    proper_fprintf(A.stats_file,"Time:    %su\n"             ,PrintTime (Rusr));
    proper_fprintf(A.stats_file,"Time:    %ss\n"             ,PrintTime (Rsys));
    proper_fprintf(A.stats_file,"\n");
  }

  Uninitialise();
  ExitThread(n);
}

/* end of PC-specific code */

#else /* SYSV-ish */
/* time now expressed in ticks of the system clock */
static char *PrintTime(clock_t *tv)
{
  static char buffer[10];
  long ticks_per_second = sysconf(_SC_CLK_TCK);
  long round  = ticks_per_second / 2L;
  long tenths = ((*tv) * 10 + round) / ticks_per_second;
  
  sprintf (buffer,"%4ld.%1ld",tenths / 10,tenths % 10);
  
  return buffer;
}

static int GCPercent(clock_t *total, clock_t *gc)
{
  long ttotal = *total;
  long tgc    = *gc;
  long round  = ttotal / 2L;
  
  if (ttotal == 0) return 0;
  
  return (100 * tgc + round) / ttotal;
}

void finish(int n)
{
  if (A.stats_flags)
  {
    word   gc_u;
    word   gc_s;

    struct tms R;
    times(&R);

    gc_u = GCPercent (& R.tms_utime,& A.gc_utime);
    gc_s = GCPercent (& R.tms_stime,& A.gc_stime);

    proper_fprintf(A.stats_file,"\n");
    proper_fprintf(A.stats_file,"Heap:    %6dK immutables\n" ,KB(A.I.top - A.I.bottom));
    proper_fprintf(A.stats_file,"Heap:    %6dK mutables\n"   ,KB(A.M.top - A.M.bottom));
    proper_fprintf(A.stats_file,"Heap:    %6d  partial GCs\n",A.partial_gcs);
    proper_fprintf(A.stats_file,"Heap:    %6d  full GCs\n"   ,A.full_gcs);

    proper_fprintf(A.stats_file,"GC Time: %su %d%%\n"        ,PrintTime (& A.gc_utime),gc_u);
    proper_fprintf(A.stats_file,"GC Time: %ss %d%%\n"        ,PrintTime (& A.gc_stime),gc_s);
    proper_fprintf(A.stats_file,"Time:    %su\n"             ,PrintTime (& R.tms_utime));
    proper_fprintf(A.stats_file,"Time:    %ss\n"             ,PrintTime (& R.tms_stime));
    proper_fprintf(A.stats_file,"\n");
  }

  exit (n);
}
/* end of SOLARIS-specific code */
#endif


/* CALL_IO3(assign_byte_long_, REF, REF, REF, NOIND) */
word assign_byte_long_c(Handle value_handle, Handle byte_no, Handle vector)
{
  unsigned value = (unsigned)DEREFHANDLE(value_handle);

  word  offset  = get_C_ulong(DEREFWORDHANDLE(byte_no));  /* SPF 31/10/93 */
  byte *pointer = DEREFBYTEHANDLE(vector);

  /* should only be called from assembly code if the   */
  /* offset is a bigint or if the pointer is not local */
  
  byte v = (byte)UNTAGGED(value);
  
  if (IS_LOCAL_MUTABLE(pointer))
  {
    assert (offset > MAXTAGGED);
    *pointer = v;
  }
  else
  {
    AssignMappedDatabaseByte (H,pointer,offset,v);
  }
  
  return TAGGED(0);
}

/* CALL_IO3(assign_word_long_, REF, REF, REF, NOIND) */
word assign_word_long_c(Handle value_handle, Handle word_no, Handle vector)
{
  word value      = (word)DEREFHANDLE(value_handle);
  unsigned offset = get_C_ulong(DEREFWORDHANDLE(word_no)); /* SPF 31/10/93 */
  word *pointer   = DEREFWORDHANDLE(vector);
  
  /* should only be called from assembly code if the   */
  /* offset is a bigint or if the pointer is not local */
  
  if (IS_LOCAL_MUTABLE(pointer))
  {
    assert (offset > MAXTAGGED);
    
    *pointer = value;
  }
  else
  {
    AssignMappedDatabaseWord (H,pointer,offset,value);
  }
  
  return TAGGED(0);
}

/* CALL_IO5(move_bytes_long_, REF, REF, REF, REF, REF, NOIND) */
/* Move a segment of bytes, typically a string.  */
word move_bytes_long_c(Handle len, Handle dest_offset_handle, Handle dest_handle,
					   Handle src_offset_handle, Handle src_handle)
{
	unsigned src_offset = get_C_ulong(DEREFWORDHANDLE(src_offset_handle));
	byte *source = DEREFBYTEHANDLE(src_handle) + src_offset;
	unsigned dest_offset = get_C_ulong(DEREFWORDHANDLE(dest_offset_handle));
	byte *destination = DEREFBYTEHANDLE(dest_handle);
	byte *dest = destination + dest_offset;
	unsigned bytes = get_C_ulong(DEREFWORDHANDLE(len));

	assert(OBJ_IS_BYTE_OBJECT(DEREFHANDLE(dest_handle)[-1]));

	if (! IS_LOCAL_MUTABLE(dest))  RemoveObjectProtection(H, destination);
	memmove(dest, source, bytes);  /* must work for overlapping segments. */
	return TAGGED(0);
}

/* CALL_IO5(move_words_long_, REF, REF, REF, REF, REF, NOIND) */
/* Move a segment of words.   Similar to move_bytes_long_ except that
   it is used for word segments. */
word move_words_long_c(Handle len, Handle dest_offset_handle, Handle dest_handle,
					   Handle src_offset_handle, Handle src_handle)
{
	unsigned src_offset = get_C_ulong(DEREFWORDHANDLE(src_offset_handle));
	word *source = DEREFWORDHANDLE(src_handle) + src_offset;
	unsigned dest_offset = get_C_ulong(DEREFWORDHANDLE(dest_offset_handle));
	word *destination = DEREFWORDHANDLE(dest_handle);
	word *dest = destination + dest_offset;
	unsigned words = get_C_ulong(DEREFWORDHANDLE(len));

	assert(!OBJ_IS_BYTE_OBJECT(DEREFHANDLE(dest_handle)[-1]));

	if (! IS_LOCAL_MUTABLE(destination)) RemoveObjectProtection(H, (byte*)destination);

	memmove(dest, source, words*sizeof(word));  /* must work for overlapping segments. */
	return TAGGED(0);
}

void runtime_assign_word(word *pointer, word value)
{
  if (IS_LOCAL_MUTABLE(pointer))
  {
    *pointer = value;
  }
  else
  {
    /* TODO: This is safe only if the object is smaller than a page so
	   that we can safely round down the pointer address to the containing
	   page.  That is currently true for all the objects we handle within
	   the RTS but should be changed.  DCJM 22/1/01.
	   It would probably be better to pass the base address and an offset
	   rather than the pointer. */
    AssignMappedDatabaseWord (H,pointer,0,value);
  }
}

/* we must copy database stacks to local store since updates to them occur inline */

StackObject *copy_mapped_stack (old)
StackObject *old;
{
  StackObject *new;
  word        *addr = Words(old);
  
  assert(OBJ_IS_STACK_OBJECT(addr[-1]));
  
  /* SPF 15/11/94 - make sure we reserve enough space
     for the C exception data */
  if (!IS_LOCAL_MUTABLE(addr) || (old->p_space < OVERFLOW_STACK_SIZE))
  {
    /* SPF 3/7/95 We MUST NOT shrink the stack (MJC's code tried to do this)
       because the suspended process may just have done a stack check that
       asked for a MUCH bigger stack than it is apparently using. */
    int res_space = 
      (old->p_space >= OVERFLOW_STACK_SIZE) ? old->p_space : OVERFLOW_STACK_SIZE;
    int extra_space = res_space - old->p_space;
    word oldL = addr[-1];
    word newL;
    word minimum = extra_space + OBJ_OBJECT_LENGTH(oldL);
    word len;
    
    /* make a power of two (why? SPF) */
    for (len = 1; len < minimum; len *= 2)
    {
       /* do nothing */
    }
    
    /* create a length word for the new stack */
    newL = len | OBJ_MUTABLE_BIT | OBJ_STACK_BIT;
    assert(OBJ_IS_LENGTH(newL));
    
    /* allocate the new stack, then copy the contents */
    new = (StackObject *) alloc(newL);
    CopyStackFrame (old,new);
    assert(((word *)new)[-1] == newL);
    
/* SPF 15/11/94 - make sure we reserve enough space for the C exception data */
   assert(new->p_space <= res_space);
   new->p_space = res_space;
#ifdef EXTRA_DEBUG
   proper_fprintf(stderr, "copied stack frame from %p to %p, length %x, reserved %x\n",
	           old, new, len, new->p_space);
#endif    
    return new;
  }
#ifdef EXTRA_DEBUG
  else proper_fprintf(stderr, "Not bothering to copy local mutable stack from %p\n",old);
#endif
  
  return old;
}

