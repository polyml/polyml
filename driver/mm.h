/*
    Title: 	mm.h 

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

#ifndef MM_H_DEFINED
#define MM_H_DEFINED
#include <time.h> /* for time_t */
#include "globals.h"
#include "objects.h"

/* This header defines the internal structure of a memory mapped database */

/*
  At the beginning of the database is a header containing the following information.
  
  'magic'          A magic number determining which format this database is in.
                   If the format changes then all databases produced with that format
                   get assigned a new magic number.
  
  'header'         The virtual address to which the header
                   should be mapped. This places it out of the way of any
                   of the other virtual address spaces.
  
  'header_length'  This indicates the length of the header in bytes.
  
  'initial_length' This indicates the length of the header and page tables in bytes.
  
  'file_length'    This indicates the length of the database in bytes.

  'fd'             The file descriptor for this open database.
  
  'max_ipages'     These values determine how many immutable and mutable memory pages this
  'max_mpages'     database is allowed to contain. Initially the database is empty but new
                   pages are added on the end and their positions are recorded in a 
                   page table with max_ipages entries.
  
  'ipage_table'    These tables have max_ipages and max_mpages entries, each entry is a file
  'mpage_table'    page number and the tables are indexed by virtual page number. Initially
                   all the pages are unallocated, but as either the immutable or mutable
                   spaces grow then virtual pages start to get used and these must be
                   allocated to file pages within the database.
  
  'i_bottom'       These values determine the virtual address space for
  'i_top'          immutables in the database. This space is initially empty.
  'i_pointer'      This is the allocation pointer for immutables.
  
  'm_bottom'       These values determine the virtual address space for
  'm_top'          mutables in the database. This space is initially empty.
  'm_pointer'      This is the allocation pointer for mutables.
  
  'io_bottom'      These values determine the address and spacing
  'io_top'         of entries in the io_vector used to map Poly/ML system calls
  'io_spacing'     to their corresponding runtime addresses.
  
  'root'           This value determines the entry point into the Poly/ML system.
  
  'nil'            The value nil is an address that is not in the mutable or immutable spaces,
                   and is guaranteed to give a segmentation fault if ever dereferenced.

  'created_date'   This value remembers when the database was created.
  'modified_date'  This value remembers when the database was last modified.
*/

/******************************************************************************
 *
 * PAGE SPECIFIC DEFINITIONS
 *
 ******************************************************************************/
#define UNALLOCATED_PAGE Word(0xDEADDEAD)

#define MM_PAGE  0x0000FFFF
#define MM_DIRTY 0x40000000

#define PAGE_NUMBER(w)   ((w) & MM_PAGE)
#define PAGE_IS_DIRTY(w) ((w) & MM_DIRTY)
#define PAGE_IS_CLEAN(w) (PAGE_IS_DIRTY(w) == 0)

#define SET_DIRTY(p) ((p) |=  MM_DIRTY)
#define SET_CLEAN(p) ((p) &= ~MM_DIRTY)

#define MM_LENGTH      4
#define MM_CPU_LENGTH  32
#define MM_NAME_LENGTH 512

typedef enum { Mutable = 0x1111, Immutable = 0x5555 } stype;

/****************************************************************************
 * 
 * Root structure
 *
 ****************************************************************************/

/* added 9/11/93 SPF */
typedef struct
{
process_base   *process_list;
process_base   *console_chain;
word           *db_specific_list; /* N.B. The offset of this field is
									 hard-wired into get/set_dbentry */
word            UNKNOWN[1];
process_base   *alt_process_list;
process_base   *alt_console_chain;
word		   *signal_vector; /* Added DCJM 18/7/00. */
/* room for expansion below */
word            UNUSED[248];
} root_struct;


/****************************************************************************
 * 
 * Space structure
 *
 * Contains the following :
 *
 * . << max_pages >>
 * Determine how many immutable and mutable memory 
 * pages this database is allowed to contain. Initially 
 * the database is empty but new pages are added on the 
 * end and their positions are recorded in a page table 
 * with max_pages entries.
 *
 * . << page_offset >>
 * File-offset of the page-table for this space.
 *
 * . << length >>
 * Size of the page table for this space (in bytes). 
 *
 * . << stype >>
 * Space type - mutable or immutable. 
 *
 * . << prot >>
 * Protection mode for this space. 
 *
 * . << mode >> 
 * Access mode for this space. 
 *
 * . << page_table >>
 * These tables have max_pages (one for mutables and
 * one for immutables) entries, each entry is a file 
 * page number and the tables are indexed by virtual 
 * page number. Initially all the pages are unallocated, 
 * but as either the immutable or mutable spaces grow, 
 * virtual pages start to get used and these must be 
 * allocated to file pages within the database. 
 *
 * . << bottom, top >>
 * First and last addresses in this space.
 * 
 * . << pointer >>
 * Allocation pointer within this space. 
 *
 * . << bitmap_bytes >>
 * Size of attached (volatile) bitmap. 
 *
 * . << bitmap >>
 * Address of attached bitmap. Used by garbage-collector to mark
 * mutated old objects (mutable space) and for finding the profile
 * counts of old objects (immutable space).
 *
 ****************************************************************************/
typedef struct
{
  unsigned  max_pages;        /* was word - SPF 22/11/93 */
  unsigned long  page_offset; /* was signed - SPF 29/3/96 */
  long  length;
  stype type;
  int   prot;
  int   mode;
  word *page_table;
  word *bottom;
  word *top;
  word *pointer;
  word  bitmap_bytes;
  word *bitmap;
} Space;

/****************************************************************************
 * 
 * MAPPEDFD definition
 *
 ****************************************************************************/

#if defined(WINDOWS_PC)

#include <windows.h>               /* This contains the HANDLE definition */
typedef HANDLE MAPPEDFD;           /* The file descriptor is a HANDLE */
#define NULLFD INVALID_HANDLE_VALUE /* null fd is -1 (?) */

#else /* UNIX version */

typedef int MAPPEDFD;	  /* The file descriptor is an int */
#define NULLFD -1		  /* null fd is -1 */

#endif

/* GCConst is a structure found in gcspace.h containing 
    the following :

   io_bottom   These values determine the address and spacing
   io_top      of entries in the io_vector used to map Poly/ML 
   io_spacing  system calls to their corresponding runtime addresses.
  
   nil  The value nil is an address that is not in the 
        mutable or immutable spaces, and is guaranteed 
        to give a segmentation fault if ever dereferenced.
 */
typedef struct
{
  word *nil;
 /* the bottom of the I/O area */
  word *io_bottom;
 /* the top of the I/O area */
  word *io_top;
  word  io_spacing;
} GCConst;

typedef struct HeaderStruct HEADER;

struct GCSpaceStruct
{
 /* heap bottom - Only used internally in mmap.c */
  word    *h_bottom; 
 /* immutable area bottom */
  word    *i_bottom;
 /* immutable area top */
  word    *i_top;
 /* mutable area bottom */
  word    *m_bottom;
 /* mutable area top */
  word    *m_top;
 /* pointer to parent (linked list like) */
  HEADER *parent;
};

typedef struct GCSpaceStruct GCSpace;

/****************************************************************************
 *
 * HEADER structure
 *
 * It defines the internal structure of a memory mapped database. 
 * The structure contains the following :
 *
 * .<< magic[MM_LENGTH] >>
 * Magic number determining which format this database is in.
 * If the format changes then all databases produced with that
 * format get assigned a new magic number. 
 *
 * . << header >>
 * Where this structure would like to be located. This shouldn't
 * matter, as this structure *ought* to be relocatable!
 *
 * . << header_length >>
 * The length of the header in bytes. 
 *
 * . << initial_length >>
 * The length of the header & page tables in bytes. 
 *
 * . << file_length >> 
 * The length of the database in bytes. 
 *
 * . << fd >>	
 * The file descriptor or Handle (PC version) for this open database.
 *
 * . << flags >>
 * Memory access flags - e.g. M_WRITABLE, M_PROFILED 
 *
 * . << filename >>
 * The name of the file holding the database. 
 *
 * . << m_space >>
 * Space for mutables - see previous structure. 
 *
 * . << i_space >>
 * Space for immutables - see previous structure. 
 *
 * . << next_page >>
 * Next page to be mapped. 
 *
 * . << root >>
 * Determines the entry point into the Poly/ML system.	
 *
 * . << created_date >>
 *  This value remembers when the database was created. 
 *
 * . << modified_date >>
 * This value remembers when the database was last modified. 
 *
 * . << cpu[MM_CPU_LENGTH] >>
 * MM_CPU_LENGTH = 32 
 *
 * . << parent[MM_NAME_LENGTH] >>
 * MM_NAME_LENGTH = 512 
 *
 * . << up >>
 * The header of the parent database.
 *
 * . << gc_const >>
 * GCConst is a structure found in gcspace.h containing 
 * the following :
 *
 * io_bottom   These values determine the address and spacing
 * io_top      of entries in the io_vector used to map Poly/ML 
 * io_spacing  system calls to their corresponding runtime addresses.
 *
 * nil  The value nil is an address that is not in the 
 *      mutable or immutable spaces, and is guaranteed 
 *      to give a segmentation fault if ever dereferenced.
 *
 * . << gc_space >>
 * GCSpace struct is defined in gcspace.h 
 *
 * . << parent_created_date >>
 * Creation date of parent - used to detect database hierarchy problems.
 *
 * . << page_size >>
 * The page size (in bytes) of the pages stored in this database.
 *
 * . << RTS_IF_version >>
 * The version of the RTS interface protocol used by this database.
 *
 ***************************************************************************/

struct HeaderStruct
{
  char    magic[MM_LENGTH];
  Header  header;
  long    header_length;
  long    initial_length;
  long    file_length;
  MAPPEDFD fd;
  int     flags;
  char   *filename;
  Space   m_space;
  Space   i_space;
  word    next_page;
  root_struct *root;            /* Handle ??? */
  time_t  created_date;
  time_t  modified_date;
  char    cpu[MM_CPU_LENGTH];
  char    parent[MM_NAME_LENGTH];
  Header  up;
  GCConst gc_const;
  GCSpace gc_space;
  time_t  parent_created_date; /* SPF 9/12/96 */
  int     page_size;           /* SPF 22/5/97 */
  int     RTS_IF_version;      /* SPF 19/2/1998 */
};


/***************************************************************************
 * 
 *  MM_CPU_TYPE
 *
 ***************************************************************************/
#if defined(INTERPRETED)
#define MM_CPU_TYPE "Interpreted"

#elif defined(SPARC)
#define MM_CPU_TYPE "Sun Sparc"

#elif defined(POWER2)
#define MM_CPU_TYPE "POWER2/PowerPC"

#elif defined(HPPA)
#define MM_CPU_TYPE "HP Precision Architecture"

#elif defined(WINDOWS_PC)
#define MM_CPU_TYPE "WNT - i386"

#elif defined(LINUX) || defined(FREEBSD)
#define MM_CPU_TYPE "Linux - i386"
#endif
#endif
