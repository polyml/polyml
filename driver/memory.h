/*
    Title: 	memory.h

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

#ifndef _MEMORY_H_DEFINED
#define _MEMORY_H_DEFINED

/**************************************************************************
 *
 * Include System Headers
 *
 **************************************************************************/
#include <stdio.h>

#include <time.h>

/**************************************************************************
 *
 * Include Basic Headers
 *
 **************************************************************************/

/* needed??? */
#include "globals.h"

/**************************************************************************
 *
 * Area Structure
 *
 * There are 2 extendable areas, one for mutables and one 
 * for immutables. Both grow downwards but they will not 
 * collide since they are extremely far apart.
 *
 * The structure contains the following :
 *
 * . << bottom >>
 * Bottom of area. 
 *
 * . << pointer >>
 * Allocation pointer. 
 *
 * . << top >>
 * Top of area. 
 *
 * . << gen_top >>
 * Top of area to garbage collect. 
 *
 * . << bitmap >> 
 * Bitmap with one bit for each word in the GC area. 
 *
 * . << nbits >>
 * Number of bits in the bitmap. 
 *
 * . << highest >>
 * Index of top word in bitmap.
 *
 * . << start[NSTARTS]
 * Starting points for bit searches. 
 *
 * . << marked >>
 * Count of words marked.
 *
 * . << copied >>
 * Count of words copied. 
 *
 * . << updated >>
 * Count of words updated. 
 *
 * . << lowest >>
 * Lowest object in before copy phase starts. 
 * 
 * . << bsize >>
 * Size of buffer in words.
 * 
 * . << percent >>
 * Percentage at which to do a full GC. 
 *
 **************************************************************************/

#define NSTARTS 1024

typedef struct
{
  word *bottom;          /* bottom of area.                                   */
  word *pointer;         /* allocation pointer.                               */
  word *top;             /* top    of area.                                   */
  word *gen_top;         /* top    of area to garbage collect.                */
  word *gen_bottom;      /* lowest object in area before copying.             */
  word *bitmap;          /* bitmap with one bit for each word in the GC area. */
  int   nbits;           /* number of bits in the bitmap.                     */
  int   highest;         /* index of top word in bitmap.                      */
  int   start[NSTARTS];  /* starting points for bit searches.                 */
  int   start_index;     /* last index used to index start array              */
  int   i_marked;        /* count of immutable words marked.                  */
  int   m_marked;        /* count of mutable words marked.                    */
  int   copied;          /* count of words copied.                            */
  int   updated;         /* count of words updated.                           */
  int   bsize;           /* size of buffer in words.                          */
  int   percent;         /* percentage at which to do a full GC.              */
} Area;

/**************************************************************************
 *
 * Representation of TIME - used in statistics 
 *
 **************************************************************************/
#if defined(WINDOWS_PC) /* PC version */
/* number of milliseconds */
typedef long TIME;

#else /* defined(SOLARIS2) */
typedef clock_t TIME; /* number of ticks of the system clock */
#endif

/**************************************************************************
 *
 * LocalMemory Structure
 *
 * The structure contains the following :
 *
 * . << heap_size >>
 * User defined heap limit in words. 
 *
 * . << M >>
 * Mutable area. 
 *
 * . << I >>
 * Immutable area. 
 *
 * . << full_gcs >>
 * Number of full garbage collects. 
 * 
 * . << partial_gcs >>
 * Number of partial garbage collects.
 *
 * . << garbage_collecting >>
 * 1 if we are in the garbage collector.                               
 *
 * . << gc_utime >>
 * Total user   time spent in GC.
 *
 * . << gc_stime >>
 * Total system time spent in GC.
 *
 * . << filename >>
 * Database filename. 
 *
 * . << flags >>
 * Database flags.
 *
 * . << stats_flags >>
 * Statistics flags. 
 *
 * . << stats_name >>
 * Statistics filename. 
 *
 * . << stats_file >>
 * Statistics file.
 * 
 * . << debug >>
 * Debugging  flags.
 *
 * . << noDisplay >>
 * X display flag.
 *
 **************************************************************************/

typedef struct
{
  int   heap_size;          /* user defined heap limit in words.      */
  Area  M;                  /* Mutable   area.                        */
  Area  I;                  /* Immutable area.                        */
  int   full_gcs;           /* number of full garbage collects.       */
  int   partial_gcs;        /* number of partial garbage collects.    */
  int   garbage_collecting; /* 1 if we are in the garbage collector.  */
  TIME  gc_utime;           /* total user   time spent in GC.         */
  TIME  gc_stime;           /* total system time spent in GC.         */
  char *filename;           /* database   filename                    */
  int   flags;              /* database   flags                       */
  int   stats_flags;        /* statistics flags                       */
  char *stats_name;         /* statistics filename                    */
  FILE *stats_file;         /* statistics file                        */
  int   debug;              /* debugging  flags                       */
  int   noDisplay;          /* X display flag                         */
  int   timeslice;          /* Poly/ML process timeslice (in ms)      */
} LocalMemory;

extern LocalMemory A;

/*********************************************************************
 *
 * DEBUG definitions 
 *
 *********************************************************************/
#define DEBUG_CHECK_OBJECTS 1
#define DEBUG_REMARK        2
#define DEBUG_SET_BREAK     4
#define DEBUG_IMMRO         8
#define DEBUG_FORCEGC       16
#define DEBUG_X             128

/*********************************************************************
 *
 * STATS definitions 
 *
 *********************************************************************/
#define STATS_TOTALS      1
#define STATS_FULL_GCS    2
#define STATS_PARTIAL_GCS 4
#define STATS_TIME        8
#define STATS_FAULTS      16

/*********************************************************************
 *
 * Previous definitions used by mmap.h 
 *
 *********************************************************************/
#include "mmap.h"

/*********************************************************************
 *
 * H - Header used by runtime system 
 *
 *********************************************************************/
extern Header H;

#endif /* _MEMORY_H_DEFINED */
