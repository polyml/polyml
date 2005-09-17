/*
    Title: 	objects.h

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

#ifndef _OBJECTS_H
#define _OBJECTS_H

/* word type "word" (at least) */
#include "globals.h"

/* Generally we use a tagged integer representation with 31 bits of
   precision and the low-order bit as a tag bit.  one represents a tagged
   integer, zero a pointer.  Code pointers are always aligned onto a
   word + 2 byte boundary.
   The exception is the Sparc which has special tagged add and subtract
   instructions.  These cause a trap if given a value which is not a
   multiple of 4.  To make use of these instructions we represent tagged
   integers using 30 bits of precision and with the low order bits
   containing 01.  The Power code-generator seems to have been ported
   from the Sparc and retained this representation although I don't think
   there's any other reason why it shouldn't also be converted to use the
   31 bit form.   DCJM 27/9/00.
 */
#if defined(SPARC) || (defined(POWER2) && defined(USE_SPARC_TAGGING))
#define OBJ_PRIVATE_MASK 0x03
#define OBJ_IS_AN_INTEGER(a) (((unsigned)(a) & OBJ_PRIVATE_MASK) == 0x01)
#define OBJ_IS_CODEPTR(a)    (((unsigned)(a) & OBJ_PRIVATE_MASK) == 0x02)
#define OBJ_IS_DATAPTR(a)    (((unsigned)(a) & OBJ_PRIVATE_MASK) == 0x00)
#define IS_INT(x) OBJ_IS_AN_INTEGER((word)x)
#define UNTAGGED(a) ((int)(a) >> 2)
#define UNTAGGED_UNSIGNED(a) ((unsigned)(a) >> 2)
#define TAGGED(a) (((a) << 2) | 0x01)
#define MAXTAGGED 0x1FFFFFFF

#else

#define OBJ_PRIVATE_MASK 0x03
#define OBJ_IS_AN_INTEGER(a) (((unsigned)(a) & 0x01) == 0x01)
#define OBJ_IS_CODEPTR(a)    (((unsigned)(a) & OBJ_PRIVATE_MASK) == 0x02)
#define OBJ_IS_DATAPTR(a)    (((unsigned)(a) & OBJ_PRIVATE_MASK) == 0x00)
#define IS_INT(x) OBJ_IS_AN_INTEGER((word)x)
#define UNTAGGED(a) ((int)(a) >> 1)
#define UNTAGGED_UNSIGNED(a) ((unsigned)(a) >> 1)
#define TAGGED(a) (((a) << 1) | 0x01)
#define MAXTAGGED 0x3FFFFFFF
#endif

/************************************************************************
 *
 * Low level definitions.
 *
 ************************************************************************/


/* Standard macro for finding the start of a code segment from
   a code-pointer. First we normalise it to get a properly aligned
   word pointer. Then we increment this word pointer until we
   get the zero word which is the end-of-code marker. The next
   word after this is the byte offset of the start of the segment,
   so we convert back to a byte pointer to do the subtraction, then
   return the result as a word pointer. SPF 24/1/95
   Note that this code must work even if "cp" is not alligned as
   a code pointer, since the initial program counter used for
   profiling may have a random alignment. SPF 26/1/96 
*/
   
#define OBJ_CODEPTR_TO_PTR(cp,pt) \
do{ \
  word *_wp = (word *)(((unsigned)(cp)) & ~OBJ_PRIVATE_MASK); \
  while (*_wp != 0) _wp++; \
  _wp++; \
  {\
    int _byte_offset = *_wp; \
    (pt) = (word *)((byte *)_wp - _byte_offset); \
  }\
}while(0)


/* Standard macro for finding the start of the constants 
   section of a code-segment. SPF 4/5/95
*/
#define OBJ_PTR_TO_CONSTS_PTR(pt,xp) \
do{ \
  /* avoid multiple uses of pt */ \
  word *_p          = (word *)(pt); \
  int _obj_length   = OBJ_OBJECT_LENGTH(_p[-1]); \
  word *_last_word  = &(_p[_obj_length - 1]); \
  int _consts_count = (int)*_last_word; \
  (xp)              = (word *)(_last_word - _consts_count); \
}while(0)

/* The constant pointer is off-word aligned but points just before the
   constant area. Adding 6 (sic) gives the address of the zero word
   that marks the end of the actual code. The 6 is architecture-specific
   (but they're all 6 so far!) SPF 26/7/96.
*/
#define OBJ_CODEPTR_POINTS_AT_CONSTANTS_SECTION(cp) \
  (*(word*)((unsigned)(cp) + 6) == 0)

/* We have to very careful here, because the RTS code segements
   are malformed - they don't contain the usual count of the
   number of constants.  This means we can't just use a combination
   of the OBJ_CODEPTR_TO_PTR and OBJ_PTR_TO_CONSTS_PTR macros.
    SPF 26/7/96
*/
#define OBJ_CODEPTR_TO_CONSTS_PTR(cp,xp) \
do{ \
  word *_wp = (word *)(((unsigned)(cp)) & ~OBJ_PRIVATE_MASK); \
  while (*_wp != 0) _wp++; \
  (xp) = _wp; \
}while(0)


/* length word flags */

/*
The following encodings are used:

(1) OBJ_PRIVATE_GC_BIT = 0
       A normal length word i.e. 7 more flags + 24 bit length.
       
(2) OBJ_PRIVATE_GC_BIT = 1, OBJ_PRIVATE_MUTABLE_BIT = 0
       A "normal" tombstone. Remaining 30 bits are (word-aligned) pointer >> 2.

(3) OBJ_PRIVATE_GC_BIT = 1, OBJ_PRIVATE_MUTABLE_BIT = 1
       A "depth" tombstone; used by share program. Remaining 30 bits are
       an unsigned integer representing the (approximate) depth of the object
       in the data-structure. 

The following encoding is no longer used:

(4) OBJ_PRIVATE_GC_BIT = 1
       A normal length word i.e. 7 more flags + 24 bit length.
       A marked but not copied object (within CopyGC). You are
       supposed to be able to tell the difference between this
       and case (2) by checking whether the supposed pointer
       is within the new allocation area. This test is
       completely unreliable (some combinations of status bits
       and length are bound to look right) and certainly doesn't
       work when CopyGC is used within the "share" program.
       This MJC-ism (I think) has been removed. Instead,
       we'll use OBJ_PRIVATE_COPYGC_BIT (formerly OBJ_UNUSED_BIT) to
       mark the not-yet-moved objects properly.

SPF 24/1/95
*/

#define OBJ_BYTE_BIT     0x01000000  /* byte object (contains no pointers) */
#define OBJ_CODE_BIT     0x02000000  /* code object (mixed bytes and words) */
#define OBJ_STACK_BIT    0x08000000  /* stack object - may contain internal pointers */
#define OBJ_NEGATIVE_BIT 0x10000000  /* sign bit for arbitrary precision ints */
#define OBJ_PRIVATE_UNUSED_BIT   0x20000000
#define OBJ_MUTABLE_BIT  0x40000000  /* object is mutable */
#define OBJ_PRIVATE_GC_BIT       0x80000000  /* object is (pointer or depth) tombstone */
#define OBJ_PRIVATE_LENGTH_MASK  0x00FFFFFF
#define OBJ_PRIVATE_FLAGS_MASK   0xFF000000

#if 0
#define OBJ_IS_TOMBSTONE(L)  ((L) &  OBJ_PRIVATE_GC_BIT)
#define OBJ_GET_TOMBSTONE(L) ((L) & ~OBJ_PRIVATE_GC_BIT)
#define OBJ_SET_TOMBSTONE(A) (OBJ_PRIVATE_GC_BIT | (A))
#endif

#define OBJ_PRIVATE_DEPTH_MASK (OBJ_PRIVATE_GC_BIT|OBJ_MUTABLE_BIT)

/* case 1 - proper length words */
#define OBJ_IS_LENGTH(L)  (((L) & OBJ_PRIVATE_GC_BIT) == 0)

/* these should only be applied to proper length words */

#define OBJ_OBJECT_LENGTH(L) ((L) & OBJ_PRIVATE_LENGTH_MASK)

#define OBJ_IS_BYTE_OBJECT(L)       ((((unsigned)L) & OBJ_BYTE_BIT) != 0)
#define OBJ_IS_CODE_OBJECT(L)       (((unsigned)(L) & OBJ_CODE_BIT) != 0)
#define OBJ_IS_STACK_OBJECT(L)      (((unsigned)(L) & OBJ_STACK_BIT) != 0)
#define OBJ_IS_NEGATIVE(L)          (((unsigned)(L) & OBJ_NEGATIVE_BIT) != 0)
#define OBJ_IS_MUTABLE_OBJECT(L)    (((unsigned)(L) & OBJ_MUTABLE_BIT) != 0)

#define OBJ_PRIVATE_TYPE_MASK    0x3F000000
/* discards GC flag and mutable bit */
#define OBJ_OBJECT_TYPE(L)   ((L) & OBJ_PRIVATE_TYPE_MASK)

/* Don't need to worry about whether shift is signed, 
   because OBJ_PRIVATE_USER_FLAGS_MASK removes the sign bit.
   We don't want the GC bit (which should be 0) anyway.
*/
#define OBJ_PRIVATE_USER_FLAGS_MASK    0x7F000000
#define OBJ_PRIVATE_FLAGS_SHIFT (8 * (sizeof(word) - 1)) /* currently 24, but will change one day */
#define OBJ_GET_OBJECT_USER_FLAGS(L)  (((L) & OBJ_PRIVATE_USER_FLAGS_MASK) >> OBJ_PRIVATE_FLAGS_SHIFT)
#define OBJ_SET_OBJECT_USER_FLAGS(L,F)(((L) & ~ OBJ_PRIVATE_USER_FLAGS_MASK) | ((F) << OBJ_PRIVATE_FLAGS_SHIFT))


#define OBJ_IS_WORD_OBJECT(L) (OBJ_OBJECT_TYPE(L) == 0)

/* case 2 - forwarding pointer */
#define OBJ_IS_POINTER(L)  (((L) & OBJ_PRIVATE_DEPTH_MASK) == OBJ_PRIVATE_GC_BIT)
#define OBJ_GET_POINTER(L)  Words(((unsigned)((L) & ~OBJ_PRIVATE_DEPTH_MASK))<<2)
#define OBJ_SET_POINTER(pt) ((((unsigned)(pt))>>2) | OBJ_PRIVATE_GC_BIT)

/* case 3 - depth */
#define OBJ_IS_DEPTH(L)  (((L) & OBJ_PRIVATE_DEPTH_MASK) == OBJ_PRIVATE_DEPTH_MASK)
#define OBJ_GET_DEPTH(L) ((int)((L) & ~OBJ_PRIVATE_DEPTH_MASK))
#define OBJ_SET_DEPTH(n) ((n) | OBJ_PRIVATE_DEPTH_MASK)


#define OBJECT_LENGTH(P) OBJ_OBJECT_LENGTH(((word *)(P))[-1])


/*********************************************************************
 *
 * Stack object 
 *
 *********************************************************************/
typedef struct
{
 /* space available */
  word  p_space;
  byte *p_pc;
 /* stack pointer */
  word *p_sp;
 /* handler pointer */
  word *p_hr;
 /* number of checked registers */
  word  p_nreg;
  word  p_reg[1];
} StackObject;

#if defined(i386)
#define CHECKED_REGS 6
#define UNCHECKED_REGS 1

#elif defined(SPARC)
#define CHECKED_REGS 18
#define UNCHECKED_REGS 3

#elif defined(POWER2)
#define CHECKED_REGS 22
#define UNCHECKED_REGS 3

#elif defined(HPPA)
#define CHECKED_REGS 21
#define UNCHECKED_REGS 2

#elif defined(INTERPRETED)
#define CHECKED_REGS 2
#define UNCHECKED_REGS 0
#endif

#if defined(i386) || (defined(LINUX) && defined(POWER2))
/* 
 * C exceptions are handled on the Poly 
 * stack, so it has to be larger. 
 */
/*
   Mklinux does not implement sigstack so we have to use this
   fudge on the PowerPC under Mklinux.
*/
#define EXTRA_STACK 1024
#else /* UNIX version */
#define EXTRA_STACK 0
#endif

/* the amount of ML stack space to reserve for registers,
   C exception handling etc. The compiler requires us to
   reserve 2 stack-frames worth (2 * 20 words) plus whatever
   we require for the register save area. We actually reserve
   slightly more than this. SPF 3/3/97
*/
#define OVERFLOW_STACK_SIZE \
  (50 + \
   sizeof(StackObject)/sizeof(word) + \
   CHECKED_REGS + \
   UNCHECKED_REGS + \
   EXTRA_STACK)

/* Exceptions */
typedef struct ex_data
{
  /* Exc identifier */
  word ex_id;
  /* Exc name */
  pstring ex_name;
  /* Exc arguments */
  word *arg;
} poly_exn;

/***************************************************************************
 * 
 * Types & Definitions for PROCESSES
 *
 ***************************************************************************/

/******************************************
 * Synchroniser - used in ``choice'' forks. 
 ******************************************/
typedef struct synchroniser_struct
{
enum {synch_choice, synch_par, synch_taken } synch_type;
struct synchroniser_struct *synch_base; 
} synchroniser;

/******************************************
 * Process base. 
 ******************************************/
typedef struct proc_base
{ 
 /* Stack for process. */
  StackObject *stack;    

 /* Processes are created by runtime system calls.
    Runnable processes are put into a doubly-linked list. 
    Links to front and back process in chain. */
  struct proc_base *f_chain; 
  struct proc_base *b_chain; 

 /* Status of process. */
  word status;    

 /* Used while blocked for various values, depending on status. 
    If status=PROCESS_BLOCKED or PROCESS_UNBLOCKED it can contain 
    the value being transferred. If status=PROCESS_IO_BLOCK it 
    contains the file-descriptor being waited for. */
  word block_data;  

 /* channel this process is blocked on */
  struct channel_struct *block_channel ;

 /* Chain of console processes. */
  struct proc_base *console_link; 

 /* Synchronisation semaphore. */
  synchroniser *synchro; 

  /* Added  DCJM 8/4/04.  These are used when returning from a foreign function. */
   word lastCallResult; /* Result returned. */
  word lastErrNo; /* Value of errno after return. */
#ifdef WINDOWS_PC
  word lastErrcode; /* Value of GetLastError() after return. */
#endif
 struct proc_base *callbackCaller; /* The process running when a callback was called. */

} process_base;

/*******************************************
 * Channel.
 *******************************************/
typedef struct channel_struct
{
  process_base *senders;
  process_base *receivers;
} channel;

/******************************************
 * CONSTANTS & MACROS for processes.
 ******************************************/
#define PROCESS_RUNABLE         TAGGED(0) /* Not waiting for anything. */
#define PROCESS_IO_BLOCK        TAGGED(1) /* Waiting for I/O */
#define PROCESS_BLOCKED         TAGGED(2) /* Waiting for a channel. */
#define PROCESS_UNBLOCKED       TAGGED(3) /* Was waiting */
/* PROCESS_INTERRUPTABLE.  Added DCJM 23/7/00.  Indicates that the
   process is waiting for a system call such as Posix.Process.sleep
   and should be set to raise a syscall exception if a signal is received.
   Used only in conjunction with PROCESS_IO_BLOCK. */
#define PROCESS_INTERRUPTABLE	TAGGED(8)

/* Mask for these bits. */
#define PROCESS_STATUS \
   (PROCESS_RUNABLE | PROCESS_IO_BLOCK  | \
    PROCESS_BLOCKED | PROCESS_UNBLOCKED | \
    PROCESS_INTERRUPTABLE)

/* 
 * An interruptible process. 
 * (CTRL+C raises an interrupt to all console processes) 
 */
#define PROCESS_IS_CONSOLE TAGGED(0x4000)

#define GET_PROCESS_STATUS(p) ((p)->status & PROCESS_STATUS)

#define NO_PROCESS ((process_base *)TAGGED(0))
#define NO_SYNCH   ((synchroniser *)TAGGED(0))
#define NO_CHANNEL ((struct channel_struct *)TAGGED(0))


#endif
