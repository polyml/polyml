/*
    Title:      Garbage Collector

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

#if defined(WINDOWS_PC)
#include <float.h>
#include <time.h> 
#include <windows.h> /* contains protection constants */
#include <malloc.h>

#else
/* UNIX */

#include <sys/times.h>

#if defined(BSD) || defined(FREEBSD) || defined(MACOSX)
#include <sys/time.h>
#endif

#include <sys/resource.h>
#include <unistd.h>
#include <sys/mman.h>

#endif

#include <string.h>
#include <stdlib.h>

#if !defined(WINDOWS_PC)
#include <assert.h>
#else
/* redefine assert, because the standard version
   vanishes	before we can read it. SPF 4/4/95 */
#define assert(exp) \
   ((void)((exp) || (crash("assertion failed: %s, %s, %i\n",	\
                           #exp, __FILE__, __LINE__), 0)))
#endif

#include "globals.h"
#include "objects.h"
#include "memory.h"
#include "run_time.h"
#include "machine_dep.h"
#include "diagnostics.h"
#include "addresses.h"

#include "proper_io.h"
#include "processes.h"
#include "timing.h"
#include "mmap.h"

/* added 18/12/95 SPF for MD_flush_instruction_cache */
#ifndef DONT_FLUSH_CACHE
#include "machine_assembly.h"
#endif


#if 0
/* zero the memory - the "standard" way */
#define wzero(start,word_count) (bzero((void *)(start),(word_count)*sizeof(int)))
#else
/* zero count words of memory, starting at start */
#if defined(__GNUC__)
__inline__
#endif
static void wzero(int *start, int word_count)
{
  int *pointer = start;
  int *top     = start + word_count;
  
  assert(0 <= word_count);

  /* unfolded loop for increased speed */
  while (pointer + 16 <= top)
  {
     pointer[0]  = 0;
     pointer[1]  = 0;
     pointer[2]  = 0;
     pointer[3]  = 0;
     pointer[4]  = 0;
     pointer[5]  = 0;
     pointer[6]  = 0;
     pointer[7]  = 0;
     pointer[8]  = 0;
     pointer[9]  = 0;
     pointer[10] = 0;
     pointer[11] = 0;
     pointer[12] = 0;
     pointer[13] = 0;
     pointer[14] = 0;
     pointer[15] = 0;
     pointer += 16;
  }

  /* ordinary loop, for correct behaviour */
  while (pointer < top)
  {
     pointer[0] = 0;
     pointer += 1;
  }
}
#endif

#if 0
static void CheckBitMap(word *start, int word_count)
{
  int i;
  for (i = 0; i < word_count; i ++)
  {
    if (start[i] != 0)
    {
      proper_fprintf(stderr,"word_count = %i; word %i is %08x\n",word_count,i,start[i]);
    }
  }
}
#endif

/*
  How the garbage collector works.
  
  Phase 1: Starting from the roots in the old mutable area, and
           any pointers kept by the runtime system, we mark all
           objects that are found within the two GC areas.
           The GC areas extend from the allocation pointer
           to the top of the area.
  
  Phase 2: Then we scan the immutable object bitmap. When we find
           a mutable object we try to find space for it in the mutable
           area, if we find an immutable object we try to find space
           for it further up the immutable area. We may have to extend
           the mutable area to make room for objects since we must not commit
           and leave mutable objects in the immutable area.
           
           Then we do the same for the mutable area, copying immutable objects
           out into the immutable area, and moving mutable objects up.
           
           We keep track of the lowest object that could not be moved.
           The allocation pointers will be reset to the lowest kept objects,
           and the area below is taken to be free.
  
  Phase 3: Then we start from the roots and runtime system objects and
           look for pointers into the GC areas that point to tombstones.
           These pointers are changed to point to the new position of
           the objects. Then we process all the objects in the areas
           doing the same thing.
           
Further notes:

  The order of processing the immutable and mutable area has been changed
  since the above comment was written (by Dave Matthews?).

  It would be nice to combine phases 2 and 3 - we could traverse the
  reachable data-structures, starting at the roots, adjusting pointers
  as we go (rather like copyGC). We would only use the bitmap created
  in phase 1 to tell us where to find space to move the new objects.
  
  The main advantage of this approach is that is likely to be
  quicker - we only have to traverse the new (small?) data-structure
  rather than scanning the (large) mutable buffer.
  
  The disadvantage is that it would leave part of the heap dirty,
  and I think parts of the RTS may depend on any unused heap
  word containing zeroes. I'll have to look at this very closely!
  Note that this is a different issue from the compiler requiring
  the area below the allocation area to be zeroed. (Should we
  fix this?) Here we are talking about the area *above* the
  allocation pointer, which may contain objects, tombstones
  and zero words only.
  
  A second disadvantage is that the "compress" pass may not give
  as good compression as currently, because it wouldn't explicitly
  start at the bottom and work up. In recompense, we would be able
  to recycle all but the length word of a tombstone, so our
  actual space usage might improve.
  
  SPF 21/10/96
  
  I've now deleted all that carefully optimised code that used to zero the
  heap - it's now the responsibility of the compiler (and alloc) to ensure
  that the store is correctly initialised whenever a GC might occur.
  
  SPF 22/10/96
  
  The GC is required to "clean" each area of the heap between pointer and top;
  this area may only contain objects, tombstones and zero words. The GC
  currently does this for the benefit of OpMutableBlock, but this behaviour
  is also required for the PrintLocalProfileCounts in run_time.c to avoid
  core dumps.
  
  SPF 23/10/96
  
  Let's try to improve the design of the garbage collector, by doing partial GCs
  in 5 phases:
  
     (1) Mark
     (2) CopyImmutables
     (3) FixupImmutable
     (4) CopyMutables
     (5) FixupMutables

   What are the advantages/disadvantages of the new approach?
      
       Advantage:
      
           We can copy mutables into the holes left by copying-out immutables,
           which gives better compaction of the mutable area. The inability
           to do this is currently a problem for some applications because
           it triggers far too many full GCs.
        
       Disadvantage:
      
           We have to run the copy and fix-up phases twice. This may be expensive.
        
   Can we get the advantage without the disadvantage by only splitting the Copy
   and Fixup phases when this looks like a win?
   
   Note: we have to separate the Mark and Copy phases, as otherwise we won't be
   able to handle weak pointers. Shame!
   
   SPF 17/12/1997
*/

/* start <= val < end */
#define INRANGE(val,start,end)\
  ((unsigned)(start) <= (unsigned)(val) && (unsigned)(val) < (unsigned)(end))

/* start < val < end */
#define INHARDRANGE(val,start,end)\
  ((unsigned)(start) < (unsigned)(val) && (unsigned)(val) < (unsigned)(end))
  
/* start <= val <= end */
#define INSOFTRANGE(val,start,end)\
  ((unsigned)(start) <= (unsigned)(val) && (unsigned)(val) <= (unsigned)(end))

#define bzero(dest,size) memset(dest,0,size)
#define bcopy(src,dest,size) memmove(dest,src,size) /* must work for overlap */

#if defined(WINDOWS_PC) /* PC version */
/* RW PAGE_EXECUTE_READWRITE only used with SF in DISCGARB phase2 */	  
#define RO PAGE_EXECUTE_READ
#define RC PAGE_EXECUTE_WRITECOPY  
#else
#define RW (PROT_READ | PROT_WRITE | PROT_EXEC)
#define RO (PROT_READ | PROT_EXEC)
#endif

#define max(a,b) (((a) > (b)) ? (a) : (b))

/* check if pointer points in GC area which extends 
   from:
        gen_bottom  - bottom of allocated area
   to the top of the area : 
        gen_top - top of area to garbage collect */
#define IN_GC_IAREA(pt) INRANGE(pt,A.I.gen_bottom,A.I.gen_top)
#define IN_GC_MAREA(pt) INRANGE(pt,A.M.gen_bottom,A.M.gen_top)

/* Code pointers are usually aligned to 2 mod 4 
   However stack->p_pc is not necessarily aligned, so we have to 
   be careful */
#define IN_GC_AREA(pt) (! IS_INT(pt) && (IN_GC_IAREA(pt) || IN_GC_MAREA(pt)))

/* Make sure bitmap addressing is ALWAYS relative to bottom, not pointer. SPF 3/10/96 */
#if 0
/* The old version. GCC 2.7.1 (at least) interprets the body of BITNO as if it were

      ((int)(pt) - (int)((area)->bottom)) / (int)sizeof(word))

   which is not what we want - it produces negative results when the pointers differ
   by more than 2GB. 
   SPF 25/2/1998
*/
#define BITNO(area,pt)       ((pt)    - (area)->bottom)
#define BIT_ADDR(area,bitno) ((bitno) + (area)->bottom)
#else
/* Avoid pointer arithmetic, because GCC doesn't do what we want. 
   WARNING: this will all go wrong when we get 64 bit pointers!
   SPF 25/2/1998
*/
#define WORDPTRSUB(p1,p2)    (((unsigned)(p1) - (unsigned)(p2)) / (unsigned)sizeof(word))
#define BITNO(area,pt)       ((int)WORDPTRSUB(pt,(area)->bottom))
#define BIT_ADDR(area,bitno) ((word *)(((unsigned)(bitno) * (unsigned)sizeof(word)) + (unsigned)((area)->bottom)))
#endif

#ifdef _DEBUG
/* MS C defines _DEBUG for debug builds. */
#define DEBUG
#endif

/* This macro is still OK (I think). */

#ifdef DEBUG

#define Check(pt)        {if (A.debug & DEBUG_CHECK_OBJECTS) _Check(pt); }
#define CheckObject(pt)  {if (A.debug & DEBUG_CHECK_OBJECTS) _CheckObject(pt); }
#define CheckPointer(pt) {if (A.debug & DEBUG_CHECK_OBJECTS) _CheckPointer(pt); }

#define ASSERT(x) assert(x)

#else

#define Check(pt)
#define CheckObject(pt)
#define CheckPointer(pt)

#define ASSERT(x)

#endif

static int IsChunkAligned(const word *ptr)
{
  return (((unsigned)ptr) % CHUNKBYTES == 0);
}



#ifdef DEBUG



static void _Check (word *pt)
{
  if (pt == 0) return;
  
  if (OBJ_IS_AN_INTEGER(Word(pt))) return;
  
  /* Test corrected 20/2/95 - the stack contains code pointers 
     (for return addresses) as well as simple data pointers */
  assert (OBJ_IS_CODEPTR(Word(pt)) || OBJ_IS_DATAPTR(Word(pt)));

  if (INRANGE(pt,A.M.pointer,A.M.top)) return;
  if (INRANGE(pt,A.I.pointer,A.I.top)) return;
  
  if (IsDatabasePointer (H,pt)) return;
  
  Crash ("Bad pointer 0x%08x found",pt);
} 

static void _CheckConstant (word **pt)
{
	_Check(*pt);
}

static void _CheckObject (word *base)
{
  word  L;
  word  F;
  word  n;
  word *end;
  word **pt; /* was *pt */
  
  assert (! IS_INT(base));
  
  _Check (base-1);

  pt  = (word **)base;
  
  L   = base[-1];
  assert (OBJ_IS_LENGTH(L));
  
  n   = OBJ_OBJECT_LENGTH(L);
  assert (n > 0);
  
  end = base + n;

  _Check (end-1);
  
  F = OBJ_OBJECT_TYPE(L);  /* discards GC flag and mutable bit */

  if ((F & ~OBJ_NEGATIVE_BIT) == OBJ_BYTE_BIT) /* possibly signed byte object */
     return; /* Nothing more to do */

  if (F == OBJ_STACK_BIT)
  {
    StackObject *stack = (StackObject *) base;
    word         skip  = stack->p_sp - base;
    
    assert (INRANGE(stack->p_sp,base,end));
    assert (INRANGE(stack->p_hr,base,end));
    
    /* Skip to the start of the stack. */
    assert (skip < n);
    
    pt += skip; 
    n  -= skip;
  }
  else if (F == OBJ_CODE_BIT) /* code object */
  {
	  /* We flush the instruction cache here in case we change any of the
	     instructions when we update addresses. */
	  MD_flush_instruction_cache(pt, (n + 1) * sizeof(word));
	  MD_update_code_addresses(pt, pt, L, _CheckConstant);
    /* Skip to the constants. */
    
    pt += n;
    n   = (word)pt[-1];
    pt -= n + 1;
  }
  else assert (F == 0); /* ordinary word object */

  while (n--) _Check (*pt++); /* SPF 4/5/95 */
}

static void _CheckPointer (word *pt)
{
  if (pt == 0) return;
  
  if (OBJ_IS_AN_INTEGER(Word(pt))) return;
  
  if (IsNil(H,pt)) return;
  
  if (IsIOPointer(H,pt)) return;

  _Check (pt);
    
  _CheckObject (pt);
}

#endif

void CopyStackFrame(StackObject *old_stack, StackObject *new_stack)
{
  /* Moves a stack, updating all references within the stack */
#define old_base ((word *)old_stack)
#define new_base ((word *)new_stack)

  word         i,n;
  word       **old;
  word       **new;
  int          offset;
  word         old_length = OBJ_OBJECT_LENGTH(old_base[-1]);
  word         new_length = OBJ_OBJECT_LENGTH(new_base[-1]);
  word        *old_top    = old_base + old_length;
  
  CheckObject (old_base);
  
  assert (OBJ_IS_STACK_OBJECT(old_base[-1]));
  assert (OBJ_IS_STACK_OBJECT(new_base[-1]));
#if 0
  /* This doesn't hold if we a copying a "frozen" stack on system start-up */
  assert (OBJ_IS_MUTABLE_OBJECT(old_base[-1]));
#endif
  assert (OBJ_IS_MUTABLE_OBJECT(new_base[-1]));
  
  /* Calculate the offset of the new stack from the old. If the frame is
     being extended objects in the new frame will be further up the stack
     than in the old one. */
       
  offset = new_base - old_base + new_length - old_length;
    
  /* Copy the registers, changing any that point into the stack. */
  
  new_stack->p_space = old_stack->p_space;
  new_stack->p_pc    = old_stack->p_pc;
  new_stack->p_sp    = old_stack->p_sp + offset;
  new_stack->p_hr    = old_stack->p_hr + offset;
  new_stack->p_nreg  = old_stack->p_nreg;

  /* p_nreg contains contains the number of CHECKED registers */
  assert(new_stack->p_nreg == CHECKED_REGS);

  for (i = 0; i < new_stack->p_nreg; i++)
  {
    /* R is a pointer to p_reg[i] within the old stack */
    word *R = Words(old_stack->p_reg[i]);
    
   /* if the register points into the old stack, make the new copy
      point at the same relative offset within the new stack,
      otherwise make the new copy identical to the old version. */
          
   /* Question: what if it's an int? Better check! SPF 4/5/95 */
    if (!OBJ_IS_AN_INTEGER(R) && INRANGE(R,old_base,old_top))
    {
      new_stack->p_reg[i] = Word(R + offset);
    }
    else
    {
      new_stack->p_reg[i] = Word(R);
    }
  }

  /* Copy unchecked registers. - The next "register" is the number of
     unchecked registers to copy. Unchecked registers are used for 
     values that might look like addresses, i.e. don't have tag bits, 
     but are not. */

  n = old_stack->p_reg[i];
  new_stack->p_reg[i] = n;
  
  i++;
  
  assert (n < 100);
  
  while (n--)
  { 
    new_stack->p_reg[i] = old_stack->p_reg[i];
    i++;
  }

  /* Skip the unused part of the stack. */
  
  i = old_stack->p_sp - old_base;

  assert (i <= old_length);

  i = old_length - i;
  
  old = (word **) old_stack->p_sp;
  new = (word **) new_stack->p_sp;
    
  while (i--)
  {
    word *old_word = *old++;
    
    /* Question: what if it's an int? Better check! SPF 17/2/97 */
    if (!OBJ_IS_AN_INTEGER(old_word) && INRANGE(old_word,old_base,old_top))
    {
      *new++ = old_word + offset;
    }
    else
    {
      *new++ = old_word;
    }
  }

  CheckObject (new_base);
#undef old_base
#undef new_base
}

/* ForConstant - Process a constant in the code segment.
   Added primarily as an auxiliary function for
   MD_update_code_addresses.  This is used for the disc garbage
   collector as well as the memory collector. DCJM 1/1/2001
*/
void ForConstant(word **pt, void (*op)())
{
	word *val = *pt;
	if (IS_INT(val) || val == 0)
	{
		/* We can get zeros in the constant area if we garbage collect
		   while compiling some code. */
		/* Do nothing. */
	}
	else if (OBJ_IS_CODEPTR(val))
	{
        word *old;
        word *new;
        /* Find the start of the code segment */
        OBJ_CODEPTR_TO_PTR(val,old);
        new = old;
        (* op) (& new);  /* new may move if we are in the update phase */
        *pt += (new-old);  /* so move original code pointer */
	}
	else
	{
		ASSERT(OBJ_IS_DATAPTR(val));  
		/* normal pointer */
		(* op) (pt);
	}
}

static void ForAddressesInStack
(
  word **pt,
  word   length,
  word  *base,
  word   is_code,
  void (*op)()
)
{
  word L    = base[-1];
  word *end = base + OBJ_OBJECT_LENGTH(L);
  
  assert(OBJ_IS_LENGTH(L));
  assert(OBJ_IS_STACK_OBJECT(L) || OBJ_IS_CODE_OBJECT(L));
  
  while (length--)
  {
    word *val = *pt; /* N.B. This pointer may be UNALIGNED!!! */
    
    if (IN_GC_IAREA(val) || IN_GC_MAREA(val)) /* In the area we are collecting */
    {
      /* 
         Don't use the IN_GC_AREA in the above test, because the
         saved pc may look like a short integer (on the byte-code
         version, for example). We mustn't use the IS_INT until
         *after* we've checked is_code. Guess who found this out
         the hard way? SPF 8/9/95
      */
    
      if (is_code || OBJ_IS_CODEPTR(val))
      {
        /* 
           Trace the value in the program counter or return address.
           What if the program counter points into the assembly code?
           That's O.K. because it won't satisfy IN_GC_AREA.
        */
        word *old;
        word *new;
        
        /* Find the start of the code segment */
        OBJ_CODEPTR_TO_PTR(val,old);
          
        new = old;
          
        (* op) (& new);  /* new may move if we are in the update phase */
      
        *pt += (new-old);  /* so move original code pointer */
      }
      
      else if (IS_INT(val))
      {
        /* 
           Do nothing, carefully. An integer may "just happen" to
           satisfy the above range checks, so now that we know
           that we're not checking the saved pc, we must exclude it.
           SPF 8/9/95
        */
      }
      
      else
      {
	/*
	   We've dealt with code-pointers; now we have to cope with:
	      (1) Pointers into the stack-object itself
	      (2) Proper pointers that we want to follow.
	*/  
         ASSERT(OBJ_IS_DATAPTR(val)); /* SPF 27/1/95 */      
      
         if (INHARDRANGE(val,base,end))
         {
           /* 
             Do nothing, carefully - pointer into stack-segment itself.
             These will be treated specially in the fix-up phase.
             Note: INHARDRANGE, not INRANGE, since if val == base, we
             just have a normal pointer.
             SPF 27/1/95
           */
         }
         else
         {
           /* normal pointer */
           (* op) (pt);
         }
       }
    }
   
    pt++;
  }
}

static void ForAddressesInObject(word  *base, void (*op)(word **))
{
  word   L  = base[-1];
  word **pt = (word **)base;
  word   n;
  
  ASSERT (OBJ_IS_LENGTH(L));

  CheckObject (base);

  if (OBJ_IS_BYTE_OBJECT(L)) return; /* Nothing more to do */

  n = OBJ_OBJECT_LENGTH(L);
  
  ASSERT (n > 0);

  if (OBJ_IS_STACK_OBJECT(L))
  {
    StackObject *stack = (StackObject *) pt;
    word         skip  = (word **)stack->p_sp - pt;

    ASSERT (skip <= n);
    
    /* don't we have a potential alignment problem with stack->p_pc here? SPF 27/10/93 */
    
    ForAddressesInStack ((word **)(& (stack->p_pc)),1,base,1,op);  /* is code pointer */
    ForAddressesInStack (& (stack->p_sp),1,base,0,op);
    ForAddressesInStack (& (stack->p_hr),1,base,0,op);
    
    ForAddressesInStack ((word **)stack->p_reg,stack->p_nreg,base,0,op);
    
    /* Skip to the start of the stack. */
      
    pt += skip; 
    n  -= skip;

    ForAddressesInStack (pt,n,base,0,op);
    
    return;
  }
  else if (OBJ_IS_CODE_OBJECT(L))
  {
	/* Process constants within the code. */
    MD_update_code_addresses(pt, pt, L, op);

    /* Skip to the constants. */
    
    pt += n;
    n   = (word)pt[-1];
    pt -= n + 1;

	while (n--)
	{
		ForConstant(pt++, op);
	}
    
    return;
  }
  else
  {
    ASSERT (OBJ_IS_WORD_OBJECT(L));
  }
  
  while (n--)
  {
    word *val = *pt;
    
    Check (val);
    
    if (IN_GC_AREA(val))
    {
      (* op) (pt);
    }
    
    pt++;
  }
}

static void OpMutableBlock(word *addr, unsigned length, void (*op)(word **))
{
  /* The mutable block contains:
       (1) Genuine objects.
       (2) Tombstones that weren't removed by the previous full GC.
       (3) Zero words, where old garbage was zapped by the previous GC.
       
    We appear to treat zero words "accidentally" as zero-length genuine
    objects. Let's see if we can optimise this!
    SPF 8/10/96
 */
  word *top_limit = addr + length;

  while (addr < top_limit)
  {
     word L = *addr;
     addr++;
      
     if (OBJ_IS_POINTER(L))    /* skip over moved object */
     {
        word *val = OBJ_GET_POINTER(L);
        CheckObject(val);
        addr += OBJ_OBJECT_LENGTH(val[-1]);
     }
     else
     {
        ASSERT(OBJ_IS_LENGTH(L)); /* SPF 24/1/95 */
      
        if (OBJ_IS_BYTE_OBJECT(L))
        {
           addr += OBJ_OBJECT_LENGTH(L);
        }
        else if (OBJ_IS_WORD_OBJECT(L))
        {
           int n = OBJ_OBJECT_LENGTH(L);
           while (n--)
           {
              /* Coerce type of addr */
              word **newaddr = (word **)addr;
              word *val = *newaddr;
              Check(val);
            
              if (IN_GC_AREA(val))
              {
                (* op)(newaddr);
              }
              
              addr++;
           }
        }
        else /* A special object (stack or code) */
        {
           ForAddressesInObject(addr, op);
           addr += OBJ_OBJECT_LENGTH(L);
        }
     }
  }
}

#if 0 /* Naive but correct */

static void SetBits(word *bm, word n, word length)
{
  while (length--)
  {
    ASSERT (! TestBit(bm, n));
    SetBit (bm,n);
    n++;
  }
}

#else /* Optimised */

#if defined(__GNUC__)
__inline__
#endif
static void SetBits(word *bitmap,int bitno,int length)
{
  /* Less naive version */
  unsigned word_index = bitno / BITS_PER_WORD;
  
  ASSERT (0 < length);

  /* Set the first part word */
  {
    unsigned start_bit_index = bitno % BITS_PER_WORD;
    unsigned stop_bit_index  = start_bit_index + length;
    /* Do we need to change more than one word? */
    if (stop_bit_index < BITS_PER_WORD)
    {
      /* N.B. test must be "<" not "<=" because a bug(?) in the Sun cc
         compiler treats a shift of BITS_PER_WORD as no shift at all!
         SPF 8/10/96
      */
      const unsigned mask1 = (~ 0) << start_bit_index;
      const unsigned mask2 = (~ 0) << stop_bit_index;
      const unsigned mask  = mask1 & ~mask2;

      ASSERT((bitmap[word_index] & mask) == 0);
      bitmap[word_index] |= mask;
      return;
    }
    else /* Set all the bits we can in the first word */
    {
      const unsigned mask  = (~ 0) << start_bit_index;

      ASSERT((bitmap[word_index] & mask) == 0);
      bitmap[word_index] |= mask;

      /* length = length - (BITS_PER_WORD - start_bit_index); */
      length = stop_bit_index - BITS_PER_WORD;
    }
  }
  
  /* Invariant: 0 <= length */
  
  /* Set as many full words as possible */
  while (BITS_PER_WORD <= length)
  {
    /* Invariant: BITS_PER_WORD <= length */
    word_index ++;
    ASSERT(bitmap[word_index] == 0);
    bitmap[word_index] = ~ 0;
    length -= BITS_PER_WORD;
    /* Invariant: 0 <= length */
  }

  /* Invariant: 0 <= length < BITS_PER_WORD */
  if (length == 0) return;
  
  /* Invariant: 0 < length < BITS_PER_WORD */

  /* Set the final part word */
  word_index ++;
  {
    const unsigned mask1 = (~ 0) << 0;
    const unsigned mask2 = (~ 0) << length;
    const unsigned mask  = mask1 & ~mask2;
    
    ASSERT((bitmap[word_index] & mask) == 0);
    bitmap[word_index] |= mask;
    return;
  }
}
#endif

/* Forward declaration */
static void MarkPointers(word **addr);

/*****************************************************************************/
/* Optimised MarkKnownPointers interface                                     */
/*****************************************************************************/
/* Invariant: pointer pt corresponds to bit bitno of bitmap, and that bit is clear. */
static void MarkKnownPointers(word *pt, Area *area, int bitno)
{
  /* 
   * Mark all the objects pointed to by this object. 
   * Recursively scans the objects apart from the last 
   * word in an object which is followed if it is a pointer. */

  word  L;
  word  n;
  
  RECURSE: /* Loop to simulate tail-recursion. */
  
  ASSERT(!TestBit(area->bitmap, bitno));
  CheckObject (pt);
  
  L = pt[-1];
  ASSERT (OBJ_IS_LENGTH(L));
  
  n = OBJ_OBJECT_LENGTH(L);
  ASSERT (n != 0);
  
  /* Add up the objects to be moved into the mutable area. */
  if (OBJ_IS_MUTABLE_OBJECT(L))
  {
     area->m_marked += n + 1;
  }
  else
  {
     area->i_marked += n + 1;
  }

  /* Mark the segment including the length word. */
  SetBits(area->bitmap, bitno - 1, n + 1);

  if (OBJ_IS_BYTE_OBJECT(L))
  {
     return; /* Ignore byte segments. */
  }
  else if (OBJ_IS_CODE_OBJECT(L) || OBJ_IS_STACK_OBJECT(L))  /* May contain code pointers */
  { 
     ForAddressesInObject(pt, MarkPointers);
     return;
  }
  else
  {
     ASSERT (OBJ_IS_WORD_OBJECT(L));
  }
  
  /* Trim uninteresting pointers off the end of the vector.
     We do this so that our "tail recursion" optimisation
     will be more effective. However, we still mark from
     the *front* of the vector (not the back) because we
     need this to make the optimisation work for lists. 
     SPF 14/1/1998
  */
  while(n != 0)
  {
     word *val = (word *) pt[n-1];
#if 0
     /* old version */
     if (IN_GC_AREA(val)) break;
#else 
     /* experimental version - try to reduce non-tail recursion. SPF 14/1/1998 */
     if (IS_INT(val))
     {
        /* do nothing */
     }
     else if (IN_GC_IAREA(val))
     {
        /* Already marked? */
        int new_bitno = BITNO(&A.I, val);
        if (!TestBit(A.I.bitmap, new_bitno))
        {
           if (n == 1) /* The only interesting word - simulate tail recursion */
           {
              pt     = val;
              area   = &A.I;
              bitno  = new_bitno;
              goto RECURSE;
           }
           else break; /* Fall through to the next loop to mark all the words */
        }
     }
     else if (IN_GC_MAREA(val))
     {
        /* Already marked? */
        int new_bitno = BITNO(&A.M, val);
        if (!TestBit(A.M.bitmap, new_bitno))
        {
           if (n == 1) /* The only interesting word - simulate tail recursion */
           {
              pt     = val;
              area   = &A.M;
              bitno  = new_bitno;
              goto RECURSE;
           }
           else break; /* Fall through to the next loop to mark all the words */
        }
     }
#endif
     n--;
  }

  while (n--)
  {
     word *val = (word *) *pt++;
     CheckPointer (val);
 
     if (IS_INT(val))
     {
        /* do nothing */
     }
     else if (IN_GC_IAREA(val))
     {
        /* Already marked? */
        int new_bitno = BITNO(&A.I, val);
        if (!TestBit(A.I.bitmap, new_bitno))
        {
           if (n == 0) /* Simulate tail recursion */
           {
              pt     = val;
              area   = &A.I;
              bitno  = new_bitno;
              goto RECURSE;
           }
           else MarkKnownPointers(val, &A.I, new_bitno);
        }
     }
     else if (IN_GC_MAREA(val))
     {
        /* Already marked? */
        int new_bitno = BITNO(&A.M, val);
        if (!TestBit(A.M.bitmap, new_bitno))
        {
           if (n == 0) /* Simulate tail recursion */
           {
              pt     = val;
              area   = &A.M;
              bitno  = new_bitno;
              goto RECURSE;
           }
           else MarkKnownPointers(val, &A.M, new_bitno);
        }
     }
     else
     {
        /* Do nothing */
     }
  }
}

/*****************************************************************************/
/* Generic MarkPointers interface                                            */
/*****************************************************************************/
static void MarkPointers(word **addr)
{
  /* 
   * Mark all the objects pointed to by this object. 
   * Recursively scans the objects apart from the last 
   * word in an object which is followed if it is a pointer. */

  word *pt = *addr;
  word  L;
  int  n;
  int bitno;
  Area *area;
  
  CheckObject (pt);
  
  if (IN_GC_IAREA(pt))
  {
     area = &A.I;
  }
  else if (IN_GC_MAREA(pt))
  {
     area = &A.M;
  }
  else
/*
  {
     Crash ("Marking bad pointer "ZERO_X"%p",pt);
  }
*/
	return; /* Return if it's outside the area. */

  bitno = BITNO(area,pt);
  if (TestBit(area->bitmap, bitno)) return; /* Already marked */

  L = pt[-1];
  ASSERT (OBJ_IS_LENGTH(L));
  
  n = OBJ_OBJECT_LENGTH(L);
  ASSERT (n != 0);
  
  /* Mark the segment including the length word. */
  SetBits (area->bitmap, bitno - 1, n + 1);

  /* Add up the objects to be moved into the mutable area. */
  if (OBJ_IS_MUTABLE_OBJECT(L))
  {
     area->m_marked += n + 1;
  }
  else
  {
     area->i_marked += n + 1;
  }

  if (OBJ_IS_BYTE_OBJECT(L))
  {
     return; /* Ignore byte segments. */
  }
  else if (OBJ_IS_CODE_OBJECT(L) || OBJ_IS_STACK_OBJECT(L))  /* May contain code pointers */
  { 
    ForAddressesInObject(pt, MarkPointers);
    return;
  }
  else
  {
     ASSERT (OBJ_IS_WORD_OBJECT(L));
  }
  
  while (n--)
  {
     word *val = (word *) *pt++;
     CheckPointer (val);
 
     if (IS_INT(val))
     {
        /* do nothing */
     }
     else if (IN_GC_IAREA(val))
     {
        /* Already marked? */
        int new_bitno = BITNO(&A.I,val);
        if (!TestBit(A.I.bitmap, new_bitno))
        {
           MarkKnownPointers(val, &A.I, new_bitno);
        }
     }
     else if (IN_GC_MAREA(val))
     {
        /* Already marked? */
        int new_bitno = BITNO(&A.M,val);
        if (!TestBit(A.M.bitmap, new_bitno))
        {
           MarkKnownPointers(val, &A.M, new_bitno);
        }
     }
     else
     {
        /* Do nothing */
     }
  }
}

/*****************************************************************************/
/* These functions are only called with pointers held by the runtime system. */
/* Weak references can occur in the runtime system, eg. streams and windows. */
/* Weak references are not marked and so unreferenced streams and windows    */
/* can be detected and closed.                                               */
/*****************************************************************************/

static void MarkRuntimeObject(word **pt, int weak)
{
  word *val = *pt;
  
  CheckPointer (val);
  
  if (weak) return;
  
  if (IN_GC_AREA(val)) MarkPointers(pt);
}

static void ReferenceRuntimeObject(word **pt, int weak)
{
  /* If the object has not been marked and this is only a weak reference */
  /* then the pointer is set to zero. This allows streams or windows     */
  /* to be closed if there is no other reference to them.                */

  word *val = *pt;
  
  CheckPointer (val);
  
  if (! weak) return;
  
  if (IS_INT(val)) return;
  
  if ((IN_GC_IAREA(val) && ! TestBit(A.I.bitmap, BITNO(&A.I,val))) ||
      (IN_GC_MAREA(val) && ! TestBit(A.M.bitmap, BITNO(&A.M,val))))
  {
    *pt = 0;
  }
}

static void UpdateRuntimeObject(word **pt, int weak)
/* weak is not used, but needed so type of the function is correct */
{
  word *val = *pt;

  if (IN_GC_AREA(val))
  {
    word L = val[-1];
    
    if (OBJ_IS_POINTER(L))
    {
      *pt = val = OBJ_GET_POINTER(L);
    }
    else ASSERT(OBJ_IS_LENGTH(L)); /* SPF 24/1/95 */
    
    CheckObject (val);
  }
}

/*********************************************************************/
/* This function is called in the mark phase to mark pointers to     */
/* objects in the gc area that are in old mutable segments.          */
/*********************************************************************/
static void MarkOldMutables(word **pt, int offset)
{
  word *val = pt[offset];
  
   /* object need not be mutable, since
      it may have become immutable since last GC. */
#if 0	
  assert (OBJ_IS_AN_INTEGER(val) || OBJ_IS_MUTABLE_OBJECT(val[-1]));
#endif
  if (IN_GC_AREA(val))
  {
    CheckObject(val);
    MarkPointers(pt+offset);
  }
}

/*********************************************************************/
/* This function is called in the update phase to update pointers to */
/* objects in the gc area that are in old mutable segments.          */
/*********************************************************************/
static void UpdateOldMutables(word **pt, int offset)
{
  word *val = pt[offset];
  
  if (IN_GC_AREA(val))
  {
    word L = val[-1];

    if (OBJ_IS_POINTER(L))
    {
      AssignMappedDatabaseWord (H,(word *)pt,offset,(word)OBJ_GET_POINTER(L));
      val = pt[offset];
    }
    else ASSERT(OBJ_IS_LENGTH(L)); /* SPF 24/1/95 */
    
    CheckObject (val);
  }
}

/**************************************************************************/
/* This function finds all the mutable objects in the local mutable area. */
/* These are scanned since they may contain references into the gc area.  */
/**************************************************************************/
static void OpNewMutables(void (*op)(word **))
{
  word *top       = A.M.top;
  word *bottom    = A.M.gen_top;
  unsigned length = WORDPTRSUB(top, bottom);
  
  OpMutableBlock(bottom, length, op);
}

#if 0 /* Naive but correct */
/* How many zero bits (maximum n) are there in the bitmap, starting at location bitno? */
static int CountZeroBits(word *bitmap, int bitno, int n)
{
  int i;
  /* Naive version */
  for (i = 0; i < n; i++)
  {
    if (TestBit(bitmap, bitno + i))
    {
      return i;
    }
  }
  return n;
}

#else /* Optimised */

/* How many zero bits (maximum n) are there in the bitmap, starting at location start? */
#if defined(__GNUC__)
__inline__
#endif
static int CountZeroBits(word *bitmap,int bitno,int n)
{
  /* Less naive version */
  unsigned word_index = bitno / BITS_PER_WORD;
  unsigned bit_index  = bitno % BITS_PER_WORD;
  unsigned bits  = bitmap[word_index];
  unsigned mask  = (unsigned)1 << bit_index;
  int zero_bits  = 0;
  ASSERT (0 < n);

  /* Check the first part word */
  while (mask != 0)
  {
     /* zero_bits < n */
     if ((bits & mask) != 0) return zero_bits;
     zero_bits ++;
     if (zero_bits == n) return zero_bits;
     mask <<= 1;
     /* zero_bits < n */
  }
  
  /* zero_bits < n */
  
  /* Check as many full words as possible */
  word_index ++;
  bits = bitmap[word_index];
  while (zero_bits < n && bits == 0)
  {
    zero_bits += BITS_PER_WORD;
    word_index ++;
    bits = bitmap[word_index];
  }

  /* Check the final part word */
  mask = (unsigned)1;
  while (zero_bits < n && ((bits & mask) == 0))
  {
     zero_bits ++;
     mask <<= 1;
  }
  
  return zero_bits;
}
#endif

#if 1 /* SPF's version of FindFree */

/* search the bitmap from the high end down looking for n contiguous zeroes */
#if defined(__GNUC__)
__inline__
#endif
static int FindFree
(
  word *bitmap, /* The bitmap itself */
  int   limit,  /* The highest numbered bit that's too small to use */
  int   bitno,  /* The lowest numbered bit that's too large to use */
  int   n       /* The number of consecutive zero bits required */
)
{
  int candidate = bitno - n;
  ASSERT (bitno > limit);
  
  while (limit < candidate)
  {
     int bits_free = CountZeroBits(bitmap,candidate,n);
  
     if (n <= bits_free)
     {
       return candidate;
     }
     
     candidate -= (n - bits_free);
  }
  /* Drop through - search failed */
  return 0;
}
  
#else /* old version (MJC) */

static word FindFree
(
  word *bitmap,
  int   limit,
  int   bitno,
  int   n
)
{
  /* search the bitmap from the high end down looking for n contiguous zeroes */
  
  word bits,words,L;
  unsigned mask;
  
  L = limit + 32;
  
  assert (bitno > limit);
  ASSERT (BITS_PER_WORD == 32); /* Code below assumes 32 bits per word */
  
  bitno--;
  
  bits = bitmap[bitno >> 5];
  mask = (unsigned)1 << (bitno & 31);

  for (;;)
  {
    while ((bits & mask))
    {
      if (bitno == limit) return 0;  /* didn't find space */

      bitno--; mask >>= 1;

      if (mask == 0)
      {
        bits = bitmap[bitno >> 5];
        mask = (unsigned)1 << 31;

        while (bits == 0xFFFFFFFF && bitno >= L)
        {
          bitno -= 32;
          bits   = bitmap[bitno >> 5];
        }
      }
    }
    
    ASSERT (! TestBit(bitmap, bitno));
    
    words = 1;
    
    do
    {
      if (words == n) return bitno;   /* found n contiguous zeroes */
      
      if (bitno == limit) return 0;  /* didn't find space */
      
      words++; bitno--; mask >>= 1;

      if (mask == 0)
      {
        bits = bitmap[bitno >> 5];
        mask = (unsigned)1 << 31;
        
        /* attempted optimisation - SPF 7/6/96 */
	while (bits == 0 && bitno >= L && words + 32 <= n)
	{
	  words += 32;
	  bitno -= 32;
	  bits   = bitmap[bitno >> 5];
	}
      }
    }
    while (! (bits & mask));
  }
}

#endif

/* Search the area downwards looking for n consecutive free words.          */
/* Return the bitmap index if sucesful or 0 (should we use -1?) on failure. */
#if defined(__GNUC__)
__inline__
#endif
static int FindFreeInArea(Area *dst, int limit, int n)
{
   /* SPF's version of the start caching code. SPF 2/10/96 */
   /* Invariant: dst->start[0] .. dst->start[dst->start_index] is a descending sequence. */
   int truncated_n = (n < NSTARTS) ? n : NSTARTS - 1;
   int i;
   int start; /* Bitmap index at which start bitmap search (searching down) */
   int free;  /* Bitmap index of new allocation */
   
   ASSERT(0 <= limit);

   /* Invariant: dst->start[0] .. dst->start[dst->start_index] is a descending sequence. */ 

   /* 
      Update the starting array, so that the first few entries are valid.
      The starting point for a given size of hole must be at least as
      small (late) as the starting point for smaller holes.
      We remember the start_index of our previous allocation, so
      that if we have the same size object again, this loop becomes
      trivial. SPF 2/10/96
   */ 
   for (i = dst->start_index; i < truncated_n; i ++)
   {
     if (dst->start[i] < dst->start[i+1])
     {
	dst->start[i+1] = dst->start[i];
     }
   }
   
   /* Invariant: dst->start[0] .. dst->start[truncated_n] is a descending sequence. */
   dst->start_index = truncated_n;
   /* Invariant: dst->start[0] .. dst->start[dst->start_index] is a descending sequence. */ 

   /* Start our search at the appropriate point. */
   start = dst->start[truncated_n];
   
   /* If we can't copy UP, give up immediately. It's important that we DON'T
      update dst->start[n], because that might INCREASE it, which isn't
      allowed. SPF 19/11/1997
   */
   if (start <= limit)
   {
      return 0;
   }
   
   free = FindFree(dst->bitmap, limit, start, n);
   /* free == 0 || limit <= free && free < start */
   
   /* 
      We DON'T update the array for big allocations, because this would cause
      us to skip holes that are actually large enough for slightly smaller
      (but still big) allocations. An allocation is "big" if it doesn't
      have its own dedicated slot in the start array. This won't actually
      cost us much, provided there's enough small allocations between
      the big ones, as these will cause the pointer to be advanced.
      SPF 2/10/96
   */
   /* dst->start[0] .. dst->start[dst->start_index] is a descending sequence */
   if (n < NSTARTS)
   {
      /* free == 0 || limit <= free && free < start */
      ASSERT(n == dst->start_index);
      dst->start[n] = (free == 0) ? limit : free;
      /* Writing "dst->start[n] = free;" is attractive but wrong. The problem
         is that even if we can't compact the immutables much, we may still
         be able to copy immutables from the mutable area into the immutable
         area, but setting dst->start[n] to 0 would prevent this.
         SPF 19/11/1997
      */
   }
   /* dst->start[0] .. dst->start[dst->start_index] is still is a descending sequence */
 
   return free;
}

static void doNothing(word **unused)
{
}

static void CopyObjectsInArea(Area *src, int compress_immutables)
{
  /* Start scanning the bitmap from the very bottom since it is    */
  /* likely that very recently created objects need copying.       */
  /* Skip whole words of zeroes since these may be quite common if */
  /* the objects to be copied are sparsely separated.              */
  
  /* Invariant: at this point there are no objects below src->gen_bottom */
  int  bitno   = BITNO(src,src->gen_bottom);
  int  highest = src->highest;
  int *bitmap  = src->bitmap;
  
  for (;;)
  {
    word *old;   /* Old object address */
    word *new;   /* New object address */
    Area *dst;   /* New object allocation area */
    word  L;     /* Length word */
    int   free;  /* Bitmap index of new allocation */
    int   n;     /* Length of allocation (including length word) */

    if (bitno >= highest) return;

    /* SPF version; Invariant: 0 < highest - bitno */
    bitno += CountZeroBits(bitmap, bitno, highest - bitno);

    if (bitno >= highest) return;
      
    ASSERT (TestBit(bitmap, bitno));
    
    /* first set bit corresponds to the length word */
    old = BIT_ADDR(src, bitno);
    
    L = *old;
    ASSERT (OBJ_IS_LENGTH(L));
    CheckObject(old+1);

    n = OBJ_OBJECT_LENGTH(L) + 1;
    bitno += n;

    free = 0;
    
    if (OBJ_IS_MUTABLE_OBJECT(L))
    {
       /* Mutable object - attempt to compact within the mutable area */
       ASSERT(src == & A.M);
       dst   = & A.M;
       free  = FindFreeInArea(dst, bitno, n);
    }
    else /* !OBJ_IS_MUTABLE_OBJECT(L) */
    {
       dst   = & A.I;
       /* 
	  If we're copying to the immutable area and we're just doing sequential
	  allocations at the bottom, we can optimise out all that "clever" search
	  code in FindFreeInArea.
	  SPF 8/12/1997
       */
       if (!compress_immutables)
       {
	  int dest_bitno = BITNO(&A.I, A.I.pointer);
          ASSERT(src == & A.M);
	  free = (n < dest_bitno) ? dest_bitno - n : 0;

       }
       else /* It's a full GC, so try to be compact within the immutable area. */ 
       {
	  free = FindFreeInArea(dst, (src == dst) ? bitno : 0, n);
       }

       /* If the immutable area is full, attempt to compact within the mutable area.  */
       /* This really is a last resort, because partial GCs will get very expensive.  */
       /* However, leaving the object unmoved would be even worse, because that would */
       /* cause the mutable buffer to grow even quicker than is really necessary.     */ 
       if (free == 0 && src == & A.M)
       {
	  dst   = & A.M;
	  free  = FindFreeInArea(dst, bitno, n);
       }
    }
    
    if (free == 0) /* no room */
    {
      /* Update src->pointer, so the old object doesn't get trampled. */
      if (old < src->pointer)
      {
         src->pointer = old;
      }
      /* We haven't been able to move this object on this GC, but we might    */
      /* still be able to move some smaller objects, which might free enough  */
      /* space that we'll be able to move this object on the next GC, even if */
      /* nothing becomes garbage before then. SPF 19/11/1997                  */
      continue;
    }
    
    /* allocate object in the bitmap */
    SetBits(dst->bitmap, free, n);
    new = BIT_ADDR(dst, free);

    /* Update dst->pointer, so the new object doesn't get trampled. SPF 4/10/96 */
    if (new < dst->pointer)
    {
       dst->pointer = new;
    }
    
    if (OBJ_IS_STACK_OBJECT(L))
    {
      *new = L; /* copy length word */
      CopyStackFrame ((StackObject *)(old+1),(StackObject *)(new+1));
      *old = OBJ_SET_POINTER(new+1);
    }
    else /* not a stack object */
    {
      int i = n;
      while (i--)
      {
        new[i] = old[i];
      }

      ASSERT(*new == L);
      *old = OBJ_SET_POINTER(new+1);
      
      CheckObject(BIT_ADDR(dst, free) + 1);

      /* If we have a code object, this is the place to flush the instruction
	 cache at its *new* address (in case we have old code cached at
	 that address). It doesn't matter if we haven't fixed up all the pointers
	 yet, since these are only accessed via the data cache, so it doesn't
	 matter if they are wrong in the instruction cache. Once we get this
	 working, we can avoid flushing the entire instruction cache on every
	 garbage collection which should improve performance somewhat.
	 SPF 14/12/95
      */
      if (OBJ_IS_CODE_OBJECT(L))
      {
		MD_flush_instruction_cache(new, n * sizeof(word));
		/* We have to update any relative addresses in the code. */
		MD_update_code_addresses((word**)(new+1), (word**)(old+1),
			L, doNothing);
      }

    }
      
    dst->copied += n;
  }
}

static void UpdatePointer(word **pt)
{
  word *val = *pt;
  word  L   = val[-1];

  if (! IN_GC_AREA(val)) return; /* DCJM 1/1/2001. */
  
  if (OBJ_IS_POINTER(L))
  {
    val = OBJ_GET_POINTER(L);
    *pt = val;
  }
  else ASSERT(OBJ_IS_LENGTH(L)); /* SPF 24/1/95 */
  
  CheckObject (val);
}

static void UpdateObjectsInArea(Area *area)
{
  word *pt      = area->pointer;
  int   bitno   = BITNO(area, pt);
  int   highest = area->highest;
  int  *bitmap  = area->bitmap;
  word  L;
  
  for (;;)
  {
     ASSERT(bitno <= highest); /* SPF */
   
     /* Zero freed space. This is necessary for OpMutableBlock,
	which expects the old mutable area to contain only
	genuine objects, tombstones and zero words. This is
	all rather sad, since zeroing the mutable buffer in
	this manner may well be one of the hot-spots of the GC.
	At least we only start at area->pointer, so we shouldn't
	normally have to zap *too* much store.
	SPF 22/10/96
     */
     while (bitno < highest && !TestBit(bitmap, bitno))
     {
	*pt++ = 0;
	bitno++;
     }
     
     if (bitno == highest) return;
       
     /* first set bit corresponds to the length word */
     L = *pt++;
     bitno++;
     
     if (OBJ_IS_POINTER(L))    /* skip over moved object */
     {
       word *val = OBJ_GET_POINTER(L);
       
       CheckObject (val);
       
       L = OBJ_OBJECT_LENGTH(val[-1]);
	 
       pt    += L;
       bitno += L;
     }
     else /* !OBJ_IS_POINTER(L) */
     {
	CheckObject (pt);

	if (OBJ_IS_WORD_OBJECT(L))
	{
	   L = OBJ_OBJECT_LENGTH(L);
	   
	   area->updated += L+1;
	   
	   while (L--)
	   {
	      word *val = (word *) *pt;
	      Check (val);
	    
	      if (IN_GC_AREA(val))
	      {
			word T = val[-1];
		
			if (OBJ_IS_POINTER(T))
			{
			   *pt = (word) OBJ_GET_POINTER(T);
			}
			else
			{
			   ASSERT(OBJ_IS_LENGTH(T)); /* SPF 24/1/95 */
			}
		
			CheckObject ((word *)*pt);
	      }
	      
	      pt++;
	      bitno++;
	   }
	}
	
	else /* !OBJ_IS_WORD_OBJECT(L) */
	{
	   L = OBJ_OBJECT_LENGTH(L);
	   area->updated += L+1;
	   ForAddressesInObject(pt, UpdatePointer);
	   pt    += L;
	   bitno += L;
	} /* !OBJ_IS_WORD_OBJECT(L) */
     }  /* !OBJ_IS_POINTER(L) */
   } /* for loop */
}

#if defined(WINDOWS_PC)
static void PrintDelta(const long new, const long old, const char letter)
{
  long dt = new - old;  /* in milliseconds */

  long secs    = dt / 1000;
  long thous   = dt % 1000;
 
  proper_fprintf(A.stats_file,"%4ld.%03ld%c",secs,thous,letter);
}

static void AddDelta(const long old, const long new, long *sum)
{
  *sum = old + new;
}

static long last_state_sys  = 0;
static long last_state_usr  = 0;

#else
/* Unix */
/* new version - times represented as ints counting clock ticks */
static void PrintDelta(const clock_t *new, const clock_t *old, const char letter)
{
  const long fracs_per_second = 100L;
  long ticks_per_second  = CLK_TCK;
  long round             = ticks_per_second / 2L;
  
  long delta_ticks = *new - *old;
  long delta_fracs = (delta_ticks * fracs_per_second + round) / ticks_per_second;
  
  long secs  = delta_fracs / fracs_per_second;
  long fracs = delta_fracs % fracs_per_second;
  
  proper_fprintf(A.stats_file,"%4ld.%02ld%c",secs,fracs,letter);
}

static void AddDelta(const clock_t *old, const clock_t *new, clock_t *sum)
{
  *sum += (*new - *old);
}

static struct tms last_state  = { 0 };
#endif /* SOLARIS2 */

#define GC_START   1
#define GC_NEWLINE 2
#define GC_FULL    4

#if defined(WINDOWS_PC)
/* Function which uses PrintDelta and 
   AddDelta - The code was reorganized here */
static void Stats(gcflags, format, a1, a2, a3)
word  gcflags;
char *format;
int   a1;
int   a2;
int   a3;
{
  word sflags = A.stats_flags;
  word show   = ((sflags & STATS_FULL_GCS) && (gcflags & GC_FULL)) || (sflags & STATS_PARTIAL_GCS); 
  
  if (sflags)
  {
    FILETIME cT,eT,sT,uT;
    long new_state_sys;
    long new_state_usr;
    HANDLE h = GetCurrentProcess();
    
    GetProcessTimes(h,&cT,&eT,&sT,&uT);
    
    /* Use 32(dwLowDateTime) + 8(from dwHighDateTime) bits of FILENAME */
    /* expressed in milliseconds */
    new_state_sys = ((long)_scalb((sT.dwHighDateTime && 0x000000FF),8) + 
				    sT.dwLowDateTime) / 10000;
    new_state_usr = ((long)_scalb((uT.dwHighDateTime && 0x000000FF),8) + 
				    uT.dwLowDateTime) / 10000;
    
    if (show)
    {
       char   buffer[200];
       /* Start of a Stats group - flush out anything preceding it.
	  (This only really makes a difference if A.stats_file is
	   connected to stdout/stderr.) SPF 17/3/97
       */
       if (gcflags & GC_START)
       {
	  proper_fflush(A.stats_file);
       }
       
       if (gcflags & GC_NEWLINE)
       {
	  proper_fprintf(A.stats_file,"\n");
       }
	 
       sprintf (buffer,format,a1,a2,a3);
       proper_fprintf(A.stats_file,"%-50s",buffer);
   
       if (sflags & STATS_TIME)
       {
          PrintDelta(new_state_usr,last_state_usr, 'u');
          PrintDelta(new_state_sys,last_state_sys, 's');
       }
       
       proper_fprintf(A.stats_file,"\n");
       proper_fflush(A.stats_file);
    }
    
    if (! (gcflags & GC_START)) /* Add time for the just-completed GC phase to running total */
    {
      AddDelta(last_state_sys,new_state_sys,&A.gc_stime);
      AddDelta(last_state_sys,new_state_usr,&A.gc_utime);
    }
 
    last_state_sys = new_state_sys;
    last_state_usr = new_state_usr;
  }
}

#else
/* UNIX version of Stats */
static void Stats(gcflags, format, a1, a2, a3) /* VARARGS2 */
word  gcflags;
char *format;
int   a1;
int   a2;
int   a3;
{
  word sflags = A.stats_flags;
  word show   = ((sflags & STATS_FULL_GCS) && (gcflags & GC_FULL))
              || (sflags & STATS_PARTIAL_GCS); 
  
  if (sflags)
  {
    struct tms new_state;
    times(&new_state);
    
    if (show)
    {
       char   buffer[200];
       /* Start of a Stats group - flush out anything preceding it.
	  (This only really makes a difference if A.stats_file is
	   connected to stdout/stderr.) SPF 17/3/97
       */
       if (gcflags & GC_START)
       {
	  proper_fflush(A.stats_file);
       }
       
       if (gcflags & GC_NEWLINE)
       {
	  proper_fprintf(A.stats_file,"\n");
       }
	 
       sprintf (buffer,format,a1,a2,a3);
       proper_fprintf(A.stats_file,"%-50s",buffer);
   
       if (sflags & STATS_TIME)
       {
	 PrintDelta (& new_state.tms_utime,& last_state.tms_utime,'u');
	 PrintDelta (& new_state.tms_stime,& last_state.tms_stime,'s');
       }
       
       proper_fprintf(A.stats_file,"\n");
       proper_fflush(A.stats_file);
    }
    
    if (! (gcflags & GC_START)) /* Add time for the just-completed GC phase to running total */
    {
       AddDelta (& last_state.tms_stime,& new_state.tms_stime,& A.gc_stime);
       AddDelta (& last_state.tms_utime,& new_state.tms_utime,& A.gc_utime);
    }
 
    last_state = new_state;
  }
}
/* end of UNIX version of Stats */
#endif

/* The problem with this version of PossiblyExpandArea is that it doesn't always expand
   it enough for the subsequent compaction phase to actually liberate words_needed
   of free space. SPF 31/7/96
*/
static int PossiblyExpandArea(Area *area, word *(*allocChunks)(const int), const int words_needed)
{
  /* Invariant: at most the first (gen_top - bottom) bits of the bitmap are dirty here. */
  /* old is the current size of the area.                     */
  /* wanted is the size we would like to expand the area to.  */
  /* requested_size is the size we can actually try to get.   */
  int old     = WORDPTRSUB(area->top, area->bottom);
  int wanted  = area->bsize + words_needed;
  int maximum = area->nbits;

  /* clip to maximum allowed area size */
  int requested_size   = (maximum <= wanted) ? maximum : wanted;
  int requested_growth = requested_size - old;

  if (0 < requested_growth) /* need to get some more pages */
  {
    int  chunks  = ROUNDUP_UNITS(requested_growth, CHUNKWORDS);
    int  words   = chunks * CHUNKWORDS;
    word *new_bottom = (* allocChunks)(chunks);
    
    assert(0 < chunks);
    assert(0 < words);

    if (new_bottom == 0)
    {
       /* Invariant: at most the first (gen_top - bottom) bits of the bitmap are dirty here. */
       return 0; /* cannot get that much */
    }
    assert((int)WORDPTRSUB(area->bottom, new_bottom) == words);
    
    {
     /* Shift bits in bitmap so they still correspond to their original
        addresses. (The problem is that Areas grow DOWN, but bitmaps
        are referenced from the BOTTOM, so expanding the area changes the
        addressing scheme. One day I'll fix this.) Note that we may NOT
        have allocated exactly requested_growth words, because we rounded
        up to a whole number of chunks, so we have to be very careful here.
        SPF 31/7/96
        
        Note that this preserves the invariant that at most the first 
        (area->gen_top - area->bottom) bits of area->bitmap can be dirty.
        Note that we only have to shift (gen_top - bottom), not
        (top - bottom) bits, as we used to, since our invariant tells
        us that the extra (top - gc_bottom) bits must all be zero.
        
        We have to think carefully here because we can't assume that
        area->gen_top (unlike area->top and area->bottom, which are
        page-aligned) is aligned on a 32 word boundary so we may have
        to copy a partial bitmap word. Actually, this isn't a problem
        because BITMAP_WORDS and BITMAP_BYTES round up to the next word
        boundary and it doesn't matter if we copy slightly too much, since
        that would only overwrite zero bits with other zero bits.
        SPF 3/10/96
     */
      int max_dirty_bits = WORDPTRSUB(area->gen_top, area->bottom);
      
      /* Check that the pre-allocated bitmap is large enough. */
      int new = WORDPTRSUB(area->top, new_bottom);
      assert(requested_size <= new);
      assert(new <= area->nbits);
      assert(old <= area->nbits);

      /* Check that we have changed the allocation by a multiple of
         BITS_PER_WORD words, as otherwise we won't get the correct
         word-alignment when we the shift the bitmap.
         SPF 3/10/96
      */
      assert(words % BITS_PER_WORD == 0);

      area->bottom = new_bottom;
      bcopy(Bytes(area->bitmap), Bytes(area->bitmap)+BITMAP_BYTES(words), BITMAP_BYTES(max_dirty_bits));
      wzero(area->bitmap, BITMAP_WORDS(words));
    }
    
    /* Invariant: at most the first (gen_top - bottom) bits of the bitmap are dirty here. */
    return 1;
  }
  
  /* Invariant: at most the first (gen_top - bottom) bits of the bitmap are dirty here. */
  return 0;
}

/* This function CHECKS whether we have enough space AFTER the compaction phase. */
static int BufferIsReallyFull(const Area *area, const int words_needed, const int full_gc)
{
  const int currently_free = WORDPTRSUB(area->pointer, area->bottom);
  const int required_buffer_free =
    full_gc ?
    area->bsize : 
    (int)((double)(100 - area->percent) * (double)area->bsize / 100.0);
    
  const int required_free  = required_buffer_free + words_needed;
  
  return (currently_free < required_free);
}

/* AFTER a full GC, make sure we have a full buffer's worth of free space available. */
static int AdjustHeapSize(
  Area     *area,
  word   *(*allocChunks)(const int),
  word   *(*freeChunks)(const int),
  const int words_needed
  )
{
  int size_changed = 0;

  const int maximum_size     = area->nbits;
  const int current_size     = WORDPTRSUB(area->top, area->bottom);
  const int currently_free   = WORDPTRSUB(area->pointer, area->bottom);
  const int required_buffer_free = area->bsize;

  const int required_free    = required_buffer_free + words_needed;
  const int required_growth  = required_free - currently_free;
  const int required_size    = current_size + required_growth;
  const int requested_size   = (required_size < maximum_size) ? required_size : maximum_size;
  const int requested_growth = requested_size - current_size;
  
  assert(area->bottom  <= area->pointer);
  assert(area->pointer <= area->gen_top);
  assert(area->gen_top <= area->top);
  
  /* Basic sanity checks. */
  assert(0 <= words_needed);
  assert(0 <  area->bsize);
  assert(0 <= current_size && current_size <= maximum_size);
  assert(0 <= currently_free && currently_free <= current_size);
  assert(0 <= required_free);
  assert(0 <= required_size);
  assert(0 <= requested_size && requested_size <= maximum_size);
  
  /* We require top and bottom to be chunk-aligned; we also require
     that the number of bits in the bitmap is adequate for a
     whole number of chunks (since we limit the heap growth to the
     the number of bits in the bitmap, but then round UP to the
     next page boundary). SPF 1/8/96
  */
  assert(IsChunkAligned(area->top));
  assert(IsChunkAligned(area->bottom));
  assert((int)ROUNDUP(area->nbits, CHUNKWORDS) == area->nbits);

  if (0 < requested_growth) /* expand the heap */
  {
     const int growby_chunks = ROUNDUP_UNITS(requested_growth, CHUNKWORDS);
     const word *old_bottom = area->bottom;
           word *new_bottom = (* allocChunks)(growby_chunks);
     
     assert(0 < growby_chunks);
     
     if (new_bottom != 0) /* We managed to grow */
     {
        assert(IsChunkAligned(old_bottom));
        assert(IsChunkAligned(new_bottom));
        assert(new_bottom < old_bottom);
        assert(WORDPTRSUB(old_bottom, new_bottom) == growby_chunks * CHUNKWORDS);
    
        /* This should never fail if the above checks succeed. */
        assert((int)WORDPTRSUB(area->top, new_bottom) <= area->nbits);
        area->bottom = new_bottom;
        size_changed = 1;
     }
  }
  else /* requested_growth <= 0 */
  {
     /* We only shrink if we've still got a FULL buffer's worth free. */
     const int requested_free   = area->bsize + words_needed;
     const int requested_shrink = currently_free - requested_free;

     /* requested_shrink could be negative here! */
     if (0 < requested_shrink)
     {
	const int shrinkby_chunks = ROUNDDOWN_UNITS(requested_shrink, CHUNKWORDS);
	
	assert(0 <= shrinkby_chunks);
   
	if (0 < shrinkby_chunks) 
	{
	  word *old_bottom = area->bottom;
          word *new_bottom = (* freeChunks)(shrinkby_chunks);
          
	  /* The free function requires chunk alignment */
	  assert(IsChunkAligned(old_bottom));
	  assert(IsChunkAligned(new_bottom));
	  
	  /* Check that we're actually freeing something */
	  assert(old_bottom < new_bottom);

	  /* Check that we're not freeing anything that we still need. */
	  assert(new_bottom <= area->pointer);
	  
	  /* Check that we freed the space we thought we freed. */
	  assert(WORDPTRSUB(new_bottom, old_bottom) == shrinkby_chunks * CHUNKWORDS);

	  area->bottom = new_bottom;
	  size_changed = 1;
	}
     }
   } /* grow or shrink the heap */
  return size_changed;
}

#define GC_PROCS 10

static GCMapFunc gc_proc[GC_PROCS] = { 0 };

void RegisterGCProc(GCMapFunc proc)
{
  word i;
  
  for (i = 0; i < GC_PROCS && gc_proc[i]; i++);
  
  if (i == GC_PROCS) Crash ("Too many gc registrations");
  
  gc_proc[i] = proc;
}

void OpGCProcs (GCOpFunc op)
{
  word i;
  
  for (i = 0; i < GC_PROCS && gc_proc[i]; i++) (* gc_proc[i]) (op);
}

#define KB(words) (((words)*sizeof(word)+1023)/1024)

static int UsageRatio(Area *area)
{
  int total = WORDPTRSUB(area->gen_top, area->pointer);
  
  if (total == 0) return 100;
  
  return (int)(100.0 * (double)area->updated / (double)total);
}

static int RecollectThisGeneration(int this_generation)
{
  return (this_generation <= 3 && UsageRatio(& A.M) <= 50);
}

void SetImmutables (int writeable)
{
  char *bottom = Bytes(A.I.bottom);
  char *top    = Bytes(A.I.top);
  long  length = top - bottom;

  if (writeable)
    SetProtectionWriteCopy(bottom,length,"SetImmutables,gc");
  else
    SetProtectionReadOnly(bottom,length,"SetImmutables,gc");
}

/* Called when Poly/ML starts up, and following a commit. */
void CreateHeap(void)
{
  static int bitmaps_allocated = 0;

  if (!bitmaps_allocated)
  {   
    /* Allocate GC bitmaps for local heap. The number of bits in 
       each bitmap must be a multiple of the number of words in
       a page - see ExpandArea for an explanation of this.
       SPF 1/8/96
    */
    const int requested_i_bitmapsize =
       (A.heap_size < I_ABSOLUTE_MAXIMUM) ?
       A.heap_size :
       I_ABSOLUTE_MAXIMUM;
       
    const int requested_m_bitmapsize =
       (A.heap_size < M_ABSOLUTE_MAXIMUM) ?
       A.heap_size :
       M_ABSOLUTE_MAXIMUM;
     
    A.I.nbits = ROUNDDOWN(requested_i_bitmapsize, CHUNKWORDS);
    A.I.bitmap = Words(malloc(BITMAP_BYTES(A.I.nbits)));
    
    if (A.I.bitmap == Words(NULL))
    {
       Crash ("Unable to allocate bitmap (%i bytes) for immutable heap\n",
              BITMAP_BYTES(A.I.nbits));
    }
    
    assert(A.I.bitmap != Words(NULL));
    assert(ALIGNED(A.I.bitmap));
  
    A.M.nbits = ROUNDDOWN(requested_m_bitmapsize, CHUNKWORDS);
    A.M.bitmap = Words(malloc(BITMAP_BYTES(A.M.nbits)));

    if (A.M.bitmap == Words(NULL))
    {
       Crash ("Unable to allocate bitmap (%i bytes) for mutable heap\n",
              BITMAP_BYTES(A.M.nbits));
    }

    assert(A.M.bitmap != Words(NULL));
    assert(ALIGNED(A.M.bitmap));
    
    bitmaps_allocated = 1;
  }

  /* malloc doesn't guarantee zero-initialisation, so we have to do it by hand */
  wzero(A.I.bitmap,BITMAP_WORDS(A.I.nbits));
  wzero(A.M.bitmap,BITMAP_WORDS(A.M.nbits));
  /* Invariant: the bitmaps are completely clean */

  /* Allocate the heap itself */
  A.I.top     = LOCAL_ITOP;  
  A.I.gen_top = A.I.top;
  A.I.pointer = A.I.top;
  A.I.bottom  = AllocateLocalImmutableChunks(ROUNDDOWN_UNITS(A.I.bsize, CHUNKWORDS));
  
  if (A.I.bottom == 0)
  {
     Exit ("Immutable buffer too large");
  }

  A.M.top     = LOCAL_MTOP;
  A.M.gen_top = A.M.top;
  A.M.pointer = A.M.top;
  A.M.bottom  = AllocateLocalMutableChunks(ROUNDDOWN_UNITS(A.M.bsize, CHUNKWORDS));

  if (A.M.bottom == 0)
  {
     Exit ("Mutable buffer too large");
  }
}

/* Called immediately before we recreate the heap following a commit */
void DestroyHeap(void)
{
   /* Discard the heap itself */
   word *new_mtop = FreeLocalMutableChunks   (ROUNDDOWN_UNITS(WORDPTRSUB(A.M.top, A.M.bottom), CHUNKWORDS));
   word *new_itop = FreeLocalImmutableChunks (ROUNDDOWN_UNITS(WORDPTRSUB(A.I.top, A.I.bottom), CHUNKWORDS));

   /* Check we've really destroyed everything */
   assert(new_mtop == LOCAL_MTOP && new_itop == LOCAL_ITOP);
   /* Retain the bitmaps for future use */
}

static void doGC(int do_full_gc, const int words_needed)
{
  /* Invariant: the bitmaps are completely clean. */
  /* Note: this version of doGC does NOT clean the store 
     - that's now the user's resposibility SPF 22/10/96
  */
  word i;
  word gcflags = GC_START;
  static int do_full_gc_next = 0;
  static int this_generation = 0;
  int immutable_overflow;

  record_gc_time(0);

  assert (! A.garbage_collecting);
  
  if (A.debug & DEBUG_IMMRO) SetImmutables (1);

  GC_AGAIN:
  /* Invariant: the bitmaps are completely clean. */
  
  /* At this point, we should have
       A.I.bottom <= A.I.pointer <= A.I.gen_top <= A.I.top       
       A.M.bottom <= A.M.pointer <= A.M.gen_top <= A.M.top       

     A.X.gen_top divides the current generation from the old one.
     A.X.pointer is the current allocation pointer.
  */
  
  assert (A.I.top     >= A.I.gen_top);
  assert (A.I.gen_top >= A.I.pointer);
  assert (A.I.pointer >= A.I.bottom);

  assert (A.M.top     >= A.M.gen_top);
  assert (A.M.gen_top >= A.M.pointer);
  assert (A.M.pointer >= A.M.bottom);

  /* Record low-water mark before we change anything */
  A.I.gen_bottom = A.I.pointer;
  A.M.gen_bottom = A.M.pointer;
  
  /* Our recovery actions may insist on a full GC */
  if (do_full_gc_next)
  {
     do_full_gc = 1;
     do_full_gc_next = 0;
  }
  
  /* Mark phase */
  A.garbage_collecting = 1; /* SPF 7/6/96 */

  gcflags |= GC_NEWLINE;
  
  if (do_full_gc)
  {
    gcflags |= GC_FULL;
    A.full_gcs++;
    Stats(gcflags,"GC: Full GC    %3d",A.full_gcs);

    /* Collect everything */
    A.I.gen_top = A.I.top;
    A.M.gen_top = A.M.top;
  }
  else
  {
     A.partial_gcs++;
     Stats(gcflags,"GC: Partial GC %3d",A.partial_gcs);
  }

  gcflags &= ~GC_START;
  gcflags &= ~GC_NEWLINE;
  
  Stats(gcflags,"GC: (%6dK,%6dK) allocated", KB(WORDPTRSUB(A.I.top,     A.I.bottom)),  KB(WORDPTRSUB(A.M.top,     A.M.bottom)));
  Stats(gcflags,"GC: (%6dK,%6dK) old",       KB(WORDPTRSUB(A.I.top,     A.I.gen_top)), KB(WORDPTRSUB(A.M.top,     A.M.gen_top)));
  Stats(gcflags,"GC: (%6dK,%6dK) new",       KB(WORDPTRSUB(A.I.gen_top, A.I.pointer)), KB(WORDPTRSUB(A.M.gen_top, A.M.pointer)));
  Stats(gcflags,"GC: (%6dK,%6dK) free",      KB(WORDPTRSUB(A.I.pointer, A.I.bottom)),  KB(WORDPTRSUB(A.M.pointer, A.M.bottom)));

  /* Bitmaps are allocated in InitialiseGC and are zeroed
     at the END of each GC, because that way we now how much
     of each bitmap (not all!) we need to touch.
     SPF 3/10/96
  */
  assert(A.I.bitmap != Words(NULL));
  assert(A.M.bitmap != Words(NULL));
  
  A.I.i_marked  = 0;
  A.I.m_marked  = 0;
  A.M.i_marked  = 0;
  A.M.m_marked  = 0;
  
  /* Do the actual marking */
  OpOldMutables(H, MarkOldMutables);
  if (! do_full_gc) OpNewMutables(MarkPointers);
  OpGCProcs(MarkRuntimeObject);
  /* Invariant: at most the first (gen_top - bottom) bits of the each bitmap can be dirty here. */

  Stats(gcflags,"GC: (%6dK,%6dK) marked in mutable buffer",   KB(A.M.i_marked), KB(A.M.m_marked));
  
  assert(A.I.m_marked == 0);
  if (A.I.i_marked != 0)
  {
     Stats(gcflags,"GC: (%6dK,%6dK) marked in immutable buffer", KB(A.I.i_marked), KB(A.I.m_marked));
  }
  
  /* Compact phase */
  A.garbage_collecting = 2; /* SPF 7/6/96 */

  /* Detect unreferenced streams, windows etc. */
  OpGCProcs(ReferenceRuntimeObject);
  
  /* If we are doing a full GC we expand the immutable area now, so that there's
     enough room to copy the immutables that are currently in the mutable buffer.
     There's no point expanding the mutable buffer now - we'll do that later 
     when we know *eaxactly* how large we want it to be.
  */ 
  if (do_full_gc) 
  {
     /* Invariant: at most the first (gen_top - bottom) bits of each bitmap can be dirty here. */
     int i_data = A.I.i_marked + A.M.i_marked;
     int i_expanded = PossiblyExpandArea (& A.I,AllocateLocalImmutableChunks,i_data);
     /* Invariant: at most the first (gen_top - bottom) bits of each bitmap can be dirty here. */
 
     if (i_expanded)
     {
        Stats(gcflags,"GC: (%6dK,%6dK) allocated now", KB(WORDPTRSUB(A.I.top, A.I.bottom)), KB(WORDPTRSUB(A.M.top, A.M.bottom)));
     }
  }
  
  /* Invariant: at most the first (gen_top - bottom) bits of each bitmap can be dirty here. */
  A.I.highest = BITNO(&A.I,A.I.gen_top);
  A.M.highest = BITNO(&A.M,A.M.gen_top);
  
  for (i = 0; i < NSTARTS; i++)
  {
      A.I.start[i] = A.I.highest;
  }
  A.I.start_index = NSTARTS - 1;
  /* Invariant: A.I.start[0] .. A.I.start[A.I.start_index] is a descending sequence. */ 
  
  for (i = 0; i < NSTARTS; i++)
  {
     A.M.start[i] = A.M.highest;
  }
  A.M.start_index = NSTARTS - 1;
  /* Invariant: A.M.start[0] .. A.M.start[A.M.start_index] is a descending sequence. */ 

  A.I.copied = 0;
  A.M.copied = 0;

  /* Invariant: there are no objects below A.X.gen_bottom. */
  
  /* Reclaim the genuine data from the mutable buffer. */
  {
     int immutable_space     = WORDPTRSUB(A.I.gen_top, A.I.gen_bottom);
     int immutable_used      = A.I.i_marked;
     int immutable_free      = immutable_space - immutable_used;
     int immutable_needed    = A.M.i_marked;
     int compress_immutables = immutable_needed / 2 < immutable_free ; /* Needs tuning!!! */
     
     /* Reset the allocation pointers. This puts garbage (and real data) below them. */
     A.M.pointer = A.M.gen_top;
     A.I.pointer = compress_immutables ? A.I.gen_top : A.I.pointer;
     /* Invariant: there are no objects below A.M.gen_bottom. */
     CopyObjectsInArea(& A.M, compress_immutables);
     Stats(gcflags,"GC: (%6dK,%6dK) copied from mutable buffer",KB(A.I.copied),KB(A.M.copied));
     
     assert(A.M.copied + A.I.copied <= A.M.m_marked + A.M.i_marked);
     assert(A.I.copied <= A.M.i_marked);
     assert(A.I.copied != A.M.i_marked || A.M.copied <= A.M.m_marked);
     /* We may have A.M.copied > A.M.m_marked, if the immutable buffer overflows */
     
     immutable_overflow = A.M.i_marked - A.I.copied;
  }


  /* If we've copied an object from the mutable area below the previous
     limit of the immutable area using a "non-compressing" copy,
     it would be unsafe to attempt to compress the immutable area (we
     might get a double indirection).
     
     However, it *is* safe if we've used a "compressing" copy from
     the mutables buffer. We won't move anything twice, because each
     object goes into the first "big enough" hole on each pass. If
     the second pass finds a "big enough" hole above the object, the
     first pass would have found this hole too, and used it.
     
     This is slightly tricky reasoning, so be careful!
     
     SPF 19/12/1997
  */


  /* The area between A.M.gen_bottom and A.M.pointer may contain
     tombstones, so we daren't increase A.M.gen_bottom.
  */
  assert(A.M.gen_bottom <= A.M.pointer);

  /* Reclaim the genuine data from the immutable buffer. */
  if (A.I.gen_bottom <= A.I.pointer)
  {
     int immutable_space     = WORDPTRSUB(A.I.gen_top, A.I.gen_bottom);
     int immutable_used      = A.I.i_marked + A.I.copied;
     int immutable_free      = immutable_space - immutable_used;
     int immutable_needed    = A.I.i_marked;
     int compress_immutables = immutable_needed / 4 < immutable_free ; /* Needs tuning!!! */

     if (compress_immutables)
     {
	A.I.copied = 0;
	A.M.copied = 0;
	/* Invariant: there are no objects below A.I.gen_bottom. */
	CopyObjectsInArea(& A.I, 1);
	Stats(gcflags,"GC: (%6dK,%6dK) copied from immutable buffer", KB(A.I.copied),KB(A.M.copied));
	
	assert(A.M.copied == 0);
	assert(A.I.copied <= A.I.i_marked);
     }
     else
     {
        /* simply reclaim the immutable data (with its embedded garbage) */
        A.I.pointer = A.I.gen_bottom;
     }

     assert(A.I.gen_bottom <= A.I.pointer);
     /* The area between A.I.gen_bottom and A.I.pointer may contain
        tombstones, so we daren't increase A.M.gen_bottom.
     */
  }
  else
  {
     /* We have extended the GC area, so we must adjust A.I.gen_bottom */ 
     assert(A.I.pointer < A.I.gen_bottom);
     A.I.gen_bottom = A.I.pointer;
  }
  
  assert (INSOFTRANGE(A.I.pointer,A.I.bottom,A.I.gen_top));
  assert (INSOFTRANGE(A.M.pointer,A.M.bottom,A.M.gen_top));
  
  
  /* Update phase */
  A.garbage_collecting = 3; /* SPF 7/6/96 */

  /* Invariant: at most the first (gen_top - bottom) bits of each bitmap can be dirty here. */
  A.I.updated = 0;
  A.M.updated = 0;
  OpOldMutables(H, UpdateOldMutables);
  if (! do_full_gc) OpNewMutables(UpdatePointer);
  UpdateObjectsInArea(& A.I);
  UpdateObjectsInArea(& A.M);
  OpGCProcs(UpdateRuntimeObject);
  Stats(gcflags, "GC: (%6dK,%6dK) updated"    , KB(A.I.updated)  , KB(A.M.updated));
  Stats(gcflags, "GC: (%6d%%,%6d%%) compacted", UsageRatio(& A.I), UsageRatio(& A.M));

  assert(A.I.updated == A.I.i_marked + A.M.i_marked - immutable_overflow);
  assert(A.M.updated == A.I.m_marked + A.M.m_marked + immutable_overflow);

  /* Invariant: at most the first (gen_top - bottom) bits of the each bitmap can be dirty. */
  wzero(A.I.bitmap, BITMAP_WORDS(WORDPTRSUB(A.I.gen_top, A.I.bottom)));
  wzero(A.M.bitmap, BITMAP_WORDS(WORDPTRSUB(A.M.gen_top, A.M.bottom)));
  /* Invariant: the bitmaps are completely clean */

  if (A.debug & DEBUG_IMMRO) SetImmutables (0);
  
  if (do_full_gc)
  {
     /* If we've had an immutable overflow, allow for this when we grow the heap */
     int i_changed = AdjustHeapSize(& A.I,AllocateLocalImmutableChunks, FreeLocalImmutableChunks, immutable_overflow);
     int i_full    = BufferIsReallyFull(& A.I,immutable_overflow,do_full_gc);
     int m_full    = BufferIsReallyFull(& A.M,words_needed,do_full_gc);
     
     /* If we're going to recollect the current generation, don't adjust the mutable buffer size yet. */
     /* We'll (probably) do that on the next collection. SPF 22/12/1997 */
     int m_changed = 
       (!i_full && m_full && RecollectThisGeneration(this_generation)) ? 0 :
       AdjustHeapSize(& A.M,AllocateLocalMutableChunks,  FreeLocalMutableChunks, words_needed);

     if (i_changed || m_changed)
     {
        Stats(gcflags,"GC: (%6dK,%6dK) allocated now", KB(WORDPTRSUB(A.I.top, A.I.bottom)), KB(WORDPTRSUB(A.M.top, A.M.bottom)));
     }
  }

  /* Invariant: the bitmaps are completely clean. */
  
  
  /* Have we cleared enough space? */
  {
     int i_full = BufferIsReallyFull(& A.I,immutable_overflow,do_full_gc);
     int m_full = BufferIsReallyFull(& A.M,words_needed,do_full_gc);
 
     if (i_full) Stats(gcflags,"GC: Immutable buffer is full");
     if (m_full) Stats(gcflags,"GC: Mutable buffer is full");
     
     if (i_full || m_full)
     {
        int recovered = 0;

	/* Recovery actions */
	if (!i_full && RecollectThisGeneration(this_generation)) /* Needs tuning!!! */
	{
	   /* The next GC will re-collect THIS generation, which should be
	      enough to recover properly.
	   */
           Stats(gcflags,"GC: Recovery action - recollect this generation");
           recovered = 1;
	}
	else if (!do_full_gc)
	{
           Stats(gcflags,"GC: Recovery action - full GC next");
	   do_full_gc_next = 1;
           recovered = 1;
	}
	else /* do_full_gc */
	{
	   const int max_i_heap = ROUNDDOWN(I_ABSOLUTE_MAXIMUM, CHUNKWORDS);
	   const int max_m_heap = ROUNDDOWN(M_ABSOLUTE_MAXIMUM, CHUNKWORDS);
  
	   /* Emergency immutable heap expansion, then retry full GC */ 
	   if (i_full && A.I.nbits < max_i_heap && BufferIsReallyFull(& A.I,words_needed,immutable_overflow))
	   {
	      word *new_bitmap = (word *)malloc(BITMAP_BYTES(max_i_heap));
	      if (new_bitmap)
	      {
		 free(A.I.bitmap);
		 A.I.bitmap = new_bitmap;
		 A.I.nbits  = max_i_heap;
		 wzero(A.I.bitmap,BITMAP_WORDS(A.I.nbits));
		 /* Invariant: the bitmaps are completely clean */
		 Stats(gcflags,"GC: Recovery action - emergency immutable growth");
		 do_full_gc_next = 1;
                 recovered = 1;
	      }
	   }
	 
	   /* Emergency mutable heap expansion, then retry full GC */ 
	   if (m_full && A.M.nbits < max_m_heap)
	   {
	      word *new_bitmap = (word *)malloc(BITMAP_BYTES(max_m_heap));
	      if (new_bitmap)
	      {
		 free(A.M.bitmap);
		 A.M.bitmap = new_bitmap;
		 A.M.nbits  = max_m_heap;
		 wzero(A.M.bitmap,BITMAP_WORDS(A.M.nbits));
		 /* Invariant: the bitmaps are completely clean */
		 Stats(gcflags,"GC: Recovery action - emergency mutable growth");
		 do_full_gc_next = 1;
                 recovered = 1;
	      }
	   }
 
	  /* If we can't recover, have we at least got the amount we need after a PARTIAL collection? */
	  if (!recovered)
	  {
	     if (BufferIsReallyFull(& A.I,0,0) || BufferIsReallyFull(& A.M,words_needed,0))
	     {
		/* If all else fails, interrupt console processes */
		Stats(gcflags,"GC: Recovery action - interrupt console processes");
		proper_fprintf(stderr,"Run out of store - interrupting console processes\n");
		interrupt_console_processes();
		/* End GC here. */
		A.garbage_collecting = 0;
		record_gc_time(0);
		RE_ENTER_POLY(555);

	     }
	     else
	     {
		Stats(gcflags,"GC: Recovery action - limp on");
	     }
	  }
	} /* do_full_gc */
     } /* i_full || m_full */
  }
  
  
  if (RecollectThisGeneration(this_generation))
  {
     /* If this was a full GC, make sure the next one is too, as we may
        need to reconfigure the mutable buffer size. If we only did a
        partial next, we would still have to mark all the immutables again
        (they would still be new) which is the main cost of a full GC.
     */
     do_full_gc_next |= do_full_gc;
     this_generation++;
  }
  else
  {
     /* Merge this generation with the old one */
     A.I.gen_top = A.I.pointer;
     A.M.gen_top = A.M.pointer;
     this_generation = 0;
  }
  
  Stats(gcflags,"GC: (%6dK,%6dK) old now",  KB(WORDPTRSUB(A.I.top,     A.I.gen_top)), KB(WORDPTRSUB(A.M.top,     A.M.gen_top)));
  Stats(gcflags,"GC: (%6dK,%6dK) new now",  KB(WORDPTRSUB(A.I.gen_top, A.I.pointer)), KB(WORDPTRSUB(A.M.gen_top, A.M.pointer)));
  Stats(gcflags,"GC: (%6dK,%6dK) free now", KB(WORDPTRSUB(A.I.pointer, A.I.bottom)),  KB(WORDPTRSUB(A.M.pointer, A.M.bottom)));

  /* Do we have enough space for the original allocation request? */
  if ((int)WORDPTRSUB(A.M.pointer, A.M.bottom) < words_needed)
  {
     /* Try our recovery action immediately */
     goto GC_AGAIN;
  }

#if defined(_SC_PAGESIZE) && defined(_SC_PHYS_PAGES) /* Only SOLARIS2, I think */
  /* If the heap is very close to what we can handle on this machine,
      do the full GC immediately, because if we wait, we'll generate
      more data in the mutable buffer which will make the thrashing caused
      by the inevitable full GC even worse.
      SPF 2/3/1998
  */
  if (do_full_gc_next)
  {
     long phys_pages      = sysconf(_SC_PHYS_PAGES);
     long phys_pagesize   = sysconf(_SC_PAGESIZE);
     int  phys_page_words = ROUNDDOWN_UNITS(phys_pagesize, sizeof(word));
     
     int immutable_words        = WORDPTRSUB(A.I.top, A.I.bottom);
     int mutable_words          = WORDPTRSUB(A.M.top, A.M.bottom);
     int immutable_bitmap_words = BITMAP_WORDS(A.I.nbits);
     int mutable_bitmap_words   = BITMAP_WORDS(A.M.nbits);
     
     int immutable_phys_pages        = ROUNDUP_UNITS(immutable_words, phys_page_words);
     int mutable_phys_pages          = ROUNDUP_UNITS(mutable_words, phys_page_words);
     int immutable_bitmap_phys_pages = ROUNDUP_UNITS(immutable_bitmap_words, phys_page_words);
     int mutable_bitmap_phys_pages   = ROUNDUP_UNITS(mutable_bitmap_words, phys_page_words);
     
     /* This crude estimate leaves out ML database, C heap, space for executable etc. */
     int heap_phys_pages = 
        immutable_phys_pages + mutable_phys_pages + immutable_bitmap_phys_pages + mutable_bitmap_phys_pages;
     
     /* heap_phys_pages * 100 is representable by an int, so this calculation shouldn't overflow. */
     int heap_load = (100 * heap_phys_pages) / phys_pages;
     
     /* I guess that a full GC will start to cause thrashing when the heap
        occupies about 90% of the physical memory. Is this a good guess?
        I don't really know - but it's a reasonable starting point.
        SPF 2/3/1998
        OK - now let's try 80%.
        SPF 2/3/1998
     */
     if (80 <= heap_load)
     {
	Stats(gcflags,"GC: Heap consumes %i%% of physical memory", heap_load);
	Stats(gcflags,"GC: Recovery action - full GC immediately");
        goto GC_AGAIN;
     }
  }
#endif /* SOLARIS2 tweak */


  /* End of garbage collection */
  A.garbage_collecting = 0; 
  record_gc_time(1);
  
  /* Invariant: the bitmaps are completely clean */
}

void FullGC(void)
{
  doGC (1,0);
}

void QuickGC(word words_needed)
{
  word words_free;
  
  assert (! MD_is_exception(poly_stack));
  
  doGC (0,words_needed);
  
  if (MD_is_exception(poly_stack))
  {
    /* This process has been interrupted. The memory request may
       not have been satisfied, and we must go directly to the
       code which raises Interrupt before any registers are changed. */

    LONGJMP(re_enter_poly,999); /* Return to poly process immediately. */
  }
  
  words_free = WORDPTRSUB(A.M.pointer, A.M.bottom);
  
  if (words_free < words_needed) Crash ("Run out of store");
}
