/*
    Title:      Copying garbage collector

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
#include <string.h>

#if defined(FREEBSD) || defined(MACOSX)
#include <stdlib.h>
#elif defined(WINDOWS_PC)
#include <malloc.h>
#else
#include <alloca.h>
#endif


/* #include <memory.h> */
#include <assert.h>
#include "globals.h"
#include "mmap.h"
#include "objects.h"
#include "diagnostics.h"

/* added 18/12/95 SPF for MD_flush_instruction_cache */
#include "machine_assembly.h"

#include "machine_dep.h"

#include "proper_io.h"

#ifdef DEBUG
#define ASSERT(x) assert(x)
#else
#define ASSERT(x)
#endif

#ifdef PORTING
/* SPF 6/7/95 */
static sparc_code_count;
static int_code_count;
static other_code_count;    
#endif

/******************************************************************************/
/*                                                                            */
/*      Variables                                                             */
/*                                                                            */
/******************************************************************************/
static GCConst  old_consts      = { 0 };
static GCConst  new_consts      = { 0 };
static GCSpace *db              = 0;
static GCSpace  from            = { 0 };
static GCSpace  to              = { 0 };
static Header   context         = 0;
static word     total_copied    = 0;
static word    *i_bitmap;
static word    *m_bitmap;

static AllocFunc newMutable  = 0;
static AllocFunc newImmutable = 0;

static word *CopyObject(word *);

#define IS_OLD_IMM_POINTER(p) \
 ((unsigned)from.i_bottom <= (unsigned)(p) && \
  (unsigned)(p) < (unsigned)from.i_top)

#define IS_OLD_MUT_POINTER(p) \
 ((unsigned)from.m_bottom <= (unsigned)(p) && \
  (unsigned)(p) < (unsigned)from.m_top)

#define IS_OLD_POINTER(p) (IS_OLD_IMM_POINTER(p) || IS_OLD_MUT_POINTER(p))

#define IS_NEW_IMM_POINTER(p) \
 ((unsigned)to.i_bottom <= (unsigned)(p) && \
  (unsigned)(p) < (unsigned)to.i_top)

#define IS_NEW_MUT_POINTER(p) \
 ((unsigned)to.m_bottom <= (unsigned)(p) && \
  (unsigned)(p) < (unsigned)to.m_top)

#define IS_NEW_POINTER(p) (IS_NEW_IMM_POINTER(p) || IS_NEW_MUT_POINTER(p))

#define NewMutable(n)   (*newMutable)(context,n)
#define NewImmutable(n) (*newImmutable)(context,n)

/******************************************************************************/
/*                                                                            */
/*      CopyCodePointer                                                       */
/* Traces the value in the program counter or return address. Note that this  */
/* value doesn't necessarily have the proper +2 mod 4 alignment.              */
/*                                                                            */
/******************************************************************************/
static byte *CopyCodePointer(byte *oldpc)
{
  word *old; 
  word *new;
        
  if (IS_OLD_POINTER(oldpc))
  {
    /* Find the code segment it is associated with. */
    OBJ_CODEPTR_TO_PTR(oldpc,old);
      
    /* copy it */
    new = CopyObject (old);
    
    /* move original code pointer */
    return (oldpc + ((new - old) * sizeof(word)));
  }
  /* pointer into I/O area? */
  else if ((byte *)old_consts.io_bottom <= oldpc && 
           oldpc < (byte *)old_consts.io_top)
  {
     int io_number = 
       (oldpc - (byte *)old_consts.io_bottom)
     / (sizeof(word) * old_consts.io_spacing);
     
     word *old_io_address = 
        &old_consts.io_bottom[io_number * old_consts.io_spacing];
  
     /* Have we successfully decoded the I/O call? */
     if ((byte *)old_io_address + 2 != oldpc)  /* handler or return address */
     {
        Crash ("CopyCodePointer: bad I/O value; oldpc=%p, old_io_address=%p, io_number=0x%02x",
               oldpc, old_io_address, io_number);
     }
     
     {
        word *new_io_address = 
          &new_consts.io_bottom[io_number * new_consts.io_spacing];

        byte *newpc = (byte *)new_io_address + 2;

        if (newpc != oldpc)
        {
           proper_printf("mapping I/O return address %p to %p (RTS call 0x%02x)\n",
                   oldpc, newpc, io_number);
        }
        
        return newpc;
     }
  }
  else
  {
    return oldpc;
  }
}

/******************************************************************************/
/*                                                                            */
/*      CopyConstant                                                          */
/* Used to copy objects which are in the constants section of a code object.  */
/*                                                                            */
/******************************************************************************/
static byte *CopyConstant(byte *old)
{
   /* A short? */
   if (OBJ_IS_AN_INTEGER(old))
   {
      /* do nothing */
      return (old);
   }

   /* A code-pointer? */
   else if (OBJ_IS_CODEPTR(old))
   {
     return (CopyCodePointer(old));
   }
   
   /* A normal pointer */
   else
   {
      ASSERT(OBJ_IS_DATAPTR(old));
      return ((byte *)CopyObject((word *)old));
   }
}

static void CopyCodeConstant(word **pt)
{
	*pt = CopyObject(*pt);
}

/******************************************************************************/
/*                                                                            */
/*      CopyStackWord                                                         */
/* Used to copy objects which are in stored within a stack frame.             */
/*                                                                            */
/******************************************************************************/
static byte *CopyStackWord (byte *old, word *base, word *end, int  byteOffset)
{
   /* A short? */
   if (OBJ_IS_AN_INTEGER(old))
   {
      /* do nothing */
      return (old);
   }

   /* A code-pointer? */
   else if (OBJ_IS_CODEPTR(old))
   {
     return (CopyCodePointer(old));
   }
   
   /* A pointer to within current stack segment? Adjust it. */
   else if (base <= (word *)old && (word *)old < end)
   {
      ASSERT(OBJ_IS_DATAPTR(old));
      return (old + byteOffset);
   }
   
   /* A normal pointer */
   else
   {
      ASSERT(OBJ_IS_DATAPTR(old));
      return ((byte *)CopyObject((word *)old));
   }
}

/******************************************************************************/
/*                                                                            */
/*      IS_DB_POINTER                                                         */
/*                                                                            */
/******************************************************************************/
static int IS_DB_POINTER(GCSpace *space, word *p)
{
  if (space == 0) return 0;
  
  if (p >= space->i_bottom && p < space->i_top) return 1;
  if (p >= space->m_bottom && p < space->m_top) return 1;

  if (space->parent == 0) return 0;
  else return IS_DB_POINTER (&(space->parent->gc_space),p);
}

/******************************************************************************/
/*                                                                            */
/*      CopyObject                                                            */
/*                                                                            */
/******************************************************************************/
static word *CopyObject (word *old)
{
  word *new;
  word  oldL; /* original length word */
  word  newL; /* new length word (oldL and newL may differ for stacks) */
  unsigned old_length;

  /* uninitialised word? */
  if (old == 0)
  {
     return old;
  }

  /* nil? */
  if (old == old_consts.nil)
  {
     return new_consts.nil;
  }
  
  /* short integer? */
  if (OBJ_IS_AN_INTEGER(Word(old)))
  {
     return old;
  }
  
  /* pointer into I/O area? */
  if (old_consts.io_bottom <= old && old < old_consts.io_top)
  {
     int io_number = (old - old_consts.io_bottom) / old_consts.io_spacing;
     
     word *old_io_address = 
        &old_consts.io_bottom[io_number * old_consts.io_spacing];
  
     /* Have we successfully decoded the I/O call? */
     if (old_io_address != old)
     {
        Crash ("CopyObject: bad I/O value; old=0x%8x, io_number=%i, old_io_address=0x%8x",
               (int)old, io_number, (int)old_io_address);
     }
     
     {
        word *new_io_address = 
          &new_consts.io_bottom[io_number * new_consts.io_spacing];

        if (new_io_address != old_io_address)
        {   
           proper_printf("mapping I/O address %p to %p (RTS call 0x%02x)\n",
                  old_io_address, new_io_address, io_number);
        }

        return new_io_address;
     }
  }
  
  /* pointer into database? */
  if (IS_DB_POINTER(db,old))
  {
     return old;
  }

  if (IS_OLD_IMM_POINTER(old))
  {
    /* if we need to restore the heap, set a bit to show where we've been */
    if (i_bitmap)
    {
      int bit_no = old - from.i_bottom;
      ASSERT(0 <= bit_no);
      SetBit(i_bitmap, bit_no);
    }
  }
  else if (IS_OLD_MUT_POINTER(old))
  {
    /* if we need to restore the heap, set a bit to show where we've been */
    if (m_bitmap)
    {
      int bit_no = old - from.m_bottom;
;
      ASSERT(0 <= bit_no);
      SetBit(m_bitmap, bit_no);
    }
  }
  else
  {
    Crash ("Bad pointer 0x%08x",old);
  }
  
  oldL = old[-1];
  
  /* new address already allocated? */
  if (OBJ_IS_POINTER(oldL))
  {
    /* tombstone contains forwarding address */
    word *new = Words(OBJ_GET_POINTER(oldL));
    
    /* Suppose the old data-structure contains forwarding pointers -
       this should be the case only if we are in the "share" program.
       Then we still have to copy the object itself. The simplest way
       to do this is to call CopyObject recursively.
       SPF 26/1/95 */
       
    if (IS_OLD_POINTER(new))
      {
#if 0
        proper_printf("warning: following old tombstone %08x\n",new);
#endif
        new = CopyObject(new);

        /* redirect the old tombstone - SPF 26/1/95 */
        old[-1] = OBJ_SET_POINTER(Word(new));
      }
    
    assert(IS_NEW_POINTER(new));
    assert(OBJ_IS_LENGTH(new[-1])); /* SPF 4/5/95 */
    
    return new;
  }
  
  /* not yet processed, so allocate a new copy of the object - SPF 24/1/95 */
  assert(OBJ_IS_LENGTH(oldL));
  old_length = OBJ_OBJECT_LENGTH(oldL);
  assert(0 < old_length);
  
  if (OBJ_IS_STACK_OBJECT(oldL))
  {
#if 0
    /* This assertion wouldn't necessarily hold in discgarb */
    assert(OBJ_IS_MUTABLE_OBJECT(oldL));
#endif
    newL = oldL & ~OBJ_MUTABLE_BIT;
  }
  else
  {
    newL = oldL;
  }

  /* N.B. we don't change the length of objects, even for stacks. */
  if (OBJ_IS_MUTABLE_OBJECT(newL))
  {
    new = NewMutable(old_length);
  }
  else
  {
    new = NewImmutable(old_length);
  }
  
  assert(IS_NEW_POINTER(new));

  /* Copy the object, without fixing up pointers. */
  memcpy(Bytes(new),Bytes(old),(int) old_length * sizeof(word));

  /* Set the length word in the new copy */
  new[-1] = newL;

  /* Set the tombstone in the old copy */
  old[-1] = OBJ_SET_POINTER(Word(new));

  /* Fix up the pointers contained in the new object. */
  if (OBJ_IS_STACK_OBJECT(newL))
  {
    StackObject *old_stack = (StackObject *) old;
    StackObject *new_stack = (StackObject *) new;
    word        *old_top   = old + old_length;
    word        *new_top   = new + old_length;
    int    old_stacksize   = old_top - old_stack->p_sp;
    
    /* Calculate the offset of the new stack from the old. */
    int byteOffset = (byte *)new - (byte *)old;
    
    /* sanity checks */
    assert (0 <= old_stacksize && old_stacksize < (int)old_length);
    assert (0 <= new_stack->p_nreg && new_stack->p_nreg < 100);
    
    /* Copy the program counter */
    new_stack->p_pc = CopyCodePointer(new_stack->p_pc);
    
    /* Adjust the standard registers that point into the stack. */
    assert (old < new_stack->p_sp && new_stack->p_sp <= old_top);
    new_stack->p_sp = 
      (word *)CopyStackWord ((byte *)new_stack->p_sp,
                             old, old_top, byteOffset);
    assert (new < new_stack->p_sp && new_stack->p_sp <= new_top);
    
    assert (old < new_stack->p_hr && new_stack->p_hr <= old_top);
    new_stack->p_hr = 
      (word *)CopyStackWord ((byte *)new_stack->p_hr,
                             old, old_top, byteOffset);
    assert(new < new_stack->p_hr && new_stack->p_hr <= new_top);
    
    {
      int new_stacksize = new_top - new_stack->p_sp;
      assert(new_stacksize == old_stacksize);
    }

    /* Fix up any other checked registers. */
    {
      byte **pt = (byte **)&(new_stack->p_reg[0]);
      int n = new_stack->p_nreg;
      while (n--)
      {
	*pt = CopyStackWord (*pt, old, old_top, byteOffset);
	pt ++;
      }
    }

    /* Skip the unchecked registers and the unused part of the stack */

    /* Fix up the main contents of the stack, from the stack-pointer up */
    {
      byte **pt  = (byte **)new_stack->p_sp;
      int n      = old_stacksize;
      while (n--)
      {
	*pt = CopyStackWord (*pt, old, old_top, byteOffset);
	pt ++;
      }
    }
  }
  else if (OBJ_IS_CODE_OBJECT(newL))
  {
    /* The last word of the code segment is the number of constants */
    word *last_word = new + old_length - 1;
    int const_count = *last_word;

    assert(0 <= const_count && const_count < (signed)old_length);

  /* If we have a code object, this is the place to flush the instruction
     cache at its *new* address (in case we have old code cached at
     that address). It doesn't matter if we haven't fixed up all the pointers
     yet, since these are only accessed via the data cache, so it doesn't
     matter if they are wrong in the instruction cache. All this caching
     stuff doesn't matter for discgarb (where we're not executing the ML
     code) but it could possibly make a difference in PolyML.commit().
     SPF 15/12/95
  */
    MD_flush_instruction_cache(new, old_length * sizeof(word));

    /* Process the constants. */
    {
      word *pt = last_word;
      int n    = const_count;
      while (n--)
      {
        pt --;
        *pt = (word)CopyConstant((byte *)*pt);
      }
    }

	MD_update_code_addresses((word**)new, (word**)old, newL, CopyCodeConstant);
  }
  else if (OBJ_IS_WORD_OBJECT(newL))
  {
    word **pt  = (word **)new;
    unsigned n = old_length;
    
    while (n--)
    {
      *pt = CopyObject (*pt);
      pt ++;
    }
  }
  
  /* check that tombstone hasn't been corrupted */
  assert(OBJ_IS_POINTER(old[-1]));
  assert(OBJ_GET_POINTER(old[-1]) == new);
  
  /* check new copy hasn't been corrupted */
  assert(new[-1] == newL);

  total_copied += old_length + 1; /* total space needed for object */
  
  return new;
}

/******************************************************************************/
/*                                                                            */
/*      RestoreOldHeap                                                        */
/*                                                                            */
/******************************************************************************/
void RestoreOldHeap (int i_bits, int m_bits)
{
   int i;
   assert (i_bitmap != 0 && m_bitmap != 0);
   
   for (i = 0; i < i_bits; i ++)
   {
     if (TestBit(i_bitmap, i))
     {
       word *old = from.i_bottom + i;
       word oldL;
       word *new;
       word newL;
       
       ASSERT(IS_OLD_IMM_POINTER(old));
       oldL = old[-1];

       ASSERT(OBJ_IS_POINTER(oldL));
       new = OBJ_GET_POINTER(oldL);
       
       ASSERT(IS_NEW_IMM_POINTER(new));
       newL = new[-1];
       
       ASSERT(OBJ_IS_LENGTH(newL));
       ASSERT(!OBJ_IS_MUTABLE_OBJECT(newL));
       old [-1] = newL;
     }
   }
   
   for (i = 0; i < m_bits; i++)
   {
     if (TestBit(m_bitmap, i))
     {
       word *old = from.m_bottom + i;
       word oldL;
       word *new;
       word newL;
       
       ASSERT(IS_OLD_MUT_POINTER(old));
       oldL = old[-1];
       
       ASSERT(OBJ_IS_POINTER(oldL));
       new = OBJ_GET_POINTER(oldL);
       
       /* The new pointer is not necessarily mutable, because
          we may have copied an immutable object that just
          happened to be living in the mutable space.
          SPF 14/6/95 */
       ASSERT(IS_NEW_POINTER(new));
       newL = new[-1];
       
       /* Once we've got the length-word from the object,
          we can check that the pointer's actually the right type.
          SPF 14/6/95 */
       ASSERT(OBJ_IS_LENGTH(newL));
       if (OBJ_IS_MUTABLE_OBJECT(newL))
       {
         ASSERT(IS_NEW_MUT_POINTER(new));
       }
       else
       {
         ASSERT(IS_NEW_IMM_POINTER(new));
       }
       
       /* If we've got a "frozen" stack, we need to "unfreeze" it */
       if (OBJ_IS_STACK_OBJECT(newL))
       {
         ASSERT(!OBJ_IS_MUTABLE_OBJECT(newL));
         old[-1] = newL | OBJ_MUTABLE_BIT;
       }
       else
       {
         old [-1] = newL;
       }
     }
   }
}

/******************************************************************************/
/*                                                                            */
/*      CopyGC                                                                */
/*  This function copies the heap into a new area, so that it can be written  */
/*  to a database. If "mustRestore" is non-zero, the heap is restored to its  */
/*  original state, afterwards, otherwise it is trashed.                      */
/*                                                                            */
/*  To save precious space in the (too-small) "mutable" area of the database, */
/*  all stack segments are made non-mutable and allocated in the immutable    */
/*  area. This is OK because the Poly/ML start-up code always copies stacks   */
/*  and makes them mutable at the start of each session.                      */
/*     SPF 23/11/95                                                           */
/*                                                                            */
/******************************************************************************/
word CopyGC 
(
  OpRootsFunc OpCopyRoots,    /* copy these roots */
  GCConst    *OLD_CONSTS,
  GCConst    *NEW_CONSTS,
  GCSpace    *DB,
  GCSpace    *FROM,
  GCSpace    *TO,
  Header      CONTEXTT, /* TT same as above */
  AllocFunc   newM,
  AllocFunc   newI,
  int         mustRestore /* do we need to restore the heap? */
)
{
  old_consts   = *OLD_CONSTS;
  new_consts   = *NEW_CONSTS;
  db           =  DB;
  from         = *FROM;
  to           = *TO;
  context      =  CONTEXTT;
  newMutable   =  newM;
  newImmutable =  newI;
  total_copied =  0;
  

#ifdef PORTING
  sparc_code_count = 0;
  int_code_count   = 0;
  other_code_count = 0;
#endif

  if (FROM == TO)
  {
     Crash ("CopyGC: from-space and to-space must differ");
  }
  
  if (mustRestore)
  { 
    /* allocate bitmaps to show where we've been */
    int i_bits = from.i_top - from.i_bottom;
    int m_bits = from.m_top - from.m_bottom;
    size_t i_bytes = BITMAP_BYTES(i_bits);
    size_t m_bytes = BITMAP_BYTES(m_bits);
    
    i_bitmap = (word *) alloca(i_bytes);
    m_bitmap = (word *) alloca(m_bytes);
    
    if (i_bitmap == 0 || m_bitmap == 0)
    {
      Crash ("Unable to allocate bitmaps in CopyGC (alloca  failed)");
    }
    
    /* initialise the bitmaps */
    memset(i_bitmap,0,i_bytes);
    memset(m_bitmap,0,m_bytes);

    /* Copy everything that is reachable from the "copy" roots */
    OpCopyRoots (CopyObject);
    *TO = to;
    
    /* Unfortunately, that has corrupted some objects in the from space,
       by turning them into tombstones. Since we still need the heap, we
       have to scan the bitmaps to restore the old length words. */
    RestoreOldHeap(i_bits, m_bits);
  }
  else
  { /* (re)initialise the bitmap pointers to 0 */
    i_bitmap = 0;
    m_bitmap = 0;

    /* Copy everything that is reachable from the "copy" roots */
    OpCopyRoots (CopyObject);
    *TO = to;
  
   /* We don't need to restore the corrupted heap, so do nothing. */
  }
#ifdef PORTING
  proper_printf("Code segments: SPARC=%i, interpreted=%i, other=%i\n",
          sparc_code_count, int_code_count, other_code_count);
#endif
  
  return total_copied;
}
