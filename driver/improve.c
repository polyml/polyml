/*
    Title:      Database optimiser.

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
This seems to include the code to handle the -c case of discgarb
in which common immutable structures are shared.
DCJM 12/4/00.
*/
#include <stdio.h>

/* #include "st.h" */

/* added 10/11/94 SPF */
#include <stdlib.h>
#include <assert.h>


#include <string.h>

#include "objects.h"
#include "copygc.h"
#include "mmap.h"
#include "diagnostics.h"
#include "proper_io.h"
#include "machine_dep.h"

#if (1) /*defined(WINDOWS_PC) || defined(HPUX) */
/* I don't know why this is used at all. DCJM 13/11/00. */
#define MALLOC malloc
#define FREE free
#define CLEANUP() 
#else
#define MALLOC Malloc
#define FREE Free
#define CLEANUP Cleanup 
/******************************************************************************/
/*                                                                            */
/*      Home-grown versions of malloc and free                                */
/*                                                                            */
/******************************************************************************/
#include <sys/types.h>
#include <sys/mman.h> 
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include "sys.h"

/* BIGCHUNKSIZE bytes must be a multiple of 2**FREELISTCOUNT words */
/* This is currently OK, providing PHYS_PAGE_BYTES is a multiple of 1K */
#define FREELISTCOUNT 11
#define BIGCHUNKSIZE (8*PHYS_PAGE_BYTES)

typedef struct MallocItemTag
{
  int size;
  struct MallocItemTag *link;
} MallocItem;

#define PAGEARRAYSIZE ((BIGCHUNKSIZE - 2 * sizeof(int) - sizeof(struct PageTableItemTag *)) / sizeof (MallocItem *))

typedef struct PageTableItemTag
{
  int size;
  struct PageTableItemTag *link;
  int nextIndex;
  MallocItem *pagearray[PAGEARRAYSIZE];
} PageTableItem;

static MallocItem *freeList[FREELISTCOUNT] = { 0 };

static PageTableItem *SmallChunksPageTable = 0;


static MallocItem *GetBigChunk(int realSize)
{
  static int Z_fd = -1;
  MallocItem *pt;

  assert(realSize > BIGCHUNKSIZE / 2);

  if (Z_fd < 0)
  {
    do {
       Z_fd = open("/dev/zero",O_RDONLY,0666);
    } while (Z_fd == NULLFD && errno == EINTR);
    if (Z_fd < 0)
    {
       SysError("Malloc: unable to open /dev/zero");
    }
  }

  pt = (MallocItem *)mmap(0, realSize, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE, Z_fd, 0L); 
                      
  if ((int)pt == -1)
  {
     SysError("GetBigChunk: mmap failed\n");
  }
  
  /* Needed because the page-table allocation doesn't call CompleteAllocation */
  pt->size = realSize;
  
  return pt;
}

static void FreeBigChunk (MallocItem *pt)
{
  assert(pt->size > BIGCHUNKSIZE / 2);

  if (munmap((caddr_t)pt,pt->size) != 0)
  { 
    SysError("FreeBigChunk: munmap failed\n");
  }
}

static void RecordSmallChunkPage(MallocItem *address)
{
  PageTableItem *pageTable = SmallChunksPageTable;
  while (pageTable && (pageTable->nextIndex == PAGEARRAYSIZE))
  {
    pageTable = pageTable->link;
  }
  
  if (pageTable == 0)
  {
    pageTable = (PageTableItem *)GetBigChunk(sizeof(PageTableItem)); 
                      
    pageTable->nextIndex = 0;
    pageTable->link = SmallChunksPageTable;
    SmallChunksPageTable = pageTable;
  }

  pageTable->pagearray[pageTable->nextIndex] = address;
  pageTable->nextIndex ++;
}

static MallocItem *GetSmallChunk(int freeListIndex, int chunkSize)
{
  if (freeListIndex == FREELISTCOUNT)
  {
    MallocItem *address;
    assert(chunkSize == BIGCHUNKSIZE);
    address = GetBigChunk(BIGCHUNKSIZE);
    RecordSmallChunkPage(address);
    return address;
  }
 
  else if (freeList[freeListIndex] == 0)
  {
    MallocItem *pt1 = GetSmallChunk(freeListIndex + 1, 2 * chunkSize);
    MallocItem *pt2 = (MallocItem *)((byte *)pt1 + chunkSize);
    
    pt2 -> link = 0;
    freeList[freeListIndex] = pt2;

    return pt1;
  }

  else
  {
    MallocItem *pt = freeList[freeListIndex];
    freeList[freeListIndex] = freeList[freeListIndex]->link;
    return pt;
  }
}

static void FreeSmallChunk(int freeListIndex, MallocItem *pt, int chunkSize)
{
  /* don't try to recombine allocations (yet) */
  assert(((freeListIndex == 0) || (chunkSize / 2 < pt-> size))
          && (pt-> size <= chunkSize));
  assert(0 <= freeListIndex && freeListIndex < FREELISTCOUNT);

  pt->link = freeList[freeListIndex];
  freeList[freeListIndex] = pt;
}

static void *CompleteAllocation(MallocItem *pt, int realSize)
{
  void *data = (void *)((byte *)pt + sizeof(MallocItem));
  pt->size = realSize;
  
  return data;
}

static void *Malloc(int size)
{
  assert (size > 0);
  {
    int realSize      = size + sizeof(MallocItem);
    int chunkSize     = BIGCHUNKSIZE;
    int nextChunkSize = chunkSize / 2;
    
    /* Does the allocation need a whole page to itself? */
    if (nextChunkSize < realSize)
    {
       return CompleteAllocation(GetBigChunk(size), realSize);
    }
  
    else
    {
      int freeListIndex = FREELISTCOUNT;
  
      /* realSize <= nextChunkSize  */
      while (freeListIndex && (realSize <= nextChunkSize))
      {
        /* realSize <= nextChunkSize  */
	freeListIndex --;
	chunkSize     = nextChunkSize;
	nextChunkSize = chunkSize / 2;
        /* realSize <= chunkSize  */
      }
     /* 0 <= freeListIndex < FREELISTCOUNT */
     /* realSize <= chunkSize  */

     return CompleteAllocation(GetSmallChunk(freeListIndex,chunkSize), realSize);
    }
  }
}

static void Free (void *data)
{
  MallocItem *pt    = (MallocItem *)((byte *)data - sizeof(MallocItem));
  int chunkSize     = BIGCHUNKSIZE;
  int nextChunkSize = chunkSize / 2;

  assert(pt->size > 0);

  /* Did the allocation need a whole page to itself? */
  if (nextChunkSize < pt->size)
  {
     FreeBigChunk(pt);
  }

  else
  {
    int freeListIndex = FREELISTCOUNT;

    /* pt->size < chunkSize  */
    while (freeListIndex && (pt->size <= nextChunkSize))
    {
      /* pt->size <= nextChunkSize  */
      freeListIndex --;
      chunkSize     = nextChunkSize;
      nextChunkSize = chunkSize / 2;
      /* pt->size <= chunkSize  */
    }
   /* 0 <= freeListIndex < FREELISTCOUNT */
   /* pt->size <= chunkSize  */

   FreeSmallChunk(freeListIndex,pt,chunkSize);
 }
}


static void FreeSmallChunkPages(PageTableItem *pageTable)
{
  
  if (pageTable != 0)
  {
    FreeSmallChunkPages(pageTable->link);
    
    while (0 < pageTable->nextIndex)
    {
      pageTable->nextIndex --;
      pageTable->pagearray[pageTable->nextIndex]->size = BIGCHUNKSIZE;
      FreeBigChunk(pageTable->pagearray[pageTable->nextIndex]);
    }
    
    FreeBigChunk((MallocItem *)pageTable);
  }
}

static void Cleanup(void)
{
  int freeListIndex;
  
  /* discard the small block free lists */
  for (freeListIndex = 0; freeListIndex < FREELISTCOUNT; freeListIndex ++)
  {
    freeList[freeListIndex] = 0;
  }
  
  /* discard the pages occupied by small blocks */
  FreeSmallChunkPages(SmallChunksPageTable);
  SmallChunksPageTable = 0;
}

#endif

/******************************************************************************/
/*                                                                            */
/*      Data objects                                                          */
/*                                                                            */
/******************************************************************************/
/* Meaning of length word:
  (1) bit 31 (msbit) = 0, bits 0-23 = length, bits 24-30 = flags
  (2) bit 31 = 1, bit 30 = 1, bits 0-30 = depth
  (2) bit 31 = 1, bit 30 = 0, bits 0-30 = forwarding pointer>>2
*/

typedef struct
{
  word  L;
  word *pt;
} Item;

typedef struct VectorStruct Vector;

struct VectorStruct
{
  word    depth;
  word    nitems;
  word    vsize;
  Item   *vector;
  Vector *next;
};

static GCConst  constants = { 0 }; /* was called "const" but that's a keyword */
static GCSpace *db        = 0;
static GCSpace  from      = { 0 };
static Vector  *vectors   = 0;

static word ImproveObject(word *);

#define IS_OLD_POINTER(p)\
 ((from.i_bottom <= (p) && (p) < from.i_top) || \
  (from.m_bottom <= (p) && (p) < from.m_top))

/******************************************************************************/
/*                                                                            */
/*      IS_DB_POINTER                                                         */
/*                                                                            */
/******************************************************************************/
static int IS_DB_POINTER (GCSpace *space, word *p)
{
  if (space == 0) return 0;
  
  if (p >= space->i_bottom && p < space->i_top) return 1;
  if (p >= space->m_bottom && p < space->m_top) return 1;
  
  return IS_DB_POINTER (space->parent ? & space->parent->gc_space : 0, p);
}

int MallocCount = 0;
int FreeCount = 0;


/******************************************************************************/
/*                                                                            */
/*      Valloc                                                                */
/*                                                                            */
/******************************************************************************/
static Item *Valloc(word n)
{
  char *p = (char *) MALLOC (n * sizeof(Item));
  
  if (p == 0)
  {
    SysError
      ("Malloc failed (already malloc'ed %d bytes, freed %d bytes, wanted %d more)",
        MallocCount, FreeCount, n * sizeof(Item));
  }
  
  MallocCount += n * sizeof(Item);
  
  return (Item *) p;
}

/******************************************************************************/
/*                                                                            */
/*      AddDepth                                                              */
/*                                                                            */
/******************************************************************************/
static Vector *AddDepth(word depth)
{
  Vector  *v;
  Vector **L = & vectors;
  
  for (v = *L; v && v->depth < depth; v = *L)
  {
    L = & v->next;
  }
  
  if (v && v->depth == depth)
  {
     return v;
  }
  
  v = (Vector *) MALLOC (sizeof(Vector));
  
  if (v == 0) 
  {
    SysError 
     ("malloc failed (already malloc'ed %d bytes, freed %d bytes, wanted %d more)", 
       MallocCount, FreeCount, sizeof(Vector));
  }
  
  MallocCount += sizeof(Vector);

  v->depth  =  depth;
  v->nitems =  0;
  v->vsize  =  0;
  v->vector =  0;
  v->next   = *L;
  
  *L = v;
  
  return v;
}

/******************************************************************************/
/*                                                                            */
/*      AddToVector                                                           */
/*                                                                            */
/******************************************************************************/
static void AddToVector(word depth, word L, word *pt)
{
  Vector *v = AddDepth (depth);
  
  assert (v->nitems <= v->vsize);
  
  if (v->nitems == v->vsize)
  {
    if (v->vsize == 0)
    {
      assert (v->vector == 0);
/*    v->vsize  = 1; */
/*    v->vsize  = 16;    SPF 10/2/95 - reduce malloc fragmentation */
      v->vsize  = 15; /* SPF 12/4/95 - allow for malloc overhead (2**n - 1) */
      v->vector = Valloc (v->vsize);
      assert (v->vector != 0);
    }
    else
    {
      word  new_vsize  = 2 * v->vsize + 1; /* SPF 12/4/95 */
      Item *new_vector = Valloc (new_vsize);
      /* word  i; */
      
      assert (new_vsize > 0);
      assert (v->vector  != 0);
      assert (new_vector != 0);

      memmove (new_vector, v->vector, v->vsize * sizeof(Item));
      
      FREE (v->vector);
      FreeCount += v->vsize * sizeof(Item);
      
      v->vector = new_vector;
      v->vsize  = new_vsize;
    }
  }
  
  assert (v->nitems < v->vsize);
  
  v->vector[v->nitems].L  = L;
  v->vector[v->nitems].pt = pt;
  
  v->nitems++;

  assert (v->nitems <= v->vsize);
}

/******************************************************************************/
/*                                                                            */
/*      CompareItems                                                          */
/*                                                                            */
/******************************************************************************/
static int CompareItems(const void *arg_a, const void *arg_b)
{
  Item *a = (Item *)arg_a;
  Item *b = (Item *)arg_b;

  word i, n;
  
  word *x = a->pt;
  word *y = b->pt;
  word  A = x[-1];
  word  B = y[-1];
  
  assert (OBJ_IS_DEPTH(A));
  assert (OBJ_IS_DEPTH(B));
  assert (A == B); /* SPF; same depth */

  assert (OBJ_IS_LENGTH(a->L));
  assert (OBJ_IS_LENGTH(b->L));
  
  if (a->L > b->L) return  1;
  if (a->L < b->L) return -1;
  
  assert (a ->L == b->L);

  /* There's a problem sharing code objects if they have relative calls/jumps
     in them to other code.  The code of two functions may be identical (e.g.
	 they both call functions 100 bytes ahead) and so they will appear the
	 same but if the functions they jump to are different they are actually
	 different.  Don't share them for the moment.  DCJM 4/1/01
  */
  if (OBJ_IS_CODE_OBJECT(a->L))
  {
	  if (x > y) return 1; else return -1;
  }
  
  n = OBJ_OBJECT_LENGTH(a->L);
  
  for (i = 0; i < n; i++, x++, y++)
  {
    word X = *x;
    word Y = *y;
    
    if (X > Y) return  1;
    if (X < Y) return -1;
  }
  
  return 0;
}

/******************************************************************************/
/*                                                                            */
/*      MergeSameItems                                                        */
/*                                                                            */
/******************************************************************************/
static word MergeSameItems(Vector *v)
{
  word  N = v->nitems;
  Item *V = v->vector;
  word  n = 0;
  word  i = 0;
  word  j;
  
  while (i < N)
  {
    assert (OBJ_IS_DEPTH(V[i].pt[-1]));
    
    for (j = i+1; j < N; j++)
    {
      assert (OBJ_IS_DEPTH(V[j].pt[-1]));

      if (CompareItems (& V[i], & V[j]) != 0) break;
      
      V[j].pt[-1] = OBJ_SET_POINTER(Word(V[i].pt)); /* an indirection */
      assert (OBJ_IS_POINTER(V[j].pt[-1]));
      
      n++;
    }

    V[i].pt[-1] = V[i].L; /* restore genuine length word */
    assert (OBJ_IS_LENGTH(V[i].pt[-1]));
    
    assert (i < j);
    i = j;
  }
  
  return n;
}


/******************************************************************************/
/*                                                                            */
/*      FixupPointer                                                          */
/*                                                                            */
/******************************************************************************/
static word *FixupPointer(word *old)
{
  word L;

  if (OBJ_IS_AN_INTEGER(Word(old))) return old; /* not a pointer */
  
  if (old == 0) return old;

  if (old == constants.nil) return old;
  
  if (constants.io_bottom <= old && old < constants.io_top) return old;
  
  if (IS_DB_POINTER(db, old)) return old;
  
  if (! IS_OLD_POINTER(old)) Crash ("Bad pointer 0x%08x", old);
  
  L = old[-1];
  
  if (OBJ_IS_DEPTH(L)) /* still a depth tombstone (because of some cycle) */
     return old; 
  
  if (OBJ_IS_POINTER(L)) /* tombstone is a pointer to a shared object */
    {
      word *new = Words(OBJ_GET_POINTER(L));
      assert (OBJ_IS_LENGTH(new[-1]));
      return new;
    }
  
  assert (OBJ_IS_LENGTH(L)); /* object is not shared */
  return old;
}

/******************************************************************************/
/*                                                                            */
/*      FixupStackChildren                                                    */
/*                                                                            */
/******************************************************************************/
static void FixupStackChildren
(
  word **pt,
  word   length,
  word  *base,
  word  *end,
  word   is_code
)
{
  while (length--)
  {
    word *val = *pt;
    
    if (IS_OLD_POINTER(val))
    {
      if (is_code || OBJ_IS_CODEPTR(val))
      {
       /* 
          Badly-aligned program counter might look like an integer, or
          a data-pointer, so the "is_code" flag over-rides the pointer-type
          check. Does this ever actually happen, even on the PC? Possibly
          it does, if we get a stack-overflow in the assembly code. This would
          probably cause the garbage-collector to break, but we can always
          hope. SPF 27/1/95.
       */
	   /*
	      Function return addresses are always aligned onto a word + 2 byte
		  boundary but traps are not.  We can get traps as a result of long
		  precision arithmetic, heap limit reached (g.c. requests) and
		  stack overflow.  This part of the code is modelled on the local
		  memory garbage collector and any of these traps may result in a
		  g.c.  Only the last of these, stack overflow, can have occurred
		  here since stack overflow trapping is the way we interrupt processes
		  when their time slice is used up.  If we have several processes
		  running when we call commit we may well have processes with pc
		  values that are not word + 2 byte aligned.  DCJM 4/1/01.
	   */
       
        word *old;
        word *new;

        /* Find the code it is associated with. After the code of the
           procedure there is a zero word followed by the byte offset from
           the start of the procedure. This gives us the address of the
           procedure block */
      
        OBJ_CODEPTR_TO_PTR(val, old);
        new = FixupPointer (old);
      
        *pt += (new-old);  /* so move original code pointer */
      }
      
      else if (OBJ_IS_AN_INTEGER(val))
        {
          /* Do nothing, carefully; we mustn't have shorts
             masquerading as pointers. */
      
        }
      
      else if (val <= base || val >= end) /* not code, but may be within stack */
      {
        /* a normal pointer */
        assert(OBJ_IS_DATAPTR(val));
        *pt = FixupPointer (val);
      }
    }
 
    pt++;
  }
}

/* Function to process constants within the code.  */
static void FixupConstant(word **old)
{
	FixupStackChildren(old, 1, 0, 0, 0);
}

/******************************************************************************/
/*                                                                            */
/*      FixupObject                                                           */
/*                                                                            */
/******************************************************************************/
static void FixupObject(word *old, int L)
{
  word *pt = old;
  word  n;

  assert (IS_OLD_POINTER(old));
  assert (OBJ_IS_LENGTH(L));
  assert (OBJ_IS_DEPTH(pt[-1]));
  
  n = OBJ_OBJECT_LENGTH(L);
  
  if (OBJ_IS_STACK_OBJECT(L))
  {
    StackObject *stack = (StackObject *) pt;
    word        *end   = old + n;
    word         skip  = stack->p_sp - pt;
    
    assert (skip <= n);
    
    FixupStackChildren ((word **)(& stack->p_pc), 1, old, end, 1);  /* is code pointer */
    FixupStackChildren (& stack->p_sp, 1, old, end, 0);
    FixupStackChildren (& stack->p_hr, 1, old, end, 0);
    
    FixupStackChildren ((word **)(stack->p_reg), stack->p_nreg, old, end, 0);
    
    /* Skip to the start of the stack. */
      
    pt += skip; 
    n  -= skip;

    FixupStackChildren ((word **)pt, n, old, end, 0);
  }
  else if (OBJ_IS_CODE_OBJECT(L))
  {
	/* Update code addresses. */
    MD_update_code_addresses((word**)pt, (word**)pt, L, FixupConstant);

    /* Skip to the constants. */
    
    pt += n;
    n   = pt[-1];
    pt -= n + 1;
	while (n--)
	{
		FixupConstant((word**)pt);
		pt++;
	}
  }
  else if (OBJ_IS_WORD_OBJECT(L))
  {
    while (n--) { *pt = (word) FixupPointer((word *)*pt); pt++; }
  }
}

/******************************************************************************/
/*                                                                            */
/*      FixupItems                                                            */
/*                                                                            */
/******************************************************************************/
static void FixupItems (Vector *v)
{
  word  N = v->nitems;
  Item *V = v->vector;
  word  i;
  
  for (i = 0; i < N; i++)
  {
    FixupObject(V[i].pt, V[i].L);
  }
}

/******************************************************************************/
/*                                                                            */
/*      max                                                                   */
/*                                                                            */
/******************************************************************************/
#ifdef max
#undef max
#endif
static word max (a, b)
word a, b;
{
  if (a > b) return a;
  return b;
}

/******************************************************************************/
/*                                                                            */
/*      ImproveStackChildren                                                  */
/*                                                                            */
/******************************************************************************/
static word ImproveStackChildren
(
  word  *pt, /* was word** */
  word   length,
  word  *base,
  word  *end,
  word   is_code
)
{
  word depth = 0;
  
  while (length--)
  {
    word *val = (word *)*pt;
    
    if (IS_OLD_POINTER(val))
    {
      if (is_code || OBJ_IS_CODEPTR(val))
      {      
        /* Traces the value in the program counter or return address */
        {
	  word *old;
	  
	  /* Find the code it is associated with. After the code of the
	     procedure there is a zero word followed by the byte offset from
	     the start of the procedure. This gives us the address of the
	     procedure block */
	
	  OBJ_CODEPTR_TO_PTR(val, old);
	  depth = max (depth, ImproveObject (old));
	}
      }

      else if (OBJ_IS_AN_INTEGER(val))
        {
           /* Do nothing, carefully; we mustn't have shorts
              masquerading as pointers. */
        }

      else if (val <= base || val >= end) /* not code, but may be within stack */
      {
        /* a normal pointer */
        assert(OBJ_IS_DATAPTR(val));
        depth = max (depth, ImproveObject (val));
      }
    }
 
    pt++;
  }
  
  return depth;
}

/* Passed to MD_update_code_addresses to process constants in the code.
   This is messy. */
static int maxConstDepth;
static void ImproveConstant(word **old)
{
	/* Remember the old value of depth.  We may recurse so we can't rely
	   on it remaining unchanged across calls to ImproveStackChildren. */
	int depth = maxConstDepth;
	maxConstDepth = max(depth, ImproveStackChildren((word*)old, 1, 0, 0, 0));
}

/******************************************************************************/
/*                                                                            */
/*      ImproveObject                                                         */
/*                                                                            */
/******************************************************************************/
static word ImproveObject(word *old)
{
  word *pt;
  word  L;
  word  n;
  word  depth = 0;

  if (OBJ_IS_AN_INTEGER(Word(old))) return 0; /* not a pointer */
  
  if (old == 0) return 0;

  if (old == constants.nil) return 0;
  
  if (old >= constants.io_bottom && old < constants.io_top) return 0;
  
  if (IS_DB_POINTER(db, old)) return 0;
  
  if (! IS_OLD_POINTER(old))
  {
    Crash ("Bad pointer 0x%08x", old);
  }
  
  L = old[-1];
  
  if (OBJ_IS_DEPTH(L)) /* tombstone contains genuine depth or 0 */
  {
    return OBJ_GET_DEPTH(L);
  }
  
  assert (OBJ_IS_LENGTH(L));
  
  if (L == 1 && old[0] == 1)
  {
    return 0; /* Why this special case? */
  }

  /* set initial depth to 0 (to cope with loops) */
  old[-1] = OBJ_SET_DEPTH(0);
  
  pt = old;
  n  = OBJ_OBJECT_LENGTH(L);
  
  if (OBJ_IS_STACK_OBJECT(L))
  {
    StackObject *stack = (StackObject *) pt;
    word        *end   = old + n;
    word         skip  = stack->p_sp - pt;

    assert (skip <= n);
    
    depth = max (depth, ImproveStackChildren (Words(& stack->p_pc), 1, old, end, 1));  /* is code pointer */
    depth = max (depth, ImproveStackChildren (Words(& stack->p_sp), 1, old, end, 0));
    depth = max (depth, ImproveStackChildren (Words(& stack->p_hr), 1, old, end, 0));
    
    depth = max (depth, ImproveStackChildren (stack->p_reg, stack->p_nreg, old, end, 0));
    
    /* Skip to the start of the stack. */
      
    pt += skip; 
    n  -= skip;

    depth = max (depth, ImproveStackChildren (pt, n, old, end, 0));
  }
  else if (OBJ_IS_CODE_OBJECT(L))
  {
	maxConstDepth = 0;
    MD_update_code_addresses((word**)pt, (word**)pt, L, ImproveConstant);
    /* Skip to the constants. */
    pt += n;
    n   = pt[-1];
    pt -= n + 1;

	while (n--)
	{
		ImproveConstant((word**)pt);
		pt++;
	}
	depth = maxConstDepth;
  }
  else if (OBJ_IS_WORD_OBJECT(L))
  {
    while (n--) depth = max (depth, ImproveObject((word *)*pt++));
  }
  
  /* What happens if we encountered this mutable object in a loop
     within the above code? Should be OK.
     We also treat "frozen" stack objects as non-sharable, since we
     don't want two processes to share the same stack. This might be
     safe (since the RTS copies stacks when it initialises) but I
     don't want to risk it. SPF 24/11/95
   */
  if (OBJ_IS_MUTABLE_OBJECT(L) || OBJ_IS_STACK_OBJECT(L))
  {
    /* restore genuine length word */
    old[-1] = L;
    
    return 0;
  }
  
  depth++;

  /* set genuine depth */
  old[-1] = OBJ_SET_DEPTH(depth);

  /* add to vector at correct depth */
  AddToVector (depth, L, old);

  
 return depth;
}

/******************************************************************************/
/*                                                                            */
/*      ImproveSharing                                                        */
/*                                                                            */
/******************************************************************************/
void ImproveSharing
(
  VoidFunc OpRoots,
  GCConst *CONSTT,
  GCSpace *DB,
  GCSpace *FROM,
  int      verbose
)
{
  word totalObjects = 0;
  word totalShared  = 0;
  
  constants   = *CONSTT;
  db      =  DB;
  from    = *FROM;
  vectors =  0;
  
  proper_printf("Sharing ...\n");
  
  OpRoots (ImproveObject);
  
  while (vectors)
  {
    Vector *v = vectors;
    word n;
    
    FixupItems(v);
    qsort (v->vector, v->nitems, sizeof(Item), CompareItems);
    
    n = MergeSameItems (v);
    
    if (n && verbose)
    {
       proper_printf("Level %4d, Objects %6d, Shared %6d\n", v->depth, v->nitems, n);
    }
    
    totalObjects += v->nitems;
    totalShared  += n;
    
    /* Get the next vector (if any) */
    vectors = v->next;
    
    /* Free the space associated with this level. SPF 16/6/95 */
    FREE(v->vector);
    FreeCount += v->vsize * sizeof(Item);
    
    FREE(v); 
    FreeCount += sizeof(Vector);
  }
  
  /* discard any resources used by the memory allocator */
  CLEANUP();
/* 
   At this stage, we have fixed up most but not all of the forwarding
   pointers. The ones that we haven't fixed up arise from situations
   such as the following:
   
           X -> Y <-> Z
           
   i.e. Y and Z form a loop, and X is isomorphic to Z. When we assigned
   the depths, we have to arbitrarily break the loop between Y and Z.
   Suppose Y is assigned to level 1, and Z is assigned to level 2.
   When we process level 1 and fixup Y, there's nothing to do, since
   Z is still an ordinary object. However when we process level 2, 
   we find that X and Z are isomorphic so we arbitrarily choose one
   of them and turn it into a "tombstone" pointing at the other. If
   we change Z into the tombstone, then Y now contains a pointer
   that needs fixing up. That's why we need the second fixup pass.
   
   Note also that if we had broken the loop the other way, we would have
   assigned Z to level 1, Y to level 2 and X to level 3, so we would
   have missed the chance to share Z and X. Perhaps that's why running
   the program repeatedly sometimes finds extra things to share? 
   
   MJC's original algorithm would have put Z in both levels 1 and 3
   (I think). Perhaps this was intended to catch such problems?
   I rather lose a little sharing than endanger the invariant that
   each object is processed (at most) once, at least until I work
   out how to handle loops properly.
   
   We could try something like the following:
     
      for (v = vectors; v; v = v->next)
      {
	FinalFixupItems(v);
      }

   but this doesn't solve the problem if one of the roots points at a
   tombstone - unfortunately OpRoots doesn't let us redirect the roots.
   We also have the same problem if any refs point at tombstones, 
   since the refs are not recorded in the vectors either.
  
   MJC's solution is to follow the "improve" pass with a
   "copygc" pass to sort out the tombstones. I don't like this
   because it seems to make the copygc invariant weaker (we can't
   assume that the object being copied is tombstone-free) but I can't
   think of anything better, so that's what we'll do.

   SPF 26/1/95

   
*/

  proper_printf ("Total Objects %6d, Total Shared %6d\n", totalObjects, totalShared);
  if (verbose)
  {
     proper_printf("malloc space allocated = %d bytes, freed = %d bytes\n\n", 
              MallocCount, FreeCount);
  }
}
