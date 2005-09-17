/*
    Title:      Storage allocation
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


#if defined(__GNUC__)
#define __USE_FIXED_PROTOTYPES__
#endif

#include <stdio.h>
#include <assert.h>
#include "globals.h"
#include "mmap.h"

typedef struct
{
  int  room;
} Buffer;


static Buffer M = {0};
static Buffer I = {0};

/* 
 * Initialise the allocator 
 */
void ResetAllocator(void)
{
  M.room = 0;
  I.room = 0;
}

/*
 * Allocates for new object of some length in the database buffer.
 * Note that databases grow UP, unlike local heaps, which grow DOWN.
 */
static word *NewObject (Header H, Space *S, Buffer *buffer, const int length)
{
  /* The buffer space up to S->pointer is mmaped, but only the top
     buffer->room words are available for use.
  */ 
  int words_required = length + 1; /* +1 for length word */
  word *next_free_word = Words(S->pointer) - buffer->room;

  /* Do we need to grow the buffer? */
  if (buffer->room < words_required)
  {
    /* not enough room in the buffer */
    int extra_words_needed = words_required - buffer->room;
    int extra_words_allocated = GrowSpace(H, S, extra_words_needed);
    
    assert(extra_words_needed <= extra_words_allocated);
    
    buffer->room += extra_words_allocated;
  }
  
  assert(buffer->room >= words_required);
  buffer->room -= words_required;
  
  /* The "+1" is because the length word should be at offset -1. */
  return(next_free_word + 1);
}

static void fillPage(Header H)
{
	int room = M.room;
	/* Allocate a dummy object to fill the remaining space. */
	word *dummy = NewObject(H, &H->m_space, &M, room-1);
	/* Set the length word to use up the space and make it a
	   byte segment so we ignore it in the g.c. */
	if (room > 1)
	{
		dummy[-1] = (room-1) | OBJ_BYTE_BIT | OBJ_MUTABLE_BIT;
	}
	else dummy[-1] = 0; /* It's probably zero anyway. */
}

/* 
 * Allocate a new mutable object
 */
word *NewMutable (Header H, const int length)
{
	/* OpOldMutables relies on the fact that rounding down the
	   start address of an object to a page boundary gives us
	   the genuine start of an object.  To maintain that we
	   have to ensure that the only objects which cross page
	   boundaries are those bigger than a page and in that
	   case we don't use the remainder of the final page. */
	int words_required = length + 1; /* +1 for length word */
	assert((unsigned)M.room < H->page_size/sizeof(word));

	if (words_required > M.room && M.room > 0)
		fillPage(H);

	/* At present we're constrained to use separate pages for
	   "large" objects.  That's because we don't always
	   pass the base address of an object to RemoveObjectProtection.
	   DCJM 22/1/01. */
	if ((unsigned)words_required > H->page_size/sizeof(word))
	{
		word *newBlock = NewObject(H, &H->m_space, &M, length);
		/* We mustn't use the rest of this page because if we
		   rounded down the start address of an object we'd be
		   in the middle of this object. */
		if (M.room > 0) fillPage(H);
		return newBlock;
	}
	else return NewObject(H, &H->m_space, &M, length);
}

/* 
 * Allocate a new immutable object
 */
word *NewImmutable (Header H, const int length)
{
  return NewObject(H, &H->i_space, &I, length);
}
